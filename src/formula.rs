use std::{borrow::Cow, rc::Rc, str::FromStr};

use crate::{SLoc, Sheet};

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug)]
enum Spec {
    dd,
    dC,
    dR,
    dSd,
    dSC,
    dSR,
    dFd,
    dFC,
    dFR,
}

// These have cell references since errors propagate, and knowing origin will help
#[derive(Clone, Debug)]
pub(crate) enum CellError {
    // Loop of cells that depend on each other
    ReferenceLoop {
        loop_point: Option<SLoc>,
    },
    // Formula cannot be parsed
    InvalidFormula {
        cell: Option<SLoc>,
        cursor: usize,
        reason: &'static str,
    },
    // Argument is a wrong type
    BadArgument {
        cell: Option<SLoc>,
        cursor: usize,
        expected_types: Vec<String>,
    },
    // Value of empty cell requested
    MissingCell {
        cell: Option<SLoc>,
    },
    // Two ranges are trying to write to the same cell
    RangeIntersect {
        cell: Option<SLoc>,
        source1: SLoc,
        source2: SLoc,
    },
}

#[derive(Clone, Debug)]
pub(crate) enum Const {
    Num(f64),
    String(String),
    Bool(bool),
    Error(CellError),
}

impl Into<String> for &Const {
    fn into(self) -> String {
        match self {
            Const::Num(n) => format!("{n}"),
            Const::String(s) => s.clone(),
            Const::Bool(b) => format!("{b}"),
            Const::Error(e) => format!("{e:?}"),
        }
    }
}
impl Into<String> for Const {
    fn into(self) -> String {
        match self {
            Const::Num(n) => format!("{n}"),
            Const::String(s) => s,
            Const::Bool(b) => format!("{b}"),
            Const::Error(e) => format!("{e:?}"),
        }
    }
}
impl FromStr for Const {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(b) = s.parse() {
            Ok(Self::Bool(b))
        } else if let Ok(n) = s.parse() {
            Ok(Self::Num(n))
        } else {
            Ok(Self::String(s.to_owned()))
        }
    }
}

impl Const {
    fn into_pos(self, pos: SLoc) -> Self {
        match self {
            Const::Error(e) => Self::Error(match e {
                CellError::ReferenceLoop { loop_point: None } => CellError::ReferenceLoop {
                    loop_point: Some(pos),
                },
                CellError::InvalidFormula {
                    cell: None,
                    cursor,
                    reason,
                } => CellError::InvalidFormula {
                    cell: Some(pos),
                    cursor,
                    reason,
                },
                CellError::BadArgument {
                    cell: None,
                    cursor,
                    expected_types: expected_type,
                } => CellError::BadArgument {
                    cell: Some(pos),
                    cursor,
                    expected_types: expected_type,
                },
                CellError::MissingCell { cell: None } => CellError::MissingCell { cell: Some(pos) },
                CellError::RangeIntersect {
                    cell: None,
                    source1,
                    source2,
                } => CellError::RangeIntersect {
                    cell: Some(pos),
                    source1,
                    source2,
                },
                _ => e,
            }),
            _ => self,
        }
    }
    fn to_pos(&self, pos: SLoc) -> Self {
        self.clone().into_pos(pos)
    }
    fn merge_err(&self, rhs: &Const) -> Option<CellError> {
        if let Self::Error(e) = self {
            Some(e.clone())
        } else if let Self::Error(e) = rhs.clone() {
            Some(e.clone())
        } else {
            None
        }
    }
}

// impl ToOwned for Const {
//     type Owned = Const;

//     fn to_owned(&self) -> Self::Owned {
//         self.clone()
//     }
// }

#[derive(Clone, Debug)]
pub(crate) enum Value {
    Const(Const),
    Ref(SLoc),
    Special(Spec),
    Add(Rc<Value>, Rc<Value>),
    Sub(Rc<Value>, Rc<Value>),
    Mul(Rc<Value>, Rc<Value>),
    Div(Rc<Value>, Rc<Value>),
    Lt(Rc<Value>, Rc<Value>),
    Func { name: String, args: Vec<Value> },
}

trait IsIdentifier {
    fn is_iden(&self) -> bool;
}
impl IsIdentifier for char {
    fn is_iden(&self) -> bool {
        self.is_alphanumeric() || self == &'.'
    }
}

fn form_tokens<'a>(formula: &'a str) -> Vec<(&'a str, usize)> {
    let mut v = vec![];
    let mut start = 0;
    let mut alpha = true;

    let mut string_mode = false;
    let mut escape_mode = false;
    for (i, c) in formula.chars().enumerate() {
        if alpha {
            if !c.is_iden() {
                alpha = false;
                v.push((&formula[start..i], start));
                start = i;
            }
        }
        if !alpha {
            if c == '"' || string_mode {
                if string_mode {
                    if c == '"' && !escape_mode {
                        string_mode = false;
                        v.push((&formula[start..=i], start));
                        start = i + 1;
                    } else if c == '\\' && !escape_mode {
                        escape_mode = true;
                    }
                } else if c == '"' {
                    string_mode = true;
                }
            } else {
                if c.is_iden() {
                    alpha = true;
                    start = i;
                    continue;
                }
                if !c.is_whitespace() {
                    v.push((&formula[i..i + 1], i));
                    start = i + 1;
                }
            }
        }
    }
    if start < formula.len() {
        v.push((&formula[start..], start));
    }
    v
}

fn form_tree_basic<'a>(tokens: Vec<(&'a str, usize)>) -> Value {
    if tokens.is_empty() {
        Value::Const(Const::Error(CellError::InvalidFormula {
            cell: None,
            cursor: 1,
            reason: "Nothing after =",
        }))
    } else if tokens.len() == 3 {
        let t1 = if let Some(reff) = is_ref(tokens[0].0) {
            Value::Ref(reff)
        } else if let Ok(cn) = tokens[0].0.parse::<f64>() {
            Value::Const(Const::Num(cn))
        } else {
            Value::Const(Const::Error(CellError::InvalidFormula {
                cell: None,
                cursor: tokens[0].1,
                reason: "Token not value or reference",
            }))
        };
        let t3 = if let Some(reff) = is_ref(tokens[2].0) {
            Value::Ref(reff)
        } else if let Ok(cn) = tokens[2].0.parse::<f64>() {
            Value::Const(Const::Num(cn))
        } else {
            Value::Const(Const::Error(CellError::InvalidFormula {
                cell: None,
                cursor: tokens[2].1,
                reason: "Token not value or reference",
            }))
        };
        dbg!((&t1, tokens[1], &t3));
        match tokens[1].0 {
            "+" => Value::Add(Rc::new(t1), Rc::new(t3)),
            // "-" => Value::Sub(Rc::new(t1), Rc::new(t3)),
            _ => Value::Const(Const::Error(CellError::InvalidFormula {
                cell: None,
                cursor: tokens[1].1,
                reason: "Operation not allowed",
            })),
        }
    } else if tokens.len() == 1 {
        if let Some(reff) = is_ref(tokens[0].0) {
            Value::Ref(reff)
        } else if let Ok(cn) = tokens[0].0.parse::<f64>() {
            Value::Const(Const::Num(cn))
        } else {
            Value::Const(Const::Error(CellError::InvalidFormula {
                cell: None,
                cursor: 1,
                reason: "Token not value or reference",
            }))
        }
    } else {
        Value::Const(Const::Error(CellError::InvalidFormula {
            cell: None,
            cursor: tokens.last().expect("Wasnt empty").1,
            reason: "Invalid number of tokens",
        }))
    }
}

fn split_parens<'a, 'b>(
    tokens: &'b [(&'a str, usize)],
) -> Result<Vec<&'b [(&'a str, usize)]>, Value> {
    let mut split_parens = vec![];
    let mut pcount = 0;
    let mut start = 0;
    let mut has_parens = false;
    for (i, t) in tokens.iter().enumerate() {
        if t.0 == "(" {
            pcount += 1;
            has_parens = true;
        }
        if t.0 == ")" {
            pcount -= 1;
        }
        if pcount < 0 {
            return Err(Value::Const(Const::Error(CellError::InvalidFormula {
                cell: None,
                cursor: t.1,
                reason: "Ending parenthesis with no opening",
            })));
        }
        if pcount == 0 {
            if has_parens {
                split_parens.push(&tokens[start + 1..i]);
                has_parens = false;
            } else {
                split_parens.push(&tokens[start..=i]);
            }
            start = i + 1;
        }
    }
    if pcount != 0 {
        Err(Value::Const(Const::Error(CellError::InvalidFormula {
            cell: None,
            cursor: tokens.last().unwrap().1,
            reason: "Missing closing parenthesis",
        })))
    } else {
        Ok(split_parens)
    }
}

// TODO functions with no arguments
// TODO parse ranges of cells
fn form_tree<'a>(tokens: &[(&'a str, usize)]) -> (Value, bool) {
    let split_parens = match split_parens(&tokens) {
        Ok(v) => v,
        Err(e) => return (e, false),
    };
    if split_parens.len() == 1 {
        let sp = split_parens[0];
        return if sp[0].0 == "(" {
            (form_tree(&sp[1..sp.len() - 1]).0, true)
        } else if let Some(r) = is_ref(sp[0].0) {
            (Value::Ref(r), false)
        } else if let Some(c) = is_const(sp[0].0) {
            (Value::Const(c), false)
        } else {
            (Value::Special(Spec::dd), false)
        };
    }
    dbg!(&split_parens);
    enum Category {
        Value,
        Operand,
        Function,
    }

    let cats = split_parens.iter().map(|s| {
        if s[0].0.chars().all(|x| x.is_alphabetic()) {
            Category::Function
        } else if s[0].0.len() == 1 && !s[0].0.chars().next().unwrap().is_alphanumeric() {
            Category::Operand
        } else {
            Category::Value
        }
    });
    #[derive(Clone, Debug)]
    enum CatParsed<'a> {
        Value((Value, bool)),
        Args(Vec<Value>),
        Operand(char),
        Function(&'a str),
    }

    let mut parseds = split_parens
        .iter()
        .zip(cats)
        .map(|(v, c)| match c {
            Category::Value => {
                if v.iter().any(|c| c.0 == ",") {
                    CatParsed::Args(v.split(|s| s.0 == ",").map(|v| form_tree(v).0).collect())
                } else {
                    CatParsed::Value(form_tree(v))
                }
            }
            Category::Operand => CatParsed::Operand(v[0].0.chars().next().unwrap()),
            Category::Function => CatParsed::Function(v[0].0),
        })
        .peekable();

    let p2 = parseds.clone();
    for e in p2 {
        dbg!(e);
    }

    // Resolve function calls
    let mut fn_pass = vec![];
    let mut i = 0;
    while parseds.peek().is_some() {
        let e = dbg!(parseds.next().expect("Had to be some"));
        if let CatParsed::Function(name) = e {
            let lp = split_parens[i].last().unwrap();
            let Some(args) = parseds.next() else {
                return (
                    Value::Const(Const::Error(CellError::InvalidFormula {
                        cell: None,
                        cursor: lp.1,
                        reason: "Function has no arguments",
                    })),
                    false,
                );
            };
            dbg!(&args);
            i += 1;
            let args = match args {
                CatParsed::Value(v) => vec![v.0],
                CatParsed::Args(a) => a,
                CatParsed::Operand(_) | CatParsed::Function(_) => {
                    return (
                        Value::Const(Const::Error(CellError::InvalidFormula {
                            cell: None,
                            cursor: lp.1,
                            reason: "Invalid function arguments",
                        })),
                        false,
                    )
                }
            };
            fn_pass.push((
                CatParsed::Value((
                    Value::Func {
                        name: name.to_lowercase(),
                        args,
                    },
                    false,
                )),
                lp.1,
            ));
        } else if let CatParsed::Args(_) = e {
            let lp = split_parens[i].last().unwrap();
            return (
                Value::Const(Const::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: lp.1,
                    reason: "Arguments with no function",
                })),
                false,
            );
        } else {
            fn_pass.push((e, split_parens[i].last().unwrap().1));
        }
        i += 1;
    }
    for (i, e) in fn_pass.iter().enumerate() {
        if i % 2 == 0 {
            // assert!(matches!(e, CatParsed::Value(_)))
            if matches!(e.0, CatParsed::Operand(_)) {
                return (
                    Value::Const(Const::Error(CellError::InvalidFormula {
                        cell: None,
                        cursor: e.1,
                        reason: "Found operand where value was expected",
                    })),
                    false,
                );
            }
        } else {
            if matches!(e.0, CatParsed::Value(_)) {
                return (
                    Value::Const(Const::Error(CellError::InvalidFormula {
                        cell: None,
                        cursor: e.1,
                        reason: "Found value where operand was expected",
                    })),
                    false,
                );
            }
        }
    }

    fn has_mul(vals: &[(CatParsed, usize)]) -> Option<usize> {
        vals.iter()
            .position(|(x, _)| matches!(x, CatParsed::Operand('*' | '/')))
    }
    while let Some(p) = has_mul(&fn_pass) {
        let inpos = p - 1;
        let lhs = fn_pass.remove(p - 1);
        let (CatParsed::Value(lhs), i) = lhs else {
            return (
                Value::Const(Const::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: lhs.1,
                    reason: "Left side of mul/div was not value",
                })),
                false,
            );
        };
        let rhs = fn_pass.remove(p);
        let CatParsed::Value(rhs) = rhs.0 else {
            return (
                Value::Const(Const::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: rhs.1,
                    reason: "Right side of mul/div was not value",
                })),
                false,
            );
        };
        if matches!(fn_pass[inpos].0, CatParsed::Operand('*')) {
            fn_pass[inpos] = (
                CatParsed::Value((Value::Mul(lhs.0.into(), rhs.0.into()), false)),
                i,
            );
        } else {
            fn_pass[inpos] = (
                CatParsed::Value((Value::Div(lhs.0.into(), rhs.0.into()), false)),
                i,
            );
        }
    }

    fn has_add(vals: &[(CatParsed, usize)]) -> Option<usize> {
        vals.iter()
            .position(|(x, _)| matches!(x, CatParsed::Operand('+' | '-')))
    }
    while let Some(p) = has_add(&fn_pass) {
        let inpos = p - 1;
        let lhs = fn_pass.remove(p - 1);
        let (CatParsed::Value(lhs), i) = lhs else {
            return (
                Value::Const(Const::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: lhs.1,
                    reason: "Left side of add/sub was not value",
                })),
                false,
            );
        };
        let rhs = fn_pass.remove(p);
        let CatParsed::Value(rhs) = rhs.0 else {
            return (
                Value::Const(Const::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: rhs.1,
                    reason: "Right side of add/sub was not value",
                })),
                false,
            );
        };
        if matches!(fn_pass[inpos].0, CatParsed::Operand('+')) {
            fn_pass[inpos] = (
                CatParsed::Value((Value::Add(lhs.0.into(), rhs.0.into()), false)),
                i,
            );
        } else {
            fn_pass[inpos] = (
                CatParsed::Value((Value::Sub(lhs.0.into(), rhs.0.into()), false)),
                i,
            );
        }
    }

    while fn_pass.len() > 1 {
        let p = 1;
        let inpos = p - 1;
        let lhs = fn_pass.remove(p - 1);
        let (CatParsed::Value(lhs), i) = lhs else {
            return (
                Value::Const(Const::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: lhs.1,
                    reason: "Left side of add/sub was not value",
                })),
                false,
            );
        };
        let rhs = fn_pass.remove(p);
        let CatParsed::Value(rhs) = rhs.0 else {
            return (
                Value::Const(Const::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: rhs.1,
                    reason: "Right side of add/sub was not value",
                })),
                false,
            );
        };
        if matches!(fn_pass[inpos].0, CatParsed::Operand('<')) {
            fn_pass[inpos] = (
                CatParsed::Value((Value::Lt(lhs.0.into(), rhs.0.into()), false)),
                i,
            );
        } else {
            return (
                Value::Const(Const::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: fn_pass[inpos].1,
                    reason: "Operand is not supported",
                })),
                false,
            );
        }
    }
    let (CatParsed::Value((ret, _)), _) = fn_pass[0].clone() else {
        return (
            Value::Const(Const::Error(CellError::InvalidFormula {
                cell: None,
                cursor: fn_pass[0].1,
                reason: "Something weird happened idk",
            })),
            false,
        );
    };
    (ret, false)
}

fn is_ref(token: &str) -> Option<SLoc> {
    if !token.chars().next().is_some_and(|c| c.is_alphabetic()) {
        None
    } else {
        let mut f = false;
        let mut col = 0;
        let mut row = 0;
        for c in token.chars() {
            if f && c.is_alphabetic() {
                return None;
            }
            if !c.is_alphabetic() {
                f = true;
            }
            if !f {
                col = col * 26 + (c.to_digit(36).expect("Digit was not valid") - 9);
            } else {
                row = row * 10 + c.to_digit(10).expect("Was supposed to be digit");
            }
        }
        Some(((col - 1) as i32, (row - 1) as i32))
    }
}

fn is_const(token: &str) -> Option<Const> {
    if token.chars().next() == Some('"') && token.chars().last() == Some('"') {
        Some(Const::String(token.to_owned()))
    } else if let Ok(n) = token.parse::<f64>() {
        Some(Const::Num(n))
    } else if token == "true" || token == "false" {
        Some(Const::Bool(token == "true"))
    } else {
        None
    }
}

impl FromStr for Value {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !matches!(s.chars().next(), Some('=') | Some('[')) {
            s.parse::<Const>()
                .map_err(|_| "Constant shouldnt fail")
                .map(|v| Value::Const(v))
        } else {
            if s.chars().next() == Some('=') {
                dbg!(&s[1..]);
                let tokens = form_tokens(&s[1..]);
                dbg!(&tokens);
                Ok(form_tree(&tokens).0)
            } else {
                Ok(Self::Const(Const::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: 0,
                    reason: "Range formulas not yet supported",
                })))
            }
        }
    }
}

impl Value {
    fn eval<'a>(&'a self, cell: &SLoc, data: &Sheet) -> Cow<'a, Const> {
        // Const::String("".into())
        match self {
            Value::Const(c) => Cow::Borrowed(c),
            Value::Ref(r) => Cow::Owned(
                data.get(r)
                    .map(|d| {
                        d.val
                            .parse::<Value>()
                            .unwrap()
                            .eval(r, data)
                            .into_owned()
                            .into_pos(*r)
                    })
                    .unwrap_or(Const::Error(CellError::MissingCell { cell: Some(*r) })),
            ),
            Value::Add(lhs, rhs) => {
                let lhs_eval = lhs.eval(cell, data);
                let rhs_eval = rhs.eval(cell, data);
                if let Some(e) = lhs_eval.merge_err(&rhs_eval) {
                    Cow::Owned(Const::Error(e))
                } else if let (Const::Num(lnum), Const::Num(rnum)) =
                    (lhs_eval.as_ref(), rhs_eval.as_ref())
                {
                    Cow::Owned(Const::Num(lnum + rnum))
                } else {
                    Cow::Owned(Const::Error(CellError::BadArgument {
                        cell: Some(*cell),
                        cursor: 0,
                        expected_types: vec!["Num".into(), "Num".into()],
                    }))
                }
            }
            Value::Sub(lhs, rhs) => {
                let lhs_eval = lhs.eval(cell, data);
                let rhs_eval = rhs.eval(cell, data);
                if let Some(e) = lhs_eval.merge_err(&rhs_eval) {
                    Cow::Owned(Const::Error(e))
                } else if let (Const::Num(lnum), Const::Num(rnum)) =
                    (lhs_eval.as_ref(), rhs_eval.as_ref())
                {
                    Cow::Owned(Const::Num(lnum - rnum))
                } else {
                    Cow::Owned(Const::Error(CellError::BadArgument {
                        cell: Some(*cell),
                        cursor: 0,
                        expected_types: vec!["Num".into(), "Num".into()],
                    }))
                }
            }
            Value::Special(_) => todo!(),
            Value::Mul(lhs, rhs) => {
                let lhs_eval = lhs.eval(cell, data);
                let rhs_eval = rhs.eval(cell, data);
                if let Some(e) = lhs_eval.merge_err(&rhs_eval) {
                    Cow::Owned(Const::Error(e))
                } else if let (Const::Num(lnum), Const::Num(rnum)) =
                    (lhs_eval.as_ref(), rhs_eval.as_ref())
                {
                    Cow::Owned(Const::Num(lnum * rnum))
                } else {
                    Cow::Owned(Const::Error(CellError::BadArgument {
                        cell: Some(*cell),
                        cursor: 0,
                        expected_types: vec!["Num".into(), "Num".into()],
                    }))
                }
            }
            Value::Div(lhs, rhs) => {
                let lhs_eval = lhs.eval(cell, data);
                let rhs_eval = rhs.eval(cell, data);
                if let Some(e) = lhs_eval.merge_err(&rhs_eval) {
                    Cow::Owned(Const::Error(e))
                } else if let (Const::Num(lnum), Const::Num(rnum)) =
                    (lhs_eval.as_ref(), rhs_eval.as_ref())
                {
                    Cow::Owned(Const::Num(lnum / rnum))
                } else {
                    Cow::Owned(Const::Error(CellError::BadArgument {
                        cell: Some(*cell),
                        cursor: 0,
                        expected_types: vec!["Num".into(), "Num".into()],
                    }))
                }
            }
            Value::Lt(lhs, rhs) => {
                let lhs_eval = lhs.eval(cell, data);
                let rhs_eval = rhs.eval(cell, data);
                if let Some(e) = lhs_eval.merge_err(&rhs_eval) {
                    Cow::Owned(Const::Error(e))
                } else if let (Const::Num(lnum), Const::Num(rnum)) =
                    (lhs_eval.as_ref(), rhs_eval.as_ref())
                {
                    Cow::Owned(Const::Bool(lnum < rnum))
                } else {
                    Cow::Owned(Const::Error(CellError::BadArgument {
                        cell: Some(*cell),
                        cursor: 0,
                        expected_types: vec!["Num".into(), "Num".into()],
                    }))
                }
            }
            Value::Func { name, args } => {
                if let Some(f) = data.get_func(name) {
                    Cow::Owned(f.call(cell, args, data))
                } else {
                    Cow::Owned(Const::Error(CellError::InvalidFormula {
                        cell: Some(*cell),
                        cursor: 0,
                        reason: "Formula not found",
                    }))
                }
            }
        }
    }

    fn get_refs(&self) -> Vec<SLoc> {
        match self {
            Value::Ref(r) => vec![*r],
            Value::Add(lhs, rhs)
            | Value::Sub(lhs, rhs)
            | Value::Mul(lhs, rhs)
            | Value::Div(lhs, rhs)
            | Value::Lt(lhs, rhs) => [lhs, rhs].iter().flat_map(|s| s.get_refs()).collect(),
            Value::Func { name: _, args } => args.iter().flat_map(|a| a.get_refs()).collect(),
            Value::Const(_) | Value::Special(_) => vec![],
        }
    }
}

struct RangeFormula {
    rbound: Value,
    cbound: Value,
    value: Value,
}

pub(crate) struct CellData {
    // val: Value,
    pub(crate) val: String,
    /// Is none when recalculating
    pub(crate) display: Option<String>,
    pub(crate) evaluating: bool,
    /// Dependants, aka cells that depend on this one
    pub(crate) deps: Vec<SLoc>,
    pub(crate) rangef: Option<SLoc>,
}

impl FromStr for CellData {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let c = CellData {
            // val: Value::Const(Const::String(s.into())),
            val: s.into(),
            display: None,
            deps: vec![],
            rangef: None,
            evaluating: false,
        };

        Ok(c)
    }
}

// impl CellData {
//     fn dirty(&mut self, data: &mut Sheet) {
//         self.display = None;
//         for dep in self.deps.iter() {
//             if let Some(dcell) = data.get_mut(dep) {
//                 dcell.dirty(data);
//             }
//         }
//     }

//     fn calc(&mut self, data: &Sheet) {
//         let v: Value = self.val.parse().unwrap();
//         let c = v.eval(sheet);

//     }
// }
impl Sheet {
    pub(crate) fn dirty(&mut self, loc: &SLoc) -> Result<(), CellError> {
        println!("dirty {loc:?}");
        let to_dirty: Vec<SLoc> = if let Some(c) = self.get_mut(loc) {
            if c.evaluating {
                return Err(CellError::ReferenceLoop {
                    loop_point: Some(*loc),
                });
            }
            c.evaluating = true;
            c.display = None;
            c.deps.clone()
        } else {
            vec![]
        };
        for d in to_dirty.iter() {
            self.dirty(d);
        }
        if let Some(c) = self.get_mut(loc) {
            c.evaluating = false;
        }
        Ok(())
    }

    pub(crate) fn recompute(&mut self) {
        println!("Recompute");
        loop {
            let recpos = self
                .iter()
                .filter(|c| {
                    c.1.display.is_none()
                        && c.1
                            .deps
                            .iter()
                            .all(|d| self.get(d).map(|c| c.display.is_some()).unwrap_or(true))
                })
                .map(|(l, _)| *l)
                .next();
            if let Some(pos) = recpos {
                let s = self.get(&pos).expect("Cell was deleted after accessed");
                let mut val = s.val.parse::<Value>().unwrap();
                let deps = val.get_refs();
                for d in deps {
                    if let Some(dc) = self.get_mut(&d) {
                        dc.deps.push(pos);
                    }
                    if d == pos {
                        val = Value::Const(Const::Error(CellError::ReferenceLoop {
                            loop_point: Some(pos),
                        }));
                    }
                }
                self.dirty(&pos);
                let tmp: Option<String> = Some(val.eval(&pos, self).to_pos(pos).into());
                let sm = self.get_mut(&pos).expect("Cell was deleted after accessed");
                sm.display = tmp;
            } else if self.iter().filter(|(_, c)| c.display.is_none()).count() == 0 {
                break;
            } else {
                self.iter_mut()
                    .filter(|(_, d)| d.display == None)
                    .take(1)
                    .for_each(|(p, d)| {
                        d.display = Some(
                            Const::Error(CellError::ReferenceLoop {
                                loop_point: Some(*p),
                            })
                            .into(),
                        )
                    });
            }
        }
    }
}

pub(crate) trait Function {
    fn name(&self) -> &'static str;
    fn call(&self, cell: &SLoc, args: &[Value], sheet: &Sheet) -> Const;
}

#[derive(Default)]
pub(crate) struct If;
impl Function for If {
    fn name(&self) -> &'static str {
        "if"
    }

    fn call(&self, cell: &SLoc, args: &[Value], sheet: &Sheet) -> Const {
        let mut e = Const::Error(CellError::BadArgument {
            cell: None,
            cursor: 4,
            expected_types: vec!["bool".into(), "any".into(), "any".into()],
        });
        let [c, y, n] = &args[..] else {
            return e;
        };
        let cval = c.eval(cell, sheet);
        if matches!(cval.as_ref(), Const::Error(_)) {
            return cval.into_owned();
        }
        let Const::Bool(c) = cval.as_ref() else {
            return Const::Error(CellError::BadArgument {
                cell: None,
                cursor: 0,
                expected_types: vec!["bool".into()],
            });
        };
        if *c {
            y.eval(cell, sheet).into_owned()
        } else {
            n.eval(cell, sheet).into_owned()
        }
    }
}
