use std::{borrow::Cow, collections::HashSet, rc::Rc, str::FromStr};

use crate::{SLoc, Sheet, SheetData, SheetEval, SheetFunc};

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug)]
pub(crate) enum Spec {
    dd,
    dC,
    dR,
    dSd,
    dSC,
    dSR,
    dFd,
    dFC,
    dFR,
    dI,
}
#[derive(Clone, Debug)]
pub(crate) struct SpecValues {
    dC: i32,
    dR: i32,
    dSd: Option<Const>,
    dSC: Option<i32>,
    dSR: Option<i32>,
    dFd: Option<Const>,
    dFC: Option<i32>,
    dFR: Option<i32>,
    depth: usize,
}
impl SpecValues {
    fn to_sloc(&self) -> SLoc {
        (self.dC, self.dR)
    }
    fn with_sloc(self, sloc: SLoc) -> Self {
        Self {
            dC: sloc.0,
            dR: sloc.1,
            depth: self.depth + 1,
            dSd: None,
            dSC: None,
            dSR: None,
            dFd: None,
            dFC: None,
            dFR: None,
        }
    }
    fn with_f_sloc(self, f: Option<(SLoc, Const)>) -> Self {
        match f {
            Some(f) => Self {
                dFC: Some(f.0 .0),
                dFR: Some(f.0 .1),
                dFd: Some(f.1),
                ..self
            },
            None => Self {
                dFC: None,
                dFR: None,
                dFd: None,
                ..self
            },
        }
    }
    fn invalid_depth(&self, depth: usize) -> bool {
        self.depth != depth && depth != 0
    }
}

// These have cell references since errors propagate, and knowing origin will help
#[derive(Clone, Debug, PartialEq)]
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
    WrongNumArguments {
        cell: Option<SLoc>,
        cursor: usize,
        arguments: Vec<usize>,
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
    InvalidInfinity {
        cell: Option<SLoc>,
        cursor: usize,
    },
}

#[derive(Clone, Debug, PartialEq)]
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
    Range(SLoc, SLoc),
    Special(Spec),
    Add(Rc<Value>, Rc<Value>),
    Sub(Rc<Value>, Rc<Value>),
    Mul(Rc<Value>, Rc<Value>),
    Div(Rc<Value>, Rc<Value>),
    Lt(Rc<Value>, Rc<Value>),
    Eq(Rc<Value>, Rc<Value>),
    And(Rc<Value>, Rc<Value>),
    Func { name: String, args: Vec<Value> },
}

trait IsIdentifier {
    fn is_iden(&self) -> bool;
}
impl IsIdentifier for char {
    fn is_iden(&self) -> bool {
        self.is_alphanumeric() || self == &'.' || self == &':'
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

#[allow(dead_code)]
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
    if split_parens.len() == 0 {
        return (
            Value::Const(Const::Error(CellError::InvalidFormula {
                cell: None,
                cursor: 0,
                reason: "No formula given",
            })),
            false,
        );
    }
    if split_parens.len() == 1 {
        let sp = split_parens[0];
        return if sp[0].0 == "(" {
            (form_tree(&sp[1..sp.len() - 1]).0, true)
        } else if let Some(r) = is_ref(sp[0].0) {
            (Value::Ref(r), false)
        } else if let Some(c) = is_const(sp[0].0) {
            (Value::Const(c), false)
        } else if let Some(s) = is_spec(sp[0].0) {
            (Value::Special(s), false)
        } else if let Some((l, h)) = is_range(sp[0].0) {
            (Value::Range(l, h), false)
        } else {
            (
                Value::Const(Const::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: sp[0].1,
                    reason: "Couldn't parse token",
                })),
                false,
            )
        };
    }
    // dbg!(&split_parens);
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

    // let p2 = parseds.clone();
    // for e in p2 {
    //     dbg!(e);
    // }

    // Resolve function calls
    let mut token_orchard = vec![];
    let mut i = 0;
    while parseds.peek().is_some() {
        // let e = dbg!(parseds.next().expect("Had to be some"));
        let e = parseds.next().expect("Had to be some");
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
            // dbg!(&args);
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
            token_orchard.push((
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
            token_orchard.push((e, split_parens[i].last().unwrap().1));
        }
        i += 1;
    }
    // Parse functions
    for (i, e) in token_orchard.iter().enumerate() {
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
    // Parse mul and div
    while let Some(p) = has_mul(&token_orchard) {
        let inpos = p - 1;
        let lhs = token_orchard.remove(p - 1);
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
        let rhs = token_orchard.remove(p);
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
        if matches!(token_orchard[inpos].0, CatParsed::Operand('*')) {
            token_orchard[inpos] = (
                CatParsed::Value((Value::Mul(lhs.0.into(), rhs.0.into()), false)),
                i,
            );
        } else {
            token_orchard[inpos] = (
                CatParsed::Value((Value::Div(lhs.0.into(), rhs.0.into()), false)),
                i,
            );
        }
    }

    fn has_add(vals: &[(CatParsed, usize)]) -> Option<usize> {
        vals.iter()
            .position(|(x, _)| matches!(x, CatParsed::Operand('+' | '-')))
    }
    // Parse + and -
    while let Some(p) = has_add(&token_orchard) {
        let inpos = p - 1;
        let lhs = token_orchard.remove(p - 1);
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
        let rhs = token_orchard.remove(p);
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
        if matches!(token_orchard[inpos].0, CatParsed::Operand('+')) {
            token_orchard[inpos] = (
                CatParsed::Value((Value::Add(lhs.0.into(), rhs.0.into()), false)),
                i,
            );
        } else {
            token_orchard[inpos] = (
                CatParsed::Value((Value::Sub(lhs.0.into(), rhs.0.into()), false)),
                i,
            );
        }
    }

    fn has_comp(vals: &[(CatParsed, usize)]) -> Option<usize> {
        vals.iter()
            .position(|(x, _)| matches!(x, CatParsed::Operand('<' | '=')))
    }
    // Parse < and =
    while let Some(p) = has_comp(&token_orchard) {
        // let p = 1;
        let inpos = p - 1;
        let lhs = token_orchard.remove(p - 1);
        let (CatParsed::Value(lhs), i) = lhs else {
            return (
                Value::Const(Const::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: lhs.1,
                    reason: "Left side of equality/inequality was not value",
                })),
                false,
            );
        };
        let rhs = token_orchard.remove(p);
        let CatParsed::Value(rhs) = rhs.0 else {
            return (
                Value::Const(Const::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: rhs.1,
                    reason: "Right side of equality/inequality was not value",
                })),
                false,
            );
        };
        if matches!(token_orchard[inpos].0, CatParsed::Operand('<')) {
            token_orchard[inpos] = (
                CatParsed::Value((Value::Lt(lhs.0.into(), rhs.0.into()), false)),
                i,
            );
        } else if matches!(token_orchard[inpos].0, CatParsed::Operand('=')) {
            token_orchard[inpos] = (
                CatParsed::Value((Value::Eq(lhs.0.into(), rhs.0.into()), false)),
                i,
            );
        } else {
            return (
                Value::Const(Const::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: token_orchard[inpos].1,
                    reason: "Operand is not supported",
                })),
                false,
            );
        }
    }

    fn has_logic(vals: &[(CatParsed, usize)]) -> Option<usize> {
        vals.iter()
            .position(|(x, _)| matches!(x, CatParsed::Operand('&')))
    }
    // Parse &
    while let Some(p) = has_logic(&token_orchard) {
        // let p = 1;
        let inpos = p - 1;
        let lhs = token_orchard.remove(p - 1);
        let (CatParsed::Value(lhs), i) = lhs else {
            return (
                Value::Const(Const::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: lhs.1,
                    reason: "Left side of logic was not value",
                })),
                false,
            );
        };
        let rhs = token_orchard.remove(p);
        let CatParsed::Value(rhs) = rhs.0 else {
            return (
                Value::Const(Const::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: rhs.1,
                    reason: "Right side of logic was not value",
                })),
                false,
            );
        };
        if matches!(token_orchard[inpos].0, CatParsed::Operand('&')) {
            token_orchard[inpos] = (
                CatParsed::Value((Value::And(lhs.0.into(), rhs.0.into()), false)),
                i,
            );
        } else {
            return (
                Value::Const(Const::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: token_orchard[inpos].1,
                    reason: "Operand is not supported",
                })),
                false,
            );
        }
    }

    let (CatParsed::Value((ret, _)), _) = token_orchard[0].clone() else {
        return (
            Value::Const(Const::Error(CellError::InvalidFormula {
                cell: None,
                cursor: token_orchard[0].1,
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
                col = col * 26 + (c.to_digit(36)? - 9);
            } else {
                row = row * 10 + c.to_digit(10)?;
            }
        }
        if col < 1 || row < 1 {
            None
        } else {
            Some(((col - 1) as i32, (row - 1) as i32))
        }
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
fn is_range(token: &str) -> Option<(SLoc, SLoc)> {
    let mut split = token.split(":");
    let start = is_ref(split.next()?)?;
    let end = is_ref(split.next()?)?;
    if split.next().is_some() {
        return None;
    }
    Some((start, end))
}
fn is_spec(token: &str) -> Option<Spec> {
    match token {
        ".R" => Some(Spec::dR),
        ".C" => Some(Spec::dC),
        ".SR" => Some(Spec::dSR),
        ".SC" => Some(Spec::dSC),
        ".S." => Some(Spec::dSd),
        ".FR" => Some(Spec::dFR),
        ".FC" => Some(Spec::dFC),
        ".I" => Some(Spec::dI),
        ".F." => Some(Spec::dFd),
        _ => None,
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
                // dbg!(&s[1..]);
                let tokens = form_tokens(&s[1..]);
                // dbg!(&tokens);
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
    /// Whenever the `spec` variable changes in a recursive call, depth should be incremented
    fn eval<'a>(
        &'a self,
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        spec: &'a SpecValues,
    ) -> Cow<'a, Const> {
        // Const::String("".into())
        let l = (spec.dC, spec.dR);
        if eval.get(&l).is_some_and(|x| spec.invalid_depth(*x)) {
            // dbg!(spec, eval.get(&l));
            eval.get_mut(&l).map(|x| *x = 0);
            return Cow::Owned(Const::Error(CellError::ReferenceLoop {
                loop_point: Some(l),
            }));
        }
        eval.get_mut(&l).map(|x| *x = spec.depth);
        let t = match self {
            Value::Const(c) => Cow::Borrowed(c),
            Value::Ref(r) => Cow::Owned({
                let vr = data
                    .get(r, &eval)
                    .map(|d| {
                        let new_spec = spec.clone().with_sloc(*r);
                        if eval.get(r).is_some_and(|x| spec.invalid_depth(*x)) {
                            Const::Error(CellError::ReferenceLoop {
                                loop_point: Some(*r),
                            })
                        } else {
                            d.val
                                .parse::<Value>()
                                .unwrap()
                                .eval(data, eval, func, &new_spec)
                                .into_owned()
                                .into_pos(*r)
                        }
                    })
                    .map_err(|e| Const::Error(e))
                    .unwrap_or_else(|e| e);
                vr
            }),
            Value::Range(_, _) => Cow::Owned(Const::Error(CellError::InvalidFormula {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                reason: "Range cannot be assigned to a cell",
            })),
            Value::Add(lhs, rhs) => {
                let lhs_eval = lhs.eval(data, eval, func, spec);
                let rhs_eval = rhs.eval(data, eval, func, spec);
                if let Some(e) = lhs_eval.merge_err(&rhs_eval) {
                    Cow::Owned(Const::Error(e))
                } else if let (Const::Num(lnum), Const::Num(rnum)) =
                    (lhs_eval.as_ref(), rhs_eval.as_ref())
                {
                    Cow::Owned(Const::Num(lnum + rnum))
                } else {
                    Cow::Owned(Const::Error(CellError::BadArgument {
                        cell: Some(spec.to_sloc()),
                        cursor: 0,
                        expected_types: vec!["Num".into(), "Num".into()],
                    }))
                }
            }
            Value::Sub(lhs, rhs) => {
                let lhs_eval = lhs.eval(data, eval, func, spec);
                let rhs_eval = rhs.eval(data, eval, func, spec);
                if let Some(e) = lhs_eval.merge_err(&rhs_eval) {
                    Cow::Owned(Const::Error(e))
                } else if let (Const::Num(lnum), Const::Num(rnum)) =
                    (lhs_eval.as_ref(), rhs_eval.as_ref())
                {
                    Cow::Owned(Const::Num(lnum - rnum))
                } else {
                    Cow::Owned(Const::Error(CellError::BadArgument {
                        cell: Some(spec.to_sloc()),
                        cursor: 0,
                        expected_types: vec!["Num".into(), "Num".into()],
                    }))
                }
            }
            Value::Special(s) => {
                match s {
                    Spec::dd => Cow::Owned(Const::Error(CellError::ReferenceLoop {
                        loop_point: Some(spec.to_sloc()),
                    })),
                    Spec::dC => Cow::Owned(Const::Num(spec.dC as f64)),
                    Spec::dR => Cow::Owned(Const::Num(spec.dR as f64)),
                    Spec::dSd => {
                        if let Some(ref v) = spec.dSd {
                            Cow::Borrowed(v)
                        } else {
                            Cow::Owned(Const::Error(CellError::InvalidFormula { cell: Some(spec.to_sloc()), cursor: 0, reason: "S and F are only valid in range formulas and functions respecively." }))
                        }
                    }
                    Spec::dSC => Cow::Owned(if let Some(v) = spec.dSC {
                        Const::Num(v as f64)
                    } else {
                        Const::Error(CellError::InvalidFormula { cell: Some(spec.to_sloc()), cursor: 0, reason: "S and F are only valid in range formulas and functions respecively." })
                    }),
                    Spec::dSR => Cow::Owned(if let Some(v) = spec.dSR {
                        Const::Num(v as f64)
                    } else {
                        Const::Error(CellError::InvalidFormula { cell: Some(spec.to_sloc()), cursor: 0, reason: "S and F are only valid in range formulas and functions respecively." })
                    }),
                    Spec::dFd => {
                        if let Some(ref v) = spec.dFd {
                            Cow::Borrowed(v)
                        } else {
                            Cow::Owned(Const::Error(CellError::InvalidFormula { cell: Some(spec.to_sloc()), cursor: 0, reason: "S and F are only valid in range formulas and functions respecively." }))
                        }
                    }
                    Spec::dFC => Cow::Owned(if let Some(v) = spec.dFC {
                        Const::Num(v as f64)
                    } else {
                        Const::Error(CellError::InvalidFormula { cell: Some(spec.to_sloc()), cursor: 0, reason: "S and F are only valid in range formulas and functions respecively." })
                    }),
                    Spec::dFR => Cow::Owned(if let Some(v) = spec.dFR {
                        Const::Num(v as f64)
                    } else {
                        Const::Error(CellError::InvalidFormula { cell: Some(spec.to_sloc()), cursor: 0, reason: "S and F are only valid in range formulas and functions respecively." })
                    }),
                    Spec::dI => Cow::Owned(Const::Error(CellError::InvalidInfinity {
                        cell: Some(spec.to_sloc()),
                        cursor: 0,
                    })),
                }
            }
            Value::Mul(lhs, rhs) => {
                let lhs_eval = lhs.eval(data, eval, func, spec);
                let rhs_eval = rhs.eval(data, eval, func, spec);
                if let Some(e) = lhs_eval.merge_err(&rhs_eval) {
                    Cow::Owned(Const::Error(e))
                } else if let (Const::Num(lnum), Const::Num(rnum)) =
                    (lhs_eval.as_ref(), rhs_eval.as_ref())
                {
                    Cow::Owned(Const::Num(lnum * rnum))
                } else {
                    Cow::Owned(Const::Error(CellError::BadArgument {
                        cell: Some(spec.to_sloc()),
                        cursor: 0,
                        expected_types: vec!["Num".into(), "Num".into()],
                    }))
                }
            }
            Value::Div(lhs, rhs) => {
                let lhs_eval = lhs.eval(data, eval, func, spec);
                let rhs_eval = rhs.eval(data, eval, func, spec);
                if let Some(e) = lhs_eval.merge_err(&rhs_eval) {
                    Cow::Owned(Const::Error(e))
                } else if let (Const::Num(lnum), Const::Num(rnum)) =
                    (lhs_eval.as_ref(), rhs_eval.as_ref())
                {
                    Cow::Owned(Const::Num(lnum / rnum))
                } else {
                    Cow::Owned(Const::Error(CellError::BadArgument {
                        cell: Some(spec.to_sloc()),
                        cursor: 0,
                        expected_types: vec!["Num".into(), "Num".into()],
                    }))
                }
            }
            Value::Lt(lhs, rhs) => {
                let lhs_eval = lhs.eval(data, eval, func, spec);
                let rhs_eval = rhs.eval(data, eval, func, spec);
                if let Some(e) = lhs_eval.merge_err(&rhs_eval) {
                    Cow::Owned(Const::Error(e))
                } else if let (Const::Num(lnum), Const::Num(rnum)) =
                    (lhs_eval.as_ref(), rhs_eval.as_ref())
                {
                    Cow::Owned(Const::Bool(lnum < rnum))
                } else {
                    Cow::Owned(Const::Error(CellError::BadArgument {
                        cell: Some(spec.to_sloc()),
                        cursor: 0,
                        expected_types: vec!["Num".into(), "Num".into()],
                    }))
                }
            }
            Value::Func { name, args } => {
                if let Some(f) = func.get_func(name) {
                    Cow::Owned(f.call(args, data, eval, func, spec))
                } else {
                    Cow::Owned(Const::Error(CellError::InvalidFormula {
                        cell: Some(spec.to_sloc()),
                        cursor: 0,
                        reason: "Formula not found",
                    }))
                }
            }
            Value::Eq(lhs, rhs) => {
                let lhs_eval = lhs.eval(data, eval, func, spec);
                let rhs_eval = rhs.eval(data, eval, func, spec);
                if let Some(e) = lhs_eval.merge_err(&rhs_eval) {
                    Cow::Owned(Const::Error(e))
                } else {
                    Cow::Owned(Const::Bool(lhs_eval == rhs_eval))
                }
            }
            Value::And(lhs, rhs) => {
                let lhs_eval = lhs.eval(data, eval, func, spec);
                let rhs_eval = rhs.eval(data, eval, func, spec);
                if let Some(e) = lhs_eval.merge_err(&rhs_eval) {
                    Cow::Owned(Const::Error(e))
                } else if let (Const::Bool(lnum), Const::Bool(rnum)) =
                    (lhs_eval.as_ref(), rhs_eval.as_ref())
                {
                    Cow::Owned(Const::Bool(*lnum && *rnum))
                } else {
                    Cow::Owned(Const::Error(CellError::BadArgument {
                        cell: Some(spec.to_sloc()),
                        cursor: 0,
                        expected_types: vec!["Num".into(), "Num".into()],
                    }))
                }
            }
        };
        eval.get_mut(&l).map(|x| *x = 0);
        t
    }

    fn get_refs(&self) -> Vec<SLoc> {
        match self {
            Value::Ref(r) => vec![*r],
            Value::Add(lhs, rhs)
            | Value::Sub(lhs, rhs)
            | Value::Mul(lhs, rhs)
            | Value::Div(lhs, rhs)
            | Value::Eq(lhs, rhs)
            | Value::And(lhs, rhs)
            | Value::Lt(lhs, rhs) => [lhs, rhs].iter().flat_map(|s| s.get_refs()).collect(),
            Value::Func { name: _, args } => args.iter().flat_map(|a| a.get_refs()).collect(),
            Value::Const(_) | Value::Special(_) => vec![],
            Value::Range(l, h) => RangeIter(*l, *h, 0).collect(),
        }
    }
}

struct RangeFormula {
    rbound: Value,
    cbound: Value,
    conditon: Value,
    value: Value,
}

#[derive(Debug, Default)]
pub(crate) struct CellData {
    // val: Value,
    pub(crate) val: String,
    /// Is none when recalculating
    pub(crate) parsed: Option<Value>,
    pub(crate) display: Option<String>,
    // Flag for detecting loops
    /// Dependants, aka cells that depend on this one
    pub(crate) dependants: HashSet<SLoc>,
    pub(crate) rangef: Option<SLoc>,
}

impl FromStr for CellData {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let c = CellData {
            // val: Value::Const(Const::String(s.into())),
            val: s.into(),
            display: None,
            dependants: HashSet::new(),
            rangef: None,
            parsed: None,
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
    fn set_eval(&mut self, cell: &SLoc, eval: usize) {
        if let Some(c) = self.1.get_mut(cell) {
            *c = eval;
        }
    }
    fn get_eval(&self, cell: &SLoc) -> usize {
        if let Some(c) = self.1.get(cell) {
            *c
        } else {
            0
        }
    }
    fn set_disp_err(&mut self, cell: &SLoc, err: CellError) {
        if let Some(c) = self.0 .0.get_mut(cell) {
            c.display = Some(Const::Error(err).into())
        }
    }
    fn set_disp(&mut self, cell: &SLoc, v: Option<String>) {
        if let Some(c) = self.0 .0.get_mut(cell) {
            c.display = v
        }
    }
    // TODO `evaluating` shouldnt be here, it doesnt work
    pub(crate) fn dirty(&mut self, loc: &SLoc) {
        println!("dirty {loc:?}");
        let c = self.get_mut(loc);
        let to_dirty: HashSet<SLoc> = if let Ok(c) = c {
            if c.display == None {
                return;
            }
            c.display = None;
            c.dependants.clone()
        } else if let Err(CellError::ReferenceLoop { loop_point: _ }) = c {
            return;
        } else {
            HashSet::new()
        };
        for d in to_dirty.iter() {
            self.dirty(d);
        }
    }

    pub(crate) fn recompute(&mut self) {
        println!("Recompute");
        let kc: Vec<_> = self.keys().cloned().collect();
        for pos in kc {
            // Remove dependants that dont depend
            let s = self.get(&pos);
            let mut torem = HashSet::new();
            if let Ok(s) = s {
                for p in s.dependants.iter() {
                    if let Ok(deps) = self.get(p) {
                        if let Ok(pv) = deps.val.parse::<Value>() {
                            if !pv.get_refs().contains(&pos) {
                                torem.insert(*p);
                            }
                        }
                    }
                }
            }
            // for pr in torem {
            //     let Ok(s) = self.get_mut(&pr) else {
            //         continue;
            //     };
            //     s.dependants.remove(&pos);
            // }
            if let Ok(s) = self.get_mut(&pos) {
                for pr in torem {
                    s.dependants.remove(&pr);
                }
            }
        }
        loop {
            let recpos = self
                .iter()
                .filter(|c| {
                    c.1.display.is_none()
                        && c.1
                            .dependants
                            .iter()
                            .all(|d| self.get(d).map(|c| c.display.is_some()).unwrap_or(true))
                })
                .map(|(l, _)| *l)
                .next();
            if let Some(pos) = recpos {
                self.dirty(&pos);

                let s = self.get(&pos);
                // dbg!(&s);
                // This will return error if a reference loop is reached.
                // This needs to continue to properly do reference loop detection and proper text updating, so I cant early return/continue
                let mut val = if let Ok(s) = s {
                    let val = s.val.clone();
                    val.parse::<Value>().unwrap()
                } else {
                    Value::Const(Const::Error(s.unwrap_err()))
                };

                // Add depends
                let depends = val.get_refs();
                // dbg!(&depends);
                for d in depends {
                    if let Ok(dc) = self.get_mut(&d) {
                        dc.dependants.insert(pos);
                    } else {
                        let mut cd = CellData::default();
                        cd.dependants.insert(pos);
                        let _ = self.insert(d, cd);
                    }
                    if d == pos {
                        val = Value::Const(Const::Error(CellError::ReferenceLoop {
                            loop_point: Some(pos),
                        }));
                    }
                }

                let spec = SpecValues {
                    dC: pos.0,
                    dR: pos.1,
                    dSd: None,
                    dSC: None,
                    dSR: None,
                    dFd: None,
                    dFC: None,
                    dFR: None,
                    depth: 0,
                };
                let tmp: Option<String> = Some(
                    val.eval(&self.0, &mut self.1, &self.2, &spec)
                        .to_pos(pos)
                        .into(),
                );
                // dbg!(&tmp);
                // let sm = self.get_mut(&pos).expect("Cell was deleted after accessed");
                // sm.display = tmp;
                self.set_disp(&pos, tmp);
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
    // Must be lowercase
    fn name(&self) -> &'static str;
    fn call(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        spec: &SpecValues,
    ) -> Const;
}

#[derive(Default)]
pub(crate) struct If;
impl Function for If {
    fn name(&self) -> &'static str {
        "if"
    }

    fn call(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        spec: &SpecValues,
    ) -> Const {
        let e = Const::Error(CellError::BadArgument {
            cell: None,
            cursor: 4,
            expected_types: vec!["bool".into(), "any".into(), "any".into()],
        });
        let [c, y, n] = &args[..] else {
            return e;
        };
        let cval = c.eval(data, eval, func, spec);
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
            y.eval(data, eval, func, spec).into_owned()
        } else {
            n.eval(data, eval, func, spec).into_owned()
        }
    }
}

struct RangeIter(SLoc, SLoc, usize);
impl Iterator for RangeIter {
    type Item = SLoc;

    fn next(&mut self) -> Option<Self::Item> {
        let height = self.1 .0 - self.0 .0 + 1;
        let width = self.1 .1 - self.0 .1 + 1;
        if self.2 == (width * height) as usize {
            None
        } else {
            let t = Some((self.2 as i32 % height, self.2 as i32 / height));
            self.2 += 1;
            t
        }
    }
}

#[derive(Default)]
pub(crate) struct Sum;
impl Function for Sum {
    fn name(&self) -> &'static str {
        "sum"
    }

    fn call(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        spec: &SpecValues,
    ) -> Const {
        let mut s = 0.0;
        println!("Sum args");
        // dbg!(&args);
        let e = Const::Error(CellError::BadArgument {
            cell: Some(spec.to_sloc()),
            cursor: 0,
            expected_types: vec!["Cell or range...".into()],
        });
        for a in args {
            if let Value::Range(low, high) = a {
                // dbg!(data);
                for elem in RangeIter(*low, *high, 0) {
                    // dbg!(&elem);
                    let v = (&data).get(&elem, &eval);
                    let v = match v {
                        Ok(v) => v,
                        Err(e) => match e {
                            CellError::MissingCell { cell: _ } => continue,
                            _ => return Const::Error(e),
                        },
                    };
                    // dbg!(v);
                    let Ok(v) = v.val.parse::<Value>() else {
                        return Const::Error(CellError::MissingCell { cell: Some(elem) });
                    };
                    let new_spec = spec.clone().with_sloc(elem);
                    // dbg!(&new_spec);
                    let cv = v.eval(data, eval, func, &new_spec);
                    if let Const::Num(n) = cv.as_ref() {
                        s += n;
                    } else if let Const::Error(err) = cv.as_ref() {
                        return Const::Error(err.clone());
                    }
                }
            } else {
                let c = a.eval(data, eval, func, spec);
                if let Const::Num(n) = c.as_ref() {
                    s += n;
                } else {
                    return e;
                }
            }
        }
        Const::Num(s)
    }
}
#[derive(Default)]
pub(crate) struct ValueFunc;
impl Function for ValueFunc {
    fn name(&self) -> &'static str {
        "value"
    }

    fn call(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        spec: &SpecValues,
    ) -> Const {
        let [c, r] = args else {
            return Const::Error(CellError::WrongNumArguments {
                cell: Some((spec.dC, spec.dR)),
                cursor: 0,
                arguments: vec![2],
            });
        };
        let c = c.eval(data, eval, func, spec);
        let Const::Num(c) = c.as_ref() else {
            if let Const::Error(_) = c.as_ref() {
                return c.into_owned();
            } else {
                return Const::Error(CellError::BadArgument {
                    cell: Some((spec.dC, spec.dR)),
                    cursor: 0,
                    expected_types: vec!["Number".into()],
                });
            }
        };
        let r = r.eval(data, eval, func, spec);
        let Const::Num(r) = r.as_ref() else {
            if let Const::Error(_) = r.as_ref() {
                return r.into_owned();
            } else {
                return Const::Error(CellError::BadArgument {
                    cell: Some((spec.dC, spec.dR)),
                    cursor: 1,
                    expected_types: vec!["Number".into()],
                });
            }
        };
        // TODO make this more rigid, find good way to do a let else
        if c < &0. || c != &c.round() || r < &0. || &r.round() != r {
            return Const::Error(CellError::BadArgument {
                cell: Some((spec.dC, spec.dR)),
                cursor: 0,
                expected_types: vec!["whole number".into()],
            });
        }
        let c = *c as i32;
        let r = *r as i32;

        let rf = data.get(&(c, r), &eval);
        // TODO add dependency here
        if let Err(e) = rf {
            return Const::Error(e);
        }
        let rf = rf.expect("Already handled err case");
        let val = match rf.val.parse::<Value>() {
            Ok(v) => v,
            Err(e) => {
                return Const::Error(CellError::InvalidFormula {
                    cell: Some((c, r)),
                    cursor: 0,
                    reason: e,
                })
            }
        };
        let new_spec = spec.clone().with_sloc((c, r));

        val.eval(data, eval, func, &new_spec).into_owned()
    }
}

#[derive(Default)]
pub(crate) struct CountIf;
impl Function for CountIf {
    fn name(&self) -> &'static str {
        "countif"
    }

    fn call(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        spec: &SpecValues,
    ) -> Const {
        let mut count = 0;
        let [range, cond] = args else {
            return Const::Error(CellError::WrongNumArguments {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                arguments: vec![2],
            });
        };
        let Value::Range(rl, rh) = range else {
            return Const::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Range".into()],
            });
        };
        dbg!(cond);
        for cell in RangeIter(*rl, *rh, 0) {
            let Ok(celldata) = data.get(&cell, eval) else {
                continue;
            };
            let inner_spec = spec.clone().with_sloc(cell);
            let val = celldata.val.parse::<Value>();
            if let Err(val) = val {
                return Const::Error(CellError::InvalidFormula {
                    cell: Some(inner_spec.to_sloc()),
                    cursor: 0,
                    reason: val,
                });
            }
            let val = val.unwrap();
            let cval = val.eval(data, eval, func, &inner_spec).into_owned();

            let fspec = spec.clone().with_f_sloc(Some((cell, cval)));
            // dbg!(spec, &fspec);

            if let Const::Bool(true) = dbg!(cond.eval(data, eval, func, &fspec).as_ref()) {
                count += 1;
            };
        }
        Const::Num(count as f64)
    }
}
