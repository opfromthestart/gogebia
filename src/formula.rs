use std::{borrow::Cow, collections::HashSet, rc::Rc, str::FromStr};

use crate::{SLoc, Sheet, SheetData, SheetEval, SheetFunc};

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug, PartialEq)]
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
    dH(u8, u8, u8),
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
    fn from_sloc(sloc: SLoc) -> Self {
        Self {
            dC: sloc.0,
            dR: sloc.1,
            dSd: None,
            dSC: None,
            dSR: None,
            dFd: None,
            dFC: None,
            dFR: None,
            depth: 0,
        }
    }
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
pub(crate) struct SLocBound(Bound, Bound);

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Range(SLoc, SLocBound);
impl From<&SLoc> for Range {
    fn from(value: &SLoc) -> Self {
        Self(
            value.clone(),
            SLocBound(Bound::Fin(value.0), Bound::Fin(value.1)),
        )
    }
}
impl Range {
    fn range(cell1: SLoc, cell2: SLoc) -> Self {
        Self(cell1, SLocBound(Bound::Fin(cell2.0), Bound::Fin(cell2.1)))
    }
    fn range_inf(cell: SLoc) -> Self {
        Self(cell, SLocBound(Bound::Inf, Bound::Inf))
    }
    fn in_range(&self, cell: &SLoc) -> bool {
        if cell.0 < self.0 .0 || cell.1 < self.0 .1 {
            false
        } else {
            self.1 .0.ge(cell.0) && self.1 .1.ge(cell.1)
        }
    }
    fn into_inf(self) -> Self {
        Self(self.0, SLocBound(Bound::Inf, Bound::Inf))
    }
}
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Const {
    Num(f64),
    String(String),
    Bool(bool),
    Error(CellError),
    Range(Range),
    Color(u8, u8, u8),
}

impl Into<String> for &Const {
    fn into(self) -> String {
        match self {
            Const::Num(n) => format!("{n}"),
            Const::String(s) => s.clone(),
            Const::Bool(b) => format!("{b}"),
            Const::Error(e) => format!("{e:?}"),
            Const::Range(_) => format!("Range is not valid here"),
            Const::Color(r, g, b) => format!("Color #{r},{g},{b}"),
        }
    }
}
impl Into<String> for Const {
    fn into(self) -> String {
        (&self).into()
    }
}
impl FromStr for Const {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        if let Ok(b) = s.parse() {
            Ok(Self::Bool(b))
        } else if let Ok(n) = s.parse() {
            Ok(Self::Num(n))
        } else if s.len() > 1
            && s.chars().next().is_some_and(|c| c == '"')
            && s.chars().last().is_some_and(|c| c == '"')
        {
            Ok(Self::String(s[1..s.len() - 1].to_owned()))
        } else {
            Err(())
        }
    }
}
impl TryInto<Bound> for &Const {
    type Error = Const;

    fn try_into(self) -> Result<Bound, <Self as TryInto<Bound>>::Error> {
        match self {
            Const::Error(CellError::InvalidInfinity { cell: _, cursor: _ }) => Ok(Bound::Inf),
            Const::Num(n) if n >= &0. && &n.round() == n => Ok(Bound::Fin(*n as i32)),
            _ => Err(Const::Error(CellError::BadArgument {
                cell: None,
                cursor: 0,
                expected_types: vec!["Whole number".into(), ".I".into()],
            })),
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

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Value {
    Const(Const),
    Ref(SLoc),
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

#[derive(Debug, Clone, PartialEq)]
enum ParenTree<'a> {
    Paren(Vec<Box<ParenTree<'a>>>),
    Token(&'a str, usize),
}

impl<'a> ParenTree<'a> {
    fn len(&self) -> usize {
        match self {
            ParenTree::Paren(p) => p.iter().map(|p| p.len()).sum(),
            ParenTree::Token(_, _) => 1,
        }
    }

    fn is_func(&self) -> bool {
        match self {
            ParenTree::Paren(_) => false,
            ParenTree::Token(t, _) => t.chars().all(char::is_alphabetic),
        }
    }
    fn is_op(&self) -> bool {
        match self {
            ParenTree::Paren(_) => false,
            ParenTree::Token(t, _) => t.chars().all(|c| !c.is_alphanumeric()),
        }
    }
    fn is_comma(&self) -> bool {
        // dbg!(self);
        match self {
            ParenTree::Token(",", _) => true,
            _ => false,
        }
    }
    fn is_mul_args(&self) -> bool {
        match self {
            ParenTree::Paren(p) => p.iter().skip(1).any(|t| t.is_comma()),
            ParenTree::Token(_, _) => false,
        }
    }
    fn iter_flat(&self) -> impl Iterator<Item = ParenTree<'a>> {
        match self {
            ParenTree::Paren(p) => p
                .iter()
                .flat_map(|p| p.iter_flat())
                .collect::<Vec<_>>()
                .into_iter(),
            ParenTree::Token(_, _) => vec![self.clone()].into_iter(),
        }
    }
    fn iter(&self) -> impl Iterator<Item = ParenTree<'a>> {
        match self {
            ParenTree::Paren(p) => p
                .iter()
                .map(|x| x.as_ref().clone())
                .collect::<Vec<_>>()
                .into_iter(),
            ParenTree::Token(_, _) => vec![self.clone()].into_iter(),
        }
    }
    fn as_op(&self) -> Option<char> {
        match self {
            ParenTree::Paren(_) => None,
            ParenTree::Token(t, _) => {
                if self.is_op() {
                    t.chars().next()
                } else {
                    None
                }
            }
        }
    }
    fn as_func(&self) -> Option<&'a str> {
        match self {
            ParenTree::Paren(_) => None,
            ParenTree::Token(t, _) => {
                if self.is_func() {
                    Some(t)
                } else {
                    None
                }
            }
        }
    }
    fn get(&self, i: usize) -> Option<(&'a str, usize)> {
        match self {
            ParenTree::Paren(p) => p.get(i).and_then(|p| p.get(0)),
            ParenTree::Token(t, id) => {
                if i == 0 {
                    Some((t, *id))
                } else {
                    None
                }
            }
        }
    }
}
// enum ParenIter<'a> {
//     Token(std::vec::IntoIter<ParenTree<'a>>),
// }
// impl<'a> Iterator for ParenIter<'a> {
//     type Item = ParenTree<'a>;

//     fn next(&mut self) -> Option<Self::Item> {
//         match self {
//             ParenIter::Token(t) => t.next(),
//         }
//     }
// }

fn split_parens<'a, 'b>(tokens: &'b [(&'a str, usize)]) -> Result<ParenTree<'a>, Value> {
    let mut split_par_vec = vec![];
    let mut pcount = 0;
    let mut start = 0;
    println!("{tokens:?} tokens");
    if tokens.len() == 1 {
        Ok(ParenTree::Token(tokens[0].0, tokens[0].1))
    } else {
        let mut pstart = None;
        for (i, t) in tokens.iter().enumerate() {
            if t.0 == "(" {
                if pcount == 0 {
                    pstart = Some(i);
                }
                pcount += 1;
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
                if let Some(ps) = pstart {
                    split_par_vec.push(Box::new(split_parens(&tokens[ps + 1..i])?));
                    pstart = None;
                } else {
                    split_par_vec.push(Box::new(split_parens(&tokens[start..=i])?));
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
            Ok(ParenTree::Paren(split_par_vec))
        }
    }
}

// TODO functions with no arguments
// TODO parse ranges of cells
fn form_tree<'a>(split_par: &ParenTree<'a>) -> (Value, bool) {
    if split_par.len() == 0 {
        return (
            Value::Const(Const::Error(CellError::InvalidFormula {
                cell: None,
                cursor: 0,
                reason: "No formula given",
            })),
            false,
        );
    }
    dbg!(&split_par);
    if let ParenTree::Token(sp, i) = split_par {
        return if let Some(r) = is_ref(sp) {
            (Value::Ref(r), false)
        } else if let Some(c) = is_const(sp) {
            (Value::Const(c), false)
        } else if let Some(s) = is_spec(sp) {
            (Value::Special(s), false)
        } else if let Some(r) = is_range(sp) {
            (Value::Const(Const::Range(r)), false)
        } else {
            dbg!(sp);
            (
                Value::Const(Const::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: *i,
                    reason: "Couldn't parse token",
                })),
                false,
            )
        };
    }
    let ParenTree::Paren(split_v) = &split_par else {
        panic!("Other case already checked");
    };
    // dbg!(&split_v);
    enum Category {
        Value,
        Operand,
        Function,
    }

    let cats = split_v.iter().map(|s| {
        if s.is_func() {
            Category::Function
        } else if s.is_op() {
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

    let mut parseds = split_v
        .iter()
        .zip(cats)
        .map(|(v, c)| match c {
            Category::Value => {
                dbg!(&v);
                if v.is_mul_args() {
                    // TODO dont do whatever it is here, need to fix split_parens
                    let vargs: Vec<_> = dbg!(v.iter().collect());

                    CatParsed::Args(
                        vargs
                            .split(|varg| varg.is_comma())
                            .map(|varg| {
                                dbg!(form_tree(&ParenTree::Paren(
                                    dbg!(varg).iter().map(|x| Box::new(x.clone())).collect(),
                                )))
                                .0
                            })
                            .collect(),
                    )
                } else {
                    println!("As val");
                    CatParsed::Value(form_tree(&v))
                }
            }
            Category::Operand => CatParsed::Operand(v.as_op().unwrap()),
            Category::Function => CatParsed::Function(v.as_func().unwrap()),
        })
        .peekable();

    let p2 = parseds.clone();
    println!("Parseds:");
    for e in p2 {
        dbg!(e);
    }
    println!("Parsed End");
    // dbg!(&parseds);

    // Resolve function calls
    let mut token_orchard = vec![];
    let mut i = 0;
    while parseds.peek().is_some() {
        // let e = dbg!(parseds.next().expect("Had to be some"));
        let e = parseds.next().expect("Had to be some");
        if let CatParsed::Function(name) = e {
            let lp = split_par.get(i).unwrap();
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
            let lp = split_par.get(i).unwrap();
            return (
                Value::Const(Const::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: lp.1,
                    reason: "Arguments with no function",
                })),
                false,
            );
        } else {
            token_orchard.push((e, split_par.get(i).unwrap().1));
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

fn letters_to_row(token: &str) -> Option<i32> {
    let mut s = 0;
    for c in token.chars() {
        s = s * 26 + (c.to_digit(36)? - 9);
    }
    Some((s - 1) as i32)
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

pub(crate) fn show_ref(loc: &SLoc) -> String {
    format!("{}{}", show_col(loc.0), loc.1 + 1)
}

pub(crate) fn show_col(x: i32) -> String {
    let mut c = String::new();
    let mut cn = x + 1;
    while cn > 0 {
        c.insert(0, (('A' as u8) + ((cn - 1) % 26) as u8) as char);
        cn = (cn - 1) / 26;
    }
    c
}

fn is_const(token: &str) -> Option<Const> {
    Const::from_str(token).ok()
}
fn is_range(token: &str) -> Option<Range> {
    let mut split = token.split(":");
    let start = is_ref(split.next()?)?;
    if let Some(nx) = split.next() {
        if &nx == &"" {
            Some(Range::range_inf(start))
        } else if let Ok(n) = nx.parse::<i32>() {
            println!("{n} {start:?}");
            if n - 1 < start.1 {
                None
            } else {
                Some(Range(start, SLocBound(Bound::Inf, Bound::Fin(n - 1))))
            }
        } else if nx.chars().all(char::is_alphabetic) {
            let c = letters_to_row(nx)?;
            println!("{c} {start:?}");
            if c < start.0 {
                None
            } else {
                Some(Range(start, SLocBound(Bound::Fin(c), Bound::Inf)))
            }
        } else {
            let end = is_ref(nx)?;
            if split.next().is_some() || end.0 < start.0 || end.1 < start.1 {
                None
            } else {
                Some(Range::range(start, end))
            }
        }
    } else {
        None
    }
}
fn is_spec(token: &str) -> Option<Spec> {
    match token {
        // ".." => Some(Spec::dd),
        ".R" => Some(Spec::dR),
        ".C" => Some(Spec::dC),
        ".SR" => Some(Spec::dSR),
        ".SC" => Some(Spec::dSC),
        // ".S." => Some(Spec::dSd),
        ".FR" => Some(Spec::dFR),
        ".FC" => Some(Spec::dFC),
        ".I" => Some(Spec::dI),
        ".F." => Some(Spec::dFd),
        _ if &token[..2] == ".H" => {
            if token.len() == 8 {
                if let Ok(n) = i32::from_str_radix(&token[2..], 16) {
                    Some(Spec::dH(
                        (n / 65536).try_into().unwrap(),
                        ((n % 65536) / 256).try_into().unwrap(),
                        (n % 256).try_into().unwrap(),
                    ))
                } else {
                    None
                }
            } else {
                match &token[2..].to_lowercase() as &str {
                    "green" => Some(Spec::dH(0, 255, 0)),
                    "red" => Some(Spec::dH(255, 0, 0)),
                    "blue" => Some(Spec::dH(0, 0, 255)),
                    _ => None,
                }
            }
        }
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
                let Ok(split) = split_parens(&tokens) else {
                    let _ = dbg!(split_parens(&tokens));
                    return Err("Couldnt parenthesize");
                };
                Ok(form_tree(&split).0)
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
            Value::Const(Const::Range(_)) => Cow::Owned(Const::Error(CellError::InvalidFormula {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                reason: "Range cannot be assigned to a cell",
            })),
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
                    Spec::dH(r, g, b) => Cow::Owned(Const::Color(*r, *g, *b)),
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

    fn get_refs(
        &self,
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        spec: &SpecValues,
    ) -> Vec<Range> {
        match self {
            Value::Ref(r) => vec![r.into()],
            Value::Add(lhs, rhs)
            | Value::Sub(lhs, rhs)
            | Value::Mul(lhs, rhs)
            | Value::Div(lhs, rhs)
            | Value::Eq(lhs, rhs)
            | Value::And(lhs, rhs)
            | Value::Lt(lhs, rhs) => [lhs, rhs]
                .iter()
                .flat_map(|s| s.get_refs(data, eval, func, spec))
                .collect(),
            Value::Func { name, args } => func
                .get_func(name)
                .iter()
                .flat_map(|f| f.get_refs(args, data, eval, func, spec))
                .collect(),
            Value::Const(Const::Range(r)) => vec![r.clone()],
            Value::Const(_) | Value::Special(_) => vec![],
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Bound {
    Fin(i32),
    Inf,
}
impl Bound {
    fn ge(&self, v: i32) -> bool {
        match self {
            Bound::Fin(n) => &v <= n,
            Bound::Inf => true,
        }
    }
}
struct RangeFormula {
    rbound: Bound,
    cbound: Bound,
    conditon: Value,
    value: Value,
}

#[derive(Debug, Default, PartialEq)]
pub(crate) struct CellData {
    pub(crate) val: String,

    /// Is none when recalculating
    // pub(crate) parsed: Option<Value>,
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
            // parsed: None,
        };

        Ok(c)
    }
}

impl CellData {
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
    pub(crate) fn val(self, val: String) -> Self {
        Self { val, ..self }
    }
}
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
        dbg!(&self);
        dbg!(&kc);
        for pos in kc.iter() {
            // Remove dependants that dont depend
            let s = self.get(&pos);
            let mut torem = HashSet::new();
            if let Ok(s) = s {
                for p in s.dependants.clone().iter() {
                    if let Ok(deps) = self.get(p) {
                        if let Ok(pv) = deps.val.parse::<Value>() {
                            if !pv
                                .get_refs(&self.0, &mut self.1, &self.2, &SpecValues::from_sloc(*p))
                                .iter()
                                .any(|r| r.in_range(&pos))
                            {
                                println!("Removing dependant {p:?} from {pos:?}");
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
        for pos in kc.iter() {
            let s = self.get(&pos);
            let mut toadd = None;
            if let Ok(s) = s {
                if let Ok(pv) = s.val.parse::<Value>() {
                    toadd = Some(pv.get_refs(
                        &self.0,
                        &mut self.1,
                        &self.2,
                        &SpecValues::from_sloc(*pos),
                    ));
                }
            }
            let mut dirty_flag = false;
            if let Some(toadd) = toadd {
                for posd in kc.iter() {
                    if toadd.iter().any(|r| r.in_range(posd)) {
                        if let Ok(c2) = self.get_mut(posd) {
                            c2.dependants.insert(*pos);
                            dirty_flag = dirty_flag || c2.display.is_none();
                        }
                    }
                }
            }
            if dirty_flag {
                self.dirty(&pos);
            }
        }
        dbg!(&self);
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
                let val = if let Ok(s) = s {
                    let val = s.val.clone();
                    match val.parse::<Value>() {
                        Ok(val) => val,
                        Err(e) => Value::Const(Const::Error(CellError::InvalidFormula {
                            cell: Some(pos),
                            cursor: 0,
                            reason: e,
                        })),
                    }
                } else {
                    Value::Const(Const::Error(s.unwrap_err()))
                };

                // Add depends
                // let depends =
                //     val.get_refs(&self.0, &mut self.1, &self.2, &SpecValues::from_sloc(pos));
                // dbg!(&depends);
                // todo!("Need to reverse this to iterate over self instead");
                // for d in depends {
                //     if let Ok(dc) = self.get_mut(&d.0) {
                //         dc.dependants.insert(pos);
                //     } else {
                //         let mut cd = CellData::default();
                //         cd.dependants.insert(pos);
                //         let _ = self.insert(d.0, cd);
                //     }
                //     if d.0 == pos {
                //         val = Value::Const(Const::Error(CellError::ReferenceLoop {
                //             loop_point: Some(pos),
                //         }));
                //     }
                // }
                // for dc in self.0 .0.iter_mut() {
                //     for d in depends.iter() {
                //         if d.in_range(dc.0) {
                //             dc.1.dependants.insert(pos);
                //         }
                //     }
                // }
                // self.dirty(&pos);

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
    fn get_refs(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        spec: &SpecValues,
    ) -> Vec<Range> {
        args.iter()
            .flat_map(|a| a.get_refs(data, eval, func, spec))
            .collect()
    }
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
            if let Value::Const(Const::Range(r)) = a {
                // dbg!(data);
                if let Some(value) = sum_range(data, r, eval, spec, func, &mut s) {
                    return value;
                }
            } else {
                let c = a.eval(data, eval, func, spec);
                dbg!(&c);
                if let Const::Num(n) = c.as_ref() {
                    s += n;
                } else if let Const::Range(r) = c.as_ref() {
                    if let Some(value) = sum_range(data, r, eval, spec, func, &mut s) {
                        return value;
                    }
                } else if let Const::Error(_) = c.as_ref() {
                    return c.into_owned();
                } else {
                    return e;
                }
            }
        }
        Const::Num(s)
    }

    fn get_refs(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        spec: &SpecValues,
    ) -> Vec<Range> {
        let mut refs: Vec<_> = args
            .iter()
            .flat_map(|a| a.get_refs(data, eval, func, spec))
            .collect();
        refs.extend(args.iter().flat_map(|a| {
            if let Value::Const(Const::Range(r)) = a {
                vec![r.clone()]
            } else {
                a.get_refs(data, eval, func, spec)
            }
        }));
        refs
    }
}

fn sum_range(
    data: &SheetData,
    r: &Range,
    eval: &mut std::collections::BTreeMap<(i32, i32), usize>,
    spec: &SpecValues,
    func: &SheetFunc,
    s: &mut f64,
) -> Option<Const> {
    for elem in data.keys().filter(|k| r.in_range(k)) {
        // dbg!(&elem);
        let v = (&data).get(&elem, &eval);
        let v = match v {
            Ok(v) => v,
            Err(e) => match e {
                CellError::MissingCell { cell: _ } => continue,
                _ => return Some(Const::Error(e)),
            },
        };
        // dbg!(v);
        let Ok(v) = v.val.parse::<Value>() else {
            return Some(Const::Error(CellError::MissingCell { cell: Some(*elem) }));
        };
        let new_spec = spec.clone().with_sloc(*elem);
        // dbg!(&new_spec);
        let cv = v.eval(data, eval, func, &new_spec);
        if let Const::Num(n) = cv.as_ref() {
            *s += n;
        } else if let Const::Error(err) = cv.as_ref() {
            return Some(Const::Error(err.clone()));
        }
    }
    None
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
    fn get_refs(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        spec: &SpecValues,
    ) -> Vec<Range> {
        let mut refs = args
            .iter()
            .flat_map(|a| a.get_refs(data, eval, func, spec))
            .collect();
        let [c, r] = args else {
            return refs;
        };
        let c = c.eval(data, eval, func, spec);
        let Const::Num(c) = c.as_ref() else {
            return refs;
        };
        let r = r.eval(data, eval, func, spec);
        let Const::Num(r) = r.as_ref() else {
            return refs;
        };
        // TODO make this more rigid, find good way to do a let else
        if c < &0. || c != &c.round() || r < &0. || &r.round() != r {
            return refs;
        }
        let c = *c as i32;
        let r = *r as i32;
        refs.push((&(c, r)).into());
        refs
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
        let Value::Const(Const::Range(r)) = range else {
            return Const::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Range".into()],
            });
        };
        // dbg!(cond);
        for cell in data.keys().filter(|k| r.in_range(k)) {
            let Ok(celldata) = data.get(&cell, eval) else {
                continue;
            };
            let inner_spec = spec.clone().with_sloc(*cell);
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

            let fspec = spec.clone().with_f_sloc(Some((*cell, cval)));
            // dbg!(spec, &fspec);

            if let Const::Bool(true) = cond.eval(data, eval, func, &fspec).as_ref() {
                count += 1;
            };
        }
        Const::Num(count as f64)
    }

    fn get_refs(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        spec: &SpecValues,
    ) -> Vec<Range> {
        let mut refs = args
            .iter()
            .flat_map(|a| a.get_refs(data, eval, func, spec))
            .collect();
        let Value::Const(Const::Range(r)) = &args[0] else {
            return refs;
        };
        refs.push(r.clone());
        // dbg!(&refs);
        refs
    }
}

#[derive(Default)]
pub(crate) struct RangeFunc;
impl Function for RangeFunc {
    fn get_refs(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        spec: &SpecValues,
    ) -> Vec<Range> {
        let mut refs = args
            .iter()
            .flat_map(|a| a.get_refs(data, eval, func, spec))
            .collect();
        if let [c, r] = args {
            let c = c.eval(data, eval, func, spec);
            let Const::Num(c) = c.as_ref() else {
                return refs;
            };
            let r = r.eval(data, eval, func, spec);
            let Const::Num(r) = r.as_ref() else {
                return refs;
            };
            // TODO make this more rigid, find good way to do a let else
            if c < &0. || c != &c.round() || r < &0. || &r.round() != r {
                return refs;
            }
            let c = *c as i32;
            let r = *r as i32;
            refs.push(Range::from(&(c, r)).into_inf());
            refs
        } else if let [c1, r1, c2, r2] = args {
            let c1 = c1.eval(data, eval, func, spec);
            let Const::Num(c1) = c1.as_ref() else {
                return refs;
            };
            let r1 = r1.eval(data, eval, func, spec);
            let Const::Num(r1) = r1.as_ref() else {
                return refs;
            };
            // TODO make this more rigid, find good way to do a let else
            if c1 < &0. || c1 != &c1.round() || r1 < &0. || &r1.round() != r1 {
                return refs;
            }
            let c1 = *c1 as i32;
            let r1 = *r1 as i32;
            let c2 = c2.eval(data, eval, func, spec);
            let Ok(c2) = TryInto::<Bound>::try_into(c2.as_ref()) else {
                return refs;
            };
            let r2 = r2.eval(data, eval, func, spec);
            let Ok(r2) = TryInto::<Bound>::try_into(r2.as_ref()) else {
                return refs;
            };
            refs.push(Range((c1, r1), SLocBound(c2, r2)));
            refs
        } else {
            refs
        }
    }

    fn name(&self) -> &'static str {
        "range"
    }

    fn call(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        spec: &SpecValues,
    ) -> Const {
        if let [c, r] = args {
            let c = c.eval(data, eval, func, spec);
            let Const::Num(c) = c.as_ref() else {
                return Const::Error(CellError::BadArgument {
                    cell: Some(spec.to_sloc()),
                    cursor: 0,
                    expected_types: vec!["Number".into()],
                });
            };
            let r = r.eval(data, eval, func, spec);
            let Const::Num(r) = r.as_ref() else {
                return Const::Error(CellError::BadArgument {
                    cell: Some(spec.to_sloc()),
                    cursor: 1,
                    expected_types: vec!["Number".into()],
                });
            };
            // TODO make this more rigid, find good way to do a let else
            if c < &0. || c != &c.round() || r < &0. || &r.round() != r {
                return Const::Error(CellError::BadArgument {
                    cell: Some(spec.to_sloc()),
                    cursor: 0,
                    expected_types: vec!["Whole number".into()],
                });
            }
            let c = *c as i32;
            let r = *r as i32;
            Const::Range(Range::range_inf((c, r)))
        } else if let [c1, r1, c2, r2] = args {
            let c1 = c1.eval(data, eval, func, spec);
            let Const::Num(c1) = c1.as_ref() else {
                return Const::Error(CellError::BadArgument {
                    cell: Some(spec.to_sloc()),
                    cursor: 0,
                    expected_types: vec!["Number".into()],
                });
            };
            let r1 = r1.eval(data, eval, func, spec);
            let Const::Num(r1) = r1.as_ref() else {
                return Const::Error(CellError::BadArgument {
                    cell: Some(spec.to_sloc()),
                    cursor: 1,
                    expected_types: vec!["Number".into()],
                });
            };
            // TODO make this more rigid, find good way to do a let else
            if c1 < &0. || c1 != &c1.round() || r1 < &0. || &r1.round() != r1 {
                return Const::Error(CellError::BadArgument {
                    cell: Some(spec.to_sloc()),
                    cursor: 0,
                    expected_types: vec!["Whole number".into()],
                });
            }
            let c1 = *c1 as i32;
            let r1 = *r1 as i32;
            let c2 = c2.eval(data, eval, func, spec);
            let c2 = match TryInto::<Bound>::try_into(c2.as_ref()) {
                Ok(b) => b,
                Err(e) => return e.into_pos(spec.to_sloc()),
            };
            let r2 = r2.eval(data, eval, func, spec);
            let r2 = match TryInto::<Bound>::try_into(r2.as_ref()) {
                Ok(b) => b,
                Err(e) => return e.into_pos(spec.to_sloc()),
            };
            Const::Range(Range((c1, r1), SLocBound(c2, r2)))
        } else {
            Const::Error(CellError::WrongNumArguments {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                arguments: vec![2, 4],
            })
        }
    }
}

#[cfg(test)]
mod test {

    use crate::formula::*;

    #[test]
    fn const_parse() {
        assert_eq!(Const::from_str("true"), Ok(Const::Bool(true)));
        assert_eq!(Const::from_str("7"), Ok(Const::Num(7.0)));
        assert_eq!(Const::from_str("7.5"), Ok(Const::Num(7.5)));
        assert_eq!(Const::from_str("-7.5"), Ok(Const::Num(-7.5)));
        assert_eq!(Const::from_str("-7.5e"), Err(()));
        assert_eq!(Const::from_str("\"Hi\""), Ok(Const::String("Hi".into())));
        assert_eq!(Const::from_str("\"Hi"), Err(()));
        assert_eq!(Const::from_str("Hi\""), Err(()));
        assert_eq!(Const::from_str("abcd"), Err(()));

        assert_eq!(Const::Num(1.).merge_err(&Const::Num(2.)), None);
        assert_eq!(
            Const::Error(CellError::ReferenceLoop {
                loop_point: Some((0, 0))
            })
            .merge_err(&Const::Num(2.)),
            Some(CellError::ReferenceLoop {
                loop_point: Some((0, 0))
            })
        );
        assert_eq!(
            Const::Num(2.).merge_err(&Const::Error(CellError::ReferenceLoop {
                loop_point: Some((0, 0))
            })),
            Some(CellError::ReferenceLoop {
                loop_point: Some((0, 0))
            })
        );
        assert_eq!(
            Const::Error(CellError::ReferenceLoop {
                loop_point: Some((0, 0))
            })
            .merge_err(&Const::Error(CellError::ReferenceLoop {
                loop_point: Some((1, 0))
            })),
            Some(CellError::ReferenceLoop {
                loop_point: Some((0, 0))
            })
        );
    }
    #[test]
    fn range_parse() {
        assert_eq!(
            is_range("A1:A1"),
            Some(Range((0, 0), SLocBound(Bound::Fin(0), Bound::Fin(0))))
        );
        assert_eq!(
            is_range("A1:A5"),
            Some(Range((0, 0), SLocBound(Bound::Fin(0), Bound::Fin(4))))
        );
        assert_eq!(
            is_range("A1:c5"),
            Some(Range((0, 0), SLocBound(Bound::Fin(2), Bound::Fin(4))))
        );
        assert_eq!(
            is_range("A1:AA123"),
            Some(Range((0, 0), SLocBound(Bound::Fin(26), Bound::Fin(122))))
        );
        assert_eq!(
            is_range("A1:ABCZ12345"),
            Some(Range(
                (0, 0),
                SLocBound(Bound::Fin(19031), Bound::Fin(12344))
            ))
        );
        assert_eq!(
            is_range("A1:"),
            Some(Range((0, 0), SLocBound(Bound::Inf, Bound::Inf)))
        );
        assert_eq!(
            is_range("A1:A"),
            Some(Range((0, 0), SLocBound(Bound::Fin(0), Bound::Inf)))
        );
        assert_eq!(
            is_range("A1:C"),
            Some(Range((0, 0), SLocBound(Bound::Fin(2), Bound::Inf)))
        );
        assert_eq!(
            is_range("A1:1"),
            Some(Range((0, 0), SLocBound(Bound::Inf, Bound::Fin(0))))
        );
        assert_eq!(
            is_range("A1:20"),
            Some(Range((0, 0), SLocBound(Bound::Inf, Bound::Fin(19))))
        );
        assert_eq!(is_range("A0:A"), None,);
        assert_eq!(is_range("A3:A1"), None,);
        assert_eq!(is_range("B1:A1"), None,);
        assert_eq!(is_range("B3:A"), None,);
        assert_eq!(is_range("ABCDE0:ABCZZ1"), None,);
        assert_eq!(is_range("ABC3:ABK1"), None,);
        assert_eq!(is_range("SUS1:AMO1"), None,);
        assert_eq!(is_range("NGU3:S"), None,);

        assert!(Range((2, 2), SLocBound(Bound::Fin(5), Bound::Fin(5))).in_range(&(2, 2)));
        assert!(Range((2, 2), SLocBound(Bound::Fin(5), Bound::Fin(5))).in_range(&(2, 3)));
        assert!(Range((2, 2), SLocBound(Bound::Fin(5), Bound::Fin(5))).in_range(&(3, 3)));
        assert!(Range((2, 2), SLocBound(Bound::Fin(5), Bound::Fin(5))).in_range(&(5, 3)));
        assert!(Range((2, 2), SLocBound(Bound::Fin(5), Bound::Fin(5))).in_range(&(5, 5)));
        assert!(!Range((2, 2), SLocBound(Bound::Fin(5), Bound::Fin(5))).in_range(&(0, 4)));
        assert!(!Range((2, 2), SLocBound(Bound::Fin(5), Bound::Fin(5))).in_range(&(4, 0)));
        assert!(!Range((2, 2), SLocBound(Bound::Fin(5), Bound::Fin(5))).in_range(&(8, 4)));
        assert!(!Range((2, 2), SLocBound(Bound::Fin(5), Bound::Fin(5))).in_range(&(4, 8)));

        assert!(Range((2, 2), SLocBound(Bound::Fin(5), Bound::Inf)).in_range(&(2, 2)));
        assert!(Range((2, 2), SLocBound(Bound::Fin(5), Bound::Inf)).in_range(&(2, 3)));
        assert!(Range((2, 2), SLocBound(Bound::Fin(5), Bound::Inf)).in_range(&(3, 3)));
        assert!(Range((2, 2), SLocBound(Bound::Fin(5), Bound::Inf)).in_range(&(5, 3)));
        assert!(Range((2, 2), SLocBound(Bound::Fin(5), Bound::Inf)).in_range(&(5, 5)));
        assert!(!Range((2, 2), SLocBound(Bound::Fin(5), Bound::Inf)).in_range(&(0, 4)));
        assert!(!Range((2, 2), SLocBound(Bound::Fin(5), Bound::Inf)).in_range(&(4, 0)));
        assert!(!Range((2, 2), SLocBound(Bound::Fin(5), Bound::Inf)).in_range(&(8, 4)));
        assert!(Range((2, 2), SLocBound(Bound::Fin(5), Bound::Inf)).in_range(&(4, 8)));

        assert!(Range((2, 2), SLocBound(Bound::Inf, Bound::Fin(5))).in_range(&(2, 2)));
        assert!(Range((2, 2), SLocBound(Bound::Inf, Bound::Fin(5))).in_range(&(2, 3)));
        assert!(Range((2, 2), SLocBound(Bound::Inf, Bound::Fin(5))).in_range(&(3, 3)));
        assert!(Range((2, 2), SLocBound(Bound::Inf, Bound::Fin(5))).in_range(&(5, 3)));
        assert!(Range((2, 2), SLocBound(Bound::Inf, Bound::Fin(5))).in_range(&(5, 5)));
        assert!(!Range((2, 2), SLocBound(Bound::Inf, Bound::Fin(5))).in_range(&(0, 4)));
        assert!(!Range((2, 2), SLocBound(Bound::Inf, Bound::Fin(5))).in_range(&(4, 0)));
        assert!(Range((2, 2), SLocBound(Bound::Inf, Bound::Fin(5))).in_range(&(8, 4)));
        assert!(!Range((2, 2), SLocBound(Bound::Inf, Bound::Fin(5))).in_range(&(4, 8)));

        assert!(Range((2, 2), SLocBound(Bound::Inf, Bound::Inf)).in_range(&(2, 2)));
        assert!(Range((2, 2), SLocBound(Bound::Inf, Bound::Inf)).in_range(&(2, 3)));
        assert!(Range((2, 2), SLocBound(Bound::Inf, Bound::Inf)).in_range(&(3, 3)));
        assert!(Range((2, 2), SLocBound(Bound::Inf, Bound::Inf)).in_range(&(5, 3)));
        assert!(Range((2, 2), SLocBound(Bound::Inf, Bound::Inf)).in_range(&(5, 5)));
        assert!(!Range((2, 2), SLocBound(Bound::Inf, Bound::Inf)).in_range(&(0, 4)));
        assert!(!Range((2, 2), SLocBound(Bound::Inf, Bound::Inf)).in_range(&(4, 0)));
        assert!(Range((2, 2), SLocBound(Bound::Inf, Bound::Inf)).in_range(&(8, 4)));
        assert!(Range((2, 2), SLocBound(Bound::Inf, Bound::Inf)).in_range(&(4, 8)));
    }
    #[test]
    fn ref_inv() {
        let tests = ["A1", "B3", "AA72", "AAA72", "AAZZ99999"];
        let tests2 = [(0, 0), (1, 3), (26, 26), (57023, 99384)];
        for t in tests {
            let forward = is_ref(t).unwrap();
            let inv = show_ref(&forward);
            assert_eq!(t, inv);
        }
        for t in tests2 {
            let forward = show_ref(&t);
            let inv = is_ref(&forward);
            assert_eq!(Some(t), inv);
        }
    }
    #[test]
    fn spec_parse() {
        assert_eq!(is_spec(".."), None);
        assert_eq!(is_spec(".C"), Some(Spec::dC));
        assert_eq!(is_spec(".R"), Some(Spec::dR));
        assert_eq!(is_spec(".F."), Some(Spec::dFd));
        assert_eq!(is_spec(".FC"), Some(Spec::dFC));
        assert_eq!(is_spec(".FR"), Some(Spec::dFR));
        assert_eq!(is_spec(".S."), None);
        assert_eq!(is_spec(".SC"), Some(Spec::dSC));
        assert_eq!(is_spec(".SR"), Some(Spec::dSR));
        assert_eq!(is_spec(".I"), Some(Spec::dI));

        assert_eq!(is_spec(".H00ff00"), Some(Spec::dH(0, 255, 0)));
        assert_eq!(is_spec(".H00ff000"), None);
        assert_eq!(is_spec(".H0f"), None);
        assert_eq!(is_spec(".H00ggff"), None);
        assert_eq!(is_spec(".HGREEN"), Some(Spec::dH(0, 255, 0)));
        assert_eq!(is_spec(".Hred"), Some(Spec::dH(255, 0, 0)));
        assert_eq!(is_spec(".HBlUe"), Some(Spec::dH(0, 0, 255)));
    }
    #[test]
    fn spec_method() {
        let mut s = SpecValues::from_sloc((1, 1));
        assert_eq!(s.to_sloc(), (1, 1));
        assert!(!s.invalid_depth(0));
        s.depth = 5;
        assert!(!s.invalid_depth(5));
        assert!(s.invalid_depth(6));
        s = s.with_sloc((2, 2));
        assert_eq!(s.to_sloc(), (2, 2));
    }
    #[test]
    fn split_paren_test() {
        assert_eq!(
            split_parens(&form_tokens("sum(sum(1,2),3)")),
            Ok(ParenTree::Paren(vec![
                Box::new(ParenTree::Token("sum", 0)),
                Box::new(ParenTree::Paren(vec![
                    Box::new(ParenTree::Token("sum", 4)),
                    Box::new(ParenTree::Paren(vec![
                        Box::new(ParenTree::Token("1", 8)),
                        Box::new(ParenTree::Token(",", 9)),
                        Box::new(ParenTree::Token("2", 10))
                    ])),
                    Box::new(ParenTree::Token(",", 12)),
                    Box::new(ParenTree::Token("3", 13))
                ]))
            ]))
        );
        assert_eq!(
            split_parens(&form_tokens("sum(range(1,2,3, 4),3,A5)")),
            Ok(ParenTree::Paren(vec![
                Box::new(ParenTree::Token("sum", 0)),
                Box::new(ParenTree::Paren(vec![
                    Box::new(ParenTree::Token("range", 4)),
                    Box::new(ParenTree::Paren(vec![
                        Box::new(ParenTree::Token("1", 10)),
                        Box::new(ParenTree::Token(",", 11)),
                        Box::new(ParenTree::Token("2", 12)),
                        Box::new(ParenTree::Token(",", 13)),
                        Box::new(ParenTree::Token("3", 14)),
                        Box::new(ParenTree::Token(",", 15)),
                        Box::new(ParenTree::Token("4", 17))
                    ])),
                    Box::new(ParenTree::Token(",", 19)),
                    Box::new(ParenTree::Token("3", 20)),
                    Box::new(ParenTree::Token(",", 21)),
                    Box::new(ParenTree::Token("A5", 22))
                ]))
            ]))
        );
        assert_eq!(
            split_parens(&form_tokens("sum(range(1,2,3, 4),3,A5)"))
                .unwrap()
                .get(0),
            Some(("sum", 0))
        );
        assert_eq!(
            split_parens(&form_tokens("sum(range(1,2,3, 4),3,A5)"))
                .unwrap()
                .get(1),
            Some(("range", 4))
        );
        assert_eq!(
            split_parens(&form_tokens("sum(range(1,2,3, 4),3,A5)"))
                .unwrap()
                .iter_flat()
                .count(),
            13
        );
    }
    #[test]
    fn formula_parse() {
        assert_eq!(
            form_tree(&split_parens(&form_tokens("78")).unwrap()),
            (Value::Const(Const::Num(78.)), false),
        );
        assert_eq!(
            form_tree(&split_parens(&form_tokens("A5")).unwrap()),
            (Value::Ref((0, 4)), false),
        );
        assert_eq!(
            form_tree(&split_parens(&form_tokens("A7+C5+3")).unwrap()),
            (
                Value::Add(
                    Rc::new(Value::Add(
                        Rc::new(Value::Ref((0, 6))),
                        Rc::new(Value::Ref((2, 4)))
                    )),
                    Rc::new(Value::Const(Const::Num(3.)))
                ),
                false
            ),
        );
        assert_eq!(
            form_tree(&split_parens(&form_tokens("5+sum(A1:A)")).unwrap()),
            (
                Value::Add(
                    Rc::new(Value::Const(Const::Num(5.))),
                    Rc::new(Value::Func {
                        name: "sum".into(),
                        args: vec![Value::Const(Const::Range(Range(
                            (0, 0),
                            SLocBound(Bound::Fin(0), Bound::Inf)
                        )))]
                    })
                ),
                false
            ),
        );
        assert_eq!(
            form_tree(&split_parens(&form_tokens("5+sum(A1:A)")).unwrap()),
            (
                Value::Add(
                    Rc::new(Value::Const(Const::Num(5.))),
                    Rc::new(Value::Func {
                        name: "sum".into(),
                        args: vec![Value::Const(Const::Range(Range(
                            (0, 0),
                            SLocBound(Bound::Fin(0), Bound::Inf)
                        )))]
                    })
                ),
                false
            ),
        );
        assert_eq!(
            form_tree(&split_parens(&form_tokens("range(1,1,5,A3)")).unwrap()),
            (
                Value::Func {
                    name: "range".into(),
                    args: vec![
                        Value::Const(Const::Num(1.)),
                        Value::Const(Const::Num(1.)),
                        Value::Const(Const::Num(5.)),
                        Value::Ref((0, 2))
                    ]
                },
                false
            ),
        );
        assert_eq!(
            form_tree(
                &split_parens(&dbg!(form_tokens("if(sum(A1:B)<20, 20, \"Small\")"))).unwrap()
            ),
            (
                Value::Func {
                    name: "if".into(),
                    args: vec![
                        Value::Lt(
                            Rc::new(Value::Func {
                                name: "sum".into(),
                                args: vec![Value::Const(Const::Range(Range(
                                    (0, 0),
                                    SLocBound(Bound::Fin(1), Bound::Inf)
                                )))]
                            }),
                            Rc::new(Value::Const(Const::Num(20.)))
                        ),
                        Value::Const(Const::Num(20.)),
                        Value::Const(Const::String("Small".into())),
                        // Value::Const(Const::Num(30.))
                    ]
                },
                false
            ),
        );
        assert_eq!(
            form_tree(&split_parens(&dbg!(form_tokens("sum(range(0,0,10,10), 20)"))).unwrap()),
            (
                Value::Func {
                    name: "sum".into(),
                    args: vec![
                        Value::Func {
                            name: "range".into(),
                            args: vec![
                                Value::Const(Const::Num(0.)),
                                Value::Const(Const::Num(0.)),
                                Value::Const(Const::Num(10.)),
                                Value::Const(Const::Num(10.)),
                            ]
                        },
                        Value::Const(Const::Num(20.)),
                        // Value::Const(Const::Num(30.))
                    ]
                },
                false
            ),
        );
    }
    #[test]
    fn func_range_test() {
        let mut sheet = Sheet::new();
        let spec = SpecValues::from_sloc((0, 0));
        // sheet.insert((0,0), )
        assert_eq!(RangeFunc.name(), "range");
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(3.)),
                    Value::Const(Const::Num(3.))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            vec![Range((1, 1), SLocBound(Bound::Fin(3), Bound::Fin(3)))]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(3.)),
                    Value::Const(Const::Num(3.))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            Const::Range(Range((1, 1), SLocBound(Bound::Fin(3), Bound::Fin(3))))
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[Value::Const(Const::Num(1.)), Value::Const(Const::Num(1.)),],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            vec![Range((1, 1), SLocBound(Bound::Inf, Bound::Inf))]
        );
        assert_eq!(
            RangeFunc.call(
                &[Value::Const(Const::Num(1.)), Value::Const(Const::Num(1.)),],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            Const::Range(Range((1, 1), SLocBound(Bound::Inf, Bound::Inf)))
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(3.))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(3.))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            Const::Error(CellError::WrongNumArguments {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                arguments: vec![2, 4]
            })
        );

        assert_eq!(
            RangeFunc.get_refs(
                &[Value::Const(Const::Num(1.)), Value::Const(Const::Num(1.)),],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            vec![Range((1, 1), SLocBound(Bound::Inf, Bound::Inf))]
        );
        assert_eq!(
            RangeFunc.call(
                &[Value::Const(Const::Num(1.)), Value::Const(Const::Num(1.)),],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            Const::Range(Range((1, 1), SLocBound(Bound::Inf, Bound::Inf)))
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[Value::Const(Const::Num(-1.)), Value::Const(Const::Num(1.)),],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[Value::Const(Const::Num(-1.)), Value::Const(Const::Num(1.)),],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            Const::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into()]
            })
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[Value::Const(Const::Num(1.6)), Value::Const(Const::Num(1.)),],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[Value::Const(Const::Num(1.6)), Value::Const(Const::Num(1.)),],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            Const::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into()]
            })
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[Value::Const(Const::Num(1.)), Value::Const(Const::Num(1.6)),],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[Value::Const(Const::Num(1.)), Value::Const(Const::Num(1.6)),],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            Const::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into()]
            })
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[Value::Const(Const::Num(1.)), Value::Const(Const::Num(-1.)),],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[Value::Const(Const::Num(1.)), Value::Const(Const::Num(-1.)),],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            Const::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into()]
            })
        );

        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(Const::Num(-1.)),
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(3.)),
                    Value::Const(Const::Num(3.))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(Const::Num(-1.)),
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(3.)),
                    Value::Const(Const::Num(3.))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            Const::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into()]
            })
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(Const::Num(1.6)),
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(3.)),
                    Value::Const(Const::Num(3.))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(Const::Num(1.6)),
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(3.)),
                    Value::Const(Const::Num(3.))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            Const::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into()]
            })
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(1.6)),
                    Value::Const(Const::Num(3.)),
                    Value::Const(Const::Num(3.))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(1.6)),
                    Value::Const(Const::Num(3.)),
                    Value::Const(Const::Num(3.))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            Const::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into()]
            })
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(-1.)),
                    Value::Const(Const::Num(3.)),
                    Value::Const(Const::Num(3.))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(-1.)),
                    Value::Const(Const::Num(3.)),
                    Value::Const(Const::Num(3.))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            Const::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into()]
            })
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(-3.)),
                    Value::Const(Const::Num(3.))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(-3.)),
                    Value::Const(Const::Num(3.))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            Const::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into(), ".I".into()]
            })
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(3.6)),
                    Value::Const(Const::Num(3.))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(3.6)),
                    Value::Const(Const::Num(3.))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            Const::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into(), ".I".into()]
            })
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(3.)),
                    Value::Const(Const::Num(3.6))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(3.)),
                    Value::Const(Const::Num(3.6))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            Const::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into(), ".I".into()]
            })
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(3.)),
                    Value::Const(Const::Num(-3.))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(1.)),
                    Value::Const(Const::Num(3.)),
                    Value::Const(Const::Num(-3.))
                ],
                &sheet.0,
                &mut sheet.1,
                &sheet.2,
                &spec
            ),
            Const::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into(), ".I".into()]
            })
        );
    }
    #[test]
    fn sheet_test() {
        let mut sheet = Sheet::new();
        assert!(sheet
            .insert(
                (0, 0),
                CellData {
                    val: "2".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_none());
        assert!(sheet
            .insert(
                (0, 1),
                CellData {
                    val: "3".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_none());

        assert!(sheet
            .insert(
                (1, 0),
                CellData {
                    val: "=A1+A2".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_none());
        sheet.recompute();
        assert_eq!(
            sheet.get(&(0, 0)),
            Ok(&CellData {
                val: "2".into(),
                display: Some("2".into()),
                dependants: [(1, 0)].into_iter().collect(),
                rangef: None
            })
        );
        assert_eq!(
            sheet.get(&(0, 1)),
            Ok(&CellData {
                val: "3".into(),
                display: Some("3".into()),
                dependants: [(1, 0)].into_iter().collect(),
                rangef: None
            })
        );
        assert_eq!(
            sheet.get(&(1, 0)),
            Ok(&CellData {
                val: "=A1+A2".into(),
                display: Some("5".into()),
                dependants: HashSet::new(),
                rangef: None
            })
        );

        assert!(sheet
            .insert(
                (1, 0),
                CellData {
                    val: "=sum(A1:A10)".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_some());
        sheet.recompute();
        // dbg!(&sheet);
        assert_eq!(
            sheet.get(&(0, 0)),
            Ok(&CellData {
                val: "2".into(),
                display: Some("2".into()),
                dependants: [(1, 0)].into_iter().collect(),
                rangef: None
            })
        );
        assert_eq!(
            sheet.get(&(0, 1)),
            Ok(&CellData {
                val: "3".into(),
                display: Some("3".into()),
                dependants: [(1, 0)].into_iter().collect(),
                rangef: None
            })
        );
        assert_eq!(
            sheet.get(&(1, 0)),
            Ok(&CellData {
                val: "=sum(A1:A10)".into(),
                display: Some("5".into()),
                dependants: HashSet::new(),
                rangef: None
            })
        );

        assert!(sheet
            .insert(
                (1, 0),
                CellData {
                    val: "=if(A1<A2, A2, A1)".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_some());
        sheet.recompute();
        assert_eq!(
            sheet.get(&(0, 0)),
            Ok(&CellData {
                val: "2".into(),
                display: Some("2".into()),
                dependants: [(1, 0)].into_iter().collect(),
                rangef: None
            })
        );
        assert_eq!(
            sheet.get(&(0, 1)),
            Ok(&CellData {
                val: "3".into(),
                display: Some("3".into()),
                dependants: [(1, 0)].into_iter().collect(),
                rangef: None
            })
        );
        assert_eq!(
            sheet.get(&(1, 0)),
            Ok(&CellData {
                val: "=if(A1<A2, A2, A1)".into(),
                display: Some("3".into()),
                dependants: HashSet::new(),
                rangef: None
            })
        );

        assert!(sheet
            .insert(
                (0, 0),
                CellData {
                    val: "=7".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_some());
        sheet.recompute();
        // dbg!(&sheet);
        assert_eq!(
            sheet.get(&(0, 0)),
            Ok(&CellData {
                val: "=7".into(),
                display: Some("7".into()),
                dependants: [(1, 0)].into_iter().collect(),
                rangef: None
            })
        );
        assert_eq!(
            sheet.get(&(0, 1)),
            Ok(&CellData {
                val: "3".into(),
                display: Some("3".into()),
                dependants: [(1, 0)].into_iter().collect(),
                rangef: None
            })
        );
        assert_eq!(
            sheet.get(&(1, 0)),
            Ok(&CellData {
                val: "=if(A1<A2, A2, A1)".into(),
                display: Some("7".into()),
                dependants: HashSet::new(),
                rangef: None
            })
        );

        assert!(sheet
            .insert(
                (1, 0),
                CellData {
                    val: "=9e9e9".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_some());
        sheet.recompute();
        // dbg!(&sheet);
        assert_eq!(
            sheet.get(&(0, 0)),
            Ok(&CellData {
                val: "=7".into(),
                display: Some("7".into()),
                dependants: HashSet::new(),
                rangef: None
            })
        );
        assert_eq!(
            sheet.get(&(0, 1)),
            Ok(&CellData {
                val: "3".into(),
                display: Some("3".into()),
                dependants: HashSet::new(),
                rangef: None
            })
        );
        let d = sheet.get(&(1, 0)).unwrap();
        assert_eq!(d.val, "=9e9e9");
        assert!(d
            .display
            .as_ref()
            .is_some_and(|f| f.contains("InvalidFormula")));

        assert!(sheet
            .insert(
                (1, 0),
                CellData {
                    val: "=".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_some());
        sheet.recompute();
        // dbg!(&sheet);
        assert_eq!(
            sheet.get(&(0, 0)),
            Ok(&CellData {
                val: "=7".into(),
                display: Some("7".into()),
                dependants: HashSet::new(),
                rangef: None
            })
        );
        assert_eq!(
            sheet.get(&(0, 1)),
            Ok(&CellData {
                val: "3".into(),
                display: Some("3".into()),
                dependants: HashSet::new(),
                rangef: None
            })
        );
        let d = sheet.get(&(1, 0)).unwrap();
        assert_eq!(d.val, "=");
        assert!(d
            .display
            .as_ref()
            .is_some_and(|f| f.contains("InvalidFormula")));

        assert!(sheet
            .insert(
                (1, 0),
                CellData {
                    val: "=sum(range(0,0,0,10),value(0,1))".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_some());
        sheet.recompute();
        assert_eq!(
            sheet.get(&(0, 0)),
            Ok(&CellData {
                val: "=7".into(),
                display: Some("7".into()),
                dependants: [(1, 0)].into_iter().collect(),
                rangef: None
            })
        );
        assert_eq!(
            sheet.get(&(0, 1)),
            Ok(&CellData {
                val: "3".into(),
                display: Some("3".into()),
                dependants: [(1, 0)].into_iter().collect(),
                rangef: None
            })
        );
        let d = sheet.get(&(1, 0)).unwrap();
        assert_eq!(d.display, Some("13".into()));

        assert!(sheet
            .insert(
                (1, 0),
                CellData {
                    val: "=sum(range(0,0,0,B2),value(0,1))".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_some());
        assert!(sheet
            .insert(
                (1, 1),
                CellData {
                    val: "-1".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_none());
        sheet.recompute();
        assert_eq!(
            sheet.get(&(0, 0)),
            Ok(&CellData {
                val: "=7".into(),
                display: Some("7".into()),
                dependants: HashSet::new(),
                rangef: None
            })
        );
        assert_eq!(
            sheet.get(&(1, 1)),
            Ok(&CellData {
                val: "-1".into(),
                display: Some("-1".into()),
                dependants: [(1, 0)].into_iter().collect(),
                rangef: None
            })
        );
        let d = sheet.get(&(1, 0)).unwrap();
        assert_eq!(
            d.display,
            Some(format!(
                "{:?}",
                CellError::BadArgument {
                    cell: Some((1, 0)),
                    cursor: 0,
                    expected_types: vec!["Whole number".into(), ".I".into()]
                }
            ))
        );
        assert!(sheet
            .insert(
                (1, 1),
                CellData {
                    val: "0.6".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_some());
        sheet.recompute();
        assert_eq!(
            sheet.get(&(0, 0)),
            Ok(&CellData {
                val: "=7".into(),
                display: Some("7".into()),
                dependants: HashSet::new(),
                rangef: None
            })
        );
        assert_eq!(
            sheet.get(&(1, 1)),
            Ok(&CellData {
                val: "0.6".into(),
                display: Some("0.6".into()),
                dependants: [(1, 0)].into_iter().collect(),
                rangef: None
            })
        );
        let d = sheet.get(&(1, 0)).unwrap();
        assert_eq!(
            d.display,
            Some(format!(
                "{:?}",
                CellError::BadArgument {
                    cell: Some((1, 0)),
                    cursor: 0,
                    expected_types: vec!["Whole number".into(), ".I".into()]
                }
            ))
        );

        assert!(sheet
            .insert(
                (1, 0),
                CellData {
                    val: "=sum(range(0,0,B2,10),value(0,1))".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_some());
        assert!(sheet
            .insert(
                (1, 1),
                CellData {
                    val: "-1".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_some());
        sheet.recompute();
        assert_eq!(
            sheet.get(&(0, 0)),
            Ok(&CellData {
                val: "=7".into(),
                display: Some("7".into()),
                dependants: HashSet::new(),
                rangef: None
            })
        );
        assert_eq!(
            sheet.get(&(1, 1)),
            Ok(&CellData {
                val: "-1".into(),
                display: Some("-1".into()),
                dependants: [(1, 0)].into_iter().collect(),
                rangef: None
            })
        );
        let d = sheet.get(&(1, 0)).unwrap();
        assert_eq!(
            d.display,
            Some(format!(
                "{:?}",
                CellError::BadArgument {
                    cell: Some((1, 0)),
                    cursor: 0,
                    expected_types: vec!["Whole number".into(), ".I".into()]
                }
            ))
        );
        assert!(sheet
            .insert(
                (1, 1),
                CellData {
                    val: "0.6".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_some());
        sheet.recompute();
        assert_eq!(
            sheet.get(&(0, 0)),
            Ok(&CellData {
                val: "=7".into(),
                display: Some("7".into()),
                dependants: HashSet::new(),
                rangef: None
            })
        );
        assert_eq!(
            sheet.get(&(1, 1)),
            Ok(&CellData {
                val: "0.6".into(),
                display: Some("0.6".into()),
                dependants: [(1, 0)].into_iter().collect(),
                rangef: None
            })
        );
        let d = sheet.get(&(1, 0)).unwrap();
        assert_eq!(
            d.display,
            Some(format!(
                "{:?}",
                CellError::BadArgument {
                    cell: Some((1, 0)),
                    cursor: 0,
                    expected_types: vec!["Whole number".into(), ".I".into()]
                }
            ))
        );

        assert!(sheet
            .insert(
                (1, 0),
                CellData {
                    val: "=if(A1<A2 & B2<A2, A2, B2)*2".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_some());
        sheet.recompute();
        let d = sheet.get(&(1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData {
                val: "=if(A1<A2 & B2<A2, A2, B2)*2".into(),
                display: Some("1.2".into()),
                dependants: HashSet::new(),
                rangef: None
            }
        );
        assert!(sheet
            .insert(
                (0, 0),
                CellData {
                    val: "1".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_some());
        sheet.recompute();
        let d = sheet.get(&(1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData {
                val: "=if(A1<A2 & B2<A2, A2, B2)*2".into(),
                display: Some("6".into()),
                dependants: HashSet::new(),
                rangef: None
            }
        );

        assert_eq!(sheet.iter().count(), 4);
        assert_eq!(sheet.iter_mut().count(), 4);
        assert!(sheet.iter().all(|(x, _)| x.0 < 2 && x.1 < 2));
        assert_eq!(
            sheet.val_mut(&(0, 0)),
            Some(Box::leak(Box::new("1".into())))
        );
        dbg!(&sheet);

        assert!(sheet
            .insert(
                (1, 0),
                CellData {
                    val: "=countif(A1:A10, .F.<4)".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_some());
        sheet.recompute();
        let d = sheet.get(&(1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData {
                val: "=countif(A1:A10, .F.<4)".into(),
                display: Some("2".into()),
                dependants: HashSet::new(),
                rangef: None
            }
        );
        assert!(sheet
            .insert(
                (1, 0),
                CellData {
                    val: "=countif(A1:A10, .F.<2)".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_some());
        sheet.recompute();
        let d = sheet.get(&(1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData {
                val: "=countif(A1:A10, .F.<2)".into(),
                display: Some("1".into()),
                dependants: HashSet::new(),
                rangef: None
            }
        );

        assert!(sheet
            .insert(
                (1, 0),
                CellData {
                    val: "=countif(A1:B10, .F.<2)".into(),
                    display: None,
                    dependants: HashSet::new(),
                    rangef: None,
                },
            )
            .is_some());
        sheet.recompute();
        let d = sheet.get(&(1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData {
                val: "=countif(A1:B10, .F.<2)".into(),
                display: Some(format!(
                    "{:?}",
                    CellError::ReferenceLoop {
                        loop_point: Some((1, 0))
                    }
                )),
                dependants: [(1, 0)].into_iter().collect(),
                rangef: None
            }
        );

        sheet.set_disp(&(1, 0), None);
        let d = sheet.get(&(1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData {
                val: "=countif(A1:B10, .F.<2)".into(),
                display: None,
                dependants: [(1, 0)].into_iter().collect(),
                rangef: None
            }
        );
        sheet.set_disp(&(1, 0), Some("Not error".into()));
        let d = sheet.get(&(1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData {
                val: "=countif(A1:B10, .F.<2)".into(),
                display: Some("Not error".into()),
                dependants: [(1, 0)].into_iter().collect(),
                rangef: None
            }
        );
        sheet.set_disp_err(
            &(1, 0),
            CellError::MissingCell {
                cell: Some((100, 100)),
            },
        );
        let d = sheet.get(&(1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData {
                val: "=countif(A1:B10, .F.<2)".into(),
                display: Some(format!(
                    "{:?}",
                    CellError::MissingCell {
                        cell: Some((100, 100))
                    }
                )),
                dependants: [(1, 0)].into_iter().collect(),
                rangef: None
            }
        );

        *sheet.val_mut(&(1, 0)).unwrap() = "=countif(A1:A10, .F.=4)-7".into();
        sheet.recompute();
        let d = sheet.get(&(1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData {
                val: "=countif(A1:A10, .F.=4)-7".into(),
                display: Some("-7".into()),
                dependants: HashSet::new(),
                rangef: None
            }
        );
        *sheet.val_mut(&(1, 0)).unwrap() = "=countif(A1:A10, .F.=1)/2".into();
        sheet.recompute();
        let d = sheet.get(&(1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData {
                val: "=countif(A1:A10, .F.=1)/2".into(),
                display: Some("0.5".into()),
                dependants: HashSet::new(),
                rangef: None
            }
        );
        *sheet.val_mut(&(1, 0)).unwrap() = "=value(0,1)".into();
        sheet.recompute();
        let d = sheet.get(&(1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData {
                val: "=value(0,1)".into(),
                display: Some("3".into()),
                dependants: HashSet::new(),
                rangef: None
            }
        );
        assert_eq!(
            sheet.get(&(0, 1)),
            Ok(&CellData {
                val: "3".into(),
                display: Some("3".into()),
                dependants: [(1, 0)].into_iter().collect(),
                rangef: None
            })
        );
        dbg!(&sheet);
        sheet.remove(&(0, 1));
        sheet.recompute();
        dbg!(&sheet);
        let d = sheet.get(&(1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData {
                val: "=value(0,1)".into(),
                display: Some(format!(
                    "{:?}",
                    CellError::MissingCell { cell: Some((0, 1)) }
                )),
                dependants: HashSet::new(),
                rangef: None
            }
        );

        assert!(sheet.contains_key(&(0, 0)));
        assert!(!sheet.contains_key(&(2, 2)));
    }
}
