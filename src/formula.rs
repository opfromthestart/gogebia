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
enum Const {
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
                CellError::InvalidFormula { cell: None, cursor } => CellError::InvalidFormula {
                    cell: Some(pos),
                    cursor,
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
enum Value {
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
    for (i, c) in formula.chars().enumerate() {
        if alpha {
            if !c.is_iden() {
                alpha = false;
                v.push((&formula[start..i], start));
                start = i;
            }
        }
        if !alpha {
            if c.is_iden() {
                alpha = true;
                start = i;
                continue;
            }
            if !c.is_whitespace() {
                v.push((&formula[i..i + 1], i));
            }
        }
    }
    v.push((&formula[start..], start));
    v
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
                if tokens.is_empty() {
                    Ok(Self::Const(Const::Error(CellError::InvalidFormula {
                        cell: None,
                        cursor: 1,
                    })))
                } else if tokens.len() == 3 {
                    let t1 = if let Some(reff) = is_ref(tokens[0].0) {
                        Self::Ref(reff)
                    } else if let Ok(cn) = tokens[0].0.parse::<f64>() {
                        Self::Const(Const::Num(cn))
                    } else {
                        Self::Const(Const::Error(CellError::InvalidFormula {
                            cell: None,
                            cursor: tokens[0].1,
                        }))
                    };
                    let t3 = if let Some(reff) = is_ref(tokens[2].0) {
                        Self::Ref(reff)
                    } else if let Ok(cn) = tokens[2].0.parse::<f64>() {
                        Self::Const(Const::Num(cn))
                    } else {
                        Self::Const(Const::Error(CellError::InvalidFormula {
                            cell: None,
                            cursor: tokens[2].1,
                        }))
                    };
                    dbg!((&t1, tokens[1], &t3));
                    match tokens[1].0 {
                        "+" => Ok(Self::Add(Rc::new(t1), Rc::new(t3))),
                        "-" => Ok(Self::Sub(Rc::new(t1), Rc::new(t3))),
                        _ => Ok(Self::Const(Const::Error(CellError::InvalidFormula {
                            cell: None,
                            cursor: tokens[1].1,
                        }))),
                    }
                } else if tokens.len() == 1 {
                    if let Some(reff) = is_ref(tokens[0].0) {
                        Ok(Self::Ref(reff))
                    } else if let Ok(cn) = tokens[0].0.parse::<f64>() {
                        Ok(Self::Const(Const::Num(cn)))
                    } else {
                        Ok(Self::Const(Const::Error(CellError::InvalidFormula {
                            cell: None,
                            cursor: 1,
                        })))
                    }
                } else {
                    Ok(Self::Const(Const::Error(CellError::InvalidFormula {
                        cell: None,
                        cursor: tokens.last().expect("Wasnt empty").1,
                    })))
                }
            } else {
                Ok(Self::Const(Const::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: 0,
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
            Value::Sub(_, _) => todo!(),
            Value::Special(_) => todo!(),
            Value::Mul(_, _) => todo!(),
            Value::Div(_, _) => todo!(),
            Value::Lt(_, _) => todo!(),
            Value::Func { name: _, args: _ } => todo!(),
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
