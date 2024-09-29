use std::{borrow::Cow, collections::HashSet, fmt::Display, rc::Rc, str::FromStr, sync::Mutex};

use sdl2::{
    rect::Rect,
    render::TextureValueError,
    surface::Surface,
    ttf::{Font, FontError},
};

use crate::{SLoc, Sheet, SheetData, SheetEval, SheetFunc, SheetRanges, BLACK, RED, WHITE};

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Spec {
    dT,
    dC,
    dR,
    dSC,
    dSR,
    dST,
    dFd,
    dFC,
    dFR,
    dFT,
    dI,
    dH(u8, u8, u8),
    dA(usize),
}
#[derive(Clone, Debug)]
#[allow(non_snake_case)]
pub(crate) struct SpecValues {
    dT: &'static str,
    dC: i32,
    dR: i32,
    dSC: Option<i32>,
    dSR: Option<i32>,
    dST: Option<&'static str>,
    dFd: Option<Const>,
    dFC: Option<i32>,
    dFT: Option<&'static str>,
    dFR: Option<i32>,
    depth: usize,
}
impl SpecValues {
    pub(crate) fn from_sloc(sloc: SLoc) -> Self {
        Self {
            dC: sloc.1,
            dR: sloc.2,
            dSC: None,
            dSR: None,
            dFd: None,
            dFC: None,
            dFR: None,
            depth: 0,
            dT: sloc.0,
            dST: None,
            dFT: None,
        }
    }
    fn to_sloc(&self) -> SLoc {
        SLoc(self.dT, self.dC, self.dR)
    }
    fn with_sloc(self, sloc: SLoc) -> Self {
        Self {
            dC: sloc.1,
            dR: sloc.2,
            depth: self.depth + 1,
            dSC: None,
            dSR: None,
            dFd: None,
            dFC: None,
            dFR: None,
            dT: sloc.0,
            dST: None,
            dFT: None,
        }
    }
    fn with_f_sloc(self, f: Option<(SLoc, Const)>) -> Self {
        match f {
            Some(f) => Self {
                dFT: Some(f.0 .0),
                dFC: Some(f.0 .1),
                dFR: Some(f.0 .2),
                dFd: Some(f.1),
                depth: self.depth + 1,
                ..self
            },
            None => Self {
                dFT: None,
                dFC: None,
                dFR: None,
                dFd: None,
                depth: self.depth + 1,
                ..self
            },
        }
    }
    fn with_s_sloc(self, s: Option<SLoc>) -> Self {
        match s {
            Some(s) => Self {
                dST: Some(s.0),
                dSC: Some(s.1),
                dSR: Some(s.2),
                depth: self.depth + 1,
                ..self
            },
            None => Self {
                dST: None,
                dSC: None,
                dSR: None,
                depth: self.depth + 1,
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
    InvalidFunction {
        cell: Option<SLoc>,
        cursor: usize,
        reason: &'static str,
    },
}
impl Display for CellError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CellError::ReferenceLoop { loop_point } => {
                if let Some(c) = loop_point {
                    write!(f, "Circular reference at {}.", show_ref_global(c))
                } else {
                    write!(f, "Circular reference at unknown location.")
                }
            }
            CellError::InvalidFormula {
                cell,
                cursor,
                reason,
            } => {
                if let Some(c) = cell {
                    write!(
                        f,
                        "Formula error \"{reason}\" at position {}:{cursor}",
                        show_ref_global(c),
                    )
                } else {
                    write!(f, "Formula parsing failed on some cell.")
                }
            }
            CellError::BadArgument {
                cell,
                cursor,
                expected_types,
            } => {
                if let Some(c) = cell {
                    write!(
                        f,
                        "Expected arguments {expected_types:?} at position {}:{cursor}",
                        show_ref_global(c),
                    )
                } else {
                    write!(f, "Some bad argument somewhere")
                }
            }
            CellError::WrongNumArguments {
                cell,
                cursor,
                arguments,
            } => {
                if let Some(c) = cell {
                    write!(
                        f,
                        "Needs {arguments:?} arguments at cell {}:{cursor}",
                        show_ref_global(c)
                    )
                } else {
                    write!(f, "Wrong number of arguments somewhere")
                }
            }
            CellError::MissingCell { cell } => {
                if let Some(c) = cell {
                    write!(f, "Cell {} missing", show_ref_global(c),)
                } else {
                    write!(f, "Some cell is missing.")
                }
            }
            CellError::RangeIntersect {
                cell,
                source1,
                source2,
            } => {
                if let Some(c) = cell {
                    write!(
                        f,
                        "Cell {} appears in ranges of {} and {}.",
                        show_ref_global(c),
                        show_ref_global(source1),
                        show_ref_global(source2)
                    )
                } else {
                    write!(f, "Range intersection at unknown cell.")
                }
            }
            CellError::InvalidInfinity { cell, cursor } => {
                if let Some(c) = cell {
                    write!(f, "Cannot use infinity at {}:{cursor}", show_ref_global(c))
                } else {
                    write!(f, "Used infinity somewhere bad.")
                }
            }
            CellError::InvalidFunction {
                cell,
                cursor,
                reason,
            } => match cell {
                Some(c) => write!(
                    f,
                    "Invalid function at {}:{cursor}: {reason}",
                    show_ref_global(c)
                ),
                None => write!(f, "Invalid function: {reason}"),
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct SLocBound(pub Bound, pub Bound);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct Range(pub SLoc, pub SLocBound);
impl From<&SLoc> for Range {
    fn from(value: &SLoc) -> Self {
        Self(*value, SLocBound(Bound::Fin(value.1), Bound::Fin(value.2)))
    }
}
#[derive(Debug, PartialEq)]
struct FinRangeIter(Range, i32);
impl Iterator for FinRangeIter {
    type Item = SLoc;

    fn next(&mut self) -> Option<Self::Item> {
        let Bound::Fin(w) = self.0.width() else {
            panic!("Infinite range in FinRangeIter")
        };
        let Bound::Fin(h) = self.0.height() else {
            panic!("Infinite range in FinRangeIter")
        };
        if self.1 == w * h {
            return None;
        }
        let rh = self.1 / w + self.0 .0 .2;
        let rw = self.1 % w + self.0 .0 .1;
        self.1 += 1;
        Some(SLoc(self.0 .0 .0, rw, rh))
    }
}
impl TryFrom<Range> for FinRangeIter {
    type Error = &'static str;

    fn try_from(value: Range) -> Result<Self, Self::Error> {
        if value.is_inf() {
            Err("Not a finite range")
        } else {
            Ok(FinRangeIter(value, 0))
        }
    }
}
impl Range {
    fn new(cell1: SLoc, cell2: SLoc) -> Self {
        Self(cell1, SLocBound(Bound::Fin(cell2.1), Bound::Fin(cell2.2)))
    }
    fn new_inf(cell: SLoc) -> Self {
        Self(cell, SLocBound(Bound::Inf, Bound::Inf))
    }
    fn in_range(&self, cell: &SLoc) -> bool {
        if cell.1 < self.0 .1 || cell.2 < self.0 .2 || self.0 .0 != cell.0 {
            false
        } else {
            self.1 .0.ge(cell.1) && self.1 .1.ge(cell.2)
        }
    }
    fn into_inf(self) -> Self {
        Self(self.0, SLocBound(Bound::Inf, Bound::Inf))
    }
    fn is_inf(&self) -> bool {
        matches!(self.1, SLocBound(Bound::Inf, _) | SLocBound(_, Bound::Inf))
    }
    fn width(&self) -> Bound {
        let lb = self.0 .1;
        let Bound::Fin(ub) = self.1 .0 else {
            return Bound::Inf;
        };
        Bound::Fin(ub - lb + 1)
    }
    fn height(&self) -> Bound {
        let lb = self.0 .1;
        let Bound::Fin(ub) = self.1 .1 else {
            return Bound::Inf;
        };
        Bound::Fin(ub - lb + 1)
    }
    fn intersect(&self, other: &Range) -> Range {
        if self.0 .0 != other.0 .0 {
            Range(self.0, SLocBound(Bound::Fin(-1), Bound::Fin(-1)))
        } else {
            Range(
                SLoc(
                    self.0 .0,
                    self.0 .1.max(other.0 .1),
                    self.0 .2.max(other.0 .2),
                ),
                SLocBound(self.1 .0.min(other.1 .0), self.1 .1.min(other.1 .1)),
            )
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum ConstType {
    Num(f64),
    String(String),
    Table(&'static str),
    Bool(bool),
    Error(CellError),
    Range(Range),
    Color((u8, u8, u8)),
    Function(Box<Value>),
}
impl ConstType {
    fn parse_exact(s: &str) -> Self {
        if let Ok(n) = s.parse() {
            Self::Num(n)
        } else if let Ok(b) = s.parse() {
            Self::Bool(b)
        } else {
            Self::String(s.to_owned())
        }
    }
    // fn is_err(&self) -> bool {
    //     matches!(self, Self::Error(_))
    // }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Const {
    ct: ConstType,
    pub(crate) bgcolor: (u8, u8, u8),
    textcolor: (u8, u8, u8),
}
impl From<ConstType> for Const {
    fn from(val: ConstType) -> Self {
        if matches!(val, ConstType::Error(_)) {
            Const {
                ct: val,
                bgcolor: WHITE.rgb(),
                textcolor: RED.rgb(),
            }
        } else {
            Const {
                ct: val,
                bgcolor: WHITE.rgb(),
                textcolor: BLACK.rgb(),
            }
        }
    }
}
impl Default for Const {
    fn default() -> Self {
        Self {
            ct: ConstType::String("".into()),
            bgcolor: WHITE.rgb(),
            textcolor: BLACK.rgb(),
        }
    }
}
impl Const {
    #[cfg(test)]
    fn default_err() -> Self {
        Self {
            ct: ConstType::Error(CellError::MissingCell { cell: None }),
            bgcolor: WHITE.rgb(),
            textcolor: RED.rgb(),
        }
    }
    fn is_error(&self) -> bool {
        matches!(self.ct, ConstType::Error(_))
    }
    fn is_function(&self) -> bool {
        matches!(self.ct, ConstType::Function(_))
    }
}

impl From<&ConstType> for String {
    fn from(val: &ConstType) -> Self {
        match val {
            ConstType::Num(n) => format!("{n}"),
            ConstType::String(s) => s.clone(),
            ConstType::Bool(b) => format!("{b}"),
            ConstType::Error(e) => format!("{e}"),
            ConstType::Range(_) => "Range is not valid here".to_string(),
            ConstType::Color((r, g, b)) => format!("Color #{r},{g},{b}"),
            ConstType::Function(_) => format!("Function"),
            ConstType::Table(t) => format!("'{t}'"),
        }
    }
}
pub(crate) trait RenderColor<T> {
    type Err;
    fn render_c(&self, r: T) -> Result<Surface, Self::Err>;
}
#[allow(dead_code)]
pub(crate) enum RenderError {
    FontError(FontError),
    TextureValueError(TextureValueError),
    String(String),
}
impl From<FontError> for RenderError {
    fn from(value: FontError) -> Self {
        Self::FontError(value)
    }
}
impl From<TextureValueError> for RenderError {
    fn from(value: TextureValueError) -> Self {
        Self::TextureValueError(value)
    }
}
impl From<String> for RenderError {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}
impl RenderColor<&Const> for Font<'_, '_> {
    fn render_c(&self, r: &Const) -> Result<Surface<'_>, RenderError> {
        let txt = self
            .render(&<&ConstType as Into<String>>::into(&r.ct))
            .solid(r.textcolor)?;
        let mut bg = Surface::new(
            txt.width(),
            txt.height(),
            sdl2::pixels::PixelFormatEnum::RGB888,
        )?;
        let fullrect = Rect::new(0, 0, bg.width(), bg.height());
        bg.fill_rect(fullrect, r.bgcolor.into())?;
        txt.blit(fullrect, &mut bg, fullrect)?;
        Ok(bg)
    }

    type Err = RenderError;
}
impl From<ConstType> for String {
    fn from(val: ConstType) -> Self {
        (&val).into()
    }
}
impl FromStr for ConstType {
    type Err = ConstType;

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
        } else if s.len() > 1
            && s.chars().next().is_some_and(|c| c == '\'')
            && s.chars().last().is_some_and(|c| c == '\'')
        {
            Ok(Self::Table(s[1..s.len() - 1].to_owned().leak()))
        } else {
            Err(Self::Error(CellError::InvalidFormula {
                cell: None,
                cursor: 0,
                reason: "Not valid constant",
            }))
        }
    }
}
impl FromStr for Const {
    type Err = Self;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        ConstType::from_str(s)
            .map(ConstType::into)
            .map_err(ConstType::into)
    }
}
impl TryInto<Bound> for &ConstType {
    type Error = ConstType;

    fn try_into(self) -> Result<Bound, <Self as TryInto<Bound>>::Error> {
        match self {
            ConstType::Error(CellError::InvalidInfinity { cell: _, cursor: _ }) => Ok(Bound::Inf),
            ConstType::Num(n) if n >= &0. && &n.round() == n => Ok(Bound::Fin(*n as i32)),
            _ => Err(ConstType::Error(CellError::BadArgument {
                cell: None,
                cursor: 0,
                expected_types: vec!["Whole number".into(), ".I".into()],
            })),
        }
    }
}

impl ConstType {
    fn into_pos(self, pos: SLoc) -> Self {
        match self {
            Self::Error(e) => Self::Error(match e {
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
                CellError::InvalidFunction {
                    cell: None,
                    cursor,
                    reason,
                } => CellError::InvalidFunction {
                    cell: Some(pos),
                    cursor,
                    reason,
                },
                CellError::InvalidInfinity { cell: None, cursor } => CellError::InvalidInfinity {
                    cell: Some(pos),
                    cursor,
                },
                CellError::WrongNumArguments {
                    cell: None,
                    cursor,
                    arguments,
                } => CellError::WrongNumArguments {
                    cell: Some(pos),
                    cursor,
                    arguments,
                },
                _ => e,
            }),
            _ => self,
        }
    }
    fn truthy(&self) -> bool {
        match self {
            ConstType::Num(n) => n != &0.0,
            ConstType::String(s) => !s.is_empty(),
            ConstType::Bool(b) => *b,
            ConstType::Error(_) => false,
            ConstType::Range(_) => false,
            ConstType::Color(_) => false,
            ConstType::Function(_) => false,
            ConstType::Table(_) => true,
        }
    }
    // fn to_pos(&self, pos: SLoc) -> Self {
    //     self.clone().into_pos(pos)
    // }
    #[cfg(test)]
    fn merge_err(&self, rhs: &Self) -> Option<CellError> {
        if let Self::Error(e) = self {
            Some(e.clone())
        } else if let Self::Error(e) = rhs.clone() {
            Some(e.clone())
        } else {
            None
        }
    }
}
impl Const {
    fn into_pos(self, pos: SLoc) -> Self {
        Self {
            ct: self.ct.into_pos(pos),
            bgcolor: self.bgcolor,
            textcolor: self.textcolor,
        }
    }
    fn to_pos(&self, pos: SLoc) -> Self {
        self.clone().into_pos(pos)
    }
    fn merge_err(&self, rhs: &Self) -> Option<Self> {
        if let ConstType::Error(_) = &self.ct {
            Some(self.clone())
        } else if let ConstType::Error(_) = &rhs.ct {
            Some(rhs.clone())
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
    Gt(Rc<Value>, Rc<Value>),
    Eq(Rc<Value>, Rc<Value>),
    Func { name: String, args: Vec<Value> },
    RangeForm(Rc<RangeFormula>),
}

trait IsIdentifier {
    fn is_iden(&self) -> bool;
}
impl IsIdentifier for char {
    fn is_iden(&self) -> bool {
        self.is_alphanumeric() || self == &'.' || self == &':' || self == &'\''
    }
}

fn form_tokens(formula: &str) -> Vec<(&str, usize)> {
    let mut v = vec![];
    let mut start = 0;
    let mut alpha = true;

    let mut string_mode: Option<String> = None;
    let mut escape_mode = false;
    let mut filename_mode = false;
    for (i, c) in formula.chars().enumerate() {
        if alpha {
            // println!("c:{c} mode: {filename_mode}");
            if filename_mode {
                if c == '\'' {
                    filename_mode = false;
                }
            } else if c == '\'' {
                filename_mode = true;
            } else if !c.is_iden() {
                // println!("token from form_tokens {}", &formula[start..i]);
                alpha = false;
                v.push((&formula[start..i], start));
                start = i;
            }
        }
        if !alpha {
            if c == '"' || string_mode.is_some() {
                // println!("c:{c} esc:{escape_mode}");
                if let Some(mut string_val) = string_mode {
                    if c == '"' && string_val.chars().next().unwrap() == '"' && !escape_mode {
                        string_mode = None;
                        string_val.push('"');
                        v.push((string_val.leak(), start));
                        start = i + 1;
                    } else if c == '\\' && !escape_mode {
                        string_mode = Some(string_val);
                        escape_mode = true;
                    } else {
                        string_val.push(c);
                        string_mode = Some(string_val);
                        escape_mode = false;
                    }
                } else if c == '"' {
                    string_mode = Some("\"".into());
                }
            } else {
                if c.is_iden() {
                    let i = if c.is_digit(10) && !v.last().is_some_and(|t| t.0 != "-") {
                        if matches!(v.get(v.len() - 2), Some(("" | "(" | ",", _))) {
                            if let Some((_, m)) = v.pop() {
                                if m == 0 {
                                    v.pop();
                                }
                                m
                            } else {
                                0
                            }
                        } else {
                            i
                        }
                    } else {
                        i
                    };
                    alpha = true;
                    start = i;
                    if c == '\'' {
                        // start = i + 1;
                        filename_mode = true;
                    }
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
    // dbg!(&v);
    v
}

// #[allow(dead_code)]
// fn form_tree_basic<'a>(tokens: Vec<(&'a str, usize)>) -> Value {
//     if tokens.is_empty() {
//         Value::Const(
//             ConstType::Error(CellError::InvalidFormula {
//                 cell: None,
//                 cursor: 1,
//                 reason: "Nothing after =",
//             })
//             .into(),
//         )
//     } else if tokens.len() == 3 {
//         let t1 = if let Some(reff) = is_ref(tokens[0].0) {
//             Value::Ref(reff)
//         } else if let Ok(cn) = tokens[0].0.parse::<f64>() {
//             Value::Const(ConstType::Num(cn).into())
//         } else {
//             Value::Const(
//                 ConstType::Error(CellError::InvalidFormula {
//                     cell: None,
//                     cursor: tokens[0].1,
//                     reason: "Token not value or reference",
//                 })
//                 .into(),
//             )
//         };
//         let t3 = if let Some(reff) = is_ref(tokens[2].0) {
//             Value::Ref(reff)
//         } else if let Ok(cn) = tokens[2].0.parse::<f64>() {
//             Value::Const(ConstType::Num(cn).into())
//         } else {
//             Value::Const(
//                 ConstType::Error(CellError::InvalidFormula {
//                     cell: None,
//                     cursor: tokens[2].1,
//                     reason: "Token not value or reference",
//                 })
//                 .into(),
//             )
//         };
//         dbg!((&t1, tokens[1], &t3));
//         match tokens[1].0 {
//             "+" => Value::Add(Rc::new(t1), Rc::new(t3)),
//             // "-" => Value::Sub(Rc::new(t1), Rc::new(t3)),
//             _ => Value::Const(
//                 ConstType::Error(CellError::InvalidFormula {
//                     cell: None,
//                     cursor: tokens[1].1,
//                     reason: "Operation not allowed",
//                 })
//                 .into(),
//             ),
//         }
//     } else if tokens.len() == 1 {
//         if let Some(reff) = is_ref(tokens[0].0) {
//             Value::Ref(reff)
//         } else if let Ok(cn) = tokens[0].0.parse::<f64>() {
//             Value::Const(ConstType::Num(cn).into())
//         } else {
//             Value::Const(
//                 ConstType::Error(CellError::InvalidFormula {
//                     cell: None,
//                     cursor: 1,
//                     reason: "Token not value or reference",
//                 })
//                 .into(),
//             )
//         }
//     } else {
//         Value::Const(
//             ConstType::Error(CellError::InvalidFormula {
//                 cell: None,
//                 cursor: tokens.last().expect("Wasnt empty").1,
//                 reason: "Invalid number of tokens",
//             })
//             .into(),
//         )
//     }
// }

#[derive(Debug, Clone, PartialEq)]
enum ParenTree<'a> {
    Paren(Cow<'a, [ParenTree<'a>]>),
    Token(&'a str, usize),
}

// enum ParenTreeIter<'a, 'b> {
//     Cow(std::slice::Iter<'b, ParenTree<'a>>),
//     Array(std::array::IntoIter<&'b ParenTree<'a>, 1>),
// }

// impl<'a, 'b> Iterator for ParenTreeIter<'a, 'b> {
//     type Item = &'b ParenTree<'a>;

//     fn next(&mut self) -> Option<Self::Item> {
//         match self {
//             ParenTreeIter::Cow(c) => c.next(),
//             ParenTreeIter::Array(a) => a.next(),
//         }
//     }
// }

enum ParenTreeIterSplit<'a, 'b, F: Fn(&ParenTree<'a>) -> bool> {
    Cow(&'b [ParenTree<'a>], usize, F),
    Array([ParenTree<'a>; 1], bool),
}

impl<'a, 'b, F: Fn(&ParenTree<'a>) -> bool> Iterator for ParenTreeIterSplit<'a, 'b, F> {
    type Item = Cow<'b, [ParenTree<'a>]>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ParenTreeIterSplit::Cow(c, i, split) => {
                let start = *i;
                let leng = c[start..].iter().position(split);
                if let Some(leng) = leng {
                    let end = start + leng;
                    *i = end + 1;
                    Some(Cow::Borrowed(&c[start..end]))
                } else if start != c.len() {
                    *i = c.len();
                    Some(Cow::Borrowed(&c[start..]))
                } else {
                    None
                }
            }
            ParenTreeIterSplit::Array(a, d) => {
                if !*d {
                    *d = true;
                    Some(Cow::Owned(a.to_vec()))
                } else {
                    None
                }
            }
        }
    }
}

impl<'a> ParenTree<'a> {
    fn downgrade<'b>(&'b self) -> &'b ParenTree<'b>
    where
        'a: 'b,
    {
        // Safety: This must work since if a reference is valid for 'a, it is valid for at least 'b
        unsafe { std::mem::transmute(self) }
    }
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
        matches!(self, ParenTree::Token(",", _))
    }
    fn is_mul_args(&self) -> bool {
        match self {
            ParenTree::Paren(p) => p.iter().skip(1).any(|t| t.is_comma()),
            ParenTree::Token(_, _) => false,
        }
    }
    #[cfg(test)]
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
    // fn iter<'b>(&'b self) -> ParenTreeIter<'a, 'b> {
    //     match self {
    //         ParenTree::Paren(p) => ParenTreeIter::Cow(p.iter()),
    //         ParenTree::Token(_, _) => ParenTreeIter::Array([self].into_iter()),
    //     }
    // }
    fn split<'b, F: Fn(&ParenTree<'a>) -> bool>(
        &'b self,
        split: F,
    ) -> ParenTreeIterSplit<'a, 'b, F> {
        match self {
            ParenTree::Paren(p) => ParenTreeIterSplit::Cow(&p, 0, split),
            ParenTree::Token(_, _) => ParenTreeIterSplit::Array([self.clone()], false),
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

fn split_parens<'a>(tokens: &[(&'a str, usize)]) -> Result<ParenTree<'a>, Value> {
    let mut split_par_vec = vec![];
    let mut pcount = 0;
    let mut start = 0;
    // println!("{tokens:?} tokens");
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
                return Err(Value::Const(
                    ConstType::Error(CellError::InvalidFormula {
                        cell: None,
                        cursor: t.1,
                        reason: "Ending parenthesis with no opening",
                    })
                    .into(),
                ));
            }
            if pcount == 0 {
                if let Some(ps) = pstart {
                    split_par_vec.push(split_parens(&tokens[ps + 1..i])?);
                    pstart = None;
                } else {
                    split_par_vec.push(split_parens(&tokens[start..=i])?);
                }
                start = i + 1;
            }
        }
        if pcount != 0 {
            Err(Value::Const(
                ConstType::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: tokens.last().unwrap().1,
                    reason: "Missing closing parenthesis",
                })
                .into(),
            ))
        } else {
            Ok(ParenTree::Paren(Cow::Owned(split_par_vec)))
        }
    }
}

// TODO functions with no arguments
// TODO parse ranges of cells
fn form_tree<'c, 'b>(split_par: &'b ParenTree<'c>, in_sheet: &'static str) -> (Value, bool)
where
{
    if split_par.len() == 0 {
        return (
            Value::Const(
                ConstType::Error(CellError::InvalidFormula {
                    cell: None,
                    cursor: 0,
                    reason: "No formula given",
                })
                .into(),
            ),
            false,
        );
    }
    // dbg!(&split_par);
    if let ParenTree::Token(sp, i) = split_par {
        return if let Some(r) = is_ref(sp, in_sheet) {
            (Value::Ref(r), false)
        } else if let Some(c) = is_const(sp) {
            (Value::Const(c), false)
        } else if let Some(s) = is_spec(sp) {
            (Value::Special(s), false)
        } else if let Some(r) = is_range(sp, in_sheet) {
            (Value::Const(ConstType::Range(r).into()), false)
        } else {
            // dbg!(sp);
            (
                Value::Const(
                    ConstType::Error(CellError::InvalidFormula {
                        cell: None,
                        cursor: *i,
                        reason: "Couldn't parse token",
                    })
                    .into(),
                ),
                false,
            )
        };
    }
    match split_par.downgrade::<'b>() {
        ParenTree::Token(_, _) => {
            panic!("Other case already checked");
        }
        ParenTree::Paren(split_v) => {
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
                        // dbg!(&v);
                        if v.is_mul_args() {
                            // TODO dont do whatever it is here, need to fix split_parens
                            let c = CatParsed::Args({
                                // dbg!(&vargs);

                                let c: Vec<_> = v
                                    .split(|varg| varg.is_comma())
                                    .map(|varg| {
                                        let x = ParenTree::Paren(varg);
                                        let (v, _) = form_tree(&x, in_sheet);
                                        v
                                    })
                                    .collect();
                                c
                            });
                            c
                        } else {
                            // println!("As val");
                            CatParsed::Value(form_tree(v, in_sheet))
                        }
                    }
                    Category::Operand => CatParsed::Operand(v.as_op().unwrap()),
                    Category::Function => CatParsed::Function(v.as_func().unwrap()),
                })
                .peekable();

            // let p2 = parseds.clone();
            // println!("Parseds:");
            // for e in p2 {
            //     dbg!(e);
            // }
            // println!("Parsed End");
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
                            Value::Const(
                                ConstType::Error(CellError::InvalidFormula {
                                    cell: None,
                                    cursor: lp.1,
                                    reason: "Function has no arguments",
                                })
                                .into(),
                            ),
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
                                Value::Const(
                                    ConstType::Error(CellError::InvalidFormula {
                                        cell: None,
                                        cursor: lp.1,
                                        reason: "Invalid function arguments",
                                    })
                                    .into(),
                                ),
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
                        Value::Const(
                            ConstType::Error(CellError::InvalidFormula {
                                cell: None,
                                cursor: lp.1,
                                reason: "Arguments with no function",
                            })
                            .into(),
                        ),
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
                            Value::Const(
                                ConstType::Error(CellError::InvalidFormula {
                                    cell: None,
                                    cursor: e.1,
                                    reason: "Found operand where value was expected",
                                })
                                .into(),
                            ),
                            false,
                        );
                    }
                } else if matches!(e.0, CatParsed::Value(_)) {
                    return (
                        Value::Const(
                            ConstType::Error(CellError::InvalidFormula {
                                cell: None,
                                cursor: e.1,
                                reason: "Found value where operand was expected",
                            })
                            .into(),
                        ),
                        false,
                    );
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
                        Value::Const(
                            ConstType::Error(CellError::InvalidFormula {
                                cell: None,
                                cursor: lhs.1,
                                reason: "Left side of mul/div was not value",
                            })
                            .into(),
                        ),
                        false,
                    );
                };
                if token_orchard.len() <= p {
                    return (
                        Value::Const(
                            ConstType::Error(CellError::InvalidFormula {
                                cell: None,
                                cursor: 0,
                                reason: "Right side of mul/div not found",
                            })
                            .into(),
                        ),
                        false,
                    );
                }
                let rhs = token_orchard.remove(p);
                let CatParsed::Value(rhs) = rhs.0 else {
                    return (
                        Value::Const(
                            ConstType::Error(CellError::InvalidFormula {
                                cell: None,
                                cursor: rhs.1,
                                reason: "Right side of mul/div was not value",
                            })
                            .into(),
                        ),
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
                        Value::Const(
                            ConstType::Error(CellError::InvalidFormula {
                                cell: None,
                                cursor: lhs.1,
                                reason: "Left side of add/sub was not value",
                            })
                            .into(),
                        ),
                        false,
                    );
                };
                if token_orchard.len() <= p {
                    return (
                        Value::Const(
                            ConstType::Error(CellError::InvalidFormula {
                                cell: None,
                                cursor: 0,
                                reason: "Right side of add/sub not found",
                            })
                            .into(),
                        ),
                        false,
                    );
                }
                let rhs = token_orchard.remove(p);
                let CatParsed::Value(rhs) = rhs.0 else {
                    return (
                        Value::Const(
                            ConstType::Error(CellError::InvalidFormula {
                                cell: None,
                                cursor: rhs.1,
                                reason: "Right side of add/sub was not value",
                            })
                            .into(),
                        ),
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
                    .position(|(x, _)| matches!(x, CatParsed::Operand('<' | '=' | '>')))
            }
            // Parse < and =
            while let Some(p) = has_comp(&token_orchard) {
                // let p = 1;
                let inpos = p - 1;
                let lhs = token_orchard.remove(p - 1);
                let (CatParsed::Value(lhs), i) = lhs else {
                    return (
                        Value::Const(
                            ConstType::Error(CellError::InvalidFormula {
                                cell: None,
                                cursor: lhs.1,
                                reason: "Left side of equality/inequality was not value",
                            })
                            .into(),
                        ),
                        false,
                    );
                };
                if token_orchard.len() <= p {
                    return (
                        Value::Const(
                            ConstType::Error(CellError::InvalidFormula {
                                cell: None,
                                cursor: 0,
                                reason: "Right side of equality/inequality not found",
                            })
                            .into(),
                        ),
                        false,
                    );
                }
                let rhs = token_orchard.remove(p);
                let CatParsed::Value(rhs) = rhs.0 else {
                    return (
                        Value::Const(
                            ConstType::Error(CellError::InvalidFormula {
                                cell: None,
                                cursor: rhs.1,
                                reason: "Right side of equality/inequality was not value",
                            })
                            .into(),
                        ),
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
                } else if matches!(token_orchard[inpos].0, CatParsed::Operand('>')) {
                    token_orchard[inpos] = (
                        CatParsed::Value((Value::Gt(lhs.0.into(), rhs.0.into()), false)),
                        i,
                    );
                } else {
                    return (
                        Value::Const(
                            ConstType::Error(CellError::InvalidFormula {
                                cell: None,
                                cursor: token_orchard[inpos].1,
                                reason: "Operand is not supported",
                            })
                            .into(),
                        ),
                        false,
                    );
                }
            }

            let (CatParsed::Value((ret, _)), _) = token_orchard[0].clone() else {
                return (
                    Value::Const(
                        ConstType::Error(CellError::InvalidFormula {
                            cell: None,
                            cursor: token_orchard[0].1,
                            reason: "Something weird happened idk",
                        })
                        .into(),
                    ),
                    false,
                );
            };
            (ret, false)
        }
    }
}

fn letters_to_row(token: &str) -> Option<i32> {
    let mut s = 0;
    for c in token.chars() {
        // dbg!(s);
        s = s * 26 + (c.to_digit(36)? - 9);
    }
    Some((s - 1) as i32)
}

fn is_ref(token: &str, in_sheet: &'static str) -> Option<SLoc> {
    match token.chars().next() {
        Some(c) if c.is_alphabetic() => {
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
                Some(SLoc(in_sheet, (col - 1) as i32, (row - 1) as i32))
            }
        }
        Some('\'') => {
            // println!("{token}");
            let mut f = false;
            let mut col = 0;
            let mut row = 0;

            let Some((endt, _)) = token.chars().enumerate().filter(|(_, c)| c == &'\'').nth(1)
            else {
                return None;
            };
            let sheet = token[1..endt].to_string().leak();
            // println!("Sheet {sheet}");

            for c in token.chars().skip(endt + 1) {
                // println!("taking {c}");
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
                Some(SLoc(sheet, (col - 1) as i32, (row - 1) as i32))
            }
        }
        _ => None,
    }
}

pub(crate) fn show_ref_local(loc: &SLoc) -> String {
    format!("{}{}", show_col(loc.1), loc.2 + 1)
}
pub(crate) fn show_ref_global(loc: &SLoc) -> String {
    format!("'{}'{}{}", loc.0, show_col(loc.1), loc.2 + 1)
}

pub(crate) fn show_col(x: i32) -> String {
    let mut c = String::new();
    let mut cn = x + 1;
    while cn > 0 {
        c.insert(0, (b'A' + ((cn - 1) % 26) as u8) as char);
        cn = (cn - 1) / 26;
    }
    c
}

fn is_const(token: &str) -> Option<Const> {
    ConstType::from_str(token).ok().map(ConstType::into)
}
fn is_range(token: &str, in_sheet: &'static str) -> Option<Range> {
    let mut split = token.split(':');
    let start = is_ref(split.next()?, in_sheet)?;
    if let Some(nx) = split.next() {
        if nx.is_empty() {
            Some(Range::new_inf(start))
        } else if let Ok(n) = nx.parse::<i32>() {
            // println!("{n} {start:?}");
            if n - 1 < start.2 {
                None
            } else {
                Some(Range(start, SLocBound(Bound::Inf, Bound::Fin(n - 1))))
            }
        } else if nx.chars().all(char::is_alphabetic) {
            let c = letters_to_row(nx)?;
            // println!("{c} {start:?}");
            if c < start.1 {
                None
            } else {
                Some(Range(start, SLocBound(Bound::Fin(c), Bound::Inf)))
            }
        } else {
            let end = is_ref(nx, in_sheet)?;
            if split.next().is_some() || end.1 < start.1 || end.2 < start.2 {
                None
            } else {
                Some(Range::new(start, end))
            }
        }
    } else {
        None
    }
}
fn is_spec(token: &str) -> Option<Spec> {
    match token {
        token if token.len() < 2 => None,
        // ".." => Some(Spec::dd),
        ".R" => Some(Spec::dR),
        ".C" => Some(Spec::dC),
        ".T" => Some(Spec::dT),
        ".SR" => Some(Spec::dSR),
        ".SC" => Some(Spec::dSC),
        ".ST" => Some(Spec::dST),
        // ".S." => Some(Spec::dSd),
        ".FR" => Some(Spec::dFR),
        ".FC" => Some(Spec::dFC),
        ".FT" => Some(Spec::dFT),
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
        _ if &token[..2] == ".A" => {
            Some(Spec::dA(token[2..].parse::<usize>().ok()?.checked_sub(1)?))
        }
        _ => None,
    }
}

impl Value {
    pub(crate) fn from_str(s: &str, in_sheet: &'static str) -> Result<Self, &'static str> {
        if !matches!(s.chars().next(), Some('=') | Some('[')) {
            Ok(Value::Const(ConstType::parse_exact(s).into()))
        } else if let Some(form) = s.strip_prefix('=') {
            // dbg!(&s[1..]);
            let tokens = form_tokens(form);
            // dbg!(&tokens);
            let Ok(split) = split_parens(&tokens) else {
                let _ = dbg!(split_parens(&tokens));
                return Err("Couldnt parenthesize");
            };
            Ok(form_tree(&split, in_sheet).0)
        } else {
            Ok(Value::RangeForm(Rc::new(is_range_formula(s, in_sheet)?)))
        }
    }
}

impl Value {
    fn is_function(&self) -> bool {
        match self {
            Value::Const(c) => c.is_function(),
            Value::Ref(_) => false,
            Value::Special(s) => matches!(s, Spec::dA(_)),
            Value::Add(l, r)
            | Value::Sub(l, r)
            | Value::Mul(l, r)
            | Value::Div(l, r)
            | Value::Lt(l, r)
            | Value::Gt(l, r)
            | Value::Eq(l, r) => l.is_function() || r.is_function(),
            Value::Func { name: _, args } => args.iter().any(|a| a.is_function()),
            Value::RangeForm(_) => false,
        }
    }

    /// Whenever the `spec` variable changes in a recursive call, depth should be incremented
    pub(crate) fn eval<'a>(
        &'a self,
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &'a SpecValues,
    ) -> Cow<'a, Const> {
        // ConstType::String("".into().into())
        let l = spec.to_sloc();
        if self.is_function() {
            return Cow::Owned(ConstType::Function(Box::new(self.clone())).into());
        }
        if eval.get(&l).is_some_and(|x| spec.invalid_depth(*x)) {
            // dbg!(spec, eval.get(&l));
            if let Some(x) = eval.get_mut(&l) {
                *x = 0;
            }
            return Cow::Owned(
                ConstType::Error(CellError::ReferenceLoop {
                    loop_point: Some(l),
                })
                .into(),
            );
        }
        if let std::collections::btree_map::Entry::Vacant(e) = eval.entry(l) {
            e.insert(spec.depth);
        } else if let Some(x) = eval.get_mut(&l) {
            *x = spec.depth;
        }
        let t = match self {
            // Value::Const(Const {
            //     ct: ConstType::Range(_),
            //     bgcolor: _,
            //     textcolor: _,
            // }) => Cow::Owned(
            //     ConstType::Error(CellError::InvalidFormula {
            //         cell: Some(spec.to_sloc()),
            //         cursor: 0,
            //         reason: "Range cannot be assigned to a cell",
            //     })
            //     .into(),
            // ),
            Value::Const(c) => Cow::Borrowed(c),
            Value::Ref(r) => Cow::Owned({
                if matches!(get_range_form(ranges, r), Ok(Some(rf)) if rf !=r) {
                    let Ok(Some(rf)) = get_range_form(ranges, r) else {
                        unreachable!("We confirmed this in the match above");
                    };
                    let vr = data
                        .get(rf, eval)
                        .map(|d| {
                            let new_spec = spec.clone().with_sloc(*r).with_s_sloc(Some(*rf));
                            if eval.get(r).is_some_and(|x| spec.invalid_depth(*x)) {
                                ConstType::Error(CellError::ReferenceLoop {
                                    loop_point: Some(*r),
                                })
                                .into()
                            } else {
                                Value::from_str(&d.val, rf.0)
                                    .unwrap()
                                    .eval(data, eval, func, ranges, &new_spec)
                                    .into_owned()
                                    .into_pos(*r)
                            }
                        })
                        .map_err(|e| ConstType::Error(e).into())
                        .unwrap_or_else(|e| e);
                    vr
                } else {
                    let vr = data
                        .get(r, eval)
                        .map(|d| {
                            let new_spec = spec.clone().with_sloc(*r);
                            if eval.get(r).is_some_and(|x| spec.invalid_depth(*x)) {
                                ConstType::Error(CellError::ReferenceLoop {
                                    loop_point: Some(*r),
                                })
                                .into()
                            } else {
                                Value::from_str(&d.val, r.0)
                                    .unwrap()
                                    .eval(data, eval, func, ranges, &new_spec)
                                    .into_owned()
                                    .into_pos(*r)
                            }
                        })
                        .map_err(|e| ConstType::Error(e).into())
                        .unwrap_or_else(|e| e);
                    vr
                }
            }),
            Value::Add(lhs, rhs) => {
                let lhs_eval = lhs.eval(data, eval, func, ranges, spec);
                let rhs_eval = rhs.eval(data, eval, func, ranges, spec);
                if let Some(e) = lhs_eval.merge_err(&rhs_eval) {
                    Cow::Owned(e)
                } else if let (ConstType::Num(lnum), ConstType::Num(rnum)) =
                    (&lhs_eval.as_ref().ct, &rhs_eval.as_ref().ct)
                {
                    Cow::Owned(ConstType::Num(lnum + rnum).into())
                } else {
                    Cow::Owned(
                        ConstType::Error(CellError::BadArgument {
                            cell: Some(spec.to_sloc()),
                            cursor: 0,
                            expected_types: vec!["Num".into(), "Num".into()],
                        })
                        .into(),
                    )
                }
            }
            Value::Sub(lhs, rhs) => {
                let lhs_eval = lhs.eval(data, eval, func, ranges, spec);
                let rhs_eval = rhs.eval(data, eval, func, ranges, spec);
                if let Some(e) = lhs_eval.merge_err(&rhs_eval) {
                    Cow::Owned(e)
                } else if let (ConstType::Num(lnum), ConstType::Num(rnum)) =
                    (&lhs_eval.as_ref().ct, &rhs_eval.as_ref().ct)
                {
                    Cow::Owned(ConstType::Num(lnum - rnum).into())
                } else {
                    Cow::Owned(
                        ConstType::Error(CellError::BadArgument {
                            cell: Some(spec.to_sloc()),
                            cursor: 0,
                            expected_types: vec!["Num".into(), "Num".into()],
                        })
                        .into(),
                    )
                }
            }
            Value::Special(s) => match s {
                Spec::dC => Cow::Owned(ConstType::Num(spec.dC as f64).into()),
                Spec::dR => Cow::Owned(ConstType::Num(spec.dR as f64).into()),
                Spec::dSC => Cow::Owned(if let Some(v) = spec.dSC {
                    ConstType::Num(v as f64).into()
                } else {
                    ConstType::Error(CellError::InvalidFormula {
                        cell: Some(spec.to_sloc()),
                        cursor: 0,
                        reason: "S is only valid in range formulas",
                    })
                    .into()
                }),
                Spec::dSR => Cow::Owned(if let Some(v) = spec.dSR {
                    ConstType::Num(v as f64).into()
                } else {
                    ConstType::Error(CellError::InvalidFormula {
                        cell: Some(spec.to_sloc()),
                        cursor: 0,
                        reason: "S is only valid in range formulas",
                    })
                    .into()
                }),
                Spec::dFd => {
                    if let Some(ref v) = spec.dFd {
                        Cow::Borrowed(v)
                    } else {
                        Cow::Owned(
                            ConstType::Error(CellError::InvalidFormula {
                                cell: Some(spec.to_sloc()),
                                cursor: 0,
                                reason: "F is only valid in range functions",
                            })
                            .into(),
                        )
                    }
                }
                Spec::dFC => Cow::Owned(if let Some(v) = spec.dFC {
                    ConstType::Num(v as f64).into()
                } else {
                    ConstType::Error(CellError::InvalidFormula {
                        cell: Some(spec.to_sloc()),
                        cursor: 0,
                        reason: "F is only valid in range functions",
                    })
                    .into()
                }),
                Spec::dFR => Cow::Owned(if let Some(v) = spec.dFR {
                    ConstType::Num(v as f64).into()
                } else {
                    ConstType::Error(CellError::InvalidFormula {
                        cell: Some(spec.to_sloc()),
                        cursor: 0,
                        reason: "F is only valid in range functions",
                    })
                    .into()
                }),
                Spec::dI => Cow::Owned(
                    ConstType::Error(CellError::InvalidInfinity {
                        cell: Some(spec.to_sloc()),
                        cursor: 0,
                    })
                    .into(),
                ),
                Spec::dH(r, g, b) => Cow::Owned(ConstType::Color((*r, *g, *b)).into()),
                Spec::dA(_) => Cow::Owned(ConstType::Function(Box::new(self.clone())).into()),
                Spec::dT => Cow::Owned(ConstType::Table(spec.dT).into()),
                Spec::dST => Cow::Owned(if let Some(v) = spec.dST {
                    ConstType::Table(v).into()
                } else {
                    ConstType::Error(CellError::InvalidFormula {
                        cell: Some(spec.to_sloc()),
                        cursor: 0,
                        reason: "S is only valid in range formulas",
                    })
                    .into()
                }),
                Spec::dFT => Cow::Owned(if let Some(v) = spec.dFT {
                    ConstType::Table(v).into()
                } else {
                    ConstType::Error(CellError::InvalidFormula {
                        cell: Some(spec.to_sloc()),
                        cursor: 0,
                        reason: "F is only valid in range functions",
                    })
                    .into()
                }),
            },
            Value::Mul(lhs, rhs) => {
                let lhs_eval = lhs.eval(data, eval, func, ranges, spec);
                let rhs_eval = rhs.eval(data, eval, func, ranges, spec);
                // println!("{lhs_eval:?} {rhs_eval:?} mulfail");
                if let Some(e) = lhs_eval.merge_err(&rhs_eval) {
                    Cow::Owned(e)
                } else if let (ConstType::Num(lnum), ConstType::Num(rnum)) =
                    (&lhs_eval.as_ref().ct, &rhs_eval.as_ref().ct)
                {
                    Cow::Owned(ConstType::Num(lnum * rnum).into())
                } else {
                    Cow::Owned(
                        ConstType::Error(CellError::BadArgument {
                            cell: Some(spec.to_sloc()),
                            cursor: 0,
                            expected_types: vec!["Num".into(), "Num".into()],
                        })
                        .into(),
                    )
                }
            }
            Value::Div(lhs, rhs) => {
                let lhs_eval = lhs.eval(data, eval, func, ranges, spec);
                let rhs_eval = rhs.eval(data, eval, func, ranges, spec);
                if let Some(e) = lhs_eval.merge_err(&rhs_eval) {
                    Cow::Owned(e)
                } else if let (ConstType::Num(lnum), ConstType::Num(rnum)) =
                    (&lhs_eval.as_ref().ct, &rhs_eval.as_ref().ct)
                {
                    Cow::Owned(ConstType::Num(lnum / rnum).into())
                } else {
                    Cow::Owned(
                        ConstType::Error(CellError::BadArgument {
                            cell: Some(spec.to_sloc()),
                            cursor: 0,
                            expected_types: vec!["Num".into(), "Num".into()],
                        })
                        .into(),
                    )
                }
            }
            Value::Lt(lhs, rhs) => {
                let lhs_eval = lhs.eval(data, eval, func, ranges, spec);
                let rhs_eval = rhs.eval(data, eval, func, ranges, spec);
                if let Some(e) = lhs_eval.merge_err(&rhs_eval) {
                    Cow::Owned(e)
                } else if let (ConstType::Num(lnum), ConstType::Num(rnum)) =
                    (&lhs_eval.as_ref().ct, &rhs_eval.as_ref().ct)
                {
                    Cow::Owned(ConstType::Bool(lnum < rnum).into())
                } else {
                    Cow::Owned(
                        ConstType::Error(CellError::BadArgument {
                            cell: Some(spec.to_sloc()),
                            cursor: 0,
                            expected_types: vec!["Num".into(), "Num".into()],
                        })
                        .into(),
                    )
                }
            }
            Value::Gt(lhs, rhs) => {
                let lhs_eval = lhs.eval(data, eval, func, ranges, spec);
                let rhs_eval = rhs.eval(data, eval, func, ranges, spec);
                if let Some(e) = lhs_eval.merge_err(&rhs_eval) {
                    Cow::Owned(e)
                } else if let (ConstType::Num(lnum), ConstType::Num(rnum)) =
                    (&lhs_eval.as_ref().ct, &rhs_eval.as_ref().ct)
                {
                    Cow::Owned(ConstType::Bool(lnum > rnum).into())
                } else {
                    Cow::Owned(
                        ConstType::Error(CellError::BadArgument {
                            cell: Some(spec.to_sloc()),
                            cursor: 0,
                            expected_types: vec!["Num".into(), "Num".into()],
                        })
                        .into(),
                    )
                }
            }
            Value::Func { name, args } => {
                if let Some(f) = func.get_func(name) {
                    Cow::Owned(f.call(args, data, eval, func, ranges, spec))
                } else {
                    Cow::Owned(
                        ConstType::Error(CellError::InvalidFormula {
                            cell: Some(spec.to_sloc()),
                            cursor: 0,
                            reason: "Formula not found",
                        })
                        .into(),
                    )
                }
            }
            Value::Eq(lhs, rhs) => {
                let lhs_eval = lhs.eval(data, eval, func, ranges, spec);
                let rhs_eval = rhs.eval(data, eval, func, ranges, spec);
                if let Some(e) = lhs_eval.merge_err(&rhs_eval) {
                    Cow::Owned(e)
                } else {
                    Cow::Owned(ConstType::Bool(lhs_eval == rhs_eval).into())
                }
            }
            Value::RangeForm(r) => {
                if r.conditon.eval(data, eval, func, ranges, spec).ct.truthy() {
                    r.value.eval(data, eval, func, ranges, spec)
                } else {
                    // Cow::Owned(
                    //     ConstType::Error(CellError::MissingCell {
                    //         cell: Some(spec.to_sloc()),
                    //     })
                    //     .into(),
                    // )
                    Cow::Owned(ConstType::String("".into()).into())
                }
            }
        };
        if let Some(x) = eval.get_mut(&l) {
            *x = 0;
        }
        t
    }

    pub(crate) fn get_refs(
        &self,
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Vec<Range> {
        match self {
            Value::Ref(r) => vec![r.into()],
            Value::Add(lhs, rhs)
            | Value::Sub(lhs, rhs)
            | Value::Mul(lhs, rhs)
            | Value::Div(lhs, rhs)
            | Value::Eq(lhs, rhs)
            | Value::Gt(lhs, rhs)
            | Value::Lt(lhs, rhs) => [lhs, rhs]
                .iter()
                .flat_map(|s| s.get_refs(data, eval, func, ranges, spec))
                .collect(),
            Value::Func { name, args } => func
                .get_func(name)
                .iter()
                .flat_map(|f| f.get_refs(args, data, eval, func, ranges, spec))
                .collect(),
            Value::Const(Const {
                ct: ConstType::Range(r),
                ..
            }) => vec![r.clone()],
            Value::RangeForm(r) => {
                let RangeFormula {
                    rbound: _,
                    cbound: _,
                    conditon,
                    value,
                } = r.as_ref();
                [conditon, value]
                    .iter()
                    .flat_map(|s| s.get_refs(data, eval, func, ranges, spec))
                    .collect()
            }
            Value::Const(_) | Value::Special(_) => vec![],
        }
    }

    fn is_range_formula(&self, spec: &SpecValues) -> Option<Range> {
        if let Self::RangeForm(r) = self {
            let c2 = match r.cbound {
                Bound::Fin(c) => Bound::Fin(c + spec.dC),
                Bound::Inf => Bound::Inf,
            };
            let r2 = match r.rbound {
                Bound::Fin(r) => Bound::Fin(r + spec.dR),
                Bound::Inf => Bound::Inf,
            };
            Some(Range(spec.to_sloc(), SLocBound(c2, r2)))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub(crate) enum Bound {
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
impl PartialOrd for Bound {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Bound {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Bound::Fin(a), Bound::Fin(b)) => a.cmp(b),
            (Bound::Fin(_), Bound::Inf) => std::cmp::Ordering::Less,
            (Bound::Inf, Bound::Fin(_)) => std::cmp::Ordering::Greater,
            (Bound::Inf, Bound::Inf) => std::cmp::Ordering::Equal,
        }
    }
}
#[derive(Debug, PartialEq)]
pub(crate) struct RangeFormula {
    rbound: Bound,
    cbound: Bound,
    conditon: Value,
    value: Value,
}

fn is_range_formula(formula: &str, in_sheet: &'static str) -> Result<RangeFormula, &'static str> {
    let Some((range, form)) = formula.split_once('=') else {
        return Err("No equal sign found");
    };
    if !range.starts_with('[') || !range.ends_with(']') {
        return Err("Range missing brackets");
    }

    let tokens_range = form_tokens(range);

    let Ok(split_range) = split_parens(&tokens_range) else {
        let _ = dbg!(split_parens(&tokens_range));
        return Err("Couldnt parenthesize");
    };
    // dbg!(&split_range);

    let ParenTree::Paren(args) = split_range else {
        return Err("Only one argument found");
    };
    let Some(ParenTree::Token(a, _)) = args.get(2) else {
        return Err("First argument must be a number or .I");
    };
    let Ok(cbound): Result<Bound, _> = (if a == &".I" {
        Ok(Bound::Inf)
    } else if a == &"" {
        Ok(Bound::Fin(0))
    } else {
        ConstType::from_str(a).and_then(|c| (&c).try_into())
    }) else {
        return Err("First argument must be a number or .I");
    };
    let Some(ParenTree::Token(b, _)) = args.get(4) else {
        return Err("Second argument must be a number or .I");
    };
    let Ok(rbound): Result<Bound, _> = (if b == &".I" {
        Ok(Bound::Inf)
    } else if b == &"" {
        Ok(Bound::Fin(0))
    } else {
        ConstType::from_str(b).and_then(|c| (&c).try_into())
    }) else {
        return Err("Second argument must be a number or .I");
    };
    // todo!("Make ] into none");
    let cond = if args.len() > 6 {
        ParenTree::Paren(Cow::Borrowed(&args[6..args.len()]))
    } else {
        ParenTree::Token("true", 0)
    };
    // dbg!(&cond);

    let cond_val = form_tree(&cond, in_sheet).0;

    let tokens_form = form_tokens(form);
    let Ok(split_form) = split_parens(&tokens_form) else {
        let _ = dbg!(split_parens(&tokens_form));
        return Err("Couldnt parenthesize");
    };
    let form_val = form_tree(&split_form, in_sheet).0;

    Ok(RangeFormula {
        rbound,
        cbound,
        conditon: cond_val,
        value: form_val,
    })
}

#[derive(Debug, Default, PartialEq)]
pub(crate) struct CellData {
    pub(crate) val: String,

    /// Is none when recalculating
    // pub(crate) parsed: Option<Value>,
    pub(crate) display: Option<Const>,
    pub(crate) rangeform: bool,
    /// Dependants, aka cells that depend on this one
    pub(crate) dependants: HashSet<SLoc>,
}

impl FromStr for CellData {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let c = CellData {
            // val: Value::Const(ConstType::String(s.into().into())),
            val: s.into(),
            display: None,
            dependants: HashSet::new(),
            // parsed: None,
            rangeform: false,
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

    #[cfg(test)]
    pub(crate) fn display(self, display: Option<Const>) -> Self {
        Self { display, ..self }
    }
    // #[cfg(test)]
    // pub(crate) fn rangeform(self, rangeform: bool) -> Self {
    //     Self { rangeform, ..self }
    // }
    #[cfg(test)]
    pub(crate) fn deps(self, dependants: HashSet<SLoc>) -> Self {
        Self { dependants, ..self }
    }
}
pub(crate) fn get_range_form<'a>(
    ranges: &'a SheetRanges,
    loc: &SLoc,
) -> Result<Option<&'a SLoc>, CellError> {
    let v: Vec<_> = ranges
        .iter()
        .filter(|r| r.in_range(loc))
        .map(|r| &r.0)
        .collect();
    if v.len() > 1 {
        Err(CellError::RangeIntersect {
            cell: Some(*loc),
            source1: *v[0],
            source2: *v[1],
        })
    } else {
        Ok(v.first().cloned())
    }
}
impl Sheet {
    // fn set_eval(&mut self, cell: &SLoc, eval: usize) {
    //     if let Some(c) = self.1.get_mut(cell) {
    //         *c = eval;
    //     }
    // }
    // fn get_eval(&self, cell: &SLoc) -> usize {
    //     if let Some(c) = self.1.get(cell) {
    //         *c
    //     } else {
    //         0
    //     }
    // }
    #[cfg(test)]
    fn set_disp_err(&mut self, cell: &SLoc, err: CellError) {
        if let Some(c) = self.data.0.get_mut(cell) {
            c.display = Some(ConstType::Error(err).into())
        }
    }
    fn set_disp(&mut self, cell: &SLoc, v: Option<Const>) {
        if let Some(c) = self.data.0.get_mut(cell) {
            c.display = v
        } else {
            println!("Error setting cell {cell:?}");
        }
    }
    // TODO `evaluating` shouldnt be here, it doesnt work
    pub(crate) fn dirty(&mut self, loc: &SLoc) {
        // println!("dirty {loc:?}");
        let c = self.get_mut(loc);
        let to_dirty: HashSet<SLoc> = if let Ok(c) = c {
            if c.display.is_none() {
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

    pub(crate) fn get_display(&mut self, loc: &SLoc) -> Option<Const> {
        match self.get(loc) {
            Ok(d) => d.display.clone(),
            Err(_) => None,
        }
        // match get_range_form(&self.ranges, loc) {
        //     Ok(Some(r)) if r != loc => {
        //         let c = self.0.get(r)?;
        //         let spec = SpecValues {
        //             dC: loc.0,
        //             dR: loc.1,
        //             dSC: Some(r.0),
        //             dSR: Some(r.1),
        //             dFd: None,
        //             dFC: None,
        //             dFR: None,
        //             depth: 0,
        //         };
        //         let val: Value = c.val.parse().ok()?;
        //         let c = Some(
        //             val.eval(&self.data, &mut self.eval, &self.funcs, &self.ranges, &spec)
        //                 .into_owned(),
        //         );
        //         self.insert(
        //             *loc,
        //             CellData {
        //                 val: "".into(),
        //                 display: c.clone(),
        //                 rangeform: true,
        //                 dependants: HashSet::new(),
        //             },
        //         );
        //         c
        //     }
        //     Err(e) => Some(ConstType::Error(e).into()),
        //     _ => self.0.get(loc).and_then(|c| c.display.clone()),
        // }
    }

    pub(crate) fn recompute_range(&mut self, view_range: Range) {
        if view_range.is_inf() {
            return;
        }
        // Cells on the screen that are in a range function
        let mut dummies = HashSet::new();
        for sheetrange in self.ranges.iter() {
            let inter = view_range.intersect(sheetrange);

            let riter: FinRangeIter = match inter.try_into() {
                Ok(i) => i,
                Err(e) => panic!("Infinte view range requested: {e:?}",),
            };
            for cell in riter {
                if !self.contains_key(&cell) {
                    // println!("{cell:?} ispartof {sheetrange:?}");
                    dummies.insert((cell, sheetrange.0));
                }
            }
        }
        for (cell, range_origin) in dummies {
            let _ = self.insert(
                cell,
                CellData {
                    val: "".to_owned(),
                    display: None,
                    rangeform: true,
                    dependants: [range_origin].into_iter().collect(),
                },
            );
            // self.get_mut(&range_origin).unwrap().dependants.insert(cell);
        }

        // Remove any unneeded offscreen elements
        // I think this is causing more problems than its helping
        // let mut offscreenrange = true;
        // let mut keyc = self.data.0.keys().cloned().collect();
        // while offscreenrange {
        //     offscreenrange = false;
        //     self.data.0.retain(|sloc, data| {
        //         let keep = view_range.in_range(sloc)
        //             || !data.rangeform
        //             || data.dependants.intersection(&keyc).next().is_some();
        //         if !keep {
        //             keyc.remove(sloc);
        //             offscreenrange = true;
        //         }
        //         keep
        //     });
        // }
        self.recompute()
    }

    pub(crate) fn recompute(&mut self) {
        // println!("Recompute");
        let mut kc: Vec<_> = self.keys().cloned().collect();
        // dbg!(&self);
        // dbg!(&kc);
        for pos in kc.iter() {
            // Remove dependants that dont depend
            let s = self.get(pos);
            let mut torem = HashSet::new();
            if let Ok(s) = s {
                for p in s.dependants.clone().iter() {
                    if let Ok(dep_val) = self.get_val(p) {
                        if let Ok(pv) = Value::from_str(dep_val, p.0) {
                            if !pv
                                .get_refs(
                                    &self.data,
                                    &mut self.eval,
                                    &self.funcs,
                                    &self.ranges,
                                    &SpecValues::from_sloc(*p),
                                )
                                .iter()
                                .any(|r| r.in_range(pos))
                            {
                                // println!("Removing dependant {p:?} from {pos:?}");
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
            if let Ok(s) = self.get_mut(pos) {
                for pr in torem {
                    s.dependants.remove(&pr);
                }
            }
        }
        // println!("Updated deps");
        self.ranges.clear();
        let mut orphan_cells = vec![];
        for pos in kc.iter() {
            let sval = self.get_val(pos);
            let mut toadd = None;
            let mut rfm = None;
            if let Ok(s) = sval {
                if let Ok(pv) = Value::from_str(s, pos.0) {
                    toadd = Some(pv.get_refs(
                        &self.data,
                        &mut self.eval,
                        &self.funcs,
                        &self.ranges,
                        &SpecValues::from_sloc(*pos),
                    ));
                    rfm = pv.is_range_formula(&SpecValues::from_sloc(*pos));
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
                self.dirty(pos);
            }
            if let Some(rfm) = rfm {
                // for posd in kc.iter() {
                //     if rfm.in_range(posd) {
                //         if let Ok(c2) = self.get_mut(posd) {
                //             if c2.rangef.is_some_and(|p| p != *pos) {
                //                 c2.display = Some(
                //                     ConstType::Error(CellError::RangeIntersect {
                //                         cell: Some(*posd),
                //                         source1: c2.rangef.unwrap(),
                //                         source2: *pos,
                //                     })
                //                     .into(),
                //                 );
                //             } else if c2.rangef.is_none() {
                //                 c2.rangef = Some(*pos);
                //             }
                //         }
                //     }
                // }
                if self.get(pos).is_ok_and(|c| !c.rangeform) {
                    self.ranges.insert(rfm);
                }
            } else if self.get(pos).is_ok_and(|x| x.rangeform) {
                // println!("Removing {pos:?} from kc");
                self.remove(pos);
                orphan_cells.push(*pos);
            }
        }
        kc.retain(|x| !orphan_cells.contains(x));
        // strip mutability
        let kc = kc;
        // dbg!(&self);
        // dbg!(&kc);
        // println!("Reeval cells");
        loop {
            let recpos = self
                .iter()
                .filter(|c| {
                    c.1.display.is_none()
                        && kc.iter().all(|d| {
                            (self.get(d))
                                .map(|ck| !ck.dependants.contains(c.0) || ck.display.is_some())
                                .unwrap_or(true)
                        })
                })
                .map(|(l, _)| *l)
                .next();
            // println!("recpos: {recpos:?}");
            if let Some(pos) = recpos {
                self.dirty(&pos);
                let s = self.get(&pos);
                // dbg!(&s);
                // This will return error if a reference loop is reached.
                // This needs to continue to properly do reference loop detection and proper text updating, so I cant early return/continue
                let (val, spec) = if let Ok(s) = s {
                    if s.rangeform {
                        match get_range_form(&self.ranges, &pos) {
                            Ok(oloc) => {
                                // let oloc = oloc.expect("Was part of range but cant find range");
                                let Some(oloc) = oloc else {
                                    let s = self.get_mut(&pos).unwrap();
                                    s.rangeform = false;
                                    continue;
                                };
                                let spec = SpecValues {
                                    dC: pos.1,
                                    dR: pos.2,
                                    dT: pos.0,
                                    dSC: Some(oloc.1),
                                    dSR: Some(oloc.2),
                                    dST: Some(oloc.0),
                                    dFd: None,
                                    dFC: None,
                                    dFR: None,
                                    depth: 0,
                                    dFT: None,
                                };
                                (
                                    match Value::from_str(&self.get(oloc).unwrap().val, pos.0) {
                                        Ok(val) => val,
                                        Err(e) => Value::Const(
                                            ConstType::Error(CellError::InvalidFormula {
                                                cell: Some(pos),
                                                cursor: 0,
                                                reason: e,
                                            })
                                            .into(),
                                        ),
                                    },
                                    spec,
                                )
                            }
                            Err(e) => (
                                Value::Const(ConstType::Error(e).into()),
                                SpecValues::from_sloc(pos),
                            ),
                        }
                    } else {
                        let val = s.val.clone();
                        let spec = SpecValues {
                            dT: pos.0,
                            dC: pos.1,
                            dR: pos.2,
                            dST: None,
                            dSC: None,
                            dSR: None,
                            dFT: None,
                            dFd: None,
                            dFC: None,
                            dFR: None,
                            depth: 0,
                        };
                        (
                            match Value::from_str(&val, pos.0) {
                                Ok(val) => val,
                                Err(e) => Value::Const(
                                    ConstType::Error(CellError::InvalidFormula {
                                        cell: Some(pos),
                                        cursor: 0,
                                        reason: e,
                                    })
                                    .into(),
                                ),
                            },
                            spec,
                        )
                    }
                } else {
                    (
                        Value::Const(ConstType::Error(s.unwrap_err()).into()),
                        SpecValues::from_sloc(pos),
                    )
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
                //         val = Value::Const(ConstType::Error(.into()CellError::ReferenceLoop {
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

                let tmp: Option<Const> = Some(
                    val.eval(&self.data, &mut self.eval, &self.funcs, &self.ranges, &spec)
                        .to_pos(pos),
                );
                // dbg!(&tmp);
                // let sm = self.get_mut(&pos).expect("Cell was deleted after accessed");
                // sm.display = tmp;
                self.set_disp(&pos, tmp);
            } else if self.iter().filter(|(_, c)| c.display.is_none()).count() == 0 {
                break;
            } else {
                self.iter_mut()
                    .filter(|(_, d)| d.display.is_none())
                    .take(1)
                    .for_each(|(p, d)| {
                        d.display = Some(
                            ConstType::Error(CellError::ReferenceLoop {
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
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Const;
    fn get_refs(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Vec<Range> {
        args.iter()
            .flat_map(|a| a.get_refs(data, eval, func, ranges, spec))
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
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Const {
        let e = ConstType::Error(CellError::BadArgument {
            cell: None,
            cursor: 4,
            expected_types: vec!["bool".into(), "any".into(), "any".into()],
        })
        .into();
        let [c, y, n] = args else {
            return e;
        };
        let cval = c.eval(data, eval, func, ranges, spec);
        if matches!(cval.as_ref().ct, ConstType::Error(_)) {
            return cval.into_owned();
        }
        let ConstType::Bool(c) = cval.as_ref().ct else {
            return ConstType::Error(CellError::BadArgument {
                cell: None,
                cursor: 0,
                expected_types: vec!["bool".into()],
            })
            .into();
        };
        if c {
            y.eval(data, eval, func, ranges, spec).into_owned()
        } else {
            n.eval(data, eval, func, ranges, spec).into_owned()
        }
    }

    fn get_refs(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Vec<Range> {
        let [c, y, n] = args else {
            return vec![];
        };
        let mut def_refs = c.get_refs(data, eval, func, ranges, spec);
        let cval = c.eval(data, eval, func, ranges, spec);
        if matches!(cval.as_ref().ct, ConstType::Error(_)) {
            return def_refs;
        }
        let ConstType::Bool(c) = cval.as_ref().ct else {
            return def_refs;
        };
        if c {
            def_refs.extend(y.get_refs(data, eval, func, ranges, spec));
        } else {
            def_refs.extend(n.get_refs(data, eval, func, ranges, spec));
        }
        def_refs
    }
}
#[derive(Default)]
pub(crate) struct ValidFunc;
impl Function for ValidFunc {
    fn name(&self) -> &'static str {
        "valid"
    }

    fn call(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Const {
        let [f, c, r] = args else {
            return ConstType::Error(CellError::WrongNumArguments {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                arguments: vec![3],
            })
            .into();
        };
        let f = f.eval(data, eval, func, ranges, spec);
        let ConstType::Table(f) = f.as_ref().ct else {
            if let ConstType::Error(_) = f.as_ref().ct {
                return f.into_owned();
            } else {
                return ConstType::Error(CellError::BadArgument {
                    cell: Some(spec.to_sloc()),
                    cursor: 0,
                    expected_types: vec!["File name".into()],
                })
                .into();
            }
        };
        let c = c.eval(data, eval, func, ranges, spec);
        let ConstType::Num(c) = c.as_ref().ct else {
            if let ConstType::Error(_) = c.as_ref().ct {
                return c.into_owned();
            } else {
                return ConstType::Error(CellError::BadArgument {
                    cell: Some(spec.to_sloc()),
                    cursor: 0,
                    expected_types: vec!["Number".into()],
                })
                .into();
            }
        };
        let r = r.eval(data, eval, func, ranges, spec);
        let ConstType::Num(r) = r.as_ref().ct else {
            if let ConstType::Error(_) = r.as_ref().ct {
                return r.into_owned();
            } else {
                return ConstType::Error(CellError::BadArgument {
                    cell: Some(spec.to_sloc()),
                    cursor: 1,
                    expected_types: vec!["Number".into()],
                })
                .into();
            }
        };
        // TODO make this more rigid, find good way to do a let else
        if c < 0. || c != c.round() || r < 0. || r.round() != r {
            return ConstType::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["whole number".into()],
            })
            .into();
        }
        let c = c as i32;
        let r = r as i32;

        let rf = data.get(&SLoc(f, c, r), eval);
        // if rf.is_err() {
        //     return ConstType::Bool(false).into();
        // }
        // let rf = rf.expect("Already handled err case");
        // if let Some(d) = rf.display.clone() {
        //     ConstType::Bool(d.ct.truthy()).into()
        // } else {
        //     let val = match Value::from_str(&rf.val, f) {
        //         Ok(v) => v,
        //         Err(e) => {
        //             return ConstType::Error(CellError::InvalidFormula {
        //                 cell: Some(SLoc(f, c, r)),
        //                 cursor: 0,
        //                 reason: e,
        //             })
        //             .into()
        //         }
        //     };
        //     let new_spec = spec.clone().with_sloc(SLoc(f, c, r));

        //     val.eval(data, eval, func, ranges, &new_spec).into_owned()
        // }
        match rf {
            Ok(rf) => {
                if let Some(d) = &rf.display {
                    ConstType::Bool(!d.is_error())
                } else {
                    let val = match Value::from_str(&rf.val, f) {
                        Ok(v) => v,
                        Err(_) => return ConstType::Bool(false).into(),
                    };
                    let new_spec = spec.clone().with_sloc(SLoc(f, c, r));

                    ConstType::Bool(val.eval(data, eval, func, ranges, &new_spec).is_error())
                }
            }
            Err(_) => ConstType::Bool(false),
        }
        .into()
    }
    fn get_refs(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Vec<Range> {
        let mut refs = args
            .iter()
            .flat_map(|a| a.get_refs(data, eval, func, ranges, spec))
            .collect();
        let [f, c, r] = args else {
            return refs;
        };
        let f = f.eval(data, eval, func, ranges, spec);
        let ConstType::Table(f) = f.as_ref().ct else {
            return refs;
        };
        let c = c.eval(data, eval, func, ranges, spec);
        let ConstType::Num(c) = c.as_ref().ct else {
            return refs;
        };
        let r = r.eval(data, eval, func, ranges, spec);
        let ConstType::Num(r) = r.as_ref().ct else {
            return refs;
        };
        // TODO make this more rigid, find good way to do a let else
        if c < 0. || c != c.round() || r < 0. || r.round() != r {
            return refs;
        }
        let c = c as i32;
        let r = r as i32;
        refs.push((&SLoc(f, c, r)).into());
        refs
    }
}

pub(crate) enum AccMode {
    Sum,
    Mean,
}

// #[derive(Default)]
pub(crate) struct Acc(pub(crate) AccMode);
impl Function for Acc {
    fn name(&self) -> &'static str {
        // "sum"
        match self.0 {
            AccMode::Sum => "sum",
            AccMode::Mean => "mean",
        }
    }

    fn call(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Const {
        let mut s = 0.0;
        let mut count = 0.0;
        // println!("Sum args");
        // dbg!(&args);
        let e = ConstType::Error(CellError::BadArgument {
            cell: Some(spec.to_sloc()),
            cursor: 0,
            expected_types: vec!["Cell or range...".into()],
        })
        .into();
        for a in args {
            if let Value::Const(Const {
                ct: ConstType::Range(r),
                ..
            }) = a
            {
                // dbg!(r);
                println!("Range {r:?}");
                if let Some(value) = sum_range(data, r, eval, spec, func, ranges, |x| {
                    s += x;
                    count += 1.
                }) {
                    return value;
                }
            } else {
                let c = a.eval(data, eval, func, ranges, spec);
                // dbg!(&c);
                println!("Val {c:?}");
                if let ConstType::Num(n) = c.ct {
                    s += n;
                    count += 1.
                } else if let ConstType::Range(r) = &c.as_ref().ct {
                    if let Some(value) = sum_range(data, r, eval, spec, func, ranges, |x| {
                        s += x;
                        count += 1.
                    }) {
                        return value;
                    }
                } else if let ConstType::Error(_) = c.as_ref().ct {
                    return c.into_owned();
                } else {
                    return e;
                }
            }
        }
        ConstType::Num(match self.0 {
            AccMode::Sum => s,
            AccMode::Mean => s / count,
        })
        .into()
    }

    fn get_refs(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Vec<Range> {
        let mut refs: Vec<_> = args
            .iter()
            .flat_map(|a| a.get_refs(data, eval, func, ranges, spec))
            .collect();
        refs.extend(args.iter().flat_map(|a| {
            if let Value::Const(Const {
                ct: ConstType::Range(r),
                ..
            }) = a
            {
                vec![r.clone()]
            } else {
                a.get_refs(data, eval, func, ranges, spec)
            }
        }));
        refs
    }
}

/// Returns Some only when there is an error
fn sum_range(
    data: &SheetData,
    r: &Range,
    eval: &mut SheetEval,
    spec: &SpecValues,
    func: &SheetFunc,
    ranges: &SheetRanges,
    // s: &mut f64,
    mut acc: impl FnMut(f64),
) -> Option<Const> {
    for elem in data.keys().filter(|k| r.in_range(k)) {
        // dbg!(&elem);
        let v = data.get(elem, eval);
        let v = match v {
            Ok(v) => v,
            Err(e) => match e {
                CellError::MissingCell { cell: _ } => continue,
                _ => return Some(ConstType::Error(e).into()),
            },
        };
        // dbg!(v);
        let Ok(v) = Value::from_str(&v.val, elem.0) else {
            return Some(ConstType::Error(CellError::MissingCell { cell: Some(*elem) }).into());
        };
        let new_spec = spec.clone().with_sloc(*elem);
        // dbg!(&new_spec);
        let cv = v.eval(data, eval, func, ranges, &new_spec);
        if let ConstType::Num(n) = cv.as_ref().ct {
            // *s += n;
            acc(n);
        } else if let ConstType::Error(_) = &cv.as_ref().ct {
            return Some(cv.into_owned());
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
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Const {
        let [f, c, r] = args else {
            return ConstType::Error(CellError::WrongNumArguments {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                arguments: vec![3],
            })
            .into();
        };
        let f = f.eval(data, eval, func, ranges, spec);
        let ConstType::Table(f) = f.as_ref().ct else {
            if let ConstType::Error(_) = f.as_ref().ct {
                return f.into_owned();
            } else {
                return ConstType::Error(CellError::BadArgument {
                    cell: Some(spec.to_sloc()),
                    cursor: 0,
                    expected_types: vec!["File name".into()],
                })
                .into();
            }
        };
        let c = c.eval(data, eval, func, ranges, spec);
        let ConstType::Num(c) = c.as_ref().ct else {
            if let ConstType::Error(_) = c.as_ref().ct {
                return c.into_owned();
            } else {
                return ConstType::Error(CellError::BadArgument {
                    cell: Some(spec.to_sloc()),
                    cursor: 1,
                    expected_types: vec!["Number".into()],
                })
                .into();
            }
        };
        let r = r.eval(data, eval, func, ranges, spec);
        let ConstType::Num(r) = r.as_ref().ct else {
            if let ConstType::Error(_) = r.as_ref().ct {
                return r.into_owned();
            } else {
                return ConstType::Error(CellError::BadArgument {
                    cell: Some(spec.to_sloc()),
                    cursor: 2,
                    expected_types: vec!["Number".into()],
                })
                .into();
            }
        };
        // TODO make this more rigid, find good way to do a let else
        if c < 0. || c != c.round() || r < 0. || r.round() != r {
            return ConstType::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["whole number".into()],
            })
            .into();
        }
        let c = c as i32;
        let r = r as i32;

        let rf = data.get(&SLoc(f, c, r), eval);
        if let Err(e) = rf {
            return ConstType::Error(e).into();
        }
        let rf = rf.expect("Already handled err case");
        if let Some(d) = rf.display.clone() {
            d
        } else {
            let val = match Value::from_str(&rf.val, f) {
                Ok(v) => v,
                Err(e) => {
                    return ConstType::Error(CellError::InvalidFormula {
                        cell: Some(SLoc(f, c, r)),
                        cursor: 0,
                        reason: e,
                    })
                    .into()
                }
            };
            let new_spec = spec.clone().with_sloc(SLoc(f, c, r));

            val.eval(data, eval, func, ranges, &new_spec).into_owned()
        }
    }
    fn get_refs(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Vec<Range> {
        let mut refs = args
            .iter()
            .flat_map(|a| a.get_refs(data, eval, func, ranges, spec))
            .collect();
        let [f, c, r] = args else {
            return refs;
        };
        let f = f.eval(data, eval, func, ranges, spec);
        let ConstType::Table(f) = f.as_ref().ct else {
            return refs;
        };
        let c = c.eval(data, eval, func, ranges, spec);
        let ConstType::Num(c) = c.as_ref().ct else {
            return refs;
        };
        let r = r.eval(data, eval, func, ranges, spec);
        let ConstType::Num(r) = r.as_ref().ct else {
            return refs;
        };
        // TODO make this more rigid, find good way to do a let else
        if c < 0. || c != c.round() || r < 0. || r.round() != r {
            return refs;
        }
        let c = c as i32;
        let r = r as i32;
        refs.push((&SLoc(f, c, r)).into());
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
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Const {
        let mut count = 0;
        let [range, cond] = args else {
            return ConstType::Error(CellError::WrongNumArguments {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                arguments: vec![2],
            })
            .into();
        };
        let Value::Const(Const {
            ct: ConstType::Range(r),
            ..
        }) = range
        else {
            return ConstType::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Range".into()],
            })
            .into();
        };
        // dbg!(cond);
        for cell in data.keys().filter(|k| r.in_range(k)) {
            let Ok(celldata) = data.get(cell, eval) else {
                continue;
            };
            let inner_spec = spec.clone().with_sloc(*cell);
            let val = Value::from_str(&celldata.val, cell.0);
            if let Err(val) = val {
                return ConstType::Error(CellError::InvalidFormula {
                    cell: Some(inner_spec.to_sloc()),
                    cursor: 0,
                    reason: val,
                })
                .into();
            }
            let val = val.unwrap();
            let cval = val.eval(data, eval, func, ranges, &inner_spec).into_owned();

            let fspec = spec.clone().with_f_sloc(Some((*cell, cval)));
            dbg!(spec, &fspec);

            let condval = cond.eval(data, eval, func, ranges, &fspec);
            dbg!(&condval);

            if condval.is_error() {
                if !matches!(
                    condval.ct,
                    ConstType::Error(CellError::MissingCell { cell: _ })
                ) {
                    return condval.into_owned();
                }
            }

            if let ConstType::Bool(true) = condval.ct {
                count += 1;
            };
        }
        ConstType::Num(count as f64).into()
    }

    fn get_refs(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Vec<Range> {
        let mut refs = args
            .iter()
            .flat_map(|a| a.get_refs(data, eval, func, ranges, spec))
            .collect();
        let Value::Const(Const {
            ct: ConstType::Range(r),
            ..
        }) = &args[0]
        else {
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
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Vec<Range> {
        let mut refs = args
            .iter()
            .flat_map(|a| a.get_refs(data, eval, func, ranges, spec))
            .collect();
        if let [c, r] = args {
            let c = c.eval(data, eval, func, ranges, spec);
            let ConstType::Num(c) = c.as_ref().ct else {
                return refs;
            };
            let r = r.eval(data, eval, func, ranges, spec);
            let ConstType::Num(r) = r.as_ref().ct else {
                return refs;
            };
            // TODO make this more rigid, find good way to do a let else
            if c < 0. || c != c.round() || r < 0. || r.round() != r {
                return refs;
            }
            let c = c as i32;
            let r = r as i32;
            refs.push(Range::from(&SLoc(spec.dT, c, r)).into_inf());
            refs
        } else if let [c1, r1, c2, r2] = args {
            let c1 = c1.eval(data, eval, func, ranges, spec);
            let ConstType::Num(c1) = c1.as_ref().ct else {
                return refs;
            };
            let r1 = r1.eval(data, eval, func, ranges, spec);
            let ConstType::Num(r1) = r1.as_ref().ct else {
                return refs;
            };
            // TODO make this more rigid, find good way to do a let else
            if c1 < 0. || c1 != c1.round() || r1 < 0. || r1.round() != r1 {
                return refs;
            }
            let c1 = c1 as i32;
            let r1 = r1 as i32;
            let c2 = c2.eval(data, eval, func, ranges, spec);
            let Ok(c2) = TryInto::<Bound>::try_into(&c2.as_ref().ct) else {
                return refs;
            };
            let r2 = r2.eval(data, eval, func, ranges, spec);
            let Ok(r2) = TryInto::<Bound>::try_into(&r2.as_ref().ct) else {
                return refs;
            };
            refs.push(Range(SLoc(spec.dT, c1, r1), SLocBound(c2, r2)));
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
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Const {
        if let [c, r] = args {
            let c = c.eval(data, eval, func, ranges, spec);
            let ConstType::Num(c) = c.as_ref().ct else {
                return ConstType::Error(CellError::BadArgument {
                    cell: Some(spec.to_sloc()),
                    cursor: 0,
                    expected_types: vec!["Number".into()],
                })
                .into();
            };
            let r = r.eval(data, eval, func, ranges, spec);
            let ConstType::Num(r) = r.as_ref().ct else {
                return ConstType::Error(CellError::BadArgument {
                    cell: Some(spec.to_sloc()),
                    cursor: 1,
                    expected_types: vec!["Number".into()],
                })
                .into();
            };
            // TODO make this more rigid, find good way to do a let else
            if c < 0. || c != c.round() || r < 0. || r.round() != r {
                return ConstType::Error(CellError::BadArgument {
                    cell: Some(spec.to_sloc()),
                    cursor: 0,
                    expected_types: vec!["Whole number".into()],
                })
                .into();
            }
            let c = c as i32;
            let r = r as i32;
            ConstType::Range(Range::new_inf(SLoc(spec.dT, c, r))).into()
        } else if let [c1, r1, c2, r2] = args {
            let c1 = c1.eval(data, eval, func, ranges, spec);
            let ConstType::Num(c1) = c1.as_ref().ct else {
                return ConstType::Error(CellError::BadArgument {
                    cell: Some(spec.to_sloc()),
                    cursor: 0,
                    expected_types: vec!["Number".into()],
                })
                .into();
            };
            let r1 = r1.eval(data, eval, func, ranges, spec);
            let ConstType::Num(r1) = r1.as_ref().ct else {
                return ConstType::Error(CellError::BadArgument {
                    cell: Some(spec.to_sloc()),
                    cursor: 1,
                    expected_types: vec!["Number".into()],
                })
                .into();
            };
            // TODO make this more rigid, find good way to do a let else
            if c1 < 0. || c1 != c1.round() || r1 < 0. || r1.round() != r1 {
                return ConstType::Error(CellError::BadArgument {
                    cell: Some(spec.to_sloc()),
                    cursor: 0,
                    expected_types: vec!["Whole number".into()],
                })
                .into();
            }
            let c1 = c1 as i32;
            let r1 = r1 as i32;
            let c2 = c2.eval(data, eval, func, ranges, spec);
            let c2 = match TryInto::<Bound>::try_into(&c2.as_ref().ct) {
                Ok(b) => b,
                Err(e) => return e.into_pos(spec.to_sloc()).into(),
            };
            let r2 = r2.eval(data, eval, func, ranges, spec);
            let r2 = match TryInto::<Bound>::try_into(&r2.as_ref().ct) {
                Ok(b) => b,
                Err(e) => return e.into_pos(spec.to_sloc()).into(),
            };
            ConstType::Range(Range(SLoc(spec.dT, c1, r1), SLocBound(c2, r2))).into()
        } else {
            ConstType::Error(CellError::WrongNumArguments {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                arguments: vec![2, 4],
            })
            .into()
        }
    }
}
#[derive(Default)]
pub(crate) struct ColorFunc;
impl Function for ColorFunc {
    fn name(&self) -> &'static str {
        "color"
    }

    fn call(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Const {
        let e = ConstType::Error(CellError::BadArgument {
            cell: None,
            cursor: 4,
            expected_types: vec!["any".into(), "bool".into(), "color".into()],
        })
        .into();
        let [v, c, color] = args else {
            return e;
        };
        let cval = c.eval(data, eval, func, ranges, spec);
        if matches!(cval.as_ref().ct, ConstType::Error(_)) {
            return cval.into_owned();
        }
        let ConstType::Bool(c) = cval.as_ref().ct else {
            return ConstType::Error(CellError::BadArgument {
                cell: None,
                cursor: 1,
                expected_types: vec!["bool".into()],
            })
            .into();
        };
        let colorval = color.eval(data, eval, func, ranges, spec);
        if matches!(colorval.as_ref().ct, ConstType::Error(_)) {
            return colorval.into_owned();
        }
        let ConstType::Color(colorc) = colorval.as_ref().ct else {
            return ConstType::Error(CellError::BadArgument {
                cell: None,
                cursor: 2,
                expected_types: vec!["color".into()],
            })
            .into();
        };
        let mut vval = v.eval(data, eval, func, ranges, spec).into_owned();
        if c {
            vval.bgcolor = colorc;
            vval
        } else {
            vval
        }
    }
}
#[derive(Default)]
pub(crate) struct AndFunc;
impl Function for AndFunc {
    fn name(&self) -> &'static str {
        "and"
    }

    fn call(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Const {
        let e = ConstType::Error(CellError::BadArgument {
            cell: None,
            cursor: 3,
            expected_types: vec!["bool".into(), "bool".into()],
        })
        .into();
        let [a, b] = args else {
            return e;
        };
        let aval = a.eval(data, eval, func, ranges, spec);
        if matches!(aval.as_ref().ct, ConstType::Error(_)) {
            return aval.into_owned();
        }
        let ConstType::Bool(a) = aval.as_ref().ct else {
            return ConstType::Error(CellError::BadArgument {
                cell: None,
                cursor: 0,
                expected_types: vec!["bool".into()],
            })
            .into();
        };
        let bval = b.eval(data, eval, func, ranges, spec);
        if matches!(bval.as_ref().ct, ConstType::Error(_)) {
            return bval.into_owned();
        }
        let ConstType::Bool(b) = bval.as_ref().ct else {
            return ConstType::Error(CellError::BadArgument {
                cell: None,
                cursor: 1,
                expected_types: vec!["bool".into()],
            })
            .into();
        };
        ConstType::Bool(a && b).into()
    }
}
#[derive(Default)]
pub(crate) struct OrFunc;
impl Function for OrFunc {
    fn name(&self) -> &'static str {
        "or"
    }

    fn call(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Const {
        let e = ConstType::Error(CellError::BadArgument {
            cell: None,
            cursor: 3,
            expected_types: vec!["bool".into(), "bool".into()],
        })
        .into();
        let [a, b] = args else {
            return e;
        };
        let aval = a.eval(data, eval, func, ranges, spec);
        if matches!(aval.as_ref().ct, ConstType::Error(_)) {
            return aval.into_owned();
        }
        let ConstType::Bool(a) = aval.as_ref().ct else {
            return ConstType::Error(CellError::BadArgument {
                cell: None,
                cursor: 0,
                expected_types: vec!["bool".into()],
            })
            .into();
        };
        let bval = b.eval(data, eval, func, ranges, spec);
        if matches!(bval.as_ref().ct, ConstType::Error(_)) {
            return bval.into_owned();
        }
        let ConstType::Bool(b) = bval.as_ref().ct else {
            return ConstType::Error(CellError::BadArgument {
                cell: None,
                cursor: 1,
                expected_types: vec!["bool".into()],
            })
            .into();
        };
        ConstType::Bool(a || b).into()
    }
}

#[derive(Default)]
pub(crate) struct TrueFunc;
impl Function for TrueFunc {
    fn name(&self) -> &'static str {
        "true"
    }

    fn call(
        &self,
        _args: &[Value],
        _data: &SheetData,
        _eval: &mut SheetEval,
        _func: &SheetFunc,
        _ranges: &SheetRanges,
        _spec: &SpecValues,
    ) -> Const {
        ConstType::Bool(true).into()
    }
}

#[derive(Default)]
pub(crate) struct FalseFunc;
impl Function for FalseFunc {
    fn name(&self) -> &'static str {
        "false"
    }

    fn call(
        &self,
        _args: &[Value],
        _data: &SheetData,
        _eval: &mut SheetEval,
        _func: &SheetFunc,
        _ranges: &SheetRanges,
        _spec: &SpecValues,
    ) -> Const {
        ConstType::Bool(false).into()
    }
}
#[derive(Default)]
pub(crate) struct RoundFunc;
impl Function for RoundFunc {
    fn name(&self) -> &'static str {
        "round"
    }

    fn call(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Const {
        let dprec = Value::Const(ConstType::Num(0.).into());
        let [num, prec] = if let [num] = args {
            [num, &dprec]
        } else if let [num, prec] = args {
            [num, prec]
        } else {
            return ConstType::Error(CellError::WrongNumArguments {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                arguments: vec![1, 2],
            })
            .into();
        };
        let nc = num.eval(data, eval, func, ranges, spec);
        let pc = prec.eval(data, eval, func, ranges, spec);
        let arge: Const = ConstType::Error(CellError::BadArgument {
            cell: Some(spec.to_sloc()),
            cursor: 0,
            expected_types: vec!["Number".into(), "Whole number?".into()],
        })
        .into();
        let numnum = match &nc.ct {
            ConstType::Num(n) => n,
            ConstType::Error(e) => return ConstType::Error(e.clone()).into(),
            _ => return arge,
        };
        let prec = match &pc.ct {
            ConstType::Num(n) => {
                if n.round() != *n {
                    return arge;
                }
                *n
            }
            ConstType::Error(e) => return ConstType::Error(e.clone()).into(),
            _ => return arge,
        };
        ConstType::Num((numnum * 10f64.powf(prec)).round() * 10f64.powf(-prec)).into()
    }
}
#[derive(Default)]
pub(crate) struct CeilFunc;
impl Function for CeilFunc {
    fn name(&self) -> &'static str {
        "ceil"
    }

    fn call(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Const {
        let dprec = Value::Const(ConstType::Num(0.).into());
        let [num, prec] = if let [num] = args {
            [num, &dprec]
        } else if let [num, prec] = args {
            [num, prec]
        } else {
            return ConstType::Error(CellError::WrongNumArguments {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                arguments: vec![1, 2],
            })
            .into();
        };
        let nc = num.eval(data, eval, func, ranges, spec);
        let pc = prec.eval(data, eval, func, ranges, spec);
        let arge: Const = ConstType::Error(CellError::BadArgument {
            cell: Some(spec.to_sloc()),
            cursor: 0,
            expected_types: vec!["Number".into(), "Whole number?".into()],
        })
        .into();
        let numnum = match &nc.ct {
            ConstType::Num(n) => n,
            ConstType::Error(e) => return ConstType::Error(e.clone()).into(),
            _ => return arge,
        };
        let prec = match &pc.ct {
            ConstType::Num(n) => {
                if n.round() != *n {
                    return arge;
                }
                *n
            }
            ConstType::Error(e) => return ConstType::Error(e.clone()).into(),
            _ => return arge,
        };
        ConstType::Num((numnum * 10f64.powf(prec)).ceil() * 10f64.powf(-prec)).into()
    }
}
#[derive(Default)]
pub(crate) struct PowFunc;
impl Function for PowFunc {
    fn name(&self) -> &'static str {
        "power"
    }

    fn call(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Const {
        let [base, exp] = args else {
            return ConstType::Error(CellError::WrongNumArguments {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                arguments: vec![2],
            })
            .into();
        };
        let bc = base.eval(data, eval, func, ranges, spec);
        let ec = exp.eval(data, eval, func, ranges, spec);
        let arge: Const = ConstType::Error(CellError::BadArgument {
            cell: Some(spec.to_sloc()),
            cursor: 0,
            expected_types: vec!["Number".into(), "Number".into()],
        })
        .into();
        let base = match &bc.ct {
            ConstType::Num(n) => n,
            ConstType::Error(e) => return ConstType::Error(e.clone()).into(),
            _ => return arge,
        };
        let exp = match &ec.ct {
            ConstType::Num(n) => *n,
            ConstType::Error(e) => return ConstType::Error(e.clone()).into(),
            _ => return arge,
        };
        ConstType::Num(base.powf(exp)).into()
    }
}
#[derive(Default)]
pub(crate) struct LogFunc;
impl Function for LogFunc {
    fn name(&self) -> &'static str {
        "log"
    }

    fn call(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Const {
        let [num, base] = {
            if let [num] = args {
                [
                    num,
                    &Value::Const(ConstType::Num(std::f64::consts::E).into()),
                ]
            } else if let [num, base] = args {
                [num, base]
            } else {
                return ConstType::Error(CellError::WrongNumArguments {
                    cell: Some(spec.to_sloc()),
                    cursor: 0,
                    arguments: vec![1, 2],
                })
                .into();
            }
        };
        let bc = num.eval(data, eval, func, ranges, spec);
        let ec = base.eval(data, eval, func, ranges, spec);
        let arge: Const = ConstType::Error(CellError::BadArgument {
            cell: Some(spec.to_sloc()),
            cursor: 0,
            expected_types: vec!["Number".into(), "Number".into()],
        })
        .into();
        let base = match &bc.ct {
            ConstType::Num(n) => n,
            ConstType::Error(e) => return ConstType::Error(e.clone()).into(),
            _ => return arge,
        };
        let exp = match &ec.ct {
            ConstType::Num(n) => *n,
            ConstType::Error(e) => return ConstType::Error(e.clone()).into(),
            _ => return arge,
        };
        ConstType::Num(base.log(exp)).into()
    }
}

impl Value {
    fn replace_args(&self, args: &[Self]) -> Self {
        // println!("Replacing {self:?}");
        match self {
            Value::Const(c) => match &c.ct {
                ConstType::Function(f) => {
                    Value::Const(ConstType::Function(Box::new(f.replace_args(args))).into())
                }
                _ => self.clone(),
            },
            Value::Ref(_) => self.clone(),
            Value::Special(s) => {
                if let Spec::dA(p) = s {
                    args.get(*p).cloned().unwrap_or(Self::Const(
                        ConstType::Error(CellError::WrongNumArguments {
                            cell: None,
                            cursor: 0,
                            arguments: vec![*p + 1],
                        })
                        .into(),
                    ))
                } else {
                    self.clone()
                }
            }
            Value::Add(l, r) => {
                Self::Add(Rc::new(l.replace_args(args)), Rc::new(r.replace_args(args)))
            }
            Value::Sub(l, r) => {
                Self::Sub(Rc::new(l.replace_args(args)), Rc::new(r.replace_args(args)))
            }
            Value::Mul(l, r) => {
                Self::Mul(Rc::new(l.replace_args(args)), Rc::new(r.replace_args(args)))
            }
            Value::Div(l, r) => {
                Self::Div(Rc::new(l.replace_args(args)), Rc::new(r.replace_args(args)))
            }
            Value::Lt(l, r) => {
                Self::Lt(Rc::new(l.replace_args(args)), Rc::new(r.replace_args(args)))
            }
            Value::Gt(l, r) => {
                Self::Gt(Rc::new(l.replace_args(args)), Rc::new(r.replace_args(args)))
            }
            Value::Eq(l, r) => {
                Self::Eq(Rc::new(l.replace_args(args)), Rc::new(r.replace_args(args)))
            }
            Value::Func { name, args: fargs } => Value::Func {
                name: name.clone(),
                args: fargs.iter().map(|a| a.replace_args(args)).collect(),
            },
            Value::RangeForm(_) => Self::Const(
                ConstType::Error(CellError::InvalidFunction {
                    cell: None,
                    cursor: 0,
                    reason: "Cannot use range formula in function",
                })
                .into(),
            ),
        }
    }
}

#[derive(Default)]
pub(crate) struct CallFunc;
impl Function for CallFunc {
    fn name(&self) -> &'static str {
        "call"
    }

    fn call(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Const {
        let Some(f) = args.get(0) else {
            return ConstType::Error(CellError::WrongNumArguments {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                arguments: vec![1, 2, 3, 4],
            })
            .into();
        };
        let f_eval = f.eval(data, eval, func, ranges, spec);
        if f_eval.is_error() {
            return f_eval.into_owned();
        }
        let ConstType::Function(f_val) = &f_eval.ct else {
            return ConstType::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Function".into(), "...".into()],
            })
            .into();
        };
        f_val
            .replace_args(&args[1..])
            .eval(data, eval, func, ranges, spec)
            .to_pos(spec.to_sloc())
    }
}

#[derive(Default)]
pub(crate) struct FilterFunc(Mutex<Vec<(SLoc, Vec<usize>)>>);
impl Function for FilterFunc {
    fn name(&self) -> &'static str {
        "filter"
    }

    fn call(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Const {
        let error: Const = ConstType::Error(CellError::BadArgument {
            cell: Some(spec.to_sloc()),
            cursor: 0,
            expected_types: vec!["Row".into(), "Filter".into()],
        })
        .into();
        // println!("filter 1");
        let Some(range) = args.get(0) else {
            return error;
        };
        // println!("filter 2");
        let ConstType::Range(range) = range.eval(data, eval, func, ranges, spec).ct.clone() else {
            return error;
        };
        // println!("filter 3");
        if range.width() != Bound::Fin(1) {
            return error;
        }

        // println!("filter 4");
        let Some(filter) = args.get(1) else {
            return error;
        };
        match spec.dSR {
            Some(src_row) => {
                let offset = spec.dR - src_row;
                // println!("Find offset {offset}");
                let src_col = spec.dSC.unwrap();
                let src_table = spec.dST.unwrap();
                let mut cache = self.0.lock().unwrap();
                if !cache
                    .iter()
                    .any(|(cell, _)| cell == &SLoc(src_table, src_col, src_row))
                {
                    cache.push((SLoc(src_table, src_col, src_row), vec![]));
                }
                let cache_row = &mut cache
                    .iter_mut()
                    .find(|(cell, _)| cell == &SLoc(src_table, src_col, src_row))
                    .expect("Just made the row")
                    .1;
                let start_row = range.0 .2;
                let col = range.0 .1;
                let table = range.0 .0;
                let mut offset_i = cache_row.len();
                if offset < (offset_i as i32) {
                    let row = start_row + (cache_row[offset as usize] as i32);
                    if let Ok(f_cell) = data.get(&SLoc(table, col, row), eval) {
                        if let Ok(f_val) = Value::from_str(&f_cell.val, table) {
                            let f_const = f_val.eval(data, eval, func, ranges, spec).into_owned();

                            let spec = spec
                                .clone()
                                .with_f_sloc(Some((SLoc(table, row, col), f_const.clone())));
                            let filter_const = filter.eval(data, eval, func, ranges, &spec);
                            if filter_const.ct.truthy() {
                                return f_const;
                            } else {
                                cache_row.clear();
                                offset_i = 0;
                            }
                        }
                    }
                }
                let mut offset_n = if offset_i == 0 {
                    0
                } else {
                    cache_row[offset_i - 1] + 1
                };
                drop(cache);

                while (offset_i as i32) < offset {
                    if offset_n > 10 {
                        return ConstType::Bool(false).into();
                    }
                    // let col = start_col + (offset_n as i32);
                    let row = start_row + (offset_n as i32);
                    // println!("Checking row {row}");
                    offset_n += 1;
                    let Ok(f_cell) = data.get(&SLoc(table, col, row), eval) else {
                        continue;
                    };
                    let Ok(f_val): Result<Value, _> = Value::from_str(&f_cell.val, table) else {
                        continue;
                    };
                    let f_const = f_val.eval(data, eval, func, ranges, spec).into_owned();
                    let spec = spec
                        .clone()
                        .with_f_sloc(Some((SLoc(table, row, col), f_const.clone())));
                    let filter_const = filter.eval(data, eval, func, ranges, &spec);
                    if filter_const.is_error() || filter_const.ct.truthy() {
                        let mut cache = self.0.lock().unwrap();
                        let cache_row = &mut cache
                            .iter_mut()
                            .find(|(cell, _)| cell == &SLoc(src_table, src_col, src_row))
                            .expect("Just made the row")
                            .1;
                        cache_row.insert(offset_i as usize, offset_n - 1);
                        offset_i += 1;
                        if offset_i == offset as usize {
                            return f_const;
                        }
                        // return filter_const.into_owned();
                    }
                }
                ConstType::Bool(false).into()
            }
            None => {
                let table = range.0 .0;
                let col = range.0 .1;
                let start_row = range.0 .2;
                for row in start_row..10 {
                    // println!("filter col {col}");
                    let Ok(f_cell) = data.get(&SLoc(table, col, row), eval) else {
                        continue;
                    };
                    // println!("Got cell {}", f_cell.val);
                    let Ok(f_val): Result<Value, _> = Value::from_str(&f_cell.val, table) else {
                        continue;
                    };
                    // println!("parse cell");
                    let f_const = f_val
                        .eval(
                            data,
                            eval,
                            func,
                            ranges,
                            &spec.clone().with_sloc(SLoc(table, col, row)),
                        )
                        .into_owned();
                    // println!("eval cell {:?}", f_const);
                    let spec = spec
                        .clone()
                        .with_f_sloc(Some((SLoc(table, row, col), f_const.clone())));
                    let filter_const = filter.eval(data, eval, func, ranges, &spec);
                    // println!("eval filter {:?}", filter_const);
                    if filter_const.is_error() {
                        return filter_const.into_owned();
                    }
                    if filter_const.ct.truthy() {
                        return f_const;
                    }
                }
                ConstType::Bool(false).into()
            }
        }
    }

    fn get_refs(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Vec<Range> {
        let arg_range = args.get(0).and_then(|c| {
            if let ConstType::Range(r) = &c.eval(data, eval, func, ranges, spec).ct {
                Some(r.clone())
            } else {
                None
            }
        });
        args.iter()
            .flat_map(|a| a.get_refs(data, eval, func, ranges, spec))
            .chain(arg_range)
            .collect()
    }
}
#[derive(Default)]
pub(crate) struct IntegrateFunc;
impl Function for IntegrateFunc {
    fn name(&self) -> &'static str {
        "intr"
    }

    fn call(
        &self,
        args: &[Value],
        data: &SheetData,
        eval: &mut SheetEval,
        func: &SheetFunc,
        ranges: &SheetRanges,
        spec: &SpecValues,
    ) -> Const {
        let [f, lb, ub] = args else {
            return ConstType::Error(CellError::WrongNumArguments {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                arguments: vec![3],
            })
            .into();
        };
        let f_eval = f.eval(data, eval, func, ranges, spec);
        if f_eval.is_error() {
            return f_eval.into_owned();
        }
        let ConstType::Function(f_val) = &f_eval.ct else {
            return ConstType::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Function".into()],
            })
            .into();
        };
        let lb = lb.eval(data, eval, func, ranges, spec);
        if lb.is_error() {
            return f_eval.into_owned();
        }
        let ConstType::Num(lb) = &lb.ct else {
            return ConstType::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 1,
                expected_types: vec!["Number".into()],
            })
            .into();
        };
        let ub = ub.eval(data, eval, func, ranges, spec);
        if ub.is_error() {
            return f_eval.into_owned();
        }
        let ConstType::Num(ub) = &ub.ct else {
            return ConstType::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 1,
                expected_types: vec!["Number".into()],
            })
            .into();
        };
        let iters = 401;
        let vals: Vec<Result<f64, Const>> = (0..iters)
            .map(|i| lb + (i as f64) / (iters as f64 - 1.) * (ub - lb))
            .map(|x| {
                let val = f_val.replace_args(&[Value::Const(ConstType::Num(x).into())]);
                let val = val.eval(data, eval, func, ranges, spec);
                if val.is_error() {
                    return Err(val.into_owned());
                }
                if let ConstType::Num(fout) = &val.ct {
                    // println!("f({x})={fout}");
                    Ok(*fout)
                } else {
                    Err(ConstType::Error(CellError::BadArgument {
                        cell: Some(spec.to_sloc()),
                        cursor: 0,
                        expected_types: vec!["Function number -> number".into()],
                    })
                    .into())
                }
            })
            .collect();
        for v in vals.iter() {
            if let Err(err) = v {
                return err.clone();
            }
        }
        let vals: Vec<f64> = vals.into_iter().map(Result::unwrap).collect();
        ConstType::Num(
            vals.into_iter()
                .enumerate()
                .map(|(i, v)| {
                    let simp_w = if i == 0 || i == iters - 1 {
                        v
                    } else if i % 2 == 0 {
                        2. * v
                    } else {
                        4. * v
                    };
                    simp_w * (ub - lb) / (iters as f64 - 1.) / 3.
                })
                .sum(),
        )
        .into()
    }
}
pub(crate) enum NumConstType {
    Pi,
    E,
}
pub(crate) struct ConstFunc(pub(crate) NumConstType);
impl Function for ConstFunc {
    fn name(&self) -> &'static str {
        match self.0 {
            NumConstType::Pi => "pi",
            NumConstType::E => "e",
        }
    }

    fn call(
        &self,
        _args: &[Value],
        _data: &SheetData,
        _eval: &mut SheetEval,
        _func: &SheetFunc,
        _ranges: &SheetRanges,
        _spec: &SpecValues,
    ) -> Const {
        ConstType::Num(match self.0 {
            NumConstType::Pi => std::f64::consts::PI,
            NumConstType::E => std::f64::consts::E,
        })
        .into()
    }
}

#[cfg(test)]
mod test {

    use crate::formula::*;

    #[test]
    fn const_type_parse() {
        let cerr = Err(ConstType::Error(CellError::InvalidFormula {
            cell: None,
            cursor: 0,
            reason: "Not valid constant",
        }));
        assert_eq!(ConstType::from_str("true"), Ok(ConstType::Bool(true)));
        assert_eq!(ConstType::from_str("7"), Ok(ConstType::Num(7.0)));
        assert_eq!(ConstType::from_str("7.5"), Ok(ConstType::Num(7.5)));
        assert_eq!(ConstType::from_str("-7.5"), Ok(ConstType::Num(-7.5)));
        assert_eq!(ConstType::from_str("-7.5e"), cerr);
        assert_eq!(
            ConstType::from_str("\"Hi\""),
            Ok(ConstType::String("Hi".into()))
        );
        assert_eq!(ConstType::from_str("\"Hi"), cerr);
        assert_eq!(ConstType::from_str("Hi\""), cerr);
        assert_eq!(ConstType::from_str("abcd"), cerr);

        assert_eq!(ConstType::Num(1.).merge_err(&ConstType::Num(2.)), None);
        assert_eq!(
            ConstType::Error(CellError::ReferenceLoop {
                loop_point: Some(SLoc("", 0, 0))
            })
            .merge_err(&ConstType::Num(2.)),
            Some(CellError::ReferenceLoop {
                loop_point: Some(SLoc("", 0, 0))
            })
        );
        assert_eq!(
            ConstType::Num(2.).merge_err(&ConstType::Error(CellError::ReferenceLoop {
                loop_point: Some(SLoc("", 0, 0))
            })),
            Some(CellError::ReferenceLoop {
                loop_point: Some(SLoc("", 0, 0))
            })
        );
        assert_eq!(
            ConstType::Error(CellError::ReferenceLoop {
                loop_point: Some(SLoc("", 0, 0))
            })
            .merge_err(&ConstType::Error(CellError::ReferenceLoop {
                loop_point: Some(SLoc("", 1, 0))
            })),
            Some(CellError::ReferenceLoop {
                loop_point: Some(SLoc("", 0, 0))
            })
        );
    }
    #[test]
    fn range_parse() {
        assert_eq!(
            is_range("A1:A1", ""),
            Some(Range(
                SLoc("", 0, 0),
                SLocBound(Bound::Fin(0), Bound::Fin(0))
            ))
        );
        assert_eq!(
            is_range("A1:A5", ""),
            Some(Range(
                SLoc("", 0, 0),
                SLocBound(Bound::Fin(0), Bound::Fin(4))
            ))
        );
        assert_eq!(
            is_range("A1:c5", ""),
            Some(Range(
                SLoc("", 0, 0),
                SLocBound(Bound::Fin(2), Bound::Fin(4))
            ))
        );
        assert_eq!(
            is_range("A1:AA123", ""),
            Some(Range(
                SLoc("", 0, 0),
                SLocBound(Bound::Fin(26), Bound::Fin(122))
            ))
        );
        assert_eq!(
            is_range("A1:ABCZ12345", ""),
            Some(Range(
                SLoc("", 0, 0),
                SLocBound(Bound::Fin(19031), Bound::Fin(12344))
            ))
        );
        assert_eq!(
            is_range("ACC1:ABCZ12345", ""),
            Some(Range(
                SLoc("", 756, 0),
                SLocBound(Bound::Fin(19031), Bound::Fin(12344))
            ))
        );
        assert_eq!(
            is_range("A1:", ""),
            Some(Range(SLoc("", 0, 0), SLocBound(Bound::Inf, Bound::Inf)))
        );
        assert_eq!(
            is_range("A1:A", ""),
            Some(Range(SLoc("", 0, 0), SLocBound(Bound::Fin(0), Bound::Inf)))
        );
        assert_eq!(
            is_range("A1:C", ""),
            Some(Range(SLoc("", 0, 0), SLocBound(Bound::Fin(2), Bound::Inf)))
        );
        assert_eq!(
            is_range("A1:CC", ""),
            Some(Range(SLoc("", 0, 0), SLocBound(Bound::Fin(80), Bound::Inf)))
        );
        assert_eq!(
            is_range("A1:1", ""),
            Some(Range(SLoc("", 0, 0), SLocBound(Bound::Inf, Bound::Fin(0))))
        );
        assert_eq!(
            is_range("A1:20", ""),
            Some(Range(SLoc("", 0, 0), SLocBound(Bound::Inf, Bound::Fin(19))))
        );
        assert_eq!(is_range("A0:A", ""), None,);
        assert_eq!(is_range("A3:A1", ""), None,);
        assert_eq!(is_range("B1:A1", ""), None,);
        assert_eq!(is_range("B3:A", ""), None,);
        assert_eq!(is_range("B3:1", ""), None,);
        assert_eq!(is_range("ABCDE0:ABCZZ1", ""), None,);
        assert_eq!(is_range("ABC3:ABK1", ""), None,);
        assert_eq!(is_range("SUS1:AMO1", ""), None,);
        assert_eq!(is_range("NGU3:S", ""), None,);
        assert_eq!(is_range("ABCD420:69", ""), None);

        assert!(
            Range(SLoc("", 2, 2), SLocBound(Bound::Fin(5), Bound::Fin(5)))
                .in_range(&SLoc("", 2, 2))
        );
        assert!(
            Range(SLoc("", 2, 2), SLocBound(Bound::Fin(5), Bound::Fin(5)))
                .in_range(&SLoc("", 2, 3))
        );
        assert!(
            Range(SLoc("", 2, 2), SLocBound(Bound::Fin(5), Bound::Fin(5)))
                .in_range(&SLoc("", 3, 3))
        );
        assert!(
            Range(SLoc("", 2, 2), SLocBound(Bound::Fin(5), Bound::Fin(5)))
                .in_range(&SLoc("", 5, 3))
        );
        assert!(
            Range(SLoc("", 2, 2), SLocBound(Bound::Fin(5), Bound::Fin(5)))
                .in_range(&SLoc("", 5, 5))
        );
        assert!(
            !Range(SLoc("", 2, 2), SLocBound(Bound::Fin(5), Bound::Fin(5)))
                .in_range(&SLoc("", 0, 4))
        );
        assert!(
            !Range(SLoc("", 2, 2), SLocBound(Bound::Fin(5), Bound::Fin(5)))
                .in_range(&SLoc("", 4, 0))
        );
        assert!(
            !Range(SLoc("", 2, 2), SLocBound(Bound::Fin(5), Bound::Fin(5)))
                .in_range(&SLoc("", 8, 4))
        );
        assert!(
            !Range(SLoc("", 2, 2), SLocBound(Bound::Fin(5), Bound::Fin(5)))
                .in_range(&SLoc("", 4, 8))
        );

        assert!(
            Range(SLoc("", 2, 2), SLocBound(Bound::Fin(5), Bound::Inf)).in_range(&SLoc("", 2, 2))
        );
        assert!(
            Range(SLoc("", 2, 2), SLocBound(Bound::Fin(5), Bound::Inf)).in_range(&SLoc("", 2, 3))
        );
        assert!(
            Range(SLoc("", 2, 2), SLocBound(Bound::Fin(5), Bound::Inf)).in_range(&SLoc("", 3, 3))
        );
        assert!(
            Range(SLoc("", 2, 2), SLocBound(Bound::Fin(5), Bound::Inf)).in_range(&SLoc("", 5, 3))
        );
        assert!(
            Range(SLoc("", 2, 2), SLocBound(Bound::Fin(5), Bound::Inf)).in_range(&SLoc("", 5, 5))
        );
        assert!(
            !Range(SLoc("", 2, 2), SLocBound(Bound::Fin(5), Bound::Inf)).in_range(&SLoc("", 0, 4))
        );
        assert!(
            !Range(SLoc("", 2, 2), SLocBound(Bound::Fin(5), Bound::Inf)).in_range(&SLoc("", 4, 0))
        );
        assert!(
            !Range(SLoc("", 2, 2), SLocBound(Bound::Fin(5), Bound::Inf)).in_range(&SLoc("", 8, 4))
        );
        assert!(
            Range(SLoc("", 2, 2), SLocBound(Bound::Fin(5), Bound::Inf)).in_range(&SLoc("", 4, 8))
        );

        assert!(
            Range(SLoc("", 2, 2), SLocBound(Bound::Inf, Bound::Fin(5))).in_range(&SLoc("", 2, 2))
        );
        assert!(
            Range(SLoc("", 2, 2), SLocBound(Bound::Inf, Bound::Fin(5))).in_range(&SLoc("", 2, 3))
        );
        assert!(
            Range(SLoc("", 2, 2), SLocBound(Bound::Inf, Bound::Fin(5))).in_range(&SLoc("", 3, 3))
        );
        assert!(
            Range(SLoc("", 2, 2), SLocBound(Bound::Inf, Bound::Fin(5))).in_range(&SLoc("", 5, 3))
        );
        assert!(
            Range(SLoc("", 2, 2), SLocBound(Bound::Inf, Bound::Fin(5))).in_range(&SLoc("", 5, 5))
        );
        assert!(
            !Range(SLoc("", 2, 2), SLocBound(Bound::Inf, Bound::Fin(5))).in_range(&SLoc("", 0, 4))
        );
        assert!(
            !Range(SLoc("", 2, 2), SLocBound(Bound::Inf, Bound::Fin(5))).in_range(&SLoc("", 4, 0))
        );
        assert!(
            Range(SLoc("", 2, 2), SLocBound(Bound::Inf, Bound::Fin(5))).in_range(&SLoc("", 8, 4))
        );
        assert!(
            !Range(SLoc("", 2, 2), SLocBound(Bound::Inf, Bound::Fin(5))).in_range(&SLoc("", 4, 8))
        );

        assert!(Range(SLoc("", 2, 2), SLocBound(Bound::Inf, Bound::Inf)).in_range(&SLoc("", 2, 2)));
        assert!(Range(SLoc("", 2, 2), SLocBound(Bound::Inf, Bound::Inf)).in_range(&SLoc("", 2, 3)));
        assert!(Range(SLoc("", 2, 2), SLocBound(Bound::Inf, Bound::Inf)).in_range(&SLoc("", 3, 3)));
        assert!(Range(SLoc("", 2, 2), SLocBound(Bound::Inf, Bound::Inf)).in_range(&SLoc("", 5, 3)));
        assert!(Range(SLoc("", 2, 2), SLocBound(Bound::Inf, Bound::Inf)).in_range(&SLoc("", 5, 5)));
        assert!(!Range(SLoc("", 2, 2), SLocBound(Bound::Inf, Bound::Inf)).in_range(&SLoc("", 0, 4)));
        assert!(!Range(SLoc("", 2, 2), SLocBound(Bound::Inf, Bound::Inf)).in_range(&SLoc("", 4, 0)));
        assert!(Range(SLoc("", 2, 2), SLocBound(Bound::Inf, Bound::Inf)).in_range(&SLoc("", 8, 4)));
        assert!(Range(SLoc("", 2, 2), SLocBound(Bound::Inf, Bound::Inf)).in_range(&SLoc("", 4, 8)));
    }
    #[test]
    fn ref_inv() {
        let tests = [
            "''A1",
            "''B3",
            "''AA72",
            "''AAA72",
            "''AAZZ99999",
            "'test.gg'G1",
        ];
        let tests2 = [
            SLoc("", 0, 0),
            SLoc("", 1, 3),
            SLoc("", 26, 26),
            SLoc("", 57023, 99384),
            SLoc("test", 8, 9),
        ];
        for t in tests {
            let forward = is_ref(t, "").unwrap();
            let inv = show_ref_global(&forward);
            assert_eq!(t, inv);
        }
        for t in tests2 {
            let forward = show_ref_global(&t);
            let inv = is_ref(&forward, "");
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
        let mut s = SpecValues::from_sloc(SLoc("", 1, 1));
        assert_eq!(s.to_sloc(), SLoc("", 1, 1));
        assert!(!s.invalid_depth(0));
        s.depth = 5;
        assert!(!s.invalid_depth(5));
        assert!(!s.invalid_depth(0));
        assert!(s.invalid_depth(6));
        s = s.with_sloc(SLoc("", 2, 2));
        assert_eq!(s.to_sloc(), SLoc("", 2, 2));
        assert_eq!(s.depth, 6);
        s = s.with_f_sloc(Some((SLoc("", 2, 2), ConstType::Bool(false).into())));
        assert_eq!(s.depth, 7);
        s = s.with_f_sloc(None);
        assert_eq!(s.depth, 8);
        s = s.with_s_sloc(Some(SLoc("", 2, 2)));
        assert_eq!(s.depth, 9);
        s = s.with_s_sloc(None);
        assert_eq!(s.depth, 10);
    }
    #[test]
    fn split_paren_test() {
        assert_eq!(
            split_parens(&form_tokens("sum(sum(1,2),3)")),
            Ok(ParenTree::Paren(Cow::Owned(vec![
                (ParenTree::Token("sum", 0)),
                (ParenTree::Paren(Cow::Owned(vec![
                    (ParenTree::Token("sum", 4)),
                    (ParenTree::Paren(Cow::Owned(vec![
                        (ParenTree::Token("1", 8)),
                        (ParenTree::Token(",", 9)),
                        (ParenTree::Token("2", 10))
                    ]))),
                    (ParenTree::Token(",", 12)),
                    (ParenTree::Token("3", 13))
                ])))
            ])))
        );
        assert_eq!(
            split_parens(&form_tokens("sum(range(1,2,3, 4),3,A5)")),
            Ok(ParenTree::Paren(Cow::Owned(vec![
                (ParenTree::Token("sum", 0)),
                (ParenTree::Paren(Cow::Owned(vec![
                    (ParenTree::Token("range", 4)),
                    (ParenTree::Paren(Cow::Owned(vec![
                        (ParenTree::Token("1", 10)),
                        (ParenTree::Token(",", 11)),
                        (ParenTree::Token("2", 12)),
                        (ParenTree::Token(",", 13)),
                        (ParenTree::Token("3", 14)),
                        (ParenTree::Token(",", 15)),
                        (ParenTree::Token("4", 17))
                    ]))),
                    (ParenTree::Token(",", 19)),
                    (ParenTree::Token("3", 20)),
                    (ParenTree::Token(",", 21)),
                    (ParenTree::Token("A5", 22))
                ])))
            ])))
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
            form_tree(&split_parens(&form_tokens("78")).unwrap(), ""),
            (Value::Const(ConstType::Num(78.).into()), false),
        );
        assert_eq!(
            form_tree(&split_parens(&form_tokens("A5")).unwrap(), ""),
            (Value::Ref(SLoc("", 0, 4)), false),
        );
        assert_eq!(
            form_tree(&split_parens(&form_tokens("A7+C5+3")).unwrap(), ""),
            (
                Value::Add(
                    Rc::new(Value::Add(
                        Rc::new(Value::Ref(SLoc("", 0, 6))),
                        Rc::new(Value::Ref(SLoc("", 2, 4)))
                    )),
                    Rc::new(Value::Const(ConstType::Num(3.).into()))
                ),
                false
            ),
        );
        assert_eq!(
            form_tree(&split_parens(&form_tokens("5+sum(A1:A)")).unwrap(), ""),
            (
                Value::Add(
                    Rc::new(Value::Const(ConstType::Num(5.).into())),
                    Rc::new(Value::Func {
                        name: "sum".into(),
                        args: vec![Value::Const(
                            ConstType::Range(Range(
                                SLoc("", 0, 0),
                                SLocBound(Bound::Fin(0), Bound::Inf)
                            ))
                            .into()
                        )]
                    })
                ),
                false
            ),
        );
        assert_eq!(
            form_tree(&split_parens(&form_tokens("5+sum(A1:A)")).unwrap(), ""),
            (
                Value::Add(
                    Rc::new(Value::Const(ConstType::Num(5.).into())),
                    Rc::new(Value::Func {
                        name: "sum".into(),
                        args: vec![Value::Const(
                            ConstType::Range(Range(
                                SLoc("", 0, 0),
                                SLocBound(Bound::Fin(0), Bound::Inf)
                            ))
                            .into()
                        )]
                    })
                ),
                false
            ),
        );
        assert_eq!(
            form_tree(&split_parens(&form_tokens("range(1,1,5,A3)")).unwrap(), ""),
            (
                Value::Func {
                    name: "range".into(),
                    args: vec![
                        Value::Const(ConstType::Num(1.).into()),
                        Value::Const(ConstType::Num(1.).into()),
                        Value::Const(ConstType::Num(5.).into()),
                        Value::Ref(SLoc("", 0, 2))
                    ]
                },
                false
            ),
        );
        assert_eq!(
            form_tree(
                &split_parens(&dbg!(form_tokens("if(sum(A1:B)<20, 20, \"Small\")"))).unwrap(),
                ""
            ),
            (
                Value::Func {
                    name: "if".into(),
                    args: vec![
                        Value::Lt(
                            Rc::new(Value::Func {
                                name: "sum".into(),
                                args: vec![Value::Const(
                                    ConstType::Range(Range(
                                        SLoc("", 0, 0),
                                        SLocBound(Bound::Fin(1), Bound::Inf)
                                    ))
                                    .into()
                                )]
                            }),
                            Rc::new(Value::Const(ConstType::Num(20.).into()))
                        ),
                        Value::Const(ConstType::Num(20.).into()),
                        Value::Const(ConstType::String("Small".into()).into()),
                        // Value::Const(ConstType::Num(30.).into())
                    ]
                },
                false
            ),
        );
        assert_eq!(
            form_tree(
                &split_parens(&dbg!(form_tokens("sum(range(0,0,10,10), 20)"))).unwrap(),
                ""
            ),
            (
                Value::Func {
                    name: "sum".into(),
                    args: vec![
                        Value::Func {
                            name: "range".into(),
                            args: vec![
                                Value::Const(ConstType::Num(0.).into()),
                                Value::Const(ConstType::Num(0.).into()),
                                Value::Const(ConstType::Num(10.).into()),
                                Value::Const(ConstType::Num(10.).into()),
                            ]
                        },
                        Value::Const(ConstType::Num(20.).into()),
                        // Value::Const(ConstType::Num(30.).into())
                    ]
                },
                false
            ),
        );
        assert_eq!(
            form_tree(
                &split_parens(&dbg!(form_tokens(
                    "if(3<1, \"\\\\String1\", \"S2\\\\\\\"\")"
                )))
                .unwrap(),
                ""
            ),
            (
                Value::Func {
                    name: "if".into(),
                    args: vec![
                        Value::Lt(
                            Rc::new(Value::Const(ConstType::Num(3.).into())),
                            Rc::new(Value::Const(ConstType::Num(1.).into()))
                        ),
                        Value::Const(ConstType::String("\\String1".into()).into()),
                        Value::Const(ConstType::String("S2\\\"".into()).into()),
                    ]
                },
                false
            ),
        );
    }
    #[test]
    fn func_range_test() {
        let mut sheet = Sheet::new();
        let spec = SpecValues::from_sloc(SLoc("", 0, 0));
        // sheet.insert((0,0), )
        assert_eq!(RangeFunc.name(), "range");
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(3.).into()),
                    Value::Const(ConstType::Num(3.).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            vec![Range(
                SLoc("", 1, 1),
                SLocBound(Bound::Fin(3), Bound::Fin(3))
            )]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(3.).into()),
                    Value::Const(ConstType::Num(3.).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            ConstType::Range(Range(
                SLoc("", 1, 1),
                SLocBound(Bound::Fin(3), Bound::Fin(3))
            ))
            .into()
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            vec![Range(SLoc("", 1, 1), SLocBound(Bound::Inf, Bound::Inf))]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            ConstType::Range(Range(SLoc("", 1, 1), SLocBound(Bound::Inf, Bound::Inf))).into()
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(3.).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(3.).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            ConstType::Error(CellError::WrongNumArguments {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                arguments: vec![2, 4]
            })
            .into()
        );

        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            vec![Range(SLoc("", 1, 1), SLocBound(Bound::Inf, Bound::Inf))]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            ConstType::Range(Range(SLoc("", 1, 1), SLocBound(Bound::Inf, Bound::Inf))).into()
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(ConstType::Num(-1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(ConstType::Num(-1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            ConstType::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into()]
            })
            .into()
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(ConstType::Num(1.6).into()),
                    Value::Const(ConstType::Num(1.).into()),
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(ConstType::Num(1.6).into()),
                    Value::Const(ConstType::Num(1.).into()),
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            ConstType::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into()]
            })
            .into()
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.6).into()),
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.6).into()),
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            ConstType::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into()]
            })
            .into()
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(-1.).into()),
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(-1.).into()),
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            ConstType::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into()]
            })
            .into()
        );

        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(ConstType::Num(-1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(3.).into()),
                    Value::Const(ConstType::Num(3.).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(ConstType::Num(-1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(3.).into()),
                    Value::Const(ConstType::Num(3.).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            ConstType::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into()]
            })
            .into()
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(ConstType::Num(1.6).into()),
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(3.).into()),
                    Value::Const(ConstType::Num(3.).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(ConstType::Num(1.6).into()),
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(3.).into()),
                    Value::Const(ConstType::Num(3.).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            ConstType::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into()]
            })
            .into()
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.6).into()),
                    Value::Const(ConstType::Num(3.).into()),
                    Value::Const(ConstType::Num(3.).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.6).into()),
                    Value::Const(ConstType::Num(3.).into()),
                    Value::Const(ConstType::Num(3.).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            ConstType::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into()]
            })
            .into()
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(-1.).into()),
                    Value::Const(ConstType::Num(3.).into()),
                    Value::Const(ConstType::Num(3.).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(-1.).into()),
                    Value::Const(ConstType::Num(3.).into()),
                    Value::Const(ConstType::Num(3.).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            ConstType::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into()]
            })
            .into()
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(-3.).into()),
                    Value::Const(ConstType::Num(3.).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(-3.).into()),
                    Value::Const(ConstType::Num(3.).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            ConstType::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into(), ".I".into()]
            })
            .into()
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(3.6).into()),
                    Value::Const(ConstType::Num(3.).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(3.6).into()),
                    Value::Const(ConstType::Num(3.).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            ConstType::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into(), ".I".into()]
            })
            .into()
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(3.).into()),
                    Value::Const(ConstType::Num(3.6).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(3.).into()),
                    Value::Const(ConstType::Num(3.6).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            ConstType::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into(), ".I".into()]
            })
            .into()
        );
        assert_eq!(
            RangeFunc.get_refs(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(3.).into()),
                    Value::Const(ConstType::Num(-3.).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec
            ),
            vec![]
        );
        assert_eq!(
            RangeFunc.call(
                &[
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(1.).into()),
                    Value::Const(ConstType::Num(3.).into()),
                    Value::Const(ConstType::Num(-3.).into())
                ],
                &sheet.data,
                &mut sheet.eval,
                &sheet.funcs,
                &sheet.ranges,
                &spec,
            ),
            ConstType::Error(CellError::BadArgument {
                cell: Some(spec.to_sloc()),
                cursor: 0,
                expected_types: vec!["Whole number".into(), ".I".into()]
            })
            .into()
        );
    }
    #[test]
    fn common_usage() {
        let mut sheet = Sheet::new();
        assert!(sheet
            .insert(SLoc("", 0, 0), CellData::default().val("2".into()),)
            .is_none());
        assert!(sheet
            .insert(SLoc("", 0, 1), CellData::default().val("3".into()))
            .is_none());

        assert!(sheet
            .insert(SLoc("", 1, 0), CellData::default().val("=A1+A2".into()),)
            .is_none());
        sheet.recompute();
        assert_eq!(
            sheet.get(&SLoc("", 0, 0)),
            Ok(&CellData::default()
                .val("2".into())
                .display(Some(ConstType::Num(2.).into()))
                .deps([SLoc("", 1, 0)].into_iter().collect()))
        );
        assert_eq!(
            sheet.get(&SLoc("", 0, 1)),
            Ok(&CellData::default()
                .val("3".into())
                .val("3".into())
                .display(Some(ConstType::Num(3.).into()))
                .deps([SLoc("", 1, 0)].into_iter().collect()))
        );

        assert_eq!(
            sheet.get(&SLoc("", 1, 0)),
            Ok(&CellData::default()
                .val("=A1+A2".into())
                .display(Some(ConstType::Num(5.).into())))
        );
        assert!(sheet
            .insert(
                SLoc("", 1, 0),
                CellData::default()
                    .val("=9e9e9".into())
                    .display(None)
                    .deps(HashSet::new())
            )
            .is_some());
        sheet.recompute();
        // dbg!(&sheet);
        let d = sheet.get(&SLoc("", 1, 0)).unwrap();
        assert_eq!(d.val, "=9e9e9");
        // assert!(d
        //     .display
        //     .as_ref()
        //     .is_some_and(|f| f.contains("InvalidFormula")));
        assert!(matches!(
            d.display,
            Some(Const {
                ct: ConstType::Error(CellError::InvalidFormula {
                    cell: _,
                    cursor: _,
                    reason: _
                }),
                ..
            })
        ));
        assert!(sheet
            .insert(
                SLoc("", 1, 0),
                CellData::default()
                    .val("=".into())
                    .display(None)
                    .deps(HashSet::new())
            )
            .is_some());
        sheet.recompute();
        // dbg!(&sheet);
        let d = sheet.get(&SLoc("", 1, 0)).unwrap();
        assert_eq!(d.val, "=");
        // assert!(d
        //     .display
        //     .as_ref()
        //     .is_some_and(|f| f.contains("InvalidFormula")));
        assert!(matches!(
            d.display,
            Some(Const {
                ct: ConstType::Error(CellError::InvalidFormula {
                    cell: _,
                    cursor: _,
                    reason: _
                }),
                ..
            })
        ));

        sheet.set_disp(&SLoc("", 1, 0), None);
        let d = sheet.get(&SLoc("", 1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData::default()
                .val("=".into())
                .display(None)
                .deps([].into_iter().collect())
        );
        sheet.set_disp(
            &SLoc("", 1, 0),
            Some(ConstType::from_str("\"Not error\"").unwrap().into()),
        );
        let d = sheet.get(&SLoc("", 1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData::default()
                .val("=".into())
                .display(Some(ConstType::String("Not error".into()).into()))
                .deps([].into_iter().collect())
        );
        sheet.set_disp_err(
            &SLoc("", 1, 0),
            CellError::MissingCell {
                cell: Some(SLoc("", 100, 100)),
            },
        );
        let d = sheet.get(&SLoc("", 1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData::default()
                .val("=".into())
                .display(Some(Const {
                    ct: ConstType::Error(CellError::MissingCell {
                        cell: Some(SLoc("", 100, 100))
                    }),
                    ..Const::default_err()
                }))
                .deps([].into_iter().collect()),
        );
        assert!(sheet.contains_key(&SLoc("", 0, 0)));
        assert!(!sheet.contains_key(&SLoc("", 2, 2)));

        assert_eq!(
            sheet.get_display(&SLoc("", 1, 0)),
            Some(
                ConstType::Error(CellError::MissingCell {
                    cell: Some(SLoc("", 100, 100))
                })
                .into()
            )
        );
        assert_eq!(sheet.iter().count(), 3);
        assert_eq!(sheet.iter_mut().count(), 3);
        assert!(sheet.iter().all(|(x, _)| x.1 < 2 && x.2 < 2));
        assert_eq!(
            sheet.val_mut(&SLoc("", 0, 0)),
            Some(Box::leak(Box::new("2".into())))
        );
    }

    #[test]
    fn sum() {
        let mut sheet = Sheet::new();
        assert!(sheet
            .insert(SLoc("", 0, 0), CellData::default().val("2".into()),)
            .is_none());
        assert!(sheet
            .insert(SLoc("", 0, 1), CellData::default().val("3".into()))
            .is_none());
        assert!(sheet
            .insert(
                SLoc("", 1, 0),
                CellData::default()
                    .val("=sum(A1:A10)".into())
                    .display(None)
                    .deps(HashSet::new())
            )
            .is_none());
        sheet.recompute();
        // dbg!(.&sheet);
        assert_eq!(
            sheet.get(&SLoc("", 0, 0)),
            Ok(&CellData::default()
                .val("2".into())
                .display(Some(ConstType::Num(2.).into()))
                .deps([SLoc("", 1, 0)].into_iter().collect()))
        );
        assert_eq!(
            sheet.get(&SLoc("", 0, 1)),
            Ok(&CellData::default()
                .val("3".into())
                .display(Some(ConstType::Num(3.).into()))
                .deps([SLoc("", 1, 0)].into_iter().collect()))
        );
        assert_eq!(
            sheet.get(&SLoc("", 1, 0)),
            Ok(&CellData::default()
                .val("=sum(A1:A10)".into())
                .display(Some(ConstType::Num(5.).into()))
                .deps(HashSet::new()))
        );
    }
    #[test]
    fn if_test() {
        let mut sheet = Sheet::new();
        assert!(sheet
            .insert(SLoc("", 0, 0), CellData::default().val("2".into()),)
            .is_none());
        assert!(sheet
            .insert(SLoc("", 0, 1), CellData::default().val("3".into()))
            .is_none());
        assert!(sheet
            .insert(SLoc("", 1, 1), CellData::default().val("0.6".into()))
            .is_none());
        assert!(sheet
            .insert(
                SLoc("", 1, 0),
                CellData::default()
                    .val("=if(A1<A2, A2, A1)".into())
                    .display(None)
                    .deps(HashSet::new())
            )
            .is_none());
        sheet.recompute();
        assert_eq!(
            sheet.get(&SLoc("", 0, 0)),
            Ok(&CellData::default()
                .val("2".into())
                .display(Some(ConstType::Num(2.).into()))
                .deps([SLoc("", 1, 0)].into_iter().collect()))
        );
        assert_eq!(
            sheet.get(&SLoc("", 0, 1)),
            Ok(&CellData::default()
                .val("3".into())
                .display(Some(ConstType::Num(3.).into()))
                .deps([SLoc("", 1, 0)].into_iter().collect()))
        );
        assert_eq!(
            sheet.get(&SLoc("", 1, 0)),
            Ok(&CellData::default()
                .val("=if(A1<A2, A2, A1)".into())
                .display(Some(ConstType::Num(3.).into()))
                .deps(HashSet::new()))
        );

        assert!(sheet
            .insert(
                SLoc("", 0, 0),
                CellData::default()
                    .val("=7".into())
                    .display(None)
                    .deps(HashSet::new())
            )
            .is_some());
        sheet.recompute();
        // dbg!(&sheet);
        assert_eq!(
            sheet.get(&SLoc("", 0, 0)),
            Ok(&CellData::default()
                .val("=7".into())
                .display(Some(ConstType::Num(7.).into()))
                .deps([SLoc("", 1, 0)].into_iter().collect()))
        );
        assert_eq!(
            sheet.get(&SLoc("", 0, 1)),
            Ok(&CellData::default()
                .val("3".into())
                .display(Some(ConstType::Num(3.).into()))
                .deps([SLoc("", 1, 0)].into_iter().collect()))
        );
        assert_eq!(
            sheet.get(&SLoc("", 1, 0)),
            Ok(&CellData::default()
                .val("=if(A1<A2, A2, A1)".into())
                .display(Some(ConstType::Num(7.).into()))
                .deps(HashSet::new()))
        );
        assert!(sheet
            .insert(
                SLoc("", 1, 0),
                CellData::default()
                    .val("=if(A1<A2 & B2<A2, A2, B2)*2".into())
                    .display(None)
                    .deps(HashSet::new())
            )
            .is_some());
        sheet.recompute();
        let d = sheet.get(&SLoc("", 1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData::default()
                .val("=if(A1<A2 & B2<A2, A2, B2)*2".into())
                .display(Some(ConstType::Num(1.2).into()))
                .deps(HashSet::new())
        );
        assert!(sheet
            .insert(
                SLoc("", 0, 0),
                CellData::default()
                    .val("1".into())
                    .display(None)
                    .deps(HashSet::new())
            )
            .is_some());
        sheet.recompute();
        let d = sheet.get(&SLoc("", 1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData::default()
                .val("=if(A1<A2 & B2<A2, A2, B2)*2".into())
                .display(Some(ConstType::Num(6.).into()))
                .deps(HashSet::new())
        );

        dbg!(&sheet);
        assert!(sheet
            .insert(
                SLoc("", 1, 0),
                CellData::default()
                    .val("=if(A1>A2, A2, A1)".into())
                    .display(None)
                    .deps(HashSet::new())
            )
            .is_some());
        sheet.recompute();
        assert_eq!(
            sheet.get(&SLoc("", 0, 0)),
            Ok(&CellData::default()
                .val("1".into())
                .display(Some(ConstType::Num(1.).into()))
                .deps([SLoc("", 1, 0)].into_iter().collect()))
        );
        assert_eq!(
            sheet.get(&SLoc("", 0, 1)),
            Ok(&CellData::default()
                .val("3".into())
                .display(Some(ConstType::Num(3.).into()))
                .deps([SLoc("", 1, 0)].into_iter().collect()))
        );
        assert_eq!(
            sheet.get(&SLoc("", 1, 0)),
            Ok(&CellData::default()
                .val("=if(A1>A2, A2, A1)".into())
                .display(Some(ConstType::Num(1.).into()))
                .deps(HashSet::new()))
        );
    }

    #[test]
    fn range() {
        let mut sheet = Sheet::new();
        assert!(sheet
            .insert(SLoc("", 0, 0), CellData::default().val("2".into()),)
            .is_none());
        assert!(sheet
            .insert(SLoc("", 0, 1), CellData::default().val("3".into()))
            .is_none());
        assert!(sheet
            .insert(SLoc("", 0, 6), CellData::default().val("3".into()))
            .is_none());
        assert!(sheet
            .insert(
                SLoc("", 1, 0),
                CellData::default()
                    .val("=sum(range(0,0,0,10),value(.T,0,1))".into())
                    .display(None)
                    .deps(HashSet::new())
            )
            .is_none());
        sheet.recompute();
        assert_eq!(
            sheet.get(&SLoc("", 0, 0)),
            Ok(&CellData::default()
                .val("2".into())
                .display(Some(ConstType::Num(2.).into()))
                .deps([SLoc("", 1, 0)].into_iter().collect()))
        );
        assert_eq!(
            sheet.get(&SLoc("", 0, 1)),
            Ok(&CellData::default()
                .val("3".into())
                .display(Some(ConstType::Num(3.).into()))
                .deps([SLoc("", 1, 0)].into_iter().collect()))
        );
        let d = sheet.get(&SLoc("", 1, 0)).unwrap();
        assert_eq!(d.display, Some(ConstType::Num(11.).into()));

        assert!(sheet
            .insert(
                SLoc("", 1, 0),
                CellData::default()
                    .val("=sum(range(0,0,0,B2),value(0,1))".into())
                    .display(None)
                    .deps(HashSet::new())
            )
            .is_some());
        assert!(sheet
            .insert(
                SLoc("", 1, 1),
                CellData::default()
                    .val("-1".into())
                    .display(None)
                    .deps(HashSet::new())
            )
            .is_none());
        sheet.recompute();
        assert_eq!(
            sheet.get(&SLoc("", 0, 0)),
            Ok(&CellData::default()
                .val("2".into())
                .display(Some(ConstType::Num(2.).into()))
                .deps(HashSet::new()))
        );
        assert_eq!(
            sheet.get(&SLoc("", 1, 1)),
            Ok(&CellData::default()
                .val("-1".into())
                .display(Some(ConstType::Num(-1.).into()))
                .deps([SLoc("", 1, 0)].into_iter().collect()))
        );
        let d = sheet.get(&SLoc("", 1, 0)).unwrap();
        assert_eq!(
            d.display,
            Some(Const {
                ct: ConstType::Error(CellError::BadArgument {
                    cell: Some(SLoc("", 1, 0)),
                    cursor: 0,
                    expected_types: vec!["Whole number".into(), ".I".into()]
                }),
                ..Const::default_err()
            })
        );
        assert!(sheet
            .insert(
                SLoc("", 1, 1),
                CellData::default()
                    .val("0.6".into())
                    .display(None)
                    .deps(HashSet::new())
            )
            .is_some());
        sheet.recompute();
        assert_eq!(
            sheet.get(&SLoc("", 0, 0)),
            Ok(&CellData::default()
                .val("2".into())
                .display(Some(ConstType::Num(2.).into()))
                .deps(HashSet::new()))
        );
        assert_eq!(
            sheet.get(&SLoc("", 1, 1)),
            Ok(&CellData::default()
                .val("0.6".into())
                .display(Some(ConstType::Num(0.6).into()))
                .deps([SLoc("", 1, 0)].into_iter().collect()))
        );
        let d = sheet.get(&SLoc("", 1, 0)).unwrap();
        assert_eq!(
            d.display,
            Some(Const {
                ct: ConstType::Error(CellError::BadArgument {
                    cell: Some(SLoc("", 1, 0)),
                    cursor: 0,
                    expected_types: vec!["Whole number".into(), ".I".into()]
                }),
                ..Const::default_err()
            })
        );

        assert!(sheet
            .insert(
                SLoc("", 1, 0),
                CellData::default()
                    .val("=sum(range(0,0,B2,10),value(0,1))".into())
                    .display(None)
                    .deps(HashSet::new())
            )
            .is_some());
        assert!(sheet
            .insert(
                SLoc("", 1, 1),
                CellData::default()
                    .val("-1".into())
                    .display(None)
                    .deps(HashSet::new())
            )
            .is_some());
        sheet.recompute();
        assert_eq!(
            sheet.get(&SLoc("", 0, 0)),
            Ok(&CellData::default()
                .val("2".into())
                .display(Some(ConstType::Num(2.).into()))
                .deps(HashSet::new()))
        );
        assert_eq!(
            sheet.get(&SLoc("", 1, 1)),
            Ok(&CellData::default()
                .val("-1".into())
                .display(Some(ConstType::Num(-1.).into()))
                .deps([SLoc("", 1, 0)].into_iter().collect()))
        );
        let d = sheet.get(&SLoc("", 1, 0)).unwrap();
        assert_eq!(
            d.display,
            Some(Const {
                ct: ConstType::Error(CellError::BadArgument {
                    cell: Some(SLoc("", 1, 0)),
                    cursor: 0,
                    expected_types: vec!["Whole number".into(), ".I".into()]
                }),
                ..Const::default_err()
            })
        );
        assert!(sheet
            .insert(
                SLoc("", 1, 1),
                CellData::default()
                    .val("0.6".into())
                    .display(None)
                    .deps(HashSet::new())
            )
            .is_some());
        sheet.recompute();
        assert_eq!(
            sheet.get(&SLoc("", 0, 0)),
            Ok(&CellData::default()
                .val("2".into())
                .display(Some(ConstType::Num(2.).into()))
                .deps(HashSet::new()))
        );
        assert_eq!(
            sheet.get(&SLoc("", 1, 1)),
            Ok(&CellData::default()
                .val("0.6".into())
                .display(Some(ConstType::Num(0.6).into()))
                .deps([SLoc("", 1, 0)].into_iter().collect()))
        );
        let d = sheet.get(&SLoc("", 1, 0)).unwrap();
        assert_eq!(
            d.display,
            Some(Const {
                ct: ConstType::Error(CellError::BadArgument {
                    cell: Some(SLoc("", 1, 0)),
                    cursor: 0,
                    expected_types: vec!["Whole number".into(), ".I".into()]
                }),
                ..Const::default_err()
            })
        );
    }
    #[test]
    fn countif() {
        let mut sheet = Sheet::new();
        assert!(sheet
            .insert(SLoc("", 0, 0), CellData::default().val("2".into()),)
            .is_none());
        assert!(sheet
            .insert(SLoc("", 0, 1), CellData::default().val("3".into()))
            .is_none());
        assert!(sheet
            .insert(SLoc("", 0, 2), CellData::default().val("1".into()))
            .is_none());

        assert!(sheet
            .insert(
                SLoc("", 1, 0),
                CellData::default()
                    .val("=countif(A1:A10, .F.<4)".into())
                    .display(None)
                    .deps(HashSet::new())
            )
            .is_none());
        sheet.recompute();
        dbg!(&sheet);
        let d = sheet.get(&SLoc("", 1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData::default()
                .val("=countif(A1:A10, .F.<4)".into())
                .display(Some(ConstType::Num(3.).into()))
                .deps(HashSet::new())
        );
        assert!(sheet
            .insert(
                SLoc("", 1, 0),
                CellData::default()
                    .val("=countif(A1:A10, .F.<2)".into())
                    .display(None)
                    .deps(HashSet::new())
            )
            .is_some());
        sheet.recompute();
        let d = sheet.get(&SLoc("", 1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData::default()
                .val("=countif(A1:A10, .F.<2)".into())
                .display(Some(ConstType::Num(1.).into()))
                .deps(HashSet::new())
        );

        assert!(sheet
            .insert(
                SLoc("", 1, 0),
                CellData::default()
                    .val("=countif(A1:B10, .F.<2)".into())
                    .display(None)
                    .deps(HashSet::new())
            )
            .is_some());
        eprintln!("Detect recursive");
        sheet.recompute();
        let d = sheet.get(&SLoc("", 1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData::default()
                .val("=countif(A1:B10, .F.<2)".into())
                .display(Some(
                    ConstType::Error(CellError::ReferenceLoop {
                        loop_point: Some(SLoc("", 1, 0))
                    })
                    .into()
                ))
                .deps([SLoc("", 1, 0)].into_iter().collect())
        );
        eprintln!("recursive done");

        *sheet.val_mut(&SLoc("", 1, 0)).unwrap() = "=countif(A1:A10, .F.=4)-7".into();
        sheet.recompute();
        let d = sheet.get(&SLoc("", 1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData::default()
                .val("=countif(A1:A10, .F.=4)-7".into())
                .display(Some(ConstType::Num(-7.).into()))
                .deps(HashSet::new())
        );
        *sheet.val_mut(&SLoc("", 1, 0)).unwrap() = "=countif(A1:A10, .F.=1)/2".into();
        sheet.recompute();
        let d = sheet.get(&SLoc("", 1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData::default()
                .val("=countif(A1:A10, .F.=1)/2".into())
                .display(Some(ConstType::Num(0.5).into()))
                .deps(HashSet::new())
        );
    }
    #[test]
    fn value() {
        let mut sheet = Sheet::new();
        assert!(sheet
            .insert(SLoc("", 0, 0), CellData::default().val("2".into()),)
            .is_none());
        assert!(sheet
            .insert(SLoc("", 0, 1), CellData::default().val("3".into()))
            .is_none());
        assert!(sheet
            .insert(SLoc("", 1, 1), CellData::default().val("4".into()))
            .is_none());
        // *sheet.val_mut(&SLoc("", 1, 0)).unwrap() = "=value(0,1)".into();
        assert!(sheet
            .insert(
                SLoc("", 1, 0),
                CellData::default().val("=value(.T,0,1)".into())
            )
            .is_none());
        sheet.recompute();
        let d = sheet.get(&SLoc("", 1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData::default()
                .val("=value(.T,0,1)".into())
                .display(Some(ConstType::Num(3.).into()))
                .deps(HashSet::new())
        );
        assert_eq!(
            sheet.get(&SLoc("", 0, 1)),
            Ok(&CellData::default()
                .val("3".into())
                .display(Some(ConstType::Num(3.).into()))
                .deps([SLoc("", 1, 0)].into_iter().collect()))
        );
        // dbg!(&sheet);
        sheet.remove(&SLoc("", 0, 1));
        sheet.recompute();
        // dbg!(&sheet);
        let d = sheet.get(&SLoc("", 1, 0)).unwrap();
        assert_eq!(
            d,
            &CellData::default()
                .val("=value(.T,0,1)".into())
                .display(Some(
                    ConstType::Error(CellError::MissingCell {
                        cell: Some(SLoc("", 0, 1))
                    })
                    .into()
                ))
                .deps(HashSet::new())
        );

        sheet.set_val(
            &SLoc("", 1, 0),
            "=value(.T,0,1)+value(.T,0,0)+value(.T,1,1)".into(),
        );
        sheet.set_val(&SLoc("", 0, 1), "5".into());
        sheet.recompute();
        // dbg!(&sheet);
        assert_eq!(
            sheet.get(&SLoc("", 1, 0)),
            Ok(&CellData::default()
                .val("=value(.T,0,1)+value(.T,0,0)+value(.T,1,1)".into())
                .display(Some(ConstType::Num(11.).into()))
                .deps(HashSet::new()))
        );
        assert_eq!(
            sheet.get(&SLoc("", 0, 0)),
            Ok(&CellData::default()
                .val("2".into())
                .deps([SLoc("", 1, 0)].into_iter().collect())
                .display(Some(ConstType::Num(2.).into())))
        );
        sheet.set_val(&SLoc("", 0, 1), "=value(.T,1,0)".into());
        eprintln!("before loop");
        sheet.recompute();

        assert_eq!(
            sheet.get(&SLoc("", 0, 1)),
            Ok(&CellData::default()
                .val("=value(.T,1,0)".into())
                .display(Some(
                    ConstType::Error(CellError::ReferenceLoop {
                        loop_point: Some(SLoc("", 0, 1))
                    })
                    .into()
                ))
                .deps([SLoc("", 1, 0)].into_iter().collect()))
        );
        assert_eq!(
            sheet.get(&SLoc("", 1, 0)),
            Ok(&CellData::default()
                .val("=value(.T,0,1)+value(.T,0,0)+value(.T,1,1)".into())
                .display(Some(
                    ConstType::Error(CellError::ReferenceLoop {
                        loop_point: Some(SLoc("", 0, 1))
                    })
                    .into()
                ))
                .deps([SLoc("", 0, 1)].into_iter().collect()))
        );
        sheet.set_val(&SLoc("", 0, 1), "=value(.T,-1,0)".into());
        sheet.recompute();
        assert_eq!(
            sheet.get_display(&SLoc("", 0, 1)),
            Some(
                ConstType::Error(CellError::BadArgument {
                    cell: Some(SLoc("", 0, 1)),
                    cursor: 0,
                    expected_types: vec!["whole number".into()]
                })
                .into()
            )
        );
        sheet.set_val(&SLoc("", 0, 1), "=value(.T,0, -1)".into());
        sheet.recompute();
        assert_eq!(
            sheet.get_display(&SLoc("", 0, 1)),
            Some(
                ConstType::Error(CellError::BadArgument {
                    cell: Some(SLoc("", 0, 1)),
                    cursor: 0,
                    expected_types: vec!["whole number".into()]
                })
                .into()
            )
        );
        sheet.set_val(&SLoc("", 0, 1), "=value(.T,0.7,0)".into());
        sheet.recompute();
        assert_eq!(
            sheet.get_display(&SLoc("", 0, 1)),
            Some(
                ConstType::Error(CellError::BadArgument {
                    cell: Some(SLoc("", 0, 1)),
                    cursor: 0,
                    expected_types: vec!["whole number".into()]
                })
                .into()
            )
        );
        sheet.set_val(&SLoc("", 0, 1), "=value(.T,0, 0.7)".into());
        sheet.recompute();
        assert_eq!(
            sheet.get_display(&SLoc("", 0, 1)),
            Some(
                ConstType::Error(CellError::BadArgument {
                    cell: Some(SLoc("", 0, 1)),
                    cursor: 0,
                    expected_types: vec!["whole number".into()]
                })
                .into()
            )
        );
    }
    #[test]
    fn recurse() {
        let mut sheet = Sheet::new();
        sheet.set_val(&SLoc("", 0, 0), "=A2+1".to_owned());
        sheet.set_val(&SLoc("", 0, 1), "=B1*2".to_owned());
        sheet.set_val(&SLoc("", 1, 0), "=A1-1".to_owned());
        sheet.recompute();
        // dbg!(&sheet);
        assert_eq!(
            sheet.get(&SLoc("", 0, 0)).unwrap().display,
            Some(
                ConstType::Error(CellError::ReferenceLoop {
                    loop_point: Some(SLoc("", 0, 0))
                })
                .into()
            )
        );
        sheet.set_val(&SLoc("", 1, 0), "=1".to_owned());
        sheet.recompute();
        dbg!(&sheet);
        assert_eq!(
            sheet.get(&SLoc("", 0, 0)).unwrap().display,
            Some(ConstType::Num(3.).into())
        );
        sheet.set_disp(
            &SLoc("", 0, 0),
            Some(ConstType::String("123".into()).into()),
        );
        assert_eq!(
            sheet.get(&SLoc("", 0, 0)).unwrap().display,
            Some(ConstType::String("123".into()).into())
        );
    }
    #[test]
    fn range_iter() {
        let range = Range(SLoc("", 0, 0), SLocBound(Bound::Inf, Bound::Fin(5)));
        let rangeiter: Result<FinRangeIter, _> = range.try_into();
        assert_eq!(rangeiter, Err("Not a finite range"));
        let range = Range(SLoc("", 0, 0), SLocBound(Bound::Fin(5), Bound::Inf));
        let rangeiter: Result<FinRangeIter, _> = range.try_into();
        assert_eq!(rangeiter, Err("Not a finite range"));
        let range = Range(SLoc("", 1, 1), SLocBound(Bound::Fin(5), Bound::Fin(6)));
        let rangeiter: FinRangeIter = range.try_into().unwrap();
        let locs: Vec<_> = rangeiter.collect();
        dbg!(&locs);
        assert_eq!(locs.len(), 30);
        assert_eq!(
            &locs[0..3],
            &[SLoc("", 1, 1), SLoc("", 2, 1), SLoc("", 3, 1)]
        );
        assert_eq!(
            &locs[27..30],
            &[SLoc("", 3, 6), SLoc("", 4, 6), SLoc("", 5, 6)]
        );
    }
    #[test]
    fn range_formula_parse() {
        assert_eq!(is_range_formula("=5", ""), Err("Range missing brackets"));
        assert_eq!(
            is_range_formula("[]=5", ""),
            Err("First argument must be a number or .I")
        );
        assert_eq!(
            is_range_formula("[0,]=5", ""),
            Err("Second argument must be a number or .I")
        );
        assert_eq!(
            is_range_formula("[0,2]=", "").unwrap(),
            RangeFormula {
                rbound: Bound::Fin(2),
                cbound: Bound::Fin(0),
                conditon: Value::Const(ConstType::Bool(true).into()),
                value: Value::Const(
                    ConstType::Error(CellError::InvalidFormula {
                        cell: None,
                        cursor: 0,
                        reason: "No formula given"
                    })
                    .into()
                ),
            }
        );
        assert_eq!(
            is_range_formula("[0,.I]=5", "").unwrap(),
            RangeFormula {
                rbound: Bound::Inf,
                cbound: Bound::Fin(0),
                conditon: Value::Const(ConstType::Bool(true).into()),
                value: Value::Const(ConstType::Num(5.).into()),
            }
        );
        assert_eq!(
            is_range_formula("[0,.I,]=5", "").unwrap(),
            RangeFormula {
                rbound: Bound::Inf,
                cbound: Bound::Fin(0),
                conditon: Value::Const(
                    ConstType::Error(CellError::InvalidFormula {
                        cell: None,
                        cursor: 6,
                        reason: "Found operand where value was expected"
                    })
                    .into()
                ),
                value: Value::Const(ConstType::Num(5.).into()),
            }
        );
        assert_eq!(
            is_range_formula("[0,.I,A1<3]=5", "").unwrap(),
            RangeFormula {
                rbound: Bound::Inf,
                cbound: Bound::Fin(0),
                conditon: Value::Lt(
                    Rc::new(Value::Ref(SLoc("", 0, 0))),
                    Rc::new(Value::Const(ConstType::Num(3.).into()))
                ),
                value: Value::Const(ConstType::Num(5.).into()),
            }
        );
        assert_eq!(
            is_range_formula("[.I,0,A1<3]=5+3", "").unwrap(),
            RangeFormula {
                rbound: Bound::Fin(0),
                cbound: Bound::Inf,
                conditon: Value::Lt(
                    Rc::new(Value::Ref(SLoc("", 0, 0))),
                    Rc::new(Value::Const(ConstType::Num(3.).into()))
                ),
                value: Value::Add(
                    Rc::new(Value::Const(ConstType::Num(5.).into())),
                    Rc::new(Value::Const(ConstType::Num(3.).into()))
                ),
            }
        );
        let rf = Value::RangeForm(Rc::new(is_range_formula("[.I,0]=1", "").unwrap()));
        assert_eq!(
            rf.is_range_formula(&SpecValues::from_sloc(SLoc("", 0, 0))),
            Some(Range(SLoc("", 0, 0), SLocBound(Bound::Inf, Bound::Fin(0))))
        );
        let rf = Value::RangeForm(Rc::new(is_range_formula("[3,0]=1", "").unwrap()));
        assert_eq!(
            rf.is_range_formula(&SpecValues::from_sloc(SLoc("", 0, 0))),
            Some(Range(
                SLoc("", 0, 0),
                SLocBound(Bound::Fin(3), Bound::Fin(0))
            ))
        );
        let rf = Value::RangeForm(Rc::new(is_range_formula("[.I,.I]=1", "").unwrap()));
        assert_eq!(
            rf.is_range_formula(&SpecValues::from_sloc(SLoc("", 0, 0))),
            Some(Range(SLoc("", 0, 0), SLocBound(Bound::Inf, Bound::Inf)))
        );
        let rf = Value::RangeForm(Rc::new(is_range_formula("[3,.I]=1", "").unwrap()));
        assert_eq!(
            rf.is_range_formula(&SpecValues::from_sloc(SLoc("", 0, 0))),
            Some(Range(SLoc("", 0, 0), SLocBound(Bound::Fin(3), Bound::Inf)))
        );
        let rf = Value::Const(ConstType::Bool(false).into());
        assert_eq!(
            rf.is_range_formula(&SpecValues::from_sloc(SLoc("", 0, 0))),
            None
        );
    }
    #[test]
    fn truthy() {
        assert!(ConstType::Num(1.0).truthy());
        assert!(!ConstType::Num(0.0).truthy());
        assert!(ConstType::String("hi".into()).truthy());
        assert!(!ConstType::String("".into()).truthy());
        assert!(ConstType::Bool(true).truthy());
        assert!(!ConstType::Bool(false).truthy());
    }
    #[test]
    fn range_formula() {
        let mut sheet = Sheet::new();

        assert!(sheet
            .insert(SLoc("", 0, 0), CellData::default().val("[0,.I]=.R".into()))
            .is_none());
        sheet.recompute_range(Range(
            SLoc("", 0, 0),
            SLocBound(Bound::Fin(0), Bound::Fin(20)),
        ));
        // TODO not have to double this
        sheet.recompute_range(Range(
            SLoc("", 0, 0),
            SLocBound(Bound::Fin(0), Bound::Fin(20)),
        ));
        dbg!(&sheet);
        // dbg!(sheet.get(&(0, 10)));
        assert_eq!(
            sheet.get_display(&SLoc("", 0, 10)),
            Some(ConstType::Num(10.).into())
        );
        assert_eq!(sheet.get_display(&SLoc("", 0, 21)), None);
        assert_eq!(sheet.get_display(&SLoc("", 1, 1)), None);
    }
}
