// #![feature(int_roundings)]
mod formula;

use std::{
    collections::BTreeMap,
    ops::{Add, Deref, DerefMut},
    str::FromStr,
    time::Duration,
};

use flate2::Compression;
use formula::{show_ref, CellData, CellError, Function};
use sdl2::{
    event::{Event, WindowEvent},
    keyboard::{Keycode, Mod},
    mouse::MouseButton,
    pixels::Color,
    rect::Rect,
    ttf,
};

use crate::formula::show_col;

const BLACK: Color = Color::RGB(20, 20, 20);
const BLUE: Color = Color::RGB(90, 90, 180);
const WHITE: Color = Color::RGB(200, 200, 200);

// type Sheet = BTreeMap<(i32, i32), CellData>;
// whut
pub(crate) type SLoc = (i32, i32);
#[derive(Debug)]
pub(crate) struct SheetData(BTreeMap<SLoc, CellData>);
pub(crate) type SheetEval = BTreeMap<SLoc, usize>;
pub(crate) struct SheetFunc(Vec<Box<dyn Function>>);
impl SheetFunc {
    fn iter(&self) -> impl Iterator<Item = &Box<dyn Function>> {
        self.0.iter()
    }
}

struct Sheet(
    pub(crate) SheetData,
    pub(crate) SheetEval,
    pub(crate) SheetFunc,
);
impl std::fmt::Debug for Sheet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)?;
        for v in self.2.iter() {
            write!(f, "{},", v.name())?;
        }
        Ok(())
    }
}

// impl Deref for Sheet {
//     type Target = BTreeMap<SLoc, CellData>;

//     fn deref(&self) -> &Self::Target {
//         &self.0
//     }
// }
// impl DerefMut for Sheet {
//     fn deref_mut(&mut self) -> &mut Self::Target {
//         &mut self.0
//     }
// }
impl SheetFunc {
    fn add_func<F: Function + Default + 'static>(&mut self) {
        self.0.push(Box::new(F::default()))
    }
    fn get_func(&self, name: &str) -> Option<&dyn Function> {
        self.0.iter().find(|f| f.name() == name).map(|x| x.as_ref())
    }
}
impl Sheet {
    fn new() -> Self {
        Self(
            SheetData(BTreeMap::new()),
            BTreeMap::new(),
            SheetFunc(vec![]),
        )
    }
    fn get(&self, cell: &SLoc) -> Result<&CellData, CellError> {
        self.0.get(cell, &self.1)
    }
    fn get_mut(&mut self, cell: &SLoc) -> Result<&mut CellData, CellError> {
        self.0.get_mut(cell, &self.1)
    }
    fn insert(&mut self, cell: SLoc, val: CellData) -> Option<CellData> {
        self.1.insert(cell.clone(), 0);
        self.0.insert(cell, val)
    }
}
impl Deref for Sheet {
    type Target = SheetData;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for Sheet {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl SheetData {
    fn insert(&mut self, cell: SLoc, val: CellData) -> Option<CellData> {
        self.0.insert(cell, val)
    }
    fn get(&self, cell: &SLoc, eval: &SheetEval) -> Result<&CellData, CellError> {
        if let Some(c) = self.0.get(cell) {
            if let Some(e) = eval.get(cell) {
                if e != &0 {
                    Err(CellError::ReferenceLoop {
                        loop_point: Some(*cell),
                    })
                } else {
                    Ok(c)
                }
            } else {
                panic!("No 1")
            }
        } else {
            Err(CellError::MissingCell { cell: Some(*cell) })
        }
    }
    fn get_mut(&mut self, cell: &SLoc, eval: &SheetEval) -> Result<&mut CellData, CellError> {
        // dbg!(&self.0);
        if let Some(c) = self.0.get_mut(cell) {
            if let Some(e) = eval.get(cell) {
                if e != &0 {
                    Err(CellError::ReferenceLoop {
                        loop_point: Some(*cell),
                    })
                } else {
                    Ok(c)
                }
            } else {
                panic!("No 2")
            }
        } else {
            Err(CellError::MissingCell { cell: Some(*cell) })
        }
    }
    fn remove(&mut self, cell: &SLoc) {
        self.0.remove(cell);
    }
    fn contains_key(&self, cell: &SLoc) -> bool {
        self.0.contains_key(cell)
    }
    fn keys(&self) -> impl Iterator<Item = &SLoc> {
        self.0.keys()
    }
    fn iter(&self) -> impl Iterator<Item = (&SLoc, &CellData)> {
        self.0.iter()
    }
    fn iter_mut(&mut self) -> impl Iterator<Item = (&SLoc, &mut CellData)> {
        self.0.iter_mut()
    }
    fn val_mut(&mut self, cell: &SLoc) -> Option<&mut String> {
        if let Some(c) = self.0.get_mut(cell) {
            Some(&mut c.val)
        } else {
            None
        }
    }
    fn set_val(&mut self, cell: &SLoc, val: String) {
        if let Some(c) = self.val_mut(cell) {
            *c = val;
        } else {
            // TODO some checks on being in ranges and things
            self.0.insert(*cell, CellData::default().val(val));
        }
    }
}

/*
Notes/todo:
entered variable, left/right work on cells when not entered, work on cursor when entered
    - when typing into a non-entered cell, reset contents
    - arrow keys always unenter

formulas
special symbols:
.. current cell but dont use this
.C current column as int
.R current row as int
.S. range equation starting value (not really valid for same reason as ..)
.SC range equation starting column as int
.SR range equation starting row as int
.FC lambda cell
.FR lambda row
.F. lambda value
.I infinity
.HRed, .HGreen cell highlight, .Hff0000
eg value(.C, .R)==..
[10,10]=.C+.R is a range equal, works on an 10x10 area, and starts in the cell it is entered in
empty defaults to .C for column and .R for row, so [,]= is the same as =
value(C,R) gets the value in that cell, also value((C,R))
// range(C1,R1,C2=.I,R2=.I) gets cells in rectangular area from (C1,R1) to (C2, R2)
No, i think range wont work, maybe special range const
// row(R) is reference to range(.C,R,.I,R)
// col(C) is reference to range(C,.R,C,.I)
[,.I]=value(.C,.R-1)+1 increments by 1
[,.I]=F(value(.C-1,.R)) maps column to function of cells to the left
// [,.SR+A1]=.R will fill A1 cells with their row nums
[,.I,.SR-.R<A1]=.R
A reference like C4 is always static
pos(range, val) returns (C,R) where value((C,R))==val
findif(range, expr) returns a value, findif(A1:E30, .FV>20)
can also use while logic in range
[,.I,value(.C,.R-1)<2]=value(.C,.R-1)*1.05, exponential
[] cannot self-reference, and it will not be evaluated in cells not in range(.SC, .SR), and a cell cannot be a part of two range calculations
no loops of two cells with []
// [.C==.SC] is equivalent to [,.I]
[,.I]=filter(col(.C-1), .F.<20) will fill the column will values in the first that are less than 20. use filter without range to do findif
string splitting can be done automatically as well
[5,.I]=split(value(.SC-1,.R), " "), which will put the first 6 words in the column to the left into different columns on the right

Dragging prediction
if arithmetic or geometric, continue the pattern
else, repeat until end.

numsolve
numsolve(LHS, RHS, start) will try to solve LHS=RHS, numsolve must be recursive
numsolve(..*..,2, 1) will set value to sqrt(2)

color function sets cell color
=color(value(.C-1, .R), .F.<0, .HRed, .HGreen) last one is default
=color(form, .HYellow) always makes it yellow

Cell and range insertions into a formula by click, drag -- I think done
scrolling -- Probably done
zoom
resizing rows and columns

menu
file picker (rfd)
idk what else a menu needs

header row that is sticky
*/

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Dirty {
    No,
    Visual,
    Recompute,
}
impl Add for Dirty {
    type Output = Dirty;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Dirty::Recompute, _) | (_, Dirty::Recompute) => Dirty::Recompute,
            (Dirty::Visual, _) | (_, Dirty::Visual) => Dirty::Visual,
            (Dirty::No, Dirty::No) => Dirty::No,
        }
    }
}

fn main() {
    let (window, mut events_loop, _context, keys) = {
        let sdl = sdl2::init().unwrap();
        let video = sdl.video().unwrap();
        let gl_attr = video.gl_attr();
        gl_attr.set_context_profile(sdl2::video::GLProfile::Core);
        gl_attr.set_context_version(3, 0);
        let window = video
            .window("Gogebia", 1200, 800)
            .opengl()
            .resizable()
            .build()
            .unwrap();
        let gl_context = window.gl_create_context().unwrap();
        // let gl = unsafe {
        //     glow::Context::from_loader_function(|s| video.gl_get_proc_address(s) as *const _)
        // };
        (
            window,
            sdl.event_pump().unwrap(),
            gl_context,
            sdl.keyboard(),
        )
    };

    // let cellsx: i32 = 20;
    // let cellsy: i32 = 18;
    let cellw = 120;
    let cellh: i32 = 30;
    let menu = 48;
    let top: i32 = 96;
    let border = 4;
    let mut height = 800;
    let mut width = 1200;
    let mut scroll = (0, 0);
    // let mut shifted = false;
    let sensitivity = cellh / 3;
    // let sensitivity = 1;

    let sdlttf = ttf::init().unwrap();
    let mainfont = sdlttf.load_font("UbuntuMono-Regular.ttf", 48).unwrap();
    let cellfont = sdlttf
        .load_font("UbuntuMono-Regular.ttf", cellh.try_into().unwrap())
        .unwrap();
    let mut canvas = window.into_canvas().build().unwrap();
    let texturer = canvas.texture_creator();
    // let window = canvas.into_window();

    let mut selected = None;
    let mut prev_val = None;
    let mut data = Sheet::new();
    data.2.add_func::<formula::If>();
    data.2.add_func::<formula::Sum>();
    data.2.add_func::<formula::ValueFunc>();
    data.2.add_func::<formula::CountIf>();
    data.2.add_func::<formula::RangeFunc>();
    let mut cursor = None;
    let mut form_select_start = None;
    let mut form_select_end = None;
    let mut form_select_cursor = None;

    if let Some(filen) = std::env::args().nth(1) {
        if let Ok(file_zipped) = std::fs::File::open(filen) {
            let filedata = flate2::read::GzDecoder::new(file_zipped);
            let file = csv::ReaderBuilder::new()
                .has_headers(false)
                .from_reader(filedata);

            for (i, line) in file.into_records().enumerate() {
                let Ok(line) = line else {
                    continue;
                };
                for (j, entry) in line.iter().enumerate() {
                    if !entry.is_empty() {
                        data.insert(
                            (j as i32, i as i32),
                            CellData::from_str(&entry.to_owned()).unwrap(),
                        );
                    }
                }
            }
        } else {
            println!("File could not be read");
        }
    } else {
        println!("No file given as input");
    }

    let mut framecount = 0;
    // when i do caching dont do this
    let mut start = true;

    'main: loop {
        // if let Some(Event::Quit { timestamp: _ }) = events_loop.poll_event() {
        //     break 'main;
        // }
        let mut dirty: Dirty = if start {
            Dirty::Recompute
        } else if framecount == 0 {
            Dirty::Visual
        } else {
            Dirty::No
        };
        start = false;
        while let Some(e) = events_loop.poll_event() {
            dirty = match e {
                Event::Quit { timestamp: _ } => break 'main,
                Event::MouseButtonDown {
                    timestamp: _,
                    window_id: _,
                    which: _,
                    mouse_btn: MouseButton::Left,
                    clicks: _,
                    x,
                    y,
                } => {
                    println!("{} {}", x / cellw, (y - top) / cellh);
                    let new = (
                        ((x + scroll.0) / cellw - 1),
                        ((y + scroll.1) - top) / cellh - 1,
                    );
                    if y < top {
                        // TODO menu and clicking into formula
                        Dirty::No
                    } else {
                        if let Some(cur) = cursor {
                            if let Some(s) = selected {
                                form_select_start = Some(new);

                                let show = show_ref(&new);
                                form_select_cursor = Some(cur..cur + show.len());
                                cursor = Some(cur + show.len());
                                // dbg!(&show);
                                if let Some(v) = data.val_mut(&s) {
                                    v.insert_str(cur, &show);
                                    Dirty::Visual
                                } else {
                                    Dirty::No
                                }
                            } else {
                                Dirty::No
                            }
                        } else {
                            let t = if selected == Some(new) {
                                println!("1");
                                Dirty::No
                            } else if new.0 >= 0 && new.1 >= 0 {
                                println!("2");
                                selected = Some(new);
                                Dirty::Visual
                            } else {
                                println!("3");
                                selected = None;
                                cursor = None;
                                Dirty::Visual
                            };

                            dbg!(selected);
                            t
                        }
                    }
                }
                Event::MouseMotion {
                    timestamp: _,
                    window_id: _,
                    which: _,
                    mousestate: _,
                    x,
                    y,
                    xrel: _,
                    yrel: _,
                } => {
                    let curc = ((x / cellw), (y - top) / cellh);
                    if y < top {
                        // TODO menu and clicking into formula
                        Dirty::No
                    } else {
                        if let (Some(s), Some(start), Some(form_cur)) =
                            (selected, form_select_start, form_select_cursor.clone())
                        {
                            if Some(curc) != form_select_end {
                                println!("{curc:?}");
                                form_select_end = Some(curc);

                                let shows = show_ref(&start);
                                let showe = show_ref(&curc);
                                println!("{}:{}", shows, showe);
                                if let Some(v) = data.val_mut(&s) {
                                    if curc > start {
                                        let rang = format!("{}:{}", shows, showe);
                                        form_select_cursor =
                                            Some(form_cur.start..form_cur.start + rang.len());
                                        cursor = Some(form_cur.start + rang.len());
                                        v.replace_range(form_cur, &rang);
                                        Dirty::Visual
                                    } else if curc < start {
                                        let rang = format!("{}:{}", showe, shows);
                                        form_select_cursor =
                                            Some(form_cur.start..form_cur.start + rang.len());
                                        cursor = Some(form_cur.start + rang.len());
                                        v.replace_range(form_cur, &rang);
                                        Dirty::Visual
                                    } else {
                                        Dirty::No
                                    }
                                } else {
                                    Dirty::No
                                }
                            } else {
                                Dirty::No
                            }
                        } else {
                            Dirty::No
                        }
                    }
                }
                Event::KeyDown {
                    timestamp: _,
                    window_id: _,
                    keycode,
                    scancode: _,
                    keymod: _,
                    repeat: _,
                } => {
                    // if keymod.intersects(Mod::LSHIFTMOD | Mod::RSHIFTMOD) {
                    //     shifted = true;
                    // } else {
                    //     shifted = false;
                    // }
                    if let Some(key) = keycode {
                        if let Some((x, y)) = selected {
                            if key == Keycode::Backspace {
                                if let Some(cur) = cursor {
                                    if let Ok(dmut) = data.get_mut(&(x, y)) {
                                        let dp = dmut.val.len();
                                        if cur > 0 && cur <= dp {
                                            dmut.val.remove(cur - 1);
                                            cursor = Some(cur - 1);
                                            Dirty::Visual
                                        } else {
                                            Dirty::No
                                        }
                                    } else {
                                        Dirty::No
                                    }
                                } else {
                                    let _ = data.dirty(&(x, y));
                                    data.remove(&(x, y));
                                    Dirty::Recompute
                                }
                                // dbg!(&data.get(&(x, y)).unwrap());
                            } else if key == Keycode::Return {
                                if cursor.is_some() {
                                    let _ = data.dirty(&(x, y));
                                    selected = Some((x, y + 1));
                                    cursor = None;
                                } else {
                                    cursor = Some(0);
                                }
                                Dirty::Recompute
                            } else if key == Keycode::Down {
                                selected = Some((x, y + 1));
                                let r = if cursor.is_some() {
                                    Dirty::Recompute
                                } else {
                                    Dirty::Visual
                                };
                                cursor = None;
                                if (y + 3) * cellh >= height - top + scroll.1 {
                                    scroll.1 = (y + 3 - (height - top) / cellh) * cellh;
                                }
                                let _ = data.dirty(&(x, y));
                                r
                            } else if key == Keycode::Left {
                                if let Some(cpos) = cursor {
                                    if cpos == 0 {
                                        // selected = Some(((x - 1).max(0), y));
                                        // cursor = None;
                                        // let _ = data.dirty(&(x, y));
                                        Dirty::No
                                    } else {
                                        cursor = Some(cpos - 1);
                                        Dirty::Visual
                                    }
                                } else {
                                    selected = Some(((x - 1).max(0), y));
                                    if (x - 1) * cellw < scroll.0 {
                                        scroll.0 = (x - 1).max(0) * cellw;
                                    }
                                    Dirty::Visual
                                }
                            } else if key == Keycode::Right {
                                if let Some(cpos) = cursor {
                                    if cpos
                                        == data
                                            .get(&(x, y))
                                            .map(|x| &x.val as &str)
                                            .unwrap_or("")
                                            .len()
                                    {
                                        // selected = Some((x + 1, y));
                                        // cursor = None;
                                        // let _ = data.dirty(&(x, y));
                                        Dirty::No
                                    } else {
                                        cursor = Some(cpos + 1);
                                        Dirty::Visual
                                    }
                                } else {
                                    selected = Some((x + 1, y));
                                    if (x + 3) * cellw > width + scroll.0 {
                                        scroll.0 = (x + 3 - width / cellw) * cellw;
                                    }
                                    Dirty::Visual
                                }
                            } else if key == Keycode::Up {
                                selected = Some((x, (y - 1).max(0)));
                                cursor = None;
                                if (y - 1) * cellh < scroll.1 {
                                    scroll.1 = (y - 1).max(0) * cellh;
                                }
                                Dirty::Visual
                            } else if key == Keycode::Escape {
                                if cursor.is_some() {
                                    if let Some(fc) = form_select_cursor.clone() {
                                        if let Some(v) = data.val_mut(&(x, y)) {
                                            v.replace_range(fc, "");
                                            form_select_cursor = None;
                                            form_select_start = None;
                                            form_select_end = None;
                                        }
                                    } else {
                                        cursor = None;
                                        let p = prev_val;
                                        prev_val = None;
                                        if let Some(prev) = p {
                                            data.set_val(&(x, y), prev);
                                        } else {
                                            data.set_val(&(x, y), "".into());
                                        }
                                    }
                                } else {
                                    selected = None;
                                }
                                Dirty::Visual
                            } else {
                                Dirty::No
                            }
                        } else {
                            if key == Keycode::Escape {
                                break 'main;
                            } else {
                                Dirty::No
                            }
                        }
                    } else {
                        Dirty::No
                    }
                }
                Event::TextInput {
                    timestamp: _,
                    window_id: _,
                    text,
                } => {
                    if let Some((x, y)) = selected {
                        if !data.contains_key(&(x, y)) {
                            assert!(data
                                .insert((x, y), CellData::from_str("").unwrap())
                                .is_none());
                        }
                        // data.get_mut(&(x, y)).unwrap().push_str(&text);
                        if let Some(cpos) = cursor {
                            data.val_mut(&(x, y)).unwrap().insert_str(cpos, &text);
                            cursor = Some(cpos + text.len());
                            form_select_cursor = None;
                            form_select_start = None;
                            form_select_end = None;
                            Dirty::Visual
                        } else {
                            prev_val = data.val_mut(&(x, y)).cloned();
                            cursor = Some(text.len());
                            data.set_val(&(x, y), text);
                            Dirty::Visual
                        }
                    } else {
                        Dirty::No
                    }
                }
                Event::Window {
                    timestamp: _,
                    window_id: _,
                    win_event: WindowEvent::Resized(nw, nh),
                } => {
                    width = nw;
                    height = nh;
                    Dirty::Visual
                }
                Event::MouseWheel {
                    timestamp: _,
                    window_id: _,
                    which: _,
                    mut x,
                    mut y,
                    direction: _,
                    precise_x: _,
                    precise_y: _,
                } => {
                    if keys.mod_state().intersects(Mod::LSHIFTMOD | Mod::RSHIFTMOD) {
                        (x, y) = (y, x);
                    }
                    scroll.0 = (scroll.0 + x * sensitivity).max(0);
                    scroll.1 = (scroll.1 - y * sensitivity).max(0);
                    Dirty::Visual
                }
                _ => Dirty::No,
            } + dirty;
        }
        framecount = (framecount + 1) % 60;
        // println!("{framecount}");
        if dirty != Dirty::No {
            dbg!(cursor);
            if dirty == Dirty::Recompute {
                data.recompute();
            }

            canvas.set_draw_color(WHITE);
            canvas.clear();

            let cell_scroll_x = scroll.0 / cellw;
            let cell_scroll_y = scroll.1 / cellh;

            let h34 = (height - top) as i32;
            canvas.set_draw_color(BLACK);
            canvas
                .fill_rect(Rect::new(
                    0,
                    top - border / 2,
                    width.try_into().unwrap(),
                    border.try_into().unwrap(),
                ))
                .unwrap();
            canvas
                .fill_rect(Rect::new(
                    0,
                    top + cellh - border / 2,
                    width.try_into().unwrap(),
                    border.try_into().unwrap(),
                ))
                .unwrap();
            for i in 2..=(h34 / cellh) {
                // horizontal lines
                canvas
                    .fill_rect(Rect::new(
                        0,
                        (i + cell_scroll_y) * cellh - scroll.1 + top - border / 2,
                        width.try_into().unwrap(),
                        border.try_into().unwrap(),
                    ))
                    .unwrap();
            }
            canvas
                .fill_rect(Rect::new(
                    cellw - border / 2,
                    top,
                    border.try_into().unwrap(),
                    h34.try_into().unwrap(),
                ))
                .unwrap();
            for i in 2..=(width / cellw) {
                // vertical lines
                canvas
                    .fill_rect(Rect::new(
                        (i + cell_scroll_x) * cellw - scroll.0 - border / 2,
                        top,
                        border.try_into().unwrap(),
                        h34.try_into().unwrap(),
                    ))
                    .unwrap();
            }

            for x in 0..=(width / cellw) {
                for y in 0..=(h34 / cellh) {
                    let (xs, ys) = (x + cell_scroll_x - 1, y + cell_scroll_y - 1);
                    if x == 0 && y == 0 {
                        continue;
                    } else if x == 0 {
                        let Ok(text) = cellfont.render(&show_col(ys)).solid(BLACK) else {
                            continue;
                        };
                        let text_text = texturer.create_texture_from_surface(text).unwrap();
                        let sm = text_text.query();
                        let (copyw, copyh) = { (cellw as u32, cellh as u32) };
                        canvas
                            .copy(
                                &text_text,
                                Some(Rect::new(
                                    0,
                                    0,
                                    (copyw).min(sm.width),
                                    (copyh).min(sm.height),
                                )),
                                Some(Rect::new(
                                    (x) * cellw,
                                    top + (y) * cellh - scroll.1.rem_euclid(cellh),
                                    (copyw).min(sm.width),
                                    (copyh).min(sm.height),
                                )),
                            )
                            .unwrap();
                        continue;
                    } else if y == 0 {
                        let Ok(text) = cellfont.render(&format!("{}", xs + 1)).solid(BLACK) else {
                            continue;
                        };
                        let text_text = texturer.create_texture_from_surface(text).unwrap();
                        let sm = text_text.query();
                        let (copyw, copyh) = { (cellw as u32, cellh as u32) };
                        canvas
                            .copy(
                                &text_text,
                                Some(Rect::new(
                                    0,
                                    0,
                                    (copyw).min(sm.width),
                                    (copyh).min(sm.height),
                                )),
                                Some(Rect::new(
                                    ((x) * cellw - scroll.0.rem_euclid(cellw)).max(cellw),
                                    top + (y) * cellh,
                                    (copyw).min(sm.width),
                                    (copyh).min(sm.height),
                                )),
                            )
                            .unwrap();
                        continue;
                    }
                    let Ok(s) = data.get(&(xs, ys)) else {
                        continue;
                    };
                    if s.display.is_some() && Some((xs, ys)) != selected {
                        let Ok(text) = cellfont.render(s.display.as_ref().unwrap()).solid(BLACK)
                        else {
                            // If empty string
                            continue;
                        };
                        let text_text = texturer.create_texture_from_surface(text).unwrap();
                        let sm = text_text.query();
                        let (copyw, copyh) = { (cellw as u32, cellh as u32) };
                        canvas
                            .copy(
                                &text_text,
                                Some(Rect::new(
                                    0,
                                    0,
                                    (copyw).min(sm.width).min(if x == 1 {
                                        (cellw - scroll.0.rem_euclid(cellw)) as u32
                                    } else {
                                        u32::MAX
                                    }),
                                    (copyh).min(sm.height).min(if y == 1 {
                                        (cellh - scroll.1.rem_euclid(cellh)) as u32
                                    } else {
                                        u32::MAX
                                    }),
                                )),
                                Some(Rect::new(
                                    ((x) * cellw - scroll.0.rem_euclid(cellw)).max(cellw),
                                    top + (y) * cellh
                                        - if y == 1 {
                                            0
                                        } else {
                                            scroll.1.rem_euclid(cellh)
                                        },
                                    (copyw).min(sm.width).min(if x == 1 {
                                        (cellw - scroll.0.rem_euclid(cellw)) as u32
                                    } else {
                                        u32::MAX
                                    }),
                                    (copyh).min(sm.height).min(if y == 1 {
                                        (cellh - scroll.1.rem_euclid(cellh)) as u32
                                    } else {
                                        u32::MAX
                                    }),
                                )),
                            )
                            .unwrap();
                    }
                }
            }

            if let Some((x, y)) = selected {
                if let Ok(s) = data.get(&(x, y)) {
                    match mainfont.render(&s.val).solid(BLACK) {
                        Ok(text) => {
                            let text_text = texturer.create_texture_from_surface(text).unwrap();
                            let sm = text_text.query();
                            canvas
                                .copy(
                                    &text_text,
                                    None,
                                    Some(Rect::new(
                                        0,
                                        menu,
                                        (width as u32).min(sm.width),
                                        ((top - menu) as u32).min(sm.height),
                                    )),
                                )
                                .unwrap();
                        }
                        Err(_) => {
                            // dbg!(e);
                        }
                    }
                    match cellfont
                        .render(if cursor.is_some() {
                            &s.val
                        } else {
                            s.display.as_ref().map(|x| x as &str).unwrap_or("")
                        })
                        .solid(BLACK)
                    {
                        Ok(text) => {
                            let text_text = texturer.create_texture_from_surface(text).unwrap();
                            let sm = text_text.query();
                            select_box(
                                &mut canvas,
                                x,
                                cellw,
                                border,
                                y,
                                cellh,
                                top,
                                sm.width.checked_sub(cellw as u32).map(|x| x + 4),
                                scroll,
                            );
                            canvas
                                .copy(
                                    &text_text,
                                    None,
                                    Some(Rect::new(
                                        (x + 1) * cellw - scroll.0 + border / 2,
                                        (y + 1) * cellh - scroll.1 + border / 2 + top,
                                        sm.width,
                                        cellh.try_into().unwrap(),
                                    )),
                                )
                                .unwrap();
                        }
                        Err(_) => {
                            select_box(&mut canvas, x, cellw, border, y, cellh, top, None, scroll);
                        }
                    }
                } else {
                    select_box(&mut canvas, x, cellw, border, y, cellh, top, None, scroll);
                }
            }

            if let Some(cpos) = cursor {
                let cpos = if let Some(formc) = form_select_cursor.clone() {
                    formc.end
                } else {
                    cpos
                };
                canvas.set_draw_color(BLACK);
                if let Ok(s) = data.get(&selected.unwrap()) {
                    canvas
                        .draw_rect(Rect::new(
                            mainfont
                                .size_of(&s.val[0..(cpos.min(s.val.len()))]) // TODO this shouldnt happen, fix it
                                .unwrap()
                                .0 as i32
                                - border / 2,
                            menu + border / 2,
                            border as u32,
                            (top - menu - border / 2) as u32,
                        ))
                        .unwrap();
                }
            }
            println!("Drew");

            canvas.present();
        }
        std::thread::sleep(Duration::from_millis(33));
    }
    if let Some(filen) = std::env::args().nth(1) {
        if let Ok(file_zipped) = std::fs::File::create(filen) {
            let filer = flate2::write::GzEncoder::new(file_zipped, Compression::best());
            let mut file = csv::WriterBuilder::new()
                .has_headers(false)
                .from_writer(filer);

            let (max_x, max_y) = data
                .keys()
                .fold((0, 0), |x, y| (x.0.max(y.0), x.1.max(y.1)));
            for i in 0..=max_y {
                let mut rec: Vec<&str> = vec![];
                for j in 0..=max_x {
                    if let Ok(v) = data.get(&(j, i)) {
                        rec.push(&v.val);
                    } else {
                        rec.push("");
                    }
                }
                file.write_record(rec).unwrap();
            }
        } else {
            println!("Could not write to file");
        }
    } else {
        println!("No file given as input");
    }
}

fn select_box(
    canvas: &mut sdl2::render::Canvas<sdl2::video::Window>,
    x: i32,
    cellw: i32,
    border: i32,
    y: i32,
    cellh: i32,
    top: i32,
    boxw: Option<u32>,
    scroll: (i32, i32),
) {
    let yoff = y * cellh - scroll.1 + border / 2 + top;
    if yoff > top {
        canvas.set_draw_color(BLUE);
        canvas
            .fill_rect(Rect::new(
                (x + 1) * cellw - scroll.0 + border / 2,
                yoff + cellh,
                (cellw - border) as u32 + boxw.unwrap_or(0),
                (cellh - border) as u32,
            ))
            .unwrap();
    }
}
