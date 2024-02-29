use std::{collections::BTreeMap, time::Duration};

use flate2::Compression;
use sdl2::{
    event::{Event, WindowEvent},
    keyboard::Keycode,
    mouse::MouseButton,
    pixels::Color,
    rect::Rect,
    ttf,
};

const BLACK: Color = Color::RGB(20, 20, 20);
const BLUE: Color = Color::RGB(90, 90, 180);
const WHITE: Color = Color::RGB(200, 200, 200);

/*
Notes/todo:
entered variable, left/right work on cells when not entered, work on cursor when entered
    - when typing into a non-entered cell, reset contents
    - arrow keys always unenter

formulas
special symbols:
.. current cell
.C current column as int
.R current row as int
.SC range equation starting column as int
.SR range equation starting row as int
.FC lambda cell
.FR lambda row
.F. lambda value
.I infinity
eg value(.C, .R)==..
[.C+10,.R+10]=.C+.R is a range equal, works on an 11x11 area, and starts in the cell it is entered in
empty defaults to .C for column and .R for row, so [,]= is the same as =

value(C,R) gets the value in that cell, also value((C,R))
range(C1,R1,C2=.I,R2=.I) gets cells in rectangular area from (C1,R1) to (C2, R2)
row(R) is reference to range(.C,R,.I,R)
col(C) is reference to range(C,.R,C,.I)
[,.I]=value(.C,.R-1)+1 increments by 1
[,.I]=F(value(.C-1,.R)) maps column to function of cells to the left
A reference like C4 is always static
pos(range, val) returns (C,R) where value((C,R))==val
// findif(range, expr) returns a value, findif(range(1,1), .FV>20)
can also use while logic in range
[cell(.C,.R-1)<2]=cell(.C,.R-1)*1.05, exponential
[] cannot self-reference, and it will not be evaluated in cells not in range(.SC, .SR), and a cell cannot be a part of two range calculations
no loops of two cells with []
[.C==.SC] is equivalent to [,.I]
[,.I]=filter(col(.C-1), .F.<20) will fill the column will values in the first that are less than 20. use filter without range to do findif
string splitting can be done automatically as well
[.C+5,.I]=split(cell(.SC-1,.R), " "), which will put the first 6 words in the column to the left into different columns on the right

Dragging prediction
if arithmetic or geometric, continue the pattern
else, repeat until end.

numsolve
numsolve(LHS, RHS, start) will try to solve LHS=RHS, numsolve must be recursive
numsolve(..*..,2, 1) will set value to sqrt(2)
*/

fn main() {
    let (window, mut events_loop, _context) = {
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
        let event_loop = sdl.event_pump().unwrap();
        (window, event_loop, gl_context)
    };

    // let cellsx: i32 = 20;
    // let cellsy: i32 = 18;
    let cellw = 60;
    let cellh: i32 = 30;
    let menu = 48;
    let top: i32 = 96;
    let border = 4;
    let mut height = 800;
    let mut width = 1200;

    let sdlttf = ttf::init().unwrap();
    let mainfont = sdlttf.load_font("UbuntuMono-Regular.ttf", 48).unwrap();
    let cellfont = sdlttf
        .load_font("UbuntuMono-Regular.ttf", cellh.try_into().unwrap())
        .unwrap();
    let mut canvas = window.into_canvas().build().unwrap();
    let texturer = canvas.texture_creator();
    // let window = canvas.into_window();

    let mut selected = None;
    let mut data: BTreeMap<(i32, i32), String> = BTreeMap::new();
    let mut cursor = None;

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
                    data.insert((j as i32, i as i32), entry.to_owned());
                }
            }
        } else {
            println!("File could not be read");
        }
    } else {
        println!("No file given as input");
    }

    let mut framecount = 0;

    'main: loop {
        // if let Some(Event::Quit { timestamp: _ }) = events_loop.poll_event() {
        //     break 'main;
        // }
        let mut dirty = framecount == 0;
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
                    let new = ((x / cellw), (y - top) / cellh);
                    let t = if y < top || selected == Some(new) {
                        selected = None;
                        cursor = None;
                        true
                    } else if new.0 >= 0 && new.1 >= 0 {
                        selected = Some(new);
                        cursor = Some(0);
                        true
                    } else {
                        selected = None;
                        cursor = None;
                        true
                    };
                    dbg!(selected);
                    t
                }
                Event::KeyDown {
                    timestamp: _,
                    window_id: _,
                    keycode,
                    scancode: _,
                    keymod: _,
                    repeat: _,
                } => {
                    if let Some(key) = keycode {
                        if let Some((x, y)) = selected {
                            if key == Keycode::Backspace {
                                if !data.contains_key(&(x, y)) {
                                    assert!(data.insert((x, y), "".into()).is_none());
                                }
                                let dp = data.get(&(x, y)).unwrap().len().checked_sub(1);
                                if let Some(dp) = dp {
                                    data.get_mut(&(x, y)).unwrap().remove(dp);
                                }
                                dbg!(&data.get(&(x, y)).unwrap());
                                true
                            } else if key == Keycode::Return || key == Keycode::Down {
                                selected = Some((x, y + 1));
                                true
                            } else if key == Keycode::Left {
                                if let Some(cpos) = cursor {
                                    if cpos == 0 {
                                        selected = Some(((x - 1).max(0), y));
                                        cursor = Some(0);
                                    } else {
                                        cursor = Some(cpos - 1);
                                    }
                                } else {
                                    println!("Weird error, selected but not cursor");
                                }
                                true
                            } else if key == Keycode::Right {
                                if let Some(cpos) = cursor {
                                    if cpos
                                        == data.get(&(x, y)).map(|x| x as &str).unwrap_or("").len()
                                    {
                                        selected = Some((x + 1, y));
                                        cursor = Some(0);
                                    } else {
                                        cursor = Some(cpos + 1);
                                    }
                                } else {
                                    println!("Weird error, selected but not cursor");
                                }
                                true
                            } else if key == Keycode::Up {
                                selected = Some((x, (y - 1).max(0)));
                                true
                            } else if key == Keycode::Escape {
                                selected = None;
                                true
                            } else {
                                false
                            }
                        } else {
                            if key == Keycode::Escape {
                                break 'main;
                            } else {
                                false
                            }
                        }
                    } else {
                        false
                    }
                }
                Event::TextInput {
                    timestamp: _,
                    window_id: _,
                    text,
                } => {
                    if let Some((x, y)) = selected {
                        if !data.contains_key(&(x, y)) {
                            assert!(data.insert((x, y), "".into()).is_none());
                        }
                        // data.get_mut(&(x, y)).unwrap().push_str(&text);
                        if let Some(cpos) = cursor {
                            data.get_mut(&(x, y)).unwrap().insert_str(cpos, &text);
                            cursor = Some(cpos + text.len());
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }
                Event::Window {
                    timestamp: _,
                    window_id: _,
                    win_event: WindowEvent::Resized(nw, nh),
                } => {
                    width = nw;
                    height = nh;
                    true
                }
                _ => false,
            } || dirty;
        }
        framecount = (framecount + 1) % 60;
        // println!("{framecount}");
        if dirty {
            canvas.set_draw_color(WHITE);
            canvas.clear();

            let h34 = (height - top) as i32;
            canvas.set_draw_color(BLACK);
            for i in 0..=(h34 / cellh) {
                // horizontal lines
                canvas
                    .fill_rect(Rect::new(
                        0,
                        i * cellh + top - border / 2,
                        width.try_into().unwrap(),
                        border.try_into().unwrap(),
                    ))
                    .unwrap();
            }
            for i in 0..=(width / cellw) {
                // vertical lines
                canvas
                    .fill_rect(Rect::new(
                        i * cellw - border / 2,
                        top,
                        border.try_into().unwrap(),
                        h34.try_into().unwrap(),
                    ))
                    .unwrap();
            }

            for x in 0..=(width / cellw) {
                for y in 0..=(h34 / cellh) {
                    let Some(s) = data.get(&(x, y)) else {
                        continue;
                    };
                    if !s.is_empty() && Some((x, y)) != selected {
                        let text = cellfont.render(s).solid(BLACK).unwrap();
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
                                    x * cellw,
                                    top + y * cellh,
                                    (copyw).min(sm.width),
                                    (copyh).min(sm.height),
                                )),
                            )
                            .unwrap();
                    }
                }
            }

            if let Some((x, y)) = selected {
                if let Some(s) = data.get(&(x, y)) {
                    match mainfont.render(s).solid(BLACK) {
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
                    match cellfont.render(s).solid(BLACK) {
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
                            );
                            canvas
                                .copy(
                                    &text_text,
                                    None,
                                    Some(Rect::new(
                                        x * cellw + border / 2,
                                        y * cellh + border / 2 + top,
                                        sm.width,
                                        cellh.try_into().unwrap(),
                                    )),
                                )
                                .unwrap();
                        }
                        Err(_) => {
                            select_box(&mut canvas, x, cellw, border, y, cellh, top, None);
                        }
                    }
                } else {
                    select_box(&mut canvas, x, cellw, border, y, cellh, top, None);
                }
            }

            if let Some(cpos) = cursor {
                canvas.set_draw_color(BLACK);
                if let Some(s) = data.get(&selected.unwrap()) {
                    canvas
                        .draw_rect(Rect::new(
                            mainfont.size_of(&s[0..cpos]).unwrap().0 as i32 - border / 2,
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
                    if let Some(v) = data.get(&(j, i)) {
                        rec.push(v);
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
) {
    canvas.set_draw_color(BLUE);
    canvas
        .fill_rect(Rect::new(
            x * cellw + border / 2,
            y * cellh + border / 2 + top,
            (cellw - border) as u32 + boxw.unwrap_or(0),
            (cellh - border) as u32,
        ))
        .unwrap();
}
