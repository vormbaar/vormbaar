use std::fmt;

use crossterm::{
    event::{DisableMouseCapture, EnableMouseCapture},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use std::{io, path::PathBuf};
use tui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    text::{Span, Spans, Text},
    widgets::{Block, Borders, Paragraph},
    Frame, Terminal,
};

#[allow(unused_imports)]
use tui_textarea::{CursorMove, Input, Key, Scrolling, TextArea};

use vorm::{Expr, FunctionContext, Value, VM};

enum Mode {
    // Normal,
    Insert,
}

impl Mode {
    fn cursor_color(&self) -> Color {
        match self {
            // Self::Normal => Color::Reset,
            Self::Insert => Color::LightBlue,
        }
    }
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            // Self::Normal => write!(f, "NORMAL"),
            Self::Insert => write!(f, "INSERT"),
        }
    }
}

/// App holds the state of the application
struct App<'a> {
    /// Current input mode
    mode: Mode,
    textinput: TextArea<'a>,
    textoutput: TextArea<'a>,
    vm: VM,
}

impl<'a> Default for App<'a> {
    fn default() -> App<'a> {
        App {
            mode: Mode::Insert,
            textinput: TextArea::default(),
            textoutput: TextArea::default(),
            vm: VM::new(),
        }
    }
}

pub fn start_ui(_path: PathBuf, vm: VM) -> anyhow::Result<()> {
    // setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // create app and run it
    let app = App {
        vm,
        ..App::default()
    };
    let res = run_app(&mut terminal, app);

    // restore terminal
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    if let Err(err) = res {
        println!("{:?}", err)
    }

    Ok(())
}

fn run_app<B: Backend>(terminal: &mut Terminal<B>, mut app: App) -> anyhow::Result<()> {
    loop {
        terminal.draw(|f| ui(f, &mut app))?;

        let input = crossterm::event::read()?.into();
        match app.mode {
            // Mode::Normal => match input {
            //     // Mappings in normal mode
            //     Input {
            //         key: Key::Char('h'),
            //         ..
            //     } => app.textinput.move_cursor(CursorMove::Back),
            //     Input {
            //         key: Key::Char('j'),
            //         ..
            //     } => app.textinput.move_cursor(CursorMove::Down),
            //     Input {
            //         key: Key::Char('k'),
            //         ..
            //     } => app.textinput.move_cursor(CursorMove::Up),
            //     Input {
            //         key: Key::Char('l'),
            //         ..
            //     } => app.textinput.move_cursor(CursorMove::Forward),
            //     Input {
            //         key: Key::Char('w'),
            //         ..
            //     } => app.textinput.move_cursor(CursorMove::WordForward),
            //     Input {
            //         key: Key::Char('b'),
            //         ctrl: false,
            //         ..
            //     } => app.textinput.move_cursor(CursorMove::WordBack),
            //     Input {
            //         key: Key::Char('^'),
            //         ..
            //     } => app.textinput.move_cursor(CursorMove::Head),
            //     Input {
            //         key: Key::Char('$'),
            //         ..
            //     } => app.textinput.move_cursor(CursorMove::End),
            //     Input {
            //         key: Key::Char('D'),
            //         ..
            //     } => {
            //         app.textinput.delete_line_by_end();
            //     }
            //     Input {
            //         key: Key::Char('C'),
            //         ..
            //     } => {
            //         app.textinput.delete_line_by_end();
            //         app.mode = Mode::Insert;
            //     }
            //     Input {
            //         key: Key::Char('p'),
            //         ..
            //     } => {
            //         app.textinput.paste();
            //     }
            //     Input {
            //         key: Key::Char('u'),
            //         ctrl: false,
            //         ..
            //     } => {
            //         app.textinput.undo();
            //     }
            //     Input {
            //         key: Key::Char('r'),
            //         ctrl: true,
            //         ..
            //     } => {
            //         app.textinput.redo();
            //     }
            //     Input {
            //         key: Key::Char('x'),
            //         ..
            //     } => {
            //         app.textinput.delete_next_char();
            //     }
            //     Input {
            //         key: Key::Char('i'),
            //         ..
            //     } => app.mode = Mode::Insert,
            //     Input {
            //         key: Key::Char('a'),
            //         ..
            //     } => {
            //         app.textinput.move_cursor(CursorMove::Forward);
            //         app.mode = Mode::Insert;
            //     }
            //     Input {
            //         key: Key::Char('A'),
            //         ..
            //     } => {
            //         app.textinput.move_cursor(CursorMove::End);
            //         app.mode = Mode::Insert;
            //     }
            //     Input {
            //         key: Key::Char('o'),
            //         ..
            //     } => {
            //         app.textinput.move_cursor(CursorMove::End);
            //         app.textinput.insert_newline();
            //         app.mode = Mode::Insert;
            //     }
            //     Input {
            //         key: Key::Char('O'),
            //         ..
            //     } => {
            //         app.textinput.move_cursor(CursorMove::Head);
            //         app.textinput.insert_newline();
            //         app.textinput.move_cursor(CursorMove::Up);
            //         app.mode = Mode::Insert;
            //     }
            //     Input {
            //         key: Key::Char('I'),
            //         ..
            //     } => {
            //         app.textinput.move_cursor(CursorMove::Head);
            //         app.mode = Mode::Insert;
            //     }
            //     Input {
            //         key: Key::Char('q'),
            //         ..
            //     } => return Ok(()),
            //     Input {
            //         key: Key::Char('e'),
            //         ctrl: true,
            //         ..
            //     } => app.textinput.scroll((1, 0)),
            //     Input {
            //         key: Key::Char('y'),
            //         ctrl: true,
            //         ..
            //     } => app.textinput.scroll((-1, 0)),
            //     Input {
            //         key: Key::Char('d'),
            //         ctrl: true,
            //         ..
            //     } => app.textinput.scroll(Scrolling::HalfPageDown),
            //     Input {
            //         key: Key::Char('u'),
            //         ctrl: true,
            //         ..
            //     } => app.textinput.scroll(Scrolling::HalfPageUp),
            //     Input {
            //         key: Key::Char('f'),
            //         ctrl: true,
            //         ..
            //     } => app.textinput.scroll(Scrolling::PageDown),
            //     Input {
            //         key: Key::Char('b'),
            //         ctrl: true,
            //         ..
            //     } => app.textinput.scroll(Scrolling::PageUp),
            //     _ => {}
            // },
            Mode::Insert => match input {
                // Input { key: Key::Esc, .. }
                // | Input {
                //     key: Key::Char('c'),
                //     ctrl: true,
                //     ..
                // } => {
                //     app.mode = Mode::Normal; // Back to normal mode with Esc or Ctrl+C
                // }
                Input {
                    key: Key::Enter, ..
                } => {
                    let message: String = app.textinput.lines().join("\n");
                    match message.trim() {
                        "exit" | "quit" | ":q" | "q" => return Ok(()),
                        _ => (),
                    }
                    let mut fcall = message.split(" ");
                    let calling_context = FunctionContext::new();
                    if let Some(name) = fcall.next() {
                        match name.as_ref() {
                            ":args" => {
                                let func = if let Some(name) = fcall.next() {
                                    Some((name, app.vm.get_function(name)))
                                } else {
                                    None
                                };
                                let msg = match func {
                                    Some((name, Some(func))) => {
                                        format!("fn {}({:?})", name, func.get_arguments())
                                    }
                                    Some((name, None)) => {
                                        format!("Could not find function {}!", name)
                                    }
                                    None => format!("No function name specified!"),
                                };
                                app.textoutput.insert_str(format!("{}", msg));
                                app.textoutput.insert_newline();
                            }
                            ":code" => {
                                let func = if let Some(name) = fcall.next() {
                                    Some((name, app.vm.get_function(name)))
                                } else {
                                    None
                                };
                                let msg = match func {
                                    Some((name, Some(func))) => {
                                        format!("fn {}:\n{:?})", name, func)
                                    }
                                    Some((name, None)) => {
                                        format!("Could not find function {}!", name)
                                    }
                                    None => format!("No function name specified!"),
                                };
                                app.textoutput.insert_str(format!("{}", msg));
                                app.textoutput.insert_newline();
                            }
                            _ => {
                                let result = app.vm.call_function(
                                    name,
                                    &calling_context,
                                    &fcall
                                        .flat_map(|s| {
                                            if let Some((name, value)) = s.split_once("=") {
                                                Some((
                                                    name.to_string(),
                                                    Expr::Value(
                                                        ron::from_str::<Value>(value).unwrap(),
                                                    ),
                                                ))
                                            } else {
                                                None
                                            }
                                        })
                                        .collect(),
                                );
                                app.textoutput.insert_str(format!("{:?}", result));
                                app.textoutput.insert_newline();
                            }
                        }
                    }
                }
                input => {
                    app.textinput.input(input); // Use default key mappings in insert mode
                }
            },
        }
    }
}

fn ui<B: Backend>(f: &mut Frame<B>, app: &mut App) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .margin(2)
        .constraints(
            [
                Constraint::Length(1),
                Constraint::Length(3),
                Constraint::Min(1),
            ]
            .as_ref(),
        )
        .split(f.size());

    let (msg, style) = match app.mode {
        // Mode::Normal => (
        //     vec![
        //         Span::raw("Press "),
        //         Span::styled("q", Style::default().add_modifier(Modifier::BOLD)),
        //         Span::raw(" to exit, "),
        //         Span::styled("e", Style::default().add_modifier(Modifier::BOLD)),
        //         Span::raw(" to start editing."),
        //     ],
        //     Style::default().add_modifier(Modifier::RAPID_BLINK),
        // ),
        Mode::Insert => (
            vec![
                Span::raw("Press "),
                Span::styled("Esc", Style::default().add_modifier(Modifier::BOLD)),
                Span::raw(" to stop editing, "),
                Span::styled("Enter", Style::default().add_modifier(Modifier::BOLD)),
                Span::raw(
                    r#" to call the function. Example: "factorial n=5" or ":args factorial"."#,
                ),
            ],
            Style::default(),
        ),
    };
    let mut text = Text::from(Spans::from(msg));
    text.patch_style(style);
    let help_message = Paragraph::new(text);
    f.render_widget(help_message, chunks[0]);

    // Change the cursor color looking at current mode
    let color = app.mode.cursor_color();
    let style = Style::default().fg(color).add_modifier(Modifier::REVERSED);
    app.textinput.set_cursor_style(style);
    app.textinput
        .set_block(Block::default().borders(Borders::ALL).title("Input"));
    f.render_widget(app.textinput.widget(), chunks[1]);
    let block = Block::default().borders(Borders::ALL).title(Span::styled(
        "Messages",
        Style::default()
            .fg(Color::Magenta)
            .add_modifier(Modifier::BOLD),
    ));

    app.textoutput.set_block(block);

    f.render_widget(app.textoutput.widget(), chunks[2]);
}
