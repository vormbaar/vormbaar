use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use std::{io, path::PathBuf};
use tui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    text::{Span, Spans, Text},
    widgets::{Block, Borders, List, ListItem, Paragraph, Wrap},
    Frame, Terminal,
};
use unicode_width::UnicodeWidthStr;

use vorm::{Expr, FunctionContext, Value, VM};

enum InputMode {
    Normal,
    Editing,
}

/// App holds the state of the application
struct App {
    /// Current value of the input box
    input: String,
    /// Current input mode
    input_mode: InputMode,
    /// History of recorded messages
    messages: Vec<String>,
    vm: VM,
}

impl Default for App {
    fn default() -> App {
        App {
            input: String::from("factorial n=5"),
            input_mode: InputMode::Editing,
            messages: Vec::new(),
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
        terminal.draw(|f| ui(f, &app))?;

        if let Event::Key(key) = event::read()? {
            match app.input_mode {
                InputMode::Normal => match key.code {
                    KeyCode::Char('e') => {
                        app.input_mode = InputMode::Editing;
                    }
                    KeyCode::Char('q') => {
                        return Ok(());
                    }
                    _ => {}
                },
                InputMode::Editing => match key.code {
                    KeyCode::Enter => {
                        let message: String = app.input.drain(..).collect();
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
                                    app.messages.push(format!("{}", msg));
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
                                    app.messages.push(format!("{}", msg));
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
                                    app.messages.push(format!("{:?}", result));
                                }
                            }
                        }
                    }
                    KeyCode::Char(c) => {
                        app.input.push(c);
                    }
                    KeyCode::Backspace => {
                        app.input.pop();
                    }
                    KeyCode::Esc => {
                        app.input_mode = InputMode::Normal;
                    }
                    _ => {}
                },
            }
        }
    }
}

fn ui<B: Backend>(f: &mut Frame<B>, app: &App) {
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

    let (msg, style) = match app.input_mode {
        InputMode::Normal => (
            vec![
                Span::raw("Press "),
                Span::styled("q", Style::default().add_modifier(Modifier::BOLD)),
                Span::raw(" to exit, "),
                Span::styled("e", Style::default().add_modifier(Modifier::BOLD)),
                Span::raw(" to start editing."),
            ],
            Style::default().add_modifier(Modifier::RAPID_BLINK),
        ),
        InputMode::Editing => (
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

    let input = Paragraph::new(app.input.as_ref())
        .style(match app.input_mode {
            InputMode::Normal => Style::default(),
            InputMode::Editing => Style::default().fg(Color::Yellow),
        })
        .block(Block::default().borders(Borders::ALL).title("Input"));
    f.render_widget(input, chunks[1]);
    match app.input_mode {
        InputMode::Normal =>
            // Hide the cursor. `Frame` does this by default, so we don't need to do anything here
            {}

        InputMode::Editing => {
            // Make the cursor visible and ask tui-rs to put it at the specified coordinates after rendering
            f.set_cursor(
                // Put cursor past the end of the input text
                chunks[1].x + app.input.width() as u16 + 1,
                // Move one line down, from the border to the input line
                chunks[1].y + 1,
            )
        }
    }

    let mut text = Text::from(Spans::from(app.messages.join("\n")));
    text.patch_style(style);
    let result_message = Paragraph::new(text).wrap(Wrap { trim: true });
    f.render_widget(result_message, chunks[2]);
}
