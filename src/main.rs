mod ui;

use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
};

use anyhow::{anyhow, bail};
use clap::*;
use content_inspector::inspect;
use serde_querystring::ParseMode;
use stringlit::s;
use tracing_chrome::ChromeLayerBuilder;
use tracing_subscriber::{prelude::*, registry::Registry};
use vorm::{Function, FunctionContext, FunctionTree, Scalar, Value, VM};

#[cfg(feature = "flame")]
use tracing_flame::FlameLayer;
#[cfg(feature = "flame")]
use tracing_subscriber::{fmt, prelude::*};

#[cfg(feature = "flame")]
fn setup_global_subscriber(file: &str) -> impl Drop {
    let fmt_layer = fmt::Layer::default();

    let (flame_layer, _guard) = FlameLayer::with_file(file).unwrap();

    tracing_subscriber::registry()
        .with(fmt_layer)
        .with(flame_layer)
        .init();
    _guard
}

#[derive(Parser)]
struct Options {
    #[cfg(feature = "flame")]
    #[arg(short, long)]
    flame: bool,
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    Ui {
        /// The file to load or create
        file: PathBuf,
    },
    /// Create an example ron file
    Create {
        /// The file to create
        file: PathBuf,
    },
    /// Run a ron or vmstate file
    Run {
        /// Which file to load
        file: PathBuf,
        /// Which function to use as entrypoint
        #[arg(long, default_value_t = String::from("main"))]
        entrypoint: String,
        /// The args to pass to the entrypoint. Defined as url querystring
        #[arg(value_name = "ARGS", default_value_t = String::from("init=5&n=10"))]
        args: String,
    },
    /// Show args of a function
    Args {
        /// Which file to load
        file: PathBuf,
        /// Which function to use as entrypoint
        function: String,
    },
    /// Deserialize the vm and print the whole vm on stdout
    Debug {
        /// Which file to load
        file: PathBuf,
    },
    /// Compile a ron to a vmstate file
    Compile {
        /// Which file to load
        file: PathBuf,
        /// Which file write the compiled file to (defaults to original name, but with vmstate as
        /// extension)
        out: Option<PathBuf>,
    },
    /// Prettify a ron file
    Pretty {
        /// Which file to prettify
        file: PathBuf,
    },
}

fn deserialize_vm<P: AsRef<Path>>(path: P) -> anyhow::Result<VM> {
    fn inner(path: &Path) -> anyhow::Result<VM> {
        let data = std::fs::read(path)?;
        if inspect(&data).is_text() {
            let data = String::from_utf8(data)?;
            Ok(ron::from_str::<VM>(&data)?)
        } else {
            Ok(bincode::deserialize::<VM>(&data)?)
        }
    }
    inner(path.as_ref())
}

fn create_demo_code() -> anyhow::Result<VM> {
    let mut vm = VM::new();
    use vorm::BinaryOp::*;
    use vorm::ExprTree::*;
    use vorm::RangeMode;
    use vorm::RangeTree::*;
    use vorm::Scalar::*;
    use vorm::Value::*;
    let main_tree = FunctionTree::new(vec![For(
        Range {
            start: Value(Scalar(I32(0))),
            stop: Arg(s!("n")),
            step: Value(Scalar(I32(1))),
            mode: RangeMode::Exclusive,
        }
        .into(),
        [
            VarAssign(s!("it"), Item.into()).into(),
            If(
                BinaryOp(
                    Eq,
                    Var(s!("it")).into(),
                    BinaryOp(Sub, Arg(s!("n")).into(), Value(Scalar(I32(1))).into()).into(),
                )
                .into(),
                [Return(
                    Call(
                        "factorial".into(),
                        BTreeMap::from([("n".to_string(), Arg("init".into()))]),
                    )
                    .into(),
                )]
                .into(),
                [Drop(
                    Call(
                        "factorial".into(),
                        BTreeMap::from([("n".to_string(), Arg("init".into()))]),
                    )
                    .into(),
                )
                .into()]
                .into(),
            ),
        ]
        .into(),
    )]);

    let factorial_tree = FunctionTree::new(vec![If(
        BinaryOp(
            Lt,
            Arg("n".to_string()).into(),
            Value(Scalar(I32(2))).into(),
        )
        .into(),
        [Return(Value(Scalar(I32(1))).into())].into(),
        [Return(
            BinaryOp(
                Mul,
                Arg("n".to_string()).into(),
                Call(
                    "factorial".to_string(),
                    BTreeMap::from([(
                        "n".to_string(),
                        BinaryOp(
                            Sub,
                            Arg("n".to_string()).into(),
                            Value(Scalar(I32(1))).into(),
                        ),
                    )]),
                )
                .into(),
            )
            .into(),
        )]
        .into(),
    )
    .into()]);

    let mut main_function_body = Vec::new();
    main_tree.flatten(&mut main_function_body);
    let main_function = Function::new(main_function_body);
    vm.add_function("main", main_function)?;

    let mut factorial_function_body = Vec::new();
    factorial_tree.flatten(&mut factorial_function_body);
    let factorial_function = Function::new(factorial_function_body);
    vm.add_function("factorial", factorial_function)?;

    Ok(vm)
}

fn save_vm<P: AsRef<Path>>(path: P, pretty: bool, vm: &VM) -> anyhow::Result<()> {
    let state = if pretty {
        let ron = ron::Options::default();
        ron.to_string_pretty(&vm, ron::ser::PrettyConfig::new().struct_names(false))
            .unwrap()
    } else {
        ron::to_string(&vm).unwrap()
    };
    std::fs::write(path.as_ref(), state).unwrap();
    Ok(())
}

fn main() -> anyhow::Result<()> {
    use tracing_subscriber::layer::SubscriberExt;

    tracing::subscriber::set_global_default(
        tracing_subscriber::registry().with(tracing_tracy::TracyLayer::new()),
    )
    .expect("set up the subscriber");

    let args = Options::parse();

    #[cfg(feature = "flame")]
    if args.flame {
        println!(
            "Running with tracing enabled. Generating file 'tracing.folded' for use with inferno."
        );
        setup_global_subscriber(&"tracing.folded");
    }

    match args.command {
        Command::Ui { file } => {
            let vm = if file.exists() {
                deserialize_vm(&file)?
            } else {
                create_demo_code()?
            };
            save_vm(&file, false, &vm)?;

            ui::start_ui(file, vm)?;
        }
        Command::Create { file } => {
            save_vm(&file, false, &create_demo_code()?)?;
        }
        Command::Debug { file } => {
            if !file.exists() {
                bail!("File doesn't exist!");
            }

            let vm = deserialize_vm(file)?;

            println!("{:#?}", vm);
        }
        Command::Args { file, function } => {
            if !file.exists() {
                bail!("File doesn't exist!");
            }

            let vm = deserialize_vm(file)?;
            if let Some(args) = vm.get_function(&function).map(Function::get_arguments) {
                println!("Found args: {:?}", args);
            } else {
                println!("No arguments found!");
            }
        }
        Command::Run {
            file,
            entrypoint,
            args,
        } => {
            if !file.exists() {
                bail!("File doesn't exist!");
            }

            let vm = deserialize_vm(file)?;

            let local_context = FunctionContext::new();

            let args =
                serde_querystring::from_str::<BTreeMap<String, i32>>(&args, ParseMode::Brackets)?;

            let args = &args
                .iter()
                .map(|(name, &value)| (name, Value::Scalar(Scalar::I32(value))))
                .collect::<BTreeMap<_, _>>();

            // call function with argument
            let result = vm
                .call_function(
                    &entrypoint,
                    &local_context,
                    &None,
                    Some(
                        &args
                            .iter()
                            .map(|(name, value)| (name.to_owned().to_owned(), value.clone()))
                            .collect(),
                    ),
                )
                .map_err(|err| anyhow!("{:?}", err))?;

            // print result
            println!("{:?}", result.into_inner());
        }
        Command::Compile { file, out } => {
            if !file.exists() {
                bail!("File doesn't exist!");
            }
            let state_text = std::fs::read_to_string(&file).unwrap();
            let vm: VM = ron::from_str(&state_text).unwrap();

            let state = bincode::serialize(&vm).unwrap();
            std::fs::write(out.unwrap_or_else(|| file.with_extension("vmstate")), state).unwrap();
        }
        Command::Pretty { file } => {
            if !file.exists() {
                bail!("File doesn't exist!");
            }
            let state_text = std::fs::read_to_string(&file).unwrap();
            let vm: VM = ron::from_str(&state_text).unwrap();
            save_vm(&file, true, &vm)?;
        }
    }

    Ok(())
}
