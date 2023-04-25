use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
};

use anyhow::{anyhow, bail};
use clap::*;
use content_inspector::inspect;
use serde_querystring::ParseMode;
use vm::*;

#[derive(Parser)]
struct Options {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
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
        #[arg(value_name = "ARGS", default_value_t = String::from("init=5"))]
        args: String,
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

fn main() -> anyhow::Result<()> {
    let args = Options::parse();

    let ron = ron::Options::default();
    match args.command {
        Command::Create { file } => {
            let mut vm = VM::new();
            use crate::BinaryOp::*;
            use crate::Value::*;
            use Expr::*;
            use Instruction::*;
            vm.add_function(
                "main",
                Function::new(vec![Return(Call(
                    "factorial".into(),
                    BTreeMap::from([("n".to_string(), Arg("init".into()))]),
                ))]),
            )
            .map_err(|err| anyhow!("{:?}", err))?;

            vm.add_function(
                "factorial",
                Function::new(vec![Return(
                    Expr::If(
                        BinaryOp(Lt, Arg("n".to_string()).into(), Value(I32(2)).into()).into(),
                        Value(I32(1)).into(),
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
                                        Value(I32(1)).into(),
                                    ),
                                )]),
                            )
                            .into(),
                        )
                        .into(),
                    )
                    .into(),
                )]),
            )
            .map_err(|err| anyhow!("{:?}", err))?;

            let state = ron
                .to_string_pretty(&vm, ron::ser::PrettyConfig::new().struct_names(false))
                .unwrap();
            std::fs::write(file, state).unwrap();
        }
        Command::Debug { file } => {
            if !file.exists() {
                bail!("File doesn't exist!");
            }

            let vm = deserialize_vm(file)?;

            println!("{:#?}", vm);
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
                serde_querystring::from_str::<BTreeMap<String, i32>>(&args, ParseMode::UrlEncoded)?;

            // call function with argument
            let result = vm
                .call_function(
                    &entrypoint,
                    &local_context,
                    &args
                        .iter()
                        .map(|(name, &value)| (name.to_owned(), Expr::Value(Value::I32(value))))
                        .collect(),
                )
                .map_err(|err| anyhow!("{:?}", err))?;

            // print result
            println!("{:?}", result);
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

            let state = ron
                .to_string_pretty(&vm, ron::ser::PrettyConfig::new().struct_names(false))
                .unwrap();
            std::fs::write(file, state).unwrap();
        }
    }

    Ok(())
}
