use stringlit::s;
use vm::*;

fn main() -> Result<(), &'static str> {
    let mut vm = VM::new();

    let state_file = std::path::Path::new("state.ron");

    if !state_file.exists() {
        vm.add_constant("a", Value::U32(5))?;

        vm.add_function(
            "main",
            Function::new(vec![
                // assign the value of the constant a to the variable b
                Instruction::VarAssign(s!("b"), Expr::Const(s!("a")).into()),
                // return the result of the binary operation Add with the values of the variable b and the constant z (argument)
                Instruction::Return(Expr::BinaryOp(
                    BinaryOp::Add,
                    Expr::Var(s!("b")).into(),
                    Expr::Arg(s!("z")).into(),
                )),
            ]),
        )?;
    } else {
        let state_text = std::fs::read_to_string(state_file).unwrap();
        vm = ron::from_str(&state_text).unwrap();
    }

    let local_context = Context::new();

    // call function with argument
    let result = vm.call_function(
        "main",
        &local_context,
        &[(s!("z"), Expr::Value(Value::I32(4)).into())].into(),
    )?;

    // print result
    println!("{:?}", result);

    let ron = ron::Options::default();

    let state = ron
        .to_string_pretty(&vm, ron::ser::PrettyConfig::new().struct_names(true))
        .unwrap();
    std::fs::write(state_file, state).unwrap();

    Ok(())
}
