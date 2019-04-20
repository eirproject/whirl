use clap::{ App, Arg, SubCommand };

use gen_nif::CompilationContext;
use gen_nif::target::WasmTarget;
use gen_nif::target::wasm::WasmTargetOptions;

fn main() {
    let matches = App::new("whirl_cli")
       .version("alpha")
       .about("")
       .author("Hans Elias B. Josephsen")
       .subcommand(SubCommand::with_name("compile_core")
                   .arg(Arg::with_name("CORE_FILE")
                        .required(true)))
       .get_matches();

    if let Some(matches) = matches.subcommand_matches("compile_core") {
        let in_file_name = matches.value_of("CORE_FILE").unwrap();
        let in_text = std::fs::read_to_string(in_file_name).unwrap();

        let join_handle: std::thread::JoinHandle<_> = std::thread::Builder::new()
            .stack_size(16 * 1024 * 1024)
            .spawn(move || {
                let res = core_erlang_compiler::parser::parse(&in_text).unwrap();
                let module = core_erlang_compiler::ir::from_parsed(&res.0);
                let cps = cps_transform::transform_module(&module);
                cps
            }).unwrap();
        let result = join_handle.join().unwrap();

        let bc_file_name = format!("{}.bc", in_file_name);
        let bc_file_path = std::path::Path::new(&bc_file_name);

        let target_options = WasmTargetOptions {
            eir_module: result.name.as_str().to_string(),
        };
        let mut context: CompilationContext<WasmTarget> = CompilationContext::new(
            result.name.as_str(), 
            target_options
            );
        context.build_function(&result, "hello_world", 1);
        context.build_function(&result, "fib", 1);
        context.build_function(&result, "fib_response", 2);
        context.finalize();
        context.write_bitcode(&bc_file_path);

    }

}
