use clap::{ App, Arg, SubCommand };

use gen_nif::CompilationContext;
use gen_nif::target::WasmTarget;
use gen_nif::target::wasm::WasmTargetOptions;

use std::path::Path;

mod gen_init;

fn main() {
    let matches = App::new("whirl_cli")
       .version("alpha")
       .about("")
       .author("Hans Elias B. Josephsen")
       .subcommand(SubCommand::with_name("compile_core_all")
                   .arg(Arg::from_usage("<OUT_DIR> --out-dir <DIR>")
                        .required(true))
                   .arg(Arg::from_usage("<CORE_FILES> --core <CORE>")
                        .required(true)
                        .multiple(true))
                  )
       .get_matches();

    if let Some(matches) = matches.subcommand_matches("compile_core_all") {
        let in_file_names = matches.values_of("CORE_FILES").unwrap();
        let out_dir = matches.value_of("OUT_DIR").unwrap();

        let out_path = Path::new(out_dir);
        assert!(out_path.exists());
        assert!(out_path.is_dir());

        let mut modules = Vec::new();
        for in_file_name in in_file_names {
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
            modules.push(result)

        }

        for module in modules.iter() {
            let bc_file_path = out_path.join(&format!("{}.bc", module.name));

            let target_options = WasmTargetOptions {
                eir_module: module.name.as_str().to_string(),
            };
            let mut context: CompilationContext<WasmTarget> = CompilationContext::new(
                module.name.as_str(), 
                target_options
            );
            for fun in module.functions.values() {
                let ident = fun.ident();
                if ident.lambda.is_none() {
                    context.build_function(module, ident.name.as_str(), ident.arity);
                }
            }
            context.finalize();
            context.write_bitcode(&bc_file_path);
        }

        let modules_atoms: Vec<_> = modules.iter()
            .map(|m| m.name.clone())
            .collect();

        gen_init::gen_init_module(
            &modules_atoms,
            &out_path.join("whirlc_init_module.bc"));

    }

}
