use std::path::Path;

use eir::Atom;

use inkwell::module::Linkage;
use inkwell::context::Context;

pub fn gen_init_module(modules: &[Atom], path: &Path) {
    let context = Context::create();
    let module = context.create_module("init_module");
    let builder = context.create_builder();

    let void_type = context.void_type();

    let init_fun_typ = void_type.fn_type(&[], false);
    let init_fun = module.add_function(
        "whirlc_module_initall", init_fun_typ, Some(Linkage::External));

    let entry_bb = init_fun.append_basic_block("entry");
    builder.position_at_end(&entry_bb);

    for module_atom in modules {
        let fun_name = format!("whirlc_module_init_{}", module_atom);
        let fun = module.add_function(
            &fun_name, init_fun_typ,
            Some(Linkage::External));
        builder.build_call(fun, &[], "");
    }

    builder.build_return(None);

    module.write_bitcode_to_path(path);
}
