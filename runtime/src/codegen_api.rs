use crate::{ VM_INST, CURR_PROC, PROC_CURRENT_REDUCTIONS, PROC_REDUCTION_LIMIT, BAIL_REASON };
use crate::{ Term, TermTag };
use crate::BailReason;
use crate::console_log;
use crate::FunctionIdent;

#[no_mangle]
pub extern "C" fn whirlrt_term_eq(proc_env: *const u8, lhs: Term, rhs: Term) -> bool {
    console_log!("Eq: {:?} ({:?}) {:?} ({:?})", lhs, lhs.term_tag(), rhs, rhs.term_tag());
    match (lhs.term_tag(), rhs.term_tag()) {
        (TermTag::Atom, TermTag::Atom) => lhs.0 == rhs.0,
        (TermTag::SmallInt, TermTag::SmallInt) => lhs.0 == rhs.0,
        _ => unimplemented!(),
    }
}

#[no_mangle]
pub extern "C" fn whirlrt_term_make_tuple(proc_env: *const u8, len: u32, terms: *const Term) -> Term {
    let terms_slice = unsafe { std::slice::from_raw_parts(terms, len as usize) };
    //console_log!("MakeTuple {:?}", terms_slice);

    let vm = unsafe { VM_INST.as_mut() }.unwrap();
    let pcb = vm.pcbs[unsafe { CURR_PROC }].as_mut().unwrap();

    let alloc_ptr = pcb.heap.alloc(1 + terms_slice.len());
    unsafe { *alloc_ptr = Term::new_tuple_header(terms_slice.len() as u32).0 };

    for (idx, term) in terms_slice.iter().enumerate() {
        unsafe { *alloc_ptr.offset((idx + 1) as isize) = term.0 };
    }

    Term::new_boxed(alloc_ptr)
}

#[no_mangle]
pub extern "C" fn whirlrt_call_cont(proc_env: *const u8, cont: Term, ret: Term) {
    console_log!("CallCont: {:?} ({:?}) ret: {:?} ({:?})", cont, cont.term_tag(), ret, ret.term_tag());
    assert!(cont.0 != 0);
    let fun;
    match cont.term_tag() {
        TermTag::Fun => {
            let (env_size, fun_ptr) = cont.get_fun().unwrap();
            assert!(env_size == 0);
            fun = fun_ptr;
        },
        TermTag::Boxed => {
            let term_ptr = cont.get_boxed().unwrap();
            let base_term = Term(unsafe { *term_ptr });
            assert!(base_term.term_tag() == TermTag::Fun);

            let (_env_size, fun_ptr) = base_term.get_fun().unwrap();
            fun = fun_ptr;
        },
        _ => unreachable!(),
    }
    let fun_ptr_casted = unsafe { std::mem::transmute::<_, extern "C" fn(*const u8, Term, Term)>(fun) };
    fun_ptr_casted(proc_env, cont, ret);
}

#[no_mangle]
pub extern "C" fn whirlrt_term_make_atom_from_string(len: u32, data: *const u8) -> Term {
    let name_slice = unsafe { std::slice::from_raw_parts(data, len as usize) };
    let name = std::str::from_utf8(name_slice).unwrap();
    let vm = unsafe { VM_INST.as_mut() }.unwrap();
    let term = vm.atoms.get_atom(name);
    console_log!("Register atom: {} -> {:?}", name, term);
    term
}

#[no_mangle]
pub extern "C" fn whirlrt_module_register_function(module_len: u32, module_data: *const u8, name_len: u32, name_data: *const u8, arity: u32, fun_ptr: extern "C" fn()) {
    let module_slice = unsafe { std::slice::from_raw_parts(module_data, module_len as usize) };
    let module = std::str::from_utf8(module_slice).unwrap();
    let name_slice = unsafe { std::slice::from_raw_parts(name_data, name_len as usize) };
    let name = std::str::from_utf8(name_slice).unwrap();

    let vm = unsafe { VM_INST.as_mut() }.unwrap();

    let module_atom = vm.atoms.get_atom(module);
    let name_atom = vm.atoms.get_atom(name);

    console_log!("Register function: {}:{}/{} -> {}", module, name, arity, unsafe { std::mem::transmute::<_, u32>(fun_ptr) });

    vm.funs.insert((module_atom, name_atom, arity), fun_ptr);
}

#[no_mangle]
pub extern "C" fn whirlrt_term_unpack_closure_env(proc_env: *const u8, env: Term, len: u32, out: *mut Term) {
    if len == 0 { return; }
    let inner_ptr = env.get_boxed().unwrap() as *const Term;
    let inner_head = unsafe { *inner_ptr };
    let (num_free, _fun_ptr) = inner_head.get_fun().unwrap();
    assert!(num_free == len);
    for n in 0..len {
        unsafe { *out.offset(n as isize) = *inner_ptr.offset(n as isize + 1) };
    }
}

#[no_mangle]
pub extern "C" fn whirlrt_temp_hacky_transmute_tup_to_fun_env(proc_env: *const u8, env: Term, fun_ptr: extern "C" fn()) {
    let inner_ptr = env.get_boxed().unwrap() as *const Term;
    let inner_head = unsafe { *inner_ptr };
    let tup_len = inner_head.get_tuple_header().unwrap();
    let fun_term = Term::new_fun(tup_len, fun_ptr);
    unsafe { *(inner_ptr as *mut Term) = fun_term };
}

#[no_mangle]
pub extern "C" fn whirlrt_term_make_fun(proc_env: *const u8, fun_ptr: extern "C" fn()) -> Term {
    Term::new_fun(0, fun_ptr)
}

#[no_mangle]
pub extern "C" fn whirlrt_term_get_fun(proc_env: *const u8, term: Term) -> extern "C" fn() {
    let (cnt, fun) = term.get_fun().unwrap();
    fun
}

#[no_mangle]
pub extern "C" fn whirlrt_term_make_smallint(proc_env: *const u8, int: i64) -> Term {
    Term::new_smallint(int)
}

#[no_mangle]
pub extern "C" fn whirlrt_check_reductions(proc_env: *const u8) -> bool {
    unsafe {
        PROC_CURRENT_REDUCTIONS += 1;
        PROC_CURRENT_REDUCTIONS < PROC_REDUCTION_LIMIT
    }
}

#[no_mangle]
pub extern "C" fn whirlrt_yield(proc_env: *const u8, fun: Term,
                                num_args: u32, args: *const Term) {
    let vm = unsafe { VM_INST.as_mut() }.unwrap();
    let pcb = vm.pcbs[unsafe { CURR_PROC }].as_mut().unwrap();

    pcb.cont = Some(fun);
    pcb.cont_args.clear();
    for n in 0..num_args {
        pcb.cont_args.push(unsafe { *args.offset(n as isize) })
    }

    unsafe { BAIL_REASON = BailReason::Yield };
    crate::throw_bail();
}
