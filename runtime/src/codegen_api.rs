use crate::{ VM, Term, TermTag, ProcessState };
use crate::BailReason;
use crate::console_log;
use crate::FunctionIdent;

#[no_mangle]
pub extern "C" fn whirlrt_term_eq(proc_env: *const u8, lhs: Term, rhs: Term) -> bool {
    let lhs_d = unsafe { lhs.deref_boxes() };
    let rhs_d = unsafe { rhs.deref_boxes() };
    let lhs = unsafe { *lhs_d };
    let rhs = unsafe { *rhs_d };
    console_log!("Eq: {:?} ({:?}) {:?} ({:?})", lhs, lhs.term_tag(), rhs, rhs.term_tag());
    match (lhs.term_tag(), rhs.term_tag()) {
        (TermTag::Atom, TermTag::Atom) => lhs.0 == rhs.0,
        (TermTag::SmallInt, TermTag::SmallInt) => lhs.0 == rhs.0,
        (a, b) if (a == b) => unimplemented!(),
        _ => false,
    }
}

#[no_mangle]
pub extern "C" fn whirlrt_term_make_tuple(proc_env: *const u8, len: u32, terms: *const Term) -> Term {
    let terms_slice = unsafe { std::slice::from_raw_parts(terms, len as usize) };

    let vm = VM::get_instance();
    let pcb = vm.pcbs[vm.proc_curr.unwrap()].as_mut().unwrap();

    console_log!("MakeTuple {:?}", terms_slice);
    let tup = pcb.heap.tuple(terms_slice.len() as u32);
    for term in terms_slice {
        console_log!("MakeTuple elem {:?} ({:?})", term, term.term_tag());
        pcb.heap.atomic(*term);
    }

    tup
}

#[no_mangle]
pub extern "C" fn whirlrt_call_cont(proc_env: *const u8, cont: Term, ret: Term) {
    let cont_unboxed = unsafe { cont.deref_boxes() };
    let cont_unboxed_h = unsafe { *cont_unboxed };
    let cc_len = cont_unboxed_h.get_fun().unwrap().0;
    console_log!("CallCont: {:?} ({:?}) ret: {:?} ({:?})", cont_unboxed_h, cont_unboxed_h.term_tag(), ret, ret.term_tag());
    for n in 0..cc_len {
        let term = unsafe { *cont_unboxed.offset(n as isize + 1) };
        console_log!("ENVARG: {:?} ({:?}) {:?}", term, term.term_tag(), term.get_fun());
    }

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
    let vm = VM::get_instance();
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

    let vm = VM::get_instance();

    let module_atom = vm.atoms.get_atom(module);
    let name_atom = vm.atoms.get_atom(name);

    //console_log!("Register function: {}:{}/{} -> {}", module, name, arity, unsafe { std::mem::transmute::<_, u32>(fun_ptr) });

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
    let vm = VM::get_instance();
    vm.proc_reds += 1;
    vm.proc_reds < vm.proc_reds_limit
}

#[no_mangle]
pub extern "C" fn whirlrt_yield(proc_env: *const u8, fun: Term,
                                num_args: u32, args: *const Term) {
    let vm = VM::get_instance();
    let pcb = vm.pcbs[vm.proc_curr.unwrap()].as_mut().unwrap();

    pcb.cont = Some(fun);
    pcb.cont_args.clear();
    for n in 0..num_args {
        pcb.cont_args.push(unsafe { *args.offset(n as isize) })
    }

    vm.proc_bail_reason = BailReason::Yield;
    crate::throw_bail();
}

#[no_mangle]
pub extern "C" fn whirlrt_unreachable_fail(proc_env: *const u8) {
    unreachable!()
}

#[no_mangle]
pub extern "C" fn whirlrt_term_unpack_tuple(
    proc_env: *const u8, tuple_term: Term, len: u32, out_buf: *mut Term) -> bool
{
    let derefed = unsafe { tuple_term.deref_boxes() };
    let inner_term = unsafe { *derefed };
    if let Some(arity) = inner_term.get_tuple_header() {
        if arity == len {
            for n in 0..len {
                let ptr = unsafe { derefed.offset(n as isize + 1) };
                let out_ptr = unsafe { out_buf.offset(n as isize) };
                console_log!("TupleElem: {} {:?}", n, unsafe {*ptr}.term_tag());
                unsafe { *out_ptr = *ptr };
            }
            return true;
        }
    }
    false
}

#[no_mangle]
pub extern "C" fn GNIF15_eir__intrinsics14_receive__start1_n_n(
    proc_env: *const u8, _env: Term,
    ok_cont: Term, _err_cont: Term,
    timeout: Term)
{
    let vm = VM::get_instance();
    if timeout != vm.atoms.get_atom("infinity") {
        unimplemented!("receive with timeout not yet implemented");
    }
    let pcb = vm.pcbs[vm.proc_curr.unwrap()].as_mut().unwrap();
    pcb.mailbox.start_receive();
    whirlrt_call_cont(proc_env, ok_cont, vm.atoms.get_atom("nil"));
    unreachable!();
}
#[no_mangle]
pub extern "C" fn GNIF15_eir__intrinsics13_receive__wait1_n_n(
    _proc_env: *const u8, _env: Term,
    ok_cont: Term, err_cont: Term,
    _structure: Term)
{
    let vm = VM::get_instance();
    let pcb = vm.pcbs[vm.proc_curr.unwrap()].as_mut().unwrap();
    pcb.cont = Some(ok_cont);
    pcb.err_cont = Some(err_cont);
    pcb.cont_args.clear();
    pcb.state = ProcessState::Receive;
    vm.proc_bail_reason = BailReason::Yield;
    crate::throw_bail();
    unreachable!();
}
#[no_mangle]
pub extern "C" fn GNIF15_eir__intrinsics15_receive__finish1_n_n(
    proc_env: *const u8, env: Term,
    ok_cont: Term, err_cont: Term,
    _structure: Term)
{
    let vm = VM::get_instance();
    let pcb = vm.pcbs[vm.proc_curr.unwrap()].as_mut().unwrap();
    pcb.mailbox.finish_receive();
    whirlrt_call_cont(proc_env, ok_cont, vm.atoms.get_atom("nil"));
    unreachable!();
}

pub extern "C" fn bail_ok_cont(proc_env: *const u8, env: Term, ret: Term) {
    {
        let vm = VM::get_instance();
        vm.proc_bail_reason = BailReason::ReturnOk(ret);
    }
    crate::throw_bail();
}
pub extern "C" fn bail_err_cont(proc_env: *const u8, env: Term, ret: Term) {
    {
        let vm = VM::get_instance();
        vm.proc_bail_reason = BailReason::ReturnErr(ret);
    }
    crate::throw_bail();
}
