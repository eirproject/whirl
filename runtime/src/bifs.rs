use crate::VM_INST;
use crate::{ Term, TermTag };
use crate::codegen_api::whirlrt_call_cont;
use crate::PidTarget;

use std::rc::Rc;

#[no_mangle]
pub extern "C" fn GNIF6_erlang2__g2_n_n(proc_env: *const u8, env: Term,
                                        ok_cont: Term, err_cont: Term,
                                        lhs: Term, rhs: Term) {
    let vm = unsafe { VM_INST.as_mut() }.unwrap();
    let ret = match (lhs.term_tag(), rhs.term_tag()) {
        (TermTag::SmallInt, TermTag::SmallInt) => {
            let lhs_int = lhs.get_smallint().unwrap();
            let rhs_int = rhs.get_smallint().unwrap();
            if lhs_int > rhs_int {
                vm.atoms.get_atom("true")
            } else {
                vm.atoms.get_atom("false")
            }
        }
        _ => unimplemented!(),
    };
    whirlrt_call_cont(proc_env, ok_cont, ret);
}

#[no_mangle]
pub extern "C" fn GNIF6_erlang2__m2_n_n(proc_env: *const u8, env: Term,
                                        ok_cont: Term, err_cont: Term,
                                        lhs: Term, rhs: Term) {
    // TODO handle errors properly
    let ret = match (lhs.term_tag(), rhs.term_tag()) {
        (TermTag::SmallInt, TermTag::SmallInt) => {
            let lhs_int = lhs.get_smallint().unwrap();
            let rhs_int = rhs.get_smallint().unwrap();
            Term::new_smallint(lhs_int - rhs_int)
        },
        _ => unimplemented!(),
    };
    console_log!("-ret ({:?} - {:?}) -> {:?}", lhs, rhs, ret);
    whirlrt_call_cont(proc_env, ok_cont, ret);
}

// erlang:'+'/2
#[no_mangle]
pub extern "C" fn GNIF6_erlang2__p2_n_n(proc_env: *const u8, env: Term,
                                        ok_cont: Term, err_cont: Term,
                                        lhs: Term, rhs: Term) {
    // TODO handle errors properly
    let ret = match (lhs.term_tag(), rhs.term_tag()) {
        (TermTag::SmallInt, TermTag::SmallInt) => {
            let lhs_int = lhs.get_smallint().unwrap();
            let rhs_int = rhs.get_smallint().unwrap();
            Term::new_smallint(lhs_int + rhs_int)
        },
        _ => unimplemented!(),
    };
    console_log!("+ret ({:?} + {:?}) -> {:?}", lhs, rhs, ret);
    whirlrt_call_cont(proc_env, ok_cont, ret);
}

#[no_mangle]
pub extern "C" fn GNIF6_erlang2__x2_n_n(proc_env: *const u8, env: Term,
                                        ok_cont: Term, err_cont: Term,
                                        pid: Term, term: Term) {
    // TODO: Handle errors correctly
    let vm = unsafe { VM_INST.as_mut() }.unwrap();
    match pid.pid_get() {
        Some(pid_num) => {
            match &vm.pids[pid_num] {
                PidTarget::JsProcess(weak) => {
                    if let Some(rc) = weak.upgrade() {
                        let mut refer = rc.borrow_mut();
                        // TODO: Copy terms properly
                        refer.0.messages.push_back(term);
                    }
                },
                _ => unimplemented!(),
            }
        },
        None => (),
    }
    whirlrt_call_cont(proc_env, ok_cont, term);
}








