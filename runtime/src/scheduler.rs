use crate::VM;
use crate::PidTarget;
use crate::ProcessState;
use crate::BailReason;
use crate::Term;

pub fn run_reductions(num: usize) {
    let vm = VM::get_instance();

    let mut total_reds = 0;
    while total_reds < num {

        // 1 reduction overhead per reschedule to prevent infinite loop
        total_reds += 1;

        let pid = &vm.pids[vm.sched_curr_proc_num];
        match pid {
            PidTarget::JsProcess(_) => (),
            PidTarget::Process(pcb_num) => total_reds += schedule_process(vm, *pcb_num),
        }

        vm.sched_curr_proc_num += 1;
        if vm.sched_curr_proc_num >= vm.pids.len() { vm.sched_curr_proc_num = 0; }

    }

}

pub fn schedule_process(vm: &mut VM, pcb_num: usize) -> usize {
    //console_log!("PCB: {}", pcb_num);

    let pcb = vm.pcbs[pcb_num].as_mut().unwrap();
    if let Some(mut cont_fun) = pcb.cont {

        // Set up process execution state
        vm.proc_curr = Some(pcb_num);
        vm.proc_bail_reason = BailReason::None;

        match pcb.state {
            ProcessState::Run => {
                let (_cnt, fun_ptr) = cont_fun.get_fun().unwrap();

                let mut args = vec![cont_fun];
                args.extend(pcb.cont_args.iter().cloned());

                let launchpad = crate::fun_n_launchpad::FUN_LAUNCHPADS[
                    args.len()];

                let ret = crate::wrap_catch_bail(&mut || {
                    let dummy: u8 = 0;
                    launchpad(&dummy, fun_ptr, &args);
                    unreachable!();
                });
                assert!(!ret);
            }
            ProcessState::Receive => {
                assert!(pcb.cont_args.len() == 0);
                //console_log!("{:?}", pcb.mailbox.messages);
                if let Some(term) = pcb.mailbox.peek_receive() {

                    // TODO abstract gc process
                    let copy = VM::get_copy();
                    let size = copy.required_size(term);
                    if pcb.heap.capacity() < size {
                        let gc = VM::get_gc();
                        gc.init_for_heap(&pcb.heap);
                        gc.seed(cont_fun);
                        gc.do_gc(&mut pcb.heap, size);
                        cont_fun = gc.map(cont_fun);
                    }
                    let copied = copy.copy(&mut pcb.heap, term);

                    let derefed = unsafe { cont_fun.deref_boxes() };
                    let (_cnt, fun_ptr) = unsafe { *derefed }.get_fun().unwrap();

                    let args = vec![cont_fun, copied];
                    let launchpad = crate::fun_n_launchpad::FUN_LAUNCHPADS[
                        args.len()];

                    let ret = crate::wrap_catch_bail(&mut || {
                        let dummy: u8 = 0;
                        launchpad(&dummy, fun_ptr, &args);
                        unreachable!();
                    });
                    assert!(!ret);

                } else {
                    vm.proc_bail_reason = BailReason::Yield;
                }
            }
        }

        // Finalize process execution
        vm.proc_curr = None;
        let total_reds = vm.proc_reds;
        vm.proc_reds = 0;

        match vm.proc_bail_reason {
            BailReason::None => unreachable!(),
            BailReason::ReturnOk(_) => {
                console_log!("ReturnOk");
                pcb.cont = None;
            }
            BailReason::ReturnErr(_) => {
                console_log!("ReturnOk");
                pcb.cont = None;
            }
            BailReason::Yield => {
                //console_log!("yield!");
            }
        }

        total_reds
    } else {
        0
    }
}
