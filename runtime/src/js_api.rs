use wasm_bindgen::prelude::*;

use std::rc::{ Rc, Weak };
use std::cell::RefCell;

use super::VM_INST;
use super::ProcessHeap;
use super::Term;
use super::PCB;
use super::PidTarget;
use super::Mailbox;

#[wasm_bindgen]
pub struct JsProcess {
    pid: Term,
    mailbox: Rc<RefCell<(Mailbox, ProcessHeap)>>,
}

#[wasm_bindgen]
impl JsProcess {

    pub fn new() -> Self {
        let mailbox = Rc::new(RefCell::new((
            Mailbox::new(),
            ProcessHeap::new(),
        )));

        let vm = unsafe { VM_INST.as_mut() }.unwrap();
        let pid_num = vm.pids.len();
        vm.pids.push(PidTarget::JsProcess(Rc::downgrade(&mailbox)));

        JsProcess {
            pid: Term::new_pid(pid_num),
            mailbox: mailbox,
        }
    }

    pub fn spawn(&mut self, heap: &mut TermHeap, fun: u32, args: &[u32]) -> u32 {
        // TODO: Copy to target heap

        let vm = unsafe { VM_INST.as_mut() }.unwrap();

        let (free_args, _fun_ptr) = heap.exported[fun as usize].get_fun().unwrap();
        assert!(free_args == 0); // spawning with closures not implemented yet

        let pcb_num = vm.pcbs.len();
        let mut pcb = PCB::new();

        let cont_ok_ptr = crate::bail_ok_cont as extern "C" fn(*const u8, Term, Term);
        let bail_cont_ok_term = Term::new_fun(0, unsafe { std::mem::transmute::<_, extern "C" fn()>(cont_ok_ptr) });

        let cont_err_ptr = crate::bail_err_cont as extern "C" fn(*const u8, Term, Term);
        let bail_cont_err_term = Term::new_fun(0, unsafe { std::mem::transmute::<_, extern "C" fn()>(cont_err_ptr) });

        pcb.cont = Some(heap.exported[fun as usize]);
        pcb.cont_args.push(bail_cont_ok_term);
        pcb.cont_args.push(bail_cont_err_term);
        for arg in args.iter() {
            pcb.cont_args.push(heap.exported[*arg as usize]);
        }

        vm.pcbs.push(Some(pcb));

        let pid_num = vm.pids.len();
        vm.pids.push(PidTarget::Process(pcb_num));

        let pid_term = Term::new_pid(pid_num);

        let exp_num = heap.exported.len();
        heap.exported.push(pid_term);
        exp_num as u32
    }

    pub fn get_pid(&self, heap: &mut TermHeap) -> u32 {
        let num = heap.exported.len();
        heap.exported.push(self.pid);
        num as u32
    }

    pub fn poll_receive(&self, heap: &mut TermHeap) -> Option<u32> {
        // TODO copy to heap
        let mut refer = self.mailbox.borrow_mut();
        let term = refer.0.messages.pop_front();
        if let Some(term) = term {
            let term_num = heap.exported.len();
            heap.exported.push(term);
            Some(term_num as u32)
        } else {
            None
        }
    }

}

#[wasm_bindgen]
pub struct TermHeap {
    heap: ProcessHeap,
    exported: Vec<Term>,
}

#[wasm_bindgen]
impl TermHeap {

    pub fn alloc(size: u32) -> Self {
        TermHeap {
            heap: ProcessHeap::new(),
            exported: Vec::new(),
        }
    }

    pub fn make_atom(&mut self, name: &str) -> u32 {
        console_log!("MakeAtom {:?}", name);
        let vm = unsafe { VM_INST.as_mut() }.unwrap();
        let term = vm.atoms.get_atom(name);
        let pos = self.exported.len();
        self.exported.push(term);
        pos as u32
    }

    pub fn make_int(&mut self, num: i64) -> u32 {
        let pos = self.exported.len();
        self.exported.push(Term::new_smallint(num));
        pos as u32
    }
    pub fn get_int(&self, term: u32) -> Option<i64> {
        let term = self.exported[term as usize];
        if let Some(num) = term.get_smallint() {
            Some(num)
        } else {
            None
        }
    }

    pub fn capture_fun(&mut self, module: u32, name: u32, arity: u32) -> u32 {
        let module_term = self.exported[module as usize];
        let name_term = self.exported[name as usize];

        console_log!("CaptureFun {:?} {:?} {:?}", module_term, name_term, arity);

        let vm = unsafe { VM_INST.as_mut() }.unwrap();
        let fun = vm.funs[&(module_term, name_term, arity)];

        let pos = self.exported.len();
        self.exported.push(Term::new_fun(0, fun));
        pos as u32
    }

    pub fn clear(&mut self) {
        self.exported.clear();
        self.heap.heap_top = 0;
    }

}
