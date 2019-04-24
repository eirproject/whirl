import { init, run_some, JsTermHeap, JsProcess } from "../out/linked.js";

console.log("BeforeInit");
init();
console.log("AfterInit");

// Allocate a term heap and make a process
let heap = JsTermHeap.alloc(1000);
let proc = JsProcess.new();

/*
// Spawn a process
let testingAtom = heap.make_atom("testing");
//let fibAtom = heap.make_atom("fib_response");
let fibAtom = heap.make_atom("fib_server");
let funTerm = heap.capture_fun(testingAtom, fibAtom, 0);
let js_pid = process.get_pid(heap);
//let pid = process.spawn(heap, funTerm, [js_pid, fiveTerm]);
let pid = process.spawn(heap, funTerm, []);

let fiveTerm = heap.make_int(BigInt(5));
let sevenTerm = heap.make_int(BigInt(7));

// Run reductions
// Nothing should really happen since we wait for a message
run_some();

let msg_tup1 = heap.make_tuple([js_pid, fiveTerm]);
process.send(heap, pid, msg_tup1);

run_some();

let msg_tup2 = heap.make_tuple([js_pid, sevenTerm]);
process.send(heap, pid, msg_tup2);

run_some();

// Expect a message in return with the result
let ret1 = process.poll_receive(heap);
let num1 = heap.get_int(ret1);
console.log("Result1: ");
console.log(num1);

let ret2 = process.poll_receive(heap);
let num2 = heap.get_int(ret2);
console.log("Result2: ");
console.log(num2);
*/

let displ = document.getElementById("display");
function updateNumber() {
    proc.send(heap, counterPid, heap.make_tuple([heap.make_atom("get_counter"), jsPid]));
    run_some();
    let rec = proc.poll_receive(heap);
    displ.innerHTML = "" + heap.get_int(rec);
}

let incrButton = document.getElementById("incr");
incrButton.onclick = () => {
    proc.send(heap, counterPid, heap.make_atom("increment"));
    updateNumber();
};

let decrButton = document.getElementById("decr");
decrButton.onclick = () => {
    proc.send(heap, counterPid, heap.make_atom("decrement"));
    updateNumber();
};


let jsPid = proc.get_pid(heap);

let counterServerFun = heap.capture_fun(heap.make_atom("testing"), heap.make_atom("counter_server"), 1);
let counterPid = proc.spawn(heap, counterServerFun, [heap.make_int(BigInt(0))]);

proc.send(heap, counterPid, heap.make_tuple([heap.make_atom("get_counter"), jsPid]));
run_some();
let rec1 = proc.poll_receive(heap);
console.log("Counter: ", heap.get_int(rec1));


