import { init, run_some, TermHeap, JsProcess } from "../out/linked.js";

console.log("BeforeInit");
init();
console.log("AfterInit");

// Allocate a term heap and make a process
let heap = TermHeap.alloc(1000);
let process = JsProcess.new();

// Spawn a process
let testingAtom = heap.make_atom("testing");
let fibAtom = heap.make_atom("fib_response");
let funTerm = heap.capture_fun(testingAtom, fibAtom, 2);
let fiveTerm = heap.make_int(BigInt(7));
let js_pid = process.get_pid(heap);
let pid = process.spawn(heap, funTerm, [js_pid, fiveTerm]);

// Run reductions
run_some();

// Expect a message in return with the result
let ret = process.poll_receive(heap);
let num = heap.get_int(ret);
console.log("Result: ");
console.log(num);
