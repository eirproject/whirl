import { init } from "../out/linked.js";

function invoke_fn_ptr_exc_launchpad() {
    console.log("yay!");
}

console.log("bef");
init();
console.log("aft");
