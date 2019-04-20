export function wrap_catch_bail(fun) {
    try {
        fun();
        return true;
    } catch (e) {
        if (e == "whirl_bail") {
            return false;
        } else {
            throw e;
        }
    }
}

export function throw_bail() {
    throw "whirl_bail";
}
