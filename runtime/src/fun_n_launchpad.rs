use crate::Term;

macro_rules! fun_call {
    ($fun:expr, $proc_env:expr, $args:expr, ($($e:expr),*)) => {
        $fun($proc_env, $( $args[$e] ),*)
    };
}

macro_rules! term_typ {
    ($e:expr) => { Term }
}

macro_rules! fun_type {
    (($($e:expr),*)) => {
        extern "C" fn(*const u8, $(term_typ!($e)),*)
    };
}

macro_rules! define_launchpad {
    ($name:ident, $n:expr, $args:tt) => {
        fn $name(
            proc_env: *const u8,
            fun_ptr: extern "C" fn(),
            args: &[Term]
        ) {
            assert!(args.len() == $n);
            let fun_casted = unsafe { std::mem::transmute::<_, fun_type!($args)>(
                fun_ptr) };
            fun_call!(fun_casted, proc_env, args, $args);
        }
    };
}

define_launchpad!(fun_launchpad_0, 0, ());
define_launchpad!(fun_launchpad_1, 1, (0));
define_launchpad!(fun_launchpad_2, 2, (0, 1));
define_launchpad!(fun_launchpad_3, 3, (0, 1, 2));
define_launchpad!(fun_launchpad_4, 4, (0, 1, 2, 3));
define_launchpad!(fun_launchpad_5, 5, (0, 1, 2, 3, 4));
define_launchpad!(fun_launchpad_6, 6, (0, 1, 2, 3, 4, 5));
define_launchpad!(fun_launchpad_7, 7, (0, 1, 2, 3, 4, 5, 6));
define_launchpad!(fun_launchpad_8, 8, (0, 1, 2, 3, 4, 5, 6, 7));
define_launchpad!(fun_launchpad_9, 9, (0, 1, 2, 3, 4, 5, 6, 7, 8));
define_launchpad!(fun_launchpad_10, 10, (0, 1, 2, 3, 4, 5, 6, 7, 8, 9));

pub const FUN_LAUNCHPADS: [fn(*const u8, extern "C" fn(), &[Term]); 11] = [
    fun_launchpad_0,
    fun_launchpad_1,
    fun_launchpad_2,
    fun_launchpad_3,
    fun_launchpad_4,
    fun_launchpad_5,
    fun_launchpad_6,
    fun_launchpad_7,
    fun_launchpad_8,
    fun_launchpad_9,
    fun_launchpad_10,
];
