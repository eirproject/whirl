


struct VM {
    pcbs: Vec<Option<PCB>>,
}

struct PCB {
    heap: ProcessHeap,
}

struct ProcessHeap {
    mem: Vec<usize>,
}
