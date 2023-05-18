# Quartz Hardware Description Language (HDL)

Quartz is a Hardware Description Language (HDL) that compiles to Verilog. The language enables users to write more maintainable, reusable, and safe code for FPGAs and ASICs. Quartz provides a higher level of abstraction for describing digital circuits than Verilog, reducing the likelihood of bugs and making the code more readable.

## Quartz Code Structure

A Quartz program is composed of functions, data structures, and modules, which describe the behavior and structure of the hardware. 

### Functions

Functions in Quartz are similar to those in many other programming languages. Here's a function that calculates the binary logarithm (base 2):

```Quartz
fn clog2(n_bits) {
    n_bits -= 1;
    let log = 0;
    while n_bits != 0 {
        n_bits >>= 1;
        log += 1;
    }
    log
}
```

### Data Structures

Data structures in Quartz include structs, enums, and arrays. The `struct` keyword is used to define composite data types, much like in C or Rust. The `enum` keyword is used to define enumerated types.

```Quartz
struct WritePort<ADDR_BITS, DATA_BITS> {
    clk: bit,
    en: bool,
    addr: bits<ADDR_BITS>,
    data: bits<DATA_BITS>,
}

enum AluOp: bits<4> {
    Add,
    AddC,
    Sub,
    SubB,
    And,
    Or,
    Xor,
    Shl,
    Lsr,
    Asr,
    Mul,
}
```

### Modules

The `mod` keyword defines a hardware module. Modules can have input (`in`), output (`out`), and internal (`reg`, `sig`, `proc`) signals.

```Quartz
mod Bram<ADDR_BITS, DATA_BITS>(
    in sig wport: WritePort<ADDR_BITS, DATA_BITS>,
    in sig rport: ReadPort<ADDR_BITS>,
    out reg rdata: bits<DATA_BITS>,
) {
    reg mem: [bits<DATA_BITS>; 1 << ADDR_BITS];
    
    proc rising(wport.clk) {
        if wport.en {
            mem[wport.addr] = wport.data;
        }
    }
    proc rising(rport.clk) {
        if rport.en {
            rdata = mem[rport.addr];
        }
    }
}
```

## Key Concepts

### Signals

Signals in Quartz (`sig`) represent wires in the hardware design. A signal can be an input, output, or an internal signal of a module.

### Registers

Registers (`reg`) in Quartz represent stateful elements in hardware design that can store a value and present it to other components in the system.

### Processes

Processes (`proc`) define behavior of the module on different events. In Quartz, these events are clock edges (`rising`, `falling`). A process is a set of statements that is executed when its triggering event occurs.

### Combinational Logic

The `comb` block defines combinational logic, i.e., logic that isn't dependent on a clock signal. The statements in a `comb` block are executed every time any of its dependent signals changes.

## Generics

Quartz also supports generics (denoted by `<...>`), which allows for writing reusable modules and functions that work with different types or sizes of data.

## Concurrency and Synchronization

Unlike many high-level programming languages, Quartz does not execute line by line in a sequential manner. Instead, it describes a hardware circuit, where every part of the code is executing concurrently.

Synchronization in Quartz, as in other HDLs, is accomplished through the use of clock signals

. A `proc` block executes when its associated clock signal transitions (usually on a rising or falling edge).

## Operators

Quartz includes many operators for performing operations on bits, including bit-wise AND (`&`), OR (`|`), XOR (`^`), bit-wise negation (`!`), bit-wise shifts (`<<`, `>>`, `>>>`), and addition (`+`), subtraction (`-`), and multiplication (`*`).

## Conclusion

Quartz provides an alternative to traditional HDLs that is easier to write, understand, and maintain. This high-level abstraction is particularly useful for complex designs, where the risk of bugs increases with the complexity of the design. With its higher level of abstraction and more intuitive syntax, Quartz can make hardware design more accessible to those used to high-level programming languages.
