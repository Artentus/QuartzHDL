# Quartz Hardware Description Language (QuartzHDL)

Quartz is a strongly typed hardware description language with Rust-like syntax that compiles to SystemVerilog.
As it is still work-in-progress some planned features are not or only partially implemented.
Quartz is designed for FPGA synthesis only and as such has only limited support for tri-state logic.

## Inbuilt types

On its core Quartz only has one inbuilt type, `bits<N>`, which is a vector of N bits.
A bit in Quartz must always be in a valid logic state, Z and X as in Verilog are not valid states.

The inbuilt type names `bit` and `bool` are aliases for `bits<1>`, and the literals `true` and `false` are aliases for `1` and `0` respectively.

## Variables

Variables in Quartz can be of two kinds:
- `sig`: a wire connection that carries a signal
- `reg`: a memory cell that stores a state

```rust
sig a: bits<8>;
reg b: bits<16>;
```

### Ports

Ports are a special kind of variable that also defines a direction, either `in`, `out` or `inout`. Combining `in` or `inout` with `reg` is not legal.

```rust
in sig a: bit,
out sig b: bit,
out reg c: bit,

// illegal
in reg d: bit,
```

## Arrays

Quartz supports arrays to define multiple values of the same type at once.
Arrays are often used to define memory blocks but are also supported with signals, ports, and in structures.

```rust
reg mem: [bits<8>; 1024];
```

Array items are accessed using the postfix `[]` syntax.

## Structures

Quartz allows the declaration of structs to group multiple values together.
Structs consist of one or more fields where each can have a different type (including other structures and arrays).

```rust
struct Instruction {
    alu_op: AluOp,
    imm: Bits<5>,
    rs: Bits<3>,
    rd: Bits<3>,
    extended: bool,
}
```

Structs can be instantiated with sig or reg:

```rust
sig instr: Instruction;
```

Structs can be assigned in whole:

```rust
instr = Instruction {
    alu_op: AluOp::Mul,
    imm: 5,
    rs: 2,
    rd: 1,
    extend: false,
};
```

Or just a field can be assigned:

```rust
instr.alu_op = AluOp::Add;
```

And of course fields can be accessed in expressions:

```rust
alu_op = instr.alu_op;
```

They can even be used as parameters to modules.

## Enumerations

Enumerations map bit values to human readable names and improve exhaustiveness checks.
An enum must always specify its underlying `bits` type and is castable to that type.

```rust
enum AluOp: bits<4> {
    Add = 0,
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

If no value is specified for a variant it will take the value of the previous variant plus 1, starting at 0 for the first variant.


## Modules

Like Verilog, Quartz divides digital designs into modules.
A module consists of ports, variables, sub-modules and processes.

```rust
mod M (
    // port list
) {
    // body: contains variables, sub-modules and processes
}
```

### Sub-modules

To instantiate a sub-module use the `let` keyword:
```rust
let sub: SubModule;
```

### Processes

Processes define the behaviour of the module. There are two kinds of processes in Quartz:
- `comb`: a combinatorial process which is executed continuously
- `proc`: a sequential process which is executed on the edges of clock signals

To specify which clock edges a sequential process is sensitive to, use a comma separated list of `rising(clk)` or `falling(clk)`.

```rust
sig addr: bits<ADDR_BITS>;
sig data: bits<DATA_BITS>;
reg mem: [bits<DATA_BITS>; 1 << ADDR_BITS];

comb {
    // body of the comb block
    // assignments here are combinatorial
    if !write {
        data = mem[addr]; // only sig variables can be assigned
    }
}

proc rising(clk) {
    // body of the proc block
    // assignments here are sequential (will happen at the clock edge)
    if write {
        mem[addr] = data; // only reg variables can be assigned
    }
}
```

Only `sig` variables can be assigned in `comb` processes, and only `reg` variables can be assigned in `proc` processes.

## Constant evaluation

Quartz has a constant evaluation context that operates on signed 64 bit integers only.
Constants can be defined inside and outside of modules, but not inside processes.
If a constant is defined inside a module, it is only accessible inside that module and its port declarations.

```rust
const C = 10;
```

### Functions

Functions can be defined to perform more complex computations on constants.
All functions must return a value.

```rust
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

### Generic constants

Modules in Quatz can be generic over constants. Generic constants behave like constants defined inside the module.

```rust
mod M<N> (
    in sig data: bits<N>,
) {
    // ...
}
```

## Extern module

Extern modules are modules without body. They offer a way to interface with existing Verilog modules.
The actual Verilog implementation of the module must be supplied to the synthesis software together with the code generated by Quartz.

```rust
extern mod Pll (
    in sig clk25: bit,

    out sig clk200: bit,
    out sig clk120: bit,
    out sig clk40: bit,
    out sig locked: bool,
);
```

## Attributes

Quartz supports attributes on modules, ports and variables. Attributes are converted into equivalent Verilog attributes.

```rust
#[attr1, attr2(param)]
sig a: bits<16>;
```

## Statements / Expressions

### Match statements / expressions

Enumerations can be used in Rust-like match statements, which are checked for exhaustivity similar to Rust. In other words, if an option were added to the above enumeration, you would get a compile error prompting you to add a clause to the following match statement.

```rust
enum AluOp: bits<4> {
    Add = 0,
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

comb {
    result = match op {
        AluOp::Add => adder.result,
        AluOp::AddC => adder.result,
        AluOp::Sub => adder.result,
        AluOp::SubB => adder.result,
        AluOp::And => lhs & rhs,
        AluOp::Or => lhs | rhs,
        AluOp::Xor => lhs ^ rhs,
        AluOp::Shl => lhs << (shift_amt as bits::<BITS>),
        AluOp::Lsr => lhs >> (shift_amt as bits::<BITS>),
        AluOp::Asr => lhs >>> (shift_amt as bits::<BITS>),
        AluOp::Mul => lhs * rhs,
    };
}
```

Also note that, like Rust, match statements are expressions and so can produce a value for an assignment.

To add a block to a match arm, use `{` and `}`:

```rust
match op {
    AluOp::Add => {
        add.op = AdderOp::Add;
        next_result = add.result;
    }
}
```

To specify a wildcard match arm, use `_`, which will disable exhaustivity checking:

```rust
adder.c_in = match op {
    AluOp::Add => 0,
    AluOp::AddC => flags_in.c,
    AluOp::Sub => 1,
    AluOp::SubB => flags_in.c,
    _ => 0,
};
```

You can also have multiple matches per arm:

```rust
flags_out = match op {
    AluOp::Add | AluOp::AddC | AluOp::Sub | AluOp::SubB => Flags {
        c: adder.c_out,
        z: z,
        s: s,
        o: o,
    },
    AluOp::And | AluOp::Or | AluOp::Xor | AluOp::Shl | AluOp::Lsr | AluOp::Asr => Flags {
        c: flags_in.c,
        z: z,
        s: flags_in.s,
        o: flags_in.o,
    },
    AluOp::Mul => Flags {
        c: flags_in.c,
        z: z,
        s: s,
        o: o,
    },
};
```

Match can also be used in similar ways to verilog case statements.

### If statements / expressions

If statements are similar to Rust. The do not have parenthesis around the condition, and have if else and else branches.

```rust
if cpu_reset {
    timer_div = 0 as bits::<TIMER_DIV_BITS>;
    timer = 0 as bits::<64>;
} else if timer_div == (CLOCKS_PER_MICRO_SECOND - 1) {
    timer_div = 0 as bits::<TIMER_DIV_BITS>;
    timer = timer + 1;
} else {
    timer_div = timer_div + 1;
}
```

They are also expressions, and so are the much more readable alternative to verilog's trinaries:

```rust
adder.rhs = if (op == AluOp::Sub) | (op == AluOp::SubB) {
    !rhs
} else {
    rhs
};
```

Note: the curly braces `{`, `}` around the then and else clauses are always required in order to avoid several common sources of bugs.

### Bit concatenation operator `@`

You can use the `@` operator to concatenate bits into wider values:

```rust
sig least_significant: bits<16>;
sig middle: bits<32>;
sig most_significant: bits<16>;
sig wide: bits<64>;
comb {
    least_significant = 0x24;
    middle = 0x42;
    most_significant = 0x92;

    wide_value = most_significant @ middle @ least_significant;
    // wide_value == 0x0092_00000042_0024
}
```

The left-most value becomes the most significant bits, the right-most value becomes the least significant bits, exactly how numbers work.

### Shifting

You can do bitwise shifting with `<<` and `>>` that function in the way you would expect from other languages.

There is also a `>>>` operator that does arithmetic right shifts. In other words, this operator copies the most significant (sign) bit into the bit positions vacated by the shift right.
