# armv4t_emu

[![](http://meritbadge.herokuapp.com/armv4t_emu)](https://crates.io/crates/armv4t_emu)
[![](https://docs.rs/armv4t_emu/badge.svg)](https://docs.rs/armv4t_emu)

An emulator for the ARMv4T instruction set, written in Rust.

Initially written as part of [iburinoc's Gameboy Advance emulator](https://github.com/iburinoc/gba-rs), armv4t_emu is now a standalone library that's ready to be used in any project that might need it!

At the moment, the crate is _almost_ feature complete, and is capable of running large programs (ostensibly) successfully. See the issue tracker for a list of missing / incomplete features.

## Example

```rust
use armv4t_emu::{reg, Cpu, ExampleMem, Mode, Memory};

let prog = &[
    0x06, 0x00, 0xa0, 0xe3, //    mov r0, #6
    0x01, 0x10, 0xa0, 0xe3, //    mov r1, #1
    0x01, 0x10, 0x81, 0xe0, // l: add r1, r1, r1
    0x01, 0x00, 0x50, 0xe2, //    subs r0, #1
    0xfc, 0xff, 0xff, 0x1a, //    bne l
    0x01, 0x6c, 0xa0, 0xe3, //    mov r6, #0x100
    0x00, 0x10, 0x86, 0xe5, //    str r1, [r6]
    0xf7, 0xf0, 0xde, 0xad  // ; trigger undefined instr exception
];
let mut mem = ExampleMem::new_with_data(prog);
let mut cpu = Cpu::new();
cpu.reg_set(Mode::User, reg::PC, 0x00);
cpu.reg_set(Mode::User, reg::CPSR, 0x10);

while cpu.step(&mut mem) {}

assert_eq!(64, mem.r32(0x100));
```

## Optional Features

There are a couple of optional features that you may want to enable, providing various bits of functionality / debugging enhancements. These are disabled by default, as though they do bloat compile times.

Feature | Description
--------|-------------
`advanced_disasm` | Uses [`capstone`](https://github.com/capstone-rust/capstone-rs) to disassemble + log instructions. _Warning:_ Substantially increases compile times.*
`serde` | Adds `Serialize` and `Deserialize` derives on important types/structs.

\* Instead of debugging emulated code by creating a ad-hoc, application-specific debugger, consider using the [`gdbstub`](https://github.com/daniel5151/gdbstub) crate.

## Missing Features

At the moment, this crate's feature set is primarily motivated by whatever functionality it's dependent projects require. As such, there are several features of the ARMv4T instruction set which are not implemented at this time:

- Custom co-processor support (see [#3](https://github.com/daniel5151/armv4t_emu/issues/3))
- Big-Endian support (see [#4](https://github.com/daniel5151/armv4t_emu/issues/4))
- Support for "cycle accurate" emulation
    - This would be tricky to implement, as `armv4t_emu` isn't an emulator for any particular ARMv4T CPU.
    - Theoretically, this could be implemented by modifying the public API to accept some kind of platform-specific timing information.
