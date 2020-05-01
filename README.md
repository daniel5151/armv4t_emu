# armv4t_emu

[![](http://meritbadge.herokuapp.com/armv4t_emu)](https://crates.io/crates/armv4t_emu)
[![](https://docs.rs/armv4t_emu/badge.svg)](https://docs.rs/armv4t_emu)

An emulator for the ARMv4t instruction set, written in Rust.

Initially written as part of [iburinoc's Gameboy Advance emulator](https://github.com/iburinoc/gba-rs), armv4t_emu is now a standalone library that's ready to be used in any project that might need it!

## Getting Started

TODO: show an example

## Optional Features

There are a couple of optional features that you may want to enable, providing various bits of functionality / debugging enhancements. These are disabled by default, as though they do bloat compile times.

Feature | Description
--------|-------------
`advanced_disasm` | Uses [`capstone`](https://github.com/capstone-rust/capstone-rs) to disassemble + log instructions. _Warning:_ Increases compile times substantially.
`serde` | Adds `Serialize` and `Deserialize` derives on important types/structs.

## Missing Functionality

At the moment, this crate's feature set is primarily motivated by whatever functionality it's dependent projects require. As such, there are several features of the ARMv4t instruction set which are not implemented at this time:

- Custom co-processor support (see [#3](https://github.com/daniel5151/armv4t_emu/issues/3))
- Big-Endian support (see [#4](https://github.com/daniel5151/armv4t_emu/issues/3))
- Support for "cycle accurate" emulation
    - This would be tricky to implement, as `armv4t_emu` isn't an emulator for any particular ARMv4t CPU.
    - Theoretically, this could be implemented by modifying the public API to accept some kind of platform-specific timing information.
