# arm7tdmi-rs

A (work in progress) ARM7-TDMI emulator written in Rust.

Initially written as part of [iburinoc's Gameboy Advance emulator](https://github.com/iburinoc/gba-rs), arm7tmdi-rs is now a standalone library that's ready to be used in any project that might need it.

## Getting Started

TODO: show an example

## Optional Features

There are a couple of optional features that you may want to enable, providing various bits of functionality / debugging enhancements. These are disabled by default, as though they do bloat compile times.

TODO: it should be possible to make the crate no-std as well.

- Enabling "advanced_disasm" will bring in [`capstone`](https://github.com/capstone-rust/capstone-rs) as a dependency, providing a more detailed instruction disassembly trace. It is disabled by default, as it brings in a large C-library as a dependency to this otherwise tiny crate.
- Enabling "serde" will enable serializing and deserializing the main Cpu type (though breakpoints will not be preserved)

## Missing Functionality

- [ ] cycle accuracy
    - [ ] accurate prefetching
- [ ] coprocessor instruction support
- [ ] support configuring endianness (currently fixed to little endian)
