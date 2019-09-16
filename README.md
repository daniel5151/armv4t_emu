# arm7tdmi-rs

A (work in progress) ARM7-TDMI emulator written in Rust.

Initially written as part of [iburinoc's Gameboy Advance emulator](https://github.com/iburinoc/gba-rs), arm7tmdi-rs is now a standalone library ready to be used in any project that might require it.

## Getting Started

TODO: show an example

## Optional Features

Enabling "advanced_disasm" will bring in [`capstone`](https://github.com/capstone-rust/capstone-rs) as a dependency for more detailed execution logs. It is disabled by default, as it brings in a large C-library as a dependency to this otherwise tiny crate.

TODO: make serde optional

## TODOs and Missing Functionality

- [ ] co-processor instructions
- [ ] cycle accuracy
    - [ ] accurate prefetching
- [ ] support configuring endianness (currently fixed to little endian)
