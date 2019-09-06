# arm7tdmi-rs

A (work in progress) ARM7-TDMI emulator written in Rust.

Initially written as part of [iburinoc's Gameboy Advance emulator](https://github.com/iburinoc/gba-rs), arm7tmdi-rs is now a standalone library ready to be used in any project that might require it.

## Getting Started

TODO: show an example

## TODOs and Missing Functionality

- [ ] co-processor instructions
- [ ] cycle accuracy
    - [ ] accurate prefetching
- [ ] endianesss configuration (currently fixed to little endian)
