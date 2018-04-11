@ vim:ft=armv4
@ expected result: 0x1fc = 1, 0x200 = 1, 0x204 = 0x200

mov r13, #0x200

mov r0, #1
stmed r13, {r0, r13}
swp r1, r0, [r13]
str r1, [r13, #4]
