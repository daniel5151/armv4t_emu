@ vim:ft=armv4
@ assume SP starts at 200
@ expected result: 0x1fc = 0xff
@ regression test for unaligned memory access

.thumb
mov r0, #0xff
lsl r0, r0, #16
mov r2, sp
str r0, [r2]
mov r3, #2
ldr r1, [r2, r3]
push {r1}
