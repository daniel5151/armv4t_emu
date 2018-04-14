@ vim:ft=armv4
@ assume SP starts at 200
@ expected result: 0x1ec = 10, 0x1f0 = 15, 0x1f4 = 5, 0x1f8 = 60, 0x1fc = 0x200

.thumb
mov r0, #10
mov r1, #15
mov r2, r1
eor r2, r2, r0
lsl r3, r1, #2
mov r4, r13
push {r0, r1, r2, r3, r4}
