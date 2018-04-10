@ vim:ft=armv4
@ expected result: 0x100 = 6, 0x104 = 0x200000e1, 0x108 = 0xe100001c, 0x10c = 6, 0x110 = 6*256

mov r0, #1
mov r1, #2
mov r2, #3
mul r3, r2, r1

mov r8, #0xf000000f
mov r9, #0xf000000f
umull r10, r11, r8, r9

mov r6, #0x100
str r3, [r6], #4
str r10, [r6], #4
str r11, [r6], #4
ldr r7, [r6, #-12]!
str r7, [r6, #12]
strb r7, [r6, #17]
