@ vim:ft=armv4
@ expected result: 0x100 = 0xf000, 0x104 = 0xfff0, 0x108 = 0x104

mov r11, #0x100

mov r0, #0xff000
strh r0, [r11, -r1]
ldrsb r1, [r11, #1]
strh r1, [r11, #4]!
mov r12, r11
str r12, [r11, #4]
