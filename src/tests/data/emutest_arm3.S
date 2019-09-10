@ vim:ft=armv4
@ expected result: 0x100 = 64

mov r0, #6
mov r1, #1

l:
add r1, r1, r1
subs r0, #1
bne l

mov r6, #0x100
str r1, [r6]
