@ vim:ft=armv4
@ assume SP starts at 200
@ expected result: 0x1f8 = 8, 0x1fc = 0x200, 0x200 = 64

.thumb
add r0, pc, #4
add sp, #-8
mov r1, sp
stmia r1!, {r0, r1}

mov r2, #6
mov r3, #1
mov r4, #0
loop:
sub r2, #1
add r3, r3, r3
cmp r2, r4
bne loop
str r3, [r1, #0]
