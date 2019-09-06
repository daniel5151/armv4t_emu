@ vim:ft=armv4
@ assume SP starts at 200
@ expected result: 0x200: 0xdeadbeef

.thumb
mov r0, sp
mov r1, #0x0
.balign 4
ldr r5, [pc, #4]
str r5, [r0, r1]
b end
nop
.word 0xdeadbeef
end:
