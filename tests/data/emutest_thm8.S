@ vim:ft=armv4
@ assume SP starts at 200
@ expected result: 0x1fc = 0x01234567
@ regression test for not forcing aligned PC load

.thumb
nop
ldr r1, [pc, #4]
b end
nop
.word 0x01234567
.word 0x89abcdef
end:
push {r1}
