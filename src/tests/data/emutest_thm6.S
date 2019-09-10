@ vim:ft=armv4
@ assume SP starts at 200
@ expected result: 0x1fc = 0
@ regression test for an issue where sbc added instead of subbing

.thumb
mov r0, #2
mov r1, #2
cmp r0, r1
sbc r0, r0
push {r0}
