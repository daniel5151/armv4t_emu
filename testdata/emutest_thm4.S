@ vim:ft=armv4
@ assume SP starts at 200
@ expected result: 0x200 = 4, 0x204 = 5

.thumb
bl func
b end

func:
mov r0, #4
mov r1, lr
bx lr

end:
str r0, [sp]
str r1, [sp, #4]
