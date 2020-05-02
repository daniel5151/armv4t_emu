@ vim:ft=armv4
@ assume SP starts at 200
@ expected result: 0x200 = 10, 0x204 = 83
@ regression test for an issue where lt did not work

.thumb
mov r2, #83
mov r3, #10
cmp r2, r3
blt lt
str r3, [sp]
str r2, [sp, #4]
b end
lt:
str r2, [sp]
str r3, [sp, #4]
end:
