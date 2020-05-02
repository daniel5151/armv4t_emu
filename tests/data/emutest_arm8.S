@ vim:ft=armv4
@ expected result: 0x200 = 10, 0x204 = 83
@ regression test for an issue where lt did not work

mov r13, #0x200

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
