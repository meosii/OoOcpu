.globl _start
_start:
li sp, 2415935488
addi sp,sp,-64
main:
li a5,1200
li a4,20
div a6,a5,a4
rem a7,a5,a4
li a0,3000
li a1,3
jal ra,jump
div a2,a0,a1
rem a3,a0,a1
li a5,1500
addi a4,a2,1000
sub a6,a4,a5
li a7,500
beq a6,a7,br
li x10,0
li x11,1
li x12,2
br:
li x13,3
li x14,4
li x15,5
li x16,6
li x17,7
li x18,8
li x19,9
li x20,10
add x21,x10,x11
add x22,x12,x13
jump:
li x13,7
li x14,8
li x15,9
li x16,10
li x17,11
li x10,0x90000000
li x11,0x90000004
li x12,0x90000008
sw x13,0(x10)
sw x14,0(x11)
sw x15,0(x12)
lw x20,0(x10)
lw x21,0(x11)
lw x22,0(x12)
add x23,x22,x21
li x10,0xaa
li x20,0xbb
add x21,x10,x11
add x22,x12,x13
li x10,0
jalr ra,x10,0x50
