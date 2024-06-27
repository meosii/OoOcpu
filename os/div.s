.globl _start
_start:
li sp, 2415935488
addi sp,sp,-64
main:
li a5,1000
li a4,20
div x16,a5,a4
rem x17,a5,a4
addi s4,x16,4
li x10,-30
li x11,256
div x12,x10,x11
rem x13,x10,x11
addi x14,x16,1
and x15,a5,a4
sub x16,a5,x17
li x17,500
addi x18,x11,244
beq x18,x17,br
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
