.globl _start
_start:
li sp, 2415935488
_int_en:
    addi sp, sp, -12
    sw a0, 0(sp)
    sw a1, 4(sp)
    sw t0, 8(sp)
    addi a0,x0,8
    addi t0,x0,11
    lui a1,1092
    srl a1,a1,t0
    csrrw a0,mstatus,a0     /*mstatus = 'b1000*/
    csrrw a1,mie,a1         /*mie = 'b100010001000*/
    lw a0, 0(sp)
    lw a1, 4(sp)
    lw t0, 8(sp)
    addi sp, sp, 12
main:
li x10,10
li x11,11
li x12,12
li x13,13
li x14,14
li x15,15
li x16,16
li x17,17
li x18,18
li x19,19

