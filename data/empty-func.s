	.file	"simple-func.c"
	.option nopic
	.attribute arch, "rv32i2p0_m2p0_a2p0_c2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
.Ltext0:
	.cfi_sections	.debug_frame
	.align	1
	.globl	simple_func
	.type	simple_func, @function
simple_func:
.LFB0:
	.file 1 "simple-func.c"
	.size	simple_func, .-simple_func
.Ltext0:
	.cfi_sections	.debug_frame
	.align	1
	.globl	simple_func
	.type	simple_func, @function
simple_func:
.LFB0:
	.file 1 "empty-func.c"
	.loc 1 5 35
	.cfi_startproc
	addi	sp,sp,-48
	.cfi_def_cfa_offset 48
	sw	s0,44(sp)
	.cfi_offset 8, -4
	addi	s0,sp,48
	.cfi_def_cfa 8, 0
	sw	a0,-36(s0)
	sw	a1,-40(s0)
	.string	"int32_t"
	.ident	"GCC: () 10.2.0"
