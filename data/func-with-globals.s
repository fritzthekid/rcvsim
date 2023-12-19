	.file	"func-with-globals.c"
	.option nopic
	.attribute arch, "rv32i2p0_m2p0_a2p0_c2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
.Ltext0:
	.cfi_sections	.debug_frame
	.globl	buffer
	.bss
	.align	2
	.type	buffer, @object
	.size	buffer, 4000
buffer:
	.zero	4000
	.text
	.align	1
	.globl	simple_func
	.type	simple_func, @function
simple_func:
.LFB0:
	.file 1 "func-with-globals.c"
	.loc 1 5 49
	.cfi_startproc
	addi	sp,sp,-48
	.cfi_def_cfa_offset 48
	sw	s0,44(sp)
	.cfi_offset 8, -4
	addi	s0,sp,48
	.cfi_def_cfa 8, 0
	sw	a0,-36(s0)
	sw	a1,-40(s0)
	sw	a2,-44(s0)
	.loc 1 6 11
	sw	zero,-20(s0)
	.loc 1 7 5
	lw	a5,-36(s0)
	sw	a5,-20(s0)
	.loc 1 8 9
	lw	a4,-40(s0)
	lw	a5,-36(s0)
	mul	a5,a4,a5
	.loc 1 8 5
	lw	a4,-20(s0)
	add	a5,a4,a5
	sw	a5,-20(s0)
	.loc 1 9 13
	lui	a5,%hi(buffer)
	addi	a5,a5,%lo(buffer)
	li	a4,18
	sw	a4,0(a5)
	.loc 1 10 13
	lui	a5,%hi(buffer)
	addi	a5,a5,%lo(buffer)
	lw	a4,-20(s0)
	sw	a4,4(a5)
	.loc 1 11 10
	lw	a4,-40(s0)
	mv	a5,a4
	slli	a5,a5,4
	add	a5,a5,a4
	.loc 1 11 5
	lw	a4,-20(s0)
	add	a5,a4,a5
	sw	a5,-20(s0)
	.loc 1 12 5
	lw	a5,-20(s0)
	addi	a5,a5,16
	sw	a5,-20(s0)
	.loc 1 13 5
	lw	a4,-20(s0)
	lw	a5,-40(s0)
	mul	a5,a4,a5
	sw	a5,-20(s0)
	.loc 1 14 8
	lw	a5,-44(s0)
	lw	a4,-20(s0)
	sw	a4,0(a5)
	.loc 1 15 10
	li	a5,0
	.loc 1 16 1
	mv	a0,a5
	lw	s0,44(sp)
	.cfi_restore 8
	.cfi_def_cfa 2, 48
	addi	sp,sp,48
	.cfi_def_cfa_offset 0
	jr	ra
	.cfi_endproc
.LFE0:
	.size	simple_func, .-simple_func
.Letext0:
	.file 2 "/usr/lib/gcc/riscv64-unknown-elf/10.2.0/include/stdint-gcc.h"
	.section	.debug_info,"",@progbits
.Ldebug_info0:
	.4byte	0xee
	.2byte	0x4
	.4byte	.Ldebug_abbrev0
	.byte	0x4
	.byte	0x1
	.4byte	.LASF9
	.byte	0xc
	.4byte	.LASF10
	.4byte	.LASF11
	.4byte	.Ltext0
	.4byte	.Letext0-.Ltext0
	.4byte	.Ldebug_line0
	.byte	0x2
	.byte	0x1
	.byte	0x6
	.4byte	.LASF0
	.byte	0x2
	.byte	0x2
	.byte	0x5
	.4byte	.LASF1
	.byte	0x3
	.4byte	.LASF12
	.byte	0x2
	.byte	0x28
	.byte	0x12
	.4byte	0x3f
	.byte	0x2
	.byte	0x4
	.byte	0x5
	.4byte	.LASF2
	.byte	0x2
	.byte	0x8
	.byte	0x5
	.4byte	.LASF3
	.byte	0x2
	.byte	0x1
	.byte	0x8
	.4byte	.LASF4
	.byte	0x2
	.byte	0x2
	.byte	0x7
	.4byte	.LASF5
	.byte	0x2
	.byte	0x4
	.byte	0x7
	.4byte	.LASF6
	.byte	0x2
	.byte	0x8
	.byte	0x7
	.4byte	.LASF7
	.byte	0x4
	.byte	0x4
	.byte	0x5
	.string	"int"
	.byte	0x2
	.byte	0x4
	.byte	0x7
	.4byte	.LASF8
	.byte	0x5
	.4byte	0x33
	.4byte	0x88
	.byte	0x6
	.4byte	0x70
	.2byte	0x3e7
	.byte	0
	.byte	0x7
	.4byte	.LASF13
	.byte	0x1
	.byte	0x3
	.byte	0x9
	.4byte	0x77
	.byte	0x5
	.byte	0x3
	.4byte	buffer
	.byte	0x8
	.4byte	.LASF14
	.byte	0x1
	.byte	0x5
	.byte	0x9
	.4byte	0x33
	.4byte	.LFB0
	.4byte	.LFE0-.LFB0
	.byte	0x1
	.byte	0x9c
	.4byte	0xeb
	.byte	0x9
	.string	"x"
	.byte	0x1
	.byte	0x5
	.byte	0x19
	.4byte	0x69
	.byte	0x2
	.byte	0x91
	.byte	0x5c
	.byte	0x9
	.string	"y"
	.byte	0x1
	.byte	0x5
	.byte	0x20
	.4byte	0x69
	.byte	0x2
	.byte	0x91
	.byte	0x58
	.byte	0x9
	.string	"out"
	.byte	0x1
	.byte	0x5
	.byte	0x2c
	.4byte	0xeb
	.byte	0x2
	.byte	0x91
	.byte	0x54
	.byte	0xa
	.string	"a"
	.byte	0x1
	.byte	0x6
	.byte	0xb
	.4byte	0x33
	.byte	0x2
	.byte	0x91
	.byte	0x6c
	.byte	0
	.byte	0xb
	.byte	0x4
	.4byte	0x33
	.byte	0
	.section	.debug_abbrev,"",@progbits
.Ldebug_abbrev0:
	.byte	0x1
	.byte	0x11
	.byte	0x1
	.byte	0x25
	.byte	0xe
	.byte	0x13
	.byte	0xb
	.byte	0x3
	.byte	0xe
	.byte	0x1b
	.byte	0xe
	.byte	0x11
	.byte	0x1
	.byte	0x12
	.byte	0x6
	.byte	0x10
	.byte	0x17
	.byte	0
	.byte	0
	.byte	0x2
	.byte	0x24
	.byte	0
	.byte	0xb
	.byte	0xb
	.byte	0x3e
	.byte	0xb
	.byte	0x3
	.byte	0xe
	.byte	0
	.byte	0
	.byte	0x3
	.byte	0x16
	.byte	0
	.byte	0x3
	.byte	0xe
	.byte	0x3a
	.byte	0xb
	.byte	0x3b
	.byte	0xb
	.byte	0x39
	.byte	0xb
	.byte	0x49
	.byte	0x13
	.byte	0
	.byte	0
	.byte	0x4
	.byte	0x24
	.byte	0
	.byte	0xb
	.byte	0xb
	.byte	0x3e
	.byte	0xb
	.byte	0x3
	.byte	0x8
	.byte	0
	.byte	0
	.byte	0x5
	.byte	0x1
	.byte	0x1
	.byte	0x49
	.byte	0x13
	.byte	0x1
	.byte	0x13
	.byte	0
	.byte	0
	.byte	0x6
	.byte	0x21
	.byte	0
	.byte	0x49
	.byte	0x13
	.byte	0x2f
	.byte	0x5
	.byte	0
	.byte	0
	.byte	0x7
	.byte	0x34
	.byte	0
	.byte	0x3
	.byte	0xe
	.byte	0x3a
	.byte	0xb
	.byte	0x3b
	.byte	0xb
	.byte	0x39
	.byte	0xb
	.byte	0x49
	.byte	0x13
	.byte	0x3f
	.byte	0x19
	.byte	0x2
	.byte	0x18
	.byte	0
	.byte	0
	.byte	0x8
	.byte	0x2e
	.byte	0x1
	.byte	0x3f
	.byte	0x19
	.byte	0x3
	.byte	0xe
	.byte	0x3a
	.byte	0xb
	.byte	0x3b
	.byte	0xb
	.byte	0x39
	.byte	0xb
	.byte	0x27
	.byte	0x19
	.byte	0x49
	.byte	0x13
	.byte	0x11
	.byte	0x1
	.byte	0x12
	.byte	0x6
	.byte	0x40
	.byte	0x18
	.byte	0x97,0x42
	.byte	0x19
	.byte	0x1
	.byte	0x13
	.byte	0
	.byte	0
	.byte	0x9
	.byte	0x5
	.byte	0
	.byte	0x3
	.byte	0x8
	.byte	0x3a
	.byte	0xb
	.byte	0x3b
	.byte	0xb
	.byte	0x39
	.byte	0xb
	.byte	0x49
	.byte	0x13
	.byte	0x2
	.byte	0x18
	.byte	0
	.byte	0
	.byte	0xa
	.byte	0x34
	.byte	0
	.byte	0x3
	.byte	0x8
	.byte	0x3a
	.byte	0xb
	.byte	0x3b
	.byte	0xb
	.byte	0x39
	.byte	0xb
	.byte	0x49
	.byte	0x13
	.byte	0x2
	.byte	0x18
	.byte	0
	.byte	0
	.byte	0xb
	.byte	0xf
	.byte	0
	.byte	0xb
	.byte	0xb
	.byte	0x49
	.byte	0x13
	.byte	0
	.byte	0
	.byte	0
	.section	.debug_aranges,"",@progbits
	.4byte	0x1c
	.2byte	0x2
	.4byte	.Ldebug_info0
	.byte	0x4
	.byte	0
	.2byte	0
	.2byte	0
	.4byte	.Ltext0
	.4byte	.Letext0-.Ltext0
	.4byte	0
	.4byte	0
	.section	.debug_line,"",@progbits
.Ldebug_line0:
	.section	.debug_str,"MS",@progbits,1
.LASF11:
	.string	"/home/eduard/work/tmp/rcvsim/example"
.LASF8:
	.string	"unsigned int"
.LASF4:
	.string	"unsigned char"
.LASF6:
	.string	"long unsigned int"
.LASF5:
	.string	"short unsigned int"
.LASF9:
	.string	"GNU C17 10.2.0 -march=rv32imac -mabi=ilp32 -g -O0"
.LASF7:
	.string	"long long unsigned int"
.LASF12:
	.string	"int32_t"
.LASF3:
	.string	"long long int"
.LASF1:
	.string	"short int"
.LASF13:
	.string	"buffer"
.LASF10:
	.string	"func-with-globals.c"
.LASF2:
	.string	"long int"
.LASF14:
	.string	"simple_func"
.LASF0:
	.string	"signed char"
	.ident	"GCC: () 10.2.0"
