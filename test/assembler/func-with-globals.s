	.file	"func-with-globals.c"
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
	.file 1 "func-with-globals.c"
	.loc 1 5 49
	.cfi_startproc
.LVL0:
	.loc 1 6 3
	.loc 1 7 3
	.loc 1 8 9 is_stmt 0
	mul	a3,a0,a1
	.loc 1 5 49
	mv	a6,a0
.LVL1:
	.loc 1 8 3 is_stmt 1
	.loc 1 11 10 is_stmt 0
	slli	a5,a1,4
	add	a5,a5,a1
	.loc 1 9 13
	lui	a4,%hi(buffer)
	addi	a4,a4,%lo(buffer)
	li	a0,18
.LVL2:
	sw	a0,0(a4)
	.loc 1 16 1
	li	a0,0
	.loc 1 8 5
	add	a3,a3,a6
.LVL3:
	.loc 1 9 3 is_stmt 1
	.loc 1 10 3
	.loc 1 11 5 is_stmt 0
	add	a5,a5,a3
	.loc 1 12 5
	addi	a5,a5,16
	.loc 1 13 5
	mul	a1,a1,a5
.LVL4:
	.loc 1 10 13
	sw	a3,4(a4)
	.loc 1 11 3 is_stmt 1
.LVL5:
	.loc 1 12 3
	.loc 1 13 3
	.loc 1 14 3
	.loc 1 14 8 is_stmt 0
	sw	a1,0(a2)
	.loc 1 15 3 is_stmt 1
	.loc 1 16 1 is_stmt 0
	ret
	.cfi_endproc
.LFE0:
	.size	simple_func, .-simple_func
	.globl	buffer
	.bss
	.align	2
	.type	buffer, @object
	.size	buffer, 4000
buffer:
	.zero	4000
	.text
.Letext0:
	.file 2 "/usr/lib/gcc/riscv64-unknown-elf/10.2.0/include/stdint-gcc.h"
	.section	.debug_info,"",@progbits
.Ldebug_info0:
	.4byte	0xf0
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
	.byte	0x18
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
	.4byte	0xed
	.byte	0x9
	.string	"x"
	.byte	0x1
	.byte	0x5
	.byte	0x19
	.4byte	0x69
	.4byte	.LLST0
	.byte	0x9
	.string	"y"
	.byte	0x1
	.byte	0x5
	.byte	0x20
	.4byte	0x69
	.4byte	.LLST1
	.byte	0xa
	.string	"out"
	.byte	0x1
	.byte	0x5
	.byte	0x2c
	.4byte	0xed
	.byte	0x1
	.byte	0x5c
	.byte	0xb
	.string	"a"
	.byte	0x1
	.byte	0x6
	.byte	0xb
	.4byte	0x33
	.4byte	.LLST2
	.byte	0
	.byte	0xc
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
	.byte	0x17
	.byte	0
	.byte	0
	.byte	0xa
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
	.byte	0xb
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
	.byte	0x17
	.byte	0
	.byte	0
	.byte	0xc
	.byte	0xf
	.byte	0
	.byte	0xb
	.byte	0xb
	.byte	0x49
	.byte	0x13
	.byte	0
	.byte	0
	.byte	0
	.section	.debug_loc,"",@progbits
.Ldebug_loc0:
.LLST0:
	.4byte	.LVL0-.Ltext0
	.4byte	.LVL2-.Ltext0
	.2byte	0x1
	.byte	0x5a
	.4byte	.LVL2-.Ltext0
	.4byte	.LFE0-.Ltext0
	.2byte	0x1
	.byte	0x60
	.4byte	0
	.4byte	0
.LLST1:
	.4byte	.LVL0-.Ltext0
	.4byte	.LVL4-.Ltext0
	.2byte	0x1
	.byte	0x5b
	.4byte	.LVL4-.Ltext0
	.4byte	.LFE0-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.byte	0x1
	.byte	0x5b
	.byte	0x9f
	.4byte	0
	.4byte	0
.LLST2:
	.4byte	.LVL0-.Ltext0
	.4byte	.LVL1-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL1-.Ltext0
	.4byte	.LVL2-.Ltext0
	.2byte	0x1
	.byte	0x5a
	.4byte	.LVL2-.Ltext0
	.4byte	.LVL3-.Ltext0
	.2byte	0x1
	.byte	0x60
	.4byte	.LVL3-.Ltext0
	.4byte	.LVL5-.Ltext0
	.2byte	0x1
	.byte	0x5d
	.4byte	.LVL5-.Ltext0
	.4byte	.LFE0-.Ltext0
	.2byte	0x7
	.byte	0xf3
	.byte	0x1
	.byte	0x5b
	.byte	0x7f
	.byte	0
	.byte	0x1e
	.byte	0x9f
	.4byte	0
	.4byte	0
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
.LASF3:
	.string	"long long int"
.LASF4:
	.string	"unsigned char"
.LASF6:
	.string	"long unsigned int"
.LASF5:
	.string	"short unsigned int"
.LASF8:
	.string	"unsigned int"
.LASF9:
	.string	"GNU C17 10.2.0 -march=rv32imac -mabi=ilp32 -g -O2"
.LASF7:
	.string	"long long unsigned int"
.LASF12:
	.string	"int32_t"
.LASF11:
	.string	"/home/eduard/work/tmp/rcvsim/csrc"
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
