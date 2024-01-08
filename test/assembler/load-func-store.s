	.file	"load-func-store.c"
	.option nopic
	.attribute arch, "rv32i2p0_m2p0_a2p0_c2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
.Ltext0:
	.cfi_sections	.debug_frame
	.align	1
	.globl	load_func_store
	.type	load_func_store, @function
load_func_store:
.LFB0:
	.file 1 "load-func-store.c"
	.loc 1 5 27
	.cfi_startproc
	.loc 1 6 3
	.loc 1 7 3
.LVL0:
	.loc 1 8 3
	.loc 1 9 3
	.loc 1 7 5 is_stmt 0
	lui	a5,%hi(buffer)
	addi	a5,a5,%lo(buffer)
	.loc 1 9 5
	lw	a3,4(a5)
	lw	a4,0(a5)
	.loc 1 16 1
	li	a0,0
	.loc 1 9 5
	add	a4,a4,a3
.LVL1:
	.loc 1 10 3 is_stmt 1
	.loc 1 10 5 is_stmt 0
	slli	a3,a4,4
	sub	a3,a3,a4
.LVL2:
	.loc 1 11 3 is_stmt 1
	.loc 1 12 3
	.loc 1 11 5 is_stmt 0
	srai	a2,a3,2
.LVL3:
	.loc 1 12 13
	sw	a4,8(a5)
	.loc 1 13 3 is_stmt 1
	.loc 1 13 13 is_stmt 0
	sw	a3,12(a5)
	.loc 1 14 3 is_stmt 1
	.loc 1 14 13 is_stmt 0
	sw	a2,16(a5)
	.loc 1 15 3 is_stmt 1
	.loc 1 16 1 is_stmt 0
	ret
	.cfi_endproc
.LFE0:
	.size	load_func_store, .-load_func_store
.Letext0:
	.file 2 "/usr/lib/gcc/riscv64-unknown-elf/10.2.0/include/stdint-gcc.h"
	.section	.debug_info,"",@progbits
.Ldebug_info0:
	.4byte	0xdc
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
	.4byte	0x82
	.byte	0x6
	.byte	0
	.byte	0x7
	.4byte	.LASF13
	.byte	0x1
	.byte	0x3
	.byte	0x10
	.4byte	0x77
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
	.byte	0x9
	.string	"a"
	.byte	0x1
	.byte	0x6
	.byte	0xb
	.4byte	0x33
	.byte	0x9
	.string	"b"
	.byte	0x1
	.byte	0x6
	.byte	0xd
	.4byte	0x33
	.byte	0xa
	.string	"c"
	.byte	0x1
	.byte	0x6
	.byte	0xf
	.4byte	0x33
	.byte	0x1
	.byte	0x5e
	.byte	0xa
	.string	"d"
	.byte	0x1
	.byte	0x6
	.byte	0x11
	.4byte	0x33
	.byte	0x1
	.byte	0x5d
	.byte	0xb
	.string	"e"
	.byte	0x1
	.byte	0x6
	.byte	0x13
	.4byte	0x33
	.4byte	.LLST0
	.byte	0
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
	.byte	0x3c
	.byte	0x19
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
	.byte	0
	.byte	0
	.byte	0x9
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
	.byte	0
	.section	.debug_loc,"",@progbits
.Ldebug_loc0:
.LLST0:
	.4byte	.LVL2-.Ltext0
	.4byte	.LVL3-.Ltext0
	.2byte	0x5
	.byte	0x7d
	.byte	0
	.byte	0x32
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL3-.Ltext0
	.4byte	.LFE0-.Ltext0
	.2byte	0x1
	.byte	0x5c
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
.LASF14:
	.string	"load_func_store"
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
.LASF2:
	.string	"long int"
.LASF0:
	.string	"signed char"
.LASF10:
	.string	"load-func-store.c"
	.ident	"GCC: () 10.2.0"
