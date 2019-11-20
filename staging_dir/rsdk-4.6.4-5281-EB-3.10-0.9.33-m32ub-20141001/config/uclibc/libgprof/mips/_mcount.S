#include <features.h>
#include <sys/asm.h>
#include <asm/unistd.h>
#include <sys/regdef.h>
	.text
	.align      2
	.globl      _mcount
	.ent        _mcount
	.type       _mcount, @function
	.set noat
_mcount:
#ifdef  __PIC__
	.set noreorder
	.cpload t9
	.set reorder
	addi    sp, -32
	.cprestore 16
#else
	addi    sp, -32
#endif
	sw      AT, 28(sp)
	sw      ra, 24(sp)
	sw      a0,  0(sp)
	sw      a1,  4(sp)
	sw      a2,  8(sp)
	sw      a3, 12(sp)
	move    a0, AT
	move    a1, ra
#ifdef __PIC__
	PTR_LA  t9, __mcount_internal
	jalr    t9
#else
	jal     __mcount_internal
#endif
	lw      AT, 28(sp)
	lw      ra, 24(sp)
	lw      a0,  0(sp)
	lw      a1,  4(sp)
	lw      a2,  8(sp)
	lw      a3, 12(sp)
	addi    sp, 40
	.set noreorder
	jr      ra
	move    ra, AT
	.set reorder
	.end _mcount
