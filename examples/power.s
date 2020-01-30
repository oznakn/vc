.global _start

.section .rodata
__F10:
        .double 10.0000
C0:
        .string " "
C1:
        .string "\n"
C2:
        .word 1
C3:
        .word 3
C4:
        .word 2
C5:
        .word 0
C6:
        .double 1.0000
C7:
        .double 1.0000

.section .text

.print_int:
        mv          t0, a0

        addi        sp, sp, -58
        sd          ra, 50(sp)

        beqz        a0, .print_int__3

        addi        s2, x0, 10
        addi        t1, x0, 0

.print_int__1:
        beqz        t0, .print_int__2

        rem         t2, t0, s2
        add         t3, t1, sp
        sb          t2, 0(t3)
        addi        t1, t1, 1
        div         t0, t0, s2

        j .print_int__1

.print_int__2:
        addi        t1, t1, -1
        add         t3, t1, sp
        lb          t4, 0(t3)
        addi        t4, t4, 48
        sb          t4, 0(t3)
        addi        a0, x0, 1
        mv          a1, t3
        addi        a2, x0, 1
        addi        a7, x0, 64
        ecall

        beqz        t1, .print_int__4

        j .print_int__2

.print_int__3:
        addi        t4, x0, 48
        sb          t4, 0(sp)

        addi        a0, x0, 1
        mv          a1, sp
        addi        a2, x0, 1
        addi        a7, x0, 64
        ecall

.print_int__4:
        addi        sp, sp, 58
        ret


.print_real:
        fld         fa1, __F10 ,a1
        addi        t1, x0, 6

.print_real__loop:
        fmul.d      fa0, fa0, fa1
        addi        t1,t1,-1
        bnez        t1, .print_real__loop

        fcvt.w.d    a0, fa0
        mv          t0, a0

        addi        sp, sp, -58
        sd          ra, 50(sp)

        beqz        a0, .print_real__3

        addi        s2, x0, 10
        addi        t1, x0, 0

.print_real__1:
        beqz        t0, .print_real__2

        rem         t2, t0, s2
        add         t3, t1, sp
        sb          t2, 0(t3)
        addi        t1, t1, 1
        div         t0, t0, s2

        j .print_real__1

.print_real__2:
        addi        t1, t1, -1
        add         t3, t1, sp

        lb          t4, 0(t3)
        addi        t4, t4, 48
        sb          t4, 0(t3)

        addi        a0, x0, 1
        mv          a1, t3
        addi        a2, x0, 1
        addi        a7, x0, 64
        ecall

        addi        t5, x0, 6
        subw        t5, t5, t1
        bnez        t5, .print_real_temp

        addi        t4, x0, 46
        sb          t4, 0(sp)

        addi        a0, x0, 1
        mv          a1, sp
        addi        a2, x0, 1
        addi        a7, x0, 64
        ecall

.print_real_temp:
        addi        t5, x0, 2
        subw        t5, t5, t1
        beqz        t5, .print_real__4

        j .print_real__2

.print_real__3:
        addi        t4, x0, 48
        sb          t4, 0(sp)

        addi        a0, x0, 1
        mv          a1, sp
        addi        a2, x0, 1
        addi        a7, x0, 64
        ecall

        addi        t4, x0, 46
        sb          t4, 0(sp)
        mv          a1, sp
        ecall

        addi        t4, x0, 48
        sb          t4, 0(sp)
        mv          a1, sp
        ecall

.print_real__4:
        addi        sp, sp, 58
        ret


_start:
        jal       main
        lw        a0, -4(sp)
        addi      a7, x0, 93
        ecall     

p:
        addi      sp, sp, -56
        sd        ra, 48(sp)
        fld       ft2, C6, s0
        fsd       ft2, 24(sp)
        lw        t2, C2
        sw        t2, 20(sp)

p__0:
        lw        t3, 20(sp)
        lw        t4, 40(sp)
        sgt       t2, t3, t4
        seqz      t2, t2
        sw        t2, 16(sp)
        lw        t2, 16(sp)
        beqz      t2, p__1
        lw        t4, 44(sp)
        fcvt.d.w  ft2, t4
        fsd       ft2, 8(sp)
        fld       ft4, 8(sp)
        fld       ft5, 24(sp)
        fmul.d    ft3, ft4, ft5
        fsd       ft3, 0(sp)
        fld       ft3, 0(sp)
        fsd       ft3, 24(sp)
        fld       fa0, 24(sp)
        call      .print_real
        addi      a0, x0, 1
        la        a1, C1
        addi      a2, x0, 2
        addi      a7, x0, 64
        ecall     
        lw        t5, 20(sp)
        lw        t6, C2
        add       t3, t5, t6
        sw        t3, 20(sp)
        j         p__0

p__1:
        fld       fa0, 24(sp)
        ld        ra, 48(sp)
        addi      sp, sp, 56
        ret       

main:
        addi      sp, sp, -20
        sd        ra, 12(sp)
        lw        t3, C3
        sw        t3, -12(sp)
        lw        t3, C4
        sw        t3, -16(sp)
        call      p
        fsd       fa0, 0(sp)
        fld       fa0, 0(sp)
        call      .print_real
        addi      a0, x0, 1
        la        a1, C1
        addi      a2, x0, 2
        addi      a7, x0, 64
        ecall     
        lw        a0, C5
        ld        ra, 12(sp)
        addi      sp, sp, 20
        ret       
