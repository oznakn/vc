.global _start

.section .rodata
__F10:
        .double 10.0000
C0:
        .string " "
C1:
        .string "\n"
C2:
        .word 2
C3:
        .word 4
C4:
        .word 12
C5:
        .word 10
C6:
        .word 3
C7:
        .word 230
C8:
        .word 0
C9:
        .word 1
C10:
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
        jal    main
        lw     a0, -4(sp)
        addi   a7, x0, 93
        ecall  

add:
        addi   sp, sp, -24
        sd     ra, 16(sp)
        lw     t4, 12(sp)
        lw     t5, 8(sp)
        add    t3, t4, t5
        sw     t3, 0(sp)
        lw     a0, 0(sp)
        ld     ra, 16(sp)
        addi   sp, sp, 24
        ret    

sub:
        addi   sp, sp, -24
        sd     ra, 16(sp)
        lw     t5, 12(sp)
        lw     t4, 8(sp)
        sub    t2, t5, t4
        sw     t2, 0(sp)
        lw     a0, 0(sp)
        ld     ra, 16(sp)
        addi   sp, sp, 24
        ret    

mul:
        addi   sp, sp, -24
        sd     ra, 16(sp)
        lw     t4, 12(sp)
        lw     t5, 8(sp)
        mul    t3, t4, t5
        sw     t3, 0(sp)
        lw     a0, 0(sp)
        ld     ra, 16(sp)
        addi   sp, sp, 24
        ret    

main:
        addi   sp, sp, -24
        sd     ra, 16(sp)
        lw     t3, C2
        sw     t3, -12(sp)
        lw     t3, C3
        sw     t3, -16(sp)
        call   add
        sw     a0, 8(sp)
        lw     a0, 8(sp)
        call   .print_int
        addi   a0, x0, 1
        la     a1, C1
        addi   a2, x0, 2
        addi   a7, x0, 64
        ecall  
        lw     a0, C4
        sw     a0, -12(sp)
        lw     a0, C5
        sw     a0, -16(sp)
        call   sub
        sw     a0, 4(sp)
        lw     a0, 4(sp)
        call   .print_int
        addi   a0, x0, 1
        la     a1, C1
        addi   a2, x0, 2
        addi   a7, x0, 64
        ecall  
        lw     a0, C6
        sw     a0, -12(sp)
        lw     a0, C7
        sw     a0, -16(sp)
        call   mul
        sw     a0, 0(sp)
        lw     a0, 0(sp)
        call   .print_int
        addi   a0, x0, 1
        la     a1, C1
        addi   a2, x0, 2
        addi   a7, x0, 64
        ecall  
        lw     a0, C8
        ld     ra, 16(sp)
        addi   sp, sp, 24
        ret    
