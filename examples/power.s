.global _start


.section .rodata
__F10:
        .double     10.0000
C0:
        .string     " "
C1:
        .string     "\n"
C2:
        .word       10
C3:
        .word       1
C4:
        .word       2
C5:
        .word       0
C6:
        .word       3
C7:
        .word       8
C8:
        .word       4
C9:
        .double     1.0000


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
        jal         ra, main
        lw          a0, -4(sp)
        addi        a7, x0, 93
        ecall       
p:
        addi        sp, sp, -36
        sd          ra, 28(sp)
        lw          t0, C3
        sw          t0, 12(sp)
        lw          t0, C3
        sw          t0, 8(sp)
p__1:
        lw          t1, 8(sp)
        lw          t2, 20(sp)
        sgt         t0, t1, t2
        seqz        t0, t0
        sw          t0, 4(sp)
        lw          t0, 4(sp)
        beqz        t0, p__2
        lw          t1, 24(sp)
        lw          t2, 12(sp)
        mulw        t0, t1, t2
        sw          t0, 0(sp)
        lw          t0, 0(sp)
        sw          t0, 12(sp)
        lw          t1, 8(sp)
        lw          t2, C3
        addw        t0, t1, t2
        sw          t0, 8(sp)
        j           p__1
p__2:
        lw          a0, 12(sp)
        ld          ra, 28(sp)
        addi        sp, sp, 36
        ret         
main:
        addi        sp, sp, -32
        sd          ra, 24(sp)
        lw          a0, C6
        sw          a0, -12(sp)
        lw          a0, C4
        sw          a0, -16(sp)
        call        p
        sw          a0, 16(sp)
        lw          a0, 16(sp)
        call        .print_int
        addi        a0, x0, 1
        la          a1, C1
        addi        a2, x0, 1
        addi        a7, x0, 64
        ecall       
        lw          a0, C2
        sw          a0, -12(sp)
        lw          a0, C4
        sw          a0, -16(sp)
        call        p
        sw          a0, 12(sp)
        lw          a0, 12(sp)
        call        .print_int
        addi        a0, x0, 1
        la          a1, C1
        addi        a2, x0, 1
        addi        a7, x0, 64
        ecall       
        lw          a0, C6
        sw          a0, -12(sp)
        lw          a0, C8
        sw          a0, -16(sp)
        call        p
        sw          a0, 8(sp)
        lw          a0, 8(sp)
        call        .print_int
        addi        a0, x0, 1
        la          a1, C1
        addi        a2, x0, 1
        addi        a7, x0, 64
        ecall       
        lw          a0, C4
        sw          a0, -12(sp)
        lw          a0, C7
        sw          a0, -16(sp)
        call        p
        sw          a0, 4(sp)
        lw          a0, 4(sp)
        call        .print_int
        addi        a0, x0, 1
        la          a1, C1
        addi        a2, x0, 1
        addi        a7, x0, 64
        ecall       
        lw          a0, C8
        sw          a0, -12(sp)
        lw          a0, C6
        sw          a0, -16(sp)
        call        p
        sw          a0, 0(sp)
        lw          a0, 0(sp)
        call        .print_int
        addi        a0, x0, 1
        la          a1, C1
        addi        a2, x0, 1
        addi        a7, x0, 64
        ecall       
        lw          a0, C5
        ld          ra, 24(sp)
        addi        sp, sp, 32
        ret         

