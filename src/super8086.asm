;---------------------------------------------------------
; super8086
; scott hutter
;---------------------------------------------------------

.cpu "65816"
.include "macros.asm"

*=$0801

BANK = $020000

    .word (+), 2005
    .null $9e, format("%d",start)
 +  .word 0

AX  = $42   ; the accumulator reg
AL  = $42   
AH  = $43
BX  = $44   ; the base addr reg
BL  = $44   
BH  = $45 
CX  = $46   ; the count reg
CL  = $46
CH  = $47
DX  = $48   ; the data reg
DL  = $48
DH  = $49

; segment registers
CS  = $4a   ; code segment reg
DS  = $4c   ; data segment reg
ES  = $4e   ; extra segment reg
SS  = $50   ; stack segment reg

IP  = $52   ; instruction pointer
FR  = $54   ; flags register
FRH = $54
FRL = $55

;index registers
SP  = $56   ; stack pointer reg
BP  = $58   ; base pointer reg
SI  = $5a   ; source index reg
DI  = $5c   ; destination index reg

;temp dp locations
TMP1 = $5e

JUMPTBL:
    .word j00, j01, j02, j03, j04, j05, j06, j07, j08, j09, j0a, j0b, j0c, j0d, j0e, j0f
    .word j10, j11, j12, j13, j14, j15, j16, j17, j18, j19, j1a, j1b, j1c, j1d, j1e, j1f
    .word j20, j21, j22, j23, j24, j25, j26, j27, j28, j29, j2a, j2b, j2c, j2d, j2e, j2f
    .word j30, j31, j32, j33, j34, j35, j36, j37, j38, j39, j3a, j3b, j3c, j3d, j3e, j3f
    .word j40, j41, j42, j43, j44, j45, j46, j47, j48, j49, j4a, j4b, j4c, j4d, j4e, j4f
    .word j50, j51, j52, j53, j54, j55, j56, j57, j58, j59, j5a, j5b, j5c, j5d, j5e, j5f
    .word j60, j61, j62, j63, j64, j65, j66, j67, j68, j69, j6a, j6b, j6c, j6d, j6e, j6f
    .word j70, j71, j72, j73, j74, j75, j76, j77, j78, j79, j7a, j7b, j7c, j7d, j7e, j7f
    .word j80, j81, j82, j83, j84, j85, j86, j87, j88, j89, j8a, j8b, j8c, j8d, j8e, j8f
    .word j90, j91, j92, j93, j94, j95, j96, j97, j98, j99, j9a, j9b, j9c, j9d, j9e, j9f
    .word ja0, ja1, ja2, ja3, ja4, ja5, ja6, ja7, ja8, ja9, jaa, jab, jac, jad, jae, jaf
    .word jb0, jb1, jb2, jb3, jb4, jb5, jb6, jb7, jb8, jb9, jba, jbb, jbc, jbd, jbe, jbf
    .word jc0, jc1, jc2, jc3, jc4, jc5, jc6, jc7, jc8, jc9, jca, jcb, jcc, jcd, jce, jcf
    .word jd0, jd1, jd2, jd3, jd4, jd5, jd6, jd7, jd8, jd9, jda, jdb, jdc, jdd, jde, jdf
    .word je0, je1, je2, je3, je4, je5, je6, je7, je8, je9, jea, jeb, jec, jed, jee, jef
    .word jf0, jf1, jf2, jf3, jf4, jf5, jf6, jf7, jf8, jf9, jfa, jfb, jfc, jfd, jfe, jff

JUMPADDR:
    .word $0000

start:    
    lda #<msg1      ; print the startup message
    sta $fe 
    lda #>msg1
    sta $ff
    jsr print

    ; initialize the cpu
    .native
    
    ; init registers
    .rega16
    stz AX
    stz BX
    stz CX
    stz DX
    stz CS
    stz DS
    stz ES
    stz SS
    stz FR

    ; set instruction pointer to $020100
    lda #$0100
    sta IP
 
    ; set stack to $FFFF
    lda #$FFFE
    sta SP
    .regaxy8

    ; push test prg
    ldy #$00
LOAD:
    lda X86PRG,y
    .setdatabank $02
    sta (IP),y
    iny
    cpy #$2a
    beq MAINLOOP
    .setdatabank $00
    jmp LOAD

X86PRG:
    .byte $b0, $01
    .byte $b3, $00
    .byte $00, $d8
    .byte $cd, $20
    .text "hello world$"
    .byte $cd, $20      ; int 21
    .byte $fe, $c2      ; inc DL
    .byte $e2, $f8      ; loop 0105
    
    .byte $cd, $20      ; int 20

    ; main opcode loop
MAINLOOP:
    lda #$00
    xba
    jsr FETCH_IP
    .setdatabank $00
    .regaxy16
    asl
    tay
    lda JUMPTBL,y
    sta JUMPADDR
    .regaxy8
    jmp (JUMPADDR)
MAINLOOP_END:
    jsr INC_IP
    jmp MAINLOOP

EXIT:
    .regaxy8
    .setdatabank $00
    lda #<txt_ax      ; print the startup message
    sta $fe 
    lda #>txt_ax
    sta $ff
    jsr print
    lda AH
    jsr prhex
    lda AL
    jsr prhex
    lda #' '
    jsr $FFD2
    lda #<txt_bx      ; print the startup message
    sta $fe 
    lda #>txt_bx
    sta $ff
    jsr print
    lda BH 
    jsr prhex
    lda BL
    jsr prhex
    lda #' '
    jsr $FFD2
    lda #<txt_cx      ; print the startup message
    sta $fe 
    lda #>txt_cx
    sta $ff
    jsr print
    lda CH 
    jsr prhex
    lda CL
    jsr prhex
    lda #' '
    jsr $FFD2
    lda #<txt_dx      ; print the startup message
    sta $fe 
    lda #>txt_dx
    sta $ff
    jsr print
    lda DH 
    jsr prhex
    lda DL
    jsr prhex
    
    lda #<txt_sp      ; print the startup message
    sta $fe 
    lda #>txt_sp
    sta $ff
    jsr print
    lda SP
    jsr prhex
    lda SP+1
    jsr prhex
    lda #' '
    jsr $FFD2
    lda #<txt_bp      ; print the startup message
    sta $fe 
    lda #>txt_bp
    sta $ff
    jsr print
    lda BP
    jsr prhex
    lda BP+2
    jsr prhex
    lda #' '
    jsr $FFD2
    lda #<txt_si      ; print the startup message
    sta $fe 
    lda #>txt_si
    sta $ff
    jsr print
    lda SI
    jsr prhex
    lda SI+2
    jsr prhex
    lda #' '
    jsr $FFD2
    lda #<txt_di      ; print the startup message
    sta $fe 
    lda #>txt_di
    sta $ff
    jsr print
    lda DI
    jsr prhex
    lda DI+2
    jsr prhex

    lda #<txt_ds      ; print the startup message
    sta $fe 
    lda #>txt_ds
    sta $ff
    jsr print
    lda DS
    jsr prhex
    lda DS+1
    jsr prhex
    lda #' '
    jsr $FFD2
    lda #<txt_es      ; print the startup message
    sta $fe 
    lda #>txt_es
    sta $ff
    jsr print
    lda ES
    jsr prhex
    lda ES+2
    jsr prhex
    lda #' '
    jsr $FFD2
    lda #<txt_ss      ; print the startup message
    sta $fe 
    lda #>txt_ss
    sta $ff
    jsr print
    lda SS
    jsr prhex
    lda SS+2
    jsr prhex
    lda #' '
    jsr $FFD2
    lda #<txt_cs      ; print the startup message
    sta $fe 
    lda #>txt_cs
    sta $ff
    jsr print
    lda CS
    jsr prhex
    lda CS+2
    jsr prhex

    lda #<txt_ip      ; print the startup message
    sta $fe 
    lda #>txt_ip
    sta $ff
    jsr print
    lda IP+1
    jsr prhex
    lda IP
    jsr prhex
    lda #' '
    jsr $FFD2
    lda #<txt_fr      ; print the startup message
    sta $fe 
    lda #>txt_fr
    sta $ff
    jsr print
    lda FRH
    jsr prhex
    lda FRL
    jsr prhex
fl_ovnv
    lda FRH
    and #$08
    beq +
    lda #<txt_fr_ov      ; print the startup message
    sta $fe 
    lda #>txt_fr_ov
    sta $ff
    jsr print
    jmp fl_updn
+   lda #<txt_fr_nv      ; print the startup message
    sta $fe 
    lda #>txt_fr_nv
    sta $ff
    jsr print 
fl_updn  
    lda FRH
    and #$04
    beq +
    lda #<txt_fr_dn      ; print the startup message
    sta $fe 
    lda #>txt_fr_dn
    sta $ff
    jsr print
    jmp fl_eidi
+   lda #<txt_fr_up      ; print the startup message
    sta $fe 
    lda #>txt_fr_up
    sta $ff
    jsr print   
fl_eidi 
    lda FRH
    and #$02
    beq +
    lda #<txt_fr_ei      ; print the startup message
    sta $fe 
    lda #>txt_fr_ei
    sta $ff
    jsr print
    jmp fl_plng
+   lda #<txt_fr_di      ; print the startup message
    sta $fe 
    lda #>txt_fr_di
    sta $ff
    jsr print  
fl_plng
    lda FRL
    and #$80
    beq +
    lda #<txt_fr_ng      ; print the startup message
    sta $fe 
    lda #>txt_fr_ng
    sta $ff
    jsr print
    jmp fl_zrnz
+   lda #<txt_fr_pl      ; print the startup message
    sta $fe 
    lda #>txt_fr_pl
    sta $ff
    jsr print  
fl_zrnz
    lda FRL
    and #$40
    beq +
    lda #<txt_fr_zr      ; print the startup message
    sta $fe 
    lda #>txt_fr_zr
    sta $ff
    jsr print
    jmp fl_acna
+   lda #<txt_fr_nz      ; print the startup message
    sta $fe 
    lda #>txt_fr_nz
    sta $ff
    jsr print 
fl_acna
    lda FRL
    and #$10
    beq +
    lda #<txt_fr_ac      ; print the startup message
    sta $fe 
    lda #>txt_fr_ac
    sta $ff
    jsr print
    jmp fl_pepo
+   lda #<txt_fr_na      ; print the startup message
    sta $fe 
    lda #>txt_fr_na
    sta $ff
    jsr print 
fl_pepo
    lda FRL
    and #$04
    beq +
    lda #<txt_fr_pe      ; print the startup message
    sta $fe 
    lda #>txt_fr_pe
    sta $ff
    jsr print
    jmp fl_cync
+   lda #<txt_fr_po      ; print the startup message
    sta $fe 
    lda #>txt_fr_po
    sta $ff
    jsr print 
fl_cync
    lda FRL
    and #$01
    beq +
    lda #<txt_fr_cy      ; print the startup message
    sta $fe 
    lda #>txt_fr_cy
    sta $ff
    jsr print
    jmp fl_done
+   lda #<txt_fr_nc      ; print the startup message
    sta $fe 
    lda #>txt_fr_nc
    sta $ff
    jsr print 
fl_done
    .emulation
    rts

GETNEXT:
    jsr INC_IP
    jsr FETCH_IP
    rts

INC_IP:
    .setdatabank $00
    .rega16
    lda IP
    clc
    adc #$01
    sta IP      ; increment IP
    .rega8
    rts

FETCH_IP:
    .setdatabank $02
    lda (IP)    ; get next value
    rts


    ; common printing routine
print:
    ldy #$00
nextch:
    lda ($fe),y
    beq +
    jsr $FFD2
    iny
    jmp nextch
+   rts

; output two hex digits for byte
prhex   
        .setdatabank $00
        phx                 ; save x
        jsr asctwo          ; get hex chars for byte in x (lower) and a (upper)
        jsr $ffd2          ; output upper nybble
        txa                 ; transfer lower to a
        plx                 ; restore x
        jsr $ffd2          ; output lower nybble
        rts

; -----------------------------------------------------------------------------
; convert byte in a to hex digits
asctwo  pha                 ; save byte
        jsr ascii           ; do low nybble
        tax                 ; save in x
        pla                 ; restore byte
        lsr                 ; shift upper nybble down
        lsr  
        lsr  
        lsr 

; convert low nybble in A to hex digit
ascii   and #$0f            ; clear upper nibble
        cmp #$0a            ; if less than a, skip next step
        bcc asc1
        adc #6              ; skip ascii chars between 9 and a
asc1    adc #$30            ; add ascii char 0 to value
        rts

msg1:
    .text $93,$0e,"Super8086 Emulator",$0d,$00

txt_ax:
    .text $0d,$0d,"ax=",$00
txt_bx:
    .text "bx=",$00
txt_cx
    .text "cx=",$00
txt_dx
    .text "dx=",$00
txt_sp:
    .text $0d,"sp=",$00
txt_bp:
    .text "bp=",$00
txt_si
    .text "si=",$00
txt_di
    .text "di=",$00
txt_ds:
    .text $0d,"ds=",$00
txt_es:
    .text "es=",$00
txt_ss
    .text "ss=",$00
txt_cs
    .text "cs=",$00
txt_ip:
    .text $0d,"ip=",$00
txt_fr:
    .text "flags=",$00

txt_fr_ov:
    .text $0d,"OV ",$00
txt_fr_nv:
    .text $0d,"NV ",$00

txt_fr_up:
    .text "UP ",$00
txt_fr_dn:
    .text "DN ",$00

txt_fr_ei:
    .text "EI ",$00
txt_fr_di:
    .text "DI ",$00

txt_fr_ng:
    .text "NG ",$00
txt_fr_pl:
    .text "PL ",$00

txt_fr_zr:
    .text "ZR ",$00
txt_fr_nz:
    .text "NZ ",$00

txt_fr_ac:
    .text "AC ",$00
txt_fr_na:
    .text "NA ",$00

txt_fr_pe:
    .text "PE ",$00
txt_fr_po:
    .text "PO ",$00

txt_fr_cy:
    .text "CY ",$00
txt_fr_nc:
    .text "NC ",$00



; -------------------------------------------------------
; flag sets and clears
; -------------------------------------------------------
cflag:
    php
    bcc +
    lda FRL
    ora #$01
    sta FRL
    plp
    rts
+   lda FRL
    and #$fe
    sta FRL
    plp
    rts

zflag:
    php
    bne +
    lda FRL
    ora #$40
    sta FRL
    plp
    rts
+   lda FRL
    and #$bf
    sta FRL
    plp
    rts

sflag:
    php
    cmp #$80
    bcc +
    lda FRL
    and #$7f
    sta FRL
    plp
    rts
+   lda FRL
    ora #$80
    sta FRL
    plp
    rts

pflag:
    php
pflag_loop2:
    lsr
    beq pflag_done2
    bcc pflag_loop2
    eor #$01
    jmp pflag_loop2
pflag_done2:
    bcs +
    lda FRL             ; get the 8086 register flag
    ora #$04            ; set bit 4
    sta FRL             ; store it
    plp
    rts
+   lda FRL             ; its an off number. get the 8086 register flag
    and #$fb            ; clear bit 4
    sta FRL             ; store it
    plp
    rts

pflag2:
    php                 ; save status reg
    tax                 ; accumulator holds value of last calculation
    stz pflag_tmp       ; clear the temp value holder
    txa
    pha                 ; save accumulator for exit
    ldy #$08            ; we are going to loop through each bit
pflag_loop:
    clc                 ; clear the carry
    rol                 ; roll the bits left, putting leftmost in carry flg
    bcc +               ; if carry is clear, this bit should be skipped
    inc pflag_tmp       ; otherwise, temp value = temp value + 1
+   dey                 ; count down
    beq pflag_clrset    ; if zero, move on
    jmp pflag_loop      ; loop and roll next bit
pflag_clrset:
    lda pflag_tmp       ; get the temp value (number of bits with 1's in them)
    and #$01            ; is the 0th bit = 1?  (if so, its an odd number)
    bne pflag_clr       ; its an even number - skip ahead
    lda FRL             ; get the 8086 register flag
    ora #$04            ; set bit 4
    sta FRL             ; store it
    jmp pflag_done      ; we are done
pflag_clr:
    lda FRL             ; its an off number. get the 8086 register flag
    and #$fb            ; clear bit 4
    sta FRL             ; store it
pflag_done:
    pla                 ; get our original A value
    plp                 ; get original status reg
    rts                 ; done
pflag_tmp:
.byte $00



; -------------------------------------------------------
; opcode operations
; -------------------------------------------------------
j00:                    ; add reg, reg
    jsr GETNEXT
    .setdatabank $00
    cmp #$c0            ; add al,al
    bne +
    lda AL
    clc
    adc AL
    sta AL
    jsr cflag
    jmp MAINLOOP_END
+   cmp #$d8            ; add al, bl
    bne +
    lda BL
    clc
    adc AL
    sta AL
    jsr pflag
    jsr cflag
    jsr sflag
    jsr zflag
    jmp MAINLOOP_END
+   cmp #$c8
    bne +
    lda CL
    clc
    adc AL
    sta AL
    jsr cflag
    jmp MAINLOOP_END
+   cmp #$d0
    bne +
    lda DL
    clc
    adc AL
    sta AL
    jsr cflag
    jmp MAINLOOP_END
+   cmp #$c4
    bne +
    lda AL
    clc
    adc AH
    sta AH
    jsr cflag
    jmp MAINLOOP_END
+   cmp #$dc
    bne +
    lda BL
    clc
    adc AH
    sta AH
    jsr cflag
    jmp MAINLOOP_END
+   cmp #$cc
    bne +
    lda CL
    clc
    adc AH
    sta AH
    jsr cflag
    jmp MAINLOOP_END
+   cmp #$d4
    bne +
    lda DL
    clc
    adc AH
    sta AH
    jsr cflag
+   jmp MAINLOOP_END
    
j01:
    jmp MAINLOOP_END
j02:
    jmp MAINLOOP_END
j03:
    jmp MAINLOOP_END
j04:
    jmp MAINLOOP_END
j05:
    jmp MAINLOOP_END
j06:                    ; push es
    .setdatabank $00
    .rega16
    lda SP
    sec
    sbc #$02
    sta SP
    lda ES
    .setdatabank16 $02
    sta (SP)
    .rega8
    jmp MAINLOOP_END
j07:                    ; pop es
    .rega16
    .setdatabank16 $02
    lda (SP)
    .setdatabank16 $00
    sta ES
    lda SP
    clc
    adc #$02
    sta SP
    .rega8
    jmp MAINLOOP_END
j08:
    jmp MAINLOOP_END
j09:
    jmp MAINLOOP_END
j0a:
    jmp MAINLOOP_END
j0b:
    jmp MAINLOOP_END
j0c:
    jmp MAINLOOP_END
j0d:
    jmp MAINLOOP_END
j0e:                    ; push cs
    .setdatabank $00
    .rega16
    lda SP
    sec
    sbc #$02
    sta SP
    lda CS
    .setdatabank16 $02
    sta (SP)
    .rega8
    jmp MAINLOOP_END
j0f:                    ; pop cs
    .rega16
    .setdatabank16 $02
    lda (SP)
    .setdatabank16 $00
    sta CS
    lda SP
    clc
    adc #$02
    sta SP
    .rega8
    jmp MAINLOOP_END
j10:
    jmp MAINLOOP_END
j11:
    jmp MAINLOOP_END
j12:
    jmp MAINLOOP_END
j13:
    jmp MAINLOOP_END
j14:
    jmp MAINLOOP_END
j15:
    jmp MAINLOOP_END
j16:                    ; push ss
    .setdatabank $00
    .rega16
    lda SP
    sec
    sbc #$02
    sta SP
    lda SS
    .setdatabank16 $02
    sta (SP)
    .rega8
    jmp MAINLOOP_END
j17:                    ; pop ss
    .rega16
    .setdatabank16 $02
    lda (SP)
    .setdatabank16 $00
    sta SS
    lda SP
    clc
    adc #$02
    sta SP
    .rega8
    jmp MAINLOOP_END
j18:
    jmp MAINLOOP_END
j19:
    jmp MAINLOOP_END
j1a:
    jmp MAINLOOP_END
j1b:
    jmp MAINLOOP_END
j1c:
    jmp MAINLOOP_END
j1d:
    jmp MAINLOOP_END
j1e:                    ; push es
    .setdatabank $00
    .rega16
    lda SP
    sec
    sbc #$02
    sta SP
    lda ES
    .setdatabank16 $02
    sta (SP)
    .rega8
    jmp MAINLOOP_END
j1f:                    ; pop ds
    .rega16
    .setdatabank16 $02
    lda (SP)
    .setdatabank16 $00
    sta DS
    lda SP
    clc
    adc #$02
    sta SP
    .rega8
    jmp MAINLOOP_END
j20:
    jmp MAINLOOP_END
j21:
    jsr GETNEXT
    .setdatabank $00
    cmp #$c0
    beq j21_ax_ax
    cmp #$d8
    beq j21_ax_bx
    cmp #$d8
    beq j21_ax_cx
    cmp #$d8
    beq j21_ax_dx
    cmp #$d8
    beq j21_bx_ax
    cmp #$d8
    beq j21_bx_bx
    cmp #$d8
    beq j21_bx_cx
    cmp #$d8
    beq j21_bx_dx
    jmp j21_cmp2

    j21_ax_ax:
        .rega16
        lda AX
        and AX
        sta AX
        .rega8
        jmp MAINLOOP_END
    j21_ax_bx:
        .rega16
        lda BX
        and AX
        sta AX
        .rega8
        jmp MAINLOOP_END
    j21_ax_cx:
        .rega16
        lda CX
        and AX
        sta AX
        .rega8
        jmp MAINLOOP_END
    j21_ax_dx:
        .rega16
        lda DX
        and AX
        sta AX
        .rega8
        jmp MAINLOOP_END
    j21_bx_ax:
        .rega16
        lda AX
        and BX
        sta BX
        .rega8
        jmp MAINLOOP_END
    j21_bx_bx:
        .rega16
        lda BX
        and BX
        sta BX
        .rega8
        jmp MAINLOOP_END
    j21_bx_cx:
        .rega16
        lda CX
        and BX
        sta BX
        .rega8
        jmp MAINLOOP_END
    j21_bx_dx:
        .rega16
        lda DX
        and BX
        sta BX
        .rega8
        jmp MAINLOOP_END

    j21_cmp2:
        cmp #$d8
        beq j21_cx_ax
        cmp #$d8
        beq j21_cx_bx
        cmp #$d8
        beq j21_cx_cx
        cmp #$d8
        beq j21_cx_dx
        cmp #$d8
        beq j21_dx_ax
        cmp #$d8
        beq j21_dx_bx
        cmp #$d8
        beq j21_dx_cx
        cmp #$d8
        beq j21_dx_dx
        jmp MAINLOOP_END

    j21_cx_ax:
        .rega16
        lda AX
        and CX
        sta CX
        .rega8
        jmp MAINLOOP_END
    j21_cx_bx:
        .rega16
        lda BX
        and CX
        sta CX
        .rega8
        jmp MAINLOOP_END
    j21_cx_cx:
        .rega16
        lda CX
        and CX
        sta CX
        .rega8
        jmp MAINLOOP_END
    j21_cx_dx:
        .rega16
        lda DX
        and CX
        sta CX
        .rega8
        jmp MAINLOOP_END
    j21_dx_ax:
        .rega16
        lda AX
        and DX
        sta DX
        .rega8
        jmp MAINLOOP_END
    j21_dx_bx:
        .rega16
        lda BX
        and DX
        sta DX
        .rega8
        jmp MAINLOOP_END
    j21_dx_cx:
        .rega16
        lda CX
        and DX
        sta DX
        .rega8
        jmp MAINLOOP_END
    j21_dx_dx:
        .rega16
        lda DX
        and DX
        sta DX
        .rega8
        jmp MAINLOOP_END

j22:
    jmp MAINLOOP_END
j23:
    jmp MAINLOOP_END
j24:
    jmp MAINLOOP_END
j25:
    jmp MAINLOOP_END
j26:
    jmp MAINLOOP_END
j27:
    jmp MAINLOOP_END
j28:
    jmp MAINLOOP_END
j29:
    jmp MAINLOOP_END
j2a:
    jmp MAINLOOP_END
j2b:
    jmp MAINLOOP_END
j2c:
    jmp MAINLOOP_END
j2d:
    jmp MAINLOOP_END
j2e:
    jmp MAINLOOP_END
j2f:
    jmp MAINLOOP_END
j30:
    jmp MAINLOOP_END
j31:
    jmp MAINLOOP_END
j32:
    jmp MAINLOOP_END
j33:
    jmp MAINLOOP_END
j34:
    jmp MAINLOOP_END
j35:
    jmp MAINLOOP_END
j36:
    jmp MAINLOOP_END
j37:
    jmp MAINLOOP_END
j38:
    jmp MAINLOOP_END
j39:
    jmp MAINLOOP_END
j3a:
    jmp MAINLOOP_END
j3b:
    jmp MAINLOOP_END
j3c:
    jmp MAINLOOP_END
j3d:
    jmp MAINLOOP_END
j3e:
    jmp MAINLOOP_END
j3f:
    jmp MAINLOOP_END
j40:
    .setdatabank $00
    .rega16
    inc AX
    .rega8
    jmp MAINLOOP_END
j41:
    .setdatabank $00
    .rega16
    inc CX
    .rega8
    jmp MAINLOOP_END
j42:
    .setdatabank $00
    .rega16
    inc DX
    .rega8
    jmp MAINLOOP_END
j43:
    .setdatabank $00
    .rega16
    inc BX
    .rega8
    jmp MAINLOOP_END
j44:
    jmp MAINLOOP_END
j45:
    jmp MAINLOOP_END
j46:
    jmp MAINLOOP_END
j47:
    jmp MAINLOOP_END
j48:
    jmp MAINLOOP_END
j49:
    jmp MAINLOOP_END
j4a:
    jmp MAINLOOP_END
j4b:
    jmp MAINLOOP_END
j4c:
    jmp MAINLOOP_END
j4d:
    jmp MAINLOOP_END
j4e:
    jmp MAINLOOP_END
j4f:
    jmp MAINLOOP_END
j50:                    ; push ax
    .setdatabank $00
    .rega16
    lda SP
    sec
    sbc #$02
    sta SP
    lda AX
    .setdatabank16 $02
    sta (SP)
    .rega8
    jmp MAINLOOP_END
j51:                    ; push cx
    .setdatabank $00
    .rega16
    lda SP
    sec
    sbc #$02
    sta SP
    lda CX
    .setdatabank16 $02
    sta (SP)
    .rega8
    jmp MAINLOOP_END
j52:                    ; push dx
    .setdatabank $00
    .rega16
    lda SP
    sec
    sbc #$02
    sta SP
    lda DX
    .setdatabank16 $02
    sta (SP)
    .rega8
    jmp MAINLOOP_END
j53:                    ; push bx
    .setdatabank $00
    .rega16
    lda SP
    sec
    sbc #$02
    sta SP
    lda BX
    .setdatabank16 $02
    sta (SP)
    .rega8
    jmp MAINLOOP_END
j54:
    jmp MAINLOOP_END
j55:                    ; push bp
    .setdatabank $00
    .rega16
    lda SP
    sec
    sbc #$02
    sta SP
    lda BP
    .setdatabank16 $02
    sta (SP)
    .rega8
    jmp MAINLOOP_END
j56:                    ; push si
    .setdatabank $00
    .rega16
    lda SP
    sec
    sbc #$02
    sta SP
    lda SI
    .setdatabank16 $02
    sta (SP)
    .rega8
    jmp MAINLOOP_END
j57:                    ; push di
    .setdatabank $00
    .rega16
    lda SP
    sec
    sbc #$02
    sta SP
    lda DI
    .setdatabank16 $02
    sta (SP)
    .rega8
    jmp MAINLOOP_END
j58:                    ; pop ax
    .rega16
    .setdatabank16 $02
    lda (SP)
    .setdatabank16 $00
    sta AX
    lda SP
    clc
    adc #$02
    sta SP
    .rega8
    jmp MAINLOOP_END
j59:                    ; pop cx
    .rega16
    .setdatabank16 $02
    lda (SP)
    .setdatabank16 $00
    sta CX
    lda SP
    clc
    adc #$02
    sta SP
    .rega8
    jmp MAINLOOP_END
j5a:                    ; pop dx
    .rega16
    .setdatabank16 $02
    lda (SP)
    .setdatabank16 $00
    sta DX
    lda SP
    clc
    adc #$02
    sta SP
    .rega8
    jmp MAINLOOP_END
j5b:                    ; pop bx
    .rega16
    .setdatabank16 $02
    lda (SP)
    .setdatabank16 $00
    sta BX
    lda SP
    clc
    adc #$02
    sta SP
    .rega8
    jmp MAINLOOP_END
j5c:
    jmp MAINLOOP_END
j5d:                    ; pop bp
    .rega16
    .setdatabank16 $02
    lda (SP)
    .setdatabank16 $00
    sta BP
    lda SP
    clc
    adc #$02
    sta SP
    .rega8
    jmp MAINLOOP_END
j5e:                    ; pop si
    .rega16
    .setdatabank16 $02
    lda (SP)
    .setdatabank16 $00
    sta SI
    lda SP
    clc
    adc #$02
    sta SP
    .rega8
    jmp MAINLOOP_END
j5f:                    ; pop di
    .rega16
    .setdatabank16 $02
    lda (SP)
    .setdatabank16 $00
    sta DI
    lda SP
    clc
    adc #$02
    sta SP
    .rega8
    jmp MAINLOOP_END
j60:
    jmp MAINLOOP_END
j61:
    jmp MAINLOOP_END
j62:
    jmp MAINLOOP_END
j63:
    jmp MAINLOOP_END
j64:
    jmp MAINLOOP_END
j65:
    jmp MAINLOOP_END
j66:
    jmp MAINLOOP_END
j67:
    jmp MAINLOOP_END
j68:
    jmp MAINLOOP_END
j69:
    jmp MAINLOOP_END
j6a:
    jmp MAINLOOP_END
j6b:
    jmp MAINLOOP_END
j6c:
    jmp MAINLOOP_END
j6d:
    jmp MAINLOOP_END
j6e:
    jmp MAINLOOP_END
j6f:
    jmp MAINLOOP_END
j70:
    jmp MAINLOOP_END
j71:
    jmp MAINLOOP_END
j72:
    jmp MAINLOOP_END
j73:
    jmp MAINLOOP_END
j74:
    jmp MAINLOOP_END
j75:
    jmp MAINLOOP_END
j76:
    jmp MAINLOOP_END
j77:                    ; ja 0150
    ; compare if above
    jsr GETNEXT
    cmp #$80
    bcs j77_back
    j77_forward:
        sta TMP1
        .rega16
        lda IP
        clc
        adc TMP1
        sta IP
        .rega8
        jmp MAINLOOP
    j77_back:
        eor #$ff
        sta TMP1
        .rega16
        lda IP
        sec
        sbc TMP1
        sta IP
        .rega8
        jmp MAINLOOP
    .setdatabank $00
    a 
    jmp MAINLOOP_END
j78:
    jmp MAINLOOP_END
j79:
    jmp MAINLOOP_END
j7a:
    jmp MAINLOOP_END
j7b:
    jmp MAINLOOP_END
j7c:
    jmp MAINLOOP_END
j7d:
    jmp MAINLOOP_END
j7e:
    jmp MAINLOOP_END
j7f:
    jmp MAINLOOP_END
j80:
    jmp MAINLOOP_END
j81:
    jmp MAINLOOP_END
j82:
    jmp MAINLOOP_END
j83:
    jmp MAINLOOP_END
j84:
    jmp MAINLOOP_END
j85:
    jmp MAINLOOP_END
j86:
    jmp MAINLOOP_END
j87:
    jmp MAINLOOP_END
j88:
    jmp MAINLOOP_END
j89:                    ; mov regX, regX
    jsr GETNEXT
    cmp #$c0
    bne +
        .setdatabank $00
        .rega16
        lda AX
        sta AX
        .rega8
        jmp MAINLOOP_END
+   cmp #$d8
    bne +
        .setdatabank $00
        .rega16
        lda BX
        sta AX
        .rega8
        jmp MAINLOOP_END
+   cmp #$c8
    bne +
        .setdatabank $00
        .rega16
        lda CX
        sta AX
        .rega8
        jmp MAINLOOP_END
+   cmp #$d0
    bne +
        .setdatabank $00
        .rega16
        lda DX
        sta AX
        .rega8
        jmp MAINLOOP_END
+   cmp #$c3
    bne +
        .setdatabank $00
        .rega16
        lda AX
        sta BX
        .rega8
        jmp MAINLOOP_END
+   cmp #$db
    bne +
        .setdatabank $00
        .rega16
        lda BX
        sta BX
        .rega8
        jmp MAINLOOP_END
+   cmp #$cb
    bne +
        .setdatabank $00
        .rega16
        lda CX
        sta BX
        .rega8
        jmp MAINLOOP_END
+   cmp #$d3
    bne +
        .setdatabank $00
        .rega16
        lda DX
        sta BX
        .rega8
        jmp MAINLOOP_END
+   cmp #$c1
    bne +
        .setdatabank $00
        .rega16
        lda AX
        sta CX
        .rega8
        jmp MAINLOOP_END
+   cmp #$d9
    bne +
        .setdatabank $00
        .rega16
        lda BX
        sta CX
        .rega8
        jmp MAINLOOP_END
+   cmp #$c9
    bne +
        .setdatabank $00
        .rega16
        lda CX
        sta CX
        .rega8
        jmp MAINLOOP_END
+   cmp #$d1
    bne +
        .setdatabank $00
        .rega16
        lda DX
        sta CX
        .rega8
        jmp MAINLOOP_END
+   cmp #$c2
    bne +
        .setdatabank $00
        .rega16
        lda AX
        sta DX
        .rega8
        jmp MAINLOOP_END
+   cmp #$da
    bne +
        .setdatabank $00
        .rega16
        lda BX
        sta DX
        .rega8
        jmp MAINLOOP_END
+   cmp #$ca
    bne +
        .setdatabank $00
        .rega16
        lda CX
        sta DX
        .rega8
        jmp MAINLOOP_END
+   cmp #$d2
    bne +
        .setdatabank $00
        .rega16
        lda DX
        sta DX
        .rega8
        jmp MAINLOOP_END
+    jmp MAINLOOP_END
      

j8a:
    jmp MAINLOOP_END
j8b:
    jmp MAINLOOP_END
j8c:
    jmp MAINLOOP_END
j8d:
    jmp MAINLOOP_END
j8e:
    jmp MAINLOOP_END
j8f:
    jmp MAINLOOP_END
j90:                    ; nop
    jmp MAINLOOP_END
j91:
    jmp MAINLOOP_END
j92:
    jmp MAINLOOP_END
j93:
    jmp MAINLOOP_END
j94:
    jmp MAINLOOP_END
j95:
    jmp MAINLOOP_END
j96:
    jmp MAINLOOP_END
j97:
    jmp MAINLOOP_END
j98:
    jmp MAINLOOP_END
j99:
    jmp MAINLOOP_END
j9a:
    jmp MAINLOOP_END
j9b:
    jmp MAINLOOP_END
j9c:                    ; pushf
    .setdatabank $00
    .rega16
    lda SP
    sec
    sbc #$02
    sta SP
    lda FR
    .setdatabank16 $02
    sta (SP)
    .rega8
    jmp MAINLOOP_END
j9d:                    ; popf
    .rega16
    .setdatabank16 $02
    lda (SP)
    .setdatabank16 $00
    sta FR
    lda SP
    clc
    adc #$02
    sta SP
    .rega8
    jmp MAINLOOP_END
j9e:                    ; sahf
    .setdatabank $00
    lda AH
    sta FRL
    jmp MAINLOOP_END
j9f:                    ; lahf
    .setdatabank $00
    sta FRL
    lda AH
    jmp MAINLOOP_END
ja0:
    jmp MAINLOOP_END
ja1:
    jmp MAINLOOP_END
ja2:
    jmp MAINLOOP_END
ja3:
    jmp MAINLOOP_END
ja4:
    jmp MAINLOOP_END
ja5:
    jmp MAINLOOP_END
ja6:
    jmp MAINLOOP_END
ja7:
    jmp MAINLOOP_END
ja8:    
    jmp MAINLOOP_END
ja9:
    jmp MAINLOOP_END
jaa:
    jmp MAINLOOP_END
jab:
    jmp MAINLOOP_END
jac:
    jmp MAINLOOP_END
jad:
    jmp MAINLOOP_END
jae:
    jmp MAINLOOP_END
jaf:
    jmp MAINLOOP_END
jb0:                        ; MOV al,01
    jsr GETNEXT
    .setdatabank $00
    sta AL
    jmp MAINLOOP_END
jb1:                        ; MOV cl,01
    jsr GETNEXT
    .setdatabank $00
    sta CL
    jmp MAINLOOP_END
jb2:                        ; MOV dl,01
    jsr GETNEXT
    .setdatabank $00
    sta DL      ; store it
    jmp MAINLOOP_END
jb3:                        ; MOV bl,01
    jsr GETNEXT
    .setdatabank $00
    sta BL
    jmp MAINLOOP_END
jb4:                        ; MOV ah,01
    jsr GETNEXT
    .setdatabank $00
    sta AH      ; store it
    jmp MAINLOOP_END
jb5:                        ; MOV ch,01
    jsr GETNEXT
    .setdatabank $00
    sta CH
    jmp MAINLOOP_END
jb6:                        ; MOV dh,01
    jsr GETNEXT
    .setdatabank $00
    sta DH
    jmp MAINLOOP_END
jb7:                        ; MOV bh,01
    jsr GETNEXT
    .setdatabank $00
    sta BH
    jmp MAINLOOP_END
jb8:                        ; mov AX, 0001
    jsr GETNEXT
    .setdatabank $00
    sta AL
    jsr GETNEXT
    .setdatabank $00
    sta AH
    jmp MAINLOOP_END
jb9:                        ; mov CX, 0001
    jsr GETNEXT
    .setdatabank $00
    sta CL
    jsr GETNEXT
    .setdatabank $00
    sta CH
    jmp MAINLOOP_END
jba:                        ; mov DX, 0001
    jsr GETNEXT
    .setdatabank $00
    sta DL
    jsr GETNEXT
    .setdatabank $00
    sta DH
    jmp MAINLOOP_END
jbb:                        ; mov BX, 0001
    jsr GETNEXT
    .setdatabank $00
    sta BL
    jsr GETNEXT
    .setdatabank $00
    sta BH
    jmp MAINLOOP_END
jbc:
    jmp MAINLOOP_END
jbd:
    jmp MAINLOOP_END
jbe:
    jmp MAINLOOP_END
jbf:
    jmp MAINLOOP_END
jc0:
    jmp MAINLOOP_END
jc1:
    jmp MAINLOOP_END
jc2:
    jmp MAINLOOP_END
jc3:
    jmp MAINLOOP_END
jc4:
    jmp MAINLOOP_END
jc5:
    jmp MAINLOOP_END
jc6:
    jmp MAINLOOP_END
jc7:
    jmp MAINLOOP_END
jc8:
    jmp MAINLOOP_END
jc9:
    jmp MAINLOOP_END
jca:
    jmp MAINLOOP_END
jcb:
    jmp MAINLOOP_END
jcc:
    jmp MAINLOOP_END
jcd:
    jsr GETNEXT
    cmp #$21
    beq jcd_putc
    cmp #$20
    beq jcd_exit
    jmp MAINLOOP_END

    jcd_putc:
        .setdatabank $00
        lda AH
        cmp #$09
        bne +
            ldy #$00
            pr_string_loop:
                .setdatabank $02
                lda (DX),y
                cmp #'$'
                beq pr_string_done
                .setdatabank $00
                jsr $FFD2
                iny
                jmp pr_string_loop
            pr_string_done:
                jmp MAINLOOP_END
+       lda DL
        jsr $FFD2
        jmp MAINLOOP_END
    jcd_exit:
        jmp EXIT
jce:
    jmp MAINLOOP_END
jcf:
    jmp MAINLOOP_END
jd0:
    jmp MAINLOOP_END
jd1:
    jmp MAINLOOP_END
jd2:
    jmp MAINLOOP_END
jd3:
    jmp MAINLOOP_END
jd4:
    jmp MAINLOOP_END
jd5:
    jmp MAINLOOP_END
jd6:
    jmp MAINLOOP_END
jd7:
    jmp MAINLOOP_END
jd8:
    jmp MAINLOOP_END
jd9:
    jmp MAINLOOP_END
jda:
    jmp MAINLOOP_END
jdb:
    jmp MAINLOOP_END
jdc:
    jmp MAINLOOP_END
jdd:
    jmp MAINLOOP_END
jde:
    jmp MAINLOOP_END
jdf:
    jmp MAINLOOP_END
je0:
    jmp MAINLOOP_END
je1:
    jmp MAINLOOP_END
je2:
    jsr GETNEXT
    pha
    .setdatabank $00
    .rega16
    lda CX
    beq je2_loopdone
    dec CX
    .rega8
    pla
    cmp #$80
    bcs je2_back
    je2_forward:
        sta TMP1
        .rega16
        lda IP
        clc
        adc TMP1
        sta IP
        .rega8
        jmp MAINLOOP
    je2_back:
        eor #$ff
        sta TMP1
        .rega16
        lda IP
        sec
        sbc TMP1
        sta IP
        .rega8
        jmp MAINLOOP
    je2_loopdone:
        .rega8
        pla
        jmp MAINLOOP
je3:
    jmp MAINLOOP_END
je4:
    jmp MAINLOOP_END
je5:
    jmp MAINLOOP_END
je6:
    jmp MAINLOOP_END
je7:
    jmp MAINLOOP_END
je8:
    jmp MAINLOOP_END
je9:
    jmp MAINLOOP_END
jea:
    jmp MAINLOOP_END
jeb:
    jsr GETNEXT
    .setdatabank $00
    cmp #$80
    bcs jeb_back
    jeb_forward:
        sta TMP1
        .rega16
        lda IP
        clc
        adc TMP1
        sta IP
        .rega8
        jmp MAINLOOP
    jeb_back:
        eor #$ff
        sta TMP1
        .rega16
        lda IP
        sec
        sbc TMP1
        sta IP
        .rega8
        jmp MAINLOOP
jec:
    jmp MAINLOOP_END
jed:
    jmp MAINLOOP_END
jee:
    jmp MAINLOOP_END
jef:
    jmp MAINLOOP_END
jf0:                    ; lock
    jmp MAINLOOP_END
jf1:
    jmp MAINLOOP_END
jf2:
    jmp MAINLOOP_END
jf3:
    jmp MAINLOOP_END
jf4:                    ; hlt
    jmp jf4
jf5:
    jmp MAINLOOP_END
jf6:
    jmp MAINLOOP_END
jf7:
    jmp MAINLOOP_END
jf8:
    jmp MAINLOOP_END
jf9:
    jmp MAINLOOP_END
jfa:
    jmp MAINLOOP_END
jfb:
    jmp MAINLOOP_END
jfc:
    jmp MAINLOOP_END
jfd:
    jmp MAINLOOP_END
jfe:                    ; INC reg
    jsr GETNEXT
    cmp #$c0
    beq jfe_al
    cmp #$c4
    beq jfe_ah
    cmp #$c3
    beq jfe_bl
    cmp #$c7
    beq jfe_bh
    cmp #$c1
    beq jfe_cl
    cmp #$c5
    beq jfe_ch
    cmp #$c2
    beq jfe_dl
    cmp #$c6
    beq jfe_dh
    jmp MAINLOOP_END

    jfe_al:
        .setdatabank $00
        inc AL
        jmp MAINLOOP_END
    jfe_ah:
        .setdatabank $00
        inc AH
        jmp MAINLOOP_END
    jfe_bl:
        .setdatabank $00
        inc BL
        jmp MAINLOOP_END
    jfe_bh:
        .setdatabank $00
        inc BH
        jmp MAINLOOP_END
    jfe_cl:
        .setdatabank $00
        inc CL
        jmp MAINLOOP_END
    jfe_ch:
        .setdatabank $00
        inc CH
        jmp MAINLOOP_END
    jfe_dl:
        .setdatabank $00
        inc DL
        jmp MAINLOOP_END
    jfe_dh:
        .setdatabank $00
        inc DH
        jmp MAINLOOP_END
jff:
    jmp MAINLOOP_END