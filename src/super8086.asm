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
    
    ; set instruction pointer to $020100
    .rega16
    lda #$0100
    sta IP
    .regaxy8

    ; push test prg
    ldy #$00
LOAD:
    lda X86PRG,y
    .setdatabank $02
    sta (IP),y
    iny
    cpy #$08
    beq MAINLOOP
    .setdatabank $00
    jmp LOAD

X86PRG:
    .byte $b2, $58, $b4, $02, $cd, $21, $eb, $f8, $00

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
    lda #<msg2      ; print the startup message
    sta $fe 
    lda #>msg2
    sta $ff
    jsr print
    .emulation
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

msg1:
    .text $93,$0e,"Super8086 Emulator",$0d,$00

msg2:
    .text $0d,"Terminated",$0d,$00

; -------------------------------------------------------
; opcode operations
; -------------------------------------------------------
j00:
    jmp MAINLOOP_END
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
j06:
    jmp MAINLOOP_END
j07:
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
j0e:
    jmp MAINLOOP_END
j0f:
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
j16:
    jmp MAINLOOP_END
j17:
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
j1e:
    jmp MAINLOOP_END
j1f:
    jmp MAINLOOP_END
j20:
    jmp MAINLOOP_END
j21:
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
    jmp MAINLOOP_END
j42:
    jmp MAINLOOP_END
j43:
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
j50:
    jmp MAINLOOP_END
j51:
    jmp MAINLOOP_END
j52:
    jmp MAINLOOP_END
j53:
    jmp MAINLOOP_END
j54:
    jmp MAINLOOP_END
j55:
    jmp MAINLOOP_END
j56:
    jmp MAINLOOP_END
j57:
    jmp MAINLOOP_END
j58:
    jmp MAINLOOP_END
j59:
    jmp MAINLOOP_END
j5a:
    jmp MAINLOOP_END
j5b:
    jmp MAINLOOP_END
j5c:
    jmp MAINLOOP_END
j5d:
    jmp MAINLOOP_END
j5e:
    jmp MAINLOOP_END
j5f:
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
j77:
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
j89:
    jmp MAINLOOP_END
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
j90:
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
j9c:
    jmp MAINLOOP_END
j9d:
    jmp MAINLOOP_END
j9e:
    jmp MAINLOOP_END
j9f:
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
jb0:
    jmp MAINLOOP_END
jb1:
    jmp MAINLOOP_END
jb2:
    jsr INC_IP
    jsr FETCH_IP
    .setdatabank $00
    sta DL      ; store it
    jmp MAINLOOP_END
jb3:
    jmp MAINLOOP_END
jb4:
    jsr INC_IP
    jsr FETCH_IP
    .setdatabank $00
    sta AH      ; store it
    jmp MAINLOOP_END
jb5:
    jmp MAINLOOP_END
jb6:
    jmp MAINLOOP_END
jb7:
    jmp MAINLOOP_END
jb8:
    jmp MAINLOOP_END
jb9:
    jmp MAINLOOP_END
jba:
    jmp MAINLOOP_END
jbb:
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
    jsr INC_IP
    jsr FETCH_IP
    cmp #$21
    beq jcd_putc
    cmp #$20
    beq jcd_exit
    jmp MAINLOOP_END

    jcd_putc:
        .setdatabank $00
        lda DL
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
    jmp MAINLOOP_END
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
    jsr INC_IP
    jsr FETCH_IP
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
jf0:
    jmp MAINLOOP_END
jf1:
    jmp MAINLOOP_END
jf2:
    jmp MAINLOOP_END
jf3:
    jmp MAINLOOP_END
jf4:
    jmp MAINLOOP_END
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
jfe:
    jsr INC_IP
    jsr FETCH_IP
    cmp #$c0
    beq jfe_al
    cmp #$c4
    beq jfe_ah
    jmp MAINLOOP_END

    jfe_al:
        .setdatabank $00
        inc AL
        jmp MAINLOOP_END
    jfe_ah:
        .setdatabank $00
        inc AH
        jmp MAINLOOP_END
jff:
    jmp MAINLOOP_END