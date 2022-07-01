;
; nes_reset_state_detect example demo. 
;
; Uses the nes_reset_state_detect library to print detected system, phase and bit patterns.
;
; Additionally, if START is pressed the code will "soft-boot" by jumping to $C000.
; This functionality is intended to allow easier inclusion of this demo ROM in 
; your own ROM using raster trickery, by allowing quick switching between the two.
;

.CODE
startup:
    jmp Main

.include "nmi_sync.s"
.include "nes_reset_state_detect.s"

.zeropage
joy:
joy0:           .res 1
joy1:           .res 1
joyP:
joyP0:          .res 1
joyP1:          .res 1
frameCounter:   .res 1
tmp:            .res 2


JOY_A       = %00000001
JOY_B       = %00000010
JOY_SELECT  = %00000100
JOY_START   = %00001000
JOY_UP      = %00010000
JOY_DOWN    = %00100000
JOY_LEFT    = %01000000
JOY_RIGHT   = %10000000

.CODE
Main:
    sei
    ldx #0
    stx $2000
    stx $2001
    lda NRSD_TOGGLE_BOOT
    pha
    txa
:
    sta $000,x
    sta $100,x
    sta $200,x
    sta $300,x
    sta $400,x
    sta $500,x
    sta $600,x
    sta $700,x
    inx
    bne :-
    pla
    sta NRSD_TOGGLE_BOOT
    dex
    txs
    lda #$C0
    sta $4017
    ; Clear nametables
    jsr ClearCHR
    jsr ClearNameTables
    jsr UploadNameTable
    ; Initialize PPU and palette
    jsr UploadASCIICHR
    jsr UploadBackgroundCHR
    jsr UploadSpriteCHR
    jsr SetPalette
    ; Initialize detection
    jsr NRSD_Initialize
MainLoop:
    jsr wait_nmi
    jsr UpdateState
    ; Set testConfig equal to frameCounter, to keep repeating the same tests with a cycle of 128 frames
    inc frameCounter
    lda frameCounter
    jsr NRSD_UpdateTestParameters
    jmp MainLoop

UpdateState:
    jsr ReadJoypads
    ; If START pressed, jump to $C400
    lda joyP0
    and #JOY_START
    beq :+
    jmp $C400
:
    ; If START pressed, offset by 2 to access joyP0 (single-step) for all movements
    ldx #0
    lda joy0
    and #JOY_SELECT
    beq :+
    ldx #2
:
    rts

ClearCHR:
    lda #$00
    sta $2000
    sta $2006
    sta $2006
    lda #0
    ldy #32
:
    sta $2007
    inx
    bne :-
    dey
    bne :-
    rts

ClearNameTables:
    lda #$20
    sta $2006
    lda #$00
    sta $2006
@bankSwitchNT1:
    lda #$80
    sta @bankSwitchNT1+1
    jsr @clear4Nametables
@bankSwitchNT0:
    lda #$00
    sta @bankSwitchNT0+1
@clear4Nametables:
    ldy #16
    ldx #0
    lda #0
:
    sta $2007
    inx
    bne :-
    dey
    bne :-
    rts

UploadNameTable:
    @ChrPtr = tmp
    lda #$20
    sta $2006
    lda #$00
    sta $2006
    lda #>NameTableData
    sta @ChrPtr+1
    lda #<NameTableData
    sta @ChrPtr
    ldx #4
    ldy #0
:
    lda (@ChrPtr),y
    iny
    sta $2007
    bne :-
    inc @ChrPtr+1
    dex
    bne :-
    rts

UploadSpriteCHR:
    lda #>($0000 + 16 * NRSD_SPRITE0_TILE_INDEX)
    sta $2006
    lda #<($0000 + 16 * NRSD_SPRITE0_TILE_INDEX)
    sta $2006
    ldy #0
:
    lda Sprite0TileData,y
    sta $2007
    iny
    cpy #16
    bne :-
    rts

UploadASCIICHR:
    @ChrPtr = tmp
    lda #$10
    sta $2006
    lda #$00
    sta $2006
    lda #>TileDataASCII
    sta @ChrPtr+1
    lda #<TileDataASCII
    sta @ChrPtr
    ldx #8
    ldy #0
:
    lda (@ChrPtr),y
    iny
    sta $2007
    bne :-
    inc @ChrPtr+1
    dex
    bne :-
    rts

UploadBackgroundCHR:
    lda #>($1000 + 16 * NRSD_SPRITE0_HIT_BG_TILE_INDEX)
    sta $2006
    lda #<($1000 + 16 * NRSD_SPRITE0_HIT_BG_TILE_INDEX)
    sta $2006
    ldy #0
:
    lda Sprite0HitBGColumnTileData,y
    sta $2007
    iny
    cpy #16
    bne :-
    rts

SetPalette:
    lda #$3F
    sta $2006
    lda #$00
    sta $2006
    ldy #0
:
    lda @palette,y
    sta $2007
    iny
    cpy #32
    bne :-    
    rts

@palette:
.byte $1D,$11,$16,$1A
.byte $1D,$11,$16,$11
.byte $1D,$11,$16,$13
.byte $1D,$11,$16,$16
;
.byte $1D,$10,$1D,$1D
.byte $1D,$1D,$1D,$1D
.byte $1D,$1D,$1D,$1D
.byte $1D,$1D,$1D,$1D

ReadJoypads:
    ldx     #1
    stx     $4016
    dex
    stx     $4016
    jsr     ReadJoy
    inx
ReadJoy:
    lda     joy,X
    pha
    ldy     #8
:   lda     $4016,X
    lsr
    ror     joy,X
    dey
    bne :-
    pla
    eor joy,X
    and joy,X
    sta joyP,X
    rts

NameTableData:
.INCBIN "labels.nam"

Sprite0TileData:
.byte %10000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
;
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000

Sprite0HitBGColumnTileData:
.byte %10111111
.byte %10111111
.byte %10111111
.byte %10111111
.byte %10111111
.byte %10111111
.byte %10111111
.byte %10111111
;
.byte %11011111
.byte %11011111
.byte %11011111
.byte %11011111
.byte %11011111
.byte %11011111
.byte %11011111
.byte %11011111

TileDataASCII:
.INCBIN "ascii.chr", $0000, $800

.segment "VECTORS"

nmi_rom:
    jmp (NRSD_nmiAddr)

reset:
    lda #0
    sta reset+1
    jmp startup

; Freeze program if this somehow gets triggered, rather
; than silently messing up timing
irq:    jmp irq

.word nmi_rom, reset, irq

.segment "HEADER"
    .byte "NES",26, 2,0, $E0 + %00001010, $10 ; 32K PRG, 8K CHR, Mapper30
    .byte 0,0,0,0,0,0,0,0
