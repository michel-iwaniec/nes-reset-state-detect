;
; NES-Reset-State-Detection library
;
;
; Author: Michel Iwaniec <michel_iwaniec@yahoo.com>
;
; Uses the nmi_sync library to ensure minimal CPU / PPU jitter between frames, authored by Shay Green
;
; See README.md for details
;

;
; Delay by a fractional amount of cycles using the scanline accumulator byte
;
; Takes 11.666 cycles (NTSC)
;
.MACRO NRSD_DELAY_ACC
    lda NRSD_scanAcc
    clc
    adc #NRSD_SCAN_ACC_ADD
    bcs :+
:
    sta NRSD_scanAcc
.ENDMACRO

;
; Sets X / Y of a single sprite
;
.MACRO NRSD_SET_SPRITE spriteIndex, spriteX, spriteY, tileIndex
    lda spriteX
    sta sprites+spriteIndex*4+3
    lda spriteY
    sta sprites+spriteIndex*4+0
    lda tileIndex
    sta sprites+spriteIndex*4+1
.ENDMACRO

sprites                         = $200          ; OAM page
NRSD_SPRITE0_XPOS               = 247
NRSD_SPRITE0_YPOS               = 190
NRSD_SPRITE0_TILE_INDEX         = $FF
NRSD_SPRITE0_HIT_BG_COLUMN      = 31
NRSD_SPRITE0_HIT_BG_TILE_INDEX  = $FF
NRSD_INIT_FINE_DELAY            = 3       ; Initial value for HBlank fine-tune delay. Allowed range: 0 <= INIT_FINE_DELAY <= 3
NRSD_INIT_SCROLL_X              = 0
NRSD_SCAN_ACC_ADD               = 171     ; NTSC: 0.666 fractional cycles / scanline -> 256 * 0.333 ~= 171

NRSD_NUM_TESTS                  = 4
NRSD_NUM_BYTES_PER_TEST         = 4
NRSD_NUM_TEST_BYTES             = 16

NRSD_SYSTEM_UNKNOWN             = 0
NRSD_SYSTEM_NTSC                = 1
NRSD_SYSTEM_PAL                 = 2

; Defines where in the nametable system name / phase name / test results are written
NRSD_TEXT_SYSTEM_X              = 10
NRSD_TEXT_SYSTEM_Y              = 2
NRSD_TEXT_PHASE_X               = 23
NRSD_TEXT_PHASE_Y               = 2
NRSD_TEXT_TESTS_X               = 16
NRSD_TEXT_TESTS_Y               = 4

; Last zeropage byte used to toggle reset between nes-phase-detect / zappy
NRSD_TOGGLE_BOOT                = $FF

; RAM where HBlank code will be copied to allow for patching hi byte of address
NRSD_HBlankCodeRAM = $300
NRSD_HBlankCodeRAM_END = $380
; ASCII version of test results
NRSD_testResultsASCII = $380

.zeropage
NRSD_nmiAddr:                   .res 2
NRSD_HBlankExecutePtr:          .res 2
NRSD_scanlineDelayExecutePtr:   .res 2
NRSD_detectedSystem:            .res 1                      ; Set once to either SYSTEM_NTSC or SYSTEM_PAL by system detection test
NRSD_detectedPattern:           .res NRSD_NUM_TESTS
NRSD_detectedPhase:             .res 1                      ; Set to either 0..3 to report reset phase, or $FF if unable to detect (continuously updated)
NRSD_testOffset:                .res 1                      ; Fine-X to use for $2005
NRSD_testOffsetCoarse:          .res 1                      ; Coarse-X to use for $2006 testing
NRSD_testResults:               .res NRSD_NUM_TEST_BYTES    ; Test results (continuosly updated)
NRSD_detectedSystemASCII:       .res 4                      ; ASCII version of NRSD_detectedSystem
NRSD_detectedPhaseASCII:        .res 1                      ; ASCII version of NRSD_detectedPhase
NRSD_ScrollX:                   .res 1                      ; Background scroll X
NRSD_ScrollY:                   .res 1                      ; Background scroll Y
NRSD_scanAcc:                   .res 1                      ; Scanline accumulator used to emulate fractional cycles
NRSD_tmp:                       .res 2
NRSD_fineDelay:                 .res 1                      ; HBlank fine-tuning delay
NRSD_r2001:                     .res 1                      ; $2001 value
NRSD_testConfig:                .res 1                      ; Defines tests to run in NMI
NRSD_hitSprite0:                .res 1

.CODE
.align $100
NRSD_nmi:
    pha
    jsr begin_nmi_sync
    lda #0
    sta $2003
    lda #>sprites
    sta $4014
    lda #$08
    sta $2001
    lda #$90
    sta $2000
    jsr NRSD_WriteTestResultsToNameTable
    lda #0
    sta $2006
    sta $2006
    lda NRSD_ScrollX
    sta $2005
    lda NRSD_ScrollY
    sta $2005
    jsr NRSD_Delay_290
    jsr end_nmi_sync
    lda NRSD_r2001
    sta $2001

    jsr NRSD_DelayUntilTestExecution

;
    lda NRSD_r2001
    sta $2001

    ; Apply fine delay
    ldx NRSD_fineDelay
    jsr NRSD_DelayByXPlus36

    lda NRSD_ScrollX
    jsr NRSD_DoPPURegWrite
    jsr NRSD_Delay_290
    lda $2002
    asl
    and #$80
    sta NRSD_hitSprite0

    ; Briefly turn on mono-bit if sprite0 was hit
    lda NRSD_r2001
    tax
    lda NRSD_hitSprite0
    asl
    lda #0
    rol
    ora NRSD_r2001
    sta $2001
    stx $2001

    ; Disable BG for remaining scanlines
    lda NRSD_r2001
    and #$F7
    sta $2001

    jsr NRSD_UpdateTestResult
    jsr NRSD_DetectPatterns
    jsr NRSD_DetectPhase
    jsr NRSD_UpdateASCII

    pla
    rti

.align $100
;
; Delays by NRSD_fineDelay cycles + 
;
NRSD_DelayByXPlus36:
    cpx #0
    bne :+
    ; 0 + 36 = 36
    nop
    nop
    nop
    nop
    nop
    nop
    rts
:
    dex
    bne :+
    ; 1 + 36 = 37
    nop
    nop
    nop
    nop
    rts
:
    dex
    bne :+
    ; 2 + 36 = 38
    nop
    nop
    rts
:
    dex
    bne :+
    ; 3 + 36 = 39
    rts
:
; Should never happen unless constant out-of-range - do endless loop if so
:
    jmp :-

.align $100
NRSD_HBlankCodeROM:
NRSD_HBlankCodeROM_test2005:
    nop
    nop
    nop
    nop
    nop
    nop
    bit $2002
NRSD_HBlankCodeROM_writeFineX:
    sta $2005
    bit $2002
    rts
NRSD_HBlankCodeROM_test2006:
    iny
    lda $00
NRSD_HBlankCodeROM_writeCoarseX:
    sty $2006
    rts

NRSD_HBlankCodeROM_test2001:
    bit $2002
    sta $2005
    nop
    nop
    nop
    nop
    lda #$00
    ldy #$18
    nop
NRSD_HBlankCodeROM_write2001:
    sta $2001
    sty $2001
    ; Set VADDR to make sure next scanlines have the expected scroll irrespective of whether
    ; "inc vert(v)" / "hori(v) = hori(t)" took place on dot 256 / 257 or not
    lda #<$2320
    sta $2006
    rts

NRSD_HBlankCodeROM_test2001_B:
    bit $2002
    sta $2005
    lda #$08
    sta $2001
    lda #$18
NRSD_HBlankCodeROM_write2001_B:
    sta $2001
    bit $2002
    rts

NRSD_CopyHBlankCodeToRAM:
    ldx #0
:
    lda NRSD_HBlankCodeROM,x
    sta NRSD_HBlankCodeRAM,x
    inx
    cpx #(NRSD_HBlankCodeRAM_END - NRSD_HBlankCodeRAM)
    bne :-
    rts

NRSD_DoPPURegWrite:
    lda #$23
    sta $2006
    ldx #$20
    stx $2006
    ; Delay 36 cycles
    ldx #7
:
    dex
    bne :-
    ldy NRSD_testOffsetCoarse
    lda NRSD_testOffset
    sta $2005
    clc
    adc #1
    jsr NRSD_DelayScanlines
    jmp (NRSD_HBlankExecutePtr)

NRSD_DelayScanlines:
    jmp (NRSD_scanlineDelayExecutePtr)

;
; Gets the test "offset" (/"sub-test") for a test
;
NRSD_GetTestConfig_testOffset:
    lda NRSD_testConfig
    lsr
    lsr
    lsr
    lsr
    and #7
    rts

;
; Sets carry if write is late, and clears it if early
;
NRSD_GetTestConfig_Timing:
    lda NRSD_testConfig
    lsr
    lsr
    rts

;
; Gets test number
;
NRSD_GetTestConfig_TestNumber:
    lda NRSD_testConfig
    lsr
    lsr
    and #3
    rts

;
; Gets test number, timing and parity in lowest 4 bits
;
NRSD_GetTestConfig_TestNumberTimingParity:
    lda NRSD_testConfig
    and #%00001111
    rts

;
; Update test parameters
;
NRSD_UpdateTestParameters:
    sta NRSD_testConfig
    jsr NRSD_GetTestConfig_TestNumber
    tay
    ;
    lda @UpdateExecutePtrLo,y
    sta NRSD_HBlankExecutePtr+0
    lda @UpdateExecutePtrHi,y
    sta NRSD_HBlankExecutePtr+1
    ;
    lda @UpdateParametersLo,y
    sta NRSD_tmp
    lda @UpdateParametersHi,y
    sta NRSD_tmp+1
    jmp (NRSD_tmp)

.define NRSD_UpdateExecutePtr_ELEMENTS NRSD_HBlankCodeROM_test2005 - NRSD_HBlankCodeROM + NRSD_HBlankCodeRAM, NRSD_HBlankCodeROM_test2006 - NRSD_HBlankCodeROM + NRSD_HBlankCodeRAM, NRSD_HBlankCodeROM_test2001 - NRSD_HBlankCodeROM + NRSD_HBlankCodeRAM, NRSD_HBlankCodeROM_test2001_B - NRSD_HBlankCodeROM + NRSD_HBlankCodeRAM
@UpdateExecutePtrLo: .lobytes NRSD_UpdateExecutePtr_ELEMENTS
@UpdateExecutePtrHi: .hibytes NRSD_UpdateExecutePtr_ELEMENTS

.define NRSD_UpdateParameters_ELEMENTS @UpdateParameters_2005, @UpdateParameters_2006, @UpdateParameters_2001, @UpdateParameters_2001_B
@UpdateParametersLo: .lobytes NRSD_UpdateParameters_ELEMENTS
@UpdateParametersHi: .hibytes NRSD_UpdateParameters_ELEMENTS

@UpdateParameters_2005:
    jsr NRSD_GetTestConfig_testOffset
    sta NRSD_testOffset
    jsr @updateXFineHiByte
    jsr @updateSprite0_xpos
    jsr @updateSprite0_ypos_2005
    jsr @updateFineDelay_2005
    jsr @updateOffsetCoarse
    jsr @updateXCoarseHiByte
    jsr @updateDelayExecutePtr_zero ; 2005 test does not do scanline delay - force to zero jsr @updateDelayExecutePtr_2005
    rts

@UpdateParameters_2006:
    ; Always use testOffset=0 for $2006 test
    ldy #0
    sty NRSD_testOffset
    jsr @updateXFineHiByte
    jsr @updateSprite0_xpos
    jsr @updateSprite0_ypos_2006
    jsr @updateFineDelay_2006
    jsr @updateOffsetCoarse
    jsr @updateXCoarseHiByte
    jsr @updateDelayExecutePtr_testOffset
    rts

@UpdateParameters_2001:
    ; Always use testOffset=0 for $2001 test
    ldy #0
    sty NRSD_testOffset
    jsr @updateXFineHiByte
    jsr @updateSprite0_xpos
    jsr @updateSprite0_ypos_2001
    ldx #1
    jsr @updateFineDelay_2001
    jsr @updateOffsetCoarse
    jsr @updateXCoarseHiByte
    jsr @updateDelayExecutePtr_testOffset
    jsr @updatePPUMaskHiByte
    rts

; Sets up a test to turn off sprite rendering, and turn it back on just before sprite#0 hit
@UpdateParameters_2001_B:
    ; Always use testOffset=0 for 2001_B test
    ldy #0
    sty NRSD_testOffset
    jsr @updateXFineHiByte
    jsr @updateSprite0_xpos
    jsr @updateSprite0_ypos_2001_B
    ldx #2
    jsr @updateFineDelay_2001
    jsr @updateOffsetCoarse
    jsr @updateXCoarseHiByte
    jsr @updateDelayExecutePtr_testOffset
    jsr @updatePPUMaskHiByte_B
    rts

@UpdateParameters_NOP:
    rts

@updateXFineHiByte:
    ldx NRSD_testOffset
    jsr NRSD_GetTestConfig_Timing
    bcs :+
    ; Switch from late to early write
    inx
:
    lda NRSD_ValueToHiRegAddrTab,x
    sta NRSD_HBlankCodeRAM + (NRSD_HBlankCodeROM_writeFineX - NRSD_HBlankCodeROM) + 2
    rts
@updateXCoarseHiByte:
    ldx NRSD_testOffsetCoarse
    jsr NRSD_GetTestConfig_Timing
    bcs :+
    ; Switch from late to early write
    inx
:
    lda NRSD_ValueToHiRegAddrTab,x
    sta NRSD_HBlankCodeRAM + (NRSD_HBlankCodeROM_writeCoarseX - NRSD_HBlankCodeROM) + 2
    rts

@updatePPUMaskHiByte:
    ldx #$18
    jsr NRSD_GetTestConfig_Timing
    bcs :+
    ; Switch from late to early write
    ldx #$00
:
    lda NRSD_ValueToHiRegAddrTab,x
    sta NRSD_HBlankCodeRAM + (NRSD_HBlankCodeROM_write2001 - NRSD_HBlankCodeROM) + 2
    rts

@updatePPUMaskHiByte_B:
    ldx #$08
    jsr NRSD_GetTestConfig_Timing
    bcs :+
    ; Switch from late to early write
    ldx #$18
:
    lda NRSD_ValueToHiRegAddrTab,x
    sta NRSD_HBlankCodeRAM + (NRSD_HBlankCodeROM_write2001_B - NRSD_HBlankCodeROM) + 2
    rts

@updateSprite0_xpos:
    lda #NRSD_SPRITE0_XPOS
    sec
    sbc NRSD_testOffset
    sta sprites+3
    rts
@updateSprite0_ypos_2005:
    ; 2005 writes do not change y-pos of sprite#0 - force offset to zero
    lda #NRSD_SPRITE0_YPOS
    sta sprites+0
    rts

@updateSprite0_ypos_2006:
    jsr NRSD_GetTestConfig_testOffset
    and #3
    clc
    adc #NRSD_SPRITE0_YPOS
    sta sprites+0
    rts

@updateSprite0_ypos_2001:
    ; Writes to $2001 to interrupt sprite fetch affects rendering on *next* scanline.
    ; Add with carry set to compensate for this.
    jsr NRSD_GetTestConfig_testOffset
    and #3
    sec
    adc #NRSD_SPRITE0_YPOS
    sta sprites+0
    rts

@updateSprite0_ypos_2001_B:
    jsr NRSD_GetTestConfig_testOffset
    and #3
    clc
    adc #NRSD_SPRITE0_YPOS
    sta sprites+0
    rts

@updateFineDelay_2005:
    ldx #3
    jsr @adjust_fineDelay_2005
    stx NRSD_fineDelay
    rts
@adjust_fineDelay_2005:
    lda NRSD_testOffset
    sec
    sbc #3
    bcs :+
    rts
:
    dex
    sec
    sbc #3
    bcs :+
    rts
:
    dex
    rts

@updateFineDelay_2006:
    ldx #3
    jsr @adjust_fineDelay_2006
    stx NRSD_fineDelay
    rts
@adjust_fineDelay_2006:
    jsr NRSD_GetTestConfig_testOffset
    ; for 2006 test, go 3 CPU cycles (=9 dots) earlier if offset >= 4
    lsr
    lsr
    lsr
    bcc :+
    dex
    dex
    dex
    rts
:
    lda NRSD_testOffset
    sec
    sbc #3
    bcs :+
    rts
:
    dex
    sec
    sbc #3
    bcs :+
    rts
:
    dex
    rts

@updateFineDelay_2001:
    jsr @adjust_fineDelay_2001
    stx NRSD_fineDelay
    rts
@adjust_fineDelay_2001:
    ; for 2001 test, go 1 CPU cycles (=3 dots) earlier if offset >= 4
    jsr NRSD_GetTestConfig_testOffset
    and #7
    cmp #4
    bcc :+
    inx
    rts
:
    rts

@updateOffsetCoarse:
    ldy #$1C
    jsr NRSD_GetTestConfig_testOffset
    lsr
    lsr
    lsr
    bcc :+
    ; Decrement by 1, as we've gone 3 CPU cycles (=9 dots) back
    dey
:
    sty NRSD_testOffsetCoarse
    rts

@updateDelayExecutePtr_zero:
    ldy #0
    beq @updateDelayExecutePtr
@updateDelayExecutePtr_testOffset:
    jsr NRSD_GetTestConfig_testOffset
    and #3
    tay
@updateDelayExecutePtr:
    lda NRSD_ScanlineDelayAddrLo,y
    sta NRSD_scanlineDelayExecutePtr+0
    lda NRSD_ScanlineDelayAddrHi,y
    sta NRSD_scanlineDelayExecutePtr+1
    rts

NRSD_ScanlineDelayNone:
    rts
NRSD_ScanlineDelayOne:
    ; Delay 114 cycles
    jsr NRSD_Delay_110
    nop
    nop
    rts
NRSD_ScanlineDelayTwo:
    ; Delay 114 cycles
    jsr NRSD_Delay_110
    nop
    nop
    ; Delay 114 cycles
    jsr NRSD_Delay_110
    nop
    nop
    rts
NRSD_ScanlineDelayThree:
    ; Delay 114 cycles
    jsr NRSD_Delay_110
    nop
    nop
    ; Delay 114 cycles
    jsr NRSD_Delay_110
    nop
    nop
    ; Delay 113 cycles
    jsr NRSD_Delay_110
    ldx $00
    rts

.define NRSD_ScanlineDelayAddr_ELEMENTS NRSD_ScanlineDelayNone, NRSD_ScanlineDelayOne, NRSD_ScanlineDelayTwo, NRSD_ScanlineDelayThree
NRSD_ScanlineDelayAddrLo: .lobytes NRSD_ScanlineDelayAddr_ELEMENTS
NRSD_ScanlineDelayAddrHi: .hibytes NRSD_ScanlineDelayAddr_ELEMENTS

;
; Table for making PPU writes early or late, by selecting the upper 8 bits of the PPU register address to match the 5 LSB of the written value.
;
.align $100
NRSD_ValueToHiRegAddrTab:
.repeat 256,i
    .byte (i & $1F) | $20
.endrep

;
; Updates 1 single bit in NRSD_testResults to reflect result of sprite#0 hit / miss
;
; frameCounter determines which bit to change in NRSD_testResults, as follows
; xxOOOttTP
;   \ /||||
;    | \||+- Frame parity. (0: left / "even", 1: right / "odd")
;    |  |+-- Timing of register write. (0: Early, 1: Late)
;    |  +--- Test number (0: $2005 fine-X, 1: $2006 coarse-X, 2: $2001 blanking at sprite CHR fetch, 3: Sprite-rendering-turn-on)
;    +------ Offset of sprite position / fine-X scroll. (0-7) / scanline delay
;
NRSD_UpdateTestResult:
    jsr NRSD_GetTestConfig_TestNumberTimingParity
    tay
    jsr NRSD_GetTestConfig_testOffset
    and #7
    tax
    lda @bitIndexToMask,x
    ldx NRSD_hitSprite0
    bmi :+
    ; sprite miss - invert bitmask and use AND to clear appropriate bit
    eor #%11111111
    and NRSD_testResults,y
    sta NRSD_testResults,y
    rts
:
    ; sprite hit - use bitmask as-is and use OR to set appropriate bit
    ora NRSD_testResults,y
    sta NRSD_testResults,y
    rts

@bitIndexToMask:
.byte %00000001
.byte %00000010
.byte %00000100
.byte %00001000
.byte %00010000
.byte %00100000
.byte %01000000
.byte %10000000

;
; Updates ASCII versions of system name / phase name / test results
;
NRSD_UpdateASCII:
    jsr @updateSystem
    jsr @updatePhase
    jsr @updateNRSD_testResults
    rts
@updateSystem:
    lda NRSD_detectedSystem
    cmp #NRSD_SYSTEM_NTSC
    bne :+
    lda #'N'
    sta NRSD_detectedSystemASCII+0
    lda #'T'
    sta NRSD_detectedSystemASCII+1
    lda #'S'
    sta NRSD_detectedSystemASCII+2
    lda #'C'
    sta NRSD_detectedSystemASCII+3
    rts
:
    cmp #NRSD_SYSTEM_PAL
    bne :+
    lda #'P'
    sta NRSD_detectedSystemASCII+0
    lda #'A'
    sta NRSD_detectedSystemASCII+1
    lda #'L'
    sta NRSD_detectedSystemASCII+2
    lda #' '
    sta NRSD_detectedSystemASCII+3
    rts
:
    lda #'?'
    sta NRSD_detectedSystemASCII+0
    sta NRSD_detectedSystemASCII+1
    sta NRSD_detectedSystemASCII+2
    sta NRSD_detectedSystemASCII+3
    rts
@updatePhase:
    lda NRSD_detectedPhase
    bmi :+
    clc
    adc #'0'
    sta NRSD_detectedPhaseASCII
    rts
:
    lda #'?'
    sta NRSD_detectedPhaseASCII
    rts
@updateNRSD_testResults:
    @testBits = NRSD_tmp
.repeat NRSD_NUM_TEST_BYTES, I
    lda NRSD_testResults+I
    sta @testBits
.repeat 8, J
    asl @testBits
    lda #0
    rol
    adc #'0'
    sta NRSD_testResultsASCII+8*I+J
.endrep
.endrep
    rts

;
; Detects whether running on NTSC or PAL
;
; TODO: Detect clone systems with mixed cycle characteristics.
;
NRSD_DetectSystem:
:       
    lda $2002
    bpl :-
:   lda $2002
    bpl :-
        
    lda #0
    sta NRSD_tmp
    sta NRSD_tmp+1
:
    lda NRSD_tmp
    clc
    adc #1
    sta NRSD_tmp
    lda NRSD_tmp+1
    adc #0
    sta NRSD_tmp+1
    lda $2002
    bpl :-
    lda NRSD_tmp
    sec
    sbc #<1250
    lda NRSD_tmp+1
    sbc #>1250
    bpl :+
    lda #NRSD_SYSTEM_NTSC
    sta NRSD_detectedSystem
    rts
:
    lda #NRSD_SYSTEM_PAL
    sta NRSD_detectedSystem
    rts

NRSD_DetectPattern:
    @patternCount   = NRSD_tmp+1
    @byteCount      = NRSD_tmp
    sta @patternCount
@patternLoop:
    lda #NRSD_NUM_BYTES_PER_TEST
    sta @byteCount
    tya
    pha
@byteLoop:
    lda NRSD_testPatterns,x
    cmp NRSD_testResults,y
    bne @misMatch
    inx
    iny
    dec @byteCount
    bne @byteLoop
    pla
    tay
    ; Entire pattern matched. Decrement last increased index and return success
    dex
    dex
    dex
    dex
    sec
    rts

@misMatch:
    ; Pattern mismatch. Skip to next pattern
    inx
    iny
    dec @byteCount
    bne @misMatch
    pla
    tay
    dec @patternCount
    bne @patternLoop
    ; No pattern matched
    clc
    rts

;
; Detects bit pattern for each test, assigning letters to them:
;
; 2005: ABCD
; 2006: GH
; CHRI: MN
; SPON: STU
;
NRSD_DetectPatterns:
    ldy #0
@testLoop:
    tya
    pha
    lda @patternInfo_patternsOffset,y
    tax
    lda @patternInfo_numPatterns,y
    pha
    tya
    asl
    asl
    tay
    pla
    jsr NRSD_DetectPattern
    pla
    tay
    lda #'?'
    bcc :+
    txa
    sec
    sbc @patternInfo_patternsOffset,y
    lsr
    lsr
    clc
    adc @patternInfo_baseChar,y
:
    sta NRSD_detectedPattern,y
    iny
    cpy #NRSD_NUM_TESTS
    bne @testLoop
    rts

@patternInfo_baseChar:
.byte 'A', 'G', 'M', 'S'
@patternInfo_patternsOffset:
.byte NRSD_testPattern_A-NRSD_testPatterns, NRSD_testPattern_G-NRSD_testPatterns, NRSD_testPattern_M-NRSD_testPatterns, NRSD_testPattern_S-NRSD_testPatterns
@patternInfo_numPatterns:
.byte 4, 2, 2, 3

NRSD_testPatterns:
;
; *** 2005-test ***
;
NRSD_testPattern_A:
.byte %01111111
.byte %01011011
.byte %01111111
.byte %01011011
NRSD_testPattern_B:
.byte %01111111
.byte %01011011
.byte %01011011
.byte %01001001
NRSD_testPattern_C:
.byte %01111111
.byte %01111111
.byte %01111111
.byte %01011011
NRSD_testPattern_D:
.byte %01111111
.byte %01011111
.byte %01111111
.byte %01011011
;
; *** 2006-test ***
;
NRSD_testPattern_G:
.byte %10111001
.byte %10010000
.byte %10111001
.byte %10010000
NRSD_testPattern_H:
.byte %10010000
.byte %00000000
.byte %10010000
.byte %00000000
;
; *** CHRI-test ***
;
NRSD_testPattern_M:
.byte %11110000
.byte %11110100
.byte %11110000
.byte %11110100
NRSD_testPattern_N:
.byte %11110100
.byte %11110110
.byte %11110100
.byte %11110110
;
; *** SPON-test ***
;
NRSD_testPattern_S:
.byte %10111111
.byte %10011111
.byte %10111111
.byte %10011111
NRSD_testPattern_T:
.byte %10011111
.byte %00001111
.byte %10011111
.byte %00001111
NRSD_testPattern_U:
.byte %10011111
.byte %00101111
.byte %10011111
.byte %00111111

;
; Detects phase by matching against specific patterns
;
NRSD_DetectPhase:
    ldx #0
@patternLoop:
    lda @testResultsTable+0,x
    cmp NRSD_detectedPattern+0
    bne @noMatch
    lda @testResultsTable+1,x
    cmp NRSD_detectedPattern+1
    bne @noMatch
    lda @testResultsTable+2,x
    cmp NRSD_detectedPattern+2
    bne @noMatch
    lda @testResultsTable+3,x
    cmp NRSD_detectedPattern+3
    bne @noMatch
    ; All 4 patterns matched - lookup phase
    txa
    lsr
    lsr
    tax
    lda @phaseLookup,x
    sta NRSD_detectedPhase
    rts
@noMatch:
    inx
    inx
    inx
    inx
    cpx #@testResultsTable_end-@testResultsTable
    bne @patternLoop
    ; No patterns matched - set phase as undetected
    lda #$FF
    sta NRSD_detectedPhase
    rts

@testResultsTable:
@testPatterns0:     ; Phase0
.byte 'A'
.byte 'G'
.byte 'M'
.byte 'S'
@testPatterns1:     ; Phase1
.byte 'A'
.byte 'G'
.byte 'N'
.byte 'S'
@testPatterns2:     ; Phase2
.byte 'B'
.byte 'H'
.byte 'N'
.byte 'T'
@testPatterns3:     ; Phase3
.byte 'C'
.byte 'G'
.byte 'M'
.byte 'U'
@testPatterns4:     ; Phase3 alternate patterns
.byte 'D'
.byte 'G'
.byte 'M'
.byte 'U'
@testPatterns5:     ; Phase1 alternate patterns
.byte 'B'
.byte 'G'
.byte 'N'
.byte 'S'
@testPatterns6:     ; Phase3 alternate patterns
.byte 'C'
.byte 'G'
.byte 'M'
.byte 'S'
@testPatterns7:     ; Phase3 alternate patterns
.byte 'D'
.byte 'G'
.byte 'M'
.byte 'S'
@testResultsTable_end:

@phaseLookup:
.byte 0, 1, 2, 3, 3, 1, 3, 3

;
; Writes the test results to fixed positions of nametable
;
; (takes 615 cycles)
;
NRSD_WriteTestResultsToNameTable:
    ; Write system name
    lda #>($2000 + 32 * NRSD_TEXT_SYSTEM_Y + NRSD_TEXT_SYSTEM_X)
    sta $2006
    lda #<($2000 + 32 * NRSD_TEXT_SYSTEM_Y + NRSD_TEXT_SYSTEM_X)
    sta $2006
.repeat 4, I
    lda NRSD_detectedSystemASCII+I
    sta $2007
.endrep
    ; Write reset phase name
    lda #>($2000 + 32 * NRSD_TEXT_PHASE_Y + NRSD_TEXT_PHASE_X)
    sta $2006
    lda #<($2000 + 32 * NRSD_TEXT_PHASE_Y + NRSD_TEXT_PHASE_X)
    sta $2006
    lda NRSD_detectedPhaseASCII
    sta $2007
    ; Write test results
.repeat NRSD_NUM_TEST_BYTES, I
    lda #>($2000 + 32 * (NRSD_TEXT_TESTS_Y + I) + NRSD_TEXT_TESTS_X)
    sta $2006
    lda #<($2000 + 32 * (NRSD_TEXT_TESTS_Y + I) + NRSD_TEXT_TESTS_X)
    sta $2006
.repeat 8, J
    lda NRSD_testResultsASCII+8*I+J
    sta $2007
.endrep
.endrep
    ; Write detected patterns
.repeat NRSD_NUM_TESTS, I
    lda #>($2000 + 32 * (NRSD_TEXT_TESTS_Y + NRSD_NUM_BYTES_PER_TEST*I + 1) + NRSD_TEXT_TESTS_X + 10)
    sta $2006
    lda #<($2000 + 32 * (NRSD_TEXT_TESTS_Y + NRSD_NUM_BYTES_PER_TEST*I + 1) + NRSD_TEXT_TESTS_X + 10)
    sta $2006
    lda NRSD_detectedPattern+I
    sta $2007
.endrep
    rts

.align $100
;
; Delays for a large number of scanlines.
;
; 21733 cycles (including JSR and RTS)
;
; Adjust this if you wish to do background tasks during detection.
;
NRSD_DelayUntilTestExecution:
    ldy #17
    ldx #234
:
    dex
    bne :-
    dey
    bne :-
    rts

;
; Delay for 110 cycles (including JSR and RTS)
;
NRSD_Delay_110:
    nop
    ldx #19
:
    dex
    bne :-
    rts

;
; Delay for 290 cycles (including JSR and RTS)
;
NRSD_Delay_290:
    ldx $00
    nop
    nop
    ldx #54
:
    dex
    bne :-
    rts

NRSD_WriteNameTableColumn:
    ; inc-by-32
    lda #$04
    sta $2000
    lda #>($2000 + NRSD_SPRITE0_HIT_BG_COLUMN)
    sta $2006
    lda #<($2000 + NRSD_SPRITE0_HIT_BG_COLUMN)
    sta $2006
    lda #NRSD_SPRITE0_HIT_BG_TILE_INDEX
    ldy #0
:
    sta $2007
    iny
    cpy #30
    bne :-
    ; inc-by-1
    lda #$00
    sta $2000
    rts

NRSD_ClearOAM:
    lda #$F0
    ldx #0
:
    sta sprites+0,x
    inx
    inx
    inx
    inx
    bne :-
    lda #0
:
    sta sprites+1,x
    sta sprites+2,x
    sta sprites+3,x
    inx
    inx
    inx
    inx
    bne :-
    rts

;
; Initializes library for testing
;
NRSD_Initialize:
    lda #<NRSD_nmi
    sta NRSD_nmiAddr
    lda #>NRSD_nmi
    sta NRSD_nmiAddr+1
:
    lda $2002
    bpl :-
:
    lda $2002
    bpl :-
    ; Detect NTSC/PAL system
    jsr NRSD_DetectSystem
    ; Setup delay
    lda #NRSD_INIT_FINE_DELAY
    sta NRSD_fineDelay
    lda #NRSD_INIT_SCROLL_X
    sta NRSD_ScrollX
    ; Copy ROM to RAM to enable self-modifying code
    jsr NRSD_CopyHBlankCodeToRAM
    jsr NRSD_WriteNameTableColumn
    jsr NRSD_ClearOAM
    NRSD_SET_SPRITE 0, #NRSD_SPRITE0_XPOS, #NRSD_SPRITE0_YPOS, #NRSD_SPRITE0_TILE_INDEX
    ; Disable sprites / BG in leftmost column
    lda #$18
    sta NRSD_r2001
    lda #0
    sta NRSD_ScrollY
    jsr NRSD_UpdateTestParameters
    ; Sync. with PPU
    jsr init_nmi_sync
    ; Enable NMI
    lda #$90
    sta $2000
    rts
