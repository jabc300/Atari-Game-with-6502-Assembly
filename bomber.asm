    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required files with register mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare the variables starting from memory address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80

JetXPos             byte        ; player0 x-position
JetYPos             byte        ; player0 y-position
BomberXPos          byte        ; player1 x-position
BomberYPos          byte        ; player1 y-position
MissileXPos         byte        ; missile x-position
MissileYPos         byte        ; missile y-position
Score               byte        ; 2-digit score stored as BCD
Timer               byte        ; 2-digit timer stored as BCD
Temp                byte        ; auxiliary variable to store temp score values
OnesDigitOffset     word        ; lookup table offset for the score 1's digit
TensDigitOffset     word        ; lookup table offset for the score 10's digit
JetSpritePtr        word        ; pointer to player0 sprite lookup table
JetColorPtr         word        ; pointer to player0 color lookup table
BomberSpritePtr     word        ; pointer to player1 sprite lookup table
BomberColorPtr      word        ; pointer to player1 color lookup table
JetAnimOffset       byte        ; player0 sprite frame offset for animation
Random              byte        ; random number generated to set enemy position
ScoreSprite         byte        ; store the sprite bit pattern for the score
TimerSprite         byte        ; store the sprite bit pattern for the timer
TerrainColor        byte        ; store the color of the terrain
RiverColor          byte        ; store the color of the river

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT = 9                  ;player0 sprite height (# rows in lookup table)
BOMBER_HEIGHT = 9               ;player1 sprite height (# rows in lookup table)
DIGITS_HEIGHT = 5               ;scoreboard digit height (#rows in lookup table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg Code
    org $F000

Reset:
    CLEAN_START                 ;call macro to reset memory AND registers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #68
    STA JetXPos                 ;JetYPos = 10
    LDA #10
    STA JetYPos                 ;JetXPos = 60
    LDA #62
    STA BomberXPos
    LDA #83
    STA BomberYPos
    LDA #%11010100
    STA Random                  ;Random = $D4
    LDA #0
    STA Score                   ;Score = 0
    STA Timer                   ;Timer = 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare a MACRO to check if we should display the missile 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    MAC DRAW_MISSILE
        LDA #%00000000
        CPX MissileYPos         ; compare X (current scanline) with missile Y pos
        BNE .SkipMissileDraw    ; if x != MissileYPos, then skip draw
.DrawMissile:
        LDA #%00000010          ; else: enable missile 0 display
        INC MissileYPos         ; MissileYPos++
.SkipMissileDraw:
        STA ENAM0               ; store the correct value in the TIA missile register
    ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the pointers to the correct lookup table addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #<JetSprite
    STA JetSpritePtr            ;store low byte
    LDA #>JetSprite
    STA JetSpritePtr+1          ;store high byte
    
    LDA #<JetColor
    STA JetColorPtr             ;store low byte
    LDA #>JetColor
    STA JetColorPtr+1           ;store high byte
    
    LDA #<BomberSprite
    STA BomberSpritePtr         ;store low byte
    LDA #>BomberSprite
    STA BomberSpritePtr+1       ;store high byte
    
    LDA #<BomberColor
    STA BomberColorPtr          ;store low byte
    LDA #>BomberColor
    STA BomberColorPtr+1        ;store high byte

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #2
    STA VBLANK                  ;turn on VBLANK
    STA VSYNC                   ;turn on VSYNC
    REPEAT 3
        STA WSYNC               ;display 3 recommended lines of VSYNC
    REPEND
    LDA #0
    STA VSYNC
    REPEAT 31
        STA WSYNC               ;display the recommended lines of VBLANK
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and task performed in the pre-VBlank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA JetXPos
    LDY #0
    JSR SetObjectXPos           ; set player0 horizontal position
    
    LDA BomberXPos
    LDY #1
    JSR SetObjectXPos           ; set player1 horizontal position
    
    LDA MissileXPos
    LDY #2
    JSR SetObjectXPos           ; set missile horizontal position
    
    JSR CalculateDigitOffset    ; Calculate the scoreboard digit lookup table offset
    
    JSR GenerateJetSound        ;Configure and enable our jet engine audio

    STA WSYNC
    STA HMOVE                   ; apply the horizontal offset previously set

    LDA $0
    STA VBLANK                  ; Turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA $0                      ; clear TIA registers before each new frame
    STA COLUBK
    STA PF0
    STA PF1
    STA PF2
    STA GRP0
    STA GRP1
    STA CTRLPF
    STA COLUBK                  ;reset TIA registers before displaying the score
    
    LDA #$1E
    STA COLUPF                  ;Set the scoreboard playfield color with yellow

    LDX #DIGITS_HEIGHT          ;start X counter with 5 (height of digits)
    
.ScoreDigitLoop:
    LDY TensDigitOffset         ;Get the tens digit offset for the Score
    LDA Digits,Y                ;load the bit pattern from the lookup table
    AND #$F0                    ;mask/ remove the graphics for the ones digit
    STA ScoreSprite             ;save the score tens digit pattern in a variable
    
    LDY OnesDigitOffset         ;Get the ones digit offset for the Score
    LDA Digits,Y                ;load the digit bit pattern from lookup table
    AND #$0F                    ;mask/remove the graphics for the tens digit
    ORA ScoreSprite             ;merge it with the saved tens digit sprite
    STA ScoreSprite             ;and save it
    STA WSYNC                   ;wait for the end of scanline
    STA PF1                     ;update the playfield to display the Score sprite
    
    LDY TensDigitOffset+1       ; get the left digit offset for the Timer
    LDA Digits,Y                ; load the digit pattern from lookup table
    AND #$F0                    ; mask/remove the graphics for the ones digit
    STA TimerSprite             ; save the timer tens digit pattern in a variable
    
    LDY OnesDigitOffset+1       ;get the left digit offset for the Timer
    LDA Digits,Y                ; load the digit pattern from the lookup table
    AND #$0F                    ; mask/remove the graphics for the tens digit
    ORA TimerSprite             ;merge with the saved tens digit graphics
    STA TimerSprite             ;and save it
    
    JSR Sleep12Cycles           ;waste some cycles
    
    STA PF1                     ;update the playfield for Timer display
    
    LDY ScoreSprite             ;preload for the next scanline
    STA WSYNC                   ;wait for next scanline
    
    STY PF1                     ;update playfield for the score display
    INC TensDigitOffset
    INC TensDigitOffset+1
    INC OnesDigitOffset
    INC OnesDigitOffset+1       ;increment all digits for the next line of data
    
    JSR Sleep12Cycles           ;waste some cycles
    
    DEX                         ; X--
    STA PF1                     ; update the playfield for the Timer display
    BNE .ScoreDigitLoop         ; if dex != 0 then branch to ScoreDigitLoop
    
    STA WSYNC
    
    LDA #0
    STA PF0
    STA PF1
    STA PF2
    STA WSYNC
    STA WSYNC
    STA WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the remaining visible scanlines of our game (2-line kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLines:
    LDA TerrainColor
    STA COLUPF                  ; set the terrain background color
    
    LDA RiverColor
    STA COLUBK                  ; set the river background color
    
    LDA #%00000001
    STA CTRLPF                  ; enagle playfield reflection
    LDA #$F0
    STA PF0                     ; setting PF0 bit pattern
    LDA #$FC
    STA PF1                     ; setting PF1 bit pattern
    LDA #0
    STA PF2                     ; setting PF2 bit pattern

    LDX #85                     ; x counts the number of remaining scanlines
.GameLineLoop:
    DRAW_MISSILE                ; macro the check if we should draw the missile

.AreWeInsideJetSprite:
    TXA
    SEC
    SBC JetYPos
    CMP #JET_HEIGHT             ;are we inside the sprite height bounds
    BCC .DrawSpriteP0           ;if result < SpriteHeight, call the draw routine
    LDA #0                      ;else, set lookup index to zero
.DrawSpriteP0:
    clc                         ; clears carry flag before addition
    adc JetAnimOffset           ; jumps to correct sprite frame in memory
    TAY
    LDA (JetSpritePtr),Y
    STA WSYNC
    STA GRP0
    LDA (JetColorPtr),Y
    STA COLUP0
    
.AreWeInsideBomberSprite:
    TXA
    SEC
    SBC BomberYPos
    CMP #BOMBER_HEIGHT          ;are we inside the sprite height bounds
    BCC .DrawSpriteP1           ;if result < SpriteHeight, call the draw routine
    LDA #0                      ;else, set lookup index to zero
.DrawSpriteP1:
    TAY
    
    LDA #%00000101
    STA NUSIZ1
    
    LDA (BomberSpritePtr),Y
    STA WSYNC
    STA GRP1
    LDA (BomberColorPtr),Y
    STA COLUP1

    DEX                         ;X--
    BNE .GameLineLoop           ;repeat next main game scanline until finished
    
    LDA #0
    STA JetAnimOffset           ;reset jet animation fram to zero each frame
    
    STA WSYNC                   ; wait for a scanline
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #2
    STA VBLANK                  ;turn VBLANK on agaim
    REPEAT 30
        STA WSYNC               ;display 30 recommended lines of VBLANK Overscan
    REPEND
    LDA #0
    STA VBLANK                  ;turn off BLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process joystick input for player0 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    LDA #%00010000              ;player0 joystick up
    BIT SWCHA
    BNE CheckP0Down             ;if bit pattern doesn't match, bypass Up block
.P0UpPressed:    
    LDX JetYPos
    CPX #$4D
    BPL .SkipYINC
    INC JetYPos
.SkipYINC:
    LDA #0
    STA JetAnimOffset           ;reset sprite animation to first frame
    
CheckP0Down:
    LDA #%00100000              ;player0 joystick down
    BIT SWCHA
    BNE CheckP0Left             ;if bit pattern doesn't match, bypass Down block
.P0DownPressed:
    LDX JetYPos
    CPX #1
    BMI .SkipYDEC
    DEC JetYPos
.SkipYDEC:
    LDA #0
    STA JetAnimOffset           ;reset sprite animation to first frame
    
CheckP0Left:
    LDA #%01000000              ; player 0 joystick left
    BIT SWCHA
    BNE CheckP0Right            ; if bit pattern doesn't match, bypass Left block 
.P0LeftPressed:  
    LDX JetXPos
    CPX #$1F
    BMI .SkipXDEC 
    DEC JetXPos
.SkipXDEC:
    LDA #JET_HEIGHT              ; 9
    STA JetAnimOffset           ; set animation offset to the second frame
    
CheckP0Right:
    LDA #%10000000              ; player 0 joystick right
    BIT SWCHA
    BNE CheckButtonPressed      ; if bit patter doesn't match, bypass Right block
.P0RightPressed:
    LDX JetXPos
    CPX #$67
    BPL .SkipXINC
    INC JetXPos
.SkipXINC:
    LDA #JET_HEIGHT             ; 9
    STA JetAnimOffset           ; set animation offset to the second frame

CheckButtonPressed:
    LDA #%10000000              ; If button is pressed
    BIT INPT4
    BNE EndInputCheck
.ButtonPressed:    
    LDA JetXPos
    CLC
    ADC #5
    STA MissileXPos             ; set the missile X position equal to the player 0

    LDA JetYPos
    CLC
    ADC #8
    STA MissileYPos             ; set the missile Y position equal to the player 0
    
EndInputCheck:                  ; fallback when no input was performed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations to update position for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateBomberPosition:
    LDA BomberYPos
    CLC
    CMP #0
    BMI .ResetBomberPosition    ; if it is < 0, then reset y-position to the top
    DEC BomberYPos              ; else, decrement enemy y-position
    JMP EndPositionUpdate
.ResetBomberPosition
    JSR GetRandomBomberPos      ;call subroutine for next random x-position
    
.SetScoreValues:
    SED                         ; set BCD for score and timer values
    LDA Timer
    CLC
    ADC #1
    STA Timer                   ; add 1 to the Timer (BCD does not like INC)
    CLD                         ; disable BCD after updating Score and Timer

EndPositionUpdate:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for object collision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0P1:
    LDA #%10000000              ; CXPPMM bit 7 detects P0 and P1 collision
    BIT CXPPMM                  ; check CXPPMM bit 7 with the above pattern
    BNE .P0P1Collided           ; if collision between P0 and P1 happened
    JSR SetTerrainRiverColor    ; else, set playfield color green/blue
    JMP CheckCollisionM0P1      ; else, skip to the next check
.P0P1Collided:
    JSR GameOver                ; call GameOver subroutine

CheckCollisionM0P1:
    LDA #%10000000              ; CXM0P bit 7 detects M0 and P1 collision
    BIT CXM0P                   ; check CXM0P register bit 7 the above pattern
    BNE .M0P1Collided           ; collision missile 0 and player 1 happened
    JMP EndCollisionCheck
.M0P1Collided:
    SED
    LDA Score
    CLC
    ADC #1
    STA Score
    CLD                         ; adds 1 to the Score using decimal mode
    LDA #0
    STA MissileYPos             ; reset the missile position

EndCollisionCheck:              ; fallback
    STA CXCLR                   ; clear all collision flags before next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    JMP StartFrame              ;continue to display the next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate audio for the jet engine sound based on the jet y position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GenerateJetSound subroutine
    LDA #1
    STA AUDV0                   ; set the new audio volume register
    
    LDA JetYPos                 ; loads the accumulator with the jet-y position
    LSR
    LSR
    LSR                         ; divide the accumulator by 8 (using right-shifts)
    STA Temp                    ; save the Y/8 in a Temp variable
    LDA #31
    SEC
    SBC Temp                    ; subtract 31-(Y/8)
    STA AUDF0                   ; set the new audio frequency/pitch register
    
    LDA #8
    STA AUDC0                   ; set the new audio tone type register

    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the colors for the terrain and river to green & blue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetTerrainRiverColor subroutine
    LDA #$C2
    STA TerrainColor            ; set terrain color to green
    LDA #$84
    STA RiverColor              ; set river color to blue
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle object horizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A is the target x-coordinate position in pixels of our object
;; Y is the object type (0:player0, 1:player1, 2:missile0, 3:missile1, 4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
    STA WSYNC
    SEC
.Div15Loop
    SBC #15
    BCS .Div15Loop
    EOR #7
    ASL
    ASL
    ASL
    ASL
    STA HMP0,y
    STA RESP0,Y
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver subroutine
    LDA #$30
    STA TerrainColor            ; set terrain color to red
    STA RiverColor              ; set river color to red
    LDA #0
    STA Score                   ; Score = 0
    JSR GetRandomBomberPos      ;call subroutine for next random x-position
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to generate a Linear-Feedback Shift Register random number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate a LFSR random number
;; Divide the random value by 4 to limit the size of the result to match river
;; Add 30 to compensate for the left green playfield
;; The routine also sets the Y-position of the bomber to the top of the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomBomberPos subroutine
    LDA Random
    ASL
    EOR Random
    ASL
    EOR Random
    ASL
    ASL
    EOR Random
    ASL
    ROL Random                  ;performs a series of shifts and bit operations
    LSR                         ;divide the value by 4 with 2 right shifts
    LSR
    STA BomberXPos              ;save it to the variable BomberXPos
    LDA #30
    ADC BomberXPos              ;adds 30 + BomberXPos to compensate for left PF
    STA BomberXPos
    
    LDA #96
    STA BomberYPos              ;set the y-position to the top of the screen
    
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle scoreboard digits to be displayed on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The scoreboard is stored using BCD, so the display shows hex numbers.
;; Convert the high AND low nibbles of the variable Score and Timer
;; into the offsets of digits lookup table so the values can be displayed.
;; Each digit has a height of 5 bytes in the lookup table
;;
;; For the low nibble we need to multiply by 5
;;  - we can use left shift to perform multiplication by 2
;;  - for any number N, the value of N*5 = (N*2*2)+N
;;
;; For the upper nibble, since its already times 16. we need to divide it
;; and then multiply by 5: 
;;  - we can use the right shift to perform division by 2
;;  - for any number, the value of (N/16)*5 = (N/2/2) + (N/2/2/2/2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateDigitOffset subroutine
    LDX #1                      ;x register is the loop counter
.PrepareScoreLoop               ;this will loop twice, first X=1, AND then X=0

    LDA Score,X                 ;Load A with Timer (X=1) or Score (X=0)
    AND #$0F                    ;remove the tens digits by masking 4 bits 00001111
    STA Temp                    ;save the value of A into Temp
    ASL                         ;shift left (it is now N*2)
    ASL                         ;shift left (it is now N*4)
    ADC Temp                    ;add the value saved in Temp(+N)
    STA OnesDigitOffset,X       ;save A in OnesDigitOfsset+1 or OnesDigitOfsset+0
    
    LDA Score,X                 ;load A with Timer (X=1) or Score(X=0)
    AND #$F0                    ;remove the ones digits by masking 4 bits 11110000
    LSR                         ;shift right (it is now N/2)
    LSR                         ;shift right (it is now N/4)
    STA Temp                    ;Save the value of A into Temp
    LSR                         ;shift right (it is now N/8)
    LSR                         ;shift right (it is now N/16)
    ADC Temp                    ;add the value saved in Temp (N/16 + N/4)
    STA TensDigitOffset,X       ;store A in TensDigitOffset+1 or TensDigitOffset+0
    
    DEX                         ;X--
    BPL .PrepareScoreLoop       ;while X >=0 loop to pass a second time
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to waste 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSR takes 6 cycles
;; RTS takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #

JetSprite:
    .byte #%00000000
    .byte #%00010100
    .byte #%01111111
    .byte #%00111110
    .byte #%00011100
    .byte #%00011100
    .byte #%00001000
    .byte #%00001000
    .byte #%00001000

JetSpriteTurn:
    .byte #%00000000
    .byte #%00001000
    .byte #%00111110
    .byte #%00011100
    .byte #%00011100
    .byte #%00011100
    .byte #%00001000
    .byte #%00001000
    .byte #%00001000

BomberSprite:
    .byte #%00000000
    .byte #%00001000
    .byte #%00001000
    .byte #%00101010
    .byte #%00111110
    .byte #%01111111
    .byte #%00101010
    .byte #%00001000
    .byte #%00011100

JetColor:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$BA
    .byte #$0E
    .byte #$08

JetColorTurn:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$0E
    .byte #$0E
    .byte #$08

BomberColor:
    .byte #$00
    .byte #$32
    .byte #$32
    .byte #$0E
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size with exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    word Reset
    word Reset