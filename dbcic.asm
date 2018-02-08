;************************************************************************
;   file dbcic.asm
;   author René Richard
;   brief
;	NES multi-region CIC heavily inspired from SUPER CIC which is itself
;	based on reverse engineering work and disassembly by segher,
;	http://hackmii.com/2010/01/the-weird-and-wonderful-cic/
;   
;   Copyright (C) 2018 by René Richard (db) <rene@db-electronics.ca>
;
;   This program is free software: you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation, either version 3 of the License, or
;   (at your option) any later version.
    
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.

;   You should have received a copy of the GNU General Public License
;   along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;************************************************************************
;
;   pinout :  CIC
;
;                   ,-------_-------.
;      +5V          |1 Vdd     GND 8| GND
;      CIC clk [71] |2 GP5     GPO 7| CIC data out    [35]
;        status out |3 GP4     GP1 6| CIC data in     [34]
;                   |4 GP3     GP2 5| CIC slave reset [70]
;                   `-------'-------
;


lockseed    equ	    0x20	; 0x20 seed calc and xfr, 0x21-0x2F seed area
keyseed	    equ	    0x30	; 0x30 seed calc and xfr, 0x31-0x3F seed area
xreg	    equ	    0x50
eereg	    equ	    0x51
swait	    equ	    0x52
lwait	    equ	    0x53
	    
dout	    equ	    0		; data out is bit0 of GPIO
din	    equ	    1		; data in is bit1 of GPIO
led	    equ	    4
	    
;************************************************************************
#include <p12f629.inc>
;************************************************************************
    __CONFIG _EC_OSC & _WDT_OFF & _PWRTE_OFF & _MCLRE_OFF & _CP_OFF & _CPD_OFF 
;************************************************************************

;************************************************************************
; macros
loadlock    macro L1,L2,L3,L4,L5,L6,L7,L8,L9,LA,LB,LC,LD,LE,LF
	movlw	L1
	movwf	lockseed+0x1
	movlw	L2
	movwf	lockseed+0x2
	movlw	L3
	movwf	lockseed+0x3
	movlw	L4
	movwf	lockseed+0x4
	movlw	L5
	movwf 	lockseed+0x5
	movlw	L6
	movwf 	lockseed+0x6
	movlw	L7
	movwf 	lockseed+0x7
	movlw	L8
	movwf 	lockseed+0x8
	movlw	L9
	movwf 	lockseed+0x9
	movlw	LA
	movwf 	lockseed+0xA
	movlw	LB
	movwf 	lockseed+0xB
	movlw	LC
	movwf 	lockseed+0xC
	movlw	LD
	movwf 	lockseed+0xD
	movlw	LE
	movwf 	lockseed+0xE
	movlw	LF
	movwf 	lockseed+0xF
endm

loadkey	    macro K2,K3,K4,K5,K6,K7,K8,K9,KA,KB,KC,KD,KE,KF
	movlw	K2
	movwf	keyseed+0x2
	movlw	K3
	movwf	keyseed+0x3
	movlw	K4
	movwf	keyseed+0x4
	movlw	K5
	movwf 	keyseed+0x5
	movlw	K6
	movwf 	keyseed+0x6
	movlw	K7
	movwf 	keyseed+0x7
	movlw	K8
	movwf 	keyseed+0x8
	movlw	K9
	movwf 	keyseed+0x9
	movlw	KA
	movwf 	keyseed+0xA
	movlw	KB
	movwf 	keyseed+0xB
	movlw	KC
	movwf 	keyseed+0xC
	movlw	KD
	movwf 	keyseed+0xD
	movlw	KE
	movwf 	keyseed+0xE
	movlw	KF
	movwf 	keyseed+0xF
endm

; the code treats the FSR as the BM:BL register
; seed memory starts at 0x20, therefore, 
	; clearing bit4 = lbmi 0
	; setting bit4 = lbmi 1
lbmi_0	macro
	bcf	FSR, 4	    
endm    
	
lbmi_1	macro
	bsf	FSR, 4
endm
	
;************************************************************************
; program start
	org	0x0000
	goto	main
isr
	org	0x0004
	bcf	INTCON, 1	; clear interrupt flag
	bsf	INTCON, 7	; re-enable interrupts

; 2 cycles to here from POR or ISR
main				
	banksel GPIO
	clrf	GPIO		; clear the output port
	movlw	0x07		; GPIO2..0 = digital I/O
	movwf	CMCON
	movlw	0x90		; global enable interrupts + enable external interrupt
	movwf	INTCON
	banksel	TRISIO
	movlw	0b000101110	; 5 = in, 4 = out, 3 = in, 2 = in, 1 = in, 0 = out
	movwf	TRISIO
	movlw	0x24		; weak pull-up on 5 and 2
	movwf	WPU
	movlw	0x00		; pull-up enable, GP2 falling edge int
	movwf	OPTION_REG
	banksel GPIO

; 16 from POR to cycles to here
; timing critical section here,
; lock sends stream ID. 15 cycles per bit--------
; stream id read at 34th, 49th, 64th and 79th cycles
	
; burn 18 cycles
	movlw	0x4		; wait = (3*W) + 5
	call	wait		; burn 17 cycles


	btfsc	GPIO, din	; check stream ID bit
	bsf	0x31, 3		; copy to lock seed
	movlw	0x02		; wait=3*W+5
	call	wait		; burn 11 cycles
	nop
	nop

	btfsc	GPIO, din	; check stream ID bit
	bsf	0x31, 0		; copy to lock seed
	movlw	0x02		;
	call	wait		; burn 11 cycles
	nop
	nop

	btfsc	GPIO, din	; check stream ID bit
	bsf	0x31, 1		; copy to lock seed
	movlw	0x02		;
	call	wait		; burn 11 cycles
	nop
	nop

	btfsc	GPIO, din	; check stream ID bit
	bsf	0x31, 2		; copy to lock seed

; 80 cycles to here
; both seeds must be loaded within cycle 154
; 154 - 80 = 74 cycles to load
; curent region is stored in eeprom, load and call proper loading subroutine
	banksel	EEADR		; 1
	movlw	0x00		; 1 - point to region byte
	movwf	EEADR		; 1
	bsf	EECON1, RD	; 1 - read eeprom
	movf	EEDATA, W	; 1 - region indicator in wreg

; 85 cycles
; 0x00 = 3193 - USA/Canada
; 0x01 = 3195 - Europe
; 0x02 = 3196 - Asia 
; 0x03 = 3197 - UK/Italy/Australia
	
	addwf	PCL, f		; 2 - computed goto for proper lock/key load
	goto	load3193	; 2 + 60 - load USA/Canada seeds
	goto	load3195	; 2 + 60 - load Europe seeds
	goto	load3196	; 2 + 60 - load Asia seeds
	goto	load3197	; 2 + 60 - load UK/Italy/Australia seeds

; 149 cycles, 5 cycles to burn
doneload
	movlw	lockseed	; lbmi 0 - load ahead from mainloop 054
	movwf	FSR
	nop
	nop
	nop

;************************************************************************
; 154 cycles to main loop
mainloop
;051: 31      ldi 1		; A := 1
;028: 5c      lxa		; X := A
;054: 74      lbmi 0		; H := 0
	movlw	0x01		; ldi 1
	movwf	xreg		; lxa - load x with a
				; ahead by 1 cycle here
				; ** 156
				
;06a: 7c c7   tml 147		; call 147	// [H:0] := next stream bit
				; tml 147 = 10 cycles + 2 for call
	call	nextstreambit	; 10 cycles + 2 for call
				; ahead by 1 cycle here
				; ** 168
	
;01a: 75      lbmi 1		; H := 1
	lbmi_1			; setting bit 4 changes 0x20 to 0x30
;00d: 7c c7   tml 147		; call 147	// [H:0] := next stream bit
				; tml 147 = 10 cycles + 2 for call
	call	nextstreambit	; 10 cycles + 2 for call
				; ahead by 1 cycle here
				; ** 181

;003: 7c f4   tml 174		; H := 0 in key mode, 8 cycles	+ 2 for call
	lbmi_0			; FSR = lockseed, no need to check for lock
				; ahead by 8 + 2 = 10 cycles
				; ** 182
				
;020: 55      in		; A := P0
;050: 67      ska 3		; if A.3 = 0	// if key
;068: a7      t 027		;	goto 027
				; 3 cycles
				; ahead by 10 + 3 cycles
	
	movlw	0x2		; burn 13 cycles
	call	wait		; 
	nop
	nop
				; ** 195
			    ; in sync
	
;027: 30      ldi 0
;053: 41      x
	movlw	INDF		; x - partly
	clrf	INDF		; ldi, 0
				; ** 197
			    ; in sync
				
;069: 46      out		; P0 := [1:0]
;034: 55      in		; A := P0
;05a: 00      nop
;02d: 47      out0		; P0 := 0
	movwf	GPIO		; out
				; ** 198 GetDin() - tengenc says 195
	movfw	GPIO		; in
	nop			; nop
	bcf	GPIO, dout	; out0
				; ** 201
			    ; in sync
				
;016: 4a      s			; [1:0] := A
	movwf	INDF		; s
			    ; in sync

;00b: 75      lbmi 1		; 1
	lbmi_1
;045: 7c 83   tml 103		; 7 skip next instruction if lock, 5 cycles + 2 for call
;011: 74      lbmi 0		; 1 H := 0 - we are key so don't skip
			    ; 8 cycles ahead
			    
;008: 60      skm 0
	btfss	INDF, 0		; skm 0 
			    ; 8 cycles ahead
;044: 93      t 013
	goto	die		; 2 cycles
			    ; 8 cycles ahead without skip
			    
;062: 7c f4   tml 174		; H := 0 in key mode, 8 cycles	+ 2 for call
	lbmi_0			; 1- FSR = lockseed, no need to check for lock
			    ; 15 cycles ahead
;018: 61      skm 1
	btfss	INDF, 1		; skm 1
;04c: b3      t 033		// die
	goto	die
			    ; 15 cycles ahead
			    
	movlw	0x3		; burn 15 cycles
	call	wait		; 
	nop
;066: ca      t 04a		
			    
	
finished
	goto	finished
;************************************************************************
nextstreambit
;	[H:0] := NEXT STREAM BIT - 10 cycles either pass
;************************************************************************
;147: 5d      xax
;163: 5c      lxa
;171: 57      xal
;138: 40      l
;15c: 20      lbli 0
;16e: 64      ska 0
;137: 9b      t 11b	; if [H:X].0 = 1 {
;15b: 35      ldi 5
;16d: 4a      s		;	[H:0] := 5
;136: 4c      rit	;	return
;			; } else {
;11b: 30      ldi 0
;14d: 4a      s		;	[H:0] := 0
;126: 4c      rit	;	return
;			; }
	
	movfw	xreg		; xax - not really exchanging but its overwritten right after
	;movwf	xreg		; lxa - what's the point?
	addwf	FSR		; add x (a really) 
	btfss	INDF,0		; l, ska 0 - skip if bit0 = 0 
	goto	nsbskip		; t 11b
	movlw	0xF0		; lbli 0
	andwf	FSR		; lbli 0
	movlw	0x05		; ldi 5
	movwf	INDF		; s
	return
nsbskip
	movlw	0xF0		; lbli 0
	andwf	FSR		; lbli 0
	clrf	INDF		; ldi 0, s
	return

	
;************************************************************************
	
loop	
	movlw	0x1
loop0
	addlw	0x30	; key stream
	movwf	FSR	; store in index reg
loop1
	nop
	nop
	movf	INDF, w ; load seed value
	movwf	0x20
	bcf	0x20, 1	; clear bit 1 
	btfsc	0x20, 0 ; copy from bit 0
	bsf	0x20, 1 ; (if set)
	bsf	0x20, 4 ; LED on
	movf	0x20, w
	movwf	GPIO
	nop
	movlw	0x10
	movwf	GPIO	; reset GPIO
	movlw	0x14
	call	wait
	nop
	nop
	nop
	nop
	nop
	btfsc	GPIO, 0 ; both pins must be low...
	goto	die
	btfsc	GPIO, 1 ; ...when no bit transfer takes place
	goto	die	; if not -> lock cic error state -> die
	incf	FSR, f	; next one
	movlw	0xf
	andwf	FSR, w
	btfss	STATUS, Z	
	goto	loop1
	call	mangle
	call	mangle
	call	mangle
	movlw	0x2	; wait 10
	call	wait	;
	nop
	nop
	btfsc	0x37, 0
	goto	swap
	banksel	TRISIO
	bcf	TRISIO, 0
	bsf	TRISIO, 1
	goto	swapskip
swap
	banksel	TRISIO
	bsf	TRISIO, 0
	bcf	TRISIO, 1
	nop
swapskip
	banksel GPIO
	movf	0x37, w
	andlw	0xf
	btfss	STATUS, Z
	goto	loop0
	goto	loop

; --------calculate new seeds--------
; had to be unrolled because PIC has an inefficient way of handling
; indirect access, no post increment, etc.
mangle
	call	mangle_lock
	nop
	nop		    ; 84
mangle_key
	movf	0x2f, w
	movwf	0x20	
mangle_key_loop
	addlw	0x1
	addwf	0x21, f
	movf	0x22, w
	movwf	0x40
	movf	0x21, w
	addwf	0x22, f
	incf	0x22, f
	comf	0x22, f
	movf	0x23, w
	movwf	0x41	; store 23 to 41
	movlw	0xf
	andwf	0x23, f
	movf	0x40, w ; add 40(22 old)+23+#1 and skip if carry
	andlw	0xf
	addwf	0x23, f
	incf	0x23, f
	btfsc	0x23, 4
	goto	mangle_key_withskip
mangle_key_withoutskip
	movf	0x41, w ; restore 23
	addwf	0x24, f ; add to 24
	movf	0x25, w
	movwf	0x40	; save 25 to 40
	movf	0x24, w
	addwf	0x25, f
	movf	0x26, w
	movwf	0x41	; save 26 to 41
	movf	0x40, w ; restore 25
	andlw	0xf	; mask nibble
	addlw	0x8	; add #8 to HIGH nibble
	movwf	0x40
	btfss	0x40, 4 ; skip if carry to 5th bit
	addwf	0x26, w
	movwf	0x26

	movf	0x41, w ; restore 26
	addlw	0x1	; inc
	addwf	0x27, f	; add to 27

	movf	0x27, w ;
	addlw	0x1	; inc
	addwf	0x28, f ; add to 28

	movf	0x28, w ;
	addlw	0x1	; inc
	addwf	0x29, f ; add to 29

	movf	0x29, w ;
	addlw	0x1	; inc
	addwf	0x2a, f ; add to 2a

	movf	0x2a, w ;
	addlw	0x1	; inc
	addwf	0x2b, f ; add to 2b

	movf	0x2b, w ;
	addlw	0x1	; inc
	addwf	0x2c, f ; add to 2c

	movf	0x2c, w ;
	addlw	0x1	; inc
	addwf	0x2d, f ; add to 2d

	movf	0x2d, w ;
	addlw	0x1	; inc
	addwf	0x2e, f ; add to 2e

	movf	0x2e, w ;
	addlw	0x1	; inc
	addwf	0x2f, f ; add to 2f

	movf	0x20, w ; restore original 0xf
	andlw	0xf
	addlw	0xf
	movwf	0x20
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	btfss	0x20, 4 ; skip if half-byte carry
	goto mangle_return ; +2 cycles in return
	nop
	goto mangle_key_loop
; 69 when goto, 69 when return
; CIC has 78 -> 9 nops

mangle_key_withskip
	movf	0x41, w ; restore 23
	addwf	0x23, f ; add to 23
	movf	0x24, w
	movwf	0x40	; save 24 to 40
	movf	0x23, w
	addwf	0x24, f
	movf	0x25, w
	movwf	0x41	; save 25 to 41
	movf	0x40, w ; restore 24
	andlw	0xf	; mask nibble
	addlw	0x8	; add #8 to HIGH nibble
	movwf	0x40
	btfss	0x40, 4 ; skip if carry to 5th bit
	addwf	0x25, w
	movwf	0x25

	movf	0x41, w ; restore 25
	addlw	0x1	; inc
	addwf	0x26, f	; add to 26

	movf	0x26, w ;
	addlw	0x1	; inc
	addwf	0x27, f ; add to 27

	movf	0x27, w ;
	addlw	0x1	; inc
	addwf	0x28, f ; add to 28

	movf	0x28, w ;
	addlw	0x1	; inc
	addwf	0x29, f ; add to 29

	movf	0x29, w ;
	addlw	0x1	; inc
	addwf	0x2a, f ; add to 2a

	movf	0x2a, w ;
	addlw	0x1	; inc
	addwf	0x2b, f ; add to 2b

	movf	0x2b, w ;
	addlw	0x1	; inc
	addwf	0x2c, f ; add to 2c

	movf	0x2c, w ;
	addlw	0x1	; inc
	addwf	0x2d, f ; add to 2d

	movf	0x2d, w ;
	addlw	0x1	; inc
	addwf	0x2e, f ; add to 2e

	movf	0x2e, w ;
	addlw	0x1	; inc
	addwf	0x2f, f ; add to 2f

	movf	0x20, w ; restore original 0xf
	andlw	0xf
	addlw	0xf
	movwf	0x20
	bcf	GPIO, 0
	movf	GPIO, w
	movwf	0x5e
	btfss	GPIO, 3
	bsf	GPIO, 0
	movf	GPIO, w
	movwf	0x5f
	bcf	GPIO, 0
	btfsc	GPIO, 3
	clrf	0x5e
	nop
	btfss	0x20, 4 ; skip if half-byte carry
	goto mangle_return ; +2 cycles in return
	movf	0x20, w		; restore w (previously destroyed)
	goto mangle_key_loop
mangle_return
	return
; 73 when goto, 73 when return
	; missing NOPs?!
; CIC has 84 -> 11 nops

mangle_lock
	movf	0x3f, w
	movwf	0x30	
mangle_lock_loop
	addlw	0x1
	addwf	0x31, f
	movf	0x32, w
	movwf	0x40
	movf	0x31, w
	addwf	0x32, f
	incf	0x32, f
	comf	0x32, f
	movf	0x33, w
	movwf	0x41	; store 33 to 41
	movlw	0xf
	andwf	0x33, f
	movf	0x40, w ; add 40(32 old)+33+#1 and skip if carry
	andlw	0xf
	addwf	0x33, f
	incf	0x33, f
	btfsc	0x33, 4
	goto	mangle_lock_withskip
mangle_lock_withoutskip
	movf	0x41, w ; restore 33
	addwf	0x34, f ; add to 34
	movf	0x35, w
	movwf	0x40	; save 35 to 40
	movf	0x34, w
	addwf	0x35, f
	movf	0x36, w
	movwf	0x41	; save 36 to 41
	movf	0x40, w ; restore 35
	andlw	0xf	; mask nibble
	addlw	0x8	; add #8 to HIGH nibble
	movwf	0x40
	btfss	0x40, 4 ; skip if carry to 5th bit
	addwf	0x36, w
	movwf	0x36

	movf	0x41, w ; restore 36
	addlw	0x1	; inc
	addwf	0x37, f	; add to 37

	movf	0x37, w ;
	addlw	0x1	; inc
	addwf	0x38, f ; add to 38

	movf	0x38, w ;
	addlw	0x1	; inc
	addwf	0x39, f ; add to 39

	movf	0x39, w ;
	addlw	0x1	; inc
	addwf	0x3a, f ; add to 3a

	movf	0x3a, w ;
	addlw	0x1	; inc
	addwf	0x3b, f ; add to 3b

	movf	0x3b, w ;
	addlw	0x1	; inc
	addwf	0x3c, f ; add to 3c

	movf	0x3c, w ;
	addlw	0x1	; inc
	addwf	0x3d, f ; add to 3d

	movf	0x3d, w ;
	addlw	0x1	; inc
	addwf	0x3e, f ; add to 3e

	movf	0x3e, w ;
	addlw	0x1	; inc
	addwf	0x3f, f ; add to 3f

	movf	0x30, w ; restore original 0xf
	andlw	0xf
	addlw	0xf
	movwf	0x30
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	btfss	0x30, 4 ; skip if half-byte carry
	goto mangle_return
	nop
	goto mangle_lock_loop
; 69 when goto, 69 when return
; CIC has 78 -> 9 nops
	
mangle_lock_withskip
	movf	0x41, w ; restore 33
	addwf	0x33, f ; add to 33
	movf	0x34, w
	movwf	0x40	; save 34 to 40
	movf	0x33, w
	addwf	0x34, f
	movf	0x35, w
	movwf	0x41	; save 35 to 41
	movf	0x40, w ; restore 34
	andlw	0xf	; mask nibble
	addlw	0x8	; add #8 to HIGH nibble
	movwf	0x40
	btfss	0x40, 4 ; skip if carry to 5th bit
	addwf	0x35, w
	movwf	0x35

	movf	0x41, w ; restore 35
	addlw	0x1	; inc
	addwf	0x36, f	; add to 36

	movf	0x36, w ;
	addlw	0x1	; inc
	addwf	0x37, f ; add to 37

	movf	0x37, w ;
	addlw	0x1	; inc
	addwf	0x38, f ; add to 38

	movf	0x38, w ;
	addlw	0x1	; inc
	addwf	0x39, f ; add to 39

	movf	0x39, w ;
	addlw	0x1	; inc
	addwf	0x3a, f ; add to 3a

	movf	0x3a, w ;
	addlw	0x1	; inc
	addwf	0x3b, f ; add to 3b

	movf	0x3b, w ;
	addlw	0x1	; inc
	addwf	0x3c, f ; add to 3c

	movf	0x3c, w ;
	addlw	0x1	; inc
	addwf	0x3d, f ; add to 3d

	movf	0x3d, w ;
	addlw	0x1	; inc
	addwf	0x3e, f ; add to 3e

	movf	0x3e, w ;
	addlw	0x1	; inc
	addwf	0x3f, f ; add to 3f

	movf	0x30, w ; restore original 0xf
	andlw	0xf
	addlw	0xf
	movwf	0x30

	btfss	0x30, 4 ; skip if half-byte carry
	goto mangle_return
	nop
	goto mangle_lock_loop
; 73 when goto, 73 when return
; CIC has 84 -> 11 nops

; --------wait: 3*(W-1)+7 cycles (including call+return). W=0 -> 256!--------
wait			    ; 2 for call
	movwf	swait	    ; 1
wait0	decfsz	swait, f    ; 1 / 2 last pass
	goto	wait0	    ; 2
	return		    ; 2

; --------wait long: 8+(3*(w-1))+(772*w). W=0 -> 256!--------
longwait
	movwf	lwait
	clrw
longwait0
	call	wait
	decfsz	lwait, f
	goto	longwait0
	return

	
; -----------------------------------------------------------------------
; 3193 - USA/Canada 
; LOCK: 3952F20F9109997 - avrcic
; LOCK: $1952f8271981115 - segher
; LOAD LOCK SEED (30 cycles)
; 30 + 28 + 2 for final goto = 60
load3193
	loadlock    0x1,0x9,0x5,0x2,0xF,0x8,0x2,0x7,0x1,0x9,0x8,0x1,0x1,0x1,0x5
; 3193 - USA/Canada 
; KEY: x952129F910DF97 - avrcic
; KEY: $x95212171985715 - segher
	loadkey	    0x9,0x5,0x2,0x1,0x2,0x1,0x7,0x1,0x9,0x8,0x5,0x7,0x1,0x5
	goto	doneload

; -----------------------------------------------------------------------
; 3195 - Europe 
; LOCK: $17BEF0AF5706617 
; LOAD LOCK SEED (30 cycles)
; 30 + 28 + 2 for final goto = 60
load3195
	loadlock    0x1,0x7,0xB,0xE,0xF,0x0,0xA,0xF,0x5,0x7,0x0,0x6,0x6,0x1,0x7
; 3195 - Europe 
; KEY: $x7BD309F6EF2F97 
	loadkey	    0x7,0xB,0xD,0x3,0x0,0x9,0xF,0x6,0xE,0xF,0x2,0xF,0x9,0x7	
	goto	doneload

; -----------------------------------------------------------------------
; 3196 - Asia 
; LOCK: 06AD70AF6EF666C  
; LOAD LOCK SEED (30 cycles)
; 30 + 28 + 2 for final goto = 60
load3196
	loadlock    0x0,0x6,0xA,0xD,0x7,0x0,0xA,0xF,0x6,0xE,0xF,0x6,0x6,0x6,0xC
; 3196 - Asia
; KEY: x6ADCF606EF2F97 
	loadkey	    0x6,0xA,0xD,0xC,0xF,0x6,0x0,0x6,0xE,0xF,0x2,0xF,0x9,0x7	
	goto	doneload
	
; -----------------------------------------------------------------------
; 3197 - UK/Italy/Australia  
; LOCK: 558937A00E0D66D   
; LOAD LOCK SEED (30 cycles)
; 30 + 28 + 2 for final goto = 60
load3197
	loadlock    0x5,0x5,0x8,0x9,0x3,0x7,0xA,0x0,0x0,0xE,0x0,0xD,0x6,0x6,0xD
; 3197 - UK/Italy/Australia
; KEY: x79AA1E0D019D99 
	loadkey	    0x7,0x9,0xA,0xA,0x1,0xE,0x0,0xD,0x0,0x1,0x9,0xD,0x9,0x9	
	goto	doneload
	
;-- change region in eeprom and die
; 0x00 = 3193 - USA/Canada
; 0x01 = 3195 - Europe
; 0x02 = 3196 - Asia 
; 0x03 = 3197 - UK/Italy/Australia
die
	banksel	EEADR
	movlw	0x00		; point to region
	movwf	EEADR		; store to address
	bsf	EECON1,RD	; read eeprom
	movf	EEDATA,W	; move to wreg
	movwf   eereg		; store in 4D
	incf	eereg,f		; increment
	btfsc	eereg, 2	; check if region is greater than 3
	clrf	eereg		; wrap around back to 0
	movf	eereg,W		; store region in wreg
	movwf	EEDATA		; store back to eeprom
	bsf	EECON1, WREN
	bcf	INTCON, GIE
	movlw	0x55
	movwf	EECON2
	movlw	0xAA
	movwf	EECON2
	bsf	EECON1, WR
	bsf	INTCON, GIE

	banksel	GPIO
; --------get caught up--------
die_trap
	bsf	GPIO, led	; LED on
	nop
	nop
	bcf	GPIO, led	; LED on
	goto	die_trap
	
; eeprom memory
; 0x00 = 3193 - USA/Canada
; 0x01 = 3195 - Europe
; 0x02 = 3196 - Asia 
; 0x03 = 3197 - UK/Italy/Australia	
DEEPROM code
	de	0x00		; region indicator byte, default to USA/Canada
	end
