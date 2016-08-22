#include <p12f629.inc>

; ---------------------------------------------------------------------
;   NES multi-region CIC heavily inspired from SUPER CIC which is itslelf
;   based on reverse engineering work and disassembly by segher,
;   http://hackmii.com/2010/01/the-weird-and-wonderful-cic/
;   
;   Copyright (C) 2016 by René Richard (db) <rene@db-electronics.ca>
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
; ---------------------------------------------------------------------
;
;   pinout :  CIC
;
;                   ,---_---.
;      +5V     [16] |1     8| GND [8]
;      CIC clk [6]  |2     7| CIC data out 0 [2]
;        status out |3     6| CIC data in  1 [1]
;             /NTSC |4     5| CIC slave reset [7]
;                   `-------'
;
;
;   Status out can be connected to a LED. It indicates:
;
;   state                   | output
;  -------------------------+----------------------------
;   OK or no lock CIC       | high
;   error                   | low
;
;   In case lockout fails, the region is switched automatically and
;   will be used after the next reset.
;
;   memory usage:
;
;   0x20		buffer for seed calc and transfer
;   0x21 - 0x2f		seed area (lock seed)
;   0x30		buffer for seed calc
;   0x31 - 0x3f		seed area (key seed; 0x31 filled in by lock)
;   0x40 - 0x41		buffer for seed calc
;   0x4d		buffer for eeprom access
;   0x4e		loop variable for longwait
;   0x4f		loop variable for wait
;   0x5e                SuperCIC pair mode detect (phase 1)
;   0x5f                SuperCIC pair mode detect (phase 2)
; ---------------------------------------------------------------------


; -----------------------------------------------------------------------
    __CONFIG _EC_OSC & _WDT_OFF & _PWRTE_OFF & _MCLRE_OFF & _CP_OFF & _CPD_OFF

; -----------------------------------------------------------------------
; code memory
	org	0x0000
	nop
	nop
	nop
	goto	init
isr
	org	0x0004
	bcf	INTCON, 1	; clear interrupt cause
	bcf	GPIO, 0
	bcf	GPIO, 1
	bsf	GPIO, 4		; LED on
	nop
	nop
	nop
	nop
	nop
	nop
	bsf	INTCON, 7	; re-enable interrupts (ISR will continue as main)
	goto	main
init
	org	0x0010
	banksel GPIO
	clrf	GPIO
	movlw	0x07	; GPIO2..0 are digital I/O (not connected to comparator)
	movwf	CMCON
	movlw	0x90	; global enable interrupts + enable external interrupt
	movwf	INTCON
	banksel	TRISIO
	movlw	0x2d	; in out in in out in
	movwf	TRISIO
	movlw	0x24	; pullups for reset+clk to avoid errors when no CIC in host 
	movwf	WPU
	movlw	0x00	; 0x80 for global pullup disable
	movwf	OPTION_REG
	
	banksel GPIO
	bsf 	GPIO, 4	; LED on
idle
	goto	idle	; wait for interrupt from lock

main
; super cic uses 614 cycle until lock sends stream id
; 31 load lock super cic
; 34 load key super cic
; 1 load 181 into wreg
; call wait with 181 = 547
; clrf
; 614 cycles from main
	
	banksel	TRISIO	    ;1
	bsf	TRISIO, 0   ;1
	bcf	TRISIO, 1   ;1
			    ;3 cycles
	
; --------INIT LOCK SEED (what the lock sends)--------
; new code by db to set the NES lock and key seeds, read from eeprom
	
	banksel	EEADR	    ; 1
	movlw	0x0F	    ; 1
	movwf	EEADR	    ; 1 - point to region byte
	bsf	EECON1,RD   ; 1 - read eeprom
	movf	EEDATA,W    ; 1 - read into wreg
	movwf	EEADR	    ; 1 - it contained a pointer into EE memory
			    ; 6 + 3
	    ; prepare fsr for lock in 0x21 - 0x2f
	movlw	0x21	    ; 1 - point to lock seed mem
	movwf	FSR	    ; 1 - in FSR
	movlw	0x0F	    ; 1 - 15 values to load
	movwf	0xCF	    ; 1 - store
			    ; 4 + 6 + 3 = 13 cycles
lockload	
	bsf	EECON1,RD   ; 1 - read eeprom
	movf	EEDATA,W    ; 1 - read
	movwf	INDF	    ; 1 - store to ram
	incf	FSR	    ; 1 - inc ram pointer
	incf	EEADR	    ; 1 - inc eeprom pointer
	decfsz	0xCF	    ; 1 (2 on skip) - iteration variable decrement
	goto	lockload    ; 2
			    ; loop 15 times
			    ; (14 x 8) + 7 = 119
			    ; 119 + 13 cycles
	
	    ; now EEADR points to lock + 15
	incf	EEADR	    ; 1 - skip 1 for align 16 data
	incf	FSR	    ; 1 - same
	movlw	0x0F	    ; 1 - 15 values to load
	movwf	0xCF	    ; 1 - store
			    ; 4 + 119 + 13 cycles
keyload	
	bsf	EECON1,RD   ; 1 - read eeprom
	movf	EEDATA,W    ; 1 - read
	movwf	INDF	    ; 1 - store to ram
	incf	FSR	    ; 1 - inc ram pointer
	incf	EEADR	    ; 1 - inc eeprom pointer
	decfsz	0xCF	    ; 1 (2 on skip) - iteration variable decrement
	goto	keyload     ; 2	
	banksel GPIO	    ; 1
			    ; loop 15 times
			    ; (14 x 8) + 7 + 1 = 120
			    ; total = 120 + 4 + 119 + 13 = 256
			    ; 614 - 256 = 358

; need to burn 358 cycles
	movlw	0x75	    ; 1 - load 117
	call	wait	    ; wait = (3*(W-1)) + 7 = 355
	nop		    ; 1
	nop		    ; 1
			
; --------lock sends stream ID. 15 cycles per bit--------
	btfsc	GPIO, 0		; check stream ID bit
	bsf	0x31, 3		; copy to lock seed
	movlw	0x2		; wait=3*W+5
	call	wait		; burn 11 cycles
	nop
	nop

	btfsc	GPIO, 0		; check stream ID bit
	bsf	0x31, 0		; copy to lock seed
	movlw	0x2		;
	call	wait		; burn 11 cycles
	nop
	nop

	btfsc	GPIO, 0		; check stream ID bit
	bsf	0x31, 1		; copy to lock seed
	movlw	0x2		;
	call	wait		; burn 11 cycles
	nop
	nop

	btfsc	GPIO, 0		; check stream ID bit
	bsf	0x31, 2		; copy to lock seed
	banksel	TRISIO
	bcf	TRISIO, 0
	bsf	TRISIO, 1
	banksel	GPIO
	nop
	movlw	0x27		; "wait" 1
	call	wait		; wait 121
				; 3*(39-1)+7 = 121
; --------main loop--------
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
	nop
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

	btfss	0x5e, 1
	goto	scic_pair_skip1
	btfsc	0x5f, 1
	goto	scic_pair_skip2
	btfsc	GPIO, 3
	goto	scic_pair_skip3
	goto	supercic_pairmode
scic_pair_skip1
	nop
	nop
scic_pair_skip2
	nop
	nop
scic_pair_skip3	
	nop
	nop
	nop
	nop

	btfss	0x30, 4 ; skip if half-byte carry
	goto mangle_return
	nop
	goto mangle_lock_loop
; 73 when goto, 73 when return
; CIC has 84 -> 11 nops

; --------wait: 3*(W-1)+7 cycles (including call+return). W=0 -> 256!--------
wait			    ; 2 for call
	movwf	0x4f	    ; 1
wait0	decfsz	0x4f, f	    ; 1 / 2 last pass
	goto	wait0	    ; 2
	return		    ; 2

; --------wait long: 8+(3*(w-1))+(772*w). W=0 -> 256!--------
longwait
	movwf	0x4e
	clrw
longwait0
	call	wait
	decfsz	0x4e, f
	goto	longwait0
	return

; --------change region in eeprom and die--------
; new code by db, need to change the region in eeprom
; region points to the start of the region's lock seed in eeprom
; 0x00 = 3193 - USA/Canada
; 0x20 = 3197 - UK/Italy/Australia
; 0x40 = 3195 - Europe 
; 0x60 = 3196 - Asia 
;   so, to change region, add 0x20 and AND with 0x60 to ensure no unwanted bits
die
	banksel	EEADR
	movlw	0x0F	    ; point to region
	movwf	EEADR	    ; store to address
	bsf	EECON1,RD   ; read eeprom
	movf	EEDATA,W    ; move to wreg
	addlw	0x20	    ; add 0x20 for next region
	andlw	0x60	    ; fix overflow at 128
	movwf	EEDATA	    ; store back to eeprom
	bsf	EECON1, WREN

die_intloop
	bcf	INTCON, GIE
	btfsc	INTCON, GIE
	goto	die_intloop
	
	movlw	0x55
	movwf	EECON2
	movlw	0xAA
	movwf	EECON2
	bsf	EECON1, WR
	bsf	INTCON, GIE

	banksel	GPIO
	bcf	GPIO, 4
; --------get caught up--------
die_trap
	goto	die_trap
; -----------------------------------------------------------------------
supercic_pairmode
	banksel	TRISIO
	bsf	TRISIO, 0
	bsf	TRISIO, 1
	banksel	GPIO
supercic_pairmode_loop
	bsf	GPIO, 4
	nop
	nop
	bcf	GPIO, 4
	goto	supercic_pairmode_loop

; eeprom memory
;   the 16th byte of each seed is unused, therefore, I used addr 0x0F to store
;   the current region since 12F629 only has 128 bytes of eeprom
DEEPROM code
;3193 - USA/Canada 
;LOCK: 3952F20F9109997
;KEY:  x952129F910DF97 
	de	0x3,0x9,0x5,0x2,0xF,0x2,0x0,0xF,0x9,0x1,0x0,0x9,0x9,0x9,0x7,0
	de	0x0,0x9,0x5,0x2,0x1,0x2,0x9,0xF,0x9,0x1,0x0,0xD,0xF,0x9,0x7,0
	
;3197 - UK/Italy/Australia 
;LOCK: 558937A00E0D66D 
;KEY:  x79AA1E0D019D99
	de	0x5,0x5,0x8,0x9,0x3,0x7,0xA,0x0,0x0,0xE,0x0,0xD,0x6,0x6,0xD,0
	de	0x0,0x7,0x9,0xA,0xA,0x1,0xE,0x0,0xD,0x0,0x1,0x9,0xD,0x9,0x9,0
	
;3195 - Europe 
;LOCK: 17BEF0AF5706617
;KEY:  x7BD309F6EF2F97  
	de	0x1,0x7,0xB,0xE,0xF,0x0,0xA,0xF,0x5,0x7,0x0,0x6,0x6,0x1,0x7,0
	de	0x0,0x7,0xB,0xD,0x3,0x0,0x9,0xF,0x6,0xE,0xF,0x2,0xF,0x9,0x7,0

;3196 - Asia 
;LOCK: 06AD70AF6EF666C
;KEY:  x6ADCF606EF2F97  
	de	0x0,0x6,0xA,0xD,0x7,0x0,0xA,0xF,0x6,0xE,0xF,0x6,0x6,0x6,0xC,0
	de	0x0,0x6,0xA,0xD,0xC,0xF,0x6,0x0,0x6,0xE,0xF,0x2,0xF,0x9,0x7,0
	
	end
