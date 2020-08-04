.INCLUDE "m128def.inc"
.equ fclk = 8000000 ; system clock frequency (for delays)
; register usage
.def temp = R16 ; temporary storage
; LCD interface
.equ lcd_D7_port = PORTC ; lcd D7 connection
.equ lcd_D7_bit = PORTC7
.equ lcd_D7_ddr = DDRC
.equ lcd_D6_port = PORTC ; lcd D6 connection
.equ lcd_D6_bit = PORTC6
.equ lcd_D6_ddr = DDRC
.equ lcd_D5_port = PORTC ; lcd D5 connection
.equ lcd_D5_bit = PORTC5
.equ lcd_D5_ddr = DDRC
.equ lcd_D4_port = PORTC ; lcd D4 connection
.equ lcd_D4_bit = PORTC4
.equ lcd_D4_ddr = DDRC
.equ lcd_E_port = PORTB ; lcd Enable pin
.equ lcd_E_bit = PORTB5
.equ lcd_E_ddr = DDRB
.equ lcd_RS_port = PORTB ; lcd Register Select pin
.equ lcd_RS_bit = PORTB2
.equ lcd_RS_ddr = DDRB
.equ text=0xaa00
.equ text1=0x0200
.equ ZEROS = 0x00
.equ ONES = 0xff
.equ any = 0x500
.equ any1 = 0x501
.equ any2 = 0x502

; LCD module Lines
.equ lcd_LineOne = 0x00 ; line 1
.equ lcd_LineTwo = 0x40 ; line 2
; LCD Defined instructions
.equ lcd_Clear = 0b00000001 ; ASCII 'space' for all characters
.equ lcd_Home = 0b00000010 ; first position on first line
.equ lcd_EntryMode = 0b00000110 ; shift cursor from left to right on read/write
.equ lcd_DisplayOff = 0b00001000 ; turn display off
.equ lcd_DisplayOn = 0b00001100 ; display on, cursor off, don't blink character
.equ lcd_FunctionReset = 0b00110000 ; reset the LCD
.equ lcd_FunctionSet4bit = 0b00101000 ; 4-bit data, 2-line display, 5 x 7 font
.equ lcd_SetCursor = 0b10000000 ; set cursor position
; ****************************** Reset Vector *******************************
.org 0x0000
 jmp start ; jump over Interrupt Vectors, Program ID etc.

; ****************************** Main Program Code **************************
start:
; initialize the stack pointer to the highest RAM address
 ldi temp,low(RAMEND);decleration of stack
 out SPL,temp
 ldi temp,high(RAMEND)
 out SPH,temp
ldi r20, ZEROS
ldi r21, ONES

; configure the microprocessor pins for the data lines
 sbi lcd_D7_ddr, lcd_D7_bit ; 4 data lines - output
 sbi lcd_D6_ddr, lcd_D6_bit
 sbi lcd_D5_ddr, lcd_D5_bit
 sbi lcd_D4_ddr, lcd_D4_bit
 HERE:out ddra, r20;a is input

call read_input ;calling input function

call calculate_crc;calling crc function
call display1   ; calling binary display in firstline of lcd
call display2   ; calling hexa display in secondline of lcd
; configure the microprocessor pins for the control lines
 sbi lcd_E_ddr, lcd_E_bit ; E line - output
 sbi lcd_RS_ddr, lcd_RS_bit ; RS line - output
; initialize the LCD controller
ldi r16, 0x00 ; Load 0 to R16
out ddra, r16 ; Configure PORTA as input
call lcd_init_4d ; initialize the LCD display for a 4-bit interface





; endless loop
 rjmp here
; ****************************** End of Main Program Code *******************
; ============================== 4-bit LCD Function Calls ======================
; Name: lcd_init_4d -- initialize the LCD module for a 4-bit data interface
lcd_init_4d:
; Power-up delay
 ldi temp, 100 ; initial 40 mSec delay
 call delayTx1mS
; IMPORTANT - At this point the LCD module is in the 8-bit mode and it is expecting to receive
; 8 bits of data, one bit on each of its 8 data lines, each time the 'E' line is pulsed.
;
; Since the LCD module is wired for the 4-bit mode, only the upper four data lines areconnected to
; the microprocessor and the lower four data lines are typically left open. Therefore, when
; the 'E' line is pulsed, the LCD controller will read whatever data has been set up on theupper
; four data lines and the lower four data lines will be high (due to internal pull-upcircuitry).
;
; Fortunately the 'FunctionReset' instruction does not care about what is on the lower fourbits so
; this instruction can be sent on just the four available data lines and it will beinterpreted
; properly by the LCD controller. The 'lcd_write_4' subroutine will accomplish this if the
; control lines have previously been configured properly.
; Set up the RS and E lines for the 'lcd_write_4' subroutine.
 cbi lcd_RS_port, lcd_RS_bit ; select the Instruction Register (RS low)
 cbi lcd_E_port, lcd_E_bit ; make sure E is initially low
; Reset the LCD controller.
 ldi temp, lcd_FunctionReset ; first part of reset sequence
 call lcd_write_4
 ldi temp, 10 ; 4.1 mS delay (min)
 call delayTx1mS
 ldi temp, lcd_FunctionReset ; second part of reset sequence
 call lcd_write_4
 ldi temp, 200 ; 100 uS delay (min)
 call delayTx1uS
 ldi temp, lcd_FunctionReset ; third part of reset sequence
 call lcd_write_4
 ldi temp, 200 ; this delay is omitted in the data sheet
 call delayTx1uS
; Preliminary Function Set instruction - used only to set the 4-bit mode.
; The number of lines or the font cannot be set at this time since the controller is still inthe
; 8-bit mode, but the data transfer mode can be changed since this parameter is determined byone
; of the upper four bits of the instruction.
 ldi temp, lcd_FunctionSet4bit ; set 4-bit mode
 call lcd_write_4
 ldi temp, 80 ; 40 uS delay (min)
 call delayTx1uS
; Function Set instruction
 ldi temp, lcd_FunctionSet4bit ; set mode, lines, and font
 call lcd_write_instruction_4d
 ldi temp, 80 ; 40 uS delay (min)
 call delayTx1uS
; The next three instructions are specified in the data sheet as part of the initialization routine,
; so it is a good idea (but probably not necessary) to do them just as specified and then redo them
; later if the application requires a different configuration.
; Display On/Off Control instruction
 ldi temp, lcd_DisplayOff ; turn display OFF
 call lcd_write_instruction_4d
 ldi temp, 80 ; 40 uS delay (min)
 call delayTx1uS
; Clear Display instruction
 ldi temp, lcd_Clear ; clear display RAM
 call lcd_write_instruction_4d
 ldi temp, 4 ; 1.64 mS delay (min)
 call delayTx1mS
; Entry Mode Set instruction
 ldi temp, lcd_EntryMode ; set desired shift characteristics
 call lcd_write_instruction_4d
 ldi temp, 80 ; 40 uS delay (min)
 call delayTx1uS
; This is the end of the LCD controller initialization as specified in the data sheet, but the display
; has been left in the OFF condition. This is a good time to turn the display back ON.
; Display On/Off Control instruction
 ldi temp, lcd_DisplayOn ; turn the display ON
 call lcd_write_instruction_4d
 ldi temp, 80 ; 40 uS delay (min)
 call delayTx1uS
 ret
; ---------------------------------------------------------------------------
; Name: lcd_write_string_4d
; Purpose: display a string of characters on the LCD
; Entry: ZH and ZL pointing to the start of the string
; (temp) contains the desired DDRAM address at which to start the display
; Exit: no parameters
; Notes: the string must end with a null (0)
; uses time delays instead of checking the busy flag
lcd_write_string_4d:
; preserve registers
 push ZH ; preserve pointer registers
 push ZL
; fix up the pointers for use with the 'lpm' instruction
;lsl ZL ; shift the pointer one bit left for the lpm instruction
;rol ZH
; set up the initial DDRAM address
 ori temp, lcd_SetCursor ; convert the plain address to a set cursor instruction
 call lcd_write_instruction_4d ; set up the first DDRAM address
 ldi temp, 80 ; 40 uS delay (min)
 call delayTx1uS
; write the string of characters
lcd_write_string_4d_01:
 ld temp, Z+ ; get a character
 cpi temp, 0 ; check for end of string
 breq lcd_write_string_4d_02 ; done
; arrive here if this is a valid character
 call lcd_write_character_4d ; display the character
 ldi temp, 80 ; 40 uS delay (min)
 call delayTx1uS
 rjmp lcd_write_string_4d_01 ; not done, send another character
; arrive here when all characters in the message have been sent to the LCD module
lcd_write_string_4d_02:
 pop ZL ; restore pointer registers
 pop ZH
 ret
; ---------------------------------------------------------------------------
; Name: lcd_write_character_4d
; Purpose: send a byte of information to the LCD data register
; Entry: (temp) contains the data byte
; Exit: no parameters
; Notes: does not deal with RW (busy flag is not implemented)
lcd_write_character_4d:
 sbi lcd_RS_port, lcd_RS_bit ; select the Data Register (RS high)
 cbi lcd_E_port, lcd_E_bit ; make sure E is initially low
 call lcd_write_4 ; write the upper 4-bits of the data
 swap temp ; swap high and low nibbles
 call lcd_write_4 ; write the lower 4-bits of the data
 ret
; ---------------------------------------------------------------------------
; Name: lcd_write_instruction_4d -- Send a byte of information to the LCD instruction register
lcd_write_instruction_4d:
 cbi lcd_RS_port, lcd_RS_bit ; select the Instruction Register (RS low)
 cbi lcd_E_port, lcd_E_bit ; make sure E is initially low
 call lcd_write_4 ; write the upper 4-bits of the instruction
 swap temp ; swap high and low nibbles
 call lcd_write_4 ; write the lower 4-bits of the instruction
 ret
; ---------------------------------------------------------------------------
; Name: lcd_write_4 Send 4-bits of information to the LCD module
; Entry: (temp) contains a byte of data with the desired 4-bits in the upper nibble
; (RS) is configured for the desired LCD register
; (E) is low
; (RW) is low
lcd_write_4:
; set up D7
 sbi lcd_D7_port, lcd_D7_bit ; assume that the D7 data is '1'
 sbrs temp, 7 ; check the actual data value
 cbi lcd_D7_port, lcd_D7_bit ; arrive here only if the data was actually '0'
; set up D6
 sbi lcd_D6_port, lcd_D6_bit ; repeat for each data bit
 sbrs temp, 6
 cbi lcd_D6_port, lcd_D6_bit
; set up D5
 sbi lcd_D5_port, lcd_D5_bit
 sbrs temp, 5
 cbi lcd_D5_port, lcd_D5_bit
; set up D4
 sbi lcd_D4_port, lcd_D4_bit
 sbrs temp, 4
 cbi lcd_D4_port, lcd_D4_bit
; write the data
 ; 'Address set-up time' (40 nS)
 sbi lcd_E_port, lcd_E_bit ; Enable pin high
 call delay1uS ; implement 'Data set-up time' (80 nS) and 'Enable pulse width' (230 nS)
 cbi lcd_E_port, lcd_E_bit ; Enable pin low
 call delay1uS ; implement 'Data hold time' (10 nS) and 'Enable cycle time' (500 nS)
 ret
; ============================== End of 4-bit LCD Subroutines ===============
; ============================== Time Delay Subroutines =====================
; Name: delayYx1mS Delay of (YH:YL) x 1 mS
delayYx1mS:
 call delay1mS ; delay for 1 mS
 sbiw YH:YL, 1 ; update the the delay counter
 brne delayYx1mS ; counter is not zero
; arrive here when delay counter is zero (total delay period is finished)
 ret
; ---------------------------------------------------------------------------
; Name: delayTx1mS Provide a delay of (temp) x 1 mS
delayTx1mS:
 call delay1mS ; delay for 1 mS
 dec temp ; update the delay counter
 brne delayTx1mS ; counter is not zero
; arrive here when delay counter is zero (total delay period is finished)
 ret
; ---------------------------------------------------------------------------
; Name: delay1mS -- Delay of 1 mS
delay1mS:
 push YL ; [2] preserve registers
 push YH ; [2]
 ldi YL, low (((fclk/1000)-18)/4) ; [1] delay counter
 ldi YH, high(((fclk/1000)-18)/4) ; [1]
delay1mS_01:
 sbiw YH:YL, 1 ; [2] update the the delay counter
 brne delay1mS_01 ; [2] delay counter is not zero
; arrive here when delay counter is zero
 pop YH ; [2] restore registers
 pop YL ; [2]
 ret ; [4]
; ---------------------------------------------------------------------------
; Name: delayTx1uS Delay of (temp) x 1 uS with a 8 MHz clock frequency
delayTx1uS:
 call delay1uS ; delay for 1 uS
 dec temp ; decrement the delay counter
 brne delayTx1uS ; counter is not zero
; arrive here when delay counter is zero (total delay period is finished)
 ret
; ---------------------------------------------------------------------------
; Name: delay1uS
; Purpose: Delay of 1 uS with a 8 MHz clock frequency
delay1uS:
 push temp ; [2] Consume clock cycles
 pop temp ; [2]
 ret ; [4]
; ============================== End of Time Delay Subroutines ==============

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~MY SUBROUTINE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.org 0x115
read_input:
in r22,pina ;putting pina into r22
ret ; end of read_input function
.org 0x0120
calculate_crc: 
		mov r26,r22 ;copying r22 to r26 
		ldi  r24,0x13 ; loading 0x10011 into r24
		lsl r24      ; i shift 3 times to make it 0x10011000
		lsl r24      ; 
		lsl r24
		ldi r25,0x00  ; loading counter value
		this:cpi r26,0x00 ; comparing r26 with zero
		brpl sll ; if r26 is plus which means there is 0 in the beginning there will be shifting
		e_or:eor r26,r24 ;if 1 in the beginning it will make eor
		jmp this ; jump to this 

		sll:lsl r26 ; shift left to r26
			inc r25 ; increment counter
			cpi r25,0x08 ; if we do 8 times eor crc is done
			breq exit ; jump to exit
			jmp e_or ; if it doesnt jump to exit contiune to do eor
			
				
			exit:
			swap r26 ; answer is in first 4 bit, i changed it last 4 bit
			ret;return
.org 0x150
display1:
		mov r19,r26 ; i moved it r19 because r26 is X pointers value
		mov r17,r26; same but for hexadisplay i could call this in display2
		ldi ZH,high(0x0100); point to the information that is to be displayed
		ldi ZL, low(0x0100);same
		ldi XH,high(0x0100);same;
		ldi XL,low(0x0100);same
		swap r17 ; i agained swap because i will rotate left, it will be easy
		ldi r18,5; it will print 4 digit
		display_loop:dec r18; decrement the counter
		breq display_exit; if counter is 0 then exit
		rol r17 ; rotate left with carry
		brcs display_1 ; if carry is 1 it will display 1
		jmp display_0 ; else it will display 0


		display_1:
		
		ldi temp,'1' ; load 1 into temp
		st X+,temp ; put temp to x pointer
		jmp display_loop ; go to display loop again

		display_0:
		
		ldi temp,'0'; load 0 into temp
		st X+,temp ; load temp into x pointer
		jmp display_loop ; go to display loop again
		
		display_exit:
		ldi temp, lcd_LineOne ; point to where the information should be displayed
		call lcd_write_string_4d ; calling the lcd writing function
		ret ; end
.org 0x300
display2:
	ldi ZH,high(0x0200); point to the information that is to be displayed
	ldi ZL, low(0x0200); same
	cpi r19,10; compare the number with 10
	brlt show_sayi ;if it is less than 10 it is a number
	jmp show_char ; else it is a characted such as A,B,C,D,E,F
	show_sayi:subi r19,-48 ; add 48 to number because 0 = 49 , 1=50 etc.
		jmp display2_exit ; jump to exit
	show_char:subi r19,-55; add 55 because A=10's asci is 55, B=11 is 56 etc.
		jmp display2_exit ;jump to exit
	display2_exit:;
	ldi temp,lcd_LineTwo;point to where the information should be displayed
	sts 0x0200,r19;store r19 into location0x0200
	call lcd_write_string_4d ;call writing function
	ret;end