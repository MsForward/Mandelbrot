data segment
; Input
	dataPtr							db ?
  args            		db 255 dup("$")   ; stores command line arguments
  argPtr         			dw 128 dup(0)   	; array of argument offsets
  argNum          		db 0            	; stores number of arguments

; Coordinates
	xMin								dq ?
	xMax								dq ?
	yMin								dq ?
	yMax								dq ?

; FPU
	TEN									dw 10d
	TENTH								dd 0.1
	TWO									dw 2
	RES_X								dw 320d
	RES_Y								dw 200d
	MAX 								dq 4
	p 									dq ?
	q										dq ?
	n 									dw 0
	m 									dw 0
	x 									dq ?
	y 									dq ?
	tmp									dq ?

	xMinMessage					db "Enter minimum x value:", "$"
	xMaxMessage					db "Enter maximum x value:", "$"
	yMinMessage					db "Enter minimum y value:", "$"
	yMaxMessage					db "Enter maximum y value:", "$"
	errorMessage				db "Not a valid number!", "$"

data ends

.386
.387
assume ds:data, cs:code

code segment

;================================= MAIN ======================================;

start:
	mov dl, "b"
	call printChar
	call init
	call getInput
	call draw
	call exit

;=============================== END MAIN ====================================;


;=============================== METHODS =====================================;

	draw proc
		push ax

		call VGAMode

		mov n, 0
		mov m, 0
		drawLoop:
			mov ax, n
			cmp ax, RES_X
			jb contDraw
			mov n, 0
			inc m
			mov ax, m
			cmp ax, RES_Y
			jae endDraw

		contDraw:			
			call getPixelColor
			inc n

		endDraw:

		call textMode

		pop ax
		ret
	draw endp

	getPixelColor proc
		push cx
		push dx

		fldz
		fistp x
		fldz
		fistp y

		call getP
		call getQ

		mov cx, 1000d
		calcPixel:
			call getTmp
			call getY
			call getX
			call checkResult
			sahf
			ja endCalcPixel
			loop calcPixel

		endCalcPixel:
		test cx, cx
		jz colorWhite
		jmp colorBlack

		colorWhite:
			mov dl, 1
			call drawPixel

		colorBlack:
			xor dl, dl
			call drawPixel
		ret
	getPixelColor endp

	drawPixel proc
	; entry: DL = 1 - white, 0 - black
		push ax
		push bx
		push es

		mov ax, 0A000h
		mov es, ax
		call getPixelIndex
		mov bx, ax
		test dl, dl
		jz black
		jnz white

		black:
			xor al, al
			jmp color

		white: 
			mov al, 0FFh

		color:
		;mov byte ptr es:[bx], al
		pop es
		pop bx
		pop ax
		ret
	drawPixel endp

	getPixelIndex proc
	; return: AX = index
		push bx
		mov ax, m
		mov bx, ax
		shl ax, 6 ; multiply by 64
		shl bx, 8 ; multiply by 256
		add ax, bx
		add ax, n
		pop bx
		ret
	getPixelIndex endp

	checkResult proc
		fld x
		fmul st, st
		fld y
		fmul st, st
		fadd
		fcomp MAX
		fstsw ax
		ret
	checkResult endp

	getTmp proc
		fld x
		fmul st, st
		fld y
		fmul st, st
		fsub
		fld p
		fadd
		fstp tmp
		ret
	getTmp endp

	getY proc
		fild TWO
		fld x
		fld y
		fmul
		fmul
		fld q
		fadd
		fstp y
		ret
	getY endp

	getX proc
		fld tmp
		fstp x
		ret
	getX endp

	getP proc
		fld xMax
		fld xMin
		fsub
		fild n
		fmul
		fild RES_X
		fdiv
		fld xMin
		fadd
		fstp p
		ret
	getP endp

	getQ proc
		fld yMax
		fld yMin
		fsub
		fild m
		fmul
		fild RES_Y
		fdiv
		fld yMin
		fadd
		fstp q
		ret
	getQ endp

	VGAMode proc
		push ax
		mov ah, 13h
		int 21h
		pop ax
		ret
	VGAMode endp

	textMode proc
		push ax
		mov ah, 03h
		int 21h
		pop ax
		ret
	textMode endp

	getInput proc
		push dx
		mov dx, offset xMinMessage
		call println
		call getNumber
		fstp xMin
		mov dx, offset xMaxMessage
		call println
		call getNumber
		fstp xMax
		mov dx, offset yMinMessage
		call println
		call getNumber
		fstp yMin
		mov dx, offset yMaxMessage
		call println
		call getNumber
		fstp yMax
		pop dx
		ret
	getInput endp

;-------------------------------GET NUMBER------------------------------------;
; Reads one number from console ----------------------------------------------;
;-----------------------------------------------------------------------------;

	getNumber proc
		push ax
		push dx
		; promt for user input until number is correct
		readInput:
			; get integer part
			call getInt
			test ah, ah
			jz numberInputError
			; check if done
			cmp al, 0Dh
			je endReadInput

			; get decimal part
			call getDecimal
			test ah, ah
			jz numberInputError

		numberInputError:
			mov dx, offset errorMessage
			call crlf
			call println
			ffree st
			fincstp
			; try reading again
			jmp readInput

		endReadInput:

		pop dx
		pop ax
		ret
	getNumber endp

;-----------------------------------------------------------------------------;

;--------------------------------- GET INT -----------------------------------;
; Reads integer from console -------------------------------------------------;
;-----------------------------------------------------------------------------;

	getInt proc
	; return: ST = integer, AL = last character entered, AH = 0 on failure
		push dx
		fldz

		getDigit:
			mov ah, 01h
			int 21h
			mov ah, 1
			cmp al, 0Dh
			je endGetInt
			cmp al, "."
			je endGetInt
			cmp al, "-"
			je changeSign

			mov dl, al
			sub dl, 30h
			cmp dl, 9
			ja invalidDigit

			fild TEN
			fmul
			mov dword ptr tmp, 0
			mov byte ptr tmp, dl
			fild tmp
			fadd
			jmp getDigit

		changeSign:
			push ax
			fldz
			fcomp
			fstsw ax
			sahf
			jnz invalidDigit
			mov dh, 1
			jmp getDigit

		invalidDigit:
			xor ah, ah

		endGetInt:
		cmp dh, 1
		jne saveInt
		fchs

		saveInt:
		pop dx
		ret
	getInt endp

	getDecimal proc
	; return: ST = decimal, AL = last character entered, AH = 0 on failure
		push dx

		getDDigit:
			mov ah, 01h
			int 21h
			mov ah, 1
			cmp al, 0Dh
			je endGetDecimal

			mov dl, al
			sub dl, 30h
			cmp dl, 9
			ja invalidDDigit

			fild TENTH
			mov dword ptr tmp, 0
			mov byte ptr tmp, dl
			fild tmp
			fmul
			fadd
			jmp getDDigit

		invalidDDigit:
			xor ah, ah
		endGetDecimal:
		pop dx
		ret	
	getDecimal endp
	
	parseArgs proc
    pusha
    push es
    
    mov si, 82h ; command line arguments offset
    mov di, offset args
    xor ch, ch
    mov cl, byte ptr es:[80h] ; number of characters entered
    
    ; clear argument counter
    xor bx, bx
    
    call removeWhitespace
    ; check for empty command line
    test cx, cx	
    jz endParseArgs
    
    parseArgsLoop:
      call readArg
      call removeWhitespace
    test cx, cx
    jnz parseArgsLoop
    
    ; save number of arguments
  endParseArgs:
    mov argNum, bl
    
    pop es
    popa    
    ret
	parseArgs endp

	removeWhitespace proc
    removeWhitespaceLoop:
      ; read next character
      mov al, byte ptr es:[si]
      ; check if whitespace
      cmp al, 20h
      ja endRemoveWhitespace

      inc si
      loop removeWhitespaceLoop
        
    endRemoveWhitespace:
      ret
	removeWhitespace endp

	readArg proc ; saves one command line argument
		push dx
		mov dl, bl
    call putAddress
    readArgLoop:
      ; read next character
      mov al, byte ptr es:[si]
      ; check if whitespace
      cmp al, 20h
      jbe endReadArg
      cmp al, '$'
      je illegalChar
      ; save character
      mov ds:[di], al
      inc di

    illegalChar:    
      inc si
      loop readArgLoop
        
    endReadArg:
      ; end string with $
      mov byte ptr ds:[di], '$'
      inc di
      ; increment argument counter
      inc bl
      pop dx
      ret
	readArg endp        

	putAddress proc ; saves pointer to argument
	; entry: DL = argument index, DI = argument offset
	    push bx
	    xor bh, bh
	    mov bl, dl
	    shl bl, 1
	    mov [argPtr + bx], di
	    pop bx
	    ret
	putAddress endp

	getArgLen proc ; return length of argument with given index
	; entry : DL = argument index
	; return: AL = argument length
    push ax
    push cx
    push di
    push es
    ; get argument address
    call getArg
    mov di, ax
    mov ax, ds
    mov es, ax
    pop ax

    mov cx, 0FFh
    mov al, "$"
    repne scasb
    mov al, 0FEh
    sub al, cl
   
   	pop es
   	pop di 
    pop cx
    ret
	getArgLen endp

	getArg proc ; returns argument offset
	; entry: DL = argument index 
	; return: AX = argument offset
    push bx
    xor bh, bh
    mov bl, dl
    shl bl, 1 
    mov ax, [argPtr + bx] 
    pop bx
    ret
	getArg endp

	printError proc ; writes error to console and exits program
	; entry: DX = error number
		mov bx, dx
		shl bx, 1
		;mov dx, [errorPtr + bx]
		call println
		call exit
	printError endp

	println proc ; writes single line to console
	; entry: DX = string offset
    call print
    call crlf
    ret
  println endp

  print proc ; writes string in DS:DX to console
  ; entry: DX = string offset
  	push ax
  	mov ah, 9h
  	int 21h
  	pop ax
  	ret
  print endp
  
  printChar proc ; writes single character to console
  ; entry: DL = character to write
    push ax
    mov ah, 2h
    int 21h
    pop ax
    ret
  printChar endp
    
  crlf proc ; prints line break 
    push ax
    push dx
    
    ; print new line
    mov dl, 0Ah
    mov ah, 2h
    int 21h
    ; print carriage return
    mov dl, 0Dh
    int 21h 
    
    pop dx
    pop ax
    ret
  crlf endp

	init proc
    ; save data segment
    mov ax, seg dataPtr
    mov ds, ax
    
    ; initialize stack
    mov ax, seg top
    mov ss, ax
    mov sp, offset top

    ; clear arithmetic registers
    xor ax, ax
    xor bx, bx
    xor cx, cx
    xor dx, dx

    finit
    cld
    ret
  init endp

  exit proc ; returns control to system
    mov ax, 4C00h
    int 21h
  exit endp

code ends

stacks segment stack
			dw 1023 dup(?)
	top dw ?
stacks ends

end start