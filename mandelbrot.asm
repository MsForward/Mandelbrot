data segment
; Input
  args            		db 255 dup("$")   ; stores command line arguments
  argPtr         			dw 128 dup(0)   	; array of argument offsets
  argNum          		db 0            	; stores number of arguments

data ends

.286
assume ds:data, cs:code

code segment

start:
	call init
	call parseArgs
	call exit

	printArgs proc ; prints command line arguments
    push ax
    push cx
    push dx
    
    ; start with argument 0
    xor cl, cl

    printArgLoop:
      ; check if argument exists
      cmp cl, argNum
      ; if no arguments left return
      je endPrintArgs
      
      ; get argument address
      mov dl, cl
      call getArg
      ; print argument to console
      mov dl, al
      call println
      
      ; go to next argument
      inc cl
      jmp printArgLoop
        
    endPrintArgs:
    
    pop dx
    pop cx
    pop ax
    ret
	printArgs endp


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
    mov ax, seg args
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
			dw 255 dup(?)
	top dw ?
stacks ends

end start