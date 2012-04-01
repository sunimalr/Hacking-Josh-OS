;*****************start of the kernel code***************
[org 0x000]
[bits 16]

[SEGMENT .text]

;START #####################################################
    mov ax, 0x0100			;location where kernel is loaded
    mov ds, ax
    mov es, ax

    cli
    mov ss, ax				;stack segment
    mov sp, 0xFFFF			;stack pointer at 64k limit
    sti

    push dx
    push es
    xor ax, ax
    mov es, ax
    cli
    mov word [es:0x21*4], _int0x21	; setup interrupt service
    mov [es:0x21*4+2], cs
    sti
    pop es
    pop dx

    mov si, strWelcomeMsg   ; load message
    mov al, 0x01            ; request sub-service 0x01
    int 0x21

	call _shell				; call the shell

    int 0x19                ; reboot
;END #######################################################

_int0x21:
    _int0x21_ser0x01:       ;service 0x01
    cmp al, 0x01            ;see if service 0x01 wanted
    jne _int0x21_end        ;goto next check (now it is end)

	_int0x21_ser0x01_start:
    lodsb                   ; load next character
    or  al, al              ; test for NUL character
    jz  _int0x21_ser0x01_end
    mov ah, 0x0E            ; BIOS teletype
    mov bh, 0x00            ; display page 0
    mov bl, 0x07            ; text attribute
    int 0x10                ; invoke BIOS
    jmp _int0x21_ser0x01_start
    _int0x21_ser0x01_end:
    jmp _int0x21_end

    _int0x21_end:
    iret

_shell:
	_shell_begin:
	;move to next line
	call _display_endl

	;display prompt
	call _display_prompt

	;get user command
	call _get_command


	;split command into components
	call _split_cmd

	;check command & perform action

	; empty command
	_cmd_none:
	mov si, strCmd0
	cmp BYTE [si], 0x00
	jne	_cmd_ver		;next command
	jmp _cmd_done

	; display version
	_cmd_ver:
	mov si, strCmd0
	mov di, cmdVer
	mov cx, 4
	repe	cmpsb
	jne	_cmd_hw		;next command

	call _display_endl
	mov si, strOsName		;display version
	mov al, 0x01
    int 0x21
	call _display_space
	mov si, txtVersion		;display version
	mov al, 0x01
    int 0x21
	call _display_space

	mov si, strMajorVer
	mov al, 0x01
    int 0x21
	mov si, strMinorVer
	mov al, 0x01
    int 0x21
	jmp _cmd_done

	;============== Hardware Info =========================

	_cmd_hw:
	mov si, strCmd0
	mov di, cmdHw
	mov cx, 3
	repe	cmpsb
	jne	_cmd_exit		;next command



	;Floppy drives
		call _display_endl
		mov si, strFloppy		;display Floppy String
		mov al, 0x01
		int 0x21

		int 0x11			; interrupt 0x11 (Equipment List)
		and ax, 1			; 0th bit : Floppy disk drive check
		cmp ax, 1
		je	_hw_floppy_present
		;No floppies
		mov ah, 0x0e
		mov al, '0'			;print Zero
		int 0x10

		_hw_floppy_present:
		int 0x11
		and ax, 0b11000000	;mask out bits 6-7, bit 6-7: number of floppy drives less 1
		shr ax, 5
		add ax, 49			;add 48 to get the ASCII+1
		mov ah,0x0e
		int 0x10
	; End of floppy info display

	;Mouse Info
		call _display_endl
		mov ax, 0
		int 0x33		; MS Mouse Reset Driver and reas status

		cmp ax, 0x0000
		je _hw_mouse_not
		cmp bx, 0xFFFF	; Two buttons
		jne _hw_mouse_2btn
		cmp bx, 0x0002	; Two buttons many drivers
		je _hw_mouse_2btn
		cmp bx, 0x0002	; Three buttons many drivers
		je _hw_mouse_3btn

		mov si, strMouse	;if settings not detected
		mov al, 0x01
		int 0x21
		jmp _hw_mouse_end

		_hw_mouse_not:
		mov si, strNoMouse
		mov al, 0x01
		int 0x21
		jmp _hw_mouse_end

		_hw_mouse_2btn:
		mov si, strMouse2
		mov al, 0x01
		int 0x21
		jmp _hw_mouse_end

		_hw_mouse_3btn:
		mov si, strMouse3
		mov al, 0x01
		int 0x21
		jmp _hw_mouse_end

		_hw_mouse_end:

	; End of mouse info


	; Serial Port info
		call _display_endl

		mov si, strSerialPort
		mov al, 0x01
		int 0x21

		int 0x11
		and ax, 0x0E00	;mask out bits 9-11 : number of Serial ports
		shr ax, 9
		add ax, 48		; add 48 to get ASCII char

		mov ah, 0x0e
		int 0x10
	; End of Serial port info

	; Parallel Port info
		call _display_endl

		mov si, strParallelPort
		mov al, 0x01
		int 0x21

		int 0x11
		and ax, 0x0C00	;mask out bits 15-14 : number of parallel ports
		shr ax, 14
		add ax, 48		; add 48 to get ASCII char

		mov ah, 0x0e
		int 0x10
	; End of Parallel port info


	; Hard Disk Info
		call _display_endl

		mov si, strHardDrives
		mov al, 0x01
		int 0x21

		mov ax, 0x0040
		push es
		mov es,ax
		mov al,[es:0x0075]	; read 40:75 from memory (number of hard drives)
		add al, 48			; add 48 to get ASCII char
		pop es
		mov ah, 0x0e
		int 0x10
	;End of Hard drive info


	; Processor info
		call _display_endl
		mov si, strProcVendor
		mov al, 0x01
		int 0x21
		mov eax, 0			;get vendor id (for CPUID)
		cpuid				;CPU identification
		mov eax, ebx		;prepare for string saving
		mov ebx, edx
		mov edx, 0x00
		mov si, strVendor
		call _save_string

		mov si, strVendor	;print string
		mov al, 0x01
		int 0x21

		call _display_endl
		mov si, strPreocBrand
		mov al, 0x01
		int 0x21

		mov eax, 0x80000000	;get CPU brand String
		cpuid
		cmp eax, 0x80000004
		jb _not_supported
		mov eax, 0x80000002
		mov si, strVendor
		cpuid
		call _save_string
		mov eax, 0x80000003
		cpuid
		call _save_string
		mov eax, 0x80000004
		cpuid
		call _save_string
		add si,16
		mov si,0x00


		mov si, strVendor
		mov al, 0x01
		int 0x21

		_not_supported:
	;End of processor info

	; Memory Info
		call _display_endl

		xor cx, cx			;clear cx
		xor dx, dx			;clear dx
		mov ax, 0xe801		;Interrupt 0x15, E801:  GET MEMORY SIZE FOR >64M CONFIGURATIONS
		int 0x15			; request upper memory size
		jc _hw_mem_err		; CF set on error
		cmp ah, 0x86		; unsupported function
		je _hw_mem_err
		cmp ah, 0x80		; invalid command
		je _hw_mem_err

		mov si, strMemInfo
		mov al, 0x01
		int 0x21

		cmp cx, 0x0000
		je _hw_mem_cx
		jmp _hw_mem_calc

		_hw_mem_cx:
		mov cx,ax		;some bioses return CX=BX=0
		mov dx,bx		;if so copy AX, and BX, to CX/DX


		_hw_mem_calc:
		;Now CX = configured memory 1M to 16M, in K
		;Now DX = configured memory above 16M, in 64K blocks
		;configured memory above 16M in MBs = DX*64/1024 = (DX/2^4)
		shr dx, 4		;divide dx by 2^4
		shr cx, 10		;divide cx by 2^10 (to convert to MBs)
		add cx,dx		;get the total memory

		mov dx, cx
		call _print_hex2dec
		mov si, strMB
		mov al, 0x01
		int 0x21


		jmp _hw_mem_end
		
		_hw_mem_err:
		mov si, strMemErr
		mov al, 0x01
		int 0x21
		
		_hw_mem_end:
	;End of memory info
	
	
	;Bios Date info
		call _display_endl
		mov si, strBiosDate
		mov al, 0x01
		int 0x21
		push es
		mov ax, 0xf000		;BIOS release date is saved in F000:FFF5 (8 bytes)
		mov es, ax
		mov si, 0xfff5
		mov bl,8
		_bios_loop:
			mov al, [es:si]
			mov ah, 0x0e
			int 0x10
			inc si
			dec bl
			cmp bl, 0
			jne _bios_loop
		pop es
	;End of bios date info

	; Dos version info
		call _display_endl
		mov si, strDosVer
		mov al, 0x01
		int 0x21

		mov ah, 0x30	; Get Dos Version
		int 0x21		; AL : major ver, AH: minor ver
		
		mov bx, ax

		xor dx, dx
		mov dl, bl
		add dl, 1
		call _print_hex2dec
		
		mov al, '.'
		mov ah, 0x0e
		int 0x10
		
		mov dl, bh
		call _print_hex2dec
	; End of dos version info

	
	; System date and time
		call _display_endl
		mov si, strDateTime
		mov al, 0x01
		int 0x21
		
		
		mov ah, 0x04	; Get System date
		int 0x1a		;  CH- century, CL- year in BCD
		
		mov al, ch
		and al, 0b11110000
		shr al, 4
		mov dx, ax
		call _print_hex2dec

		mov al, ch
		and al, 0b00001111
		mov dx, ax
		call _print_hex2dec
		
		
		xor ax, ax
		mov al, cl
		and al, 0b11110000
		shr al, 4
		mov dx, ax
		call _print_hex2dec

		mov al, cl
		and al, 0b00001111
		mov dx, ax
		call _print_hex2dec
		
		mov al, '/'
		mov ah, 0x0e
		int 0x10
		
		mov ah, 0x04	; Get System date again
		int 0x1a		
		mov cx, dx
		mov al, ch
		and al, 0b11110000
		shr al, 4
		mov dx, ax
		call _print_hex2dec

		mov al, ch
		and al, 0b00001111
		mov dx, ax
		call _print_hex2dec

		mov al, '/'
		mov ah, 0x0e
		int 0x10
		
		xor ax, ax
		mov al, cl
		and al, 0b11110000
		shr al, 4
		mov dx, ax
		call _print_hex2dec

		mov al, cl
		and al, 0b00001111
		mov dx, ax
		call _print_hex2dec
		
		
		
		mov al, ' '
		mov ah, 0x0e
		int 0x10
		
		mov ah, 0x02	; Get System time
		int 0x1a		;  CH- hours, CL- mins in BCD
		xor ax, ax
		
		;Converts BCD to decimal and prints
		mov al, ch
		and al, 0b11110000
		shr al, 4
		mov dx, ax
		call _print_hex2dec

		mov al, ch
		and al, 0b00001111
		mov dx, ax
		call _print_hex2dec
		
		mov al, ':'
		mov ah, 0x0e
		int 0x10
		
		xor ax, ax
		mov al, cl
		and al, 0b11110000
		shr al, 4
		mov dx, ax
		call _print_hex2dec

		mov al, cl
		and al, 0b00001111
		mov dx, ax
		call _print_hex2dec

	
	jmp _cmd_done
	



	; exit shell
	_cmd_exit:
	mov si, strCmd0
	mov di, cmdExit
	mov cx, 5
	repe	cmpsb
	jne	_cmd_unknown		;next command

	je _shell_end			;exit from shell

	_cmd_unknown:
	call _display_endl
	mov si, msgUnknownCmd		;unknown command
	mov al, 0x01
    int 0x21

	_cmd_done:

	;call _display_endl
	jmp _shell_begin

	_shell_end:
	ret
;=============== Shell End =======================

_get_command:
	;initiate count
	mov BYTE [cmdChrCnt], 0x00
	mov di, strUserCmd

	_get_cmd_start:
	mov ah, 0x10		;get character
	int 0x16

	cmp al, 0x00		;check if extended key
	je _extended_key
	cmp al, 0xE0		;check if new extended key
	je _extended_key

	cmp al, 0x08		;check if backspace pressed
	je _backspace_key

	cmp al, 0x0D		;check if Enter pressed
	je _enter_key

	mov bh, [cmdMaxLen]		;check if maxlen reached
	mov bl, [cmdChrCnt]
	cmp bh, bl
	je	_get_cmd_start

	;add char to buffer, display it and start again
	mov [di], al			;add char to buffer
	inc di					;increment buffer pointer
	inc BYTE [cmdChrCnt]	;inc count

	mov ah, 0x0E			;display character
	mov bl, 0x07
	int 0x10
	jmp	_get_cmd_start

	_extended_key:			;extended key - do nothing now
	jmp _get_cmd_start

	_backspace_key:
	mov bh, 0x00			;check if count = 0
	mov bl, [cmdChrCnt]
	cmp bh, bl
	je	_get_cmd_start		;yes, do nothing

	dec BYTE [cmdChrCnt]	;dec count
	dec di

	;check if beginning of line
	mov	ah, 0x03		;read cursor position
	mov bh, 0x00
	int 0x10

	cmp dl, 0x00
	jne	_move_back
	dec dh
	mov dl, 79
	mov ah, 0x02
	int 0x10

	mov ah, 0x09		; display without moving cursor
	mov al, ' '
    mov bh, 0x00
    mov bl, 0x07
	mov cx, 1			; times to display
    int 0x10
	jmp _get_cmd_start

	_move_back:
	mov ah, 0x0E		; BIOS teletype acts on backspace!
    mov bh, 0x00
    mov bl, 0x07
    int 0x10
	mov ah, 0x09		; display without moving cursor
	mov al, ' '
    mov bh, 0x00
    mov bl, 0x07
	mov cx, 1			; times to display
    int 0x10
	jmp _get_cmd_start

	_enter_key:
	mov BYTE [di], 0x00
	ret

_split_cmd:
	;adjust si/di
	mov si, strUserCmd
	;mov di, strCmd0

	;move blanks
	_split_mb0_start:
	cmp BYTE [si], 0x20
	je _split_mb0_nb
	jmp _split_mb0_end

	_split_mb0_nb:
	inc si
	jmp _split_mb0_start

	_split_mb0_end:
	mov di, strCmd0

	_split_1_start:			;get first string
	cmp BYTE [si], 0x20
	je _split_1_end
	cmp BYTE [si], 0x00
	je _split_1_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_1_start

	_split_1_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb1_start:
	cmp BYTE [si], 0x20
	je _split_mb1_nb
	jmp _split_mb1_end

	_split_mb1_nb:
	inc si
	jmp _split_mb1_start

	_split_mb1_end:
	mov di, strCmd1

	_split_2_start:			;get second string
	cmp BYTE [si], 0x20
	je _split_2_end
	cmp BYTE [si], 0x00
	je _split_2_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_2_start

	_split_2_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb2_start:
	cmp BYTE [si], 0x20
	je _split_mb2_nb
	jmp _split_mb2_end

	_split_mb2_nb:
	inc si
	jmp _split_mb2_start

	_split_mb2_end:
	mov di, strCmd2

	_split_3_start:			;get third string
	cmp BYTE [si], 0x20
	je _split_3_end
	cmp BYTE [si], 0x00
	je _split_3_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_3_start

	_split_3_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb3_start:
	cmp BYTE [si], 0x20
	je _split_mb3_nb
	jmp _split_mb3_end

	_split_mb3_nb:
	inc si
	jmp _split_mb3_start

	_split_mb3_end:
	mov di, strCmd3

	_split_4_start:			;get fourth string
	cmp BYTE [si], 0x20
	je _split_4_end
	cmp BYTE [si], 0x00
	je _split_4_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_4_start

	_split_4_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb4_start:
	cmp BYTE [si], 0x20
	je _split_mb4_nb
	jmp _split_mb4_end

	_split_mb4_nb:
	inc si
	jmp _split_mb4_start

	_split_mb4_end:
	mov di, strCmd4

	_split_5_start:			;get last string
	cmp BYTE [si], 0x20
	je _split_5_end
	cmp BYTE [si], 0x00
	je _split_5_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_5_start

	_split_5_end:
	mov BYTE [di], 0x00

	ret

;======= Display Commands ===================
_display_space:
	push ax
	push bx
	mov ah, 0x0E                            ; BIOS teletype
	mov al, 0x20
    mov bh, 0x00                            ; display page 0
    mov bl, 0x07                            ; text attribute
    int 0x10                                ; invoke BIOS
	pop bx
	pop ax
	ret

_display_endl:
	push ax
	push bx
	mov ah, 0x0E		; BIOS teletype acts on newline!
    mov al, 0x0D
	mov bh, 0x00
    mov bl, 0x07
    int 0x10
	mov ah, 0x0E		; BIOS teletype acts on linefeed!
    mov al, 0x0A
	mov bh, 0x00
    mov bl, 0x07
    int 0x10
	pop bx
	pop ax
	ret

_display_prompt:
	mov si, strPrompt
	mov al, 0x01
	int 0x21
	ret


_print_reg:
;print the CX reg
	push dx;	;save registers dx,ax,bx
	push ax;
	push bx;
	mov dx, cx
	mov cx, 16	; loop for 16 times
	print_loop:
		mov ax, 32768	;set  ax = ob1000....
		and ax, dx		; mask out the MSB of dx
		shr ax, 15		; shift it to right 15 times
		add al, 48		; add 48 to get the ASCII
		mov ah, 0x0e
		int 0x10		; print it out
		shl dx, 1		; shift left dx by 1
	loop print_loop

	print_end:
	pop bx;		; restore the registers
	pop ax
	pop dx
	ret
	

_print_reg_text:
	;prints the contents on EBX
	push eax
	mov al,bl
	mov ah, 0x0e
	int 0x10
	shr ebx,8

	mov al,bl
	mov ah, 0x0e
	int 0x10
	shr ebx,8

	mov al,bl
	mov ah, 0x0e
	int 0x10
	shr ebx,8

	mov al,bl
	mov ah, 0x0e
	int 0x10
	shr ebx,8
	pop eax
	ret

_save_string:
	mov dword [si], eax
	mov dword [si+4], ebx
	mov dword [si+8], ecx
	mov dword [si+12], edx
	add si, 16
	ret


_print_hex2dec:
	;converts the number in dx to decimal and print it
	push ax
	push bx
	push cx
	push si
	mov ax,dx                ; copy number into AX
	mov si,10                ; SI will be our divisor
	xor cx,cx                ; clean up the CX

_non_zero:
	xor dx,dx                ; clear DX
	div si                   ; divide by 10
	push dx                  ; push number onto the stack
	inc cx                   ; increment CX to do it more times
	or ax,ax                 ; end of the number?
	jne _non_zero             ; if not repeat

_write_digits:
	pop dx                   ; get the digit off DX
	add dl,48                ; add 48 to get ASCII
	mov al, dl
	mov ah, 0x0e
	int 0x10
	loop _write_digits


	pop si
	pop cx
	pop bx
	pop ax
	ret

[SEGMENT .data]
    strWelcomeMsg   db  "Welcome to JOSH Ver 0.03", 0x00
	strPrompt		db	"JOSH>>", 0x00
	cmdMaxLen		db	255			;maximum length of commands

	strOsName		db	"JOSH", 0x00	;OS details
	strMajorVer		db	"0", 0x00
	strMinorVer		db	".03", 0x00

	strFloppy		db	"Floppy drives found    : ", 0x00
	strMouse2		db	"Two button mouse installed.", 0x00
	strMouse3		db	"Three button mouse installed.", 0x00
	strMouse		db	"Mouse installed", 0x00
	strNoMouse		db	"Mouse driver not installed.", 0x00
	strParallelPort	db	"Parallel Ports found   : ", 0x00
	strSerialPort	db	"Serial Ports found     : ", 0x00
	strHardDrives	db	"Hard drives installed  : ", 0x00
	strProcVendor	db	"Processor Vendor       : ", 0x00
	strPreocBrand	db	"Processor brand string : ", 0x00
	strMemInfo		db	"Amount of memory found : ", 0x00
	strMemErr		db	"Unable to detect the amount of memory!", 0x00
	strMB			db	" MB", 0x00
	strBiosDate		db	"Bios release date      : ", 0x00
	strDosVer		db	"Dos version            : ", 0x00
	strDateTime		db	"System date and time   : ", 0x00

	cmdVer			db	"ver", 0x00		; internal commands
	cmdExit			db	"exit", 0x00
	cmdHw			db	"hw", 0x00
	cmdProc			db	"-p", 0x00


	txtVersion		db	"version", 0x00	;messages and other strings
	msgUnknownCmd	db	"Unknown command or bad file name!", 0x00

[SEGMENT .bss]
	strUserCmd	resb	256		;buffer for user commands
	cmdChrCnt	resb	1		;count of characters
	strCmd0		resb	256		;buffers for the command components
	strCmd1		resb	256
	strCmd2		resb	256
	strCmd3		resb	256
	strCmd4		resb	256

	cmdCount	resb	1
	strVendor	resb	256
	mmap_ent	resb	10

;********************end of the kernel code********************
