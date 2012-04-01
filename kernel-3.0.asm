;R.M.S. Rathnayake
;090436X


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
	
	call _display_endl
	call _display_endl
	
	mov si, strInfo   ; load message
    mov al, 0x01            ; request sub-service 0x01
    int 0x21
	call _display_endl

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
	jne	_cmd_list		;next command

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

	

	_cmd_list:
	mov si, strCmd0
	mov di, cmdInfo
	mov cx, 7
	repe cmpsb
	jne _cmd_HardInfo  ;next command
	
	call _info
	jmp _cmd_done

	_cmd_HardInfo:
	mov si, strCmd0
	mov di, cmdHardInfo
	mov cx, 4
	repe	cmpsb
	jne	_cmd_exit		;next command

	
	
	call _display_endl
	call _cpuid
	call _bios_date
	
	call _mem_detect
	call _diskette
	call _hard_drive
	call _display_serial_ports	
	call _display_endl
	
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

_display_space:
	mov ah, 0x0E                            ; BIOS teletype
	mov al, 0x20
    mov bh, 0x00                            ; display page 0
    mov bl, 0x07                            ; text attribute
    int 0x10                                ; invoke BIOS
	ret

_display_endl:
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
	ret

_display_serial_ports:
	call _display_endl
	
	mov si, strSerial
		mov al, 0x01
		int 0x21
		
	xor ax, ax
	int 0x11
	and ax, 0xe00
	shr ax, 9
	add ax, 48
	mov ah, 0x0e
	int 0x10
	ret 
	
	
	
_diskette:
	call _display_endl
	mov si, strDiskette
	mov al, 0x01
	int 0x21
	xor ax, ax
	int 0x11
	and ax, 0x80
	shr ax, 6
	add ax, 49
	mov ah, 0x0e
	int 0x10
	ret

_info:
	call _display_endl
	call _display_endl
	mov si, strList
	mov al, 0x01
	int 0x21
	call _display_endl
	mov si, strVer
	mov al, 0x01
	int 0x21
	call _display_endl
	mov si, strHard
	mov al, 0x01
	int 0x21
	call _display_endl
	mov si, strReboot
	mov al, 0x01
	int 0x21
	call _display_endl
		
	ret

_hard_drive:
	call _display_endl

		mov si, strHardDrive
		mov al, 0x01
		int 0x21

		mov ax, 0x0040
		push es
		mov es,ax
		mov al,[es:0x0075]	; read 40:75 
		add al, 48			
		pop es
		mov ah, 0x0e
		int 0x10
		ret
		
_bios_date:
		call _display_endl
		mov si, strBios
		mov al, 0x01
		int 0x21
		push es
		mov ax, 0xf000		;BIOS release date is saved in F000:FFF5
		mov es, ax
		mov si, 0xfff5
		mov bl,8
		_loop:
			mov al, [es:si]
			mov ah, 0x0e
			int 0x10
			inc si
			dec bl
			cmp bl, 0
			jne _loop
		pop es
		ret

	
	
_cpuid:
	call _display_endl
	mov si, strVendor
		mov al, 0x01
		int 0x21
	push eax
	push ebx
	push ecx
	push edx
	
	xor eax, eax
	mov eax, 0x00
	cpuid
	mov [strCPUID], ebx
	mov [strCPUID+4], edx
	mov [strCPUID+8], ecx
	
	pop edx
	pop ecx
	pop ebx
	pop eax
	mov si, strCPUID
	call _disp_str
	call _display_endl
	mov si, strCpuType
		mov al, 0x01
		int 0x21
	
	mov eax, 0x80000002
	mov si, strBrand
	cpuid
	call _string_store
	mov eax, 0x80000003
	cpuid
	call _string_store
	mov eax, 0x80000004
	cpuid
	call _string_store
	add si,16
	mov si,0x00


	mov si, strBrand
	mov al, 0x01
	int 0x21
	
	ret
	
_mem_detect:
	call _display_endl
	
	xor ax,ax
	xor bx,bx
	xor cx,cx
	xor dx,dx
	
	mov ax, 0xe801
	int 0x15
	jc _error
	cmp ah, 0x86		; unsupported function
	je _error
	cmp ah, 0x80		; invalid command
	je _error
	mov si, strMemory
	mov al, 0x01
	int 0x21
	cmp cx, 0x0000
	je _cx_zero
	jmp _mem_calc

_cx_zero:
	mov cx,ax		;some bios return CX=DX=0
	mov dx,bx		;copy AX, and BX, to CX/DX

_mem_calc:
	
	shr dx, 4		;divide dx by 2^4
	shr cx, 10		;divide cx by 2^10 (to convert to MBs)
	add cx,dx		;get the total memory
	mov dx, cx
	call _hex2dec
	mov si, strMB
	mov al, 0x01
	int 0x21


	jmp _memory_detected

_error:
	mov si, strMemError
	mov al, 0x01
	int 0x21	
	
_memory_detected:
	ret
	
		

	
_disp_str:
	lodsb ; load next character
	or al, al ; test for NUL character
	jz _printed
	mov ah, 0x0E ; BIOS teletype
	mov bh, 0x00 ; display page 0
	mov bl, 0x07 ; text attribute
	int 0x10 ; invoke BIOS
	jmp _disp_str
_printed:
	ret
	

_display_test:
	mov si, strTest
	mov al, 0x01
	int 0x21
	ret

_display_prompt:
	mov si, strPrompt
	mov al, 0x01
	int 0x21
	ret
	
_string_store:
	mov dword [si], eax
	mov dword [si+4], ebx
	mov dword [si+8], ecx
	mov dword [si+12], edx
	add si, 16
	ret

_hex2dec:
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
	

_print_char:
	push ax                  ; save that AX register
	mov al, dl
        mov ah, 0x0E            ; BIOS teletype acts on newline!
        mov bh, 0x00
        mov bl, 0x07
        int 0x10

	pop ax                   ; restore that AX register
	ret


[SEGMENT .data]
    strWelcomeMsg   db  "Welcome to JOSH Ver 0.04 Hacked by Sunimal Rathnayake", 0x00
	strPrompt		db	"JOSH>>", 0x00
	strInfo			db	"Enter cmdlist to view available commands",0x00
	strVer			db	"ver     -- version",0x00
	strHard			db	"hdif    -- hardware information",0x00
	strReboot		db	"exit    -- reboot",0x00
	strList			db	"cmdlist -- available commands",0x00
	cmdMaxLen		db	255			;maximum length of commands

	strOsName		db	"JOSH", 0x00	;OS details
	strMajorVer		db	"0", 0x00
	strMinorVer		db	".04", 0x00
	strTest			db	"This is just a test.", 0x00
	strMemError 	db	"Memory detect error",0x00
	strMemory		db	"Total Memory               : ",0x00
	strMB			db	" MB",0x00
	strHardDrive	db	"Hard drives installed      : ",0x00
	strSerial 		db	"Serial ports available     : ",0x00
	strDiskette		db	"Floppy drives available    : ",0x00
	strVendor		db	"CPU vendor                 : ",0x00
	strCpuType		db	"CPU type                   : ",0x00
	strBios			db	"Bios release date          : ",0x00

	cmdVer			db	"ver", 0x00		; internal commands
	cmdExit			db	"exit", 0x00
	cmdTest			db	"test", 0x00
	cmdHardInfo		db	"hdif", 0x00
	cmdDiskette		db	"floppy", 0x00
	cmdInfo			db	"cmdlist", 0x00
	

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
	strBrand	resb	256
	strCPUID	resb	16		;string variable

;********************end of the kernel code********************
