; Computadors III
; Plantilla per a la practica 5

; segment de pila

pila   		segment stack
		dw	100 dup(?)
pila		ends

; etiquetes

POS_TAULA_RSI_TECLAT		EQU	9*4
POS_TAULA_RSI_TEMPORITZADOR	EQU	8*4
; *** Noves etiquetes aqui ***
; ****************************

; segment de dades

dades		segment

rsi_teclat_vella	DW	2 DUP(?)
rsi_temporitzador_vella	DW	2 DUP(?)

; *** Noves variables aqui ***

    pkey db "Prem qualsevol tecla per continuar...$"
    tecseg db "Tecles pitjades per segon:$"
    varascii db "   $"; Variable que utilitzarem per imprimir resultats numerics
    salt db 0dh, 0ah, "$"
    saltsegons db " segon(s)", 0dh, 0ah, "$"
    pitjat db "S' han pitjat $"
    teclescada db " tecles cada $"
    tipus_tecla db 0; Variable per poder sortir del bucle al main()
    tics dw 0; Comptem les interrupcions de rellotge
    numtecles dw 0; Comptador de tecles total
    tecles_space dw 0; Comptador de tecles que es reseteja cada cop que apretem space
    tecla dw 0; Utilitzada per fer els calculs amb la tecla numerica
    teclescadan dw 0; Utilitzada per fer la mitjana de tecles a la opcio de premer un numero
    

; ****************************

dades		ends


;
;	segment de codi
;

codi		segment
		assume  cs: codi, ss: pila, ds: dades
		
inici:
		mov	ax, dades	; necessari sempre per a fer
		mov	ds, ax      ; que DS apunti a les dades

		CALL	instala_vector_rsi_teclat
		CALL	instala_vector_rsi_temporitzador
        
        xor dx, dx
; *** Programa principal aqui ***

bucle1:cmp tipus_tecla, 1
       je fi_programa

fi_bucle1: jmp bucle1

; *******************************

fi_programa:

		CALL	restaura_vector_rsi_temporitzador
		CALL	restaura_vector_rsi_teclat
		
fi:
		lea dx, pkey
        mov ah, 9
        int 21h
		
		; wait for any key....    
        mov ah, 1
        int 21h
		
		mov	ax, 4c00h
		int	21h


; --- RSI TECLAT ---------------------------------------------------------------

instala_vector_rsi_teclat PROC NEAR
		PUSH	AX
		PUSH	ES
		MOV	AX,0
		MOV	ES,AX
		MOV	AX,ES:[POS_TAULA_RSI_TECLAT]
		MOV	rsi_teclat_vella,AX
		MOV	AX,ES:[POS_TAULA_RSI_TECLAT+2]
		MOV	[rsi_teclat_vella+2],AX
		CLI
		MOV	AX,OFFSET rsi_teclat
		MOV	ES:[POS_TAULA_RSI_TECLAT],AX
		MOV	AX,SEG rsi_teclat
		MOV	ES:[POS_TAULA_RSI_TECLAT+2],AX
		STI
		POP	ES
		POP	AX
		RET
instala_vector_rsi_teclat ENDP

restaura_vector_rsi_teclat PROC NEAR
		PUSH	AX
		PUSH	ES
		MOV	AX,0
		MOV	ES,AX
		CLI
		MOV	AX,rsi_teclat_vella
		MOV	ES:[POS_TAULA_RSI_TECLAT],AX
		MOV	AX,[rsi_teclat_vella+2]
		MOV	ES:[POS_TAULA_RSI_TECLAT+2],AX
		STI
		POP	ES
		POP	AX
		RET
restaura_vector_rsi_teclat ENDP

rsi_teclat	PROC FAR
		PUSH	AX
		PUSH    CX
; *** RSI Teclat aqui ***
           mov ax, 0
           
           in al, 60h
           mov cl, al
    
           ;Mirem si el nou event es una tecla pitjada o alliberada i si es alliberada saltem
           and cl, 10000000b
           cmp cl, 00000000b; modificat comparant amb 0 per recomanacio del chema per no tenir problemes amb al intro numeric
           jne firsitec
           
           ;Veiem de quin numero de tecla estem parlant
           and al, 01111111b
           cmp al, 1; Si es la tecla ESC s' ha acabat el programa
           je firsitec_esc
           cmp al, 57; Si es espai anem a l' event space
           je firsitec_space
           cmp al, 10; Si es una de les tecles numeriques fem els altres calculs
           jle firsitec_num
          
firsitec_normal:
           inc numtecles
           inc tecles_space
           jmp firsitec
           
firsitec_num:
           inc numtecles
           inc tecles_space
           mov ah, 0
           mov tecla, ax
           dec tecla; Li restem 1 per saber quin es el numero exacte
           call eventnumber
           jmp firsitec

firsitec_space:
           inc numtecles
           call eventspace
           jmp firsitec

firsitec_esc:           
           mov tipus_tecla, 1
           jmp firsitec
           
; ***********************
firsitec:		    MOV 	AL,20h		; comanda EOI
		            OUT 	20h,AL		;
		            POP CX
		            POP	AX
		            IRET
rsi_teclat	ENDP

; ------------------------------------------------------------------------------

; --- RSI TEMPORITZADOR --------------------------------------------------------

instala_vector_rsi_temporitzador PROC NEAR
		PUSH	AX
		PUSH	ES
		MOV	AX,0
		MOV	ES,AX
		MOV	AX,ES:[POS_TAULA_RSI_TEMPORITZADOR]
		MOV	rsi_temporitzador_vella,AX
		MOV	AX,ES:[POS_TAULA_RSI_TEMPORITZADOR+2]
		MOV	[rsi_temporitzador_vella+2],AX
		CLI
		MOV	AX,offset rsi_temporitzador
		MOV	ES:[POS_TAULA_RSI_TEMPORITZADOR],AX
		MOV	AX,seg rsi_temporitzador
		MOV	ES:[POS_TAULA_RSI_TEMPORITZADOR+2],AX
		STI
		POP	ES
		POP	AX
		RET
instala_vector_rsi_temporitzador ENDP

restaura_vector_rsi_temporitzador PROC NEAR
		PUSH	AX
		PUSH	ES
		MOV	AX,0
		MOV	ES,AX
		CLI
		MOV	AX,rsi_temporitzador_vella
		MOV	ES:[POS_TAULA_RSI_TEMPORITZADOR],AX
		MOV	AX,[rsi_temporitzador_vella+2]
		MOV	ES:[POS_TAULA_RSI_TEMPORITZADOR+2],AX
		STI
		POP	ES
		POP	AX
		RET
restaura_vector_rsi_temporitzador ENDP

rsi_temporitzador PROC FAR
		PUSH	AX
; *** RSI Temporitzador aqui ***

        INC tics

; ******************************
		MOV 	AL,20h		; comanda EOI
		OUT 	20h,AL		;
		POP	AX
		IRET
rsi_temporitzador ENDP

; ------------------------------------------------------------------------------

; --- SUBRUTINES ---------------------------------------------------------------

; *** Altres subrutines aqui ***

crear_ascii proc 
                 push bp
                 mov bp, sp
                 
                 push di
                 push si
                 push dx
                 push cx
                 push bx
                 push ax

                 
                 mov si, 0
                 mov dx, 0
                 mov cx, 0  
                 mov di, 0
                 
                 ;posem el numero de les dades llegides
                 mov ax, [bp+4]
                 mov bx, 10; anirem dividint per 10
                 
                 ;si es 0 directament escupirem "0$$"
                 cmp ax, 0
                 jne mentre0
                 mov bx, [bp+6]
                 mov byte ptr [bx], 48
                 mov byte ptr [bx+1], 36
                 mov byte ptr [bx+2], 36
                 jmp fi_tot
                 
    
mentre0: cmp ax, 0
         je fi_mentre1
         div bx
         push dx
         mov dx, 0
         inc cx
         jmp mentre0
         

fi_mentre1:  mov dx, 0; 
            
mentre_pila: cmp dx, 3
             je fi_tot
             cmp cx, 0
             jne poppila
             mov bx, [bp+6]; 
             mov byte ptr [bx+di], 36
             inc dx
             inc di
             jmp mentre_pila


poppila:  mov bx, [bp+6]
          pop ax
          add ax, 48; Amb aixo farem la representacio ASCII correcta
          mov [bx+di], al
          dec cx
          inc dx
          inc di
          jmp mentre_pila
          
fi_tot:
           
           pop ax
           pop bx
           pop cx
           pop dx
           pop si
           pop di
           
           pop bp
           
           ret
           
           crear_ascii endp
     
imprimir_fila  proc
               push bp
               mov bp, sp
               
               push ax
               push dx
               
               mov dx, [bp+4]
               mov ah, 9
               int 21h
               
               mov dx, [bp+6]
               mov ah, 9
               int 21h
               
               mov dx, [bp+8]
               mov ah, 9
               int 21h
               
               pop dx
               pop ax
               
               pop bp
               
               ret
               
               imprimir_fila endp

    neteja proc
    	PUSH DX
    	PUSH DS
    	MOV DX,40h
    	MOV DS,DX
    	MOV DX,DS:[80h]
    	MOV DS:[1Ah],DX
    	MOV DS:[1Ch],DX
    	POP DS
    	POP DX	
    	RET
    neteja endp
    
eventnumber proc
    ;Calculem quantes tecles s' han pres cada n segons
    
    push ax
    push dx
    
    mov dx, 0
    mov ax, numtecles
    div tecla
    
    ;Ja tenim a ax aquesta mitjana
    mov teclescadan, ax
   
   
    ;Imprimim missatge "S' han pitjat <teclescadan> tecles cada <tecla> segons"
    
    lea dx, pitjat
    mov ah, 9
    int 21h
    
    ;Passem teclescadan a ASCII
    
    lea bx, varascii
    push bx
    push teclescadan
    call crear_ascii
    add sp, 4
    
    lea dx, varascii
    mov ah, 9
    int 21h
    
    lea dx, teclescada
    mov ah, 9
    int 21h
    
    ;Passem la variable tecla a ASCII
    
    lea bx, varascii
    push bx
    push tecla
    call crear_ascii
    add sp, 4
    
    lea dx, varascii
    mov ah, 9
    int 21h
    
    lea dx, saltsegons
    mov ah, 9
    int 21h
    
    pop dx
    pop ax
    
    ret    
    eventnumber endp

eventspace proc
        
        push ax
        push bx
        push dx
    
		;Calcul real de segons transcorreguts
		mov dx, 0
		mov ax, tics
		mov bx, 18
		div bx
		mov dx, 0
		cmp ax, 0; COMPTE AMB LES DIVISIONS ENTRE ZERO!
		je divisio_zero
		;Ja tenim el numero real de segons a ax	
		
		;Calcul de tecles per segon
		mov bx, ax
		mov dx, 0
		mov ax, tecles_space
		div bx
		mov dx, 0
		jmp continuar
		
divisio_zero:
        mov ax, 0; aqui simplement posem un zero per evitar una preciosa exception
        jmp continuar
		
		
		;Ja tenim el numero de tecles/segon (registre AX)
		; Hem de passarles a ASCII
		   
continuar:       
       lea bx, varascii
       push bx
       push ax
       call crear_ascii
       add sp, 4
		
	   ;Imprimir tecles/segon    
       lea bx, salt
       push bx
       lea bx, varascii
       push bx
       lea bx, tecseg
       push bx
       call imprimir_fila
       add sp, 6
       call neteja
    
    
    ;Restejem el comptador de space al final
    mov tecles_space, 0
    
    ;I el de tics tambe
    mov tics, 0
    
    pop dx
    pop bx
    pop ax
    
    ret
    eventspace endp
    

; ******************************

; ------------------------------------------------------------------------------

codi		ends
		end	inici

