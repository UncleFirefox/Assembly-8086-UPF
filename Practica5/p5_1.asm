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
; ****************************

dades		ends


;
;	segment de codi
;

codi		segment
		assume  cs: codi, ss: pila, ds: dades
		
inici: 
		mov	ax, dades	; necessari sempre per a fer
		mov	ds, ax		; que DS apunti a les dades

		CALL	instala_vector_rsi_teclat
		CALL	instala_vector_rsi_temporitzador

; *** Programa principal aqui ***

bucle_esc: in al, 60h
           mov cl, al
    
           ;Mirem si el nou event es una tecla pitjada o alliberada i si es alliberada saltem
           and cl, 10000000b
           cmp cl, 00000000b; modificat comparant amb 0 per recomanacio del chema per no tenir problemes amb al intro numeric
           jne bucle_esc
           
           ;Veiem de quin numero de tecla estem parlant
           and al, 01111111b
           cmp al, 1; Si es la tecla ESC s' ha acabat el programa
           je fi_programa
           jmp bucle_esc


; *******************************

fi_programa:

		CALL	restaura_vector_rsi_temporitzador
		CALL	restaura_vector_rsi_teclat

fi:
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
; *** RSI Teclat aqui ***
; ***********************
		MOV 	AL,20h		; comanda EOI
		OUT 	20h,AL		;
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
; ******************************
		MOV 	AL,20h		; comanda EOI
		OUT 	20h,AL		;
		POP	AX
		IRET
rsi_temporitzador ENDP

; ------------------------------------------------------------------------------

; --- SUBRUTINES ---------------------------------------------------------------

; *** Altres subrutines aqui ***
; ******************************

; ------------------------------------------------------------------------------

codi		ends
		end	inici

