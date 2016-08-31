dades segment
    ; add your data here! 
    p1 db "1) De quin color es l' aigua?", 0dh, 0ah, "1-Incolora", 0dh, 0ah, "2-Blava", 0dh, 0ah, "3-Groga", 0dh, 0ah, "4-NS/NC", 0dh, 0ah, "$"
    p2 db "2) De quin color es el cel?", 0dh, 0ah, "1-Incolor", 0dh, 0ah, "2-Blau", 0dh, 0ah, "3-Negre", 0dh, 0ah, "4-NS/NC", 0dh, 0ah, "$"
    p3 db "3) De quin color son els teus ulls?", 0dh, 0ah, "1-Marrons", 0dh, 0ah, "2-Blaus", 0dh, 0ah, "3-Verds", 0dh, 0ah, "4-NS/NC", 0dh, 0ah, "$"
    rerronia db "Resposta Erronia", 0dh, 0ah,"$"
    totcorrecte db "Has encertat totes les preguntes!", 0dh, 0ah,"$" 
    fi db "Fi del programa, prem qualsevol tecla per sortir...$"

ends

pila segment stack
    dw   128  dup(0)
ends                    

codi segment
start:

    assume cs: codi, ss: pila, ds: dades

; set segment registers:
    mov ax, dades
    mov ds, ax
    mov es, ax

    ; add your code here
    
    
           ;Imprimim la pregunta
pregunta1: lea dx, p1
           mov ah, 9
           int 21h
           sub sp, 2
           call input_teclado; Esperem input per teclat
           pop cx
           cmp cx, 2; Si la resposta es bona saltem a la seguent pregunta
           je pregunta2
           cmp cx, 79
           je pregunta2
           cmp cx, 3; Si es qualsevol de les altres respostes mostrem error i sortim
           je error
           cmp cx, 80
           cmp cx, 4
           je error
           cmp cx, 81
           je error
           cmp cx, 1; Si es l' ESC evidentment sortim directament
           je fi_esc
           jmp pregunta1; Si es qualsevol altre input tornem a preguntar 
           
           
        
pregunta2: lea dx, p2
           mov ah, 9
           int 21h
           sub sp, 2
           call input_teclado
           pop cx
           cmp cx, 3
           je pregunta3
           cmp cx, 80
           je pregunta3
           cmp cx, 2
           je error
           cmp cx, 79
           je error
           cmp cx, 4
           je error
           cmp cx, 81
           je error
           cmp cx, 1
           je fi_esc
           jmp pregunta2
        
pregunta3: lea dx, p3
           mov ah, 9
           int 21h
           sub sp, 2
           call input_teclado
           pop cx
           cmp cx, 4
           je tot_correcte
           cmp cx, 81
           je tot_correcte
           cmp cx, 2
           je error
           cmp cx, 79
           je error
           cmp cx, 3
           je error
           cmp cx, 1
           je fi_esc
           jmp pregunta2
           
tot_correcte: lea dx, totcorrecte
              mov ah, 9
              int 21h
              
              jmp fi_programa           
           
error: lea dx, rerronia
       mov ah, 9
       int 21h
    
fi_programa:
    
    lea dx, fi
    mov ah, 9
    int 21h
    
    mov ah, 1
    int 21h

fi_esc:    
    
    mov ax, 4c00h ; exit to operating system.
    int 21h    

input_teclado proc
              
              push bp
              mov bp, sp
              
              push ax
              push cx
              
              
bucle1:
    
    ;inhibir
    cli
    in al, 21h
    or al, 10b
    out 21h, al
    sti
    
    ;llegir estat
    
    xor ax, ax
    in al, 64h
    and al, 1
    cmp al, 1
    jne bucle1
    
    ; desinhibir
    cli
    in al, 21h
    and al, 11111101b
    out 21h, al
    sti
    
    ;llegir dades
    
    in al, 60h
    xor cx, cx
    mov cl, al
    
    ;Mirem si el nou event es una tecla pitjada o alliberada i si es alliberada saltem
    and cl, 10000000b
    cmp cl, 00000000b; modificat comparant amb 0 per recomanacio del chema per no tenir problemes amb al intro numeric
    jne bucle1
    
    ;Veiem de quin numero de tecla estem parlant
    and al, 01111111b
    mov [bp+4], ax
    
    call neteja
    
    pop cx
    pop ax
    
    pop bp
    
    ret
              
    input_teclado endp

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
            
codi ends 

end start ; set entry point and stop the assembler.
