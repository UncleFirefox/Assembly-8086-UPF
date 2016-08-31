; multi-segment executable file template.

dades segment
    ; add your data here! 
    msg db "S' ha pitjat la tecla amb codi:$"
    num db "  $"
    salt db 0dh, 0ah, "$"
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
    
    ; port que detecta tecla 64h
    
    
bucle1:
    
    ; RECORDEM QUE NOMES HA D' EXECUTAR-SE UN COP (TECLA PITJADA)
    
    
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
    mov cl, al
    
    ;Mirem si el nou event es una tecla pitjada o alliberada i si es alliberada saltem
    and cl, 10000000b
    cmp cl, 00000000b; modificat comparant amb 0 per recomanacio del chema per no tenir problemes amb al intro numeric
    jne bucle1
    
    ;Veiem de quin numero de tecla estem parlant
    and al, 01111111b
    cmp al, 1; Si es la tecla ESC s' ha acabat el programa
    je fi_programa
    lea bx, num
    push bx
    push ax
    call crear_ascii
    add sp, 4
    
    ;I amb les dades que necessitem ja nomes imprimim el resultat
    lea bx, salt
    push bx
    lea bx, num
    push bx
    lea bx, msg
    push bx
    call imprimir_fila
    add sp, 6
    call neteja
    jmp bucle1  
    
    


fi_programa:
                 
    
    ;End of the program
    
    mov ax, 4c00h ; exit to operating system.
    int 21h
    
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
                 
    
mentre0: cmp ax, 0
         je fi_mentre1
         div bx
         push dx
         mov dx, 0
         inc cx
         jmp mentre0
         

fi_mentre1:  mov dx, 0; 
            
mentre_pila: cmp dx, 2
             je fi_tot
             cmp cx, 0
             jne poppila
             mov bx, [bp+6]; 
             mov byte ptr [bx+di], 32
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
            
codi ends 

end start ; set entry point and stop the assembler.
