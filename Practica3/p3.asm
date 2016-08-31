; multi-segment executable file template.

dades segment
    ; add your data here! 
    row1 dw 15 dup(0)
    row2 dw 15 dup(0) 
    pkey db "Prem qualsevol tecla per continuar...$"
    rowtext db 40 dup (0)
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
    ; lets start sorting the row

inici: mov si, 1
       mov bx, 2
       
       mov row1, 1
       
       mov rowtext, 49
       mov rowtext+1, 13
       mov rowtext+2, 10
       mov rowtext+3, 36
       
       lea cx, rowtext
       push cx
       call imprimir_fila
       add sp, 2
       
main_mentre: cmp si, 9
             je fi_main_mentre
             mov dx, 0
             mov ax, si
             div bx
             cmp dx, 0
             je parells
             lea cx, row2
             push cx
             lea cx, row1
             push cx
             call genera_fila
             add sp, 4
             lea cx, rowtext
             push cx
             lea cx, row2
             push cx
             call crear_ascii
             add sp, 4
             lea cx, rowtext
             push cx
             call imprimir_fila
             add sp, 2
             inc si
             jmp main_mentre
             
parells: lea cx, row1
         push cx
         lea cx, row2
         push cx
         call genera_fila
         add sp, 4
         lea cx, rowtext
         push cx
         lea cx, row1
         push cx
         call crear_ascii
         add sp, 4
         lea cx, rowtext
         push cx
         call imprimir_fila
         add sp, 2
         inc si
         jmp main_mentre                 


fi_main_mentre: 
                 
    
    ;End of the program
    
    lea dx, pkey
    mov ah, 9
    int 21h        ; mode especial per mostrar a la pantalla
    
    ; wait for any key....    
    mov ah, 1
    int 21h
    
    mov ax, 4c00h ; exit to operating system.
    int 21h

genera_fila  proc 
             push bp
             mov bp, sp
             
             push si
             push dx
             push cx
             push bx
             push ax
             mov si, [bp+4]; si = row1
             mov di, [bp+6]; di = row2
                  
    ;first of all fill the extremes with ones
    
    ;left extreme
    mov word ptr [di], 1
    
    ;Now we gotta fill the center
    
    ;First of all, considering we work with words we multiply
    ;length by two, when it reaches that position, we simply jump to the end
    ;and because we filled the extremes we start at 2
    
    mov bx, 2
    mov cx, [si+bx]
    
for: cmp cx,0
     je end_for
     mov ax, 0
     add ax, [si+bx]; 
     sub bx, 2
     add ax, [si+bx]
     add bx, 2
     mov [di + bx], ax
     add bx, 2
     mov cx, [si + bx]
     jmp for
     
     
     
end_for:
    
    ;We finally put a 1 on the right
    mov word ptr [di + bx], 1
    
    ;Recover the registry values before the call
    pop ax
    pop bx
    pop cx
    pop dx
    pop si
    
    pop bp
    
    ret
    
    genera_fila endp
    
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
                 
    
mentre0: mov bx, [bp+4]; 
         cmp word ptr [bx + si], 0;  
         je fi_bucles
         mov ax, [bx+si]; 
         mov dx, 0
         
    
mentre1: cmp ax, 0; Pushing values
         je fi_mentre1
         mov bx, 10
         div bx ; 
         push dx
         mov dx, 0
         inc cx
         jmp mentre1

fi_mentre1: add si, 2 
            mov dx, 0; 
            
mentre_pila: cmp dx, 4
             je mentre0
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
          
fi_bucles: mov bx, [bp+6]
           mov byte ptr [bx+di], 13
           inc bx
           mov byte ptr [bx+di], 10
           inc bx
           mov byte ptr[bx+di], 36
           
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
               
               pop dx
               pop ax
               
               pop bp
               
               ret
               
               imprimir_fila endp    
    
        
codi ends 
               
end start ; set entry point and stop the assembler.
