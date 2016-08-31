; multi-segment executable file template.

dades segment
    ; add your data here! 
    row dw 1, 8, 121, 91, 8, 13, 21, 0 
    pkey db "Prem qualsevol tecla per continuar...$"
    rowtext db 29 dup (0)
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

         mov si, 0
         mov dx, 0
         mov cx, 0  
         mov di, 0
    
mentre0: cmp row+si, 0 
         je fi_bucles
         mov bx, 10
         mov ax, row+si
         
    
mentre1: cmp ax, 0
         je fi_mentre1
         div bx
         push dx
         mov dx, 0
         inc cx
         jmp mentre1

fi_mentre1: add si, 2 
            mov bx, 0
            
mentre_pila: cmp bx, 4
             je mentre0
             cmp cx, 0
             jne pushpila
             mov rowtext+di, 32
             inc bx
             inc di
             jmp mentre_pila


pushpila: pop ax
          add ax, 48; Amb aixo farem la representacio ASCII correcta
          mov rowtext+di, al
          dec cx
          inc bx
          inc di
          jmp mentre_pila
          
fi_bucles: mov rowtext+di, 36

                 
            
    lea dx, rowtext
    mov ah, 9
    int 21h        ; mode especial per mostrar a la pantalla
    
    lea dx, pkey
    mov ah, 9
    int 21h        ; mode especial per mostrar a la pantalla
    
    ; wait for any key....    
    mov ah, 1
    int 21h
    
    mov ax, 4c00h ; exit to operating system.
    int 21h    
ends

end start ; set entry point and stop the assembler.
