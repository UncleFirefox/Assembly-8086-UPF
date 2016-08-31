; multi-segment executable file template.

data segment
    ; add your data here!
    row1 dw 1, 4, 6, 4, 1, 0
    row2 dw 10 dup(0)
    ;length db 5
    pkey db "Prem qualsevol tecla per continuar...$"
ends

stack segment
    dw   128  dup(0)
ends

code segment
start:
; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax

    ; add your code here
    
    ;first of all fill the extremes with ones
    
    ;left extreme
    mov row2, 1
    
    ;Now we gotta fill the center
    
    ;First of all, considering we work with words we multiply
    ;length by two, when it reaches that position, we simply jump to the end
    ;and because we filled the extremes we start at 2
    
    mov bx, 2
    
for: cmp cx,0
     je end_for
     mov ax, 0
     add ax, row1+bx
     sub bx, 2
     add ax, row1+bx
     add bx, 2
     mov row2 + bx, ax
     add bx, 2
     mov cx, row1 + bx
     jmp for
     
     
     
end_for:
    
    ;We finally put a 1 on the right
    mov row2 + bx, 1
    
    lea dx, pkey
    mov ah, 9
    int 21h        ; output string at ds:dx
    
    ; wait for any key....    
    mov ah, 1
    int 21h
    
    mov ax, 4c00h ; exit to operating system.
    int 21h    
ends

end start ; set entry point and stop the assembler.
