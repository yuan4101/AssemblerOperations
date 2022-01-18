
.model small              
.stack 200h
    
.data     
    ;Constantes
    ln          equ 10
    rt          equ 13
    fn          equ '$'
    zero        db  '     0', fn
    base        db  -1
    msgLn       db  ln, rt, fn
    zeroEnd     dw  0, fn
    ochoEnd     dw  8, fn
    ochoBin     dw  8 dup ('$')
    
    ;Acceso
    msgPassword         db  "Ingrese la contrasenia: ", fn
    password            db  "grupo6cc"
    passCount           db  8 
    passTry             db  4
    msgPassCorrect      db  "Contrasenia correcta: BIENVENIDO", ln, ln, rt, fn
    msgPassIncorrect    db  "Contrasenia invalida, repita", ln, ln, rt, fn
    msgSalida           db ln, rt, "Saliendo...", fn
    
    ;Menu
    msgMenu db  ln, ln, rt, "---------- Menu principal ----------", ln, ln, rt
    mOp1    db  "1. Suma y diferencia", ln, rt
    mOp2    db  "2. Multiplicacion y division", ln, rt
    mOp3    db  "3. Hex to bin con operandos logicos", ln, rt
    mOp4    db  "4. Serie alternada", ln, rt
    mOp5    db  "5. Salir", ln, ln, rt
    mSel    db  "> ", fn 
    
    ;Lectura          
    inputNum        db  ln, rt, "Ingrese un numero entre -128 a 127: ", fn
    num1            dw  0
    num2            dw  0
    sign            dw  20h, fn
    c               dw  0, fn
    d               dw  0, fn
    u               dw  0, fn
    resultado       dw  0
    countCaracter   db  ?
    buffer          db  5, ?, 5 dup('')
    
    ;Opcion 1
    msgOpcion1          db  ln, rt, "---------- Suma y diferencia ----------", fn
    msgResultadoSuma    db  ln, rt, "El resultado de la suma es: ", fn
    msgResultadoResta   db  ln, rt, "El resultado de la resta es: ", fn 
    
    
    ;Opcion 2
    msgOpcion2          db  ln, rt, "---------- Multiplicacion y division ----------", fn
    msgResultadoMul     db  ln, rt, "El resultado de la multiplicacion es: ", fn   
    msgResultadoDiv     db  ln, rt, "El resultado de la division entera es: ", fn
    msgDivZero          db  ln, rt, "No se puede divir por 0", fn     
    
    
    ;Opcion 3
        ; mensajes 
        c_msgLinea1         db 10,13, "",'$'
        c_msgTitulo         db 10,13, "---------- Hexadecimal a binario y operaciones ----------",'$'
        c_msgLinea2         db 10,13, "",'$'
        c_msgPedirc_hex1    db 10,13, 'Digite el numero 1 en hexadecimal entre 00 y FF : $'
        c_msgPedirc_hex2    db 10,13, 'Digite el numero 2 en hexadecimal entre 00 y FF : $'
        c_msgc_hexa         db 0dh,0ah, 'Numero en hexadecimal  : $'
        c_msgc_bin          db 0dh,0ah, 'Numero en binario  :     $'
        c_msgError          db 0dh,0ah, 'Entrada invalida $'
        c_msgSalida         db 0dh,0ah, 'Ha salido del programa $'
        c_msgLEsp           db 0dh,0ah, '                         $'
        msgOperaciones      db ln, rt, "---------- Operaciones ----------", fn
         
        ;mensaje op en binario  
        c_msgnot           db 0dh,0ah, 'NOT binario:     $'
        c_msgand           db 0dh,0ah, 'AND binario:     $'
        c_msgxor           db 0dh,0ah, 'XOR binario:     $'
        c_msgor            db 0dh,0ah, 'OR  binario:     $'  
         
        ;mensaje op en hexa 
        c_msgnoth          db 0dh,0ah, 'NOT hexadecimal: $'
        c_msgandh          db 0dh,0ah, 'AND hexadecimal: $'
        c_msgxorh          db 0dh,0ah, 'XOR hexadecimal: $'
        c_msgorh           db 0dh,0ah, 'OR  hexadecimal: $'
        
        ;variables hexa
        c_maxL             db 4
        c_c_hexL           db 0
        c_hex              dw 0,'$'  
        c_hex1             dw 0,'$'  
        c_hex2             dw 0,'$'  
        c_hexp             dw 8 dup ('$') 
         
        ;variable bin 
        c_cont             dw 8 ,'$'  
        c_bin              dw 8 dup ('$')
        c_bin1             dw 8 dup ('$')
        c_bin2             dw 8 dup('$') 
        c_bin1aux          dw 8 dup('$')
        c_bin2aux          dw 8 dup('$')
        
        ;bandera
        c_yp               db 0
    
    ;Opcion 4
    reps        db  15, fn
    n           db  1, fn
    pot         db  1, fn
    msgOpcion4  db  ln, rt, "---------- Primeros 15 numeros serie alternada ----------", fn
             
    
.code

; Ajustes de la consola
cls proc
    mov ax, 0600h  
    mov bh, 70h    
    mov cx, 0000h  
    mov dx, 184Fh 
    int 10h
        
    ret  
endp

; Finaliza el programa
finProgram proc
    mov ax, 4c00h
    int 21h
    ret
endp 

resetCh proc
    mov sign, 20h
    mov c, 0h
    mov d, 0h
    mov u, 0h
    ret
endp

; Lee el primer caracter ingresado
leerCaracter proc       
    mov ah, 01
    int 21h

    ret
endp

; Imprime una cadena de texto 
impCadena proc
    mov ah, 09h
    int 21h
    ret
endp

; Hex to ASCCII
ajustarU proc
    add al, 30h
    ret
endp

; ASCCII to Hex
ajustarD proc  
    sub al, 30h
    ret
endp

; Hex to ASCCII
ajustarUx proc
    add ax, 30h
    ret
endp

; ASCCII to Hex
ajustarDx proc  
    sub ax, 30h
    ret
endp

; Muestra el menu
mostrarMenu proc
    mov dx, offset msgMenu
    call impCadena   
    ret
endp

; Lee un numero de maximo 3 digitos con signo
leerNumero proc  
    mov dx, offset inputNum
    call impCadena
    
    mov dx, offset buffer
    mov ah, 0Ah
    int 21h
    
    mov bl, buffer[1]
    mov countCaracter, bl 
        
comprobar:
    mov al, buffer[2]
    cmp al, "-"
    je negativo
    jmp positivo
    ret 
     
positivo:
    mov al, countCaracter            
    call ajustarU
    
    cmp al, "3"
    je centenasP
    
    cmp al, "2"
    je decenasP
    
    cmp al, "1"
    je unidadesP
    
    mov ax, 0000h
    ret

negativo:
    sub bl, 1
    mov al, bl            
    call ajustarU
    
    cmp al, "3"
    je centenasN
    
    cmp al, "2"
    je decenasN
    
    cmp al, "1"
    je unidadesN
    
    cmp al, "0"
    ret

    centenasN: 
    mov al, buffer[3] 
    call ajustarD
    mov ah, 00h
    mov bl, -100
    imul bl
    mov c, ax
    
    decenasN: 
    mov bl, countCaracter
    mov bh, 00h
    mov al, buffer[bx]
    call ajustarD
    mov ah, 00h
    mov bl, -10
    imul bl
    mov d, ax
    
    unidadesN:
    mov bl, countCaracter
    mov bh, 00h
    mov al, buffer[bx+1]
    call ajustarD
    mov ah, 00h
    mov bl, -1
    imul bl
    mov u, ax
    
    mov ax, 0000h
    mov ax, c
    add ax, d
    add ax, u
    
    call resetCh
    
    ret
    
    centenasP: 
    mov al, buffer[2] 
    call ajustarD
    mov ah, 00h
    mov bl, 100
    imul bl
    mov c, ax
    
    decenasP: 
    mov bl, countCaracter
    mov bh, 00h
    mov al, buffer[bx]
    call ajustarD
    mov ah, 00h
    mov bl, 10
    imul bl
    mov d, ax
    
    unidadesP:
    mov bl, countCaracter
    mov bh, 00h
    mov al, buffer[bx+1]
    call ajustarD
    mov ah, 00h
    mov u, ax
    
    mov ax, 0000h
    mov ax, c
    add ax, d
    add ax, u
    
    call resetCh
    
    ret
endp

; Imprime un numero con signo
impResultado proc
    
    mov ax, resultado
    call ajustarUx
    cmp ax, "0"
    call ajustarDx
    jl menor
    jg mayor
    je zerol
    
menor:      
    mov bx, -1
    mul bx
    mov resultado, ax
    mov sign, 2dh
    
    mov dx, offset sign
    call impCadena
    mov ax, resultado
    call print_ax     
    
    ret   
        
mayor:      
    mov sign, 20h
    
    mov dx, offset sign
    call impCadena
    mov ax, resultado
    call print_ax      
    
    ret   
    
zerol:
    mov dx, offset zero
    call impCadena
    
    ret

endp

print_ax proc
cmp ax, 0
jne print_ax_r
    push ax
    mov al, '0'
    mov ah, 0eh
    int 10h
    pop ax
    ret 
print_ax_r:
    pusha
    mov dx, 0
    cmp ax, 0
    je pn_done
    mov bx, 10
    div bx    
    call print_ax_r
    mov ax, dx
    add al, 30h
    mov ah, 0eh
    int 10h    
    jmp pn_done
pn_done:
    popa  
    ret  
endp

; Hex to bin
hexToBin proc
    
    push ds
    mov ax,0h
    push ax
    mov ax,@data   ;inicializar ds        
    mov ds,ax
    
    ; Variables Hexa
    mov c_maxL, 4
    mov c_c_hexL, 0   
    mov ax, zeroEnd
    mov c_hex, ax
    mov c_hex1, ax
    mov c_hex2, ax
    mov ax, ochoBin
    mov c_hexp, ax
    
    ; Variables Bin
    mov ax, ochoEnd
    mov c_cont, ax
    mov ax, ochoBin
    mov c_bin, ax
    mov c_bin1, ax
    mov c_bin2, ax
    mov c_bin1aux, ax
    mov c_bin2aux, ax 
    
    ; Bandera
    mov c_yp, 0
    
    call printMsgInicio
    call saltoLinea
    ;numero 1 
    call pedirc_hexa1 
    call convH2B   
         mov ax,c_hex
         mov c_hex1,ax 
    call printc_msgc_hexa
    call mostrarNumc_hex1 ;c_hex1 
    call printc_msgc_bin
    call mostrarNumc_bin1 ;c_bin1 
    
    call saltoLinea
    ;NOT del primer numero c_bin
    lea dx, c_msgnot   
    call impStr        
    call notc_bin1     
    call impc_bin1     
            
          
    ;NOT del primer numero en c_hexa    
    lea dx, c_msgnoth
    call impStr 
    call convertirc_binc_hex1
    
     
    
    call saltoLinea                 
    mov c_bin,0 
    
    ;numero 2   
    mov dx,dx
    call pedirc_hexa2
    call convH2B  
         mov ax,c_hex
         mov c_hex2,ax 
         
    ;mostrar num2    
    call printc_msgc_hexa
    call mostrarNumc_hex2
    call printc_msgc_bin
    call mostrarNumc_bin2  
    
    call saltoLinea
    
    ;not del segundo numero c_bin
    lea dx, c_msgnot
    call impStr
    call notc_bin2 
    call impc_bin2 
     
     
    ;not del segundo numero en c_hexa    
    lea dx, c_msgnoth
    call impStr 
    call impc_hex
    call convertirc_binc_hex2
   
    ;operaciones 
    call saltoLinea
    mov dx, offset msgOperaciones
    call impCadena 
    
    call saltoLinea
    ;AND  c_binario
    lea dx,c_msgand
    call impStr   
    call c_bin_and 
    call impc_bin  
    ;AND c_hexa
    lea dx, c_msgandh
    call impStr 
    call convertirc_binc_hexOrig
            
    call saltoLinea        
    ;OR       
    lea dx,c_msgor        
    call impStr  
    call c_bin_or 
    call impc_bin  
    ;OR c_hexa
    lea dx, c_msgorh
    call impStr 
    call convertirc_binc_hexOrig      
          
    call saltoLinea   
    ;XOR   
    lea dx,c_msgxor
    call impStr  
    call c_bin_xor 
    call impc_bin 
    ;XOR c_hexa
    lea dx, c_msgxorh
    call impStr 
    call convertirc_binc_hexOrig

    call pause
    jmp menu
    
endp 

impc_bin proc
    lea dx, c_bin 
    call impStr
    ret  
endp
     
 ;impresion de c_bin1  
impc_bin1 proc
    lea dx, c_bin1 
    call impStr
    ret  
endp

  ;impresion de c_bin  
impc_bin2 proc
    
    lea dx, c_bin2 
    call impStr
    ret  
endp

;impresion de c_hexadecimal  
impc_hex proc
    lea dx, c_hexp 
    call impStr
    ret  
endp

;salto de linea 
saltoLinea proc 
    lea dx, c_msgLEsp
    call impStr   
    ret
endp 
       
       
printMsgInicio proc 
    lea dx, c_msgLinea1
    call impStr 
    
    lea dx, c_msgTitulo
    call impStr
    
    lea dx, c_msgLinea2
    call impStr
    
    ret
endp

printc_msgPedirc_hex1 proc 
    lea dx, c_msgPedirc_hex1
    mov ah, 09h
    int 21h    
    ret
endp

printc_msgPedirc_hex2 proc 
    lea dx, c_msgPedirc_hex2
    mov ah, 09h
    int 21h    
    ret
endp

printc_msgc_hexa proc 
    lea dx, c_msgc_hexa
    mov ah, 09h
    int 21h    
    ret
endp

printc_msgc_bin proc 
    lea dx, c_msgc_bin
    mov ah, 09h
    int 21h    
    ret
endp

mostrarError proc
    lea dx,c_msgError
    mov ah,9
    int 21h
    jmp menu 
ret
endp

;procedimietnos pedir numeros 

pedirc_hexa1  proc 
    call printc_msgPedirc_hex1
    
    lea dx,c_maxL       
    mov ah,10  
    int 21h          
   
    mov cl,c_c_hexL     
    cmp cl,2
    jne mostrarError  
   
    mov ch,0            
    lea si,c_hex     
    lea di,c_bin
    
    ret 
    call mostrarError                   

endp

pedirc_hexa2  proc 
    call printc_msgPedirc_hex2
    lea dx,c_maxL       
    mov ah,10  
    int 21h              
   
    mov cl,c_c_hexL     
    cmp cl,2
    jne mostrarError   
   
    mov ch,0             
    lea si,c_hex   
    lea di,c_bin
    
    ret 
    call mostrarError                  
    
ret 
endp

;mostrar numero1 en c_hexa y en c_bin 
mostrarNumc_bin1 proc 

    lea dx,c_bin1
    mov ah,9
    int 21h 
    
    xor dx,dx
   ret 
endp

mostrarNumc_hex1 proc 
    lea dx,c_hex1
    mov ah,9
    int 21h  
    ret
endp

;mostrar numero2 en c_hexa
mostrarNumc_bin2 proc 

    lea dx,c_bin2
    mov ah,9
    int 21h 
    
    xor dx,dx
    ret 
endp

mostrarNumc_hex2 proc 
    lea dx,c_hex2
    mov ah,9
    int 21h  
    ret
endp    

;c_binario
c_binary proc 
    push cx           
    mov cl,4         
    shl al,cl    
    mov cx,4   
    
    sc_bin:
    shl al,1                
    jc one                
    mov byte ptr [di],'0'    
    jmp restart                 
    one:                        
    mov byte ptr [di],'1' 
    
    restart:
    add di,1  
    loop sc_bin 
    
    pop cx                      
    ret
endp

;convertir num 
convH2B PROC
    
    s:
    mov al,[si]           
    cmp al,'9'            
    ja english            
    cmp al,'0'             
    jb mostrarErr         
    jmp toc_binary          
    
    english:
    or al,32           
    cmp al,'a'            
    jb mostrarErr         
    cmp al,'f'            
    ja mostrarErr         
    sub al,7             
    toc_binary:
    sub al,30h           
    call c_binary 
      
    inc si               
    loop s             
          
    jmp exit    
    
    mostrarErr:                   
    lea dx,c_msgError
    mov ah,9
    int 21h 
    
    exit: 
    
    cmp c_yp, 0
    jz rbn1
    jnz rbn2 
    rbn1: 
    mov c_yp, 1 
    call c_binINc_bin1  
    
    ret
    rbn2:
    call c_binINc_bin2 
    
    ret
endp

convertirc_binc_hexOrig proc 
    mov si, 0 
    mov di, 0
    mov cx, 0  
    MOV BX,0
    MOV CL,1  
    mov dx,dx 
    inicio:
    cmp di,8
    je salida
    mov ax, c_bin[di] 
    cmp al,30h
    jne check
     
    c_continuar:
    SUB AL,30H    
                  
    SHL BX,CL    
    OR BL,AL  
    ;inc si
    inc di   
    jmp inicio   
    check:
    cmp al,31h
    jne fin
    jmp c_continuar
        
    trans:
    mov cl,1
    mov ch,0
    salida:
    
    cmp ch,4
    je fin 
    inc ch          
   
    mov dl,bh       
    shr dl,4        
   
    cmp dl,0AH     
    jl digito       
   
    add dl,37H      
    mov ah,2 
    INT 21H  
   
    rol bx,4        
    jmp salida
    
    digito:
    add dl,30H       
    mov ah,2 
    INT 21H
   
    rol bx,4           
    jmp salida 
    
    fin:
    ret
    
ret 
endp

convertirc_binc_hex1 proc 
    mov si, 0 
    mov di, 0
    mov cx, 0  
    MOV BX,0
    MOV CL,1  
    mov dx,dx 
    inicio1:
    cmp di,8
    je salida1
    mov ax, c_bin1[di] 
    cmp al,30h
    jne check1
     
    c_continuar1:
    SUB AL,30H     
                     
    SHL BX,CL       
    OR BL,AL  
    
    inc di   
    jmp inicio1   
    check1:
    cmp al,31h
    jne fin1
    jmp c_continuar1
        
    trans1:
    mov cl,1
    mov ch,0
    salida1:
    
    cmp ch,4
    je fin1 
    inc ch           
   
    mov dl,bh        
    shr dl,4         
   
    cmp dl,0AH      
    jl digito1       
   
    add dl,37H      
    mov ah,2 
    INT 21H  
    
    rol bx,4         
    jmp salida1
    
    digito1:
    add dl,30H         
    mov ah,2 
    INT 21H
    
    rol bx,4           
    jmp salida1 
    
    fin1:
    ret
    
ret 
endp

convertirc_binc_hex2 proc 
    
    mov si, 0 
    mov di, 0
    mov cx, 0  
    MOV BX,0
    MOV CL,1  
    mov dx,dx 
    inicio2:
    cmp di,8
    je salida2
    mov ax, c_bin2[di] 
    cmp al,30h
    jne check2
     
    c_continuar2:
    SUB AL,30H     
                       
    SHL BX,CL      
    OR BL,AL  
    
    inc di   
    jmp inicio2   
    check2:
    cmp al,31h
    jne fin2
    jmp c_continuar2
        
    trans2:
    mov cl,1
    mov ch,0
    salida2:
    
    cmp ch,4
    je fin2 
    inc ch          
   
    mov dl,bh        
    shr dl,4        
   
    cmp dl,0AH      
    jl digito2         
   
    add dl,37H       
    mov ah,2 
    INT 21H  
    
    rol bx,4        
    jmp salida2
    
    digito2:
    add dl,30H         
    mov ah,2 
    INT 21H
    
    rol bx,4           
    jmp salida2 
    
    fin2:
    ret
 
endp 
    

c_binINc_bin1 proc 
    mov si, 0 
    mov di, 0
    mov cx, 0
    rut:     
    cmp cx, 8
    jz finR
    mov ax, c_bin[si]
    mov c_bin1[di], ax
    inc cx
    inc si 
    inc di
    jmp rut 
    finR:
    call c_binINc_bin1aux 
    ret

endp 

c_binINc_bin2 proc 
    mov si, 0
    mov cx, 0
    rut2:     
    cmp cx, 8
    jz finR2
    mov ax, c_bin[si]
    mov c_bin2[si], ax
    inc cx
    inc si
    jmp rut2 
    finR2:
     call c_binINc_bin2aux
    ret
    
endp

c_binINc_bin1aux proc 
    mov si, 0 
    mov di, 0
    mov cx, 0
    rutaux:     
    cmp cx, 8
    jz finRaux
    mov ax, c_bin1[si]
    mov c_bin1aux[di], ax
    inc cx
    inc si 
    inc di
    jmp rutaux 
    finRaux:
    ret
    
endp 

c_binINc_bin2aux proc 
    mov si, 0
    mov cx, 0
    rut2aux:     
    cmp cx, 8
    jz finR2aux
    mov ax, c_bin2[si]
    mov c_bin2aux[si], ax
    inc cx
    inc si
    jmp rut2aux 
    finR2aux:
    ret
    
endp 

notc_bin1 proc 
    mov si, 0 
    rutnot:     
    cmp si, 8
    jz finnot
    mov ax, c_bin1[si]  
   
    cmp ah, 31h 
    je p30h      
    jne p31h 
    p30h: 
    mov ah, 30h 
    jmp pb
    p31h: 
    mov ah, 31h
    jmp pb  
    pb:
    cmp al, 31h  
    je p30l      
    jne p31l 
    p30l: 
    mov al, 30h 
    jmp pf
    p31l: 
    mov al, 31h
    jmp pf 
    pf:
    mov c_bin1[si], ax
    add si, 2 
    jmp rutnot 
    finnot:
    ret 

endp

notc_bin2 proc
      
    mov si, 0 
    
    rutnot2: 
    cmp si, 8
    jz finnot2
    mov ax, c_bin2[si] 
    
    cmp ah, 31h 
    je p30h2      
    jne p31h2
     
    p30h2: 
    mov ah, 30h 
    jmp pb2
    
    p31h2: 
    mov ah, 31h
    jmp pb2
       
    pb2:
    cmp al, 31h  
    je p30l2      
    jne p31l2
     
    p30l2: 
    mov al, 30h 
    jmp pf2 
    
    p31l2: 
    mov al, 31h
    jmp pf2
     
    pf2:
    mov c_bin2[si], ax
    add si, 2 
    jmp rutnot2 
    
    finnot2:
    ret 

endp 

c_bin_And proc 
    mov si, 0
    mov cx, 0
    rutand:     
    cmp cx, 8
    jz finand
    
    mov ax, c_bin1aux[si] 
    mov bx, c_bin2aux[si]
    AND ax, bx
    mov c_bin[si], ax 
    inc cx
    inc si
    jmp rutand
    finand:
    ret  
endp 
    
c_bin_or proc 
    mov si, 0
    mov cx, 0
    rutor:     
    cmp cx, 8
    jz finor
   
    mov ax, c_bin1aux[si] 
    mov bx, c_bin2aux[si]
    OR ax, bx
    mov c_bin[si], ax 
    inc cx
    inc si
    jmp rutor
    finor:
    ret  
endp 

c_bin_xor proc 
    mov si, 0
    rutxor:     
    cmp si, 8
    jz finxor
    
    mov ax, c_bin1aux[si] 
    mov bx, c_bin2aux[si] 
    ;parte alta 
     cmp ah, bh
     je ph30h
     jne ph31h
     ph30h:
        mov ah, 30h
        jmp fpl
     ph31h:
        mov ah, 31h
        jmp fpl
     
    ;parte baja
     fpl:
     cmp al, bl
     je pl30h
     jne pl31h 
     pl30h:
        mov al, 30h
        jmp fpa
     pl31h:
        mov al, 31h
        jmp fpa
     
    fpa: 
    mov c_bin[si], ax 
    add si, 2
    jmp rutxor
    finxor:
    ret  
endp 

impStr proc 
    mov ah, 09h
    int 21h 
    ret
endp 

pausa proc 
    mov ah, 00h
    int 16h
    ret
endp 

; Ingreso con contrasenia
login proc    
     
reset:       
    mov bx, offset password
    mov cl, passCount
    mov dx, offset msgPassword
    call impCadena 
    
again:         
    mov ah, 08h
    int 21h
    
    cmp al, [bx]
    jne error 
    inc bx
    loop again
    
    mov dx, offset msgPassCorrect
    call impCadena
    jmp salidaLogin
    
error:
    mov dx, offset msgPassIncorrect
    call impCadena
    sub passTry, 1
    mov cl, passTry  
    loop reset 

over:
    mov dx, offset msgSalida
    call impCadena
    call finProgram
    
salidaLogin:
    ret
    
endp

;Pausa
pause proc
    mov ah, 08h         ;Pause
    int 21h
    ret
endp

; Ejecuta el menu
ejecutarMenu proc 
menu:
    call mostrarMenu
    call leerCaracter
     
    cmp al, "1"
    je opcion1
        
    cmp al, "2"
    je opcion2
        
    cmp al, "3"
    je opcion3
        
    cmp al, "4"
    je opcion4
        
    cmp al, "5"
    je salir
    
    jmp menu  
    
salir:
    call finProgram
    
opcion1:
    call saltoLinea  
    mov dx, offset msgOpcion1
    call impCadena
    
    call saltoLinea
                 
    call leerNumero
    mov num1, ax
    
    call leerNumero
    mov num2, ax 
    
    mov ax, num1
    add ax, num2
    mov resultado, ax
    
    call saltoLinea
    
    mov dx, offset msgResultadoSuma
    call impCadena
    call impResultado
    
    mov ax, num1
    sub ax, num2
    mov resultado, ax
    
    mov dx, offset msgResultadoResta
    call impCadena
    call impResultado 
                     
    call saltoLinea
    call pause
    
    jmp menu

opcion2:
    call saltoLinea
    mov dx, offset msgOpcion2
    call impCadena
    
    call saltoLinea
    
    call leerNumero
    mov num1, ax
    
    call leerNumero
    mov num2, ax
    
    mov ax, num1
    mov bx, num2
    imul bx
    mov resultado, ax
    
    call saltoLinea
    
    mov dx, offset msgResultadoMul
    call impCadena
    mov ax, resultado
    call impResultado
    
    mov cx, 000h
    
    mov ax, num2
    call ajustarUx
    cmp ax, "0"
    je divzero
    jl num2ToP
    
    contNum1:
    mov ax, num1
    call ajustarUx
    cmp ax, "0"
    jl num1ToP
    
    contDiv: 
    mov ax, num1
    mov bx, num2
    div bl
    mov ah, 00h
    mov resultado, ax
    
    add cl, ch
    add cl, 30h
    cmp cl, "1"
    je resultToN

    divResult:    
    mov dx, offset msgResultadoDiv
    call impCadena
    call impResultado
    
    call saltoLinea
    call pause
    jmp menu
    
    divzero:
    mov dx, offset msgDivZero
    call impCadena
    
    call saltoLinea
    call pause
    jmp menu
    
    num1ToP:
    mov ax, num1
    mov bl, -1
    mul bl
    mov ah, 00h
    mov num1, ax
    mov ch, 1
    jmp contDiv
     
    
    num2ToP:
    mov ax, num2
    mov bl, -1
    mul bl 
    mov ah, 00h
    mov num2, ax
    mov cl, 1
    jmp contNum1
    
    resultToN:
    mov ax, resultado
    mov bl, -1
    imul bl
    mov resultado, ax
    jmp divResult

opcion3:
    call hexToBin 
    
    call saltoLinea
    call pause
    jmp menu

opcion4:
    call saltoLinea
    mov dx, offset msgOpcion4
    call impCadena
    
    call saltoLinea
    
    repeticiones:
    mov ax, 000h
    mov al, reps
    call ajustarU
    cmp al, "0"
    call ajustarD
    je finOp4
    
    mov cx, 0000h
    mov cl, n
    
    producto:
    mov al, base
    mov bl, pot
    imul bl
    mov pot, al
    loop producto
    
    mov al, pot
    mov bl, n
    imul bl
    mov resultado, ax
    mov dx, offset msgLn
    call impCadena
    call impResultado
    mov pot, 0001h
    mov al, n
    add al, 1
    mov n, al
    mov al, reps
    sub al, 1
    mov reps, al
    jmp repeticiones
    
    finOp4:
    mov reps, 000Fh
    mov n, 0001h
    mov pot, 0001h 
    
    call saltoLinea
    call pause
    jmp menu    

endp

main proc
    mov ax, @data
    mov ds, ax
    
    call cls    
    
    call login   
    
    call ejecutarMenu
   
    call finProgram 
endp

end main