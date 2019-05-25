read = 0
write = 1
exit = 200
 
        .global _start
        .type _start, @function
        .section .text

_start:
    sub $exit, %rsp
    mov %rsp, %rsi
    mov $exit, %rdx
    mov $read, %rax
    mov $read, %rdi
    
    #Na poczatku chcemy wywolac funkcje sys_read zatem podajemy odpowiednie
    #argumenty, czyli %rax=0, %rdi=$read (do czytania), %rsi=wskaznik na stos
    #%rdx=rozmiar. Jednoczesnie w rejestrze %rax otrzymujemy wynik wywolania
    #procedury, czyli ilosc wczytanych znakow + '/0'.
    syscall
    
    mov %rax, %rbx
    #odejmujemy 1 bo ostatni znak to '/0'
    sub $1, %rbx 
 
#procedura do odwracania stringu   
.for:
    #kazdy znak ma 8 bajtow
    mov (%rsp, %rbx), %r10b
    mov (%rsp, %r8), %r11b
    mov %r10b, (%rsp, %r8)
    mov %r11b, (%rsp, %rbx)
    #przechodzimy do nastepnego znaku od konca
    sub $1, %rbx
    #przechodzimy do nastepnego znaku odd poczatku
    add $1, %r8
    
    #gdy wszystkie znaki zostana zamienione wychodzimy
    cmp %r8, %rbx
    jle .end
    
    jmp .for
   
#na koniec wywolujemy funkcje sys_write z odpowiednimi
#argumentami, jest ona odpowiedzialna za zapis.
.end:
    mov $write, %rdi
    mov %rax, %rdx
    mov $write, %rax
    mov %rsp, %rsi
    add $exit, %rsp  
      
    syscall
       
        .size   _start, . - _start