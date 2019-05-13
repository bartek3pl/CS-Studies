	.text
	.global	clz
	.type	clz, @function
	
	# Metoda to prosty algorytm zliczajacy zera
	# wiadace za pomoca przeszukiwania binarnego
	
clz:
    xor %eax,%eax
    #sprawdzamy przypadek gdy wejscie = 0 (zwracamy 64)
    mov $64, %rax   
    cmp $0, %rdi    
    je return
    
    #ustawiamy poczatkowa sume zer wiodacych na 0
    mov $0, %rax    
N0:    
    #maska z zapalona prawa polowa bitow
    mov $0x00000000FFFFFFFF, %rdx
    #porownujemy wejscie z maska
    cmp %rdx, %rdi 		  
    #jesli wejscie mniejsze rowne od maski to wykonujemy ponizsze 2 instrukcje
    #wpp skaczemy do kolejnego bloku
    ja N1 
    add $32, %rax #dodajemy do wyniku liczbe zer wiadacych (32)
    shl $32, %rdi #przesuwamy bity wejscia o 32 miejsca w lewo
    
    #kazdy kolejny blok jest analogiczny
N1:    
    mov $0x0000FFFFFFFFFFFF, %rdx
    cmp %rdx, %rdi
    ja N2
    add $16, %rax
    shl $16, %rdi
N2:
    mov $0x00FFFFFFFFFFFFFF, %rdx
    cmp %rdx, %rdi
    ja N3
    add $8, %rax
    shl $8, %rdi
N3:
    mov $0x0FFFFFFFFFFFFFFF, %rdx
    cmp %rdx, %rdi
    ja N4
    add $4, %rax
    shl $4, %rdi
N4:
    mov $0x3FFFFFFFFFFFFFFF, %rdx
    cmp %rdx, %rdi
    ja N5
    add $2, %rax
    shl $2, %rdi
N5:
    mov $0x7FFFFFFFFFFFFFFF, %rdx
    cmp %rdx, %rdi
    ja return
    add $1, %rax
    
return:
    ret

	.size	clz, .-clz
