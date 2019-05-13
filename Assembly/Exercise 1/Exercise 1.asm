	.text
	.global	bitrev
	.type	bitrev, @function
	
	# Metoda (dziel i zwyciezaj):
	# Zamieniamy parzyste i nieparzyste bity
	# Zamieniamy kolejne pary bitow
	# Zamieniamy kolejne czworki bitow
	# Zamieniamy bajty
	# Zamieniamy dwu-bajtowe pary
	# Zamieniamy cztero-bajtowe pary
	
bitrev:
    mov %rdi, %rax
    
    #Kazdy krok wyzej podanej metody jest analogiczny
    #i rozni sie jedynie wartoscia maski
    
    mov $0x5555555555555555, %rsi
    and %rsi, %rax
    shl %rax
    shr %rdi
    and %rsi, %rdi
    or %rdi, %rax
    mov %rax, %rdi
    
    mov $0x3333333333333333, %rsi
    and %rsi, %rax
    shl $2, %rax
    shr $2, %rdi
    and %rsi, %rdi
    or %rdi, %rax
    mov %rax, %rdi
    
    mov $0x0F0F0F0F0F0F0F0F, %rsi
    and %rsi, %rax
    shl $4, %rax
    shr $4, %rdi
    and %rsi, %rdi
    or %rdi, %rax
    mov %rax, %rdi
    
    mov $0x00FF00FF00FF00FF, %rsi
    and %rsi, %rax
    shl $8, %rax 
    shr $8, %rdi 
    and %rsi, %rdi 
    or %rdi, %rax
    mov %rax, %rdi
    
    mov $0x0000FFFF0000FFFF, %rsi
    and %rsi, %rax
    shl $16, %rax
    shr $16, %rdi
    and %rsi, %rdi
    or %rdi, %rax
    mov %rax, %rdi
   
    mov $0x0000000FFFFFFFFF, %rsi
    and %rsi, %rax
    shl $32, %rax
    shr $32, %rdi
    and %rsi, %rdi
    or %rdi, %rax
   
    ret

	.size	bitrev, .-bitrev
