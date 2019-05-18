	.text
	.global	insert_sort
	.type	insert_sort, @function
	
insert_sort:
    #xor %rax, %rax # A[j]
    #xor %rbx, %rbx # val = A[i]
    #xor %rcx, %rcx # i
    #xor %rdx, %rdx # j
    
    #srawdzanie czy adres first jest rowny adresowi last
    cmp %rsi, %rdi
    je .return
    
    mov $8, %rcx # i = 1
    
.for1:
    mov (%rcx,%rdi), %rbx # val = A[i]
    mov %rcx, %rdx # j = i
    sub $8, %rdx # j = i-1 
    mov (%rdx,%rdi), %rax # A[j]
    
.check:
    cmp $0, %rdx # j >= 0
    jl .value
    
    cmp %rbx, %rax # A[j] > val
    jle .value
    
.for2:
    add $8, %rdx # j++
    mov %rax, (%rdx,%rdi) # A[j+1]=A[j] : j+1 = i
    
    sub $16, %rdx # j -= 2
    mov (%rdx,%rdi), %rax # A[j]
    jmp .check
 
.value:
    add $8, %rdx # j++
    mov %rbx, (%rdx,%rdi) # A[j+1] = val (A[i])
    
    #sprawdzanie czy adres aktualnego elementu jest 
    #rowny adresowi ostatniego elementu tablicy
    lea (%rcx, %rdi), %r8
    cmp %rsi, %r8
    je .return
    
    add $8, %rcx # i++
    jmp .for1
    
.return:
    ret
    
	.size	insert_sort, . - insert_sort
