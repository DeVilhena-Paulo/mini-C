	.text
	.globl	main
main:
	pushq %rbp
	movq $100, %rdi
	call create
	movq %rax, %r10
	movq %r10, %rdi
	movq $20, %rsi
	call print_row
	movq %rax, %r10
	movq $0, %r10
	movq %r10, %rax
	popq %rbp
	ret
print_row:
	pushq %rbp
	movq %rsp, %rbp
	addq $-24, %rsp
	movq %rdi, -24(%rbp)
	movq %rsi, -16(%rbp)
	movq $0, %r10
	movq %r10, -8(%rbp)
L60:
	movq -8(%rbp), %r8
	movq -16(%rbp), %r10
	cmpq %r10, %r8
	jle L57
	movq $10, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $0, %r10
	movq %r10, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
L57:
	movq -24(%rbp), %rdi
	movq -8(%rbp), %rsi
	call get
	movq %rax, %r10
	cmpq $0, %r10
	jnz L53
	movq $46, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
L49:
	movq -8(%rbp), %r10
	addq $1, %r10
	movq %r10, -8(%rbp)
	jmp L60
L53:
	movq $42, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	jmp L49
create:
	pushq %rbp
	movq %rsp, %rbp
	addq $-24, %rsp
	movq %rdi, -24(%rbp)
	movq -24(%rbp), %r10
	cmpq $0, %r10
	jnz L38
	movq $0, %r10
L24:
	movq %r10, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
L38:
	movq $16, %rdi
	call sbrk@PLT
	movq %rax, %r10
	movq %r10, -16(%rbp)
	movq -16(%rbp), %r10
	movq $0, %r8
	movq %r8, 8(%r10)
	movq -16(%rbp), %r15
	movq %r15, -8(%rbp)
	movq -24(%rbp), %rdi
	addq $-1, %rdi
	call create
	movq %rax, %r10
	movq -8(%rbp), %r15
	movq %r10, 0(%r15)
	movq -16(%rbp), %r10
	jmp L24
set:
	pushq %rbp
	movq %rsi, %r10
	cmpq $0, %r10
	jnz L17
	movq %rdx, %r10
	movq %r10, 8(%rdi)
L11:
	movq %r10, %rax
	popq %rbp
	ret
L17:
	movq 0(%rdi), %r10
	addq $-1, %rsi
	movq %r10, %rdi
	call set
	movq %rax, %r10
	jmp L11
get:
	pushq %rbp
	cmpq $0, %rsi
	jnz L6
	movq 8(%rdi), %r10
L1:
	movq %r10, %rax
	popq %rbp
	ret
L6:
	movq 0(%rdi), %rdi
	addq $-1, %rsi
	call get
	movq %rax, %r10
	jmp L1
	.data
