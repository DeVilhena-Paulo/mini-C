	.text
	.globl	main
fact:
	pushq %rbp
	movq %rdi, %r9
	movq %rsi, %r10
	cmpq $1, %r9
	jle L8
	movq %r9, %r8
	addq $-1, %r8
	imulq %r10, %r9
	movq %r8, %rdi
	movq %r9, %rsi
	call fact
	movq %rax, %r10
L1:
	movq %r10, %rax
	popq %rbp
	ret
	jmp L1
print_g:
	pushq %rbp
	movq %rsp, %rbp
	addq $-56, %rsp
	movq %r12, -32(%rbp)
	movq %rbx, -40(%rbp)
	movq %rdi, %rbx
	movq %rsi, %r10
	movq %rdx, %r10
	movq %rcx, -56(%rbp)
	movq %r8, %r10
	movq %r9, -48(%rbp)
	movq 72(%rbp), %r15
	movq %r15, -24(%rbp)
	movq 64(%rbp), %r10
	movq 56(%rbp), %r10
	movq 48(%rbp), %r15
	movq %r15, -16(%rbp)
	movq 40(%rbp), %r10
	movq 32(%rbp), %r15
	movq %r15, -8(%rbp)
	movq 24(%rbp), %r12
	movq 16(%rbp), %r10
	movq $10, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $42, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $42, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $42, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $42, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq %rbx, %r10
	addq $65, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq -56(%rbp), %r10
	addq $65, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq -48(%rbp), %r10
	addq $65, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq -24(%rbp), %r10
	addq $65, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq -16(%rbp), %r10
	addq $65, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq -8(%rbp), %r10
	addq $65, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq %r12, %r10
	addq $65, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $42, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $42, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $42, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $42, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $10, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $0, %r10
	movq %r10, %rax
	movq -32(%rbp), %r12
	movq -40(%rbp), %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
print_f:
	pushq %rbp
	movq %rsp, %rbp
	addq $-104, %rsp
	movq %r12, -96(%rbp)
	movq %rbx, -104(%rbp)
	movq %rdi, %rbx
	movq %rsi, -88(%rbp)
	movq %rdx, %r12
	movq %rcx, -80(%rbp)
	movq %r8, -72(%rbp)
	movq %r9, -64(%rbp)
	movq 16(%rbp), %r15
	movq %r15, -56(%rbp)
	movq $10, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $45, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $45, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $45, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $45, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq %rbx, %r10
	addq $65, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq -88(%rbp), %r10
	addq $65, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq %r12, %r10
	addq $65, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq -80(%rbp), %r10
	addq $65, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq -72(%rbp), %r10
	addq $65, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq -64(%rbp), %r10
	addq $65, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq -56(%rbp), %r10
	addq $65, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $45, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $45, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $45, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $45, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $10, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq %rbx, %rdi
	movq %rbx, %rcx
	movq -88(%rbp), %rbx
	movq -88(%rbp), %rax
	movq %r12, %r8
	movq %r12, %r9
	movq -80(%rbp), %r12
	movq -80(%rbp), %r10
	movq -72(%rbp), %r15
	movq %r15, -8(%rbp)
	movq -72(%rbp), %r15
	movq %r15, -16(%rbp)
	movq -64(%rbp), %r15
	movq %r15, -24(%rbp)
	movq -64(%rbp), %r15
	movq %r15, -32(%rbp)
	movq -56(%rbp), %r15
	movq %r15, -40(%rbp)
	movq -56(%rbp), %r15
	movq %r15, -48(%rbp)
	movq %rcx, %rsi
	movq %rbx, %rdx
	movq %rax, %rcx
	pushq %r12
	pushq %r10
	pushq -8(%rbp)
	pushq -16(%rbp)
	pushq -24(%rbp)
	pushq -32(%rbp)
	pushq -40(%rbp)
	pushq -48(%rbp)
	call print_g
	movq %rax, %r10
	addq $64, %rsp
	movq %r10, %rax
	movq -96(%rbp), %r12
	movq -104(%rbp), %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
main:
	pushq %rbp
	movq %rsp, %rbp
	addq $-16, %rsp
	movq %rbx, -8(%rbp)
	movq $3, %r8
	movq $1, %r10
	movq %r8, %rdi
	movq %r10, %rsi
	call fact
	movq %rax, %r10
	addq $48, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $10, %r10
	movq %r10, %rdi
	call putchar@PLT
	movq %rax, %r10
	movq $0, %rcx
	movq $1, %rbx
	movq $2, %rax
	movq $3, %r9
	movq $4, %r8
	movq $5, %r10
	movq $6, -16(%rbp)
	movq %rcx, %rdi
	movq %rbx, %rsi
	movq %rax, %rdx
	movq %r9, %rcx
	movq %r10, %r9
	pushq -16(%rbp)
	call print_f
	movq %rax, %r10
	addq $8, %rsp
	movq $0, %r10
	movq %r10, %rax
	movq -8(%rbp), %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
