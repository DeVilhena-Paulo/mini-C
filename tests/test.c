
int fact (int n, int acc) {
    if (n <= 1) return acc;
    return fact(n - 1, n * acc);
}


int print_g(int a1, int a2, int b1, int b2, int c1, int c2,
	    int d1, int d2, int e1, int e2, int f1, int f2, int g1, int g2) {
    putchar(10);
    putchar('*'); putchar('*'); putchar('*'); putchar('*');

    putchar('A' + a1); putchar('A' + b2); putchar('A' + c2);
    putchar('A' + d1); putchar('A' + e2); putchar('A' + f2);
    putchar('A' + g1);

    putchar('*'); putchar('*'); putchar('*'); putchar('*');
    putchar(10);
    return 0;
}

int print_f(int a, int b, int c, int d, int e, int f, int g) {
    putchar(10);
    putchar('-'); putchar('-'); putchar('-'); putchar('-');

    putchar('A' + a); putchar('A' + b); putchar('A' + c);
    putchar('A' + d); putchar('A' + e); putchar('A' + f);
    putchar('A' + g);

    putchar('-'); putchar('-'); putchar('-'); putchar('-');
    putchar(10);
    return print_g(a, a, b, b, c, c, d, d, e, e, f, f, g, g);
}


int main() {
    putchar('0' + fact(3, 1));
    putchar(10);
    print_f(0, 1, 2, 3, 4, 5, 6);
    return 0;
}
