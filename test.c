

struct List { int head; struct List *next; };

int get(struct List *l, int i) {
  if (i == 0) return l->head;
  return get(l->next, i-1);
}

int set(struct List *l, int i, int v) {
   if (i == 0) return l->head = v;
   return set(l->next, i-1, v);
}

struct List* create(int n) {
  struct List *r;
  if (n == 0) return 0;
  r = sbrk(sizeof(struct List));
  r->head = 0;
  r->next = create(n-1);
  return r;
}

int print_row(struct List *r, int i) {
    int j;
    j = 0;
    while (j <= i) {
      if (get(r, j) != 0)
	  putchar('*');
      else
	  putchar('.');
      j = j + 1;
    }
    putchar(10);
    return 0;
}


int main() {
    struct List *r;
    r = create(100);
    print_row(r, 20);
    return 0;
}
