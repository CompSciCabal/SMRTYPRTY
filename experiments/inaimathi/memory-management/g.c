#include <stdlib.h>
#include <stdio.h>

///// The primitive
enum LispType { LISP_INTEGER, LISP_CHARACTER, LISP_PRIMITIVE, LISP_CONS, LISP_FREE };
struct Memory; // need it for the *primitive pointer type
struct Cell {
  enum LispType type;
  int mark;
  union {
    int integer;
    char character;
    struct Cell* cell;
    struct Cell* (*primitive) (struct Memory* mem, struct Cell* args);
  } car;
  struct Cell* cdr;
};

///// Memory
struct Memory {
  int size;
  struct Cell* all;
  struct Cell* free;
  struct Cell* nil;

  struct Cell* root;
};

struct Memory* new_memory(int cell_count) {
  // allocate stuff
  struct Memory* mem = malloc(sizeof(struct Memory));
  struct Cell* nil = malloc(sizeof(struct Cell));
  struct Cell* all_memory = malloc(sizeof(struct Cell) * cell_count);

  // Initialize cons pointers
  struct Cell* cur;
  int i;
  cur = &all_memory[0];
  cur->type = LISP_FREE;
  cur->car.cell = nil;
  cur->cdr = nil; // The "last" one links to NIL
  for (i = 1; i < cell_count; ++i) {
    cur = &all_memory[i];
    cur->type = LISP_FREE;
    cur->car.cell = nil;
    cur->cdr = &all_memory[i-1];
  };

  mem->size = cell_count;
  mem->free = cur;
  mem->nil = nil;
  mem->root = nil;
  mem->all = all_memory;
  return mem;
}

void deallocate(struct Memory* mem, struct Cell* cell) {
  cell->type = LISP_FREE;
  cell->car.cell = mem->nil;
  cell->cdr = mem->free;
  mem->free = cell;
}

void mark(struct Memory* mem, struct Cell* cell) {
  if ((cell != mem->nil) && (cell->mark == 0)) {
    cell->mark = 1;
    if (cell->type == LISP_CONS) {
      mark(mem, cell->car.cell);
      mark(mem, cell->cdr);
    }
  }
}

void sweep(struct Memory* mem) {
  int i;
  struct Cell* cur;
  for (i = 0; i < mem->size; ++i) {
    cur = &mem->all[i];
    if (cur->mark == 1) {
      cur->mark = 0;
    } else if (cur->type != LISP_FREE) {
      deallocate(mem, cur);
    }
  }
}

void run_collection(struct Memory* mem) {
  mark(mem, mem->root);
  sweep(mem);
}

struct Cell* _allocate(struct Memory* mem) {
  struct Cell* new;
  new = mem->free;
  mem->free = mem->free->cdr;
  new->cdr = mem->nil;
  return new;
}

struct Cell* allocate(struct Memory* mem) {
  if (mem->nil == mem->free) {
    run_collection(mem);
    if (mem->nil == mem->free) {
      printf("OUT OF MEMORY\n");
      exit(1);
    } else {
      return _allocate(mem);
    }
  } else {
    return _allocate(mem);
  }
}

///// Allocating primitives
struct Cell* new_integer(struct Memory* mem, int i) {
  struct Cell* new = allocate(mem);
  new->type = LISP_INTEGER;
  new->car.integer = i;
  return new;
}

struct Cell* new_character(struct Memory* mem, char c) {
  struct Cell* new = allocate(mem);
  new->type = LISP_CHARACTER;
  new->car.character = c;
  return new;
} 

struct Cell* new_primitive(struct Memory* mem, struct Cell* (*prim) (struct Memory*, struct Cell*)) {
  struct Cell* new = allocate(mem);
  new->type = LISP_PRIMITIVE;
  new->car.primitive = prim;
  return new;
}

struct Cell* new_cons(struct Memory* mem, struct Cell* a, struct Cell* b) {
  struct Cell* new = allocate(mem);
  new->type = LISP_CONS;
  new->car.cell = a;
  new->cdr = b;
}

///// Testing/demo functions
void print_cell_type(struct Cell* cell) {
  switch (cell->type) {
  case LISP_FREE: printf("."); break;
  case LISP_INTEGER: printf("i"); break;
  case LISP_CONS: printf("|"); break;
  case LISP_CHARACTER: printf("c"); break;
  default: printf("o");
  }
}

void print_free_list(struct Memory* mem) {
  struct Cell* cur = mem->free;
  while (cur != mem->nil) {
    print_cell_type(cur);
    cur = cur->cdr;
  }
}

void print_memory(struct Memory* mem) {
  int i;
  struct Cell* cur;
  printf("      ");
  for (i = 0; i < mem->size; ++i) {
    cur = &mem->all[i];
    if (cur->mark) printf("+");
    else printf(" ");
  }
  printf("\nMem:  ");
  for (i = 0; i < mem->size; ++i) {
    cur = &mem->all[i];
    print_cell_type(cur);
  }
  printf("\nFree: ");
  print_free_list(mem);
  printf("\n\n");
}

void fill_memory(struct Memory* mem) {
  while (mem->nil != mem->free) {
    new_integer(mem, 34);
  }
}


///// Main thing
int main() {
  struct Cell* c;
  struct Cell* lst;
  struct Memory* mem = new_memory(100);

  print_memory(mem);

  c = new_cons(mem, new_character(mem, 'c'), new_integer(mem, 34));
  print_memory(mem);
  
  lst = new_cons(mem, new_integer(mem, 1), new_cons(mem, new_integer(mem, 2), new_cons(mem, new_integer(mem, 3), mem->nil)));
  mem->root = lst;
  print_memory(mem);

  fill_memory(mem);
  print_memory(mem);

  deallocate(mem, c);
  print_memory(mem);

  mark(mem, mem->root);
  print_memory(mem);
  sweep(mem);
  print_memory(mem);

  lst = new_cons(mem, new_character(mem, 'a'), new_cons(mem, new_character(mem, 'b'), new_cons(mem, new_character(mem, 'c'), mem->nil)));
  mem->root = lst;
  print_memory(mem);
  mark(mem, mem->root);
  print_memory(mem);
  sweep(mem);
  print_memory(mem);
  
  fill_memory(mem);

  return 0;
}
