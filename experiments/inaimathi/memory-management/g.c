#include <stdlib.h>
#include <stdio.h>

///// The primitive
enum LispType { LISP_INTEGER, LISP_CHARACTER, LISP_PRIMITIVE, LISP_CONS, LISP_FREE };
struct Memory; // need it for the *primitive pointer type
struct Cell {
  enum LispType type;
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
  int memory_size;
  struct Cell* all;
  struct Cell* free;

  struct Cell* nil;
};

struct Memory* new_memory(int cell_count) {
  // allocate stuff
  struct Memory* mem = malloc(sizeof(struct Memory));
  struct Cell* nil = malloc(sizeof(struct Cell));
  struct Cell* all_memory = malloc(sizeof(struct Cell) * cell_count);

  // Initialize the CDR pointers
  struct Cell* cur;
  int i;
  cur = &all_memory[0];
  cur->type = LISP_FREE;
  cur->cdr = nil; // The "last" one links to NIL
  for (i = 1; i < cell_count; ++i) {
    cur = &all_memory[i];
    cur->type = LISP_FREE;
    cur->cdr = &all_memory[i-1];
  };
  
  mem->memory_size = cell_count;
  mem->free = cur;
  mem->nil = nil;
  mem->all = all_memory;
  return mem;
}

void print_memory(struct Memory* mem) {
  int i;
  struct Cell* cur;
  for (i = 0; i < mem->memory_size; ++i) {
    cur = &mem->all[i];
    if (cur->type == LISP_FREE) printf(".");
    else printf("|");
  }
  printf("\n");
}

struct Cell* allocate(struct Memory* mem) {
  struct Cell* new = mem->free;
  mem->free = mem->free->cdr;
  return new;
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

///// Main thing
int main() {
  struct Memory* mem = new_memory(100);
  printf("Testing\n");
  print_memory(mem);
  return 0;
}
