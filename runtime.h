#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>


#define TAG_BITS ((uintptr_t)2)
#define TAG_MASK (((uintptr_t)1<<TAG_BITS)-1)

#define T_FIXNUM   0
#define T_STRING   1
#define T_LIST     2
#define T_CLOSURE  3

#define T_ANY 123456

//#define T_FLOAT
//#define T_NEXT_HERE
//#define T_NEXT_NONE

typedef struct regs_t {
  // registers array
  void *E; // current environment
  void *P; // parent environment
  void *A; // args scratchpad
  void *C; // code pointer
  void *R; // return value
  void *T; // temporary, used by CLOSURE, PAIR and other macros
  void *N; // number of arguments to the current function (size of E)
  void *v_void;
  void *v_yes;
  void *v_no;
  void *v_empty;
  void *fin;  // the closure, which would recieve evaluation result
  void *run;  // the closure, which would recieve the resulting program
  void *host; // called to resolve builtin functions
  char *(*get_tag_name)(int tag);
  void (*bad_tag)(struct regs_t *regs, void *object, int expected);
  void (*handle_args)(struct regs_t *regs, intptr_t expected, void *tag, void *meta);
  char* (*print_object_f)(struct regs_t *regs, void *object);
} regs_t;

typedef void (*pfun)(regs_t *regs);

#define E regs->E
#define P regs->P
#define A regs->A
#define C regs->C
#define R regs->R
#define T regs->T
#define N regs->N
#define v_void regs->v_void
#define v_yes regs->v_yes
#define v_no regs->v_no
#define v_empty regs->v_empty
#define fin regs->fin
#define run regs->run
#define host regs->host

#define print_object(object) regs->print_object_f(regs, object)

#define getArg(N) ((void**)(E))[N]
#define getTag(X) ((uintptr_t)(X)&TAG_MASK)
#define getVal(X) ((uintptr_t)(X)&~TAG_MASK)

// make strings and pairs statically allocable
#define ALLOC(dst, size) dst = malloc(size*sizeof(void*))
#define FIXNUM(dst,x) dst = (void*)(((uintptr_t)(x)<<TAG_BITS) | T_FIXNUM)
#define STRING(dst,x) /*printf("%p\n", x);*/ dst = (void*)((uintptr_t)x | T_STRING)
#define CONS(dst,a,b) \
  ALLOC(T, 2); \
  STORE(T, 0, a); \
  STORE(T, 1, b); \
  dst = (void*)((uintptr_t)T | T_LIST);
#define CLOSURE(dst,f) \
  ALLOC(T, 1); \
  STORE(T, 0, f); \
  dst = (void*)((uintptr_t)T | T_CLOSURE);
#define CAR(x) ((void**)getVal(x))[0]
#define CDR(x) ((void**)getVal(x))[1]
#define CALL(f) \
  P = (void*)((uintptr_t)f-T_CLOSURE); \
  (((pfun*)P)[0])(regs);
#define CALL_NO_ENV(f) (((pfun*)((uintptr_t)f-T_CLOSURE))[0])(regs);

#define STORE(dst,off,src) ((void**)(dst))[(int)(off)] = (void*)(src)
#define LOAD(dst,src,off) dst = ((void**)(src))[(int)(off)]
#define COPY(dst,p,src,q) ((void**)(dst))[(int)(p)] = ((void**)(src))[(int)(q)]
#define MOVE(dst,src) dst = (void*)(src)
#define IOR(dst,a,b) dst = (void*)((uintptr_t)(a)|(uintptr_t)(b))

#define CHECK_TAG(src,expected) if (getTag(src) != expected) regs->bad_tag(regs, src, expected)
#define CHECK_NARGS(expected,tag) \
  if ((intptr_t)N != (intptr_t)expected) { \
    regs->handle_args(regs, (intptr_t)expected, tag, v_empty); \
    return; \
  }
#define CHECK_NARGS_ABOVE(tag) \
  if ((intptr_t)N < 1) { \
    regs->handle_args(regs, -1, tag, v_empty); \
    return; \
  }

void entry(regs_t *regs);
