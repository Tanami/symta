#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

// used for debugging
#define D fprintf(stderr, "%d:%s\n", __LINE__, __FILE__);

#define TAG_BITS ((uintptr_t)2)
#define TAG_MASK (((uintptr_t)1<<TAG_BITS)-1)
#define GET_TAG(x) ((uintptr_t)(x)&TAG_MASK)

#define T_CLOSURE  0
#define T_FIXNUM   1

//#define T_FLOAT
//#define T_NEXT_HERE
//#define T_NEXT_NONE

#define MAX_POOLS 1024*100
#define POOL_SIZE 64
#define POOL_BYTE_SIZE (POOL_SIZE*sizeof(void*))
#define POOL_MASK (uintptr_t)(POOL_BYTE_SIZE-1)
#define POOL_BASE (~POOL_MASK)
#define POOL_HANDLER(x) (((pfun*)((uintptr_t)(x)&POOL_BASE))[0])

typedef struct regs_t {
  // registers array
  void *E; // current environment
  void *P; // parent environment
  void *A; // args scratchpad
  void *C; // code pointer
  void *R; // return value
  void *T; // temporary, used by CLOSURE, CONS and other macros

  // constants
  void *v_void;
  void *v_empty;

  // utils
  void *fin;  // the closure, which would receive evaluation result
  void *run;  // the closure, which would receive the resulting program
  void *host; // called to resolve builtin functions (runtime API)

  // runtime's C API
  char *(*get_tag_name)(int tag);
  void (*handle_args)(struct regs_t *regs, intptr_t expected, void *tag, void *meta);
  char* (*print_object_f)(struct regs_t *regs, void *object);
  int (*new_pool)();
  void** (*alloc)(int count);
  void *(*alloc_symbol)(struct regs_t *regs, char *s);
  void (*fixnum)();

  // for multithreading, caching could be used to get on-demand pools for each thread, minimizing locking
  void **pools[MAX_POOLS];
} regs_t;


typedef void (*pfun)(regs_t *regs);

#define E regs->E
#define P regs->P
#define A regs->A
#define C regs->C
#define R regs->R
#define T regs->T
#define v_void regs->v_void
#define v_empty regs->v_empty
#define fin regs->fin
#define run regs->run
#define host regs->host

// number of arguments to the current function (size of E)
#define NARGS ((intptr_t)POOL_HANDLER(E))

#define print_object(object) regs->print_object_f(regs, object)

#define ALLOC(dst,code,pool,count) \
  if (((uintptr_t)regs->pools[pool]&POOL_MASK) + ((uintptr_t)(count||1)*sizeof(void*)-1) >= POOL_BYTE_SIZE) { \
    regs->pools[pool] = regs->alloc(count+1); \
    *regs->pools[pool]++ = (void*)(code); \
  } \
  MOVE(dst, regs->pools[pool]); \
  regs->pools[pool] += count;
#define FIXNUM(dst,x) dst = (void*)(((uintptr_t)(x)<<TAG_BITS) | T_FIXNUM)
#define SYMBOL(dst,x) dst = regs->alloc_symbol(regs,x)
#define CALL_BASE(f) POOL_HANDLER(f)(regs);
#define CALL(f) \
  MOVE(P, f); \
  CALL_BASE(f);
#define CALL_TAGGED(f) \
  if (GET_TAG(f) == T_CLOSURE) { \
    CALL(f); \
  } else if (GET_TAG(f) == T_FIXNUM) { \
    MOVE(P, f); \
    regs->fixnum(); \
  } else { \
    printf("bad tag = %d\n", (int)GET_TAG(f)); \
    abort(); \
  }

#define STORE(dst,off,src) ((void**)(dst))[(int)(off)] = (void*)(src)
#define LOAD(dst,src,off) dst = ((void**)(src))[(int)(off)]
#define COPY(dst,p,src,q) ((void**)(dst))[(int)(p)] = ((void**)(src))[(int)(q)]
#define MOVE(dst,src) dst = (void*)(src)

#define CHECK_NARGS(expected,tag) \
  if (NARGS != (intptr_t)expected) { \
    regs->handle_args(regs, (intptr_t)expected, tag, v_empty); \
    return; \
  }
#define CHECK_NARGS_ABOVE(tag) \
  if (NARGS < 1) { \
    regs->handle_args(regs, -1, tag, v_empty); \
    return; \
  }

void entry(regs_t *regs);
