#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

// used for debugging
#define D fprintf(stderr, "%d:%s\n", __LINE__, __FILE__);

#define TAG_BITS ((uintptr_t)3)
#define TAG_MASK (((uintptr_t)1<<TAG_BITS)-1)
#define GET_TAG(x) ((uintptr_t)(x)&TAG_MASK)

#define SIGN_BIT ((uintptr_t)1<<(sizeof(uintptr_t)*8-1))

#define T_FIXNUM  0
#define T_CLOSURE 1
#define T_LIST    2
#define T_FLOAT   3
#define T_PTR     4
#define T_TAIL    5 /* list without head */
#define T_FIXTEXT 6 /* immediate text */

// sign preserving shifts
#define ASHL(x,count) ((x)*(1<<(count)))
#define ASHR(x,count) ((x)/(1<<(count)))
#define FIXNUM(x) ASHL((intptr_t)(x),TAG_BITS)
#define UNFIXNUM(x) ASHR((intptr_t)(x),TAG_BITS)

#define HEAP_SIZE (1024*1024*32)
#define MAX_LIST_SIZE (HEAP_SIZE/2)

typedef struct regs_t {
  // registers array
  void *E; // current environment
  void *P; // parent environment
  void *A; // args scratchpad
  void *C; // code pointer
  void *R; // return value

  void **H; // heap pointer
  void **HeapEnd;

  // constants
  void *Void;
  void *Empty;

  // utils
  void *fin;  // the closure, which would receive evaluation result
  void *run;  // the closure, which would receive the resulting program
  void *host; // called to resolve builtin functions (runtime API)

  // runtime's C API
  void (*bad_tag)(struct regs_t *regs);
  void (*handle_args)(struct regs_t *regs, intptr_t expected, void *tag, void *meta);
  char* (*print_object_f)(struct regs_t *regs, void *object);
  void (*gc)(struct regs_t *regs);
  void *(*alloc_text)(struct regs_t *regs, char *s);
  void (*fixnum)(struct regs_t *regs);
  void (*list)(struct regs_t *regs);
  void (*fixtext)(struct regs_t *regs);
} regs_t;


typedef void (*pfun)(regs_t *regs);

#define E regs->E
#define P regs->P
#define A regs->A
#define C regs->C
#define R regs->R
#define H regs->H
#define HeapEnd regs->HeapEnd
#define Void regs->Void
#define Empty regs->Empty
#define fin regs->fin
#define run regs->run
#define host regs->host

#define POOL_HANDLER(x) (((pfun*)((void**)((uintptr_t)(x)&~TAG_MASK)-1))[0])

#define ALLOC(dst,code,count) \
  for (;;) { \
    dst = (void*)H; \
    H += (count)+1; \
    if (H < HeapEnd) { \
      *(void**)dst = (void*)(code); \
      dst = (void*)((void**)dst+1); \
      dst = ADD_TAG(dst,T_CLOSURE); \
      break; \
    } \
    regs->gc(regs); \
  }


#define LIST_SIZE(o) ((intptr_t)POOL_HANDLER(o))
#define NARGS LIST_SIZE(E)

#define IS_LIST(o) (GET_TAG(o) == T_LIST)
#define IS_ARGLIST(o) (LIST_SIZE(o) < FIXNUM(MAX_LIST_SIZE))

// FIXME: most of LIST_FLIP uses could be optimized out
#define LIST_FLIP(o) ((void*)((uintptr_t)(o)^(T_CLOSURE|T_LIST)))

#define print_object(object) regs->print_object_f(regs, object)
#define MIN(a,b) ((a) < (b) ? (a) : (b))


#define LIST(dst,size) ALLOC(dst,FIXNUM(size),size)
#define LOAD_FIXNUM(dst,x) dst = (void*)((uintptr_t)(x)<<TAG_BITS)
#define TEXT(dst,x) dst = regs->alloc_text(regs,(char*)(x))
#define ADD_TAG(src,tag) ((void*)((uintptr_t)(src) | (tag)))
#define DEL_TAG(src) ((void*)((uintptr_t)(src) & ~(TAG_MASK>>1)))
#define BRANCH(cond,label) if ((cond) != FIXNUM(0)) { label(regs); return; }
#define CALL(f) MOVE(P, f); POOL_HANDLER(f)(regs);
#define CALL_TAGGED(f) \
  MOVE(P, f); \
  if (GET_TAG(P) == T_CLOSURE) { \
    POOL_HANDLER(P)(regs); \
  } else if (GET_TAG(P) == T_FIXNUM) { \
    regs->fixnum(regs); \
  } else if (GET_TAG(P) == T_LIST) { \
      regs->list(regs); \
  } else if (GET_TAG(P) == T_FIXTEXT) { \
      regs->fixtext(regs); \
  } else { \
    regs->bad_tag(regs); /*should never happen*/ \
  }
#define REF1(base,off) *(uint8_t*)((uint8_t*)(base)+(off)-1)
#define REF4(base,off) *(uint32_t*)((uint8_t*)(base)+(off)*4-1)
#define REF(base,off) *(void**)((uint8_t*)(base)+(off)*sizeof(void*)-1)
#define LOAD(dst,src,src_off) dst = REF(src,src_off)
#define STORE(dst,dst_off,src) REF(dst,dst_off) = (void*)(src)
#define COPY(dst,dst_off,src,src_off) REF(dst,dst_off) = REF(src,src_off)
#define MOVE(dst,src) dst = (void*)(src)

#define CHECK_NARGS(expected,tag) \
  if (NARGS != FIXNUM(expected)) { \
    regs->handle_args(regs, FIXNUM(expected), tag, Empty); \
    return; \
  }
#define CHECK_VARARGS(tag) \
  if (NARGS < FIXNUM(1)) { \
    regs->handle_args(regs, -1, tag, Empty); \
    return; \
  }

void entry(regs_t *regs);
