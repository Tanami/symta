#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

// used for debugging
#define D fprintf(stderr, "%d:%s\n", __LINE__, __FILE__);

#define TAG_BITS ((uintptr_t)3)
#define TAG_MASK (((uintptr_t)1<<TAG_BITS)-1)
#define GET_TAG(x) ((uintptr_t)(x)&TAG_MASK)
#define ADD_TAG(src,tag) ((void*)((uintptr_t)(src) | (tag)))
#define DEL_TAG(src) ((void*)((uintptr_t)(src) & ~TAG_MASK))
#define IMMEDIATE(x) (GET_TAG(x) == T_FIXNUM || GET_TAG(x) == T_FIXTEXT)


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

#define FIXNUM_NEG(o) (void*)(-(intptr_t)(o))
#define FIXNUM_ADD(a,b) (void*)((intptr_t)(a) + (intptr_t)(b))
#define FIXNUM_SUB(a,b) (void*)((intptr_t)(a) - (intptr_t)(b))
#define FIXNUM_MUL(a,b) (void*)(UNFIXNUM(a) * (intptr_t)(b))
#define FIXNUM_DIV(a,b) (void*)(FIXNUM((intptr_t)(a) / (intptr_t)(b)))
#define FIXNUM_REM(a,b) (void*)((intptr_t)(a) % (intptr_t)(b))
#define FIXNUM_IS(a,b) (void*)FIXNUM((intptr_t)(a) == (intptr_t)(b))
#define FIXNUM_ISNT(a,b) (void*)FIXNUM((intptr_t)(a) != (intptr_t)(b))
#define FIXNUM_LT(a,b) (void*)FIXNUM((intptr_t)(a) < (intptr_t)(b))
#define FIXNUM_GT(a,b) (void*)FIXNUM((intptr_t)(a) > (intptr_t)(b))

#define HEAP_DEPTH 1024
#define HEAP_SIZE (32*1024*1024)
#define MAX_LIST_SIZE (HEAP_SIZE/2)
#define BASE_HEAD_SIZE 2
#define OBJ_HEAD_SIZE 2

#define REGS void *P, struct api_t *api
#define REGS_ARGS(P) P, api

typedef struct api_t {
  void *base;
  void *top; // heap top

  struct api_t *other;

  intptr_t level;

  // constants
  void *void_;
  void *empty_;
  void *host_;

  //void *Goto; // nonlocal goto token (TODO)

  // runtime's C API
  void (*bad_tag)(REGS);
  void* (*handle_args)(REGS, intptr_t expected, intptr_t size, void *tag, void *meta);
  char* (*print_object_f)(struct api_t *api, void *object);
  void *(*gc)(struct api_t *api, void *base, void *end, void *root);
  void *(*alloc_text)(struct api_t *api, char *s);
  void *(*fixnum)(REGS);
  void *(*list)(REGS);
  void *(*fixtext)(REGS);

  void *heap[HEAP_SIZE];
} api_t;

typedef void *(*pfun)(REGS);

#define Void api->void_
#define Empty api->empty_
#define Host api->host_
#define Top api->top
#define Base api->base
#define Level api->level

#define POOL_HANDLER(x) (((pfun*)((void**)((uintptr_t)(x)&~TAG_MASK)-1))[0])
#define OBJECT_LEVEL(x) ((uintptr_t)(((void**)((uintptr_t)(x)&~TAG_MASK)-2)[0]))
#define ON_CURRENT_LEVEL(x) (Top <= (void*)(x) && (void*)(x) < Base)

#define ALLOC_BASIC(dst,code,count) \
  Top = (void**)Top - ((count)+OBJ_HEAD_SIZE); \
  *((void**)Top+0) = (void*)Level; \
  *((void**)Top+1) = (void*)(code); \
  dst = (void**)Top+OBJ_HEAD_SIZE;

#define ALLOC(dst,code,count) \
  ALLOC_BASIC(dst,code,count); \
  dst = ADD_TAG(dst, T_CLOSURE);

#define IS_LIST(o) (GET_TAG(o) == T_LIST)

// FIXME: most of LIST_FLIP uses could be optimized out
#define LIST_FLIP(o) ((void*)((uintptr_t)(o)^(T_CLOSURE|T_LIST)))

#define print_object(object) api->print_object_f(api, object)
#define MIN(a,b) ((a) < (b) ? (a) : (b))

#define LOCAL_LABEL(name) name:;
#define LOCAL_BRANCH(cnd,name) if (cnd) goto name;
#define LOCAL_JMP(name) goto name;
#define BEGIN_CODE static void __dummy___ () {
#define END_CODE }
#define LIST(dst,size) ALLOC(dst,FIXNUM(size),size)
#define LOAD_FIXNUM(dst,x) dst = (void*)((uintptr_t)(x)<<TAG_BITS)
#define TEXT(dst,x) dst = api->alloc_text(api,(char*)(x))
#define DECL_LABEL(name) static void *name(REGS);
#define ARGLIST(dst,size) ALLOC_BASIC(dst,FIXNUM(size),size)
#define NARGS(e) ((intptr_t)*((void**)(e)-1))
#define getArg(i) (*((void**)E+(i)))
#define PROLOGUE void *E = (void**)Top+OBJ_HEAD_SIZE;
#define ENTRY(name) } void *name(REGS) {PROLOGUE;
#define LABEL(name) } static void *name(REGS) {PROLOGUE;
#define VAR(name) void *name;
#define RETURN(value) \
   if (IMMEDIATE(value) && !LIFTS_LIST(Base)) { \
     return (void*)(value); \
   } \
   /*D; fprintf(stderr, "GC %p:%p -> %p\n", Top, Base, api->other->top);*/ \
   value = api->gc(api->other, Top, Base, (void*)(value)); \
   return (void*)(value);
#define RETURN_NO_GC(value) return (void*)(value);
#define GOSUB(label) label(REGS_ARGS(P));
#define BRANCH(cond,label) if ((cond) != FIXNUM(0)) { label(REGS_ARGS(P)); return; }
#define LIFTS_CONS(dst,head,tail) \
  Top=(void**)Top-2; \
  *((void**)Top+0) = (head); \
  *((void**)Top+1) = (tail); \
  dst = Top;
#define LIFTS_HEAD(xs) (*((void**)(xs)+0))
#define LIFTS_TAIL(xs) (*((void**)(xs)+1))
#define LIFTS_LIST(base) (*((void**)(base)+1))
#define LIFT(base,pos,value) \
  *((void**)(base)+(pos)) = (value); \
  if (!IMMEDIATE(value) && OBJECT_LEVEL(base) < OBJECT_LEVEL(value)) { \
    LIFTS_CONS(LIFTS_LIST(Base), (void**)(base)+(pos), LIFTS_LIST(Base)); \
  }
#define HEAP_FLIP() api = api->other;
#define PUSH_BASE() \
  HEAP_FLIP(); \
  Level += 2; \
  /*printf("Entering %ld\n", Level);*/ \
  Top = (void**)Top-BASE_HEAD_SIZE; \
  *((void**)Top+0) = Base; \
  LIFTS_LIST(Top) = 0; \
  Base = Top;
#define POP_BASE() \
  /*printf("Leaving %ld\n", Level);*/ \
  Top = (void**)Base+BASE_HEAD_SIZE; \
  Base = *(void**)Base; \
  Level -= 2; \
  HEAP_FLIP();
#define CALL(k,f) k = POOL_HANDLER(f)(REGS_ARGS(f)); POP_BASE()
#define CALL_TAGGED(k,f) \
  if (GET_TAG(f) == T_CLOSURE) { \
    k = POOL_HANDLER(f)(REGS_ARGS(f)); \
  } else if (GET_TAG(f) == T_FIXNUM) { \
    k = api->fixnum(REGS_ARGS(f)); \
  } else if (GET_TAG(f) == T_LIST) { \
    k = api->list(REGS_ARGS(f)); \
  } else if (GET_TAG(f) == T_FIXTEXT) { \
    k = api->fixtext(REGS_ARGS(f)); \
  } else { \
    api->bad_tag(REGS_ARGS(f)); /*should never happen*/ \
  } \
  POP_BASE();
#define REF1(base,off) *(uint8_t*)((uint8_t*)(base)+(off)-1)
#define REF4(base,off) *(uint32_t*)((uint8_t*)(base)+(off)*4-1)
#define REF(base,off) *(void**)((uint8_t*)(base)+(off)*sizeof(void*)-1)
#define ARG_LOAD(dst,src,src_off) dst = *((void**)(src)+(src_off))
#define ARG_STORE(dst,dst_off,src) *((void**)(dst)+(dst_off)) = (void*)(src)
#define LOAD(dst,src,src_off) dst = REF(src,src_off)
#define STORE(dst,dst_off,src) REF(dst,dst_off) = (void*)(src)
#define COPY(dst,dst_off,src,src_off) REF(dst,dst_off) = REF(src,src_off)
#define MOVE(dst,src) dst = (void*)(src)

#define CHECK_NARGS(expected,size,meta) \
  if (NARGS(E) != FIXNUM(expected)) { \
    return api->handle_args(REGS_ARGS(P), FIXNUM(expected), FIXNUM(size), Void, meta); \
  }
#define CHECK_VARARGS(size,meta) \
  if (NARGS(E) < FIXNUM(0)) { \
    return api->handle_args(REGS_ARGS(P), FIXNUM(-1), FIXNUM(size), Void, meta); \
  }

void *entry(REGS);
