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

#define REGS void *E, void *P, struct api_t *api, void *Base
#define REGS_ARGS(E,P) E, P, api, NewBase

typedef struct api_t {
  void *top; // heap top

  struct api_t *other;

  // constants
  void *void_;
  void *empty_;
  void *host_; // called to resolve builtin functions

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

#define POOL_HANDLER(x) (((pfun*)((void**)((uintptr_t)(x)&~TAG_MASK)-1))[0])

#define ALLOC(dst,code,count) \
  Top = (void**)Top - ((count)+1); \
  dst = Top; \
  *(void**)dst = (void*)(code); \
  dst = (void**)dst + 1; \
  dst = ADD_TAG(dst, T_CLOSURE);

#define LIST_SIZE(o) ((intptr_t)POOL_HANDLER(o))
#define NARGS LIST_SIZE(E)

#define IS_LIST(o) (GET_TAG(o) == T_LIST)
#define IS_ARGLIST(o) (LIST_SIZE(o) < FIXNUM(MAX_LIST_SIZE))

// FIXME: most of LIST_FLIP uses could be optimized out
#define LIST_FLIP(o) ((void*)((uintptr_t)(o)^(T_CLOSURE|T_LIST)))

#define print_object(object) api->print_object_f(api, object)
#define MIN(a,b) ((a) < (b) ? (a) : (b))

#define FLIP_HEAP() api = api->other;
#define LOCAL_ALLOC(dst,name,code,count) \
  void *name[(count)+1]; \
  name[0] = (void*)(code); \
  dst = ADD_TAG((void*)((void**)name+1), T_CLOSURE);
#define LOCAL_LIST(dst,name,count) LOCAL_ALLOC(dst,name,FIXNUM(count),count)
#define LOCAL_LABEL(name) name:;
#define LOCAL_BRANCH(cnd,name) if (cnd) goto name;
#define LOCAL_JMP(name) goto name;
#define BEGIN_CODE static void __dummy___ () {
#define END_CODE }
#define LIST(dst,size) ALLOC(dst,FIXNUM(size),size)
#define LOAD_FIXNUM(dst,x) dst = (void*)((uintptr_t)(x)<<TAG_BITS)
#define TEXT(dst,x) dst = api->alloc_text(api,(char*)(x))
#define ENTRY(name) } void *name(REGS) {void *NewBase;
#define DECL_LABEL(name) static void *name(REGS);
#define LABEL(name) } static void *name(REGS) {void *NewBase;
#define VAR(name) void *name;
#define RETURN(value) \
   if (GET_TAG(value) == T_FIXNUM || GET_TAG(value) == T_FIXTEXT) { \
     Top = Base; \
     return (void*)(value); \
   } \
   /*D; fprintf(stderr, "GC %p:%p -> %p\n", Top, Base, api->other->top);*/ \
   value = api->gc(api->other, Top, Base, (void*)(value)); \
   Top = Base; \
   return (void*)(value);
#define RETURN_NO_GC(value) return (void*)(value);
#define GOSUB(label) label(E,P,api,Base);
#define BRANCH(cond,label) if ((cond) != FIXNUM(0)) { label(REGS_ARGS); return; }
#define CALL(k,f,e) k = POOL_HANDLER(f)(REGS_ARGS(e,f));
#define CALL_TAGGED(k,f,e) \
  if (GET_TAG(f) == T_CLOSURE) { \
    k = POOL_HANDLER(f)(REGS_ARGS(e,f)); \
  } else if (GET_TAG(f) == T_FIXNUM) { \
    k = api->fixnum(REGS_ARGS(e,f)); \
  } else if (GET_TAG(f) == T_LIST) { \
    k = api->list(REGS_ARGS(e,f)); \
  } else if (GET_TAG(f) == T_FIXTEXT) { \
    k = api->fixtext(REGS_ARGS(e,f)); \
  } else { \
    api->bad_tag(REGS_ARGS(e,f)); /*should never happen*/ \
  }
#define REF1(base,off) *(uint8_t*)((uint8_t*)(base)+(off)-1)
#define REF4(base,off) *(uint32_t*)((uint8_t*)(base)+(off)*4-1)
#define REF(base,off) *(void**)((uint8_t*)(base)+(off)*sizeof(void*)-1)
#define LOAD(dst,src,src_off) dst = REF(src,src_off)
#define STORE(dst,dst_off,src) REF(dst,dst_off) = (void*)(src)
#define COPY(dst,dst_off,src,src_off) REF(dst,dst_off) = REF(src,src_off)
#define MOVE(dst,src) dst = (void*)(src)

#define CHECK_NARGS(expected,size,meta) \
  if (NARGS != FIXNUM(expected)) { \
    return api->handle_args(REGS_ARGS(E,P), FIXNUM(expected), FIXNUM(size), Void, meta); \
  }
#define CHECK_VARARGS(size,meta) \
  if (NARGS < FIXNUM(0)) { \
    return api->handle_args(REGS_ARGS(E,P), FIXNUM(-1), FIXNUM(size), Void, meta); \
  }

void *entry(REGS);
