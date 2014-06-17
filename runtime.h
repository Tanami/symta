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
#define T_VIEW    4
#define T_PTR     5
#define T_FIXTEXT 6 /* immediate text */
#define T_DATA    7
#define T_TEXT    8
#define T_CONS    9
#define T_VOID    10

#define T_INTEGER T_FIXNUM

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

  void *method;

  //void *Goto; // nonlocal goto token (TODO)

  // runtime's C API
  void* (*handle_args)(REGS, void *E, intptr_t expected, intptr_t size, void *tag, void *meta);
  char* (*print_object_f)(struct api_t *api, void *object);
  void *(*gc)(struct api_t *api, void *base, void *end, void *root);
  void *(*alloc_text)(struct api_t *api, char *s);
  void (*fatal)(struct api_t *api, char *msg);
  void **(*resolve_method)(struct api_t *api, char *name);
  int (*resolve_type)(struct api_t *api, char *name);

  void *heap[HEAP_SIZE];
} api_t;

typedef void *(*pfun)(REGS);

#define Void api->void_
#define Empty api->empty_
#define Host api->host_
#define Top api->top
#define Base api->base
#define Level api->level

#define OBJECT_CODE(x) (((pfun*)((void**)((uintptr_t)(x)&~TAG_MASK)-1))[0])
#define DATA_TAG(o) ((uintptr_t)OBJECT_CODE(o)&0xFFFFFFFF)
#define OBJECT_LEVEL(x) ((uintptr_t)(((void**)((uintptr_t)(x)&~TAG_MASK)-2)[0]))

#define ALLOC_BASIC(dst,code,count) \
  Top = (void**)Top - ((count)+OBJ_HEAD_SIZE); \
  *((void**)Top+0) = (void*)Level; \
  *((void**)Top+1) = (void*)(code); \
  dst = (void**)Top+OBJ_HEAD_SIZE;

#define ALLOC_CLOSURE(dst,code,count) \
  ALLOC_BASIC(dst,code,count); \
  dst = ADD_TAG(dst, T_CLOSURE);

#define ALLOC_DATA(dst,code,count) \
  ALLOC_BASIC(dst,(void*)(code),count); \
  dst = ADD_TAG(dst, T_DATA);

#define IS_LIST(o) (GET_TAG(o) == T_LIST)

#define print_object(object) api->print_object_f(api, object)
#define MIN(a,b) ((a) < (b) ? (a) : (b))

#define LOCAL_LABEL(name) name:;
#define BRANCH(cnd,name) if (cnd) goto name;
#define ZBRANCH(cnd,name) if (!(cnd)) goto name;
#define JMP(name) goto name;
#define LOCAL_CLOSURE(dst,size) ALLOC_CLOSURE(dst,FIXNUM(size),size)
#define BEGIN_CODE static void __dummy___ () {
#define END_CODE }
#define LOAD_FIXNUM(dst,x) dst = (void*)((uintptr_t)(x)<<TAG_BITS)
#define TEXT(dst,x) dst = api->alloc_text(api,(char*)(x))
#define RESOLVE_METHOD(dst,name) dst = api->resolve_method(api, name);
#define DECL_LABEL(name) static void *name(REGS);
#define ARGLIST(dst,size) ALLOC_BASIC(dst,FIXNUM(size),size)
#define LIST_ALLOC(dst,size) \
  ARGLIST(dst,size); \
  dst = ADD_TAG(dst, T_LIST);
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
#define CALL_NO_POP(k,f) k = OBJECT_CODE(f)(REGS_ARGS(f));
#define CALL(k,f) CALL_NO_POP(k,f); POP_BASE();

#define CALL_TAGGED_NO_POP(k,o,m) \
  { \
    uintptr_t tag = (uintptr_t)GET_TAG(o); \
    if (tag == T_CLOSURE) { \
      k = OBJECT_CODE(o)(REGS_ARGS(o)); \
    } else { \
      ARG_LOAD(api->method, Top, 2); \
      ARG_STORE(Top, 2, o); \
      if (tag == T_DATA) { \
        tag = DATA_TAG(o); \
      } \
      k = OBJECT_CODE(((void**)(m))[tag])(REGS_ARGS(o)); \
    } \
  }
#define CALL_TAGGED(k,o,m) CALL_TAGGED_NO_POP(k,o,m); POP_BASE();
#define CALL_TAGGED_DYNAMIC_NO_POP(k,o) \
  if (GET_TAG(o) == T_CLOSURE) { \
    k = OBJECT_CODE(o)(REGS_ARGS(o)); \
  } else { \
    api->fatal("FIXME: resolve method at runtime\n"); \
  }
#define CALL_TAGGED_DYNAMIC(k,o) CALL_TAGGED_DYNAMIC_NO_POP(k,o,f); POP_BASE();

//FIXME: refactor these
#define VIEW_REF1(base,off) *(uint8_t*)((uint8_t*)(base)+(off)-T_VIEW)
#define VIEW_REF4(base,off) *(uint32_t*)((uint8_t*)(base)+(off)*4-T_VIEW)
#define VIEW_GET(base,off) *(void**)((uint8_t*)(base)+(off)*sizeof(void*)-T_VIEW)
#define CLOSURE_REF1(base,off) *(uint8_t*)((uint8_t*)(base)+(off)-T_CLOSURE)
#define CLOSURE_REF4(base,off) *(uint32_t*)((uint8_t*)(base)+(off)*4-T_CLOSURE)
#define CLOSURE_REF(base,off) *(void**)((uint8_t*)(base)+(off)*sizeof(void*)-T_CLOSURE)
#define DATA_REF1(base,off) *(uint8_t*)((uint8_t*)(base)+(off)-T_DATA)
#define DATA_REF4(base,off) *(uint32_t*)((uint8_t*)(base)+(off)*4-T_DATA)
#define DATA_REF(base,off) *(void**)((uint8_t*)(base)+(off)*sizeof(void*)-T_DATA)
#define LIST_REF(base,off) *(void**)((uint8_t*)(base)+(off)*sizeof(void*)-T_LIST)
#define ARG_LOAD(dst,src,src_off) dst = *((void**)(src)+(src_off))
#define ARG_STORE(dst,dst_off,src) *((void**)(dst)+(dst_off)) = (void*)(src)
#define LOAD(dst,src,src_off) dst = CLOSURE_REF(src,src_off)
#define STORE(dst,dst_off,src) CLOSURE_REF(dst,dst_off) = (void*)(src)
#define COPY(dst,dst_off,src,src_off) CLOSURE_REF(dst,dst_off) = CLOSURE_REF(src,src_off)
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
