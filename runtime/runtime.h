#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define SYMTA_DEBUG 1

// used for debugging
#define D fprintf(stderr, "%d:%s\n", __LINE__, __FILE__);

#define TAG_BITS ((uintptr_t)3)
#define TAG_MASK (((uintptr_t)1<<TAG_BITS)-1)
#define GET_TAG(x) ((uintptr_t)(x)&TAG_MASK)
#define ADD_TAG(src,tag) ((void*)((uintptr_t)(src) | (tag)))
#define DEL_TAG(src) ((void*)((uintptr_t)(src) & ~TAG_MASK))
#define IMMEDIATE(x) (GET_TAG(x) <= T_FLOAT)

#define T_FIXNUM  0
#define T_FIXTEXT 1 /* immediate text */
#define T_FLOAT   2
#define T_CLOSURE 3
#define T_LIST    4
#define T_VIEW    5
#define T_CONS    6
#define T_DATA    7
#define T_OBJECT  8
#define T_TEXT    9
#define T_VOID    10
#define T_GENERIC_LIST 11
#define T_GENERIC_TEXT 12
#define T_HARD_LIST 13
#define T_NAME 14
#define T_NAME_TEXT 15

#define T_INTEGER T_FIXNUM

// sign preserving shifts
#define ASHL(x,count) ((x)*(1<<(count)))
#define ASHR(x,count) ((x)/(1<<(count)))
#define FIXNUM(x) ASHL((intptr_t)(x),TAG_BITS)
#define UNFIXNUM(x) ASHR((intptr_t)(x),TAG_BITS)

#define FIXNUM_NEG(dst,o) dst = (void*)(-(intptr_t)(o))
#define FIXNUM_ADD(dst,a,b) dst = (void*)((intptr_t)(a) + (intptr_t)(b))
#define FIXNUM_SUB(dst,a,b) dst = (void*)((intptr_t)(a) - (intptr_t)(b))
#define FIXNUM_MUL(dst,a,b) dst = (void*)(UNFIXNUM(a) * (intptr_t)(b))
#define FIXNUM_DIV(dst,a,b) dst = (void*)(FIXNUM((intptr_t)(a) / (intptr_t)(b)))
#define FIXNUM_REM(dst,a,b) dst = (void*)((intptr_t)(a) % (intptr_t)(b))
#define FIXNUM_EQ(dst,a,b) dst = (void*)FIXNUM((intptr_t)(a) == (intptr_t)(b))
#define FIXNUM_NE(dst,a,b) dst = (void*)FIXNUM((intptr_t)(a) != (intptr_t)(b))
#define FIXNUM_LT(dst,a,b) dst = (void*)FIXNUM((intptr_t)(a) < (intptr_t)(b))
#define FIXNUM_GT(dst,a,b) dst = (void*)FIXNUM((intptr_t)(a) > (intptr_t)(b))
#define FIXNUM_LTE(dst,a,b) dst = (void*)FIXNUM((intptr_t)(a) <= (intptr_t)(b))
#define FIXNUM_GTE(dst,a,b) dst = (void*)FIXNUM((intptr_t)(a) >= (intptr_t)(b))
#define FIXNUM_TAG(dst,x) dst = (void*)FIXNUM(GET_TAG(x))
#define FIXNUM_UNFIXNUM(dst,x) dst = (void*)UNFIXNUM(x)

#define HEAP_SIZE (32*1024*1024)
#define BASE_HEAD_SIZE 2
#define OBJ_HEAD_SIZE 2

#define MAX_LEVEL 512*1024

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
  void *resolve_;

  void *method;

  void *m_ampersand;
  void *m_underscore;

  // runtime's C API
  void (*bad_type)(REGS, char *expected, int arg_index, char *name);
  void* (*handle_args)(REGS, void *E, intptr_t expected, intptr_t size, void *tag, void *meta);
  char* (*print_object_f)(struct api_t *api, void *object);
  void *(*gc)(struct api_t *api, void *root);
  void *(*alloc_text)(struct api_t *api, char *s);
  void (*fatal)(struct api_t *api, char *msg);
  void **(*resolve_method)(struct api_t *api, char *name);
  int (*resolve_type)(struct api_t *api, char *name);
  void (*set_type_size_and_name)(struct api_t *api, intptr_t tag, intptr_t size, void *name);
  void (*set_method)(struct api_t *api, void *method, void *type, void *handler);
  void *(*find_export)(struct api_t *api, void *name, void *exports);
  void *(*load_lib)(struct api_t *api, char *name);

  void *marks[MAX_LEVEL];

  void *heap[HEAP_SIZE];
} api_t;

typedef void *(*pfun)(REGS);

#define Void api->void_
#define Empty api->empty_
#define Resolve api->resolve_
#define Top api->top
#define Base api->base
#define Level api->level

#define OBJECT_CODE(x) (((pfun*)((void**)((uintptr_t)(x)&~TAG_MASK)-1))[0])
#define DATA_TAG(o) ((uintptr_t)OBJECT_CODE(o)&0xFFFFFFFF)
#define OBJECT_LEVEL(x) ((uintptr_t)(((void**)((uintptr_t)(x)&~TAG_MASK)-2)[0]))

#ifdef SYMTA_DEBUG
#define HEAP_GUARD() \
  if ((uint8_t*)Top - (uint8_t*)api->heap < 1024*4) { \
    api->fatal(api, "out of memory"); \
  }
#else
#define HEAP_GUARD()
#endif

#define ALLOC_BASIC(dst,code,count) \
  HEAP_GUARD(); \
  Top = (void**)Top - ((uintptr_t)(count)+OBJ_HEAD_SIZE); \
  *((void**)Top+0) = (void*)Level; \
  *((void**)Top+1) = (void*)(code); \
  dst = (void**)Top+OBJ_HEAD_SIZE;

#define ALLOC_CLOSURE(dst,code,count) \
  ALLOC_BASIC(dst,code,count); \
  dst = ADD_TAG(dst, T_CLOSURE);

#define ALLOC_DATA(dst,code,count) \
  ALLOC_BASIC(dst,(void*)(code),count); \
  dst = ADD_TAG(dst, T_DATA);

#define RESOLVE_TYPE(dst,name) \
  dst = (void*)(intptr_t)api->resolve_type(api, (char*)(name));
#define RESOLVE_METHOD(dst,name) dst = api->resolve_method(api, name);
#define SET_TYPE_SIZE_AND_NAME(tag,size,name) api->set_type_size_and_name(api,tag,size,name);
#define DMET(method,type,handler) api->set_method(api,method,type,handler);

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
#define GC(dst,value) \
  /*fprintf(stderr, "GC %p:%p -> %p\n", Top, Base, api->other->top);*/ \
  dst = api->gc(api, (void*)(value));
#define RETURN(value) \
   if (IMMEDIATE(value) && !LIFTS_LIST(Base)) { \
     return (void*)(value); \
   } \
   GC(value,value); \
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
#define MARK(name) api->marks[Level>>1] = (void*)(name);
#define HEAP_FLIP() api = api->other;
#define PUSH_BASE() \
  HEAP_FLIP(); \
  Level += 2; \
  MARK(0); \
  /*fprintf(stderr, "Entering %ld\n", Level);*/ \
  Top = (void**)Top-BASE_HEAD_SIZE; \
  *((void**)Top+0) = Base; \
  LIFTS_LIST(Top) = 0; \
  Base = Top;
#define POP_BASE() \
  /*fprintf(stderr, "Leaving %ld\n", Level);*/ \
  Top = (void**)Base+BASE_HEAD_SIZE; \
  Base = *(void**)Base; \
  Level -= 2; \
  HEAP_FLIP();
#define CALL_NO_POP(k,f) k = OBJECT_CODE(f)(REGS_ARGS(f));
#define CALL(k,f) CALL_NO_POP(k,f); POP_BASE();

#define CALL_METHOD_WITH_TAG(k,o,m,tag) \
   { \
      void *p; \
      api->method = m; \
      if (tag == T_DATA) { \
        tag = DATA_TAG(o); \
      } \
      p = ((void**)(m))[tag]; \
      k = OBJECT_CODE(p)(REGS_ARGS(p)); \
   }

#define CALL_METHOD(k,o,m) \
  { \
    uintptr_t tag = (uintptr_t)GET_TAG(o); \
    CALL_METHOD_WITH_TAG(k,o,m,tag); \
    POP_BASE(); \
  }

#define CALL_TAGGED_NO_POP(k,o) \
  { \
    uintptr_t tag = (uintptr_t)GET_TAG(o); \
    if (tag == T_CLOSURE) { \
      k = OBJECT_CODE(o)(REGS_ARGS(o)); \
    } else { \
      void *as = ADD_TAG((void**)Top+OBJ_HEAD_SIZE, T_LIST); \
      void *e; \
      ARGLIST(e,2); \
      ARG_STORE(e,0,o); \
      ARG_STORE(e,1,as); \
      CALL_METHOD_WITH_TAG(k,o,api->m_ampersand,tag); \
    } \
  }
#define CALL_TAGGED(k,o) CALL_TAGGED_NO_POP(k,o); POP_BASE();

//FIXME: refactor these
#define VIEW_REF1(base,off) *(uint8_t*)((uint8_t*)(base)+(off)-T_VIEW)
#define VIEW_REF4(base,off) *(uint32_t*)((uint8_t*)(base)+(off)*4-T_VIEW)
#define VIEW_GET(base,off) *(void**)((uint8_t*)(base)+(off)*sizeof(void*)-T_VIEW)
#define CONS_REF1(base,off) *(uint8_t*)((uint8_t*)(base)+(off)-T_CONS)
#define CONS_REF4(base,off) *(uint32_t*)((uint8_t*)(base)+(off)*4-T_CONS)
#define CONS_REF(base,off) *(void**)((uint8_t*)(base)+(off)*sizeof(void*)-T_CONS)
#define CLOSURE_REF1(base,off) *(uint8_t*)((uint8_t*)(base)+(off)-T_CLOSURE)
#define CLOSURE_REF4(base,off) *(uint32_t*)((uint8_t*)(base)+(off)*4-T_CLOSURE)
#define CLOSURE_REF(base,off) *(void**)((uint8_t*)(base)+(off)*sizeof(void*)-T_CLOSURE)
#define DATA_REF1(base,off) *(uint8_t*)((uint8_t*)(base)+(off)-T_DATA)
#define DATA_REF4(base,off) *(uint32_t*)((uint8_t*)(base)+(off)*4-T_DATA)
#define DATA_REF(base,off) *(void**)((uint8_t*)(base)+(off)*sizeof(void*)-T_DATA)
#define DATA_SET(dst,dst_off,src) LIFT(&DATA_REF(dst,0),dst_off,src)
#define LIST_REF(base,off) *(void**)((uint8_t*)(base)+(off)*sizeof(void*)-T_LIST)
#define ARG_LOAD(dst,src,src_off) dst = *((void**)(src)+(src_off))
#define ARG_STORE(dst,dst_off,src) *((void**)(dst)+(dst_off)) = (void*)(src)
#define LOAD(dst,src,src_off) dst = CLOSURE_REF(src,src_off)
#define STORE(dst,dst_off,src) CLOSURE_REF(dst,dst_off) = (void*)(src)
#define COPY(dst,dst_off,src,src_off) CLOSURE_REF(dst,dst_off) = CLOSURE_REF(src,src_off)
#define MOVE(dst,src) dst = (void*)(src)
#define TAGGED(dst,src,tag) dst = ADD_TAG(src,tag)
#define DGET(dst,src,off) dst = DATA_REF(src, off)
#define DSET(dst,off,src) DATA_SET(dst, off, src)
#define DINIT(dst,off,src) DATA_REF(dst, off) = src
#define UNTAGGED_STORE(dst,off,src) *(void**)((uint8_t*)(dst)+(uint64_t)(off)) = src

#define IS_BIGTEXT(o) (GET_TAG(o) == T_DATA && DATA_TAG(o) == T_TEXT)
#define IS_TEXT(o) (GET_TAG(o) == T_FIXTEXT || IS_BIGTEXT(o))
#define BIGTEXT_SIZE(o) DATA_REF4(o,0)
#define BIGTEXT_DATA(o) ((char*)&DATA_REF1(o,4))

#define FATAL(msg) api->fatal(api, BIGTEXT_DATA(msg));

#define CHECK_NARGS(expected,size,meta) \
  if (NARGS(E) != FIXNUM(expected)) { \
    return api->handle_args(REGS_ARGS(P), E, FIXNUM(expected), FIXNUM(size), Void, meta); \
  }
#define CHECK_VARARGS(size,meta) \
  if (NARGS(E) < FIXNUM(0)) { \
    return api->handle_args(REGS_ARGS(P), E, FIXNUM(-1), FIXNUM(size), Void, meta); \
  }

void *entry(REGS);
