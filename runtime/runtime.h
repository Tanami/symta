#ifndef SYMTA_RUNTIME_H
#define SYMTA_RUNTIME_H

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>

#define SYMTA_DEBUG 1

// used for debugging
#define D fprintf(stderr, "%d:%s\n", __LINE__, __FILE__);

#define TAGL_BITS ((uintptr_t)3)
#define PTR_BITS ((uintptr_t)40)
#define TAGH_BITS ((uintptr_t)10)

#define TAGH_SHIFT (64-TAGH_BITS)

#define TAGL_MASK (((uintptr_t)1<<TAGL_BITS)-1)
#define PTR_MASK ((((uintptr_t)1<<PTR_BITS)-1)<<3)
#define TAGH_MASK ((((uintptr_t)1<<TAGH_BITS)-1)<<TAGH_SHIFT)

#define O_TAGL(o) ((uintptr_t)(o)&TAGL_MASK)
#define O_TAGH(o) ((uintptr_t)(o)>>TAGH_SHIFT)
#define O_TAG(o) ((uintptr_t)(o)&(TAGH_MASK|TAGL_MASK))
#define O_TYPE(o) (O_TAGL(o) == T_DATA ? O_TAGH(o) : O_TAGL(o))
#define O_PTR(o) ((uintptr_t)(o)&PTR_MASK)
#define REF1(base,off) (*(uint8_t*)(O_PTR(base)+(off)))
#define REF4(base,off) (*(uint32_t*)(O_PTR(base)+(off)*4))
#define REF(base,off) (*(void**)(O_PTR(base)+(off)*sizeof(void*)))
#define O_CODE(x) REF(x,-2)
#define O_LEVEL(x) ((uintptr_t)REF(x,-1))
#define O_FN(x) ((pfun)O_CODE(x))

#define NARGS(x) ((intptr_t)O_CODE(x))

#define TAG(tag) (((uintptr_t)(tag)<<TAGH_SHIFT) | T_DATA)

#define ADD_TAGL(src,tag) ((void*)((uintptr_t)(src) | tag))
#define ADD_TAG(src,tag) ((void*)((uintptr_t)(src) | TAG(tag)))

#define IMMEDIATE(x) (O_TAGL(x) != T_DATA)


#define T_INT     0
#define T_FLOAT   1
#define T_FIXTEXT 2
#define T_DATA    3
#define T_CLOSURE 4
#define T_LIST    5
#define T_VIEW    6
#define T_CONS    7
#define T_OBJECT  8
#define T_TEXT    9
#define T_VOID    10
#define T_GENERIC_LIST 11
#define T_GENERIC_TEXT 12
#define T_HARD_LIST    13
#define T_NAME         14
#define T_NAME_TEXT    15

// sign preserving shifts
#define ASHL(x,count) ((x)*(1<<(count)))
#define ASHR(x,count) ((x)/(1<<(count)))
#define FIXNUM(x) ASHL((intptr_t)(x),TAGL_BITS)
#define UNFIXNUM(x) ASHR((intptr_t)(x),TAGL_BITS)

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
#define FIXNUM_TAG(dst,x) dst = (void*)FIXNUM(O_TAGL(x))
#define FIXNUM_UNFIXNUM(dst,x) dst = (void*)UNFIXNUM(x)

#define LOAD_FLOAT(dst,x) { \
    double d_ = (double)(x); \
    uint64_t t_ = (*(uint64_t*)&d_); \
    dst = (void*)((t_&~TAGL_MASK) | T_FLOAT); \
  }

#define UNFLOAT(dst,x) { \
    uint64_t t_ = (uint64_t)(x)&~TAGL_MASK; \
    dst = *(double*)&t_; \
  }

#define HEAP_SIZE (32*1024*1024)
#define BASE_HEAD_SIZE 2
#define OBJ_HEAD_SIZE 2

#define MAX_LEVEL 512*1024

// P holds points to closure of current function
// E holds pointer to arglist of current function
#define REGS void *P, struct api_t *api
#define REGS_ARGS(P) P, api

typedef struct api_t {
  void *base;
  void *top; // heap top

  struct api_t *other;

  intptr_t level; // stack frame depth

  void *method; // current method, we execute

  void *jmp_return;

  // constants
  void *void_;
  void *empty_;
  void *m_ampersand;
  void *m_underscore;

  // runtime's C API
  void (*bad_type)(REGS, char *expected, int arg_index, char *name);
  void* (*handle_args)(REGS, void *E, intptr_t expected, intptr_t size, void *tag, void *meta);
  char* (*print_object_f)(struct api_t *api, void *object);
  void *(*gc)(struct api_t *api, void *root);
  void *(*alloc_text)(struct api_t *api, char *s);
  void (*fatal)(struct api_t *api, void *msg);
  void **(*resolve_method)(struct api_t *api, char *name);
  int (*resolve_type)(struct api_t *api, char *name);
  void (*set_type_size_and_name)(struct api_t *api, intptr_t tag, intptr_t size, void *name);
  void (*set_method)(struct api_t *api, void *method, void *type, void *handler);
  void *(*find_export)(struct api_t *api, void *name, void *exports);
  void *(*load_lib)(struct api_t *api, char *name);
  char *(*text_chars)(struct api_t *api, void *text);

  void *marks[MAX_LEVEL];

  void *heap[HEAP_SIZE];
} api_t;

typedef void *(*pfun)(REGS);

#define Void api->void_
#define Empty api->empty_
#define Top api->top
#define Base api->base
#define Level api->level

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
  dst = (void**)Top - (uintptr_t)(count); \
  Top = (void**)dst - OBJ_HEAD_SIZE; \
  *((void**)Top+0) = (void*)(code); \
  *((void**)Top+1) = (void*)Level;

#define ALLOC_NO_CODE(dst,count) \
  HEAP_GUARD(); \
  dst = (void**)Top - (uintptr_t)(count); \
  Top = (void**)dst - 1; \
  *((void**)Top+0) = (void*)Level;

#define ALLOC_CLOSURE(dst,code,count) \
  ALLOC_BASIC(dst,code,count); \
  dst = ADD_TAG(dst, T_CLOSURE);

#define ALLOC_DATA(dst,tag,count) \
  ALLOC_NO_CODE(dst,count); \
  dst = ADD_TAG(dst,tag);

#define ARGLIST(dst,size) ALLOC_BASIC(dst,FIXNUM(size),size)

#define LIST_ALLOC(dst,size) \
  ARGLIST(dst,size); \
  dst = ADD_TAG(dst, T_LIST);

#define RESOLVE_TYPE(dst,name) \
  dst = (void*)(intptr_t)api->resolve_type(api, (char*)(name));
#define RESOLVE_METHOD(dst,name) dst = api->resolve_method(api, name);
#define SET_TYPE_SIZE_AND_NAME(tag,size,name) api->set_type_size_and_name(api,tag,size,name);
#define DMET(method,type,handler) api->set_method(api,method,type,handler);

#define IS_LIST(o) (O_TAG(o) == TAG(T_LIST))

#define print_object(object) api->print_object_f(api, object)
#define MIN(a,b) ((a) < (b) ? (a) : (b))

#define LOCAL_LABEL(name) name:;
#define BRANCH(cnd,name) if (cnd) goto name;
#define ZBRANCH(cnd,name) if (!(cnd)) goto name;
#define JMP(name) goto name;
#define LOCAL_CLOSURE(dst,size) ALLOC_CLOSURE(dst,FIXNUM(size),size)
#define BEGIN_CODE static void __dummy___ () {
#define END_CODE }
#define LOAD_FIXNUM(dst,x) dst = (void*)FIXNUM(x)
#define TEXT(dst,x) dst = api->alloc_text(api,(char*)(x))
#define DECL_LABEL(name) static void *name(REGS);
#define THIS_METHOD(dst) dst = ADD_TAG(api->method, T_LIST);
#define TYPE_ID(dst,o) dst = (void*)FIXNUM(O_TYPE(o));

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
#define LIFTS_CONS(dst,head,tail) \
  Top=(void**)Top-2; \
  *((void**)Top+0) = (head); \
  *((void**)Top+1) = (tail); \
  dst = Top;
#define LIFTS_HEAD(xs) (*((void**)(xs)+0))
#define LIFTS_TAIL(xs) (*((void**)(xs)+1))
#define LIFTS_LIST(base) (*((void**)(base)-2))
#define LIFT(base,pos,value) \
  { \
    void **p_ = (void**)(base)+(pos); \
    *p_ = (value); \
    if (!IMMEDIATE(value) && O_LEVEL(base) < O_LEVEL(value)) { \
      LIFTS_CONS(LIFTS_LIST(Base), p_, LIFTS_LIST(Base)); \
    } \
  }
#define MARK(name) api->marks[Level>>1] = (void*)(name);
#define HEAP_FLIP() api = api->other;
#define PUSH_BASE() \
  HEAP_FLIP(); \
  Level += 2; \
  MARK(0); \
  /*fprintf(stderr, "Entering %ld\n", Level);*/ \
  *((void**)Top-1) = Base; \
  LIFTS_LIST(Top) = 0; \
  Base = Top; \
  Top = (void**)Top-BASE_HEAD_SIZE;
#define POP_BASE() \
  /*fprintf(stderr, "Leaving %ld\n", Level);*/ \
  Top = Base; \
  Base = *((void**)Top-1); \
  Level -= 2; \
  HEAP_FLIP();
#define CALL_NO_POP(k,f) k = O_FN(f)(REGS_ARGS(f));
#define CALL(k,f) CALL_NO_POP(k,f); POP_BASE();

#define CALL_METHOD_WITH_TAG_NO_SAVE(k,o,m) \
   { \
      void *f_; \
      f_ = ((void**)(m))[O_TYPE(o)]; \
      k = O_FN(f_)(REGS_ARGS(f_)); \
   }

#define CALL_METHOD_WITH_TAG(k,o,m) \
   api->method = m; \
   CALL_METHOD_WITH_TAG_NO_SAVE(k,o,m);

#define CALL_METHOD(k,o,m) \
  { \
    CALL_METHOD_WITH_TAG(k,o,m); \
    POP_BASE(); \
  }

#define CALL_TAGGED_NO_POP(k,o) \
  { \
    if (O_TAG(o) == TAG(T_CLOSURE)) { \
      k = O_FN(o)(REGS_ARGS(o)); \
    } else { \
      void *as = ADD_TAG((void**)Top+OBJ_HEAD_SIZE, T_LIST); \
      void *e; \
      ARGLIST(e,2); \
      ARG_STORE(e,0,o); \
      ARG_STORE(e,1,as); \
      CALL_METHOD_WITH_TAG(k,o,api->m_ampersand); \
    } \
  }
#define CALL_TAGGED(k,o) CALL_TAGGED_NO_POP(k,o); POP_BASE();

#define ARG_LOAD(dst,src,src_off) dst = *((void**)(src)+(src_off))
#define ARG_STORE(dst,dst_off,src) *((void**)(dst)+(dst_off)) = (void*)(src)
#define LOAD(dst,src,src_off) dst = REF(src,src_off)
#define STORE(dst,dst_off,src) REF(dst,dst_off) = (void*)(src)
#define COPY(dst,dst_off,src,src_off) REF(dst,dst_off) = REF(src,src_off)
#define MOVE(dst,src) dst = (void*)(src)
#define TAGGED(dst,src,tag) dst = ADD_TAG(src,tag)
#define DATA_SET(dst,dst_off,src) LIFT(&REF(dst,0),dst_off,src)
#define DGET(dst,src,off) dst = REF(src, off)
#define DSET(dst,off,src) DATA_SET(dst, off, src)
#define DINIT(dst,off,src) REF(dst, off) = src
#define UNTAGGED_STORE(dst,off,src) *(void**)((uint8_t*)(dst)+(uint64_t)(off)) = src

#define FATAL(msg) api->fatal(api, msg);

#define SET_UNWIND_HANDLER(r,h) api->marks[Level>>1] = h;
#define REMOVE_UNWIND_HANDLER(r) api->marks[Level>>1] = 0;

typedef struct {
  jmp_buf anchor;
  intptr_t level;
  api_t *api;
} jmp_state;

#define SETJMP(dst) { \
    jmp_state *js_; \
    ALLOC_BASIC(api->jmp_return, 0, ((sizeof(jmp_state)+7)>>3)); \
    js_ = (jmp_state*)api->jmp_return; \
    js_->level = api->level; \
    js_->api = api; \
    setjmp(js_->anchor); \
    api = js_->api; \
    dst = api->jmp_return; \
  }

#define LONGJMP(state,value) { \
    jmp_state *js_; \
    js_ = (jmp_state*)state; \
    while (js_->level != api->level) { \
      void *h_ = api->marks[Level>>1]; \
      if (O_TAG(h_) == TAG(T_CLOSURE) { \
          void *k_; \
          PUSH_BASE(); \
          ARGLIST(E,0); \
          CALL(k_,h_) \
      } \
      if (!IMMEDIATE(value) || LIFTS_LIST(Base)) { \
        GC(value,value); \
      } \
      POP_BASE(); \
    } \
    api->jmp_return = value; \
    longjmp(js_->anchor, 0); \
  }

#define CHECK_NARGS(expected,size,meta) \
  if (NARGS(E) != FIXNUM(expected)) { \
    return api->handle_args(REGS_ARGS(P), E, FIXNUM(expected), FIXNUM(size), Void, meta); \
  }
#define CHECK_VARARGS(size,meta) \
  if (NARGS(E) < FIXNUM(0)) { \
    return api->handle_args(REGS_ARGS(P), E, FIXNUM(-1), FIXNUM(size), Void, meta); \
  }

// kludge for FFI identifiers
#define text_ char*
#define voidp_ void*
#define u4 uint32_t

#define FFI_VAR(type,name) type name;

#define FFI_TO_INT(dst,src) \
  if (O_TAGL(src) != T_INT) \
    api->bad_type(REGS_ARGS(P), "int", 0, 0); \
  dst = (int)UNFIXNUM(src);
#define FFI_FROM_INT(dst,src) dst = (void*)FIXNUM((intptr_t)src);

#define FFI_TO_U4(dst,src) \
  if (O_TAGL(src) != T_INT) \
    api->bad_type(REGS_ARGS(P), "int", 0, 0); \
  dst = (uint32_t)UNFIXNUM(src);
#define FFI_FROM_U4(dst,src) dst = (void*)FIXNUM((intptr_t)src);

#define FFI_TO_DOUBLE(dst,src) UNFLOAT(dst,src);
#define FFI_FROM_DOUBLE(dst,src) LOAD_FLOAT(dst,src);

#define FFI_TO_FLOAT(dst,src) { \
    double _x; \
    UNFLOAT(_x,src); \
    _dst = (float)_x; \
  }
#define FFI_FROM_FLOAT(dst,src) LOAD_FLOAT(dst,(double)src);

#define FFI_TO_TEXT_(dst,src) dst = api->text_chars(api,src);
#define FFI_FROM_TEXT_(dst,src) dst = api->alloc_text(api,src);

#define FFI_TO_VOIDP_(dst,src) dst = (void*)(src);
#define FFI_FROM_VOIDP_(dst,src) dst = (void*)(src);

#define FFI_GET(dst,type,ptr,off) dst = (void*)FIXNUM(((type*)(ptr))[UNFIXNUM(off)]);
#define FFI_SET(type,ptr,off,val) ((type*)(ptr))[UNFIXNUM(off)] = (type)UNFIXNUM(val);

void *entry(REGS);

#endif //SYMTA_RUNTIME_H
