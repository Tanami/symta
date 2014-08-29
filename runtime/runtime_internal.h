#ifndef RUNTIME_INTERNAL_H
#define RUNTIME_INTERNAL_H

#include "runtime.h"

#define FN_GET_TAG   FIXNUM(-1)
#define FN_GET_SIZE  FIXNUM(-2)
#define FN_GET_META  FIXNUM(-3)
#define FN_GET_NARGS FIXNUM(-4)

#define MAX_TYPES (1024)
#define MAX_METHODS (8*1024)
#define MAX_LIBS 1024

// predefine method slots
#define M_SIZE 0
#define M_NAME 1
#define M_SINK 2

#define LIST_SIZE(o) ((uintptr_t)OBJECT_CODE(o))

#define C_ANY(o,arg_index,meta)

#define C_FN(o,arg_index,meta) \
  if (GET_TAG(o) != T_CLOSURE) \
    api->bad_type(REGS_ARGS(P), "fn", arg_index, meta)

#define C_FIXNUM(o,arg_index,meta) \
  if (GET_TAG(o) != T_FIXNUM) \
    api->bad_type(REGS_ARGS(P), "int", arg_index, meta)

#define C_FLOAT(o,arg_index,meta) \
  if (GET_TAG(o) != T_FLOAT) \
    api->bad_type(REGS_ARGS(P), "float", arg_index, meta)

#define C_TEXT(o,arg_index,meta) \
  if (!IS_TEXT(o)) \
    api->bad_type(REGS_ARGS(P), "text", arg_index, meta)

#define BUILTIN_CLOSURE(dst,code) { ALLOC_CLOSURE(dst, code, 0); }


#define BUILTIN_CHECK_NARGS(expected,tag,name) \
  if (NARGS(E) != FIXNUM(expected)) { \
    void *meta, *ttag, *t; \
    LIST_ALLOC(meta, 1); \
    TEXT(t, name); \
    LIST_REF(meta,0) = t; \
    if (tag) { \
      TEXT(ttag, tag); \
    } else { \
      ttag = Void; \
    } \
    return api->handle_args(REGS_ARGS(P), E, FIXNUM(expected), FIXNUM(0), ttag, meta); \
  }
#define BUILTIN_CHECK_VARARGS(expected,tag,name) \
  if (NARGS(E) < FIXNUM(expected)) { \
    void *meta, *ttag, *t; \
    LIST_ALLOC(meta, 1); \
    TEXT(t, name); \
    LIST_REF(meta,0) = t; \
    if (tag) { \
      TEXT(ttag, tag); \
    } else { \
      ttag = Void; \
    } \
    return api->handle_args(REGS_ARGS(P), E, -FIXNUM(expected), FIXNUM(0), ttag, meta); \
  }

#define BUILTIN0(sname, name) \
  static void *b_##name(REGS) { \
  PROLOGUE; \
  void *A, *R; \
  BUILTIN_CHECK_NARGS(0,0,sname);
#define BUILTIN1(sname,name,a_check,a) \
  static void *b_##name(REGS) { \
  PROLOGUE; \
  void *A, *R, *a; \
  BUILTIN_CHECK_NARGS(1,0,sname); \
  a = getArg(0); \
  a_check(a, 0, sname);
#define BUILTIN2(sname,name,a_check,a,b_check,b) \
  static void *b_##name(REGS) { \
  PROLOGUE; \
  void *A, *R, *a, *b; \
  BUILTIN_CHECK_NARGS(2,0,sname); \
  a = getArg(0); \
  a_check(a, 0, sname); \
  b = getArg(1); \
  b_check(b, 1, sname);
#define BUILTIN3(sname,name,a_check,a,b_check,b,c_check,c) \
  static void *b_##name(REGS) { \
  PROLOGUE; \
  void *A, *R, *a, *b,*c; \
  BUILTIN_CHECK_NARGS(3,0,sname); \
  a = getArg(0); \
  a_check(a, 0, sname); \
  b = getArg(1); \
  b_check(b, 1, sname); \
  c = getArg(2); \
  c_check(c, 2, sname);
#define BUILTIN_VARARGS(sname,name) \
  static void *b_##name(REGS) { \
  PROLOGUE; \
  void *A, *R; \
  BUILTIN_CHECK_VARARGS(0,0,sname);
#define RETURNS(r) R = (void*)(r); RETURN(R); }
#define RETURNS_VOID Top = Base; }


#endif //SYMTA_H
