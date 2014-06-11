#include <dlfcn.h>
#include <stdarg.h>

#include "runtime.h"

static void *b_list(REGS);
static void *b_view(REGS);
static void *b_text(REGS);
static void *b_cons(REGS);

#define getArg(i) REF(E,i)
#define getVal(x) ((uintptr_t)(x)&~TAG_MASK)

static api_t apis[2]; // one for each heap

#define HEAP_OBJECT(o) ((void*)apis < (void*)(o) && (void*)(o) < (void*)(apis+2))

static void fatal(char *fmt, ...) {
   va_list ap;
   va_start(ap,fmt);
   vfprintf(stderr, fmt, ap);
   va_end(ap);
   abort();
}

static void bad_type(REGS, char *expected, int arg_index, char *name) {
  int i, nargs = (int)UNFIXNUM(NARGS);
  printf("arg %d isnt %s, in: %s", arg_index, expected, name);
  for (i = 0; i < nargs; i++) printf(" %s", print_object(getArg(i)));
  printf("\n");
  abort();
}

static void bad_call(REGS, void *method) {
  int i, nargs = (int)UNFIXNUM(NARGS);
  printf("bad call: %s", print_object(getArg(0)));
  printf(" %s", print_object(method));
  for (i = 1; i < nargs; i++) printf(" %s", print_object(getArg(i)));
  printf("\n");
  abort();
}

#define CAR(x) ((void**)getVal(x))[0]
#define CDR(x) ((void**)getVal(x))[1]
#define CONS(a, b) \
  ALLOC(R, b_cons, 2); \
  STORE(R, 0, a); \
  STORE(R, 1, b); \

static char *print_object_r(api_t *api, char *out, void *o);

// FIXME: use heap instead
static char print_buffer[1024*1024*2];
char* print_object_f(api_t *api, void *object) {
  print_object_r(api, print_buffer, object);
  return print_buffer;
}

static char *text_to_cstring(void *o) {
  int i;
  int l = UNFIXNUM(*(uint32_t*)o);
  char *p = (char*)o + 4;
  char *out = print_buffer;
  for (i = 0; i < l; i++) *out++ = *p++;
  *out = 0;
  return print_buffer;
}


#define C_ANY(o,arg_index,meta)

#define C_FIXNUM(o,arg_index,meta) \
  if (GET_TAG(o) != T_FIXNUM) \
    bad_type(REGS_ARGS(E,P), "integer", arg_index, meta)

#define C_TEXT(o,arg_index,meta) \
  if (GET_TAG(o) != T_FIXTEXT && (GET_TAG(o) != T_CLOSURE || POOL_HANDLER(o) != b_text)) \
    bad_type(REGS_ARGS(E,P), "text", arg_index, meta)

#define C_CONS(o,arg_index,meta) \
  if (GET_TAG(o) != T_CLOSURE || POOL_HANDLER(o) != b_cons) \
    bad_type(REGS_ARGS(E,P), "cons", arg_index, meta)

#define BUILTIN_CHECK_NARGS(expected,tag,name) \
  if (NARGS != FIXNUM(expected)) { \
    void *meta, *ttag, *t; \
    LIST(meta, 1); \
    TEXT(t, name); \
    REF(meta,0) = t; \
    if (tag) { \
      TEXT(ttag, tag); \
    } else { \
      ttag = Void; \
    } \
    return api->handle_args(REGS_ARGS(E,P), FIXNUM(expected), FIXNUM(0), ttag, meta); \
  }
#define BUILTIN_CHECK_VARARGS(expected,tag,name) \
  if (NARGS < FIXNUM(expected)) { \
    void *meta, *ttag, *t; \
    LIST(meta, 1); \
    TEXT(t, name); \
    REF(meta,0) = t; \
    if (tag) { \
      TEXT(ttag, tag); \
    } else { \
      ttag = Void; \
    } \
    return api->handle_args(REGS_ARGS(E,P), -FIXNUM(expected), FIXNUM(0), ttag, meta); \
  }

#define BUILTIN0(sname, name) \
  static void *b_##name(REGS) { \
  void *A, *R; \
  BUILTIN_CHECK_NARGS(0,0,sname);
#define BUILTIN1(sname,name,a_check,a) \
  static void *b_##name(REGS) { \
  void *A, *R, *a; \
  BUILTIN_CHECK_NARGS(1,0,sname); \
  a = getArg(0); \
  a_check(a, 0, sname);
#define BUILTIN2(sname,name,a_check,a,b_check,b) \
  static void *b_##name(REGS) { \
  void *A, *R, *a, *b; \
  BUILTIN_CHECK_NARGS(2,0,sname); \
  a = getArg(0); \
  a_check(a, 0, sname); \
  b = getArg(1); \
  b_check(b, 1, sname);
#define BUILTIN3(sname,name,a_check,a,b_check,b,c_check,c) \
  static void *b_##name(REGS) { \
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
  void *A, *R; \
  BUILTIN_CHECK_VARARGS(0,0,sname);
#define BUILTIN_HANDLER(sname,name,a_check,a) \
  static void *b_##name(REGS) { \
  void *A, *R, *a; \
  BUILTIN_CHECK_VARARGS(1,sname,sname); \
  a = getArg(0); \
  a_check(a, 0, sname);
#define RETURNS(r) Top = Base; return (void*)(r); }
#define RETURNS_VOID Top = Base; }


static void *s_size, *s_get, *s_set, *s_hash;
static void *s_neg, *s_plus, *s_sub, *s_mul, *s_div, *s_rem, *s_is, *s_isnt, *s_lt, *s_gt, *s_lte, *s_gte;
static void *s_mask, *s_ior, *s_xor; //NOTE: ~X can be implemented as X^0xFFFFFFFF
static void *s_shl, *s_shr;
static void *s_head, *s_tail, *s_add, *s_end, *s_code, *s_text;
static void *s_x; //duplicate

#define TEXT_SIZE(o) UNFIXNUM(REF4(o,0))
#define TEXT_DATA(o) ((char*)&REF1(o,4))

static int texts_equal(void *a, void *b) {
  intptr_t al, bl;
  if (GET_TAG(a) == T_FIXTEXT || GET_TAG(b) == T_FIXTEXT) return a == b;
  al = TEXT_SIZE(a);
  bl = TEXT_SIZE(b);
  return al == bl && !memcmp(TEXT_DATA(a), TEXT_DATA(b), UNFIXNUM(al));
}


#define MOD_ADLER 65521
uint32_t hash(uint8_t *data, int len) {
  uint32_t a = 1, b = 0;
  int index;
  for (index = 0; index < len; ++index) {
    a = (a + data[index]) % MOD_ADLER;
    b = (b + a) % MOD_ADLER;
  } 
  return (b << 16) | a;
}

BUILTIN2("void is",void_is,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a == b))
BUILTIN2("void isnt",void_isnt,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a != b))
BUILTIN1("void end",void_end,C_ANY,o)
RETURNS(FIXNUM(1))
BUILTIN2("void get",void_get,C_ANY,o,C_ANY,key)
RETURNS(Void)
BUILTIN_HANDLER("void",void,C_TEXT,x)
  STORE(E, 0, P);
  if (texts_equal(x,s_is)) return b_void_is(REGS_ARGS(E,P));
  else if (texts_equal(x,s_isnt)) return b_void_isnt(REGS_ARGS(E,P));
  else if (texts_equal(x,s_end)) return b_void_end(REGS_ARGS(E,P));
  else if (texts_equal(x,s_get)) return b_void_get(REGS_ARGS(E,P));
  else bad_call(REGS_ARGS(E,P),x);
RETURNS_VOID

#define IS_TEXT(o) (GET_TAG(o) == T_CLOSURE && POOL_HANDLER(o) == b_text)


BUILTIN2("text is",text_is,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(IS_TEXT(b) ? texts_equal(a,b) : 0))
BUILTIN2("text isnt",text_isnt,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(IS_TEXT(b) ? !texts_equal(a,b) : 1))
BUILTIN1("text size",text_size,C_ANY,o)
RETURNS(TEXT_SIZE(o))
BUILTIN2("text {}",text_get,C_ANY,o,C_FIXNUM,index)
  char t[2];
  if ((uintptr_t)REF4(o,0) <= (uintptr_t)index) {
    printf("index out of bounds\n");
    TEXT(P, "{}");
    bad_call(REGS_ARGS(E,P),P);
  }
  t[0] = REF1(o,4+UNFIXNUM(index));
  t[1] = 0;
  TEXT(R,t);
RETURNS(R)
BUILTIN1("text end",text_end,C_ANY,o)
RETURNS(FIXNUM(1))
BUILTIN1("text hash",text_hash,C_ANY,o)
RETURNS(FIXNUM(hash((uint8_t*)o+4, *(int32_t*)o)))
BUILTIN_HANDLER("text",text,C_TEXT,x)
  STORE(E, 0, P);
  if (texts_equal(x,s_size)) return b_text_size(REGS_ARGS(E,P));
  else if (texts_equal(x,s_get)) return b_text_get(REGS_ARGS(E,P));
  else if (texts_equal(x,s_is)) return b_text_is(REGS_ARGS(E,P));
  else if (texts_equal(x,s_isnt)) return b_text_isnt(REGS_ARGS(E,P));
  else if (texts_equal(x,s_end)) return b_text_end(REGS_ARGS(E,P));
  else if (texts_equal(x,s_hash)) return b_text_hash(REGS_ARGS(E,P));
  else bad_call(REGS_ARGS(E,P),x);
RETURNS_VOID

BUILTIN2("text is",fixtext_is,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(GET_TAG(b) == T_FIXTEXT ? texts_equal(a,b) : 0))
BUILTIN2("text isnt",fixtext_isnt,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(GET_TAG(b) == T_FIXTEXT ? !texts_equal(a,b) : 1))
BUILTIN1("text size",fixtext_size,C_ANY,o)
  uint64_t x = (uint64_t)o;
  int i = 3;
  int l = 0;
  while (0x7F<<i) {
    i += 7;
    l++;
  }
RETURNS(FIXNUM(l))
#define GET_CHAR()
BUILTIN2("text {}",fixtext_get,C_ANY,o,C_FIXNUM,index)
  char t[20];
  uint64_t c;
  int i = UNFIXNUM(index);
  if (i >= 8) {
bounds_error:
    printf("index out of bounds\n");
    TEXT(P, "{}");
    bad_call(REGS_ARGS(E,P),P);
  }
  c = ((uint64_t)o>>(i*7))&(0x7F<<TAG_BITS);
  if (!c) goto bounds_error;
RETURNS(ADD_TAG(c,T_FIXTEXT))
BUILTIN1("text end",fixtext_end,C_ANY,o)
RETURNS(FIXNUM(1))
BUILTIN1("text hash",fixtext_hash,C_ANY,o)
RETURNS(FIXNUM(((uint64_t)o&(((uint64_t)1<<32)-1))^((uint64_t)o>>32)))
BUILTIN1("text code",fixtext_code,C_ANY,o)
RETURNS(FIXNUM((uint64_t)o>>TAG_BITS))
BUILTIN_HANDLER("text",fixtext,C_TEXT,x)
  STORE(E, 0, P);
  if (texts_equal(x,s_size)) return b_fixtext_size(REGS_ARGS(E,P));
  else if (texts_equal(x,s_get)) return b_fixtext_get(REGS_ARGS(E,P));
  else if (texts_equal(x,s_is)) return b_fixtext_is(REGS_ARGS(E,P));
  else if (texts_equal(x,s_isnt)) return b_fixtext_isnt(REGS_ARGS(E,P));
  else if (texts_equal(x,s_end)) return b_fixtext_end(REGS_ARGS(E,P));
  else if (texts_equal(x,s_hash)) return b_fixtext_hash(REGS_ARGS(E,P));
  else if (texts_equal(x,s_code)) return b_fixtext_code(REGS_ARGS(E,P));
  else bad_call(REGS_ARGS(E,P),x);
RETURNS_VOID


#define VIEW(dst,base,start,size) \
  ALLOC(dst, b_view, (sizeof(void*) < 8 ? 3 : 2)); \
  STORE(dst, 0, base); \
  REF4(dst,sizeof(void*)/4) = (uint32_t)(start); \
  REF4(dst,sizeof(void*)/4+1) = (uint32_t)(size);
#define VIEW_START(o) REF4(o,sizeof(void*)/4)
#define VIEW_SIZE(o) REF4(o,sizeof(void*)/4+1)
#define VIEW_REF(o,start,i) *((void**)REF(o,0) + start + (i))

BUILTIN2("view is",view_is,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a == b))
BUILTIN2("view isnt",view_isnt,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a != b))
BUILTIN1("view size",view_size,C_ANY,o)
RETURNS((uintptr_t)VIEW_SIZE(o))
BUILTIN2("view {}",view_get,C_ANY,o,C_FIXNUM,index)
  uint32_t start = VIEW_START(o);
  uint32_t size = VIEW_SIZE(o);
  if (size <= (uint32_t)(uintptr_t)index) {
    printf("index out of bounds\n");
    TEXT(R, "{}");
    bad_call(REGS_ARGS(E,P),R);
  }
RETURNS(VIEW_REF(o, start, UNFIXNUM(index)))
BUILTIN3("view {!}",view_set,C_ANY,o,C_FIXNUM,index,C_ANY,value)
  uint32_t start = VIEW_START(o);
  uint32_t size = VIEW_SIZE(o);
  if (size <= (uint32_t)(uintptr_t)index) {
    printf("view {!}: index out of bounds\n");
    TEXT(P, "{!}");
    bad_call(REGS_ARGS(E,P),P);
  }
  if (GET_TAG(value) != T_FIXNUM && GET_TAG(value) != T_FIXTEXT) {
  }
  VIEW_REF(o, start, UNFIXNUM(index)) = value;
RETURNS(Void)
BUILTIN1("view end",view_end,C_ANY,o)
RETURNS(FIXNUM(0))
BUILTIN1("view head",view_head,C_ANY,o)
RETURNS(VIEW_REF(o, VIEW_START(o), 0))
BUILTIN1("view tail",view_tail,C_ANY,o)
  uint32_t size = UNFIXNUM(VIEW_SIZE(o));
  if (size == 1) R = Empty;
  else {
    uint32_t start = VIEW_START(o);
    A = o;
    VIEW(R, &VIEW_REF(A,0,0), start+1, FIXNUM(size-1));
  }
RETURN(R)
RETURNS(0)
BUILTIN2("view add",view_add,C_ANY,o,C_ANY,x)
  void *r;
  void **p, **q;
  int size = (int)UNFIXNUM(VIEW_SIZE(o));
  LIST(r, size+1);
  p = &REF(r,0);
  *p++ = x;
  q = &VIEW_REF(o,VIEW_START(o),0);
  while(size-- > 0) *p++ = *q++;
RETURNS(LIST_FLIP(r))
BUILTIN_HANDLER("list",view,C_TEXT,x)
  STORE(E, 0, P);
  if (texts_equal(x,s_get)) return b_view_get(REGS_ARGS(E,P));
  else if (texts_equal(x,s_set)) return b_view_set(REGS_ARGS(E,P));
  else if (texts_equal(x,s_size)) return b_view_size(REGS_ARGS(E,P));
  else if (texts_equal(x,s_is)) return b_view_is(REGS_ARGS(E,P));
  else if (texts_equal(x,s_isnt)) return b_view_isnt(REGS_ARGS(E,P));
  else if (texts_equal(x,s_head)) return b_view_head(REGS_ARGS(E,P));
  else if (texts_equal(x,s_tail)) return b_view_tail(REGS_ARGS(E,P));
  else if (texts_equal(x,s_end)) return b_view_end(REGS_ARGS(E,P));
  else if (texts_equal(x,s_add)) return b_view_add(REGS_ARGS(E,P));
  else bad_call(REGS_ARGS(E,P),x);
RETURNS_VOID

BUILTIN2("list is",list_is,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a == b))
BUILTIN2("list isnt",list_isnt,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a != b))
BUILTIN1("list size",list_size,C_ANY,o)
RETURNS(POOL_HANDLER(P))
BUILTIN2("list {}",list_get,C_ANY,o,C_FIXNUM,index)
  if ((uintptr_t)POOL_HANDLER(o) <= (uintptr_t)index) {
    printf("index out of bounds\n");
    TEXT(P, "{}");
    bad_call(REGS_ARGS(E,P),P);
  }
  o = LIST_FLIP(o);
RETURNS(REF(o, UNFIXNUM(index)))
BUILTIN3("list {!}",list_set,C_ANY,o,C_FIXNUM,index,C_ANY,value)
  if ((uintptr_t)POOL_HANDLER(o) <= (uintptr_t)index) {
    printf("list {!}: index out of bounds\n");
    TEXT(P, "{!}");
    bad_call(REGS_ARGS(E,P),P);
  }
  o = LIST_FLIP(o);
  REF(o, UNFIXNUM(index)) = value;
RETURNS(Void)
BUILTIN1("list end",list_end,C_ANY,o)
RETURNS(FIXNUM(0))
BUILTIN1("list head",list_head,C_ANY,o)
  o = LIST_FLIP(o);
RETURNS(REF(o,0))
BUILTIN1("list tail",list_tail,C_ANY,o)
  intptr_t size = UNFIXNUM(POOL_HANDLER(o));
  if (size == 1) R = Empty;
  else {
    A = o;
    VIEW(R, &REF(LIST_FLIP(A),0), 1, FIXNUM(size-1));
  }
RETURN(R)
RETURNS(0)
BUILTIN2("list add",list_add,C_ANY,o,C_ANY,x)
  void **p, **q;
  intptr_t s = UNFIXNUM(POOL_HANDLER(o));
  A = x;
  LIST(R, s+1);
  p = &REF(R,0);
  *p++ = A;
  o = LIST_FLIP(o);
  q = &REF(o,0);
  while(s-- > 0) *p++ = *q++;
RETURNS(LIST_FLIP(R))
BUILTIN_HANDLER("list",list,C_TEXT,x)
  STORE(E, 0, P);
  if (texts_equal(x,s_get)) return b_list_get(REGS_ARGS(E,P));
  else if (texts_equal(x,s_set)) return b_list_set(REGS_ARGS(E,P));
  else if (texts_equal(x,s_size)) return b_list_size(REGS_ARGS(E,P));
  else if (texts_equal(x,s_is)) return b_list_is(REGS_ARGS(E,P));
  else if (texts_equal(x,s_isnt)) return b_list_isnt(REGS_ARGS(E,P));
  else if (texts_equal(x,s_head)) return b_list_head(REGS_ARGS(E,P));
  else if (texts_equal(x,s_tail)) return b_list_tail(REGS_ARGS(E,P));
  else if (texts_equal(x,s_end)) return b_list_end(REGS_ARGS(E,P));
  else if (texts_equal(x,s_add)) return b_list_add(REGS_ARGS(E,P));
  else bad_call(REGS_ARGS(E,P),x);
RETURNS_VOID

BUILTIN_VARARGS("[list]",make_list)
  int size = (int)UNFIXNUM(NARGS);
  if (size == 0) R = Empty;
  else {
    R = LIST_FLIP(E);
  }
RETURN(R) // have to, otherwise it will endup one environment above
RETURNS(R)

BUILTIN1("integer neg",integer_neg,C_ANY,o)
RETURNS(-(intptr_t)o)
BUILTIN2("integer +",integer_add,C_ANY,a,C_FIXNUM,b)
RETURNS((intptr_t)a + (intptr_t)b)
BUILTIN2("integer -",integer_sub,C_ANY,a,C_FIXNUM,b)
RETURNS((intptr_t)a - (intptr_t)b)
BUILTIN2("integer *",integer_mul,C_ANY,a,C_FIXNUM,b)
RETURNS(UNFIXNUM(a) * (intptr_t)b)
BUILTIN2("integer /",integer_div,C_ANY,a,C_FIXNUM,b)
RETURNS(FIXNUM((intptr_t)a / (intptr_t)b))
BUILTIN2("integer %",integer_rem,C_ANY,a,C_FIXNUM,b)
RETURNS((intptr_t)a % (intptr_t)b)
BUILTIN2("integer is",integer_is,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a == b))
BUILTIN2("integer isnt",integer_isnt,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a != b))
BUILTIN2("integer <",integer_lt,C_ANY,a,C_FIXNUM,b)
RETURNS(FIXNUM((intptr_t)a < (intptr_t)b))
BUILTIN2("integer >",integer_gt,C_ANY,a,C_FIXNUM,b)
RETURNS(FIXNUM((intptr_t)a > (intptr_t)b))
BUILTIN2("integer <<",integer_lte,C_ANY,a,C_FIXNUM,b)
RETURNS(FIXNUM((intptr_t)a <= (intptr_t)b))
BUILTIN2("integer >>",integer_gte,C_ANY,a,C_FIXNUM,b)
RETURNS(FIXNUM((intptr_t)a <= (intptr_t)b))
BUILTIN2("integer mask",integer_mask,C_ANY,a,C_FIXNUM,b)
RETURNS((uintptr_t)a & (uintptr_t)b)
BUILTIN2("integer ior",integer_ior,C_ANY,a,C_FIXNUM,b)
RETURNS((uintptr_t)a | (uintptr_t)b)
BUILTIN2("integer xor",integer_xor,C_ANY,a,C_FIXNUM,b)
RETURNS((uintptr_t)a ^ (uintptr_t)b)
BUILTIN2("integer shl",integer_shl,C_ANY,a,C_FIXNUM,b)
RETURNS((intptr_t)a<<UNFIXNUM(b))
BUILTIN2("integer shr",integer_shr,C_ANY,a,C_FIXNUM,b)
RETURNS(((intptr_t)a>>UNFIXNUM(b))&~(TAG_MASK>>1))
BUILTIN2("integer x",integer_x,C_ANY,size,C_ANY,init)
  void **p;
  intptr_t s = UNFIXNUM(size);
  if (s < 0) {
    TEXT(R,"integer x");
    bad_call(REGS_ARGS(E,P), R);
  } else if (size == 0) {
    R = Empty;
  } else {
    // FIXME: alloc in parent environment
    LIST(R,s);
    p = &REF(R,0);
    while(s-- > 0) *p++ = init;
    R = LIST_FLIP(R);
  }
RETURN(R)
RETURNS(0)
BUILTIN1("integer end",integer_end,C_ANY,o)
RETURNS(FIXNUM(1))
BUILTIN1("integer text",integer_text,C_ANY,o)
RETURNS(ADD_TAG((uint64_t)o&~TAG_MASK,T_FIXTEXT))
BUILTIN1("integer hash",integer_hash,C_ANY,o)
RETURNS(o)
BUILTIN_HANDLER("integer",fixnum,C_TEXT,x)
  STORE(E, 0, P);
  if (x == s_plus) return b_integer_add(REGS_ARGS(E,P));
  else if (x == s_sub) return b_integer_sub(REGS_ARGS(E,P));
  else if (x == s_lt) return b_integer_lt(REGS_ARGS(E,P));
  else if (x == s_gt) return b_integer_gt(REGS_ARGS(E,P));
  else if (x == s_mul) return b_integer_mul(REGS_ARGS(E,P));
  else if (x == s_div) return b_integer_div(REGS_ARGS(E,P));
  else if (x == s_rem) return b_integer_rem(REGS_ARGS(E,P));
  else if (x == s_is) return b_integer_is(REGS_ARGS(E,P));
  else if (x == s_isnt) return b_integer_isnt(REGS_ARGS(E,P));
  else if (x == s_lte) return b_integer_lte(REGS_ARGS(E,P));
  else if (x == s_gte) return b_integer_gte(REGS_ARGS(E,P));
  else if (x == s_neg) return b_integer_neg(REGS_ARGS(E,P));
  else if (x == s_mask) return b_integer_mask(REGS_ARGS(E,P));
  else if (x == s_ior) return b_integer_ior(REGS_ARGS(E,P));
  else if (x == s_xor) return b_integer_xor(REGS_ARGS(E,P));
  else if (x == s_shl) return b_integer_shl(REGS_ARGS(E,P));
  else if (x == s_shr) return b_integer_shr(REGS_ARGS(E,P));
  else if (x == s_end) return b_integer_end(REGS_ARGS(E,P));
  else if (x == s_hash) return b_integer_hash(REGS_ARGS(E,P));
  else if (x == s_text) return b_integer_text(REGS_ARGS(E,P));
  else if (x == s_x) return b_integer_x(REGS_ARGS(E,P));
  else bad_call(REGS_ARGS(E,P),x);
RETURNS_VOID

BUILTIN1("cons head",cons_head,C_ANY,o)
RETURNS(CAR(o))
BUILTIN1("cons tail",cons_tail,C_ANY,o)
RETURNS(CDR(o))
BUILTIN2("cons is",cons_is,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a == b))
BUILTIN2("cons isnt",cons_isnt,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a != b))
BUILTIN1("cons end",cons_end,C_ANY,o)
RETURNS(FIXNUM(0))
BUILTIN2("cons add",cons_add,C_ANY,o,C_ANY,head)
  A = head;
  P = o;
  CONS(A, P);
RETURNS(R)
BUILTIN_HANDLER("list",cons,C_TEXT,x)
  STORE(E, 0, P);
  if (texts_equal(x,s_head)) return b_cons_head(REGS_ARGS(E,P));
  else if (texts_equal(x,s_tail)) return b_cons_tail(REGS_ARGS(E,P));
  else if (texts_equal(x,s_end)) return b_cons_end(REGS_ARGS(E,P));
  else if (texts_equal(x,s_add)) return b_cons_add(REGS_ARGS(E,P));
  else if (texts_equal(x,s_is)) return b_cons_is(REGS_ARGS(E,P));
  else if (texts_equal(x,s_isnt)) return b_cons_isnt(REGS_ARGS(E,P));
  else bad_call(REGS_ARGS(E,P),x);
RETURNS_VOID


BUILTIN2("empty is",empty_is,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a == b))
BUILTIN2("empty isnt",empty_isnt,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a != b))
BUILTIN1("empty end",empty_end,C_ANY,o)
RETURNS(FIXNUM(1))
BUILTIN2("empty add",empty_add,C_ANY,o,C_ANY,head)
  A = head;
  LIST(R, 1);
  STORE(R, 0, A);
RETURNS(LIST_FLIP(R))
BUILTIN_HANDLER("empty",empty,C_TEXT,x)
  STORE(E, 0, P);
  if (texts_equal(x,s_end)) return b_empty_end(REGS_ARGS(E,P));
  else if (texts_equal(x,s_add)) return b_empty_add(REGS_ARGS(E,P));
  else if (texts_equal(x,s_is)) return b_empty_is(REGS_ARGS(E,P));
  else if (texts_equal(x,s_isnt)) return b_empty_isnt(REGS_ARGS(E,P));
  else bad_call(REGS_ARGS(E,P),x);
RETURNS_VOID

BUILTIN1("tag_of",tag_of,C_ANY,o)
  ALLOC(A, FIXNUM(-1), 1); // signal that we want tag
  CALL_TAGGED(R,o,A);
RETURNS(R)

BUILTIN0("halt",halt)
  printf("halted.\n");
  exit(0);
RETURNS_VOID

BUILTIN1("log",log,C_ANY,a)
  fprintf(stderr, "log: %s\n", print_object(a));
RETURN(a)
RETURNS(a)

BUILTIN1("set_error_handler",set_error_handler,C_ANY,h)
  fatal("FIXME: implement set_error_handler\n");
RETURNS(Void)

BUILTIN1("load_file",load_file,C_ANY,path)
  fatal("FIXME: implement load_file\n");
RETURNS(Void)

BUILTIN1("utf8_to_text",utf8_to_text,C_ANY,bytes)
  fatal("FIXME: implement utf8_to_text\n");
RETURNS(Void)

static int is_unicode(char *s) {
  while (*s) {
    if (*(uint8_t*)s & 0x80) return 1;
    s++;
  }
  return 0;
}

static void *text_immediate_encoding(char *s) {
  uint64_t r = 0;
  uint64_t c;
  int i = 3;
  while (*s) {
    if (i >= 64) return 0;
    c = (uint8_t)*s++;
    if (c & 0x80) return 0;
    r |= c << i;
    i += 7;
  }
  return ADD_TAG(r,T_FIXTEXT);
}

static int text_immediate_decode(char *dst, void *r) {
  uint8_t *p = (uint8_t*)dst;
  uint64_t x = (uint64_t)r;
  uint64_t c;
  int i = 3;
  while (i < 64) {
    c = (x>>i) & 0x7f;
    i += 7;
    if (!c) break;
    *p++ = c;
  }
  *p = 0;
  return p-(uint8_t*)dst;
}

static void *alloc_text(api_t *api, char *s) {
  int l, a;
  void *r;
  char buf[1024];

  if (is_unicode(s)) fatal("FIXME: implement unicode\n");

  r = text_immediate_encoding(s);
  if (r) return r;

  l = strlen(s);
  a = (l+4+TAG_MASK)>>TAG_BITS;
  ALLOC(r, b_text, a);
  REF4(r,0) = (uint32_t)FIXNUM(l);
  memcpy(&REF1(r,4), s, l);
  return r;
}


static char *read_whole_file_as_string(char *input_file_name) {
  char *file_contents;
  long input_file_size;
  FILE *input_file = fopen(input_file_name, "rb");
  if (!input_file) return 0;
  fseek(input_file, 0, SEEK_END);
  input_file_size = ftell(input_file);
  rewind(input_file);
  file_contents = malloc(input_file_size + 1);
  file_contents[input_file_size] = 0;
  fread(file_contents, sizeof(char), input_file_size, input_file);
  fclose(input_file);
  return file_contents;
}

BUILTIN1("read_file_as_text",read_file_as_text,C_TEXT,filename_text)
  char *filename = text_to_cstring(filename_text);
  char *contents = read_whole_file_as_string(filename);
  if (contents) {
    TEXT(R, contents);
    free(contents);
  } else {
    R = Void;
  }
RETURNS(R)

BUILTIN2("_apply",_apply,C_ANY,f,C_ANY,args)
  // NOTE: no typecheck, because this function should be hidden from user
  //       intended use is the fast re-apply in handlers
  args = LIST_FLIP(args);
  MOVE(E, args);
  CALL(R,f,E);
RETURNS(R)

BUILTIN1("_no_method",_no_method,C_TEXT,name)
  printf("method not found: %s\n", print_object(name));
  STORE(E, 0, P);
  bad_call(REGS_ARGS(E,P), name);
  abort();
RETURNS(Void)


static struct {
  char *name;
  void *fun;
} builtins[] = {
  {"tag_of", b_tag_of},
  {"halt", b_halt},
  {"log", b_log},
  {"list", b_make_list},
  {"_apply", b__apply},
  {"_no_method", b__no_method},
  {"read_file_as_text", b_read_file_as_text},
  //{"save_string_as_file", b_save_text_as_file},
  {0, 0}
};

BUILTIN1("host",host,C_TEXT,name)
  int i;
  for (i = 0; ; i++) {
    if (!builtins[i].name) {
      fatal("host doesn't provide `%s`\n", print_object(name));
    }
    if (texts_equal(builtins[i].name, name)) {
      R = builtins[i].fun;
      break;
    }
  }
RETURNS(R)

static char *print_object_r(api_t *api, char *out, void *o) {
  int i;
  int tag = GET_TAG(o);

  if (o == Empty) {
    out += sprintf(out, "()");
  } else if (o == Void) {
    out += sprintf(out, "Void");
  } else if (tag == T_CLOSURE) {
    pfun handler = POOL_HANDLER(o);
    if (IS_ARGLIST(o)) {
      out += sprintf(out, "#(arglist %p)", o);
    } else if (handler == b_view) {
      uint32_t start = VIEW_START(o);
      int size = (int)UNFIXNUM(VIEW_SIZE(o));
      out += sprintf(out, "(");
      for (i = 0; i < size; i++) {
        if (i) out += sprintf(out, " ");
        out = print_object_r(api, out, VIEW_REF(o,start,i));
      }
      out += sprintf(out, ")");
    } else if (handler == b_text) {
      int size = (int)TEXT_SIZE(o);
      char *p = TEXT_DATA(o);
      while (size-- > 0) *out++ = *p++;
      *out = 0;
    } else if (handler == b_cons) {
      out += sprintf(out, "(");
      for (;;) {
        out = print_object_r(api, out, CAR(o));
        o = CDR(o);
        if (o == Empty) break;
        out += sprintf(out, " ");
      }
      out += sprintf(out, ")");
    } else {
      //FIXME: check metainfo to see if this object has associated print routine
      out += sprintf(out, "#(closure %p %p)", handler, o);
    }
  } else if (tag == T_FIXNUM) {
    // FIXME: this relies on the fact that shift preserves sign
    out += sprintf(out, "%ld", (intptr_t)o>>TAG_BITS);
  } else if (tag == T_LIST) {
    int size = (int)UNFIXNUM(POOL_HANDLER(o));
    o = LIST_FLIP(o);
    out += sprintf(out, "(");
    for (i = 0; i < size; i++) {
      if (i) out += sprintf(out, " ");
      out = print_object_r(api, out, REF(o,i));
    }
    out += sprintf(out, ")");
  } else if (tag == T_FIXTEXT) {
    out += text_immediate_decode(out, o);
  } else {
    out += sprintf(out, "#(ufo %d %p)", tag, o);
  }
  return out;
}

static void bad_tag(REGS) {
  fatal("bad tag = %d\n", (int)GET_TAG(P));
}

static void *handle_args(REGS, intptr_t expected, intptr_t size, void *tag, void *meta) {
  intptr_t got = NARGS;

  if (got == FIXNUM(-1)) { //request for tag
    RETURN_NO_GC(tag);
  } else if (got == FIXNUM(-2)) {
    RETURN_NO_GC(size)
  } else if (got == FIXNUM(-3)) {
    RETURN_NO_GC(meta);
  }

  if (meta != Empty) {
  }
  if (UNFIXNUM(expected) < 0) {
    printf("bad number of arguments: got %ld, expected at least %ld\n",
       UNFIXNUM(got)-1, -UNFIXNUM(expected)-1);
  } else {
    printf("bad number of arguments: got %ld, expected %ld\n", UNFIXNUM(got)-1, UNFIXNUM(expected)-1);
  }
  fatal("during call to `%s`\n", print_object(tag));
}


static void *closure_size_request;

#define NEEDS_GC(o) (gc_base <= (void*)(o) && (void*)(o) < gc_end)

#define OBJECT_HEAP(o) ((void*)&apis[1] <= (void*)(o) && (void*)(o) < (void*)&apis[2])

static void *gc(api_t *api, void *gc_base, void *gc_end, void *o) {
  void *p, *q;
  int i, size, tag = GET_TAG(o);
  void *E, *P, *A, *C, *R; // dummies
  char buf[1024];

  //sprintf(buf, "%s", print_object(o));
  //fprintf(stderr, "%p: %s\n", o, print_object(o));

  if (IMMEDIATE(o)) {
    p = o;
  } else if (!NEEDS_GC(o)) {
    // FIXME: validate this external reference (safe-check we haven't damaged anything)
    //fprintf(stderr, "external: %p\n", o);
    p = o;
  } else if (GET_TAG(o) == T_CLOSURE) {
    pfun handler = POOL_HANDLER(o);
    int in_heap = HEAP_OBJECT(handler);

    if (in_heap && !NEEDS_GC(o)) {
      // already moved
      p = handler;
    } else if (IS_ARGLIST(o)) {
      size = (int)UNFIXNUM(handler);
      LIST(p, size);
      STORE(o, -1, p);
      for (i = 0; i < size; i++) {
        q = REF(o,i);
        q = gc(api, gc_base, gc_end, q);
        STORE(p, i, q);
      }
    } else if (handler == b_view) {
      uint32_t start = VIEW_START(o);
      uint32_t size = VIEW_SIZE(o);
      VIEW(p, 0, start, size);
      STORE(o, -1, p);
      q = ADD_TAG(&VIEW_REF(o,0,0), T_LIST);
      q = gc(api, gc_base, gc_end, q);
      q = LIST_FLIP(q);
      STORE(p, 0, &REF(q, 0));
    } else if (handler == b_text) {
      TEXT(p, TEXT_DATA(o));
      STORE(o, -1, p);
    } else if (handler == b_cons) {
      CONS(0, 0);
      p = R;
      CAR(p) = gc(api, gc_base, gc_end, CAR(o));
      CDR(p) = gc(api, gc_base, gc_end, CDR(o));
    } else {
      void *fixed_size;
      CALL(fixed_size,o,closure_size_request);
      size = UNFIXNUM(fixed_size);
      ALLOC(p, handler, size);
      STORE(o, -1, p);
      for (i = 0; i < size; i++) {
        STORE(p, i, gc(api, gc_base, gc_end, REF(o,i)));
      }
    }
  } else if (GET_TAG(o) == T_LIST) {
    o = LIST_FLIP(o);
    p = POOL_HANDLER(o);
    if (IS_ARGLIST(o)) { // still wasn't moved?
      size = (int)UNFIXNUM(p);
      LIST(p, size);
      STORE(o, -1, LIST_FLIP(p));
      for (i = 0; i < size; i++) {
        q = gc(api, gc_base, gc_end, REF(o,i));
        STORE(p, i, q);
      }
      p = LIST_FLIP(p);
    }
  } else {
    printf("cant gc #(ufo %d %p)\n", (int)GET_TAG(o), o);
    abort();
  }

  //fprintf(stderr, "%s: %p -> %p (%ld)\n", ""/*buf*/, o, p, (intptr_t)(p-o));
  return p;
}


static void *gc_entry(api_t *api, void *gc_base, void *gc_end, void *o) {
  int i;
  api_t *other = api->other;
  void *xs = LIFTS_LIST(gc_end);
  if (xs) {
    void *ys = LIFTS_LIST(Base);
    while (xs) {
      void **x = (void**)LIFTS_HEAD(xs);
      *x = gc(api, gc_base, gc_end, *x);
      if (ON_CURRENT_LEVEL(x)) {
        // object got lifted to the level of it's holder
        //fprintf(stderr, "lifted!\n");
      } else { // needs future lifting
        LIFTS_CONS(ys, x, ys);
      }
      xs = LIFTS_TAIL(xs);
    }
    LIFTS_LIST(Base) = ys;
  }

  return gc(api, gc_base, gc_end, o);
}

static api_t *init_api(void *ptr) {
  int i;
  api_t *api = (api_t*)ptr;

  api->bad_tag = bad_tag;
  api->handle_args = handle_args;
  api->print_object_f = print_object_f;
  api->gc = gc_entry;
  api->alloc_text = alloc_text;
  api->fixnum = b_fixnum;
  api->list = b_list;
  api->fixtext = b_fixtext;
  
  return api;
}

#define CLOSURE(dst,code) { ALLOC(dst, code, 0); }
int main(int argc, char **argv) {
  int i;
  char *module;
  void *lib;
  pfun entry, setup;
  api_t *api;
  void *R;

  void *E = 0; // current environment
  void *P = 0; // parent environment

  if (argc != 2) {
    printf("usage: %s <start_module>\n", argv[0]);
    abort();
  }

  module = argv[1];

  api = init_api(apis);
  api->other = init_api(apis+1);
  api->other->other = api;

  api->base = api->top = api->heap+HEAP_SIZE-BASE_HEAD_SIZE;
  api->other->base = api->other->top = api->other->heap+HEAP_SIZE-BASE_HEAD_SIZE;

  api->level = 0;
  api->other->level = 1;

  CLOSURE(Void, b_void);
  CLOSURE(Empty, b_empty);
  CLOSURE(Host, b_host);

  api->other->void_ = api->void_;
  api->other->empty_ = api->empty_;
  api->other->host_ = api->host_;

  ALLOC(closure_size_request, FIXNUM(-2), 1); // signal that we want closure size

  ALLOC(E,FIXNUM(0),0);

  for (i = 0; ; i++) {
    void *t;
    if (!builtins[i].name) break;
    TEXT(builtins[i].name, builtins[i].name);
    CLOSURE(t, builtins[i].fun);
    builtins[i].fun = t;
  }

  TEXT(s_neg, "neg");
  TEXT(s_plus, "+");
  TEXT(s_sub, "-");
  TEXT(s_mul, "*");
  TEXT(s_div, "/");
  TEXT(s_rem, "%");
  TEXT(s_is, "is");
  TEXT(s_isnt, "isnt");
  TEXT(s_lt, "<");
  TEXT(s_gt, ">");
  TEXT(s_lte, "<<");
  TEXT(s_gte, ">>");

  TEXT(s_mask, "mask");
  TEXT(s_ior, "ior");
  TEXT(s_xor, "xor");
  TEXT(s_shl, "shl");
  TEXT(s_shr, "shr");

  TEXT(s_head, "head");
  TEXT(s_tail, "tail");
  TEXT(s_add, "add");
  TEXT(s_end, "end");

  TEXT(s_size, "size");
  TEXT(s_get, "{}");
  TEXT(s_set, "{!}");
  TEXT(s_hash, "hash");
  TEXT(s_code, "code");
  TEXT(s_text, "text");

  TEXT(s_x, "x");

  lib = dlopen(module, RTLD_LAZY);
  if (!lib) fatal("dlopen couldnt load %s\n", module);

  entry = (pfun)dlsym(lib, "entry");
  if (!entry) fatal("dlsym couldnt find symbol `entry` in %s\n", module);

  setup = (pfun)dlsym(lib, "setup");
  if (!setup) fatal("dlsym couldnt find symbol `setup` in %s\n", module);

  Base = Top;
  HEAP_FLIP();
  R = setup(REGS_ARGS(E,P)); // init module's statics
  HEAP_FLIP();
  Base = Top;
  HEAP_FLIP();
  R = entry(REGS_ARGS(E,P)); 
  HEAP_FLIP();

  printf("%s\n", print_object(R));

  return 0;
}
