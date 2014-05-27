#include <dlfcn.h>

#include "runtime.h"

static void b_list(regs_t*);
static void b_view(regs_t*);
static void b_text(regs_t*);
static void b_cons(regs_t*);

#define getArg(i) REF(E,i)
#define getVal(x) ((uintptr_t)(x)&~TAG_MASK)


static void *heap_base[HEAP_SIZE+POOL_SIZE];
static void *heap_tags[HEAP_SIZE/4];
static void **heap_ptr;
static void **heap_end;
static int pools_count = 0;

#define LIST_POOL (POOL_SIZE+0)
#define META_POOL (POOL_SIZE+1)
#define CONS_POOL (POOL_SIZE+2)
#define TEXT_POOL (POOL_SIZE+3)
#define VIEW_POOL (POOL_SIZE+4)

static void **alloc(int count) {
  void **r = heap_ptr;
  heap_ptr += (count+POOL_SIZE-1)&~(POOL_SIZE-1);
  if ((void**)heap_ptr > heap_end) {
    printf("FIXME: can't alloc %d cells, implement GC\n", count);
    abort();
  }
  return r;
}

static int new_pool(regs_t *regs) {
  return pools_count++;
}

static void bad_type(regs_t *regs, char *expected, int arg_index, char *name) {
  int i, nargs = (int)UNFIXNUM(NARGS);
  printf("arg %d isnt %s, in: %s", arg_index, expected, name);
  for (i = 1; i < nargs; i++) printf(" %s", print_object(getArg(i)));
  printf("\n");
  abort();
}

static void bad_call(regs_t *regs, void *method) {
  int i, nargs = (int)UNFIXNUM(NARGS);
  printf("bad call: %s", print_object(getArg(1)));
  printf(" %s", print_object(method));
  for (i = 2; i < nargs; i++) printf(" %s", print_object(getArg(i)));
  printf("\n");
  abort();
}



#define CAR(x) ((void**)getVal(x))[0]
#define CDR(x) ((void**)getVal(x))[1]
#define CONS(dst,a,b) \
  ALLOC(T, b_cons, CONS_POOL, 2); \
  STORE(T, 0, a); \
  STORE(T, 1, b); \
  MOVE(dst, T);

static char *print_object_r(regs_t *regs, char *out, void *o);

// FIXME: use heap instead
static char print_buffer[1024*1024*2];
char* print_object_f(regs_t *regs, void *object) {
  print_object_r(regs, print_buffer, object);
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
    bad_type(regs, "integer", arg_index, meta)

#define C_TEXT(o,arg_index,meta) \
  if (GET_TAG(o) != T_CLOSURE || POOL_HANDLER(o) != b_text) \
    bad_type(regs, "text", arg_index, meta)

#define C_CONS(o,arg_index,meta) \
  if (GET_TAG(o) != T_CLOSURE || POOL_HANDLER(o) != b_cons) \
    bad_type(regs, "cons", arg_index, meta)

#define BUILTIN_CHECK_NARGS(expected,tag) \
  if (NARGS != FIXNUM(expected)) { \
    static void *stag = 0; \
    if (!stag) TEXT(stag, tag); \
    regs->handle_args(regs, FIXNUM(expected), stag, Empty); \
    return; \
  }
#define BUILTIN_CHECK_VARARGS(expected,tag) \
  if (NARGS < FIXNUM(expected)) { \
    static void *stag = 0; \
    if (!stag) TEXT(stag, tag); \
    regs->handle_args(regs, FIXNUM(-1), stag, Empty); \
    return; \
  }

#define CALL0(f,k) \
  LIST(E, 1); \
  STORE(E, 0, k); \
  CALL(f);

#define CALL1(f,k,a) \
  LIST(E, 2); \
  STORE(E, 0, k); \
  STORE(E, 1, a); \
  CALL(f);

#define CALL2(f,k,a,b) \
  LIST(E, 3); \
  STORE(E, 0, k); \
  STORE(E, 1, a); \
  STORE(E, 2, b); \
  CALL(f);

#define BUILTIN0(sname, name) \
  static void b_##name(regs_t *regs) { \
  void *k; \
  BUILTIN_CHECK_NARGS(1,sname); \
  k = getArg(0);
#define BUILTIN1(sname,name,a_check,a) \
  static void b_##name(regs_t *regs) { \
  void *k, *a; \
  BUILTIN_CHECK_NARGS(2,sname); \
  k = getArg(0); \
  a = getArg(1); \
  a_check(a, 0, sname);
#define BUILTIN2(sname,name,a_check,a,b_check,b) \
  static void b_##name(regs_t *regs) { \
  void *k, *a, *b; \
  BUILTIN_CHECK_NARGS(3,sname); \
  k = getArg(0); \
  a = getArg(1); \
  a_check(a, 0, sname); \
  b = getArg(2); \
  b_check(b, 1, sname);
#define BUILTIN3(sname,name,a_check,a,b_check,b,c_check,c) \
  static void b_##name(regs_t *regs) { \
  void *k, *a, *b,*c; \
  BUILTIN_CHECK_NARGS(4,sname); \
  k = getArg(0); \
  a = getArg(1); \
  a_check(a, 0, sname); \
  b = getArg(2); \
  b_check(b, 1, sname); \
  c = getArg(3); \
  c_check(c, 1, sname);
#define BUILTIN_VARARGS(sname,name) \
  static void b_##name(regs_t *regs) { \
  void *k; \
  BUILTIN_CHECK_VARARGS(1,sname); \
  k = getArg(0);
#define BUILTIN_HANDLER(sname,name,a_check,a) \
  static void b_##name(regs_t *regs) { \
  void *a; \
  BUILTIN_CHECK_VARARGS(2,sname); \
  a = getArg(1); \
  a_check(a, 0, sname);


#define RETURNS(r) CALL0(k,(void*)(r)); }
#define RETURNS_VOID }

// E[0] = environment, E[1] = continuation, E[2] = function_name
// run continuation recieves entry point into user specified program
// which it runs with supplied host resolver, which resolves all builtin symbols
BUILTIN0("run",run)
  CALL1(k,fin,host);
RETURNS_VOID

BUILTIN0("fin",fin)
  T = k;
RETURNS_VOID


static void *s_size, *s_get, *s_set, *s_hash;
static void *s_neg, *s_plus, *s_sub, *s_mul, *s_div, *s_rem, *s_is, *s_isnt, *s_lt, *s_gt, *s_lte, *s_gte;
static void *s_mask, *s_ior, *s_xor; //NOTE: ~X can be implemented as X^0xFFFFFFFF
static void *s_shl, *s_shr;
static void *s_head, *s_tail, *s_add, *s_end;
static void *s_x; //duplicate

static int texts_equal(void *a, void *b) {
  uint32_t al = REF4(a,0);
  uint32_t bl = REF4(b,0);
  return al == bl && !memcmp(&REF1(a,4), &REF1(b,4), UNFIXNUM(al));
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
  STORE(E, 1, P);
  if (texts_equal(x,s_is)) b_void_is(regs);
  else if (texts_equal(x,s_isnt)) b_void_isnt(regs);
  else if (texts_equal(x,s_end)) b_void_end(regs);
  else if (texts_equal(x,s_get)) b_void_get(regs);
  else bad_call(regs,x);
RETURNS_VOID

#define IS_TEXT(x) (GET_TAG(x) == T_CLOSURE && POOL_HANDLER(x) == b_text)
BUILTIN2("text is",text_is,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(IS_TEXT(b) ? texts_equal(a,b) : 0))
BUILTIN2("text isnt",text_isnt,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(IS_TEXT(b) ? !texts_equal(a,b) : 1))
BUILTIN1("text size",text_size,C_ANY,o)
RETURNS((uintptr_t)*(uint32_t*)o)
BUILTIN2("text {}",text_get,C_ANY,o,C_FIXNUM,index)
  void *r;
  char t[2];
  if ((uintptr_t)*(uint32_t*)o <= (uintptr_t)index) {
    printf("index out of bounds\n");
    TEXT(P, "{}");
    bad_call(regs,P);
  }
  t[0] = *((char*)o + 4 + UNFIXNUM(index));
  t[1] = 0;
  TEXT(r,t);
RETURNS(r)
BUILTIN1("text end",text_end,C_ANY,o)
RETURNS(FIXNUM(1))
BUILTIN1("text hash",text_hash,C_ANY,o)
RETURNS(FIXNUM(hash((uint8_t*)o+4, *(int32_t*)o)))
BUILTIN_HANDLER("text",text,C_TEXT,x)
  STORE(E, 1, P);
  if (texts_equal(x,s_size)) b_text_size(regs);
  else if (texts_equal(x,s_get)) b_text_get(regs);
  else if (texts_equal(x,s_is)) b_text_is(regs);
  else if (texts_equal(x,s_isnt)) b_text_isnt(regs);
  else if (texts_equal(x,s_end)) b_text_end(regs);
  else if (texts_equal(x,s_hash)) b_text_hash(regs);
  else bad_call(regs,x);
RETURNS_VOID



#define VIEW(dst,o,start,size) \
  ALLOC(dst,b_view,VIEW_POOL, (sizeof(void*) < 8 ? 3 : 2)); \
  STORE(dst, 0, o); \
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
    TEXT(P, "{}");
    bad_call(regs,P);
  }
RETURNS(VIEW_REF(o, start, UNFIXNUM(index)))
BUILTIN3("view {!}",view_set,C_ANY,o,C_FIXNUM,index,C_ANY,value)
  uint32_t start = VIEW_START(o);
  uint32_t size = VIEW_SIZE(o);
  if (size <= (uint32_t)(uintptr_t)index) {
    printf("view {!}: index out of bounds\n");
    TEXT(P, "{!}");
    bad_call(regs,P);
  }
  VIEW_REF(o, start, UNFIXNUM(index)) = value;
RETURNS(Void)
BUILTIN1("view end",view_end,C_ANY,o)
RETURNS(FIXNUM(0))
BUILTIN1("view head",view_head,C_ANY,o)
RETURNS(VIEW_REF(o, VIEW_START(o), 0))
BUILTIN1("view tail",view_tail,C_ANY,o)
  void *r;
  uint32_t size = UNFIXNUM(VIEW_SIZE(o));
  if (size == 1) r = Empty;
  else {
    uint32_t start = VIEW_START(o);
    VIEW(r, &VIEW_REF(o,start,0), start+1, FIXNUM(size-1));
  }
RETURNS(r)
BUILTIN2("view add",view_add,C_ANY,o,C_ANY,x)
  void *r;
  void **p, **q;
  int size = (int)UNFIXNUM(VIEW_SIZE(o));
  LIST(r, size+1);
  p = &REF(r,0);
  *p++ = x;
  q = &VIEW_REF(o,VIEW_START(o),0);
  while(size-- > 0) *p++ = *q++;
RETURNS(r)
BUILTIN_HANDLER("list",view,C_TEXT,x)
  STORE(E, 1, P);
  if (texts_equal(x,s_get)) b_view_get(regs);
  else if (texts_equal(x,s_set)) b_view_set(regs);
  else if (texts_equal(x,s_size)) b_view_size(regs);
  else if (texts_equal(x,s_is)) b_view_is(regs);
  else if (texts_equal(x,s_isnt)) b_view_isnt(regs);
  else if (texts_equal(x,s_head)) b_view_head(regs);
  else if (texts_equal(x,s_tail)) b_view_tail(regs);
  else if (texts_equal(x,s_end)) b_view_end(regs);
  else if (texts_equal(x,s_add)) b_view_add(regs);
  else bad_call(regs,x);
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
    bad_call(regs,P);
  }
RETURNS(REF(o, UNFIXNUM(index)))
BUILTIN3("list {!}",list_set,C_ANY,o,C_FIXNUM,index,C_ANY,value)
  if ((uintptr_t)POOL_HANDLER(o) <= (uintptr_t)index) {
    printf("list {!}: index out of bounds\n");
    TEXT(P, "{!}");
    bad_call(regs,P);
  }
  REF(o, UNFIXNUM(index)) = value;
RETURNS(Void)
BUILTIN1("list end",list_end,C_ANY,o)
RETURNS(FIXNUM(0))
BUILTIN1("list head",list_head,C_ANY,o)
RETURNS(*(void**)o)
BUILTIN1("list tail",list_tail,C_ANY,o)
  void *r;
  intptr_t size = UNFIXNUM(POOL_HANDLER(o));
  if (size == 1) r = Empty;
  else {
    VIEW(r, &REF(o,0), 1, FIXNUM(size-1));
  }
RETURNS(r)
BUILTIN2("list add",list_add,C_ANY,o,C_ANY,x)
  void *r;
  void **p, **q;
  intptr_t s = UNFIXNUM(POOL_HANDLER(o));
  LIST(r, s+1);
  p = &REF(r,0);
  *p++ = x;
  q = &REF(o,0);
  while(s-- > 0) *p++ = *q++;
RETURNS(r)
BUILTIN_HANDLER("list",list,C_TEXT,x)
  STORE(E, 1, P);
  if (texts_equal(x,s_get)) b_list_get(regs);
  else if (texts_equal(x,s_set)) b_list_set(regs);
  else if (texts_equal(x,s_size)) b_list_size(regs);
  else if (texts_equal(x,s_is)) b_list_is(regs);
  else if (texts_equal(x,s_isnt)) b_list_isnt(regs);
  else if (texts_equal(x,s_head)) b_list_head(regs);
  else if (texts_equal(x,s_tail)) b_list_tail(regs);
  else if (texts_equal(x,s_end)) b_list_end(regs);
  else if (texts_equal(x,s_add)) b_list_add(regs);
  else bad_call(regs,x);
RETURNS_VOID

BUILTIN_VARARGS("list new",make_list)
  void *r;
  void **p, **q;
  int size = (int)UNFIXNUM(NARGS)-1;
  int i;
  if (size == 0) r = Empty;
  else {
    LIST(r, size);
    p = &REF(r,0);
    q = &REF(E,1);
    while (size-- > 0) *p++ = *q++;
  }
RETURNS(r)

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
  void *r;
  void **p;
  intptr_t s = UNFIXNUM(size);
  if (s < 0) {
    TEXT(R,"integer x");
    bad_call(regs, R);
  } else if (s == 0) {
    r = Empty;
  } else {
    LIST(r,s);
    p = (void**)r;
    while(s-- > 0) *p++ = init;
  }
RETURNS(r)
BUILTIN1("integer end",integer_end,C_ANY,o)
RETURNS(FIXNUM(1))
BUILTIN1("integer hash",integer_hash,C_ANY,o)
RETURNS(o)
BUILTIN_HANDLER("integer",fixnum,C_TEXT,x)
  STORE(E, 1, P);
  if (texts_equal(x,s_plus)) b_integer_add(regs);
  else if (texts_equal(x,s_sub)) b_integer_sub(regs);
  else if (texts_equal(x,s_mul)) b_integer_mul(regs);
  else if (texts_equal(x,s_div)) b_integer_div(regs);
  else if (texts_equal(x,s_rem)) b_integer_rem(regs);
  else if (texts_equal(x,s_is)) b_integer_is(regs);
  else if (texts_equal(x,s_isnt)) b_integer_isnt(regs);
  else if (texts_equal(x,s_lt)) b_integer_lt(regs);
  else if (texts_equal(x,s_gt)) b_integer_gt(regs);
  else if (texts_equal(x,s_lte)) b_integer_lte(regs);
  else if (texts_equal(x,s_gte)) b_integer_gte(regs);
  else if (texts_equal(x,s_neg)) b_integer_neg(regs);
  else if (texts_equal(x,s_end)) b_integer_end(regs);
  else if (texts_equal(x,s_mask)) b_integer_mask(regs);
  else if (texts_equal(x,s_ior)) b_integer_ior(regs);
  else if (texts_equal(x,s_xor)) b_integer_xor(regs);
  else if (texts_equal(x,s_shl)) b_integer_shl(regs);
  else if (texts_equal(x,s_shr)) b_integer_shr(regs);
  else if (texts_equal(x,s_hash)) b_integer_hash(regs);
  else if (texts_equal(x,s_x)) b_integer_x(regs);
  else bad_call(regs,x);
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
  void *r;
  CONS(r, head, o);
RETURNS(r)
BUILTIN_HANDLER("list",cons,C_TEXT,x)
  STORE(E, 1, P);
  if (texts_equal(x,s_head)) b_cons_head(regs);
  else if (texts_equal(x,s_tail)) b_cons_tail(regs);
  else if (texts_equal(x,s_end)) b_cons_end(regs);
  else if (texts_equal(x,s_add)) b_cons_add(regs);
  else if (texts_equal(x,s_is)) b_cons_is(regs);
  else if (texts_equal(x,s_isnt)) b_cons_isnt(regs);
  else bad_call(regs,x);
RETURNS_VOID


BUILTIN2("empty is",empty_is,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a == b))
BUILTIN2("empty isnt",empty_isnt,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a != b))
BUILTIN1("empty end",empty_end,C_ANY,o)
RETURNS(FIXNUM(1))
BUILTIN2("empty add",empty_add,C_ANY,o,C_ANY,head)
  void *r;
  CONS(r, head, o);
RETURNS(r)
BUILTIN_HANDLER("empty",empty,C_TEXT,x)
  STORE(E, 1, P);
  if (texts_equal(x,s_end)) b_empty_end(regs);
  else if (texts_equal(x,s_add)) b_empty_add(regs);
  else if (texts_equal(x,s_is)) b_empty_is(regs);
  else if (texts_equal(x,s_isnt)) b_empty_isnt(regs);
  else bad_call(regs,x);
RETURNS_VOID

// FIXME: we can re-use single META_POOL, changing only `k`
BUILTIN1("tag_of",tag_of,C_ANY,a)
  ALLOC(E, FIXNUM(0), META_POOL, 1); // signal that we want meta-info
  STORE(E, 0, k);
  CALL_TAGGED(a);
RETURNS_VOID

BUILTIN0("halt",halt)
  printf("halted.\n");
  exit(0);
RETURNS_VOID

BUILTIN1("log",log,C_ANY,a)
  printf("log: %s\n", print_object(a));
RETURNS(a)

BUILTIN1("set_error_handler",set_error_handler,C_ANY,h)
  printf("FIXME: implement set_error_handler\n");
  abort();
RETURNS(Void)

BUILTIN1("load_file",load_file,C_ANY,path)
  printf("FIXME: implement load_file\n");
  abort();
RETURNS(Void)

BUILTIN1("utf8_to_text",utf8_to_text,C_ANY,bytes)
  printf("FIXME: implement utf8_to_text\n");
  abort();
RETURNS(Void)

BUILTIN1("text_out",text_out,C_TEXT,o)
  int i;
  int l = UNFIXNUM(*(uint32_t*)o);
  char *p = (char*)o + 4;
  for (i = 0; i < l; i++) putchar(p[i]);
  fflush(stdout);
RETURNS(Void)

BUILTIN3("_fn_if",_fn_if,C_ANY,a,C_ANY,b,C_ANY,c)
  LIST(E, 1);
  STORE(E, 0, k);
  if (a != FIXNUM(0)) {
    CALL(b);
  } else {
    CALL(c);
  }
RETURNS_VOID

static int is_unicode(char *s) {
  return 0;
}
// FIXME1: use different pool-descriptors to encode length
// FIXME2: immediate encoding for text:
//         one 7-bit char, then nine 6-bit chars (61 bit in total)
//         7-bit char includes complete ASCII
//         6-bit char includes all letters, all digits `_` and 0 (to indicate EOF)
static void *alloc_text(regs_t *regs, char *s) {
  int l, a;
  void *p;

  if (is_unicode(s)) {
    printf("FIXME: implement unicode\n");
    abort();
  }

  l = strlen(s);
  a = (l+4+TAG_MASK)>>TAG_BITS;
  ALLOC(p, b_text, TEXT_POOL, a);
  *(uint32_t*)((char*)p-1) = (uint32_t)FIXNUM(l);
  memcpy((char*)p+3, s, l);
  return p;
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
  void *r;
  char *filename = text_to_cstring(filename_text);
  char *contents = read_whole_file_as_string(filename);
  if (contents) {
    TEXT(r, contents);
    free(contents);
  } else {
    r = Void;
  }
RETURNS(r)

BUILTIN2("_apply",_apply,C_ANY,f,C_ANY,args)
  // NOTE: no typecheck, because this function should be hidden from user
  //       intended use is fast re-apply in handlers
  MOVE(E, args);
  CALL(f);
RETURNS_VOID

BUILTIN1("_no_method",_no_method,C_TEXT,name)
  printf("method not found: %s\n", print_object(name));
  STORE(E, 1, P);
  bad_call(regs, name);
  abort();
RETURNS(Void)


static struct {
  char *name;
  void *fun;
} builtins[] = {
  {"tag_of", b_tag_of},
  {"halt", b_halt},
  {"log", b_log},
  {"_fn_if", b__fn_if},
  {"list", b_make_list},
  {"_apply", b__apply},
  {"_no_method", b__no_method},
  {"read_file_as_text", b_read_file_as_text},
  //{"save_string_as_file", b_save_text_as_file},
  {0, 0}
};

BUILTIN_VARARGS("host",host)
  int i,j, n = (int)UNFIXNUM(NARGS)-1;
  void *f;

  if (n >= POOL_SIZE-2) {
    printf("host: implement large lists\n");
    abort();
  }

  f = getArg(1);
  LIST(A, n);
  STORE(A, 0, k);
  for (j = 1; j < n; j++) {
    void *name = getArg(j+1);
    for (i = 0; ; i++) {
      if (!builtins[i].name) {
        // FIXME: return void instead
        printf("host doesn't provide `%s`\n", print_object(name));
        abort();
      }
      if (texts_equal(builtins[i].name, name)) {
        break;
      }
    }
    STORE(A, j, builtins[i].fun);
  }
  MOVE(E, A);
  CALL_TAGGED(f);
RETURNS_VOID





static char *print_object_r(regs_t *regs, char *out, void *o) {
  int i;
  int tag = GET_TAG(o);

  if (o == Empty) {
    out += sprintf(out, "()");
  } else if (o == Void) {
    out += sprintf(out, "Void");
  } else if (tag == T_CLOSURE) {
    pfun handler = POOL_HANDLER(o);
    if ((intptr_t)handler < FIXNUM(MAX_LIST_SIZE)) {
      int size = (int)UNFIXNUM(handler);
      out += sprintf(out, "(");
      for (i = 0; i < size; i++) {
        if (i) out += sprintf(out, " ");
        out = print_object_r(regs, out, REF(o,i));
      }
      out += sprintf(out, ")");
    } else if (handler == b_view) {
      uint32_t start = VIEW_START(o);
      int size = (int)UNFIXNUM(VIEW_SIZE(o));
      out += sprintf(out, "(");
      for (i = 0; i < size; i++) {
        if (i) out += sprintf(out, " ");
        out = print_object_r(regs, out, VIEW_REF(o,start,i));
      }
      out += sprintf(out, ")");
    } else if (handler == b_text) {
      int l = UNFIXNUM(*(uint32_t*)o);
      char *p = (char*)o + 4;
      for (i = 0; i < l; i++) *out++ = *p++;
      *out = 0;
    } else if (handler == b_cons) {
      out += sprintf(out, "(");
      for (;;) {
        out = print_object_r(regs, out, CAR(o));
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
  } else {
    out += sprintf(out, "#(ufo %d %p)", tag, o);
  }
  return out;
}

static void bad_tag(regs_t *regs) {
  printf("bad tag = %d\n", (int)GET_TAG(P));
  abort();
}

static void handle_args(regs_t *regs, intptr_t expected, void *tag, void *meta) {
  intptr_t got = NARGS;
  void *k = getArg(0);
  if (got == FIXNUM(0)) { //request for tag
    CALL0(k, tag);
    return;
  } else if (got == FIXNUM(-1)) {
    CALL0(k, meta);
    return;
  }
  if (meta != Empty) {
  }
  printf("bad number of arguments: got=%ld, expected=%ld\n", UNFIXNUM(got)-1, UNFIXNUM(expected)-1);
  printf("during call to `%s`\n", print_object(tag));
  abort();
}

static regs_t *new_regs() {
  int i;
  regs_t *regs = (regs_t*)malloc(sizeof(regs_t));
  memset(regs, 0, sizeof(regs_t));

  regs->bad_tag = bad_tag;
  regs->handle_args = handle_args;
  regs->print_object_f = print_object_f;
  regs->new_pool = new_pool;
  regs->alloc = alloc;
  regs->alloc_text = alloc_text;
  regs->fixnum = b_fixnum;
  regs->list = b_list;
  
  // mark pools as full
  for (i = 0; i < MAX_POOLS; i++) regs->pools[i] = (void*)POOL_MASK;

  return regs;
}

#define CLOSURE(dst,code) \
  { \
    int builtin_pool = regs->new_pool(); \
    ALLOC(dst, code, builtin_pool, 0); \
  }

int main(int argc, char **argv) {
  int i;
  char *module;
  void *lib;
  pfun entry;
  regs_t *regs;

  if (argc != 2) {
    printf("usage: %s <start_module>\n", argv[0]);
    abort();
  }

  module = argv[1];

  heap_ptr = (void**)((uintptr_t)(heap_base + POOL_SIZE) & POOL_BASE);
  heap_end = heap_ptr + HEAP_SIZE;

  regs = new_regs();

  // multi-list pools
  for (i = 0; i < POOL_SIZE; i++) regs->new_pool();

  regs->new_pool(); // list pool
  regs->new_pool(); // meta pool
  regs->new_pool(); // cons pool
  regs->new_pool(); // text pool
  regs->new_pool(); // view pool

  CLOSURE(Void, b_void);
  CLOSURE(Empty, b_empty);

  CLOSURE(run, b_run);
  CLOSURE(fin, b_fin);
  CLOSURE(host, b_host);

  for (i = 0; ; i++) {
    if (!builtins[i].name) break;
    TEXT(builtins[i].name, builtins[i].name);
    CLOSURE(T, builtins[i].fun);
    builtins[i].fun = T;
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

  TEXT(s_x, "x");

  lib = dlopen(module, RTLD_LAZY);
  if (!lib) {
    printf("dlopen couldnt load %s\n", module);
    abort();
  }

  entry = (pfun)dlsym(lib, "entry");
  if (!entry) {
    printf("dlsym couldnt find symbol `entry` in %s\n", module);
    abort();
  }

  entry(regs);

  printf("%s\n", print_object(T));
}
