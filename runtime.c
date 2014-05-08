#include <dlfcn.h>

#include "runtime.h"

#define getArg(i) ((void**)(E))[i]
#define getVal(x) ((uintptr_t)(x)&~TAG_MASK)


#define HEAP_SIZE (1024*1024*32)
#define MAX_ARRAY_SIZE (HEAP_SIZE/2)

static void *heap_base[HEAP_SIZE+POOL_SIZE];
static void *heap_tags[HEAP_SIZE/4];
static void **heap_ptr;
static void **heap_end;
static int pools_count = 0;

#define ARRAY_POOL  (POOL_SIZE+0)
#define META_POOL   (POOL_SIZE+1)
#define LIST_POOL   (POOL_SIZE+2)
#define SYMBOL_POOL (POOL_SIZE+3)

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
  int i, nargs = (int)NARGS;
  printf("arg %d isnt %s, in: %s", arg_index, expected, name);
  for (i = 1; i < nargs; i++) printf(" %s", print_object(getArg(i)));
  printf("\n");
  abort();
}

static void bad_call(regs_t *regs, void *head) {
  int i, nargs = (int)NARGS;
  printf("bad call: %s", print_object(head));
  for (i = 1; i < nargs; i++) printf(" %s", print_object(getArg(i)));
  printf("\n");
  abort();
}


#define CAR(x) ((void**)getVal(x))[0]
#define CDR(x) ((void**)getVal(x))[1]
#define CONS(dst,a,b) \
  ALLOC(T, b_list, LIST_POOL, 2); \
  STORE(T, 0, a); \
  STORE(T, 1, b); \
  MOVE(dst, T);

#define C_ANY(o,arg_index,meta)

#define C_FIXNUM(o,arg_index,meta) \
  if (GET_TAG(o) != T_FIXNUM) \
    bad_type(regs, "integer", arg_index, meta)

#define C_SYMBOL(o,arg_index,meta) \
  if (GET_TAG(o) != T_CLOSURE || POOL_HANDLER(o) != b_symbol) \
    bad_type(regs, "symbol", arg_index, meta)

#define C_LIST(o,arg_index,meta) \
  if (GET_TAG(o) != T_CLOSURE || POOL_HANDLER(o) != b_list) \
    bad_type(regs, "list", arg_index, meta)

#define BUILTIN_CHECK_NARGS(expected,tag) \
  if (NARGS != expected) { \
    static void *stag = 0; \
    if (!stag) SYMBOL(stag, tag); \
    regs->handle_args(regs, (intptr_t)expected, stag, v_empty); \
    return; \
  }
#define BUILTIN_CHECK_NARGS_ABOVE(tag) \
  if (NARGS < 1) { \
    static void *stag = 0; \
    if (!stag) SYMBOL(stag, tag); \
    regs->handle_args(regs, -1, stag, v_empty); \
    return; \
  }


#define CALL0(f,k) \
  ALLOC(E, 1, 1, 1); \
  STORE(E, 0, k); \
  CALL(f);

#define CALL1(f,k,a) \
  ALLOC(E, 2, 2, 2); \
  STORE(E, 0, k); \
  STORE(E, 1, a); \
  CALL(f);

#define CALL2(f,k,a,b) \
  ALLOC(E, 3, 3, 3); \
  STORE(E, 0, k); \
  STORE(E, 1, a); \
  STORE(E, 2, b); \
  CALL(f);

#define BUILTIN0(sname, name)           \
  static void b_##name(regs_t *regs) { \
  void *k; \
  BUILTIN_CHECK_NARGS(1,sname); \
  k = getArg(0);
#define BUILTIN1(sname,name,a_check, a)         \
  static void b_##name(regs_t *regs) { \
  void *k, *a; \
  BUILTIN_CHECK_NARGS(2,sname); \
  k = getArg(0); \
  a = getArg(1); \
  a_check(a, 0, sname);
#define BUILTIN2(sname,name,a_check,a,b_check,b)    \
  static void b_##name(regs_t *regs) { \
  void *k, *a, *b; \
  BUILTIN_CHECK_NARGS(3,sname); \
  k = getArg(0); \
  a = getArg(1); \
  a_check(a, 0, sname); \
  b = getArg(2); \
  b_check(b, 1, sname);
#define BUILTIN3(sname,name,a_check,a,b_check,b,c_check,c)    \
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
#define BUILTIN_VARARGS(sname,name)    \
  static void b_##name(regs_t *regs) { \
  void *k; \
  BUILTIN_CHECK_NARGS_ABOVE(sname); \
  k = getArg(0);

#define RETURNS(r) CALL0(k,(r)); }
#define RETURNS_VOID }

// E[0] = environment, E[1] = continuation, E[2] = function_name
// run continuation recieves entry point into user specified program
// which it runs with supplied host resolver, which resolves all builtin symbols
BUILTIN0("run",run) CALL1(k,fin,host); RETURNS_VOID

BUILTIN0("fin",fin) T = k; RETURNS_VOID

BUILTIN_VARARGS("void",void)
  printf("FIXME: implement `void`\n");
  abort();
RETURNS(0)

static int symbols_equal(void *a, void *b) {
  uint32_t al = *(uint32_t*)a;
  uint32_t bl = *(uint32_t*)b;
  return al == bl && !memcmp((uint8_t*)a+4, (uint8_t*)b+4, al/(1<<TAG_BITS));
}


#define TO_FIXNUM(x) (((uintptr_t)(x)*(1<<TAG_BITS)) + 1)

static void *s_size, *s_get;

BUILTIN_VARARGS("symbol",symbol)
  intptr_t n = NARGS;
  intptr_t l = *(uint32_t*)P;
  void *r;
  if (n == 2) {
    void *op, *a = P;
    op = getArg(1);
    C_SYMBOL(op, 0, "symbol");
    if (symbols_equal(op,s_size)) {
      r = (void*)l;
    } else {
      bad_call(regs,P);
    }
  } else if (n == 3) {
    void *op, *a = P, *b;
    char t[2];
    op = getArg(1);
    C_SYMBOL(op, 0, "symbol");
    b = getArg(2);
    if (symbols_equal(op,s_get)) {
      C_FIXNUM(b, 1, "text_get");
      if (l <= (intptr_t)b) {
         printf("index out of bounds\n");
         bad_call(regs,P);
      }
      t[0] = *((char*)a + 4+ (intptr_t)b/(1<<TAG_BITS));
      t[1] = 0;
      SYMBOL(r,t);
    } else {
      bad_call(regs,P);
    }
  } else {
    bad_call(regs,P);
  }
RETURNS(r)

static void *s_neg, *s_add, *s_sub, *s_mul, *s_div, *s_rem, *s_is, *s_isnt, *s_lt, *s_gt, *s_lte, *s_gte;

BUILTIN_VARARGS("integer",fixnum)
  intptr_t n = NARGS;
  void *r;
  if (n == 3) {
    void *op, *a = P, *b;
    op = getArg(1);
    C_SYMBOL(op, 0, "integer");
    b = getArg(2);
    C_FIXNUM(b, 1, "integer");
    if (symbols_equal(op,s_add)) {
      r = (void*)((intptr_t)a + (intptr_t)b - 1);
    } else if (symbols_equal(op,s_sub)) {
      r = (void*)((intptr_t)a - (intptr_t)b + 1);
    } else if (symbols_equal(op,s_mul)) {
      r = (void*)(((intptr_t)a / (1<<TAG_BITS)) * ((intptr_t)b-1) + 1);
    } else if (symbols_equal(op,s_div)) {
      r = (void*)(TO_FIXNUM((intptr_t)a / ((intptr_t)b-1)));
    } else if (symbols_equal(op,s_rem)) {
      r = (void*)(TO_FIXNUM(((intptr_t)a/(1<<TAG_BITS)) % ((intptr_t)b/(1<<TAG_BITS))));
    } else if (symbols_equal(op,s_is)) {
      r = (void*)(TO_FIXNUM(a == b));
    } else if (symbols_equal(op,s_isnt)) {
      r = (void*)(TO_FIXNUM(a != b));
    } else if (symbols_equal(op,s_lt)) {
      r = (void*)(TO_FIXNUM((intptr_t)a < (intptr_t)b));
    } else if (symbols_equal(op,s_gt)) {
      r = (void*)(TO_FIXNUM((intptr_t)a > (intptr_t)b));
    } else if (symbols_equal(op,s_lte)) {
      r = (void*)(TO_FIXNUM((intptr_t)a <= (intptr_t)b));
    } else if (symbols_equal(op,s_gte)) {
      r = (void*)(TO_FIXNUM((intptr_t)a >= (intptr_t)b));
    } else {
      bad_call(regs,P);
    }
  } else if (n == 2) {
    void *op, *a = P;
    op = getArg(1);
    C_SYMBOL(op, 0, "integer");
    if (symbols_equal(op,s_neg)) {
      r = (void*)((intptr_t)2-(intptr_t)a);
    } else {
      bad_call(regs,P);
    }
  } else {
    bad_call(regs,P);
  }
RETURNS(r)

static void *s_head, *s_tail, *s_headed, *s_end;

BUILTIN_VARARGS("list",list)
  intptr_t n = NARGS;
  void *r;
  if (n == 2) {
    void *op, *a = P;
    op = getArg(1);
    C_SYMBOL(op, 0, "list");
    if (symbols_equal(op,s_head)) {
      r = (void*)(CAR(a));
    } else if (symbols_equal(op,s_tail)) {
      r = (void*)(CDR(a));
    } else if (symbols_equal(op,s_end)) {
      r = (void*)(TO_FIXNUM(a == v_empty));
    } else {
      bad_call(regs,P);
    }
  } else if (n == 3) {
    void *op, *a = P, *b;
    op = getArg(1);
    C_SYMBOL(op, 0, "list");
    b = getArg(2);
    if (symbols_equal(op,s_headed)) {
      CONS(r, b, a);
    } else {
      bad_call(regs,P);
    }
  } else {
    bad_call(regs,P);
  }
RETURNS(r)

// FIXME: we can re-use single META_POOL, changing only `k`
BUILTIN1("tag_of",tag_of,C_ANY,a)
  ALLOC(E, 0, META_POOL, 1); // signal that we want meta-info
  STORE(E, 0, k);
  CALL_TAGGED(a);
RETURNS_VOID

BUILTIN0("halt",halt)
  abort();
RETURNS_VOID

BUILTIN1("dbg",dbg,C_ANY,a)
  printf("%s\n", print_object(a));
RETURNS(a)

BUILTIN1("set_error_handler",set_error_handler,C_ANY,h)
  printf("FIXME: implement set_error_handler\n");
  abort();
RETURNS(v_void)

BUILTIN1("load_file",load_file,C_ANY,path)
  printf("FIXME: implement load_file\n");
  abort();
RETURNS(v_void)

BUILTIN1("utf8_to_text",utf8_to_text,C_ANY,bytes)
  printf("FIXME: implement utf8_to_text\n");
  abort();
RETURNS(v_void)


BUILTIN1("text_out",text_out,C_SYMBOL,o)
  int i;
  int l = *(uint32_t*)o / (1<<TAG_BITS);
  char *p = (char*)o + 4;
  for (i = 0; i < l; i++) putchar(p[i]);
  fflush(stdout);
RETURNS(v_void)



BUILTIN3("_fn_if",_fn_if,C_ANY,a,C_ANY,b,C_ANY,c)
  ALLOC(E, 1, 1, 1);
  STORE(E, 0, k);
  if ((intptr_t)a != 1) {
    CALL(b);
  } else {
    CALL(c);
  }
RETURNS_VOID

static int is_unicode(char *s) {
  return 0;
}
// FIXME1: use different pool-descriptors to encode length
// FIXME2: immediate encoding for symbols:
//         one 7-bit char, then nine 6-bit chars (61 bit in total)
//         7-bit char includes complete ASCII
//         6-bit char includes all letters, all digits `_` and 0 (to indicate EOF)
static void *alloc_symbol(regs_t *regs, char *s) {
  int l, a;
  void *p;

  if (is_unicode(s)) {
    printf("FIXME: implement unicode symbols\n");
    abort();
  }

  l = strlen(s);
  a = (l+4+TAG_MASK)>>TAG_BITS;
  ALLOC(p,b_symbol,SYMBOL_POOL,a);
  *(uint32_t*)p = (uint32_t)TO_FIXNUM(l);
  memcpy(((uint32_t*)p+1), s, l);
  return p;
}

BUILTIN_VARARGS("list",make_list)
  void *xs = v_empty;
  int i = (int)NARGS;
  while (i-- > 1) {
    CONS(xs, getArg(i), xs);
  }
RETURNS(xs)

static struct {
  char *name;
  void *fun;
} builtins[] = {
  {"tag_of", b_tag_of},
  {"_fn_if", b__fn_if},
  {"list", b_make_list},
  {0, 0}
};

BUILTIN_VARARGS("host",host)
  int i,j, n = NARGS-1;
  void *f;

  if (NARGS >= POOL_SIZE-1) {
    printf("host: implement large arrays\n");
    abort();
  }

  f = getArg(1);
  ALLOC(A,(intptr_t)n,n,n);
  STORE(A,0,k);
  for (j = 1; j < n; j++) {
    void *name = getArg(j+1);
    for (i = 0; ; i++) {
      if (!builtins[i].name) {
        // FIXME: return void instead
        printf("host doesn't provide `%s`\n", print_object(name));
        abort();
      }
      if (symbols_equal(builtins[i].name, name)) {
        break;
      }
    }
    STORE(A,j,builtins[i].fun);
  }
  MOVE(E,A);
  CALL_TAGGED(f);
RETURNS_VOID

static char *print_object_r(regs_t *regs, char *out, void *o) {
  int tag = GET_TAG(o);

  if (tag == T_CLOSURE) {
    pfun handler = POOL_HANDLER(o);
    if ((uintptr_t)handler < MAX_ARRAY_SIZE) {
      out += sprintf(out, "$(array %d %p)", (int)(uintptr_t)handler, o);
    } else if (handler == b_symbol) {
      int i;
      int l = *(uint32_t*)o / (1<<TAG_BITS);
      char *p = (char*)o + 4;
      for (i = 0; i < l; i++) *out++ = *p++;
      *out = 0;
    } else if (handler == b_list) {
      out += sprintf(out, "(");
      for (;;) {
        out = print_object_r(regs, out, CAR(o));
        o = CDR(o);
        if (o == v_empty) break;
        out += sprintf(out, " ");
      }
      out += sprintf(out, ")");
    } else if (o == v_empty) {
      out += sprintf(out, "()");
    } else if (o == v_void) {
      out += sprintf(out, "Void");
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

// FIXME: use heap instead
static char print_buffer[1024*16];
char* print_object_f(regs_t *regs, void *object) {
  print_object_r(regs, print_buffer, object);
  return print_buffer;
}

static void handle_args(regs_t *regs, intptr_t expected, void *tag, void *meta) {
  intptr_t got = NARGS;
  void *k = getArg(0);
  if (got == 0) { //request for tag
    CALL0(k, tag);
    return;
  } else if (got == -1) {
    CALL0(k, meta);
    return;
  }
  printf("bad number of arguments: got=%ld, expected=%ld\n", got-1, expected-1);
  if (meta != v_void) {
  }
  abort();
}

static regs_t *new_regs() {
  int i;
  regs_t *regs = (regs_t*)malloc(sizeof(regs_t));
  memset(regs, 0, sizeof(regs_t));

  regs->handle_args = handle_args;
  regs->print_object_f = print_object_f;
  regs->new_pool = new_pool;
  regs->alloc = alloc;
  regs->alloc_symbol = alloc_symbol;
  regs->fixnum = b_fixnum;
  
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

  // multi-array pools
  for (i = 0; i < POOL_SIZE; i++) regs->new_pool();

  regs->new_pool(); // array pool
  regs->new_pool(); // meta pool
  regs->new_pool(); // list pool
  regs->new_pool(); // symbol pool

  CLOSURE(v_void, b_void);
  CLOSURE(v_empty, b_list);

  CLOSURE(run, b_run);
  CLOSURE(fin, b_fin);
  CLOSURE(host, b_host);

  for (i = 0; ; i++) {
    if (!builtins[i].name) break;
    SYMBOL(builtins[i].name, builtins[i].name);
    CLOSURE(builtins[i].fun, builtins[i].fun);
  }

  SYMBOL(s_neg, "neg");
  SYMBOL(s_add, "+");
  SYMBOL(s_sub, "-");
  SYMBOL(s_mul, "*");
  SYMBOL(s_div, "/");
  SYMBOL(s_rem, "%");
  SYMBOL(s_is, "is");
  SYMBOL(s_is, "isnt");
  SYMBOL(s_lt, "<");
  SYMBOL(s_gt, ">");
  SYMBOL(s_lte, "<<");
  SYMBOL(s_gte, ">>");

  SYMBOL(s_head, "head");
  SYMBOL(s_tail, "tail");
  SYMBOL(s_headed, "headed");
  SYMBOL(s_end, "end");

  SYMBOL(s_size, "size");
  SYMBOL(s_get, "get");


  lib = dlopen(module, RTLD_LAZY);
  if (!lib) {
    printf("cant load %s\n", module);
    abort();
  }

  entry = (pfun)dlsym(lib, "entry");
  if (!entry) {
    printf("cant find symbol `entry` in %s\n", module);
    abort();
  }

  entry(regs);

  printf("%s\n", print_object(T));
}
