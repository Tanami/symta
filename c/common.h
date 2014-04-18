#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef void (*pfun)();

#define TAG_BITS ((uintptr_t)2)
#define TAG_MASK (((uintptr_t)1<<TAG_BITS)-1)

#define getArg(N) ((void**)(E))[N]
#define getTag(X) ((uintptr_t)(X)&TAG_MASK)
#define getVal(X) ((uintptr_t)(X)&~TAG_MASK)


#define T_FIXNUM   0
#define T_STRING   1
#define T_PAIR     2
#define T_CLOSURE  3

//#define T_FLOAT
//#define T_NEXT_HERE
//#define T_NEXT_NONE

// make strings and pairs statically allocable
#define ALLOC(dst, size) dst = malloc(size*sizeof(void*))
#define FIXNUM(dst,x) dst = (void*)(((uintptr_t)(x)<<TAG_BITS) | T_FIXNUM)
#define STRING(dst,x) printf("%p\n", x); dst = (void*)((uintptr_t)x | T_STRING)
#define CONS(dst,a,b) \
  ALLOC(T, 2); \
  STORE(T, 0, a); \
  STORE(T, 1, b); \
  dst = (void*)((uintptr_t)T | T_PAIR);
#define CLOSURE(dst,f) \
  ALLOC(T, 1); \
  STORE(T, 0, f); \
  dst = (void*)((uintptr_t)T | T_CLOSURE);
#define CAR(x) ((void**)getVal(x))[0]
#define CDR(x) ((void**)getVal(x))[1]
#define CALL(f) \
  P = (void*)((uintptr_t)f-T_CLOSURE); \
  (((pfun*)P)[0])();
#define STORE(dst,off,src) ((void**)(dst))[(int)(off)] = (void*)(src)
#define LOAD(dst,src,off) dst = ((void**)(src))[(int)(off)]
#define COPY(dst,p,src,q) ((void**)(dst))[(int)(p)] = ((void**)(src))[(int)(q)]
#define MOVE(dst,src) dst = (void*)(src)
#define IOR(dst,a,b) dst = (void*)((uintptr_t)(a)|(uintptr_t)(b))
#define CHECK_TAG(src,expected) if (getTag(src) != expected) bad_tag(getTag(src), expected)
#define CHECK_NARGS(expected,meta) \
  if ((intptr_t)N != (intptr_t)expected) { \
    bad_number_of_arguments((intptr_t)N, (intptr_t)expected, meta); \
    return; \
  }
#define CHECK_NARGS_ABOVE(expected,meta) \
  if ((intptr_t)N >= (intptr_t)expected) { \
    bad_number_of_arguments((intptr_t)N, (intptr_t)expected, meta); \
    return; \
  }

#define BUILTIN_CHECK_NARGS(expected,meta) \
  if ((intptr_t)N != (intptr_t)expected) { \
    builtin_bad_number_of_arguments((intptr_t)N, (intptr_t)expected, meta); \
    return; \
  }
#define BUILTIN_CHECK_NARGS_ABOVE(expected,meta) \
  if ((intptr_t)N < (intptr_t)expected) { \
    builtin_bad_number_of_arguments((intptr_t)N, (intptr_t)expected, meta); \
    return; \
  }


static void bad_tag(int tag, int expected) {
  printf("bad tag=%d, expected tag=%d\n", tag, expected);
  abort();
}

static void bad_number_of_arguments(int got, int expected, void *meta) {
  if (got < 0) { //request for metainfo?
  }
  printf("bad number of arguments: got=%d, expected=%d\n", got, expected);
  if (meta) {
  }
  abort();
}

static void builtin_bad_number_of_arguments(int got, int expected, char *name) {
  if (got < 0) { //request for metainfo?
  }
  printf("bad number of arguments: got=%d, expected=%d\n", got, expected);
  printf("  during invokation of `%s`\n", name);
  abort();
}

static void
  *E, // current environment
  *P, // parent environment
  *A, // args scratchpad
  *C, // code pointer
  *R, // return value
  *T, // temporary, used by CLOSURE, PAIR and other macros
  *N, // number of arguments to the current function (size of E)
  *v_void,
  *v_yes,
  *v_no,
  *v_empty,
  *fin, // the closure, which would recieve evaluation result
  *run, // the closure, which would recieve the resulting program
  *host; // called to resolve builtin functions



#define BUILTIN0(name) \
  static void b_##name() { \
  void *k; \
  BUILTIN_CHECK_NARGS(1,0); \
  k = getArg(0);

#define BUILTIN1(name,a) \
  static void b_##name() { \
  void *k, *a; \
  BUILTIN_CHECK_NARGS(2,0); \
  k = getArg(0); \
  a = getArg(1);

#define BUILTIN2(name,a,b) \
  static void b_##name() { \
  void *k, *a, *b; \
  BUILTIN_CHECK_NARGS(3,0); \
  k = getArg(0); \
  a = getArg(1); \
  b = getArg(2);

#define BUILTIN_ANY(name) \
  static void b_##name() { \
  void *k; \
  BUILTIN_CHECK_NARGS_ABOVE(1, #name); \
  k = getArg(0);

#define ENDBUILTIN(r) \
    ALLOC(E, 1); \
    STORE(E, 0, (r)); \
    MOVE(N, 1); \
    CALL(k); \
  }

#define ENDBUILTIN_VOID \
  }


//E[0] = environment, E[1] = continuation, E[2] = function_name

// run continuation recieves entry point into user specified program
// it should run it with supplyed host resolver, which should resolve all unknown symbols

BUILTIN0(run)
  printf("got into run!\n");
  ALLOC(E, 2);
  STORE(E, 0, fin); // continuation
  STORE(E, 1, host); // resolver
  MOVE(N, 2);
  CALL(k);
ENDBUILTIN_VOID

static void print_object(void *o) {
  int tag = getTag(o);

  if (tag == T_FIXNUM) {
    printf("%d", (intptr_t)getVal(o)>>TAG_BITS);
  } else if (tag == T_STRING) {
    printf("\"%s\"", (char*)getVal(o));
  } else if (tag == T_CLOSURE) {
    printf("#(closure #%08x)", getVal(o));
  } else if (tag == T_PAIR) {
    printf("(");
    for (;;)  {
      print_object(CAR(o));
      o = CDR(o);
      if (o == v_empty) break;
      printf(" ");
    }
    printf(")");
  } else if (o == v_void) {
    printf("Void");
  } else if (o == v_yes) {
    printf("Yes");
  } else if (o == v_no) {
    printf("No");
  } else if (o == v_empty) {
    printf("()");
  } else {
    printf("#(ufo %p)", o);
  }
}

BUILTIN0(fin)
  printf("got into fin; result:\n");
  print_object(k);
  printf("\n");
ENDBUILTIN_VOID

BUILTIN_ANY(void)
  printf("FIXME: implement `void`\n");
  abort();
ENDBUILTIN(0)

BUILTIN_ANY(yes)
  printf("FIXME: implement `yes`\n");
  abort();
ENDBUILTIN(0)

BUILTIN_ANY(no)
  printf("FIXME: implement `no`\n");
  abort();
ENDBUILTIN(0)

BUILTIN_ANY(empty)
  printf("FIXME: implement `empty`\n");
  abort();
ENDBUILTIN(0)

BUILTIN2(add,a,b)
  CHECK_TAG(a, T_FIXNUM);
  CHECK_TAG(b, T_FIXNUM);
ENDBUILTIN(getVal(a) + getVal(b))

BUILTIN2(mul,a,b)
  CHECK_TAG(a, T_FIXNUM);
  CHECK_TAG(b, T_FIXNUM);
ENDBUILTIN((getVal(a)>>TAG_BITS) * getVal(b))

BUILTIN1(tag_of,a)
  if (getTag(a) == T_CLOSURE) {
    MOVE(N, -1);
    CALL(k);
    return;
  }
  printf("FIXME: implement for other builtin types\n");
  abort();
ENDBUILTIN(0)

BUILTIN_ANY(list)
  void *r = v_empty;
  int i = N;
  while (i-- > 1) {
    CONS(r, getArg(i), r);
  }
ENDBUILTIN(r)

static struct {
  char *name;
  void (*fun)();
} builtins[] = {
  {"+", b_add},
  {"*", b_mul},
  {"tag_of", b_tag_of},
  {"list", b_list},
  {0, 0}
};

BUILTIN1(host,t_name)
  CHECK_TAG(t_name, T_STRING);
  int i;
  char *name = (char*)getVal(t_name);
  for (i = 0; ; i++) {
    if (!builtins[i].name) {
      printf("host doesn't provide `%s`\n", name);
      abort();
    }
    if (!strcmp(builtins[i].name, name)) {
      break;
    }
  }
  CLOSURE(R, builtins[i].fun);
ENDBUILTIN(R)



// FIXME: the real implementation should encode these strings as a values of a tagged pointers
static uint8_t string_atoms[] = {
  0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 
  2, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 
  4, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 
  6, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 
  8, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 
  10, 0, 0, 0, 0, 0, 0, 0, 11, 0, 0, 0, 0, 0, 0, 0, 
  12, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 0, 
  14, 0, 0, 0, 0, 0, 0, 0, 15, 0, 0, 0, 0, 0, 0, 0, 
  16, 0, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 0, 0, 0, 0, 
  18, 0, 0, 0, 0, 0, 0, 0, 19, 0, 0, 0, 0, 0, 0, 0, 
  20, 0, 0, 0, 0, 0, 0, 0, 21, 0, 0, 0, 0, 0, 0, 0, 
  22, 0, 0, 0, 0, 0, 0, 0, 23, 0, 0, 0, 0, 0, 0, 0, 
  24, 0, 0, 0, 0, 0, 0, 0, 25, 0, 0, 0, 0, 0, 0, 0, 
  26, 0, 0, 0, 0, 0, 0, 0, 27, 0, 0, 0, 0, 0, 0, 0, 
  28, 0, 0, 0, 0, 0, 0, 0, 29, 0, 0, 0, 0, 0, 0, 0, 
  30, 0, 0, 0, 0, 0, 0, 0, 31, 0, 0, 0, 0, 0, 0, 0, 
  32, 0, 0, 0, 0, 0, 0, 0, 33, 0, 0, 0, 0, 0, 0, 0, 
  34, 0, 0, 0, 0, 0, 0, 0, 35, 0, 0, 0, 0, 0, 0, 0, 
  36, 0, 0, 0, 0, 0, 0, 0, 37, 0, 0, 0, 0, 0, 0, 0, 
  38, 0, 0, 0, 0, 0, 0, 0, 39, 0, 0, 0, 0, 0, 0, 0, 
  40, 0, 0, 0, 0, 0, 0, 0, 41, 0, 0, 0, 0, 0, 0, 0, 
  42, 0, 0, 0, 0, 0, 0, 0, 43, 0, 0, 0, 0, 0, 0, 0, 
  44, 0, 0, 0, 0, 0, 0, 0, 45, 0, 0, 0, 0, 0, 0, 0, 
  46, 0, 0, 0, 0, 0, 0, 0, 47, 0, 0, 0, 0, 0, 0, 0, 
  48, 0, 0, 0, 0, 0, 0, 0, 49, 0, 0, 0, 0, 0, 0, 0, 
  50, 0, 0, 0, 0, 0, 0, 0, 51, 0, 0, 0, 0, 0, 0, 0, 
  52, 0, 0, 0, 0, 0, 0, 0, 53, 0, 0, 0, 0, 0, 0, 0, 
  54, 0, 0, 0, 0, 0, 0, 0, 55, 0, 0, 0, 0, 0, 0, 0, 
  56, 0, 0, 0, 0, 0, 0, 0, 57, 0, 0, 0, 0, 0, 0, 0, 
  58, 0, 0, 0, 0, 0, 0, 0, 59, 0, 0, 0, 0, 0, 0, 0, 
  60, 0, 0, 0, 0, 0, 0, 0, 61, 0, 0, 0, 0, 0, 0, 0, 
  62, 0, 0, 0, 0, 0, 0, 0, 63, 0, 0, 0, 0, 0, 0, 0, 
  64, 0, 0, 0, 0, 0, 0, 0, 65, 0, 0, 0, 0, 0, 0, 0, 
  66, 0, 0, 0, 0, 0, 0, 0, 67, 0, 0, 0, 0, 0, 0, 0, 
  68, 0, 0, 0, 0, 0, 0, 0, 69, 0, 0, 0, 0, 0, 0, 0, 
  70, 0, 0, 0, 0, 0, 0, 0, 71, 0, 0, 0, 0, 0, 0, 0, 
  72, 0, 0, 0, 0, 0, 0, 0, 73, 0, 0, 0, 0, 0, 0, 0, 
  74, 0, 0, 0, 0, 0, 0, 0, 75, 0, 0, 0, 0, 0, 0, 0, 
  76, 0, 0, 0, 0, 0, 0, 0, 77, 0, 0, 0, 0, 0, 0, 0, 
  78, 0, 0, 0, 0, 0, 0, 0, 79, 0, 0, 0, 0, 0, 0, 0, 
  80, 0, 0, 0, 0, 0, 0, 0, 81, 0, 0, 0, 0, 0, 0, 0, 
  82, 0, 0, 0, 0, 0, 0, 0, 83, 0, 0, 0, 0, 0, 0, 0, 
  84, 0, 0, 0, 0, 0, 0, 0, 85, 0, 0, 0, 0, 0, 0, 0, 
  86, 0, 0, 0, 0, 0, 0, 0, 87, 0, 0, 0, 0, 0, 0, 0, 
  88, 0, 0, 0, 0, 0, 0, 0, 89, 0, 0, 0, 0, 0, 0, 0, 
  90, 0, 0, 0, 0, 0, 0, 0, 91, 0, 0, 0, 0, 0, 0, 0, 
  92, 0, 0, 0, 0, 0, 0, 0, 93, 0, 0, 0, 0, 0, 0, 0, 
  94, 0, 0, 0, 0, 0, 0, 0, 95, 0, 0, 0, 0, 0, 0, 0, 
  96, 0, 0, 0, 0, 0, 0, 0, 97, 0, 0, 0, 0, 0, 0, 0, 
  98, 0, 0, 0, 0, 0, 0, 0, 99, 0, 0, 0, 0, 0, 0, 0, 
  100, 0, 0, 0, 0, 0, 0, 0, 101, 0, 0, 0, 0, 0, 0, 0, 
  102, 0, 0, 0, 0, 0, 0, 0, 103, 0, 0, 0, 0, 0, 0, 0, 
  104, 0, 0, 0, 0, 0, 0, 0, 105, 0, 0, 0, 0, 0, 0, 0, 
  106, 0, 0, 0, 0, 0, 0, 0, 107, 0, 0, 0, 0, 0, 0, 0, 
  108, 0, 0, 0, 0, 0, 0, 0, 109, 0, 0, 0, 0, 0, 0, 0, 
  110, 0, 0, 0, 0, 0, 0, 0, 111, 0, 0, 0, 0, 0, 0, 0, 
  112, 0, 0, 0, 0, 0, 0, 0, 113, 0, 0, 0, 0, 0, 0, 0, 
  114, 0, 0, 0, 0, 0, 0, 0, 115, 0, 0, 0, 0, 0, 0, 0, 
  116, 0, 0, 0, 0, 0, 0, 0, 117, 0, 0, 0, 0, 0, 0, 0, 
  118, 0, 0, 0, 0, 0, 0, 0, 119, 0, 0, 0, 0, 0, 0, 0, 
  120, 0, 0, 0, 0, 0, 0, 0, 121, 0, 0, 0, 0, 0, 0, 0, 
  122, 0, 0, 0, 0, 0, 0, 0, 123, 0, 0, 0, 0, 0, 0, 0, 
  124, 0, 0, 0, 0, 0, 0, 0, 125, 0, 0, 0, 0, 0, 0, 0, 
  126, 0, 0, 0, 0, 0, 0, 0, 127, 0, 0, 0, 0, 0, 0, 0, 
  128, 0, 0, 0, 0, 0, 0, 0, 129, 0, 0, 0, 0, 0, 0, 0, 
  130, 0, 0, 0, 0, 0, 0, 0, 131, 0, 0, 0, 0, 0, 0, 0, 
  132, 0, 0, 0, 0, 0, 0, 0, 133, 0, 0, 0, 0, 0, 0, 0, 
  134, 0, 0, 0, 0, 0, 0, 0, 135, 0, 0, 0, 0, 0, 0, 0, 
  136, 0, 0, 0, 0, 0, 0, 0, 137, 0, 0, 0, 0, 0, 0, 0, 
  138, 0, 0, 0, 0, 0, 0, 0, 139, 0, 0, 0, 0, 0, 0, 0, 
  140, 0, 0, 0, 0, 0, 0, 0, 141, 0, 0, 0, 0, 0, 0, 0, 
  142, 0, 0, 0, 0, 0, 0, 0, 143, 0, 0, 0, 0, 0, 0, 0, 
  144, 0, 0, 0, 0, 0, 0, 0, 145, 0, 0, 0, 0, 0, 0, 0, 
  146, 0, 0, 0, 0, 0, 0, 0, 147, 0, 0, 0, 0, 0, 0, 0, 
  148, 0, 0, 0, 0, 0, 0, 0, 149, 0, 0, 0, 0, 0, 0, 0, 
  150, 0, 0, 0, 0, 0, 0, 0, 151, 0, 0, 0, 0, 0, 0, 0, 
  152, 0, 0, 0, 0, 0, 0, 0, 153, 0, 0, 0, 0, 0, 0, 0, 
  154, 0, 0, 0, 0, 0, 0, 0, 155, 0, 0, 0, 0, 0, 0, 0, 
  156, 0, 0, 0, 0, 0, 0, 0, 157, 0, 0, 0, 0, 0, 0, 0, 
  158, 0, 0, 0, 0, 0, 0, 0, 159, 0, 0, 0, 0, 0, 0, 0, 
  160, 0, 0, 0, 0, 0, 0, 0, 161, 0, 0, 0, 0, 0, 0, 0, 
  162, 0, 0, 0, 0, 0, 0, 0, 163, 0, 0, 0, 0, 0, 0, 0, 
  164, 0, 0, 0, 0, 0, 0, 0, 165, 0, 0, 0, 0, 0, 0, 0, 
  166, 0, 0, 0, 0, 0, 0, 0, 167, 0, 0, 0, 0, 0, 0, 0, 
  168, 0, 0, 0, 0, 0, 0, 0, 169, 0, 0, 0, 0, 0, 0, 0, 
  170, 0, 0, 0, 0, 0, 0, 0, 171, 0, 0, 0, 0, 0, 0, 0, 
  172, 0, 0, 0, 0, 0, 0, 0, 173, 0, 0, 0, 0, 0, 0, 0, 
  174, 0, 0, 0, 0, 0, 0, 0, 175, 0, 0, 0, 0, 0, 0, 0, 
  176, 0, 0, 0, 0, 0, 0, 0, 177, 0, 0, 0, 0, 0, 0, 0, 
  178, 0, 0, 0, 0, 0, 0, 0, 179, 0, 0, 0, 0, 0, 0, 0, 
  180, 0, 0, 0, 0, 0, 0, 0, 181, 0, 0, 0, 0, 0, 0, 0, 
  182, 0, 0, 0, 0, 0, 0, 0, 183, 0, 0, 0, 0, 0, 0, 0, 
  184, 0, 0, 0, 0, 0, 0, 0, 185, 0, 0, 0, 0, 0, 0, 0, 
  186, 0, 0, 0, 0, 0, 0, 0, 187, 0, 0, 0, 0, 0, 0, 0, 
  188, 0, 0, 0, 0, 0, 0, 0, 189, 0, 0, 0, 0, 0, 0, 0, 
  190, 0, 0, 0, 0, 0, 0, 0, 191, 0, 0, 0, 0, 0, 0, 0, 
  192, 0, 0, 0, 0, 0, 0, 0, 193, 0, 0, 0, 0, 0, 0, 0, 
  194, 0, 0, 0, 0, 0, 0, 0, 195, 0, 0, 0, 0, 0, 0, 0, 
  196, 0, 0, 0, 0, 0, 0, 0, 197, 0, 0, 0, 0, 0, 0, 0, 
  198, 0, 0, 0, 0, 0, 0, 0, 199, 0, 0, 0, 0, 0, 0, 0, 
  200, 0, 0, 0, 0, 0, 0, 0, 201, 0, 0, 0, 0, 0, 0, 0, 
  202, 0, 0, 0, 0, 0, 0, 0, 203, 0, 0, 0, 0, 0, 0, 0, 
  204, 0, 0, 0, 0, 0, 0, 0, 205, 0, 0, 0, 0, 0, 0, 0, 
  206, 0, 0, 0, 0, 0, 0, 0, 207, 0, 0, 0, 0, 0, 0, 0, 
  208, 0, 0, 0, 0, 0, 0, 0, 209, 0, 0, 0, 0, 0, 0, 0, 
  210, 0, 0, 0, 0, 0, 0, 0, 211, 0, 0, 0, 0, 0, 0, 0, 
  212, 0, 0, 0, 0, 0, 0, 0, 213, 0, 0, 0, 0, 0, 0, 0, 
  214, 0, 0, 0, 0, 0, 0, 0, 215, 0, 0, 0, 0, 0, 0, 0, 
  216, 0, 0, 0, 0, 0, 0, 0, 217, 0, 0, 0, 0, 0, 0, 0, 
  218, 0, 0, 0, 0, 0, 0, 0, 219, 0, 0, 0, 0, 0, 0, 0, 
  220, 0, 0, 0, 0, 0, 0, 0, 221, 0, 0, 0, 0, 0, 0, 0, 
  222, 0, 0, 0, 0, 0, 0, 0, 223, 0, 0, 0, 0, 0, 0, 0, 
  224, 0, 0, 0, 0, 0, 0, 0, 225, 0, 0, 0, 0, 0, 0, 0, 
  226, 0, 0, 0, 0, 0, 0, 0, 227, 0, 0, 0, 0, 0, 0, 0, 
  228, 0, 0, 0, 0, 0, 0, 0, 229, 0, 0, 0, 0, 0, 0, 0, 
  230, 0, 0, 0, 0, 0, 0, 0, 231, 0, 0, 0, 0, 0, 0, 0, 
  232, 0, 0, 0, 0, 0, 0, 0, 233, 0, 0, 0, 0, 0, 0, 0, 
  234, 0, 0, 0, 0, 0, 0, 0, 235, 0, 0, 0, 0, 0, 0, 0, 
  236, 0, 0, 0, 0, 0, 0, 0, 237, 0, 0, 0, 0, 0, 0, 0, 
  238, 0, 0, 0, 0, 0, 0, 0, 239, 0, 0, 0, 0, 0, 0, 0, 
  240, 0, 0, 0, 0, 0, 0, 0, 241, 0, 0, 0, 0, 0, 0, 0, 
  242, 0, 0, 0, 0, 0, 0, 0, 243, 0, 0, 0, 0, 0, 0, 0, 
  244, 0, 0, 0, 0, 0, 0, 0, 245, 0, 0, 0, 0, 0, 0, 0, 
  246, 0, 0, 0, 0, 0, 0, 0, 247, 0, 0, 0, 0, 0, 0, 0, 
  248, 0, 0, 0, 0, 0, 0, 0, 249, 0, 0, 0, 0, 0, 0, 0, 
  250, 0, 0, 0, 0, 0, 0, 0, 251, 0, 0, 0, 0, 0, 0, 0, 
  252, 0, 0, 0, 0, 0, 0, 0, 253, 0, 0, 0, 0, 0, 0, 0, 
  254, 0, 0, 0, 0, 0, 0, 0, 255, 0, 0, 0, 0, 0, 0, 0
};


static void entry();

// gcc -O3 test.c && ./a.out
int main(int argc, char **argv) {
  CLOSURE(run, b_run);
  CLOSURE(fin, b_fin);
  CLOSURE(host, b_host);
  CLOSURE(v_void, b_void);
  CLOSURE(v_yes, b_yes);
  CLOSURE(v_no, b_no);
  CLOSURE(v_empty, b_empty);
  entry();
  printf("main() say goodbay\n");
}
