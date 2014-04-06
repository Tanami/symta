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


#define T_INTEGER  0
#define T_STRING   1
#define T_PAIR     2
#define T_CLOSURE  3

// make strings and pairs statically allocable
#define INTEGER(dst,x) dst = (void*)(((uintptr_t)(x)<<TAG_BITS) | T_INTEGER)
#define STRING(dst,x)  dst = (void*)((uintptr_t)strdup(x) | T_STRING)
#define PAIR(dst,a,b) \
  ALLOC(T, 2); \
  STORE(T, 0, a); \
  STORE(T, 1, b); \
  dst = (void*)((uintptr_t)T | T_PAIR);
#define ALLOC(dst, size) dst = malloc(size*sizeof(void*))
#define CALL(f) \
  P = (void*)((uintptr_t)f-T_CLOSURE); \
  (((pfun*)P)[0])();
#define STORE(dst,off,src) ((void**)(dst))[(int)(off)] = (void*)(src)
#define LOAD(dst,src,off) dst = ((void**)(src))[(int)(off)]
#define COPY(dst,p,src,q) ((void**)(dst))[(int)(p)] = ((void**)(src))[(int)(q)]
#define MOVE(dst,src) dst = (void*)(src)
#define IOR(dst,a,b) dst = (void*)((uintptr_t)(a)|(uintptr_t)(b))
#define CHECK_TAG(src,expected) if (getTag(src) != expected) bad_tag(getTag(src), expected)
#define CHECK_NARGS(expected) if ((int)(uintptr_t)N != (int)expected) bad_number_of_arguments((int)(uintptr_t)N, (int)expected)


#define CLOSURE(dst,f) \
  ALLOC(T, 1); \
  STORE(T, 0, f); \
  dst = (void*)((uintptr_t)T | T_CLOSURE);


static void bad_tag(int tag, int expected) {
  printf("bad tag=%d, expected tag=%d\n", tag, expected);
  abort();
}

static void bad_number_of_arguments(int got, int expected) {
  printf("bad number of arguments: got=%d, expected=%d\n", got, expected);
  abort();
}


static void entry();
static void run_f();
static void fin_f();
static void host_f();

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
  *fin, // the closure, which would recieve evaluation result
  *run, // the closure, which would recieve the resulting program
  *host;



int main(int argc, char **argv) {
  CLOSURE(run, run_f);
  CLOSURE(fin, fin_f);
  CLOSURE(host, host_f);
  entry();
}

#define GETARG(N) ((void**)(E))[N]

//E[0] = environment, E[1] = continuation, E[2] = function_name

// run continuation recieves entry point into user specified program
// it should run it with supplyed host resolver, which should resolve all unknown symbols
static void run_f() {
  printf("got into run!\n");
  LOAD(C, E, 0);
  ALLOC(E, 2);
  STORE(E, 0, fin); // continuation
  STORE(E, 1, host); // resolver
  MOVE(N, 2);
  CALL(C);
}

static void fin_f() {
  CHECK_NARGS(1);
  printf("got into fin! result = %d\n", getVal(getArg(0))>>TAG_BITS);

  abort();
}


static void b_add() {
  void *k, *a, *b;
  CHECK_NARGS(3);
  k = getArg(0);
  a = getArg(1);
  b = getArg(2);
  CHECK_TAG(a, T_INTEGER);
  CHECK_TAG(b, T_INTEGER);
  ALLOC(E, 1);
  STORE(E, 0, getVal(a) + getVal(b));
  MOVE(N, 1);
  CALL(k);
}

static void b_mul() {
  void *k, *a, *b;
  CHECK_NARGS(3);
  k = getArg(0);
  a = getArg(1);
  b = getArg(2);
  CHECK_TAG(a, T_INTEGER);
  CHECK_TAG(b, T_INTEGER);
  ALLOC(E, 1);
  STORE(E, 0, (getVal(a)>>TAG_BITS) * getVal(b));
  MOVE(N, 1);
  CALL(k);
}


static struct {
  char *name;
  void (*fun)();
} builtins[] = {
  {"+", b_add},
  {"*", b_mul},
  {0, 0}
};

static void host_f() {
  int i;
  uintptr_t *k, *t_name;
  CHECK_NARGS(2);
  k = getArg(0);
  t_name = getArg(1);
  CHECK_TAG(t_name, T_STRING);

  char *name = (char*)getVal(t_name);
  for (i = 0; builtins[i].name; i++) {
    if (!strcmp(builtins[i].name, name)) {
      ALLOC(E, 1);
      CLOSURE(R, builtins[i].fun);
      STORE(E, 0, R);
      MOVE(N, 1);
      CALL(k);
    }
  }

  printf("host doesn't provide \"%s\"\n", name);
  abort();
}
