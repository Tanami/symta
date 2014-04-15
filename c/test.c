#include "common.h"

static void f3132();
static void f3138();
static void f3133();
static void f3134();
static void f3135();
static void f3136();
static void f3137();
#define s3161 (data3159+0)
#define s3160 (data3159+16)
static uint8_t data3159[];
static void *list3139;
static void f3148();
static void f3152();
static void f3153();
static void f3154();
static void f3155();
static void f3149();
static void f3150();
static void f3151();
#define s3158 (data3156+0)
#define s3157 (data3156+16)
static uint8_t data3156[];
static void init_list3139() {
  MOVE(R, run);
  CHECK_TAG(R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 1);
  ALLOC(R, 1);
  STORE(R, 0, f3148);
  IOR(R, R, T_CLOSURE);
  STORE(A, 0, R);
  MOVE(E, A);
  MOVE(N, 1);
  CALL(C);
}

static void f3148() {
  printf("entering %s\n", "f3148");
  CHECK_NARGS(2, 0);
  ALLOC(R, 2);
  STORE(R, 0, f3149);
  STORE(R, 1, E);
  IOR(R, R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 1);
  ALLOC(R, 1);
  STORE(R, 0, f3152);
  IOR(R, R, T_CLOSURE);
  STORE(A, 0, R);
  MOVE(E, A);
  MOVE(N, 1);
  CALL(C);
}

static void f3152() {
  printf("entering %s\n", "f3152");
  CHECK_NARGS(2, 0);
  ALLOC(R, 2);
  STORE(R, 0, f3153);
  STORE(R, 1, E);
  IOR(R, R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 1);
  STRING(R, s3157);
  STORE(A, 0, R);
  MOVE(E, A);
  MOVE(N, 1);
  CALL(C);
}

static void f3153() {
  printf("entering %s\n", "f3153");
  CHECK_NARGS(1, 0);
  ALLOC(R, 3);
  STORE(R, 0, f3154);
  COPY(R, 1, P, 1);
  STORE(R, 2, E);
  IOR(R, R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 1);
  FIXNUM(R, 123);
  STORE(A, 0, R);
  MOVE(E, A);
  MOVE(N, 1);
  CALL(C);
}

static void f3154() {
  printf("entering %s\n", "f3154");
  CHECK_NARGS(1, 0);
  ALLOC(R, 4);
  STORE(R, 0, f3155);
  COPY(R, 1, P, 1);
  COPY(R, 2, P, 2);
  STORE(R, 3, E);
  IOR(R, R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 1);
  FIXNUM(R, 789);
  STORE(A, 0, R);
  MOVE(E, A);
  MOVE(N, 1);
  CALL(C);
}

static void f3155() {
  printf("entering %s\n", "f3155");
  CHECK_NARGS(1, 0);
  LOAD(R, P, 1);
  LOAD(R, R, 1);
  CHECK_TAG(R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 4);
  LOAD(R, P, 1);
  LOAD(R, R, 0);
  STORE(A, 0, R);
  LOAD(R, P, 2);
  LOAD(R, R, 0);
  STORE(A, 1, R);
  LOAD(R, P, 3);
  LOAD(R, R, 0);
  STORE(A, 2, R);
  LOAD(R, E, 0);
  STORE(A, 3, R);
  MOVE(E, A);
  MOVE(N, 4);
  CALL(C);
}

static void f3149() {
  printf("entering %s\n", "f3149");
  CHECK_NARGS(1, 0);
  ALLOC(R, 3);
  STORE(R, 0, f3150);
  COPY(R, 1, P, 1);
  STORE(R, 2, E);
  IOR(R, R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 1);
  STRING(R, s3158);
  STORE(A, 0, R);
  MOVE(E, A);
  MOVE(N, 1);
  CALL(C);
}

static void f3150() {
  printf("entering %s\n", "f3150");
  CHECK_NARGS(1, 0);
  LOAD(R, P, 1);
  LOAD(R, R, 1);
  CHECK_TAG(R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 2);
  ALLOC(R, 3);
  STORE(R, 0, f3151);
  COPY(R, 1, P, 2);
  COPY(R, 2, P, 1);
  IOR(R, R, T_CLOSURE);
  STORE(A, 0, R);
  LOAD(R, E, 0);
  STORE(A, 1, R);
  MOVE(E, A);
  MOVE(N, 2);
  CALL(C);
}

static void f3151() {
  printf("entering %s\n", "f3151");
  CHECK_NARGS(1, 0);
  LOAD(R, P, 1);
  LOAD(R, R, 0);
  CHECK_TAG(R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 2);
  LOAD(R, P, 2);
  LOAD(R, R, 0);
  STORE(A, 0, R);
  LOAD(R, E, 0);
  STORE(A, 1, R);
  MOVE(E, A);
  MOVE(N, 2);
  CALL(C);
}

static uint8_t data3156[] = {
  108, 105, 115, 116, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  43, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

static void entry() {
  init_list3139();
  list3139 = getArg(0);
  MOVE(R, run);
  CHECK_TAG(R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 1);
  ALLOC(R, 1);
  STORE(R, 0, f3132);
  IOR(R, R, T_CLOSURE);
  STORE(A, 0, R);
  MOVE(E, A);
  MOVE(N, 1);
  CALL(C);
}

static void f3132() {
  printf("entering %s\n", "f3132");
  CHECK_NARGS(2, 0);
  ALLOC(R, 2);
  STORE(R, 0, f3133);
  STORE(R, 1, E);
  IOR(R, R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 1);
  ALLOC(R, 1);
  STORE(R, 0, f3138);
  IOR(R, R, T_CLOSURE);
  STORE(A, 0, R);
  MOVE(E, A);
  MOVE(N, 1);
  CALL(C);
}

static void f3138() {
  printf("entering %s\n", "f3138");
  CHECK_NARGS(3, 0);
  LOAD(R, E, 0);
  CHECK_TAG(R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 1);
  MOVE(R, list3139);
  STORE(A, 0, R);
  MOVE(E, A);
  MOVE(N, 1);
  CALL(C);
}

static void f3133() {
  printf("entering %s\n", "f3133");
  CHECK_NARGS(1, 0);
  ALLOC(R, 3);
  STORE(R, 0, f3134);
  COPY(R, 1, P, 1);
  STORE(R, 2, E);
  IOR(R, R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 1);
  STRING(R, s3160);
  STORE(A, 0, R);
  MOVE(E, A);
  MOVE(N, 1);
  CALL(C);
}

static void f3134() {
  printf("entering %s\n", "f3134");
  CHECK_NARGS(1, 0);
  LOAD(R, P, 1);
  LOAD(R, R, 1);
  CHECK_TAG(R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 2);
  ALLOC(R, 3);
  STORE(R, 0, f3135);
  COPY(R, 1, P, 1);
  COPY(R, 2, P, 2);
  IOR(R, R, T_CLOSURE);
  STORE(A, 0, R);
  LOAD(R, E, 0);
  STORE(A, 1, R);
  MOVE(E, A);
  MOVE(N, 2);
  CALL(C);
}

static void f3135() {
  printf("entering %s\n", "f3135");
  CHECK_NARGS(1, 0);
  ALLOC(R, 4);
  STORE(R, 0, f3136);
  COPY(R, 1, P, 1);
  COPY(R, 2, P, 2);
  STORE(R, 3, E);
  IOR(R, R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 1);
  STRING(R, s3161);
  STORE(A, 0, R);
  MOVE(E, A);
  MOVE(N, 1);
  CALL(C);
}

static void f3136() {
  printf("entering %s\n", "f3136");
  CHECK_NARGS(1, 0);
  LOAD(R, P, 1);
  LOAD(R, R, 1);
  CHECK_TAG(R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 2);
  ALLOC(R, 4);
  STORE(R, 0, f3137);
  COPY(R, 1, P, 2);
  COPY(R, 2, P, 1);
  COPY(R, 3, P, 3);
  IOR(R, R, T_CLOSURE);
  STORE(A, 0, R);
  LOAD(R, E, 0);
  STORE(A, 1, R);
  MOVE(E, A);
  MOVE(N, 2);
  CALL(C);
}

static void f3137() {
  printf("entering %s\n", "f3137");
  CHECK_NARGS(1, 0);
  LOAD(R, P, 1);
  LOAD(R, R, 0);
  CHECK_TAG(R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 3);
  LOAD(R, P, 2);
  LOAD(R, R, 0);
  STORE(A, 0, R);
  LOAD(R, P, 3);
  LOAD(R, R, 0);
  STORE(A, 1, R);
  LOAD(R, E, 0);
  STORE(A, 2, R);
  MOVE(E, A);
  MOVE(N, 3);
  CALL(C);
}

static uint8_t data3159[] = {
  43, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  42, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};
