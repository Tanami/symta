#include "common.h"
static void f1244();
static void f1250();
static void f1251();
static void f1245();
static void f1246();
static void f1247();
static void f1248();
static void f1249();
static void entry() {
  MOVE(R, run);
  CHECK_TAG(R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 1);
  ALLOC(R, 1);
  STORE(R, 0, f1244);
  IOR(R, R, T_CLOSURE);
  STORE(A, 0, R);
  MOVE(E, A);
  MOVE(N, 1);
  CALL(C);
}

static void f1244() {
  printf("entering %s\n", "f1244");
  CHECK_NARGS(2);
  ALLOC(R, 2);
  STORE(R, 0, f1245);
  STORE(R, 1, E);
  IOR(R, R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 1);
  ALLOC(R, 1);
  STORE(R, 0, f1250);
  IOR(R, R, T_CLOSURE);
  STORE(A, 0, R);
  MOVE(E, A);
  MOVE(N, 1);
  CALL(C);
}

static void f1250() {
  printf("entering %s\n", "f1250");
  CHECK_NARGS(3);
  LOAD(R, E, 2);
  CHECK_TAG(R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 3);
  ALLOC(R, 2);
  STORE(R, 0, f1251);
  STORE(R, 1, E);
  IOR(R, R, T_CLOSURE);
  STORE(A, 0, R);
  INTEGER(R, 123);
  STORE(A, 1, R);
  INTEGER(R, 789);
  STORE(A, 2, R);
  MOVE(E, A);
  MOVE(N, 3);
  CALL(C);
}

static void f1251() {
  printf("entering %s\n", "f1251");
  CHECK_NARGS(1);
  LOAD(R, P, 1);
  LOAD(R, R, 1);
  CHECK_TAG(R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 3);
  LOAD(R, P, 1);
  LOAD(R, R, 0);
  STORE(A, 0, R);
  LOAD(R, E, 0);
  STORE(A, 1, R);
  INTEGER(R, 456);
  STORE(A, 2, R);
  MOVE(E, A);
  MOVE(N, 3);
  CALL(C);
}

static void f1245() {
  printf("entering %s\n", "f1245");
  CHECK_NARGS(1);
  ALLOC(R, 3);
  STORE(R, 0, f1246);
  COPY(R, 1, P, 1);
  STORE(R, 2, E);
  IOR(R, R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 1);
  STRING(R, "*");
  STORE(A, 0, R);
  MOVE(E, A);
  MOVE(N, 1);
  CALL(C);
}

static void f1246() {
  printf("entering %s\n", "f1246");
  CHECK_NARGS(1);
  LOAD(R, P, 1);
  LOAD(R, R, 1);
  CHECK_TAG(R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 2);
  ALLOC(R, 3);
  STORE(R, 0, f1247);
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

static void f1247() {
  printf("entering %s\n", "f1247");
  CHECK_NARGS(1);
  ALLOC(R, 4);
  STORE(R, 0, f1248);
  COPY(R, 1, P, 1);
  COPY(R, 2, P, 2);
  STORE(R, 3, E);
  IOR(R, R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 1);
  STRING(R, "+");
  STORE(A, 0, R);
  MOVE(E, A);
  MOVE(N, 1);
  CALL(C);
}

static void f1248() {
  printf("entering %s\n", "f1248");
  CHECK_NARGS(1);
  LOAD(R, P, 1);
  LOAD(R, R, 1);
  CHECK_TAG(R, T_CLOSURE);
  MOVE(C, R);
  ALLOC(A, 2);
  ALLOC(R, 4);
  STORE(R, 0, f1249);
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

static void f1249() {
  printf("entering %s\n", "f1249");
  CHECK_NARGS(1);
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
