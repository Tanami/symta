#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct avl_node avl_node;

struct avl_node {
  void *data;
  avl_node *left;
  avl_node *right;
  int height;
};

static avl_node* avl_find(avl_node* t, void *item, int*(*cmp)(void*,void*));
static avl_node* avl_find_first(avl_node *t);
static avl_node* avl_find_last(avl_node *t);
static avl_node* avl_insert(avl_node *t, void *item, int (*cmp)(void*,void*));
static void avl_apply(avl_node* t, void(*f)(void*));
static void *avl_apply_breakable(avl_node* t, void*(*f)(void*));

static int avl_max_nodes;
static avl_node *avl_nodes;
static int avl_nodes_used;

static void avl_init(int max_nodes) {
  avl_max_nodes = max_nodes;
  avl_nodes = (avl_node*)malloc(max_nodes*sizeof(avl_node));
}

static avl_node *avl_alloc() {
 return avl_nodes+avl_nodes_used++;
}

static void avl_clear() {
  avl_nodes_used = 0;
}
 
static avl_node* avl_find(avl_node* t, void *item, int*(*cmp)(void*,void*)) {
  if (!t) return 0;
  if (cmp(item, t->data)) return avl_find(t->left, item, cmp);
  if (cmp(t->data, item)) return avl_find(t->right, item, cmp);
  return t;
}
 
static avl_node* avl_find_first(avl_node* t) {
  if(!t) return 0;
  while (t->left) t = t->left;
  return t;
}
 
static avl_node* avl_find_last(avl_node* t) {
  if (!t) return 0;
  while (t->right) t = t->right;
  return t;
}

static int avl_height(avl_node* n) {
  return n ? n->height : -1;
}

static int avl_max(int l, int r) {
  return l > r ? l : r;
}
 
static avl_node *single_rotate_with_left(avl_node* k2) {
  avl_node* k1 = k2->left;
  k2->left = k1->right;
  k1->right = k2;
  k2->height = avl_max(avl_height(k2->left), avl_height(k2->right)) + 1;
  k1->height = avl_max(avl_height(k1->left), k2->height) + 1;
  return k1;
}

static avl_node* single_rotate_with_right(avl_node* k1) {
  avl_node* k2 = k1->right;
  k1->right = k2->left;
  k2->left = k1;
  k1->height = avl_max(avl_height(k1->left), avl_height(k1->right)) + 1;
  k2->height = avl_max(avl_height(k2->right), k1->height) + 1;
  return k2;
}

static avl_node* double_rotate_with_left(avl_node* k3) {
  k3->left = single_rotate_with_right(k3->left);
  return single_rotate_with_left(k3);
}
 
static avl_node* double_rotate_with_right(avl_node* k1) {
  k1->right = single_rotate_with_left(k1->right);
  return single_rotate_with_right(k1);
}


static int (*avl_cmp)(void*,void*);
static void *avl_item;

static avl_node* avl_insert_r(avl_node *t) {
  if (!t) {
    t = avl_alloc();
    t->data = avl_item;
    t->height = 0;
    t->left = t->right = 0;
    return t;
  }

  if (avl_cmp(avl_item, t->data)) {
    t->left = avl_insert_r(t->left);
    if (avl_height(t->left) - avl_height(t->right) == 2) {
      if (avl_cmp(avl_item, t->left->data)) t = single_rotate_with_left(t);
      else t = double_rotate_with_left(t);
    }
  } else {
    t->right = avl_insert_r(t->right);
    if (avl_height(t->right) - avl_height(t->left) == 2) {
      if (!avl_cmp(avl_item, t->right->data)) t = single_rotate_with_right(t);
      else t = double_rotate_with_right(t);
    }
  }

  t->height = avl_max(avl_height(t->left), avl_height(t->right)) + 1;
  return t;
}

static avl_node* avl_insert(avl_node *t, void *item, int (*cmp)(void*,void*)) {
  avl_item = item;
  avl_cmp = cmp;
  return avl_insert_r(t);
}

static void avl_apply(avl_node* t, void(*f)(void*)) {
  if (!t) return;
  avl_apply(t->left,f);
  f(t->data);
  avl_apply(t->right,f);
}

static void *avl_apply_breakable(avl_node* t, void*(*f)(void*)) {
  if (!t) return 0;
  if(avl_apply_breakable(t->left,f)) return t;
  if(f(t->data)) return t;
  if(avl_apply_breakable(t->right,f)) return t;
  return 0;
}

