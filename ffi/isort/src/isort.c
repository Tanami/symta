/*
Copyright (C) 2003-2004 The Pentagram team
Copyright (C) 2015 Nikita Sadkov

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/


#include <stdio.h>

static int calln;
static int calln2;

#include "avl.c"


#define F_TRANSLUCENT     0x01
#define F_ANIMATED        0x02
#define F_SOLID           0x04
#define F_DRAW_FIRST      0x10
#define F_FLAT            0x20
#define F_DITHER          0x40
#define F_PROCESSED       0x80000


#define f_draw(x) ((x)->flags&F_DRAW_FIRST)
#define f_anim(x) ((x)->flags&F_ANIMATED)
#define f_solid(x) ((x)->flags&F_SOLID)
#define f_flat(x) ((x)->flags&F_FLAT)
#define f_trans(x) ((x)->flags&F_TRANSLUCENT)

typedef struct SortItem SortItem;
typedef struct DepNode DepNode;

struct DepNode {
  DepNode *next;
  SortItem *val;
};

static int max_dep_nodes;
static DepNode *dep_nodes;
static int dep_nodes_used;

static void dep_nodes_init(int max_nodes) {
  max_dep_nodes = max_nodes;
  dep_nodes = (DepNode*)malloc(max_nodes*sizeof(DepNode));
}

static DepNode *alloc_dep_node() {
  return dep_nodes+dep_nodes_used++;
}

static void dep_nodes_clear() {
  dep_nodes_used = 0;
}


struct SortItem {
   /* Bounding Box layout
         1    
       /   \      
     /       \     1 = Left  Far  Top LFT --+
   2           3   2 = Left  Near Top LNT -++
   | \       / |   3 = Right Far  Top RFT +-+
   |   \   /   |   4 = Right Near Top RNT +++
   |     4     |   5 = Left  Near Bot LNB -+-
   |     |     |   6 = Right Far  Bot RFB +--
   5     |     6   7 = Right Near Bot LNB ++- 
     \   |   /     8 = Left  Far  Bot LFB --- (not shown)
       \ | /    
         7   */

  int x, xleft; // Worldspace bounding box x (xright = x)
  int y, yfar;  // Worldspace bounding box y (ynear = y)
  int z, ztop;  // Worldspace bounding box z (ztop = z)

  int sxleft;   // Screenspace bounding box left extent    (LNT x coord)
  int sxright;  // Screenspace bounding box right extent   (RFT x coord)

  int sxtop;    // Screenspace bounding box top x coord    (LFT x coord)
  int sytop;    // Screenspace bounding box top extent     (LFT y coord)

  int sxbot;    // Screenspace bounding box bottom x coord (RNB x coord) ss origin
  int sybot;    // Screenspace bounding box bottom extent  (RNB y coord) ss origin

  DepNode *deps_list; // dependencies of this item

  int flags;
  int item_num; // Owner item number
  int shape_num;
};

static int item_compareA(SortItem *a, SortItem *b) {
  if (a->z < b->z) return 1;
  if (a->z > b->z) return 0;
  return a->x < b->x || (a->x == b->x && a->y < b->y);
}

static int si_item_compareA(void *aa, void*bb) {
  SortItem *a = (SortItem*)aa;
  SortItem *b = (SortItem*)bb;
  return item_compareA(a,b);
}


static int item_compareB(SortItem *a, SortItem *b) {
  // Specialist z flat handling
  if (f_flat(a) && f_flat(b)) {
    // Differing z is easy for flats
    if (a->ztop != b->ztop) return a->ztop < b->ztop;

    // Equal z

    // Animated always gets drawn after
    if (f_anim(a) != f_anim(b)) return f_anim(a) < f_anim(b);

    // Trans always gets drawn after
    if (f_trans(a) != f_trans(b)) return f_trans(a) < f_trans(b);

    // Draw always gets drawn first
    if (f_draw(a) != f_draw(b)) return f_draw(a) > f_draw(b);

    // Solid always gets drawn first
    if (f_solid(a) != f_solid(b)) return f_solid(a) > f_solid(b);
  } else { // Mixed, or non flat
    // Clearly in z
    if (a->ztop <= b->z) return 1;
    if (a->z >= b->ztop) return 0;
  }

  // Clearly in x?
  if (a->x <= b->xleft) return 1;
  if (a->xleft >= b->x) return 0;

  // Clearly in y?
  if (a->y <= b->yfar) return 1;
  if (a->yfar >= b->y) return 0;

  // Are overlapping in all 3 dimentions if we come here

  // Overlapping z-bottom check
  // If an object's base (z-bottom) is higher another's, it should be rendered after.
  // This check must be on the z-bottom and not the z-top because two objects with the
  // same z-position may have different heights (think of a mouse sorting vs the Avatar).
  if (a->z < b->z) return 1;
  if (a->z > b->z) return 0;

  // Biased Clearly in z
  if ((a->ztop+a->z)/2 <= b->z) return 1;
  if (a->z >= (b->ztop+b->z)/2) return 0;

  // Biased Clearly X
  if ((a->x+a->xleft)/2 <= b->xleft) return 1;
  if (a->xleft >= (b->x+b->xleft)/2) return 0;

  // Biased Clearly Y
  if ((a->y+a->yfar)/2 <= b->yfar) return 1;
  if (a->yfar >= (b->y+b->yfar)/2) return 0;

  if (a->x + a->y != b->x + b->y) // Partial in X + Y front
    return (a->x + a->y < b->x + b->y);

  if (a->xleft + a->yfar != b->xleft + b->yfar) // Partial in X + Y back
    return (a->xleft + a->yfar < b->xleft + b->yfar);
 
  if (a->x != b->x) return a->x < b->x;  // Partial in x?
  if (a->y != b->y) return a->y < b->y; // Partial in y?

  return a->shape_num < b->shape_num;
}

static int overlap(SortItem *a, SortItem *b) {
  int dt0,dt1,db0,db1;

  if(a->sxright <= b->sxleft) return 0; //right_clear
  if(a->sxleft >= b->sxright) return 0; //left_clear

  // This function is a bit of a hack. It uses dot products between
  // points and the lines. Nothing is normalized since that isn't 
  // important

  dt0 = a->sxtop - b->sxbot;
  dt1 = a->sytop - b->sybot;

  // 'normal' of top left line ( 2,-1) of the bounding box
  if(dt0 + dt1*2 >= 0) return 0;

  // 'normal' of top right line ( 2, 1) of the bounding box
  if(-dt0 + dt1*2 >= 0) return 0;

  db0 = a->sxbot - b->sxtop;
  db1 = a->sybot - b->sytop;

  // 'normal' of bot left line (-2,-1) of the bounding box
  if(db0 - db1*2 >= 0) return 0;

  // 'normal' of bot right line (-2, 1) of the bounding box
  if(-db0 - db1*2 >= 0) return 0;

  return 1;
}

static void dep_push_back(SortItem *a, SortItem *b) {
  DepNode *n;
  DepNode *nn = alloc_dep_node();
  nn->val = b;
  nn->next = a->deps_list;
  a->deps_list = nn;
}

static void dep_insert_sorted(SortItem *a, SortItem *b) {
  DepNode *n;
  DepNode *nn = alloc_dep_node();
  nn->val = b;

  for (n = a->deps_list; n; n = n->next) {
    if (item_compareA(n->val,b)) {
      nn->next = n->next;
      n->next = nn;
      return;
    }
  }

  nn->next = a->deps_list;
  a->deps_list = nn;
}


static int sort_items_max;
static SortItem *sort_items;
static int sort_items_used;

static void sort_items_init(int max_items) {
  sort_items_max = max_items;
  sort_items = (SortItem*)malloc(max_items*sizeof(SortItem));
}

static void sort_items_clear() {
  sort_items_used = 0;
}

static SortItem *alloc_sort_item() {
  return sort_items+sort_items_used++;
}

static avl_node *display_list = 0;
static int *display_list_result;
static int order_counter;

static void order_item(void *aa) {
  DepNode *n, *prev, *next;
  SortItem *si = (SortItem*)aa;
  if (si->flags&F_PROCESSED) return;
  si->flags |= F_PROCESSED; // avoid infinite recursion

  prev = 0;
  for (n = si->deps_list; n; n = next) {
    next = n->next;
    n->next = prev;
    prev = n;
  }

  for (n = prev; n; n = n->next) order_item(n->val);

  display_list_result[order_counter++] = si->item_num;
}

static int ready;

#define MAX_AVL_NODES (1<<18)
#define MAX_DEP_NODES (1<<17)
#define MAX_SORT_ITEMS (1024*6)

void isort_begin()
{
  if (!ready) {
    avl_init(MAX_AVL_NODES);
    dep_nodes_init(MAX_DEP_NODES);
    sort_items_init(MAX_SORT_ITEMS);
    display_list_result = (int*)malloc(MAX_SORT_ITEMS*sizeof(int));
    ready = 1;
  }

  display_list = 0;

  calln = 0;
  calln2 = 0;
}

void isort_add(int id, int flags, int x, int y, int z, int x2, int y2, int z2) {
  SortItem *si = alloc_sort_item();

  si->deps_list = 0;

  si->item_num = id;
  si->shape_num = id;

  si->x = x;
  si->y = y;
  si->z = z;
  si->xleft = x2;
  si->yfar = y2;
  si->ztop = z2;

  // Screenspace bounding box left extent    (LNT x coord)
  si->sxleft = si->xleft/4 - si->y/4;
  // Screenspace bounding box right extent   (RFT x coord)
  si->sxright= si->x/4 - si->yfar/4;

  // Screenspace bounding box top x coord    (LFT x coord)
  si->sxtop = si->xleft/4 - si->yfar/4;
  // Screenspace bounding box top extent     (LFT y coord)
  si->sytop = si->xleft/8 + si->yfar/8 - si->ztop;

  // Screenspace bounding box bottom x coord (RNB x coord)
  si->sxbot = si->x/4 - si->y/4;
  // Screenspace bounding box bottom extent  (RNB y coord)
  si->sybot = si->x/8 + si->y/8 - si->z;

  si->flags = flags;
  if (!(z2-z)) si->flags |= F_FLAT;

  display_list = avl_insert(display_list, si, si_item_compareA);
}

// following is the most CPU taxing part of this code
// it can be greatly optimized using the fact that most
// items remain static and don't move
static void add_deps() {
  SortItem *a, *b, *end=sort_items+sort_items_used;
  for (a=sort_items; a<end; a++) {
    for (b=a; b<end; b++) {
      if (overlap(a,b)) {
        if (item_compareB(a,b)) { // which is infront?
          dep_insert_sorted(b, a); // a is behind b
        } else {
          dep_push_back(a, b);
        }
      }
    }
  }
}

void produce_display_list_result() {
  order_counter = 0;  // Reset the order_counter
  avl_apply(display_list, order_item);
}

int isort_end() {
  add_deps();
  produce_display_list_result();

  avl_clear();
  dep_nodes_clear();
  sort_items_clear();

  printf("%d, %d\n", calln, calln2);

  return order_counter;
}

int *isort_result() {
  return display_list_result;
}

void isort_free_result() {
  //free(display_list_result);
}

#if 0
int main(int, char **) {
  isort_begin();

  isort_add(0,0, 0,0,0, 10,10,10);
  isort_add(0,0, 0,0,5, 10,10,10);
  isort_add(0,0, 10,10,5, 10,10,10);

  isort_end();

  isort_free_result();

  printf("goodbye! (%d)\n", display_list_result_size);
  return 0;
}
#endif