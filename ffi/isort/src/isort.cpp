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


#include <cassert>
#include <fstream>
#include <cstdio>
#include <list>
#include <vector>

static int calln;
static int calln2;

#include "avl.cpp"


#define FLAG_TRANSLUCENT     0x01
#define FLAG_ANIMATED        0x02
#define FLAG_SOLID           0x04
#define FLAG_DRAW_FIRST      0x10
#define FLAG_DITHER          0x40


#define CANT_HAPPEN() do { assert(false); } while(0)
#define CANT_HAPPEN_MSG(msg) do { assert(msg && false); } while(0)

struct SortItem;

struct Node {
  Node    *next;
  Node    *prev;
  SortItem  *val;
  Node() : next(0), prev(0), val(0) { }
};

#define MAX_NODES (1<<17)
static Node nodes[MAX_NODES];
static Node *unused_nodes;

static void init_nodes() {
  int i;
  for (i=0; i<MAX_NODES; i++) {
    Node *n = &nodes[i];
    n->next = unused_nodes;
    unused_nodes = n;
  }
}

struct DependsList
{
  Node *list, *tail;

  DependsList() : list(0), tail(0) { }

  ~DependsList() { clear(); }

  struct iterator {
    Node *n;
    SortItem *&operator *() { return n->val; }
    iterator(Node *node) : n(node) { }
    iterator &operator++() { n = n->next; return *this; }
    bool operator != (const iterator &o) { return n != o.n; }
  };

  iterator begin() { return iterator(list); }
  iterator end() { return iterator(0); }

  void clear() {
    if (tail) {
      tail->next = unused_nodes; 
      unused_nodes = list;
      tail = 0;
      list = 0;
    }
  }

  void push_back(SortItem *other);
  void insert_sorted(SortItem *other);
};

struct SortItem
{
  SortItem(SortItem *n) : next(n), prev(0), item_num(0), order(-1), depends() { }

  SortItem *next, *prev;

  int item_num; // Owner item number
  int shape_num;


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

  int  x, xleft; // Worldspace bounding box x (xright = x)
  int  y, yfar;  // Worldspace bounding box y (ynear = y)
  int  z, ztop;  // Worldspace bounding box z (ztop = z)

  int  sxleft;   // Screenspace bounding box left extent    (LNT x coord)
  int  sxright;  // Screenspace bounding box right extent   (RFT x coord)

  int  sxtop;    // Screenspace bounding box top x coord    (LFT x coord)
  int  sytop;    // Screenspace bounding box top extent     (LFT y coord)

  int  sxbot;    // Screenspace bounding box bottom x coord (RNB x coord) ss origin
  int  sybot;    // Screenspace bounding box bottom extent  (RNB y coord) ss origin

  bool  f32x32 : 1;     // Needs 1 bit  0
  bool  flat   : 1;     // Needs 1 bit  1
  bool  solid  : 1;     // Needs 1 bit  3
  bool  draw   : 1;     // Needs 1 bit  4
  bool  anim   : 1;     // Needs 1 bit  7
  bool  trans  : 1;     // Needs 1 bit  8

  int  order;    // Rendering order. -1 is not yet drawn

  DependsList depends;

  inline bool overlap(const SortItem &si2) const; //we overlap si2
  inline bool operator<(const SortItem& si2) const;
  inline bool ListLessThan(const SortItem* other) const;
};

inline void DependsList::push_back(SortItem *other)
{
  Node *nn = unused_nodes;
  unused_nodes = unused_nodes->next;
  nn->val = other;

  // Put it at the end
  if (tail) tail->next = nn;
  if (!list) list = nn;
  nn->next = 0;
  nn->prev = tail;
  tail = nn;
}

inline void DependsList::insert_sorted(SortItem *other)
{
  Node *nn = unused_nodes;
  unused_nodes = unused_nodes->next;
  nn->val = other;

  for (Node *n = list; n != 0; n = n->next)
  {
    // Get the insert point... which is before the first item that has higher z than us
    if (other->ListLessThan(n->val)) 
    {
      nn->next = n;
      nn->prev = n->prev;
      n->prev = nn;
      if (nn->prev) nn->prev->next = nn;
      else list = nn;
      return;
    }
  }

  // No suitable, so put at end
  if (tail) tail->next = nn;
  if (!list) list = nn;
  nn->next = 0;
  nn->prev = tail;
  tail = nn;
}


// Comparison for the sorted lists
inline bool SortItem::ListLessThan(const SortItem* other) const
{
  if (z < other->z) return 1;
  if (z > other->z) return 0;
  return x < other->x || (x == other->x && y < other->y);
}

// Check to see if we overlap si2
inline bool SortItem::overlap(const SortItem &si2) const
{
  int dt0,dt1,db0,db1;

  if(sxright <= si2.sxleft) return 0; //right_clear
  if(sxleft >= si2.sxright) return 0; //left_clear

  // This function is a bit of a hack. It uses dot products between
  // points and the lines. Nothing is normalized since that isn't 
  // important

  dt0 = sxtop - si2.sxbot;
  dt1 = sytop - si2.sybot;

  // 'normal' of top  left line ( 2,-1) of the bounding box
  if(dt0 + dt1*2 >= 0) return 0;

  // 'normal' of top right line ( 2, 1) of the bounding box
  if(-dt0 + dt1*2 >= 0) return 0;

  db0 = sxbot - si2.sxtop;
  db1 = sybot - si2.sytop;

  // 'normal' of bot  left line (-2,-1) of the bounding box
  if(db0 - db1*2 >= 0) return 0;

  // 'normal' of bot right line (-2, 1) of the bounding box
  if(-db0 - db1*2 >= 0) return 0;

  return 1;
}

inline bool SortItem::operator<(const SortItem& si2) const {
  const SortItem& si1 = *this;

  // Specialist z flat handling
  if (si1.flat && si2.flat) {
    // Differing z is easy for flats
    if (si1.ztop != si2.ztop) return si1.ztop < si2.ztop;

    // Equal z

    // Animated always gets drawn after
    if (si1.anim != si2.anim) return si1.anim < si2.anim;

    // Trans always gets drawn after
    if (si1.trans != si2.trans) return si1.trans < si2.trans;

    // Draw always gets drawn first
    if (si1.draw != si2.draw) return si1.draw > si2.draw;

    // Solid always gets drawn first
    if (si1.solid != si2.solid) return si1.solid > si2.solid;

    // 32x32 flats get drawn first
    if (si1.f32x32 != si2.f32x32) return si1.f32x32 > si2.f32x32;
  } else { // Mixed, or non flat
    // Clearly in z
    if (si1.ztop <= si2.z) return true;
    if (si1.z >= si2.ztop) return false;
  }

  // Clearly in x?
  if (si1.x <= si2.xleft) return true;
  if (si1.xleft >= si2.x) return false;

  // Clearly in y?
  if (si1.y <= si2.yfar) return true;
  if (si1.yfar >= si2.y) return false;

  // Are overlapping in all 3 dimentions if we come here

  // Overlapping z-bottom check
  // If an object's base (z-bottom) is higher another's, it should be rendered after.
  // This check must be on the z-bottom and not the z-top because two objects with the
  // same z-position may have different heights (think of a mouse sorting vs the Avatar).
  if (si1.z < si2.z) return true;
  if (si1.z > si2.z) return false;

  // Biased Clearly in z
  if ((si1.ztop+si1.z)/2 <= si2.z) return true;
  if (si1.z >= (si2.ztop+si2.z)/2) return false;

  // Biased Clearly X
  if ((si1.x+si1.xleft)/2 <= si2.xleft) return true;
  if (si1.xleft >= (si2.x+si2.xleft)/2) return false;

  // Biased Clearly Y
  if ((si1.y+si1.yfar)/2 <= si2.yfar) return true;
  if (si1.yfar >= (si2.y+si2.yfar)/2) return false;

  // Partial in X + Y front
  if (si1.x + si1.y != si2.x + si2.y) return (si1.x + si1.y < si2.x + si2.y);

  // Partial in X + Y back
  if (si1.xleft + si1.yfar != si2.xleft + si2.yfar)
    return (si1.xleft + si1.yfar < si2.xleft + si2.yfar);

  // Partial in x?
  if (si1.x != si2.x) return si1.x < si2.x;

  // Partial in y?
  if (si1.y != si2.y) return si1.y < si2.y;

  return si1.shape_num < si2.shape_num;
}



// 
// ItemSorter
//
class ItemSorter {
  SortItem  *items;
  SortItem  *items_tail;
  SortItem  *items_unused;

  int order_counter;

public:
  ItemSorter();
  ~ItemSorter();

  void BeginDisplayList();
  void AddItem(int id, int flags, int x, int y, int z, int x2, int y2, int z2);
  void OrderDisplayList();
  void OrderSortItem(SortItem *);
};

static ItemSorter *isorter;
static avl_node *display_list = 0;
static int *display_list_result;
static int display_list_result_size;

ItemSorter::ItemSorter() : items(0), items_tail(0), items_unused(0)
{
  int i = 2048;
  while (i--) items_unused = new SortItem(items_unused);
}

ItemSorter::~ItemSorter()
{
  // 
  if (items_tail) {
    items_tail->next = items_unused;
    items_unused = items;
  }
  items = 0;
  items_tail = 0;

  while (items_unused)
  {
    SortItem *next = items_unused->next;
    delete items_unused;
    items_unused = next;
  }

  if (items) delete items;
}

void ItemSorter::BeginDisplayList()
{
  if (items_tail) {
    items_tail->next = items_unused;
    items_unused = items;
  }
  items = 0;
  items_tail = 0;
}

static int si_ListLessThan(void *aa, void*bb) {
  SortItem *a = (SortItem*)aa;
  SortItem *b = (SortItem*)bb;
  return a->ListLessThan(b);
}


static void add_deps(avl_node *t, SortItem *a) {
  SortItem *b;

  if(!t) return;

  add_deps(t->left,a);

  b = (SortItem*)t->data;

  if (a->overlap(*b)) {
    if (*a < *b) { // which is infront?
      b->depends.insert_sorted(a); // a is behind b
    } else {
      a->depends.push_back(b);
    }
  }
  add_deps(t->right,a);
}

static void order_item(void *aa) {
  SortItem *it = (SortItem*)aa;
  if (it->order == -1) isorter->OrderSortItem(it);
}

static void generate_result(void *aa) {
  SortItem *it = (SortItem*)aa;
  if (it->order>=0) display_list_result[it->order] = it->item_num;
}

void ItemSorter::OrderDisplayList()
{
  order_counter = 0;  // Reset the order_counter
  avl_apply(display_list, order_item);
  display_list_result = (int*)malloc(order_counter*sizeof(int));
  avl_apply(display_list, generate_result);
  display_list_result_size = order_counter;
}

void ItemSorter::OrderSortItem(SortItem *si)
{
  // Resursion, detection
  si->order = -2;
  
  // Iterate through our dependancies, and paint them, if possible
  DependsList::iterator it = si->depends.begin();
  DependsList::iterator end = si->depends.end();
  while (it != end)
  {
    // Well, it can't. Implies infinite recursive sorting.
    //if ((*it)->order == -2) CANT_HAPPEN_MSG("Detected cycle in the dependency graph");

    if ((*it)->order == -1) OrderSortItem((*it));

    ++it;
  }

  // Set our painting order
  si->order = order_counter;
  order_counter++;
}

void ItemSorter::AddItem(int id, int flags,
                         int x, int y, int z,
                         int x2, int y2, int z2)
{
  if (!items_unused) items_unused = new SortItem(0);
  SortItem *si = items_unused;
  items_unused = items_unused->next;

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

  si->f32x32 = 0;
  si->flat = !(z2-z);

  si->draw = (flags&FLAG_DRAW_FIRST) ? 1 : 0;
  si->solid = (flags&FLAG_SOLID) ? 1 : 0;
  si->anim = (flags&FLAG_ANIMATED) ? 1 : 0;
  si->trans = (flags&FLAG_TRANSLUCENT) ? 1 : 0;
  si->order = -1;

  add_deps(display_list,si);
  display_list = avl_insert(display_list, si, si_ListLessThan);

  if (items_tail) items_tail->next = si;
  if (!items) items = si;
  si->next = 0;
  si->prev = items_tail;
  items_tail = si;
}

extern "C" {

static int ready;

#define MAX_AVL_NODES (1<<18)

void isort_begin()
{
  if (!ready) {
    init_nodes();
    avl_init(MAX_AVL_NODES);
    ready = 1;
  }

  display_list = 0;

  calln = 0;
  calln2 = 0;

  isorter = new ItemSorter;
  isorter->BeginDisplayList();
}

void isort_add(int id, int flags, int x, int y, int z, int x2, int y2, int z2)
{
  isorter->AddItem(id,flags,x,y,z,x2,y2,z2);
}

int isort_end() {
  isorter->OrderDisplayList();

  delete isorter;
  isorter = 0;

  avl_clear();

  printf("%d, %d\n", calln, calln2);

  return display_list_result_size;
}

int *isort_result() {
  return display_list_result;
}

void isort_free_result() {
  free(display_list_result);
}

} //extern "C" 

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