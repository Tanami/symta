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

#define CANT_HAPPEN() do { assert(false); } while(0)
#define CANT_HAPPEN_MSG(msg) do { assert(msg && false); } while(0)

struct SortItem;

struct DependsList
{
  struct Node {
    Node    *next;
    Node    *prev;
    SortItem  *val;
    Node() : next(0), prev(0), val(0) { }
  };

  Node *list;
  Node *tail;
  Node *unused; 

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
      tail->next = unused; 
      unused = list;
      tail = 0;
      list = 0;
    }
  }

  void push_back(SortItem *other)
  {
    if (!unused) unused = new Node();
    Node *nn = unused;
    unused = unused->next;
    nn->val = other;

    // Put it at the end
    if (tail) tail->next = nn;
    if (!list) list = nn;
    nn->next = 0;
    nn->prev = tail;
    tail = nn;
  }

  void insert_sorted(SortItem *other);

  DependsList() : list(0), tail(0), unused(0) { }

  ~DependsList() { 
    clear();
    while (unused)  {
      Node *n = unused->next;
      delete unused;
      unused = n;
    }
  }
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
  bool  occl   : 1;     // Needs 1 bit  2
  bool  solid  : 1;     // Needs 1 bit  3
  bool  draw   : 1;     // Needs 1 bit  4
  bool  anim   : 1;     // Needs 1 bit  7
  bool  trans  : 1;     // Needs 1 bit  8

  bool  occluded : 1;   // Set true if occluded

  int  order;    // Rendering order. -1 is not yet drawn

  DependsList depends;

  inline bool overlap(const SortItem &si2) const; //we overlap si2
  inline bool occludes(const SortItem &si2) const; //we occlude si2?
  inline bool operator<(const SortItem& si2) const;
  inline bool ListLessThan(const SortItem* other) const;
};


int calln;

void DependsList::insert_sorted(SortItem *other)
{
  if (!unused) unused = new Node();
  Node *nn = unused;
  unused = unused->next;
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
  int point_top_diff[2] = { sxtop - si2.sxbot, sytop - si2.sybot };
  int point_bot_diff[2] = { sxbot - si2.sxtop, sybot - si2.sytop };

  // This function is a bit of a hack. It uses dot products between
  // points and the lines. Nothing is normalized since that isn't 
  // important

  // 'normal' of top  left line ( 2,-1) of the bounding box
  int dot_top_left = point_top_diff[0] + point_top_diff[1] * 2;

  // 'normal' of top right line ( 2, 1) of the bounding box
  int dot_top_right = -point_top_diff[0] + point_top_diff[1] * 2;

  // 'normal' of bot  left line (-2,-1) of the bounding box
  int dot_bot_left =  point_bot_diff[0] - point_bot_diff[1] * 2;

  // 'normal' of bot right line (-2, 1) of the bounding box
  int dot_bot_right = -point_bot_diff[0] - point_bot_diff[1] * 2;

  bool right_clear = sxright <= si2.sxleft;
  bool left_clear = sxleft >= si2.sxright;
  bool top_left_clear = dot_top_left >= 0;
  bool top_right_clear = dot_top_right >= 0;
  bool bot_left_clear = dot_bot_left >= 0;
  bool bot_right_clear = dot_bot_right >= 0;

  bool clear = right_clear | left_clear | 
    bot_right_clear | bot_left_clear |
    top_right_clear | top_left_clear;

  return !clear;
}

// Check to see if we occlude si2
inline bool SortItem::occludes(const SortItem &si2) const
{
  const int point_top_diff[2] = { sxtop - si2.sxtop, sytop - si2.sytop };
  const int point_bot_diff[2] = { sxbot - si2.sxbot, sybot - si2.sybot };

  // This function is a bit of a hack. It uses dot products between
  // points and the lines. Nothing is normalized since that isn't 
  // important

  // 'normal' of top left line ( 2, -1) of the bounding box
  const int dot_top_left = point_top_diff[0] + point_top_diff[1] * 2;

  // 'normal' of top right line ( 2, 1) of the bounding box
  const int dot_top_right = -point_top_diff[0] + point_top_diff[1] * 2;

  // 'normal' of bot  left line (-2,-1) of the bounding box
  const int dot_bot_left =  point_bot_diff[0] - point_bot_diff[1] * 2;

  // 'normal' of bot right line (-2, 1) of the bounding box
  const int dot_bot_right = -point_bot_diff[0] - point_bot_diff[1] * 2;


  const bool right_res = sxright >= si2.sxright;
  const bool left_res = sxleft <= si2.sxleft;
  const bool top_left_res = dot_top_left <= 0;
  const bool top_right_res = dot_top_right <= 0;
  const bool bot_left_res = dot_bot_left <= 0;
  const bool bot_right_res = dot_bot_right <= 0;

  const bool occluded = right_res & left_res & 
    bot_right_res & bot_left_res &
    top_right_res & top_left_res;

  return occluded;
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

    // Occludes always get drawn first
    if (si1.occl != si2.occl) return si1.occl > si2.occl;

    // 32x32 flats get drawn first
    if (si1.f32x32 != si2.f32x32) return si1.f32x32 > si2.f32x32;
  } else { // Mixed, or non flat
    // Clearly in z
    if (si1.ztop <= si2.z) return true;
    else if (si1.z >= si2.ztop) return false;
  }

  // Clearly in x?
  if (si1.x <= si2.xleft) return true;
  else if (si1.xleft >= si2.x) return false;

  // Clearly in y?
  if (si1.y <= si2.yfar) return true;
  else if (si1.yfar >= si2.y) return false;

  // Are overlapping in all 3 dimentions if we come here

  // Overlapping z-bottom check
  // If an object's base (z-bottom) is higher another's, it should be rendered after.
  // This check must be on the z-bottom and not the z-top because two objects with the
  // same z-position may have different heights (think of a mouse sorting vs the Avatar).
  if (si1.z < si2.z) return true;
  else if (si1.z > si2.z) return false;

  // Biased Clearly in z
  if ((si1.ztop+si1.z)/2 <= si2.z) return true;
  else if (si1.z >= (si2.ztop+si2.z)/2) return false;

  // Biased Clearly X
  if ((si1.x+si1.xleft)/2 <= si2.xleft) return true;
  else if (si1.xleft >= (si2.x+si2.xleft)/2) return false;

  // Biased Clearly Y
  if ((si1.y+si1.yfar)/2 <= si2.yfar) return true;
  else if (si1.yfar >= (si2.y+si2.yfar)/2) return false;

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
class ItemSorter
{
  SortItem  *items;
  SortItem  *items_tail;
  SortItem  *items_unused;

  int order_counter;

public:
  ItemSorter();
  ~ItemSorter();

  enum HitFace {
    X_FACE, Y_FACE, Z_FACE
  };

  // Begin creating the display list
  void BeginDisplayList();

  void AddItem(int id, int flags, int x, int y, int z, int x2, int y2, int z2);

  int *OrderDisplayList();    // Finishes the display list

  int DisplayListSize() {return order_counter;}

  // Trace and find an object. Returns objid.
  // If face is non-NULL, also return the face of the 3d bbox (x,y) is on
  //int Trace(int x, int y, HitFace* face = 0, bool item_highlight=false);

private:
  void OrderSortItem(SortItem *);
};


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
  // Get the shapes, if required
  //if (!shapes) shapes = GameData::get_instance()->getMainShapes();

  // 
  if (items_tail) {
    items_tail->next = items_unused;
    items_unused = items;
  }
  items = 0;
  items_tail = 0;

  // Set the RenderSurface, and reset the item list
  //surf = rs;
  order_counter = 0;
}

SortItem *prev = 0;

int *ItemSorter::OrderDisplayList()
{
 int i = 0;
 int *result;
  prev = 0;
  SortItem *it = items;
  SortItem *end = 0;
  order_counter = 0;  // Reset the order_counter
  while (it != end)
  {
    if (it->order == -1) OrderSortItem(it);
    it = it->next;
  }

 result = (int*)malloc(order_counter*sizeof(int));

 it = items;
  end = 0;
 while (it != end)
  {
   if (it->order>=0) result[it->order] = it->item_num;
     it = it->next;
 }
 return result;
}

void ItemSorter::OrderSortItem(SortItem *si)
{
  // Don't paint this, or dependencies if occluded
  if (si->occluded) return;

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

#define FLAG_TRANSLUCENT     0x01
#define FLAG_ANIMATED        0x02
#define FLAG_SOLID           0x04
#define FLAG_OCCLUDER        0x08
#define FLAG_DRAW_FIRST      0x10
#define FLAG_DITHER          0x40

void ItemSorter::AddItem(int id, int flags,
                         int x, int y, int z,
                         int x2, int y2, int z2)
{
  if (!items_unused) items_unused = new SortItem(0);
  SortItem *si = items_unused;

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
  si->occl = (flags&FLAG_OCCLUDER) ? 1 : 0;
  si->anim = (flags&FLAG_ANIMATED) ? 1 : 0;
  si->trans = (flags&FLAG_TRANSLUCENT) ? 1 : 0;
  si->occluded = 0;
  si->order = -1;

  SortItem *addpoint = 0;
  for (SortItem * si2 = items; si2 != 0; si2 = si2->next)
  {
    // we insert before the first item that has higher z than us
    if (!addpoint && si->ListLessThan(si2)) addpoint = si2;

    // Doesn't overlap
    if (si2->occluded || !si->overlap(*si2)) continue;

    // Attempt to find which is infront
    if (*si < *si2)
    {
      // si2 occludes si (us)
      if (si2->occl && si2->occludes(*si))
      {
        // No need to do any more checks, this isn't visible
        si->occluded = true;
        break;
      }

      // si1 is behind si2, so add it to si2's dependency list
      si2->depends.insert_sorted(si);
    }
    else
    {
      // ss occludes si2. Sadly, we can't remove it from the list.
      if (si->occl && si->occludes(*si2)) si2->occluded = true;
      // si2 is behind si1, so add it to si1's dependency list
      else si->depends.push_back(si2);
    }
  }

  // Add it to the list
  items_unused = items_unused->next;

  // have a position
  //addpoint = 0;
  if (addpoint)
  {
    si->next = addpoint;
    si->prev = addpoint->prev;
    addpoint->prev = si;
    if (si->prev) si->prev->next = si;
    else items = si;
  }
  // Add it to the end of the list
  else 
  {
    if (items_tail) items_tail->next = si;
    if (!items) items = si;
    si->next = 0;
    si->prev = items_tail;
    items_tail = si;
  }

}

extern "C" {

static ItemSorter *isorter;

static int *draw_list;
static int draw_list_size;

void isort_begin()
{
  calln = 0;
  isorter = new ItemSorter;
  isorter->BeginDisplayList();
}

void isort_add(int id, int flags, int x, int y, int z, int x2, int y2, int z2)
{
  isorter->AddItem(id,flags,x,y,z,x2,y2,z2);
}

int isort_end() {
  draw_list = isorter->OrderDisplayList();
  draw_list_size = isorter->DisplayListSize();

  delete isorter;
  isorter = 0;

  printf("%d\n", calln);

  return draw_list_size;
}

int *isort_result() {
  return draw_list;
}

void isort_free_result() {
  free(draw_list);
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

  printf("goodbye! (%d)\n", draw_list_size);
  return 0;
}
#endif