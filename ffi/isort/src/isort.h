/*
Copyright (C) 2003-2005 The Pentagram team

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

#ifndef ITEMSORTER_H
#define ITEMSORTER_H

// C Standard Library and STL
#include <cassert>
#include <fstream>
#include <cstdio>
#include <list>
#include <vector>

#define CANT_HAPPEN() do { assert(false); } while(0)
#define CANT_HAPPEN_MSG(msg) do { assert(msg && false); } while(0)

typedef	unsigned char	uint8;
typedef	unsigned short	uint16;
typedef	unsigned int	uint32;

typedef	signed char		sint8;
typedef	signed short	sint16;
typedef	signed int	sint32;


struct SortItem;

class ItemSorter
{
	//MainShapeArchive	*shapes;
	//RenderSurface	*surf;

	SortItem	*items;
	SortItem	*items_tail;
	SortItem	*items_unused;
	sint32		sort_limit;

	int order_counter;

public:
	ItemSorter();
	~ItemSorter();

	enum HitFace {
		X_FACE, Y_FACE, Z_FACE
	};

	// Begin creating the display list
	void BeginDisplayList();

 void AddItem(int id, int flags,
               int x, int y, int z,
               int x2, int y2, int z2);

	int *OrderDisplayList();		// Finishes the display list

 int DisplayListSize() {return order_counter;}

	// Trace and find an object. Returns objid.
	// If face is non-NULL, also return the face of the 3d bbox (x,y) is on
	uint16 Trace(sint32 x, sint32 y, HitFace* face = 0, bool item_highlight=false );

private:
	void OrderSortItem(SortItem	*);
};


#endif //ITEMSORTER_H
