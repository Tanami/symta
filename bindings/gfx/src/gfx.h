#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define GFX_RGB     0
#define GFX_RGBA    1
#define GFX_INDEXED 2

typedef struct {
  int w; // width
  int h; // height
  int type;
  int key; // color key
  int hs_x; // hot spot x
  int hs_y; // hot spot y
  uint32_t *data; // pixels
  uint32_t *cmap; // color map
} gfx_t;

gfx_t *new_gfx(int w, int h, int type);
uint32_t gfx_get(gfx_t *gfx, int x, int y);
void gfx_set(gfx_t *gfx, int x, int y, uint32_t color);
void gfx_clear(gfx_t *gfx, uint32_t color);

gfx_t *load_png(char *filename);
void save_png(char *filename, gfx_t *gfx);

#define unless(x) if(!(x))
#define times(i,e) for(i=0; i<(e); i++)

#define R8G8B8(R,G,B) (((R)<<16)|((G)<<8)|(B))
#define R8G8B8A8(R,G,B,A) (((A)<<24)|((R)<<16)|((G)<<8)|(B))

#define fromR8G8B8(R,G,B,C) do { \
  uint32_t _fromC = (C)&0xFFFFFFFF; \
  B = (((_fromC)>> 0)&0xFF); \
  G = (((_fromC)>> 8)&0xFF); \
  R = (((_fromC)>>16)&0xFF); \
 } while (0)
#define fromR8G8B8A8(R,G,B,A,C) do { \
  uint32_t _fromC = (C)&0xFFFFFFFF; \
  B = (((_fromC)>> 0)&0xFF); \
  G = (((_fromC)>> 8)&0xFF); \
  R = (((_fromC)>>16)&0xFF); \
  A = (((_fromC)>>24)&0xFF); \
 } while (0)
#define fromR5G6B5(R,G,B,C) do { \
  uint32_t _fromC = (C)&0xFFFF; \
  B = ((((_fromC)>> 0)&0x1f)<<3)|0x7; \
  G = ((((_fromC)>> 5)&0x3f)<<2)|0x3; \
  R = ((((_fromC)>>11)&0x1f)<<3)|0x7; \
 } while (0)
#define fromR5G5B5A1(A,R,G,B,C) do { \
  uint32_t _fromC = (C)&0xFFFF; \
  A = (_fromC)&1; \
  B = ((((_fromC)>> 1)&0x1f)<<3)|0x7; \
  G = ((((_fromC)>> 6)&0x1f)<<3)|0x7; \
  R = ((((_fromC)>>11)&0x1f)<<3)|0x7; \
 } while (0)
#define fromA1R5G5B5(A,R,G,B,C) do { \
  uint32_t _fromC = (C)&0xFFFF; \
  B = ((((_fromC)>> 0)&0x1f)<<3)|0x7; \
  G = ((((_fromC)>> 5)&0x1f)<<3)|0x7; \
  R = ((((_fromC)>>10)&0x1f)<<3)|0x7; \
  A = ((_fromC)>>15)&1; \
 } while (0)
#define fromA4R4G4B4(A,R,G,B,C) do { \
  uint32_t _fromC = (C)&0xFFFF; \
  B = ((((_fromC)>> 0)&0xf)<<4)|0xf; \
  G = ((((_fromC)>> 4)&0xf)<<4)|0xf; \
  R = ((((_fromC)>> 8)&0xf)<<4)|0xf; \
  A = ((((_fromC)>>12)&0xf)<<4)|0xf; \
  B |= B>>4; \
  G |= G>>4; \
  R |= R>>4; \
  A |= A>>4; \
 } while (0)

