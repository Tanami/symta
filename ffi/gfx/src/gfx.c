#include "gfx.h"

void *ffi_alloc_(int size) {
  return malloc(size);
}

void ffi_free_(void *ptr) {
  free(ptr);
}

void ffi_memset_(void *ptr, int value, uint32_t size) {
  memset(ptr, value, size);
}

gfx_t *new_gfx(uint32_t w, uint32_t h) {
  gfx_t *gfx = (gfx_t*)malloc(sizeof(gfx_t));
  if (!gfx) return 0;
  gfx->data = (uint32_t*)malloc(w*h*sizeof(uint32_t));
  if (!gfx->data) {
    free(gfx);
    return 0;
  }
  gfx->w = w;
  gfx->h = h;
  gfx->cmap = 0;
  gfx->x = 0;
  gfx->y = 0;
  gfx->bflags = 0;
  gfx->recolor_map = 0;
  gfx->blit_bright = 0;
  gfx->zdata = 0;
  return gfx;
}

void free_gfx(gfx_t *gfx) {
  if (gfx->cmap) free(gfx->cmap);
  free(gfx->data);
  free(gfx);
}

void gfx_resize(gfx_t *gfx, uint32_t w, uint32_t h) {
  free(gfx->data);
  gfx->data = (uint32_t*)malloc(w*h*sizeof(uint32_t));
  gfx->w = w;
  gfx->h = h;
  if (gfx->zdata) gfx->zdata = 0;
}

uint32_t gfx_w(gfx_t *gfx) {
  return gfx->w;
}

uint32_t gfx_h(gfx_t *gfx) {
  return gfx->h;
}

void *gfx_enable_cmap(gfx_t *gfx) {
  unless (gfx->cmap) {
    gfx->cmap = malloc(sizeof(uint32_t)*GFX_CMAP_SIZE);
  }
  return gfx->cmap;
}

uint32_t *gfx_cmap(gfx_t *gfx) {
  return gfx->cmap;
}

void gfx_set_cmap(gfx_t *gfx, uint32_t *cmap) {
  unless (gfx->cmap) {
    gfx->cmap = malloc(sizeof(uint32_t)*GFX_CMAP_SIZE);
  }
  memcpy(gfx->cmap, cmap, sizeof(uint32_t)*GFX_CMAP_SIZE);
}

int gfx_x(gfx_t *gfx) {
  return gfx->x;
}

int gfx_y(gfx_t *gfx) {
  return gfx->y;
}

void gfx_set_xy(gfx_t *gfx, int x, int y) {
  gfx->x = x;
  gfx->y = y;
}

uint32_t gfx_get(gfx_t *gfx, int x, int y) {
  if ((uint32_t)x >= (uint32_t)gfx->w) return 0;
  if ((uint32_t)y >= (uint32_t)gfx->h) return 0;
  return gfx->data[gfx->w*y+x];
}

void gfx_set(gfx_t *gfx, int x,int y, uint32_t color) {
  if ((uint32_t)x >= (uint32_t)gfx->w) return;
  if ((uint32_t)y >= (uint32_t)gfx->h) return;
  gfx->data[gfx->w*y+x] = color;
}

void gfx_clear(gfx_t *gfx, uint32_t color) {
  int x, y;
  for (y = 0; y < gfx->h; y++) {
    for (x = 0; x < gfx->w; x++) {
      gfx->data[gfx->w*y+x] = color;
    }
  }
}

static void gfx_hline(gfx_t *gfx, uint32_t color, int x, int y, int length) {
  int i, e;
  int w = gfx->w;
  int h = gfx->h;
  uint32_t *d = gfx->data;
  if ((uint32_t)y >= (uint32_t)h) return;
  if (x < 0) {
    length = x + length;
    x = 0;
  }
  if (x+length > w) length = w - x;
  i = y*w + x;
  e = i + length;
  for (; i < e; i++) {
    d[i] = color;
  }
}

static void gfx_vline(gfx_t *gfx, uint32_t color, int x, int y, int length) {
  int i, e;
  int w = gfx->w;
  int h = gfx->h;
  uint32_t *d = gfx->data;
  if ((uint32_t)x >= (uint32_t)w) return;
  if (y < 0) {
    length = y + length;
    y = 0;
  }
  if (y+length > h) length = h - y;
  i = y*w + x;
  e = i + length*w;
  for (; i < e; i+=w) {
    d[i] = color;
  }
}

void gfx_line(gfx_t *gfx, uint32_t color, int sx, int sy, int dx, int dy) {
  int t, x, y, xlen, ylen, incr, p = 0;
  if (sx == dx) {
    if (sy < dy) gfx_vline(gfx, color, sx, sy, dy - sy + 1);
    else gfx_vline(gfx, color, dx, dy, sy - dy + 1);
    return;
  }
  if (sy == dy) {
    if (sx < dx) gfx_vline(gfx, color, sx, sy, dx - sx + 1);
    else gfx_vline(gfx, color, dx, dy, sx - dx + 1);
    return;
  }
  if (sy > dy) {
    t = sx;
    sx = dx;
    dx = t;
    t = sy;
    sy = dy;
    dy = t;
  }
  ylen = dy - sy;

  if (sx > dx) {
    xlen = sx - dx;
    incr = -1;
  } else {
    xlen = dx - sx;
    incr = 1;
  }

  y = sy;
  x = sx;

  if (xlen > ylen) {
    if (sx > dx) {
      t = sx;
      sx = dx;
      dx = t;
      y = dy;
    }

    p = (ylen << 1) - xlen;
    for (x = sx; x < dx; ++x) {
      gfx_set(gfx, x, y, color);
      if (p >= 0) {
        y += incr;
        p += (ylen - xlen) << 1;
      } else {
        p += (ylen << 1);
      }
    }
    return;
  }

  if (ylen > xlen) {
    p = (xlen << 1) - ylen;

    for (y = sy; y < dy; ++y) {
      gfx_set(gfx, x, y, color);
      if (p >= 0) {
        x += incr;
        p += (xlen - ylen) << 1;
      } else {
        p += (xlen << 1);
      }
    }
		
    return;
  }

  if (ylen == xlen) {
    while (y != dy) {
      gfx_set(gfx, x, y, color);
      x += incr;
      ++y;
    }
  }
}

void gfx_rect(gfx_t *gfx, uint32_t color, int fill, int x, int y, int w, int h) {
  if (fill) {
    int e = y+h;
    for (; y < e; y++) gfx_hline(gfx, color, x, y, w);
  } else {
    gfx_hline(gfx, color, x, y, w);
    gfx_hline(gfx, color, x, y+h - 1, w);
    gfx_vline(gfx, color, x, y+1, h-2);
    gfx_vline(gfx, color, x+w-1, y+1, h-2);
  }
}

static void gfx_circle_empty(gfx_t *gfx, uint32_t color, int x, int y, int r) {
  int p = 1 - r;
  int px = 0;
  int py = r;
  for (; px <= py + 1; px++) {
    gfx_set(gfx, x+px, y+py, color);
    gfx_set(gfx, x+px, y-py, color);
    gfx_set(gfx, x-px, y+py, color);
    gfx_set(gfx, x-px, y-py, color);
    gfx_set(gfx, x+py, y+px, color);
    gfx_set(gfx, x+py, y-px, color);
    gfx_set(gfx, x-py, y+px, color);
    gfx_set(gfx, x-py, y-px, color);
    if (p < 0) p += 2*px + 3;
    else {
      p += 2*(px - py) + 5;
      py--;
    }
  }
}

static void gfx_circle_filled(gfx_t *gfx, uint32_t color, int x, int y, int r) {
  int p = 1 - r;
  int px = 0;
  int py = r;
  for (; px <= py; px++) {
    gfx_vline(gfx, color, x+px, y,    py+1);
    gfx_vline(gfx, color, x+px, y-py, py);
    if (px != 0) {
      gfx_vline(gfx, color, x-px, y,    py+1);
      gfx_vline(gfx, color, x-px, y-py, py);
    }
    if (p < 0) p += 2*px + 3;
    else {
      p += 2*(px - py) + 5;
      py--;
      if (py >= px) {
        gfx_vline(gfx, color, x+py+1, y,    px+1);
        gfx_vline(gfx, color, x+py+1, y-px, px);
        gfx_vline(gfx, color, x-py-1, y,    px+1);
        gfx_vline(gfx, color, x-py-1, y-px, px);
      }
    }
  }
}

void gfx_circle(gfx_t *gfx, uint32_t color, int fill, int x, int y, int r) {
  if (fill) gfx_circle_filled(gfx, color, x, y, r);
  else gfx_circle_empty(gfx, color, x, y, r);
}

typedef struct lerp {
  double x;
  double i;
} lerp;

static void lerp_init(lerp *l, int sx, int ex, int first_step, int steps) {
  l->i = (double)(ex - sx) / steps;
  l->x = (double)sx + first_step*l->i;
}

static void lerp_advance(lerp *l) {
  l->x += l->i;
}

#define SWAP(x,y) do {int t_ = x; x = y; y = t_;} while(0)

#define TRIANGLE_ROW(a,b) do { \
  int x1 = (int)a.x; \
  int x2 = (int)b.x; \
  if (x1 < x2) gfx_hline(gfx, color, x1, y, x2-x1); \
  else gfx_hline(gfx, color, x2, y, x1-x2); \
} while (0)

void gfx_triangle(gfx_t *gfx, uint32_t color, int ax, int ay, int bx, int by, int cx, int cy) {
  int beg_y, cen_y, end_y;
  int y, e;
  lerp l, r;

  if(ax < 0 && bx < 0 && cx < 0) return;
  if(ax >= gfx->w && bx >= gfx->w && cx >= gfx->w) return;
  if(ax == bx && ax == cx) return;

  if (ay > by) {
    SWAP(ax,bx);
    SWAP(ay,by);
  }

  if (ay > cy) {
    SWAP(ax,cx);
    SWAP(ay,cy);
  }

  if(by > cy) {
    SWAP(bx,cx);
    SWAP(by,cy);
  }

  beg_y = ay;
  cen_y = by;
  end_y = cy;

  if(end_y == beg_y || end_y < 0 || beg_y >= gfx->h) return;

  if (beg_y < 0) {
    lerp_init(&r, ax, cx, -beg_y, end_y-beg_y);
    y = 0;
  } else {
    lerp_init(&r, ax, cx, 0, end_y-beg_y);
    y = beg_y;
  }

  if (y < cen_y) {
    if (beg_y < 0) lerp_init(&l, ax, bx,-beg_y, cen_y-beg_y);
    else lerp_init(&l, ax, bx, 0, cen_y-beg_y);

    if (cen_y > gfx->h) e = gfx->h;
    else e = cen_y;

    for (; y < e; ++y) {
      TRIANGLE_ROW(l,r);
      lerp_advance(&l);
      lerp_advance(&r);
    }
  }

  if(cen_y < end_y) {
    lerp_init(&l, bx, cx, y-cen_y, end_y-cen_y);
    if (end_y > gfx->h) end_y = gfx->h;
    
    for (; y < end_y; ++y) {
      TRIANGLE_ROW(l,r);
      lerp_advance(&l);
      lerp_advance(&r);
    }
  }
}

static int show_error = 1;

void gfx_set_bflags_clear(gfx_t *gfx) {
  gfx->bflags = 0;
}

void gfx_set_bflags_flip_x(gfx_t *gfx) {
  gfx->bflags |= GFX_BFLAGS_FLIP_X;
}

void gfx_set_bflags_flip_y(gfx_t *gfx) {
  gfx->bflags |= GFX_BFLAGS_FLIP_Y;
}

void gfx_set_blit_dither(gfx_t *gfx, int amount) {
  gfx->bflags |= GFX_BFLAGS_DITHER;
}

void gfx_set_blit_bright(gfx_t *gfx, int amount) {
  gfx->bflags |= GFX_BFLAGS_BRIGHTEN;
  gfx->blit_bright = amount;
}

void gfx_set_blit_z(gfx_t *gfx, uint32_t z) {
  gfx->blit_z = z;
}

void gfx_set_blit_rect(gfx_t *gfx, int x, int y, int w, int h) {
  gfx->bx = x;
  gfx->by = y;
  gfx->bw = w;
  gfx->bh = h;
  gfx->bflags|=GFX_BFLAGS_RECT;
}

void gfx_set_recolor_map(gfx_t *gfx, uint32_t *map) {
  gfx->recolor_map = map;
}

void gfx_set_zdata(gfx_t *gfx, uint32_t *zdata) {
  gfx->zdata = zdata;
}


#define DITHER dither && ((y&1) ^ (pd&1))

#define BRIGHTEN(R,G,B) \
  do { \
    R += bright; \
    G += bright; \
    B += bright; \
    if (bright > 0) { \
      if (R > 255) R = 255; \
      if (G > 255) G = 255; \
      if (B > 255) B = 255; \
    } else { \
      if (R < 0) R = 0; \
      if (G < 0) G = 0; \
      if (B < 0) B = 0; \
    } \
  } while(0)


#define begin_blit() \
  while (y < ey) { \
    pd = y*dw + x; \
    ex = pd + w; \
    ps = sy*sw + sx; \
    while (pd < ex) { \
      do { \
        if (zdata) { \
          if(zdata[pd]>z) break; \
        }

#define end_blit(output) \
        DC = (output); \
      } while (0); \
      pd += 1; \
      ps += xi; \
    } \
    y += 1; \
    sy += yi; \
  } \

//source and destination colors
#define SC s[ps]
#define DC d[pd]


void gfx_blit(gfx_t *gfx, int x, int y, gfx_t *src) {
  int i, r, g, b, a;
  gfx_t *dst = gfx;
  int cx = 0;
  int cy = 0;
  int cw = dst->w;
  int ch = dst->h;
  int ow;
  int oh;
  int xi; // x increment
  int yi; // y increment
  int ex = 0;
  int ey = 0;
  uint32_t *d = dst->data;
  int dw = dst->w;
  uint32_t *s = src->data;
  int sw = src->w;
  int sh = src->h;
  uint32_t *m = src->recolor_map ? src->recolor_map : src->cmap;
  int pd = 0; // destination pointer
  int ps = 0; // sorce pointer
  int flip_x = src->bflags&GFX_BFLAGS_FLIP_X;
  int flip_y = src->bflags&GFX_BFLAGS_FLIP_Y;
  int dither = src->bflags&GFX_BFLAGS_DITHER;
  int bright = 0;
  int sx, sy, w, h; //source rect
  uint32_t *zdata = dst->zdata;
  uint32_t z = src->blit_z+1;

  if (src->bflags & GFX_BFLAGS_RECT) {
    sx = src->bx;
    sy = src->by;
    w = src->bw;
    h = src->bh;
  } else {
    sx = 0;
    sy = 0;
    w = src->w;
    h = src->h;
  }

  if (src->bflags & GFX_BFLAGS_BRIGHTEN) {
    bright = src->blit_bright;
  }

  src->bflags = 0;
  src->recolor_map = 0;

  x += flip_x ? -src->x : src->x;
  y += flip_y ? -src->y : src->y;

  if (sx < 0) {
    w += sx;
    sx = 0;
  }

  if (sy < 0) {
    h += sy;
    sy = 0;
  }

  if (sx + w >= sw) w = sw - sx;
  if (sy + h >= sh) h = sh - sy;

  if (x >= cw || y >= ch) return;
  if (x+w <= cx || y+h <= cy) return;

  ow = w;
  oh = h;

  if (x < cx) {
    int i = cx - x;
    if (flip_x) ow -= i;
    else sx += i;
    w -= i;
    x = cx;
  }

  if (y < cy) {
    int i = cy - y;
    if (flip_y) oh -= i;
    else sy += i;
    h -= i;
    y = cy;
  }

  ey = y + h;

  if (x+w > cw) w = cw - x;
  if (ey > ch) ey = ch;

  if (flip_x) {
    sx = sx + ow - 1;
    xi = -1;
  } else {
    xi = 1;
  }

  if (flip_y) {
    sy = sy + oh - 1;
    yi = -1;
  } else {
    yi = 1;
  }

  //fprintf(stderr, "%dx%d: %d,%d:%dx%d %d\n", sw,sh, sx, sy, w,h, ey-y);

  if (dst->cmap) {
    if (!src->cmap) {
      fprintf(stderr, "gfx.c: can't blit truecolor into indexed\n");
      abort();
    }
    begin_blit()
    uint32_t c;
    if (DITHER) {
      c = DC;
    } else {
      c = SC;
      if (zdata) zdata[pd] = z;
    }
    end_blit(c)
  } else {
    if (src->cmap) {
      begin_blit()
      int sr, sg, sb, sa;
      uint32_t c;
      if (SC >= GFX_CMAP_SIZE) {
        if (show_error) {
          fprintf(stderr, "gfx.c: color map index is too big = 0x%X\n", SC);
          show_error = 0;
        }
        SC = 0;
        //abort();
      }
      c = m[SC];
      fromR8G8B8A8(sr,sg,sb,sa,c);
      if (sa) {
        c = DC;
      } else if (DITHER) {
        c = DC;
      } else {
        if (bright) {
          BRIGHTEN(sr,sg,sb);
          c = R8G8B8(sr,sg,sb);
        }
        if (zdata) zdata[pd] = z;
      }
      end_blit(c)
    } else {

      begin_blit()
      int sm; // source multiplier
      uint32_t c; // result color
      int sr, sg, sb, sa;
      fromR8G8B8A8(sr,sg,sb,sa,SC);

      if (sa == 0) {

        if (bright) {
          BRIGHTEN(sr,sg,sb);
          c = R8G8B8(sr,sg,sb);
        } else {
          c = SC;

        }
        if (zdata) zdata[pd] = z;
      } else if (sa == 0xFF) {
        c = DC;
      } else {
        int dr, dg, db, da;

        fromR8G8B8A8(dr,dg,db,da,DC);

        if (da == 0) {
          //NOTE: X>>8 is a division by 256, while max alpha is 0xFF
          //      this leads to some loss of precision
          if (bright) {
            BRIGHTEN(sr,sg,sb);
          }
          sm = 0xFF - sa;
          r = (sr*sm + dr*sa)>>8;
          g = (sg*sm + dg*sa)>>8;
          b = (sb*sm + db*sa)>>8;
          c = R8G8B8(r,g,b);

        } else {

          // incorrect, but should be okay for now

          if (bright) {
            BRIGHTEN(sr,sg,sb);
            c = R8G8B8(sr,sg,sb);
          } else {
            c = SC;

          }
          if (zdata) zdata[pd] = z;
        }
      }
      if (DITHER) {
        c = DC;
      }
      end_blit(c);
    }
  }
}

#undef SC
#undef DC

static uint32_t margins_result[4];

void *gfx_margins(gfx_t *gfx) {
  int w = gfx->w;
  int h = gfx->h;
  uint32_t *d = gfx->data;
  uint32_t *m = gfx->cmap;
  int x1 = w;
  int x2 = -1;
  int sx = 0;
  int xb = w;
  int xe = 0;
  int yb = h;
  int ye = 0;
  int x;
  int y;
  for (y = 0; y < h; y++) {
    sx = y*w;
    xb = w;
    xe = -1;
    for (x = 0; x < w; x++) {
      uint32_t c = d[x+sx];
      if (m) c = m[c];
      if ((c>>24) != 255) {
        if (xb == w) xb = x;
        xe = x;
      }
    }
    if (xe != -1) {
      if (yb == h) yb = y;
      if (xb < x1) x1 = xb;
      if (xe > x2) x2 = xe;
      ye = y;
    }
  }
  if (x1 != w) {
    margins_result[0] = x1;
    margins_result[1] = yb;
    margins_result[2] = x2-x1+1;
    margins_result[3] = ye-yb+1;
  } else {
    margins_result[0] = 0;
    margins_result[1] = 0;
    margins_result[2] = w;
    margins_result[3] = h;
  }
  return margins_result;
}


uint32_t array[] = {123,456,789};

void *test(char *name, int x, float y) {
  fprintf(stderr, "%d:%f: Hello, %s! %d,%d,%d\n", x, y, name, array[0], array[1], array[2]);
  return (void*)array;
}


/*
int main(int argc, char **argv) {
  gfx_t *gfx;

  if (argc != 3) {
    printf("Usage: %s <infile> <outfile>\n", argv[0]);
  }

  gfx = gfx_load_png(argv[1]);
  gfx_save_png(argv[2], gfx);

  fprintf(stderr, "%dx%dx%d\n", gfx->w, gfx->h, gfx->type);

  return 0;
}
*/
