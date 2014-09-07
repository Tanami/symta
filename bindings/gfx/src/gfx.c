#include "gfx.h"

gfx_t *new_gfx(uint32_t w, uint32_t h, uint32_t type) {
  gfx_t *gfx = (gfx_t*)malloc(sizeof(gfx_t));
  if (!gfx) return 0;
  gfx->data = (uint32_t*)malloc(w*h*4);
  if (!gfx->data) {
    free(gfx);
    return 0;
  }
  gfx->w = w;
  gfx->h = h;
  gfx->type = type;
  gfx->cmap = 0;
  gfx->key = GFX_NO_KEY;
  gfx->hotspot_x = 0;
  gfx->hotspot_y = 0;
  return gfx;
}

void gfx_resize(gfx_t *gfx, uint32_t w, uint32_t h) {
  free(gfx->data);
  gfx->data = (uint32_t*)malloc(w*h*4);
  gfx->w = w;
  gfx->h = h;
}

uint32_t gfx_w(gfx_t *gfx) {
  return gfx->w;
}

uint32_t gfx_h(gfx_t *gfx) {
  return gfx->h;
}

uint32_t gfx_type(gfx_t *gfx) {
  return gfx->type;
}

uint32_t gfx_key(gfx_t *gfx) {
  return gfx->key;
}

uint32_t gfx_hotspot_x(gfx_t *gfx) {
  return gfx->hotspot_x;
}

uint32_t gfx_hotspot_y(gfx_t *gfx) {
  return gfx->hotspot_y;
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
    gfx_hline(gfx, color, x, y+1, h-2);
    gfx_hline(gfx, color, x+w-1, y+1, h-2);
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
