#include "gfx.h"

gfx_t *new_gfx(int w, int h, int type) {
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
  gfx->key = -1;
  gfx->hs_x = 0;
  gfx->hs_y = 0;
  return gfx;
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

uint32_t gfx_hs_x(gfx_t *gfx) {
  return gfx->hs_x;
}

uint32_t gfx_hs_y(gfx_t *gfx) {
  return gfx->hs_y;
}

uint32_t gfx_get(gfx_t *gfx, int x, int y) {
  return gfx->data[gfx->w*y+x];
}

void gfx_set(gfx_t *gfx, int x, int y, uint32_t color) {
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

  gfx = load_png(argv[1]);
  save_png(argv[2], gfx);

  fprintf(stderr, "%dx%dx%d\n", gfx->w, gfx->h, gfx->type);

  return 0;
}
*/
