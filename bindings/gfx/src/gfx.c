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

int test(char *name, int x) {
  fprintf(stderr, "Hello, %s! %d\n", name, x);
  return 123;
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
