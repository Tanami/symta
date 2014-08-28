#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <setjmp.h>
#include <png.h>

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

char *downcase(char *t) {
  char *s = t;
  while(*s) {if(isupper(*s)) *s = tolower(*s); s++;}
  return t;
}

char *upcase(char *t) {
  char *s = t;
  while(*s) {if(islower(*s)) *s = toupper(*s); s++;}
  return t;
}

static void pathParts(char *Dir, char *Name, char *Ext, char *Path) {
  char *P, *Q;
  int L;

  if ((P = strrchr(Path, '/'))) {
    if (Dir) {
      L = P-Path;
      strncpy(Dir, Path, L);
      Dir[L] = 0;
    }
    P++;
  } else {
    if (Dir) Dir[0] = 0;
    P = Path;
  }
  if ((Q = strrchr(P, '.'))) {
    if (Ext) strcpy(Ext, Q+1);
    if (Name) {
       L = Q-P;
       strncpy(Name, P, L);
       Name[L] = 0;
    }
  } else {
    if (Name) strcpy(Name, P);
    if (Ext) Ext[0] = 0;
  }
}

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

static gfx_t *new_gfx(int w, int h, int type) {
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

static uint32_t gfx_get(gfx_t *gfx, int x, int y) {
  return gfx->data[gfx->w*y+x];
}

static void gfx_set(gfx_t *gfx, int x, int y, uint32_t color) {
  gfx->data[gfx->w*y+x] = color;
}

static void gfx_clear(gfx_t *gfx, uint32_t color) {
  int x, y;
  for (y = 0; y < gfx->h; y++) {
    for (x = 0; x < gfx->w; x++) {
      gfx->data[gfx->w*y+x] = color;
    }
  }
}

#ifndef png_jmpbuf
#  define png_jmpbuf(png_ptr)   ((png_ptr)->jmpbuf)
#endif

static gfx_t *load_png(char *filename) {
  gfx_t *gfx = 0;
  int x, y;
  png_structp png_ptr = NULL;
  png_infop info_ptr = NULL;
  png_infop end_info = NULL;
  png_uint_32 width, height, rowbytes;
  int bit_depth, color_type;
  png_byte *image_data = 0;
  png_bytep *row_pointers = 0;
  uint8_t sig[8];
  FILE *infile;

  infile = fopen(filename, "rb");

  if (!infile) {
    fprintf(stderr, "cant open `%s`\n", filename);
    return 0;
  }

  if (0) {
fail:
    if (gfx) free(gfx);
    if (image_data) free(image_data);
    if (row_pointers) free(row_pointers);
    fclose(infile);
    return 0;
  }

  fread(sig, 1, 8, infile);
  if (!png_check_sig(sig, 8)) {
    fprintf(stderr, "bad signature for `%s`\n", filename);
    return 0;
  }

  png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr) {
    fprintf(stderr, "png_create_info_struct: out of memory for `%s`\n", filename);
    goto fail;
  }

  info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr) {
    png_destroy_read_struct(&png_ptr, NULL, NULL);
    fprintf(stderr, "png_create_info_struct: out of memory for `%s`\n", filename);
    goto fail;
  }

  end_info = png_create_info_struct(png_ptr);
  if (!end_info) {
    fprintf(stderr, "error: png_create_info_struct returned 0.\n");
    png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp) NULL);
    goto fail;
  }

  // setjmp() must be called in every function that calls a PNG-reading libpng function
  if (setjmp(png_jmpbuf(png_ptr))) {
jmpbuf_fail:
    png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);
    goto fail;
  }

  png_init_io(png_ptr, infile);
  png_set_sig_bytes(png_ptr, 8); // tell libpng that we already read the 8 signature bytes
  png_read_info(png_ptr, info_ptr); // read all PNG info up to image data

  png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type, NULL, NULL, NULL);

  png_read_update_info(png_ptr, info_ptr);

  rowbytes = png_get_rowbytes(png_ptr, info_ptr);
  image_data = malloc(rowbytes * height * sizeof(png_byte)+15);
  if (!image_data) {
    fprintf(stderr, "load_png: could not allocate memory for PNG image data\n");
    png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);
    goto jmpbuf_fail;
  }

  row_pointers = malloc(height * sizeof(png_bytep));
  if (!row_pointers) {
    fprintf(stderr, "load_png: could not allocate memory for PNG row pointers\n");
    png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);
    goto jmpbuf_fail;
  }

  for (y = 0; y < height; y++) {
    row_pointers[height - 1 - y] = image_data + y * rowbytes;
  }

  png_read_image(png_ptr, row_pointers);

  png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);
  fclose(infile);

  if (bit_depth != 8) {
    fprintf(stderr, "load_png: unsupported bit_depth=%d\n", bit_depth);
    goto fail;
  }

  if (color_type == PNG_COLOR_TYPE_RGB) { //RGB
    gfx = new_gfx(width, height, GFX_RGB);
    for (y = 0; y < height; y++) {
      png_byte *row = row_pointers[y];
      for (x = 0; x < width; x++) {
        gfx_set(gfx, x, y, R8G8B8(row[0], row[1], row[2]));
        row += 3;
      }
    }
  } else {
    fprintf(stderr, "load_png: unsupported color_type=%d\n", color_type);
    goto fail;
  }

  free(image_data);
  free(row_pointers);

  return gfx;
}

void save_png(char *filename, gfx_t *gfx) {
  png_structp Png;
  png_infop Info;
  int I, X, Y, BPR;
  png_byte *Q, **Rows;
  FILE *F;
  png_color Pal[256];
  int Bits = gfx->type == GFX_INDEXED ? 8 :
             gfx->type == GFX_RGBA ? 32 :
                          24;

  F = fopen(filename, "wb");
  if (!F) {
    printf("cant create %s\n", filename);
    abort();
  }
  Png = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  Info = png_create_info_struct(Png);
  png_set_IHDR(Png,
               Info,
               gfx->w,
               gfx->h,
               8, // depth of color channel
               gfx->type == GFX_INDEXED ? PNG_COLOR_TYPE_PALETTE :
               gfx->type == GFX_RGBA ? PNG_COLOR_TYPE_RGB_ALPHA :
                            PNG_COLOR_TYPE_RGB,
               PNG_INTERLACE_NONE,
               PNG_COMPRESSION_TYPE_DEFAULT,
               PNG_FILTER_TYPE_DEFAULT);

  BPR = (gfx->w*Bits + 7)/8;
  Rows = png_malloc(Png, gfx->h * sizeof(png_byte *));
  if (gfx->type == GFX_INDEXED) {
    times (I, 256) {
      fromR8G8B8(Pal[I].red, Pal[I].green, Pal[I].blue, gfx->cmap[I]);
    }
    png_set_PLTE(Png, Info, Pal, 256);
    if (gfx->key != -1) { // that is little ticky
      png_color_16 CK;
      CK.index = gfx->key; //just in case png uses it
      png_byte Alpha[256];
      times (I, 256) Alpha[I] = I==gfx->key ? 0 : 0xFF;
      png_set_tRNS(Png, Info, Alpha, 256, &CK);
    }
    times (Y, gfx->h) {
      Rows[Y] = Q = png_malloc(Png, BPR);
      times (X, gfx->w) *Q++ = (uint8_t)gfx_get(gfx,X,Y);
    }
  } else if (gfx->type == GFX_RGB) {
    times (Y, gfx->h) {
      Rows[Y] = Q = png_malloc(Png, BPR);
      times (X, gfx->w) {
        fromR8G8B8(Q[0],Q[1],Q[2], gfx_get(gfx,X,Y));
        Q += 3;
      }
    }
  } else if (gfx->type == GFX_RGBA) {
    times (Y, gfx->h) {
      Rows[Y] = Q = png_malloc(Png, BPR);
      times (X, gfx->w) {
        fromR8G8B8A8(Q[0],Q[1],Q[2],Q[3], gfx_get(gfx,X,Y));
        Q += 4;
      }
    }
  } else {
    printf("  Cant save %d-typed PNGs\n", gfx->type);
    abort();
  }

  png_init_io(Png, F);
  png_set_rows(Png, Info, Rows);
  png_write_png(Png, Info, PNG_TRANSFORM_IDENTITY, NULL);

  times (Y, gfx->h) png_free(Png, Rows[Y]);
  png_free(Png, Rows);

  png_destroy_write_struct(&Png, &Info);
  fclose(F);
}


// FIXME: don't really need these selector here - export load/save png directly
static gfx_t *gfx_load(char *filename) {
  char Dir[1024], Name[256], Ext[32];
  pathParts(Dir, Name, Ext, filename);
  downcase(Ext);
  if (!strcmp(Ext, "png")) {
    return load_png(filename);
  } else {
    fprintf(stderr, "gfx_load: can't load `%s` files", Ext);
    abort();
  }
}

static void gfx_save(char *filename, gfx_t *gfx) {
  char Dir[1024], Name[256], Ext[32];
  pathParts(Dir, Name, Ext, filename);
  downcase(Ext);
  if (!strcmp(Ext, "png")) {
    save_png(filename, gfx);
  } else {
    fprintf(stderr, "gfx_save: can't save `%s` files", Ext);
    abort();
  }
}



int main(int argc, char **argv) {
  gfx_t *gfx;

  if (argc != 3) {
    printf("Usage: %s <infile> <outfile>\n", argv[0]);
  }

  gfx = gfx_load(argv[1]);
  gfx_save(argv[2], gfx);

  fprintf(stderr, "%dx%dx%d\n", gfx->w, gfx->h, gfx->type);

  return 0;
}
