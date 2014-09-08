#include <SDL.h>
#include <stdio.h>

#include <gfx.h>

SDL_Window* window;
SDL_Surface* surface;

static int window_w;
static int window_h;
static int window_x;
static int window_y;

static done_init;

static char *title = "Symta";

static char *show_init() {
  if (done_init) return 0;
  if (SDL_Init(SDL_INIT_VIDEO) < 0) return (char*)SDL_GetError();
  window_x = SDL_WINDOWPOS_UNDEFINED;
  window_y = SDL_WINDOWPOS_UNDEFINED;
  done_init = 1;
  return 0;
}

static char *show_resize(int w, int h) {
  char *result;
  window_w = w > 10 ? w : 10;
  window_h = h > 10 ? h : 10;

  if (window && surface->w == window_w && surface->h == window_h) return 0;

  result = show_init();
  if (result) return result;

  if (window) {
    SDL_GetWindowPosition(window, &window_x, &window_y);
    SDL_DestroyWindow(window);
  }

  //flags: SDL_WINDOW_HIDDEN SDL_WINDOW_FULLSCREEN SDL_WINDOW_RESIZABLE
  window = SDL_CreateWindow(title, window_x, window_y, window_w, window_h, SDL_WINDOW_SHOWN);
  if(window == NULL) return (char*)SDL_GetError();
  surface = SDL_GetWindowSurface(window);

  return 0;
}

static char *upload_gfx(gfx_t *gfx) {
  uint8_t *p;
  uint32_t *s;
  uint32_t *d;
  uint32_t *end;
  int pitch;
  int x, y;
  int w = gfx->w;
  int h = gfx->h;

  if (gfx->cmap) {
    return "can't display indexed gfx";
  }

  SDL_LockSurface(surface);

  p = (uint8_t*)surface->pixels;
  s = gfx->data;
  for (y = 0; y < h; y++) {
    d = (uint32_t*)(p + surface->pitch*y);
    end = s + w;
    while (s < end) {
      *d++ = *s++;
    }
  }

  SDL_UnlockSurface(surface);

  SDL_UpdateWindowSurface(window);
  SDL_Delay(2000);

  return 0;
}

char *show_gfx(gfx_t *gfx) {
  char *result;
  result = show_resize(gfx->w, gfx->h);
  if (result) return result;

  //SDL_FillRect(surface, NULL, SDL_MapRGB(surface->format, 0xFF, 0xFF, 0xFF));
  result = upload_gfx(gfx);
  if (result) return result;

  return "";
}

void show_close() {
  if (!done_init) return;
  if (window) SDL_DestroyWindow(window);
  SDL_Quit();
  window = NULL;
  surface = NULL;
  done_init = 0;
}
