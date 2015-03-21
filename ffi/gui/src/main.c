#include <SDL.h>
#include <SDL_mixer.h>
#include <stdio.h>
#include <ctype.h>

#include <gfx.h>

static SDL_Window* window;
static SDL_Surface* surface;

static int window_w;
static int window_h;
static int window_x;
static int window_y;

static done_init;

static char *title = "Symta";

static char *show_events;

typedef struct {
  SDL_Keycode key;
  char *name;
} keyname_t;

#define MAX_KEYNAMES 1024
static keyname_t keynames[MAX_KEYNAMES];
static int keynames_used;

char *lower_string(char *s) {
  char *p;
  for (p = s; *p; ++p) *p = tolower(*p);
  return s;
}

#define NAME_KEY(k,n) \
  keynames[keynames_used].key = k; \
  keynames[keynames_used].name = lower_string(strdup(n)); \
  ++keynames_used;

static void init_keynames() {
  if (keynames_used) return;
  NAME_KEY(SDLK_RETURN, "RETURN");
  NAME_KEY(SDLK_ESCAPE, "ESCAPE");
  NAME_KEY(SDLK_BACKSPACE, "BACKSPACE");
  NAME_KEY(SDLK_TAB, "TAB");
  NAME_KEY(SDLK_SPACE, " ");
  NAME_KEY(SDLK_EXCLAIM, "!");
  NAME_KEY(SDLK_QUOTEDBL, "\"");
  NAME_KEY(SDLK_HASH, "#");
  NAME_KEY(SDLK_PERCENT, "%");
  NAME_KEY(SDLK_DOLLAR, "$");
  NAME_KEY(SDLK_AMPERSAND, "&");
  NAME_KEY(SDLK_QUOTE, "'");
  NAME_KEY(SDLK_LEFTPAREN, "(");
  NAME_KEY(SDLK_RIGHTPAREN, ")");
  NAME_KEY(SDLK_ASTERISK, "*");
  NAME_KEY(SDLK_PLUS, "+");
  NAME_KEY(SDLK_COMMA, ",");
  NAME_KEY(SDLK_MINUS, "-");
  NAME_KEY(SDLK_PERIOD, ".");
  NAME_KEY(SDLK_SLASH, "/");
  NAME_KEY(SDLK_0, "0");
  NAME_KEY(SDLK_1, "1");
  NAME_KEY(SDLK_2, "2");
  NAME_KEY(SDLK_3, "3");
  NAME_KEY(SDLK_4, "4");
  NAME_KEY(SDLK_5, "5");
  NAME_KEY(SDLK_6, "6");
  NAME_KEY(SDLK_7, "7");
  NAME_KEY(SDLK_8, "8");
  NAME_KEY(SDLK_9, "9");
  NAME_KEY(SDLK_COLON, ":");
  NAME_KEY(SDLK_SEMICOLON, ";");
  NAME_KEY(SDLK_LESS, "<");
  NAME_KEY(SDLK_EQUALS, "=");
  NAME_KEY(SDLK_GREATER, ">");
  NAME_KEY(SDLK_QUESTION, "?");
  NAME_KEY(SDLK_AT, "@");
  NAME_KEY(SDLK_LEFTBRACKET, "[");
  NAME_KEY(SDLK_BACKSLASH, "\\");
  NAME_KEY(SDLK_RIGHTBRACKET, "]");
  NAME_KEY(SDLK_CARET, "^");
  NAME_KEY(SDLK_UNDERSCORE, "_");
  NAME_KEY(SDLK_BACKQUOTE, "`");
  NAME_KEY(SDLK_a, "a");
  NAME_KEY(SDLK_b, "b");
  NAME_KEY(SDLK_c, "c");
  NAME_KEY(SDLK_d, "d");
  NAME_KEY(SDLK_e, "e");
  NAME_KEY(SDLK_f, "f");
  NAME_KEY(SDLK_g, "g");
  NAME_KEY(SDLK_h, "h");
  NAME_KEY(SDLK_i, "i");
  NAME_KEY(SDLK_j, "j");
  NAME_KEY(SDLK_k, "k");
  NAME_KEY(SDLK_l, "l");
  NAME_KEY(SDLK_m, "m");
  NAME_KEY(SDLK_n, "n");
  NAME_KEY(SDLK_o, "o");
  NAME_KEY(SDLK_p, "p");
  NAME_KEY(SDLK_q, "q");
  NAME_KEY(SDLK_r, "r");
  NAME_KEY(SDLK_s, "s");
  NAME_KEY(SDLK_t, "t");
  NAME_KEY(SDLK_u, "u");
  NAME_KEY(SDLK_v, "v");
  NAME_KEY(SDLK_w, "w");
  NAME_KEY(SDLK_x, "x");
  NAME_KEY(SDLK_y, "y");
  NAME_KEY(SDLK_z, "z");
  NAME_KEY(SDLK_CAPSLOCK, "CAPSLOCK");
  NAME_KEY(SDLK_F1, "F1");
  NAME_KEY(SDLK_F2, "F2");
  NAME_KEY(SDLK_F3, "F3");
  NAME_KEY(SDLK_F4, "F4");
  NAME_KEY(SDLK_F5, "F5");
  NAME_KEY(SDLK_F6, "F6");
  NAME_KEY(SDLK_F7, "F7");
  NAME_KEY(SDLK_F8, "F8");
  NAME_KEY(SDLK_F9, "F9");
  NAME_KEY(SDLK_F10, "F10");
  NAME_KEY(SDLK_F11, "F11");
  NAME_KEY(SDLK_F12, "F12");
  NAME_KEY(SDLK_PRINTSCREEN, "PRINTSCREEN");
  NAME_KEY(SDLK_SCROLLLOCK, "SCROLLLOCK");
  NAME_KEY(SDLK_PAUSE, "PAUSE");
  NAME_KEY(SDLK_INSERT, "INSERT");
  NAME_KEY(SDLK_HOME, "HOME");
  NAME_KEY(SDLK_PAGEUP, "PAGEUP");
  NAME_KEY(SDLK_DELETE, "DELETE");
  NAME_KEY(SDLK_END, "END");
  NAME_KEY(SDLK_PAGEDOWN, "PAGEDOWN");
  NAME_KEY(SDLK_RIGHT, "RIGHT");
  NAME_KEY(SDLK_LEFT, "LEFT");
  NAME_KEY(SDLK_DOWN, "DOWN");
  NAME_KEY(SDLK_UP, "UP");
  NAME_KEY(SDLK_NUMLOCKCLEAR, "NUMLOCKCLEAR");
  NAME_KEY(SDLK_KP_DIVIDE, "KP_DIVIDE");
  NAME_KEY(SDLK_KP_MULTIPLY, "KP_MULTIPLY");
  NAME_KEY(SDLK_KP_MINUS, "KP_MINUS");
  NAME_KEY(SDLK_KP_PLUS, "KP_PLUS");
  NAME_KEY(SDLK_KP_ENTER, "KP_ENTER");
  NAME_KEY(SDLK_KP_1, "KP_1");
  NAME_KEY(SDLK_KP_2, "KP_2");
  NAME_KEY(SDLK_KP_3, "KP_3");
  NAME_KEY(SDLK_KP_4, "KP_4");
  NAME_KEY(SDLK_KP_5, "KP_5");
  NAME_KEY(SDLK_KP_6, "KP_6");
  NAME_KEY(SDLK_KP_7, "KP_7");
  NAME_KEY(SDLK_KP_8, "KP_8");
  NAME_KEY(SDLK_KP_9, "KP_9");
  NAME_KEY(SDLK_KP_0, "KP_0");
  NAME_KEY(SDLK_KP_PERIOD, "KP_PERIOD");
  NAME_KEY(SDLK_APPLICATION, "APPLICATION");
  NAME_KEY(SDLK_POWER, "POWER");
  NAME_KEY(SDLK_KP_EQUALS, "KP_EQUALS");
  NAME_KEY(SDLK_F13, "F13");
  NAME_KEY(SDLK_F14, "F14");
  NAME_KEY(SDLK_F15, "F15");
  NAME_KEY(SDLK_F16, "F16");
  NAME_KEY(SDLK_F17, "F17");
  NAME_KEY(SDLK_F18, "F18");
  NAME_KEY(SDLK_F19, "F19");
  NAME_KEY(SDLK_F20, "F20");
  NAME_KEY(SDLK_F21, "F21");
  NAME_KEY(SDLK_F22, "F22");
  NAME_KEY(SDLK_F23, "F23");
  NAME_KEY(SDLK_F24, "F24");
  NAME_KEY(SDLK_EXECUTE, "EXECUTE");
  NAME_KEY(SDLK_HELP, "HELP");
  NAME_KEY(SDLK_MENU, "MENU");
  NAME_KEY(SDLK_SELECT, "SELECT");
  NAME_KEY(SDLK_STOP, "STOP");
  NAME_KEY(SDLK_AGAIN, "AGAIN");
  NAME_KEY(SDLK_UNDO, "UNDO");
  NAME_KEY(SDLK_CUT, "CUT");
  NAME_KEY(SDLK_COPY, "COPY");
  NAME_KEY(SDLK_PASTE, "PASTE");
  NAME_KEY(SDLK_FIND, "FIND");
  NAME_KEY(SDLK_MUTE, "MUTE");
  NAME_KEY(SDLK_VOLUMEUP, "VOLUMEUP");
  NAME_KEY(SDLK_VOLUMEDOWN, "VOLUMEDOWN");
  NAME_KEY(SDLK_KP_COMMA, "KP_COMMA");
  NAME_KEY(SDLK_KP_EQUALSAS400, "KP_EQUALSAS400");
  NAME_KEY(SDLK_ALTERASE, "ALTERASE");
  NAME_KEY(SDLK_SYSREQ, "SYSREQ");
  NAME_KEY(SDLK_CANCEL, "CANCEL");
  NAME_KEY(SDLK_CLEAR, "CLEAR");
  NAME_KEY(SDLK_PRIOR, "PRIOR");
  NAME_KEY(SDLK_RETURN2, "RETURN2");
  NAME_KEY(SDLK_SEPARATOR, "SEPARATOR");
  NAME_KEY(SDLK_OUT, "OUT");
  NAME_KEY(SDLK_OPER, "OPER");
  NAME_KEY(SDLK_CLEARAGAIN, "CLEARAGAIN");
  NAME_KEY(SDLK_CRSEL, "CRSEL");
  NAME_KEY(SDLK_EXSEL, "EXSEL");
  NAME_KEY(SDLK_KP_00, "KP_00");
  NAME_KEY(SDLK_KP_000, "KP_000");
  NAME_KEY(SDLK_THOUSANDSSEPARATOR, "THOUSANDSSEPARATOR");
  NAME_KEY(SDLK_DECIMALSEPARATOR, "DECIMALSEPARATOR");
  NAME_KEY(SDLK_CURRENCYUNIT, "CURRENCYUNIT");
  NAME_KEY(SDLK_CURRENCYSUBUNIT, "CURRENCYSUBUNIT");
  NAME_KEY(SDLK_KP_LEFTPAREN, "KP_LEFTPAREN");
  NAME_KEY(SDLK_KP_RIGHTPAREN, "KP_RIGHTPAREN");
  NAME_KEY(SDLK_KP_LEFTBRACE, "KP_LEFTBRACE");
  NAME_KEY(SDLK_KP_RIGHTBRACE, "KP_RIGHTBRACE");
  NAME_KEY(SDLK_KP_TAB, "KP_TAB");
  NAME_KEY(SDLK_KP_BACKSPACE, "KP_BACKSPACE");
  NAME_KEY(SDLK_KP_A, "KP_A");
  NAME_KEY(SDLK_KP_B, "KP_B");
  NAME_KEY(SDLK_KP_C, "KP_C");
  NAME_KEY(SDLK_KP_D, "KP_D");
  NAME_KEY(SDLK_KP_E, "KP_E");
  NAME_KEY(SDLK_KP_F, "KP_F");
  NAME_KEY(SDLK_KP_XOR, "KP_XOR");
  NAME_KEY(SDLK_KP_POWER, "KP_POWER");
  NAME_KEY(SDLK_KP_PERCENT, "KP_PERCENT");
  NAME_KEY(SDLK_KP_LESS, "KP_LESS");
  NAME_KEY(SDLK_KP_GREATER, "KP_GREATER");
  NAME_KEY(SDLK_KP_AMPERSAND, "KP_AMPERSAND");
  NAME_KEY(SDLK_KP_DBLAMPERSAND, "KP_DBLAMPERSAND");
  NAME_KEY(SDLK_KP_VERTICALBAR, "KP_VERTICALBAR");
  NAME_KEY(SDLK_KP_DBLVERTICALBAR, "KP_DBLVERTICALBAR");
  NAME_KEY(SDLK_KP_COLON, "KP_COLON");
  NAME_KEY(SDLK_KP_HASH, "KP_HASH");
  NAME_KEY(SDLK_KP_SPACE, "KP_SPACE");
  NAME_KEY(SDLK_KP_AT, "KP_AT");
  NAME_KEY(SDLK_KP_EXCLAM, "KP_EXCLAM");
  NAME_KEY(SDLK_KP_MEMSTORE, "KP_MEMSTORE");
  NAME_KEY(SDLK_KP_MEMRECALL, "KP_MEMRECALL");
  NAME_KEY(SDLK_KP_MEMCLEAR, "KP_MEMCLEAR");
  NAME_KEY(SDLK_KP_MEMADD, "KP_MEMADD");
  NAME_KEY(SDLK_KP_MEMSUBTRACT, "KP_MEMSUBTRACT");
  NAME_KEY(SDLK_KP_MEMMULTIPLY, "KP_MEMMULTIPLY");
  NAME_KEY(SDLK_KP_MEMDIVIDE, "KP_MEMDIVIDE");
  NAME_KEY(SDLK_KP_PLUSMINUS, "KP_PLUSMINUS");
  NAME_KEY(SDLK_KP_CLEAR, "KP_CLEAR");
  NAME_KEY(SDLK_KP_CLEARENTRY, "KP_CLEARENTRY");
  NAME_KEY(SDLK_KP_BINARY, "KP_BINARY");
  NAME_KEY(SDLK_KP_OCTAL, "KP_OCTAL");
  NAME_KEY(SDLK_KP_DECIMAL, "KP_DECIMAL");
  NAME_KEY(SDLK_KP_HEXADECIMAL, "KP_HEXADECIMAL");
  NAME_KEY(SDLK_LCTRL, "LCTRL");
  NAME_KEY(SDLK_LSHIFT, "LSHIFT");
  NAME_KEY(SDLK_LALT, "LALT");
  NAME_KEY(SDLK_LGUI, "LGUI");
  NAME_KEY(SDLK_RCTRL, "RCTRL");
  NAME_KEY(SDLK_RSHIFT, "RSHIFT");
  NAME_KEY(SDLK_RALT, "RALT");
  NAME_KEY(SDLK_RGUI, "RGUI");
  NAME_KEY(SDLK_MODE, "MODE");
  NAME_KEY(SDLK_AUDIONEXT, "AUDIONEXT");
  NAME_KEY(SDLK_AUDIOPREV, "AUDIOPREV");
  NAME_KEY(SDLK_AUDIOSTOP, "AUDIOSTOP");
  NAME_KEY(SDLK_AUDIOPLAY, "AUDIOPLAY");
  NAME_KEY(SDLK_AUDIOMUTE, "AUDIOMUTE");
  NAME_KEY(SDLK_MEDIASELECT, "MEDIASELECT");
  NAME_KEY(SDLK_WWW, "WWW");
  NAME_KEY(SDLK_MAIL, "MAIL");
  NAME_KEY(SDLK_CALCULATOR, "CALCULATOR");
  NAME_KEY(SDLK_COMPUTER, "COMPUTER");
  NAME_KEY(SDLK_AC_SEARCH, "AC_SEARCH");
  NAME_KEY(SDLK_AC_HOME, "AC_HOME");
  NAME_KEY(SDLK_AC_BACK, "AC_BACK");
  NAME_KEY(SDLK_AC_FORWARD, "AC_FORWARD");
  NAME_KEY(SDLK_AC_STOP, "AC_STOP");
  NAME_KEY(SDLK_AC_REFRESH, "AC_REFRESH");
  NAME_KEY(SDLK_AC_BOOKMARKS, "AC_BOOKMARKS");
  NAME_KEY(SDLK_BRIGHTNESSDOWN, "BRIGHTNESSDOWN");
  NAME_KEY(SDLK_BRIGHTNESSUP, "BRIGHTNESSUP");
  NAME_KEY(SDLK_DISPLAYSWITCH, "DISPLAYSWITCH");
  NAME_KEY(SDLK_KBDILLUMTOGGLE, "KBDILLUMTOGGLE");
  NAME_KEY(SDLK_KBDILLUMDOWN, "KBDILLUMDOWN");
  NAME_KEY(SDLK_KBDILLUMUP, "KBDILLUMUP");
  NAME_KEY(SDLK_EJECT, "EJECT");
  NAME_KEY(SDLK_SLEEP, "SLEEP");
}

static char *find_keyname(SDL_Keycode key) {
  int i;
  for (i = 0; i < keynames_used; i++) {
    if (keynames[i].key == key) return keynames[i].name;
  }
  return 0;
}

static char *show_init() {
  if (done_init) return 0;
  init_keynames();
  if (SDL_Init(SDL_INIT_VIDEO|SDL_INIT_AUDIO) < 0) return (char*)SDL_GetError();
  window_x = SDL_WINDOWPOS_UNDEFINED;
  window_y = SDL_WINDOWPOS_UNDEFINED;

  if (Mix_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 4096) == -1) {
    return (char*)Mix_GetError();
  }
    
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

  SDL_ShowCursor(0);

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
  //SDL_Delay(2000);

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
  if (show_events) {
    free(show_events);
    show_events = 0;
  }
  Mix_CloseAudio();
  SDL_Quit();
  window = NULL;
  surface = NULL;
  done_init = 0;
}

char *show_get_events() {
  int i;
  static int shift = 0;
  SDL_Event e;
  char tmp[1024];
  char tmp2[128];
  int xs_used = 0;
  int xs_size = 10;
  char **xs;
  char *p;
  int result_size = 1;

  if (!done_init) return "error";

  if (show_events) {
    free(show_events);
    show_events = 0;
  }

  xs = (char**)malloc(xs_size*sizeof(char*));

  while (SDL_PollEvent(&e) != 0) {
    if (e.type == SDL_QUIT) {
      sprintf(tmp, "quit");
    } else if (e.type == SDL_KEYDOWN || e.type == SDL_KEYUP) {
      char *name = find_keyname(e.key.keysym.sym);
      // FIXME: find a better way to handle SHIFT input
      if (shift && name && !name[1]) {
        int c = name[0];
        tmp2[1] = 0;
        if ('a' <= c && c <= 'z') {
          tmp2[0] = c-'a'+'A';
          name = tmp2;
        } else if ('0' <= c && c <= '9') {
          tmp2[0] = ")!@#$%^&*("[c-'0'];
          name = tmp2;
        } else {
          char *p = "`~-_=+[{]}\\|;:'\",<.>/?";
          for (; *p; p += 2) if (*p == c) {
            tmp2[0] = p[1];
            name = tmp2;
            break;
          }
        }
      }
      if (!name) {
        sprintf(tmp2, "unknown_%d", e.key.keysym.sym);
        name = tmp2;
      } else if (!strcmp(name,"lshift") || !strcmp(name,"rshift")) {
        shift = (e.type == SDL_KEYDOWN) ? 1 : 0;
      }
      char *state = e.type == SDL_KEYDOWN ? "1" : "0";
      if (name[0] == '\\' || name[0] == '`') {
        sprintf(tmp, "(key `\\%s` %s)", name, state);
      } else {
        sprintf(tmp, "(key `%s` %s)", name, state);
      }
    } else if (e.type == SDL_MOUSEMOTION) {
      sprintf(tmp, "(mice_move (%d %d))", e.motion.x, e.motion.y);
    } else if (e.type == SDL_MOUSEBUTTONDOWN || e.type == SDL_MOUSEBUTTONUP) {
      char *name;
      char *state = e.type == SDL_MOUSEBUTTONDOWN ? "1" : "0";
      if (e.button.button == SDL_BUTTON_LEFT) name = "left";
      else if (e.button.button == SDL_BUTTON_RIGHT) name = "right";
      else if (e.button.button == SDL_BUTTON_MIDDLE) name = "middle";
      else {
        sprintf(tmp2, "%d", e.button.button);
        name = tmp2;
      }
      sprintf(tmp, "(mice %s %s)", name, state);
    } else if (e.type == SDL_MOUSEWHEEL) {
      sprintf(tmp, "(mice_wheel %d)", e.wheel.y);
    } else if (e.type == SDL_WINDOWEVENT) {
      if (e.window.event == SDL_WINDOWEVENT_RESIZED) {
        sprintf(tmp, "(resize %d %d)", e.window.data1, e.window.data2);
      } else {
        continue;
      }
    } else {
      continue;
    }
    
    if (xs_used == xs_size) {
      char **new_xs;
      xs_size = 2*xs_size;
      new_xs = (char**)malloc(xs_size*sizeof(char*));
      memcpy(new_xs, xs, xs_used*sizeof(char*));
      free(xs);
      xs = new_xs;
    }
    xs[xs_used++] = strdup(tmp);
  }

  for (i = 0; i < xs_used; i++) {
    result_size += strlen(xs[i]);
  }

  p = show_events = (char*)malloc((result_size+xs_used)*sizeof(char));

  for (i = 0; i < xs_used; i++) {
    p += sprintf(p, "%s", xs[i]);
    if (i+1 != xs_used) *p++ = ' ';
    free(xs[i]);
  }
  free(xs);

  *p = 0;

  return show_events;
}

void show_cursor(int state) {
  SDL_ShowCursor(state ? 1 : 0);
}

void show_sleep(uint32_t ms) {
  SDL_Delay(ms);
}

uint32_t show_get_ticks() {
  return SDL_GetTicks();
}


#define MAX_SOUNDS (1<<15)
#define SND_MUSIC 1

typedef struct {
  char *origin;
  int flags;
  void *sound;
} sound;

static sound sounds[MAX_SOUNDS];
static int next_sound = 1;

int show_sound_load(char *filename, int music) {
  int i;
  show_init();
  for (i = next_sound; sounds[i].sound; i++);
  if (music) {
    sounds[i].sound = Mix_LoadMUS(filename);
  } else {
    sounds[i].sound = Mix_LoadWAV(filename);
  }
  if (!sounds[next_sound].sound) return 0;
  sounds[next_sound].origin = strdup(filename);
  sounds[next_sound].flags = music;
  next_sound = i + 1;
  return i;
}

void show_sound_free(int id) {
  free(sounds[id].origin);
  if (sounds[id].flags&SND_MUSIC) Mix_FreeMusic((Mix_Music*)sounds[id].sound);
  else Mix_FreeChunk((Mix_Chunk*)sounds[id].sound);
  sounds[id].sound = 0;
  if (next_sound > id) next_sound = id;
}

#define MUSIC_CHANNEL 0xFFFFFFF

int show_sound_play(int id, int channel, int loop) {
  if (sounds[id].flags&SND_MUSIC) {
    Mix_PlayMusic((Mix_Music*)sounds[id].sound, loop+1);
    return MUSIC_CHANNEL;
  }
  return Mix_PlayChannel(channel, (Mix_Chunk*)sounds[id].sound, loop+0);
}

int show_sound_playing(int channel) {
  if (channel == MUSIC_CHANNEL) {
    return Mix_PlayingMusic() != 0;
  }
  return Mix_Playing(channel);
}

