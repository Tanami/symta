TARGET = lib/libpng.a

#LOC = -DASMV
#LOC = -DDEBUG -g

PREFIX =
CC = $(PREFIX)gcc
CFLAGS = $(LOC) -O3 -Wall -I./include -DBZ_STRICT_ANSI
EXTRA_CFLAGS = -DNO_VIZ

AS = $(CC)
ASFLAGS = $(LOC) -Wall

LD = $(CC)
LDFLAGS = $(LOC)

AR = $(PREFIX)ar
ARFLAGS = rcs

STRIP = $(PREFIX)strip
CP = cp -fp
RM = rm -f

SRCS = png.c       pngmem.c    pngrio.c    pngset.c    pngwrite.c \
       pngerror.c  pngpread.c  pngrtran.c  pngtrans.c  pngwtran.c \
       pngget.c    pngread.c   pngrutil.c  pngwio.c    pngwutil.c

OBJS = $(addprefix lib/,$(SRCS:.c=.o))
OBJA =

#$(info $$OBJS is [${OBJS}])


all: $(TARGET)

lib/:
	mkdir lib

lib/%.o: src/%.c
	$(CC) $(CFLAGS) $(EXTRA_CFLAGS) -c -o $@ $<

$(TARGET): lib/ $(OBJS) $(OBJA)
	$(AR) $(ARFLAGS) $@ $(OBJS) $(OBJA)

.PHONY: install uninstall clean

clean:
	-$(RM) $(TARGET)
	-$(RM) *.o
	-$(RM) *.exe


