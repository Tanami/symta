TARGET = lib/libz.a

#LOC = -DASMV
#LOC = -DDEBUG -g

PREFIX =
CC = $(PREFIX)gcc
CFLAGS = $(LOC) -O3 -Wall -I./include
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

SRCS = adler32.c compress.c uncompr.c crc32.c deflate.c \
       inffast.c inflate.c inftrees.c trees.c trees.c zutil.c gzio.c

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
