TARGET = lib/main

#LOC = -DASMV
#LOC = -DDEBUG -g


PREFIX =
CC = $(PREFIX)gcc
CFLAGS = $(LOC) -O3 -I./src
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

SRCS = isort.c

OBJS = $(addprefix lib/,$(SRCS:.c=.o))
OBJA =

#$(info $$OBJS is [${OBJS}])


all: $(TARGET)

lib/:
	mkdir lib

lib/%.o: src/%.c
	$(CC) $(CFLAGS) $(EXTRA_CFLAGS) -c -o $@ $<

$(TARGET): lib/ $(OBJS) $(OBJA)
	$(LD) -fpic -shared -o $@ $(OBJS) $(OBJA) $(LDFLAGS)

.PHONY: install uninstall clean

clean:
	-$(RM) $(TARGET)
	-$(RM) *.o
	-$(RM) *.exe


