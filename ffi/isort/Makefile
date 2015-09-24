TARGET = lib/main

#LOC = -DASMV
#LOC = -DDEBUG -g


PREFIX =
CC = $(PREFIX)g++
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

SRCS = isort.cpp

OBJS = $(addprefix lib/,$(SRCS:.cpp=.o))
OBJA =

#$(info $$OBJS is [${OBJS}])


all: $(TARGET)

lib/:
	mkdir lib

lib/%.o: src/%.cpp
	$(CC) $(CFLAGS) $(EXTRA_CFLAGS) -c -o $@ $<

$(TARGET): lib/ $(OBJS) $(OBJA)
	$(LD) -fpic -shared -o $@ $(OBJS) $(OBJA) $(LDFLAGS)

.PHONY: install uninstall clean

clean:
	-$(RM) $(TARGET)
	-$(RM) *.o
	-$(RM) *.exe


