CC = cc
CFLAGS = -Wall

# `weakdef.c` and `strongdef.c` are compiled once for the executable and once
# for the shared library, mirroring the way lsof builds both `lsof` and
# `liblsof` from the same sources. Tenjin duplicates such sources into
# `_xjdup_N` copies, one per target, so the weak-definition handling has to
# happen per target.
all: program libweak.so

program: main.o weakdef.o strongdef.o
	$(CC) $(CFLAGS) -o $@ $^

libweak.so: weakdef.c strongdef.c
	$(CC) $(CFLAGS) -shared -fPIC -o $@ weakdef.c strongdef.c

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f *.o *.so program

.PHONY: all clean
