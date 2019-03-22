INSTALL_DIR ?= $(PACT_BENCH)/local

SRCS=pager.c test.c

.PHONY: clean install all
all: test

%.o : %.c pager.h
	$(CC) $(CFLAGS) $< -c -o $@

libpager.a: pager.o
	ar rcs $@ $^

test: test.o libpager.a
	$(CC) $(CFLAGS) $^ -o $@

install: libpager.a pager.h
	mkdir -p $(INSTALL_DIR)/lib
	mkdir -p $(INSTALL_DIR)/include/pager
	cp libpager.a $(INSTALL_DIR)/lib
	cp pager.h $(INSTALL_DIR)/include/pager

clean:
	rm -f test *.o *.a
