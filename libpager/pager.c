#include "pager.h"

#include <stdio.h>

#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

struct sigaction sa, old_sigaction;

void do_nothing(UNUSED void *ptr) {}

page_entry_st table_data[DEFAULT_TABLE_SIZE] = {{0}};

page_table_st table = {
    .entries = table_data, .size = DEFAULT_TABLE_SIZE, .next_insert = 0};

page_functions_st heap_functions = {.deleter = (deleter_ft)free,
                                    .copier = (copier_ft)memcpy};

page_functions_st stack_functions = {.deleter = (deleter_ft)do_nothing,
                                     .copier = (copier_ft)memcpy};

static long page_size() { return sysconf(_SC_PAGE_SIZE); }

static void *start_of_page(void *addr) {
  return (void *)((long)addr & ~(page_size() - 1));
}

static void *end_of_page(void *addr) {
  return (void *)((long)start_of_page(addr) + page_size());
}

static bool in_aligned_extent(void *start, size_t size, void *ptr) {
  char *sop = (char *)start_of_page(start);
  char *eop = (char *)end_of_page((char *)start + size);
  return (char *)ptr >= sop && (char *)ptr < eop;
}

static void handler(int sig, siginfo_t *si, void *unused) {
  void *addr = si->si_addr;
  bool sorted = false;

  for (size_t i = 0; i < table.size; ++i) {
    void *host_ptr = table.entries[i].host;
    size_t size = table.entries[i].last_size;

    if (in_aligned_extent(host_ptr, size, addr)) {
      mprotect(start_of_page(host_ptr), size,
               PROT_READ | PROT_WRITE | PROT_EXEC);

      table.entries[i].written = true;
      sorted = true;
    }
  }

  if (!sorted) {
    old_sigaction.sa_sigaction(sig, si, unused);
  }
}

static void set_handler() {
  static bool done = false;
  if (!done) {
    sa.sa_flags = SA_RESTART | SA_SIGINFO;
    sigemptyset(&sa.sa_mask);
    sa.sa_sigaction = handler;

    if (sigaction(SIGSEGV, &sa, &old_sigaction) == -1) {
      exit(3);
    }

    done = true;
  }
}

void new_table_entry(void *h, void *d, size_t s, page_functions_st fns) {
  // Logic:
  //  - if there is an entry in the table with the same host pointer as this
  //    one, then update it and delete whatever was there before
  //  - if there is an empty slot, then insert there
  //  - otherwise, delete whatever is at next insertion point and move the
  //    pointer along one.
  //  - in every case, we finish by updating one entry with a new struct
  //  - because we initially fill it up in order, then if we find an empty entry
  //    we know that there can't be one to overwrite before it.

  size_t insert_point = table.next_insert;
  bool must_delete = true;

  for (size_t i = 0; i < table.size; ++i) {
    page_entry_st e = table.entries[i];
    if (!e.host || e.host == h) {
      insert_point = i;
      must_delete = !!e.host;  // don't delete whatever's there if it was NULL
      break;
    }
  }

  page_entry_st old_e = table.entries[insert_point];
  if (must_delete) {
    old_e.functions.deleter(old_e.device);
  }

  page_entry_st new_e = {.host = h,
                         .device = d,
                         .last_size = s,
                         .written = false,
                         .functions = fns};
  table.entries[insert_point] = new_e;
  fns.copier(d, h, s);

  // mprotect host pointer now
  mprotect(start_of_page(h), s, PROT_READ | PROT_EXEC);
  set_handler();

  table.next_insert = (table.next_insert + 1) % table.size;
}

void *get_table_entry(void *h) {
  for (size_t i = 0; i < table.size; ++i) {
    if (table.entries[i].host == h) {
      if (table.entries[i].written) {
        table.entries[i].functions.copier(table.entries[i].device, h,
                                          table.entries[i].last_size);
        table.entries[i].written = false;

        mprotect(start_of_page(h), table.entries[i].last_size,
                 PROT_READ | PROT_EXEC);
      }
      return table.entries[i].device;
    }
  }

  return NULL;
}
