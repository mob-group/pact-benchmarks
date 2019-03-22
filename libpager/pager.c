#include "pager.h"

#include <stdlib.h>
#include <string.h>

void do_nothing(UNUSED void *ptr) {}

page_entry_st table_data[DEFAULT_TABLE_SIZE] = {0};

page_table_st table = {
    .entries = table_data, .size = DEFAULT_TABLE_SIZE, .next_insert = 0};

page_functions_st heap_functions = {.deleter = (deleter_ft)free,
                                    .copier = (copier_ft)memcpy};

page_functions_st stack_functions = {.deleter = (deleter_ft)do_nothing,
                                     .copier = (copier_ft)memcpy};

void new_table_entry(void *h, void *d, size_t s, page_functions_st fns) {
  for (size_t i = 0; i < table.size; ++i) {
    if (table.entries[i].host == h) {
      table.entries[i].functions.deleter(table.entries[i].device);
      table.next_insert = i;
    }
  }

  // Handle the case where the table is full and we need to delete something
  // here (by deleting).
  size_t ni = table.next_insert;

  table.entries[ni].host = h;
  table.entries[ni].device = d;
  table.entries[ni].last_size = s;
  table.entries[ni].written = false;
  table.entries[ni].functions = fns;

  // Also mprotect here to track whether the host gets written or not

  fns.copier(d, h, s);

  table.next_insert = (ni + 1) % table.size;
}

void *get_table_entry(void *h) {
  for (size_t i = 0; i < table.size; ++i) {
    if (table.entries[i].host == h) {
      if (table.entries[i].written) {
        table.entries[i].functions.copier(table.entries[i].device, h,
                                          table.entries[i].last_size);
      }
      return table.entries[i].device;
    }
  }

  return NULL;
}
