#include "pager.h"

#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

void do_nothing(UNUSED void *ptr) {}

page_entry_st table_data[DEFAULT_TABLE_SIZE] = {0};

page_table_st table = {
    .entries = table_data, .size = DEFAULT_TABLE_SIZE, .next_insert = 0};

page_functions_st heap_functions = {.deleter = (deleter_ft)free,
                                    .copier = (copier_ft)memcpy};

page_functions_st stack_functions = {.deleter = (deleter_ft)do_nothing,
                                     .copier = (copier_ft)memcpy};

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

  table.next_insert = (table.next_insert + 1) % table.size;
}

void *get_table_entry(void *h) {
  for (size_t i = 0; i < table.size; ++i) {
    if (table.entries[i].host == h) {
      if (table.entries[i].written) {
        table.entries[i].functions.copier(table.entries[i].device, h,
                                          table.entries[i].last_size);
        table.entries[i].written = false;
      }
      return table.entries[i].device;
    }
  }

  return NULL;
}
