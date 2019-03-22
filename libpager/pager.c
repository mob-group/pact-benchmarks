#include "pager.h"

page_entry_st default_page_table_data[DEFAULT_TABLE_SIZE] = {0};

page_table_st default_page_table = {.entries = default_page_table_data,
                                    .size = DEFAULT_TABLE_SIZE,
                                    .next_insert = 0};

void new_table_entry(page_table_st *tab, void *h, void *d, size_t s) {
  for (size_t i = 0; i < tab->size; ++i) {
    if (tab->entries[i].host == h) {
      // Delete through that page table entry using FPs
      tab->next_insert = i;
    }
  }

  size_t ni = tab->next_insert;

  tab->entries[ni].host = h;
  tab->entries[ni].device = d;
  tab->entries[ni].last_size = s;
  tab->entries[ni].written = false;

  // Also mprotect here to track whether the host gets written or not

  tab->next_insert = (ni + 1) % tab->size;
}
