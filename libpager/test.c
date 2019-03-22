#include "pager.h"

#include <stdio.h>

int main(int argc, char **argv) {
  int h = 5;
  int d = 9;

  int h2 = 39;
  int d2 = 6;

  new_table_entry(&default_page_table, &h, &d, 1);
  new_table_entry(&default_page_table, &h2, &d2, 1);

  for (size_t i = 0; i < default_page_table.size; ++i) {
    page_entry_st ent = default_page_table.entries[i];
    printf("h: %p d: %p\n", ent.host, ent.device);
  }
}
