#include "pager.h"

#include <stdio.h>

int main(int argc, char **argv) {
  page_entry_st e;
  printf("%zu\n", sizeof(e));
  printf("%zu\n", sizeof(default_page_table));

  for (size_t i = 0; i < default_page_table.size; ++i) {
    page_entry_st e = default_page_table.entries[i];
  }
}
