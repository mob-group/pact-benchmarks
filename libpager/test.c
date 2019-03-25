#include "pager.h"

#include <stdio.h>
#include <stdlib.h>

int main(void) {
  for (int i = 0; i < 11; ++i) {
    int *h = malloc(sizeof(int));
    *h = 1;

    int *d = malloc(sizeof(int));

    new_table_entry(h, d, sizeof(int), heap_functions);

    for (int i = 0; i < 8; ++i) {
      printf("h:%p d:%p\n", table.entries[i].host, table.entries[i].device);
    }
    printf("\n");
  }

  int *h = malloc(sizeof(int));
  *h = 4;

  int *d = malloc(sizeof(int));

  new_table_entry(h, d, sizeof(int), heap_functions);
  for (int i = 0; i < 8; ++i) {
    printf("h:%p d:%p\n", table.entries[i].host,
           get_table_entry(table.entries[i].host));
  }
  printf("\n");

  *h = 2;
  printf("%d\n", *(int *)get_table_entry(h));

  *h = 3;
  printf("%d\n", *(int *)get_table_entry(h));
}
