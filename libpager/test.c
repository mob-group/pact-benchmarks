#include "pager.h"

#include <stdio.h>
#include <stdlib.h>

int main(void) {
  for (int i = 0; i < 11; ++i) {
    int *h = malloc(sizeof(int));
    *h = 1;

    int *d = malloc(sizeof(int));

    new_table_entry(h, d, sizeof(int), heap_functions);
  }

  int *h = malloc(sizeof(int));
  *h = 3;
  int *d = malloc(sizeof(int));

  new_table_entry(h, d, sizeof(int), heap_functions);

  printf("%d\n", *(int *)get_table_entry(h));
  *h = 4;
  printf("%d\n", *(int *)get_table_entry(h));
}
