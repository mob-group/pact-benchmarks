#include "pager.h"

#include <stdio.h>
#include <stdlib.h>

int main(void) {
  int *h = malloc(sizeof(int));
  *h = 4;

  int *d = malloc(sizeof(int));

  new_table_entry(h, d, sizeof(int), heap_functions);

  *h = 2;
  printf("%d\n", *(int *)get_table_entry(h));

  *h = 3;
  printf("%d\n", *(int *)get_table_entry(h));
}
