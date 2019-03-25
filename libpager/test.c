#include "pager.h"

#include <stdio.h>
#include <stdlib.h>

int main(void) {
  int h = 4;
  int d;

  new_table_entry(&h, &d, sizeof(int), stack_functions);
  printf("%d\n", *(int *)get_table_entry(&h));
}
