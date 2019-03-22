#ifndef PAGER_H
#define PAGER_H

#include <stdbool.h>
#include <stddef.h>

#define UNUSED __attribute__((unused))

#define DEFAULT_TABLE_SIZE 8

typedef void (*deleter_ft)(void *);
typedef void (*copier_ft)(void *, void *, size_t);

typedef struct page_functions {
  deleter_ft deleter;
  copier_ft copier;
} page_functions_st;

page_functions_st heap_functions;
page_functions_st stack_functions;

/**
 * A single entry in a page table. Stores host and device pointers, the last
 * allocated size on the device for this host and whether or not the memory has
 * been modified host-side since the entry was created.
 */
typedef struct page_entry {
  void *host;
  void *device;
  size_t last_size;
  bool written;

  page_functions_st functions;
} page_entry_st;

/**
 * A page table that can be iterated through.
 */
typedef struct page_table {
  page_entry_st *entries;
  size_t size;
  size_t next_insert;
} page_table_st;

/**
 * Create a new entry in a page table with the specified pointers. Will be
 * responsible for:
 *  - checking if there is an existing entry for that host pointer, and if so,
 *    deleting the device pointer that already exists for it.
 *  - mprotecting the page containing the host pointer so that if it's written
 *    to, we detect that and set the flag.
 *  - creating a table entry.
 */
void new_table_entry(void *h, void *d, size_t s, page_functions_st fns);

/**
 * Gets the device pointer for this particular host pointer - if the written
 * flag has been set then it needs to also do a copy through the table entry's
 * function pointer table.
 */
void *get_table_entry(void *h);

/**
 * Stores the backing data for the default page table.
 */
page_entry_st table_data[DEFAULT_TABLE_SIZE];

/**
 * The actual default page table.
 */
page_table_st table;

#endif
