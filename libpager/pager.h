#ifndef PAGER_H
#define PAGER_H

#include <stdbool.h>
#include <stddef.h>

#define DEFAULT_TABLE_SIZE 8

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
} page_entry_st;

/**
 * A page table that can be iterated through.
 */
typedef struct page_table {
  page_entry_st *entries;
  size_t size;
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
void new_table_entry(page_table *tab, void *h, void *d, size_t s);

/**
 * Stores the backing data for the default page table.
 */
page_entry_st default_page_table_data[DEFAULT_TABLE_SIZE];

/**
 * The actual default page table.
 */
page_table_st default_page_table;

#endif
