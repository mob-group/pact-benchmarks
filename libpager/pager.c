#include "pager.h"

page_entry_st default_page_table_data[DEFAULT_TABLE_SIZE] = {0};

page_table_st default_page_table = {.entries = default_page_table_data,
                                    .size = DEFAULT_TABLE_SIZE};
