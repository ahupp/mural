#ifndef SEARCH_H
#define SEARCH_H

#include <vector>
#include "tags.h"

vector<TagInfo> find_best_fuzzy_matches(const vector<TagInfo>& tags,
                                        const string& query,
                                        const size_t max_items);

#endif
