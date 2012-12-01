#include <algorithm>

#include "tags.h"



bool is_fuzzy_match(const string& symbol, const string& query,
                    int* inter_count_ret) {

  size_t qi = 0;
  size_t si = 0;
  int lastmatch = -1;

  // The number of characters between fuzzy matches.  This is 0 if
  // query is a substring of symbol.  Larger values of inter_count
  // will generally be worse matches than smaller values.
  int inter_count = 0;

  while (qi < query.length() && si < symbol.length()) {
    if (query[qi] == symbol[si]) {
      if (lastmatch != -1) {
        inter_count += si - lastmatch - 1;
      }
      lastmatch = si;
      ++qi;
    }
    ++si;
  }
  *inter_count_ret = inter_count;
  return qi == query.length();
}



typedef pair<const TagInfo*, int> FuzzyMatch;

bool is_better_match(const FuzzyMatch& lhs, const FuzzyMatch& rhs) {
  if (lhs.second < rhs.second) {
    return true;
  } else if (lhs.second == rhs.second) {
    if (lhs.first->symbol.length() < rhs.first->symbol.length()) {
      return true;
    }
  }
  return false;
}

vector<TagInfo> find_fuzzy_matches(const vector<TagInfo>& tags,
                                   const string& query,
                                   const size_t max_items) {
  vector<FuzzyMatch> matches;

  // Searches that don't explicitly look like a method ("::") will
  // never include methods, and vice-versa.
  bool methods_only = is_method_tag(query);

  bool ignore_case = query == lowercase(query);

  for (size_t i = 0; i < tags.size(); ++i) {
    int inter_count;
    const TagInfo& tag = tags[i];

    if (tag.is_method != methods_only) {
      // could split the tags into method and non-method sets and only
      // search one, but right now it's fast enough.
      continue;
    }

    if (is_fuzzy_match(ignore_case ? tag.symbol_lc : tag.symbol,
                       query, &inter_count)) {
      // Passing &tag here is an important perf optimization to avoid
      // excessive copying when there are many possible results (like
      // "A")
      matches.push_back(make_pair(&tag, inter_count));
    }
  }

  size_t limit = min(max_items, matches.size());
  partial_sort(matches.begin(), matches.begin()+limit, matches.end(),
               is_better_match);

  vector<TagInfo> result;
  for (size_t i = 0; i < limit; ++i) {
    result.push_back(*(matches[i].first));
  }

  return result;
}
