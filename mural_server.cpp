
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>

#include "timer.h"

using namespace std;

/*
 * first is a symbol or file name
 * second is the fuzzy match score, smaller is better
 */
typedef pair<string, int> fuzzy_match;

bool is_fuzzy_match(const string& symbol, const string& query,
                    int* inter_count) {

  int qi = 0;
  int si = 0;
  int lastmatch = -1;

  // The number of characters between fuzzy matches.  This is 0 if
  // query is a substring of symbol.  Larger values of inter_count
  // will generally be worse matches than smaller values.
  *inter_count = 0;
  while (qi < query.length() && si < symbol.length()) {
    if (query[qi] == symbol[si]) {
      if (lastmatch != -1) {
        *inter_count += si - lastmatch - 1;
      }
      lastmatch = si;
      ++qi;
    }
    ++si;
  }
  return qi == query.length();
}


vector<string> read_tags_file(const char* tagfile) {
  vector<string> tags;
  ifstream tag_stream(tagfile);

  bool expecting_filename = false;
  string line;
  while (!tag_stream.eof()) {
    getline(tag_stream, line);

    if (expecting_filename) {
      expecting_filename = false;

      size_t comma = line.find(',');
      if (comma != string::npos) {
        tags.push_back(line.substr(0, comma));
      }
    } else if (line[0] == 0x0C) {
      expecting_filename = true;
      continue;
    } else {

      size_t tag_start = line.find(0x7F);
      if (tag_start == string::npos) {
        continue;
      }
      size_t tag_end = line.find(0x01, tag_start);
      if (tag_end == string::npos) {
        continue;
      }

      string symbol = line.substr(tag_start + 1, tag_end-tag_start-1);
      tags.push_back(symbol);
    }
  }
  return tags;
}

bool is_better_match(const fuzzy_match& lhs, const fuzzy_match& rhs) {
  if (lhs.second < rhs.second) {
    return true;
  } else if (lhs.second == rhs.second) {
    if (lhs.first.length() < rhs.first.length()) {
      return true;
    }
  }
  return false;
}

vector<fuzzy_match> find_fuzzy_matches(const vector<string>& tags,
                                       const string& query) {
  vector<fuzzy_match> matches;
  for (int i = 0; i < tags.size(); ++i) {
    const string& symbol = tags[i];
    int inter_count;
    if (is_fuzzy_match(symbol, query, &inter_count)) {
      matches.push_back(make_pair(symbol, inter_count));
    }
  }
  return matches;
}

int main(int argc, char** argv) {

  if (argc != 2) {
    cerr << "usage: " << argv[0] << " tagfile" << endl;
    return 1;
  }

  vector<string> tags = read_tags_file(argv[1]);

  while (!cin.eof()) {
    string query;
    getline(cin, query);
    if (query.length()) {
      Timer t;
      vector<fuzzy_match> matches = find_fuzzy_matches(tags, query);

      const size_t limit = 32;
      if (matches.size() > limit) {
        partial_sort(matches.begin(), matches.begin()+limit, matches.end(),
                     is_better_match);
      }

      for (int i = 0; i < min(limit, matches.size()); ++i) {
        cout << "MATCH " << matches[i].first << endl;
      }
      cout << "DONE " << t.elapsedMS() << "ms " <<
        "#match: " << matches.size() <<  endl;
    } else {
      cout << "DONE" << endl;
    }
  }
  return 0;
}


