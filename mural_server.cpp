
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>

#include "timer.h"

using namespace std;

struct TagInfo {
  string symbol;
  string file;
  int row;

  TagInfo(const string& _symbol, const string& _file, int _row)
  : symbol(_symbol), row(_row), file(_file) {}
};


bool is_fuzzy_match(const string& symbol, const string& query,
                    int* inter_count) {

  int qi = 0;
  int si = 0;
  int lastmatch = -1;

  // The number of characters between fuzzy matches.  This is 0 if
  // query is a substring of symbol.  Larger values of inter_count
  // will generally be worse matches than smaller values.
  *inter_count = 0;

  // Searches that don't explicitly look like a method ("::") will
  // never include methods, and vice-versa.  This avoids polluting the
  // results with lots of results from the same class.
  if ((symbol.find("::") == string::npos) !=
      (query.find("::") == string::npos)) {
    return false;
  }

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


vector<TagInfo> read_tags_file(const char* tagfile) {

  ifstream tag_stream(tagfile);

  vector<TagInfo> tags;

  bool expecting_filename = false;
  string current_filename;
  while (!tag_stream.eof()) {
    string line;
    getline(tag_stream, line);

    if (expecting_filename) {
      expecting_filename = false;

      size_t comma = line.find(',');
      if (comma != string::npos) {
        current_filename = line.substr(0, comma);
        tags.push_back(TagInfo(current_filename, current_filename, 0));
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
      size_t col_end = line.find(',', tag_end);
      if (col_end == string::npos) {
        continue;
      }

      string symbol = line.substr(tag_start + 1, tag_end-tag_start-1);
      string column = line.substr(tag_end+1, col_end);

      tags.push_back(TagInfo(symbol, current_filename, atoi(column.c_str())));
    }
  }
  return tags;
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
                                   const size_t limit) {
  vector<FuzzyMatch> matches;

  for (int i = 0; i < tags.size(); ++i) {
    int inter_count;
    const TagInfo& tag = tags[i];
    if (is_fuzzy_match(tag.symbol, query, &inter_count)) {
      matches.push_back(make_pair(&tag, inter_count));
    }
  }

  if (matches.size() > limit) {
    partial_sort(matches.begin(), matches.begin()+limit, matches.end(),
                 is_better_match);
  }

  vector<TagInfo> result;
  for (int i = 0; i < min(limit, matches.size()); ++i) {
    result.push_back(*(matches[i].first));
  }

  return result;
}

int main(int argc, char** argv) {

  if (argc != 2) {
    cerr << "usage: " << argv[0] << " tagfile" << endl;
    return 1;
  }

  vector<TagInfo> tags = read_tags_file(argv[1]);

  while (!cin.eof()) {
    string query;
    getline(cin, query);

    if (query.length()) {
      Timer t;
      vector<TagInfo> matches = find_fuzzy_matches(tags, query, 32);

      for (int i = 0; i < matches.size(); ++i) {
        const TagInfo& match = matches[i];
        cout << "MATCH " << match.symbol << endl;
      }
      cout << "DONE " << t.elapsedMS() << "ms " <<
        "#match: " << matches.size() <<  endl;
    } else {
      cout << "DONE" << endl;
    }
  }
  return 0;
}


