#ifndef TAGS_H
#define TAGS_H

#include <vector>
#include <string>

using namespace std;

bool is_method_tag(const string& tag);

string lowercase(const string& s);

struct TagInfo {
  string symbol;
  string symbol_lc;
  string file;
  int row;
  bool is_method;

  TagInfo(const string& _symbol, const string& _file, int _row)
      : symbol(_symbol), symbol_lc(lowercase(_symbol)),
        file(_file), row(_row), is_method(is_method_tag(_symbol))
    {}
};

void read_tags_file(const string& tags_file, vector<TagInfo>& tags);

#endif
