#include <set>
#include <fstream>
#include <iostream>
#include <unistd.h>
#include <cstdlib>
#include <stdlib.h> 

#include "tags.h"

bool is_method_tag(const string& tag) {
  return tag.find("::") != string::npos;
}

string lowercase(const string& s) {
  string ret = s;
  for (size_t i = 0; i < ret.length(); ++i) {
    ret[i] = tolower(ret[i]);
  }
  return ret;
}


vector<string> split(const string& line, char delim) {

  vector<string> result;

  size_t i = 0;
  while (1) {

    size_t next = line.find(delim, i);
    if (next == string::npos) {
      break;
    }

    result.push_back(line.substr(i, next - i));

    i = next + 1;
  }
  result.push_back(line.substr(i, line.length() - i));

  return result;
}

void read_ctags_file(ifstream& tag_stream, vector<TagInfo>& tags) {

  set<string> seen_files;

  while (!tag_stream.eof()) {
    string line;
    getline(tag_stream, line);

    if (line[0] == '!') {
      continue;
    }

    vector<string> splat = split(line, '\t');
    if (splat.size() != 4) {
      continue;
    }

    string row = splat[2];
    if (row.length() > 2 && row.substr(row.length()-2, row.length()) == ";\"") {
      row = row.substr(0, row.length() - 2);
    }

    string filename = splat[1];
    if (seen_files.find(filename) == seen_files.end()) {
      seen_files.insert(filename);
      tags.push_back(TagInfo(filename, filename, 0));
    }

    tags.push_back(TagInfo(splat[0], filename, atoi(row.c_str())));
  }
}

void read_etags_file(ifstream& tag_stream, vector<TagInfo>& tags) {

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
      size_t row_end = line.find(',', tag_end);
      if (row_end == string::npos) {
        continue;
      }

      string symbol = line.substr(tag_start + 1, tag_end-tag_start-1);
      string row = line.substr(tag_end+1, row_end);

      tags.push_back(TagInfo(symbol, current_filename, atoi(row.c_str())));
    }
  }
}

void read_tags_file(const string& tags_file, vector<TagInfo>& tags) {

  if (0 != access(tags_file.c_str(), R_OK)) {
    cerr << "error: " << tags_file << " not readable" << endl;
    exit(1);
  }

  tags.clear();

  ifstream tags_stream(tags_file.c_str());

  string line;
  getline(tags_stream, line);
  tags_stream.seekg(0);

  if (line.length() && line[0] == '!') {
    read_ctags_file(tags_stream, tags);
  } else {
    read_etags_file(tags_stream, tags);
  }
}
