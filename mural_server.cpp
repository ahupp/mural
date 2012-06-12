
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <sstream>
#include <sys/epoll.h>
#include <sys/inotify.h>
#include <assert.h>

#include "timer.h"
#include "enforce.h"

using namespace std;

bool is_method_tag(const string& tag) {
  return tag.find("::") != string::npos;
}

struct TagInfo {
  string symbol;
  string file;
  int row;
  bool is_method;

  TagInfo(const string& _symbol, const string& _file, int _row)
   : symbol(_symbol), file(_file), row(_row), is_method(is_method_tag(_symbol))
    {}
};


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

void read_ctags_file(const string& tagfile, vector<TagInfo>& tags) {

  tags.clear();

  ifstream tag_stream(tagfile.c_str());

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

void read_etags_file(const string& tagfile, vector<TagInfo>& tags) {

  tags.clear();

  ifstream tag_stream(tagfile.c_str());

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

  for (size_t i = 0; i < tags.size(); ++i) {
    int inter_count;
    const TagInfo& tag = tags[i];

    if (tag.is_method != methods_only) {
      // could split the tags into method and non-method sets and only
      // search one, but right now it's fast enough.
      continue;
    }
    if (is_fuzzy_match(tag.symbol, query, &inter_count)) {
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

void handle_one_query(const vector<TagInfo>& tags) {

  string query;
  getline(cin, query);
  if (!query.length()) {
    cout << "DONE" << endl;
    return;
  }

  Timer t;
  vector<TagInfo> matches = find_fuzzy_matches(tags, query, 32);

  for (size_t i = 0; i < matches.size(); ++i) {
    const TagInfo& match = matches[i];
    cout << "MATCH\t"
         << match.symbol << "\t"
         << match.file << "\t"
         << match.row << endl;;
  }
  cout << "DONE " << t.elapsedMS() << "ms "
       << "#match: " << matches.size() << endl;
}

void mural_epoll_add(const int epoll_fd, const int fd) {
  struct epoll_event evt;
  evt.data.fd = fd;
  evt.events = EPOLLIN;
  enforce(epoll_ctl(epoll_fd, EPOLL_CTL_ADD, fd, &evt), "mural_add_epoll");
}

void read_tags_file(const string& tags_file, vector<TagInfo>& tags) {

  if (0 != access(tags_file.c_str(), R_OK)) {
    cerr << "error: " << tags_file << " not readable" << endl;
    exit(1);
  }

  read_etags_file(tags_file, tags);
}

void read_inotify_events(const int inotify_fd,
                         const string& tag_file,
                         vector<TagInfo>& tags) {

  const int EVENT_SIZE = sizeof(struct inotify_event);
  const int BUF_SIZE = 1024 * (EVENT_SIZE + 16);
  char buf[BUF_SIZE];

  // The examples I've read check for EINTR, but its not clear when
  // that would happen, or what I should do about it.  So just fail
  // hard.  They also include a check for len == 0, same thing.
  int len = enforce(read(inotify_fd, buf, BUF_SIZE), "read");

  int i = 0;
  while (i < len) {

    struct inotify_event *event = (struct inotify_event*) &buf[i];

    if (event->mask & IN_CLOSE_WRITE) {
      // it can only be one thing...
      read_tags_file(tag_file, tags);
    }

    i += EVENT_SIZE + event->len;
  }
}

int main(int argc, char** argv) {

  if (argc != 2) {
    cerr << "usage: " << argv[0] << " tagfile" << endl;
    return 1;
  }

  const string tags_file(argv[1]);

  vector<TagInfo> tags;
  read_tags_file(tags_file, tags);

  int inotify_fd = enforce(inotify_init(), "inotify_init");
  int efd = enforce(epoll_create(2), "epoll_create");

  mural_epoll_add(efd, 0);
  mural_epoll_add(efd, inotify_fd);

  enforce(
    inotify_add_watch(inotify_fd, tags_file.c_str(), IN_CLOSE_WRITE),
    "inotify_add_watch");

  const int MAXEVENTS = 64;
  struct epoll_event* events = new struct epoll_event[MAXEVENTS];

  while (1) {
    int n = epoll_wait(efd, events, MAXEVENTS, -1);
    for (int i = 0; i < n; ++i) {
      int active_fd = events[i].data.fd;
      if (active_fd == 0) {
        handle_one_query(tags);
      } else if (active_fd == inotify_fd) {
        read_inotify_events(inotify_fd, tags_file, tags);
      } else {
        assert(false);
      }
    }
    if (cin.eof()) {
      break;
    }
  }
  return 0;
}


