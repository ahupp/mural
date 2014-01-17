
using namespace std;

#include <iostream>
#include <cstdlib>
#include <stdio.h>
#include <string>
#include <vector>
#include <sstream>
#include <sys/epoll.h>
#include <sys/inotify.h>
#include <assert.h>
#include <time.h>

#include "tags.h"
#include "search.h"

int enforce(int a, const char* blame) {
  if (a == -1) {
    perror(blame);
    exit(1);
  }
  return a;
}

class Timer {
  struct timespec start;
public:
  Timer() {
    clock_gettime(CLOCK_MONOTONIC, &start);
  }

  long elapsedMS() {
    struct timespec end;
    clock_gettime(CLOCK_MONOTONIC, &end);

    return 1000*(end.tv_sec - start.tv_sec) +
      (long)((end.tv_nsec - start.tv_nsec)/(1000*1000));
  }
};


void handle_one_query(const vector<TagInfo>& tags) {

  string query;
  getline(cin, query);
  if (!query.length()) {
    cout << "DONE" << endl;
    return;
  }

  Timer t;
  vector<TagInfo> matches = find_best_fuzzy_matches(tags, query, 32);

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

  string tags_file(argv[1]);

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


