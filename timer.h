
#include <time.h>

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
