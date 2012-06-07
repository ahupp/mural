#ifndef ENFORCE_H
#define ENFORCE_H

int enforce(int a, const char* blame) {
  if (a == -1) {
    perror(blame);
    exit(1);
  }
  return a;
}

#endif
