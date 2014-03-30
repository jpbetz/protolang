/* cbits
$ gcc -fPIC -shared cbits.c -o cbits.so
$ clang -fPIC -shared cbits.c -o cbits.so
*/

#include "stdio.h"

// putchard - putchar that takes a double and returns 0.
double print(double X) {
  printf("%f\n", X);
  fflush(stdout);
  return 0;
}