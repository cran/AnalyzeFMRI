
#ifdef USING_R
  typedef double Sfloat;
  typedef int Sint;
#define SINT_MAX INT_MAX
#else
  typedef float Sfloat;
  typedef long Sint;
#define SINT_MAX LONG_MAX
#endif
