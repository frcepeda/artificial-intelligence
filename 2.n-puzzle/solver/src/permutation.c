#include "permutation.h"

#define FLIM 17

long long pnum(int n, int k, int *a){
	int b = 0, r = 0;
	static long long f[FLIM] = {1};
	if (!f[1])
		for (int i = 1; i < FLIM; i++)
			f[i] = f[i-1] * i;

	for (int i = 0; i < n; i++){
		int me = a[i] - __builtin_popcount(b & ((1<<a[i])-1));
		r += f[n-1] / f[n-1-(k-1)] * me;
		b |= 1 << a[i];
	}

	return r;
}
