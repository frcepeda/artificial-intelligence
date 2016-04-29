#include "permutation.h"
#include <stdio.h>

#define FLIM 17

typedef long long ll;

ll pnum(ll *a){
	register int b = 0, n = 16, k = 8, i;
	ll r = 0;
	static ll f[FLIM] = {1};

	if (!f[1])
		for (i = 1; i < FLIM; i++)
			f[i] = f[i-1] * i;

	for (i = 0; k; i++){
		register int ai = a[i];
		register int me = ai - __builtin_popcount(b & ((1<<ai)-1));
		n--, k--;
		r += f[n] / f[n-k] * me;
		b |= 1 << ai;
	}

	return r;
}

/*
int main(){
	int i, n, k, a[16];
	scanf("%d %d", &n, &k);
	for (i = 0; i < n; i++)
		scanf("%d", &a[i]);
	printf("%lld\n", pnum(n, k, a));
}
//*/
