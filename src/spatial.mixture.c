#include "R.h"
#include <Rmath.h>

void spatial_mixture(double *lik_ratio, int *dim, int *ksize, int *mask, int *kernel_type, double *gamma, double *p, double *ans) {

	int e, i, j, k, m1, m2, m3, x, y, z, num;
	double t, f, alpha, v0;


	x = *dim;
	y = *(dim + 1);
	z = *(dim + 2);

	e = (*ksize - 1) / 2;

	for(i = 0; i < x; i++) {
		for(j = 0; j < y; j++) {
			for(k = 0; k < z; k++) {
				if(*(mask + i * y * z + j * z + k) == 1) {

					t = 1.0;
					num = 1;
					
					if(*kernel_type == 3) {
						for(m1 = -e; m1 <= e; m1++) {
							for(m2 = -e; m2 <= e; m2++) {
								for(m3 = -e; m3 <= e; m3++) {
									if( (m1 != 0) || (m2 != 0) || (m3 != 0)) {
										if(((i + m1 + 1) > 0) && ((i + m1 + 1) <= x) && ((j + m2 + 1) > 0) && ((j + m2 + 1) <= y) && ((k + m3 + 1) > 0) && ((k + m3 + 1) <= z)) {
											if(*(mask + (i + m1) * y * z + (j + m2) * z + (k + m3)) == 1) {
												t *= (1.0 + *gamma * *(lik_ratio + (i + m1) * y * z + (j + m2) * z + (k + m3)));
												num +=1;
												
											}
										}
									}
								}
							}
						}	
						
					}	
					if(*kernel_type == 2) {
						for(m1 = -e; m1 <= e; m1++) {
							for(m2 = -e; m2 <= e; m2++) {
								if( (m1 != 0) || (m2 != 0)) {
									if(((i + m1 + 1) > 0) && ((i + m1 + 1) <= x) && ((j + m2 + 1) > 0) && ((j + m2 + 1) <= y)) {
										if(*(mask + (i + m1) * y * z + (j + m2) * z + k) == 1) {
											t *= (1.0 + *gamma * *(lik_ratio + (i + m1) * y * z + (j + m2) * z + k));
											num +=1;
											
										}
									}
								}
							}
						}
					}
					
					alpha = *p / R_pow_di(1.0 + *gamma, num - 1);
					
					v0 = *(lik_ratio + i * y * z + j * z + k);
					f = (1 - alpha * R_pow_di(1.0 + *gamma, num) / *gamma) / alpha;
					*(ans + i * y * z + j * z + k) = 1.0 / (1.0 + (1.0 / *gamma + f / t) / v0);

/* 					Rprintf("alpha = %f v0 = %f f = %f t = %f num = %d\n", alpha, v0, f, t, num); */
				}
			}
		}
	}
}
									





