
#include <Rmath.h>
#include <R.h>

typedef struct {

	double *array;
	int *array_dim;
	int *mask;
	int *nmat;
	int *nmat_dim;
} ppp;

typedef double optimfn(int n, double *par, void *ex);

void transform_par(double*, double*);
void inv_par(double*, double*);
double fx0(double*, double*);
double fx1(double*, double*);
double lik_ratio(double*, double*);
void pseudo_lik_Hartvig_est(double*, int*, int*, int*, int*, double*, double*);
void Hartvig(double*, int*, int*, int*, int*, double*, double*);
double fn(int, double*, ppp*);



void transform_par(double *par, double *invpar) {

	int i;

	for(i = 0; i < 4; i++) 
		invpar[i] = log(par[i]);
	invpar[4] = log(par[4] / (1.0 - par[4] - par[5]));
	invpar[5] = log(par[5] / (1.0 - par[4] - par[5]));
	invpar[6] = log(par[6]);
}

void inv_par(double *invpar, double *par) {

	int i;
	for(i = 0; i < 4; i++) 
		par[i] = exp(invpar[i]);
	par[4] = exp(invpar[4]) / (1.0 + exp(invpar[4]) + exp(invpar[5]));
	par[5] = exp(invpar[5]) / (1.0 + exp(invpar[4]) + exp(invpar[5]));
	par[6] = exp(invpar[6]);
}


double fx0(double *x, double *par) {

	double p_0, p_neg, ans;
	p_0 = par[4];
	p_neg = 1.0 - par[4] - par[5];
	
	ans = dnorm(*x, 0.0, 1.0, 0);
	ans *= p_0 / (p_0 + p_neg);
	
	if(*x < 0) 
		ans += dgamma(-1.0 * *x, par[2], 1.0 / par[3], 0) * p_neg / (p_0 + p_neg);

	return ans;
	
}

void fx0_1(double *x, double *par, double *ans) {

	double p_0, p_neg;
	p_0 = par[4];
	p_neg = 1.0 - par[4] - par[5];
	


	*ans = dnorm(*x, 0.0, 1.0, 0);
	*ans *= p_0 / (p_0 + p_neg);
	
	if(*x < 0) 
		*ans += dgamma(-1.0 * *x, par[2], 1.0 / par[3], 0) * p_neg / (p_0 + p_neg);

}

double fx1(double *x, double *par) {

	double ans = 0.0;

	if(*x > 0)
		ans = dgamma(*x, par[0], 1.0 / par[1], 0);
	
	return ans;
	
}

double lik_ratio(double *x, double *par) {

	double ans;
	
	ans = fx1(x, par) / fx0(x, par);
	
	return ans;
}


void pseudo_lik_Hartvig_est(double *array, int *array_dim, int *mask, int *nmat, int *nmat_dim, double *invpar, double *ans){

 
  int i, j, k, i1, j1, k1, x, y, z, n;  
  double par[7], sum = 0.0, prod1 = 1.0, prod2 = 1.0;
  
  inv_par(invpar, par);

  x = *(array_dim);
  y = *(array_dim + 1);
  z = *(array_dim + 2);



  for(i = 0; i < x; i++){
	  for(j = 0; j < y; j++){
		  for(k = 0; k < z; k++){

			  prod1 = 1.0;
			  prod2 = 1.0;

			  if(*(mask + i * y * z + j * z + k)) {
				  
				  prod1 = fx0((array + i * y * z + j * z + k), par);
				  prod2 = (1.0 + par[6] * lik_ratio((array + i * y * z + j * z + k), par));
				  
				  for(n = 0; n < *nmat_dim; n++) {
					  i1 = i + *(nmat + n * 3 + 0);
					  j1 = j + *(nmat + n * 3 + 1);
					  k1 = k + *(nmat + n * 3 + 2);
					  if(i1 >= 0 && i1 < x && j1 >= 0 && j1 < y && k1 >= 0 && k1 < z) {
						  if(*(mask + i1 * y * z + j1 * z + k1)) {
							  prod1 *= fx0((array + i1 * y * z + j1 * z + k1), par);
							  prod2 *= (1.0 + par[6] * lik_ratio((array + i1 * y * z + j1 * z + k1), par));
							  
							 
						  }
					  }
				  }
				  prod2 += 1.0 - par[5] * (1.0 + par[6]) / par[6];
				  prod2 *= par[5] / (par[6] * R_pow_di((1.0 + par[6]), *nmat_dim + 1));
				  
				  sum += log(prod1 * prod2);
				
			  }
		  }
	  }
  }
  *ans = sum;
  
}



/* void Hartvig(double *array, int *array_dim, int *mask, int *nmat, int *nmat_dim, double *par_start, double *par_est) { */

/* 	int fail, fncount; */
/* 	double invpar[7], tmp_inv_par[7], Fmin, abstol = 0.00000000001, intol = 0.000000001; */
/* 	ppp store; */

/* 	transform_par(par_start, invpar);  */
	

/* 	store.array = array; */
/* 	store.array_dim = array_dim; */
/* 	store.mask = mask; */
/* 	store.nmat = nmat; */
/* 	store.nmat_dim = nmat_dim; */
	
/* 	nmmin(7, tmp_inv_par, inv_par, &Fmin, fn, */
/* 	      &fail, abstol, intol, &store, */
/* 	      1.0, 0.5, 2.0, 1, */
/* 	      &fncount, 500); */
	
/* 	inv_par(tmp_inv_par, par_est); */

/* } */

/* double fn(int n, double *invpar, ppp *ex) { */

/* 	double par[7], ans; */
	
/* 	inv_par(invpar, par); */
	
/* 	pseudo_lik_Hartvig_est(ex->array, ex->array_dim, ex->mask, ex->nmat, ex->nmat_dim, par, &ans); */
	
/* 	return ans; */
/* } */




