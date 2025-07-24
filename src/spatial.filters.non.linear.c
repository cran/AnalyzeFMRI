#include <R.h>
#include "verR.h"
#include <math.h>

/*functions that apply non-linear gaussian weighted smoothing to
  (i)   3D arrays
  (ii)  4D arrays (using sums of squared differences between time-series)*/

void non_lin_gauss_smooth(float *array, Sint *array_dim, float *mask, float *radius, float *g, float *pixdim, float *ans_array){

/* spatial smoothing using a gaussian kernel with sd=g to supply the weights */

  int i,j,k,l,i1,j1,k1,nk,*u;
  Sint x,y,z;
  float f=0.0,total=0.0,a,b,c,dist,yi,yj;

 x= *(array_dim);
 y= *(array_dim+1);
 z= *(array_dim+2);

 // u=Calloc(3,int);
 u = (int *)calloc(3, sizeof(int));
 nk=0;
 for(i=-19;i<20;i++){
   for(j=-19;j<20;j++){
     for(k=-19;k<20;k++){
       a=((float) i)*((float) i)*(*(pixdim))*(*(pixdim));
       b=((float) j)*((float) j)*(*(pixdim+1))*(*(pixdim+1));
       c=((float) k)*((float) k)*(*(pixdim+2))*(*(pixdim+2));
       dist=sqrt(a+b+c);
       
       if(dist<=*radius){
	 *(u+3*nk)=i;
	 *(u+3*nk+1)=j;
	 *(u+3*nk+2)=k;
	 nk+=1;
	 //	 u=Realloc(u,3*(nk+1),int);
	 u = (int *)realloc(u, 3 * (nk + 1) * sizeof(int));
       }
     }
   }
 }

 // u=Realloc(u,3*nk,int);
 u = (int *)realloc(u, 3 * nk * sizeof(int));

 for(i=0;i<x;i++){
   for(j=0;j<y;j++){
     for(k=0;k<z;k++){
       
       

       f=0.0;
       total=0.0;
       
       for(l=0;l<nk;l++){
	 i1=i+*(u+l*3);
	 j1=j+*(u+l*3+1);
	 k1=k+*(u+l*3+2);

	 if(i1>=0 && i1<x && j1>=0 && j1<y && k1>=0 && k1<z){
	   if(*(mask+i1*y*z+j1*z+k1)==1.0){
	     yi=*(array+i*y*z+j*z+k);
	     yj=*(array+i1*y*z+j1*z+k1);
	     a=exp(-(yi-yj)*(yi-yj)/(2*(*g)*(*g)));
	     total+=yj*a;
	     f+=a;
	   }
	 }
       }

       *(ans_array+i*y*z+j*z+k)=total/f;
     }
   }
 }


}

void temporal_non_lin_gauss_smooth(float *array, Sint *array_dim, float *mask, float *radius, float *g, float *pixdim, float *ans_array){

  /*smooths time-series spatially based on sums of squared differences in pairs of time-series*/
  int i,j,k,l,m,n,i1,j1,k1,nk,*u;
  Sint x,y,z,t;
  float f=0.0,a,b,c,dist,temp;

 x= *(array_dim);
 y= *(array_dim+1);
 z= *(array_dim+2);
 t= *(array_dim+3);

 // u=Calloc(3,int);
 u = (int *)calloc(3, sizeof(int));
 nk=0;
 for(i=-19;i<20;i++){
   for(j=-19;j<20;j++){
     for(k=-19;k<20;k++){
       a=((float) i)*(*pixdim);
       b=((float) j)*(*(pixdim+1));
       c=((float) k)*(*(pixdim+2));
       dist=sqrt(a*a+b*b+c*c);
       
       if(dist<=*radius){
	 *(u+3*nk)=i;
	 *(u+3*nk+1)=j;
	 *(u+3*nk+2)=k;
	 nk+=1;
	 // u=Realloc(u,3*(nk+1),int); 
	 u = (int *)realloc(u, 3 * (nk + 1) * sizeof(int));
      }
     }
   }
 }

 // u=Realloc(u,3*nk,int);
 u = (int *)realloc(u, 3 * nk * sizeof(int));

 for(i=0;i<x;i++){
   for(j=0;j<y;j++){
     for(k=0;k<z;k++){
    
       

       f=0.0;
       
       for(l=0;l<nk;l++){
	 i1=i+*(u+l*3);
	 j1=j+*(u+l*3+1);
	 k1=k+*(u+l*3+2);

	 if(i1>=0 && i1<x && j1>=0 && j1<y && k1>=0 && k1<z){
/* 	   if(i1==i+1  && j1==j+1  && k1==k+1){ */
	 if(*(mask+i1*y*z+j1*z+k1)==1.0){
	     temp=0.0;
	     for(m=0;m<t;m++){
	       temp+=(*(array+i*y*z*t+j*z*t+k*t+m)-*(array+i1*y*z*t+j1*z*t+k1*t+m))*(*(array+i*y*z*t+j*z*t+k*t+m)-*(array+i1*y*z*t+j1*z*t+k1*t+m));
										   }
	     a=exp(-temp/(2*(*g)*(*g)));
   for(n=0;n<t;n++){
     *(ans_array+i*y*z*t+j*z*t+k*t+n)+=*(array+i1*y*z*t+j1*z*t+k1*t+n)*a;
/*       *(ans_array+i*y*z*t+j*z*t+k*t+n)=a; */
}
	   
             f+=a;
	   }
	 }
       }
       if(*(mask+i*y*z+j*z+k)==1.0){
        for(n=0;n<t;n++) *(ans_array+i*y*z*t+j*z*t+k*t+n)/=f;
       }
      
     }
   }
 }

}
