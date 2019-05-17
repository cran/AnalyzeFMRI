#include <R.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

struct header{ 
  int sizeof_hdr;
  char data_type[10];
  char db_name[18];
  int extents;
  short session_error;
  char regular;
  char hkey_un0;
  short dim[8];
  char vox_units[4];
  char cal_units[8];
  short unused1;
  short datatype;
  short bitpix;
  short dim_un0;
  float pixdim[8];
  float vox_offset;
  float funused1;
  float funused2;
  float funused3;
  float cal_max;
  float cal_min;
  float compressed;
  float verified;
  int glmax;
  int glmin;
  char descrip[80];
  char aux_file[24];
  char orient;

  /*
SPM uses one of the Analyze header fields in an unorthodox way. This is the Originator field (data_history.originator - see Mayo/Analyze site ). 
In the basic format, this is meant to be 10 bytes of text. In SPM, this space is used to contain three short (two byte) integers. 
These numbers describe the current centre or 'Origin' of the image. Specifically, they give the coordinate of the central voxel, in voxels, in X, then Y then Z. 
For example, for images that are aligned to the templates, this Origin field would contain the coordinates of the voxel nearest to the midline of the Anterior Commisure. 
Note that if the Origin is set to 0 0 0, then SPM routines will assume that the origin is in fact the central voxel of the image.
  */

  //  char originator[10];
  short originator[5];

  char generated[10];
  char scannum[10];
  char patient_id[10];
  char exp_date[10];
  char exp_time[10];
  char hist_un0[3];
  int views; 
  int vols_added; 
  int start_field; 
  int field_skip; 
  int omax; 
  int omin;
  int smax;
  int smin;
};


struct data_array{
  int x;
  int y;
  int z;
  int t;
  int n;
  float *data;
};

void swaptest_wrap_JM(int *, char**);
void swaptest_JM(int *, char*);
void swap_JM(void*, int);

void readchar_JM(char*, char*, int*, int, long, int);
void read2byte_JM(short*, char*, int* , int, long, int);
void read4byte_JM(int*, char*, int* , int, long, int);
void readfloat_JM(float*, char*, int*, int, long, int);
void readdouble_JM(double*, char*, int*, int, long, int);

void readchar_v1_JM(int*, char**, int* , int*, int*, int*);
void read2byte_v1_JM(int*, char**, int* , int*, int*, int*);
void read4byte_v1_JM(int*, char**, int* , int*, int*, int*);
void readfloat_v1_JM(float*, char**, int*, int*, int*, int*);
void readdouble_v1_JM(double*, char**, int*, int*, int*, int*);

void read2byte_F_JM(float*, char*, int* , int, long, int);
void read4byte_F_JM(float*, char*, int* , int, long, int);
void readfloat_F_JM(float*, char*, int*, int, long, int);
void readdouble_F_JM(float*, char*, int*, int, long, int);

void write8bit_JM(int*, char**, int*);
void write2byte_JM(int*, char**, int*);
void writefloat_JM(float*, char**, int*);


void read_analyze_header_JM(struct header*, char*, int*);
void print_analyze_header_JM(struct header*);

void read_data_as_float_JM(struct data_array *, struct header *, char*, int*);
void create_data_matrix_JM(struct data_array *,struct data_array *, int*, float*);
void size_mask_JM(struct data_array *, struct data_array *, int*);
void mask_mask_JM(struct data_array *, struct data_array *, int*, int*);
void create_mask_JM(struct data_array *, struct data_array *, int*);
void max_vec_JM(float*, int, float*);
  
int min_JM (int *, int *);
int max_JM (int *, int *);

void rowcentre_JM (float *, int, int);
void colstandard_JM (float *, int, int);
void rowstd_JM (float *, int, int, int);
void transpose_mat_JM (float *, int *, int *, float *);
void mmult_JM (float *, int, int, float *, int, int, float *);
void orthog_mat_JM (float *, int, float *);
void gramsch_JM (float *, int, int, int);
void svd_JM (float *, int *, int *, float *, float *, float *);

void Symm_logcosh_JM (float *, int, float *, int, int, float, float *, float *);
void Symm_exp_JM (float *, int, float *, int, int, float, float *, float *);
void Def_logcosh_JM (float *, int, float *, int, int, float, float *);
void Def_exp_JM (float *, int, float *, int, int, float, float *);
void calc_A_JM(float*, float*, float*, int*, int*, int*, float*, float*);      
void calc_K_JM(float*, int*, int*, float*);

void icainc_JM(float* ,float* ,int* ,int* ,int* ,float* ,int* ,int* ,int* ,int* ,float* ,int* ,float* ,float* ,float* ,float* ,float*);


void F77_NAME (sgesdd) (char *, int *, int *, float *, int *, float *,
			float *, int *, float *, int *, float *, int *,
			int *, int *);
void F77_NAME (sgemm) (char *, char *, int *, int *, int *, float *, float *,
		       int *, float *, int *, float *, float *, int *);




/*  I/O functions */

void swap_JM(void *result, int size)
{
  /* Swaps the bytes when required */
  int i;
    char *p = result, tmp;

    if (size == 1) return;
    for (i = 0; i < size / 2; i++) {
	tmp = p[i];
	p[i] = p[size - i - 1];
	p[size - i - 1] = tmp;
    }}

void swaptest_wrap_JM(int *ans, char **name)
{
  swaptest_JM(ans, name[0]);
}

void swaptest_JM(int *ans, char *name)
{
  /*This function tests for the endian-ness of the files by checking the first field of the .hdr file which is always 348 (i.e. the header file length in bytes)*/ 
  FILE *fd;

  if((fd = fopen(name, "rb")) == NULL) error("Cannot open file");
  
  fread(ans, 4, 1, fd);
  fclose(fd);
}

void readchar_JM(char *ans, char *name, int *swapbytes, int n, long offset, int whence)
{
  /* Reads in a sequence of 1 byte characters */
  FILE *fd;
  
  if((fd = fopen(name, "rb")) == NULL) error("Cannot open file \n");

  fseek(fd, offset, whence);

  fread(ans, 1, n, fd);

  fclose(fd);
}

void read2byte_JM(short *ans, char *name, int *swapbytes, int n, long offset, int whence)
{
  /* Reads in a sequence of 2 byte short integers */
  FILE *fd;
  int i;
  short buf;
  

  if((fd = fopen(name, "rb")) == NULL) error("Cannot open file");

  fseek(fd, offset, whence);
  
  for(i = 0; i < n; i++){
	  fread(&buf, 2, 1, fd);
	  if (*swapbytes == 1) swap_JM(&buf, 2);
	  *(ans + i) = buf;
  }

  fclose(fd);
}


void read4byte_JM(int *ans, char *name, int *swapbytes, int n, long offset, int whence)
{
  /* Reads in a sequence of 4 byte short integers */
  FILE *fd;
  int i;
  int buf;
  

  if((fd = fopen(name, "rb")) == NULL) error("Cannot open file");

  fseek(fd, offset, whence);
  
  for(i = 0; i < n; i++){
	  fread(&buf, 4, 1, fd);
	  if (*swapbytes == 1) swap_JM(&buf, 4);
	  *(ans + i) = buf;
  }

  fclose(fd);
}
void readfloat_JM(float *ans, char *name, int *swapbytes, int n, long offset, int whence)
{
  /* Reads in a sequence of 4 byte floats */
  FILE *fd;
  int i;
  float buf;
  

  if((fd = fopen(name, "rb")) == NULL) error("Cannot open file");

  fseek(fd, offset, whence);
  
  for(i = 0; i < n; i++){
	  fread(&buf, 4, 1, fd);
	  if (*swapbytes == 1) swap_JM(&buf, 4);
	  *(ans + i) = buf;
  }

  fclose(fd);
}

void readdouble_JM(double *ans, char *name, int *swapbytes, int n, long offset, int whence)
{
  /* Reads in a sequence of 8 byte doubles */
  FILE *fd;
  int i;
  double buf;
  
  if((fd = fopen(name, "rb")) == NULL) error("Cannot open file \n");

  fseek(fd, offset, whence);
  
 
  for(i = 0; i < n; i++){
	  fread(&buf, 8, 1, fd);
	  if (*swapbytes == 1) swap_JM(&buf, 8);
	  *(ans + i) = buf;
  }

  fclose(fd);
}


void readchar_v1_JM(int *ans, char **name, int *swapbytes, int *n, int *offset, int *whence)
{
  /* Reads in a sequence of 1 byte characters */
  FILE *fd;
  unsigned char *tmp;
  int i;

  if((fd = fopen(name[0], "rb")) == NULL) error("Cannot open file \n");

  tmp = Calloc(*n, unsigned char);

  fseek(fd, (long) *offset, *whence);

  fread(tmp, 1, *n, fd);
  for(i = 0; i < *n; i++){
	  *(ans + i) = (int) *(tmp + i);}
  
  Free(tmp);
  fclose(fd);
}

void read2byte_v1_JM(int *ans, char **name, int *swapbytes, int *n, int *offset, int *whence)
{
  /* Reads in a sequence of 2 byte short integers, converts them to int  */
  
  FILE *fd;
  int i;
  short buf;
  

  if((fd = fopen(name[0], "rb")) == NULL) error("Cannot open file");

  fseek(fd, (long) *offset, *whence);
  
  for(i = 0; i < *n; i++){
	  fread(&buf, 2, 1, fd);
	  if (*swapbytes == 1) swap_JM(&buf, 2);
	  *(ans + i)= (int) buf;
  }

  fclose(fd);
}
void read4byte_v1_JM(int *ans, char **name, int *swapbytes, int *n, int *offset, int *whence)
{
  /* Reads in a sequence of 4 byte short integers and converts them to int */
  FILE *fd;
  int i;
  int buf;
  

  if((fd = fopen(name[0], "rb")) == NULL) error("Cannot open file");

  fseek(fd, (long) *offset, *whence);
  
  for(i = 0; i < *n; i++){
	  fread(&buf, 4, 1, fd);
	  if (*swapbytes == 1) swap_JM(&buf, 4);
	  *(ans + i) = buf;
  }

  fclose(fd);
}


void readfloat_v1_JM(float *ans, char **name, int *swapbytes, int *n, int *offset, int *whence)
{
  /* Reads in a sequence of 4 byte floats */
  FILE *fd;
  int i;
  float buf;
  

  if((fd = fopen(name[0], "rb")) == NULL) error("Cannot open file");

  fseek(fd, (long) *offset, *whence);
  
  for(i = 0; i < *n; i++){
	  fread(&buf, 4, 1, fd);
	  if (*swapbytes == 1) swap_JM(&buf, 4);
	  *(ans + i)= buf;
  }

  fclose(fd);
}

void readdouble_v1_JM(double *ans, char **name, int *swapbytes, int *n, int *offset, int *whence)
{
  /* Reads in a sequence of 8 byte doubles */
  FILE *fd;
  int i;
  double buf;
  
  if((fd = fopen(name[0], "rb")) == NULL) error("Cannot open file");

  fseek(fd, (long) *offset, *whence);
  
 
  for(i = 0; i < *n; i++){
	  fread(&buf, 8, 1, fd);
	  if (*swapbytes == 1) swap_JM(&buf, 8);
	  *(ans + i)= buf;
  }
  
  fclose(fd);
}

void read2byte_F_JM(float *ans, char *name, int *swapbytes, int n, long offset, int whence)
{
  /* Reads in a sequence of 2 byte short integers */
  FILE *fd;
  int i;
  short buf;
  

  if((fd = fopen(name, "rb")) == NULL) error("Cannot open file");

  fseek(fd, offset, whence);
  
  for(i = 0; i < n; i++){
	  fread(&buf, 2, 1, fd);
	  if (*swapbytes == 1) swap_JM(&buf, 2);
	  *(ans + i)= (float) buf;
  }
  
  fclose(fd);
}

void read4byte_F_JM(float *ans, char *name, int *swapbytes, int n, long offset, int whence)
{
  /* Reads in a sequence of 2 byte short integers */
  FILE *fd;
  int i;
  long buf;
  

  if((fd = fopen(name, "rb")) == NULL) error("Cannot open file");
 

  fseek(fd, offset, whence);
  
  for(i = 0; i < n; i++){
	  fread(&buf, 4, 1, fd);
	  if (*swapbytes == 1) swap_JM(&buf, 4);
	  *(ans + i)= (float) buf;
  }
  
  fclose(fd);
}

void readfloat_F_JM(float *ans, char *name, int *swapbytes, int n, long offset, int whence)
{
  /* Reads in a sequence of 4 byte floats */
  FILE *fd;
  int i;
  float buf;
  

  if((fd = fopen(name, "rb")) == NULL) error("Cannot open file");

  fseek(fd, offset, whence);
  
  for(i = 0; i < n; i++){
	  fread(&buf, 4, 1, fd);
	  if (*swapbytes == 1) swap_JM(&buf, 4);
	  *(ans + i)= buf;
  }

  fclose(fd);
}

void readdouble_F_JM(float *ans, char *name, int *swapbytes, int n, long offset, int whence)
{
  /* Reads in a sequence of 8 byte doubles */
  FILE *fd;
  int i;
  double buf;
  
  if((fd = fopen(name, "rb")) == NULL) error("Cannot open file");

  fseek(fd, offset, whence);
  
 
  for(i = 0; i < n; i++){
	  fread(&buf, 8, 1, fd);
	  if (*swapbytes == 1) swap_JM(&buf, 8);
	  *(ans + i) =  (float) buf;
  }

  fclose(fd);
}

void write8bit_JM(int *imp, char **name, int *n)
{
  /* Writes in a sequence of 8 bit unsigned char integers */
  FILE *fp;
  unsigned char *temp;
  int i;

  temp = Calloc(*n, unsigned char);

  for(i = 0; i < *n; i++) {
/*      if((*(imp+i))>255) *(imp+1)=255; */
/*      if((*(impp+i))<0) *(imp+1)=0; */
    *(temp + i) = (unsigned char) *(imp + i);
  }

  fp = fopen(name[0], "wb");
  
  fwrite(temp, 1, *n, fp);

  Free(temp);
  fclose(fp);
}

void write2byte_JM(int *imp, char **name, int *n)
{
  /* Writes in a sequence of 2 byte short integers */
  FILE *fp;
  short *temp;
  int i;

  temp = Calloc(*n, short);

  for(i = 0; i < *n; i++) {
  *(temp + i) = (short) *(imp + i);
  }

  fp = fopen(name[0], "wb");
  
  fwrite(temp, 2, *n, fp);

  Free(temp);
  fclose(fp);
}

void writefloat_JM(float *imp, char **name, int *n)
{
  /* Writes a sequence of 4 byte floats  */
  FILE *fp;

  fp = fopen(name[0], "wb");
  
  fwrite(imp, 4, *n, fp);

  fclose(fp);
}


void read_data_as_float_JM(struct data_array *array, struct header *head, char *img_file, int *swapbytes){

	
  int n;
  n = (*array).n;
  (*array).x = (*head).dim[1];  
  (*array).y = (*head).dim[2];
  (*array).z = (*head).dim[3];
  (*array).t = (*head).dim[4];

  if((*head).datatype == 4){read2byte_F_JM((*array).data, img_file, swapbytes, n, 0L, 0);}
  if((*head).datatype == 8){read4byte_F_JM((*array).data, img_file, swapbytes, n, 0L, 0);}
  if((*head).datatype == 16){readfloat_F_JM((*array).data, img_file, swapbytes, n, 0L, 0);}
  if((*head).datatype == 64){readdouble_F_JM((*array).data, img_file, swapbytes, n, 0L, 0);}
  
  
  return;
}
void read_analyze_header_wrap_JM(char **name, 
			      int *swapbytes, 
			      int *sizeof_hdr,
			      char **data_type,
			      char **db_name,
			      int *extents,
			      int *session_error,
			      char **regular,
			      char **hkey_un0,
			      int *dim,
			      char **vox_units,
			      char **cal_units,
			      int *unused1,
			      int *datatype,
			      int *bitpix,
			      int *dim_un0,
			      float *pixdim,
			      float *vox_offset,
			      float *funused1,
			      float *funused2,
			      float *funused3,
			      float *cal_max,
			      float *cal_min,
			      float *compressed,
			      float *verified,
			      int *glmax,
			      int *glmin,
			      char **descrip,
			      char **aux_file,
			      char **orient,
				 //char **originator,
			      int *originator,
			      char **generated,
			      char **scannum,
			      char **patient_id,
			      char **exp_date,
			      char **exp_time,
			      char **hist_un0,
			      int *views, 
			      int *vols_added, 
			      int *start_field, 
			      int *field_skip, 
			      int *omax, 
			      int *omin,
			      int *smax,
			      int *smin
			      ) 
     
{
  /*Reads in all the fields of a .hdr header file*/
  
  int i;
  short tmp,tmp1[8], tmp2[5];
  
  read4byte_JM(sizeof_hdr, name[0], swapbytes, 1, 0L, 0);
  
  readchar_JM(data_type[0], name[0], swapbytes, 10, 4L, 0);

  readchar_JM(db_name[0], name[0], swapbytes, 18, 14L, 0);
  
  read4byte_JM(extents, name[0], swapbytes, 1, 32L, 1);
  
  read2byte_JM(&tmp, name[0], swapbytes, 1, 36L, 1);
  *session_error = (int) tmp;

  readchar_JM(regular[0], name[0], swapbytes, 1, 38L, 0);
  readchar_JM(hkey_un0[0], name[0], swapbytes, 1, 39L, 0);
  read2byte_JM(tmp1, name[0], swapbytes, 8, 40L, 1);
  for(i = 0; i < 8; i++){dim[i] = (int) tmp1[i];}

  readchar_JM(vox_units[0], name[0], swapbytes, 4, 56L, 0);
  readchar_JM(cal_units[0], name[0], swapbytes, 8, 60L, 0);
 
  read2byte_JM(&tmp, name[0], swapbytes, 1, 68L, 1);
  *unused1 = (int) tmp;

  read2byte_JM(&tmp, name[0], swapbytes, 1, 70L, 1);
  *datatype = (int) tmp;

  read2byte_JM(&tmp, name[0], swapbytes, 1, 72L, 1);
  *bitpix = (int) tmp;

  read2byte_JM(&tmp, name[0], swapbytes, 1, 74L, 1);
  *dim_un0 = (int) tmp;

  readfloat_JM(pixdim, name[0], swapbytes, 8, 76L, 1);
  readfloat_JM(vox_offset, name[0], swapbytes, 1, 108L, 1);
  readfloat_JM(funused1, name[0], swapbytes, 1, 112L, 1);
  readfloat_JM(funused2, name[0], swapbytes, 1, 116L, 1);
  readfloat_JM(funused3, name[0], swapbytes, 1, 120L, 1);
  readfloat_JM(cal_max, name[0], swapbytes, 1, 124L, 1);
  readfloat_JM(cal_min, name[0], swapbytes, 1, 128L, 1);
  readfloat_JM(compressed, name[0], swapbytes, 1, 132L, 1);
  readfloat_JM(verified, name[0], swapbytes, 1, 136L, 1);
  read4byte_JM(glmax, name[0], swapbytes, 1, 140L ,1);
  read4byte_JM(glmin, name[0], swapbytes, 1, 144L, 1);
  readchar_JM(descrip[0], name[0], swapbytes, 80, 148L, 0);
  readchar_JM(aux_file[0], name[0], swapbytes, 24, 228L, 0);
  readchar_JM(orient[0], name[0], swapbytes, 1, 252L, 0);


  read2byte_JM(tmp2, name[0], swapbytes, 5, 253L, 1);
  for(i = 0; i < 5; i++){originator[i] = (int) tmp2[i];}

  //  readchar_JM(originator[0], name[0], swapbytes, 10, 253L, 0);
  readchar_JM(generated[0], name[0], swapbytes, 10, 263L, 0);
  readchar_JM(scannum[0], name[0], swapbytes, 10, 273L, 0);
  readchar_JM(patient_id[0], name[0], swapbytes, 10, 283L, 0);
  readchar_JM(exp_date[0], name[0], swapbytes, 10, 293L, 0);
  readchar_JM(exp_time[0], name[0], swapbytes, 10, 303L, 0);
  readchar_JM(hist_un0[0], name[0], swapbytes, 4, 313L, 0);
  read4byte_JM(views, name[0], swapbytes, 1, 316L, 1);
  read4byte_JM(vols_added, name[0], swapbytes, 1, 320L, 1);
  read4byte_JM(start_field, name[0], swapbytes, 1, 324L, 1);
  read4byte_JM(field_skip, name[0], swapbytes, 1, 328L, 1);
  read4byte_JM(omax, name[0], swapbytes, 1, 332L, 1);
  read4byte_JM(omin, name[0], swapbytes, 1, 336L, 1);
  read4byte_JM(smax, name[0], swapbytes, 1, 340L, 1);
  read4byte_JM(smin, name[0], swapbytes, 1, 344L, 1);

}

void read_analyze_header_JM(struct header *head, char *name, int *swapbytes)

{
  /*Reads in all the fields of a .hdr header file*/
  

  read4byte_JM(&head->sizeof_hdr, name, swapbytes, 1, 0L, 0);
  
  readchar_JM(head->data_type,name, swapbytes, 10, 4L, 0);

  readchar_JM(head->db_name,name, swapbytes, 18, 14L, 0);
  
  read4byte_JM(&head->extents, name, swapbytes, 1, 32L, 1);
  
  read2byte_JM(&head->session_error, name, swapbytes, 1, 36L, 1);
  
  readchar_JM(&head->regular, name, swapbytes, 1, 38L, 0);
  readchar_JM(&head->hkey_un0, name, swapbytes, 1, 39L, 0);
  read2byte_JM(&head->dim[0], name, swapbytes, 8, 40L, 1);

  readchar_JM(head->vox_units, name, swapbytes, 4, 56L, 0);
  readchar_JM(head->cal_units, name, swapbytes, 8, 60L, 0);
  read2byte_JM(&head->unused1, name, swapbytes, 1, 68L, 1);
  read2byte_JM(&head->datatype, name, swapbytes, 1, 70L, 1);
  read2byte_JM(&head->bitpix, name, swapbytes, 1, 72L, 1);
  read2byte_JM(&head->dim_un0, name, swapbytes, 1, 74L, 1);
  readfloat_JM(&head->pixdim[0], name, swapbytes, 8, 76L, 1);
  readfloat_JM(&head->vox_offset, name, swapbytes, 1, 108L, 1);
  readfloat_JM(&head->funused1, name, swapbytes, 1, 112L, 1);
  readfloat_JM(&head->funused2, name, swapbytes, 1, 116L, 1);
  readfloat_JM(&head->funused3, name, swapbytes, 1, 120L, 1);
  readfloat_JM(&head->cal_max, name, swapbytes, 1, 124L, 1);
  readfloat_JM(&head->cal_min, name, swapbytes, 1, 128L, 1);
  readfloat_JM(&head->compressed, name, swapbytes, 1, 132L, 1);
  readfloat_JM(&head->verified, name, swapbytes, 1, 136L, 1);
  read4byte_JM(&head->glmax, name, swapbytes, 1, 140L, 1);
  read4byte_JM(&head->glmin, name, swapbytes, 1, 144L, 1);
  readchar_JM(head->descrip, name, swapbytes, 80, 148L, 0);
  readchar_JM(head->aux_file, name, swapbytes, 24, 228L, 0);
  readchar_JM(&head->orient, name, swapbytes, 1, 252L, 0);

  read2byte_JM(&head->originator[0], name, swapbytes, 5, 253L, 1);
  //  readchar_JM(head->originator, name, swapbytes, 10, 253L, 0);
  readchar_JM(head->generated, name, swapbytes, 10, 263L, 0);
  readchar_JM(head->scannum, name, swapbytes, 10, 273L, 0);
  readchar_JM(head->patient_id, name, swapbytes, 10, 283L, 0);
  readchar_JM(head->exp_date, name, swapbytes, 10, 293L, 0);
  readchar_JM(head->exp_time, name, swapbytes, 10, 303L, 0);
  readchar_JM(head->hist_un0, name, swapbytes, 4, 313L, 0);
  read4byte_JM(&head->views, name, swapbytes, 1, 316L, 1);
  read4byte_JM(&head->vols_added, name, swapbytes, 1, 320L, 1);
  read4byte_JM(&head->start_field, name, swapbytes, 1, 324L, 1);
  read4byte_JM(&head->field_skip, name, swapbytes, 1, 328L, 1);
  read4byte_JM(&head->omax, name, swapbytes, 1, 332L, 1);
  read4byte_JM(&head->omin, name, swapbytes, 1, 336L, 1);
  read4byte_JM(&head->smax, name, swapbytes, 1, 340L, 1);
  read4byte_JM(&head->smin, name, swapbytes, 1, 344L, 1);
}


void write_analyze_header_wrap_JM(char **name, 
			       int *sizeof_hdr,
			       char **data_type,
			       char **db_name,
			       int *extents,
			       int *session_error,
			       char **regular,
			       char **hkey_un0,
			       int *dim,
			       char **vox_units,
			       char **cal_units,
			       int *unused1,
			       int *datatype,
			       int *bitpix,
			       int *dim_un0,
			       float *pixdim,
			       float *vox_offset,
			       float *funused1,
			       float *funused2,
			       float *funused3,
			       float *cal_max,
			       float *cal_min,
			       float *compressed,
			       float *verified,
			       int *glmax,
			       int *glmin,
			       char **descrip,
			       char **aux_file,
			       char **orient,
				 //char **originator,
			      int *originator,
			       char **generated,
			       char **scannum,
			       char **patient_id,
			       char **exp_date,
			       char **exp_time,
			       char **hist_un0,
			       int *views, 
			       int *vols_added, 
			       int *start_field, 
			       int *field_skip, 
			       int *omax, 
			       int *omin,
			       int *smax,
			       int *smin)
{

  /*Writes all the fields of a .hdr header file*/ 
  FILE *fp;
  int i;
  short dim1[8],tmp, tmp2[5];

  fp = fopen(name[0],"wb");
  if (fp == NULL) error("file writing error");
  
  fwrite(sizeof_hdr, 4, 1, fp);
  for(i = 0; i < 10; i++) fwrite(*(data_type) + i, 1, 1, fp);
  for(i = 0; i < 18; i++) fwrite(*(db_name) + i, 1, 1, fp);
  fwrite(extents, 4, 1, fp);
  fwrite(session_error, 2, 1, fp);
  fwrite(*(regular), 1, 1, fp);
  fwrite(*(hkey_un0), 1, 1, fp);
  for(i = 0; i < 8; i++) dim1[i] = (short) *(dim + i);
  fwrite(&dim1, 2, 8, fp);
  for(i = 0; i < 4; i++) fwrite(*(vox_units) + i, 1, 1, fp);
  for(i = 0; i < 8; i++) fwrite(*(cal_units) + i, 1, 1, fp);
  tmp = (short) *unused1; fwrite(&tmp, 2, 1, fp);
  tmp = (short) *datatype; fwrite(&tmp, 2, 1, fp);
  tmp = (short) *bitpix; fwrite(&tmp, 2, 1, fp);
  tmp = (short) *dim_un0; fwrite(&tmp, 2, 1, fp);
  fwrite(pixdim, 4, 8, fp);
  fwrite(vox_offset, 4, 1, fp);
  fwrite(funused1, 4, 1, fp);
  fwrite(funused2, 4, 1, fp);
  fwrite(funused3, 4, 1, fp);
  fwrite(cal_max, 4, 1, fp);
  fwrite(cal_min, 4, 1, fp);
  fwrite(compressed, 4, 1, fp);
  fwrite(verified, 4, 1, fp);
  fwrite(glmax, 4, 1, fp);
  fwrite(glmin, 4, 1, fp);
  for(i = 0; i < 80; i++) fwrite(*(descrip) + i, 1, 1, fp);
  for(i = 0; i < 24; i++) fwrite(*(aux_file) + i, 1, 1, fp);
  fwrite(*orient, 1, 1, fp);
  //  for(i = 0; i < 10; i++) fwrite(*(originator) + i, 1, 1, fp);
  for(i = 0; i < 5; i++) tmp2[i] = (short) *(originator + i);
  fwrite(&tmp2, 2, 5, fp);

  for(i = 0; i < 10; i++) fwrite(*(generated) + i, 1, 1, fp);
  for(i = 0; i < 10; i++) fwrite(*(scannum) + i, 1, 1, fp);
  for(i = 0; i < 10; i++) fwrite(*(patient_id) + i, 1, 1, fp); 
  for(i = 0; i < 10; i++) fwrite(*(exp_date) + i, 1, 1, fp);
  for(i = 0; i < 10; i++) fwrite(*(exp_time) + i, 1, 1, fp);
  for(i = 0; i < 3; i++) fwrite(*(hist_un0) + i, 1, 1, fp);
  fwrite(views, 4, 1, fp);
  fwrite(vols_added, 4, 1, fp);
  fwrite(start_field, 4, 1, fp);
  fwrite(field_skip, 4, 1, fp);
  fwrite(omax, 4, 1, fp);
  fwrite(omin, 4, 1, fp);
  fwrite(smax, 4, 1, fp);
  fwrite(smin, 4, 1, fp);

  fclose(fp);
}


void print_analyze_header_JM(struct header *head)
{
  

  Rprintf("size = %d\n", (*head).sizeof_hdr);
  Rprintf("data_type = %s\n", (*head).data_type);
  Rprintf("db_name = %s\n", (*head).db_name);
  Rprintf("extents = %d\n", (*head).extents);
  Rprintf("session_error = %d\n", (*head).session_error);
  Rprintf("regular = %c\n", (*head).regular);
  Rprintf("hkey_un0 = %c\n", (*head).hkey_un0);
  Rprintf("dim = %d %d %d %d %d %d %d %d\n",
	 (*head).dim[0],
	 (*head).dim[1],
	 (*head).dim[2],
	 (*head).dim[3],
	 (*head).dim[4],
	 (*head).dim[5],
	 (*head).dim[6],
	 (*head).dim[7]);
  Rprintf("vox_units = %s\n", (*head).vox_units);
  Rprintf("cal_units = %s\n", (*head).cal_units);
  Rprintf("unused1 = %d\n", (*head).unused1);
  Rprintf("datatype = %d\n", (*head).datatype);
  Rprintf("bitpix = %d\n", (*head).bitpix);
  Rprintf("dim_un0 = %d\n", (*head).dim_un0); 
  Rprintf("dim = %01.0f %01.0f %01.0f %01.0f %01.0f %01.0f %01.0f %01.0f \n",
	 (*head).pixdim[0],
	 (*head).pixdim[1],
	 (*head).pixdim[2],
	 (*head).pixdim[3],
	 (*head).pixdim[4],
	 (*head).pixdim[5],
	 (*head).pixdim[6],
	 (*head).pixdim[7]);
  Rprintf("vox_offset = %f\n", (*head).vox_offset);
  Rprintf("funused1 = %f\n", (*head).funused1);
  Rprintf("funused2 = %f\n", (*head).funused2);
  Rprintf("funused3 = %f\n", (*head).funused3);
  Rprintf("cal_max = %f\n", (*head).cal_max);
  Rprintf("cal_min = %f\n", (*head).cal_min);
  Rprintf("compressed = %f\n", (*head).compressed);
  Rprintf("verified = %f\n", (*head).verified);
  Rprintf("glmax = %d\n", (*head).glmax);
  Rprintf("glmin = %d\n", (*head).glmin);
  Rprintf("descrip = %s\n", (*head).descrip);
  Rprintf("aux_file = %s\n", (*head).aux_file);
  Rprintf("orient = %c\n", (*head).orient);
  Rprintf("originator = %s\n", (*head).originator);
  Rprintf("generated = %s\n", (*head).generated);
  Rprintf("scannum = %s\n", (*head).scannum);
  Rprintf("patient_id = %s\n", (*head).patient_id);
  Rprintf("exp_date = %s\n", (*head).exp_date);
  Rprintf("exp_time = %s\n", (*head).exp_time);
  Rprintf("hist_un0 = %s\n", (*head).hist_un0);
  Rprintf("views = %d\n", (*head).views);
  Rprintf("vols_added = %d\n", (*head).vols_added);
  Rprintf("start_field = %d\n", (*head).start_field);
  Rprintf("field_skip = %d\n", (*head).field_skip);
  Rprintf("omax = %d\n", (*head).omax);
  Rprintf("omin = %d\n", (*head).omin);
  Rprintf("smax = %d\n", (*head).smax);
  Rprintf("smin = %d\n", (*head).smin);
}



/*  ICA for fMRI functions */

void ica_fmri_JM(char **file, float *w_matrix, int *n_comp, int *rowflag1, int *colflag1, int *funflag1, int *maxit1, int *defflag1, float *alpha1, float *lim1, int *maskflag, char **msk_file, int *slices, int *nsl, float *ans_sources, float *ans_tc){


  int mask_size = 0,count;
  int i,j,k,l,x,y,z,n,nm,t,nc,ans,file_name_length,rowflag,colflag,funflag,maxit,defflag;
  float alpha,lim,*data_matrix; 
  float *pre_processed_data_matrix, *k_matrix, *w_calc_matrix, *a_matrix, *s_matrix;
  int swapbytes = 1,mask_swapbytes = 1;
  struct header *head,*mask_head;
  struct data_array *array,*mask;
  char *in_file,*mask_file,mask_flag = 'F';
  
  rowflag = *rowflag1;
  colflag = *colflag1;
  funflag = *funflag1;
  maxit = *maxit1;
  defflag = *defflag1;
  alpha = *alpha1;
  lim = *lim1;
  in_file = file[0];
  mask_file = msk_file[0];

  char *img_file = NULL, *hdr_file = NULL, *mask_img = NULL, *mask_hdr = NULL;
img_file = (char*)calloc(strlen(in_file) + 1, sizeof(char));
hdr_file = (char*)calloc(strlen(in_file) + 1, sizeof(char));
mask_img = (char*)calloc(strlen(mask_file) + 1, sizeof(char));
mask_hdr = (char*)calloc(strlen(mask_file) + 1, sizeof(char));
  
  if(*maskflag == 1) mask_flag = 'T';
  nc = *n_comp;


  head = Calloc(1,struct header);
  mask_head = Calloc(1,struct header);
  array = Calloc(1,struct data_array);
  mask = Calloc(1,struct data_array);
    
  
  /*  Read in dataset */
  Rprintf("Reading in dataset\n");
  file_name_length = strlen(in_file);
  strcpy(img_file, in_file);
  img_file[file_name_length - 3] = '.';
  img_file[file_name_length - 2] = 'i';
  img_file[file_name_length - 1] = 'm';
  img_file[file_name_length - 0] = 'g';
  strcpy(hdr_file, in_file);
  hdr_file[file_name_length - 3] = '.';
  hdr_file[file_name_length - 2] = 'h';
  hdr_file[file_name_length - 1] = 'd';
  hdr_file[file_name_length - 0] = 'r';

  
  /*  strncat(img_file, in_file, file_name_length - 4);
  strcat(img_file, ".img");
  strncat(hdr_file, in_file, file_name_length - 4);
  strcat(hdr_file, ".hdr"); */
  

  swaptest_JM(&ans, hdr_file);
  if(ans == 348) swapbytes = 0;
  
  read_analyze_header_JM(head, hdr_file, &swapbytes);
/*    print_analyze_header_JM(head); */
  
  n = ((*head).dim[1]*(*head).dim[2]*(*head).dim[3]*(*head).dim[4]); 
  (*array).n = n;
  (*array).data = Calloc((*array).n,float);
  
  read_data_as_float_JM(array, head, img_file, &swapbytes);
  
  /*  Read in/create mask */
  nm = ((*array).x*(*array).y*(*array).z);
  (*mask).n = nm;
  (*mask).data = Calloc(nm,float);


  if(mask_flag == 'T'){
    Rprintf("Reading in mask\n");
    file_name_length = strlen(mask_file);
    strcpy(mask_img, mask_file);
    mask_img[file_name_length - 3] = '.';
    mask_img[file_name_length - 2] = 'i';
    mask_img[file_name_length - 1] = 'm';
    mask_img[file_name_length - 0] = 'g';
    strcpy(mask_hdr, mask_file);
    mask_hdr[file_name_length - 3] = '.';
    mask_hdr[file_name_length - 2] = 'h';
    mask_hdr[file_name_length - 1] = 'd';
    mask_hdr[file_name_length - 0] = 'r';
  
    /*    
    file_name_length = strlen(mask_file);
    strncat(mask_img,mask_file,file_name_length - 4);
    strcat(mask_img,".img");
    strncat(mask_hdr,mask_file,file_name_length - 4);
    strcat(mask_hdr,".hdr");
    */
    
    swaptest_JM(&ans, mask_hdr);
    if(ans == 348) mask_swapbytes = 0;
    
    
    read_analyze_header_JM(mask_head, mask_hdr, &mask_swapbytes);
    read_data_as_float_JM(mask, mask_head, mask_img, &mask_swapbytes);
    mask_mask_JM(array, mask, slices, nsl);
    size_mask_JM(array, mask, &mask_size);
    Rprintf("Mask size = %d\n",mask_size);
  }
  else
    {
      Rprintf("Making mask\n");
      create_mask_JM(array, mask, &mask_size); 
      mask_mask_JM(array, mask, slices, nsl);
      size_mask_JM(array, mask, &mask_size);
      Rprintf("Mask size = %d\n",mask_size);
    }
  
  /*  Create 2D data matrix (columns are voxel time series) */
  Rprintf("Creating data matrix\n");
  data_matrix = Calloc(mask_size*(*array).t,float);
  create_data_matrix_JM(array, mask, &mask_size, data_matrix);
  Free((*array).data);
  
  
  
  t = (*array).t;
  
 
  pre_processed_data_matrix = Calloc(mask_size*t,float);
  k_matrix = Calloc(t*t,float);
  w_calc_matrix = Calloc(nc*nc,float);
  a_matrix = Calloc(t*nc,float);
  s_matrix = Calloc(nc*mask_size,float);


  Rprintf("Running ICA\n");
  icainc_JM(data_matrix, w_matrix, &t, &mask_size, &nc, &alpha, &rowflag, &colflag, &funflag, &maxit, &lim, &defflag, pre_processed_data_matrix, k_matrix, w_calc_matrix, a_matrix, s_matrix); 


     /*  Free memory */
  if(mask_flag == 'T'){
    Free(mask_head);}
  Free(head);
  Free(data_matrix); 
/*    Free(data_matrix1);  */
  Free(pre_processed_data_matrix);
  Free(k_matrix); 
  Free(w_calc_matrix); 


  count = 0;
  x = (*array).x;
  y = (*array).y;
  z = (*array).z;
  for(i = 0;i<x;i++){
    for(j = 0;j<y;j++){
      for(k = 0;k<z;k++){
	if(*((*mask).data + k*x*y + j*x + i) == 1){
	  for(l = 0;l<nc;l++){
	    *(ans_sources + l*x*y*z + k*x*y + j*x + i)=*(s_matrix + l*mask_size + count);
	  }
	  count+=1;
	}
      }
    }
  }

  for(i = 0;i<t*nc;i++)*(ans_tc + i)=*(a_matrix + i);

  
  Free(array);
  Free((*mask).data);
  Free(a_matrix); 
  Free(s_matrix);
  Free(mask);
  
  
}


void size_mask_JM(struct data_array *array, struct data_array *mask, int *mask_size){
  
  int i,x,y,z;
  float tmp;
  x = (*array).x;
  y = (*array).y;
  z = (*array).z;
  
  tmp = 0;
  for(i = 0;i<(x*y*z);i++){
    tmp += *((*mask).data + i);}

  *mask_size = (int) tmp;
  return;  
}

void mask_mask_JM(struct data_array *array, struct data_array *mask, int *slices, int *nsl){
  
  int i,j,k,x,y,z;
  float tmp;
  x = (*array).x;
  y = (*array).y;
  z = (*array).z;
  
  for(k = 0;k<z;k++){
      tmp = 0;
      for(j = 0;j<*nsl;j++){
	  if((k + 1) == slices[j]) tmp = 1;
      }
      if(tmp == 0){
	  for(i = 0;i<x*y;i++){
	      *((*mask).data + k*x*y + i) = 0;
	  }
      }
  }

  return;  
}



void create_mask_JM(struct data_array *array, struct data_array *mask, int *mask_size){
  
  int i,j,k,l,x,y,z,t;
  float max;
  x = (*array).x;
  y = (*array).y;
  z = (*array).z;
  t = (*array).t;
  //  n = (*array).n;
  
  
  for(i = 0;i<x;i++){
    for(j = 0;j<y;j++){
      for(k = 0;k<z;k++){
	for(l = 0;l<t;l++){
	  *((*mask).data + k*x*y + j*x + i)+=*((*array).data + l*x*y*z + k*x*y + j*x + i);}
	*((*mask).data + k*x*y + j*x + i)/=t;
      }}}

  
  max_vec_JM((*mask).data,(x*y*z),&max);
  max/=10;
  
  *mask_size = 0;
  for(i = 0;i<(x*y*z);i++){
    if(*((*mask).data + i)<max)
      *((*mask).data + i) = 0;
    else {
      *((*mask).data + i) = 1;
      *mask_size+=1;
    }
 }
      

  return;  
}
 
void max_vec_JM(float *vec, int n, float *ans){

  int i;
  *ans = *vec;
  for(i = 1;i<n;i++){
    if(*(vec + i)>*ans) *ans = *(vec + i);}

  return;
}

void create_data_matrix_JM(struct data_array *array, struct data_array *mask, int *mask_size, float *data_matrix){
  
  int i,j,k,l,x,y,z,t,count;
  x = (*array).x;
  y = (*array).y;
  z = (*array).z;
  t = (*array).t;
  //  n = (*array).n;
  count = 0;
  

  for(i = 0;i<x;i++){
    for(j = 0;j<y;j++){
      for(k = 0;k<z;k++){
	if(*((*mask).data + k*x*y + j*x + i) == 1.0){	
	  for(l = 0;l<t;l++){*(data_matrix + l*(*mask_size) + count) = *((*array).data + l*(x*y*z) + k*x*y + j*x + i);}
	  count+=1;}
      }
    }
  }

  return;
}

/*  ICA computations functions */


void xerbla_ (char *msg, int *info)
{
	/* replacement for xerbla function in BLAS library to avoid Fortran I/O */
   char buf[7];

    strncpy(buf, msg, 6);
    buf[6] = '\0';
    PROBLEM "xerbla %s (%d)", buf, *info ERROR;
}

void
rowcentre_JM (float *ans, int n, int p)
{
/*  mean centres nxp matrix ans */
	double tmp;
	int i, j;
	for (i = 0; i < n; i++) {
		tmp = 0;
		for (j = 0; j < p; j++) {
			tmp = tmp + ((double) ans[p * i + j]) / p;
		}
		for (j = 0; j < p; j++) {
			ans[p * i + j] -= (float) tmp;
		}
	}
}

void
colstandard_JM (float *ans, int n, int p)
{
/*  transform columns of nxp matrix ans to have zero mean and unit variance */
	double tmp[2];
	double tmp1;
	int i, j;
	for (i = 0; i < p; i++) {
		tmp[0] = 0;
		tmp[1] = 0;
		
		for (j = 0; j < n; j++) {
			tmp[0] += (double) ans[p * j + i];
			tmp[1] += ((double) ans[p * j + i]) * ((double) ans[p * j + i]);
		}
		
		tmp[0] = tmp[0] / n;
		tmp1 = (tmp[1] - n * (tmp[0]) * (tmp[0])) / (n - 1);
		
		tmp[1] = sqrt (tmp1);
		for (j = 0; j < n; j++) {
			ans[p * j + i] =
				(float) ((((double) ans[p * j + i]) - tmp[0]) / tmp[1]);
		}
	}
}


void
svd_JM (float *mat, int *n, int *p, float *u, float *d, float *v)
{
	
	/*  calculates svd decomposition of nxp matrix mat */
	/*    mat is a pointer to an nxp array of floats */
	/*    n is a pointer to an integer specifying the no. of rows of mat */
	/*    p is a pointer to an integer specifying the no. of cols of mat */
	/*    u is a pointer to a float array of dimension (n,n) */
	/*    d is a pointer to a float array of dimension min(n,p) */
	/*    v is a pointer to a float array of dimension (p,p) */
	
	
	int info,iwork_size, *iwork, lwork, a, b;
	float *work, *mat1, *u1, *v1;
	char jobz = 'A';
	
	iwork_size = 8 * min_JM (n, p);
	
	a = max_JM(n,p);
	b = 4 * min_JM(n,p) * min_JM(n,p) + 4 * min_JM(n,p);
	lwork= 3 * min_JM(n,p) * min_JM(n,p) + max_JM(&a, &b);
	
	work = Calloc (lwork, float);
	iwork = Calloc (iwork_size, int);
	mat1 = Calloc ((*n) * (*p), float);
	u1 = Calloc ((*n) * (*n), float);
	v1 = Calloc ((*p) * (*p), float);
	
	transpose_mat_JM (mat, n, p, mat1);
	
	F77_CALL (sgesdd) (&jobz, n, p, mat1, n, d, u1, n, v1, p, work,
			   &lwork, iwork, &info);
		

	transpose_mat_JM (u1, n, n, u);
	
	transpose_mat_JM (v1, p, p, v);
	
	Free (mat1);
	Free (u1);
	Free (v1);
	Free (work);
	Free (iwork);
	
}

void
transpose_mat_JM (float *mat, int *n, int *p, float *ans)
{
/*    transpose nxp matrix mat */
	int i, j;
	
	for (i = 0; i < *n; i++) {
		for (j = 0; j < *p; j++) {
			*(ans + j * (*n) + i) = *(mat + i * (*p) + j);
		}
	}
}


int
min_JM (int *a, int *b)
{
/*  find minimum of a and b */
	int ans;
	
	ans = *b;
	if (*a < *b)
		ans = *a;
	
	return ans;
}

int
max_JM (int *a, int *b)
{
/*  find maximum of a and b */
	
	int ans;
	
	ans = *b;
	if (*a > *b)
		ans = *a;
	
	return ans;
}


void
mmult_JM (float *A, int n, int p, float *B, int q, int r, float *C)
{
/*    matrix multiplication using FORTRAN BLAS routine SGEMM */
/*    A is (n*p) and B is (q*r), A*B returned to C  */
	
	float alpha = 1.0, beta = 0.0;
	int M, K, N;
	char transA = 'N', transB = 'N';
	
	if (p != q) {
		error ("Error, matrices not suitable\nfor multiplication");
	}
	else {
		M = n;
		K = p;
		N = r;
		
		F77_CALL (sgemm) (&transA, &transB, &N, &M, &K, &alpha, B, &N,
				  A, &K, &beta, C, &N);
		
	}
}

void
orthog_mat_JM (float *mat, int e, float *orthog)
{
	/* take Wmat, (e*e), and return orthogonalized version to orthog_W */
	float *u, *v, *d, *temp;
	int i;
	
	
	u = Calloc (e * e, float);
	d = Calloc (e, float);
	v = Calloc (e * e, float);
	temp = Calloc (e * e, float);
	
	svd_JM (mat, &e, &e, u, d, v);
	for (i = 0; i < e; i++) {
		temp[i * e + i] = 1 / (d[i]);
	}
	
	mmult_JM (u, e, e, temp, e, e, v);
	transpose_mat_JM (u, &e, &e, temp);
	mmult_JM (v, e, e, temp, e, e, u);
	mmult_JM (u, e, e, mat, e, e, orthog);
	
	
	Free (u);
	Free (v);
	Free (d);
	Free (temp);
	
}

void
Symm_logcosh_JM (float *w_init, int e, float *data, int f, int p, float alpha, float *w_final, float *Tol)
{
	
	/* Function that carries out Symmetric ICA using a logcosh approximation to the neg. entropy function */
	
	float *mat1, *mat2, *mat3, *mat4, *mat5, *mat6;
	int i, j;
	float mean;
	
	if (e != f) {
		error ("error in Symm_logcosh_JM, dims dont match");
	}
	else {
		mat1 = Calloc (e * p, float);
		mat2 = Calloc (e * p, float);
		mat3 = Calloc (e * e, float);
		mat4 = Calloc (e * e, float);
		mat5 = Calloc (e * e, float);
		mat6 = Calloc (e * e, float);
		
		mmult_JM (w_init, e, e, data, e, p, mat1);  
		
		
		for (i = 0; i < e; i++) {
			for (j = 0; j < p; j++) {
				mat1[i * p + j] = tanh (alpha * mat1[i * p + j]);
			}
		}			
		transpose_mat_JM (data, &e, &p, mat2);
		for (i = 0; i < e; i++) {
			for (j = 0; j < p; j++) {
				mat2[i * p + j] = (mat2[i * p + j]) / p;
			}
		}			
		mmult_JM (mat1, e, p, mat2, p, e, mat3);       
		for (i = 0; i < e; i++) {
			for (j = 0; j < p; j++) {
				mat1[i * p + j] =
					(alpha * (1 - (mat1[i * p + j]) * (mat1[i * p + j])));
			}
		}
		
		for (i = 0; i < e; i++) {
			mean = 0;
			for (j = 0; j < p; j++) {
				mean += ((mat1[i * p + j]) / p);
			}
			mat4[i * e + i] = mean;
		}		       
		mmult_JM (mat4, e, e, w_init, e, e, mat5); 
		for (i = 0; i < e; i++) {
			for (j = 0; j < e; j++) {
				mat4[i * e + j] = (mat3[i * e + j] - mat5[i * e + j]);
			}
		}
		
		transpose_mat_JM (w_init, &e, &e, mat6);
		orthog_mat_JM (mat4, e, w_final);
		
		
		mmult_JM (w_final, e, e, mat6, e, e, mat5);       
		mean = 0;
		for (i = 0; i < e; i++) {
			if (fabs (1 - fabs (mat5[i * e + i])) > mean) {
				mean = (fabs (1 - fabs (mat5[i * e + i])));
			}
		}
		*Tol = mean;
		Free (mat1);
		Free (mat2);
		Free (mat3);
		Free (mat4);
		Free (mat5);
		Free (mat6);
	}
}

void
Def_logcosh_JM (float *w_init, int e, float *data, int f, int p, float alpha, float *w_final)
{	
/* Function that carries out Deflation ICA using an logcosh approximation to the neg. entropy function */
	
	float *mat1, *mat2, *mat3, *mat4;
	int i, j;
	float mean;
	
	if (e != f) {
		error ("error in Def_logcosh_JM, dims dont match");
	}
	else {
		mat1 = Calloc (1 * p, float);
		mat2 = Calloc (e * p, float);
		mat3 = Calloc (1 * e, float);
		mat4 = Calloc (1 * e, float);
		
		mmult_JM (w_init, 1, e, data, e, p, mat1);
		
		
		for (i = 0; i < p; i++) {
			mat1[i] = tanh (alpha * mat1[i]);
		}			
		transpose_mat_JM (data, &e, &p, mat2);
		for (i = 0; i < e; i++) {
			for (j = 0; j < p; j++) {
				mat2[i * p + j] = (mat2[i * p + j]) / p;
			}
		}
		
		mmult_JM (mat1, 1, p, mat2, p, e, mat3);
		for (i = 0; i < p; i++) {
			mat1[i] = (alpha * (1 - (mat1[i]) * (mat1[i])));
		}
		
		mean = 0;
		for (j = 0; j < p; j++) {
			mean += ((mat1[j]) / p);
		}
		for (i = 0; i < e; i++) {
			mat4[i] = (w_init[i]) * mean;
		}			
		for (i = 0; i < e; i++) {
			w_final[i] = (mat3[i] - mat4[i]);
		}		
				
		Free (mat1);
		Free (mat2);
		Free (mat3);
		Free (mat4);
		
	}
}
	
	void
Symm_exp_JM (float *w_init, int e, float *data, int f, int p, float alpha, float *w_final, float *Tol)
		{	
    /* Function that carries out Symmetric ICA using a exponential approximation to the neg. entropy function */

float *mat1, *mat2, *mat3, *mat4, *mat5, *mat0, *mat6;
int i, j;
float mean;

if (e != f) {
    error ("error in Symm_exp_JM, dims dont match");
}
else {
    mat0 = Calloc (e * p, float);
    mat1 = Calloc (e * p, float);
    mat2 = Calloc (e * p, float);
    mat3 = Calloc (e * e, float);
    mat4 = Calloc (e * e, float);
    mat5 = Calloc (e * e, float);
    mat6 = Calloc (e * e, float);
    mmult_JM (w_init, e, e, data, e, p, mat1);  
    for (i = 0; i < e; i++) {
	for (j = 0; j < p; j++) {
	    mat0[i * p + j] =
		(mat1[i * p + j]) * exp (-0.5 * (mat1[i * p + j]) *
					 (mat1[i * p + j]));
	}
    }		      
    transpose_mat_JM (data, &e, &p, mat2);
    for (i = 0; i < e; i++) {
	for (j = 0; j < p; j++) {
	    mat2[i * p + j] = (mat2[i * p + j]) / p;
	}
    }		       
    mmult_JM (mat0, e, p, mat2, p, e, mat3);       
    for (i = 0; i < e; i++) {
	for (j = 0; j < p; j++) {
	    mat1[i * p + j] =
		((1 - (mat1[i * p + j]) * (mat1[i * p + j])) * 
		 exp (-0.5 * (mat1 [i * p + j]) * (mat1 [i * p + j])));
	}
    }

    for (i = 0; i < e; i++) {
	mean = 0;
	for (j = 0; j < p; j++) {
	    mean += ((mat1[i * p + j]) / p);
	}
	mat4[i * e + i] = mean;
    }		       
    mmult_JM (mat4, e, e, w_init, e, e, mat5); 
    for (i = 0; i < e; i++) {
	for (j = 0; j < e; j++) {
	    mat4[i * e + j] = (mat3[i * e + j] - mat5[i * e + j]);
	}
    }

    transpose_mat_JM (w_init, &e, &e, mat6);
    orthog_mat_JM (mat4, e, w_final);	

    mmult_JM (w_final, e, e, mat6, e, e, mat5);	
    mean = 0;
    for (i = 0; i < e; i++) {
	if (fabs (1 - fabs (mat5[i * e + i])) > mean) {
	    mean = (fabs (1 - fabs (mat5[i * e + i])));
	}
    }
    *Tol = mean;
    Free (mat1);
    Free (mat2);
    Free (mat3);
    Free (mat4);
    Free (mat5);
    Free (mat0);
    Free (mat6);
}
}

void
Def_exp_JM (float *w_init, int e, float *data, int f, int p, float alpha, float *w_final)
{	
    /* Function that carries out Deflation ICA using an exponential approximation to the neg. entropy function */

float *mat1, *mat2, *mat3, *mat4;
int i, j;
float mean;

if (e != f) {
    error ("error in Def_exp_JM, dims dont match");
}
else {
    mat1 = Calloc (1 * p, float);
    mat2 = Calloc (e * p, float);
    mat3 = Calloc (1 * e, float);
    mat4 = Calloc (1 * e, float);

    mmult_JM (w_init, 1, e, data, e, p, mat1);	

    for (i = 0; i < p; i++) {
	mat1[i] = ((mat1[i]) * exp (-0.5 * (mat1[i]) * (mat1[i])));
    }

    transpose_mat_JM (data, &e, &p, mat2);
    for (i = 0; i < e; i++) {
	for (j = 0; j < p; j++) {
	    mat2[i * p + j] = (mat2[i * p + j]) / p;
	}
    }

    mmult_JM (mat1, 1, p, mat2, p, e, mat3);       

    mmult_JM (w_init, 1, e, data, e, p, mat1);	
    for (i = 0; i < p; i++) {
	mat1[i] =
	    ((1 -
	      (mat1[i]) * (mat1[i])) * exp (-.5 * (mat1[i]) * (mat1[i])));
    }		       
    mean = 0;
    for (j = 0; j < p; j++) {
	mean += ((mat1[j]) / p);
    }
    for (i = 0; i < e; i++) {
	mat4[i] = (w_init[i]) * mean;
    }		     
    for (i = 0; i < e; i++) {
	w_final[i] = (mat3[i] - mat4[i]);
    }		       


    Free (mat1);
    Free (mat2);
    Free (mat3);
    Free (mat4);

}
}

void
gramsch_JM (float *ww, int n, int m, int k)
{
int ip, jp;
float tmp;
/* do Gram-Schmidt on row k of (n*m) matrix ww */
k -= 1;
if (k > n) {
    error ("Error in gramsch");
}
else {
    for (ip = 0; ip < k; ip++) {
	tmp = 0;
	for (jp = 0; jp < m; jp++) {
	    tmp += ((ww[m * ip + jp]) * (ww[m * k + jp]));
	}
	for (jp = 0; jp < m; jp++) {
	    ww[m * k + jp] = (ww[m * k + jp] - ((ww[m * ip + jp]) * tmp));
	}
    }
}
}

void
rowstd_JM (float *ww, int n, int m, int k)
{
/* for ww (n*m), make ||ww[k, ]|| equal 1 */
float tmp = 0;
int i;
k -= 1;
if (k > n) {
    error ("Error in rowstd");
}
else {
    for (i = 0; i < m; i++) {
	tmp += ((ww[k * m + i]) * (ww[k * m + i]));
    }
    tmp = sqrt (tmp);
    for (i = 0; i < m; i++) {
	ww[k * m + i] = ((ww[k * m + i]) / tmp);
    }
}
}


void 
calc_K_JM(float *x, int *n, int *p, float *K)
{
    int i, j;
    float *xxt, *xt, *u, *d, *v, *temp1, *temp2;

    xxt = Calloc (*n * *n, float);
    xt = Calloc (*n * *p, float);

    /* transpose x matrix */
    transpose_mat_JM (x, n, p, xt); 

    /* calculate sample covariance matrix xxt */
    mmult_JM (x, *n, *p, xt, *p, *n, xxt); 
    for (i = 0; i < *n; i++) {
	    for (j = 0; j < *n; j++) {
		    xxt[*n * i + j] = xxt[*n * i + j] / *p;
	    }
    }	
    Free (xt);

    /* calculate svd decomposition of xxt */ 
    u = Calloc (*n * *n, float);
    d = Calloc (*n, float);
    v = Calloc (*n * *n, float);

    svd_JM (xxt, n, n, u, d, v); 


    /* calculate K matrix*/
    temp1 = Calloc (*n * *n, float);
    temp2 = Calloc (*n * *n, float);

    for (i = 0; i < *n; i++) {
	    temp1[*n * i + i] = 1 / sqrt (d[i]);
    }

    transpose_mat_JM (u, n, n, temp2);
    mmult_JM (temp1, *n, *n, temp2, *n, *n, K);
    Free (temp1);
    Free (temp2);
}

void
calc_A_JM(float *w, float *k, float *data, int *e, int *n, int *p, float *A, float *unmixed_data)
{
	/* calculate un-mixing matrix A */
	int i;
	float *um, *umt, *umumt, *uu, *dd, *vv, *temp1, *temp2, *temp3;

	um = Calloc (*e * *n, float);
	umt = Calloc (*n * *e, float);
	
	mmult_JM (w, *e, *e, k, *e, *n, um);
	mmult_JM (um, *e, *n, data, *n, *p, unmixed_data);	
	transpose_mat_JM (um, e, n, umt);	
	
	umumt = Calloc (*e * *e, float);
	mmult_JM (um, *e, *n, umt, *n, *e, umumt);	
	
	uu = Calloc (*e * *e, float);
	dd = Calloc (*e, float);
	vv = Calloc (*e * *e, float);
	svd_JM (umumt, e, e, uu, dd, vv);
	
	temp1 = Calloc (*e * *e, float);
	for (i = 0; i < *e; i++) {
		temp1[*e * i + i] = 1 / (dd[i]);
	}
	
	temp2 = Calloc (*e * *e, float);
	temp3 = Calloc (*e * *e, float);
	transpose_mat_JM (vv, e, e, temp3);
	mmult_JM (temp3, *e, *e, temp1, *e, *e, temp2);
	transpose_mat_JM (uu, e, e, vv);
	mmult_JM (temp2, *e, *e, vv, *e, *e, uu);
	
	mmult_JM (umt, *n, *e, uu, *e, *e, A);

	Free(um);
	Free(umt);
	Free(umumt);
	Free(uu);
	Free(dd);
	Free(vv);
	Free(temp1);
	Free(temp2);
	Free(temp3);

}


void
icainc_JM (float *data_matrix, float *w_matrix, int *nn, int *pp, int *ee,
	float *alpha, int *rowflag, int *colflag, int *funflag, int *maxit,
	float *lim, int *defflag, float *data_pre, float *Kmat1,
	float *w_final, float *ansa, float *ansx2)
{

	/* main ICA function */
	
	int i, j, k, n, p, e;
	float tol;
	float *temp_w1, *temp_w2;
	float *data1, *Kmat, *temp1, *w_init;
	
	
	
	n = *nn;
	p = *pp;
	e = *ee;
	
	/* make a copy of the data matrix*/
	data1 = Calloc (n * p, float);
	for (i = 0; i < n; i++) {
		for (j = 0; j < p; j++) {
			data_pre[i * p + j] = data_matrix[i * p + j];
		}
	}
	
	/* row center data matrix if required*/
	if (*rowflag == 1) {
		rowcentre_JM (data_pre, n, p);
			Rprintf ("Centering\n");
	}
	
	/* standardize columns of data matrix if required*/
	if (*colflag == 1) {
		colstandard_JM (data_pre, n, p);
	} 
	
	/* calculate pre-whitening matrix Kmat */
       	Rprintf ("Whitening\n");
	Kmat = Calloc (n * n, float);    
	calc_K_JM(data_pre, &n, &p, Kmat); 
	
	/* pre-whiten data and reduce dimension from size n to size e */
       
	for (i = 0; i < e; i++) {
		for (j = 0; j < n; j++) {
			Kmat1[i * n + j] = Kmat[i * n + j];
		}
	}
	mmult_JM (Kmat1, e, n, data_pre, n, p, data1);
	
	/* calculate initial (orthogonal) unmixing matrix w */
	temp1 = Calloc (e * e, float);	
	w_init = Calloc (e * e, float);
	for (i = 0; i < e; i++) {
		for (j = 0; j < e; j++) {
			temp1[i * e + j] = w_matrix[i * e + j];
		}
	}
	orthog_mat_JM (temp1, e, w_init);     
	
	
	
	
	/* Main ICA code */
	
	
    if (*defflag == 0) {
	    if (*funflag == 1) {
		    
		    
		    Rprintf("Symmetric FastICA using logcosh approx. to neg-entropy function\n");
		    
		    i = 1;
		    Symm_logcosh_JM (w_init, e, data1, e, p, *alpha, w_final, &tol);
		   
		    Rprintf ("Iteration %d tol=%f\n", i, tol);
		    i = 2;
		    
		    while ((tol > (*lim)) && (i < (*maxit))) {
			    Symm_logcosh_JM (w_final, e, data1, e, p, *alpha, w_final, &tol);
			    
			    Rprintf ("Iteration %d tol=%f\n", i, tol);
			    i += 1;
	    }
	    }
	    
	    if (*funflag == 2) {
		    
		    Rprintf("Symmetric FastICA using exponential approx. to neg-entropy function\n");
		    
		    i = 1;
		    Symm_exp_JM (w_init, e, data1, e, p, *alpha, w_final, &tol);
		    Rprintf ("Iteration %d tol=%f\n", i, tol);
		    
		    i = 2;
		    while ((tol > (*lim)) && (i < (*maxit))) {
			    Symm_exp_JM (w_final, e, data1, e, p, *alpha, w_final, &tol);
			    Rprintf ("Iteration %d tol=%f\n", i, tol);
			    i += 1;
		    }
	    }
    }
    
    if (*defflag == 1) {
	    temp_w1 = Calloc (e, float);
	    temp_w2 = Calloc (e, float);
	    
	    if (*funflag == 1) {
		   
		    Rprintf ("Deflation FastICA using logcosh approx. to neg-entropy function\n");
		    
		    for (i = 0; i < e; i++) {
			    k = 0;
			    gramsch_JM (w_init, e, e, i + 1); 
			    rowstd_JM (w_init, e, e, i + 1);
			    tol = 1;
			    
			    while ((tol > (*lim)) && (k < (*maxit))) {
				    for (j = 0; j < e; j++) {
					    temp_w1[j] = w_init[i * e + j];
				    }
				    Def_logcosh_JM (temp_w1, e, data1, e, p, *alpha, temp_w2);
		    for (j = 0; j < e; j++) {
			    w_init[i * e + j] = temp_w2[j];
		    }
		    gramsch_JM (w_init, e, e, i + 1);
		    rowstd_JM (w_init, e, e, i + 1);
		    tol = 0;
		    for (j = 0; j < e; j++) {
			    tol += ((temp_w1[j]) * (w_init[i * e + j]));
		    }
		    tol = (fabs (fabs (tol) - 1));
		    k += 1;
			    }
			    
			    
			    Rprintf ("Component %d needed %d iterations tol=%f\n",
					     i + 1, k, tol);
			    
		    }
	    }
	    if (*funflag == 2) {
		    
		    
		    Rprintf ("Deflation FastICA using exponential approx. to neg-entropy function\n");
		    
		    for (i = 0; i < e; i++) {
			    k = 0;
			    gramsch_JM (w_init, e, e, i + 1);
			    rowstd_JM (w_init, e, e, i + 1);
			    tol = 1;
			    
			    while ((tol > (*lim)) && (k < (*maxit))) {
				    for (j = 0; j < e; j++) {
					    temp_w1[j] = w_init[i * e + j];
		    }
				    Def_exp_JM (temp_w1, e, data1, e, p, *alpha, temp_w2);
				    for (j = 0; j < e; j++) {
					    w_init[i * e + j] = temp_w2[j];
				    }
				    gramsch_JM (w_init, e, e, i + 1);
				    rowstd_JM (w_init, e, e, i + 1);
				    tol = 0;
				    for (j = 0; j < e; j++) {
					    tol += ((temp_w1[j]) * (w_init[i * e + j]));
				    }
		    tol = (fabs (fabs (tol) - 1));
		    k += 1;
			    }
			    
			  
			    Rprintf ("Component %d needed %d iterations tol=%f\n",
					     i + 1, k, tol);
			    
		    }
	    }
	    for (i = 0; i < e; i++) {
		    for (j = 0; j < e; j++) {
			    w_final[i * e + j] = w_init[i * e + j];
		    }
	    } 
	    Free (temp_w1);
	    Free (temp_w2);
	    
    }
    
    
    /* calculate mixing matrix ansa */
    calc_A_JM(w_final, Kmat1, data_pre, &e, &n, &p, ansa, ansx2);
    
    
    
    Free (data1);
    Free (Kmat);
    Free (temp1);
    Free (w_init);
    
}
/* Analysis functions */

/* void fmri_JM(char **file, int *maskflag, char **msk_file, int *slices, int *nsl, float *s_mat, float *ans){ */


/*   int mask_size = 0; */
/*   int i,j,k,l,x,y,z,n,nm,t,nc,file_name_length,test; */
/*   float alpha,lim,*data_matrix;  */
/*   float *pre_processed_data_matrix, *k_matrix, *w_calc_matrix, *a_matrix, *s_matrix; */
/*   int swapbytes = 1,mask_swapbytes = 1; */
/*   struct header *head,*mask_head; */
/*   struct data_array *array,*mask; */
/*   char *in_file,*mask_file,img_file[200] = "\0",hdr_file[200] = "\0",mask_img[200] = "\0",mask_hdr[200] = "\0",mask_flag = 'F'; */
  
 
/*   in_file = file[0]; */
/*   mask_file = msk_file[0]; */
/*   if(*maskflag == 1) mask_flag = 'T'; */


/*   head = Calloc(1,struct header); */
/*   mask_head = Calloc(1,struct header); */
/*   array = Calloc(1,struct data_array); */
/*   mask = Calloc(1,struct data_array); */
    
  
/*   /\*  Read in dataset *\/ */
/*   Rprintf("Reading in dataset\n"); */
/*   file_name_length = strlen(in_file); */
/*   strncat(img_file,in_file,file_name_length - 4); */
/*   strcat(img_file,".img"); */
/*   strncat(hdr_file,in_file,file_name_length - 4); */
/*   strcat(hdr_file,".hdr"); */
  

/*   swaptest_JM(&test, hdr_file); */
/*   if(test == 348) swapbytes = 0; */
  
/*   read_analyze_header_JM(head, hdr_file, &swapbytes); */
/* /\*    print_analyze_header_JM(head); *\/ */
  
/*   n = ((*head).dim[1]*(*head).dim[2]*(*head).dim[3]*(*head).dim[4]);  */
/*   (*array).n = n; */
/*   (*array).data = Calloc((*array).n,float); */
  
/*   read_data_as_float_JM(array, head, img_file, &swapbytes); */
  
/*   /\*  Read in/create mask *\/ */
/*   nm = ((*array).x*(*array).y*(*array).z); */
/*   (*mask).n = nm; */
/*   (*mask).data = Calloc(nm,float); */


/*   if(mask_flag == 'T'){ */
/*     Rprintf("Reading in mask\n"); */
/*     file_name_length = strlen(mask_file); */
/*     strncat(mask_img,mask_file,file_name_length - 4); */
/*     strcat(mask_img,".img"); */
/*     strncat(mask_hdr,mask_file,file_name_length - 4); */
/*     strcat(mask_hdr,".hdr"); */
    
/*     swaptest_JM(&test, mask_hdr); */
/*     if(test == 348) mask_swapbytes = 0; */
    
    
/*     read_analyze_header_JM(mask_head, mask_hdr, &mask_swapbytes); */
/*     read_data_as_float_JM(mask, mask_head, mask_img, &mask_swapbytes); */
/*     mask_mask_JM(array, mask, slices, nsl); */
/*     size_mask_JM(array, mask, &mask_size);  */
/*     Rprintf("Mask size = %d\n",mask_size);  */
/*   } */
/*   else */
/*     { */
/*       Rprintf("Making mask\n"); */
/*       create_mask_JM(array, mask, &mask_size);  */
/*       mask_mask_JM(array, mask, slices, nsl); */
/*       size_mask_JM(array, mask, &mask_size);  */
/*       Rprintf("Mask size = %d\n",mask_size);  */
/*     } */
  
/*   /\*  Create 2D data matrix (columns are voxel time series) *\/ */
/*   Rprintf("Creating data matrix\n"); */
/*   data_matrix = Calloc(mask_size*(*array).t,float); */
/*   create_data_matrix_JM(array, mask, &mask_size, data_matrix); */
/*   Free((*array).data);  */

/*   /\* Detrend using S matrix  *\/ */
/*   Rprintf("Starting matrix multiplication\n"); */
/*   mmult_JM(s_mat, (*array).t, (*array).t, data_matrix, (*array).t, mask_size, ans); */
/*   Rprintf("Finishing matrix multiplication\n"); */
  


/*      /\*  Free memory *\/ */
/*   if(mask_flag == 'T') Free(mask_head); */
/*   Free(head); */
/*   Free(data_matrix); */
/*   Free(array); */
/*   Free((*mask).data); */
/*   Free(mask); */
  
  
/* } */

