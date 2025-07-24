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
 char dim_info;
 short dim[8];
 float intent_p1;
 float intent_p2;
 float intent_p3;
 short intent_code;
 short datatype;
 short bitpix;
 short slice_start;
 float pixdim[8];
 float vox_offset;
 float scl_slope;
 float scl_inter;
 short slice_end;
 char slice_code;
 char xyzt_units;
 float cal_max;
 float cal_min;
 float slice_duration;
 float toffset;
 int glmax;
 int glmin;
 char descrip[80];
 char aux_file[24];
 short qform_code ;   
 short sform_code ;   
 float quatern_b ;    
 float quatern_c ;    
 float quatern_d ;    
 float qoffset_x ;    
 float qoffset_y ;    
 float qoffset_z ;    
 float srow_x[4] ;    
 float srow_y[4] ;    
 float srow_z[4] ;    
 char intent_name[16];
 char magic[4] ;      
 char extension[4] ;      


};


struct data_array{
  int x;
  int y;
  int z;
  int t;
  int n;
  float *data;
};


void read_nifti_header_JM(struct header*, char*, int*);
void readchar_JM(char*, char*, int*, int, long, int);
void read2byte_JM(short*, char*, int* , int, long, int);
void read4byte_JM(int*, char*, int* , int, long, int);
void readfloat_JM(float*, char*, int*, int, long, int);


void write8bitappend_JM(int*, char**, int*);
void write2byteappend_JM(int*, char**, int*);
void writefloatappend_JM(float*, char**, int*);


void read_nifti_header_wrap_JM(char **name, 
			      int *swapbytes, 
			      int *sizeof_hdr,
			      char **data_type,
			      char **db_name,
			      int *extents,
			      int *session_error,
			      char **regular,
			      char **dim_info,
			      int *dim,
			      float *intent_p1,
			      float *intent_p2,
			      float *intent_p3,
			      int *intent_code,
			      int *datatype,
			      int *bitpix,
			      int *slice_start,
			      float *pixdim,
			      float *vox_offset,
			      float *scl_slope,
			      float *scl_inter,
			      short *slice_end,
			      char **slice_code,
			      char **xyzt_units,
			      float *cal_max,
			      float *cal_min,
			      float *slice_duration,
			      float *toffset,
			      int *glmax,
			      int *glmin,
			      char **descrip,
			      char **aux_file,
			      int *qform_code,
			      int *sform_code,   
			      float *quatern_b,
			      float *quatern_c,
			      float *quatern_d,
			      float *qoffset_x,
			      float *qoffset_y,
			      float *qoffset_z,
			      float *srow_x,
			      float *srow_y,
			      float *srow_z,
			      char **intent_name,
			      char **magic,
			      char **extension

			      ) 
     
{
  /*Reads in all the fields of a .hdr header file*/
  
  int i;
  short tmp,tmp1[8];
  
  read4byte_JM(sizeof_hdr, name[0], swapbytes, 1, 0L, 0);
  readchar_JM(data_type[0], name[0], swapbytes, 10, 4L, 0);
  readchar_JM(db_name[0], name[0], swapbytes, 18, 14L, 0);
  read4byte_JM(extents, name[0], swapbytes, 1, 32L, 0);
  read2byte_JM(&tmp, name[0], swapbytes, 1, 36L, 0);
  *session_error = (int) tmp;
  readchar_JM(regular[0], name[0], swapbytes, 1, 38L, 0);
  readchar_JM(dim_info[0], name[0], swapbytes, 1, 39L, 0);
  read2byte_JM(tmp1, name[0], swapbytes, 8, 40L, 0);
  for(i = 0; i < 8; i++){dim[i] = (int) tmp1[i];}
  readfloat_JM(intent_p1, name[0], swapbytes, 1, 56L, 0);
  readfloat_JM(intent_p2, name[0], swapbytes, 1, 60L, 0);
  readfloat_JM(intent_p3, name[0], swapbytes, 1, 64L, 0);
  read2byte_JM(&tmp, name[0], swapbytes, 1, 68L, 0);
  *intent_code = (int) tmp;
  read2byte_JM(&tmp, name[0], swapbytes, 1, 70L, 0);
  *datatype = (int) tmp;
  read2byte_JM(&tmp, name[0], swapbytes, 1, 72L, 0);
  *bitpix = (int) tmp;
  read2byte_JM(&tmp, name[0], swapbytes, 1, 74L, 0);
  *slice_start = (int) tmp;
  readfloat_JM(pixdim, name[0], swapbytes, 8, 76L, 0);
  readfloat_JM(vox_offset, name[0], swapbytes, 1, 108L, 0);
  readfloat_JM(scl_slope, name[0], swapbytes, 1, 112L, 0);
  readfloat_JM(scl_inter, name[0], swapbytes, 1, 116L, 0);
  read2byte_JM(&tmp, name[0], swapbytes, 1, 120L, 0);
  *slice_end = (int) tmp;
  readchar_JM(slice_code[0], name[0], swapbytes, 1, 122L, 0);
  readchar_JM(xyzt_units[0], name[0], swapbytes, 1, 123L, 0);
  readfloat_JM(cal_max, name[0], swapbytes, 1, 124L, 0);
  readfloat_JM(cal_min, name[0], swapbytes, 1, 128L, 0);
  readfloat_JM(slice_duration, name[0], swapbytes, 1, 132L, 1);
  readfloat_JM(toffset, name[0], swapbytes, 1, 136L, 1);
  read4byte_JM(glmax, name[0], swapbytes, 1, 140L ,1);
  read4byte_JM(glmin, name[0], swapbytes, 1, 144L, 1);
  readchar_JM(descrip[0], name[0], swapbytes, 80, 148L, 0);
  readchar_JM(aux_file[0], name[0], swapbytes, 24, 228L, 0);
   read2byte_JM(&tmp, name[0], swapbytes, 1, 252L, 0);
  *qform_code = (int) tmp;
  read2byte_JM(&tmp, name[0], swapbytes, 1, 254L, 0);
  *sform_code = (int) tmp;
  readfloat_JM(quatern_b, name[0], swapbytes, 1, 256L, 0);
  readfloat_JM(quatern_c, name[0], swapbytes, 1, 260L, 0);
  readfloat_JM(quatern_d, name[0], swapbytes, 1, 264L, 0);
  readfloat_JM(qoffset_x, name[0], swapbytes, 1, 268L, 0);
  readfloat_JM(qoffset_y, name[0], swapbytes, 1, 272L, 0);
  readfloat_JM(qoffset_z, name[0], swapbytes, 1, 276L, 0);
  readfloat_JM(srow_x, name[0], swapbytes, 4, 280L, 0);
  readfloat_JM(srow_y, name[0], swapbytes, 4, 296L, 0);
  readfloat_JM(srow_z, name[0], swapbytes, 4, 312L, 0);
  readchar_JM(intent_name[0], name[0], swapbytes, 16, 328L, 0);
  readchar_JM(magic[0], name[0], swapbytes, 4, 344L, 0);
  readchar_JM(extension[0], name[0], swapbytes, 1, 348L, 0);
  readchar_JM(extension[1], name[0], swapbytes, 1, 349L, 0);
  readchar_JM(extension[2], name[0], swapbytes, 1, 350L, 0);
  readchar_JM(extension[3], name[0], swapbytes, 1, 351L, 0);



}

void read_nifti_header_JM(struct header *head, char *name, int *swapbytes)

{
  /*Reads in all the fields of a .hdr header file*/
  

  read4byte_JM(&head->sizeof_hdr, name, swapbytes, 1, 0L, 0);  
  readchar_JM(head->data_type,name, swapbytes, 10, 4L, 0);
  readchar_JM(head->db_name,name, swapbytes, 18, 14L, 0);
  read4byte_JM(&head->extents, name, swapbytes, 1, 32L, 1);
  read2byte_JM(&head->session_error, name, swapbytes, 1, 36L, 1);
  readchar_JM(&head->regular, name, swapbytes, 1, 38L, 0);
  readchar_JM(&head->dim_info, name, swapbytes, 1, 39L, 0);
  read2byte_JM(&head->dim[0], name, swapbytes, 8, 40L, 1);
  readfloat_JM(&head->intent_p1, name, swapbytes, 1, 56L, 1);
  readfloat_JM(&head->intent_p2, name, swapbytes, 1, 60L, 1);
  readfloat_JM(&head->intent_p3, name, swapbytes, 1, 64L, 1);
  read2byte_JM(&head->intent_code, name, swapbytes, 1, 68L, 1);
  read2byte_JM(&head->datatype, name, swapbytes, 1, 70L, 1);
  read2byte_JM(&head->bitpix, name, swapbytes, 1, 72L, 1);
  read2byte_JM(&head->slice_start, name, swapbytes, 1, 74L, 1);
  readfloat_JM(&head->pixdim[0], name, swapbytes, 8, 76L, 1);
  readfloat_JM(&head->vox_offset, name, swapbytes, 1, 108L, 1);
  readfloat_JM(&head->scl_slope, name, swapbytes, 1, 112L, 1);
  readfloat_JM(&head->scl_inter, name, swapbytes, 1, 116L, 1);
  read2byte_JM(&head->slice_end, name, swapbytes, 1, 120L, 1);
  readchar_JM(&head->slice_code, name, swapbytes, 1, 122L, 0);
  readchar_JM(&head->xyzt_units, name, swapbytes, 1, 123L, 0);
  readfloat_JM(&head->cal_max, name, swapbytes, 1, 124L, 1);
  readfloat_JM(&head->cal_min, name, swapbytes, 1, 128L, 1);
  readfloat_JM(&head->slice_duration, name, swapbytes, 1, 132L, 1);
  readfloat_JM(&head->toffset, name, swapbytes, 1, 136L, 1);
  read4byte_JM(&head->glmax, name, swapbytes, 1, 140L, 1);
  read4byte_JM(&head->glmin, name, swapbytes, 1, 144L, 1);
  readchar_JM(head->descrip, name, swapbytes, 80, 148L, 0);
  readchar_JM(head->aux_file, name, swapbytes, 24, 228L, 0);
  read2byte_JM(&head->qform_code, name, swapbytes, 1, 252L, 1);
  read2byte_JM(&head->sform_code, name, swapbytes, 1, 254L, 1);
  readfloat_JM(&head->quatern_b, name, swapbytes, 1, 256L, 1);
  readfloat_JM(&head->quatern_c, name, swapbytes, 1, 260L, 1);
  readfloat_JM(&head->quatern_d, name, swapbytes, 1, 264L, 1);
  readfloat_JM(&head->qoffset_x, name, swapbytes, 1, 268L, 1);
  readfloat_JM(&head->qoffset_y, name, swapbytes, 1, 272L, 1);
  readfloat_JM(&head->qoffset_z, name, swapbytes, 1, 276L, 1);
  readfloat_JM(&head->srow_x[0], name, swapbytes, 4, 280L, 1);
  readfloat_JM(&head->srow_y[0], name, swapbytes, 4, 296L, 1);
  readfloat_JM(&head->srow_z[0], name, swapbytes, 4, 312L, 1);
  readchar_JM(head->intent_name, name, swapbytes, 16, 328L, 0);
  readchar_JM(head->magic, name, swapbytes, 4, 344L, 0);
  readchar_JM(head->extension, name, swapbytes, 4, 348L, 0);


}


void write_nifti_header_wrap_JM(char **name, 
			      int *sizeof_hdr,
			      char **data_type,
			      char **db_name,
			      int *extents,
			      int *session_error,
			      char **regular,
			      char **dim_info,
			      int *dim,
			      float *intent_p1,
			      float *intent_p2,
			      float *intent_p3,
			      int *intent_code,
			      int *datatype,
			      int *bitpix,
			      int *slice_start,
			      float *pixdim,
			      float *vox_offset,
			      float *scl_slope,
			      float *scl_inter,
			      short *slice_end,
			      char **slice_code,
			      char **xyzt_units,
			      float *cal_max,
			      float *cal_min,
			      float *slice_duration,
			      float *toffset,
			      int *glmax,
			      int *glmin,
			      char **descrip,
			      char **aux_file,
			      int *qform_code,
			      int *sform_code,   
			      float *quatern_b,
			      float *quatern_c,
			      float *quatern_d,
			      float *qoffset_x,
			      float *qoffset_y,
			      float *qoffset_z,
			      float *srow_x,
			      float *srow_y,
			      float *srow_z,
			      char **intent_name,
			      char **magic)
{

  /*Writes all the fields of a .hdr header file*/ 
  FILE *fp;
  int i;
  short dim1[8],tmp;

  char *extension;
  char val = (char)0;
  extension = &val;


  fp = fopen(name[0],"wb");
  if(fp == NULL) error("file writing error");
  fwrite(sizeof_hdr, 4, 1, fp);
  for(i = 0; i < 10; i++) fwrite(*(data_type) + i, 1, 1, fp);
  for(i = 0; i < 18; i++) fwrite(*(db_name) + i, 1, 1, fp);
  fwrite(extents, 4, 1, fp);
  tmp = (short) *session_error; fwrite(&tmp, 2, 1, fp);
  fwrite(*(regular), 1, 1, fp);
  fwrite(*(dim_info), 1, 1, fp);
  for(i = 0; i < 8; i++) dim1[i] = (short) *(dim + i);
  fwrite(&dim1, 2, 8, fp);
  fwrite(intent_p1, 4, 1, fp);
  fwrite(intent_p2, 4, 1, fp);
  fwrite(intent_p3, 4, 1, fp);
  tmp = (short) *intent_code; fwrite(&tmp, 2, 1, fp);
  tmp = (short) *datatype; fwrite(&tmp, 2, 1, fp);
  tmp = (short) *bitpix; fwrite(&tmp, 2, 1, fp);
  tmp = (short) *slice_start; fwrite(&tmp, 2, 1, fp);
  fwrite(pixdim, 4, 8, fp);
  fwrite(vox_offset, 4, 1, fp);
  fwrite(scl_slope, 4, 1, fp);
  fwrite(scl_inter, 4, 1, fp);
  tmp = (short) *slice_end; fwrite(&tmp, 2, 1, fp);
  fwrite(*(slice_code), 1, 1, fp);
  fwrite(*(xyzt_units), 1, 1, fp);
  fwrite(cal_max, 4, 1, fp);
  fwrite(cal_min, 4, 1, fp);
  fwrite(slice_duration, 4, 1, fp);
  fwrite(toffset, 4, 1, fp);
  fwrite(glmax, 4, 1, fp);
  fwrite(glmin, 4, 1, fp);
  for(i = 0; i < 80; i++) fwrite(*(descrip) + i, 1, 1, fp);
  for(i = 0; i < 24; i++) fwrite(*(aux_file) + i, 1, 1, fp);
  tmp = (short) *qform_code; fwrite(&tmp, 2, 1, fp);
  tmp = (short) *sform_code; fwrite(&tmp, 2, 1, fp);
  fwrite(quatern_b, 4, 1, fp);
  fwrite(quatern_c, 4, 1, fp);
  fwrite(quatern_d, 4, 1, fp);
  fwrite(qoffset_x, 4, 1, fp);
  fwrite(qoffset_y, 4, 1, fp);
  fwrite(qoffset_z, 4, 1, fp);
  fwrite(srow_x, 4, 4, fp);
  fwrite(srow_y, 4, 4, fp);
  fwrite(srow_z, 4, 4, fp);
  for(i = 0; i < 16; i++) fwrite(*(intent_name) + i, 1, 1, fp);
  for(i = 0; i < 3; i++) fwrite(*(magic) + i, 1, 1, fp);
  fwrite(extension, 1, 1, fp); 

  fclose(fp);
}


void read_nifti_magic_wrap(char **name,
			   int *swapbytes, 
                           char **magic) 

{

  readchar_JM(magic[0], name[0], swapbytes, 4, 344L, 0);

}



void write8bitappend_JM(int *imp, char **name, int *n)
{
  /* Writes in a sequence of 8 bit unsigned char integers */
  FILE *fp;
  unsigned char *temp;
  int i;
  float *extension;
  float val = 0;  

  extension = &val;

  //  temp = Calloc(*n, unsigned char);
  temp = (unsigned char *)calloc(*n, sizeof(unsigned char));
 
  for(i = 0; i < *n; i++) {
/*      if((*(imp+i))>255) *(imp+1)=255; */
/*      if((*(impp+i))<0) *(imp+1)=0; */
    *(temp + i) = (unsigned char) *(imp + i);
  }

  fp = fopen(name[0], "ab");
  
  
  fwrite(extension, 4, 1, fp);
  fwrite(temp, 1, *n, fp);

  // Free(temp);
  free(temp);
  fclose(fp);
}

void write2byteappend_JM(int *imp, char **name, int *n)
{
  /* Writes in a sequence of 2 byte short integers */
  FILE *fp;
  short *temp;
  int i;
  float *extension;
  float val = 0;  

  extension = &val;

  //  temp = Calloc(*n, short);
  temp = (short *)calloc(*n, sizeof(short));

  for(i = 0; i < *n; i++) {
  *(temp + i) = (short) *(imp + i);
  }

  fp = fopen(name[0], "ab");
  
  fwrite(extension, 4, 1, fp);
  fwrite(temp, 2, *n, fp);

  //  Free(temp);
  free(temp);
  fclose(fp);
}

void writefloatappend_JM(float *imp, char **name, int *n)
{
  /* Writes a sequence of 4 byte floats  */
  FILE *fp;  
  float *extension;
  float val = 0;  

  extension = &val;

  fp = fopen(name[0], "ab");
  
  fwrite(extension, 4, 1, fp);
  fwrite(imp, 4, *n, fp);

  fclose(fp);
}

