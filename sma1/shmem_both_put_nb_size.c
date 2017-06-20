/**********************************************************************
!----------------------------------------------------------------------
! Copyright (c) 2010, Cray Inc.
! All rights reserved.
! 
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are
! met:
! 
! * Redistributions of source code must retain the above copyright
!   notice, this list of conditions and the following disclaimer.
! 
! * Redistributions in binary form must reproduce the above copyright
!   notice, this list of conditions and the following disclaimer in the
!   documentation and/or other materials provided with the distribution.
! 
! * Neither the name Cray Inc. nor the names of its contributors may be
!   used to endorse or promote products derived from this software
!   without specific prior written permission.

! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
! A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
! OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
! SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
! LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
! DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
! THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!----------------------------------------------------------------------
!
! Purpose:  Functional tests for the following shmem_put routines ---
!      Uses various amounts of data to transfer a multiple number of times.
!      Uses both non-blocking and blocking calls.
!           shmem_short_put
!           shmem_int_put_nb
!           shmem_long_put_nb
!           shmem_float_put_nb
!           shmem_double_put_nb
!
!*********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <mpp/shmem.h>

#define MAX_SIZE 65536
#ifdef QUICK_TEST
  #define MAX_ITER 100
#else
  #define MAX_ITER 10
#endif

void
shmalloc_error(void)
{
  fprintf(stderr, "FAIL: not enough memory available\n");
  exit(1);
}

int main(int argc, char **argv)
{
  int i,j,iter;
  int my_pe,n_pes;
  int *flag,*one;
  size_t max_elements,max_elements_bytes;
  size_t elements[16] = {1,2,4,8,12,16,24,32,64,128,256,512,1024,2048,4096,8192};
  int num_elements = 16;

  short *srce_short,*targ_short;
  int *srce_int,*targ_int;
  long *srce_long,*targ_long;
  float *srce_float,*targ_float;
  double *srce_double,*targ_double;

  shmem_init();
  my_pe = shmem_my_pe();
  n_pes = shmem_n_pes();
  flag = shmem_malloc((size_t) sizeof(int));
  one  = shmem_malloc((size_t) sizeof(int));
  *one  = 1;

/*  fail if trying to use odd number of processors  */
  if ( (n_pes % 2) != 0 ){
        fprintf(stderr, "FAIL - test requires even number of PEs\n");
        exit(1);
  }

  if(my_pe == 0)
    fprintf(stderr, "shmem_both_put_nb_size(%s)\n", argv[0]);

/*  alloc arrays   */

  max_elements = (size_t) (MAX_SIZE / sizeof(int));
  max_elements_bytes = (size_t) (sizeof(int)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_int_put_nb        max_elements = %d\n",max_elements);
  srce_int = shmem_malloc(max_elements_bytes);
  targ_int = shmem_malloc(max_elements_bytes);
  if((srce_int == NULL) || (targ_int == NULL))
    shmalloc_error();

  max_elements = (size_t) (MAX_SIZE / sizeof(short));
  max_elements_bytes = (size_t) (sizeof(short)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_short_put         max_elements = %d\n",max_elements);
  srce_short = shmem_malloc(max_elements_bytes);
  targ_short = shmem_malloc(max_elements_bytes);
  if((srce_short == NULL) || (targ_short == NULL))
    shmalloc_error();

   max_elements = (size_t) (MAX_SIZE / sizeof(long));
  max_elements_bytes = (size_t) (sizeof(long)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_long_put_nb       max_elements = %d\n",max_elements);
  srce_long = shmem_malloc(max_elements_bytes);
  targ_long = shmem_malloc(max_elements_bytes);
  if((srce_long == NULL) || (targ_long == NULL))
    shmalloc_error();

  max_elements = (size_t) (MAX_SIZE / sizeof(float));
  max_elements_bytes = (size_t) (sizeof(float)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_float_put_nb      max_elements = %d\n",max_elements);
  srce_float = shmem_malloc(max_elements_bytes);
  targ_float = shmem_malloc(max_elements_bytes);
  if((srce_float == NULL) || (targ_float == NULL))
    shmalloc_error();

  max_elements = (size_t) (MAX_SIZE / sizeof(double));
  max_elements_bytes = (size_t) (sizeof(double)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_double_put_nb     max_elements = %d\n",max_elements);
  srce_double = shmem_malloc(max_elements_bytes);
  targ_double = shmem_malloc(max_elements_bytes);
  if((srce_double == NULL) || (targ_double == NULL))
    shmalloc_error();

  if(my_pe == 0)
    fprintf(stderr,"Actual value used for   max_elements = %d\n",max_elements);
  /* try the different sizes MAX_ITER times */
  for (iter = 0; iter < MAX_ITER; iter++) {
   for (i = 0; i < num_elements; i++) {
    *flag = 0;
    if (elements[i] <= max_elements) {
     if ( (my_pe % 2) == 0 )
       for(j = 0; j < elements[i]; j++) {
         srce_short[j] = (short)(my_pe+j);
         srce_int[j] = (int)(iter*10000+elements[i]*100+my_pe+j);
         srce_long[j] = (long)(iter*10000+elements[i]*100+my_pe+j);
         srce_float[j] = (float)(iter*10000+elements[i]*100+my_pe+j);
         srce_double[j] = (double)(iter*10000+elements[i]*100+my_pe+j);
       }
     else
       for(j = 0; j < elements[i]; j++) {
         targ_short[j] = (short)(my_pe+j);
         targ_int[j] = (int)(iter*10000+elements[i]*100+my_pe+j);
         targ_long[j] = (long)(iter*10000+elements[i]*100+my_pe+j);
         targ_float[j] = (float)(iter*10000+elements[i]*100+my_pe+j);
         targ_double[j] = (double)(iter*10000+elements[i]*100+my_pe+j);
       }
     shmem_barrier_all();
     if ( (my_pe % 2) == 0 ) {
#ifndef OPENSHMEM
       shmemx_int_put_nb(targ_int,srce_int,elements[i],my_pe+1,NULL);
       shmemx_long_put_nb(targ_long,srce_long,elements[i],my_pe+1,NULL);
       shmemx_float_put_nb(targ_float,srce_float,elements[i],my_pe+1,NULL);
       shmemx_double_put_nb(targ_double,srce_double,elements[i],my_pe+1,NULL);
#else
       shmem_int_put_nbi(targ_int,srce_int,elements[i],my_pe+1);
       shmem_long_put_nbi(targ_long,srce_long,elements[i],my_pe+1);
       shmem_float_put_nbi(targ_float,srce_float,elements[i],my_pe+1);
       shmem_double_put_nbi(targ_double,srce_double,elements[i],my_pe+1);
#endif
       /* this one is blocking */
       shmem_short_put(targ_short,srce_short,elements[i],my_pe+1);
       shmem_quiet();
       shmem_int_put(flag,one,(size_t)1,my_pe+1);
     } else {
       shmem_int_wait(flag,0);
       for(j = 0; j < elements[i]; j++) {
         if ( targ_short[j] != (short)(my_pe+j-1) )
           fprintf(stderr,
           "FAIL: PE [%d] iter=%d i=%d targ_short[%d]=%d not equal %d\n",
              my_pe,iter,i,j,targ_short[j],my_pe+j-1);
         if ( targ_int[j] != (int)(iter*10000+elements[i]*100+my_pe+j-1) )
           fprintf(stderr, 
           "FAIL: PE [%d] iter=%d i=%d targ_int[%d]=%d not equal %d\n",
              my_pe,iter,i,j,targ_int[j],iter*10000+elements[i]*100+my_pe+j-1);
         if ( targ_long[j] != (long)(iter*10000+elements[i]*100+my_pe+j-1) )
           fprintf(stderr,
           "FAIL: PE [%d] iter=%d i=%d targ_long[%d]=%d not equal %d\n",
              my_pe,iter,i,j,targ_long[j],iter*10000+elements[i]*100+my_pe+j-1);
         if ( targ_float[j] != (float)(iter*10000+elements[i]*100+my_pe+j-1) )
           fprintf(stderr,
           "FAIL: PE [%d] iter=%d i=%d targ_long[%d]=%f not equal %d\n",
              my_pe,iter,i,j,targ_float[j],iter*10000+elements[i]*100+my_pe+j-1);
         if ( targ_double[j] != (double)(iter*10000+elements[i]*100+my_pe+j-1) )
           fprintf(stderr,
           "FAIL: PE [%d] iter=%d i=%d targ_double[%d]=%f not equal %d\n",
              my_pe,iter,i,j,targ_double[j],iter*10000+elements[i]*100+my_pe+j-1);
         }
     }
    }
   }
  }
  shmem_free(srce_short);  shmem_free(targ_short);
  shmem_free(srce_int);  shmem_free(targ_int);
  shmem_free(srce_long);  shmem_free(targ_long);
  shmem_free(srce_float);  shmem_free(targ_float);
  shmem_free(srce_double);  shmem_free(targ_double);
#ifdef NEEDS_FINALIZE
  shmem_finalize(); 
#endif
  return 0;
}
