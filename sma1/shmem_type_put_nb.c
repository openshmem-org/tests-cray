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
!           shmem_short_put_nb
!           shmem_int_put_nb
!           shmem_long_put_nb
!           shmem_longlong_put_nb
!           shmem_float_put_nb
!           shmem_double_put_nb
!           shmem_longdouble_put_nb
!
! Notes:  If type longdouble is available, compile with -DHAVE_LONG_DOUBLE
!
!*********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <mpp/shmem.h>

#define MAX_SIZE 100

void
shmalloc_error(void)
{
  fprintf(stderr, "FAIL: not enough memory available\n");
  exit(1);
}

int main(int argc, char **argv)
{
  int j;
  int my_pe,n_pes;
  int *flag,*one;
  size_t max_elements,max_elements_bytes;

  short *srce_short,*targ_short;
  int *srce_int,*targ_int;
  long *srce_long,*targ_long;
  float *srce_float,*targ_float;
  double *srce_double,*targ_double;
  long long *srce_longlong,*targ_longlong;
#ifdef HAVE_LONG_DOUBLE
  long double *srce_longdouble,*targ_longdouble;
#endif


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
    fprintf(stderr, "shmem_type_put_nb(%s)\n", argv[0]);

/*  shmem_short_put_nb test   */
  *flag = 0;
  max_elements = (size_t) (MAX_SIZE / sizeof(short));
  max_elements_bytes = (size_t) (sizeof(short)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_short_put_nb      max_elements = %d\n",max_elements);
  srce_short = shmem_malloc(max_elements_bytes);
  targ_short = shmem_malloc(max_elements_bytes);
  if((srce_short == NULL) || (targ_short == NULL))
    shmalloc_error();
  if ( (my_pe % 2) == 0 )
    for(j = 0; j < max_elements; j++) 
      srce_short[j] = (short)(my_pe+j);
  else
    for(j = 0; j < max_elements; j++) 
      targ_short[j] = (short)(my_pe+j);
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
#ifndef OPENSHMEM
    shmemx_short_put_nb(targ_short,srce_short,max_elements,my_pe+1,NULL);
#else
    shmem_short_put_nbi(targ_short,srce_short,max_elements,my_pe+1);
#endif
    shmem_quiet();
    shmem_int_put(flag,one,(size_t)1,my_pe+1);
  } else {
    shmem_int_wait(flag,0);
    for(j = 0; j < max_elements; j++)
      if ( targ_short[j] != (short)(my_pe+j-1) )
        fprintf(stderr, "FAIL: PE [%d] targ_short[%d]=%d my_pe+j-1=%d\n",
                               my_pe,j,targ_short[j],my_pe+j-1);
  }
  shmem_free(srce_short);  shmem_free(targ_short);

/*  shmem_int_put_nb test   */
  *flag = 0;
  max_elements = (size_t) (MAX_SIZE / sizeof(int));
  max_elements_bytes = (size_t) (sizeof(int)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_int_put_nb        max_elements = %d\n",max_elements);
  srce_int = shmem_malloc(max_elements_bytes);
  targ_int = shmem_malloc(max_elements_bytes);
  if((srce_int == NULL) || (targ_int == NULL))
    shmalloc_error();
  if ( (my_pe % 2) == 0 )
    for(j = 0; j < max_elements; j++)
      srce_int[j] = (int)(my_pe+j);
  else
    for(j = 0; j < max_elements; j++)
      targ_int[j] = (int)(my_pe+j);
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
#ifndef OPENSHMEM
    shmemx_int_put_nb(targ_int,srce_int,max_elements,my_pe+1,NULL);
#else
    shmem_int_put_nbi(targ_int,srce_int,max_elements,my_pe+1);
#endif
    shmem_quiet();
    shmem_int_put(flag,one,(size_t)1,my_pe+1);
  } else {
    shmem_int_wait(flag,0);
    for(j = 0; j < max_elements; j++)
      if ( targ_int[j] != (int)(my_pe+j-1) )
	fprintf(stderr, "FAIL: PE [%d] targ_int[%d]=%d my_pe+j-1=%d\n",
                               my_pe,j,targ_int[j],my_pe+j-1);
  }
  shmem_free(srce_int);  shmem_free(targ_int);
  
/*  shmem_long_put_nb test   */
  *flag = 0;
  max_elements = (size_t) (MAX_SIZE / sizeof(long));
  max_elements_bytes = (size_t) (sizeof(long)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_long_put_nb       max_elements = %d\n",max_elements);
  srce_long = shmem_malloc(max_elements_bytes);
  targ_long = shmem_malloc(max_elements_bytes);
  if((srce_long == NULL) || (targ_long == NULL))
    shmalloc_error();
  if ( (my_pe % 2) == 0 )
    for(j = 0; j < max_elements; j++)
      srce_long[j] = (long)(my_pe+j);
  else
    for(j = 0; j < max_elements; j++)
      targ_long[j] = (long)(my_pe+j);
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
#ifndef OPENSHMEM
    shmemx_long_put_nb(targ_long,srce_long,max_elements,my_pe+1,NULL);
#else
    shmem_long_put_nbi(targ_long,srce_long,max_elements,my_pe+1);
#endif
    shmem_quiet();
    shmem_int_put(flag,one,(size_t)1,my_pe+1);
  } else {
    shmem_int_wait(flag,0);
    for(j = 0; j < max_elements; j++)
      if ( targ_long[j] != (long)(my_pe+j-1) )
        fprintf(stderr, "FAIL: PE [%d] targ_long[%d]=%d my_pe+j-1=%d\n",
                               my_pe,j,targ_long[j],my_pe+j-1);
  }
  shmem_free(srce_long);  shmem_free(targ_long);

/*  shmem_longlong_put_nb test   */
  *flag = 0;
  max_elements = (size_t) (MAX_SIZE / sizeof(long long));
  max_elements_bytes = (size_t) (sizeof(long long)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_longlong_put_nb   max_elements = %d\n",max_elements);
  srce_longlong = shmem_malloc(max_elements_bytes);
  targ_longlong = shmem_malloc(max_elements_bytes);
  if((srce_longlong == NULL) || (targ_longlong == NULL))
    shmalloc_error();
  if ( (my_pe % 2) == 0 )
    for(j = 0; j < max_elements; j++)
      srce_longlong[j] = (long long)(my_pe+j);
  else
    for(j = 0; j < max_elements; j++)
      targ_longlong[j] = (long long)(my_pe+j);
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
#ifndef OPENSHMEM
    shmemx_longlong_put_nb(targ_longlong,srce_longlong,max_elements,my_pe+1,NULL);
#else
    shmem_longlong_put_nbi(targ_longlong,srce_longlong,max_elements,my_pe+1);
#endif
    shmem_quiet();
    shmem_int_put(flag,one,(size_t)1,my_pe+1);
  } else {
    shmem_int_wait(flag,0);
    for(j = 0; j < max_elements; j++)
      if ( targ_longlong[j] != (long long)(my_pe+j-1) )
        fprintf(stderr, "FAIL: PE [%d] targ_longlong[%d]=%d my_pe+j-1=%d\n",
                               my_pe,j,targ_longlong[j],my_pe+j-1);
  }
  shmem_free(srce_longlong);  shmem_free(targ_longlong);

/*  shmem_float_put_nb test   */
  *flag = 0;
  max_elements = (size_t) (MAX_SIZE / sizeof(float));
  max_elements_bytes = (size_t) (sizeof(float)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_float_put_nb      max_elements = %d\n",max_elements);
  srce_float = shmem_malloc(max_elements_bytes);
  targ_float = shmem_malloc(max_elements_bytes);
  if((srce_float == NULL) || (targ_float == NULL))
    shmalloc_error();
  if ( (my_pe % 2) == 0 )
    for(j = 0; j < max_elements; j++)
      srce_float[j] = (float)(my_pe+j);
  else
    for(j = 0; j < max_elements; j++)
      targ_float[j] = (float)(my_pe+j);
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
#ifndef OPENSHMEM
    shmemx_float_put_nb(targ_float,srce_float,max_elements,my_pe+1,NULL);
#else
    shmem_float_put_nbi(targ_float,srce_float,max_elements,my_pe+1);
#endif
    shmem_quiet();
    shmem_int_put(flag,one,(size_t)1,my_pe+1);
  } else {
    shmem_int_wait(flag,0);
    for(j = 0; j < max_elements; j++)
      if ( targ_float[j] != (float)(my_pe+j-1) )
        fprintf(stderr, "FAIL: PE [%d] targ_float[%d]=%d my_pe+j-1=%d\n",
                               my_pe,j,targ_float[j],my_pe+j-1);
  }
  shmem_free(srce_float);  shmem_free(targ_float);

/*  shmem_double_put_nb test   */
  *flag = 0;
  max_elements = (size_t) (MAX_SIZE / sizeof(double));
  max_elements_bytes = (size_t) (sizeof(double)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_double_put_nb     max_elements = %d\n",max_elements);
  srce_double = shmem_malloc(max_elements_bytes);
  targ_double = shmem_malloc(max_elements_bytes);
  if((srce_double == NULL) || (targ_double == NULL))
    shmalloc_error();
  if ( (my_pe % 2) == 0 )
    for(j = 0; j < max_elements; j++)
      srce_double[j] = (double)(my_pe+j);
  else
    for(j = 0; j < max_elements; j++)
      targ_double[j] = (double)(my_pe+j);
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
#ifndef OPENSHMEM
    shmemx_double_put_nb(targ_double,srce_double,max_elements,my_pe+1,NULL);
#else
    shmem_double_put_nbi(targ_double,srce_double,max_elements,my_pe+1);
#endif
    shmem_quiet();
    shmem_int_put(flag,one,(size_t)1,my_pe+1);
  } else {
    shmem_int_wait(flag,0);
    for(j = 0; j < max_elements; j++)
      if ( targ_double[j] != (double)(my_pe+j-1) )
        fprintf(stderr, "FAIL: PE [%d] targ_double[%d]=%d my_pe+j-1=%d\n",
                               my_pe,j,targ_double[j],my_pe+j-1);
  }
  shmem_free(srce_double);  shmem_free(targ_double);

/*  shmem_longdouble_put_nb test   */
#ifdef HAVE_LONG_DOUBLE
  *flag = 0;
  max_elements = (size_t) (MAX_SIZE / sizeof(long double));
  max_elements_bytes = (size_t) (sizeof(long double)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_longdouble_put_nb max_elements = %d\n",max_elements);
  srce_longdouble = shmem_malloc(max_elements_bytes);
  targ_longdouble = shmem_malloc(max_elements_bytes);
  if((srce_longdouble == NULL) || (targ_longdouble == NULL))
    shmalloc_error();
  if ( (my_pe % 2) == 0 )
    for(j = 0; j < max_elements; j++)
      srce_longdouble[j] = (long double)(my_pe+j);
  else
    for(j = 0; j < max_elements; j++)
      targ_longdouble[j] = (long double)(my_pe+j);
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    shmem_longdouble_put_nb(targ_longdouble,srce_longdouble,max_elements,my_pe+1,NULL);
    shmem_quiet();
    shmem_int_put(flag,one,(size_t)1,my_pe+1);
  } else {
    shmem_int_wait(flag,0);
    for(j = 0; j < max_elements; j++)
      if ( targ_longdouble[j] != (long double)(my_pe+j-1) )
        fprintf(stderr, "FAIL: PE [%d] targ_longdouble[%d]=%d my_pe+j-1=%d\n",
                               my_pe,j,targ_longdouble[j],my_pe+j-1);
  }
  shmem_free(srce_longdouble);  shmem_free(targ_longdouble);
#endif

#ifdef NEEDS_FINALIZE
  shmem_finalize(); 
#endif
  return 0;
}
