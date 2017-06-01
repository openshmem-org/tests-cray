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
! Purpose:  Functional tests for the following shmem_broadcast routines ---
!           shmem_broadcast32
!           shmem_broadcast64
!           shmem_broadcast (GENERIC 32)
!           shmem_broadcast (GENERIC 64)
!
! Note:  compile with -DSHMEM_GENERIC_32 to run 
!        shmem_broadcast (GENERIC 32) instead of shmem_broadcast (GENERIC 64)
!
!*********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <mpp/shmem.h>
#include <mpp/shmemx.h>

#define MAX_SIZE 40
#define IMAX 5

void
shmalloc_error(void)
{
  fprintf(stderr, "FAIL: not enough memory available\n");
  exit(1);
}

long pSync1[_SHMEM_BCAST_SYNC_SIZE], pSync2[_SHMEM_BCAST_SYNC_SIZE];

int main(int argc, char **argv)
{
  int i,j;
  int my_pe,n_pes,PE_root;
  size_t max_elements,max_elements_bytes;

  int *srce_int,*targ_int,ans_int;
  long *srce_long,*targ_long,ans_long;
  float *srce_float,*targ_float,ans_float;
  double *srce_double,*targ_double,ans_double;

  shmem_init();
  my_pe = shmem_my_pe();
  n_pes = shmem_n_pes();

/*  fail if trying to use only one processor  */
  if ( n_pes  <= 1 ){
        fprintf(stderr, "FAIL - test requires at least two PEs\n");
        exit(1);
  }

  if(my_pe == 0)
    fprintf(stderr, "shmem_broadcast(%s) n_pes=%d\n", argv[0],n_pes);
  /* initialize the pSync arrays */
  for (i=0; i < _SHMEM_BCAST_SYNC_SIZE; i++) {
    pSync1[i] = _SHMEM_SYNC_VALUE;
    pSync2[i] = _SHMEM_SYNC_VALUE;
  }
  shmem_barrier_all();  /* Wait for all PEs to initialize pSync1 & pSync2 */
  PE_root=1;  /* we'll broadcast from this PE */

/*  shmem_broadcast32 test   */
  max_elements = (size_t) (MAX_SIZE / sizeof(int));
  max_elements_bytes = (size_t) (sizeof(int)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_broadcast32             max_elements = %d\n",
                                                  max_elements);
  srce_int = shmem_malloc(max_elements_bytes);
  targ_int = shmem_malloc(max_elements_bytes);
  srce_float = shmem_malloc(max_elements_bytes);
  targ_float = shmem_malloc(max_elements_bytes);
  if((srce_int == NULL) || (targ_int == NULL) ||
     (srce_float == NULL) || (targ_float == NULL))
     shmalloc_error();
  for(j = 0; j < max_elements; j++) {
    srce_int[j] = (int)(my_pe+j);
    srce_float[j] = (float)(my_pe+j);
    targ_int[j] = (int)(100*my_pe+j);
    targ_float[j] = (float)(100*my_pe+j);
  }
  shmem_barrier_all();
  for(i = 0; i < IMAX; i+=2) {
    /* i is even -- using int */
    if (my_pe == PE_root)
      for(j = 0; j < max_elements; j++) {
        srce_int[j] = (int)(my_pe+i+j);
      }
    /* broadcast from PE_root to all PEs using pSync1 */
    shmem_broadcast32(targ_int,srce_int,max_elements,PE_root,0,0,n_pes,pSync1);
    for(j = 0; j < max_elements; j++) {
      if (my_pe == PE_root) {
        ans_int= (int)(100*my_pe+j);
      } else {
        ans_int= (int)(PE_root+i+j);
      }
      if ( targ_int[j] != ans_int )
	fprintf(stderr, "FAIL: PE [%d] targ_int[%d]=%d ans_int=%d\n",
                               my_pe,j,targ_int[j],ans_int);
    }
    /* i+1 is odd -- using float */
    if (my_pe == PE_root)
      for(j = 0; j < max_elements; j++) {
        srce_float[j] = (float)(PE_root+i+1+j);
      }
    /* broadcast from PE_root to all PEs using pSync2 */
    shmem_broadcast32(targ_float,srce_float,max_elements,PE_root,0,0,n_pes,pSync2);
    for(j = 0; j < max_elements; j++) {
      if (my_pe == PE_root) {
        ans_float= (float)(100*my_pe+j);
      } else {
        ans_float= (float)(PE_root+i+1+j);
      }
      if ( targ_float[j] != ans_float )
        fprintf(stderr, "FAIL: PE [%d] targ_float[%d]=%10.0f ans_float=%10.0f\n",
                               my_pe,j,targ_float[j],ans_float);
    }
  }
  shmem_free(srce_int);    shmem_free(targ_int);
  shmem_free(srce_float);  shmem_free(targ_float);
  
/*  shmem_broadcast64 test   */
  max_elements = (size_t) (MAX_SIZE / sizeof(long));
  max_elements_bytes = (size_t) (sizeof(long)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_broadcast64             max_elements = %d\n",
                                                  max_elements);
  srce_long = shmem_malloc(max_elements_bytes);
  targ_long = shmem_malloc(max_elements_bytes);
  srce_double = shmem_malloc(max_elements_bytes);
  targ_double = shmem_malloc(max_elements_bytes);
  if((srce_long == NULL) || (targ_long == NULL) ||
     (srce_double == NULL) || (targ_double == NULL))
     shmalloc_error();
  for(j = 0; j < max_elements; j++) {
    srce_long[j] = (long)(my_pe+j);
    srce_double[j] = (double)(my_pe+j);
    targ_long[j] = (long)(100*my_pe+j);
    targ_double[j] = (double)(100*my_pe+j);
  }
  shmem_barrier_all();
  for(i = 0; i < IMAX; i+=2) {
    /* i is even -- using long */
    if (my_pe == PE_root)
      for(j = 0; j < max_elements; j++) {
        srce_long[j] = (long)(my_pe+i+j);
      }
    /* broadcast from PE_root to all PEs using pSync1 */
    shmem_broadcast64(targ_long,srce_long,max_elements,PE_root,0,0,n_pes,pSync1);
    for(j = 0; j < max_elements; j++) {
      if (my_pe == PE_root) {
        ans_long= (long)(100*my_pe+j);
      } else {
        ans_long= (long)(PE_root+i+j);
      }
      if ( targ_long[j] != ans_long )
        fprintf(stderr, "FAIL: PE [%d] targ_long[%d]=%d ans_long=%d\n",
                               my_pe,j,targ_long[j],ans_long);
    }
    /* i+1 is odd -- using double */
    if (my_pe == PE_root)
      for(j = 0; j < max_elements; j++) {
        srce_double[j] = (double)(PE_root+i+1+j);
      }
    /* broadcast from PE_root to all PEs using pSync2 */
    shmem_broadcast64(targ_double,srce_double,max_elements,PE_root,0,0,n_pes,pSync2);
    for(j = 0; j < max_elements; j++) {
      if (my_pe == PE_root) {
        ans_double= (double)(100*my_pe+j);
      } else {
        ans_double= (double)(PE_root+i+1+j);
      }
      if ( targ_double[j] != ans_double )
        fprintf(stderr, "FAIL: PE [%d] targ_double[%d]=%10.0f ans_double=%10.0f\n",
                               my_pe,j,targ_double[j],ans_double);
    }
  }
  shmem_free(srce_long);  shmem_free(targ_long);
  shmem_free(srce_double);  shmem_free(targ_double);

#ifndef OPENSHMEM
#ifdef SHMEM_C_GENERIC_32

/*  shmemx_broadcast (GENERIC 32) test   */
  max_elements = (size_t) (MAX_SIZE / sizeof(int));
  max_elements_bytes = (size_t) (sizeof(int)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmemx_broadcast (GENERIC 32)  max_elements = %d\n",
                                                  max_elements);
  srce_int = shmem_malloc(max_elements_bytes);
  targ_int = shmem_malloc(max_elements_bytes);
  if((srce_int == NULL) || (targ_int == NULL))
    shmalloc_error();
    for(j = 0; j < max_elements; j++) {
      srce_int[j] = (int)(my_pe+j);
      targ_int[j] = (int)(2*my_pe+j);
    }
  shmem_barrier_all();
    /* broadcast from PE 1 to all PEs */
    shmemx_broadcast(targ_int,srce_int,max_elements,1,0,0,n_pes,pSync1);
    for(j = 0; j < max_elements; j++) {
      if (my_pe == 1) {
        ans_int= (int)(j+2);
      } else {
        ans_int= (int)(j+1);
      }
      if ( targ_int[j] != ans_int )
        fprintf(stderr, "FAIL: PE [%d] targ_int[%d]=%d ans_int=%d\n",
                               my_pe,j,targ_int[j],ans_int);
    }
  shmem_free(srce_int);  shmem_free(targ_int);

#else

/*  shmemx_broadcast (GENERIC 64) test   */
  max_elements = (size_t) (MAX_SIZE / sizeof(long));
  max_elements_bytes = (size_t) (sizeof(long)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmemx_broadcast (GENERIC 64)  max_elements = %d\n",
                                                  max_elements);
  srce_long = shmem_malloc(max_elements_bytes);
  targ_long = shmem_malloc(max_elements_bytes);
  if((srce_long == NULL) || (targ_long == NULL))
    shmalloc_error();
  for(j = 0; j < max_elements; j++) {
    srce_long[j] = (long)(my_pe+j);
    targ_long[j] = (long)(2*my_pe+j);
  }
  shmem_barrier_all();
    /* broadcast from PE 1 to all PEs */
    shmemx_broadcast(targ_long,srce_long,max_elements,1,0,0,n_pes,pSync1);
    for(j = 0; j < max_elements; j++) {
      if (my_pe == 1) {
        ans_long = (long)(j+2);
      } else {
        ans_long = (long)(j+1);
      }
      if ( targ_long[j] != ans_long )
        fprintf(stderr, "FAIL: PE [%d] targ_long[%d]=%d ans_long=%d\n",
                               my_pe,j,targ_long[j],ans_long);
    }
  shmem_free(srce_long);  shmem_free(targ_long);

#endif
#endif

#ifdef NEEDS_FINALIZE
  shmem_finalize(); 
#endif
  return 0;
}
