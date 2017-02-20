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
!           shmem_put32
!
!*********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <mpp/shmem.h>

#define MAX_SIZE 1024
#ifdef QUICK_TEST
  #define MAX_ITER 10
#else
  #define MAX_ITER 3
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
  size_t elements[11] = {1,2,4,8,12,16,24,32,64,128,256};
  int num_elements = 11;

  char *srce_char,*targ_char;
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
    fprintf(stderr, "shmem_put32(%s)\n", argv[0]);

/*  shmem_put32 test   */
  max_elements = (size_t) (MAX_SIZE / sizeof(int));
  max_elements_bytes = (size_t) (sizeof(int)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_put32 max_elements = %d\n",max_elements);
  srce_int = shmem_malloc(max_elements_bytes);
  targ_int = shmem_malloc(max_elements_bytes);
  if((srce_int == NULL) || (targ_int == NULL))
    shmalloc_error();
  /* try the different sizes MAX_ITER times */
  for (iter = 0; iter < MAX_ITER; iter++) {
   for (i = 0; i < num_elements; i++) {
    *flag = 0;
    if (elements[i] <= max_elements) {
     if (iter==0) 
       if(my_pe == 0) 
         fprintf(stderr,"shmem_put32     elements = %d\n",elements[i]);
     if ( (my_pe % 2) == 0 )
       for(j = 0; j < elements[i]; j++)
         srce_int[j] = (int)(iter*10000+elements[i]*100+my_pe+j);
     else
       for(j = 0; j < elements[i]; j++)
         targ_int[j] = (int)(iter*10000+elements[i]*100+my_pe+j);
     shmem_barrier_all();
     if ( (my_pe % 2) == 0 ) {
       shmem_put32(targ_int,srce_int,elements[i],my_pe+1);
       shmem_quiet();
       shmem_int_put(flag,one,(size_t)1,my_pe+1);
     } else {
       shmem_int_wait(flag,0);
       for(j = 0; j < elements[i]; j++)
         if ( targ_int[j] != (int)(iter*10000+elements[i]*100+my_pe+j-1) )
           fprintf(stderr, 
           "FAIL: PE [%d] iter=%d i=%d targ_int[%d]=%d not equal %d\n",
              my_pe,iter,i,j,targ_int[j],iter*10000+elements[i]*100+my_pe+j-1);
     }
    }
   }
  }
  shmem_free(srce_int);  shmem_free(targ_int);

#ifdef NEEDS_FINALIZE
  shmem_finalize(); 
#endif
  return 0;
}
