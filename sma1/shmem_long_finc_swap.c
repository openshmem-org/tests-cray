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
! Purpose:  Functional tests for the following shmem long routines ---
!           shmem_long_finc
!           shmem_long_swap
!
!*********************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <mpp/shmem.h>

#ifdef QUICK_TEST
  #define ITER     50
#else
  #define ITER     5
#endif

long count;

int main(int argc, char **argv)
{
  int i,j;
  long modj,oldj,oldxmodj,oldxa;
  int my_pe,n_pes;
  size_t max_elements,max_elements_bytes;
  static long *x,*xa;

  shmem_init();
  my_pe = shmem_my_pe();
  n_pes = shmem_n_pes();
#ifdef HAVE_SET_CACHE_INV
  shmem_set_cache_inv();
#endif

/*  fail if trying to use only one processor  */
  if ( n_pes  <= 1 ){
        fprintf(stderr, "FAIL - test requires at least two PEs\n");
        exit(1);
  }

  if(my_pe == 0)
    fprintf(stderr, "shmem_long_swap(%s) n_pes=%d\n", argv[0],n_pes);

/*  shmalloc x & xa on all pes (only use the ones on PE 0)  */

  max_elements_bytes = (size_t) (sizeof(long) * n_pes);
  x = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    x[i] = 0;
  max_elements_bytes = (size_t) (sizeof(long) * n_pes * ITER);
  xa = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes*ITER; i++)
    xa[i] = 0;
  count = 0;
  shmem_barrier_all();

  for(i=0; i<ITER; i++) {
    if (my_pe != 0) {
      oldj = shmem_long_finc(&count, 0);  /* get index oldj from PE 0 */
      modj = (oldj % (n_pes-1));  /* PE 0 is just the counter/checker */
        /* record PE value in x[modj] */
      oldxmodj = shmem_long_swap(&x[modj], my_pe, 0); 
      /* printf("PE=%d,oldj=%ld,modj=%ld,oldxmodj=%ld\n",my_pe,oldj,modj,oldxmodj); */
        /* record PE value in xa[oldj] -- tells PE involved for each count */
      oldxa = shmem_long_swap(&xa[oldj], my_pe, 0);
      /* printf("PE=%d,i=%d,oldj=%ld,oldxa=%ld\n",my_pe,i,oldj,oldxa); */
      if (oldxa != 0)
        fprintf(stderr, "FAIL PE %d of %d: i=%d, oldxa = %ld expected = 0\n",
                         my_pe, n_pes, i, oldxa);
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {  /* check last x[j] array PEs vs saved ans in xa[i] */
    i = (ITER-1)*(n_pes-1);
    for(j=1 ; j<n_pes; j++) {
      printf("j=%d,x[%d]=%ld,xa[%d]=%ld\n",j,j-1,x[j-1],i,xa[i]);
      if (x[j-1] != xa[i])
        fprintf(stderr, "FAIL PE %d of %d: x[%d] = %ld expected = %ld\n", 
                         my_pe, n_pes, j-1, x[j-1], ITER);
      i++;
    }
  }

  shmem_barrier_all();
#ifdef NEEDS_FINALIZE
  shmem_finalize(); 
#endif
  return 0;
}
