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
! Purpose:  Functional tests for the following shmem_finc routines ---
!           shmem_short_finc
!           shmem_int_finc
!           shmem_long_finc
!           shmem_longlong_finc
!           shmem_finc
!
! Note:  compile with -DSHMEM_GENERIC_32 to run 
!          shmem_finc (GENERIC 32) instead of shmem_finc (GENERIC 64)
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

short count_short;
int count_int;
long count_long;
long long count_longlong;

int main(int argc, char **argv)
{
  int i,j;
  short    modjs, oldjs, oldxmodjs;
  int      modji, oldji, oldxmodji;
  long     modjl, oldjl, oldxmodjl;
  long long modjll,oldjll,oldxmodjll;
  int my_pe,n_pes;
  size_t max_elements,max_elements_bytes;
  static short *xs;
  static int   *xi;
  static long  *xl;
  static long long *xll;

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
    fprintf(stderr, "shmem_finc(%s) n_pes=%d\n", argv[0],n_pes);

/*  test shmem_short_finc  */
#ifdef HAVE_SHORT

  /*  shmalloc xs on all pes (only use the one on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(short) * n_pes);
  xs = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xs[i] = 0;
  count_short = 0;
  shmem_barrier_all();

  for(i=0; i<ITER; i++) {
    if (my_pe != 0) {
      oldjs = shmem_short_finc(&count_short, 0);  /* get index oldjs from PE 0 */
      modjs = (oldjs % (n_pes-1));  /* PE 0 is just the counter/checker */
        /* increment value in xs[modjs] */
      oldxmodjs = shmem_short_finc(&xs[modjs], 0);
      /* printf("PE=%d,oldjs=%d,modjs=%d,oldxmodjs=%d\n",
                 my_pe,oldjs,modjs,oldxmodjs); */
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {         /* check xs[j] array on PE 0 */
    for(j=1 ; j<n_pes; j++) {
      if (xs[j-1] != ITER)
        fprintf(stderr, "FAIL PE %d of %d: xs[%d] = %d expected = %d\n",
                         my_pe, n_pes, j-1, xs[j-1], ITER);
    }
  }
  shmem_free(xs);

#endif

/*  test shmem_int_finc  */

  /*  shmalloc xi on all pes (only use the one on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(int) * n_pes);
  xi = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xi[i] = 0;
  count_int = 0;
  shmem_barrier_all();

  for(i=0; i<ITER; i++) {
    if (my_pe != 0) {
      oldji = shmem_int_finc(&count_int, 0);  /* get index oldji from PE 0 */
      modji = (oldji % (n_pes-1));  /* PE 0 is just the counter/checker */
        /* increment value in xi[modji] */
      oldxmodji = shmem_int_finc(&xi[modji], 0);
      /* printf("PE=%d,oldji=%d,modji=%d,oldxmodji=%d\n",
                 my_pe,oldji,modji,oldxmodji); */
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {         /* check xi[j] array on PE 0 */
    for(j=1 ; j<n_pes; j++) {
      if (xi[j-1] != ITER)
        fprintf(stderr, "FAIL PE %d of %d: xi[%d] = %d expected = %d\n",
                         my_pe, n_pes, j-1, xi[j-1], ITER);
    }
  }
  shmem_free(xi);

/*  test shmem_long_finc  */

  /*  shmalloc xl on all pes (only use the one on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(long) * n_pes);
  xl = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xl[i] = 0;
  count_long = 0;
  shmem_barrier_all();

  for(i=0; i<ITER; i++) {
    if (my_pe != 0) {
      oldjl = shmem_long_finc(&count_long, 0);  /* get index oldjl from PE 0 */
      modjl = (oldjl % (n_pes-1));  /* PE 0 is just the counter/checker */
        /* increment value in xl[modjl] */
      oldxmodjl = shmem_long_finc(&xl[modjl], 0);
      /* printf("PE=%d,oldjl=%ld,modjl=%ld,oldxmodjl=%ld\n",
                 my_pe,oldjl,modjl,oldxmodjl); */
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {         /* check xl[j] array on PE 0 */
    for(j=1 ; j<n_pes; j++) {
      if (xl[j-1] != ITER)
        fprintf(stderr, "FAIL PE %d of %d: xl[%d] = %ld expected = %ld\n",
                         my_pe, n_pes, j-1, xl[j-1], ITER);
    }
  }
  shmem_free(xl);

/*  test shmem_longlong_finc  */

#ifdef HAVE_LONG_LONG

  /*  shmalloc xll on all pes (only use the one on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(long long) * n_pes);
  xll = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xll[i] = 0;
  count_longlong = 0;
  shmem_barrier_all();

  for(i=0; i<ITER; i++) {
    if (my_pe != 0) {
      oldjll = shmem_longlong_finc(&count_longlong, 0);  /* get index oldjll from PE 0 */
      modjll = (oldjll % (n_pes-1));  /* PE 0 is just the counter/checker */
        /* increment value in xll[modjll] */
      oldxmodjll = shmem_longlong_finc(&xll[modjll], 0); 
      /* printf("PE=%d,oldjll=%ld,modjll=%ld,oldxmodjll=%ld\n",
                 my_pe,oldjll,modjll,oldxmodjll); */
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {         /* check xll[j] array on PE 0 */
    for(j=1 ; j<n_pes; j++) {
      if (xll[j-1] != ITER)
        fprintf(stderr, "FAIL PE %d of %d: xll[%d] = %ld expected = %ld\n", 
                         my_pe, n_pes, j-1, xll[j-1], ITER);
    }
  }
  shmem_free(xll);

#endif

#ifdef SHMEM_C_GENERIC_32

/*  test shmem_finc (GENERIC 32)  */

  /*  shmalloc xi on all pes (only use the one on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(int) * n_pes);
  xi = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xi[i] = 0;
  count_int = 0;
  shmem_barrier_all();

  for(i=0; i<ITER; i++) {
    if (my_pe != 0) {
      oldji = shmem_finc(&count_int, 0);  /* get index oldji from PE 0 */
      modji = (oldji % (n_pes-1));  /* PE 0 is just the counter/checker */
        /* increment value in xi[modji] */
      oldxmodji = shmem_finc(&xi[modji], 0);
      /* printf("PE=%d,oldji=%d,modji=%d,oldxmodji=%d\n",
                 my_pe,oldji,modji,oldxmodji); */
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {         /* check xi[j] array on PE 0 */
    for(j=1 ; j<n_pes; j++) {
      if (xi[j-1] != ITER)
        fprintf(stderr, "FAIL pe %d of %d: xi[%d] = %d expected = %d\n",
                         my_pe, n_pes, j-1, xi[j-1], ITER);
    }
  }
  shmem_free(xi);

#else

/*  test shmem_finc (GENERIC 64)  */

  /*  shmalloc xl on all pes (only use the one on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(long) * n_pes);
  xl = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xl[i] = 0;
  count_long = 0;
  shmem_barrier_all();

  for(i=0; i<ITER; i++) {
    if (my_pe != 0) {
#if (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L)
      oldjl = shmem_finc(&count_long, 0);  /* get index oldjl from PE 0 */
#else
      oldjl = shmem_long_finc(&count_long, 0);  /* get index oldjl from PE 0 */
#endif
      modjl = (oldjl % (n_pes-1));  /* PE 0 is just the counter/checker */
        /* increment value in xl[modjl] */
#if (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L)
      oldxmodjl = shmem_finc(&xl[modjl], 0);
#else
      oldxmodjl = shmem_long_finc(&xl[modjl], 0);
#endif
      /* printf("PE=%d,oldjl=%ld,modjl=%ld,oldxmodjl=%ld\n",
                 my_pe,oldjl,modjl,oldxmodjl); */
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {         /* check xl[j] array on PE 0 */
    for(j=1 ; j<n_pes; j++) {
      if (xl[j-1] != ITER)
        fprintf(stderr, "FAIL pe %d of %d: xl[%d] = %ld expected = %ld\n",
                         my_pe, n_pes, j-1, xl[j-1], ITER);
    }
  }
  shmem_free(xl);

#endif

  shmem_barrier_all();
#ifdef NEEDS_FINALIZE
  shmem_finalize(); 
#endif
  return 0;
}
