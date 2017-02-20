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
! Purpose:  Functional tests for the following shmem routines ---
!           shmem_short_finc
!           shmem_int_finc
!           shmem_long_finc
!           shmem_longlong_finc
!           shmem_finc
!           shmem_short_swap
!           shmem_int_swap
!           shmem_long_swap
!           shmem_longlong_swap
!           shmem_swap
!           shmem_short_cswap
!           shmem_int_cswap
!           shmem_long_cswap
!           shmem_longlong_cswap
!           shmem_cswap
!
! Note:  compile with -DSHMEM_GENERIC_32 to run 
!          shmem_finc (GENERIC 32) instead of shmem_finc (GENERIC 64)
!          shmem_swap (GENERIC 32) instead of shmem_swap (GENERIC 64)
!          shmem_cswap (GENERIC 32) instead of shmem_cswap (GENERIC 64)
!
!*********************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <mpp/shmem.h>

#ifdef QUICK_TEST
  #define ITER     50  /* ITER must be even number */
#else
  #define ITER     10  /* ITER must be even number */
#endif

short count_short;
int count_int;
long count_long;
long long count_longlong;

int main(int argc, char **argv)
{
  int i,j;
  short     modjs, oldjs, oldxmodjs, oldxas, my_pes, vals;
  int       modji, oldji, oldxmodji, oldxai, my_pei, vali;
  long      modjl, oldjl, oldxmodjl, oldxal, my_pel, vall;
  long long modjll,oldjll,oldxmodjll,oldxall,my_pell,valll;
  int my_pe,n_pes;
  size_t max_elements,max_elements_bytes;
  static short *xs,*xas;
  static int   *xi,*xai;
  static long  *xl,*xal;
  static long long *xll,*xall;

  shmem_init();
  my_pe = shmem_my_pe();
  n_pes = shmem_n_pes();
  my_pes = (short) my_pe;
  my_pei = (int)  my_pe;
  my_pel = (long) my_pe;
  my_pell = (long long) my_pe;
  vals = 1;  vali = 1;  vall = 1;  valll = 1;
#ifdef HAVE_SET_CACHE_INV
  shmem_set_cache_inv();
#endif

/*  fail if trying to use only one processor  */
  if ( n_pes  <= 1 ){
        fprintf(stderr, "FAIL - test requires at least two PEs\n");
        exit(1);
  }

  if(my_pe == 0)
    fprintf(stderr, "shmem_cswap(%s) n_pes=%d\n", argv[0],n_pes);

/*  test shmem_short_finc & shmem_short_swap & shmem_short_cswap */
#ifdef HAVE_SHORT

  /*  shmalloc xs & xas on all pes (only use the ones on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(short) * n_pes);
  xs = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xs[i] = 0;
  max_elements_bytes = (size_t) (sizeof(short) * n_pes * ITER);
  xas = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes*ITER; i++)
    if (((i/(n_pes-1)) % 2) == 0) {
      xas[i] = 1;
    } else {
      xas[i] = 0;
    }
  count_short = 0;
  shmem_barrier_all();

  for(i=0; i<ITER; i++) {
    if (i == ITER-1) shmem_barrier_all();  /* all PEs participate last time */
    if (my_pe != 0) {
      oldjs = shmem_short_finc(&count_short, 0);  /* get index oldjs from PE 0 */
      modjs = (oldjs % (n_pes-1));  /* PE 0 is just the counter/checker */
        /* conditionally record PE value in xas[oldjs] --
             tells PE involved for each count */
      oldxas = shmem_short_cswap(&xas[oldjs], vals, my_pes, 0);
      /* printf("PE=%d,i=%d,oldjs=%d,oldxas=%d\n",my_pe,i,oldjs,oldxas); */
      if (oldxas == 1) {
          /* record PE value in xs[modjs] */
        oldxmodjs = shmem_short_swap(&xs[modjs], my_pes, 0); 
        /* printf("PE=%d,oldjs=%d,modjs=%d,oldxmodjs=%d\n",
                   my_pe,oldjs,modjs,oldxmodjs); */
      }
      if (oldxas != 0 && oldxas != 1)
        fprintf(stderr, "FAIL PE %d of %d: i=%d, oldxas = %d expected = 0\n",
                         my_pe, n_pes, i, oldxas);
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {  /* check last xs[j] array PEs vs saved ans in xas[i] */
    i = (ITER-2)*(n_pes-1);
    for(j=1 ; j<n_pes; j++) {
      /* printf("j=%d,xs[%d]=%d,xas[%d]=%d\n",j,j-1,xs[j-1],i,xas[i]); */
      if (xs[j-1] != xas[i])
        fprintf(stderr, "FAIL PE %d of %d: xs[%d] = %d expected = %d\n", 
                         my_pe, n_pes, j-1, xs[j-1], xas[i]);
      i++;
    }
  }
  shmem_free(xs);  shmem_free(xas);

#endif

/*  test shmem_int_finc & shmem_int_swap & shmem_int_cswap */

  /*  shmalloc xi & xai on all pes (only use the ones on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(int) * n_pes);
  xi = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xi[i] = 0;
  max_elements_bytes = (size_t) (sizeof(int) * n_pes * ITER);
  xai = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes*ITER; i++)
    if (((i/(n_pes-1)) % 2) == 0) {
      xai[i] = 1;
    } else {
      xai[i] = 0;
    }
  count_int = 0;
  shmem_barrier_all();

  for(i=0; i<ITER; i++) {
    if (i == ITER-1) shmem_barrier_all();  /* all PEs participate last time */
    if (my_pe != 0) {
      oldji = shmem_int_finc(&count_int, 0);  /* get index oldji from PE 0 */
      modji = (oldji % (n_pes-1));  /* PE 0 is just the counter/checker */
        /* conditionally record PE value in xai[oldji] --
             tells PE involved for each count */
      oldxai = shmem_int_cswap(&xai[oldji], vali, my_pei, 0);
      /* printf("PE=%d,i=%d,oldji=%d,oldxai=%d\n",my_pe,i,oldji,oldxai); */
      if (oldxai == 1) {
          /* record PE value in xi[modji] */
        oldxmodji = shmem_int_swap(&xi[modji], my_pei, 0);
        /* printf("PE=%d,oldji=%d,modji=%d,oldxmodji=%d\n",
                   my_pe,oldji,modji,oldxmodji); */
      }
      if (oldxai != 0 && oldxai != 1)
        fprintf(stderr, "FAIL PE %d of %d: i=%d, oldxai = %d expected = 0\n",
                         my_pe, n_pes, i, oldxai);
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {  /* check last xi[j] array PEs vs saved ans in xai[i] */
    i = (ITER-2)*(n_pes-1);
    for(j=1 ; j<n_pes; j++) {
      /* printf("j=%d,xi[%d]=%d,xai[%d]=%d\n",j,j-1,xi[j-1],i,xai[i]); */
      if (xi[j-1] != xai[i])
        fprintf(stderr, "FAIL PE %d of %d: xi[%d] = %d expected = %d\n",
                         my_pe, n_pes, j-1, xi[j-1], xai[i]);
      i++;
    }
  }
  shmem_free(xi);  shmem_free(xai);

/*  test shmem_long_finc & shmem_long_swap & shmem_long_cswap */

  /*  shmalloc xl & xal on all pes (only use the ones on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(long) * n_pes);
  xl = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xl[i] = 0;
  max_elements_bytes = (size_t) (sizeof(long) * n_pes * ITER);
  xal = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes*ITER; i++)
    if (((i/(n_pes-1)) % 2) == 0) {
      xal[i] = 1;
    } else {
      xal[i] = 0;
    }
  count_long = 0;
  shmem_barrier_all();

  for(i=0; i<ITER; i++) {
    if (i == ITER-1) shmem_barrier_all();  /* all PEs participate last time */
    if (my_pe != 0) {
      oldjl = shmem_long_finc(&count_long, 0);  /* get index oldjl from PE 0 */
      modjl = (oldjl % (n_pes-1));  /* PE 0 is just the counter/checker */
        /* conditionally record PE value in xal[oldjl] --
             tells PE involved for each count */
      oldxal = shmem_long_cswap(&xal[oldjl], vall, my_pel, 0);
      /* printf("PE=%d,i=%d,oldjl=%d,oldxal=%d\n",my_pe,i,oldjl,oldxal); */
      if (oldxal == 1) {
          /* record PE value in xl[modjl] */
      oldxmodjl = shmem_long_swap(&xl[modjl], my_pel, 0);
      /* printf("PE=%d,oldjl=%ld,modjl=%ld,oldxmodjl=%ld\n",
                 my_pe,oldjl,modjl,oldxmodjl); */
      }
      if (oldxal != 0 && oldxal != 1)
        fprintf(stderr, "FAIL PE %d of %d: i=%d, oldxal = %ld expected = 0\n",
                         my_pe, n_pes, i, oldxal);
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {  /* check last xl[j] array PEs vs saved ans in xal[i] */
    i = (ITER-2)*(n_pes-1);
    for(j=1 ; j<n_pes; j++) {
      /* printf("j=%d,xl[%d]=%ld,xal[%d]=%ld\n",j,j-1,xl[j-1],i,xal[i]); */
      if (xl[j-1] != xal[i])
        fprintf(stderr, "FAIL PE %d of %d: xl[%d] = %ld expected = %ld\n",
                         my_pe, n_pes, j-1, xl[j-1], xal[i]);
      i++;
    }
  }
  shmem_free(xl);  shmem_free(xal);

/*  test shmem_longlong_finc & shmem_longlong_swap & shmem_longlong_cswap */

#ifdef HAVE_LONG_LONG

  /*  shmalloc xll & xall on all pes (only use the ones on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(long long) * n_pes);
  xll = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xll[i] = 0;
  max_elements_bytes = (size_t) (sizeof(long long) * n_pes * ITER);
  xall = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes*ITER; i++)
    if (((i/(n_pes-1)) % 2) == 0) {
      xall[i] = 1;
    } else {
      xall[i] = 0;
    }
  count_longlong = 0;
  shmem_barrier_all();

  for(i=0; i<ITER; i++) {
    if (i == ITER-1) shmem_barrier_all();  /* all PEs participate last time */
    if (my_pe != 0) {
      oldjll = shmem_longlong_finc(&count_longlong, 0);  /* get index oldjll from PE 0 */
      modjll = (oldjll % (n_pes-1));  /* PE 0 is just the counter/checker */
        /* conditionally record PE value in xall[oldjll] --
             tells PE involved for each count */
      oldxall = shmem_longlong_cswap(&xall[oldjll], valll, my_pell, 0);
      /* printf("PE=%d,i=%d,oldjll=%d,oldxall=%d\n",my_pe,i,oldjll,oldxall); */
      if (oldxall == 1) {
          /* record PE value in xll[modjll] */
        oldxmodjll = shmem_longlong_swap(&xll[modjll], my_pell, 0);
        /* printf("PE=%d,oldjll=%ld,modjll=%ld,oldxmodjll=%ld\n",
                   my_pe,oldjll,modjll,oldxmodjll); */
      }
      if (oldxall != 0 && oldxall != 1)
        fprintf(stderr, "FAIL PE %d of %d: i=%d, oldxall = %ld expected = 0\n",
                         my_pe, n_pes, i, oldxall);
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {  /* check last xll[j] array PEs vs saved ans in xall[i] */
    i = (ITER-2)*(n_pes-1);
    for(j=1 ; j<n_pes; j++) {
      /* printf("j=%d,xll[%d]=%ld,xall[%d]=%ld\n",j,j-1,xll[j-1],i,xall[i]); */
      if (xll[j-1] != xall[i])
        fprintf(stderr, "FAIL PE %d of %d: xll[%d] = %ld expected = %ld\n",
                         my_pe, n_pes, j-1, xll[j-1], xall[i]);
      i++;
    }
  }
  shmem_free(xll);  shmem_free(xall);

#endif

#ifdef SHMEM_C_GENERIC_32

/*  test shmem_finc & shmem_swap & shmem_cswap (GENERIC 32)  */

  /*  shmalloc xi & xai on all pes (only use the ones on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(int) * n_pes);
  xi = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xi[i] = 0;
  max_elements_bytes = (size_t) (sizeof(int) * n_pes * ITER);
  xai = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes*ITER; i++)
    if (((i/(n_pes-1)) % 2) == 0) {
      xai[i] = 1;
    } else {
      xai[i] = 0;
    }
  count_int = 0;
  shmem_barrier_all();

  for(i=0; i<ITER; i++) {
    if (i == ITER-1) shmem_barrier_all();  /* all PEs participate last time */
    if (my_pe != 0) {
      oldji = shmem_finc(&count_int, 0);  /* get index oldji from PE 0 */
      modji = (oldji % (n_pes-1));  /* PE 0 is just the counter/checker */
        /* conditionally record PE value in xai[oldji] --
             tells PE involved for each count */
      oldxai = shmem_cswap(&xai[oldji], vali, my_pei, 0);
      /* printf("PE=%d,i=%d,oldji=%d,oldxai=%d\n",my_pe,i,oldji,oldxai); */
      if (oldxai == 1) {
            /* record PE value in xi[modji] */
        oldxmodji = shmem_swap(&xi[modji], my_pei, 0);
        /* printf("PE=%d,oldji=%d,modji=%d,oldxmodji=%d\n",
                   my_pe,oldji,modji,oldxmodji); */
      }
      if (oldxai != 0 && oldxai != 1)
        fprintf(stderr, "FAIL pe %d of %d: i=%d, oldxai = %d expected = 0\n",
                         my_pe, n_pes, i, oldxai);
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {  /* check last xi[j] array PEs vs saved ans in xai[i] */
    i = (ITER-2)*(n_pes-1);
    for(j=1 ; j<n_pes; j++) {
      /* printf("j=%d,xi[%d]=%d,xai[%d]=%d\n",j,j-1,xi[j-1],i,xai[i]); */
      if (xi[j-1] != xai[i])
        fprintf(stderr, "FAIL pe %d of %d: xi[%d] = %d expected = %d\n",
                         my_pe, n_pes, j-1, xi[j-1], xai[i]);
      i++;
    }
  }
  shmem_free(xi);  shmem_free(xai);

#else

/*  test shmem_finc & shmem_swap & shmem_cswap (GENERIC 64)  */

  /*  shmalloc xl & xal on all pes (only use the ones on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(long) * n_pes);
  xl = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xl[i] = 0;
  max_elements_bytes = (size_t) (sizeof(long) * n_pes * ITER);
  xal = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes*ITER; i++)
    if (((i/(n_pes-1)) % 2) == 0) {
      xal[i] = 1;
    } else {
      xal[i] = 0;
    }
  count_long = 0;
  shmem_barrier_all();

  for(i=0; i<ITER; i++) {
    if (i == ITER-1) shmem_barrier_all();  /* all PEs participate last time */
    if (my_pe != 0) {
#if (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L)
      oldjl = shmem_finc(&count_long, 0);  /* get index oldjl from PE 0 */
#else
      oldjl = shmem_long_finc(&count_long, 0);  /* get index oldjl from PE 0 */
#endif
      modjl = (oldjl % (n_pes-1));  /* PE 0 is just the counter/checker */
        /* conditionally record PE value in xal[oldjl] --
             tells PE involved for each count */
#if (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L)
      oldxal = shmem_cswap(&xal[oldjl], vall, my_pell, 0);
#else
      oldxal = shmem_long_cswap(&xal[oldjl], vall, my_pell, 0);
#endif
      /* printf("PE=%d,i=%d,oldjl=%d,oldxal=%d\n",my_pe,i,oldjl,oldxal); */
      if (oldxal == 1) {
            /* record PE value in xl[modjl] */
#if (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L)
        oldxmodjl = shmem_swap(&xl[modjl], my_pell, 0);
#else
        oldxmodjl = shmem_long_swap(&xl[modjl], my_pell, 0);
#endif
        /* printf("PE=%d,oldjl=%ld,modjl=%ld,oldxmodjl=%ld\n",
                   my_pe,oldjl,modjl,oldxmodjl); */
      }
      if (oldxal != 0 && oldxal != 1)
        fprintf(stderr, "FAIL pe %d of %d: i=%d, oldxal = %ld expected = 0\n",
                         my_pe, n_pes, i, oldxal);
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {  /* check last xl[j] array PEs vs saved ans in xal[i] */
    i = (ITER-2)*(n_pes-1);
    for(j=1 ; j<n_pes; j++) {
      /* printf("j=%d,xl[%d]=%ld,xal[%d]=%ld\n",j,j-1,xl[j-1],i,xal[i]); */
      if (xl[j-1] != xal[i])
        fprintf(stderr, "FAIL pe %d of %d: xl[%d] = %ld expected = %10.2f\n",
                         my_pe, n_pes, j-1, xl[j-1], xal[i]);
      i++;
    }
  }
  shmem_free(xl);  shmem_free(xal);

#endif

  shmem_barrier_all();
#ifdef NEEDS_FINALIZE
  shmem_finalize(); 
#endif
  return 0;
}
