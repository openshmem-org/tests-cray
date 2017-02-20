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
!           shmem_short_swap
!           shmem_int_swap
!           shmem_long_swap
!           shmem_longlong_swap
!           shmem_float_swap
!           shmem_double_swap
!           shmem_swap
!
! Note:  compile with -DSHMEM_GENERIC_32 to run 
!          shmem_swap (GENERIC 32) instead of shmem_swap (GENERIC 64)
!
!*********************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <mpp/shmem.h>

#ifdef QUICK_TEST
  #define ITER     10
#else
  #define ITER     3
#endif

int main(int argc, char **argv)
{
  int i,j;
  short     oldjs, oldxs, my_pes;
  int       oldji, oldxi, my_pei;
  long      oldjl, oldxl, my_pel;
  long long oldjll,oldxll,my_pell;
  float     oldjf, oldxf, my_pef;
  double    oldjd, oldxd, my_ped;
  int my_pe,n_pes;
  size_t max_elements,max_elements_bytes;
  static short *xs;
  static int   *xi;
  static long  *xl;
  static long long *xll;
  static float  *xf;
  static double *xd;

  shmem_init();
  my_pe = shmem_my_pe();
  n_pes = shmem_n_pes();
  my_pes = (short) my_pe;
  my_pei = (int)  my_pe;
  my_pel = (long) my_pe;
  my_pell = (long long) my_pe;
  my_pef = (float) my_pe;
  my_ped = (double) my_pe;
#ifdef HAVE_SET_CACHE_INV
  shmem_set_cache_inv();
#endif

/*  fail if trying to use only one processor  */
  if ( n_pes  <= 1 ){
        fprintf(stderr, "FAIL - test requires at least two PEs\n");
        exit(1);
  }

  if(my_pe == 0)
    fprintf(stderr, "shmem_swap(%s) n_pes=%d\n", argv[0],n_pes);

/*  test shmem_short_swap  */
#ifdef HAVE_SHORT

  /*  shmalloc xs on all pes (only check the ones on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(short) * n_pes);
  xs = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xs[i] = 0;
  shmem_barrier_all();

  oldjs = 0;
  for(i=0; i<ITER; i++) {
    if (my_pe != 0) {
      my_pes = my_pes + (short) 1;
      /* record PE value in xs[my_pe] -- save PE number */
      oldxs = shmem_short_swap(&xs[my_pe], my_pes, 0);
      /* printf("PE=%d,i=%d,my_pes=%d,oldxs=%d\n",my_pe,i,my_pes,oldxs); */
      if (oldxs != oldjs)
        fprintf(stderr, "FAIL PE %d of %d: i=%d, oldxs = %d expected = %d\n",
                         my_pe, n_pes, i, oldxs, oldjs);
      oldjs = my_pes;
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {  /* check xs[j] array vs PE# + ITER */
    i = (int) ITER + 1;
    for(j=1 ; j<n_pes; j++) {
      /* printf("j=%d,xs[%d]=%d,i=%d\n",j,j,xs[j],i); */
      if (xs[j] != (short) i)
        fprintf(stderr, "FAIL PE %d of %d: xs[%d] = %d expected = %d\n",
                         my_pe, n_pes, j, xs[j],i);
      i++;
    }
  }
  shmem_free(xs);

#endif

/*  test shmem_int_swap  */

  /*  shmalloc xi on all pes (only check the ones on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(int) * n_pes);
  xi = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xi[i] = 0;
  shmem_barrier_all();

  oldji = 0;
  for(i=0; i<ITER; i++) {
    if (my_pe != 0) {
      my_pei = my_pei + (int) 1;
      /* record PE value in xi[my_pe] -- save PE number */
      oldxi = shmem_int_swap(&xi[my_pe], my_pei, 0);
      /* printf("PE=%d,i=%d,my_pei=%d,oldxi=%d\n",my_pe,i,my_pei,oldxi); */
      if (oldxi != oldji)
        fprintf(stderr, "FAIL PE %d of %d: i=%d, oldxi = %d expected = %d\n",
                         my_pe, n_pes, i, oldxi, oldji);
      oldji = my_pei;
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {  /* check xi[j] array vs PE# + ITER */
    i = (int) ITER + 1;
    for(j=1 ; j<n_pes; j++) {
      /* printf("j=%d,xi[%d]=%d,i=%d\n",j,j,xi[j],i); */
      if (xi[j] != i)
        fprintf(stderr, "FAIL PE %d of %d: xi[%d] = %d expected = %d\n",
                         my_pe, n_pes, j, xi[j],i);
      i++;
    }
  }
  shmem_free(xi);

/*  test shmem_long_swap  */

  /*  shmalloc xl on all pes (only check the ones on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(long) * n_pes);
  xl = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xl[i] = 0;
  shmem_barrier_all();

  oldjl = 0;
  for(i=0; i<ITER; i++) {
    if (my_pe != 0) {
      my_pel = my_pel + (long) 1;
      /* record PE value in xl[my_pe] -- save PE number */
      oldxl = shmem_long_swap(&xl[my_pe], my_pel, 0);
      /* printf("PE=%d,i=%d,my_pel=%d,oldxl=%d\n",my_pe,i,my_pel,oldxl); */
      if (oldxl != oldjl)
        fprintf(stderr, "FAIL PE %d of %d: i=%d, oldxl = %d expected = %d\n",
                         my_pe, n_pes, i, oldxl, oldjl);
      oldjl = my_pel;
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {  /* check xl[j] array vs PE# + ITER */
    i = (int) ITER + 1;
    for(j=1 ; j<n_pes; j++) {
      /* printf("j=%d,xl[%d]=%d,i=%d\n",j,j,xl[j],i); */
      if (xl[j] != (long)i)
        fprintf(stderr, "FAIL PE %d of %d: xl[%d] = %ld expected = %d\n",
                         my_pe, n_pes, j, xl[j],i);
      i++;
    }
  }
  shmem_free(xl);

/*  test shmem_longlong_swap  */

#ifdef HAVE_LONG_LONG

  /*  shmalloc xll on all pes (only check the ones on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(long long) * n_pes);
  xll = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xll[i] = 0;
  shmem_barrier_all();

  oldjll = 0;
  for(i=0; i<ITER; i++) {
    if (my_pe != 0) {
      my_pell = my_pell + (long long) 1;
      /* record PE value in xll[my_pe] -- save PE number */
      oldxll = shmem_longlong_swap(&xll[my_pe], my_pell, 0);
      /* printf("PE=%d,i=%d,my_pell=%ld,oldxll=%d\n",my_pe,i,my_pell,oldxll); */
      if (oldxll != (long long) oldjll)
        fprintf(stderr, "FAIL PE %d of %d: i=%d, oldxll = %ld expected = %ld\n",
                         my_pe, n_pes, i, oldxll, oldjll);
      oldjll = my_pell;
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {  /* check xll[j] array vs PE# + ITER */
    i = (int) ITER + 1;
    for(j=1 ; j<n_pes; j++) {
      /* printf("j=%d,xll[%d]=%ld,i=%d\n",j,j,xll[j],i); */
      if (xll[j] != (long long) i)
        fprintf(stderr, "FAIL PE %d of %d: xll[%d] = %d expected = %d\n",
                         my_pe, n_pes, j, xll[j],i);
      i++;
    }
  }
  shmem_free(xll);

#endif

/*  test shmem_float_swap  */

  /*  shmalloc xf on all pes (only use the ones on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(float) * n_pes);
  xf = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xf[i] = (float) 0;
  shmem_barrier_all();

  oldjf = (float) 0;
  for(i=0; i<ITER; i++) {
    if (my_pe != 0) {
      my_pef = my_pef + (float) 1;
      /* record PE value in xf[my_pe] -- save PE number */
      oldxf = shmem_float_swap(&xf[my_pe], my_pef, 0);
      /* printf("PE=%d,i=%d,my_pef=%10.2f,oldxf=%10.2f\n",my_pe,i,my_pef,oldxf); */
      if (oldxf != oldjf)
        fprintf(stderr, "FAIL PE %d of %d: i=%d, oldxf = %10.2f expected = %10.2f\n",
                         my_pe, n_pes, i, oldxf, oldjf);
      oldjf = my_pef;
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {  /* check xs[j] array vs PE# + ITER */
    i = (int) ITER + 1;
    for(j=1 ; j<n_pes; j++) {
      /* printf("j=%d,xf[%d]=%10.2f,i=%d\n",j,j,xf[j],i); */
      if (xf[j] != (float) i)
        fprintf(stderr, "FAIL PE %d of %d: xf[%d] = %10.2f expected = %10.2f\n",
                         my_pe, n_pes, j-1, xf[j], (float)i);
      i++;
    }
  }
  shmem_free(xf);

/*  test shmem_double_swap  */

  /*  shmalloc xd on all pes (only use the ones on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(double) * n_pes);
  xd = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xd[i] = (double) 0;
  shmem_barrier_all();

  oldjd = (double) 0;
  for(i=0; i<ITER; i++) {
    if (my_pe != 0) {
      my_ped = my_ped + (double) 1;
      /* record PE value in xd[my_pe] -- save PE number */
      oldxd = shmem_double_swap(&xd[my_pe], my_ped, 0);
      /* printf("PE=%d,i=%d,my_ped=%10.2f,oldxd=%10.2f\n",my_pe,i,my_ped,oldxd);
 */
      if (oldxd != oldjd)
        fprintf(stderr, "FAIL PE %d of %d: i=%d, oldxd = %10.2f expected = %10.2f\n",
                         my_pe, n_pes, i, oldxd, oldjd);
      oldjd = my_ped;
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {  /* check xd[j] array vs PE# + ITER */
    i = (int) ITER + 1;
    for(j=1 ; j<n_pes; j++) {
      /* printf("j=%d,xd[%d]=%10.2f,i=%d\n",j,j,xd[j],i); */
      if (xd[j] != (double) i)
        fprintf(stderr, "FAIL PE %d of %d: xd[%d] = %10.2f expected = %10.2f\n",
                         my_pe, n_pes, j, xd[j], (double)i);
      i++;
    }
  }
  shmem_free(xd);

#ifdef SHMEM_C_GENERIC_32

/*  test shmem_swap (GENERIC 32)  */

  my_pei = (int)  my_pe;
  /*  shmalloc xi on all pes (only check the ones on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(int) * n_pes);
  xi = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xi[i] = 0;
  shmem_barrier_all();

  oldji = 0;
  for(i=0; i<ITER; i++) {
    if (my_pe != 0) {
      my_pei = my_pei + (int) 1;
      /* record PE value in xi[my_pe] -- save PE number */
      oldxi = shmem_swap(&xi[my_pe], my_pei, 0);
      /* printf("PE=%d,i=%d,my_pei=%d,oldxi=%d\n",my_pe,i,my_pei,oldxi); */
      if (oldxi != oldji)
        fprintf(stderr, "FAIL pe %d of %d: i=%d, oldxi = %d expected = %d\n",
                         my_pe, n_pes, i, oldxi, oldji);
      oldji = my_pei;
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {  /* check xi[j] array vs PE# + ITER */
    i = (int) ITER + 1;
    for(j=1 ; j<n_pes; j++) {
      /* printf("j=%d,xi[%d]=%d,i=%d\n",j,j,xi[j],i); */
      if (xi[j] != i)
        fprintf(stderr, "FAIL pe %d of %d: xi[%d] = %d expected = %d\n",
                         my_pe, n_pes, j, xi[j],i);
      i++;
    }
  }
  shmem_free(xi);

#else

/*  test shmem_swap (GENERIC 64)  */

  my_pel = (long) my_pe;
  /*  shmalloc xl on all pes (only check the ones on PE 0)  */
  max_elements_bytes = (size_t) (sizeof(long) * n_pes);
  xl = shmem_malloc( max_elements_bytes );
  for(i=0; i<n_pes; i++)
    xl[i] = 0;
  shmem_barrier_all();

  oldjl = 0;
  for(i=0; i<ITER; i++) {
    if (my_pe != 0) {
      my_pel = my_pel + (long) 1;
      /* record PE value in xl[my_pe] -- save PE number */
#if (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L)
      oldxl = shmem_swap(&xl[my_pe], my_pel, 0);
#else
      oldxl = shmem_long_swap(&xl[my_pe], my_pel, 0);
#endif
      /* printf("PE=%d,i=%d,my_pel=%d,oldxl=%d\n",my_pe,i,my_pel,oldxl); */
      if (oldxl != oldjl)
        fprintf(stderr, "FAIL pe %d of %d: i=%d, oldxl = %d expected = %d\n",
                         my_pe, n_pes, i, oldxl, oldjl);
      oldjl = my_pel;
    }
  }
  shmem_barrier_all();

  if (my_pe == 0) {  /* check xl[j] array vs PE# + ITER */
    i = (int) ITER + 1;
    for(j=1 ; j<n_pes; j++) {
      /* printf("j=%d,xl[%d]=%d,i=%d\n",j,j,xl[j],i); */
      if (xl[j] != (long)i)
        fprintf(stderr, "FAIL pe %d of %d: xl[%d] = %ld expected = %d\n",
                         my_pe, n_pes, j, xl[j],i);
      i++;
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
