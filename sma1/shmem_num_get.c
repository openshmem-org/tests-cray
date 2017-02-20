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
! Purpose:  Functional tests for the following shmem_get routines ---
!           shmem_getmem
!           shmem_get16
!           shmem_get32
!           shmem_get64
!           shmem_get128
!           shmem_get (GENERIC 32)
!           shmem_get (GENERIC 64)
!
! Note:  compile with -DSHMEM_GENERIC_32 to run 
!          shmem_get (GENERIC 32) instead of shmem_get (GENERIC 64)
!        PGI compiler needs -Msignextend option.
!
!*********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <mpp/shmem.h>

#define MAX_SIZE 100000

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

  char *srce_char,*targ_char;
  short *srce_short,*targ_short;
  int *srce_int,*targ_int;
  long *srce_long,*targ_long;

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
    fprintf(stderr, "shmem_num_get(%s)\n", argv[0]);

/*  shmem_getmem test   */
  *flag = 0;
  max_elements = (size_t) (MAX_SIZE / sizeof(char));
  max_elements_bytes = (size_t) (sizeof(char)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_getmem            max_elements = %d\n",max_elements);
  srce_char = shmem_malloc(max_elements_bytes);
  targ_char = shmem_malloc(max_elements_bytes);
  if((srce_char == NULL) || (targ_char == NULL))
    shmalloc_error();
  if ( (my_pe % 2) != 0 )
    for(j = 0; j < max_elements; j++) 
      srce_char[j] = (char)(my_pe+j);
  else
    for(j = 0; j < max_elements; j++) 
      targ_char[j] = (char)(my_pe+j);
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    shmem_getmem(targ_char,srce_char,max_elements,my_pe+1);
    for(j = 0; j < max_elements; j++)
      if ( targ_char[j] != (char)(my_pe+j+1) )
        fprintf(stderr, "FAIL: PE [%d] targ_char[%d]=%d my_pe+j+1=%d\n",
                               my_pe,j,targ_char[j],my_pe+j+1);
  }
  shmem_free(srce_char);  shmem_free(targ_char);

/*  shmem_get16 test   */
  *flag = 0;
  max_elements = (size_t) (MAX_SIZE / sizeof(short));
  if(max_elements > 20000) max_elements=20000;
  max_elements_bytes = (size_t) (sizeof(short)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_get16             max_elements = %d\n",max_elements);
  srce_short = shmem_malloc(max_elements_bytes);
  targ_short = shmem_malloc(max_elements_bytes);
  if((srce_short == NULL) || (targ_short == NULL))
    shmalloc_error();
  if ( (my_pe % 2) != 0 )
    for(j = 0; j < max_elements; j++) 
      srce_short[j] = (short)(my_pe+j);
  else
    for(j = 0; j < max_elements; j++) 
      targ_short[j] = (short)(my_pe+j);
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    shmem_get16(targ_short,srce_short,max_elements,my_pe+1);
    for(j = 0; j < max_elements; j++)
      if ( targ_short[j] != (short)(my_pe+j+1) )
        fprintf(stderr, "FAIL: PE [%d] targ_short[%d]=%d my_pe+j+1=%d\n",
                               my_pe,j,targ_short[j],my_pe+j+1);
  }
  shmem_free(srce_short);  shmem_free(targ_short);

/*  shmem_get32 test   */
  *flag = 0;
  max_elements = (size_t) (MAX_SIZE / sizeof(int));
  max_elements_bytes = (size_t) (sizeof(int)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_get32             max_elements = %d\n",max_elements);
  srce_int = shmem_malloc(max_elements_bytes);
  targ_int = shmem_malloc(max_elements_bytes);
  if((srce_int == NULL) || (targ_int == NULL))
    shmalloc_error();
  if ( (my_pe % 2) != 0 )
    for(j = 0; j < max_elements; j++)
      srce_int[j] = (int)(my_pe+j);
  else
    for(j = 0; j < max_elements; j++)
      targ_int[j] = (int)(my_pe+j);
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    shmem_get32(targ_int,srce_int,max_elements,my_pe+1);
    for(j = 0; j < max_elements; j++)
      if ( targ_int[j] != (int)(my_pe+j+1) )
	fprintf(stderr, "FAIL: PE [%d] targ_int[%d]=%d my_pe+j+1=%d\n",
                               my_pe,j,targ_int[j],my_pe+j+1);
  }
  shmem_free(srce_int);  shmem_free(targ_int);
  
/*  shmem_get64 test   */
  *flag = 0;
  max_elements = (size_t) (MAX_SIZE / sizeof(long));
  max_elements_bytes = (size_t) (sizeof(long)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_get64             max_elements = %d\n",max_elements);
  srce_long = shmem_malloc(max_elements_bytes);
  targ_long = shmem_malloc(max_elements_bytes);
  if((srce_long == NULL) || (targ_long == NULL))
    shmalloc_error();
  if ( (my_pe % 2) != 0 )
    for(j = 0; j < max_elements; j++)
      srce_long[j] = (long)(my_pe+j);
  else
    for(j = 0; j < max_elements; j++)
      targ_long[j] = (long)(my_pe+j);
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    shmem_get64(targ_long,srce_long,max_elements,my_pe+1);
    for(j = 0; j < max_elements; j++)
      if ( targ_long[j] != (long)(my_pe+j+1) )
        fprintf(stderr, "FAIL: PE [%d] targ_long[%d]=%d my_pe+j+1=%d\n",
                               my_pe,j,targ_long[j],my_pe+j+1);
  }
  shmem_free(srce_long);  shmem_free(targ_long);

/*  shmem_get128 test   */
  *flag = 0;
  max_elements = (size_t) (MAX_SIZE / sizeof(long));
  if ( (max_elements % 2) != 0)
    max_elements = max_elements-1;
  max_elements_bytes = (size_t) (sizeof(long)*max_elements);
  max_elements = max_elements/2;
  if(my_pe == 0)
    fprintf(stderr,"shmem_get128            max_elements = %d\n",max_elements);
  srce_long = shmem_malloc(max_elements_bytes);
  targ_long = shmem_malloc(max_elements_bytes);
  if((srce_long == NULL) || (targ_long == NULL))
    shmalloc_error();
  if ( (my_pe % 2) != 0 )
    for(j = 0; j < 2*max_elements; j++)
      srce_long[j] = (long)(my_pe+j);
  else
    for(j = 0; j < 2*max_elements; j++)
      targ_long[j] = (long)(my_pe+j);
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    shmem_get128(targ_long,srce_long,max_elements,my_pe+1);
    for(j = 0; j < 2*max_elements; j++)
      if ( targ_long[j] != (long)(my_pe+j+1) )
        fprintf(stderr, "FAIL: PE [%d] targ_long[%d]=%d my_pe+j+1=%d\n",
                               my_pe,j,targ_long[j],my_pe+j+1);
  }
  shmem_free(srce_long);  shmem_free(targ_long);

#ifdef SHMEM_C_GENERIC_32

/*  shmem_get (GENERIC 32) test   */
  *flag = 0;
  max_elements = (size_t) (MAX_SIZE / sizeof(int));
  max_elements_bytes = (size_t) (sizeof(int)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_get (GENERIC 32)  max_elements = %d\n",max_elements);
  srce_int = shmem_malloc(max_elements_bytes);
  targ_int = shmem_malloc(max_elements_bytes);
  if((srce_int == NULL) || (targ_int == NULL))
    shmalloc_error();
  if ( (my_pe % 2) != 0 )
    for(j = 0; j < max_elements; j++)
      srce_int[j] = (int)(my_pe+j);
  else
    for(j = 0; j < max_elements; j++)
      targ_int[j] = (int)(my_pe+j);
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    shmem_get(targ_int,srce_int,max_elements,my_pe+1);
    for(j = 0; j < max_elements; j++)
      if ( targ_int[j] != (int)(my_pe+j+1) )
        fprintf(stderr, "FAIL: PE [%d] targ_int[%d]=%d my_pe+j+1=%d\n",
                               my_pe,j,targ_int[j],my_pe+j+1);
  }
  shmem_free(srce_int);  shmem_free(targ_int);

#else

/*  shmem_get (GENERIC 64) test   */
  *flag = 0;
  max_elements = (size_t) (MAX_SIZE / sizeof(long));
  max_elements_bytes = (size_t) (sizeof(long)*max_elements);
  if(my_pe == 0)
    fprintf(stderr,"shmem_get (GENERIC 64)  max_elements = %d\n",max_elements);
  srce_long = shmem_malloc(max_elements_bytes);
  targ_long = shmem_malloc(max_elements_bytes);
  if((srce_long == NULL) || (targ_long == NULL))
    shmalloc_error();
  if ( (my_pe % 2) != 0 )
    for(j = 0; j < max_elements; j++)
      srce_long[j] = (long)(my_pe+j);
  else
    for(j = 0; j < max_elements; j++)
      targ_long[j] = (long)(my_pe+j);
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
#if (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L)
    shmem_get(targ_long,srce_long,max_elements,my_pe+1);
#else
    shmem_getmem(targ_long,srce_long,max_elements_bytes,my_pe+1);
#endif

    for(j = 0; j < max_elements; j++)
      if ( targ_long[j] != (long)(my_pe+j+1) )
        fprintf(stderr, "FAIL: PE [%d] targ_long[%d]=%d my_pe+j+1=%d\n",
                               my_pe,j,targ_long[j],my_pe+j+1);
  }
  shmem_free(srce_long);  shmem_free(targ_long);

#endif

#ifdef NEEDS_FINALIZE
  shmem_finalize(); 
#endif
  return 0;
}
