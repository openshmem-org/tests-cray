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
! Purpose:  Functional tests for the following shmem_*_p routines ---
!           shmem_char_p
!           shmem_short_p
!           shmem_int_p
!           shmem_long_p
!           shmem_longlong_p
!           shmem_float_p
!           shmem_double_p
!           shmem_longdouble_p
!
! Notes:  If type longdouble is available, compile with -DHAVE_LONG_DOUBLE
!
!*********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <mpp/shmem.h>

void
shmalloc_error(void)
{
  fprintf(stderr, "FAIL: not enough memory available\n");
  exit(1);
}

  char targ_char;
  short targ_short;
  int targ_int;
  long targ_long;
  float targ_float;
  double targ_double;
  long long targ_longlong;
#ifdef HAVE_LONG_DOUBLE
  long double targ_longdouble;
#endif

int main(int argc, char **argv)
{
  int j;
  int my_pe,n_pes;
  int *flag,one;
  char value_char;
  short value_short;
  int value_int;
  long value_long;
  float value_float;
  double value_double;
  long long value_longlong;
#ifdef HAVE_LONG_DOUBLE
  long double value_longdouble;
#endif

  shmem_init();
  my_pe = shmem_my_pe();
  n_pes = shmem_n_pes();
  flag = shmem_malloc((size_t) sizeof(int));
  one  = 1;

/*  fail if trying to use odd number of processors  */
  if ( (n_pes % 2) != 0 ){
        fprintf(stderr, "FAIL - test requires even number of PEs\n");
        exit(1);
  }

  if(my_pe == 0)
    fprintf(stderr, "shmem_type_p(%s)\n", argv[0]);

/*  shmem_char_p test   */
  *flag = 0;
  if(my_pe == 0)
    fprintf(stderr,"shmem_char_p\n");
  if ( (my_pe % 2) == 0 )
      value_char = (char) my_pe;
  else
      targ_char = (char) my_pe;
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    shmem_char_p(&targ_char,value_char,my_pe+1);
    shmem_quiet();
    shmem_int_p(flag,one,my_pe+1);
  } else {
    shmem_int_wait(flag,0);
    if ( targ_char != (char)(my_pe-1) )
        fprintf(stderr, "FAIL: PE [%d] targ_char=%d my_pe-1=%d\n",
                               my_pe,targ_char,my_pe-1);
  }

/*  shmem_short_p test   */
  *flag = 0;
  if(my_pe == 0)
    fprintf(stderr,"shmem_short_p\n");
  if ( (my_pe % 2) == 0 )
      value_short = (short) my_pe;
  else
      targ_short = (short) my_pe;
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    shmem_short_p(&targ_short,value_short,my_pe+1);
    shmem_quiet();
    shmem_int_p(flag,one,my_pe+1);
  } else {
    shmem_int_wait(flag,0);
    if ( targ_short != (short)(my_pe-1) )
        fprintf(stderr, "FAIL: PE [%d] targ_short=%d my_pe-1=%d\n",
                               my_pe,targ_short,my_pe-1);
  }

/*  shmem_int_p test   */
  *flag = 0;
  if(my_pe == 0)
    fprintf(stderr,"shmem_int_p\n");
  if ( (my_pe % 2) == 0 )
      value_int = (int) my_pe;
  else
      targ_int = (int) my_pe;
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    shmem_int_p(&targ_int,value_int,my_pe+1);
    shmem_quiet();
    shmem_int_p(flag,one,my_pe+1);
  } else {
    shmem_int_wait(flag,0);
    if ( targ_int != (int)(my_pe-1) )
	fprintf(stderr, "FAIL: PE [%d] targ_int=%d my_pe-1=%d\n",
                               my_pe,targ_int,my_pe-1);
  }
  
/*  shmem_long_p test   */
  *flag = 0;
  if(my_pe == 0)
    fprintf(stderr,"shmem_long_p\n");
  if ( (my_pe % 2) == 0 )
      value_long = (long) my_pe;
  else
      targ_long = (long) my_pe;
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    shmem_long_p(&targ_long,value_long,my_pe+1);
    shmem_quiet();
    shmem_int_p(flag,one,my_pe+1);
  } else {
    shmem_int_wait(flag,0);
    if ( targ_long != (long)(my_pe-1) )
        fprintf(stderr, "FAIL: PE [%d] targ_long=%d my_pe-1=%d\n",
                               my_pe,targ_long,my_pe-1);
  }

/*  shmem_longlong_p test   */
  *flag = 0;
  if(my_pe == 0)
    fprintf(stderr,"shmem_longlong_p\n");
  if ( (my_pe % 2) == 0 )
      value_longlong = (long long) my_pe;
  else
      targ_longlong = (long long) my_pe;
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    shmem_longlong_p(&targ_longlong,value_longlong,my_pe+1);
    shmem_quiet();
    shmem_int_p(flag,one,my_pe+1);
  } else {
    shmem_int_wait(flag,0);
    if ( targ_longlong != (long long)(my_pe-1) )
        fprintf(stderr, "FAIL: PE [%d] targ_longlong=%d my_pe-1=%d\n",
                               my_pe,targ_longlong,my_pe-1);
  }

/*  shmem_float_p test   */
  *flag = 0;
  if(my_pe == 0)
    fprintf(stderr,"shmem_float_p\n");
  if ( (my_pe % 2) == 0 )
      value_float = (float) my_pe;
  else
      targ_float = (float) my_pe;
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    shmem_float_p(&targ_float,value_float,my_pe+1);
    shmem_quiet();
    shmem_int_p(flag,one,my_pe+1);
  } else {
    shmem_int_wait(flag,0);
    if ( targ_float != (float)(my_pe-1) )
        fprintf(stderr, "FAIL: PE [%d] targ_float=%10.2f my_pe-1=%d\n",
                               my_pe,targ_float,my_pe-1);
  }

/*  shmem_double_p test   */
  *flag = 0;
  if(my_pe == 0)
    fprintf(stderr,"shmem_double_p\n");
  if ( (my_pe % 2) == 0 )
      value_double = (double) my_pe;
  else
      targ_double = (double) my_pe;
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    shmem_double_p(&targ_double,value_double,my_pe+1);
    shmem_quiet();
    shmem_int_p(flag,one,my_pe+1);
  } else {
    shmem_int_wait(flag,0);
    if ( targ_double != (double)(my_pe-1) )
        fprintf(stderr, "FAIL: PE [%d] targ_double=%10.2f my_pe-1=%d\n",
                               my_pe,targ_double,my_pe-1);
  }

/*  shmem_longdouble_p test   */
#ifdef HAVE_LONG_DOUBLE
  *flag = 0;
  if(my_pe == 0)
    fprintf(stderr,"shmem_longdouble_p\n");
  if ( (my_pe % 2) == 0 )
      value_longdouble = (long double) my_pe;
  else
      targ_longdouble = (long double) my_pe;
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    shmem_longdouble_p(&targ_longdouble,value_longdouble,my_pe+1);
    shmem_quiet();
    shmem_int_p(flag,one,my_pe+1);
  } else {
    shmem_int_wait(flag,0);
    if ( targ_longdouble != (long double)(my_pe-1) )
        fprintf(stderr, "FAIL: PE [%d] targ_longdouble=%10.2f my_pe-1=%d\n",
                               my_pe,targ_longdouble,my_pe-1);
  }
#endif

  shmem_free(flag);
#ifdef NEEDS_FINALIZE
  shmem_finalize(); 
#endif
  return 0;
}
