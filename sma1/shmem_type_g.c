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
! Purpose:  Functional tests for the following shmem_*_g routines ---
!           shmem_char_g
!           shmem_short_g
!           shmem_int_g
!           shmem_long_g
!           shmem_longlong_g
!           shmem_float_g
!           shmem_double_g
!           shmem_longdouble_g
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
  int one;
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
  one  = 1;

/*  fail if trying to use odd number of processors  */
  if ( (n_pes % 2) != 0 ){
        fprintf(stderr, "FAIL - test requires even number of PEs\n");
        exit(1);
  }

  if(my_pe == 0)
    fprintf(stderr, "shmem_type_g(%s)\n", argv[0]);

/*  shmem_char_g test   */

  if(my_pe == 0)
    fprintf(stderr,"shmem_char_g\n");
  if ( (my_pe % 2) == 0 )
      value_char = (char) my_pe;
  else
      targ_char = (char) my_pe;
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    value_char = shmem_char_g(&targ_char,my_pe+1);
    if ( value_char != (char)(my_pe+1) )
        fprintf(stderr, "FAIL: PE [%d] value_char=%d my_pe+1=%d\n",
                               my_pe,value_char,my_pe+1);
  }

/*  shmem_short_g test   */

  if(my_pe == 0)
    fprintf(stderr,"shmem_short_g\n");
  if ( (my_pe % 2) == 0 )
      value_short = (short) my_pe;
  else
      targ_short = (short) my_pe;
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    value_short = shmem_short_g(&targ_short,my_pe+1);
    if ( value_short != (short)(my_pe+1) )
        fprintf(stderr, "FAIL: PE [%d] value_short=%d my_pe+1=%d\n",
                               my_pe,value_short,my_pe+1);
  }

/*  shmem_int_g test   */
  if(my_pe == 0)
    fprintf(stderr,"shmem_int_g\n");
  if ( (my_pe % 2) == 0 )
      value_int = (int) my_pe;
  else
      targ_int = (int) my_pe;
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    value_int = shmem_int_g(&targ_int,my_pe+1);
    if ( value_int != (int)(my_pe+1) )
	fprintf(stderr, "FAIL: PE [%d] value_int=%d my_pe+1=%d\n",
                               my_pe,value_int,my_pe+1);
  }
  
/*  shmem_long_g test   */
  if(my_pe == 0)
    fprintf(stderr,"shmem_long_g\n");
  if ( (my_pe % 2) == 0 )
      value_long = (long) my_pe;
  else
      targ_long = (long) my_pe;
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    value_long = shmem_long_g(&targ_long,my_pe+1);
    if ( value_long != (long)(my_pe+1) )
        fprintf(stderr, "FAIL: PE [%d] value_long=%d my_pe+1=%d\n",
                               my_pe,value_long,my_pe+1);
  }

/*  shmem_longlong_g test   */
  if(my_pe == 0)
    fprintf(stderr,"shmem_longlong_g\n");
  if ( (my_pe % 2) == 0 )
      value_longlong = (long long) my_pe;
  else
      targ_longlong = (long long) my_pe;
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    value_longlong = shmem_longlong_g(&targ_longlong,my_pe+1);
    if ( value_longlong != (long long)(my_pe+1) )
        fprintf(stderr, "FAIL: PE [%d] value_longlong=%d my_pe+1=%d\n",
                               my_pe,value_longlong,my_pe+1);
  }

/*  shmem_float_g test   */
  if(my_pe == 0)
    fprintf(stderr,"shmem_float_g\n");
  if ( (my_pe % 2) == 0 )
      value_float = (float) my_pe;
  else
      targ_float = (float) my_pe;
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    value_float = shmem_float_g(&targ_float,my_pe+1);
    if ( value_float != (float)(my_pe+1) )
        fprintf(stderr, "FAIL: PE [%d] value_float=%10.2f my_pe+1=%d\n",
                               my_pe,value_float,my_pe+1);
  }

/*  shmem_double_g test   */
  if(my_pe == 0)
    fprintf(stderr,"shmem_double_g\n");
  if ( (my_pe % 2) == 0 )
      value_double = (double) my_pe;
  else
      targ_double = (double) my_pe;
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    value_double = shmem_double_g(&targ_double,my_pe+1);
    if ( value_double != (double)(my_pe+1) )
        fprintf(stderr, "FAIL: PE [%d] value_double=%10.2f my_pe+1=%d\n",
                               my_pe,value_double,my_pe+1);
  }

/*  shmem_longdouble_g test   */
#ifdef HAVE_LONG_DOUBLE
  if(my_pe == 0)
    fprintf(stderr,"shmem_longdouble_g\n");
  if ( (my_pe % 2) == 0 )
      value_longdouble = (long double) my_pe;
  else
      targ_longdouble = (long double) my_pe;
  shmem_barrier_all();
  if ( (my_pe % 2) == 0 ) {
    value_longdouble =shmem_longdouble_g(&targ_longdouble,my_pe+1);
    if ( value_longdouble != (long double)(my_pe+1) )
        fprintf(stderr, "FAIL: PE [%d] value_longdouble=%10.2f my_pe+1=%d\n",
                               my_pe,value_longdouble,my_pe+1);
  }
#endif

#ifdef NEEDS_FINALIZE
  shmem_finalize(); 
#endif
  return 0;
}
