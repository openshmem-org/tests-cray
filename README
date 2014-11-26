           CRAY SHMEM TESTS

These tests have not been reviewed for compatibility with OpenSHMEM.  
In particular, Cray SHMEM demands that programs call shmem_finalize()
or invoke the shmem_finalize(); function before exiting.  OpenSHMEM
has no such requirement.  Some SHMEM calls may be extensions of
functionality in OpenSHMEM.

Many of the tests have conditional compilation macros to conditionally
include (or exclude) specific parts of tests.  These macros are:

  macro                      defining with -D means:
  ------------------------   --------------------------------------------
  HAVE_SHORT                 include type "short" test code
  HAVE_LONG_LONG             include type "long long" test code
  HAVE_LONG_DOUBLE           include type "long double" test code
  HAVE_SET_CACHE_INV         include shmem_set_cache_inv call
  NEEDS_FINALIZE             include shmem_finalize call
  SHMEM_C_GENERIC_32         C generic routines are 32-bit types
  SHMEM_FORTRAN_GENERIC_64   Fortran generic type routines are 64-bit types
  NREDUCE                    perform this number of reductions
  _FULLACTIVESETONLY         test only with the full active set of PEs

Notes on SHMEM_C_GENERIC_32 and SHMEM_FORTRAN_GENERIC_64:
Many of these tests have been written to support 2 variants of
the SHMEM generic routines, that is, the SHMEM routines that do not
explicitly include a type in the name but map onto a specific data type.
Specifically, these tests assume either a 32-bit type or a 64-bit type
for the implementation of the generic routines.
The SHMEM_C_GENERIC_32 and SHMEM_FORTRAN_GENERIC_64 macros control
which variant of the generic routines are being used.  Defining
SHMEM_C_GENERIC_32 will require a SHMEM library that has 32-bit types
for the C generic routines.  Otherwise, 64-bit types are assumed.
Defining SHMEM_FORTRAN_GENERIC_64 will require a SHMEM library that has
64-bit types for the Fortran generic routines.  Otherwise, 32-bit types
are assumed.

All the tests are self checking.  If a test passes, the exit status is
0 and no "FAIL" messages are written to stderr.  If a test detects an
error, one or more "FAIL" messages are written to stderr.  Since it is
also possible that a test will abort for some other reason than the test
detecting an error, the combination of an exit status of 0 and no "FAIL"
messages in stderr means the test passed.

Most of the tests also write additional information to stdout.

The following set of sma1 tests must be compiled with the -Msignextend
option if compiled with the PGI compiler:

shmem_num_get.c                         
shmem_num_put.c                         
shmem_num_put_nb.c                      
shmem_num_put_nb_size.c                 
shmem_both_put_nb_size.c                
shmem_num_put_size.c                    
shmem_type_g.c                          
shmem_type_p.c                          

Most of the tests may be run with any number of processors, although
the tests may not exhibit scalability.  The tests are designed to 
test the functionality of the SHMEM routines.  The following set of sma1
tests are designed to run with an EVEN number of processors:

shmem_num_get.c
shmem_num_put.c
shmem_num_put_nb.c
shmem_num_put_size.c
shmem_num_put_nb_size.c
shmem_both_put_nb_size.c
shmem_type_g.c
shmem_type_p.c
shmem_type_get.c
shmem_type_put.c
shmem_type_put_nb.c

The following set of sma1 tests may be run with -DNREDUCE=1 or
with no override:

all_and_int_ext.c
all_max_double_ext.c
all_min_double_ext.c
all_or_int_ext.c
all_prod_double_ext.c
all_sum_double_ext.c
all_xor_int_ext.c

The following set of sma1 tests may be run with -DSHMEM_C_GENERIC_32 or
with no override:

shmem_broadcast_all.c
shmem_finc_only.c
shmem_finc_swap.c
shmem_finc_fadd.c
shmem_finc_cswap.c
shmem_num_get.c
shmem_num_put.c
shmem_num_put_nb.c
shmem_swap_only.c

The following set of sma1 tests may be run with -D_FULLACTIVESETONLY or
with no override:

all_and_int_ext.c
all_max_double_ext.c
all_min_double_ext.c
all_or_int_ext.c
all_prod_double_ext.c
all_sum_double_ext.c
all_xor_int_ext.c

The following set of sma1 tests may be run with -DHAVE_LONG_LONG or
with no override:

shmem_finc_cswap.c
shmem_finc_fadd.c
shmem_finc_only.c
shmem_finc_swap.c
shmem_swap_only.c

The following set of sma1 tests may be run with -DHAVE_SET_CACHE_INV or
with no override:

shmem_finc_cswap.c
shmem_finc_fadd.c
shmem_finc_only.c
shmem_finc_swap.c
shmem_lock_set_clear.c
shmem_lock_test_clear.c
shmem_long_finc_only.c
shmem_long_finc_swap.c
shmem_swap_only.c

The following set of sma1 tests run with the PGI compiler need -Msignextend :

shmem_num_get.c
shmem_num_put.c
shmem_num_put_nb.c
shmem_num_put_size.c
shmem_num_put_nb_size.c
shmem_both_put_nb_size.c
shmem_type_g.c
shmem_type_p.c

The following set of sma2 tests may be run with -D_FULLACTIVESETONLY or
with no override:

all_and_alltypes.cpp
all_max_alltypes.cpp
all_min_alltypes.cp
all_or_alltypes.cpp
all_prod_alltypes.cpp
all_sum_alltypes.cpp
all_xor_alltypes.cpp

The following set of sma2 tests may be run with -DHAVE_SHORT or
with no override:

all_and_alltypes.cpp
all_max_alltypes.cpp
all_min_alltypes.cpp
all_or_alltypes.cpp
all_prod_alltypes.cpp
all_sum_alltypes.cpp
all_xor_alltypes.cpp

The following set of sma2 tests may be run with -DHAVE_LONG_DOUBLE or
with no override:

all_max_alltypes.cpp
all_min_alltypes.cpp
all_prod_alltypes.cpp
all_sum_alltypes.cpp

The following set of smaf tests require the compiler option -fcray-pointer
if compiled with the GNU compiler:

shmem_lock_set_clear.F90
shmem_lock_test_clear.F90

The following set of smaf tests may be run with -DSHMEM_FORTRAN_GENERIC_64 or
with no override:

get_tst_inc_char.F90
get_tst_inc_complex.F90
get_tst_inc_integer.F90
get_tst_inc_logical.F90
get_tst_inc_real.F90
put_tst_inc_char.F90
put_tst_inc_complex.F90
put_tst_inc_integer.F90
put_tst_inc_logical.F90
put_tst_inc_real.F90

The following set of smaf tests may be run with -DHAVE_LONG_DOUBLE or
with no override:

get_tst_inc_real.F90
put_tst_inc_real.F90
shmem_prod_to_all_all.F90
shmem_prod_to_all_even.F90

All tests in sma1, sma2, and smaf must be run with -DNEEDS_FINALIZE if
the implementation requires a call to shmem_finalize();

========================================================================
