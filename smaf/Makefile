###
### Depending on the compiling and linking environment used, some of these
### tests compile, link and run successfully only when certain
### macros are defined or certain compile or link options are specified.
### See the top level README file for a description of these macros.
###

# Tests with .F90 extension
TESTS_F90= \
	get_tst_inc_char \
	get_tst_inc_complex \
	get_tst_inc_integer \
	get_tst_inc_logical \
	get_tst_inc_real \
	put_tst_inc_char \
	put_tst_inc_complex \
	put_tst_inc_integer \
	put_tst_inc_logical \
	put_tst_inc_real \
	shmem_and_to_all_all \
	shmem_and_to_all_even \
	shmem_finc_inc_cswap \
	shmem_finc_inc_fadd \
	shmem_finc_inc_only \
	shmem_finc_inc_swap \
	shmem_max_to_all_all \
	shmem_max_to_all_even \
	shmem_min_to_all_all \
	shmem_min_to_all_even \
	shmem_or_to_all_all \
	shmem_or_to_all_even \
	shmem_prod_to_all_all \
	shmem_prod_to_all_even \
	shmem_sum_to_all_all \
	shmem_sum_to_all_even \
	shmem_xor_to_all_all \
	shmem_xor_to_all_even \
	shmem_lock_set_clear  \
	shmem_lock_test_clear

all:	$(TESTS_F90)

$(TESTS_F90): %:	%.F90
	-$(FTN) $(FFLAGS) -o $(CRAY_TEST_BUILD_BIN)/smaf/$@ $< $(FLIBS) \
		> $(CRAY_TEST_BUILD_OUT)/sma1/$@ 2>&1

clean:
	rm -f $(TESTS_F90)
