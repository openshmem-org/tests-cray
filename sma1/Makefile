###
### Depending on the compiling and linking environment used, some of these
### tests compile, link and run successfully only when certain
### macros are defined or certain compile or link options are specified.
### See the top level README file for a description of these macros.
###

CC=cc

CFLAGS=

TESTS= \
	shmalloc_loop \
	shmalloc_shrealloc \
	shmem_both_put_nb_size \
	shmem_broadcast_all \
	shmem_finc_cswap \
	shmem_finc_fadd \
	shmem_finc_only \
	shmem_finc_swap \
	shmem_lock_set_clear \
	shmem_lock_test_clear \
	shmem_long_finc_only \
	shmem_long_finc_swap \
	all_and_int_ext \
	all_max_double_ext \
	all_min_double_ext \
	all_or_int_ext \
	all_prod_double_ext \
	all_sum_double_ext \
	all_xor_int_ext \
	shmem_num_get \
	shmem_num_put \
	shmem_num_put_nb \
	shmem_num_put_nb_size \
	shmem_num_put_size \
	shmem_swap_only \
	shmem_type_g \
	shmem_type_get \
	shmem_type_p \
	shmem_type_put \
	shmem_type_put_nb 

all:	$(TESTS)

$(TESTS): %:	%.c
	-$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f $(TESTS)
