#
#----------------------------------------------------------------------
# Copyright (c) 2016, Cray Inc.
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
# 
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# 
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# 
# * Neither the name Cray Inc. nor the names of its contributors may be
#   used to endorse or promote products derived from this software
#   without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#----------------------------------------------------------------------
#
#
### Depending on the compiling and linking environment used, some of these
### tests compile, link and run successfully only when certain
### macros are defined or certain compile or link options are specified.
### See the top level README file for a description of these macros.
###

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
	shmem_type_put_nb \
	align \
	shmem_global_exit

all:	$(TESTS)

$(TESTS): %:	%.c
	-$(CC) $(CFLAGS) -o $(CRAY_TEST_BUILD_BIN)/sma1/$@ $< $(CLIBS) \
		> $(CRAY_TEST_BUILD_OUT)/sma1/$@ 2>&1

clean:
	rm -f $(TESTS)
