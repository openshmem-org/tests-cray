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
		> $(CRAY_TEST_BUILD_OUT)/smaf/$@ 2>&1

clean:
	rm -f $(TESTS_F90)
