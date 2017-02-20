/* *
 *----------------------------------------------------------------------
 * Copyright (c) 2016, Cray Inc.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * 
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * 
 * * Neither the name Cray Inc. nor the names of its contributors may be
 *   used to endorse or promote products derived from this software
 *   without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *----------------------------------------------------------------------
 * Purpose: Test for alignment in shmem_align. This test doesnot perform 
 * the functional test for shmem_align routine, rather it performs the
 * limits in Cray SHMEM implementation. Limitations includes
 *  - user alignment values greater than 64 bytes should return NULL
 *  - user alignment values less than or equal to 64 bytes will still be
 *    64 byte aligned
 *
 * Note: The following routine is being tested
 * shmem_align
 */
/* *
 * $HeadURL$
 * $Date$
 * $Rev$
 * $Author$
 * */

/************************************************************************************/
 
#include <stdio.h>
#include <stdlib.h>
#include <shmem.h>
#include <unistd.h>
#include <stdint.h>

#define SIZE 1000
#define BYTE 64
#define is_aligned(PTR) \
        (((uintptr_t)(const void *)(PTR)) % (BYTE) == 0)
int me;

void *test_align_less_than_64(size_t align) {
    int i;
    int *A;
    A = shmem_align(align, SIZE * sizeof(int));

    if (align <= 64) {
        for (i = 0; i < SIZE; i++) {
            A[i] = i;
        }
        shmem_barrier_all();
        if (me == 0) shmem_int_put(A, A, SIZE, 1);
    }
    return A;
}

void test_output(char test[], int status) {
    if (status) {
        if (me == 0) {
            printf("PASS: Test for %s passed\n", test);
        }
    } else {
        if (me == 0) {
            printf("FAIL: Test for %s Failed\n", test);
        }
    }
}

int main(void) {
    int *A;

    shmem_init();
    me = shmem_my_pe();

    A = test_align_less_than_64 (63);
    test_output("Alignment less than 64", ((A != NULL) && is_aligned(A)) );
    shmem_free(A);
    
    A = test_align_less_than_64 (64);
    test_output("Alignment equal to  64", ((A != NULL) && is_aligned(A)) );
    shmem_free(A);

    A = test_align_less_than_64 (65);
    test_output("Alignment greater than 64", ((A != NULL) && is_aligned(A)) );
    shmem_free(A);
    shmem_finalize();
    return 0;
}
