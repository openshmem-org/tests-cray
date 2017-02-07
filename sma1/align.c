/* *
 * Copyright (c) 2015 Cray Inc.
 *
 * The contents of this file is proprietary information of Cray Inc.
 * and may not be disclosed without prior written consent.
 *
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
