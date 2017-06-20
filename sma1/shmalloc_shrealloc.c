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
! Purpose:  Functional tests for the following shmalloc routines ---
!           shmem_shmalloc
!           shmem_shfree
!           shmem_shmem_realloc 
!
!*********************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <mpp/shmem.h>

#define BUFSIZE1 1000000
#define BUFSIZE2 100000    /* smaller than BUFSIZE1 */
#define BUFSIZE3 1000000   /* same size as BUFSIZE1 */
#define BUFSIZE4 1200000   /* larger than  BUFSIZE1 */
#define TOO_BIG  1000000000000

int
shmalloc_error(void)
{
    fprintf(stderr, "FAIL: not enough memory available\n");
    shmem_barrier_all();
    shmem_global_exit(1);
}

int main(int argc, char **argv)
{
  int i,j;
  int my_pe,n_pes,PE_root;
  size_t max_elements,max_elements_bytes;
  static long *buffer,*buffer1,*buffer2,*bptr,*bsave;
  long count;

  shmem_init();
  my_pe = shmem_my_pe();
  n_pes = shmem_n_pes();

/*  fail if trying to use only one processor  */
  if ( n_pes  <= 1 ){
        fprintf(stderr, "FAIL - test requires at least two PEs\n");
        exit(1);
  }

  if(my_pe == 0)
    fprintf(stderr, "shmalloc/shmem_realloc(%s) n_pes=%d\n", argv[0],n_pes);

/*  try to control garbage on shmalloc/shmem_realloc - smaller space  */

    buffer1 = (long *) shmem_malloc( (size_t) (sizeof(long) * BUFSIZE1) );
    if(my_pe == 0)
      printf("11 buffer1=0x%08x\n",buffer1);
    if (buffer1 == NULL) shmalloc_error();
    for(i=0; i<BUFSIZE1; i++)
        buffer1[i] = 10;
    shmem_free(buffer1);
    buffer2 = (long *) shmem_malloc( (size_t) (sizeof(long) * BUFSIZE2) );
    if(my_pe == 0)
      printf("12 buffer2=0x%08x\n",buffer2);
    if (buffer2 == NULL) shmalloc_error();
    for(i=0; i<BUFSIZE2; i++)
        buffer2[i] = 100;
    shmem_free(buffer2);

    /*  now allocate some space  */
    buffer = (long *) shmem_malloc( (size_t) (sizeof(long) * BUFSIZE1) );
    if(my_pe == 0)
      printf("21 buffer=0x%08x\n",buffer);
    if (buffer == NULL) shmalloc_error();
    for(i=0; i<BUFSIZE1; i++)
        buffer[i] = 1;
    count = 0;
    for(i=0; i<BUFSIZE1; i++)
        count += buffer[i];
    if(my_pe == 0)
      printf("PE %d of %d: count = %ld\n", my_pe, n_pes, count);

    /*  now shmem_realloc a shorter array  */
    bsave = buffer;
    bptr = (long *) shmem_realloc(buffer, (size_t) (sizeof(long) * BUFSIZE2) );
    if (bptr == NULL) {
        shmalloc_error();
    } else {
        buffer = bptr;
    }
    if (bsave != buffer) {
      if(my_pe == 0) {
        printf("buffer value has changed\n");
        printf("22 buffer=0x%08x\n",buffer);
        printf("Checking a few values beyond end of array\n");
      }
      for (i=BUFSIZE2; i<BUFSIZE2+4; i++)
        if (buffer[i] == 1)
          fprintf(stderr, "FAIL 00 > PE %d of %d: buffer[%d]=1, BUFSIZE2=%d\n",
                           my_pe, n_pes, i, BUFSIZE2);
    }
    count = 0;
    for(i=0; i<BUFSIZE2; i++)
        count += buffer[i];
    if (count != BUFSIZE2)
      fprintf(stderr, "FAIL 01 > PE %d of %d: count = %ld expected = %ld\n",
                           my_pe, n_pes, count, BUFSIZE2);
    shmem_free(buffer);

/*  shmalloc / assign data / count data  */

    max_elements_bytes = (size_t) (sizeof(long) * BUFSIZE1);
    buffer = (long *) shmem_malloc( max_elements_bytes );
    if(my_pe == 0)
      printf("02 buffer=0x%08x\n",buffer);
    if (buffer == NULL) shmalloc_error();
    for(i=0; i<BUFSIZE1; i++)
        buffer[i] = 1;
    count = 0;
    for(i=0; i<BUFSIZE1; i++)
        count += buffer[i];
    if (count != BUFSIZE1)
      fprintf(stderr, "FAIL 02 > PE %d of %d: count = %ld expected = %ld\n", 
                           my_pe, n_pes, count, BUFSIZE1);

/*  shmalloc - ask for TOO_BIG elements - should return NULL  */

    if(my_pe == 0)
      fprintf(stderr, "Out of memory errors are expected\n");
    shmem_barrier_all();
    max_elements_bytes = (size_t) (sizeof(long) * TOO_BIG);
    bptr = (long *) shmem_malloc( max_elements_bytes );
    if (bptr != NULL)
      fprintf(stderr, "FAIL -- requesting TOO_BIG bytes doesn't get NULL ptr\n");

/*  shmem_realloc / check that data is still there - same size allocation */
    bptr = (long *) shmem_realloc(buffer, sizeof(long) * BUFSIZE3);
    if(my_pe == 0)
      printf("03 bptr=0x%08x\n",bptr);
    if (bptr == NULL) {
        shmalloc_error();
    } else {
        buffer = bptr;
    }
    count = 0;
    for(i=0; i<BUFSIZE3; i++)
        count += buffer[i];
    if (count != BUFSIZE3)
      fprintf(stderr, "FAIL 03 > PE %d of %d: count = %ld expected = %ld\n", 
                           my_pe, n_pes, count, BUFSIZE3);

/*  shmem_realloc / larger size / check that data is still there  */
    buffer = (long *) shmem_realloc(buffer, sizeof(long) * BUFSIZE4);
    if(my_pe == 0)
      printf("04 buffer=0x%08x\n",buffer);
    if (buffer == NULL) shmalloc_error();
    count = 0;
    /*  only BUFSIZE3 elements are initialized  */
    for(i=0; i<BUFSIZE3; i++)
        count += buffer[i];
    if (count != BUFSIZE3)
      fprintf(stderr, "FAIL 04 > PE %d of %d: count = %ld expected = %ld\n",
                           my_pe, n_pes, count, BUFSIZE3);

/*  shmem_realloc / ask for TOO_BIG - check that data is still there -
              pointer should be left unchanged  */
    shmem_barrier_all();
    bsave = buffer;
    if(my_pe == 0) {
      printf("bsave=0x%08x;buffer=0x%08x\n",bsave,buffer);
      fprintf(stderr, "Out of memory errors are expected\n");
    }
    shmem_barrier_all();
    bptr = (long *) shmem_realloc(buffer, sizeof(long) * TOO_BIG);
    if (bptr != NULL) {
      fprintf(stderr, "FAIL -- shmem_realloc TOO_BIG bytes doesn't get NULL ptr\n");
    } else {
      buffer = bsave;
    }
    if(my_pe == 0)
      printf("bptr=0x%08x;buffer=0x%08x\n",bptr,buffer);
    count = 0;
    for(i=0; i<BUFSIZE3; i++)
        count += buffer[i];
    if (count != BUFSIZE3)
      fprintf(stderr, "FAIL 05 > PE %d of %d: count = %ld expected = %ld\n",
                             my_pe, n_pes, count, BUFSIZE3);

    shmem_barrier_all();
    shmem_finalize(); 
    return 0;
}
