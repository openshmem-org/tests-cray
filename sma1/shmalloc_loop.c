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
!
!*********************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <mpp/shmem.h>

#define BUFSIZE1 1000000
#define BUFSIZE2 2000000
#define BUFSIZE3 3000000
#define BUFSIZE4 3000000
#define MAXJ     5

int
shmalloc_error(void)
{
    fprintf(stderr, "FAIL: not enough memory available\n");
    shmem_barrier_all();
    shmem_finalize();
    exit(1);
}

int main(int argc, char **argv)
{
  int i,j;
  int my_pe,n_pes,PE_root;
  size_t max_elements,max_elements_bytes;
  static long *buffer1,*buffer2,*buffer3,*buffer4;
  long count;

  shmem_init();
  my_pe = shmem_my_pe();
  n_pes = shmem_n_pes();

/*  fail if trying to use only one processor  */
  if ( n_pes  <= 1 ){
        fprintf(stderr, "FAIL - test requires at least two PEs\n");
        exit(1);
  }

  buffer4=NULL;
  if(my_pe == 0)
    fprintf(stderr, "shmalloc_loop(%s) n_pes=%d\n", argv[0],n_pes);

  for(j=0; j<MAXJ; j++)
  {

/*  shmalloc / assign buffer1 / count data  */

    max_elements_bytes = (size_t) (sizeof(long) * BUFSIZE1);
    buffer1 = shmem_malloc( max_elements_bytes );
    if(my_pe == 0)
      printf("1 - %02d PE=%4d, buffer1=0x%08x\n",j,my_pe,buffer1);
    if (buffer1 == NULL) shmalloc_error();
    for(i=0; i<BUFSIZE1; i++)
        buffer1[i] = 1;
    count = 0;
    for(i=0; i<BUFSIZE1; i++)
        count += buffer1[i];
    if (count != BUFSIZE1)
      fprintf(stderr, "FAIL 01 > PE %d of %d: count = %ld expected = %ld\n", 
                           my_pe, n_pes, count, BUFSIZE1);

/*  shfree - give up allocation for buffer4  */

    if(my_pe == 0 & buffer4 != NULL)
      printf("1 - %02d free --- buffer4=0x%08x, %d bytes\n",
                               j,buffer4,BUFSIZE4);
    shmem_free(buffer4);

/*  shmalloc / assign buffer2 / count data  */

    max_elements_bytes = (size_t) (sizeof(long) * BUFSIZE2);
    buffer2 = shmem_malloc( max_elements_bytes );
    if(my_pe == 0)
      printf("2 - %02d PE=%4d, buffer2=0x%08x\n",j,my_pe,buffer2);
    if (buffer2 == NULL) shmalloc_error();
    for(i=0; i<BUFSIZE2; i++)
        buffer2[i] = 1;
    count = 0;
    for(i=0; i<BUFSIZE2; i++)
        count += buffer2[i];
    if (count != BUFSIZE2)
      fprintf(stderr, "FAIL 02 > PE %d of %d: count = %ld expected = %ld\n",
                           my_pe, n_pes, count, BUFSIZE2);

/*  shfree - give up allocation for buffer1  */

    if(my_pe == 0)
      printf("2 - %02d free --- buffer1=0x%08x, %d bytes\n",
                               j,buffer1,BUFSIZE1);
    shmem_free(buffer1);

/*  shmalloc / assign buffer3 / count data  */

    max_elements_bytes = (size_t) (sizeof(long) * BUFSIZE3);
    buffer3 = shmem_malloc( max_elements_bytes );
    if(my_pe == 0)
      printf("3 - %02d PE=%4d, buffer3=0x%08x\n",j,my_pe,buffer3);
    if (buffer3 == NULL) shmalloc_error();
    for(i=0; i<BUFSIZE3; i++)
        buffer3[i] = 1;
    count = 0;
    for(i=0; i<BUFSIZE3; i++)
        count += buffer3[i];
    if (count != BUFSIZE3)
      fprintf(stderr, "FAIL 03 > PE %d of %d: count = %ld expected = %ld\n",
                           my_pe, n_pes, count, BUFSIZE3);

/*  shfree - give up allocation for buffer2  */

    if(my_pe == 0)
      printf("3 - %02d free --- buffer2=0x%08x, %d bytes\n",
                               j,buffer2,BUFSIZE2);
    shmem_free(buffer2);

/*  shmalloc / assign buffer4 / count data  */

    max_elements_bytes = (size_t) (sizeof(long) * BUFSIZE4);
    buffer4 = shmem_malloc( max_elements_bytes );
    if(my_pe == 0)
      printf("4 - %02d PE=%4d, buffer4=0x%08x\n",j,my_pe,buffer4);
    if (buffer4 == NULL) shmalloc_error();
    for(i=0; i<BUFSIZE4; i++)
        buffer4[i] = 1;
    count = 0;
    for(i=0; i<BUFSIZE4; i++)
        count += buffer4[i];
    if (count != BUFSIZE4)
      fprintf(stderr, "FAIL 04 > PE %d of %d: count = %ld expected = %ld\n",
                           my_pe, n_pes, count, BUFSIZE4);

/*  shfree - give up allocation for buffer3  */

    if(my_pe == 0)
      printf("4 - %02d free --- buffer3=0x%08x, %d bytes\n",
                               j,buffer3,BUFSIZE3);
    shmem_free(buffer3);

  }

    shmem_barrier_all();
#ifdef NEEDS_FINALIZE
    shmem_finalize(); 
#endif
    return 0;
}
