!**********************************************************************
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
! Purpose:  Functional tests for the following shmem_xor routines 
!           using ALL processors ---
!             shmem_int2_xor_to_all
!             shmem_int4_xor_to_all
!             shmem_int8_xor_to_all
!
!**********************************************************************
  program test_shmem_xor_to_all_all
  implicit none
  include "mpp/shmem.fh"
  integer psync(shmem_reduce_sync_size)
  data psync /shmem_reduce_sync_size*shmem_sync_value/
  integer, parameter :: nr=3
  integer :: n_pes,my_pe,i,j,ismax
  integer, allocatable :: iseed(:)
  integer*2 :: int2(nr),int2xor(nr),int20(nr),ans_i2(nr),ans_i20(nr)
  integer*2, allocatable :: i2(:,:)
  integer*2 :: pwrk_i2(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comi2/ int2,int2xor,ans_i2,pwrk_i2
  integer*4 :: int4(nr),int4xor(nr),int40(nr),ans_i4(nr),ans_i40(nr)
  integer*4, allocatable :: i4(:,:)
  integer*4 :: pwrk_i4(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comi4/ int4,int4xor,ans_i4,pwrk_i4
  integer*8 :: int8(nr),int8xor(nr),int80(nr),ans_i8(nr),ans_i80(nr)
  integer*8, allocatable :: i8(:,:)
  integer*8 :: pwrk_i8(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comi8/ int8,int8xor,ans_i8,pwrk_i8
  real*4, allocatable :: r4(:,:)
  real*8, allocatable :: r8(:,:)

  call shmem_init
  n_pes = shmem_n_pes()
  my_pe = shmem_my_pe()

! test shmem_int2_xor_to_all
  if (my_pe == 0) then
    allocate (i2(nr,n_pes))
    allocate (r4(nr,n_pes))
    call random_seed(size=ismax)
    allocate (iseed(ismax))
    iseed = 12345
    call random_seed(put=iseed)
    call random_number(r4)
    i2=10000*r4
    ! use only odd numbers, so 0 is not the answer to 'xor'
    do j=1,n_pes
      do i=1,nr
        if (mod(i2(i,j),2_2).eq.0) i2(i,j)=i2(i,j)+1_2
      enddo
!     print *, 'my_pe=',my_pe,' i2(:,j)=',i2(:,j)
    enddo
    ! create expected answers
    ans_i20=i2(:,1)
!   write (6,fmt="(3(z4,2x))") ans_i20
    do j=2,n_pes
      ans_i20=ieor(ans_i20,i2(:,j))
    enddo
    write (6,fmt="('Expected  int*2 xor are ',3i12)") ans_i20
    ! send out the data and answers
    do i = 1,n_pes
      int20(:)=i2(:,i)
      call shmem_putmem(int2,int20,2*nr,i-1)    ! data
      call shmem_putmem(ans_i2,ans_i20,2*nr,i-1)  ! expected answers
    enddo 
    deallocate (r4)
                  endif
  call shmem_barrier_all
! print *, 'my_pe=',my_pe,' int2=',int2
  call shmem_int2_xor_to_all (int2xor,int2,nr,0,0,n_pes,pwrk_i2,psync)
! call shmem_int4_xor_to_all (int2xor,int2,nr,0,0,n_pes,pwrk_i2,psync)
! write (6,fmt="('Results on PE ',i5,' are ',3i12)") my_pe,int2xor
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (int2xor(i) .ne. ans_i2(i)) then
      print *, 'FAIL int*2 - PE=',my_pe,' xor=',int2xor(i),' ans=',ans_i2(i)
                                    endif
  enddo
  call shmem_barrier_all

! test shmem_int4_xor_to_all
  if (my_pe == 0) then
    allocate (i4(nr,n_pes))
    allocate (r4(nr,n_pes))
    call random_seed(put=iseed)
    call random_number(r4)
    i4=1000000000*r4
    ! use only odd numbers, so 0 is not the answer to 'xor'
    do j=1,n_pes
      do i=1,nr
        if (mod(i4(i,j),2_4).eq.0) i4(i,j)=i4(i,j)+1_4
      enddo
!     print *, 'my_pe=',my_pe,' i4(:,j)=',i4(:,j)
    enddo
    ! create expected answers
    ans_i40=i4(:,1)
!   write (6,fmt="(3(z8,2x))") ans_i40
    do j=2,n_pes
      ans_i40=ieor(ans_i40,i4(:,j))
    enddo
    write (6,fmt="('Expected  int*4 xor are ',3i12)") ans_i40
    ! send out the data and answers
    do i = 1,n_pes
      int40(:)=i4(:,i)
      call shmem_put4(int4,int40,nr,i-1)    ! data
      call shmem_put4(ans_i4,ans_i40,nr,i-1)  ! expected answers
    enddo 
    deallocate (r4)
                  endif
  call shmem_barrier_all
  call shmem_int4_xor_to_all (int4xor,int4,nr,0,0,n_pes,pwrk_i4,psync)
! write (6,fmt="('Results on PE ',i5,' are ',3i12)") my_pe,int4xor
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (int4xor(i) .ne. ans_i4(i)) then
      print *, 'FAIL int*4 - PE=',my_pe,' xor=',int4xor(i),' ans=',ans_i4(i)
                                    endif
  enddo
  call shmem_barrier_all

! test shmem_int8_xor_to_all
  if (my_pe == 0) then
    allocate (i8(nr,n_pes))
    allocate (r8(nr,n_pes))
    call random_seed(put=iseed)
    call random_number(r8)
    i8=1000000000000_8*r8
    ! use only odd numbers, so 0 is not the answer to 'xor'
    do j=1,n_pes
      do i=1,nr
        if (mod(i8(i,j),2_8).eq.0) i8(i,j)=i8(i,j)+1_8
      enddo
!     print *, 'my_pe=',my_pe,' i8(:,j)=',i8(:,j)
    enddo
    ! create expected answers
    ans_i80=i8(:,1)
!   write (6,fmt="(3(z16,2x))") ans_i80
    do j=2,n_pes
      ans_i80=ieor(ans_i80,i8(:,j))
    enddo
    write (6,fmt="('Expected  int*8 xor are ',3i18)") ans_i80
    ! send out the data and answers
    do i = 1,n_pes
      int80(:)=i8(:,i)
      call shmem_put8(int8,int80,nr,i-1)    ! data
      call shmem_put8(ans_i8,ans_i80,nr,i-1)  ! expected answers
    enddo 
    deallocate (r8)
                  endif
  call shmem_barrier_all
  call shmem_int8_xor_to_all (int8xor,int8,nr,0,0,n_pes,pwrk_i8,psync)
! write (6,fmt="('Results on PE ',i5,' are ',3i18)") my_pe,int8xor
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (int8xor(i) .ne. ans_i8(i)) then
      print *, 'FAIL int*8 - PE=',my_pe,' xor=',int8xor(i),' ans=',ans_i8(i)
                                    endif
  enddo

  call shmem_barrier_all
#ifdef NEEDS_FINALIZE
  call shmem_finalize
#endif
  end
