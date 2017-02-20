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
! Purpose:  Functional tests for the following shmem_min routines 
!           using ALL processors ---
!             shmem_real4_min_to_all
!             shmem_real8_min_to_all
!             shmem_int2_min_to_all
!             shmem_int4_min_to_all
!             shmem_int8_min_to_all
!
!**********************************************************************
  program test_shmem_min_to_all_all
  implicit none
  include "mpp/shmem.fh"
  integer psync(shmem_reduce_sync_size)
  data psync /shmem_reduce_sync_size*shmem_sync_value/
  integer, parameter :: nr=3
  integer :: n_pes,my_pe,i,j,ismax
  integer, allocatable :: iseed(:)
  real*4 :: real4(nr),real4min(nr),real40(nr),ans_r4(nr),ans_r40(nr)
  real*4, allocatable :: r4(:,:)
  real*4 :: pwrk_r4(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comr4/ real4,real4min,ans_r4,pwrk_r4
  real*8 :: real8(nr),real8min(nr),real80(nr),ans_r8(nr),ans_r80(nr)
  real*8, allocatable :: r8(:,:)
  real*8 :: pwrk_r8(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comr8/ real8,real8min,ans_r8,pwrk_r8
  integer*2 :: int2(nr),int2min(nr),int20(nr),ans_i2(nr),ans_i20(nr)
  integer*2, allocatable :: i2(:,:)
  integer*2 :: pwrk_i2(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comi2/ int2,int2min,ans_i2,pwrk_i2
  integer*4 :: int4(nr),int4min(nr),int40(nr),ans_i4(nr),ans_i40(nr)
  integer*4, allocatable :: i4(:,:)
  integer*4 :: pwrk_i4(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comi4/ int4,int4min,ans_i4,pwrk_i4
  integer*8 :: int8(nr),int8min(nr),int80(nr),ans_i8(nr),ans_i80(nr)
  integer*8, allocatable :: i8(:,:)
  integer*8 :: pwrk_i8(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comi8/ int8,int8min,ans_i8,pwrk_i8

#ifdef OPENSHMEM_FORT_SHORT_HEADER
  integer  shmem_my_pe, shmem_n_pes
#endif

  call shmem_init
  n_pes = shmem_n_pes()
  my_pe = shmem_my_pe()

! test shmem_real4_min_to_all
  if (my_pe == 0) then
    allocate (r4(nr,n_pes))
    call random_seed(size=ismax)
    allocate (iseed(ismax))
    iseed = 12345
    call random_seed(put=iseed)
    call random_number(r4)
    ! create expected answers
    do i = 1,nr
      ans_r40(i)=huge(ans_r40)
      do j=1,n_pes
        ans_r40(i)=min(ans_r40(i),r4(i,j))
      enddo
    enddo
    write (6,fmt="('Expected real*4 min are ',3f12.7)") ans_r40
    ! send out the data and answers
    do i = 1,n_pes
      real40(:)=r4(:,i)
      call shmem_put4(real4,real40,nr,i-1)    ! data
      call shmem_put4(ans_r4,ans_r40,nr,i-1)  ! expected answers
    enddo 
                  endif
  call shmem_barrier_all
  call shmem_real4_min_to_all (real4min,real4,nr,0,0,n_pes,pwrk_r4,psync)
! write (6,fmt="('Results on PE ',i5,' are ',3f12.7)") my_pe,real4min
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (real4min(i) .ne. ans_r4(i)) then
      print *, 'FAIL real*4 - PE=',my_pe,' min=',real4min(i),' ans=',ans_r4(i)
                                    endif
  enddo
  call shmem_barrier_all

! test shmem_real8_min_to_all
  if (my_pe == 0) then
    allocate (r8(nr,n_pes))
    call random_seed(put=iseed)
    call random_number(r8)
    ! create expected answers
    do i = 1,nr
      ans_r80(i)=huge(ans_r80)
      do j=1,n_pes
        ans_r80(i)=min(ans_r80(i),r8(i,j))
      enddo
    enddo
    write (6,fmt="('Expected real*8 min are ',3f18.13)") ans_r80
    ! send out the data and answers
    do i = 1,n_pes
      real80(:)=r8(:,i)
      call shmem_put8(real8,real80,nr,i-1)    ! data
      call shmem_put8(ans_r8,ans_r80,nr,i-1)  ! expected answers
    enddo 
                  endif
  call shmem_barrier_all
  call shmem_real8_min_to_all (real8min,real8,nr,0,0,n_pes,pwrk_r8,psync)
! write (6,fmt="('Results on PE ',i5,' are ',3f18.13)") my_pe,real8min
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (real8min(i) .ne. ans_r8(i)) then
      print *, 'FAIL real*8 - PE=',my_pe,' min=',real8min(i),' ans=',ans_r8(i)
                                    endif
  enddo
  call shmem_barrier_all

#ifndef OPENSHMEM
! test shmem_int2_min_to_all
  if (my_pe == 0) then
    allocate (i2(nr,n_pes))
    call random_seed(put=iseed)
    call random_number(r4)
    i2=10000*r4
    ! create expected answers
    do i = 1,nr
      ans_i20(i)=huge(ans_i20)
      do j=1,n_pes
        ans_i20(i)=min(ans_i20(i),i2(i,j))
      enddo
    enddo
    write (6,fmt="('Expected  int*2 min are ',3i12)") ans_i20
    ! send out the data and answers
    do i = 1,n_pes
      int20(:)=i2(:,i)
      call shmem_putmem(int2,int20,2*nr,i-1)    ! data
      call shmem_putmem(ans_i2,ans_i20,2*nr,i-1)  ! expected answers
    enddo 
                  endif
  call shmem_barrier_all
  call shmem_int2_min_to_all (int2min,int2,nr,0,0,n_pes,pwrk_i2,psync)
! write (6,fmt="('Results on PE ',i5,' are ',3i12)") my_pe,int2min
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (int2min(i) .ne. ans_i2(i)) then
      print *, 'FAIL int*2 - PE=',my_pe,' min=',int2min(i),' ans=',ans_i2(i)
                                    endif
  enddo
  call shmem_barrier_all
#endif

! test shmem_int4_min_to_all
  if (my_pe == 0) then
    allocate (i4(nr,n_pes))
    call random_seed(put=iseed)
    call random_number(r4)
    i4=1000000000*r4
    ! create expected answers
    do i = 1,nr
      ans_i40(i)=huge(ans_i40)
      do j=1,n_pes
        ans_i40(i)=min(ans_i40(i),i4(i,j))
      enddo
    enddo
    write (6,fmt="('Expected  int*4 min are ',3i12)") ans_i40
    ! send out the data and answers
    do i = 1,n_pes
      int40(:)=i4(:,i)
      call shmem_put4(int4,int40,nr,i-1)    ! data
      call shmem_put4(ans_i4,ans_i40,nr,i-1)  ! expected answers
    enddo 
                  endif
  call shmem_barrier_all
  call shmem_int4_min_to_all (int4min,int4,nr,0,0,n_pes,pwrk_i4,psync)
! write (6,fmt="('Results on PE ',i5,' are ',3i12)") my_pe,int4min
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (int4min(i) .ne. ans_i4(i)) then
      print *, 'FAIL int*4 - PE=',my_pe,' min=',int4min(i),' ans=',ans_i4(i)
                                    endif
  enddo
  call shmem_barrier_all

! test shmem_int8_min_to_all
  if (my_pe == 0) then
    allocate (i8(nr,n_pes))
    call random_seed(put=iseed)
    call random_number(r4)
    i8=10000000000000000_8*r4
    ! create expected answers
    do i = 1,nr
      ans_i80(i)=huge(ans_i80)
      do j=1,n_pes
        ans_i80(i)=min(ans_i80(i),i8(i,j))
      enddo
    enddo
    write (6,fmt="('Expected  int*8 min are ',3i18)") ans_i80
    ! send out the data and answers
    do i = 1,n_pes
      int80(:)=i8(:,i)
      call shmem_put8(int8,int80,nr,i-1)    ! data
      call shmem_put8(ans_i8,ans_i80,nr,i-1)  ! expected answers
    enddo 
                  endif
  call shmem_barrier_all
  call shmem_int8_min_to_all (int8min,int8,nr,0,0,n_pes,pwrk_i8,psync)
! write (6,fmt="('Results on PE ',i5,' are ',3i18)") my_pe,int8min
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (int8min(i) .ne. ans_i8(i)) then
      print *, 'FAIL int*8 - PE=',my_pe,' min=',int8min(i),' ans=',ans_i8(i)
                                    endif
  enddo

  call shmem_barrier_all
#ifdef NEEDS_FINALIZE
  call shmem_finalize
#endif
  end
