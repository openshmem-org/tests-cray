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
! Purpose:  Functional tests for the following shmem_sum routines 
!           using ALL processors ---
!             shmem_real4_sum_to_all
!             shmem_real8_sum_to_all
!             shmem_int2_sum_to_all
!             shmem_int4_sum_to_all
!             shmem_int8_sum_to_all
!
!**********************************************************************
  program test_shmem_sum_to_all_all
  implicit none
  include "mpp/shmem.fh"
  integer psync(shmem_reduce_sync_size)
  data psync /shmem_reduce_sync_size*shmem_sync_value/
  integer, parameter :: nr=3
  integer :: n_pes,my_pe,i,j,ismax
  integer, allocatable :: iseed(:)
  real*4 :: real4(nr),real4sum(nr),real40(nr),ans_r4(nr),ans_r40(nr)
  real*4, allocatable :: r4(:,:)
  real*4 :: pwrk_r4(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comr4/ real4,real4sum,ans_r4,pwrk_r4
  real*8 :: real8(nr),real8sum(nr),real80(nr),ans_r8(nr),ans_r80(nr)
  real*8, allocatable :: r8(:,:)
  real*8 :: pwrk_r8(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comr8/ real8,real8sum,ans_r8,pwrk_r8
  integer*2 :: int2(nr),int2sum(nr),int20(nr),ans_i2(nr),ans_i20(nr)
  integer*2, allocatable :: i2(:,:)
  integer*2 :: pwrk_i2(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comi2/ int2,int2sum,ans_i2,pwrk_i2
  integer*4 :: int4(nr),int4sum(nr),int40(nr),ans_i4(nr),ans_i40(nr)
  integer*4, allocatable :: i4(:,:)
  integer*4 :: pwrk_i4(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comi4/ int4,int4sum,ans_i4,pwrk_i4
  integer*8 :: int8(nr),int8sum(nr),int80(nr),ans_i8(nr),ans_i80(nr)
  integer*8, allocatable :: i8(:,:)
  integer*8 :: pwrk_i8(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comi8/ int8,int8sum,ans_i8,pwrk_i8

#ifdef OPENSHMEM_FORT_SHORT_HEADER
  integer  shmem_my_pe, shmem_n_pes
#endif

  call shmem_init
  n_pes = shmem_n_pes()
  my_pe = shmem_my_pe()

! test shmem_real4_sum_to_all
  if (my_pe == 0) then
    allocate (r4(nr,n_pes))
    call random_seed(size=ismax)
    allocate (iseed(ismax))
    iseed = 12345
    call random_seed(put=iseed)
    call random_number(r4)
    ! create expected answers
    ans_r40=sum(r4,dim=2)
    write (6,fmt="('Expected real*4 sum are ',3f12.7)") ans_r40
    ! send out the data and answers
    do i = 1,n_pes
      real40(:)=r4(:,i)
      call shmem_put4(real4,real40,nr,i-1)    ! data
      call shmem_put4(ans_r4,ans_r40,nr,i-1)  ! expected answers
    enddo 
                  endif
  call shmem_barrier_all
  call shmem_real4_sum_to_all (real4sum,real4,nr,0,0,n_pes,pwrk_r4,psync)
! write (6,fmt="('Results on PE ',i5,' are ',3f12.7)") my_pe,real4sum
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (abs(real4sum(i)-ans_r4(i)) .gt. ans_r4(i)*n_pes*epsilon(ans_r4)) then
      print *, 'FAIL real*4 - PE=',my_pe,' sum=',real4sum(i),' ans=',ans_r4(i)
                                                               endif
  enddo
  call shmem_barrier_all

! test shmem_real8_sum_to_all
  if (my_pe == 0) then
    allocate (r8(nr,n_pes))
    call random_seed(put=iseed)
    call random_number(r8)
    ! create expected answers
    ans_r80=sum(r8,dim=2)
    write (6,fmt="('Expected real*8 sum are ',3f18.13)") ans_r80
    ! send out the data and answers
    do i = 1,n_pes
      real80(:)=r8(:,i)
      call shmem_put8(real8,real80,nr,i-1)    ! data
      call shmem_put8(ans_r8,ans_r80,nr,i-1)  ! expected answers
    enddo 
                  endif
  call shmem_barrier_all
  call shmem_real8_sum_to_all (real8sum,real8,nr,0,0,n_pes,pwrk_r8,psync)
! write (6,fmt="('Results on PE ',i5,' are ',3f18.13)") my_pe,real8sum
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (abs(real8sum(i)-ans_r8(i)) .gt. ans_r8(i)*n_pes*epsilon(ans_r8)) then
      print *, 'FAIL real*8 - PE=',my_pe,' sum=',real8sum(i),' ans=',ans_r8(i)
                                                               endif
  enddo
  call shmem_barrier_all

#ifndef OPENSHMEM
! test shmem_int2_sum_to_all
  if (my_pe == 0) then
    allocate (i2(nr,n_pes))
    call random_seed(put=iseed)
    call random_number(r4)
    i2=10*r4
    ! create expected answers
!   ans_i20=sum(i2,dim=2)
    do i=1,nr
      ans_i20(i)=0
      do j=1,n_pes
        ans_i20(i)=ans_i20(i)+i2(i,j)
      enddo
    enddo
    write (6,fmt="('Expected  int*2 sum are ',3i12)") ans_i20
    ! send out the data and answers
    do i = 1,n_pes
      int20(:)=i2(:,i)
      call shmem_putmem(int2,int20,2*nr,i-1)    ! data
      call shmem_putmem(ans_i2,ans_i20,2*nr,i-1)  ! expected answers
    enddo 
                  endif
  call shmem_barrier_all
  call shmem_int2_sum_to_all (int2sum,int2,nr,0,0,n_pes,pwrk_i2,psync)
! write (6,fmt="('Results on PE ',i5,' are ',3i12)") my_pe,int2sum
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (int2sum(i) .ne. ans_i2(i)) then
      print *, 'FAIL int*2 - PE=',my_pe,' sum=',int2sum(i),' ans=',ans_i2(i)
                                    endif
  enddo
  call shmem_barrier_all
#endif

! test shmem_int4_sum_to_all
  if (my_pe == 0) then
    allocate (i4(nr,n_pes))
    call random_seed(put=iseed)
    call random_number(r4)
    i4=100000*r4
    ! create expected answers
    ans_i40=sum(i4,dim=2)
    write (6,fmt="('Expected  int*4 sum are ',3i12)") ans_i40
    ! send out the data and answers
    do i = 1,n_pes
      int40(:)=i4(:,i)
      call shmem_put4(int4,int40,nr,i-1)    ! data
      call shmem_put4(ans_i4,ans_i40,nr,i-1)  ! expected answers
    enddo 
                  endif
  call shmem_barrier_all
  call shmem_int4_sum_to_all (int4sum,int4,nr,0,0,n_pes,pwrk_i4,psync)
! write (6,fmt="('Results on PE ',i5,' are ',3i12)") my_pe,int4sum
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (int4sum(i) .ne. ans_i4(i)) then
      print *, 'FAIL int*4 - PE=',my_pe,' sum=',int4sum(i),' ans=',ans_i4(i)
                                    endif
  enddo
  call shmem_barrier_all

! test shmem_int8_sum_to_all
  if (my_pe == 0) then
    allocate (i8(nr,n_pes))
    call random_seed(put=iseed)
    call random_number(r4)
    i8=1000000000000_8*r4
    ! create expected answers
    ans_i80=sum(i8,dim=2)
    write (6,fmt="('Expected  int*8 sum are ',3i18)") ans_i80
    ! send out the data and answers
    do i = 1,n_pes
      int80(:)=i8(:,i)
      call shmem_put8(int8,int80,nr,i-1)    ! data
      call shmem_put8(ans_i8,ans_i80,nr,i-1)  ! expected answers
    enddo 
                  endif
  call shmem_barrier_all
  call shmem_int8_sum_to_all (int8sum,int8,nr,0,0,n_pes,pwrk_i8,psync)
! write (6,fmt="('Results on PE ',i5,' are ',3i18)") my_pe,int8sum
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (int8sum(i) .ne. ans_i8(i)) then
      print *, 'FAIL int*8 - PE=',my_pe,' sum=',int8sum(i),' ans=',ans_i8(i)
                                    endif
  enddo

  call shmem_barrier_all
#ifdef NEEDS_FINALIZE
  call shmem_finalize
#endif
  end
