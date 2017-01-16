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
! Purpose:  Functional tests for the following shmem_or routines 
!           using even/odd processors ---
!             shmem_int2_or_to_all
!             shmem_int4_or_to_all
!             shmem_int8_or_to_all
!
!**********************************************************************
  program test_shmem_or_to_all_even
  implicit none
  include "mpp/shmem.fh"
  integer psynce(shmem_reduce_sync_size),psynco(shmem_reduce_sync_size)
  data psynce /shmem_reduce_sync_size*shmem_sync_value/
  data psynco /shmem_reduce_sync_size*shmem_sync_value/
  integer, parameter :: nr=3
  integer :: n_pes,my_pe,i,j,ismax,nri2
  integer, allocatable :: iseed(:)
  integer*2 :: int2(nr),int2ore(nr),int20(nr),ans_i2(nr),ans_i20e(nr)
  integer*2 :: int2oro(nr),ans_i20o(nr)
  integer*2, allocatable :: i2(:,:)
  integer*2 :: pwrk_i2e(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  integer*2 :: pwrk_i2o(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comi2/ int2,int2ore,int2oro,ans_i2,pwrk_i2e,pwrk_i2o
  integer*4 :: int4(nr),int4ore(nr),int40(nr),ans_i4(nr),ans_i40e(nr)
  integer*4 :: int4oro(nr),ans_i40o(nr)
  integer*4, allocatable :: i4(:,:)
  integer*4 :: pwrk_i4e(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  integer*4 :: pwrk_i4o(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comi4/ int4,int4ore,int4oro,ans_i4,pwrk_i4e,pwrk_i4o
  integer*8 :: int8(nr),int8ore(nr),int80(nr),ans_i8(nr),ans_i80e(nr)
  integer*8 :: int8oro(nr),ans_i80o(nr)
  integer*8, allocatable :: i8(:,:)
  integer*8 :: pwrk_i8e(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  integer*8 :: pwrk_i8o(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comi8/ int8,int8ore,int8oro,ans_i8,pwrk_i8e,pwrk_i8o
  real*4, allocatable :: r4(:,:)
  real*8, allocatable :: r8(:,:)

#ifdef OPENSHMEM_FORT_SHORT_HEADER
  integer  shmem_my_pe, shmem_n_pes
#endif

  call shmem_init
  n_pes = shmem_n_pes()
  my_pe = shmem_my_pe()

#ifndef OPENSHMEM
! test shmem_int2_or_to_all
  if (my_pe == 0) then
    allocate (i2(nr,n_pes))
    allocate (r4(nr,n_pes))
    call random_seed(size=ismax)
    allocate (iseed(ismax))
    iseed = 12345
    call random_seed(put=iseed)
    call random_number(r4)
    i2=10000*r4
    ! create expected answers
    ans_i20e=i2(:,1)
    ans_i20o=i2(:,2)
    do i = 1,nr
      do j=3,n_pes
       if (mod(j,2) .eq. 1) then     !   doing only even pes (j=1 == pe=0)
        ans_i20e(i)=ior(ans_i20e(i),i2(i,j))
                            else
        ans_i20o(i)=ior(ans_i20o(i),i2(i,j))
                            endif
      enddo
    enddo
    write (6,fmt="('Expected  int*2  or-e are ',3i12)") ans_i20e
    write (6,fmt="('Expected  int*2  or-o are ',3i12)") ans_i20o
    ! send out the data and answers
    do i = 1,n_pes
      int20(:)=i2(:,i)
      call shmem_putmem(int2,int20,2*nr,i-1)    ! data
      if (mod(i,2) .eq. 1) then
        call shmem_putmem(ans_i2,ans_i20e,2*nr,i-1)  ! expected answers - even
                           else
        call shmem_putmem(ans_i2,ans_i20o,2*nr,i-1)  ! expected answers - odd
                           endif
    enddo 
    deallocate (r4)
                  endif
  call shmem_barrier_all
! print *, 'my_pe=',my_pe,' int2=',int2
! print *, 'my_pe=',my_pe,' ans_i2e=',ans_i2e
! print *, 'my_pe=',my_pe,' ans_i2o=',ans_i2o
  if (mod(my_pe,2) .eq. 0) then
  call shmem_int2_or_to_all (int2ore,int2,nr,0,1,  &
                              (n_pes+1)/2,pwrk_i2e,psynce)
! call shmem_int4_or_to_all (int2ore,int2,nr,0,1,  &
!                             (n_pes+1)/2,pwrk_i2e,psynce)
                           else
  call shmem_int2_or_to_all (int2oro,int2,nr,1,1,  &
                              (n_pes)/2,pwrk_i2o,psynco)
! call shmem_int4_or_to_all (int2oro,int2,nr,1,1,  &
!                             (n_pes)/2,pwrk_i2o,psynco)
                           endif
! if (mod(my_pe,2) .eq. 0) then
!   write (6,fmt="('Results on PE ',i5,' are ',3i12)") my_pe,int2ore
!                          else
!   write (6,fmt="('Results on PE ',i5,' are ',3i12)") my_pe,int2oro
!                          endif
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (mod(my_pe,2) .eq. 0) then
      if (int2ore(i) .ne. ans_i2(i)) &
        print *, 'FAIL int*2e - PE=',my_pe,' or=',int2ore(i),' ans=',ans_i2(i)
                             else
      if (int2oro(i) .ne. ans_i2(i)) &
        print *, 'FAIL int*2o - PE=',my_pe,' or=',int2oro(i),' ans=',ans_i2(i)
                             endif
  enddo
  call shmem_barrier_all
#endif

! test shmem_int4_or_to_all
  if (my_pe == 0) then
    allocate (i4(nr,n_pes))
    allocate (r4(nr,n_pes))
#ifdef OPENSHMEM
    call random_seed(size=ismax)
    allocate (iseed(ismax))
    iseed = 12345
#endif
    call random_seed(put=iseed)
    call random_number(r4)
    i4=1000000000*r4
    ! create expected answers
    ans_i40e=i4(:,1)
    ans_i40o=i4(:,2)
    do i = 1,nr
      do j=3,n_pes
       if (mod(j,2) .eq. 1) then     !   doing only even pes (j=1 == pe=0)
        ans_i40e(i)=ior(ans_i40e(i),i4(i,j))
                            else
        ans_i40o(i)=ior(ans_i40o(i),i4(i,j))
                            endif
      enddo
    enddo
    write (6,fmt="('Expected  int*4  or-e are ',3i12)") ans_i40e
    write (6,fmt="('Expected  int*4  or-o are ',3i12)") ans_i40o
    ! send out the data and answers
    do i = 1,n_pes
      int40(:)=i4(:,i)
      call shmem_put4(int4,int40,nr,i-1)    ! data
      if (mod(i,2) .eq. 1) then
        call shmem_put4(ans_i4,ans_i40e,nr,i-1)  ! expected answers - even
                           else
        call shmem_put4(ans_i4,ans_i40o,nr,i-1)  ! expected answers - odd
                           endif
    enddo 
    deallocate (r4)
                  endif
  call shmem_barrier_all
! print *, 'my_pe=',my_pe,' int4=',int4
! print *, 'my_pe=',my_pe,' ans_i4e=',ans_i4e
! print *, 'my_pe=',my_pe,' ans_i4o=',ans_i4o
  if (mod(my_pe,2) .eq. 0) then
  call shmem_int4_or_to_all (int4ore,int4,nr,0,1,  &
                              (n_pes+1)/2,pwrk_i4e,psynce)
                           else
  call shmem_int4_or_to_all (int4oro,int4,nr,1,1,  &
                              (n_pes)/2,pwrk_i4o,psynco)
                           endif
! if (mod(my_pe,2) .eq. 0) then
!   write (6,fmt="('Results on PE ',i5,' are ',3i12)") my_pe,int4ore
!                          else
!   write (6,fmt="('Results on PE ',i5,' are ',3i12)") my_pe,int4oro
!                          endif
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (mod(my_pe,2) .eq. 0) then
      if (int4ore(i) .ne. ans_i4(i)) &
        print *, 'FAIL int*4e - PE=',my_pe,' or=',int4ore(i),' ans=',ans_i4(i)
                             else
      if (int4oro(i) .ne. ans_i4(i)) &
        print *, 'FAIL int*4o - PE=',my_pe,' or=',int4oro(i),' ans=',ans_i4(i)
                             endif
  enddo
  call shmem_barrier_all

! test shmem_int8_or_to_all
  if (my_pe == 0) then
    allocate (i8(nr,n_pes))
    allocate (r8(nr,n_pes))
    call random_seed(put=iseed)
    call random_number(r8)
    i8=1000000000000_8*r8
    ! create expected answers
    ans_i80e=i8(:,1)
    ans_i80o=i8(:,2)
    do i = 1,nr
      do j=3,n_pes
       if (mod(j,2) .eq. 1) then     !   doing only even pes (j=1 == pe=0)
        ans_i80e(i)=ior(ans_i80e(i),i8(i,j))
                            else
        ans_i80o(i)=ior(ans_i80o(i),i8(i,j))
                            endif
      enddo
    enddo
    write (6,fmt="('Expected  int*8  or-e are ',3i18)") ans_i80e
    write (6,fmt="('Expected  int*8  or-o are ',3i18)") ans_i80o
    ! send out the data and answers
    do i = 1,n_pes
      int80(:)=i8(:,i)
      call shmem_put8(int8,int80,nr,i-1)    ! data
      if (mod(i,2) .eq. 1) then
        call shmem_put8(ans_i8,ans_i80e,nr,i-1)  ! expected answers - even
                           else
        call shmem_put8(ans_i8,ans_i80o,nr,i-1)  ! expected answers - odd
                           endif
    enddo 
    deallocate (r8)
                  endif
  call shmem_barrier_all
! print *, 'my_pe=',my_pe,' int8=',int8
! print *, 'my_pe=',my_pe,' ans_i8e=',ans_i8e
! print *, 'my_pe=',my_pe,' ans_i8o=',ans_i8o
  if (mod(my_pe,2) .eq. 0) then
  call shmem_int8_or_to_all (int8ore,int8,nr,0,1,  &
                              (n_pes+1)/2,pwrk_i8e,psynce)
                           else
  call shmem_int8_or_to_all (int8oro,int8,nr,1,1,  &
                              (n_pes)/2,pwrk_i8o,psynco)
                           endif
! if (mod(my_pe,2) .eq. 0) then
!   write (6,fmt="('Results on PE ',i5,' are ',3i18)") my_pe,int8ore
!                          else
!   write (6,fmt="('Results on PE ',i5,' are ',3i18)") my_pe,int8oro
!                          endif
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (mod(my_pe,2) .eq. 0) then
      if (int8ore(i) .ne. ans_i8(i)) &
        print *, 'FAIL int*8e - PE=',my_pe,' or=',int8ore(i),' ans=',ans_i8(i)
                             else
      if (int8oro(i) .ne. ans_i8(i)) &
        print *, 'FAIL int*8o - PE=',my_pe,' or=',int8oro(i),' ans=',ans_i8(i)
                             endif
  enddo

  call shmem_barrier_all
#ifdef NEEDS_FINALIZE
  call shmem_finalize
#endif
  end
