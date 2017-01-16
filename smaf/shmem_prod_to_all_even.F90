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
! Purpose:  Functional tests for the following shmem_prod routines 
!           using even processors ---
!             shmem_real4_prod_to_all
!             shmem_real8_prod_to_all
!             shmem_int2_prod_to_all
!             shmem_int4_prod_to_all
!             shmem_int8_prod_to_all
!
!**********************************************************************
  program test_shmem_prod_to_all_even
  implicit none
  include "mpp/shmem.fh"
  integer psync(shmem_reduce_sync_size)
  data psync /shmem_reduce_sync_size*shmem_sync_value/
  integer, parameter :: nr=3
  integer :: n_pes,my_pe,i,j,nri2
  integer :: n1,n2,n3,modit,modit2,modit4,m
  real*4 :: real4(nr),real4prod(nr),real40(nr),ans_r4(nr),ans_r40(nr)
  real*4, allocatable :: r4(:,:)
  real*4 :: dat_r4(0:7)=(/0.5,0.25,0.125,0.0625,16.,8.,4.,2./)
  real*4 :: pwrk_r4(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comr4/ real4,real4prod,ans_r4,pwrk_r4
  real*8 :: real8(nr),real8prod(nr),real80(nr),ans_r8(nr),ans_r80(nr)
  real*8, allocatable :: r8(:,:)
  real*8 :: dat_r8(0:7)=(/0.5d0,0.25d0,0.125d0,0.0625d0,16.d0,8.d0,4.d0,2.d0/)
  real*8 :: pwrk_r8(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comr8/ real8,real8prod,ans_r8,pwrk_r8
#ifdef HAVE_LONG_DOUBLE
  real*16 :: real16(nr),real16prod(nr),real160(nr),ans_r16(nr),ans_r160(nr)
  real*16, allocatable :: r16(:,:)
  real*16 :: dat_r16(0:7)=(/0.5d0,0.25d0,0.125d0,0.0625d0,16.d0,8.d0,4.d0,2.d0/)
  real*16 :: pwrk_r16(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comr16/ real16,real16prod,ans_r16,pwrk_r16
#endif
  integer*2 :: int2(nr),int2prod(nr),int20(nr),ans_i2(nr),ans_i20(nr)
  integer*2, allocatable :: i2(:,:)
  integer*2 :: pwrk_i2(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comi2/ int2,int2prod,ans_i2,pwrk_i2
  integer*4 :: int4(nr),int4prod(nr),int40(nr),ans_i4(nr),ans_i40(nr)
  integer*4, allocatable :: i4(:,:)
  integer*4 :: pwrk_i4(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comi4/ int4,int4prod,ans_i4,pwrk_i4
  integer*8 :: int8(nr),int8prod(nr),int80(nr),ans_i8(nr),ans_i80(nr)
  integer*8, allocatable :: i8(:,:)
  integer*8 :: pwrk_i8(max(nr/2+1,shmem_reduce_min_wrkdata_size))
  common /comi8/ int8,int8prod,ans_i8,pwrk_i8

#ifdef OPENSHMEM_FORT_SHORT_HEADER
  integer  shmem_my_pe, shmem_n_pes
#endif

  call shmem_init
  n_pes = shmem_n_pes()
  my_pe = shmem_my_pe()
  if (my_pe == 0) write (6,fmt="('Using ',i6,'  PEs')") n_pes

! test shmem_real4_prod_to_all
  real4prod(:)=111.0
  if (my_pe == 0) then
    allocate (r4(nr,n_pes))
    do i=1,n_pes
      n1=mod(i,8); n2=mod(i+1,8); n3=mod(i+2,8)
      r4(1,i)=dat_r4(n1); r4(2,i)=dat_r4(n2); r4(3,i)=dat_r4(n3);
    enddo
    ! create expected answers
    do i = 1,nr
      ans_r40(i)=1.0
      do j=1,n_pes
       if (mod(j,2) .eq. 1) then              !   doing only even pes
        ans_r40(i)=ans_r40(i)*r4(i,j)
                            endif
      enddo
    enddo
    write (6,fmt="('Expected real*4 prod are ',3f12.7)") ans_r40
    ! send out the data and answers
    do i = 1,n_pes
     if (mod(i,2) .eq. 1) then              !   doing only even pes
      real40(:)=r4(:,i)
      call shmem_put4(ans_r4,ans_r40,nr,i-1)  ! expected answers
                          else
      real40(:)=111.0
      call shmem_put4(ans_r4,real40,nr,i-1)  ! expected answers
                          endif
      call shmem_put4(real4,real40,nr,i-1)    ! data
    enddo 
                  endif
  call shmem_barrier_all
  if (mod(my_pe,2) .eq. 0) then
   call shmem_real4_prod_to_all (real4prod,real4,nr,0,1,  &
                                (n_pes+1)/2,pwrk_r4,psync)
                           endif
! if (my_pe .lt. 8) &
! write (6,fmt="('Results on PE ',i5,' are ',3f12.7)") my_pe,real4prod
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (abs(real4prod(i)-ans_r4(i)) .gt. ans_r4(i)*n_pes*epsilon(ans_r4)) then
      print *, 'FAIL real*4 - PE=',my_pe,' prod=',real4prod(i),' ans=',ans_r4(i)
                                                               endif
  enddo
  call shmem_barrier_all

! test shmem_real8_prod_to_all
  real8prod(:)=111.0d0
  if (my_pe == 0) then
    allocate (r8(nr,n_pes))
    do i=1,n_pes
      n1=mod(i,8); n2=mod(i+1,8); n3=mod(i+2,8)
      r8(1,i)=dat_r8(n1); r8(2,i)=dat_r8(n2); r8(3,i)=dat_r8(n3);
    enddo
    ! create expected answers
    do i = 1,nr
      ans_r80(i)=1.0d0
      do j=1,n_pes
       if (mod(j,2) .eq. 1) then              !   doing only even pes
        ans_r80(i)=ans_r80(i)*r8(i,j)
                            endif
      enddo
    enddo
    write (6,fmt="('Expected real*8 prod are ',3f18.13)") ans_r80
    ! send out the data and answers
    do i = 1,n_pes
     if (mod(i,2) .eq. 1) then              !   doing only even pes
      real80(:)=r8(:,i)
      call shmem_put8(ans_r8,ans_r80,nr,i-1)  ! expected answers
                          else
      real80(:)=111.0d0
      call shmem_put8(ans_r8,real80,nr,i-1)  ! expected answers
                          endif
      call shmem_put8(real8,real80,nr,i-1)    ! data
    enddo 
                  endif
  call shmem_barrier_all
  if (mod(my_pe,2) .eq. 0) then
   call shmem_real8_prod_to_all (real8prod,real8,nr,0,1,  &
                                (n_pes+1)/2,pwrk_r8,psync)
                           endif
! write (6,fmt="('Results on PE ',i5,' are ',3f18.13)") my_pe,real8prod
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (abs(real8prod(i)-ans_r8(i)) .gt. ans_r8(i)*n_pes*epsilon(ans_r8)) then
      print *, 'FAIL real*8 - PE=',my_pe,' prod=',real8prod(i),' ans=',ans_r8(i)
                                                               endif
  enddo
  call shmem_barrier_all

#ifdef HAVE_LONG_DOUBLE

! test shmem_real16_prod_to_all
  real16prod(:)=111.0d0
  if (my_pe == 0) then
    allocate (r16(nr,n_pes))
    do i=1,n_pes
      n1=mod(i,8); n2=mod(i+1,8); n3=mod(i+2,8)
      r16(1,i)=dat_r16(n1); r16(2,i)=dat_r16(n2); r16(3,i)=dat_r16(n3);
    enddo
    ! create expected answers
    do i = 1,nr
      ans_r160(i)=1.0d0
      do j=1,n_pes
       if (mod(j,2) .eq. 1) then              !   doing only even pes
        ans_r160(i)=ans_r160(i)*r16(i,j)
                            endif
      enddo
    enddo
    write (6,fmt="('Expected real*16 prod are ',3f18.13)") ans_r80
    ! send out the data and answers
    do i = 1,n_pes
     if (mod(i,2) .eq. 1) then              !   doing only even pes
      real160(:)=r16(:,i)
      call shmem_put128(ans_r16,ans_r160,nr,i-1)  ! expected answers
                          else
      real160(:)=111.0d0
      call shmem_put128(ans_r16,real160,nr,i-1)  ! expected answers
                          endif
      call shmem_put128(real16,real160,nr,i-1)    ! data
    enddo
                  endif
  call shmem_barrier_all
! print *,'my_pe,ans_r16=',my_pe,ans_r16
  if (mod(my_pe,2) .eq. 0) then
   call shmem_real16_prod_to_all (real16prod,real16,nr,0,1,  &
                                (n_pes+1)/2,pwrk_r16,psync)
                           endif
! write (6,fmt="('Results on PE ',i5,' are ',3f18.13)") my_pe,real16prod
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (abs(real16prod(i)-ans_r16(i)) .gt. ans_r16(i)*n_pes*epsilon(ans_r16)) then
      print *, 'FAIL real*16 - PE=',my_pe,' prod=',real16prod(i), &
               ' ans=',ans_r16(i)
                                                               endif
  enddo
  call shmem_barrier_all

#endif

#ifndef OPENSHMEM
! test shmem_int2_prod_to_all
  int2prod(:)=111
  if (my_pe == 0) then
    allocate (i2(nr,n_pes))
    if (n_pes .le. 16) then
      modit=2; modit2=4; modit4=8;
    else if (n_pes .lt. 80) then
      m=n_pes/16+1
      modit=2**m; modit2=modit*2; modit4=modit*4;
    else
      m=n_pes/16
      modit=n_pes/m;  modit2=n_pes/(m-2);  modit4=n_pes/(m-3);
    endif
    do i=1,n_pes
      i2(:,i)=1
      if (mod(i,modit ).eq.0) i2(1,i)=2
      if (mod(i,modit2).eq.1) i2(2,i)=2
      if (mod(i,modit4).eq.3) i2(3,i)=2
    enddo
    ! create expected answers
    do i = 1,nr
      ans_i20(i)=1
      do j=1,n_pes
       if (mod(j,2) .eq. 1) then              !   doing only even pes
        ans_i20(i)=ans_i20(i)*i2(i,j)
                            endif
      enddo
    enddo
    write (6,fmt="('Expected  int*2 prod are ',3i12)") ans_i20
    ! send out the data and answers
    nri2=2*nr          ! i2 implemented as 2 bytes
    do i = 1,n_pes
     if (mod(i,2) .eq. 1) then              !   doing only even pes
      int20(:)=i2(:,i)
      call shmem_putmem(ans_i2,ans_i20,nri2,i-1)  ! expected answers
                          else
      int20(:)=111
      call shmem_putmem(ans_i2,int20,nri2,i-1)  ! expected answers
                          endif
      call shmem_putmem(int2,int20,nri2,i-1)    ! data
    enddo
                  endif
  call shmem_barrier_all
  if (mod(my_pe,2) .eq. 0) then
!  call shmem_int4_prod_to_all (int2prod,int2,nr,0,1,  &
!                              (n_pes+1)/2,pwrk_i2,psync)
   call shmem_int2_prod_to_all (int2prod,int2,nr,0,1,  &
                               (n_pes+1)/2,pwrk_i2,psync)
                           endif
! write (6,fmt="('Results on PE ',i5,' are ',3i12)") my_pe,int2prod
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (int2prod(i) .ne. ans_i2(i)) then
      print *, 'FAIL int*2 - PE=',my_pe,' prod=',int2prod(i),' ans=',ans_i2(i)
                                    endif
  enddo
  call shmem_barrier_all
#endif

! test shmem_int4_prod_to_all
  int4prod(:)=111
  if (my_pe == 0) then
    allocate (i4(nr,n_pes))
    if (n_pes .le. 32) then
      modit=2; modit2=4; modit4=8;
    else if (n_pes .lt. 160) then
      m=n_pes/32+1
      modit=2**m; modit2=modit*2; modit4=modit*4;
    else
      m=n_pes/32
      modit=n_pes/m;  modit2=n_pes/(m-2);  modit4=n_pes/(m-3);
    endif
    do i=1,n_pes
      i4(:,i)=1
      if (mod(i,modit ).eq.0) i4(1,i)=2
      if (mod(i,modit2).eq.1) i4(2,i)=2
      if (mod(i,modit4).eq.3) i4(3,i)=2
    enddo
    ! create expected answers
    do i = 1,nr
      ans_i40(i)=1
      do j=1,n_pes
       if (mod(j,2) .eq. 1) then              !   doing only even pes
        ans_i40(i)=ans_i40(i)*i4(i,j)
                            endif
      enddo
    enddo
    write (6,fmt="('Expected  int*4 prod are ',3i12)") ans_i40
    ! send out the data and answers
    do i = 1,n_pes
     if (mod(i,2) .eq. 1) then              !   doing only even pes
      int40(:)=i4(:,i)
      call shmem_put4(ans_i4,ans_i40,nr,i-1)  ! expected answers
                          else
      int40(:)=111
      call shmem_put4(ans_i4,int40,nr,i-1)  ! expected answers
                          endif
      call shmem_put4(int4,int40,nr,i-1)    ! data
    enddo 
                  endif
  call shmem_barrier_all
  if (mod(my_pe,2) .eq. 0) then
   call shmem_int4_prod_to_all (int4prod,int4,nr,0,1,  &
                               (n_pes+1)/2,pwrk_i4,psync)
                           endif
! write (6,fmt="('Results on PE ',i5,' are ',3i12)") my_pe,int4prod
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (int4prod(i) .ne. ans_i4(i)) then
      print *, 'FAIL int*4 - PE=',my_pe,' prod=',int4prod(i),' ans=',ans_i4(i)
                                    endif
  enddo
  call shmem_barrier_all

! test shmem_int8_prod_to_all
  int8prod(:)=111
  if (my_pe == 0) then
    allocate (i8(nr,n_pes))
    if (n_pes .le. 64) then
      modit=2; modit2=4; modit4=8;
    else if (n_pes .lt. 320) then
      m=n_pes/64+1
      modit=2**m; modit2=modit*2; modit4=modit*4;
    else
      m=n_pes/64
      modit=n_pes/m;  modit2=n_pes/(m+1);  modit4=n_pes/(m+2);
    endif
    do i=1,n_pes
      i8(:,i)=1
      if (mod(i,modit ).eq.0) i8(1,i)=2
      if (mod(i,modit2).eq.1) i8(2,i)=2
      if (mod(i,modit4).eq.3) i8(3,i)=2
    enddo
    ! create expected answers
    do i = 1,nr
      ans_i80(i)=1
      do j=1,n_pes
       if (mod(j,2) .eq. 1) then              !   doing only even pes
        ans_i80(i)=ans_i80(i)*i8(i,j)
                            endif
      enddo
    enddo
    write (6,fmt="('Expected  int*8 prod are ',3i18)") ans_i80
    ! send out the data and answers
    do i = 1,n_pes
     if (mod(i,2) .eq. 1) then              !   doing only even pes
      int80(:)=i8(:,i)
      call shmem_put8(ans_i8,ans_i80,nr,i-1)  ! expected answers
                          else
      int80(:)=111
      call shmem_put8(ans_i8,int80,nr,i-1)  ! expected answers
                          endif
      call shmem_put8(int8,int80,nr,i-1)    ! data
    enddo 
                  endif
  call shmem_barrier_all
  if (mod(my_pe,2) .eq. 0) then
   call shmem_int8_prod_to_all (int8prod,int8,nr,0,1,  &
                               (n_pes+1)/2,pwrk_i8,psync)
                           endif
! write (6,fmt="('Results on PE ',i5,' are ',3i18)") my_pe,int8prod
  call shmem_barrier_all
  ! check answers
  do i = 1,nr
    if (int8prod(i) .ne. ans_i8(i)) then
      print *, 'FAIL int*8 - PE=',my_pe,' prod=',int8prod(i),' ans=',ans_i8(i)
                                    endif
  enddo

  call shmem_barrier_all
#ifdef NEEDS_FINALIZE
  call shmem_finalize
#endif
  end
