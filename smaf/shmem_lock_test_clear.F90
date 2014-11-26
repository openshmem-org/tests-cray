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
! Purpose:  Functional tests for the following shmem_lock routines ---
!           shmem_set_lock 
!           shmem_test_lock
!           shmem_clear_lock
!
!*********************************************************************/
  program shmem_lock_test_clear
  implicit none
  include "mpp/shmem.fh"
  integer, parameter :: ITER=2
  integer(kind=8) count,lock
  common/longint/ count,lock
  integer :: i,j
  integer(kind=8) :: modj,oldj,oldxmodj,newcount
  integer(kind=8) :: x(1)    ! allocate via CRAY pointer & shpalloc
  integer :: my_pe,n_pes,ret_val,test_cnt
  integer :: errcode,abort=0
  integer :: max_elements_int
  pointer (addr,x)

  call start_pes(0)
  my_pe = shmem_my_pe()
  n_pes = shmem_n_pes()

!   fail if trying to use only one processor
  if ( n_pes .le. 1 ) then
        print *, "FAIL - test requires at least two PEs"
#ifdef NEEDS_FINALIZE
        call shmem_finalize()
#endif
        stop 111
  endif
  if (my_pe .eq. 0) &
    print *, "shmem_lock_test_clear    n_pes =", n_pes

!   shmalloc x for n_pes elements on all pes (only use the one on PE 0)

  select case (kind(i))
    case (4)                      !  default integer is i*4
      max_elements_int = 2 * n_pes    !  allocating for n_pes with integer*8
    case (8)                      !  default integer is i*8
      max_elements_int =     n_pes    !  allocating for n_pes with integer*8
  end select
  call shpalloc (addr,max_elements_int,errcode,abort)
  do  i=1, n_pes
    x(i) = 0 
  enddo
  count = 0 
  test_cnt = 0
  call shmem_barrier_all()

  do  i=1, ITER
    if (my_pe .ne. 0) then
      if (mod(my_pe,2) .eq. 0) then  !  my_pe is even and not zero
        !  spin on test_lock
        ret_val = 1
        do while ( ret_val .eq. 1 )
          ret_val = shmem_test_lock(lock)
          test_cnt = test_cnt + 1
        enddo
      else                           !  my_pe is odd
        call shmem_set_lock(lock)    !  just set_lock
      endif
!     print *, 'my_pe=',my_pe,' test_cnt=',test_cnt
      !  emulate  oldj = shmem_long_finc(&count, 0) starting wth lock above 
      call shmem_get8(oldj,count,1,0)      !  get oldj from PE 0's count
      newcount = oldj+1
      call shmem_put8(count,newcount,1,0)  !  update count on PE 0
      call shmem_quiet                     !  insure that write completes
      call shmem_clear_lock(lock)
      !  end of emulation
      modj = mod(oldj,int(n_pes-1,8))      !  PE 0 is counter/checker
      !  increment value in x(modj)
      oldxmodj = shmem_int8_finc(x(modj+1),0) 
!     write (*,fmt="('PE=',i6,', oldj=',i20,', modj=',i20,', oldxmodj=',i20)") &
!                     my_pe,oldj,modj,oldxmodj
    endif
  enddo
  call shmem_barrier_all()

  if (mod(my_pe,2) .eq. 0) then
    write (*,fmt="('PE=',i6,' test_cnt=',i12)") my_pe,test_cnt
  endif

  if (my_pe .eq. 0) then      !  check x(j) array on PE 0
    do  j=1, n_pes-1
      if (x(j) .ne. ITER) then
        write (6,fmt="('FAIL PE',i6,' of ',i6,': x(',i12,') = ',i12,&
                      &' expected = ',i12)") &
                       my_pe, n_pes, j, x(j), ITER
      endif
    enddo
  endif

  call shmem_barrier_all();
#ifdef NEEDS_FINALIZE
  call shmem_finalize(); 
#endif
  end
