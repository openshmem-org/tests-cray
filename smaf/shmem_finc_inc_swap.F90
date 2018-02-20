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
! Purpose:  Functional tests for the following shmem routines
!           using "include 'mpp/shmem.fh'" --
!           shmem_int4_finc
!           shmem_int8_finc
!           shmem_finc
!           shmem_int4_swap
!           shmem_int8_swap
!           shmem_real4_swap
!           shmem_real8_swap
!           shmem_swap
!
!**********************************************************************
      PROGRAM shmem_finc_inc_swap
      implicit none     
      include 'mpp/shmem.fh'
      integer pe, npes, i, j, iter, nmax
      integer (kind=4) pei, npesi
      integer (kind=8) pel, npesl
#ifdef QUICK_TEST
      parameter (ITER=50, nmax=2048)
#else
      parameter (ITER=5, nmax=2048)
#endif

      integer          :: countg, modjg, oldjg, oldxmodjg, xg(0:nmax)
      integer          :: oldxag, xag(0:nmax*ITER)
      common /cmig/ countg, modjg, oldjg, oldxmodjg, xg, oldxag, xag
      integer (kind=4) :: counti, modji, oldji, oldxmodji, xi(0:nmax)
      integer (kind=4) :: oldxai, xai(0:nmax*ITER)
      common /cmi4/ counti, modji, oldji, oldxmodji, xi, oldxai, xai
      integer (kind=8) :: countl, modjl, oldjl, oldxmodjl, xl(0:nmax)
      integer (kind=8) :: oldxal, xal(0:nmax*ITER)
      common /cmi8/ countl, modjl, oldjl, oldxmodjl, xl, oldxal, xal
      real (kind=4)    :: valuef
      real (kind=4)    :: oldxmodjf, oldxaf, xf(0:nmax), xaf(0:nmax*ITER)
      common /cmr4/ oldxmodjf, oldxaf, xf, xaf
      real (kind=8)    :: valued
      real (kind=8)    :: oldxmodjd, oldxad, xd(0:nmax), xad(0:nmax*ITER)
      common /cmr8/ oldxmodjd, oldxad, xd, xad

#ifdef OPENSHMEM_FORT_SHORT_HEADER
      integer  SHMEM_MY_PE, SHMEM_N_PES
      integer(kind=4) shmem_int4_finc, shmem_int4_swap
      integer(kind=8) shmem_int8_finc, shmem_int8_swap
      integer(kind=4) shmem_real4_swap
      integer(kind=8) shmem_real8_swap
#endif

      call START_PES(0)
      pe=SHMEM_MY_PE();  pei=pe;  pel=pe;
      npes=SHMEM_N_PES()

      if(pe .eq. 0) then
          WRITE(*,1001)
          if (npes .gt. nmax) then
               WRITE(*,1002) npes, nmax
               go to 999   !  array dimensions too small
          endif
          if (npes .le. 1) then
               WRITE(*,1003)
               go to 999   !  need more than 1 processor
          endif
      endif

!   test shmem_int4_finc & shmem_int4_swap

      do i=0,npes-1
        xi(i) = 0
      enddo
      do i=0,npes*ITER-1
        xai(i) = 0
      enddo
      counti = 0
      call shmem_barrier_all()

      do i=0,ITER-1
        ! make sure each pe has to participate in the last iteration
        if (i .eq. ITER-1) call shmem_barrier_all()
        if (pe .ne. 0) then
          oldji = shmem_int4_finc(counti, 0);  !  get index oldji from PE 0 
          npesi = npes
          modji = mod (oldji,(npesi-1_4))   ! PE 0 is just the counter/checker
            ! record PE value in xi(modji)
          oldxmodji = shmem_int4_swap(xi(modji), pei, 0)
          ! write (6,fmt="('PE=',i6,' oldji=',i8,' modji=',i8,&
          !               &' oldxmodji=',i8)") &
          !                 pei,oldji,modji,oldxmodji
            ! record PE value in xai[oldji] -- tells PE involved for each count 
          oldxai = shmem_int4_swap(xai(oldji), pei, 0);
          ! write (6,fmt="('PE=',i6,' i=',i8,' oldji=',i8,' oldxai=',i8)") &
          !                 pei,i,oldji,oldxai
          if (oldxai .ne. 0) then
            write (6,fmt="('FAIL PE',i6,' of ',i6,'--i=',i8,&
                          &' oldxai=',i8,' expected=0')") &
                            pei,npes,i,oldxai
          endif
        endif
      enddo
      call shmem_barrier_all()

      if (pe .eq. 0) then  ! check last xi(j) array PEs vs saved ans in xai(i)
        i = (ITER-1)*(npes-1)
        do j=1, npes-1
          if (xi(j-1) .ne. xai(i)) then
            write (6,fmt="('FAIL PE',i6,' of ',i6,'--xi(',i6,')=',i8,&
                          &' expected=',i6)") &
                            pe,npes,j-1,xi(j-1),xai(i)
          endif
          i = i + 1
        enddo
      endif

!   test shmem_int8_finc & shmem_int8_swap

      do i=0,npes-1
        xl(i) = 0
      enddo
      do i=0,npes*ITER-1
        xal(i) = 0
      enddo
      countl = 0
      call shmem_barrier_all()

      do i=0,ITER-1
        ! make sure each pe has to participate in the last iteration
        if (i .eq. ITER-1) call shmem_barrier_all()
        if (pe .ne. 0) then
          oldjl = shmem_int8_finc(countl, 0);  !  get index oldjl from PE 0 
          npesl = npes
          modjl = mod (oldjl,(npesl-1_8))   ! PE 0 is just the counter/checker
            ! record PE value in xl(modjl)
          oldxmodjl = shmem_int8_swap(xl(modjl), pel, 0)
          ! write (6,fmt="('PE=',i6,' oldjl=',i8,' modjl=',i8,&
          !               &' oldxmodjl=',i8)") &
          !                 pel,oldjl,modjl,oldxmodjl
            ! record PE value in xal[oldjl] -- tells PE involved for each count 
          oldxal = shmem_int8_swap(xal(oldjl), pel, 0);
          ! write (6,fmt="('PE=',i6,' i=',i8,' oldjl=',i8,' oldxal=',i8)") &
          !                 pel,i,oldjl,oldxal
          if (oldxal .ne. 0) then
            write (6,fmt="('FAIL PE',i6,' of ',i6,'--i=',i8,&
                          &' oldxal=',i8,' expected=0')") &
                            pel,npes,i,oldxal
          endif
        endif
      enddo
      call shmem_barrier_all()

      if (pe .eq. 0) then  ! check last xl(j) array PEs vs saved ans in xal(i)
        i = (ITER-1)*(npes-1)
        do j=1, npes-1
          if (xl(j-1) .ne. xal(i)) then
            write (6,fmt="('FAIL PE',i6,' of ',i6,'--xl(',i6,')=',i8,&
                          &' expected=',i6)") &
                            pe,npes,j-1,xl(j-1),xal(i)
          endif
          i = i + 1
        enddo
      endif

!   test shmem_int4_finc & shmem_real4_swap

      do i=0,npes-1
        xf(i) = 0
      enddo
      do i=0,npes*ITER-1
        xaf(i) = 0
      enddo
      counti = 0
      call shmem_barrier_all()

      do i=0,ITER-1
        ! make sure each pe has to participate in the last iteration
        if (i .eq. ITER-1) call shmem_barrier_all()
        if (pe .ne. 0) then
          oldji = shmem_int4_finc(counti, 0);  !  get index oldji from PE 0 
          npesi = npes
          modji = mod (oldji,(npesi-1_4))   ! PE 0 is just the counter/checker
            ! record PE value in xi(modji)
          valuef = real(pe,4)
          oldxmodjf = shmem_real4_swap(xf(modji), valuef, 0)
          ! write (6,fmt="('PE=',i6,' oldji=',i8,' modji=',i8,&
          !               &' oldxmodjf=',f9.0)") &
          !                 pei,oldji,modji,oldxmodjf
            ! record PE value in xaf[oldji] -- tells PE involved for each count 
          oldxaf = shmem_real4_swap(xaf(oldji), valuef, 0);
          ! write (6,fmt="('PE=',i6,' i=',i8,' oldji=',i8,' oldxaf=',f9.0)") &
          !                 pe,i,oldji,oldxaf
          if (oldxaf .ne. real(0,4)) then
            write (6,fmt="('FAIL PE',i6,' of ',i6,'--i=',i8,&
                          &' oldxaf=',f9.0,' expected=0')") &
                            pe,npes,i,oldxaf
          endif
        endif
      enddo
      call shmem_barrier_all()

      if (pe .eq. 0) then  ! check last xi(j) array PEs vs saved ans in xai(i)
        i = (ITER-1)*(npes-1)
        do j=1, npes-1
          if (xf(j-1) .ne. xaf(i)) then
            write (6,fmt="('FAIL PE',i6,' of ',i6,'--xf(',i6,')=',f9.0,&
                          &' expected=',f9.0)") &
                            pe,npes,j-1,xf(j-1),xaf(i)
          endif
          i = i + 1
        enddo
      endif

!   test shmem_int8_finc & shmem_real8_swap

      do i=0,npes-1
        xd(i) = 0
      enddo
      do i=0,npes*ITER-1
        xad(i) = 0
      enddo
      countl = 0
      call shmem_barrier_all()

      do i=0,ITER-1
        ! make sure each pe has to participate in the last iteration
        if (i .eq. ITER-1) call shmem_barrier_all()
        if (pe .ne. 0) then
          oldjl = shmem_int8_finc(countl, 0);  !  get index oldjl from PE 0 
          npesl = npes
          modjl = mod (oldjl,(npesl-1_4))   ! PE 0 is just the counter/checker
            ! record PE value in xl(modjl)
          valued = real(pe,8)
          oldxmodjd = shmem_real8_swap(xd(modjl), valued, 0)
          ! write (6,fmt="('PE=',i6,' oldjl=',i8,' modjl=',i8,&
          !               &' oldxmodjd=',f9.0)") &
          !                 pel,oldjl,modjl,oldxmodjd
            ! record PE value in xad[oldjl] -- tells PE involved for each count 
          oldxad = shmem_real8_swap(xad(oldjl), valued, 0);
          ! write (6,fmt="('PE=',i6,' i=',i8,' oldjl=',i8,' oldxad=',i8)") &
          !                 pe,i,oldjl,oldxad
          if (oldxad .ne. real(0,8)) then
            write (6,fmt="('FAIL PE',i6,' of ',i6,'--i=',i8,&
                          &' oldxad=',f9.0,' expected=0')") &
                            pe,npes,i,oldxad
          endif
        endif
      enddo
      call shmem_barrier_all()

      if (pe .eq. 0) then  ! check last xd(j) array PEs vs saved ans in xad(i)
        i = (ITER-1)*(npes-1)
        do j=1, npes-1
          if (xd(j-1) .ne. xad(i)) then
            write (6,fmt="('FAIL PE',i6,' of ',i6,'--xd(',i6,')=',f9.0,&
                          &' expected=',f9.0)") &
                            pe,npes,j-1,xd(j-1),xad(i)
          endif
          i = i + 1
        enddo
      endif

#ifndef OPENSHMEM
!   test shmem_finc & shmem_swap (GENERIC integer only)

      do i=0,npes-1
        xg(i) = 0
      enddo
      do i=0,npes*ITER-1
        xag(i) = 0
      enddo
      countg = 0
      call shmem_barrier_all()

      do i=0,ITER-1
        ! make sure each pe has to participate in the last iteration
        if (i .eq. ITER-1) call shmem_barrier_all()
        if (pe .ne. 0) then
          oldjg = shmem_finc(countg, 0);  !  get index oldjg from PE 0 
          modjg = mod (oldjg,(npes-1))    ! PE 0 is just the counter/checker
            ! record PE value in xg(modjg)
          oldxmodjg = shmem_swap(xg(modjg), pe, 0)
          ! write (6,fmt="('PE=',i6,' oldjg=',i8,' modjg=',i8,&
          !               &' oldxmodjg=',i8)") &
          !                 pe,oldjg,modjg,oldxmodjg
            ! record PE value in xag[oldjg] -- tells PE involved for each count 
          oldxag = shmem_swap(xag(oldjg), pe, 0);
          ! write (6,fmt="('PE=',i6,' i=',i8,' oldjg=',i8,' oldxag=',i8)") &
          !                 pe,i,oldjg,oldxag
          if (oldxag .ne. 0) then
            write (6,fmt="('FAIL PE',i6,' of ',i6,'--i=',i8,&
                          &' oldxag=',i8,' expected=0')") &
                            pe,npes,i,oldxag
          endif
        endif
      enddo
      call shmem_barrier_all()

      if (pe .eq. 0) then  ! check last xg(j) array PEs vs saved ans in xag(i)
        i = (ITER-1)*(npes-1)
        do j=1, npes-1
          if (xg(j-1) .ne. xag(i)) then
            write (6,fmt="('FAIL PE',i6,' of ',i6,'--xg(',i6,')=',i8,&
                          &' expected=',i6)") &
                            pe,npes,j-1,xg(j-1),xag(i)
          endif
          i = i + 1
        enddo
      endif
#endif

999   continue
      call shmem_barrier_all()
#ifdef NEEDS_FINALIZE
      call shmem_finalize()
#endif

!     ------------------------------------------------------------------
 1001 format(' Testing shmem_swap()')
 1002 format(' npes=',i6,'  exceeds nmax=',i6,'  FAIL')
 1003 format(' Test requires more than 1 PE      FAIL')
      end
