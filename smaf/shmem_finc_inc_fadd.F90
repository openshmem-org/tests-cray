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
!           shmem_int4_fadd
!           shmem_int8_fadd
!           shmem_fadd
!
!**********************************************************************
      PROGRAM shmem_finc_inc_fadd
      implicit none     
      include 'mpp/shmem.fh'
      integer pe, npes, i, j, iter, nmax
      integer (kind=4) npesi
      integer (kind=8) npesl
      parameter (ITER=50, nmax=2048)

      integer          :: countg, modjg, oldjg, oldxmodjg, xg(0:nmax), valueg
      common /cmig/ countg, modjg, oldjg, oldxmodjg, xg, valueg
      integer (kind=4) :: counti, modji, oldji, oldxmodji, xi(0:nmax), valuei
      common /cmi4/ counti, modji, oldji, oldxmodji, xi, valuei
      integer (kind=8) :: countl, modjl, oldjl, oldxmodjl, xl(0:nmax), valuel
      common /cmi8/ countl, modjl, oldjl, oldxmodjl, xl, valuel

      call START_PES(0)
      pe=SHMEM_MY_PE()
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

!   test shmem_int4_fadd

      do i=0,npes-1
        xi(i) = 0
      enddo
      counti = 0
      valuei = 10
      call shmem_barrier_all()

      do i=0,ITER-1
        if (pe .ne. 0) then
          oldji = shmem_int4_finc(counti, 0);  !  get index oldji from PE 0 
          npesi = npes
          modji = mod (oldji,(npesi-1_4))   ! PE 0 is just the counter/checker
            ! add 10 to value in xi(modji)
          oldxmodji = shmem_int4_fadd(xi(modji), valuei, 0)
          ! write (6,fmt="('PE=',i6,' oldji=',i8,' modji=',i8,&
          !               &' oldxmodji=',i8,' valuei=',i8)") &
          !                 pe,oldji,modji,oldxmodji,valuei
        endif
      enddo
      call shmem_barrier_all()

      if (pe .eq. 0) then
        do j=1, npes-1
          if (xi(j-1) .ne. valuei*ITER) then
            write (6,fmt="('FAIL PE',i6,' of ',i6,'--xi(',i6,')=',i8,&
                          &' expected=',i6)") &
                            pe,npes,j-1,xi(j-1),valuei*ITER
          endif
        enddo
      endif

!   test shmem_int8_fadd

      do i=0,npes-1
        xl(i) = 0
      enddo
      countl = 0
      valuel = 10
      call shmem_barrier_all()

      do i=0,ITER-1
        if (pe .ne. 0) then
          oldjl = shmem_int8_finc(countl, 0);  !  get index oldjl from PE 0
          npesl = npes
          modjl = mod (oldjl,(npesl-1))   ! PE 0 is just the counter/checker
            ! add 10 to value in xl(modjl)
          oldxmodjl = shmem_int8_fadd(xl(modjl), valuel, 0)
          ! write (6,fmt="('PE=',i6,' oldjl=',i8,' modjl=',i8,&
          !               &' oldxmodjl=',i8,' valuel=',i8)") &
          !                 pe,oldjl,modjl,oldxmodjl,valuel
        endif
      enddo
      call shmem_barrier_all()

      if (pe .eq. 0) then
        do j=1, npes-1
          if (xl(j-1) .ne. valuel*ITER) then
            write (6,fmt="('FAIL PE',i6,' of ',i6,'--xl(',i6,')=',i8,&
                          &' expected=',i6)") &
                            pe,npes,j-1,xl(j-1),valuel*ITER
          endif
        enddo
      endif

!   test shmem_fadd (GENERIC)

      do i=0,npes-1
        xg(i) = 0
      enddo
      countg = 0
      valueg = 10
      call shmem_barrier_all()

      do i=0,ITER-1
        if (pe .ne. 0) then
          oldjg = shmem_finc(countg, 0);  !  get index oldjg from PE 0
          modjg = mod (oldjg,(npes-1))    ! PE 0 is just the counter/checker
            ! add 10 to value in xg(modjg)
          oldxmodjg = shmem_fadd(xg(modjg), valueg, 0)
          ! write (6,fmt="('PE=',i6,' oldjg=',i8,' modjg=',i8,&
          !               &' oldxmodjg=',i8,' valueg=',i8)") &
          !                 pe,oldjg,modjg,oldxmodjg,valueg
        endif
      enddo
      call shmem_barrier_all()

      if (pe .eq. 0) then
        do j=1, npes-1
          if (xg(j-1) .ne. valueg*ITER) then
            write (6,fmt="('FAIL PE',i6,' of ',i6,'--xg(',i6,')=',i8,&
                          &' expected=',i6)") &
                            pe,npes,j-1,xg(j-1),valueg*ITER
          endif
        enddo
      endif

999   continue
      call shmem_barrier_all()
#ifdef NEEDS_FINALIZE
      call shmem_finalize()
#endif

!     ------------------------------------------------------------------
 1001 format(' Testing shmem_fadd()')
 1002 format(' npes=',i6,'  exceeds nmax=',i6,'  FAIL')
 1003 format(' Test requires more than 1 PE      FAIL')
      end
