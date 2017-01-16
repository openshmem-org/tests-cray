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
! Purpose:  Functional tests for the following shmem_put routines
!           using "include 'mpp/shmem.fh'" --
!           shmem_character_put
!           shmem_putmem
!           shmem_put4
!           shmem_put8
!           shmem_put 
!
!     Need -DSHMEM_FORTRN_GENERIC_64  if using  -s default64 compiler option (or equivalent)
!
!**********************************************************************
      PROGRAM put_tst_inc_character
      implicit none     
      include 'mpp/shmem.fh'
      integer pe, npes, lastpe, i, N, M
      integer ierror
      external shmem_put, shmem_putmem, shmem_character_put
      external shmem_put4, shmem_put8
      external subrrg, subr4g, subr8g, subr26g
      parameter (N = 40*2048, M=128)

      character         :: xrr, yrr, prerr, postrr, brr, prr, vrr
      common  /cmrr/ xrr(N), prerr(M), yrr(N), postrr(M)
      character (len=4) :: xr4, yr4, prer4, postr4, br4, pr4, vr4
      common /cmr4/ xr4(N), prer4(M), yr4(N), postr4(M)
      character (len=8) :: xr8, yr8, prer8, postr8, br8, pr8, vr8
      common /cmr8/ xr8(N), prer8(M), yr8(N), postr8(M)
      character*26     :: xr26,yr26,prer26,postr26,br26,pr26,vr26
      common /cmr26/ xr26(N), prer26(M), yr26(N), postr26(M)

#ifdef OPENSHMEM_FORT_SHORT_HEADER
      integer SHMEM_MY_PE, SHMEM_N_PES
#endif

      vrr='C';           prr='Z';             brr='B';
      vr4='ABCD';        pr4='    ';          br4='WXYZ';
      vr8='ABCDEFGH';    pr8='        ';      br8='STUVWXYZ';
      vr26='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      pr26='--------------------------'
      br26='abcdefghijklmnopqrstuvwxyz'

      call START_PES(0)
      pe=SHMEM_MY_PE()
      npes=SHMEM_N_PES()
      lastpe=npes-1

      if(pe.eq.0) then
          WRITE(*,1001)
          WRITE(*,1002) N,M
      endif

      call shmem_barrier_all()

#ifdef SHMEM_FORTRN_GENERIC_64

!     tests using  -s default64 compiler option (or equivalent)
!     Test character call
!     call subrr ( 111,   N,N,subrrg       )   ! call to generic shmem_put
      call subrr ( 112,   N,N,shmem_character_put )
  
!     Test character (len=4) calls
!     call subr4 ( 211,   N,N,subr4g       )   ! call to generic shmem_put
      call subr4 ( 212,   N,N,shmem_put4   )
      call subr4 ( 213, 4*N,N,shmem_putmem )   ! len is in bytes

!     Test character (len=8) calls
      call subr8 ( 311,   N,N,subr8g       )   ! call to generic shmem_put
      call subr8 ( 312,   N,N,shmem_put8   )
      call subr8 ( 313, 8*N,N,shmem_putmem )   ! len is in bytes

!     Test character*26 calls
!     call subr26 ( 411,26*N,N,subr26g      )  ! call to generic shmem_put
      call subr26 ( 412,26*N,N,shmem_putmem )  ! len is in bytes

#else

!     tests using default compiler options  REAL==REAL*4  INTEGER==INTEGER*4
!     Test character call
!     call subrr ( 111,   N,N,subrrg       )   ! call to generic shmem_put
      call subrr ( 112,   N,N,shmem_character_put )
 
#ifndef OPENSHMEM
!     Test character (len=4) calls
      call subr4 ( 211,   N,N,subr4g       )   ! call to generic shmem_put
      call subr4 ( 212,   N,N,shmem_put4   )

!     Test character (len=8) calls
      call subr8 ( 311, 2*N,N,subr8g       )   ! call to generic shmem_put
      call subr8 ( 312,   N,N,shmem_put8   )
#endif
      call subr4 ( 213, 4*N,N,shmem_putmem )   ! len is in bytes
      call subr8 ( 313, 8*N,N,shmem_putmem )   ! len is in bytes

!     Test character*26 calls
!     call subr26 ( 411,26*N,N,subr26g      )  ! call to generic shmem_put
      call subr26 ( 412,26*N,N,shmem_putmem )  ! len is in bytes


#endif

      call shmem_barrier_all()
#ifdef NEEDS_FINALIZE
      call shmem_finalize()
#endif

!     ------------------------------------------------------------------
 1001 format(' Testing shmem_put()'/)
 1002 format(' N = ',i5,' M = ',i5)
!     ------------------------------------------------------------------

  contains

    subroutine subrr (k,len,nck,subname)
      integer k,len,nck
      xrr=vrr;  yrr=prr;  prerr=brr;  postrr=brr;
      call shmem_barrier_all()
      if(pe.eq.0) call subname (yrr,xrr,len,lastpe)
      call shmem_barrier_all()
      if(pe.eq.lastpe) then
        call checkrr ( prerr,  M,brr,k)
        call checkrr (   yrr,nck,vrr,k)
        call checkrr (postrr,  M,brr,k)
      endif
      call shmem_barrier_all()
      return
    end subroutine subrr
    subroutine checkrr (rr,n,v4,k)
      character     rr(n),v4
      integer n,k
      lp: do i = 1,N
           if (rr(i) .ne. v4 ) then
             print *, 'FAIL: case k=',k,'  pe=',pe,'  i=',i,'  rr(i)=',rr(i)
             exit lp
           endif
          enddo lp
      return
    end subroutine checkrr

    subroutine subr4 (k,len,nck,subname)
      integer k,len,nck
      xr4=vr4;  yr4=pr4;  prer4=br4;  postr4=br4;
      call shmem_barrier_all()
      if(pe.eq.0) call subname (yr4,xr4,len,lastpe)
      call shmem_barrier_all()
      if(pe.eq.lastpe) then
        call checkr4 ( prer4,  M,br4,k)
        call checkr4 (   yr4,nck,vr4,k)
        call checkr4 (postr4,  M,br4,k)
      endif
      call shmem_barrier_all()
      return
    end subroutine subr4
    subroutine checkr4 (r4,n,v4,k)
      character (len=4) r4(n),v4
      integer n,k
      lp: do i = 1,N
           if (r4(i) .ne. v4 ) then
             print *, 'FAIL: case k=',k,'  pe=',pe,'  i=',i,'  r4(i)=',r4(i)
             exit lp
           endif
          enddo lp
      return
    end subroutine checkr4

    subroutine subr8 (k,len,nck,subname)
      integer k,len,nck
      xr8=vr8;  yr8=pr8;  prer8=br8;  postr8=br8;
      call shmem_barrier_all()
      if(pe.eq.0) call subname (yr8,xr8,len,lastpe)
      call shmem_barrier_all()
      if(pe.eq.lastpe) then
        call checkr8 ( prer8,  M,br8,k)
        call checkr8 (   yr8,nck,vr8,k)
        call checkr8 (postr8,  M,br8,k)
      endif
      call shmem_barrier_all()
      return
    end subroutine subr8
    subroutine checkr8 (r8,n,v8,k)
      character (len=8) r8(n),v8
      integer n,k
      lp: do i = 1,N
           if (r8(i) .ne. v8 ) then
             print *, 'FAIL: case k=',k,'  pe=',pe,'  i=',i,'  r8(i)=',r8(i)
             exit lp
           endif
          enddo lp
      return
    end subroutine checkr8

    subroutine subr26 (k,len,nck,subname)
      integer k,len,nck
      xr26=vr26;  yr26=pr26;  prer26=br26;  postr26=br26;
      call shmem_barrier_all()
      if(pe.eq.0) call subname (yr26,xr26,len,lastpe)
      call shmem_barrier_all()
      if(pe.eq.lastpe) then
        call checkr26 ( prer26,  M,br26,k)
        call checkr26 (   yr26,nck,vr26,k)
        call checkr26 (postr26,  M,br26,k)
      endif
      call shmem_barrier_all()
      return
    end subroutine subr26
    subroutine checkr26 (r26,n,v26,k)
      character*26     r26(n),v26
      integer n,k
      lp: do i = 1,N
           if (r26(i) .ne. v26 ) then
             print *, 'FAIL: case k=',k,'  pe=',pe,'  i=',i,'  r26(i)=',r26(i)
             exit lp
           endif
          enddo lp
      return
    end subroutine checkr26

    END

    subroutine subrrg (yrr,xrr,len,pe)
       include 'mpp/shmem.fh'
       integer len,pe
       character     :: xrr, yrr
#ifndef OPENSHMEM
       call shmem_put (yrr,xrr,len,pe)
#endif
       return
    end subroutine subrrg

    subroutine subr4g (yr4,xr4,len,pe)
       include 'mpp/shmem.fh'
       integer len,pe
       character (len=4) :: xr4, yr4
#ifndef OPENSHMEM
       call shmem_put (yr4,xr4,len,pe)
#endif
       return
    end subroutine subr4g

    subroutine subr8g (yr8,xr8,len,pe)
       include 'mpp/shmem.fh'
       integer len,pe
       character (len=8) :: xr8, yr8
#ifndef OPENSHMEM
       call shmem_put (yr8,xr8,len,pe)
#endif
       return
    end subroutine subr8g

    subroutine subr26g (yr26,xr26,len,pe)
       include 'mpp/shmem.fh'
       integer len,pe
       character*26     :: xr26, yr26
#ifndef OPENSHMEM
       call shmem_put (yr26,xr26,len,pe)
#endif
       return
    end subroutine subr26g
