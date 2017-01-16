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
!           shmem_integer_put
!           shmem_putmem
!           shmem_put4
!           shmem_put8
!           shmem_put32
!           shmem_put64
!           shmem_put 
!
!     Need -DSHMEM_FORTRN_GENERIC_64  if using  -s default64 compiler option (or equivalent)
!
!**********************************************************************
      PROGRAM put_tst_inc_integer
      implicit none     
      include 'mpp/shmem.fh'
      integer pe, npes, lastpe, i, N, M
      integer ierror
      external shmem_put, shmem_putmem, shmem_integer_put
      external shmem_put4, shmem_put8 
      external shmem_put32, shmem_put64
      external subrrg, subr1g, subr2g, subr4g, subr8g
      parameter (N = 40*2048, M=128)

      integer          :: xrr, yrr, prerr, postrr, brr, prr, vrr
      common  /cmrr/ xrr(N), prerr(M), yrr(N), postrr(M)
      integer (kind=1) :: xr1, yr1, prer1, postr1, br1, pr1, vr1
      common /cmr1/ xr1(N), prer1(M), yr1(N), postr1(M)
      integer (kind=2) :: xr2, yr2, prer2, postr2, br2, pr2, vr2
      common /cmr2/ xr2(N), prer2(M), yr2(N), postr2(M)      
      integer (kind=4) :: xr4, yr4, prer4, postr4, br4, pr4, vr4
      common /cmr4/ xr4(N), prer4(M), yr4(N), postr4(M)
      integer (kind=8) :: xr8, yr8, prer8, postr8, br8, pr8, vr8
      common /cmr8/ xr8(N), prer8(M), yr8(N), postr8(M)

#ifdef OPENSHMEM_FORT_SHORT_HEADER
      integer SHMEM_MY_PE, SHMEM_N_PES
#endif

      vrr=1;       prr=-1;       brr=-100;
      vr1=1;       pr1=-1;       br1=-100;
      vr2=1;       pr2=-1;       br2=-100;
      vr4=1;       pr4=-1;       br4=-100;
      vr8=1;       pr8=-1;       br8=-100;

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
!     Test integer call
      call subrr ( 111,   N,N,subrrg       )   ! call to generic shmem_put
      call subrr ( 112,   N,N,shmem_integer_put )

!     Test integer (kind=1) calls
!     call subr1 ( 211,   N,N,subr1g       )   ! call to generic shmem_put
      call subr1 ( 212,   N,N,shmem_putmem )   ! len is in bytes
 
!     Test integer (kind=2) calls
!     call subr2 ( 311,   N,N,subr2g       )   ! call to generic shmem_put
      call subr2 ( 312, 2*N,N,shmem_putmem )   ! len is in bytes

!     Test integer (kind=4) calls
!     call subr4 ( 411,   N,N,subr4g       )   ! call to generic shmem_put
      call subr4 ( 412,   N,N,shmem_put4   )
      call subr4 ( 413,   N,N,shmem_put32  )
      call subr4 ( 414, 4*N,N,shmem_putmem )   ! len is in bytes

!     Test integer (kind=8) calls
      call subr8 ( 511,   N,N,subr8g       )   ! call to generic shmem_put
      call subr8 ( 512,   N,N,shmem_put8   )
      call subr8 ( 513,   N,N,shmem_put64  )
      call subr8 ( 514, 8*N,N,shmem_putmem )   ! len is in bytes

#else

!     tests using default compiler options  REAL==REAL*4  INTEGER==INTEGER*4
!     Test integer call
      call subrr ( 111,   N,N,subrrg       )   ! call to generic shmem_put
      call subrr ( 112,   N,N,shmem_integer_put )

!     Test integer (kind=1) calls
!     call subr1 ( 211,   N,N,subr1g       )   ! call to generic shmem_put
      call subr1 ( 212,   N,N,shmem_putmem )   ! len is in bytes

!     Test integer (kind=2) calls
!     call subr2 ( 311,   N,N,subr2g       )   ! call to generic shmem_put
      call subr2 ( 312, 2*N,N,shmem_putmem )   ! len is in bytes

!     Test integer (kind=4) calls
!     call subr4 ( 411,   N,N,subr4g       )   ! call to generic shmem_put
      call subr4 ( 412,   N,N,shmem_put4   )
      call subr4 ( 413,   N,N,shmem_put32  )
      call subr4 ( 414, 4*N,N,shmem_putmem )   ! len is in bytes

!     Test integer (kind=8) calls
      call subr8 ( 511, 2*N,N,subr8g       )   ! call to generic shmem_put
      call subr8 ( 512,   N,N,shmem_put8   )
      call subr8 ( 513,   N,N,shmem_put64  )
      call subr8 ( 514, 8*N,N,shmem_putmem )   ! len is in bytes

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
      integer       rr(n),v4
      integer n,k
      lp: do i = 1,N
           if (rr(i) .ne. v4 ) then
             print *, 'FAIL: case k=',k,'  pe=',pe,'  i=',i,'  rr(i)=',rr(i)
             exit lp
           endif
          enddo lp
      return
    end subroutine checkrr

    subroutine subr1 (k,len,nck,subname)
      integer k,len,nck
      xr1=vr1;  yr1=pr1;  prer1=br1;  postr1=br1;
      call shmem_barrier_all()
      if(pe.eq.0) call subname (yr1,xr1,len,lastpe)
      call shmem_barrier_all()
      if(pe.eq.lastpe) then
        call checkr1 ( prer1,  M,br1,k)
        call checkr1 (   yr1,nck,vr1,k)
        call checkr1 (postr1,  M,br1,k)
      endif
      call shmem_barrier_all()
      return
    end subroutine subr1
    subroutine checkr1 (r1,n,v4,k)
      integer (kind=1) r1(n),v4
      integer n,k
      lp: do i = 1,N
           if (r1(i) .ne. v4 ) then
             print *, 'FAIL: case k=',k,'  pe=',pe,'  i=',i,'  r1(i)=',r1(i)
             exit lp
           endif
          enddo lp
      return
    end subroutine checkr1

    subroutine subr2 (k,len,nck,subname)
      integer k,len,nck
      xr2=vr2;  yr2=pr2;  prer2=br2;  postr2=br2;
      call shmem_barrier_all()
      if(pe.eq.0) call subname (yr2,xr2,len,lastpe)
      call shmem_barrier_all()
      if(pe.eq.lastpe) then
        call checkr2 ( prer2,  M,br2,k)
        call checkr2 (   yr2,nck,vr2,k)
        call checkr2 (postr2,  M,br2,k)
      endif
      call shmem_barrier_all()
      return
    end subroutine subr2
    subroutine checkr2 (r2,n,v4,k)
      integer (kind=2) r2(n),v4
      integer n,k
      lp: do i = 1,N
           if (r2(i) .ne. v4 ) then
             print *, 'FAIL: case k=',k,'  pe=',pe,'  i=',i,'  r2(i)=',r2(i)
             exit lp
           endif
          enddo lp
      return
    end subroutine checkr2

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
      integer (kind=4) r4(n),v4
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
      integer (kind=8) r8(n),v8
      integer n,k
      lp: do i = 1,N
           if (r8(i) .ne. v8 ) then
             print *, 'FAIL: case k=',k,'  pe=',pe,'  i=',i,'  r8(i)=',r8(i)
             exit lp
           endif
          enddo lp
      return
    end subroutine checkr8

    END

    subroutine subrrg (yrr,xrr,len,pe)
       include 'mpp/shmem.fh'
       integer len,pe
       integer       :: xrr, yrr
#ifdef OPENSHMEM
       call shmem_integer_put (yrr,xrr,len,pe)
#else
       call shmem_put (yrr,xrr,len,pe)
#endif
       return
    end subroutine subrrg

    subroutine subr1g (yr1,xr1,len,pe)
       include 'mpp/shmem.fh'
       integer len,pe
       integer (kind=1) :: xr1, yr1
#ifdef OPENSHMEM
       call shmem_integer_put (yr1,xr1,len,pe)
#else
       call shmem_put (yr1,xr1,len,pe)
#endif
       return
    end subroutine subr1g

    subroutine subr2g (yr2,xr2,len,pe)
       include 'mpp/shmem.fh'
       integer len,pe
       integer (kind=2) :: xr2, yr2
#ifdef OPENSHMEM
       call shmem_integer_put (yr2,xr2,len,pe)
#else
       call shmem_put (yr2,xr2,len,pe)
#endif
       return
    end subroutine subr2g

    subroutine subr4g (yr4,xr4,len,pe)
       include 'mpp/shmem.fh'
       integer len,pe
       integer (kind=4) :: xr4, yr4
#ifdef OPENSHMEM
       call shmem_integer_put (yr4,xr4,len,pe)
#else
       call shmem_put (yr4,xr4,len,pe)
#endif
       return
    end subroutine subr4g

    subroutine subr8g (yr8,xr8,len,pe)
       include 'mpp/shmem.fh'
       integer len,pe
       integer (kind=8) :: xr8, yr8
#ifdef OPENSHMEM
       call shmem_integer_put (yr8,xr8,len,pe)
#else
       call shmem_put (yr8,xr8,len,pe)
#endif
       return
    end subroutine subr8g
