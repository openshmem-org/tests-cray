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
!           shmem_double_put
!           shmem_real_put
!           shmem_putmem
!           shmem_put4
!           shmem_put8
!           shmem_put32
!           shmem_put64
!           shmem_put128
!           shmem_put 
!
!     Need -DSHMEM_FORTRN_GENERIC_64  if using  -s default64 compiler option (or equivalent)
!
!**********************************************************************
      PROGRAM put_tst_inc_real
      implicit none     
      include 'mpp/shmem.fh'
      integer pe, npes, lastpe, i, N, M
      integer ierror
      external shmem_put, shmem_putmem, shmem_real_put, shmem_double_put
      external shmem_put4, shmem_put8 
      external shmem_put32, shmem_put64, shmem_put128
      external subrrg, subr4g, subrdg, subr8g, subr16g
      parameter (N = 40*2048, M=128)

      real             :: xrr, yrr, prerr, postrr, brr, prr, vrr
      common  /cmrr/ xrr(N), prerr(M), yrr(N), postrr(M)
      real (kind=4)    :: xr4, yr4, prer4, postr4, br4, pr4, vr4
      common  /cmr4/ xr4(N), prer4(M), yr4(N), postr4(M)
      double precision :: xrd, yrd, prerd, postrd, brd, prd, vrd
      common  /cmrd/ xrd(N), prerd(M), yrd(N), postrd(M)
      real (kind=8)    :: xr8, yr8, prer8, postr8, br8, pr8, vr8
      common  /cmr8/ xr8(N), prer8(M), yr8(N), postr8(M)
#ifdef HAVE_LONG_DOUBLE
      real (kind=16)   :: xr16, yr16, prer16, postr16, br16, pr16, vr16
      common /cmr16/ xr16(N), prer16(M), yr16(N), postr16(M)
#endif

#ifdef OPENSHMEM_FORT_SHORT_HEADER
      integer SHMEM_MY_PE, SHMEM_N_PES
#endif

      vrr=1.;     prr=-1.;     brr=-100.;
      vr4=1.;     pr4=-1.;     br4=-100.;
      vrd=1.d0;   prd=-1.d0;   brd=-100.d0;
      vr8=1.d0;   pr8=-1.d0;   br8=-100.d0;
#ifdef HAVE_LONG_DOUBLE
      vr16=1.d0;  pr16=-1.d0;  br16=-100.d0;
#endif

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
!     Test real call
      call subrr ( 111,   N,N,subrrg       )   ! call to generic shmem_put
      call subrr ( 112,   N,N,shmem_real_put )
  
!     Test real (kind=4) calls
!     call subr4 ( 211,   N,N,subr4g       )   ! call to generic shmem_put
      call subr4 ( 212,   N,N,shmem_put4   )
      call subr4 ( 213,   N,N,shmem_put32  )
      call subr4 ( 214, 4*N,N,shmem_putmem )   ! len is in bytes

!     Test double precision call
      call subrd ( 311, 2*N,N,subrdg       )   ! call to generic shmem_put
      call subrd ( 312,   N,N,shmem_double_put )

!     Test real (kind=8) calls
      call subr8 ( 411,   N,N,subr8g       )   ! call to generic shmem_put
      call subr8 ( 412,   N,N,shmem_put8   )
      call subr8 ( 413,   N,N,shmem_put64  )
      call subr8 ( 414, 8*N,N,shmem_putmem )   ! len is in bytes

#ifdef HAVE_LONG_DOUBLE
!     Test real (kind=16) calls
      call subr16( 511, 2*N,N,subr16g      )   ! call to generic shmem_put
      call subr16( 512,   N,N,shmem_put128 )
      call subr16( 513,16*N,N,shmem_putmem )   ! len is in bytes
#endif

#else

!     tests using default compiler options  REAL==REAL*4  INTEGER==INTEGER*4
!     Test real call
      call subrr ( 111,   N,N,subrrg       )   ! call to generic shmem_put
      call subrr ( 112,   N,N,shmem_real_put )
 
!     Test real (kind=4) calls
      call subr4 ( 211,   N,N,subr4g       )   ! call to generic shmem_put
      call subr4 ( 212,   N,N,shmem_put4   )
      call subr4 ( 213,   N,N,shmem_put32  )
      call subr4 ( 214, 4*N,N,shmem_putmem )   ! len is in bytes

!     Test double precision call
      call subrd ( 311, 2*N,N,subrdg       )   ! call to generic shmem_put
      call subrd ( 312,   N,N,shmem_double_put )

!     Test real (kind=8) calls
      call subr8 ( 411, 2*N,N,subr8g       )   ! call to generic shmem_put
      call subr8 ( 412,   N,N,shmem_put8   )
      call subr8 ( 413,   N,N,shmem_put64  )
      call subr8 ( 414, 8*N,N,shmem_putmem )   ! len is in bytes

#ifdef HAVE_LONG_DOUBLE
!     Test real (kind=16) calls
      call subr16( 511, 4*N,N,subr16g      )   ! call to generic shmem_put
      call subr16( 512,   N,N,shmem_put128 )
      call subr16( 513,16*N,N,shmem_putmem )   ! len is in bytes
#endif

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
      real          rr(n),v4
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
      real (kind=4) r4(n),v4
      integer n,k
      lp: do i = 1,N
           if (r4(i) .ne. v4 ) then
             print *, 'FAIL: case k=',k,'  pe=',pe,'  i=',i,'  r4(i)=',r4(i)
             exit lp
           endif
          enddo lp
      return
    end subroutine checkr4

    subroutine subrd (k,len,nck,subname)
      integer k,len,nck
      xrd=vrd;  yrd=prd;  prerd=brd;  postrd=brd;
      call shmem_barrier_all()
      if(pe.eq.0) call subname (yrd,xrd,len,lastpe)
      call shmem_barrier_all()
      if(pe.eq.lastpe) then
        call checkrd ( prerd,  M,brd,k)
        call checkrd (   yrd,nck,vrd,k)
        call checkrd (postrd,  M,brd,k)
      endif
      call shmem_barrier_all()
      return
    end subroutine subrd
    subroutine checkrd (rd,n,v8,k)
      double precision rd(n),v8
      integer n,k
      lp: do i = 1,N
           if (rd(i) .ne. v8 ) then
             print *, 'FAIL: case k=',k,'  pe=',pe,'  i=',i,'  rd(i)=',rd(i)
             exit lp
           endif
          enddo lp
      return
    end subroutine checkrd

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
      real (kind=8) r8(n),v8
      integer n,k
      lp: do i = 1,N
           if (r8(i) .ne. v8 ) then
             print *, 'FAIL: case k=',k,'  pe=',pe,'  i=',i,'  r8(i)=',r8(i)
             exit lp
           endif
          enddo lp
      return
    end subroutine checkr8

#ifdef HAVE_LONG_DOUBLE
    subroutine subr16 (k,len,nck,subname)
      integer k,len,nck
      xr16=vr16;  yr16=pr16;  prer16=br16;  postr16=br16;
      call shmem_barrier_all()
      if(pe.eq.0) call subname (yr16,xr16,len,lastpe)
      call shmem_barrier_all()
      if(pe.eq.lastpe) then
        call checkr16 ( prer16,  M,br16,k)
        call checkr16 (   yr16,nck,vr16,k)
        call checkr16 (postr16,  M,br16,k)
      endif
      call shmem_barrier_all()
      return
    end subroutine subr16
    subroutine checkr16 (r16,n,v16,k)
      real (kind=16) r16(n),v16
      integer n,k
      lp: do i = 1,N
           if (r16(i) .ne. v16 ) then
             print *, 'FAIL: case k=',k,'  pe=',pe,'  i=',i,'  r16(i)=',r16(i)
             exit lp
           endif
          enddo lp
      return
    end subroutine checkr16
#endif

    END

    subroutine subrrg (yrr,xrr,len,pe)
       include 'mpp/shmem.fh'
       integer len,pe
       real          :: xrr, yrr
#ifdef OPENSHMEM
       call shmem_real_put (yrr,xrr,len,pe)
#else
       call shmem_put (yrr,xrr,len,pe)
#endif
       return
    end subroutine subrrg

    subroutine subr4g (yr4,xr4,len,pe)
       include 'mpp/shmem.fh'
       integer len,pe
       real (kind=4) :: xr4, yr4
#ifdef OPENSHMEM
       call shmem_real_put (yr4,xr4,len,pe)
#else
       call shmem_put (yr4,xr4,len,pe)
#endif
       return
    end subroutine subr4g

    subroutine subrdg (yrd,xrd,len,pe)
       include 'mpp/shmem.fh'
       integer len,pe
       double precision :: xrd, yrd
#ifdef OPENSHMEM
       call shmem_real_put (yrd,xrd,len,pe)
#else
       call shmem_put (yrd,xrd,len,pe)
#endif
       return
    end subroutine subrdg

    subroutine subr8g (yr8,xr8,len,pe)
       include 'mpp/shmem.fh'
       integer len,pe
       real (kind=8) :: xr8, yr8
#ifdef OPENSHMEM
       call shmem_real_put (yr8,xr8,len,pe)
#else
       call shmem_put (yr8,xr8,len,pe)
#endif
       return
    end subroutine subr8g

#ifdef HAVE_LONG_DOUBLE
    subroutine subr16g (yr16,xr16,len,pe)
       include 'mpp/shmem.fh'
       integer len,pe
       real (kind=16) :: xr16, yr16
#ifdef OPENSHMEM
       call shmem_real_put (yr16,xr16,len,pe)
#else
       call shmem_put (yr16,xr16,len,pe)
#endif
       return
    end subroutine subr16g
#endif
