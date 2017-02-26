/**********************************************************************
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
! Purpose:  	Test shmem reduction routine shmem_int_and_to_all
! 	The following cases are being tested:
!	1) The symmetric arrays of the following types:
!		global
!		local static
!		dynamically allocated in symmetric heap
!	2) The Source are Target are a) different b) the same array
!	3) The active set can be configured by redefining 3 macros:
!		MAXSTART  (0 <= PE_Start <=  MAXSTART)
!		MAXSTRIDE  (0 <= logPE_stride <=  MAXSTRIDE)
!		MAXRMLAST  (the last rmlast processes will be removed 
!			from the active set, 0 <= rmlast <=  MAXRMLAST)
!	The following checks are being done:
!	1) The Target is correct in processes in the active set
!	2) The Source has not been changed in processes in the active
!	   set in case Source and Target are different arrays
!	3) The Source and Target have not been changed in processes
!	   out of the active set
!	4) The values of pSync have been restored to the original values
!	   in processes in the active set
! 	There is a minimal number of synch calls - shmem_barrier_all is
!	called only after Source/Target initialization block.
!
!*********************************************************************/

#include <mpp/shmem.h>
#include <stdlib.h>
#include <stdio.h>

#ifndef NREDUCE
#define NREDUCE 7
#endif

#define PWRKELEM NREDUCE/2+_SHMEM_REDUCE_MIN_WRKDATA_SIZE
#define SINIT (i==0? 1 : (i+lpe)%2) /* don't redefine SINIT unless the calculation of chtarget is modified according to the new value of SINIT */
#define TINIT -500

#ifdef _FULLACTIVESETONLY
/* Set the next three macros to 0 to test the full active set only */
#define MAXSTART 0
#define MAXSTRIDE 0
#define MAXRMLAST 0
#else
#define MAXSTART 2
#define MAXSTRIDE 2
#define MAXRMLAST 1
#endif

int gSource_int[NREDUCE];
int gTarget_int[NREDUCE];
int *dSource_int;
int *dTarget_int;
int gpWrk_int[PWRKELEM];
int *dpWrk_int;

long gpSync[_SHMEM_REDUCE_SYNC_SIZE];
long *dpSync;

int and_int(int *Source, int *Target, int PE_start, int logPE_stride, int PE_size, int rstride, int SameST, int *pWrk, long *pSync, char *Case);
int check_sval_notchanged(int *array,char *Case);
int check_tval_notchanged(int *array,char *Case);

int main()
{
   int start,stride,rmlast,rstride,np_aset,inset,lpe;
   int my_pe,n_pes;
   int i,fail,n_err,asfail,nasfail=0;
   char Case[40];
   
   static int sSource_int[NREDUCE];
   static int sTarget_int[NREDUCE];
   static int spWrk_int[PWRKELEM];
   
   static long spSync[_SHMEM_REDUCE_SYNC_SIZE];


   shmem_init();
   my_pe = shmem_my_pe();
   n_pes = shmem_n_pes();
   lpe=my_pe;

   dpSync=shmem_malloc(_SHMEM_REDUCE_SYNC_SIZE*sizeof(long));
   for(i=0;i<_SHMEM_REDUCE_SYNC_SIZE;i++) {
      gpSync[i]=_SHMEM_SYNC_VALUE;
      dpSync[i]=_SHMEM_SYNC_VALUE;
      spSync[i]=_SHMEM_SYNC_VALUE;
   }
      
   dSource_int=shmem_malloc(NREDUCE*sizeof(int));
   dTarget_int=shmem_malloc(NREDUCE*sizeof(int));
   dpWrk_int=shmem_malloc((NREDUCE/2+1 > _SHMEM_REDUCE_MIN_WRKDATA_SIZE ? NREDUCE/2+1 : _SHMEM_REDUCE_MIN_WRKDATA_SIZE)*sizeof(int));

   for(start=0;start<=MAXSTART;start++) {
      rstride=1; 
      for(stride=0;stride<=MAXSTRIDE;stride++) {
         for(rmlast=0;rmlast<=MAXRMLAST;rmlast++)
	 {
	    np_aset=(n_pes+rstride-1-start)/rstride-rmlast; /* number of processes in the active set */
	    if(np_aset > 0) /* if active set is not empty */
	    {
	       if(my_pe==0) printf("\nActive set triplet: PE_start=%d,logPE_stride=%d,PE_size=%d \n",start,stride,np_aset);
	       if((my_pe>=start) && ((my_pe-start)%rstride==0) && ((my_pe-start)/rstride<np_aset)) inset=1;
	       else inset=0;

/* Initialize Source and Target arrays */
	       for(i=0;i<NREDUCE;i++) {
                  sSource_int[i]=SINIT;
                  sTarget_int[i]=TINIT;
                  gSource_int[i]=SINIT;
                  gTarget_int[i]=TINIT;
                  dSource_int[i]=SINIT;
                  dTarget_int[i]=TINIT;
	       }
               shmem_barrier_all();

/* CASE: static arrays, source is different from target */
               sprintf(Case,"static, source!=target");
	       if(inset) 
	          asfail=and_int(sSource_int,sTarget_int,start,stride,np_aset,rstride,0,dpWrk_int,gpSync,Case);
	       else {	/* check that values of source and target have not been changed */
	          nasfail+=check_sval_notchanged(sSource_int,Case);
		  nasfail+=check_tval_notchanged(sTarget_int,Case);
	       }
		  
	       
/* CASE: global arrays, source is different from target */
               sprintf(Case,"global, source!=target");
	       if(inset)
                  asfail=and_int(gSource_int,gTarget_int,start,stride,np_aset,rstride,0,spWrk_int,dpSync,Case);
	       else {	/* check that values of source and target have not been changed */
	          nasfail+=check_sval_notchanged(gSource_int,Case);
		  nasfail+=check_tval_notchanged(gTarget_int,Case);
	       }
	       
/* CASE: symmetric heap arrays, source is different from target */
               sprintf(Case,"sym heap, source!=target");
	       if(inset)
                  asfail=and_int(dSource_int,dTarget_int,start,stride,np_aset,rstride,0,gpWrk_int,spSync,Case);
	       else {	/* check that values of source and target have not been changed */
	          nasfail+=check_sval_notchanged(dSource_int,Case);
		  nasfail+=check_tval_notchanged(dTarget_int,Case);
	       }
	       

/* Reinitialize Source arrays for new tests */
	       for(i=0;i<NREDUCE;i++) {
                  sSource_int[i]=SINIT;
                  gSource_int[i]=SINIT;
                  dSource_int[i]=SINIT;
	       }
               shmem_barrier_all();

/* CASE: static arrays, source and target are the same array */
               sprintf(Case,"static, source==target");
	       if(inset)
                  asfail=and_int(sSource_int,sSource_int,start,stride,np_aset,rstride,1,gpWrk_int,dpSync,Case);
	       else 	/* check that values of source have not been changed */
	          nasfail+=check_sval_notchanged(sSource_int,Case);

/* CASE: global arrays, source and target are the same array */
               sprintf(Case,"global, source==target");
	       if(inset)
                  asfail=and_int(gSource_int,gSource_int,start,stride,np_aset,rstride,1,dpWrk_int,spSync,Case);
	       else 	/* check that values of source have not been changed */
	          nasfail+=check_sval_notchanged(gSource_int,Case);

/* CASE: symmetric heap arrays, source and target are the same array */
               sprintf(Case,"sym heap, source==target");
	       if(inset)
                  asfail=and_int(dSource_int,dSource_int,start,stride,np_aset,rstride,1,spWrk_int,gpSync,Case);
	       else 	/* check that values of source have not been changed */
	          nasfail+=check_sval_notchanged(dSource_int,Case);

	       
	    }	/* end of if active set is not empty */
	 } 	/* end of for loop on rmlast */
	 rstride*=2;
      } 	/* end of for loop on stride */
   } 		/* end of for loop on start */

   shmem_barrier_all();  
#ifdef NEEDS_FINALIZE
   shmem_finalize();
#endif
   return(0);
}


/* Test function for type int */
int and_int(int *Source, int *Target, int PE_start, int logPE_stride, int PE_size, int rstride, int SameST, int *pWrk, long *pSync, char *Case)
{
   int my_pe;
   int i,j,fail,n_err,ret_val,lpe;
   int chtarget;

   my_pe = shmem_my_pe();
   ret_val=0;

   shmem_int_and_to_all(Target, Source, NREDUCE, PE_start, logPE_stride, PE_size, pWrk, pSync);

/* check that values of pSync have been restored to the original values */
/*
   fail=0;
   n_err=0;
   for(i=0;i<_SHMEM_REDUCE_SYNC_SIZE;i++)
      if(pSync[i]!=_SHMEM_SYNC_VALUE) {
	 if(!fail) printf("FAIL pSync[%d]=%d (was %d) in process %d (in the active set), Case: %s\n",i,pSync[i],_SHMEM_SYNC_VALUE,my_pe,Case);
	 fail=1;
	 n_err++;
      }
   if(fail) ret_val+=n_err;
*/
/* if Source is different from Target check that values of Source have not been changed */
   if(!SameST) ret_val+=check_sval_notchanged(Source,Case);

/* Check the values of Target */
   fail=0;
   n_err=0;
   for(i=0;i<NREDUCE;i++)
   {
      chtarget=1;
      for(lpe=PE_start,j=0;j<PE_size;lpe+=rstride,j++)
         chtarget=chtarget && SINIT;
      if(Target[i]!=chtarget) {
	 if(!fail) printf("FAIL Target[%d]=%d (should be %d) in process %d (in the active set), Case: %s\n",i,Target[i],chtarget,my_pe,Case);
   	 fail=1;
 	 n_err++;
      }
   }
   if(fail) ret_val+=n_err;

   return ret_val;
}

int check_sval_notchanged(int *array,char *Case)
{
   int i,fail,n_err,my_pe,lpe;

   my_pe = shmem_my_pe();
   fail=0;
   n_err=0;
   lpe=my_pe;
   for(i=0;i<NREDUCE;i++)
      if(array[i]!=SINIT) {
	 if(!fail) printf("FAIL source[%d]=%d (was %d) in process %d, Case: %s\n",i,array[i],(int)(SINIT),my_pe,Case);
	 fail=1;
	 n_err++;
      }

   return n_err;
}

int check_tval_notchanged(int *array,char *Case)
{
   int i,fail,n_err,my_pe;

   my_pe = shmem_my_pe();
   fail=0;
   n_err=0;
   for(i=0;i<NREDUCE;i++)
      if(array[i]!=TINIT) {
	 if(!fail) printf("FAIL target[%d]=%d (was %d) in process %d, Case: %s\n",i,array[i],(int)(TINIT),my_pe,Case);
	 fail=1;
	 n_err++;
      }

   return n_err;
}

