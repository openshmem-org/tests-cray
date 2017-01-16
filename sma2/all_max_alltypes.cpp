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
! Purpose:  	Test various shmem_max reduction routines for the
!		following data types:
!			double
!			float
!			int
!			long
!			long long
!			short
!			long double
!	The symmetric arrays are declared static global
! 	The following cases are being tested:
!	1) The Source are Target are a) different b) the same array
!	2) The active set can be configured by redefining 3 macros:
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
#include <typeinfo>
#include <cstring>
#include <iostream>
using namespace std;

#define NREDUCE 7
#define PWRKELEM NREDUCE/2+_SHMEM_REDUCE_MIN_WRKDATA_SIZE
#define SINIT (i-lpe) /* don't redefine SINIT unless the calculation of chtarget is fixed according to the new value of SINIT */
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

#ifdef HAVE_LONG_DOUBLE
static long double Source_longdbl[NREDUCE];

static long double Target_longdbl[NREDUCE];

static long double pWrk_longdbl[PWRKELEM];
#endif

static double Source_double[NREDUCE];
static float Source_float[NREDUCE];
static int Source_int[NREDUCE];
static long Source_long[NREDUCE];
static long long Source_longlong[NREDUCE];
static short Source_short[NREDUCE];

static double Target_double[NREDUCE];
static float Target_float[NREDUCE];
static int Target_int[NREDUCE];
static long Target_long[NREDUCE];
static long long Target_longlong[NREDUCE];
static short Target_short[NREDUCE];

static double pWrk_double[PWRKELEM];
static float pWrk_float[PWRKELEM];
static int pWrk_int[PWRKELEM];
static long pWrk_long[PWRKELEM];
static long long pWrk_longlong[PWRKELEM];
static short pWrk_short[PWRKELEM];

long gpSync1[_SHMEM_REDUCE_SYNC_SIZE];
long gpSync2[_SHMEM_REDUCE_SYNC_SIZE];

char Case[40];

template <class T> int check_sval_notchanged(T *);

/* Template test function to be called by the processes in active group */
template <class T> int max(T *Source, T *Target, T *pWrk, long *gpSync, int PE_start, int logPE_stride, int PE_size, int rstride, int SameST)
{
   int my_pe,n_pes;
   int i,j,fail,n_err,ret_val,lpe;
   T chtarget;
   
   my_pe = shmem_my_pe();
   n_pes = shmem_n_pes();
   ret_val=0;

   double comp_double;
   float comp_float;
   int comp_int;
   long comp_long;
   long long comp_longlong;
   short comp_short;
#ifdef HAVE_LONG_DOUBLE
   long double comp_longdbl;
#endif

/* If you modify the block below, make sure that strncmp for type long is called after strncmp for long long and long double */

   if(strncmp(typeid(Source[0]).name(),"double",6)==0) shmem_double_max_to_all((double *)Target, (double *)Source, NREDUCE, PE_start, logPE_stride, PE_size, (double *)pWrk, gpSync);
   else if(strncmp(typeid(Source[0]).name(),"float",5)==0) shmem_float_max_to_all((float *)Target, (float *)Source, NREDUCE, PE_start, logPE_stride, PE_size, (float *)pWrk, gpSync);
   else if(strncmp(typeid(Source[0]).name(),"int",3)==0) shmem_int_max_to_all((int *)Target, (int *)Source, NREDUCE, PE_start, logPE_stride, PE_size, (int *)pWrk, gpSync);
   else if(strncmp(typeid(Source[0]).name(),"long long",9)==0) shmem_longlong_max_to_all((long long *)Target, (long long *)Source, NREDUCE, PE_start, logPE_stride, PE_size, (long long *)pWrk, gpSync);
#ifdef HAVE_SHORT
   else if(strncmp(typeid(Source[0]).name(),"short",5)==0) shmem_short_max_to_all((short *)Target, (short *)Source, NREDUCE, PE_start, logPE_stride, PE_size, (short *)pWrk, gpSync);
#endif
#ifdef HAVE_LONG_DOUBLE
   else if(strncmp(typeid(Source[0]).name(),"long double",11)==0) shmem_longdouble_max_to_all((long double *)Target, (long double *)Source, NREDUCE, PE_start, logPE_stride, PE_size, (long double *)pWrk, gpSync);
   else if(typeid(Source[0]) == typeid(comp_longdbl)) shmem_longdouble_max_to_all((long double *)Target, (long double *)Source, NREDUCE, PE_start, logPE_stride, PE_size, (long double *)pWrk, gpSync);
#endif
   else if(strncmp(typeid(Source[0]).name(),"long",4)==0) shmem_long_max_to_all((long *)Target, (long *)Source, NREDUCE, PE_start, logPE_stride, PE_size, (long *)pWrk, gpSync);
   else if(typeid(Source[0]) == typeid(comp_double)) shmem_double_max_to_all((double *)Target, (double *)Source, NREDUCE, PE_start, logPE_stride, PE_size, (double *)pWrk, gpSync);
   else if(typeid(Source[0]) == typeid(comp_float)) shmem_float_max_to_all((float *)Target, (float *)Source, NREDUCE, PE_start, logPE_stride, PE_size, (float *)pWrk, gpSync);
   else if(typeid(Source[0]) == typeid(comp_int)) shmem_int_max_to_all((int *)Target, (int *)Source, NREDUCE, PE_start, logPE_stride, PE_size, (int *)pWrk, gpSync);
   else if(typeid(Source[0]) == typeid(comp_longlong)) shmem_longlong_max_to_all((long long *)Target, (long long *)Source, NREDUCE, PE_start, logPE_stride, PE_size, (long long *)pWrk, gpSync);
   else if(typeid(Source[0]) == typeid(comp_long)) shmem_long_max_to_all((long *)Target, (long *)Source, NREDUCE, PE_start, logPE_stride, PE_size, (long *)pWrk, gpSync);
#ifdef HAVE_SHORT
   else if(typeid(Source[0]) == typeid(comp_short)) shmem_short_max_to_all((short *)Target, (short *)Source, NREDUCE, PE_start, logPE_stride, PE_size, (short *)pWrk, gpSync);
#endif
   else cout << "FAIL type " << typeid(Source[0]).name() << " not recognized in the template test function" << endl;

/* check that values of pSync have been restored to the original values */
   fail=0;
   n_err=0;
   for(i=0;i<_SHMEM_REDUCE_SYNC_SIZE;i++)
      if(gpSync[i]!=_SHMEM_SYNC_VALUE) {
	 if(!fail) cout << "FAIL gpSync[" << i << "]=" << gpSync[i] << " (was " << _SHMEM_SYNC_VALUE << ") in process " << my_pe << ", Case: " << Case << endl;
	 fail=1;
	 n_err++;
      }
   if(fail) ret_val+=n_err;

/* if Source is different from Target check that values of Source have not been changed */
   if(!SameST) ret_val+=check_sval_notchanged(Source);

/* Check the values of Target */
   fail=0;
   n_err=0;
   for(i=0;i<NREDUCE;i++)
   {
      chtarget=-n_pes;
      for(lpe=PE_start,j=0;j<PE_size;lpe+=rstride,j++)
         chtarget=chtarget >= SINIT ? chtarget : SINIT;
      if(Target[i]-chtarget>1.e-8 || chtarget-Target[i]>1.e-8) {
	 if(!fail) cout << "FAIL target[" << i << "]=" << Target[i] << " (should be " << chtarget << ") in process " << my_pe << " (in the active set), Case: " << Case << endl;
   	 fail=1;
 	 n_err++;
      }
   }
   if(fail) ret_val+=n_err;

   return ret_val;
}

template <class T> int check_sval_notchanged(T *array)
{
   int i,fail,n_err,my_pe,lpe;

   my_pe = shmem_my_pe();
   fail=0;
   n_err=0;
   lpe=my_pe;
   for(i=0;i<NREDUCE;i++)
      if(array[i]!=SINIT) {
	 if(!fail) cout << "FAIL source[" << i << "]=" << array[i] << " (was " << (T)(SINIT) << ") in process " << my_pe << ", Case: " << Case << endl;
	 fail=1;
	 n_err++;
      }

   return n_err;
}

template <class T> int check_tval_notchanged(T *array)
{
   int i,fail,n_err,my_pe;

   my_pe = shmem_my_pe();
   fail=0;
   n_err=0;
   for(i=0;i<NREDUCE;i++)
      if(array[i]!=TINIT) {
	 if(!fail) cout << "FAIL target[" << i << "]=" << array[i] << " (was " << (T)(TINIT) << ") in process " << my_pe << ", Case: " << Case << endl;
	 fail=1;
	 n_err++;
      }

   return n_err;
}

int main()
{
   int start,stride,rmlast,rstride,np_aset,inset,lpe;
   int my_pe,n_pes;
   int i,asfail,nasfail;

   shmem_init();
   my_pe = shmem_my_pe();
   n_pes = shmem_n_pes();
   lpe=my_pe;

   asfail=nasfail=0;

   for(i=0;i<_SHMEM_REDUCE_SYNC_SIZE;i++) {
      gpSync1[i]=_SHMEM_SYNC_VALUE;
      gpSync2[i]=_SHMEM_SYNC_VALUE;
   }
   shmem_barrier_all();

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

               for(i=0;i<NREDUCE;i++) {
                  Source_double[i]=SINIT;
                  Source_float[i]=SINIT;
                  Source_int[i]=SINIT;
                  Source_long[i]=SINIT;
                  Source_longlong[i]=SINIT;
                  Source_short[i]=SINIT;
                  Target_double[i]=TINIT;
                  Target_float[i]=TINIT;
                  Target_int[i]=TINIT;
                  Target_long[i]=TINIT;
                  Target_longlong[i]=TINIT;
                  Target_short[i]=TINIT;
#ifdef HAVE_LONG_DOUBLE
                  Source_longdbl[i]=SINIT;
                  Target_longdbl[i]=TINIT;
#endif
	       }
               shmem_barrier_all();

/* CASE: type double, source is different from target */
	       sprintf(Case,"type double, source!=target");
	       if(inset) 
	          asfail+=max(Source_double,Target_double,pWrk_double,gpSync1,start,stride,np_aset,rstride,0);
	       else {	/* check that values of source and target have not been changed */
	          nasfail+=check_sval_notchanged(Source_double);
		  nasfail+=check_tval_notchanged(Target_double);
	       }
		  
/* CASE: type float, source is different from target */
	       sprintf(Case,"type float, source!=target");
	       if(inset) 
	          asfail+=max(Source_float,Target_float,pWrk_float,gpSync2,start,stride,np_aset,rstride,0);
	       else {	/* check that values of source and target have not been changed */
	          nasfail+=check_sval_notchanged(Source_float);
		  nasfail+=check_tval_notchanged(Target_float);
	       }
		  
/* CASE: type int, source is different from target */
	       sprintf(Case,"type int, source!=target");
	       if(inset) 
	          asfail+=max(Source_int,Target_int,pWrk_int,gpSync1,start,stride,np_aset,rstride,0);
	       else {	/* check that values of source and target have not been changed */
	          nasfail+=check_sval_notchanged(Source_int);
		  nasfail+=check_tval_notchanged(Target_int);
	       }
		  
/* CASE: type long, source is different from target */
	       sprintf(Case,"type long, source!=target");
	       if(inset) 
	          asfail+=max(Source_long,Target_long,pWrk_long,gpSync2,start,stride,np_aset,rstride,0);
	       else {	/* check that values of source and target have not been changed */
	          nasfail+=check_sval_notchanged(Source_long);
		  nasfail+=check_tval_notchanged(Target_long);
	       }
		  
/* CASE: type long long, source is different from target */
	       sprintf(Case,"type long long, source!=target");
	       if(inset) 
	          asfail+=max(Source_longlong,Target_longlong,pWrk_longlong,gpSync1,start,stride,np_aset,rstride,0);
	       else {	/* check that values of source and target have not been changed */
	          nasfail+=check_sval_notchanged(Source_longlong);
		  nasfail+=check_tval_notchanged(Target_longlong);
	       }
		  
/* CASE: type short, source is different from target */
#ifdef HAVE_SHORT
	       sprintf(Case,"type short, source!=target");
	       if(inset) 
	          asfail+=max(Source_short,Target_short,pWrk_short,gpSync2,start,stride,np_aset,rstride,0);
	       else {	/* check that values of source and target have not been changed */
	          nasfail+=check_sval_notchanged(Source_short);
		  nasfail+=check_tval_notchanged(Target_short);
	       }
#endif
		  
#ifdef HAVE_LONG_DOUBLE
/* CASE: type long double, source is different from target */
	       sprintf(Case,"type long double, source!=target");
	       if(inset) 
	          asfail+=max(Source_longdbl,Target_longdbl,pWrk_longdbl,gpSync1,start,stride,np_aset,rstride,0);
	       else {	/* check that values of source and target have not been changed */
	          nasfail+=check_sval_notchanged(Source_longdbl);
		  nasfail+=check_tval_notchanged(Target_longdbl);
	       }
#endif
  
	       
               for(i=0;i<NREDUCE;i++) {
                  Source_double[i]=SINIT;
                  Source_float[i]=SINIT;
                  Source_int[i]=SINIT;
                  Source_long[i]=SINIT;
                  Source_longlong[i]=SINIT;
                  Source_short[i]=SINIT;
#ifdef HAVE_LONG_DOUBLE
                  Source_longdbl[i]=SINIT;
#endif
	       }
	       shmem_barrier_all();

/* CASE: type double, source and target are the same array */
	       sprintf(Case,"type double, source==target");
	       if(inset) 
	          asfail+=max(Source_double,Source_double,pWrk_double,gpSync1,start,stride,np_aset,rstride,1);
	       else 	/* check that values of source have not been changed */
	          nasfail+=check_sval_notchanged(Source_double);
	       
/* CASE: type float, source and target are the same array */
	       sprintf(Case,"type float, source==target");
	       if(inset) 
	          asfail+=max(Source_float,Source_float,pWrk_float,gpSync2,start,stride,np_aset,rstride,1);
	       else 	/* check that values of source have not been changed */
	          nasfail+=check_sval_notchanged(Source_float);
	       
/* CASE: type int, source and target are the same array */
	       sprintf(Case,"type int, source==target");
	       if(inset) 
	          asfail+=max(Source_int,Source_int,pWrk_int,gpSync1,start,stride,np_aset,rstride,1);
	       else 	/* check that values of source have not been changed */
	          nasfail+=check_sval_notchanged(Source_int);
	       
/* CASE: type long, source and target are the same array */
	       sprintf(Case,"type long, source==target");
	       if(inset) 
	          asfail+=max(Source_long,Source_long,pWrk_long,gpSync2,start,stride,np_aset,rstride,1);
	       else 	/* check that values of source have not been changed */
	          nasfail+=check_sval_notchanged(Source_long);
	       
/* CASE: type long long, source and target are the same array */
	       sprintf(Case,"type long long, source==target");
	       if(inset) 
	          asfail+=max(Source_longlong,Source_longlong,pWrk_longlong,gpSync1,start,stride,np_aset,rstride,1);
	       else 	/* check that values of source have not been changed */
	          nasfail+=check_sval_notchanged(Source_longlong);
	       
/* CASE: type short, source and target are the same array */
#ifdef HAVE_SHORT
	       sprintf(Case,"type short, source==target");
	       if(inset) 
	          asfail+=max(Source_short,Source_short,pWrk_short,gpSync2,start,stride,np_aset,rstride,1);
	       else 	/* check that values of source have not been changed */
	          nasfail+=check_sval_notchanged(Source_short);
#endif
	       
#ifdef HAVE_LONG_DOUBLE
/* CASE: type long double, source and target are the same array */
	       sprintf(Case,"type long double, source==target");
	       if(inset) 
	          asfail+=max(Source_longdbl,Source_longdbl,pWrk_longdbl,gpSync1,start,stride,np_aset,rstride,1);
	       else 	/* check that values of source have not been changed */
	          nasfail+=check_sval_notchanged(Source_longdbl);
#endif
	       
	    }
	 } 	/* end of for loop on rmlast */
	 rstride*=2;
      } 	/* end of for loop on stride */
   } 		/* end of for loop on start */

   shmem_barrier_all();  /* sync sender and receiver */
#ifdef NEEDS_FINALIZE
   shmem_finalize();
#endif
   return asfail+nasfail;
}




