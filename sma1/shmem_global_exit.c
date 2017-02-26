/**********************************************************************
! Purpose:  Functional tests for shmem_global_exit() routine
!           1. checks whether the shmem_global_exit() broadcasts message 
!              to the other PEs and force terminates the program
!           2. checks whether the force termination works when all the 
!              PEs call the global_exit routine and when just one 
!              particular PE calls the routine
!
! Compile:  cc -D<PARAMS> test_shmem_global_exit()
!           PARAMS: ALLPES - call shmem_global_exit() from all PEs
!                   ONEPE  - call shmem_global_exit() from one PE
!
! Execute:  aprun -n<> -N<> ./sma <SHMEM_EXIT_VALUE|PE_ID>          
!**********************************************************************/
#include <stdio.h>
#include <shmem.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv) {
    shmem_init();
    int i = 0;
    int me = shmem_my_pe();
    int ne = shmem_n_pes();

    if (argc != 2) {
        if (me == 0) {
            printf("Program terminating, check whether all the required " \
                    "arguments are provided\n");
        }
        exit(0);
    } else {
#ifdef ALLPES
        int value = atoi(argv[1]);
#elif ONEPE
        int pe_call = atoi(argv[1]);
#endif

#ifdef ALLPES
        shmem_global_exit(value);
#elif ONEPE
        if (me == (pe_call-1)) {
            shmem_global_exit(me);
        }
        shmem_barrier_all();
#endif

        /*
         * Creating a scenario where different PEs are in different states
         * and test for whether all the PEs in the different states are halted
         * when the shmem_global_exit() is initiated. As the number of PEs are
         * fixed in the input case(TLIST), we have provided the different 
         * states for the PEs which donot initiate the shmem_global_exit()
         * */
        if (me == 0) {
            int count = 0;
            for (i = 0; i < 10000; i++) {
                count += 1;
            }
        } else if (me == 1) {
            sleep(3);
        } else if (me == 2) {
            sleep(1);
        }
    }
    shmem_finalize();
    return 0;
}
