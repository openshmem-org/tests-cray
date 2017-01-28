#!/bin/bash

#
# Copyright (c) 2015 Cray Inc.
#
# The contents of this file is proprietary information of Cray Inc.
# and may not be disclosed without prior written consent.
#

# basic configuration script
# setup and modify the configuration script as necessary


user_setup() {
    if [ -z $CRAY_TEST_PATH ]; then export CRAY_TEST_PATH=$PWD/testbin; fi
    if [ -z $CRAY_TEST_FILE ]; then export CRAY_TEST_FILE=$PWD; fi
    if [ -z $NPES ]; then export NPES=28; fi
    if [ -z $CC ]; then export CC=cc; fi
    if [ -z $CXX ]; then export CXX=CC; fi
    if [ -z $FTN ]; then export FTN=ftn; fi
    if [ -z $LAUNCHER ]; then LAUNCHER=aprun; fi
    if [ -z $CUSTOM_SHMEM_DIR ]; then
      export CUSTOM_SHMEM_DIR="/lus/scratch/nravi/opt/sma"
    fi
}

default_config() {
    if [ ! -z "$CUSTOM_SHMEM_DIR" ]; then
        export CFLAGS+="   -I${CUSTOM_SHMEM_DIR}/include/ "
        export CXXFLAGS+=" -I${CUSTOM_SHMEM_DIR}/include/ "
        export FFLAGS+="   -I${CUSTOM_SHMEM_DIR}/include/ "
        export FCFLAGS+="  -I${CUSTOM_SHMEM_DIR}/include/ "
        export CLIBS+="    -L${CUSTOM_SHMEM_DIR}/lib64 -lsma "
        export CXXLIBS+="  -L${CUSTOM_SHMEM_DIR}/lib64 -lsma "
        export FLIBS+="    -L${CUSTOM_SHMEM_DIR}/lib64 -lsma "
        export FCLIBS+="   -L${CUSTOM_SHMEM_DIR}/lib64 -lsma "
    fi
    
    export CRAY_TEST_RUN=$CRAY_TEST_PATH/run/             # main run directory
    export CRAY_TEST_RUN_OUT=$CRAY_TEST_PATH/run/out/     # run results are stored

    export CRAY_TEST_BUILD=$CRAY_TEST_PATH/build          # main build directory
    export CRAY_TEST_BUILD_BIN=$CRAY_TEST_PATH/build/bin  # build binaries are stored
    export CRAY_TEST_BUILD_OUT=$CRAY_TEST_PATH/build/out/ # build outputs are stored
}


create_testenv() {
    user_setup
    default_config
}

create_path() {
    mkdir -p $CRAY_TEST_PATH
    mkdir -p $CRAY_TEST_RUN
    mkdir -p $CRAY_TEST_RUN_OUT
    mkdir -p $CRAY_TEST_RUN_OUT/sma1
    mkdir -p $CRAY_TEST_RUN_OUT/sma2
    mkdir -p $CRAY_TEST_RUN_OUT/smaf
    mkdir -p $CRAY_TEST_BUILD
    mkdir -p $CRAY_TEST_BUILD_BIN
    mkdir -p $CRAY_TEST_BUILD_BIN/sma1
    mkdir -p $CRAY_TEST_BUILD_BIN/sma2
    mkdir -p $CRAY_TEST_BUILD_BIN/smaf
    mkdir -p $CRAY_TEST_BUILD_OUT
    mkdir -p $CRAY_TEST_BUILD_OUT/sma1
    mkdir -p $CRAY_TEST_BUILD_OUT/sma2
    mkdir -p $CRAY_TEST_BUILD_OUT/smaf
}

check_path() {
    if [ ! -d $CRAY_TEST_PATH  ]; then
        create_path
    else 
        rm -rf $CRAY_TEST_PATH
        create_path
    fi
}

create_testenv
check_path
