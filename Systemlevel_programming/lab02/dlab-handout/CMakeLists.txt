cmake_minimum_required(VERSION 3.14)
project(dlab_handout)

set(CMAKE_CXX_STANDARD 14)

include_directories(.)

add_executable(dlab_handout
        bits.c
        bits.h
        btest.c
        btest.h
        decl.c
        getopt.c
        getopt.h
        README
        tailor.h
        tests.c)
