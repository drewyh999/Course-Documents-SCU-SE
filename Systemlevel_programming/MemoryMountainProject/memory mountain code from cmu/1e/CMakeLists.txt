cmake_minimum_required(VERSION 3.15)
project(1e C)

set(CMAKE_C_STANDARD 11)

include_directories(.)

add_executable(1e
        clock.c
        clock.h
        fcyc2.c
        fcyc2.h
        mountain.c)
