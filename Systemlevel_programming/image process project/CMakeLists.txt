cmake_minimum_required(VERSION 3.15)
project(image_process_project C)

set(CMAKE_C_STANDARD 11)

include_directories(.)

add_executable(image_process_project
        cache.c
        cache.h
        defs.h
        driver.c
        rotate.c
        smooth.c)
