cmake_minimum_required(VERSION 3.18 FATAL_ERROR)

project(dmr
    VERSION 0.3
    DESCRIPTION "Fortran Library for OpenMP Device Memory Routines"
    LANGUAGES Fortran)

###########################################################
# Build Type
###########################################################
set(default_build_type "Release")
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    message(STATUS "Setting build type to '${default_build_type}' as none was specified.")
    set(CMAKE_BUILD_TYPE "${default_build_type}"
        CACHE STRING "Choose the type of build." FORCE)
    # Set the possible values of build type for cmake-gui
    set_property(CACHE CMAKE_BUILD_TYPE
        PROPERTY
            STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

###########################################################
# Build helpers
###########################################################
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

############################################################
# Compiler vendor specific options and bug checks
############################################################
if(CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC")
    include(NVFortranCompiler)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    include(GNUFortranCompiler)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "IntelLLVM")
    include(IntelOneAPIFortranCompiler)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "XL")
    include(XLFortranCompiler)
endif()

###########################################################
# OpenMP
###########################################################
find_package(OpenMP REQUIRED)

###########################################################
# Build options
###########################################################
include(CMakeDependentOption)
option(DMR_ENABLE_REAL_128
    "enable support for 128 bits Real and Complex" OFF)
option(DMR_ENABLE_F2008
    "enable support for Fortran 2008 syntax" OFF)
option(DMR_ENABLE_OMP5.1
    "enable OpenMP 5.1 specifications syntax" ON)
option(DMR_ENABLE_TESTS
    "enable unit tests" OFF)
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_INSTALL_PREFIX}/mod")

###########################################################
# Dependencies
###########################################################
# Compiler dependencies
if(DMR_ENABLE_REAL_128 STREQUAL "ON")
    if(NOT(CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM"))
        message(FATAL_ERROR "128 bits reals are supported only by Intel LLVM Compiler")
    endif()
endif()

if(DMR_ENABLE_OMP5.1 STREQUAL "ON")
    if(NOT(CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM"))
        message(FATAL_ERROR "OpenMP 5.1 Specifications syntax is supported only by Intel LLVM Compiler")
    endif()
endif()

if(DMR_ENABLE_F2008 STREQUAL "ON")
    if(NOT(CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM"))
        message(FATAL_ERROR "Fortran 2008 syntax is supported only by Intel LLVM Compiler")
    endif()
endif()

###########################################################
# Check if src is present and if not, if sources can be generated
###########################################################
if(EXISTS ${CMAKE_BINARY_DIR}/src/lib)
   message(NOTICE "dmr sources are available in src directory: nothing has been generated from src_generator")
else()
   message(NOTICE "dmr sources not found: trying to generate files from src_generator")
   find_package (Python3 COMPONENTS Interpreter Development)
   if(Python3_FOUND)
      file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/src/lib)
      file(COPY_FILE ${CMAKE_SOURCE_DIR}/cmake/CMakeLists.src ${CMAKE_BINARY_DIR}/src/lib/CMakeLists.txt)
      execute_process(COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/src_generator/generate_library.py ${CMAKE_BINARY_DIR}/src/lib WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/src_generator)
      message(NOTICE "dmr sources generation completed")
   else()
      message(FATAL_ERROR "Python3 executable cannot be found: dmr sources cannot be generated")
   endif()
endif()

if(DMR_ENABLE_TESTS STREQUAL "ON")
   if(EXISTS ${CMAKE_BINARY_DIR}/src/tests)
      message(NOTICE "dmr tests are available in tests directory: nothing has been generated from src_generator")
   else()
      message(NOTICE "dmr tests not found: trying to generate files from src_generator")
      find_package (Python3 COMPONENTS Interpreter Development)
      if(Python3_FOUND)
         file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/src/tests)
         file(COPY_FILE ${CMAKE_SOURCE_DIR}/cmake/CMakeLists.tests ${CMAKE_BINARY_DIR}/src/tests/CMakeLists.txt)
         execute_process(COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/src_generator/generate_tests.py ${CMAKE_BINARY_DIR}/src/tests WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/src_generator)
         message(NOTICE "dmr tests generation completed")
      else()
         message(FATAL_ERROR "Python3 executable cannot be found: dmr tests cannot be generated")
      endif()
   endif()
endif()
###########################################################
# Add directories
###########################################################
add_subdirectory(${CMAKE_BINARY_DIR}/src/lib)
if(DMR_ENABLE_TESTS)
    add_subdirectory(${CMAKE_BINARY_DIR}/src/tests)
endif()
