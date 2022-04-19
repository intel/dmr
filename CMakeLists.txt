cmake_minimum_required(VERSION 3.21 FATAL_ERROR)

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
 elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Cray")
    include(CrayFortranCompiler)
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

if (NOT CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_INSTALL_PREFIX}/mod")
endif()

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
if((EXISTS ${PROJECT_SOURCE_DIR}/src/lib))
   message(NOTICE "dmr sources are available in src directory of the project root directory: nothing has been generated from src_generator")
   # Add source directory
   add_subdirectory(${PROJECT_SOURCE_DIR}/src/lib)
elseif(EXISTS ${CMAKE_CURRENT_BINARY_DIR}/src/lib)
   message(NOTICE "dmr sources are available in src directory of the CMake build directory: nothing has been generated from src_generator")
   # Add source directory
   add_subdirectory(${CMAKE_CURRENT_BINARY_DIR}/src/lib)
else()
   message(NOTICE "dmr sources not found: trying to generate files from src_generator")
   find_package (Python3 COMPONENTS Interpreter Development)
   if(Python3_FOUND)
      file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/src/lib)
      file(COPY_FILE ${CMAKE_CURRENT_SOURCE_DIR}/cmake/CMakeLists.src ${CMAKE_CURRENT_BINARY_DIR}/src/lib/CMakeLists.txt)
      execute_process(COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/src_generator/generate_library.py ${CMAKE_CURRENT_BINARY_DIR}/src/lib WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src_generator)
      message(NOTICE "dmr sources generation completed")
      # Add source directory
      add_subdirectory(${CMAKE_CURRENT_BINARY_DIR}/src/lib ${CMAKE_BINARY_DIR}/src/lib)
   else()
      message(FATAL_ERROR "Python3 executable cannot be found: dmr sources cannot be generated")
   endif()
endif()

if(DMR_ENABLE_TESTS STREQUAL "ON")
   if(EXISTS ${PROJECT_SOURCE_DIR}/src/tests)
      message(NOTICE "dmr tests are available in tests directory of the project root directory: nothing has been generated from src_generator")
      # Add test directory
      add_subdirectory(${PROJECT_SOURCE_DIR}/src/tests)
   elseif(EXISTS ${CMAKE_CURRENT_BINARY_DIR}/src/tests)
      message(NOTICE "dmr tests are available in tests directory of the CMake build directory: nothing has been generated from src_generator")
      # Add test directory
      add_subdirectory(${CMAKE_CURRENT_BINARY_DIR}/src/tests)
   else()
      message(NOTICE "dmr tests not found: trying to generate files from src_generator")
      find_package (Python3 COMPONENTS Interpreter Development)
      if(Python3_FOUND)
         file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/src/tests)
         file(COPY_FILE ${CMAKE_CURRENT_SOURCE_DIR}/cmake/CMakeLists.tests ${CMAKE_CURRENT_BINARY_DIR}/src/tests/CMakeLists.txt)
         execute_process(COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/src_generator/generate_tests.py ${CMAKE_CURRENT_BINARY_DIR}/src/tests WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src_generator)
         message(NOTICE "dmr tests generation completed")
         # Add test directory
         add_subdirectory(${CMAKE_CURRENT_BINARY_DIR}/src/tests)
      else()
         message(FATAL_ERROR "Python3 executable cannot be found: dmr tests cannot be generated")
      endif()
   endif()
endif()
