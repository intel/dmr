include(CheckFortranCompilerFlag)

if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 12.0.0)
	message(FATAL_ERROR "Compiler Version ${CMAKE_Fortran_COMPILER_VERSION}. "
        "GPU acceleration requires Cray Fortran version 12.0.0 or higher ")
endif()

set(CRAYFORTRAN_COMPILE_OPTIONS)
set(CRAYFORTRAN_LINK_OPTIONS)
set(CRAYFORTRAN_OPTIONS)

set(CRAYFORTRAN_COMPILE_OPTIONS "-fopenmp")

list(APPEND CRAYFORTRAN_OPTIONS ${CRAYFORTRAN_COMPILE_OPTIONS})

message("   ftn related compile and link options: ${CRAYFORTRAN_OPTIONS}")
set(CMAKE_REQUIRED_LINK_OPTIONS ${CRAYFORTRAN_OPTIONS})
check_fortran_compiler_flag("${CRAYFORTRAN_OPTIONS}" CRAYFORTRAN_VALID_OPTIONS)
unset(CMAKE_REQUIRED_LINK_OPTIONS)
if(NOT CRAYFORTRAN_VALID_OPTIONS)
	unset(CRAYFORTRAN_VALID_OPTIONS CACHE)
message(FATAL_ERROR "ftn related option check failed! "
					"Please check CMakeError.log for the exact error.")
endif()

add_library(compilerCustomConfigc INTERFACE)
target_compile_options(compilerCustomConfigc
	INTERFACE
		${CRAYFORTRAN_COMPILE_OPTIONS})
target_link_options(compilerCustomConfigc
	INTERFACE
		${CRAYFORTRAN_LINK_OPTIONS})
