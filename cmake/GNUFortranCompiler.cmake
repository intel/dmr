include(CheckFortranCompilerFlag)

set(GFORTRAN_COMPILE_OPTIONS)
set(GFORTRAN_LINK_OPTIONS)
set(GFORTRAN_OPTIONS)

#if(DEVICEXLIB_ENABLE_ACC STREQUAL "OPENMPGPU")
#	list(APPEND GFORTRAN_COMPILE_OPTIONS "-fopenmp" "-foffload=nvptx-none")
#endif()

list(APPEND GFORTRAN_OPTIONS ${GFORTRAN_COMPILE_OPTIONS} ${GFORTRAN_LINK_OPTIONS})
message("   gfortran related compile and link options: ${GFORTRAN_OPTIONS}")

set(CMAKE_REQUIRED_LINK_OPTIONS ${GFORTRAN_OPTIONS})
check_fortran_compiler_flag("${GFORTRAN_OPTIONS}" GFORTRAN_VALID_FLAG)
unset(CMAKE_REQUIRED_LINK_OPTIONS)
if(NOT GFORTRAN_VALID_FLAG)
	unset(GFORTRAN_VALID_FLAG CACHE)
message(FATAL_ERROR "gfortran related option check failed! "
					"Please check CMakeError.log for the exact error.")
endif()

add_library(compilerCustomConfigc INTERFACE)
target_compile_options(compilerCustomConfigc
	INTERFACE
		${GFORTRAN_COMPILE_OPTIONS})
target_link_options(compilerCustomConfigc
	INTERFACE
		${GFORTRAN_LINK_OPTIONS})
