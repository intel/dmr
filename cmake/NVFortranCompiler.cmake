include(CheckFortranCompilerFlag)

if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 19.10)
	message(FATAL_ERROR "Compiler Version ${CMAKE_Fortran_COMPILER_VERSION}. "
						"GPU acceleration requires PGI 19.10 or NVIDIA HPC SDK 20.7 or higher!")
endif()

set(NVFORTRAN_COMPILE_OPTIONS)
set(NVFORTRAN_LINK_OPTIONS)
set(NVFORTRAN_OPTIONS)

set(NVFORTRAN_COMPILE_OPTIONS "-Mcache_align" "-Mlarge_arrays" "-fast" "-mp=gpu")

list(APPEND NVFORTRAN_OPTIONS ${NVFORTRAN_COMPILE_OPTIONS})

message("   nvfortran CUDA related compile and link options: ${NVFORTRAN_OPTIONS}")
set(CMAKE_REQUIRED_LINK_OPTIONS ${NVFORTRAN_OPTIONS})
check_fortran_compiler_flag("${NVFORTRAN_OPTIONS}" NVFORTRAN_VALID_OPTIONS)
unset(CMAKE_REQUIRED_LINK_OPTIONS)
if(NOT NVFORTRAN_VALID_OPTIONS)
	unset(NVFORTRAN_VALID_OPTIONS CACHE)
message(FATAL_ERROR "nvfortran CUDA related option check failed! "
					"Please check CMakeError.log for the exact error.")
endif()

add_library(compilerCustomConfigc INTERFACE)
target_compile_options(compilerCustomConfigc
	INTERFACE
		${NVFORTRAN_COMPILE_OPTIONS})
target_link_options(compilerCustomConfigc
	INTERFACE
		${NVFORTRAN_LINK_OPTIONS})
