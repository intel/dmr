include(CheckFortranCompilerFlag)

if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 19.10)
	message(FATAL_ERROR "Compiler Version ${CMAKE_Fortran_COMPILER_VERSION}. "
						"GPU acceleration requires PGI 19.10 or NVIDIA HPC SDK 20.7 or higher!")
endif()

set(NVFORTRAN_COMPILE_OPTIONS)
set(NVFORTRAN_LINK_OPTIONS)
set(NVFORTRAN_OPTIONS)

set(NVFORTRAN_COMPILE_OPTIONS "-Mcache_align" "-Mlarge_arrays" "-fast" "-cuda")

if(DEFINED NVFORTRAN_CUDA_VERSION)
	list(APPEND NVFORTRAN_COMPILE_OPTIONS "-gpu=cuda${NVFORTRAN_CUDA_VERSION}")
endif()

if(DEFINED NVFORTRAN_CUDA_CC)
	list(APPEND NVFORTRAN_COMPILE_OPTIONS "-gpu=cc${NVFORTRAN_CUDA_CC}")
endif()

if(DEVICEXLIB_ENABLE_GPU_BLAS STREQUAL "CUBLAS")
	list(APPEND NVFORTRAN_LINK_OPTIONS "-cudalib=cublas")
endif()

list(APPEND NVFORTRAN_OPTIONS ${NVFORTRAN_COMPILE_OPTIONS} ${NVFORTRAN_LINK_OPTIONS})

message("   nvfortran CUDA related compile and link options: ${NVFORTRAN_OPTIONS}")
set(CMAKE_REQUIRED_LINK_OPTIONS ${NVFORTRAN_OPTIONS})
check_fortran_compiler_flag("${NVFORTRAN_OPTIONS}" NVFORTRAN_VALID_OPTIONS)
unset(CMAKE_REQUIRED_LINK_OPTIONS)
if(NOT NVFORTRAN_VALID_OPTIONS)
	unset(NVFORTRAN_VALID_OPTIONS CACHE)
message(FATAL_ERROR "nvfortran CUDA related option check failed! "
					"Please check CMakeError.log for the exact error.")
endif()

add_library(compilerCustomConfig INTERFACE)
target_compile_definitions(compilerCustomConfig
	INTERFACE
		"__PGI")
target_compile_options(compilerCustomConfig
	INTERFACE
		${NVFORTRAN_COMPILE_OPTIONS})
target_link_options(compilerCustomConfig
	INTERFACE
		${NVFORTRAN_LINK_OPTIONS})