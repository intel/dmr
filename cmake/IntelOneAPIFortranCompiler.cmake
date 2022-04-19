include(CheckFortranCompilerFlag)

if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 2022.0.0)
	message(FATAL_ERROR "Intel OneAPI is supported from v2022.0.0")
endif()

set(IFX_COMPILE_OPTIONS)
set(IFX_LINK_OPTIONS)
set(IFX_OPTIONS)

list(APPEND IFX_COMPILE_OPTIONS "-fiopenmp" "-fopenmp-targets=spir64")

list(APPEND IFX_OPTIONS ${IFX_COMPILE_OPTIONS})
message("   ifx related compile options: ${IFX_OPTIONS}")
set(CMAKE_REQUIRED_LINK_OPTIONS ${IFX_OPTIONS})
check_fortran_compiler_flag("${IFX_OPTIONS}" IFX_VALID_OPTIONS)
unset(CMAKE_REQUIRED_LINK_OPTIONS)
if(NOT IFX_VALID_OPTIONS)
	unset(IFX_VALID_OPTIONS CACHE)
	message(FATAL_ERROR "ifx related option check failed! "
						"Please check CMakeError.log for the exact error.")
endif()

add_library(compilerCustomConfigc INTERFACE)
target_compile_options(compilerCustomConfigc
	INTERFACE
		${IFX_COMPILE_OPTIONS})
target_link_options(compilerCustomConfigc
	INTERFACE
		${IFX_COMPILE_OPTIONS}
		${IFX_LINK_OPTIONS})
