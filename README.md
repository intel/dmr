<a name="top"></a>

# DMR

### DMR, fortran library for C/C++ OpenMP\* Device Memory Routines

- DMR is a Fortran library for mimicking OpenMP device memory routines in Fortran
- DMR is Fortran 2008 standard compliant;

#### Table of Contents

+ [What is DMR?](#what-is-dmr?)
	+ [What are OpenMP Device Memory Routines?](#what-are-omp-routines?)
+ [Main features](#main-features)
+ [Status](#status)
+ [DMR for the Impatient](#DMR-for-the-impatient)
+ [Copyrights](#copyrights)

## What is DMR?

OpenMP API version 4.5 introduced device memory routines, that support allocation of memory and pointers management in the data environment of target devices: these routines only support C/C++ languages, while the Fortran support is still missing also in the latest OpenMP API (version 5.0). DMR introduces Fortran support to OpenMP device memory routines.

### What are OpenMP Device Memory Routines?

From OpenMP API version 4.5<sup>1</sup> is possible to directly manage device memory environment from C and C++ languages. In particular, is possible to allocate, deallocate device pointers and to move data between host and device using pointers. There's no support for Fortran language, but from Fortran Standard 2003 is possible to easily mixing Fortran and C.

Using C functions, Fortran interfaces to these functions and Fortran routines, DMR provides the capability to allocate, deallocate Fortran pointers on a device and to transfer data to and from a Fortran pointer allocated on the device to an host target.

#### Cited references

[1] _OpenMP API version 4.5_, OpenMP Architecture Review Board, 2015, https://www.openmp.org/wp-content/uploads/openmp-4.5.pdf

Go to [Top](#top)

## Main features

DMR is aimed to be a Fortran library for managing OpenMP allocation of memory and pointers in the data environment of target device:

+ [x] KISS and user-friendly:
  + [x] simple API;
  + [x] easy building and porting on heterogeneous architectures;
+ [x] comprehensive:
  + [x] pointers allocation;
  + [x] pointers deallocation;
  + [x] pointers data transfer between host and device;
+ [ ] well documented:
  + [ ] clear documentation of OpenMP API implemented;
  + [ ] complete API reference;

Any feature request is welcome.

Go to [Top](#top)

## Status

DMR provides the following Fortran routines:

+ [x] pointer presence check on device (omp_target_is_present --> omp_target_is_present_f)
+ [x] pointer allocation on device (omp_target_alloc --> omp_target_alloc_f)
+ [x] pointer deallocation from device (omp_target_free --> omp_target_free_f)
+ [x] pointer already mapped on the device (omp_get_mapped_ptr --> omp_get_mapped_ptr_f)<sup>2</sup>
+ [x] data synchronization between host and device (omp_target_memcpy --> omp_target_memcpy_f)
+ [x] rectangular data transfer (omp_target_memcpy_rect --> omp_target_memcpy_rect_f)

DMR provides Fortran interfaces to the following OpenMP C device memory routines:

+ [x] omp_target_alloc
+ [x] omp_target_free
+ [x] omp_target_is_present
+ [x] omp_get_mapped_ptr<sup>2</sup>
+ [x] omp_target_memcpy
+ [x] omp_target_memcpy_rect
+ [x] omp_target_associate_pointer
+ [x] omp_target_disassociate_pointer

[2] _OpenMP API version 5.1 Public Comment Draft_, OpenMP Architecture Review Board, 2020, https://www.openmp.org/wp-content/uploads/openmp-TR9.pdf

Go to [Top](#top)

## DMR for the Impatient

DMR can be compiled using the provided makefile, compatible with GNU gfortran, IBM XLF and Intel ifort/ifx compilers.

In order to compile the library, use the following command:
```
make compiler=1 debug=1
```

Where *compiler* can be one of the following: **gnu**, **ibm**, **intel**.

Go to [Top](#top)

## Copyrights

&copy; 2020 Giacomo Rossi, Intel Corporation

Anyone is interest to use, to develop or to contribute to DMR is welcome!

Go to [Top](#top)
