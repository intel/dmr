!* ========================================================================== *
!*                                                                            *
!* Copyright (C) 2020 Intel Corporation                                       *
!* This file is part of the FALCO library.                                    *
!*                                                                            *
!* For information on the license, see the LICENSE file.                      *
!* Further information: https://github.com/giacrossi/FALCO/                   *
!* SPDX-License-Identifier: BSD-3-Clause                                      *
!*                                                                            *
!* ========================================================================== *
!* Giacomo Rossi (Intel Corporation)                                          *
!* ========================================================================== *

program test_falco
   use omp_lib
   use falco
   use falco_c_functions
   use penf
   use, intrinsic :: iso_c_binding

   implicit none

   integer(I1P), pointer, contiguous :: a(:)
   integer(I4P) :: i
   integer(I4P) :: omp_initial, omp_default

   omp_default = omp_get_default_device()
   omp_initial = omp_get_initial_device_c()

   allocate(a(1:100))

   !$omp target data map(tofrom:a)
   !!$omp target enter data map(alloc:a)

   !$omp target teams distribute parallel do
   do i=1,100
      a(i) = 0_I1P
   enddo
   !$omp end target teams distribute parallel do

   print *, omp_get_mapped_ptr_f(a, omp_default)

   !!$omp target exit data map(delete:a)
   !$omp end target data
endprogram test_falco
