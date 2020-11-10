!* ========================================================================== *
!*                                                                            *
!* Copyright (C) 2020 Intel Corporation                                       *
!* This file is part of the DMR library.                                      *
!*                                                                            *
!* For information on the license, see the LICENSE file.                      *
!* Further information: https://github.com/giacrossi/dmr/                     *
!* SPDX-License-Identifier: BSD-3-Clause                                      *
!*                                                                            *
!* ========================================================================== *
!* Giacomo Rossi (Intel Corporation)                                          *
!* ========================================================================== *

program dmr_test_c_functions
   use iso_c_binding
   use dmr_environment
   use dmr_c_functions
   use omp_lib
   implicit none

   integer(I4P), parameter           :: i=100, j=200, k=300
   type(c_ptr)                       :: cptr, cptr_dev
   integer(I4P), target, allocatable :: fptr(:,:,:)
   integer(I4P), pointer             :: fptr_dev(:,:,:)
   type(c_int)                       :: pres

   cptr_dev = omp_target_alloc_c(i*j*k*byte_size(1_I4P), omp_get_default_device())
   call c_f_pointer(cprt_dev, fptr_dev, [i,j,k])

   print *, 'Data is on GPU? '
   pres = omp_target_is_present(cptr_dev, omp_get_default_device())
   if (pres==0_I4P) then
      print *, 'False'
   else
      print *, 'True'
   endif

!      subroutine init_device_data
!!$omp target teams device(omp_get_default_device()) is_device_ptr(fptr_dev)
!!$omp distribute parallel do private (l, m, n)
!      endsubroutine init_device_data

endprogram dmr_test_c_functions
