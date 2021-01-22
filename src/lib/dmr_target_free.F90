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

submodule (dmr) dmr_target_free
   use, intrinsic :: iso_c_binding
   use omp_lib
   use dmr_environment
   use dmr_c_functions

   implicit none

   contains

      ! OpenMP Target Free Integer Routines
      module subroutine omp_target_free_f_int8(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I4P), intent(in)                         :: omp_dev
         type(c_ptr)                                      :: cptr_dev
         integer(kind=c_int)                              :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free_c(cptr_dev, omp_device)
         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int8

      module subroutine omp_target_free_f_int16(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I4P), intent(in)                         :: omp_dev
         type(c_ptr)                                      :: cptr_dev
         integer(kind=c_int)                              :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free_c(cptr_dev, omp_device)
         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int16

      module subroutine omp_target_free_f_int32(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I4P), intent(in)                         :: omp_dev
         type(c_ptr)                                      :: cptr_dev
         integer(kind=c_int)                              :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free_c(cptr_dev, omp_device)
         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int32

      module subroutine omp_target_free_f_int64(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I4P), intent(in)                         :: omp_dev
         type(c_ptr)                                      :: cptr_dev
         integer(kind=c_int)                              :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free_c(cptr_dev, omp_device)
         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int64

      ! OpenMP Target Free Real Routines
      module subroutine omp_target_free_f_real32(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I4P), intent(in)                      :: omp_dev
         type(c_ptr)                                   :: cptr_dev
         integer(kind=c_int)                           :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free_c(cptr_dev, omp_device)
         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real32

      module subroutine omp_target_free_f_real64(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I4P), intent(in)                      :: omp_dev
         type(c_ptr)                                   :: cptr_dev
         integer(kind=c_int)                           :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free_c(cptr_dev, omp_device)
         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real64

#if defined _real128
      module subroutine omp_target_free_f_real128(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I4P), intent(in)                       :: omp_dev
         type(c_ptr)                                    :: cptr_dev
         integer(kind=c_int)                            :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free_c(cptr_dev, omp_device)
         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real128
#endif

      ! OpenMP Target Free Complex Routines
      module subroutine omp_target_free_f_cmplx32(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I4P), intent(in)                         :: omp_dev
         type(c_ptr)                                      :: cptr_dev
         integer(kind=c_int)                              :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free_c(cptr_dev, omp_device)
         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx32

      module subroutine omp_target_free_f_cmplx64(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I4P), intent(in)                         :: omp_dev
         type(c_ptr)                                      :: cptr_dev
         integer(kind=c_int)                              :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free_c(cptr_dev, omp_device)
         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx64

#if defined _real128
      module subroutine omp_target_free_f_cmplx128(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I4P), intent(in)                          :: omp_dev
         type(c_ptr)                                       :: cptr_dev
         integer(kind=c_int)                               :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free_c(cptr_dev, omp_device)
         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx128
#endif
endsubmodule dmr_target_free
