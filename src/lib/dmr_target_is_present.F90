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

submodule (dmr) dmr_target_is_present
   use, intrinsic :: iso_c_binding
   use omp_lib
   use dmr_environment
   use dmr_c_functions

   implicit none

   contains

      ! OpenMP Target Is Present Integer Routines
      module function omp_target_is_present_f_int8(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8
         integer(I1P), target, intent(in) :: fptr_dev(..)
         integer(I4P), intent(in)         :: omp_dev
         type(c_ptr)                      :: cptr_dev
         integer(kind=c_int)              :: omp_device

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int8 = .true.
         else
            omp_target_is_present_f_int8 = .false.
         endif
       endfunction omp_target_is_present_f_int8

      module function omp_target_is_present_f_int16(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16
         integer(I2P), target, intent(in) :: fptr_dev(..)
         integer(I4P), intent(in)         :: omp_dev
         type(c_ptr)                      :: cptr_dev
         integer(kind=c_int)              :: omp_device

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int16 = .true.
         else
            omp_target_is_present_f_int16 = .false.
         endif
       endfunction omp_target_is_present_f_int16

      module function omp_target_is_present_f_int32(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32
         integer(I4P), target, intent(in) :: fptr_dev(..)
         integer(I4P), intent(in)         :: omp_dev
         type(c_ptr)                      :: cptr_dev
         integer(kind=c_int)              :: omp_device

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int32 = .true.
         else
            omp_target_is_present_f_int32 = .false.
         endif
       endfunction omp_target_is_present_f_int32

      module function omp_target_is_present_f_int64(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64
         integer(I8P), target, intent(in) :: fptr_dev(..)
         integer(I4P), intent(in)         :: omp_dev
         type(c_ptr)                      :: cptr_dev
         integer(kind=c_int)              :: omp_device

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I8P) /= 0_I8P) then
            omp_target_is_present_f_int64 = .true.
         else
            omp_target_is_present_f_int64 = .false.
         endif
       endfunction omp_target_is_present_f_int64

      ! OpenMP Target Is Present Real Routines
      module function omp_target_is_present_f_real32(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32
         real(R4P), target, intent(in) :: fptr_dev(..)
         integer(I4P), intent(in)      :: omp_dev
         type(c_ptr)                   :: cptr_dev
         integer(kind=c_int)           :: omp_device

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real32 = .true.
         else
            omp_target_is_present_f_real32 = .false.
         endif
       endfunction omp_target_is_present_f_real32

      module function omp_target_is_present_f_real64(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64
         real(R8P), target, intent(in) :: fptr_dev(..)
         integer(I4P), intent(in)      :: omp_dev
         type(c_ptr)                   :: cptr_dev
         integer(kind=c_int)           :: omp_device

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real64 = .true.
         else
            omp_target_is_present_f_real64 = .false.
         endif
       endfunction omp_target_is_present_f_real64

#if defined _real128
      module function omp_target_is_present_f_real128(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128
         real(R16P), target, intent(in) :: fptr_dev(..)
         integer(I4P), intent(in)       :: omp_dev
         type(c_ptr)                    :: cptr_dev
         integer(kind=c_int)            :: omp_device

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real128 = .true.
         else
            omp_target_is_present_f_real128 = .false.
         endif
       endfunction omp_target_is_present_f_real128
#endif

      ! OpenMP Target Is Present Complex Routines
      module function omp_target_is_present_f_cmplx32(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32
         complex(R4P), target, intent(in) :: fptr_dev(..)
         integer(I4P), intent(in)         :: omp_dev
         type(c_ptr)                      :: cptr_dev
         integer(kind=c_int)              :: omp_device

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx32 = .true.
         else
            omp_target_is_present_f_cmplx32 = .false.
         endif
       endfunction omp_target_is_present_f_cmplx32

      module function omp_target_is_present_f_cmplx64(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64
         complex(R8P), target, intent(in) :: fptr_dev(..)
         integer(I4P), intent(in)         :: omp_dev
         type(c_ptr)                      :: cptr_dev
         integer(kind=c_int)              :: omp_device

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx64 = .true.
         else
            omp_target_is_present_f_cmplx64 = .false.
         endif
       endfunction omp_target_is_present_f_cmplx64

#if defined _real128
      module function omp_target_is_present_f_cmplx128(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128
         complex(R16P), target, intent(in) :: fptr_dev(..)
         integer(I4P), intent(in)          :: omp_dev
         type(c_ptr)                       :: cptr_dev
         integer(kind=c_int)               :: omp_device

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx128 = .true.
         else
            omp_target_is_present_f_cmplx128 = .false.
         endif
       endfunction omp_target_is_present_f_cmplx128
#endif

endsubmodule dmr_target_is_present
