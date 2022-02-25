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
   use dmr_environment

   implicit none

   contains
      ! OpenMP Target Is Present Integer Routines
      module function omp_target_is_present_f_int8(fptr_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int8
         integer(I1P), pointer, contiguous, intent(in) :: fptr_dev(..)

         if (associated(fptr_dev)) then
            omp_target_is_present_f_int8 = .true.
         else
            omp_target_is_present_f_int8 = .false.
         endif
      endfunction omp_target_is_present_f_int8
!
      module function omp_target_is_present_f_int16(fptr_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int16
         integer(I2P), pointer, contiguous, intent(in) :: fptr_dev(..)

         if (associated(fptr_dev)) then
            omp_target_is_present_f_int16 = .true.
         else
            omp_target_is_present_f_int16 = .false.
         endif
      endfunction omp_target_is_present_f_int16
!
      module function omp_target_is_present_f_int32(fptr_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int32
         integer(I4P), pointer, contiguous, intent(in) :: fptr_dev(..)

         if (associated(fptr_dev)) then
            omp_target_is_present_f_int32 = .true.
         else
            omp_target_is_present_f_int32 = .false.
         endif
      endfunction omp_target_is_present_f_int32
!
      module function omp_target_is_present_f_int64(fptr_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int64
         integer(I8P), pointer, contiguous, intent(in) :: fptr_dev(..)

         if (associated(fptr_dev)) then
            omp_target_is_present_f_int64 = .true.
         else
            omp_target_is_present_f_int64 = .false.
         endif
      endfunction omp_target_is_present_f_int64
!
!
      ! OpenMP Target Is Present Real Routines
      module function omp_target_is_present_f_real32(fptr_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real32
         real(R4P), pointer, contiguous, intent(in)    :: fptr_dev(..)

         if (associated(fptr_dev)) then
            omp_target_is_present_f_real32 = .true.
         else
            omp_target_is_present_f_real32 = .false.
         endif
      endfunction omp_target_is_present_f_real32
!
      module function omp_target_is_present_f_real64(fptr_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real64
         real(R8P), pointer, contiguous, intent(in)    :: fptr_dev(..)

         if (associated(fptr_dev)) then
            omp_target_is_present_f_real64 = .true.
         else
            omp_target_is_present_f_real64 = .false.
         endif
      endfunction omp_target_is_present_f_real64
!
#if defined _real128
      module function omp_target_is_present_f_real128(fptr_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real128
         real(R16P), pointer, contiguous, intent(in)   :: fptr_dev(..)

         if (associated(fptr_dev)) then
            omp_target_is_present_f_real128 = .true.
         else
            omp_target_is_present_f_real128 = .false.
         endif
      endfunction omp_target_is_present_f_real128
#endif
!
!
      ! OpenMP Target Is Present Complex Routines
      module function omp_target_is_present_f_cmplx32(fptr_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx32
         complex(R4P), pointer, contiguous, intent(in) :: fptr_dev(..)

         if (associated(fptr_dev)) then
            omp_target_is_present_f_cmplx32 = .true.
         else
            omp_target_is_present_f_cmplx32 = .false.
         endif
      endfunction omp_target_is_present_f_cmplx32
!
      module function omp_target_is_present_f_cmplx64(fptr_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx64
         complex(R8P), pointer, contiguous, intent(in) :: fptr_dev(..)

         if (associated(fptr_dev)) then
            omp_target_is_present_f_cmplx64 = .true.
         else
            omp_target_is_present_f_cmplx64 = .false.
         endif
      endfunction omp_target_is_present_f_cmplx64
!
#if defined _real128
      module function omp_target_is_present_f_cmplx128(fptr_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx128
         complex(R16P), pointer, contiguous, intent(in):: fptr_dev(..)

         if (associated(fptr_dev)) then
            omp_target_is_present_f_cmplx128 = .true.
         else
            omp_target_is_present_f_cmplx128 = .false.
         endif
      endfunction omp_target_is_present_f_cmplx128
#endif
!
!
      ! OpenMP Target Is Present Logical Routines
      module function omp_target_is_present_f_lgcl32(fptr_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_lgcl32
         logical(I4P), pointer, contiguous, intent(in) :: fptr_dev(..)

         if (associated(fptr_dev)) then
            omp_target_is_present_f_lgcl32 = .true.
         else
            omp_target_is_present_f_lgcl32 = .false.
         endif
      endfunction omp_target_is_present_f_lgcl32
!
!
endsubmodule dmr_target_is_present