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
      module function omp_target_is_present_f_int8_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_1
         integer(I1P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int8_1 = .true.
         else
            omp_target_is_present_f_int8_1 = .false.
         endif
       endfunction omp_target_is_present_f_int8_1

      module function omp_target_is_present_f_int8_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_2
         integer(I1P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int8_2 = .true.
         else
            omp_target_is_present_f_int8_2 = .false.
         endif
      endfunction omp_target_is_present_f_int8_2

      module function omp_target_is_present_f_int8_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_3
         integer(I1P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int8_3 = .true.
         else
            omp_target_is_present_f_int8_3 = .false.
         endif
       endfunction omp_target_is_present_f_int8_3

      module function omp_target_is_present_f_int8_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_4
         integer(I1P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int8_4 = .true.
         else
            omp_target_is_present_f_int8_4 = .false.
         endif
      endfunction omp_target_is_present_f_int8_4

      module function omp_target_is_present_f_int8_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_5
         integer(I1P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int8_5 = .true.
         else
            omp_target_is_present_f_int8_5 = .false.
         endif
      endfunction omp_target_is_present_f_int8_5

      module function omp_target_is_present_f_int8_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_6
         integer(I1P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int8_6 = .true.
         else
            omp_target_is_present_f_int8_6 = .false.
         endif
      endfunction omp_target_is_present_f_int8_6

      module function omp_target_is_present_f_int8_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_7
         integer(I1P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int8_7 = .true.
         else
            omp_target_is_present_f_int8_7 = .false.
         endif
      endfunction omp_target_is_present_f_int8_7

      module function omp_target_is_present_f_int16_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_1
         integer(I2P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int16_1 = .true.
         else
            omp_target_is_present_f_int16_1 = .false.
         endif
       endfunction omp_target_is_present_f_int16_1

      module function omp_target_is_present_f_int16_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_2
         integer(I2P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int16_2 = .true.
         else
            omp_target_is_present_f_int16_2 = .false.
         endif
      endfunction omp_target_is_present_f_int16_2

      module function omp_target_is_present_f_int16_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_3
         integer(I2P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int16_3 = .true.
         else
            omp_target_is_present_f_int16_3 = .false.
         endif
       endfunction omp_target_is_present_f_int16_3

      module function omp_target_is_present_f_int16_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_4
         integer(I2P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int16_4 = .true.
         else
            omp_target_is_present_f_int16_4 = .false.
         endif
      endfunction omp_target_is_present_f_int16_4

      module function omp_target_is_present_f_int16_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_5
         integer(I2P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int16_5 = .true.
         else
            omp_target_is_present_f_int16_5 = .false.
         endif
      endfunction omp_target_is_present_f_int16_5

      module function omp_target_is_present_f_int16_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_6
         integer(I2P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int16_6 = .true.
         else
            omp_target_is_present_f_int16_6 = .false.
         endif
      endfunction omp_target_is_present_f_int16_6

      module function omp_target_is_present_f_int16_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_7
         integer(I2P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int16_7 = .true.
         else
            omp_target_is_present_f_int16_7 = .false.
         endif
      endfunction omp_target_is_present_f_int16_7

      module function omp_target_is_present_f_int32_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_1
         integer(I4P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int32_1 = .true.
         else
            omp_target_is_present_f_int32_1 = .false.
         endif
       endfunction omp_target_is_present_f_int32_1

      module function omp_target_is_present_f_int32_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_2
         integer(I4P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int32_2 = .true.
         else
            omp_target_is_present_f_int32_2 = .false.
         endif
      endfunction omp_target_is_present_f_int32_2

      module function omp_target_is_present_f_int32_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_3
         integer(I4P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int32_3 = .true.
         else
            omp_target_is_present_f_int32_3 = .false.
         endif
       endfunction omp_target_is_present_f_int32_3

      module function omp_target_is_present_f_int32_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_4
         integer(I4P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int32_4 = .true.
         else
            omp_target_is_present_f_int32_4 = .false.
         endif
      endfunction omp_target_is_present_f_int32_4

      module function omp_target_is_present_f_int32_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_5
         integer(I4P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int32_5 = .true.
         else
            omp_target_is_present_f_int32_5 = .false.
         endif
      endfunction omp_target_is_present_f_int32_5

      module function omp_target_is_present_f_int32_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_6
         integer(I4P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int32_6 = .true.
         else
            omp_target_is_present_f_int32_6 = .false.
         endif
      endfunction omp_target_is_present_f_int32_6

      module function omp_target_is_present_f_int32_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_7
         integer(I4P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int32_7 = .true.
         else
            omp_target_is_present_f_int32_7 = .false.
         endif
      endfunction omp_target_is_present_f_int32_7

      module function omp_target_is_present_f_int64_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_1
         integer(I8P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I8P) /= 0_I8P) then
            omp_target_is_present_f_int64_1 = .true.
         else
            omp_target_is_present_f_int64_1 = .false.
         endif
       endfunction omp_target_is_present_f_int64_1

      module function omp_target_is_present_f_int64_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_2
         integer(I8P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I8P) /= 0_I8P) then
            omp_target_is_present_f_int64_2 = .true.
         else
            omp_target_is_present_f_int64_2 = .false.
         endif
      endfunction omp_target_is_present_f_int64_2

      module function omp_target_is_present_f_int64_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_3
         integer(I8P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I8P) /= 0_I8P) then
            omp_target_is_present_f_int64_3 = .true.
         else
            omp_target_is_present_f_int64_3 = .false.
         endif
       endfunction omp_target_is_present_f_int64_3

      module function omp_target_is_present_f_int64_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_4
         integer(I8P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I8P) /= 0_I8P) then
            omp_target_is_present_f_int64_4 = .true.
         else
            omp_target_is_present_f_int64_4 = .false.
         endif
      endfunction omp_target_is_present_f_int64_4

      module function omp_target_is_present_f_int64_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_5
         integer(I8P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I8P) /= 0_I8P) then
            omp_target_is_present_f_int64_5 = .true.
         else
            omp_target_is_present_f_int64_5 = .false.
         endif
      endfunction omp_target_is_present_f_int64_5

      module function omp_target_is_present_f_int64_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_6
         integer(I8P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I8P) /= 0_I8P) then
            omp_target_is_present_f_int64_6 = .true.
         else
            omp_target_is_present_f_int64_6 = .false.
         endif
      endfunction omp_target_is_present_f_int64_6

      module function omp_target_is_present_f_int64_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_7
         integer(I8P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I8P) /= 0_I8P) then
            omp_target_is_present_f_int64_7 = .true.
         else
            omp_target_is_present_f_int64_7 = .false.
         endif
      endfunction omp_target_is_present_f_int64_7

      ! OpenMP Target Is Present Real Routines
      module function omp_target_is_present_f_real32_1(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_1
         real(R4P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real32_1 = .true.
         else
            omp_target_is_present_f_real32_1 = .false.
         endif
       endfunction omp_target_is_present_f_real32_1

      module function omp_target_is_present_f_real32_2(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_2
         real(R4P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real32_2 = .true.
         else
            omp_target_is_present_f_real32_2 = .false.
         endif
      endfunction omp_target_is_present_f_real32_2

      module function omp_target_is_present_f_real32_3(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_3
         real(R4P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real32_3 = .true.
         else
            omp_target_is_present_f_real32_3 = .false.
         endif
       endfunction omp_target_is_present_f_real32_3

      module function omp_target_is_present_f_real32_4(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_4
         real(R4P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real32_4 = .true.
         else
            omp_target_is_present_f_real32_4 = .false.
         endif
      endfunction omp_target_is_present_f_real32_4

      module function omp_target_is_present_f_real32_5(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_5
         real(R4P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real32_5 = .true.
         else
            omp_target_is_present_f_real32_5 = .false.
         endif
      endfunction omp_target_is_present_f_real32_5

      module function omp_target_is_present_f_real32_6(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_6
         real(R4P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real32_6 = .true.
         else
            omp_target_is_present_f_real32_6 = .false.
         endif
      endfunction omp_target_is_present_f_real32_6

      module function omp_target_is_present_f_real32_7(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_7
         real(R4P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real32_7 = .true.
         else
            omp_target_is_present_f_real32_7 = .false.
         endif
      endfunction omp_target_is_present_f_real32_7

      module function omp_target_is_present_f_real64_1(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_1
         real(R8P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real64_1 = .true.
         else
            omp_target_is_present_f_real64_1 = .false.
         endif
       endfunction omp_target_is_present_f_real64_1

      module function omp_target_is_present_f_real64_2(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_2
         real(R8P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real64_2 = .true.
         else
            omp_target_is_present_f_real64_2 = .false.
         endif
      endfunction omp_target_is_present_f_real64_2

      module function omp_target_is_present_f_real64_3(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_3
         real(R8P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real64_3 = .true.
         else
            omp_target_is_present_f_real64_3 = .false.
         endif
       endfunction omp_target_is_present_f_real64_3

      module function omp_target_is_present_f_real64_4(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_4
         real(R8P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real64_4 = .true.
         else
            omp_target_is_present_f_real64_4 = .false.
         endif
      endfunction omp_target_is_present_f_real64_4

      module function omp_target_is_present_f_real64_5(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_5
         real(R8P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real64_5 = .true.
         else
            omp_target_is_present_f_real64_5 = .false.
         endif
      endfunction omp_target_is_present_f_real64_5

      module function omp_target_is_present_f_real64_6(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_6
         real(R8P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real64_6 = .true.
         else
            omp_target_is_present_f_real64_6 = .false.
         endif
      endfunction omp_target_is_present_f_real64_6

      module function omp_target_is_present_f_real64_7(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_7
         real(R8P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real64_7 = .true.
         else
            omp_target_is_present_f_real64_7 = .false.
         endif
      endfunction omp_target_is_present_f_real64_7

#if defined _real128
      module function omp_target_is_present_f_real128_1(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_1
         real(R16P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real128_1 = .true.
         else
            omp_target_is_present_f_real128_1 = .false.
         endif
       endfunction omp_target_is_present_f_real128_1

      module function omp_target_is_present_f_real128_2(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_2
         real(R16P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real128_2 = .true.
         else
            omp_target_is_present_f_real128_2 = .false.
         endif
      endfunction omp_target_is_present_f_real128_2

      module function omp_target_is_present_f_real128_3(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_3
         real(R16P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real128_3 = .true.
         else
            omp_target_is_present_f_real128_3 = .false.
         endif
       endfunction omp_target_is_present_f_real128_3

      module function omp_target_is_present_f_real128_4(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_4
         real(R16P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real128_4 = .true.
         else
            omp_target_is_present_f_real128_4 = .false.
         endif
      endfunction omp_target_is_present_f_real128_4

      module function omp_target_is_present_f_real128_5(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_5
         real(R16P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real128_5 = .true.
         else
            omp_target_is_present_f_real128_5 = .false.
         endif
      endfunction omp_target_is_present_f_real128_5

      module function omp_target_is_present_f_real128_6(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_6
         real(R16P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real128_6 = .true.
         else
            omp_target_is_present_f_real128_6 = .false.
         endif
      endfunction omp_target_is_present_f_real128_6

      module function omp_target_is_present_f_real128_7(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_7
         real(R16P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real128_7 = .true.
         else
            omp_target_is_present_f_real128_7 = .false.
         endif
      endfunction omp_target_is_present_f_real128_7
#endif

      ! OpenMP Target Is Present Complex Routines
      module function omp_target_is_present_f_cmplx32_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_1
         complex(R4P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx32_1 = .true.
         else
            omp_target_is_present_f_cmplx32_1 = .false.
         endif
       endfunction omp_target_is_present_f_cmplx32_1

      module function omp_target_is_present_f_cmplx32_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_2
         complex(R4P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx32_2 = .true.
         else
            omp_target_is_present_f_cmplx32_2 = .false.
         endif
      endfunction omp_target_is_present_f_cmplx32_2

      module function omp_target_is_present_f_cmplx32_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_3
         complex(R4P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx32_3 = .true.
         else
            omp_target_is_present_f_cmplx32_3 = .false.
         endif
       endfunction omp_target_is_present_f_cmplx32_3

      module function omp_target_is_present_f_cmplx32_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_4
         complex(R4P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx32_4 = .true.
         else
            omp_target_is_present_f_cmplx32_4 = .false.
         endif
      endfunction omp_target_is_present_f_cmplx32_4

      module function omp_target_is_present_f_cmplx32_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_5
         complex(R4P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx32_5 = .true.
         else
            omp_target_is_present_f_cmplx32_5 = .false.
         endif
      endfunction omp_target_is_present_f_cmplx32_5

      module function omp_target_is_present_f_cmplx32_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_6
         complex(R4P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx32_6 = .true.
         else
            omp_target_is_present_f_cmplx32_6 = .false.
         endif
      endfunction omp_target_is_present_f_cmplx32_6

      module function omp_target_is_present_f_cmplx32_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_7
         complex(R4P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx32_7 = .true.
         else
            omp_target_is_present_f_cmplx32_7 = .false.
         endif
      endfunction omp_target_is_present_f_cmplx32_7

      module function omp_target_is_present_f_cmplx64_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_1
         complex(R8P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx64_1 = .true.
         else
            omp_target_is_present_f_cmplx64_1 = .false.
         endif
       endfunction omp_target_is_present_f_cmplx64_1

      module function omp_target_is_present_f_cmplx64_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_2
         complex(R8P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx64_2 = .true.
         else
            omp_target_is_present_f_cmplx64_2 = .false.
         endif
      endfunction omp_target_is_present_f_cmplx64_2

      module function omp_target_is_present_f_cmplx64_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_3
         complex(R8P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx64_3 = .true.
         else
            omp_target_is_present_f_cmplx64_3 = .false.
         endif
       endfunction omp_target_is_present_f_cmplx64_3

      module function omp_target_is_present_f_cmplx64_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_4
         complex(R8P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx64_4 = .true.
         else
            omp_target_is_present_f_cmplx64_4 = .false.
         endif
      endfunction omp_target_is_present_f_cmplx64_4

      module function omp_target_is_present_f_cmplx64_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_5
         complex(R8P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx64_5 = .true.
         else
            omp_target_is_present_f_cmplx64_5 = .false.
         endif
      endfunction omp_target_is_present_f_cmplx64_5

      module function omp_target_is_present_f_cmplx64_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_6
         complex(R8P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx64_6 = .true.
         else
            omp_target_is_present_f_cmplx64_6 = .false.
         endif
      endfunction omp_target_is_present_f_cmplx64_6

      module function omp_target_is_present_f_cmplx64_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_7
         complex(R8P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx64_7 = .true.
         else
            omp_target_is_present_f_cmplx64_7 = .false.
         endif
      endfunction omp_target_is_present_f_cmplx64_7

#if defined _real128
      module function omp_target_is_present_f_cmplx128_1(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_1
         complex(R16P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx128_1 = .true.
         else
            omp_target_is_present_f_cmplx128_1 = .false.
         endif
       endfunction omp_target_is_present_f_cmplx128_1

      module function omp_target_is_present_f_cmplx128_2(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_2
         complex(R16P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx128_2 = .true.
         else
            omp_target_is_present_f_cmplx128_2 = .false.
         endif
      endfunction omp_target_is_present_f_cmplx128_2

      module function omp_target_is_present_f_cmplx128_3(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_3
         complex(R16P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx128_3 = .true.
         else
            omp_target_is_present_f_cmplx128_3 = .false.
         endif
       endfunction omp_target_is_present_f_cmplx128_3

      module function omp_target_is_present_f_cmplx128_4(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_4
         complex(R16P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx128_4 = .true.
         else
            omp_target_is_present_f_cmplx128_4 = .false.
         endif
      endfunction omp_target_is_present_f_cmplx128_4

      module function omp_target_is_present_f_cmplx128_5(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_5
         complex(R16P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx128_5 = .true.
         else
            omp_target_is_present_f_cmplx128_5 = .false.
         endif
      endfunction omp_target_is_present_f_cmplx128_5

      module function omp_target_is_present_f_cmplx128_6(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_6
         complex(R16P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx128_6 = .true.
         else
            omp_target_is_present_f_cmplx128_6 = .false.
         endif
      endfunction omp_target_is_present_f_cmplx128_6

      module function omp_target_is_present_f_cmplx128_7(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_7
         complex(R16P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"

         omp_device = int(omp_dev, c_int)
         cptr_dev   = c_loc(fptr_dev)

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx128_7 = .true.
         else
            omp_target_is_present_f_cmplx128_7 = .false.
         endif
      endfunction omp_target_is_present_f_cmplx128_7
#endif

endsubmodule dmr_target_is_present
