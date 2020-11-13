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

submodule (dmr) dmr_target_alloc
   use, intrinsic :: iso_c_binding
   use omp_lib
   use dmr_environment
   use dmr_c_functions

   implicit none

   contains

      ! OpenMP Target Alloc Integer Routines
      module subroutine omp_target_alloc_f_int8_1(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                         :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
      endsubroutine omp_target_alloc_f_int8_1

      module subroutine omp_target_alloc_f_int8_2(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                         :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
      endsubroutine omp_target_alloc_f_int8_2

      module subroutine omp_target_alloc_f_int8_3(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                         :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
      endsubroutine omp_target_alloc_f_int8_3

      module subroutine omp_target_alloc_f_int8_4(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
      endsubroutine omp_target_alloc_f_int8_4

      module subroutine omp_target_alloc_f_int8_5(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])
      endsubroutine omp_target_alloc_f_int8_5

      module subroutine omp_target_alloc_f_int8_6(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])
      endsubroutine omp_target_alloc_f_int8_6

      module subroutine omp_target_alloc_f_int8_7(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])
      endsubroutine omp_target_alloc_f_int8_7

      module subroutine omp_target_alloc_f_int16_1(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                         :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
      endsubroutine omp_target_alloc_f_int16_1

      module subroutine omp_target_alloc_f_int16_2(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                         :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
      endsubroutine omp_target_alloc_f_int16_2

      module subroutine omp_target_alloc_f_int16_3(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                         :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
      endsubroutine omp_target_alloc_f_int16_3

      module subroutine omp_target_alloc_f_int16_4(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
      endsubroutine omp_target_alloc_f_int16_4

      module subroutine omp_target_alloc_f_int16_5(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])
      endsubroutine omp_target_alloc_f_int16_5

      module subroutine omp_target_alloc_f_int16_6(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])
      endsubroutine omp_target_alloc_f_int16_6

      module subroutine omp_target_alloc_f_int16_7(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])
      endsubroutine omp_target_alloc_f_int16_7

      module subroutine omp_target_alloc_f_int32_1(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                         :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
      endsubroutine omp_target_alloc_f_int32_1

      module subroutine omp_target_alloc_f_int32_2(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                         :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
      endsubroutine omp_target_alloc_f_int32_2

      module subroutine omp_target_alloc_f_int32_3(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                         :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
      endsubroutine omp_target_alloc_f_int32_3

      module subroutine omp_target_alloc_f_int32_4(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
      endsubroutine omp_target_alloc_f_int32_4

      module subroutine omp_target_alloc_f_int32_5(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])
      endsubroutine omp_target_alloc_f_int32_5

      module subroutine omp_target_alloc_f_int32_6(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])
      endsubroutine omp_target_alloc_f_int32_6

      module subroutine omp_target_alloc_f_int32_7(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])
      endsubroutine omp_target_alloc_f_int32_7

      module subroutine omp_target_alloc_f_int64_1(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                         :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
      endsubroutine omp_target_alloc_f_int64_1

      module subroutine omp_target_alloc_f_int64_2(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                         :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
      endsubroutine omp_target_alloc_f_int64_2

      module subroutine omp_target_alloc_f_int64_3(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                         :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
      endsubroutine omp_target_alloc_f_int64_3

      module subroutine omp_target_alloc_f_int64_4(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
      endsubroutine omp_target_alloc_f_int64_4

      module subroutine omp_target_alloc_f_int64_5(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])
      endsubroutine omp_target_alloc_f_int64_5

      module subroutine omp_target_alloc_f_int64_6(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])
      endsubroutine omp_target_alloc_f_int64_6

      module subroutine omp_target_alloc_f_int64_7(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])
      endsubroutine omp_target_alloc_f_int64_7

      ! OpenMP Target Alloc Real Routines
      module subroutine omp_target_alloc_f_real32_1(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                      :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
      endsubroutine omp_target_alloc_f_real32_1

      module subroutine omp_target_alloc_f_real32_2(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                      :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
      endsubroutine omp_target_alloc_f_real32_2

      module subroutine omp_target_alloc_f_real32_3(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                      :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
      endsubroutine omp_target_alloc_f_real32_3

      module subroutine omp_target_alloc_f_real32_4(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                      :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
      endsubroutine omp_target_alloc_f_real32_4

      module subroutine omp_target_alloc_f_real32_5(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                      :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])
      endsubroutine omp_target_alloc_f_real32_5

      module subroutine omp_target_alloc_f_real32_6(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                      :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])
      endsubroutine omp_target_alloc_f_real32_6

      module subroutine omp_target_alloc_f_real32_7(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                      :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])
      endsubroutine omp_target_alloc_f_real32_7

      module subroutine omp_target_alloc_f_real64_1(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                      :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
      endsubroutine omp_target_alloc_f_real64_1

      module subroutine omp_target_alloc_f_real64_2(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                      :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
      endsubroutine omp_target_alloc_f_real64_2

      module subroutine omp_target_alloc_f_real64_3(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                      :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
      endsubroutine omp_target_alloc_f_real64_3

      module subroutine omp_target_alloc_f_real64_4(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                      :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
      endsubroutine omp_target_alloc_f_real64_4

      module subroutine omp_target_alloc_f_real64_5(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                      :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])
      endsubroutine omp_target_alloc_f_real64_5

      module subroutine omp_target_alloc_f_real64_6(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                      :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])
      endsubroutine omp_target_alloc_f_real64_6

      module subroutine omp_target_alloc_f_real64_7(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                      :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])
      endsubroutine omp_target_alloc_f_real64_7

#if defined _real128
      module subroutine omp_target_alloc_f_real128_1(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                       :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
      endsubroutine omp_target_alloc_f_real128_1

      module subroutine omp_target_alloc_f_real128_2(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                       :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
      endsubroutine omp_target_alloc_f_real128_2

      module subroutine omp_target_alloc_f_real128_3(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                       :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
      endsubroutine omp_target_alloc_f_real128_3

      module subroutine omp_target_alloc_f_real128_4(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
      endsubroutine omp_target_alloc_f_real128_4

      module subroutine omp_target_alloc_f_real128_5(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])
      endsubroutine omp_target_alloc_f_real128_5

      module subroutine omp_target_alloc_f_real128_6(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])
      endsubroutine omp_target_alloc_f_real128_6

      module subroutine omp_target_alloc_f_real128_7(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])
      endsubroutine omp_target_alloc_f_real128_7
#endif

      ! OpenMP Target Alloc Complex Routines
      module subroutine omp_target_alloc_f_cmplx32_1(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                         :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * dimensions, c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
      endsubroutine omp_target_alloc_f_cmplx32_1

      module subroutine omp_target_alloc_f_cmplx32_2(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                         :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
      endsubroutine omp_target_alloc_f_cmplx32_2

      module subroutine omp_target_alloc_f_cmplx32_3(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                         :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
      endsubroutine omp_target_alloc_f_cmplx32_3

      module subroutine omp_target_alloc_f_cmplx32_4(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
      endsubroutine omp_target_alloc_f_cmplx32_4

      module subroutine omp_target_alloc_f_cmplx32_5(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])
      endsubroutine omp_target_alloc_f_cmplx32_5

      module subroutine omp_target_alloc_f_cmplx32_6(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])
      endsubroutine omp_target_alloc_f_cmplx32_6

      module subroutine omp_target_alloc_f_cmplx32_7(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])
      endsubroutine omp_target_alloc_f_cmplx32_7

      module subroutine omp_target_alloc_f_cmplx64_1(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                         :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * dimensions, c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
      endsubroutine omp_target_alloc_f_cmplx64_1

      module subroutine omp_target_alloc_f_cmplx64_2(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                         :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
      endsubroutine omp_target_alloc_f_cmplx64_2

      module subroutine omp_target_alloc_f_cmplx64_3(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                         :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
      endsubroutine omp_target_alloc_f_cmplx64_3

      module subroutine omp_target_alloc_f_cmplx64_4(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
      endsubroutine omp_target_alloc_f_cmplx64_4

      module subroutine omp_target_alloc_f_cmplx64_5(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])
      endsubroutine omp_target_alloc_f_cmplx64_5

      module subroutine omp_target_alloc_f_cmplx64_6(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])
      endsubroutine omp_target_alloc_f_cmplx64_6

      module subroutine omp_target_alloc_f_cmplx64_7(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])
      endsubroutine omp_target_alloc_f_cmplx64_7

#if defined _real128
      module subroutine omp_target_alloc_f_cmplx128_1(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                          :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * dimensions, c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
      endsubroutine omp_target_alloc_f_cmplx128_1

      module subroutine omp_target_alloc_f_cmplx128_2(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                          :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
      endsubroutine omp_target_alloc_f_cmplx128_2

      module subroutine omp_target_alloc_f_cmplx128_3(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                          :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
      endsubroutine omp_target_alloc_f_cmplx128_3

      module subroutine omp_target_alloc_f_cmplx128_4(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                          :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
      endsubroutine omp_target_alloc_f_cmplx128_4

      module subroutine omp_target_alloc_f_cmplx128_5(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                          :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])
      endsubroutine omp_target_alloc_f_cmplx128_5

      module subroutine omp_target_alloc_f_cmplx128_6(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                          :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])
      endsubroutine omp_target_alloc_f_cmplx128_6

      module subroutine omp_target_alloc_f_cmplx128_7(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                          :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])
      endsubroutine omp_target_alloc_f_cmplx128_7
#endif

endsubmodule dmr_target_alloc
