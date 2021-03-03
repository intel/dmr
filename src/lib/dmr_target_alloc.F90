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
#if defined _F2008
      module subroutine omp_target_alloc_f_int8(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                         :: dimensions(:)
         integer(I4P), intent(in)                         :: omp_dev
         integer(I8P), intent(in), optional               :: bounds(:)
         type(c_ptr)                                      :: cptr_dev
         integer(kind=c_int)                              :: omp_device
         integer(I1P), pointer                            :: fptr1(:),       fptr2(:,:),       fptr3(:,:,:),       &
                                                             fptr4(:,:,:,:), fptr5(:,:,:,:,:), fptr6(:,:,:,:,:,:), &
                                                             fptr7(:,:,:,:,:,:,:)

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr, [dimensions])
                  fptr_dev(bounds(1):bounds(2)) => fptr
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
               endif
            rank(2)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr2, [dimensions(1), dimensions(2)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr2
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
               endif
            rank(3)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr3, [dimensions(1), dimensions(2), dimensions(3)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr3
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
               endif
            rank(4)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr4, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr4
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               endif
            rank(5)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr5, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10)) => fptr5
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5)])
               endif
            rank(6)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr6, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12)) => fptr6
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6)])
               endif
            rank(7)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr7, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr7
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6), dimensions(7)])
               endif
            endselect
         endif
      endsubroutine omp_target_alloc_f_int8

      module subroutine omp_target_alloc_f_int16(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                         :: dimensions(:)
         integer(I4P), intent(in)                         :: omp_dev
         integer(I8P), intent(in), optional               :: bounds(:)
         type(c_ptr)                                      :: cptr_dev
         integer(kind=c_int)                              :: omp_device
         integer(I2P), pointer                            :: fptr1(:),       fptr2(:,:),       fptr3(:,:,:),       &
                                                             fptr4(:,:,:,:), fptr5(:,:,:,:,:), fptr6(:,:,:,:,:,:), &
                                                             fptr7(:,:,:,:,:,:,:)

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr, [dimensions])
                  fptr_dev(bounds(1):bounds(2)) => fptr
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
               endif
            rank(2)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr2, [dimensions(1), dimensions(2)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr2
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
               endif
            rank(3)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr3, [dimensions(1), dimensions(2), dimensions(3)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr3
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
               endif
            rank(4)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr4, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr4
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               endif
            rank(5)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr5, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10)) => fptr5
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5)])
               endif
            rank(6)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr6, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12)) => fptr6
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6)])
               endif
            rank(7)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr7, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr7
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6), dimensions(7)])
               endif
            endselect
         endif
      endsubroutine omp_target_alloc_f_int16

      module subroutine omp_target_alloc_f_int32(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                         :: dimensions(:)
         integer(I4P), intent(in)                         :: omp_dev
         integer(I8P), intent(in), optional               :: bounds(:)
         type(c_ptr)                                      :: cptr_dev
         integer(kind=c_int)                              :: omp_device
         integer(I4P), pointer                            :: fptr1(:),       fptr2(:,:),       fptr3(:,:,:),       &
                                                             fptr4(:,:,:,:), fptr5(:,:,:,:,:), fptr6(:,:,:,:,:,:), &
                                                             fptr7(:,:,:,:,:,:,:)

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr, [dimensions])
                  fptr_dev(bounds(1):bounds(2)) => fptr
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
               endif
            rank(2)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr2, [dimensions(1), dimensions(2)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr2
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
               endif
            rank(3)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr3, [dimensions(1), dimensions(2), dimensions(3)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr3
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
               endif
            rank(4)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr4, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr4
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               endif
            rank(5)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr5, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10)) => fptr5
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5)])
               endif
            rank(6)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr6, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12)) => fptr6
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6)])
               endif
            rank(7)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr7, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr7
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6), dimensions(7)])
               endif
            endselect
         endif
      endsubroutine omp_target_alloc_f_int32

      module subroutine omp_target_alloc_f_int64(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                         :: dimensions(:)
         integer(I4P), intent(in)                         :: omp_dev
         integer(I8P), intent(in), optional               :: bounds(:)
         type(c_ptr)                                      :: cptr_dev
         integer(kind=c_int)                              :: omp_device
         integer(I8P), pointer                            :: fptr1(:),       fptr2(:,:),       fptr3(:,:,:),       &
                                                             fptr4(:,:,:,:), fptr5(:,:,:,:,:), fptr6(:,:,:,:,:,:), &
                                                             fptr7(:,:,:,:,:,:,:)

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr, [dimensions])
                  fptr_dev(bounds(1):bounds(2)) => fptr
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
               endif
            rank(2)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr2, [dimensions(1), dimensions(2)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr2
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
               endif
            rank(3)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr3, [dimensions(1), dimensions(2), dimensions(3)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr3
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
               endif
            rank(4)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr4, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr4
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               endif
            rank(5)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr5, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10)) => fptr5
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5)])
               endif
            rank(6)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr6, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12)) => fptr6
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6)])
               endif
            rank(7)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr7, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr7
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6), dimensions(7)])
               endif
            endselect
         endif
      endsubroutine omp_target_alloc_f_int64
#else
      module subroutine omp_target_alloc_f_int8_1(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I8P), intent(in)                       :: dimensions
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(2)
         integer(I1P), pointer, contiguous              :: fptr(:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(bounds(1):bounds(2)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
         endif
      endsubroutine omp_target_alloc_f_int8_1

      module subroutine omp_target_alloc_f_int8_2(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I8P), intent(in)                       :: dimensions(2)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(4)
         integer(I1P), pointer, contiguous              :: fptr(:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int8_2

      module subroutine omp_target_alloc_f_int8_3(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                       :: dimensions(3)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(6)
         integer(I1P), pointer, contiguous              :: fptr(:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int8_3

      module subroutine omp_target_alloc_f_int8_4(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(4)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(8)
         integer(I1P), pointer, contiguous              :: fptr(:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int8_4

      module subroutine omp_target_alloc_f_int8_5(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(5)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(10)
         integer(I1P), pointer, contiguous              :: fptr(:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(bounds(1):bounds( 2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int8_5

      module subroutine omp_target_alloc_f_int8_6(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(6)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(12)
         integer(I1P), pointer, contiguous              :: fptr(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int8_6

      module subroutine omp_target_alloc_f_int8_7(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(7)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(14)
         integer(I1P), pointer, contiguous              :: fptr(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int8_7

      module subroutine omp_target_alloc_f_int16_1(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I8P), intent(in)                       :: dimensions
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(2)
         integer(I2P), pointer, contiguous              :: fptr(:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(bounds(1):bounds(2)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
         endif
      endsubroutine omp_target_alloc_f_int16_1

      module subroutine omp_target_alloc_f_int16_2(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I8P), intent(in)                       :: dimensions(2)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(4)
         integer(I2P), pointer, contiguous              :: fptr(:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int16_2

      module subroutine omp_target_alloc_f_int16_3(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                       :: dimensions(3)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(6)
         integer(I2P), pointer, contiguous              :: fptr(:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int16_3

      module subroutine omp_target_alloc_f_int16_4(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(4)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(8)
         integer(I2P), pointer, contiguous              :: fptr(:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int16_4

      module subroutine omp_target_alloc_f_int16_5(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(5)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(10)
         integer(I2P), pointer, contiguous              :: fptr(:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(bounds(1):bounds( 2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int16_5

      module subroutine omp_target_alloc_f_int16_6(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(6)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(12)
         integer(I2P), pointer, contiguous              :: fptr(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int16_6

      module subroutine omp_target_alloc_f_int16_7(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(7)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(14)
         integer(I2P), pointer, contiguous              :: fptr(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int16_7

      module subroutine omp_target_alloc_f_int32_1(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I8P), intent(in)                       :: dimensions
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(2)
         integer(I4P), pointer, contiguous              :: fptr(:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(bounds(1):bounds(2)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
         endif
      endsubroutine omp_target_alloc_f_int32_1

      module subroutine omp_target_alloc_f_int32_2(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I8P), intent(in)                       :: dimensions(2)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(4)
         integer(I4P), pointer, contiguous              :: fptr(:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int32_2

      module subroutine omp_target_alloc_f_int32_3(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                       :: dimensions(3)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(6)
         integer(I4P), pointer, contiguous              :: fptr(:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int32_3

      module subroutine omp_target_alloc_f_int32_4(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(4)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(8)
         integer(I4P), pointer, contiguous              :: fptr(:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int32_4

      module subroutine omp_target_alloc_f_int32_5(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(5)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(10)
         integer(I4P), pointer, contiguous              :: fptr(:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(bounds(1):bounds( 2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int32_5

      module subroutine omp_target_alloc_f_int32_6(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(6)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(12)
         integer(I4P), pointer, contiguous              :: fptr(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int32_6

      module subroutine omp_target_alloc_f_int32_7(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(7)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(14)
         integer(I4P), pointer, contiguous              :: fptr(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int32_7

      module subroutine omp_target_alloc_f_int64_1(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I8P), intent(in)                       :: dimensions
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(2)
         integer(I8P), pointer, contiguous              :: fptr(:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(bounds(1):bounds(2)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
         endif
      endsubroutine omp_target_alloc_f_int64_1

      module subroutine omp_target_alloc_f_int64_2(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I8P), intent(in)                       :: dimensions(2)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(4)
         integer(I8P), pointer, contiguous              :: fptr(:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int64_2

      module subroutine omp_target_alloc_f_int64_3(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                       :: dimensions(3)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(6)
         integer(I8P), pointer, contiguous              :: fptr(:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int64_3

      module subroutine omp_target_alloc_f_int64_4(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(4)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(8)
         integer(I8P), pointer, contiguous              :: fptr(:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int64_4

      module subroutine omp_target_alloc_f_int64_5(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(5)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(10)
         integer(I8P), pointer, contiguous              :: fptr(:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(bounds(1):bounds( 2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int64_5

      module subroutine omp_target_alloc_f_int64_6(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(6)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(12)
         integer(I8P), pointer, contiguous              :: fptr(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int64_6

      module subroutine omp_target_alloc_f_int64_7(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(7)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(14)
         integer(I8P), pointer, contiguous              :: fptr(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endif
         endif
      endsubroutine omp_target_alloc_f_int64_7
#endif

      ! OpenMP Target Alloc Real Routines
#if defined _F2008
      module subroutine omp_target_alloc_f_real32(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                      :: dimensions(:)
         integer(I4P), intent(in)                      :: omp_dev
         integer(I8P), intent(in), optional            :: bounds(:)
         type(c_ptr)                                   :: cptr_dev
         integer(kind=c_int)                           :: omp_device
         integer(I1P), pointer                         :: fptr1(:),       fptr2(:,:),       fptr3(:,:,:),       &
                                                          fptr4(:,:,:,:), fptr5(:,:,:,:,:), fptr6(:,:,:,:,:,:), &
                                                          fptr7(:,:,:,:,:,:,:)

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr, [dimensions])
                  fptr_dev(bounds(1):bounds(2)) => fptr
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
               endif
            rank(2)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr2, [dimensions(1), dimensions(2)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr2
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
               endif
            rank(3)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr3, [dimensions(1), dimensions(2), dimensions(3)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr3
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
               endif
            rank(4)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr4, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr4
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               endif
            rank(5)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr5, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10)) => fptr5
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5)])
               endif
            rank(6)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr6, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12)) => fptr6
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6)])
               endif
            rank(7)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr7, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr7
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6), dimensions(7)])
               endif
            endselect
         endif
      endsubroutine omp_target_alloc_f_real32

      module subroutine omp_target_alloc_f_real64(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                      :: dimensions(:)
         integer(I4P), intent(in)                      :: omp_dev
         integer(I8P), intent(in), optional            :: bounds(:)
         type(c_ptr)                                   :: cptr_dev
         integer(kind=c_int)                           :: omp_device
         integer(I1P), pointer                         :: fptr1(:),       fptr2(:,:),       fptr3(:,:,:),       &
                                                          fptr4(:,:,:,:), fptr5(:,:,:,:,:), fptr6(:,:,:,:,:,:), &
                                                          fptr7(:,:,:,:,:,:,:)

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr, [dimensions])
                  fptr_dev(bounds(1):bounds(2)) => fptr
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
               endif
            rank(2)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr2, [dimensions(1), dimensions(2)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr2
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
               endif
            rank(3)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr3, [dimensions(1), dimensions(2), dimensions(3)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr3
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
               endif
            rank(4)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr4, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr4
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               endif
            rank(5)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr5, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10)) => fptr5
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5)])
               endif
            rank(6)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr6, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12)) => fptr6
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6)])
               endif
            rank(7)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr7, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr7
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6), dimensions(7)])
               endif
            endselect
         endif
      endsubroutine omp_target_alloc_f_real64

#if defined _real128
      module subroutine omp_target_alloc_f_real128(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                       :: dimensions(:)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(:)
         type(c_ptr)                                    :: cptr_dev
         integer(kind=c_int)                            :: omp_device
         integer(I1P), pointer                          :: fptr1(:),       fptr2(:,:),       fptr3(:,:,:),       &
                                                           fptr4(:,:,:,:), fptr5(:,:,:,:,:), fptr6(:,:,:,:,:,:), &
                                                           fptr7(:,:,:,:,:,:,:)

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr, [dimensions])
                  fptr_dev(bounds(1):bounds(2)) => fptr
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
               endif
            rank(2)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr2, [dimensions(1), dimensions(2)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr2
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
               endif
            rank(3)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr3, [dimensions(1), dimensions(2), dimensions(3)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr3
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
               endif
            rank(4)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr4, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr4
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               endif
            rank(5)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr5, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10)) => fptr5
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5)])
               endif
            rank(6)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr6, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12)) => fptr6
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6)])
               endif
            rank(7)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr7, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr7
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6), dimensions(7)])
               endif
            endselect
         endif
      endsubroutine omp_target_alloc_f_real128
#endif
#else
      module subroutine omp_target_alloc_f_real32_1(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I8P), intent(in)                    :: dimensions
         integer(I4P), intent(in)                    :: omp_dev
         integer(I8P), intent(in), optional          :: bounds(2)
         real(R4P), pointer, contiguous              :: fptr(:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(bounds(1):bounds(2)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
         endif
      endsubroutine omp_target_alloc_f_real32_1

      module subroutine omp_target_alloc_f_real32_2(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I8P), intent(in)                    :: dimensions(2)
         integer(I4P), intent(in)                    :: omp_dev
         integer(I8P), intent(in), optional          :: bounds(4)
         real(R4P), pointer, contiguous              :: fptr(:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
         endif
      endsubroutine omp_target_alloc_f_real32_2

      module subroutine omp_target_alloc_f_real32_3(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                    :: dimensions(3)
         integer(I4P), intent(in)                    :: omp_dev
         integer(I8P), intent(in), optional          :: bounds(6)
         real(R4P), pointer, contiguous              :: fptr(:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
         endif
      endsubroutine omp_target_alloc_f_real32_3

      module subroutine omp_target_alloc_f_real32_4(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                    :: dimensions(4)
         integer(I4P), intent(in)                    :: omp_dev
         integer(I8P), intent(in), optional          :: bounds(8)
         real(R4P), pointer, contiguous              :: fptr(:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
         endif
      endsubroutine omp_target_alloc_f_real32_4

      module subroutine omp_target_alloc_f_real32_5(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                    :: dimensions(5)
         integer(I4P), intent(in)                    :: omp_dev
         integer(I8P), intent(in), optional          :: bounds(10)
         real(R4P), pointer, contiguous              :: fptr(:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(bounds(1):bounds( 2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            endif
         endif
      endsubroutine omp_target_alloc_f_real32_5

      module subroutine omp_target_alloc_f_real32_6(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                    :: dimensions(6)
         integer(I4P), intent(in)                    :: omp_dev
         integer(I8P), intent(in), optional          :: bounds(12)
         real(R4P), pointer, contiguous              :: fptr(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            endif
         endif
      endsubroutine omp_target_alloc_f_real32_6

      module subroutine omp_target_alloc_f_real32_7(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                    :: dimensions(7)
         integer(I4P), intent(in)                    :: omp_dev
         integer(I8P), intent(in), optional          :: bounds(14)
         real(R4P), pointer, contiguous              :: fptr(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endif
         endif
      endsubroutine omp_target_alloc_f_real32_7

      module subroutine omp_target_alloc_f_real64_1(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I8P), intent(in)                    :: dimensions
         integer(I4P), intent(in)                    :: omp_dev
         integer(I8P), intent(in), optional          :: bounds(2)
         real(R8P), pointer, contiguous              :: fptr(:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(bounds(1):bounds(2)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
         endif
      endsubroutine omp_target_alloc_f_real64_1

      module subroutine omp_target_alloc_f_real64_2(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I8P), intent(in)                    :: dimensions(2)
         integer(I4P), intent(in)                    :: omp_dev
         integer(I8P), intent(in), optional          :: bounds(4)
         real(R8P), pointer, contiguous              :: fptr(:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
         endif
      endsubroutine omp_target_alloc_f_real64_2

      module subroutine omp_target_alloc_f_real64_3(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                    :: dimensions(3)
         integer(I4P), intent(in)                    :: omp_dev
         integer(I8P), intent(in), optional          :: bounds(6)
         real(R8P), pointer, contiguous              :: fptr(:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
         endif
      endsubroutine omp_target_alloc_f_real64_3

      module subroutine omp_target_alloc_f_real64_4(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                    :: dimensions(4)
         integer(I4P), intent(in)                    :: omp_dev
         integer(I8P), intent(in), optional          :: bounds(8)
         real(R8P), pointer, contiguous              :: fptr(:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
         endif
      endsubroutine omp_target_alloc_f_real64_4

      module subroutine omp_target_alloc_f_real64_5(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                    :: dimensions(5)
         integer(I4P), intent(in)                    :: omp_dev
         integer(I8P), intent(in), optional          :: bounds(10)
         real(R8P), pointer, contiguous              :: fptr(:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(bounds(1):bounds( 2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            endif
         endif
      endsubroutine omp_target_alloc_f_real64_5

      module subroutine omp_target_alloc_f_real64_6(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                    :: dimensions(6)
         integer(I4P), intent(in)                    :: omp_dev
         integer(I8P), intent(in), optional          :: bounds(12)
         real(R8P), pointer, contiguous              :: fptr(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            endif
         endif
      endsubroutine omp_target_alloc_f_real64_6

      module subroutine omp_target_alloc_f_real64_7(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                    :: dimensions(7)
         integer(I4P), intent(in)                    :: omp_dev
         integer(I8P), intent(in), optional          :: bounds(14)
         real(R8P), pointer, contiguous              :: fptr(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endif
         endif
      endsubroutine omp_target_alloc_f_real64_7

#if defined _real128
      module subroutine omp_target_alloc_f_real128_1(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I8P), intent(in)                     :: dimensions
         integer(I4P), intent(in)                     :: omp_dev
         integer(I8P), intent(in), optional           :: bounds(2)
         real(R16P), pointer, contiguous              :: fptr(:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(bounds(1):bounds(2)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
         endif
      endsubroutine omp_target_alloc_f_real128_1

      module subroutine omp_target_alloc_f_real128_2(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I8P), intent(in)                     :: dimensions(2)
         integer(I4P), intent(in)                     :: omp_dev
         integer(I8P), intent(in), optional           :: bounds(4)
         real(R16P), pointer, contiguous              :: fptr(:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
         endif
      endsubroutine omp_target_alloc_f_real128_2

      module subroutine omp_target_alloc_f_real128_3(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                     :: dimensions(3)
         integer(I4P), intent(in)                     :: omp_dev
         integer(I8P), intent(in), optional           :: bounds(6)
         real(R16P), pointer, contiguous              :: fptr(:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
         endif
      endsubroutine omp_target_alloc_f_real128_3

      module subroutine omp_target_alloc_f_real128_4(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                     :: dimensions(4)
         integer(I4P), intent(in)                     :: omp_dev
         integer(I8P), intent(in), optional           :: bounds(8)
         real(R16P), pointer, contiguous              :: fptr(:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
         endif
      endsubroutine omp_target_alloc_f_real128_4

      module subroutine omp_target_alloc_f_real128_5(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                     :: dimensions(5)
         integer(I4P), intent(in)                     :: omp_dev
         integer(I8P), intent(in), optional           :: bounds(10)
         real(R16P), pointer, contiguous              :: fptr(:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(bounds(1):bounds( 2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            endif
         endif
      endsubroutine omp_target_alloc_f_real128_5

      module subroutine omp_target_alloc_f_real128_6(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                     :: dimensions(6)
         integer(I4P), intent(in)                     :: omp_dev
         integer(I8P), intent(in), optional           :: bounds(12)
         real(R16P), pointer, contiguous              :: fptr(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            endif
         endif
      endsubroutine omp_target_alloc_f_real128_6

      module subroutine omp_target_alloc_f_real128_7(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                     :: dimensions(7)
         integer(I4P), intent(in)                     :: omp_dev
         integer(I8P), intent(in), optional           :: bounds(14)
         real(R16P), pointer, contiguous              :: fptr(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endif
         endif
      endsubroutine omp_target_alloc_f_real128_7
#endif
#endif

      ! OpenMP Target Alloc Complex Routines
#if defined _F2008
      module subroutine omp_target_alloc_f_cmplx32(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                         :: dimensions(:)
         integer(I4P), intent(in)                         :: omp_dev
         integer(I8P), intent(in), optional               :: bounds(:)
         type(c_ptr)                                      :: cptr_dev
         integer(kind=c_int)                              :: omp_device
         integer(I1P), pointer                            :: fptr1(:),       fptr2(:,:),       fptr3(:,:,:),       &
                                                             fptr4(:,:,:,:), fptr5(:,:,:,:,:), fptr6(:,:,:,:,:,:), &
                                                             fptr7(:,:,:,:,:,:,:)

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr, [dimensions])
                  fptr_dev(bounds(1):bounds(2)) => fptr
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
               endif
            rank(2)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr2, [dimensions(1), dimensions(2)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr2
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
               endif
            rank(3)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr3, [dimensions(1), dimensions(2), dimensions(3)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr3
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
               endif
            rank(4)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr4, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr4
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               endif
            rank(5)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr5, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10)) => fptr5
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5)])
               endif
            rank(6)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr6, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12)) => fptr6
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6)])
               endif
            rank(7)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr7, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr7
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6), dimensions(7)])
               endif
            endselect
         endif
      endsubroutine omp_target_alloc_f_cmplx32

      module subroutine omp_target_alloc_f_cmplx64(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                         :: dimensions(:)
         integer(I4P), intent(in)                         :: omp_dev
         integer(I8P), intent(in), optional               :: bounds(:)
         type(c_ptr)                                      :: cptr_dev
         integer(kind=c_int)                              :: omp_device
         integer(I1P), pointer                            :: fptr1(:),       fptr2(:,:),       fptr3(:,:,:),       &
                                                             fptr4(:,:,:,:), fptr5(:,:,:,:,:), fptr6(:,:,:,:,:,:), &
                                                             fptr7(:,:,:,:,:,:,:)

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr, [dimensions])
                  fptr_dev(bounds(1):bounds(2)) => fptr
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
               endif
            rank(2)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr2, [dimensions(1), dimensions(2)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr2
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
               endif
            rank(3)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr3, [dimensions(1), dimensions(2), dimensions(3)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr3
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
               endif
            rank(4)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr4, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr4
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               endif
            rank(5)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr5, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10)) => fptr5
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5)])
               endif
            rank(6)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr6, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12)) => fptr6
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6)])
               endif
            rank(7)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr7, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr7
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6), dimensions(7)])
               endif
            endselect
         endif
      endsubroutine omp_target_alloc_f_cmplx64

#if defined _real128
      module subroutine omp_target_alloc_f_cmplx128(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                          :: dimensions(:)
         integer(I4P), intent(in)                          :: omp_dev
         integer(I8P), intent(in), optional                :: bounds(:)
         type(c_ptr)                                       :: cptr_dev
         integer(kind=c_int)                               :: omp_device
         integer(I1P), pointer                             :: fptr1(:),       fptr2(:,:),       fptr3(:,:,:),       &
                                                              fptr4(:,:,:,:), fptr5(:,:,:,:,:), fptr6(:,:,:,:,:,:), &
                                                              fptr7(:,:,:,:,:,:,:)

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr, [dimensions])
                  fptr_dev(bounds(1):bounds(2)) => fptr
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
               endif
            rank(2)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr2, [dimensions(1), dimensions(2)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr2
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
               endif
            rank(3)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr3, [dimensions(1), dimensions(2), dimensions(3)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr3
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
               endif
            rank(4)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr4, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr4
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               endif
            rank(5)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr5, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
                  fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10)) => fptr5
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5)])
               endif
            rank(6)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr6, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12)) => fptr6
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6)])
               endif
            rank(7)
               if (present(bounds)) then
                  call c_f_pointer(cptr_dev, fptr7, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
                  fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                           bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr7
               else
                  call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                        dimensions(5), dimensions(6), dimensions(7)])
               endif
            endselect
         endif
      endsubroutine omp_target_alloc_f_cmplx128
#endif
#else
      module subroutine omp_target_alloc_f_cmplx32_1(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I8P), intent(in)                       :: dimensions
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(2)
         complex(R4P), pointer, contiguous              :: fptr(:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * dimensions, c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(bounds(1):bounds(2)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx32_1

      module subroutine omp_target_alloc_f_cmplx32_2(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I8P), intent(in)                       :: dimensions(2)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(4)
         complex(R4P), pointer, contiguous              :: fptr(:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx32_2

      module subroutine omp_target_alloc_f_cmplx32_3(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                       :: dimensions(3)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(6)
         complex(R4P), pointer, contiguous              :: fptr(:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx32_3

      module subroutine omp_target_alloc_f_cmplx32_4(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(4)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(8)
         complex(R4P), pointer, contiguous              :: fptr(:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx32_4

      module subroutine omp_target_alloc_f_cmplx32_5(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(5)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(10)
         complex(R4P), pointer, contiguous              :: fptr(:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(bounds(1):bounds( 2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx32_5

      module subroutine omp_target_alloc_f_cmplx32_6(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(6)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(12)
         complex(R4P), pointer, contiguous              :: fptr(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx32_6

      module subroutine omp_target_alloc_f_cmplx32_7(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(7)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(14)
         complex(R4P), pointer, contiguous              :: fptr(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx32_7

      module subroutine omp_target_alloc_f_cmplx64_1(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I8P), intent(in)                       :: dimensions
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(2)
         complex(R8P), pointer, contiguous              :: fptr(:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * dimensions, c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(bounds(1):bounds(2)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx64_1

      module subroutine omp_target_alloc_f_cmplx64_2(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I8P), intent(in)                       :: dimensions(2)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(4)
         complex(R8P), pointer, contiguous              :: fptr(:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx64_2

      module subroutine omp_target_alloc_f_cmplx64_3(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                       :: dimensions(3)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(6)
         complex(R8P), pointer, contiguous              :: fptr(:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx64_3

      module subroutine omp_target_alloc_f_cmplx64_4(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(4)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(8)
         complex(R8P), pointer, contiguous              :: fptr(:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx64_4

      module subroutine omp_target_alloc_f_cmplx64_5(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(5)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(10)
         complex(R8P), pointer, contiguous              :: fptr(:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(bounds(1):bounds( 2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx64_5

      module subroutine omp_target_alloc_f_cmplx64_6(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(6)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(12)
         complex(R8P), pointer, contiguous              :: fptr(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx64_6

      module subroutine omp_target_alloc_f_cmplx64_7(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(7)
         integer(I4P), intent(in)                       :: omp_dev
         integer(I8P), intent(in), optional             :: bounds(14)
         complex(R8P), pointer, contiguous              :: fptr(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx64_7

#if defined _real128
      module subroutine omp_target_alloc_f_cmplx128_1(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I8P), intent(in), optional              :: bounds(2)
         complex(R16P), pointer, contiguous              :: fptr(:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * dimensions, c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(bounds(1):bounds(2)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx128_1

      module subroutine omp_target_alloc_f_cmplx128_2(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I8P), intent(in), optional              :: bounds(4)
         complex(R16P), pointer, contiguous              :: fptr(:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx128_2

      module subroutine omp_target_alloc_f_cmplx128_3(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I8P), intent(in), optional              :: bounds(6)
         complex(R16P), pointer, contiguous              :: fptr(:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx128_3

      module subroutine omp_target_alloc_f_cmplx128_4(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I8P), intent(in), optional              :: bounds(8)
         complex(R16P), pointer, contiguous              :: fptr(:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx128_4

      module subroutine omp_target_alloc_f_cmplx128_5(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I8P), intent(in), optional              :: bounds(10)
         complex(R16P), pointer, contiguous              :: fptr(:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(bounds(1):bounds( 2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx128_5

      module subroutine omp_target_alloc_f_cmplx128_6(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I8P), intent(in), optional              :: bounds(12)
         complex(R16P), pointer, contiguous              :: fptr(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds(5):bounds(6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx128_6

      module subroutine omp_target_alloc_f_cmplx128_7(fptr_dev, dimensions, omp_dev, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I8P), intent(in), optional              :: bounds(14)
         complex(R16P), pointer, contiguous              :: fptr(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_alloc.i90"

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) then
            if (present(bounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(bounds(1):bounds( 2), bounds( 3):bounds( 4), bounds( 5):bounds( 6), bounds(7):bounds(8), &
                        bounds(9):bounds(10), bounds(11):bounds(12), bounds(13):bounds(14)) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endif
         endif
      endsubroutine omp_target_alloc_f_cmplx128_7
#endif
#endif

endsubmodule dmr_target_alloc
