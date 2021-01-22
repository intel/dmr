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
      module subroutine omp_target_alloc_f_int8(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                         :: dimensions(:)
         integer(I4P), intent(in)                         :: omp_dev
         type(c_ptr)                                      :: cptr_dev
         integer(kind=c_int)                              :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endselect
         endif
      endsubroutine omp_target_alloc_f_int8

      module subroutine omp_target_alloc_f_int16(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                         :: dimensions(:)
         integer(I4P), intent(in)                         :: omp_dev
         type(c_ptr)                                      :: cptr_dev
         integer(kind=c_int)                              :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endselect
         endif
      endsubroutine omp_target_alloc_f_int16

      module subroutine omp_target_alloc_f_int32(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                         :: dimensions(:)
         integer(I4P), intent(in)                         :: omp_dev
         type(c_ptr)                                      :: cptr_dev
         integer(kind=c_int)                              :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endselect
         endif
      endsubroutine omp_target_alloc_f_int32

      module subroutine omp_target_alloc_f_int64(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                         :: dimensions(:)
         integer(I4P), intent(in)                         :: omp_dev
         type(c_ptr)                                      :: cptr_dev
         integer(kind=c_int)                              :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endselect
         endif
      endsubroutine omp_target_alloc_f_int64

      ! OpenMP Target Alloc Real Routines
      module subroutine omp_target_alloc_f_real32(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                      :: dimensions(:)
         integer(I4P), intent(in)                      :: omp_dev
         type(c_ptr)                                   :: cptr_dev
         integer(kind=c_int)                           :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endselect
         endif
      endsubroutine omp_target_alloc_f_real32

      module subroutine omp_target_alloc_f_real64(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                      :: dimensions(:)
         integer(I4P), intent(in)                      :: omp_dev
         type(c_ptr)                                   :: cptr_dev
         integer(kind=c_int)                           :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endselect
         endif
      endsubroutine omp_target_alloc_f_real64

#if defined _real128
      module subroutine omp_target_alloc_f_real128(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                       :: dimensions(:)
         integer(I4P), intent(in)                       :: omp_dev
         type(c_ptr)                                    :: cptr_dev
         integer(kind=c_int)                            :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endselect
         endif
      endsubroutine omp_target_alloc_f_real128
#endif

      ! OpenMP Target Alloc Complex Routines
      module subroutine omp_target_alloc_f_cmplx32(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                         :: dimensions(:)
         integer(I4P), intent(in)                         :: omp_dev
         type(c_ptr)                                      :: cptr_dev
         integer(kind=c_int)                              :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endselect
         endif
      endsubroutine omp_target_alloc_f_cmplx32

      module subroutine omp_target_alloc_f_cmplx64(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                         :: dimensions(:)
         integer(I4P), intent(in)                         :: omp_dev
         type(c_ptr)                                      :: cptr_dev
         integer(kind=c_int)                              :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endselect
         endif
      endsubroutine omp_target_alloc_f_cmplx64

#if defined _real128
      module subroutine omp_target_alloc_f_cmplx128(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(..)
         integer(I8P), intent(in)                          :: dimensions(:)
         integer(I4P), intent(in)                          :: omp_dev
         type(c_ptr)                                       :: cptr_dev
         integer(kind=c_int)                               :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_dev)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                     dimensions(5), dimensions(6), dimensions(7)])
            endselect
         endif
      endsubroutine omp_target_alloc_f_cmplx128
#endif

endsubmodule dmr_target_alloc
