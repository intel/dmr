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

submodule (dmr) dmr_get_mapped_ptr
   use, intrinsic :: iso_c_binding
   use omp_lib
   use dmr_environment
   use dmr_c_functions

   implicit none

   contains

#if defined _OpenMP_5_1
#if defined _F2008
      ! OpenMP Get Mapped Pointer Integer Routines
      module subroutine omp_get_mapped_ptr_f_int8(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I1P), pointer, intent(out) :: fptr_dev(..)
         integer(I1P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)           :: omp_dev
         type(c_ptr)                        :: cptr_dev
         integer(kind=c_int)                :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_get_mapped_ptr_c(c_loc(fptr_hos), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_hos)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                     size(fptr_hos,7)])
            endselect
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int8

      module subroutine omp_get_mapped_ptr_f_int16(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I2P), pointer, intent(out) :: fptr_dev(..)
         integer(I2P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)           :: omp_dev
         type(c_ptr)                        :: cptr_dev
         integer(kind=c_int)                :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_get_mapped_ptr_c(c_loc(fptr_hos), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_hos)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                     size(fptr_hos,7)])
            endselect
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int16

      module subroutine omp_get_mapped_ptr_f_int32(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I4P), pointer, intent(out) :: fptr_dev(..)
         integer(I4P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)           :: omp_dev
         type(c_ptr)                        :: cptr_dev
         integer(kind=c_int)                :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_get_mapped_ptr_c(c_loc(fptr_hos), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_hos)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                     size(fptr_hos,7)])
            endselect
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int32

      module subroutine omp_get_mapped_ptr_f_int64(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I8P), pointer, intent(out) :: fptr_dev(..)
         integer(I8P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)           :: omp_dev
         type(c_ptr)                        :: cptr_dev
         integer(kind=c_int)                :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_get_mapped_ptr_c(c_loc(fptr_hos), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_hos)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                     size(fptr_hos,7)])
            endselect
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int64

      ! OpenMP Get Mapped Pointer Real Routines
      module subroutine omp_get_mapped_ptr_f_real32(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R4P), pointer, intent(out) :: fptr_dev(..)
         real(R4P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)        :: omp_dev
         type(c_ptr)                     :: cptr_dev
         integer(kind=c_int)             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_get_mapped_ptr_c(c_loc(fptr_hos), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_hos)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                     size(fptr_hos,7)])
            endselect
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real32

      module subroutine omp_get_mapped_ptr_f_real64(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R8P), pointer, intent(out) :: fptr_dev(..)
         real(R8P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)        :: omp_dev
         type(c_ptr)                     :: cptr_dev
         integer(kind=c_int)             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_get_mapped_ptr_c(c_loc(fptr_hos), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_hos)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                     size(fptr_hos,7)])
            endselect
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real64

#if defined _real128
      module subroutine omp_get_mapped_ptr_f_real128(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R16P), pointer, intent(out) :: fptr_dev(..)
         real(R16P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)         :: omp_dev
         type(c_ptr)                      :: cptr_dev
         integer(kind=c_int)              :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_get_mapped_ptr_c(c_loc(fptr_hos), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_hos)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                     size(fptr_hos,7)])
            endselect
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real128
#endif

      ! OpenMP Get Mapped Pointer Complex Routines
      module subroutine omp_get_mapped_ptr_f_cmplx32(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R4P), pointer, intent(out) :: fptr_dev(..)
         complex(R4P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)           :: omp_dev
         type(c_ptr)                        :: cptr_dev
         integer(kind=c_int)                :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_get_mapped_ptr_c(c_loc(fptr_hos), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_hos)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                     size(fptr_hos,7)])
            endselect
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx32

      module subroutine omp_get_mapped_ptr_f_cmplx64(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R8P), pointer, intent(out) :: fptr_dev(..)
         complex(R8P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)           :: omp_dev
         type(c_ptr)                        :: cptr_dev
         integer(kind=c_int)                :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_get_mapped_ptr_c(c_loc(fptr_hos), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_hos)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                     size(fptr_hos,7)])
            endselect
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx64

#if defined _real128
      module subroutine omp_get_mapped_ptr_f_cmplx128(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R16P), pointer, intent(out) :: fptr_dev(..)
         complex(R16P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)            :: omp_dev
         type(c_ptr)                         :: cptr_dev
         integer(kind=c_int)                 :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = omp_get_mapped_ptr_c(c_loc(fptr_hos), omp_device)

         if (c_associated(cptr_dev)) then
            select rank (fptr_hos)
            rank(1)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
            rank(2)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
            rank(3)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
            rank(4)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4)])
            rank(5)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5)])
            rank(6)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
            rank(7)
               call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                     size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                     size(fptr_hos,7)])
            endselect
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx128
#endif
#else
      module subroutine omp_get_mapped_ptr_f_int8_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I1P), target,  contiguous, intent(in)  :: fptr_hos(:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int8_1

      module subroutine omp_get_mapped_ptr_f_int8_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I1P), target,  contiguous, intent(in)  :: fptr_hos(:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int8_2

      module subroutine omp_get_mapped_ptr_f_int8_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I1P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int8_3

      module subroutine omp_get_mapped_ptr_f_int8_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I1P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int8_4

      module subroutine omp_get_mapped_ptr_f_int8_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I1P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int8_5

      module subroutine omp_get_mapped_ptr_f_int8_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I1P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int8_6

      module subroutine omp_get_mapped_ptr_f_int8_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I1P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                  size(fptr_hos,7)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int8_7

      module subroutine omp_get_mapped_ptr_f_int16_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I2P), target,  contiguous, intent(in)  :: fptr_hos(:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int16_1

      module subroutine omp_get_mapped_ptr_f_int16_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I2P), target,  contiguous, intent(in)  :: fptr_hos(:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int16_2

      module subroutine omp_get_mapped_ptr_f_int16_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I2P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int16_3

      module subroutine omp_get_mapped_ptr_f_int16_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I2P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int16_4

      module subroutine omp_get_mapped_ptr_f_int16_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I2P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int16_5

      module subroutine omp_get_mapped_ptr_f_int16_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I2P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int16_6

      module subroutine omp_get_mapped_ptr_f_int16_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I2P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                  size(fptr_hos,7)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int16_7

      module subroutine omp_get_mapped_ptr_f_int32_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I4P), target,  contiguous, intent(in)  :: fptr_hos(:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int32_1

      module subroutine omp_get_mapped_ptr_f_int32_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I4P), target,  contiguous, intent(in)  :: fptr_hos(:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int32_2

      module subroutine omp_get_mapped_ptr_f_int32_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I4P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int32_3

      module subroutine omp_get_mapped_ptr_f_int32_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I4P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int32_4

      module subroutine omp_get_mapped_ptr_f_int32_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I4P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int32_5

      module subroutine omp_get_mapped_ptr_f_int32_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int32_6

      module subroutine omp_get_mapped_ptr_f_int32_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                  size(fptr_hos,7)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int32_7

      module subroutine omp_get_mapped_ptr_f_int64_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I8P), target,  contiguous, intent(in)  :: fptr_hos(:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int64_1

      module subroutine omp_get_mapped_ptr_f_int64_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I8P), target,  contiguous, intent(in)  :: fptr_hos(:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int64_2

      module subroutine omp_get_mapped_ptr_f_int64_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I8P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int64_3

      module subroutine omp_get_mapped_ptr_f_int64_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I8P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int64_4

      module subroutine omp_get_mapped_ptr_f_int64_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I8P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int64_5

      module subroutine omp_get_mapped_ptr_f_int64_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int64_6

      module subroutine omp_get_mapped_ptr_f_int64_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                  size(fptr_hos,7)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_int64_7

      ! OpenMP Target Alloc Real Routines
      module subroutine omp_get_mapped_ptr_f_real32_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out) :: fptr_dev(:)
         real(R4P), target,  contiguous, intent(in)  :: fptr_hos(:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real32_1

      module subroutine omp_get_mapped_ptr_f_real32_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         real(R4P), target,  contiguous, intent(in)  :: fptr_hos(:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real32_2

      module subroutine omp_get_mapped_ptr_f_real32_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         real(R4P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real32_3

      module subroutine omp_get_mapped_ptr_f_real32_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         real(R4P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real32_4

      module subroutine omp_get_mapped_ptr_f_real32_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         real(R4P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real32_5

      module subroutine omp_get_mapped_ptr_f_real32_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         real(R4P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real32_6

      module subroutine omp_get_mapped_ptr_f_real32_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         real(R4P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                  size(fptr_hos,7)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real32_7

      module subroutine omp_get_mapped_ptr_f_real64_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out) :: fptr_dev(:)
         real(R8P), target,  contiguous, intent(in)  :: fptr_hos(:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real64_1

      module subroutine omp_get_mapped_ptr_f_real64_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         real(R8P), target,  contiguous, intent(in)  :: fptr_hos(:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real64_2

      module subroutine omp_get_mapped_ptr_f_real64_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         real(R8P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real64_3

      module subroutine omp_get_mapped_ptr_f_real64_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         real(R8P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real64_4

      module subroutine omp_get_mapped_ptr_f_real64_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         real(R8P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real64_5

      module subroutine omp_get_mapped_ptr_f_real64_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         real(R8P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real64_6

      module subroutine omp_get_mapped_ptr_f_real64_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         real(R8P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                  size(fptr_hos,7)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real64_7

#if defined _real128
      module subroutine omp_get_mapped_ptr_f_real128_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out) :: fptr_dev(:)
         real(R16P), target,  contiguous, intent(in)  :: fptr_hos(:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real128_1

      module subroutine omp_get_mapped_ptr_f_real128_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         real(R16P), target,  contiguous, intent(in)  :: fptr_hos(:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real128_2

      module subroutine omp_get_mapped_ptr_f_real128_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         real(R16P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real128_3

      module subroutine omp_get_mapped_ptr_f_real128_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         real(R16P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real128_4

      module subroutine omp_get_mapped_ptr_f_real128_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         real(R16P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real128_5

      module subroutine omp_get_mapped_ptr_f_real128_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         real(R16P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real128_6

      module subroutine omp_get_mapped_ptr_f_real128_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         real(R16P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                  size(fptr_hos,7)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_real128_7
#endif

      ! OpenMP Target Alloc Complex Routines
      module subroutine omp_get_mapped_ptr_f_cmplx32_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out) :: fptr_dev(:)
         complex(R4P), target,  contiguous, intent(in)  :: fptr_hos(:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx32_1

      module subroutine omp_get_mapped_ptr_f_cmplx32_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         complex(R4P), target,  contiguous, intent(in)  :: fptr_hos(:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx32_2

      module subroutine omp_get_mapped_ptr_f_cmplx32_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         complex(R4P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx32_3

      module subroutine omp_get_mapped_ptr_f_cmplx32_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         complex(R4P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx32_4

      module subroutine omp_get_mapped_ptr_f_cmplx32_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         complex(R4P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx32_5

      module subroutine omp_get_mapped_ptr_f_cmplx32_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         complex(R4P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx32_6

      module subroutine omp_get_mapped_ptr_f_cmplx32_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         complex(R4P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                  size(fptr_hos,7)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx32_7

      module subroutine omp_get_mapped_ptr_f_cmplx64_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out) :: fptr_dev(:)
         complex(R8P), target,  contiguous, intent(in)  :: fptr_hos(:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx64_1

      module subroutine omp_get_mapped_ptr_f_cmplx64_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         complex(R8P), target,  contiguous, intent(in)  :: fptr_hos(:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx64_2

      module subroutine omp_get_mapped_ptr_f_cmplx64_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         complex(R8P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx64_3

      module subroutine omp_get_mapped_ptr_f_cmplx64_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         complex(R8P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx64_4

      module subroutine omp_get_mapped_ptr_f_cmplx64_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         complex(R8P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx64_5

      module subroutine omp_get_mapped_ptr_f_cmplx64_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         complex(R8P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx64_6

      module subroutine omp_get_mapped_ptr_f_cmplx64_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         complex(R8P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                  size(fptr_hos,7)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx64_7

#if defined _cmplx128
      module subroutine omp_get_mapped_ptr_f_cmplx128_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:)
         complex(R16P), target,  contiguous, intent(in)  :: fptr_hos(:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx128_1

      module subroutine omp_get_mapped_ptr_f_cmplx128_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         complex(R16P), target,  contiguous, intent(in)  :: fptr_hos(:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx128_2

      module subroutine omp_get_mapped_ptr_f_cmplx128_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         complex(R16P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx128_3

      module subroutine omp_get_mapped_ptr_f_cmplx128_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         complex(R16P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx128_4

      module subroutine omp_get_mapped_ptr_f_cmplx128_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         complex(R16P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx128_5

      module subroutine omp_get_mapped_ptr_f_cmplx128_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         complex(R16P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx128_6

      module subroutine omp_get_mapped_ptr_f_cmplx128_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         complex(R16P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:,:)
         include "src/lib/submodules/include/dmr_get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            call c_f_pointer(cptr_dev, fptr_dev, [size(fptr_hos,1), size(fptr_hos,2), size(fptr_hos,3), &
                                                  size(fptr_hos,4), size(fptr_hos,5), size(fptr_hos,6), &
                                                  size(fptr_hos,7)])
         else
            fptr_dev => null()
         endif
      endsubroutine omp_get_mapped_ptr_f_cmplx128_7
#endif
#endif
#endif
endsubmodule dmr_get_mapped_ptr
