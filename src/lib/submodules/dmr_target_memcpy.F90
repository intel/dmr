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

submodule (dmr) dmr_target_memcpy
   use, intrinsic :: iso_c_binding
   use omp_lib
   use dmr_environment
   use dmr_c_functions

   implicit none

   contains
      ! OpenMP Target Memcpy Integer Routines
      module subroutine omp_target_memcpy_f_int8(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(..)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(out)                     :: ierr
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                      :: dst_off, src_off
         integer(I8P)                                  :: n_elements
         integer(c_size_t)                             :: total_dim, omp_dst_offset, omp_src_offset
         type(c_ptr)                                   :: cptr_dst, cptr_src
         integer(c_int)                                :: omp_dst_device, omp_src_device

         if (omp_src_dev == omp_get_initial_device_c()) then
            n_elements = size(fptr_src)
         else
            n_elements = size(fptr_dst)
         endif

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int8

      module subroutine omp_target_memcpy_f_int16(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(..)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(out)                     :: ierr
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                      :: dst_off, src_off
         integer(I8P)                                  :: n_elements
         integer(c_size_t)                             :: total_dim, omp_dst_offset, omp_src_offset
         type(c_ptr)                                   :: cptr_dst, cptr_src
         integer(c_int)                                :: omp_dst_device, omp_src_device

         if (omp_src_dev == omp_get_initial_device_c()) then
            n_elements = size(fptr_src)
         else
            n_elements = size(fptr_dst)
         endif

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int16

      module subroutine omp_target_memcpy_f_int32(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(..)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(out)                     :: ierr
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                      :: dst_off, src_off
         integer(I8P)                                  :: n_elements
         integer(c_size_t)                             :: total_dim, omp_dst_offset, omp_src_offset
         type(c_ptr)                                   :: cptr_dst, cptr_src
         integer(c_int)                                :: omp_dst_device, omp_src_device

         if (omp_src_dev == omp_get_initial_device_c()) then
            n_elements = size(fptr_src)
         else
            n_elements = size(fptr_dst)
         endif

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int32

      module subroutine omp_target_memcpy_f_int64(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(..)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(out)                     :: ierr
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                      :: dst_off, src_off
         integer(I8P)                                  :: n_elements
         integer(c_size_t)                             :: total_dim, omp_dst_offset, omp_src_offset
         type(c_ptr)                                   :: cptr_dst, cptr_src
         integer(c_int)                                :: omp_dst_device, omp_src_device

         if (omp_src_dev == omp_get_initial_device_c()) then
            n_elements = size(fptr_src)
         else
            n_elements = size(fptr_dst)
         endif

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int64

      ! OpenMP Target Memcpy Real Routines
      module subroutine omp_target_memcpy_f_real32(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(..)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(out)                  :: ierr
         integer(I4P), intent(in)                   :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                   :: dst_off, src_off
         integer(I8P)                               :: n_elements
         integer(c_size_t)                          :: total_dim, omp_dst_offset, omp_src_offset
         type(c_ptr)                                :: cptr_dst, cptr_src
         integer(c_int)                             :: omp_dst_device, omp_src_device

         if (omp_src_dev == omp_get_initial_device_c()) then
            n_elements = size(fptr_src)
         else
            n_elements = size(fptr_dst)
         endif

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real32

      module subroutine omp_target_memcpy_f_real64(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(..)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(out)                  :: ierr
         integer(I4P), intent(in)                   :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                   :: dst_off, src_off
         integer(I8P)                               :: n_elements
         integer(c_size_t)                          :: total_dim, omp_dst_offset, omp_src_offset
         type(c_ptr)                                :: cptr_dst, cptr_src
         integer(c_int)                             :: omp_dst_device, omp_src_device

         if (omp_src_dev == omp_get_initial_device_c()) then
            n_elements = size(fptr_src)
         else
            n_elements = size(fptr_dst)
         endif

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real64

#if defined _real128
      module subroutine omp_target_memcpy_f_real128(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(..)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(out)                   :: ierr
         integer(I4P), intent(in)                    :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                    :: dst_off, src_off
         integer(I8P)                                :: n_elements
         integer(c_size_t)                           :: total_dim, omp_dst_offset, omp_src_offset
         type(c_ptr)                                 :: cptr_dst, cptr_src
         integer(c_int)                              :: omp_dst_device, omp_src_device

         if (omp_src_dev == omp_get_initial_device_c()) then
            n_elements = size(fptr_src)
         else
            n_elements = size(fptr_dst)
         endif

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real128
#endif

      ! OpenMP Target Memcpy Complex Routines
      module subroutine omp_target_memcpy_f_cmplx32(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(..)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(out)                     :: ierr
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                      :: dst_off, src_off
         integer(I8P)                                  :: n_elements
         integer(c_size_t)                             :: total_dim, omp_dst_offset, omp_src_offset
         type(c_ptr)                                   :: cptr_dst, cptr_src
         integer(c_int)                                :: omp_dst_device, omp_src_device

         if (omp_src_dev == omp_get_initial_device_c()) then
            n_elements = size(fptr_src)
         else
            n_elements = size(fptr_dst)
         endif

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx32

      module subroutine omp_target_memcpy_f_cmplx64(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(..)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(out)                     :: ierr
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                      :: dst_off, src_off
         integer(I8P)                                  :: n_elements
         integer(c_size_t)                             :: total_dim, omp_dst_offset, omp_src_offset
         type(c_ptr)                                   :: cptr_dst, cptr_src
         integer(c_int)                                :: omp_dst_device, omp_src_device

         if (omp_src_dev == omp_get_initial_device_c()) then
            n_elements = size(fptr_src)
         else
            n_elements = size(fptr_dst)
         endif

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx64

#if defined _real128
      module subroutine omp_target_memcpy_f_cmplx128(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(..)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(out)                      :: ierr
         integer(I4P), intent(in)                       :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                       :: dst_off, src_off
         integer(I8P)                                   :: n_elements
         integer(c_size_t)                              :: total_dim, omp_dst_offset, omp_src_offset
         type(c_ptr)                                    :: cptr_dst, cptr_src
         integer(c_int)                                 :: omp_dst_device, omp_src_device

         if (omp_src_dev == omp_get_initial_device_c()) then
            n_elements = size(fptr_src)
         else
            n_elements = size(fptr_dst)
         endif

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx128
#endif

endsubmodule dmr_target_memcpy
