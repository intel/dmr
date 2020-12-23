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
      module subroutine omp_target_memcpy_f_int8(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), intent(out) :: sc_dst
         integer(I1P), intent(in)  :: sc_src
         integer(I4P), intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(tofrom:sc_src) is_device_ptr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(tofrom:sc_dst) is_device_ptr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_int8

      module subroutine omp_target_memcpy_f_int8_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int8_1

      module subroutine omp_target_memcpy_f_int8_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int8_2

      module subroutine omp_target_memcpy_f_int8_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int8_3

      module subroutine omp_target_memcpy_f_int8_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int8_4

      module subroutine omp_target_memcpy_f_int8_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int8_5

      module subroutine omp_target_memcpy_f_int8_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int8_6

      module subroutine omp_target_memcpy_f_int8_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int8_7

      module subroutine omp_target_memcpy_f_int16(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), intent(out) :: sc_dst
         integer(I2P), intent(in)  :: sc_src
         integer(I4P), intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(tofrom:sc_src) is_device_ptr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(tofrom:sc_dst) is_device_ptr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_int16

      module subroutine omp_target_memcpy_f_int16_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int16_1

      module subroutine omp_target_memcpy_f_int16_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int16_2

      module subroutine omp_target_memcpy_f_int16_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int16_3

      module subroutine omp_target_memcpy_f_int16_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int16_4

      module subroutine omp_target_memcpy_f_int16_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int16_5

      module subroutine omp_target_memcpy_f_int16_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int16_6

      module subroutine omp_target_memcpy_f_int16_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int16_7

      module subroutine omp_target_memcpy_f_int32(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), intent(out) :: sc_dst
         integer(I4P), intent(in)  :: sc_src
         integer(I4P), intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(tofrom:sc_src) is_device_ptr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(tofrom:sc_dst) is_device_ptr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_int32

      module subroutine omp_target_memcpy_f_int32_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int32_1

      module subroutine omp_target_memcpy_f_int32_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int32_2

      module subroutine omp_target_memcpy_f_int32_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int32_3

      module subroutine omp_target_memcpy_f_int32_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int32_4

      module subroutine omp_target_memcpy_f_int32_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int32_5

      module subroutine omp_target_memcpy_f_int32_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int32_6

      module subroutine omp_target_memcpy_f_int32_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int32_7

      module subroutine omp_target_memcpy_f_int64(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), intent(out) :: sc_dst
         integer(I8P), intent(in)  :: sc_src
         integer(I4P), intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(tofrom:sc_src) is_device_ptr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(tofrom:sc_dst) is_device_ptr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_int64

      module subroutine omp_target_memcpy_f_int64_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int64_1

      module subroutine omp_target_memcpy_f_int64_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int64_2

      module subroutine omp_target_memcpy_f_int64_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int64_3

      module subroutine omp_target_memcpy_f_int64_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int64_4

      module subroutine omp_target_memcpy_f_int64_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int64_5

      module subroutine omp_target_memcpy_f_int64_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int64_6

      module subroutine omp_target_memcpy_f_int64_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_int64_7

      ! OpenMP Target Memcpy Real Routines
      module subroutine omp_target_memcpy_f_real32(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    intent(out) :: sc_dst
         real(R4P),    intent(in)  :: sc_src
         integer(I4P), intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(tofrom:sc_src) is_device_ptr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(tofrom:sc_dst) is_device_ptr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_real32

      module subroutine omp_target_memcpy_f_real32_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real32_1

      module subroutine omp_target_memcpy_f_real32_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real32_2

      module subroutine omp_target_memcpy_f_real32_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real32_3

      module subroutine omp_target_memcpy_f_real32_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real32_4

      module subroutine omp_target_memcpy_f_real32_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real32_5

      module subroutine omp_target_memcpy_f_real32_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real32_6

      module subroutine omp_target_memcpy_f_real32_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real32_7

      module subroutine omp_target_memcpy_f_real64(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    intent(out) :: sc_dst
         real(R8P),    intent(in)  :: sc_src
         integer(I4P), intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(tofrom:sc_src) is_device_ptr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(tofrom:sc_dst) is_device_ptr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_real64

      module subroutine omp_target_memcpy_f_real64_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real64_1

      module subroutine omp_target_memcpy_f_real64_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real64_2

      module subroutine omp_target_memcpy_f_real64_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real64_3

      module subroutine omp_target_memcpy_f_real64_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real64_4

      module subroutine omp_target_memcpy_f_real64_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real64_5

      module subroutine omp_target_memcpy_f_real64_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real64_6

      module subroutine omp_target_memcpy_f_real64_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real64_7

#if defined _real128
      module subroutine omp_target_memcpy_f_real128(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   intent(out) :: sc_dst
         real(R16P),   intent(in)  :: sc_src
         integer(I4P), intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(tofrom:sc_src) is_device_ptr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(tofrom:sc_dst) is_device_ptr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_real128

      module subroutine omp_target_memcpy_f_real128_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real128_1

      module subroutine omp_target_memcpy_f_real128_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real128_2

      module subroutine omp_target_memcpy_f_real128_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real128_3

      module subroutine omp_target_memcpy_f_real128_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real128_4

      module subroutine omp_target_memcpy_f_real128_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real128_5

      module subroutine omp_target_memcpy_f_real128_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real128_6

      module subroutine omp_target_memcpy_f_real128_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_real128_7
#endif

      ! OpenMP Target Memcpy Complex Routines
      module subroutine omp_target_memcpy_f_cmplx32(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), intent(out) :: sc_dst
         complex(R4P), intent(in)  :: sc_src
         integer(I4P), intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(tofrom:sc_src) is_device_ptr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(tofrom:sc_dst) is_device_ptr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_cmplx32

      module subroutine omp_target_memcpy_f_cmplx32_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx32_1

      module subroutine omp_target_memcpy_f_cmplx32_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx32_2

      module subroutine omp_target_memcpy_f_cmplx32_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx32_3

      module subroutine omp_target_memcpy_f_cmplx32_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx32_4

      module subroutine omp_target_memcpy_f_cmplx32_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx32_5

      module subroutine omp_target_memcpy_f_cmplx32_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx32_6

      module subroutine omp_target_memcpy_f_cmplx32_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx32_7

      module subroutine omp_target_memcpy_f_cmplx64(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), intent(out) :: sc_dst
         complex(R8P), intent(in)  :: sc_src
         integer(I4P), intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(tofrom:sc_src) is_device_ptr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(tofrom:sc_dst) is_device_ptr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_cmplx64

      module subroutine omp_target_memcpy_f_cmplx64_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx64_1

      module subroutine omp_target_memcpy_f_cmplx64_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx64_2

      module subroutine omp_target_memcpy_f_cmplx64_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx64_3

      module subroutine omp_target_memcpy_f_cmplx64_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx64_4

      module subroutine omp_target_memcpy_f_cmplx64_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx64_5

      module subroutine omp_target_memcpy_f_cmplx64_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx64_6

      module subroutine omp_target_memcpy_f_cmplx64_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx64_7

#if defined _real128
      module subroutine omp_target_memcpy_f_cmplx128(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), intent(out) :: sc_dst
         complex(R16P), intent(in)  :: sc_src
         integer(I4P),  intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(tofrom:sc_src) is_device_ptr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(tofrom:sc_dst) is_device_ptr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_cmplx128

      module subroutine omp_target_memcpy_f_cmplx128_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx128_1

      module subroutine omp_target_memcpy_f_cmplx128_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx128_2

      module subroutine omp_target_memcpy_f_cmplx128_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx128_3

      module subroutine omp_target_memcpy_f_cmplx128_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx128_4

      module subroutine omp_target_memcpy_f_cmplx128_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx128_5

      module subroutine omp_target_memcpy_f_cmplx128_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx128_6

      module subroutine omp_target_memcpy_f_cmplx128_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"

         n_elements = size(fptr_src)

         omp_dst_offset = int(dst_off, c_size_t)
         omp_src_offset = int(src_off, c_size_t)
         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)
      endsubroutine omp_target_memcpy_f_cmplx128_7
#endif

endsubmodule dmr_target_memcpy
