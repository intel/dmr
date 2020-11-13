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

submodule (dmr) dmr_target_memcpy_rect
   use, intrinsic :: iso_c_binding
   use omp_lib
   use dmr_environment
   use dmr_c_functions

   implicit none

   contains

     ! OpenMP Target Memcpy Rect Integer Routines
      module subroutine omp_target_memcpy_rect_f_int8_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_int8_2

      module subroutine omp_target_memcpy_rect_f_int8_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int8_3

      module subroutine omp_target_memcpy_rect_f_int8_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int8_4

      module subroutine omp_target_memcpy_rect_f_int8_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int8_5

      module subroutine omp_target_memcpy_rect_f_int8_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int8_6

      module subroutine omp_target_memcpy_rect_f_int8_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int8_7

      module subroutine omp_target_memcpy_rect_f_int16_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int16_2

      module subroutine omp_target_memcpy_rect_f_int16_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int16_3

      module subroutine omp_target_memcpy_rect_f_int16_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int16_4

      module subroutine omp_target_memcpy_rect_f_int16_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int16_5

      module subroutine omp_target_memcpy_rect_f_int16_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int16_6

      module subroutine omp_target_memcpy_rect_f_int16_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int16_7

      module subroutine omp_target_memcpy_rect_f_int32_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int32_2

      module subroutine omp_target_memcpy_rect_f_int32_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int32_3

      module subroutine omp_target_memcpy_rect_f_int32_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int32_4

      module subroutine omp_target_memcpy_rect_f_int32_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int32_5

      module subroutine omp_target_memcpy_rect_f_int32_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int32_6

      module subroutine omp_target_memcpy_rect_f_int32_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int32_7

      module subroutine omp_target_memcpy_rect_f_int64_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int64_2

      module subroutine omp_target_memcpy_rect_f_int64_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int64_3

      module subroutine omp_target_memcpy_rect_f_int64_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int64_4

      module subroutine omp_target_memcpy_rect_f_int64_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int64_5

      module subroutine omp_target_memcpy_rect_f_int64_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int64_6

      module subroutine omp_target_memcpy_rect_f_int64_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_int64_7

     ! OpenMP Target Memcpy Rect Real Routines
      module subroutine omp_target_memcpy_rect_f_real32_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R4P),    contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_real32_2

      module subroutine omp_target_memcpy_rect_f_real32_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R4P),    contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_real32_3

      module subroutine omp_target_memcpy_rect_f_real32_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R4P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_real32_4

      module subroutine omp_target_memcpy_rect_f_real32_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R4P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_real32_5

      module subroutine omp_target_memcpy_rect_f_real32_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R4P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_real32_6

      module subroutine omp_target_memcpy_rect_f_real32_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R4P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_real32_7

      module subroutine omp_target_memcpy_rect_f_real64_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R8P),    contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_real64_2

      module subroutine omp_target_memcpy_rect_f_real64_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R8P),    contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_real64_3

      module subroutine omp_target_memcpy_rect_f_real64_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R8P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_real64_4

      module subroutine omp_target_memcpy_rect_f_real64_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R8P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_real64_5

      module subroutine omp_target_memcpy_rect_f_real64_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R8P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_real64_6

      module subroutine omp_target_memcpy_rect_f_real64_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R8P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_real64_7

#if defined _real128
      module subroutine omp_target_memcpy_rect_f_real128_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R16P),   contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_real128_2

      module subroutine omp_target_memcpy_rect_f_real128_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R16P),   contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_real128_3

      module subroutine omp_target_memcpy_rect_f_real128_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R16P),   contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      end subroutine omp_target_memcpy_rect_f_real128_4

      module subroutine omp_target_memcpy_rect_f_real128_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R16P),   contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_real128_5

      module subroutine omp_target_memcpy_rect_f_real128_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R16P),   contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_real128_6

      module subroutine omp_target_memcpy_rect_f_real128_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R16P),   contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_real128_7
#endif

     ! OpenMP Target Memcpy Rect Complex Routines
      module subroutine omp_target_memcpy_rect_f_cmplx32_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(2_I8P * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_cmplx32_2

      module subroutine omp_target_memcpy_rect_f_cmplx32_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(2_I8P * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_cmplx32_3

      module subroutine omp_target_memcpy_rect_f_cmplx32_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(2_I8P * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_cmplx32_4

      module subroutine omp_target_memcpy_rect_f_cmplx32_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(2_I8P * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_cmplx32_5

      module subroutine omp_target_memcpy_rect_f_cmplx32_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(2_I8P * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_cmplx32_6

      module subroutine omp_target_memcpy_rect_f_cmplx32_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(2_I8P * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_cmplx32_7

      module subroutine omp_target_memcpy_rect_f_cmplx64_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(2_I8P * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_cmplx64_2

      module subroutine omp_target_memcpy_rect_f_cmplx64_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(2_I8P * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_cmplx64_3

      module subroutine omp_target_memcpy_rect_f_cmplx64_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(2_I8P * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_cmplx64_4

      module subroutine omp_target_memcpy_rect_f_cmplx64_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(2_I8P * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_cmplx64_5

      module subroutine omp_target_memcpy_rect_f_cmplx64_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(2_I8P * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_cmplx64_6

      module subroutine omp_target_memcpy_rect_f_cmplx64_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(2_I8P * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_cmplx64_7

#if defined _real128
      module subroutine omp_target_memcpy_rect_f_cmplx128_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P),  parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(2_I8P * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_cmplx128_2

      module subroutine omp_target_memcpy_rect_f_cmplx128_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P),  parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(2_I8P * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_cmplx128_3

      module subroutine omp_target_memcpy_rect_f_cmplx128_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P),  parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(2_I8P * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_cmplx128_4

      module subroutine omp_target_memcpy_rect_f_cmplx128_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P),  parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(2_I8P * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_cmplx128_5

      module subroutine omp_target_memcpy_rect_f_cmplx128_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P),  parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(2_I8P * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_cmplx128_6

      module subroutine omp_target_memcpy_rect_f_cmplx128_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P),  parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"

         fptr_rank = int(rank(fptr_dst), c_int)

         do i=fptr_dims, 1, -1
            volume_dims(i)   = int(cpy_dims(fptr_dims-i+1), c_size_t)
            cptr_dst_dims(i) = int(size(fptr_dst,fptr_dims-i+1), c_size_t)
            cptr_src_dims(i) = int(size(fptr_src,fptr_dims-i+1), c_size_t)
         enddo

         do i=1, fptr_dims
            omp_dst_offsets(i) = int(dst_offs(i), c_size_t)
            omp_src_offsets(i) = int(src_offs(i), c_size_t)
         enddo

         omp_dst_device = int(omp_dst_dev, c_int)
         omp_src_device = int(omp_src_dev, c_int)

         cptr_dst = c_loc(fptr_dst)
         cptr_src = c_loc(fptr_src)

         elem_dim = int(2_I8P * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)
      endsubroutine omp_target_memcpy_rect_f_cmplx128_7
#endif
endsubmodule dmr_target_memcpy_rect

