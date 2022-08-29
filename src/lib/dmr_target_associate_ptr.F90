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

submodule (dmr) dmr_target_associate_ptr

   implicit none

   contains
      ! OpenMP Target Associate Pointer Integer Routines
      module function ompx_target_associate_ptr_f_int8_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int8_1
         integer(I1P), target,  intent(out) :: fptr_hos(:)
         integer(I1P), pointer, intent(in)  :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I1P), c_size_t)
            ompx_target_associate_ptr_f_int8_1 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int8_1 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int8_1
      module function ompx_target_associate_ptr_f_int8_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int8_2
         integer(I1P), target,  intent(out) :: fptr_hos(:,:)
         integer(I1P), pointer, intent(in)  :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I1P), c_size_t)
            ompx_target_associate_ptr_f_int8_2 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int8_2 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int8_2
      module function ompx_target_associate_ptr_f_int8_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int8_3
         integer(I1P), target,  intent(out) :: fptr_hos(:,:,:)
         integer(I1P), pointer, intent(in)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I1P), c_size_t)
            ompx_target_associate_ptr_f_int8_3 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int8_3 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int8_3
      module function ompx_target_associate_ptr_f_int8_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int8_4
         integer(I1P), target,  intent(out) :: fptr_hos(:,:,:,:)
         integer(I1P), pointer, intent(in)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I1P), c_size_t)
            ompx_target_associate_ptr_f_int8_4 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int8_4 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int8_4
      module function ompx_target_associate_ptr_f_int8_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int8_5
         integer(I1P), target,  intent(out) :: fptr_hos(:,:,:,:,:)
         integer(I1P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I1P), c_size_t)
            ompx_target_associate_ptr_f_int8_5 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int8_5 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int8_5
      module function ompx_target_associate_ptr_f_int8_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int8_6
         integer(I1P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:)
         integer(I1P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I1P), c_size_t)
            ompx_target_associate_ptr_f_int8_6 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int8_6 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int8_6
      module function ompx_target_associate_ptr_f_int8_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int8_7
         integer(I1P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:,:)
         integer(I1P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I1P), c_size_t)
            ompx_target_associate_ptr_f_int8_7 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int8_7 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int8_7

      module function ompx_target_associate_ptr_f_int16_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int16_1
         integer(I2P), target,  intent(out) :: fptr_hos(:)
         integer(I2P), pointer, intent(in)  :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I2P), c_size_t)
            ompx_target_associate_ptr_f_int16_1 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int16_1 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int16_1
      module function ompx_target_associate_ptr_f_int16_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int16_2
         integer(I2P), target,  intent(out) :: fptr_hos(:,:)
         integer(I2P), pointer, intent(in)  :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I2P), c_size_t)
            ompx_target_associate_ptr_f_int16_2 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int16_2 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int16_2
      module function ompx_target_associate_ptr_f_int16_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int16_3
         integer(I2P), target,  intent(out) :: fptr_hos(:,:,:)
         integer(I2P), pointer, intent(in)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I2P), c_size_t)
            ompx_target_associate_ptr_f_int16_3 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int16_3 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int16_3
      module function ompx_target_associate_ptr_f_int16_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int16_4
         integer(I2P), target,  intent(out) :: fptr_hos(:,:,:,:)
         integer(I2P), pointer, intent(in)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I2P), c_size_t)
            ompx_target_associate_ptr_f_int16_4 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int16_4 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int16_4
      module function ompx_target_associate_ptr_f_int16_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int16_5
         integer(I2P), target,  intent(out) :: fptr_hos(:,:,:,:,:)
         integer(I2P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I2P), c_size_t)
            ompx_target_associate_ptr_f_int16_5 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int16_5 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int16_5
      module function ompx_target_associate_ptr_f_int16_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int16_6
         integer(I2P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:)
         integer(I2P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I2P), c_size_t)
            ompx_target_associate_ptr_f_int16_6 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int16_6 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int16_6
      module function ompx_target_associate_ptr_f_int16_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int16_7
         integer(I2P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:,:)
         integer(I2P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I2P), c_size_t)
            ompx_target_associate_ptr_f_int16_7 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int16_7 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int16_7

      module function ompx_target_associate_ptr_f_int32_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int32_1
         integer(I4P), target,  intent(out) :: fptr_hos(:)
         integer(I4P), pointer, intent(in)  :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I4P), c_size_t)
            ompx_target_associate_ptr_f_int32_1 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int32_1 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int32_1
      module function ompx_target_associate_ptr_f_int32_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int32_2
         integer(I4P), target,  intent(out) :: fptr_hos(:,:)
         integer(I4P), pointer, intent(in)  :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I4P), c_size_t)
            ompx_target_associate_ptr_f_int32_2 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int32_2 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int32_2
      module function ompx_target_associate_ptr_f_int32_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int32_3
         integer(I4P), target,  intent(out) :: fptr_hos(:,:,:)
         integer(I4P), pointer, intent(in)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I4P), c_size_t)
            ompx_target_associate_ptr_f_int32_3 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int32_3 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int32_3
      module function ompx_target_associate_ptr_f_int32_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int32_4
         integer(I4P), target,  intent(out) :: fptr_hos(:,:,:,:)
         integer(I4P), pointer, intent(in)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I4P), c_size_t)
            ompx_target_associate_ptr_f_int32_4 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int32_4 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int32_4
      module function ompx_target_associate_ptr_f_int32_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int32_5
         integer(I4P), target,  intent(out) :: fptr_hos(:,:,:,:,:)
         integer(I4P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I4P), c_size_t)
            ompx_target_associate_ptr_f_int32_5 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int32_5 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int32_5
      module function ompx_target_associate_ptr_f_int32_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int32_6
         integer(I4P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I4P), c_size_t)
            ompx_target_associate_ptr_f_int32_6 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int32_6 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int32_6
      module function ompx_target_associate_ptr_f_int32_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int32_7
         integer(I4P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I4P), c_size_t)
            ompx_target_associate_ptr_f_int32_7 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int32_7 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int32_7

      module function ompx_target_associate_ptr_f_int64_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int64_1
         integer(I8P), target,  intent(out) :: fptr_hos(:)
         integer(I8P), pointer, intent(in)  :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I8P), c_size_t)
            ompx_target_associate_ptr_f_int64_1 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int64_1 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int64_1
      module function ompx_target_associate_ptr_f_int64_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int64_2
         integer(I8P), target,  intent(out) :: fptr_hos(:,:)
         integer(I8P), pointer, intent(in)  :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I8P), c_size_t)
            ompx_target_associate_ptr_f_int64_2 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int64_2 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int64_2
      module function ompx_target_associate_ptr_f_int64_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int64_3
         integer(I8P), target,  intent(out) :: fptr_hos(:,:,:)
         integer(I8P), pointer, intent(in)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I8P), c_size_t)
            ompx_target_associate_ptr_f_int64_3 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int64_3 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int64_3
      module function ompx_target_associate_ptr_f_int64_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int64_4
         integer(I8P), target,  intent(out) :: fptr_hos(:,:,:,:)
         integer(I8P), pointer, intent(in)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I8P), c_size_t)
            ompx_target_associate_ptr_f_int64_4 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int64_4 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int64_4
      module function ompx_target_associate_ptr_f_int64_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int64_5
         integer(I8P), target,  intent(out) :: fptr_hos(:,:,:,:,:)
         integer(I8P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I8P), c_size_t)
            ompx_target_associate_ptr_f_int64_5 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int64_5 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int64_5
      module function ompx_target_associate_ptr_f_int64_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int64_6
         integer(I8P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:)
         integer(I8P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I8P), c_size_t)
            ompx_target_associate_ptr_f_int64_6 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int64_6 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int64_6
      module function ompx_target_associate_ptr_f_int64_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_int64_7
         integer(I8P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:,:)
         integer(I8P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1_I8P), c_size_t)
            ompx_target_associate_ptr_f_int64_7 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_int64_7 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_int64_7

      ! OpenMP Target Associate Pointer Real Routines
      module function ompx_target_associate_ptr_f_real32_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real32_1
         real(R4P), target,  intent(out)    :: fptr_hos(:)
         real(R4P), pointer, intent(in)     :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R4P), c_size_t)
            ompx_target_associate_ptr_f_real32_1 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real32_1 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real32_1
      module function ompx_target_associate_ptr_f_real32_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real32_2
         real(R4P), target,  intent(out)    :: fptr_hos(:,:)
         real(R4P), pointer, intent(in)     :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R4P), c_size_t)
            ompx_target_associate_ptr_f_real32_2 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real32_2 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real32_2
      module function ompx_target_associate_ptr_f_real32_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real32_3
         real(R4P), target,  intent(out)    :: fptr_hos(:,:,:)
         real(R4P), pointer, intent(in)     :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R4P), c_size_t)
            ompx_target_associate_ptr_f_real32_3 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real32_3 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real32_3
      module function ompx_target_associate_ptr_f_real32_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real32_4
         real(R4P), target,  intent(out)    :: fptr_hos(:,:,:,:)
         real(R4P), pointer, intent(in)     :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R4P), c_size_t)
            ompx_target_associate_ptr_f_real32_4 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real32_4 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real32_4
      module function ompx_target_associate_ptr_f_real32_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real32_5
         real(R4P), target,  intent(out)    :: fptr_hos(:,:,:,:,:)
         real(R4P), pointer, intent(in)     :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R4P), c_size_t)
            ompx_target_associate_ptr_f_real32_5 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real32_5 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real32_5
      module function ompx_target_associate_ptr_f_real32_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real32_6
         real(R4P), target,  intent(out)    :: fptr_hos(:,:,:,:,:,:)
         real(R4P), pointer, intent(in)     :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R4P), c_size_t)
            ompx_target_associate_ptr_f_real32_6 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real32_6 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real32_6
      module function ompx_target_associate_ptr_f_real32_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real32_7
         real(R4P), target,  intent(out)    :: fptr_hos(:,:,:,:,:,:,:)
         real(R4P), pointer, intent(in)     :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R4P), c_size_t)
            ompx_target_associate_ptr_f_real32_7 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real32_7 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real32_7

      module function ompx_target_associate_ptr_f_real64_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real64_1
         real(R8P), target,  intent(out)    :: fptr_hos(:)
         real(R8P), pointer, intent(in)     :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R8P), c_size_t)
            ompx_target_associate_ptr_f_real64_1 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real64_1 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real64_1
      module function ompx_target_associate_ptr_f_real64_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real64_2
         real(R8P), target,  intent(out)    :: fptr_hos(:,:)
         real(R8P), pointer, intent(in)     :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R8P), c_size_t)
            ompx_target_associate_ptr_f_real64_2 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real64_2 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real64_2
      module function ompx_target_associate_ptr_f_real64_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real64_3
         real(R8P), target,  intent(out)    :: fptr_hos(:,:,:)
         real(R8P), pointer, intent(in)     :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R8P), c_size_t)
            ompx_target_associate_ptr_f_real64_3 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real64_3 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real64_3
      module function ompx_target_associate_ptr_f_real64_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real64_4
         real(R8P), target,  intent(out)    :: fptr_hos(:,:,:,:)
         real(R8P), pointer, intent(in)     :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R8P), c_size_t)
            ompx_target_associate_ptr_f_real64_4 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real64_4 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real64_4
      module function ompx_target_associate_ptr_f_real64_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real64_5
         real(R8P), target,  intent(out)    :: fptr_hos(:,:,:,:,:)
         real(R8P), pointer, intent(in)     :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R8P), c_size_t)
            ompx_target_associate_ptr_f_real64_5 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real64_5 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real64_5
      module function ompx_target_associate_ptr_f_real64_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real64_6
         real(R8P), target,  intent(out)    :: fptr_hos(:,:,:,:,:,:)
         real(R8P), pointer, intent(in)     :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R8P), c_size_t)
            ompx_target_associate_ptr_f_real64_6 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real64_6 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real64_6
      module function ompx_target_associate_ptr_f_real64_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real64_7
         real(R8P), target,  intent(out)    :: fptr_hos(:,:,:,:,:,:,:)
         real(R8P), pointer, intent(in)     :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R8P), c_size_t)
            ompx_target_associate_ptr_f_real64_7 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real64_7 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real64_7

#if defined _real128
      module function ompx_target_associate_ptr_f_real128_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real128_1
         real(R16P), target,  intent(out)   :: fptr_hos(:)
         real(R16P), pointer, intent(in)    :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R16P), c_size_t)
            ompx_target_associate_ptr_f_real128_1 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real128_1 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real128_1
      module function ompx_target_associate_ptr_f_real128_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real128_2
         real(R16P), target,  intent(out)   :: fptr_hos(:,:)
         real(R16P), pointer, intent(in)    :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R16P), c_size_t)
            ompx_target_associate_ptr_f_real128_2 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real128_2 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real128_2
      module function ompx_target_associate_ptr_f_real128_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real128_3
         real(R16P), target,  intent(out)   :: fptr_hos(:,:,:)
         real(R16P), pointer, intent(in)    :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R16P), c_size_t)
            ompx_target_associate_ptr_f_real128_3 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real128_3 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real128_3
      module function ompx_target_associate_ptr_f_real128_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real128_4
         real(R16P), target,  intent(out)   :: fptr_hos(:,:,:,:)
         real(R16P), pointer, intent(in)    :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R16P), c_size_t)
            ompx_target_associate_ptr_f_real128_4 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real128_4 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real128_4
      module function ompx_target_associate_ptr_f_real128_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real128_5
         real(R16P), target,  intent(out)   :: fptr_hos(:,:,:,:,:)
         real(R16P), pointer, intent(in)    :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R16P), c_size_t)
            ompx_target_associate_ptr_f_real128_5 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real128_5 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real128_5
      module function ompx_target_associate_ptr_f_real128_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real128_6
         real(R16P), target,  intent(out)   :: fptr_hos(:,:,:,:,:,:)
         real(R16P), pointer, intent(in)    :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R16P), c_size_t)
            ompx_target_associate_ptr_f_real128_6 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real128_6 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real128_6
      module function ompx_target_associate_ptr_f_real128_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_real128_7
         real(R16P), target,  intent(out)   :: fptr_hos(:,:,:,:,:,:,:)
         real(R16P), pointer, intent(in)    :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._R16P), c_size_t)
            ompx_target_associate_ptr_f_real128_7 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_real128_7 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_real128_7
#endif

      ! OpenMP Target Associate Pointer Complex Routines
      module function ompx_target_associate_ptr_f_cmplx32_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx32_1
         complex(R4P), target,  intent(out) :: fptr_hos(:)
         complex(R4P), pointer, intent(in)  :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)
            ompx_target_associate_ptr_f_cmplx32_1 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx32_1 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx32_1
      module function ompx_target_associate_ptr_f_cmplx32_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx32_2
         complex(R4P), target,  intent(out) :: fptr_hos(:,:)
         complex(R4P), pointer, intent(in)  :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)
            ompx_target_associate_ptr_f_cmplx32_2 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx32_2 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx32_2
      module function ompx_target_associate_ptr_f_cmplx32_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx32_3
         complex(R4P), target,  intent(out) :: fptr_hos(:,:,:)
         complex(R4P), pointer, intent(in)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)
            ompx_target_associate_ptr_f_cmplx32_3 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx32_3 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx32_3
      module function ompx_target_associate_ptr_f_cmplx32_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx32_4
         complex(R4P), target,  intent(out) :: fptr_hos(:,:,:,:)
         complex(R4P), pointer, intent(in)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)
            ompx_target_associate_ptr_f_cmplx32_4 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx32_4 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx32_4
      module function ompx_target_associate_ptr_f_cmplx32_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx32_5
         complex(R4P), target,  intent(out) :: fptr_hos(:,:,:,:,:)
         complex(R4P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)
            ompx_target_associate_ptr_f_cmplx32_5 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx32_5 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx32_5
      module function ompx_target_associate_ptr_f_cmplx32_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx32_6
         complex(R4P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:)
         complex(R4P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)
            ompx_target_associate_ptr_f_cmplx32_6 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx32_6 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx32_6
      module function ompx_target_associate_ptr_f_cmplx32_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx32_7
         complex(R4P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:,:)
         complex(R4P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)
            ompx_target_associate_ptr_f_cmplx32_7 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx32_7 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx32_7

      module function ompx_target_associate_ptr_f_cmplx64_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx64_1
         complex(R8P), target,  intent(out) :: fptr_hos(:)
         complex(R8P), pointer, intent(in)  :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)
            ompx_target_associate_ptr_f_cmplx64_1 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx64_1 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx64_1
      module function ompx_target_associate_ptr_f_cmplx64_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx64_2
         complex(R8P), target,  intent(out) :: fptr_hos(:,:)
         complex(R8P), pointer, intent(in)  :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)
            ompx_target_associate_ptr_f_cmplx64_2 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx64_2 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx64_2
      module function ompx_target_associate_ptr_f_cmplx64_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx64_3
         complex(R8P), target,  intent(out) :: fptr_hos(:,:,:)
         complex(R8P), pointer, intent(in)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)
            ompx_target_associate_ptr_f_cmplx64_3 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx64_3 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx64_3
      module function ompx_target_associate_ptr_f_cmplx64_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx64_4
         complex(R8P), target,  intent(out) :: fptr_hos(:,:,:,:)
         complex(R8P), pointer, intent(in)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)
            ompx_target_associate_ptr_f_cmplx64_4 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx64_4 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx64_4
      module function ompx_target_associate_ptr_f_cmplx64_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx64_5
         complex(R8P), target,  intent(out) :: fptr_hos(:,:,:,:,:)
         complex(R8P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)
            ompx_target_associate_ptr_f_cmplx64_5 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx64_5 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx64_5
      module function ompx_target_associate_ptr_f_cmplx64_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx64_6
         complex(R8P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:)
         complex(R8P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)
            ompx_target_associate_ptr_f_cmplx64_6 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx64_6 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx64_6
      module function ompx_target_associate_ptr_f_cmplx64_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx64_7
         complex(R8P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:,:)
         complex(R8P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)
            ompx_target_associate_ptr_f_cmplx64_7 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx64_7 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx64_7

#if defined _real128
      module function ompx_target_associate_ptr_f_cmplx128_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx128_1
         complex(R16P), target,  intent(out):: fptr_hos(:)
         complex(R16P), pointer, intent(in) :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)
            ompx_target_associate_ptr_f_cmplx128_1 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx128_1 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx128_1
      module function ompx_target_associate_ptr_f_cmplx128_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx128_2
         complex(R16P), target,  intent(out):: fptr_hos(:,:)
         complex(R16P), pointer, intent(in) :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)
            ompx_target_associate_ptr_f_cmplx128_2 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx128_2 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx128_2
      module function ompx_target_associate_ptr_f_cmplx128_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx128_3
         complex(R16P), target,  intent(out):: fptr_hos(:,:,:)
         complex(R16P), pointer, intent(in) :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)
            ompx_target_associate_ptr_f_cmplx128_3 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx128_3 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx128_3
      module function ompx_target_associate_ptr_f_cmplx128_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx128_4
         complex(R16P), target,  intent(out):: fptr_hos(:,:,:,:)
         complex(R16P), pointer, intent(in) :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)
            ompx_target_associate_ptr_f_cmplx128_4 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx128_4 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx128_4
      module function ompx_target_associate_ptr_f_cmplx128_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx128_5
         complex(R16P), target,  intent(out):: fptr_hos(:,:,:,:,:)
         complex(R16P), pointer, intent(in) :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)
            ompx_target_associate_ptr_f_cmplx128_5 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx128_5 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx128_5
      module function ompx_target_associate_ptr_f_cmplx128_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx128_6
         complex(R16P), target,  intent(out):: fptr_hos(:,:,:,:,:,:)
         complex(R16P), pointer, intent(in) :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)
            ompx_target_associate_ptr_f_cmplx128_6 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx128_6 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx128_6
      module function ompx_target_associate_ptr_f_cmplx128_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_cmplx128_7
         complex(R16P), target,  intent(out):: fptr_hos(:,:,:,:,:,:,:)
         complex(R16P), pointer, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)
            ompx_target_associate_ptr_f_cmplx128_7 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_cmplx128_7 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_cmplx128_7
#endif

      ! OpenMP Target Associate Pointer Logical Routines
      module function ompx_target_associate_ptr_f_lgcl32_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_lgcl32_1
         logical(I4P), target,  intent(out) :: fptr_hos(:)
         logical(I4P), pointer, intent(in)  :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._I4P), c_size_t)
            ompx_target_associate_ptr_f_lgcl32_1 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_lgcl32_1 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_lgcl32_1
      module function ompx_target_associate_ptr_f_lgcl32_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_lgcl32_2
         logical(I4P), target,  intent(out) :: fptr_hos(:,:)
         logical(I4P), pointer, intent(in)  :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._I4P), c_size_t)
            ompx_target_associate_ptr_f_lgcl32_2 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_lgcl32_2 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_lgcl32_2
      module function ompx_target_associate_ptr_f_lgcl32_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_lgcl32_3
         logical(I4P), target,  intent(out) :: fptr_hos(:,:,:)
         logical(I4P), pointer, intent(in)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._I4P), c_size_t)
            ompx_target_associate_ptr_f_lgcl32_3 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_lgcl32_3 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_lgcl32_3
      module function ompx_target_associate_ptr_f_lgcl32_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_lgcl32_4
         logical(I4P), target,  intent(out) :: fptr_hos(:,:,:,:)
         logical(I4P), pointer, intent(in)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._I4P), c_size_t)
            ompx_target_associate_ptr_f_lgcl32_4 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_lgcl32_4 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_lgcl32_4
      module function ompx_target_associate_ptr_f_lgcl32_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_lgcl32_5
         logical(I4P), target,  intent(out) :: fptr_hos(:,:,:,:,:)
         logical(I4P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._I4P), c_size_t)
            ompx_target_associate_ptr_f_lgcl32_5 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_lgcl32_5 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_lgcl32_5
      module function ompx_target_associate_ptr_f_lgcl32_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_lgcl32_6
         logical(I4P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:)
         logical(I4P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._I4P), c_size_t)
            ompx_target_associate_ptr_f_lgcl32_6 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_lgcl32_6 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_lgcl32_6
      module function ompx_target_associate_ptr_f_lgcl32_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: ompx_target_associate_ptr_f_lgcl32_7
         logical(I4P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:,:)
         logical(I4P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
         integer(c_size_t)                  :: total_dim, omp_dev_offset
         type(c_ptr)                        :: cptr_dev, cptr_hos
         integer(kind=c_int)                :: omp_device
         integer(I8P)                       :: n_elements

         omp_dev_offset = int(dev_off, c_size_t)
         omp_device = int(omp_dev, c_int)

         n_elements = size(fptr_hos)

         cptr_hos = c_loc(fptr_hos)
         cptr_dev = c_loc(fptr_dev)

         if (c_associated(cptr_dev)) then
            total_dim = int(n_elements * byte_size(1._I4P), c_size_t)
            ompx_target_associate_ptr_f_lgcl32_7 = int(omp_target_associate_ptr(cptr_hos, cptr_dev, &
                                                                          total_dim, omp_dev_offset, omp_device), I4P)
         else
            ompx_target_associate_ptr_f_lgcl32_7 = 1000
         endif
      endfunction ompx_target_associate_ptr_f_lgcl32_7

endsubmodule dmr_target_associate_ptr