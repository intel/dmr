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

submodule (dmr) dmr_target_free

   implicit none

   contains
      ! OpenMP Target Free Integer Routines
      module subroutine omp_target_free_f_int8_1(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int8_1

      module subroutine omp_target_free_f_int8_2(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int8_2

      module subroutine omp_target_free_f_int8_3(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int8_3

      module subroutine omp_target_free_f_int8_4(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int8_4

      module subroutine omp_target_free_f_int8_5(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int8_5

      module subroutine omp_target_free_f_int8_6(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int8_6

      module subroutine omp_target_free_f_int8_7(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int8_7

      module subroutine omp_target_free_f_int16_1(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int16_1

      module subroutine omp_target_free_f_int16_2(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int16_2

      module subroutine omp_target_free_f_int16_3(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int16_3

      module subroutine omp_target_free_f_int16_4(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int16_4

      module subroutine omp_target_free_f_int16_5(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int16_5

      module subroutine omp_target_free_f_int16_6(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int16_6

      module subroutine omp_target_free_f_int16_7(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int16_7

      module subroutine omp_target_free_f_int32_1(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int32_1

      module subroutine omp_target_free_f_int32_2(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int32_2

      module subroutine omp_target_free_f_int32_3(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int32_3

      module subroutine omp_target_free_f_int32_4(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int32_4

      module subroutine omp_target_free_f_int32_5(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int32_5

      module subroutine omp_target_free_f_int32_6(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int32_6

      module subroutine omp_target_free_f_int32_7(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int32_7

      module subroutine omp_target_free_f_int64_1(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int64_1

      module subroutine omp_target_free_f_int64_2(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int64_2

      module subroutine omp_target_free_f_int64_3(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int64_3

      module subroutine omp_target_free_f_int64_4(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int64_4

      module subroutine omp_target_free_f_int64_5(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int64_5

      module subroutine omp_target_free_f_int64_6(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int64_6

      module subroutine omp_target_free_f_int64_7(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_int64_7


      ! OpenMP Target Free Real Routines
      module subroutine omp_target_free_f_real32_1(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real32_1

      module subroutine omp_target_free_f_real32_2(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real32_2

      module subroutine omp_target_free_f_real32_3(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real32_3

      module subroutine omp_target_free_f_real32_4(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real32_4

      module subroutine omp_target_free_f_real32_5(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real32_5

      module subroutine omp_target_free_f_real32_6(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real32_6

      module subroutine omp_target_free_f_real32_7(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real32_7

      module subroutine omp_target_free_f_real64_1(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real64_1

      module subroutine omp_target_free_f_real64_2(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real64_2

      module subroutine omp_target_free_f_real64_3(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real64_3

      module subroutine omp_target_free_f_real64_4(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real64_4

      module subroutine omp_target_free_f_real64_5(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real64_5

      module subroutine omp_target_free_f_real64_6(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real64_6

      module subroutine omp_target_free_f_real64_7(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real64_7

#if defined _real128
      module subroutine omp_target_free_f_real128_1(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real128_1

      module subroutine omp_target_free_f_real128_2(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real128_2

      module subroutine omp_target_free_f_real128_3(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real128_3

      module subroutine omp_target_free_f_real128_4(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real128_4

      module subroutine omp_target_free_f_real128_5(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real128_5

      module subroutine omp_target_free_f_real128_6(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real128_6

      module subroutine omp_target_free_f_real128_7(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_real128_7

#endif

      ! OpenMP Target Free Complex Routines
      module subroutine omp_target_free_f_cmplx32_1(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx32_1

      module subroutine omp_target_free_f_cmplx32_2(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx32_2

      module subroutine omp_target_free_f_cmplx32_3(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx32_3

      module subroutine omp_target_free_f_cmplx32_4(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx32_4

      module subroutine omp_target_free_f_cmplx32_5(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx32_5

      module subroutine omp_target_free_f_cmplx32_6(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx32_6

      module subroutine omp_target_free_f_cmplx32_7(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx32_7

      module subroutine omp_target_free_f_cmplx64_1(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx64_1

      module subroutine omp_target_free_f_cmplx64_2(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx64_2

      module subroutine omp_target_free_f_cmplx64_3(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx64_3

      module subroutine omp_target_free_f_cmplx64_4(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx64_4

      module subroutine omp_target_free_f_cmplx64_5(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx64_5

      module subroutine omp_target_free_f_cmplx64_6(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx64_6

      module subroutine omp_target_free_f_cmplx64_7(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx64_7

#if defined _real128
      module subroutine omp_target_free_f_cmplx128_1(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx128_1

      module subroutine omp_target_free_f_cmplx128_2(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx128_2

      module subroutine omp_target_free_f_cmplx128_3(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx128_3

      module subroutine omp_target_free_f_cmplx128_4(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx128_4

      module subroutine omp_target_free_f_cmplx128_5(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx128_5

      module subroutine omp_target_free_f_cmplx128_6(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx128_6

      module subroutine omp_target_free_f_cmplx128_7(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_cmplx128_7

#endif

      ! OpenMP Target Free Logical Routines
      module subroutine omp_target_free_f_lgcl32_1(fptr_dev, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_lgcl32_1

      module subroutine omp_target_free_f_lgcl32_2(fptr_dev, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_lgcl32_2

      module subroutine omp_target_free_f_lgcl32_3(fptr_dev, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_lgcl32_3

      module subroutine omp_target_free_f_lgcl32_4(fptr_dev, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_lgcl32_4

      module subroutine omp_target_free_f_lgcl32_5(fptr_dev, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_lgcl32_5

      module subroutine omp_target_free_f_lgcl32_6(fptr_dev, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_lgcl32_6

      module subroutine omp_target_free_f_lgcl32_7(fptr_dev, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
         type(c_ptr)                                     :: cptr_dev
         integer(kind=c_int)                             :: omp_device

         omp_device = int(omp_dev, c_int)

         cptr_dev = c_loc(fptr_dev)

         call omp_target_free(cptr_dev, omp_device)

         nullify(fptr_dev)
      endsubroutine omp_target_free_f_lgcl32_7



endsubmodule dmr_target_free