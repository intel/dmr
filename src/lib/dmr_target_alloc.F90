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

   implicit none

   contains

      ! OpenMP Target Alloc Integer 32 bits dimensions Routines
      module subroutine ompx_target_alloc_f_int8_1_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds
         integer(I1P), pointer, contiguous               :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(dimensions,I8P) * byte_size(1_I1P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int8_1_32bit

      module subroutine ompx_target_alloc_f_int8_2_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(2)
         integer(I1P), pointer, contiguous               :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I1P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int8_2_32bit

      module subroutine ompx_target_alloc_f_int8_3_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(3)
         integer(I1P), pointer, contiguous               :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I1P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int8_3_32bit

      module subroutine ompx_target_alloc_f_int8_4_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(4)
         integer(I1P), pointer, contiguous               :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I1P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int8_4_32bit

      module subroutine ompx_target_alloc_f_int8_5_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(5)
         integer(I1P), pointer, contiguous               :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I1P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int8_5_32bit

      module subroutine ompx_target_alloc_f_int8_6_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(6)
         integer(I1P), pointer, contiguous               :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I1P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int8_6_32bit

      module subroutine ompx_target_alloc_f_int8_7_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(7)
         integer(I1P), pointer, contiguous               :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I1P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P, lbounds(7):lbounds(7)+dimensions(7)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int8_7_32bit

      module subroutine ompx_target_alloc_f_int16_1_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds
         integer(I2P), pointer, contiguous               :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(dimensions,I8P) * byte_size(1_I2P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int16_1_32bit

      module subroutine ompx_target_alloc_f_int16_2_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(2)
         integer(I2P), pointer, contiguous               :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I2P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int16_2_32bit

      module subroutine ompx_target_alloc_f_int16_3_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(3)
         integer(I2P), pointer, contiguous               :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I2P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int16_3_32bit

      module subroutine ompx_target_alloc_f_int16_4_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(4)
         integer(I2P), pointer, contiguous               :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I2P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int16_4_32bit

      module subroutine ompx_target_alloc_f_int16_5_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(5)
         integer(I2P), pointer, contiguous               :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I2P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int16_5_32bit

      module subroutine ompx_target_alloc_f_int16_6_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(6)
         integer(I2P), pointer, contiguous               :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I2P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int16_6_32bit

      module subroutine ompx_target_alloc_f_int16_7_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(7)
         integer(I2P), pointer, contiguous               :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I2P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P, lbounds(7):lbounds(7)+dimensions(7)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int16_7_32bit

      module subroutine ompx_target_alloc_f_int32_1_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds
         integer(I4P), pointer, contiguous               :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(dimensions,I8P) * byte_size(1_I4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int32_1_32bit

      module subroutine ompx_target_alloc_f_int32_2_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(2)
         integer(I4P), pointer, contiguous               :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int32_2_32bit

      module subroutine ompx_target_alloc_f_int32_3_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(3)
         integer(I4P), pointer, contiguous               :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int32_3_32bit

      module subroutine ompx_target_alloc_f_int32_4_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(4)
         integer(I4P), pointer, contiguous               :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int32_4_32bit

      module subroutine ompx_target_alloc_f_int32_5_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(5)
         integer(I4P), pointer, contiguous               :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int32_5_32bit

      module subroutine ompx_target_alloc_f_int32_6_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(6)
         integer(I4P), pointer, contiguous               :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int32_6_32bit

      module subroutine ompx_target_alloc_f_int32_7_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(7)
         integer(I4P), pointer, contiguous               :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P, lbounds(7):lbounds(7)+dimensions(7)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int32_7_32bit

      module subroutine ompx_target_alloc_f_int64_1_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds
         integer(I8P), pointer, contiguous               :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(dimensions,I8P) * byte_size(1_I8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int64_1_32bit

      module subroutine ompx_target_alloc_f_int64_2_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(2)
         integer(I8P), pointer, contiguous               :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int64_2_32bit

      module subroutine ompx_target_alloc_f_int64_3_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(3)
         integer(I8P), pointer, contiguous               :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int64_3_32bit

      module subroutine ompx_target_alloc_f_int64_4_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(4)
         integer(I8P), pointer, contiguous               :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int64_4_32bit

      module subroutine ompx_target_alloc_f_int64_5_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(5)
         integer(I8P), pointer, contiguous               :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int64_5_32bit

      module subroutine ompx_target_alloc_f_int64_6_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(6)
         integer(I8P), pointer, contiguous               :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int64_6_32bit

      module subroutine ompx_target_alloc_f_int64_7_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(7)
         integer(I8P), pointer, contiguous               :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1_I8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P, lbounds(7):lbounds(7)+dimensions(7)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int64_7_32bit


      ! OpenMP Target Alloc Real 32 bits dimensions Routines
      module subroutine ompx_target_alloc_f_real32_1_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds
         real(R4P), pointer, contiguous                  :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(dimensions,I8P) * byte_size(1._R4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real32_1_32bit

      module subroutine ompx_target_alloc_f_real32_2_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(2)
         real(R4P), pointer, contiguous                  :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._R4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real32_2_32bit

      module subroutine ompx_target_alloc_f_real32_3_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(3)
         real(R4P), pointer, contiguous                  :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._R4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real32_3_32bit

      module subroutine ompx_target_alloc_f_real32_4_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(4)
         real(R4P), pointer, contiguous                  :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._R4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real32_4_32bit

      module subroutine ompx_target_alloc_f_real32_5_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(5)
         real(R4P), pointer, contiguous                  :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._R4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real32_5_32bit

      module subroutine ompx_target_alloc_f_real32_6_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(6)
         real(R4P), pointer, contiguous                  :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._R4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real32_6_32bit

      module subroutine ompx_target_alloc_f_real32_7_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(7)
         real(R4P), pointer, contiguous                  :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._R4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P, lbounds(7):lbounds(7)+dimensions(7)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real32_7_32bit

      module subroutine ompx_target_alloc_f_real64_1_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds
         real(R8P), pointer, contiguous                  :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(dimensions,I8P) * byte_size(1._R8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real64_1_32bit

      module subroutine ompx_target_alloc_f_real64_2_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(2)
         real(R8P), pointer, contiguous                  :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._R8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real64_2_32bit

      module subroutine ompx_target_alloc_f_real64_3_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(3)
         real(R8P), pointer, contiguous                  :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._R8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real64_3_32bit

      module subroutine ompx_target_alloc_f_real64_4_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(4)
         real(R8P), pointer, contiguous                  :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._R8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real64_4_32bit

      module subroutine ompx_target_alloc_f_real64_5_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(5)
         real(R8P), pointer, contiguous                  :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._R8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real64_5_32bit

      module subroutine ompx_target_alloc_f_real64_6_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(6)
         real(R8P), pointer, contiguous                  :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._R8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real64_6_32bit

      module subroutine ompx_target_alloc_f_real64_7_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(7)
         real(R8P), pointer, contiguous                  :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._R8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P, lbounds(7):lbounds(7)+dimensions(7)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real64_7_32bit

#if defined _real128
      module subroutine ompx_target_alloc_f_real128_1_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds
         real(R16P), pointer, contiguous                 :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(dimensions,I8P) * byte_size(1._R16P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real128_1_32bit

      module subroutine ompx_target_alloc_f_real128_2_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(2)
         real(R16P), pointer, contiguous                 :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._R16P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real128_2_32bit

      module subroutine ompx_target_alloc_f_real128_3_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(3)
         real(R16P), pointer, contiguous                 :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._R16P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real128_3_32bit

      module subroutine ompx_target_alloc_f_real128_4_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(4)
         real(R16P), pointer, contiguous                 :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._R16P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real128_4_32bit

      module subroutine ompx_target_alloc_f_real128_5_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(5)
         real(R16P), pointer, contiguous                 :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._R16P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real128_5_32bit

      module subroutine ompx_target_alloc_f_real128_6_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(6)
         real(R16P), pointer, contiguous                 :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._R16P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real128_6_32bit

      module subroutine ompx_target_alloc_f_real128_7_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(7)
         real(R16P), pointer, contiguous                 :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._R16P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P, lbounds(7):lbounds(7)+dimensions(7)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real128_7_32bit

#endif

      ! OpenMP Target Alloc Complex 32 bits dimensions Routines
      module subroutine ompx_target_alloc_f_cmplx32_1_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds
         complex(R4P), pointer, contiguous               :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * dimensions,I8P) * byte_size(1._R4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx32_1_32bit

      module subroutine ompx_target_alloc_f_cmplx32_2_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(2)
         complex(R4P), pointer, contiguous               :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * product(dimensions),I8P) * byte_size(1._R4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx32_2_32bit

      module subroutine ompx_target_alloc_f_cmplx32_3_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(3)
         complex(R4P), pointer, contiguous               :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * product(dimensions),I8P) * byte_size(1._R4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx32_3_32bit

      module subroutine ompx_target_alloc_f_cmplx32_4_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(4)
         complex(R4P), pointer, contiguous               :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * product(dimensions),I8P) * byte_size(1._R4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx32_4_32bit

      module subroutine ompx_target_alloc_f_cmplx32_5_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(5)
         complex(R4P), pointer, contiguous               :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * product(dimensions),I8P) * byte_size(1._R4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx32_5_32bit

      module subroutine ompx_target_alloc_f_cmplx32_6_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(6)
         complex(R4P), pointer, contiguous               :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * product(dimensions),I8P) * byte_size(1._R4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx32_6_32bit

      module subroutine ompx_target_alloc_f_cmplx32_7_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(7)
         complex(R4P), pointer, contiguous               :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * product(dimensions),I8P) * byte_size(1._R4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P, lbounds(7):lbounds(7)+dimensions(7)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx32_7_32bit

      module subroutine ompx_target_alloc_f_cmplx64_1_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds
         complex(R8P), pointer, contiguous               :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * dimensions,I8P) * byte_size(1._R8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx64_1_32bit

      module subroutine ompx_target_alloc_f_cmplx64_2_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(2)
         complex(R8P), pointer, contiguous               :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * product(dimensions),I8P) * byte_size(1._R8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx64_2_32bit

      module subroutine ompx_target_alloc_f_cmplx64_3_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(3)
         complex(R8P), pointer, contiguous               :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * product(dimensions),I8P) * byte_size(1._R8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx64_3_32bit

      module subroutine ompx_target_alloc_f_cmplx64_4_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(4)
         complex(R8P), pointer, contiguous               :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * product(dimensions),I8P) * byte_size(1._R8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx64_4_32bit

      module subroutine ompx_target_alloc_f_cmplx64_5_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(5)
         complex(R8P), pointer, contiguous               :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * product(dimensions),I8P) * byte_size(1._R8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx64_5_32bit

      module subroutine ompx_target_alloc_f_cmplx64_6_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(6)
         complex(R8P), pointer, contiguous               :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * product(dimensions),I8P) * byte_size(1._R8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx64_6_32bit

      module subroutine ompx_target_alloc_f_cmplx64_7_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(7)
         complex(R8P), pointer, contiguous               :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * product(dimensions),I8P) * byte_size(1._R8P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P, lbounds(7):lbounds(7)+dimensions(7)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx64_7_32bit

#if defined _real128
      module subroutine ompx_target_alloc_f_cmplx128_1_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds
         complex(R16P), pointer, contiguous              :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * dimensions,I8P) * byte_size(1._R16P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx128_1_32bit

      module subroutine ompx_target_alloc_f_cmplx128_2_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(2)
         complex(R16P), pointer, contiguous              :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * product(dimensions),I8P) * byte_size(1._R16P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx128_2_32bit

      module subroutine ompx_target_alloc_f_cmplx128_3_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(3)
         complex(R16P), pointer, contiguous              :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * product(dimensions),I8P) * byte_size(1._R16P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx128_3_32bit

      module subroutine ompx_target_alloc_f_cmplx128_4_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(4)
         complex(R16P), pointer, contiguous              :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * product(dimensions),I8P) * byte_size(1._R16P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx128_4_32bit

      module subroutine ompx_target_alloc_f_cmplx128_5_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(5)
         complex(R16P), pointer, contiguous              :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * product(dimensions),I8P) * byte_size(1._R16P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx128_5_32bit

      module subroutine ompx_target_alloc_f_cmplx128_6_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(6)
         complex(R16P), pointer, contiguous              :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * product(dimensions),I8P) * byte_size(1._R16P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx128_6_32bit

      module subroutine ompx_target_alloc_f_cmplx128_7_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(7)
         complex(R16P), pointer, contiguous              :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(2_I8P * product(dimensions),I8P) * byte_size(1._R16P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P, lbounds(7):lbounds(7)+dimensions(7)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx128_7_32bit

#endif

      ! OpenMP Target Alloc Logical 32 bits dimensions Routines
      module subroutine ompx_target_alloc_f_lgcl32_1_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds
         logical(I4P), pointer, contiguous               :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(dimensions,I8P) * byte_size(1._I4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_lgcl32_1_32bit

      module subroutine ompx_target_alloc_f_lgcl32_2_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(2)
         logical(I4P), pointer, contiguous               :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._I4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_lgcl32_2_32bit

      module subroutine ompx_target_alloc_f_lgcl32_3_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(3)
         logical(I4P), pointer, contiguous               :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._I4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_lgcl32_3_32bit

      module subroutine ompx_target_alloc_f_lgcl32_4_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(4)
         logical(I4P), pointer, contiguous               :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._I4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_lgcl32_4_32bit

      module subroutine ompx_target_alloc_f_lgcl32_5_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(5)
         logical(I4P), pointer, contiguous               :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._I4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_lgcl32_5_32bit

      module subroutine ompx_target_alloc_f_lgcl32_6_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(6)
         logical(I4P), pointer, contiguous               :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._I4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_lgcl32_6_32bit

      module subroutine ompx_target_alloc_f_lgcl32_7_32bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: lbounds(7)
         logical(I4P), pointer, contiguous               :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(int(product(dimensions),I8P) * byte_size(1._I4P),c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I4P, lbounds(2):lbounds(2)+dimensions(2)-1_I4P, lbounds(3):lbounds(3)+dimensions(3)-1_I4P, lbounds(4):lbounds(4)+dimensions(4)-1_I4P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I4P, lbounds(6):lbounds(6)+dimensions(6)-1_I4P, lbounds(7):lbounds(7)+dimensions(7)-1_I4P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_lgcl32_7_32bit




      ! OpenMP Target Alloc Integer 64 bits dimensions Routines
      module subroutine ompx_target_alloc_f_int8_1_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds
         integer(I1P), pointer, contiguous               :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(dimensions * byte_size(1_I1P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int8_1_64bit

      module subroutine ompx_target_alloc_f_int8_2_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(2)
         integer(I1P), pointer, contiguous               :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I1P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int8_2_64bit

      module subroutine ompx_target_alloc_f_int8_3_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(3)
         integer(I1P), pointer, contiguous               :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I1P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int8_3_64bit

      module subroutine ompx_target_alloc_f_int8_4_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(4)
         integer(I1P), pointer, contiguous               :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I1P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int8_4_64bit

      module subroutine ompx_target_alloc_f_int8_5_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(5)
         integer(I1P), pointer, contiguous               :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I1P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int8_5_64bit

      module subroutine ompx_target_alloc_f_int8_6_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(6)
         integer(I1P), pointer, contiguous               :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I1P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int8_6_64bit

      module subroutine ompx_target_alloc_f_int8_7_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(7)
         integer(I1P), pointer, contiguous               :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I1P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P, lbounds(7):lbounds(7)+dimensions(7)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int8_7_64bit

      module subroutine ompx_target_alloc_f_int16_1_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds
         integer(I2P), pointer, contiguous               :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(dimensions * byte_size(1_I2P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int16_1_64bit

      module subroutine ompx_target_alloc_f_int16_2_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(2)
         integer(I2P), pointer, contiguous               :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I2P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int16_2_64bit

      module subroutine ompx_target_alloc_f_int16_3_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(3)
         integer(I2P), pointer, contiguous               :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I2P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int16_3_64bit

      module subroutine ompx_target_alloc_f_int16_4_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(4)
         integer(I2P), pointer, contiguous               :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I2P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int16_4_64bit

      module subroutine ompx_target_alloc_f_int16_5_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(5)
         integer(I2P), pointer, contiguous               :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I2P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int16_5_64bit

      module subroutine ompx_target_alloc_f_int16_6_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(6)
         integer(I2P), pointer, contiguous               :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I2P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int16_6_64bit

      module subroutine ompx_target_alloc_f_int16_7_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(7)
         integer(I2P), pointer, contiguous               :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I2P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P, lbounds(7):lbounds(7)+dimensions(7)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int16_7_64bit

      module subroutine ompx_target_alloc_f_int32_1_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds
         integer(I4P), pointer, contiguous               :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(dimensions * byte_size(1_I4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int32_1_64bit

      module subroutine ompx_target_alloc_f_int32_2_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(2)
         integer(I4P), pointer, contiguous               :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int32_2_64bit

      module subroutine ompx_target_alloc_f_int32_3_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(3)
         integer(I4P), pointer, contiguous               :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int32_3_64bit

      module subroutine ompx_target_alloc_f_int32_4_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(4)
         integer(I4P), pointer, contiguous               :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int32_4_64bit

      module subroutine ompx_target_alloc_f_int32_5_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(5)
         integer(I4P), pointer, contiguous               :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int32_5_64bit

      module subroutine ompx_target_alloc_f_int32_6_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(6)
         integer(I4P), pointer, contiguous               :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int32_6_64bit

      module subroutine ompx_target_alloc_f_int32_7_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(7)
         integer(I4P), pointer, contiguous               :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P, lbounds(7):lbounds(7)+dimensions(7)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int32_7_64bit

      module subroutine ompx_target_alloc_f_int64_1_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds
         integer(I8P), pointer, contiguous               :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(dimensions * byte_size(1_I8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int64_1_64bit

      module subroutine ompx_target_alloc_f_int64_2_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(2)
         integer(I8P), pointer, contiguous               :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int64_2_64bit

      module subroutine ompx_target_alloc_f_int64_3_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(3)
         integer(I8P), pointer, contiguous               :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int64_3_64bit

      module subroutine ompx_target_alloc_f_int64_4_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(4)
         integer(I8P), pointer, contiguous               :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int64_4_64bit

      module subroutine ompx_target_alloc_f_int64_5_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(5)
         integer(I8P), pointer, contiguous               :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int64_5_64bit

      module subroutine ompx_target_alloc_f_int64_6_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(6)
         integer(I8P), pointer, contiguous               :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int64_6_64bit

      module subroutine ompx_target_alloc_f_int64_7_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(7)
         integer(I8P), pointer, contiguous               :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1_I8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P, lbounds(7):lbounds(7)+dimensions(7)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_int64_7_64bit


      ! OpenMP Target Alloc Real 64 bits dimensions Routines
      module subroutine ompx_target_alloc_f_real32_1_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds
         real(R4P), pointer, contiguous                  :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(dimensions * byte_size(1._R4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real32_1_64bit

      module subroutine ompx_target_alloc_f_real32_2_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(2)
         real(R4P), pointer, contiguous                  :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._R4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real32_2_64bit

      module subroutine ompx_target_alloc_f_real32_3_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(3)
         real(R4P), pointer, contiguous                  :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._R4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real32_3_64bit

      module subroutine ompx_target_alloc_f_real32_4_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(4)
         real(R4P), pointer, contiguous                  :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._R4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real32_4_64bit

      module subroutine ompx_target_alloc_f_real32_5_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(5)
         real(R4P), pointer, contiguous                  :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._R4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real32_5_64bit

      module subroutine ompx_target_alloc_f_real32_6_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(6)
         real(R4P), pointer, contiguous                  :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._R4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real32_6_64bit

      module subroutine ompx_target_alloc_f_real32_7_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(7)
         real(R4P), pointer, contiguous                  :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._R4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P, lbounds(7):lbounds(7)+dimensions(7)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real32_7_64bit

      module subroutine ompx_target_alloc_f_real64_1_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds
         real(R8P), pointer, contiguous                  :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(dimensions * byte_size(1._R8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real64_1_64bit

      module subroutine ompx_target_alloc_f_real64_2_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(2)
         real(R8P), pointer, contiguous                  :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._R8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real64_2_64bit

      module subroutine ompx_target_alloc_f_real64_3_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(3)
         real(R8P), pointer, contiguous                  :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._R8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real64_3_64bit

      module subroutine ompx_target_alloc_f_real64_4_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(4)
         real(R8P), pointer, contiguous                  :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._R8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real64_4_64bit

      module subroutine ompx_target_alloc_f_real64_5_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(5)
         real(R8P), pointer, contiguous                  :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._R8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real64_5_64bit

      module subroutine ompx_target_alloc_f_real64_6_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(6)
         real(R8P), pointer, contiguous                  :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._R8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real64_6_64bit

      module subroutine ompx_target_alloc_f_real64_7_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(7)
         real(R8P), pointer, contiguous                  :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._R8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P, lbounds(7):lbounds(7)+dimensions(7)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real64_7_64bit

#if defined _real128
      module subroutine ompx_target_alloc_f_real128_1_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds
         real(R16P), pointer, contiguous                 :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(dimensions * byte_size(1._R16P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real128_1_64bit

      module subroutine ompx_target_alloc_f_real128_2_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(2)
         real(R16P), pointer, contiguous                 :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._R16P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real128_2_64bit

      module subroutine ompx_target_alloc_f_real128_3_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(3)
         real(R16P), pointer, contiguous                 :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._R16P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real128_3_64bit

      module subroutine ompx_target_alloc_f_real128_4_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(4)
         real(R16P), pointer, contiguous                 :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._R16P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real128_4_64bit

      module subroutine ompx_target_alloc_f_real128_5_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(5)
         real(R16P), pointer, contiguous                 :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._R16P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real128_5_64bit

      module subroutine ompx_target_alloc_f_real128_6_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(6)
         real(R16P), pointer, contiguous                 :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._R16P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real128_6_64bit

      module subroutine ompx_target_alloc_f_real128_7_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(7)
         real(R16P), pointer, contiguous                 :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._R16P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P, lbounds(7):lbounds(7)+dimensions(7)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_real128_7_64bit

#endif

      ! OpenMP Target Alloc Complex 64 bits dimensions Routines
      module subroutine ompx_target_alloc_f_cmplx32_1_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds
         complex(R4P), pointer, contiguous               :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * dimensions * byte_size(1._R4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx32_1_64bit

      module subroutine ompx_target_alloc_f_cmplx32_2_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(2)
         complex(R4P), pointer, contiguous               :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * product(dimensions) * byte_size(1._R4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx32_2_64bit

      module subroutine ompx_target_alloc_f_cmplx32_3_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(3)
         complex(R4P), pointer, contiguous               :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * product(dimensions) * byte_size(1._R4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx32_3_64bit

      module subroutine ompx_target_alloc_f_cmplx32_4_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(4)
         complex(R4P), pointer, contiguous               :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * product(dimensions) * byte_size(1._R4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx32_4_64bit

      module subroutine ompx_target_alloc_f_cmplx32_5_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(5)
         complex(R4P), pointer, contiguous               :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * product(dimensions) * byte_size(1._R4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx32_5_64bit

      module subroutine ompx_target_alloc_f_cmplx32_6_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(6)
         complex(R4P), pointer, contiguous               :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * product(dimensions) * byte_size(1._R4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx32_6_64bit

      module subroutine ompx_target_alloc_f_cmplx32_7_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(7)
         complex(R4P), pointer, contiguous               :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * product(dimensions) * byte_size(1._R4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P, lbounds(7):lbounds(7)+dimensions(7)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx32_7_64bit

      module subroutine ompx_target_alloc_f_cmplx64_1_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds
         complex(R8P), pointer, contiguous               :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * dimensions * byte_size(1._R8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx64_1_64bit

      module subroutine ompx_target_alloc_f_cmplx64_2_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(2)
         complex(R8P), pointer, contiguous               :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * product(dimensions) * byte_size(1._R8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx64_2_64bit

      module subroutine ompx_target_alloc_f_cmplx64_3_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(3)
         complex(R8P), pointer, contiguous               :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * product(dimensions) * byte_size(1._R8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx64_3_64bit

      module subroutine ompx_target_alloc_f_cmplx64_4_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(4)
         complex(R8P), pointer, contiguous               :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * product(dimensions) * byte_size(1._R8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx64_4_64bit

      module subroutine ompx_target_alloc_f_cmplx64_5_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(5)
         complex(R8P), pointer, contiguous               :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * product(dimensions) * byte_size(1._R8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx64_5_64bit

      module subroutine ompx_target_alloc_f_cmplx64_6_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(6)
         complex(R8P), pointer, contiguous               :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * product(dimensions) * byte_size(1._R8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx64_6_64bit

      module subroutine ompx_target_alloc_f_cmplx64_7_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(7)
         complex(R8P), pointer, contiguous               :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * product(dimensions) * byte_size(1._R8P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P, lbounds(7):lbounds(7)+dimensions(7)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx64_7_64bit

#if defined _real128
      module subroutine ompx_target_alloc_f_cmplx128_1_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds
         complex(R16P), pointer, contiguous              :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * dimensions * byte_size(1._R16P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx128_1_64bit

      module subroutine ompx_target_alloc_f_cmplx128_2_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(2)
         complex(R16P), pointer, contiguous              :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * product(dimensions) * byte_size(1._R16P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx128_2_64bit

      module subroutine ompx_target_alloc_f_cmplx128_3_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(3)
         complex(R16P), pointer, contiguous              :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * product(dimensions) * byte_size(1._R16P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx128_3_64bit

      module subroutine ompx_target_alloc_f_cmplx128_4_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(4)
         complex(R16P), pointer, contiguous              :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * product(dimensions) * byte_size(1._R16P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx128_4_64bit

      module subroutine ompx_target_alloc_f_cmplx128_5_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(5)
         complex(R16P), pointer, contiguous              :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * product(dimensions) * byte_size(1._R16P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx128_5_64bit

      module subroutine ompx_target_alloc_f_cmplx128_6_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(6)
         complex(R16P), pointer, contiguous              :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * product(dimensions) * byte_size(1._R16P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx128_6_64bit

      module subroutine ompx_target_alloc_f_cmplx128_7_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(7)
         complex(R16P), pointer, contiguous              :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(2_I8P * product(dimensions) * byte_size(1._R16P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P, lbounds(7):lbounds(7)+dimensions(7)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_cmplx128_7_64bit

#endif

      ! OpenMP Target Alloc Logical 64 bits dimensions Routines
      module subroutine ompx_target_alloc_f_lgcl32_1_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds
         logical(I4P), pointer, contiguous               :: fptr(:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(dimensions * byte_size(1._I4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions])
               fptr_dev(lbounds:lbounds+dimensions-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_lgcl32_1_64bit

      module subroutine ompx_target_alloc_f_lgcl32_2_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(2)
         logical(I4P), pointer, contiguous               :: fptr(:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._I4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_lgcl32_2_64bit

      module subroutine ompx_target_alloc_f_lgcl32_3_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(3)
         logical(I4P), pointer, contiguous               :: fptr(:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._I4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_lgcl32_3_64bit

      module subroutine ompx_target_alloc_f_lgcl32_4_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(4)
         logical(I4P), pointer, contiguous               :: fptr(:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._I4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_lgcl32_4_64bit

      module subroutine ompx_target_alloc_f_lgcl32_5_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(5)
         logical(I4P), pointer, contiguous               :: fptr(:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._I4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_lgcl32_5_64bit

      module subroutine ompx_target_alloc_f_lgcl32_6_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(6)
         logical(I4P), pointer, contiguous               :: fptr(:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._I4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_lgcl32_6_64bit

      module subroutine ompx_target_alloc_f_lgcl32_7_64bit(fptr_dev, dimensions, omp_dev, ierr, lbounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: lbounds(7)
         logical(I4P), pointer, contiguous               :: fptr(:,:,:,:,:,:,:)
         type(c_ptr)                                     :: cptr_dev


         cptr_dev = omp_target_alloc(int(product(dimensions) * byte_size(1._I4P), c_size_t), int(omp_dev,c_int))

         if (c_associated(cptr_dev)) then
            if (present(lbounds)) then
               call c_f_pointer(cptr_dev, fptr, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
               fptr_dev(lbounds(1):lbounds(1)+dimensions(1)-1_I8P, lbounds(2):lbounds(2)+dimensions(2)-1_I8P, lbounds(3):lbounds(3)+dimensions(3)-1_I8P, lbounds(4):lbounds(4)+dimensions(4)-1_I8P, &
                        lbounds(5):lbounds(5)+dimensions(5)-1_I8P, lbounds(6):lbounds(6)+dimensions(6)-1_I8P, lbounds(7):lbounds(7)+dimensions(7)-1_I8P) => fptr
            else
               call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                                 dimensions(5), dimensions(6), dimensions(7)])
            endif
            ierr = 0
         else
            fptr_dev => null()
            ierr = 1000
         endif
      endsubroutine ompx_target_alloc_f_lgcl32_7_64bit


endsubmodule dmr_target_alloc