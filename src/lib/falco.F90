!* ========================================================================== *
!*
!* FALCO - FortrAn Library for C OpenMP offload functions mapping
!*
!* SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
!* http://software.intel.com/en-us/articles/
!*        intel-sample-source-code-license-agreement/
!*
!* Copyright 2020 Giacomo Rossi, Intel Corporation
!*
!* THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED,
!* INCLUDING BUT NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY,
!* FITNESS FOR A PARTICULAR PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL
!* PROPERTY RIGHTS.
!*
!* ========================================================================== *

module falco
   use, intrinsic :: iso_c_binding
   use penf
   use omp_lib

   implicit none

   save

   interface
      function omp_get_default_device_c() bind(C,NAME='omp_get_default_device_c')
         use iso_c_binding, only : c_int
         implicit none
         integer(c_int) :: omp_get_default_device_c
      end function omp_get_default_device_c

      function omp_get_initial_device_c() bind(C,NAME='omp_get_initial_device_c')
         use iso_c_binding, only : c_int
         implicit none
         integer(c_int) :: omp_get_initial_device_c
      end function omp_get_initial_device_c

      function omp_target_alloc_c(total_byte_dim, dev_id) bind(C,name='omp_target_alloc_c')
         use iso_c_binding, only : c_ptr, c_size_t, c_int
         implicit none
         type(c_ptr)                   :: omp_target_alloc_c
         integer(kind=c_size_t), value :: total_byte_dim
         integer(kind=c_int),    value :: dev_id
      endfunction omp_target_alloc_c

      subroutine omp_target_free_c(dev_ptr, dev_id) bind(c, name='omp_target_free_c')
         use iso_c_binding, only : c_ptr, c_int
         type(c_ptr),         value, intent(in) :: dev_ptr
         integer(kind=c_int), value, intent(in) :: dev_id
      endsubroutine omp_target_free_c

      function omp_target_is_present_c(ptr, dev_id) bind(c, name='omp_target_is_present_c')
         use iso_c_binding, only : c_int, c_ptr
         integer(c_int)             :: omp_target_is_present_c
         type(c_ptr),         value :: ptr
         integer(kind=c_int), value :: dev_id
      endfunction omp_target_is_present_c

      function omp_target_memcpy_c(dst, src, total_byte_dim, dst_off, src_off, &
            dst_dev_id, src_dev_id) bind(c, name='omp_target_memcopy_c')
         use iso_c_binding, only : c_int, c_ptr, c_size_t
         integer(c_int)                :: omp_target_memcpy_c
         type(c_ptr),            value :: dst, src
         integer(kind=c_size_t), value :: total_byte_dim, dst_off, src_off
         integer(kind=c_int),    value :: dst_dev_id, src_dev_id
      endfunction omp_target_memcpy_c

      function omp_target_memcpy_rect_c(dst, src, elem_byte_dim, dims, volume, &
            dst_off, src_off, dst_dims, src_dims, dst_dev_id, src_dev_id) bind(c, name='omp_target_memcopy_rect_c')
         use iso_c_binding, only : c_int, c_ptr, c_size_t
         integer(c_int)                :: omp_target_memcpy_rect_c
         type(c_ptr),            value :: dst, src
         integer(kind=c_size_t), value :: elem_byte_dim
         integer(kind=c_int),    value :: dims
         integer(kind=c_size_t), value :: volume, dst_off, src_off, dst_dims, src_dims
         integer(kind=c_int),    value :: dst_dev_id, src_dev_id
      endfunction omp_target_memcpy_rect_c

      function omp_target_associate_ptr_c(hst_ptr, dev_ptr, total_byte_dim, &
            dev_off, dev_id) bind(c, name='omp_target_associate_ptr_c')
         use iso_c_binding, only : c_int, c_ptr, c_size_t
         integer(c_int)                :: omp_target_associate_ptr_c
         type(c_ptr),            value :: hst_ptr, dev_ptr
         integer(kind=c_size_t), value :: total_byte_dim, dev_off
         integer(kind=c_int),    value :: dev_id
      endfunction omp_target_associate_ptr_c

      function omp_target_disassociate_ptr_c(hst_ptr, dev_id) bind(c, name='omp_target_disassociate_ptr_c')
         use iso_c_binding, only : c_int, c_ptr
         integer(c_int)             :: omp_target_disassociate_ptr_c
         type(c_ptr),         value :: hst_ptr
         integer(kind=c_int), value :: dev_id
      endfunction omp_target_disassociate_ptr_c
   endinterface

   interface omp_target_free_f
      module procedure &
                       omp_target_free_f_I1P_1,  omp_target_free_f_I1P_2,  omp_target_free_f_I1P_3,  &
                       omp_target_free_f_I1P_4,  omp_target_free_f_I1P_5,  omp_target_free_f_I1P_6,  &
                       omp_target_free_f_I1P_7,  &
                       omp_target_free_f_I2P_1,  omp_target_free_f_I2P_2,  omp_target_free_f_I2P_3,  &
                       omp_target_free_f_I2P_4,  omp_target_free_f_I2P_5,  omp_target_free_f_I2P_6,  &
                       omp_target_free_f_I2P_7,  &
                       omp_target_free_f_I4P_1,  omp_target_free_f_I4P_2,  omp_target_free_f_I4P_3,  &
                       omp_target_free_f_I4P_4,  omp_target_free_f_I4P_5,  omp_target_free_f_I4P_6,  &
                       omp_target_free_f_I4P_7,  &
                       omp_target_free_f_I8P_1,  omp_target_free_f_I8P_2,  omp_target_free_f_I8P_3,  &
                       omp_target_free_f_I8P_4,  omp_target_free_f_I8P_5,  omp_target_free_f_I8P_6,  &
                       omp_target_free_f_I8P_7,  &
#if defined _R16P
                       omp_target_free_f_R16P_1, omp_target_free_f_R16P_2, omp_target_free_f_R16P_3, &
                       omp_target_free_f_R16P_4, omp_target_free_f_R16P_5, omp_target_free_f_R16P_6, &
                       omp_target_free_f_R16P_7, &
#endif
                       omp_target_free_f_R4P_1,  omp_target_free_f_R4P_2,  omp_target_free_f_R4P_3,  &
                       omp_target_free_f_R4P_4,  omp_target_free_f_R4P_5,  omp_target_free_f_R4P_6,  &
                       omp_target_free_f_R4P_7,  &
                       omp_target_free_f_R8P_1,  omp_target_free_f_R8P_2,  omp_target_free_f_R8P_3,  &
                       omp_target_free_f_R8P_4,  omp_target_free_f_R8P_5,  omp_target_free_f_R8P_6,  &
                       omp_target_free_f_R8P_7,  &
#if defined _R16P
                       omp_target_free_f_C16P_1, omp_target_free_f_C16P_2, omp_target_free_f_C16P_3, &
                       omp_target_free_f_C16P_4, omp_target_free_f_C16P_5, omp_target_free_f_C16P_6, &
                       omp_target_free_f_C16P_7,  &
#endif
                       omp_target_free_f_C4P_1,  omp_target_free_f_C4P_2,  omp_target_free_f_C4P_3,  &
                       omp_target_free_f_C4P_4,  omp_target_free_f_C4P_5,  omp_target_free_f_C4P_6,  &
                       omp_target_free_f_C4P_7,  &
                       omp_target_free_f_C8P_1,  omp_target_free_f_C8P_2,  omp_target_free_f_C8P_3,  &
                       omp_target_free_f_C8P_4,  omp_target_free_f_C8P_5,  omp_target_free_f_C8P_6,  &
                       omp_target_free_f_C8P_7
   endinterface omp_target_free_f

   interface omp_target_alloc_f
      module procedure &
                       omp_target_alloc_f_I1P_1,  omp_target_alloc_f_I1P_2,  omp_target_alloc_f_I1P_3,  &
                       omp_target_alloc_f_I1P_4,  omp_target_alloc_f_I1P_5,  omp_target_alloc_f_I1P_6,  &
                       omp_target_alloc_f_I1P_7,  &
                       omp_target_alloc_f_I2P_1,  omp_target_alloc_f_I2P_2,  omp_target_alloc_f_I2P_3,  &
                       omp_target_alloc_f_I2P_4,  omp_target_alloc_f_I2P_5,  omp_target_alloc_f_I2P_6,  &
                       omp_target_alloc_f_I2P_7,  &
                       omp_target_alloc_f_I4P_1,  omp_target_alloc_f_I4P_2,  omp_target_alloc_f_I4P_3,  &
                       omp_target_alloc_f_I4P_4,  omp_target_alloc_f_I4P_5,  omp_target_alloc_f_I4P_6,  &
                       omp_target_alloc_f_I4P_7,  &
                       omp_target_alloc_f_I8P_1,  omp_target_alloc_f_I8P_2,  omp_target_alloc_f_I8P_3,  &
                       omp_target_alloc_f_I8P_4,  omp_target_alloc_f_I8P_5,  omp_target_alloc_f_I8P_6,  &
                       omp_target_alloc_f_I8P_7,  &
#if defined _R16P
                       omp_target_alloc_f_R16P_1, omp_target_alloc_f_R16P_2, omp_target_alloc_f_R16P_3, &
                       omp_target_alloc_f_R16P_4, omp_target_alloc_f_R16P_5, omp_target_alloc_f_R16P_6, &
                       omp_target_alloc_f_R16P_7, &
#endif
                       omp_target_alloc_f_R4P_1,  omp_target_alloc_f_R4P_2,  omp_target_alloc_f_R4P_3,  &
                       omp_target_alloc_f_R4P_4,  omp_target_alloc_f_R4P_5,  omp_target_alloc_f_R4P_6,  &
                       omp_target_alloc_f_R4P_7,  &
                       omp_target_alloc_f_R8P_1,  omp_target_alloc_f_R8P_2,  omp_target_alloc_f_R8P_3,  &
                       omp_target_alloc_f_R8P_4,  omp_target_alloc_f_R8P_5,  omp_target_alloc_f_R8P_6,  &
                       omp_target_alloc_f_R8P_7,  &
#if defined _R16P
                       omp_target_alloc_f_C16P_1, omp_target_alloc_f_C16P_2, omp_target_alloc_f_C16P_3, &
                       omp_target_alloc_f_C16P_4, omp_target_alloc_f_C16P_5, omp_target_alloc_f_C16P_6, &
                       omp_target_alloc_f_C16P_7,  &
#endif
                       omp_target_alloc_f_C4P_1,  omp_target_alloc_f_C4P_2,  omp_target_alloc_f_C4P_3,  &
                       omp_target_alloc_f_C4P_4,  omp_target_alloc_f_C4P_5,  omp_target_alloc_f_C4P_6,  &
                       omp_target_alloc_f_C4P_7,  &
                       omp_target_alloc_f_C8P_1,  omp_target_alloc_f_C8P_2,  omp_target_alloc_f_C8P_3,  &
                       omp_target_alloc_f_C8P_4,  omp_target_alloc_f_C8P_5,  omp_target_alloc_f_C8P_6,  &
                       omp_target_alloc_f_C8P_7
   endinterface omp_target_alloc_f

   interface omp_target_memcpy_f
      module procedure &
                       omp_target_memcpy_f_I1P_1,  omp_target_memcpy_f_I1P_2,  omp_target_memcpy_f_I1P_3,  &
                       omp_target_memcpy_f_I1P_4,  omp_target_memcpy_f_I1P_5,  omp_target_memcpy_f_I1P_6,  &
                       omp_target_memcpy_f_I1P_7,  &
                       omp_target_memcpy_f_I2P_1,  omp_target_memcpy_f_I2P_2,  omp_target_memcpy_f_I2P_3,  &
                       omp_target_memcpy_f_I2P_4,  omp_target_memcpy_f_I2P_5,  omp_target_memcpy_f_I2P_6,  &
                       omp_target_memcpy_f_I2P_7,  &
                       omp_target_memcpy_f_I4P_1,  omp_target_memcpy_f_I4P_2,  omp_target_memcpy_f_I4P_3,  &
                       omp_target_memcpy_f_I4P_4,  omp_target_memcpy_f_I4P_5,  omp_target_memcpy_f_I4P_6,  &
                       omp_target_memcpy_f_I4P_7,  &
                       omp_target_memcpy_f_I8P_1,  omp_target_memcpy_f_I8P_2,  omp_target_memcpy_f_I8P_3,  &
                       omp_target_memcpy_f_I8P_4,  omp_target_memcpy_f_I8P_5,  omp_target_memcpy_f_I8P_6,  &
                       omp_target_memcpy_f_I8P_7,  &
#if defined _R16P
                       omp_target_memcpy_f_R16P_1, omp_target_memcpy_f_R16P_2, omp_target_memcpy_f_R16P_3, &
                       omp_target_memcpy_f_R16P_4, omp_target_memcpy_f_R16P_5, omp_target_memcpy_f_R16P_6, &
                       omp_target_memcpy_f_R16P_7, &
#endif
                       omp_target_memcpy_f_R4P_1,  omp_target_memcpy_f_R4P_2,  omp_target_memcpy_f_R4P_3,  &
                       omp_target_memcpy_f_R4P_4,  omp_target_memcpy_f_R4P_5,  omp_target_memcpy_f_R4P_6,  &
                       omp_target_memcpy_f_R4P_7,  &
                       omp_target_memcpy_f_R8P_1,  omp_target_memcpy_f_R8P_2,  omp_target_memcpy_f_R8P_3,  &
                       omp_target_memcpy_f_R8P_4,  omp_target_memcpy_f_R8P_5,  omp_target_memcpy_f_R8P_6,  &
                       omp_target_memcpy_f_R8P_7,  &
#if defined _R16P
                       omp_target_memcpy_f_C16P_1, omp_target_memcpy_f_C16P_2, omp_target_memcpy_f_C16P_3, &
                       omp_target_memcpy_f_C16P_4, omp_target_memcpy_f_C16P_5, omp_target_memcpy_f_C16P_6, &
                       omp_target_memcpy_f_C16P_7,  &
#endif
                       omp_target_memcpy_f_C4P_1,  omp_target_memcpy_f_C4P_2,  omp_target_memcpy_f_C4P_3,  &
                       omp_target_memcpy_f_C4P_4,  omp_target_memcpy_f_C4P_5,  omp_target_memcpy_f_C4P_6,  &
                       omp_target_memcpy_f_C4P_7,  &
                       omp_target_memcpy_f_C8P_1,  omp_target_memcpy_f_C8P_2,  omp_target_memcpy_f_C8P_3,  &
                       omp_target_memcpy_f_C8P_4,  omp_target_memcpy_f_C8P_5,  omp_target_memcpy_f_C8P_6,  &
                       omp_target_memcpy_f_C8P_7
   endinterface omp_target_memcpy_f

   public omp_target_alloc_c, omp_target_free_c, omp_target_is_present_c, omp_target_memcpy_c, &
          omp_target_memcpy_rect_c, omp_target_associate_ptr_c, omp_target_disassociate_ptr_c, &
          omp_get_default_device_c, omp_get_initial_device_c, &
          omp_target_alloc_f, omp_target_free_f, omp_target_memcpy_f

   private

   contains

      ! OpenMP Target Free Integer Routines
      subroutine omp_target_free_f_I1P_1(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I1P_1

      subroutine omp_target_free_f_I1P_2(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I1P_2

      subroutine omp_target_free_f_I1P_3(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I1P_3

      subroutine omp_target_free_f_I1P_4(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I1P_4

      subroutine omp_target_free_f_I1P_5(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I1P_5

      subroutine omp_target_free_f_I1P_6(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I1P_6

      subroutine omp_target_free_f_I1P_7(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I1P_7

      subroutine omp_target_free_f_I2P_1(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I2P_1

      subroutine omp_target_free_f_I2P_2(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I2P_2

      subroutine omp_target_free_f_I2P_3(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I2P_3

      subroutine omp_target_free_f_I2P_4(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I2P_4

      subroutine omp_target_free_f_I2P_5(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I2P_5

      subroutine omp_target_free_f_I2P_6(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I2P_6

      subroutine omp_target_free_f_I2P_7(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I2P_7

      subroutine omp_target_free_f_I4P_1(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I4P_1

      subroutine omp_target_free_f_I4P_2(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I4P_2

      subroutine omp_target_free_f_I4P_3(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I4P_3

      subroutine omp_target_free_f_I4P_4(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I4P_4

      subroutine omp_target_free_f_I4P_5(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I4P_5

      subroutine omp_target_free_f_I4P_6(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I4P_6

      subroutine omp_target_free_f_I4P_7(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I4P_7

      subroutine omp_target_free_f_I8P_1(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I8P_1

      subroutine omp_target_free_f_I8P_2(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I8P_2

      subroutine omp_target_free_f_I8P_3(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I8P_3

      subroutine omp_target_free_f_I8P_4(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I8P_4

      subroutine omp_target_free_f_I8P_5(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I8P_5

      subroutine omp_target_free_f_I8P_6(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I8P_6

      subroutine omp_target_free_f_I8P_7(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_I8P_7

      ! OpenMP Target Alloc Integer Routines
      subroutine omp_target_alloc_f_I1P_1(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1_I1P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_I1P_1

      subroutine omp_target_alloc_f_I1P_2(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_I1P_2

      subroutine omp_target_alloc_f_I1P_3(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_I1P_3

      subroutine omp_target_alloc_f_I1P_4(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_I1P_4

      subroutine omp_target_alloc_f_I1P_5(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_I1P_5

      subroutine omp_target_alloc_f_I1P_6(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_I1P_6

      subroutine omp_target_alloc_f_I1P_7(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_I1P_7

      subroutine omp_target_alloc_f_I2P_1(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1_I2P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_I2P_1

      subroutine omp_target_alloc_f_I2P_2(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_I2P_2

      subroutine omp_target_alloc_f_I2P_3(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_I2P_3

      subroutine omp_target_alloc_f_I2P_4(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_I2P_4

      subroutine omp_target_alloc_f_I2P_5(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_I2P_5

      subroutine omp_target_alloc_f_I2P_6(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_I2P_6

      subroutine omp_target_alloc_f_I2P_7(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_I2P_7

      subroutine omp_target_alloc_f_I4P_1(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1_I4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_I4P_1

      subroutine omp_target_alloc_f_I4P_2(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_I4P_2

      subroutine omp_target_alloc_f_I4P_3(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_I4P_3

      subroutine omp_target_alloc_f_I4P_4(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_I4P_4

      subroutine omp_target_alloc_f_I4P_5(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_I4P_5

      subroutine omp_target_alloc_f_I4P_6(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_I4P_6

      subroutine omp_target_alloc_f_I4P_7(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_I4P_7

      subroutine omp_target_alloc_f_I8P_1(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1_I8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_I8P_1

      subroutine omp_target_alloc_f_I8P_2(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_I8P_2

      subroutine omp_target_alloc_f_I8P_3(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_I8P_3

      subroutine omp_target_alloc_f_I8P_4(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_I8P_4

      subroutine omp_target_alloc_f_I8P_5(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_I8P_5

      subroutine omp_target_alloc_f_I8P_6(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_I8P_6

      subroutine omp_target_alloc_f_I8P_7(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_I8P_7

      ! OpenMP Target Memcpy Integer Routines
      subroutine omp_target_memcpy_f_I1P_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I1P_1

      subroutine omp_target_memcpy_f_I1P_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I1P_2

      subroutine omp_target_memcpy_f_I1P_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I1P_3

      subroutine omp_target_memcpy_f_I1P_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I1P_4

      subroutine omp_target_memcpy_f_I1P_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I1P_5

      subroutine omp_target_memcpy_f_I1P_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I1P_6

      subroutine omp_target_memcpy_f_I1P_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I1P_7

      subroutine omp_target_memcpy_f_I2P_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I2P_1

      subroutine omp_target_memcpy_f_I2P_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I2P_2

      subroutine omp_target_memcpy_f_I2P_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I2P_3

      subroutine omp_target_memcpy_f_I2P_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I2P_4

      subroutine omp_target_memcpy_f_I2P_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I2P_5

      subroutine omp_target_memcpy_f_I2P_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I2P_6

      subroutine omp_target_memcpy_f_I2P_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I2P_7

      subroutine omp_target_memcpy_f_I4P_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I4P_1

      subroutine omp_target_memcpy_f_I4P_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I4P_2

      subroutine omp_target_memcpy_f_I4P_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I4P_3

      subroutine omp_target_memcpy_f_I4P_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I4P_4

      subroutine omp_target_memcpy_f_I4P_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I4P_5

      subroutine omp_target_memcpy_f_I4P_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I4P_6

      subroutine omp_target_memcpy_f_I4P_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I4P_7

      subroutine omp_target_memcpy_f_I8P_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I8P_1

      subroutine omp_target_memcpy_f_I8P_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I8P_2

      subroutine omp_target_memcpy_f_I8P_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I8P_3

      subroutine omp_target_memcpy_f_I8P_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I8P_4

      subroutine omp_target_memcpy_f_I8P_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I8P_5

      subroutine omp_target_memcpy_f_I8P_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I8P_6

      subroutine omp_target_memcpy_f_I8P_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_I8P_7

      ! OpenMP Target Free Real Routines
      subroutine omp_target_free_f_R4P_1(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R4P_1

      subroutine omp_target_free_f_R4P_2(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R4P_2

      subroutine omp_target_free_f_R4P_3(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R4P_3

      subroutine omp_target_free_f_R4P_4(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R4P_4

      subroutine omp_target_free_f_R4P_5(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R4P_5

      subroutine omp_target_free_f_R4P_6(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R4P_6

      subroutine omp_target_free_f_R4P_7(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R4P_7

      subroutine omp_target_free_f_R8P_1(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R8P_1

      subroutine omp_target_free_f_R8P_2(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R8P_2

      subroutine omp_target_free_f_R8P_3(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R8P_3

      subroutine omp_target_free_f_R8P_4(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R8P_4

      subroutine omp_target_free_f_R8P_5(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R8P_5

      subroutine omp_target_free_f_R8P_6(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R8P_6

      subroutine omp_target_free_f_R8P_7(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R8P_7

#if defined _R16P
      subroutine omp_target_free_f_R16P_1(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R16P_1

      subroutine omp_target_free_f_R16P_2(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R16P_2

      subroutine omp_target_free_f_R16P_3(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R16P_3

      subroutine omp_target_free_f_R16P_4(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R16P_4

      subroutine omp_target_free_f_R16P_5(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R16P_5

      subroutine omp_target_free_f_R16P_6(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R16P_6

      subroutine omp_target_free_f_R16P_7(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_R16P_7
#endif

      ! OpenMP Target Alloc Real Routines
      subroutine omp_target_alloc_f_R4P_1(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)          :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1._R4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_R4P_1

      subroutine omp_target_alloc_f_R4P_2(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)          :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_R4P_2

      subroutine omp_target_alloc_f_R4P_3(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)          :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_R4P_3

      subroutine omp_target_alloc_f_R4P_4(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_R4P_4

      subroutine omp_target_alloc_f_R4P_5(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_R4P_5

      subroutine omp_target_alloc_f_R4P_6(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_R4P_6

      subroutine omp_target_alloc_f_R4P_7(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_R4P_7

      subroutine omp_target_alloc_f_R8P_1(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)          :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1._R8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_R8P_1

      subroutine omp_target_alloc_f_R8P_2(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)          :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_R8P_2

      subroutine omp_target_alloc_f_R8P_3(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)          :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_R8P_3

      subroutine omp_target_alloc_f_R8P_4(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_R8P_4

      subroutine omp_target_alloc_f_R8P_5(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_R8P_5

      subroutine omp_target_alloc_f_R8P_6(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_R8P_6

      subroutine omp_target_alloc_f_R8P_7(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_R8P_7

#if defined _R16P
      subroutine omp_target_alloc_f_R16P_1(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)           :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1._R16P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_R16P_1

      subroutine omp_target_alloc_f_R16P_2(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)           :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_R16P_2

      subroutine omp_target_alloc_f_R16P_3(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)           :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_R16P_3

      subroutine omp_target_alloc_f_R16P_4(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)           :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_R16P_4

      subroutine omp_target_alloc_f_R16P_5(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)           :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_R16P_5

      subroutine omp_target_alloc_f_R16P_6(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)           :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_R16P_6

      subroutine omp_target_alloc_f_R16P_7(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)           :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_R16P_7
#endif

      ! OpenMP Target Memcpy Real Routines
      subroutine omp_target_memcpy_f_R4P_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R4P_1

      subroutine omp_target_memcpy_f_R4P_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R4P_2

      subroutine omp_target_memcpy_f_R4P_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R4P_3

      subroutine omp_target_memcpy_f_R4P_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R4P_4

      subroutine omp_target_memcpy_f_R4P_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R4P_5

      subroutine omp_target_memcpy_f_R4P_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R4P_6

      subroutine omp_target_memcpy_f_R4P_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R4P_7

      subroutine omp_target_memcpy_f_R8P_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R8P_1

      subroutine omp_target_memcpy_f_R8P_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R8P_2

      subroutine omp_target_memcpy_f_R8P_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R8P_3

      subroutine omp_target_memcpy_f_R8P_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R8P_4

      subroutine omp_target_memcpy_f_R8P_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R8P_5

      subroutine omp_target_memcpy_f_R8P_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R8P_6

      subroutine omp_target_memcpy_f_R8P_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R8P_7

#if defined _R16P
      subroutine omp_target_memcpy_f_R16P_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R16P_1

      subroutine omp_target_memcpy_f_R16P_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R16P_2

      subroutine omp_target_memcpy_f_R16P_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R16P_3

      subroutine omp_target_memcpy_f_R16P_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R16P_4

      subroutine omp_target_memcpy_f_R16P_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R16P_5

      subroutine omp_target_memcpy_f_R16P_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R16P_6

      subroutine omp_target_memcpy_f_R16P_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_R16P_7
#endif

      ! OpenMP Target Free Complex Routines
      subroutine omp_target_free_f_C4P_1(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C4P_1

      subroutine omp_target_free_f_C4P_2(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C4P_2

      subroutine omp_target_free_f_C4P_3(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C4P_3

      subroutine omp_target_free_f_C4P_4(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C4P_4

      subroutine omp_target_free_f_C4P_5(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C4P_5

      subroutine omp_target_free_f_C4P_6(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C4P_6

      subroutine omp_target_free_f_C4P_7(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C4P_7

      subroutine omp_target_free_f_C8P_1(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C8P_1

      subroutine omp_target_free_f_C8P_2(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C8P_2

      subroutine omp_target_free_f_C8P_3(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C8P_3

      subroutine omp_target_free_f_C8P_4(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C8P_4

      subroutine omp_target_free_f_C8P_5(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C8P_5

      subroutine omp_target_free_f_C8P_6(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C8P_6

      subroutine omp_target_free_f_C8P_7(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C8P_7

#if defined _R16P
      subroutine omp_target_free_f_C16P_1(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C16P_1

      subroutine omp_target_free_f_C16P_2(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C16P_2

      subroutine omp_target_free_f_C16P_3(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C16P_3

      subroutine omp_target_free_f_C16P_4(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C16P_4

      subroutine omp_target_free_f_C16P_5(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C16P_5

      subroutine omp_target_free_f_C16P_6(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C16P_6

      subroutine omp_target_free_f_C16P_7(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_C16P_7
#endif

      ! OpenMP Target Alloc Complex Routines
      subroutine omp_target_alloc_f_C4P_1(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)             :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * dimensions, c_size_t) * byte_size(1._R4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_C4P_1

      subroutine omp_target_alloc_f_C4P_2(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)          :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_C4P_2

      subroutine omp_target_alloc_f_C4P_3(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)          :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_C4P_3

      subroutine omp_target_alloc_f_C4P_4(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_C4P_4

      subroutine omp_target_alloc_f_C4P_5(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_C4P_5

      subroutine omp_target_alloc_f_C4P_6(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_C4P_6

      subroutine omp_target_alloc_f_C4P_7(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_C4P_7

      subroutine omp_target_alloc_f_C8P_1(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)             :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * dimensions, c_size_t) * byte_size(1._R8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_C8P_1

      subroutine omp_target_alloc_f_C8P_2(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)             :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_C8P_2

      subroutine omp_target_alloc_f_C8P_3(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)             :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_C8P_3

      subroutine omp_target_alloc_f_C8P_4(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)             :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_C8P_4

      subroutine omp_target_alloc_f_C8P_5(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)             :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_C8P_5

      subroutine omp_target_alloc_f_C8P_6(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)             :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_C8P_6

      subroutine omp_target_alloc_f_C8P_7(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)             :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_C8P_7

#if defined _R16P
      subroutine omp_target_alloc_f_C16P_1(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)              :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * dimensions, c_size_t) * byte_size(1._R16P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_C16P_1

      subroutine omp_target_alloc_f_C16P_2(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)              :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_C16P_2

      subroutine omp_target_alloc_f_C16P_3(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)              :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_C16P_3

      subroutine omp_target_alloc_f_C16P_4(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)              :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_C16P_4

      subroutine omp_target_alloc_f_C16P_5(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)              :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_C16P_5

      subroutine omp_target_alloc_f_C16P_6(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)              :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_C16P_6

      subroutine omp_target_alloc_f_C16P_7(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)              :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_C16P_7
#endif

      ! OpenMP Target Memcpy Complex Routines
      subroutine omp_target_memcpy_f_C4P_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C4P_1

      subroutine omp_target_memcpy_f_C4P_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C4P_2

      subroutine omp_target_memcpy_f_C4P_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C4P_3

      subroutine omp_target_memcpy_f_C4P_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C4P_4

      subroutine omp_target_memcpy_f_C4P_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C4P_5

      subroutine omp_target_memcpy_f_C4P_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C4P_6

      subroutine omp_target_memcpy_f_C4P_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C4P_7

      subroutine omp_target_memcpy_f_C8P_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C8P_1

      subroutine omp_target_memcpy_f_C8P_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C8P_2

      subroutine omp_target_memcpy_f_C8P_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C8P_3

      subroutine omp_target_memcpy_f_C8P_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C8P_4

      subroutine omp_target_memcpy_f_C8P_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C8P_5

      subroutine omp_target_memcpy_f_C8P_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C8P_6

      subroutine omp_target_memcpy_f_C8P_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C8P_7

#if defined _R16P
      subroutine omp_target_memcpy_f_C16P_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C16P_1

      subroutine omp_target_memcpy_f_C16P_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C16P_2

      subroutine omp_target_memcpy_f_C16P_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C16P_3

      subroutine omp_target_memcpy_f_C16P_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C16P_4

      subroutine omp_target_memcpy_f_C16P_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C16P_5

      subroutine omp_target_memcpy_f_C16P_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C16P_6

      subroutine omp_target_memcpy_f_C16P_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_C16P_7
#endif

endmodule falco
