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

module falco_c_functions
   use, intrinsic :: iso_c_binding
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

   public omp_target_alloc_c, omp_target_free_c, omp_target_is_present_c, omp_target_memcpy_c, &
          omp_target_memcpy_rect_c, omp_target_associate_ptr_c, omp_target_disassociate_ptr_c

endmodule falco_c_functions
