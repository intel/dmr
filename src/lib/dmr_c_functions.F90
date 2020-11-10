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

module dmr_c_functions
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
         type(c_ptr)                               :: omp_target_alloc_c
         integer(kind=c_size_t), value, intent(in) :: total_byte_dim
         integer(kind=c_int),    value, intent(in) :: dev_id
      endfunction omp_target_alloc_c

      subroutine omp_target_free_c(dev_ptr, dev_id) bind(c, name='omp_target_free_c')
         use iso_c_binding, only : c_ptr, c_int
         type(c_ptr),         value, intent(in) :: dev_ptr
         integer(kind=c_int), value, intent(in) :: dev_id
      endsubroutine omp_target_free_c

      function omp_target_is_present_c(ptr, dev_id) bind(c, name='omp_target_is_present_c')
         use iso_c_binding, only : c_int, c_ptr
         integer(c_int)                         :: omp_target_is_present_c
         type(c_ptr),         value, intent(in) :: ptr
         integer(kind=c_int), value, intent(in) :: dev_id
      endfunction omp_target_is_present_c

      function omp_get_mapped_ptr_c(ptr, dev_id) bind(c, name='omp_get_mapped_ptr_c')
         use iso_c_binding, only : c_int, c_ptr
         type(c_ptr)                            :: omp_get_mapped_ptr_c
         type(c_ptr),         value, intent(in) :: ptr
         integer(kind=c_int), value, intent(in) :: dev_id
      endfunction omp_get_mapped_ptr_c

      function omp_target_memcpy_c(dst, src, total_byte_dim, dst_off, src_off, &
            dst_dev_id, src_dev_id) bind(c, name='omp_target_memcopy_c')
         use iso_c_binding, only : c_int, c_ptr, c_size_t
         integer(c_int)                :: omp_target_memcpy_c
         type(c_ptr),            value :: dst, src
         integer(kind=c_size_t), value :: total_byte_dim, dst_off, src_off
         integer(kind=c_int),    value :: dst_dev_id, src_dev_id
      endfunction omp_target_memcpy_c

      function omp_target_memcpy_rect_c(dst, src, elem_byte_dim, dims, volume, &
            dst_offs, src_offs, dst_dims, src_dims, dst_dev_id, src_dev_id) bind(c, name='omp_target_memcopy_rect_c')
         use iso_c_binding, only : c_int, c_ptr, c_size_t
         integer(c_int)                :: omp_target_memcpy_rect_c
         type(c_ptr),            value :: dst, src
         integer(kind=c_size_t), value :: elem_byte_dim
         integer(kind=c_int),    value :: dims
         integer(kind=c_size_t)        :: volume(*), dst_offs(*), src_offs(*), dst_dims(*), src_dims(*)
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

   public omp_target_alloc_c, omp_target_free_c, omp_target_is_present_c, omp_get_mapped_ptr_c, &
          omp_target_memcpy_c, &
          omp_target_memcpy_rect_c, omp_target_associate_ptr_c, omp_target_disassociate_ptr_c

endmodule dmr_c_functions
