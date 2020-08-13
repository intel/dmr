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
   use iso_c_binding
   use penf
   implicit none

   interface
      type(c_ptr) function omp_target_alloc_c(byte_dimension, device_number) bind(C,name='omp_target_alloc_c')
         use iso_c_binding
         implicit none
         integer(kind=c_size_t), value :: byte_dimension
         integer(kind=c_int),    value :: device_number
      endfunction omp_target_alloc_c
      subroutine c_omp_free(a, dev_num) bind(c, name='omp_target_free')
         use iso_c_binding
         type(c_ptr),         value, intent(in) :: a
         integer(kind=c_int), value, intent(in) :: dev_num
      endsubroutine c_omp_free
      integer(c_int) function omp_target_wrapper(host, device, nvalues, dst_off, src_off) bind(c, name='omp_tgt_memcopy')
         use iso_c_binding
         type(c_ptr),         value :: host, device
         integer(kind=c_int), value :: nvalues, dst_off, src_off
      endfunction omp_target_wrapper
   endinterface

   integer, parameter :: R16P = selected_real_kind(33,4931)
   integer, parameter :: R8P  = selected_real_kind(15,307)
   integer, parameter :: R4P  = selected_real_kind(6,37)
   integer, parameter :: I8P = selected_int_kind(18)
   integer, parameter :: I4P = selected_int_kind(9)

   real(c_double),    parameter :: cd_dummy = 0
   integer(c_size_t), parameter :: size_of_double = storage_size(cd_dummy,c_size_t)/8_I4P

   integer, parameter :: i=1000_I4P
   real(kind=c_float), pointer :: fptr_dev(:)
   real(kind=c_float), target,  allocatable :: fptr_hos(:)
   type(c_ptr) :: cptr, cptr_dev
   integer(kind=c_int) :: ierr

   allocate(fptr_hos(i))
   cptr_dev = omp_target_alloc_c(i*size_of_double, omp_get_default_device())
   call c_f_pointer (cptr_dev, fptr_dev, [1])
   print *, associated(fptr_dev)
   call init_dev_pointer(fptr_dev, i)
   cptr = c_loc(fptr_hos)
   print *, 'start device copy'
   ierr = omp_target_wrapper(cptr, cptr_dev, i, 0_I4P, 0_I4P)
   print *, 'ierr ', ierr
   print *, 'fptr_hos ', fptr_hos(1), fptr_hos(i)
   call c_omp_free(cptr_dev, omp_get_default_device())

   contains
      subroutine init_dev_pointer(fptr_dev, i)
         real(kind=c_float), intent(inout) :: fptr_dev(:)
         integer,            intent(in)    :: i
         integer                           :: j

         !$omp target teams device(omp_get_default_device()) is_device_ptr(fptr_dev)
         !$omp distribute parallel do
         do j=1, i
            fptr_dev(j) = fptr_dev(j) + 1.0
         enddo
         !$omp end distribute parallel do
         !$omp end target teams

      endsubroutine init_dev_pointer

endmodule falco
