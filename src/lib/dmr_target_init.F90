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

submodule (dmr) dmr_target_init
   use omp_lib
   use dmr_environment

   implicit none

   contains

      ! DMR Init Integer Routines
      module subroutine omp_target_init_int8_1(array, val, omp_dev)
         implicit none
         integer(I1P), intent(inout) :: array(:)
         integer(I1P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds, ubounds
         integer(I8P)                :: i

         lbounds = lbound(array,1)
         ubounds = ubound(array,1)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do
         do i=lbounds, ubounds
            array(i) = val
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int8_1

      module subroutine omp_target_init_int8_2(array, val, omp_dev)
         implicit none
         integer(I1P), intent(inout) :: array(:,:)
         integer(I1P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(2), ubounds(2)
         integer(I8P)                :: i, j

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(2)
         do j=lbounds(2), ubounds(2)
            do i=lbounds(1), ubounds(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int8_2

      module subroutine omp_target_init_int8_3(array, val, omp_dev)
         implicit none
         integer(I1P), intent(inout) :: array(:,:,:)
         integer(I1P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(3), ubounds(3)
         integer(I8P)                :: i, j, k

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(3)
         do k=lbounds(3), ubounds(3)
            do j=lbounds(2), ubounds(2)
               do i=lbounds(1), ubounds(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int8_3

      module subroutine omp_target_init_int8_4(array, val, omp_dev)
         implicit none
         integer(I1P), intent(inout) :: array(:,:,:,:)
         integer(I1P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(4), ubounds(4)
         integer(I8P)                :: i, j, k, l

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(4)
         do l=lbounds(4), ubounds(4)
            do k=lbounds(3), ubounds(3)
               do j=lbounds(2), ubounds(2)
                  do i=lbounds(1), ubounds(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int8_4

      module subroutine omp_target_init_int8_5(array, val, omp_dev)
         implicit none
         integer(I1P), intent(inout) :: array(:,:,:,:,:)
         integer(I1P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(5), ubounds(5)
         integer(I8P)                :: i, j, k, l, m

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(5)
         do m=lbounds(5), ubounds(5)
            do l=lbounds(4), ubounds(4)
               do k=lbounds(3), ubounds(3)
                  do j=lbounds(2), ubounds(2)
                     do i=lbounds(1), ubounds(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int8_5

      module subroutine omp_target_init_int8_6(array, val, omp_dev)
         implicit none
         integer(I1P), intent(inout) :: array(:,:,:,:,:,:)
         integer(I1P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(6), ubounds(6)
         integer(I8P)                :: i, j, k, l, m, n

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(6)
         do n=lbounds(6), ubounds(6)
            do m=lbounds(5), ubounds(5)
               do l=lbounds(4), ubounds(4)
                  do k=lbounds(3), ubounds(3)
                     do j=lbounds(2), ubounds(2)
                        do i=lbounds(1), ubounds(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int8_6

      module subroutine omp_target_init_int8_7(array, val, omp_dev)
         implicit none
         integer(I1P), intent(inout) :: array(:,:,:,:,:,:,:)
         integer(I1P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(7), ubounds(7)
         integer(I8P)                :: i, j, k, l, m, n, o

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(7)
         do o=lbounds(7), ubounds(7)
            do n=lbounds(6), ubounds(6)
               do m=lbounds(5), ubounds(5)
                  do l=lbounds(4), ubounds(4)
                     do k=lbounds(3), ubounds(3)
                        do j=lbounds(2), ubounds(2)
                           do i=lbounds(1), ubounds(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int8_7

      module subroutine omp_target_init_int16_1(array, val, omp_dev)
         implicit none
         integer(I2P), intent(inout) :: array(:)
         integer(I2P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds, ubounds
         integer(I8P)                :: i

         lbounds = lbound(array,1)
         ubounds = ubound(array,1)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do
         do i=lbounds, ubounds
            array(i) = val
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int16_1

      module subroutine omp_target_init_int16_2(array, val, omp_dev)
         implicit none
         integer(I2P), intent(inout) :: array(:,:)
         integer(I2P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(2), ubounds(2)
         integer(I8P)                :: i, j

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(2)
         do j=lbounds(2), ubounds(2)
            do i=lbounds(1), ubounds(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int16_2

      module subroutine omp_target_init_int16_3(array, val, omp_dev)
         implicit none
         integer(I2P), intent(inout) :: array(:,:,:)
         integer(I2P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(3), ubounds(3)
         integer(I8P)                :: i, j, k

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(3)
         do k=lbounds(3), ubounds(3)
            do j=lbounds(2), ubounds(2)
               do i=lbounds(1), ubounds(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int16_3

      module subroutine omp_target_init_int16_4(array, val, omp_dev)
         implicit none
         integer(I2P), intent(inout) :: array(:,:,:,:)
         integer(I2P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(4), ubounds(4)
         integer(I8P)                :: i, j, k, l

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(4)
         do l=lbounds(4), ubounds(4)
            do k=lbounds(3), ubounds(3)
               do j=lbounds(2), ubounds(2)
                  do i=lbounds(1), ubounds(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int16_4

      module subroutine omp_target_init_int16_5(array, val, omp_dev)
         implicit none
         integer(I2P), intent(inout) :: array(:,:,:,:,:)
         integer(I2P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(5), ubounds(5)
         integer(I8P)                :: i, j, k, l, m

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(5)
         do m=lbounds(5), ubounds(5)
            do l=lbounds(4), ubounds(4)
               do k=lbounds(3), ubounds(3)
                  do j=lbounds(2), ubounds(2)
                     do i=lbounds(1), ubounds(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int16_5

      module subroutine omp_target_init_int16_6(array, val, omp_dev)
         implicit none
         integer(I2P), intent(inout) :: array(:,:,:,:,:,:)
         integer(I2P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(6), ubounds(6)
         integer(I8P)                :: i, j, k, l, m, n

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(6)
         do n=lbounds(6), ubounds(6)
            do m=lbounds(5), ubounds(5)
               do l=lbounds(4), ubounds(4)
                  do k=lbounds(3), ubounds(3)
                     do j=lbounds(2), ubounds(2)
                        do i=lbounds(1), ubounds(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int16_6

      module subroutine omp_target_init_int16_7(array, val, omp_dev)
         implicit none
         integer(I2P), intent(inout) :: array(:,:,:,:,:,:,:)
         integer(I2P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(7), ubounds(7)
         integer(I8P)                :: i, j, k, l, m, n, o

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(7)
         do o=lbounds(7), ubounds(7)
            do n=lbounds(6), ubounds(6)
               do m=lbounds(5), ubounds(5)
                  do l=lbounds(4), ubounds(4)
                     do k=lbounds(3), ubounds(3)
                        do j=lbounds(2), ubounds(2)
                           do i=lbounds(1), ubounds(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int16_7

      module subroutine omp_target_init_int32_1(array, val, omp_dev)
         implicit none
         integer(I4P), intent(inout) :: array(:)
         integer(I4P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds, ubounds
         integer(I8P)                :: i

         lbounds = lbound(array,1)
         ubounds = ubound(array,1)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do
         do i=lbounds, ubounds
            array(i) = val
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int32_1

      module subroutine omp_target_init_int32_2(array, val, omp_dev)
         implicit none
         integer(I4P), intent(inout) :: array(:,:)
         integer(I4P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(2), ubounds(2)
         integer(I8P)                :: i, j

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(2)
         do j=lbounds(2), ubounds(2)
            do i=lbounds(1), ubounds(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int32_2

      module subroutine omp_target_init_int32_3(array, val, omp_dev)
         implicit none
         integer(I4P), intent(inout) :: array(:,:,:)
         integer(I4P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(3), ubounds(3)
         integer(I8P)                :: i, j, k

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(3)
         do k=lbounds(3), ubounds(3)
            do j=lbounds(2), ubounds(2)
               do i=lbounds(1), ubounds(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int32_3

      module subroutine omp_target_init_int32_4(array, val, omp_dev)
         implicit none
         integer(I4P), intent(inout) :: array(:,:,:,:)
         integer(I4P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(4), ubounds(4)
         integer(I8P)                :: i, j, k, l

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(4)
         do l=lbounds(4), ubounds(4)
            do k=lbounds(3), ubounds(3)
               do j=lbounds(2), ubounds(2)
                  do i=lbounds(1), ubounds(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int32_4

      module subroutine omp_target_init_int32_5(array, val, omp_dev)
         implicit none
         integer(I4P), intent(inout) :: array(:,:,:,:,:)
         integer(I4P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(5), ubounds(5)
         integer(I8P)                :: i, j, k, l, m

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(5)
         do m=lbounds(5), ubounds(5)
            do l=lbounds(4), ubounds(4)
               do k=lbounds(3), ubounds(3)
                  do j=lbounds(2), ubounds(2)
                     do i=lbounds(1), ubounds(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int32_5

      module subroutine omp_target_init_int32_6(array, val, omp_dev)
         implicit none
         integer(I4P), intent(inout) :: array(:,:,:,:,:,:)
         integer(I4P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(6), ubounds(6)
         integer(I8P)                :: i, j, k, l, m, n

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(6)
         do n=lbounds(6), ubounds(6)
            do m=lbounds(5), ubounds(5)
               do l=lbounds(4), ubounds(4)
                  do k=lbounds(3), ubounds(3)
                     do j=lbounds(2), ubounds(2)
                        do i=lbounds(1), ubounds(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int32_6

      module subroutine omp_target_init_int32_7(array, val, omp_dev)
         implicit none
         integer(I4P), intent(inout) :: array(:,:,:,:,:,:,:)
         integer(I4P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(7), ubounds(7)
         integer(I8P)                :: i, j, k, l, m, n, o

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(7)
         do o=lbounds(7), ubounds(7)
            do n=lbounds(6), ubounds(6)
               do m=lbounds(5), ubounds(5)
                  do l=lbounds(4), ubounds(4)
                     do k=lbounds(3), ubounds(3)
                        do j=lbounds(2), ubounds(2)
                           do i=lbounds(1), ubounds(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int32_7

      module subroutine omp_target_init_int64_1(array, val, omp_dev)
         implicit none
         integer(I8P), intent(inout) :: array(:)
         integer(I8P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds, ubounds
         integer(I8P)                :: i

         lbounds = lbound(array,1)
         ubounds = ubound(array,1)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do
         do i=lbounds, ubounds
            array(i) = val
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int64_1

      module subroutine omp_target_init_int64_2(array, val, omp_dev)
         implicit none
         integer(I8P), intent(inout) :: array(:,:)
         integer(I8P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(2), ubounds(2)
         integer(I8P)                :: i, j

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(2)
         do j=lbounds(2), ubounds(2)
            do i=lbounds(1), ubounds(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int64_2

      module subroutine omp_target_init_int64_3(array, val, omp_dev)
         implicit none
         integer(I8P), intent(inout) :: array(:,:,:)
         integer(I8P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(3), ubounds(3)
         integer(I8P)                :: i, j, k

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(3)
         do k=lbounds(3), ubounds(3)
            do j=lbounds(2), ubounds(2)
               do i=lbounds(1), ubounds(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int64_3

      module subroutine omp_target_init_int64_4(array, val, omp_dev)
         implicit none
         integer(I8P), intent(inout) :: array(:,:,:,:)
         integer(I8P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(4), ubounds(4)
         integer(I8P)                :: i, j, k, l

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(4)
         do l=lbounds(4), ubounds(4)
            do k=lbounds(3), ubounds(3)
               do j=lbounds(2), ubounds(2)
                  do i=lbounds(1), ubounds(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int64_4

      module subroutine omp_target_init_int64_5(array, val, omp_dev)
         implicit none
         integer(I8P), intent(inout) :: array(:,:,:,:,:)
         integer(I8P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(5), ubounds(5)
         integer(I8P)                :: i, j, k, l, m

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(5)
         do m=lbounds(5), ubounds(5)
            do l=lbounds(4), ubounds(4)
               do k=lbounds(3), ubounds(3)
                  do j=lbounds(2), ubounds(2)
                     do i=lbounds(1), ubounds(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int64_5

      module subroutine omp_target_init_int64_6(array, val, omp_dev)
         implicit none
         integer(I8P), intent(inout) :: array(:,:,:,:,:,:)
         integer(I8P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(6), ubounds(6)
         integer(I8P)                :: i, j, k, l, m, n

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(6)
         do n=lbounds(6), ubounds(6)
            do m=lbounds(5), ubounds(5)
               do l=lbounds(4), ubounds(4)
                  do k=lbounds(3), ubounds(3)
                     do j=lbounds(2), ubounds(2)
                        do i=lbounds(1), ubounds(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int64_6

      module subroutine omp_target_init_int64_7(array, val, omp_dev)
         implicit none
         integer(I8P), intent(inout) :: array(:,:,:,:,:,:,:)
         integer(I8P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(7), ubounds(7)
         integer(I8P)                :: i, j, k, l, m, n, o

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(7)
         do o=lbounds(7), ubounds(7)
            do n=lbounds(6), ubounds(6)
               do m=lbounds(5), ubounds(5)
                  do l=lbounds(4), ubounds(4)
                     do k=lbounds(3), ubounds(3)
                        do j=lbounds(2), ubounds(2)
                           do i=lbounds(1), ubounds(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_int64_7

      ! DMR Init Real Routines
      module subroutine omp_target_init_real32_1(array, val, omp_dev)
         implicit none
         real(R4P),    intent(inout) :: array(:)
         real(R4P),    intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds, ubounds
         integer(I8P)                :: i

         lbounds = lbound(array,1)
         ubounds = ubound(array,1)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do
         do i=lbounds, ubounds
            array(i) = val
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real32_1

      module subroutine omp_target_init_real32_2(array, val, omp_dev)
         implicit none
         real(R4P),    intent(inout) :: array(:,:)
         real(R4P),    intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(2), ubounds(2)
         integer(I8P)                :: i, j

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(2)
         do j=lbounds(2), ubounds(2)
            do i=lbounds(1), ubounds(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real32_2

      module subroutine omp_target_init_real32_3(array, val, omp_dev)
         implicit none
         real(R4P),    intent(inout) :: array(:,:,:)
         real(R4P),    intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(3), ubounds(3)
         integer(I8P)                :: i, j, k

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(3)
         do k=lbounds(3), ubounds(3)
            do j=lbounds(2), ubounds(2)
               do i=lbounds(1), ubounds(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real32_3

      module subroutine omp_target_init_real32_4(array, val, omp_dev)
         implicit none
         real(R4P),    intent(inout) :: array(:,:,:,:)
         real(R4P),    intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(4), ubounds(4)
         integer(I8P)                :: i, j, k, l

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(4)
         do l=lbounds(4), ubounds(4)
            do k=lbounds(3), ubounds(3)
               do j=lbounds(2), ubounds(2)
                  do i=lbounds(1), ubounds(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real32_4

      module subroutine omp_target_init_real32_5(array, val, omp_dev)
         implicit none
         real(R4P),    intent(inout) :: array(:,:,:,:,:)
         real(R4P),    intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(5), ubounds(5)
         integer(I8P)                :: i, j, k, l, m

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(5)
         do m=lbounds(5), ubounds(5)
            do l=lbounds(4), ubounds(4)
               do k=lbounds(3), ubounds(3)
                  do j=lbounds(2), ubounds(2)
                     do i=lbounds(1), ubounds(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real32_5

      module subroutine omp_target_init_real32_6(array, val, omp_dev)
         implicit none
         real(R4P),    intent(inout) :: array(:,:,:,:,:,:)
         real(R4P),    intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(6), ubounds(6)
         integer(I8P)                :: i, j, k, l, m, n

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(6)
         do n=lbounds(6), ubounds(6)
            do m=lbounds(5), ubounds(5)
               do l=lbounds(4), ubounds(4)
                  do k=lbounds(3), ubounds(3)
                     do j=lbounds(2), ubounds(2)
                        do i=lbounds(1), ubounds(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real32_6

      module subroutine omp_target_init_real32_7(array, val, omp_dev)
         implicit none
         real(R4P),    intent(inout) :: array(:,:,:,:,:,:,:)
         real(R4P),    intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(7), ubounds(7)
         integer(I8P)                :: i, j, k, l, m, n, o

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(7)
         do o=lbounds(7), ubounds(7)
            do n=lbounds(6), ubounds(6)
               do m=lbounds(5), ubounds(5)
                  do l=lbounds(4), ubounds(4)
                     do k=lbounds(3), ubounds(3)
                        do j=lbounds(2), ubounds(2)
                           do i=lbounds(1), ubounds(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real32_7

      module subroutine omp_target_init_real64_1(array, val, omp_dev)
         implicit none
         real(R8P),    intent(inout) :: array(:)
         real(R8P),    intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds, ubounds
         integer(I8P)                :: i

         lbounds = lbound(array,1)
         ubounds = ubound(array,1)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do
         do i=lbounds, ubounds
            array(i) = val
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real64_1

      module subroutine omp_target_init_real64_2(array, val, omp_dev)
         implicit none
         real(R8P),    intent(inout) :: array(:,:)
         real(R8P),    intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(2), ubounds(2)
         integer(I8P)                :: i, j

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(2)
         do j=lbounds(2), ubounds(2)
            do i=lbounds(1), ubounds(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real64_2

      module subroutine omp_target_init_real64_3(array, val, omp_dev)
         implicit none
         real(R8P),    intent(inout) :: array(:,:,:)
         real(R8P),    intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(3), ubounds(3)
         integer(I8P)                :: i, j, k

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(3)
         do k=lbounds(3), ubounds(3)
            do j=lbounds(2), ubounds(2)
               do i=lbounds(1), ubounds(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real64_3

      module subroutine omp_target_init_real64_4(array, val, omp_dev)
         implicit none
         real(R8P),    intent(inout) :: array(:,:,:,:)
         real(R8P),    intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(4), ubounds(4)
         integer(I8P)                :: i, j, k, l

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(4)
         do l=lbounds(4), ubounds(4)
            do k=lbounds(3), ubounds(3)
               do j=lbounds(2), ubounds(2)
                  do i=lbounds(1), ubounds(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real64_4

      module subroutine omp_target_init_real64_5(array, val, omp_dev)
         implicit none
         real(R8P),    intent(inout) :: array(:,:,:,:,:)
         real(R8P),    intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(5), ubounds(5)
         integer(I8P)                :: i, j, k, l, m

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(5)
         do m=lbounds(5), ubounds(5)
            do l=lbounds(4), ubounds(4)
               do k=lbounds(3), ubounds(3)
                  do j=lbounds(2), ubounds(2)
                     do i=lbounds(1), ubounds(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real64_5

      module subroutine omp_target_init_real64_6(array, val, omp_dev)
         implicit none
         real(R8P),    intent(inout) :: array(:,:,:,:,:,:)
         real(R8P),    intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(6), ubounds(6)
         integer(I8P)                :: i, j, k, l, m, n

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(6)
         do n=lbounds(6), ubounds(6)
            do m=lbounds(5), ubounds(5)
               do l=lbounds(4), ubounds(4)
                  do k=lbounds(3), ubounds(3)
                     do j=lbounds(2), ubounds(2)
                        do i=lbounds(1), ubounds(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real64_6

      module subroutine omp_target_init_real64_7(array, val, omp_dev)
         implicit none
         real(R8P),    intent(inout) :: array(:,:,:,:,:,:,:)
         real(R8P),    intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(7), ubounds(7)
         integer(I8P)                :: i, j, k, l, m, n, o

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(7)
         do o=lbounds(7), ubounds(7)
            do n=lbounds(6), ubounds(6)
               do m=lbounds(5), ubounds(5)
                  do l=lbounds(4), ubounds(4)
                     do k=lbounds(3), ubounds(3)
                        do j=lbounds(2), ubounds(2)
                           do i=lbounds(1), ubounds(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real64_7

#if defined _real128
      module subroutine omp_target_init_real128_1(array, val, omp_dev)
         implicit none
         real(R16P),   intent(inout) :: array(:)
         real(R16P),   intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds, ubounds
         integer(I8P)                :: i

         lbounds = lbound(array,1)
         ubounds = ubound(array,1)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do
         do i=lbounds, ubounds
            array(i) = val
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real128_1

      module subroutine omp_target_init_real128_2(array, val, omp_dev)
         implicit none
         real(R16P),   intent(inout) :: array(:,:)
         real(R16P),   intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(2), ubounds(2)
         integer(I8P)                :: i, j

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(2)
         do j=lbounds(2), ubounds(2)
            do i=lbounds(1), ubounds(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real128_2

      module subroutine omp_target_init_real128_3(array, val, omp_dev)
         implicit none
         real(R16P),   intent(inout) :: array(:,:,:)
         real(R16P),   intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(3), ubounds(3)
         integer(I8P)                :: i, j, k

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(3)
         do k=lbounds(3), ubounds(3)
            do j=lbounds(2), ubounds(2)
               do i=lbounds(1), ubounds(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real128_3

      module subroutine omp_target_init_real128_4(array, val, omp_dev)
         implicit none
         real(R16P),   intent(inout) :: array(:,:,:,:)
         real(R16P),   intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(4), ubounds(4)
         integer(I8P)                :: i, j, k, l

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(4)
         do l=lbounds(4), ubounds(4)
            do k=lbounds(3), ubounds(3)
               do j=lbounds(2), ubounds(2)
                  do i=lbounds(1), ubounds(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real128_4

      module subroutine omp_target_init_real128_5(array, val, omp_dev)
         implicit none
         real(R16P),   intent(inout) :: array(:,:,:,:,:)
         real(R16P),   intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(5), ubounds(5)
         integer(I8P)                :: i, j, k, l, m

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(5)
         do m=lbounds(5), ubounds(5)
            do l=lbounds(4), ubounds(4)
               do k=lbounds(3), ubounds(3)
                  do j=lbounds(2), ubounds(2)
                     do i=lbounds(1), ubounds(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real128_5

      module subroutine omp_target_init_real128_6(array, val, omp_dev)
         implicit none
         real(R16P),   intent(inout) :: array(:,:,:,:,:,:)
         real(R16P),   intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(6), ubounds(6)
         integer(I8P)                :: i, j, k, l, m, n

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(6)
         do n=lbounds(6), ubounds(6)
            do m=lbounds(5), ubounds(5)
               do l=lbounds(4), ubounds(4)
                  do k=lbounds(3), ubounds(3)
                     do j=lbounds(2), ubounds(2)
                        do i=lbounds(1), ubounds(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real128_6

      module subroutine omp_target_init_real128_7(array, val, omp_dev)
         implicit none
         real(R16P),   intent(inout) :: array(:,:,:,:,:,:,:)
         real(R16P),   intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(7), ubounds(7)
         integer(I8P)                :: i, j, k, l, m, n, o

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(7)
         do o=lbounds(7), ubounds(7)
            do n=lbounds(6), ubounds(6)
               do m=lbounds(5), ubounds(5)
                  do l=lbounds(4), ubounds(4)
                     do k=lbounds(3), ubounds(3)
                        do j=lbounds(2), ubounds(2)
                           do i=lbounds(1), ubounds(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_real128_7
#endif

      ! DMR Init Complex Routines
      module subroutine omp_target_init_cmplx32_1(array, val, omp_dev)
         implicit none
         complex(R4P), intent(inout) :: array(:)
         complex(R4P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds, ubounds
         integer(I8P)                :: i

         lbounds = lbound(array,1)
         ubounds = ubound(array,1)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do
         do i=lbounds, ubounds
            array(i) = val
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx32_1

      module subroutine omp_target_init_cmplx32_2(array, val, omp_dev)
         implicit none
         complex(R4P), intent(inout) :: array(:,:)
         complex(R4P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(2), ubounds(2)
         integer(I8P)                :: i, j

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(2)
         do j=lbounds(2), ubounds(2)
            do i=lbounds(1), ubounds(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx32_2

      module subroutine omp_target_init_cmplx32_3(array, val, omp_dev)
         implicit none
         complex(R4P), intent(inout) :: array(:,:,:)
         complex(R4P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(3), ubounds(3)
         integer(I8P)                :: i, j, k

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(3)
         do k=lbounds(3), ubounds(3)
            do j=lbounds(2), ubounds(2)
               do i=lbounds(1), ubounds(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx32_3

      module subroutine omp_target_init_cmplx32_4(array, val, omp_dev)
         implicit none
         complex(R4P), intent(inout) :: array(:,:,:,:)
         complex(R4P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(4), ubounds(4)
         integer(I8P)                :: i, j, k, l

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(4)
         do l=lbounds(4), ubounds(4)
            do k=lbounds(3), ubounds(3)
               do j=lbounds(2), ubounds(2)
                  do i=lbounds(1), ubounds(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx32_4

      module subroutine omp_target_init_cmplx32_5(array, val, omp_dev)
         implicit none
         complex(R4P), intent(inout) :: array(:,:,:,:,:)
         complex(R4P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(5), ubounds(5)
         integer(I8P)                :: i, j, k, l, m

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(5)
         do m=lbounds(5), ubounds(5)
            do l=lbounds(4), ubounds(4)
               do k=lbounds(3), ubounds(3)
                  do j=lbounds(2), ubounds(2)
                     do i=lbounds(1), ubounds(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx32_5

      module subroutine omp_target_init_cmplx32_6(array, val, omp_dev)
         implicit none
         complex(R4P), intent(inout) :: array(:,:,:,:,:,:)
         complex(R4P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(6), ubounds(6)
         integer(I8P)                :: i, j, k, l, m, n

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(6)
         do n=lbounds(6), ubounds(6)
            do m=lbounds(5), ubounds(5)
               do l=lbounds(4), ubounds(4)
                  do k=lbounds(3), ubounds(3)
                     do j=lbounds(2), ubounds(2)
                        do i=lbounds(1), ubounds(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx32_6

      module subroutine omp_target_init_cmplx32_7(array, val, omp_dev)
         implicit none
         complex(R4P), intent(inout) :: array(:,:,:,:,:,:,:)
         complex(R4P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(7), ubounds(7)
         integer(I8P)                :: i, j, k, l, m, n, o

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(7)
         do o=lbounds(7), ubounds(7)
            do n=lbounds(6), ubounds(6)
               do m=lbounds(5), ubounds(5)
                  do l=lbounds(4), ubounds(4)
                     do k=lbounds(3), ubounds(3)
                        do j=lbounds(2), ubounds(2)
                           do i=lbounds(1), ubounds(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx32_7

      module subroutine omp_target_init_cmplx64_1(array, val, omp_dev)
         implicit none
         complex(R8P), intent(inout) :: array(:)
         complex(R8P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds, ubounds
         integer(I8P)                :: i

         lbounds = lbound(array,1)
         ubounds = ubound(array,1)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do
         do i=lbounds, ubounds
            array(i) = val
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx64_1

      module subroutine omp_target_init_cmplx64_2(array, val, omp_dev)
         implicit none
         complex(R8P), intent(inout) :: array(:,:)
         complex(R8P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(2), ubounds(2)
         integer(I8P)                :: i, j

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(2)
         do j=lbounds(2), ubounds(2)
            do i=lbounds(1), ubounds(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx64_2

      module subroutine omp_target_init_cmplx64_3(array, val, omp_dev)
         implicit none
         complex(R8P), intent(inout) :: array(:,:,:)
         complex(R8P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(3), ubounds(3)
         integer(I8P)                :: i, j, k

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(3)
         do k=lbounds(3), ubounds(3)
            do j=lbounds(2), ubounds(2)
               do i=lbounds(1), ubounds(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx64_3

      module subroutine omp_target_init_cmplx64_4(array, val, omp_dev)
         implicit none
         complex(R8P), intent(inout) :: array(:,:,:,:)
         complex(R8P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(4), ubounds(4)
         integer(I8P)                :: i, j, k, l

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(4)
         do l=lbounds(4), ubounds(4)
            do k=lbounds(3), ubounds(3)
               do j=lbounds(2), ubounds(2)
                  do i=lbounds(1), ubounds(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx64_4

      module subroutine omp_target_init_cmplx64_5(array, val, omp_dev)
         implicit none
         complex(R8P), intent(inout) :: array(:,:,:,:,:)
         complex(R8P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(5), ubounds(5)
         integer(I8P)                :: i, j, k, l, m

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(5)
         do m=lbounds(5), ubounds(5)
            do l=lbounds(4), ubounds(4)
               do k=lbounds(3), ubounds(3)
                  do j=lbounds(2), ubounds(2)
                     do i=lbounds(1), ubounds(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx64_5

      module subroutine omp_target_init_cmplx64_6(array, val, omp_dev)
         implicit none
         complex(R8P), intent(inout) :: array(:,:,:,:,:,:)
         complex(R8P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(6), ubounds(6)
         integer(I8P)                :: i, j, k, l, m, n

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(6)
         do n=lbounds(6), ubounds(6)
            do m=lbounds(5), ubounds(5)
               do l=lbounds(4), ubounds(4)
                  do k=lbounds(3), ubounds(3)
                     do j=lbounds(2), ubounds(2)
                        do i=lbounds(1), ubounds(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx64_6

      module subroutine omp_target_init_cmplx64_7(array, val, omp_dev)
         implicit none
         complex(R8P), intent(inout) :: array(:,:,:,:,:,:,:)
         complex(R8P), intent(in)    :: val
         integer(I4P), intent(in)    :: omp_dev
         integer(I8P)                :: lbounds(7), ubounds(7)
         integer(I8P)                :: i, j, k, l, m, n, o

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(7)
         do o=lbounds(7), ubounds(7)
            do n=lbounds(6), ubounds(6)
               do m=lbounds(5), ubounds(5)
                  do l=lbounds(4), ubounds(4)
                     do k=lbounds(3), ubounds(3)
                        do j=lbounds(2), ubounds(2)
                           do i=lbounds(1), ubounds(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx64_7

#if defined _real128
      module subroutine omp_target_init_cmplx128_1(array, val, omp_dev)
         implicit none
         complex(R16P), intent(inout) :: array(:)
         complex(R16P), intent(in)    :: val
         integer(I4P),  intent(in)    :: omp_dev
         integer(I8P)                 :: lbounds, ubounds
         integer(I8P)                 :: i

         lbounds = lbound(array,1)
         ubounds = ubound(array,1)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do
         do i=lbounds, ubounds
            array(i) = val
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx128_1

      module subroutine omp_target_init_cmplx128_2(array, val, omp_dev)
         implicit none
         complex(R16P), intent(inout) :: array(:,:)
         complex(R16P), intent(in)    :: val
         integer(I4P),  intent(in)    :: omp_dev
         integer(I8P)                 :: lbounds(2), ubounds(2)
         integer(I8P)                 :: i, j

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(2)
         do j=lbounds(2), ubounds(2)
            do i=lbounds(1), ubounds(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx128_2

      module subroutine omp_target_init_cmplx128_3(array, val, omp_dev)
         implicit none
         complex(R16P), intent(inout) :: array(:,:,:)
         complex(R16P), intent(in)    :: val
         integer(I4P),  intent(in)    :: omp_dev
         integer(I8P)                 :: lbounds(3), ubounds(3)
         integer(I8P)                 :: i, j, k

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(3)
         do k=lbounds(3), ubounds(3)
            do j=lbounds(2), ubounds(2)
               do i=lbounds(1), ubounds(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx128_3

      module subroutine omp_target_init_cmplx128_4(array, val, omp_dev)
         implicit none
         complex(R16P), intent(inout) :: array(:,:,:,:)
         complex(R16P), intent(in)    :: val
         integer(I4P),  intent(in)    :: omp_dev
         integer(I8P)                 :: lbounds(4), ubounds(4)
         integer(I8P)                 :: i, j, k, l

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(4)
         do l=lbounds(4), ubounds(4)
            do k=lbounds(3), ubounds(3)
               do j=lbounds(2), ubounds(2)
                  do i=lbounds(1), ubounds(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx128_4

      module subroutine omp_target_init_cmplx128_5(array, val, omp_dev)
         implicit none
         complex(R16P), intent(inout) :: array(:,:,:,:,:)
         complex(R16P), intent(in)    :: val
         integer(I4P),  intent(in)    :: omp_dev
         integer(I8P)                 :: lbounds(5), ubounds(5)
         integer(I8P)                 :: i, j, k, l, m

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(5)
         do m=lbounds(5), ubounds(5)
            do l=lbounds(4), ubounds(4)
               do k=lbounds(3), ubounds(3)
                  do j=lbounds(2), ubounds(2)
                     do i=lbounds(1), ubounds(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx128_5

      module subroutine omp_target_init_cmplx128_6(array, val, omp_dev)
         implicit none
         complex(R16P), intent(inout) :: array(:,:,:,:,:,:)
         complex(R16P), intent(in)    :: val
         integer(I4P),  intent(in)    :: omp_dev
         integer(I8P)                 :: lbounds(6), ubounds(6)
         integer(I8P)                 :: i, j, k, l, m, n

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(6)
         do n=lbounds(6), ubounds(6)
            do m=lbounds(5), ubounds(5)
               do l=lbounds(4), ubounds(4)
                  do k=lbounds(3), ubounds(3)
                     do j=lbounds(2), ubounds(2)
                        do i=lbounds(1), ubounds(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx128_6

      module subroutine omp_target_init_cmplx128_7(array, val, omp_dev)
         implicit none
         complex(R16P), intent(inout) :: array(:,:,:,:,:,:,:)
         complex(R16P), intent(in)    :: val
         integer(I4P),  intent(in)    :: omp_dev
         integer(I8P)                 :: lbounds(7), ubounds(7)
         integer(I8P)                 :: i, j, k, l, m, n, o

         lbounds = lbound(array)
         ubounds = ubound(array)
#if defined _OpenMP_5_1
         !$omp target device(omp_dev) has_device_addr(array)
#else
         !$omp target device(omp_dev) is_device_ptr(array)
#endif
         !$omp teams distribute parallel do collapse(7)
         do o=lbounds(7), ubounds(7)
            do n=lbounds(6), ubounds(6)
               do m=lbounds(5), ubounds(5)
                  do l=lbounds(4), ubounds(4)
                     do k=lbounds(3), ubounds(3)
                        do j=lbounds(2), ubounds(2)
                           do i=lbounds(1), ubounds(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end teams distribute parallel do
         !$omp end target
      endsubroutine omp_target_init_cmplx128_7
#endif

endsubmodule dmr_target_init
