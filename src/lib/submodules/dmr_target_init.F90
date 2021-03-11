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
#if defined _F2008
      module subroutine omp_target_init_int8(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         integer(I1P), intent(inout)        :: array(..)
         integer(I1P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i, j, k, l, m, n, o
         integer(I4P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif

         if (present(lbound_s)) then
            allocate(lbounds_(1))
            lbounds_(1) = lbound_s
         elseif (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = lbound(array)
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array)
         endif

         select rank(array)
         rank(1)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do i=lbounds_(1), ubounds_(1)
               array(i) = val
            enddo
            !$omp end target teams distribute parallel do
         rank(2)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j) = val
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(3)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k) = val
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(4)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l) = val
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(5)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(6)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(7)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do o=lbounds_(7), ubounds_(7)
               do n=lbounds_(6), ubounds_(6)
                  do m=lbounds_(5), ubounds_(5)
                     do l=lbounds_(4), ubounds_(4)
                        do k=lbounds_(3), ubounds_(3)
                           do j=lbounds_(2), ubounds_(2)
                              do i=lbounds_(1), ubounds_(1)
                                 array(i,j,k,l,m,n,o) = val
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         endselect
      endsubroutine omp_target_init_int8

      module subroutine omp_target_init_int16(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         integer(I2P), intent(inout)        :: array(..)
         integer(I2P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i, j, k, l, m, n, o
         integer(I4P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif

         if (present(lbound_s)) then
            allocate(lbounds_(1))
            lbounds_(1) = lbound_s
         elseif (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = lbound(array)
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array)
         endif

         select rank(array)
         rank(1)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do i=lbounds_(1), ubounds_(1)
               array(i) = val
            enddo
            !$omp end target teams distribute parallel do
         rank(2)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j) = val
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(3)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k) = val
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(4)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l) = val
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(5)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(6)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(7)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do o=lbounds_(7), ubounds_(7)
               do n=lbounds_(6), ubounds_(6)
                  do m=lbounds_(5), ubounds_(5)
                     do l=lbounds_(4), ubounds_(4)
                        do k=lbounds_(3), ubounds_(3)
                           do j=lbounds_(2), ubounds_(2)
                              do i=lbounds_(1), ubounds_(1)
                                 array(i,j,k,l,m,n,o) = val
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         endselect
      endsubroutine omp_target_init_int16

      module subroutine omp_target_init_int32(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         integer(I4P), intent(inout)        :: array(..)
         integer(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i, j, k, l, m, n, o
         integer(I4P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif

         if (present(lbound_s)) then
            allocate(lbounds_(1))
            lbounds_(1) = lbound_s
         elseif (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = lbound(array)
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array)
         endif

         select rank(array)
         rank(1)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do i=lbounds_(1), ubounds_(1)
               array(i) = val
            enddo
            !$omp end target teams distribute parallel do
         rank(2)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j) = val
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(3)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k) = val
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(4)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l) = val
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(5)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(6)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(7)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do o=lbounds_(7), ubounds_(7)
               do n=lbounds_(6), ubounds_(6)
                  do m=lbounds_(5), ubounds_(5)
                     do l=lbounds_(4), ubounds_(4)
                        do k=lbounds_(3), ubounds_(3)
                           do j=lbounds_(2), ubounds_(2)
                              do i=lbounds_(1), ubounds_(1)
                                 array(i,j,k,l,m,n,o) = val
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         endselect
      endsubroutine omp_target_init_int32

      module subroutine omp_target_init_int64(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         integer(I8P), intent(inout)        :: array(..)
         integer(I8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i, j, k, l, m, n, o
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif

         if (present(lbound_s)) then
            allocate(lbounds_(1))
            lbounds_(1) = lbound_s
         elseif (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = lbound(array)
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array)
         endif

         select rank(array)
         rank(1)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do i=lbounds_(1), ubounds_(1)
               array(i) = val
            enddo
            !$omp end target teams distribute parallel do
         rank(2)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j) = val
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(3)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k) = val
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(4)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l) = val
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(5)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(6)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(7)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do o=lbounds_(7), ubounds_(7)
               do n=lbounds_(6), ubounds_(6)
                  do m=lbounds_(5), ubounds_(5)
                     do l=lbounds_(4), ubounds_(4)
                        do k=lbounds_(3), ubounds_(3)
                           do j=lbounds_(2), ubounds_(2)
                              do i=lbounds_(1), ubounds_(1)
                                 array(i,j,k,l,m,n,o) = val
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         endselect
      endsubroutine omp_target_init_int64
#else
      module subroutine omp_target_init_int8_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(inout)        :: array(:)
         integer(I1P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do i=lbounds_, ubounds_
            array(i) = val
         enddo
      endsubroutine omp_target_init_int8_1

      module subroutine omp_target_init_int8_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(inout)        :: array(:,:)
         integer(I1P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i, j
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do j=lbounds_(2), ubounds_(2)
            do i=lbounds_(1), ubounds_(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int8_2

      module subroutine omp_target_init_int8_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(inout)        :: array(:,:,:)
         integer(I1P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i, j, k
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do k=lbounds_(3), ubounds_(3)
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int8_3

      module subroutine omp_target_init_int8_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(inout)        :: array(:,:,:,:)
         integer(I1P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i, j, k, l
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do l=lbounds_(4), ubounds_(4)
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int8_4

      module subroutine omp_target_init_int8_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(inout)        :: array(:,:,:,:,:)
         integer(I1P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i, j, k, l, m
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do m=lbounds_(5), ubounds_(5)
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int8_5

      module subroutine omp_target_init_int8_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(inout)        :: array(:,:,:,:,:,:)
         integer(I1P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i, j, k, l, m, n
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do n=lbounds_(6), ubounds_(6)
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int8_6

      module subroutine omp_target_init_int8_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(inout)        :: array(:,:,:,:,:,:,:)
         integer(I1P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i, j, k, l, m, n, o
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do o=lbounds_(7), ubounds_(7)
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int8_7

      module subroutine omp_target_init_int16_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(inout)        :: array(:)
         integer(I2P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do i=lbounds_, ubounds_
            array(i) = val
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int16_1

      module subroutine omp_target_init_int16_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(inout)        :: array(:,:)
         integer(I2P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i, j
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do j=lbounds_(2), ubounds_(2)
            do i=lbounds_(1), ubounds_(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int16_2

      module subroutine omp_target_init_int16_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(inout)        :: array(:,:,:)
         integer(I2P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i, j, k
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do k=lbounds_(3), ubounds_(3)
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int16_3

      module subroutine omp_target_init_int16_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(inout)        :: array(:,:,:,:)
         integer(I2P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i, j, k, l
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do l=lbounds_(4), ubounds_(4)
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int16_4

      module subroutine omp_target_init_int16_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(inout)        :: array(:,:,:,:,:)
         integer(I2P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i, j, k, l, m
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do m=lbounds_(5), ubounds_(5)
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int16_5

      module subroutine omp_target_init_int16_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(inout)        :: array(:,:,:,:,:,:)
         integer(I2P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i, j, k, l, m, n
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do n=lbounds_(6), ubounds_(6)
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int16_6

      module subroutine omp_target_init_int16_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(inout)        :: array(:,:,:,:,:,:,:)
         integer(I2P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i, j, k, l, m, n, o
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do o=lbounds_(7), ubounds_(7)
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int16_7

      module subroutine omp_target_init_int32_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(inout)        :: array(:)
         integer(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do i=lbounds_, ubounds_
            array(i) = val
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int32_1

      module subroutine omp_target_init_int32_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(inout)        :: array(:,:)
         integer(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i, j
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do j=lbounds_(2), ubounds_(2)
            do i=lbounds_(1), ubounds_(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int32_2

      module subroutine omp_target_init_int32_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(inout)        :: array(:,:,:)
         integer(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i, j, k
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do k=lbounds_(3), ubounds_(3)
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int32_3

      module subroutine omp_target_init_int32_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(inout)        :: array(:,:,:,:)
         integer(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i, j, k, l
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do l=lbounds_(4), ubounds_(4)
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int32_4

      module subroutine omp_target_init_int32_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(inout)        :: array(:,:,:,:,:)
         integer(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i, j, k, l, m
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do m=lbounds_(5), ubounds_(5)
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int32_5

      module subroutine omp_target_init_int32_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(inout)        :: array(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i, j, k, l, m, n
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do n=lbounds_(6), ubounds_(6)
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int32_6

      module subroutine omp_target_init_int32_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(inout)        :: array(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i, j, k, l, m, n, o
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do o=lbounds_(7), ubounds_(7)
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int32_7

      module subroutine omp_target_init_int64_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(inout)        :: array(:)
         integer(I8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do i=lbounds_, ubounds_
            array(i) = val
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int64_1

      module subroutine omp_target_init_int64_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(inout)        :: array(:,:)
         integer(I8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i, j
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do j=lbounds_(2), ubounds_(2)
            do i=lbounds_(1), ubounds_(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int64_2

      module subroutine omp_target_init_int64_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(inout)        :: array(:,:,:)
         integer(I8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i, j, k
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do k=lbounds_(3), ubounds_(3)
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int64_3

      module subroutine omp_target_init_int64_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(inout)        :: array(:,:,:,:)
         integer(I8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i, j, k, l
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do l=lbounds_(4), ubounds_(4)
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int64_4

      module subroutine omp_target_init_int64_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(inout)        :: array(:,:,:,:,:)
         integer(I8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i, j, k, l, m
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do m=lbounds_(5), ubounds_(5)
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int64_5

      module subroutine omp_target_init_int64_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(inout)        :: array(:,:,:,:,:,:)
         integer(I8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i, j, k, l, m, n
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do n=lbounds_(6), ubounds_(6)
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int64_6

      module subroutine omp_target_init_int64_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(inout)        :: array(:,:,:,:,:,:,:)
         integer(I8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i, j, k, l, m, n, o
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do o=lbounds_(7), ubounds_(7)
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_int64_7
#endif

      ! DMR Init Real Routines
#if defined _F2008
      module subroutine omp_target_init_real32(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         real(R4P),    intent(inout)        :: array(..)
         real(R4P),    intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i, j, k, l, m, n, o
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif

         if (present(lbound_s)) then
            allocate(lbounds_(1))
            lbounds_(1) = lbound_s
         elseif (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = lbound(array)
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array)
         endif

         select rank(array)
         rank(1)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do i=lbounds_(1), ubounds_(1)
               array(i) = val
            enddo
            !$omp end target teams distribute parallel do
         rank(2)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j) = val
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(3)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k) = val
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(4)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l) = val
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(5)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(6)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(7)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do o=lbounds_(7), ubounds_(7)
               do n=lbounds_(6), ubounds_(6)
                  do m=lbounds_(5), ubounds_(5)
                     do l=lbounds_(4), ubounds_(4)
                        do k=lbounds_(3), ubounds_(3)
                           do j=lbounds_(2), ubounds_(2)
                              do i=lbounds_(1), ubounds_(1)
                                 array(i,j,k,l,m,n,o) = val
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         endselect
      endsubroutine omp_target_init_real32

      module subroutine omp_target_init_real64(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         real(R8P),    intent(inout)        :: array(..)
         real(R8P),    intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i, j, k, l, m, n, o
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif

         if (present(lbound_s)) then
            allocate(lbounds_(1))
            lbounds_(1) = lbound_s
         elseif (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = lbound(array)
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array)
         endif

         select rank(array)
         rank(1)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do i=lbounds_(1), ubounds_(1)
               array(i) = val
            enddo
            !$omp end target teams distribute parallel do
         rank(2)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j) = val
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(3)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k) = val
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(4)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l) = val
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(5)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(6)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(7)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do o=lbounds_(7), ubounds_(7)
               do n=lbounds_(6), ubounds_(6)
                  do m=lbounds_(5), ubounds_(5)
                     do l=lbounds_(4), ubounds_(4)
                        do k=lbounds_(3), ubounds_(3)
                           do j=lbounds_(2), ubounds_(2)
                              do i=lbounds_(1), ubounds_(1)
                                 array(i,j,k,l,m,n,o) = val
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         endselect
      endsubroutine omp_target_init_real64

#if defined _real128
      module subroutine omp_target_init_real128(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         real(R16P),   intent(inout)        :: array(..)
         real(R16P),   intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i, j, k, l, m, n, o
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif

         if (present(lbound_s)) then
            allocate(lbounds_(1))
            lbounds_(1) = lbound_s
         elseif (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = lbound(array)
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array)
         endif

         select rank(array)
         rank(1)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do i=lbounds_(1), ubounds_(1)
               array(i) = val
            enddo
            !$omp end target teams distribute parallel do
         rank(2)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j) = val
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(3)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k) = val
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(4)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l) = val
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(5)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(6)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(7)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do o=lbounds_(7), ubounds_(7)
               do n=lbounds_(6), ubounds_(6)
                  do m=lbounds_(5), ubounds_(5)
                     do l=lbounds_(4), ubounds_(4)
                        do k=lbounds_(3), ubounds_(3)
                           do j=lbounds_(2), ubounds_(2)
                              do i=lbounds_(1), ubounds_(1)
                                 array(i,j,k,l,m,n,o) = val
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         endselect
      endsubroutine omp_target_init_real128
#endif
#else
      module subroutine omp_target_init_real32_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P),    intent(inout)        :: array(:)
         real(R4P),    intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do i=lbounds_, ubounds_
            array(i) = val
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real32_1

      module subroutine omp_target_init_real32_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P),    intent(inout)        :: array(:,:)
         real(R4P),    intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i, j
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do j=lbounds_(2), ubounds_(2)
            do i=lbounds_(1), ubounds_(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real32_2

      module subroutine omp_target_init_real32_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P),    intent(inout)        :: array(:,:,:)
         real(R4P),    intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i, j, k
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do k=lbounds_(3), ubounds_(3)
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real32_3

      module subroutine omp_target_init_real32_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P),    intent(inout)        :: array(:,:,:,:)
         real(R4P),    intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i, j, k, l
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do l=lbounds_(4), ubounds_(4)
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real32_4

      module subroutine omp_target_init_real32_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P),    intent(inout)        :: array(:,:,:,:,:)
         real(R4P),    intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i, j, k, l, m
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do m=lbounds_(5), ubounds_(5)
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real32_5

      module subroutine omp_target_init_real32_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P),    intent(inout)        :: array(:,:,:,:,:,:)
         real(R4P),    intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i, j, k, l, m, n
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do n=lbounds_(6), ubounds_(6)
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real32_6

      module subroutine omp_target_init_real32_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P),    intent(inout)        :: array(:,:,:,:,:,:,:)
         real(R4P),    intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i, j, k, l, m, n, o
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do o=lbounds_(7), ubounds_(7)
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real32_7

      module subroutine omp_target_init_real64_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P),    intent(inout)        :: array(:)
         real(R8P),    intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do i=lbounds_, ubounds_
            array(i) = val
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real64_1

      module subroutine omp_target_init_real64_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P),    intent(inout)        :: array(:,:)
         real(R8P),    intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i, j
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do j=lbounds_(2), ubounds_(2)
            do i=lbounds_(1), ubounds_(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real64_2

      module subroutine omp_target_init_real64_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P),    intent(inout)        :: array(:,:,:)
         real(R8P),    intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i, j, k
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do k=lbounds_(3), ubounds_(3)
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real64_3

      module subroutine omp_target_init_real64_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P),    intent(inout)        :: array(:,:,:,:)
         real(R8P),    intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i, j, k, l
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do l=lbounds_(4), ubounds_(4)
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real64_4

      module subroutine omp_target_init_real64_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P),    intent(inout)        :: array(:,:,:,:,:)
         real(R8P),    intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i, j, k, l, m
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do m=lbounds_(5), ubounds_(5)
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real64_5

      module subroutine omp_target_init_real64_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P),    intent(inout)        :: array(:,:,:,:,:,:)
         real(R8P),    intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i, j, k, l, m, n
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do n=lbounds_(6), ubounds_(6)
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real64_6

      module subroutine omp_target_init_real64_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P),    intent(inout)        :: array(:,:,:,:,:,:,:)
         real(R8P),    intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i, j, k, l, m, n, o
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do o=lbounds_(7), ubounds_(7)
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real64_7

#if defined _real128
      module subroutine omp_target_init_real128_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P),   intent(inout)        :: array(:)
         real(R16P),   intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do i=lbounds_, ubounds_
            array(i) = val
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real128_1

      module subroutine omp_target_init_real128_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P),   intent(inout)        :: array(:,:)
         real(R16P),   intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i, j
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do j=lbounds_(2), ubounds_(2)
            do i=lbounds_(1), ubounds_(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real128_2

      module subroutine omp_target_init_real128_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P),   intent(inout)        :: array(:,:,:)
         real(R16P),   intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i, j, k
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do k=lbounds_(3), ubounds_(3)
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real128_3

      module subroutine omp_target_init_real128_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P),   intent(inout)        :: array(:,:,:,:)
         real(R16P),   intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i, j, k, l
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do l=lbounds_(4), ubounds_(4)
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real128_4

      module subroutine omp_target_init_real128_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P),   intent(inout)        :: array(:,:,:,:,:)
         real(R16P),   intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i, j, k, l, m
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do m=lbounds_(5), ubounds_(5)
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real128_5

      module subroutine omp_target_init_real128_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P),   intent(inout)        :: array(:,:,:,:,:,:)
         real(R16P),   intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i, j, k, l, m, n
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do n=lbounds_(6), ubounds_(6)
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real128_6

      module subroutine omp_target_init_real128_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P),   intent(inout)        :: array(:,:,:,:,:,:,:)
         real(R16P),   intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i, j, k, l, m, n, o
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do o=lbounds_(7), ubounds_(7)
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_real128_7
#endif
#endif

      ! DMR Init Complex Routines
#if defined _F2008
      module subroutine omp_target_init_cmplx32(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         complex(R4P), intent(inout)        :: array(..)
         complex(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i, j, k, l, m, n, o
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif

         if (present(lbound_s)) then
            allocate(lbounds_(1))
            lbounds_(1) = lbound_s
         elseif (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = lbound(array)
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array)
         endif

         select rank(array)
         rank(1)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do i=lbounds_(1), ubounds_(1)
               array(i) = val
            enddo
            !$omp end target teams distribute parallel do
         rank(2)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j) = val
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(3)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k) = val
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(4)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l) = val
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(5)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(6)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(7)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do o=lbounds_(7), ubounds_(7)
               do n=lbounds_(6), ubounds_(6)
                  do m=lbounds_(5), ubounds_(5)
                     do l=lbounds_(4), ubounds_(4)
                        do k=lbounds_(3), ubounds_(3)
                           do j=lbounds_(2), ubounds_(2)
                              do i=lbounds_(1), ubounds_(1)
                                 array(i,j,k,l,m,n,o) = val
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         endselect
      endsubroutine omp_target_init_cmplx32

      module subroutine omp_target_init_cmplx64(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         complex(R8P), intent(inout)        :: array(..)
         complex(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i, j, k, l, m, n, o
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif

         if (present(lbound_s)) then
            allocate(lbounds_(1))
            lbounds_(1) = lbound_s
         elseif (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = lbound(array)
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array)
         endif

         select rank(array)
         rank(1)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do i=lbounds_(1), ubounds_(1)
               array(i) = val
            enddo
            !$omp end target teams distribute parallel do
         rank(2)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j) = val
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(3)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k) = val
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(4)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l) = val
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(5)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(6)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(7)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do o=lbounds_(7), ubounds_(7)
               do n=lbounds_(6), ubounds_(6)
                  do m=lbounds_(5), ubounds_(5)
                     do l=lbounds_(4), ubounds_(4)
                        do k=lbounds_(3), ubounds_(3)
                           do j=lbounds_(2), ubounds_(2)
                              do i=lbounds_(1), ubounds_(1)
                                 array(i,j,k,l,m,n,o) = val
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         endselect
      endsubroutine omp_target_init_cmplx64

#if defined _real128
      module subroutine omp_target_init_cmplx128(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         complex(R16P), intent(inout)        :: array(..)
         complex(R16P), intent(in)           :: val
         integer(I4P),  intent(in), optional :: omp_dev
         integer(I8P),  intent(in), optional :: lbound_s, ubound_s
         integer(I8P),  intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P),  allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                        :: i, j, k, l, m, n, o
         integer(I8P)                        :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif

         if (present(lbound_s)) then
            allocate(lbounds_(1))
            lbounds_(1) = lbound_s
         elseif (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = lbound(array)
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array)
         endif

         select rank(array)
         rank(1)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do i=lbounds_(1), ubounds_(1)
               array(i) = val
            enddo
            !$omp end target teams distribute parallel do
         rank(2)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j) = val
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(3)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k) = val
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(4)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l) = val
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(5)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(6)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         rank(7)
#if defined _OpenMP_5_1
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
            !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
            do o=lbounds_(7), ubounds_(7)
               do n=lbounds_(6), ubounds_(6)
                  do m=lbounds_(5), ubounds_(5)
                     do l=lbounds_(4), ubounds_(4)
                        do k=lbounds_(3), ubounds_(3)
                           do j=lbounds_(2), ubounds_(2)
                              do i=lbounds_(1), ubounds_(1)
                                 array(i,j,k,l,m,n,o) = val
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            !$omp end target teams distribute parallel do
         endselect
      endsubroutine omp_target_init_cmplx128
#endif
#else
      module subroutine omp_target_init_cmplx32_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(inout)        :: array(:)
         complex(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do i=lbounds_, ubounds_
            array(i) = val
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx32_1

      module subroutine omp_target_init_cmplx32_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(inout)        :: array(:,:)
         complex(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i, j
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do j=lbounds_(2), ubounds_(2)
            do i=lbounds_(1), ubounds_(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx32_2

      module subroutine omp_target_init_cmplx32_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(inout)        :: array(:,:,:)
         complex(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i, j, k
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do k=lbounds_(3), ubounds_(3)
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx32_3

      module subroutine omp_target_init_cmplx32_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(inout)        :: array(:,:,:,:)
         complex(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i, j, k, l
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do l=lbounds_(4), ubounds_(4)
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx32_4

      module subroutine omp_target_init_cmplx32_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(inout)        :: array(:,:,:,:,:)
         complex(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i, j, k, l, m
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do m=lbounds_(5), ubounds_(5)
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx32_5

      module subroutine omp_target_init_cmplx32_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(inout)        :: array(:,:,:,:,:,:)
         complex(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i, j, k, l, m, n
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do n=lbounds_(6), ubounds_(6)
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx32_6

      module subroutine omp_target_init_cmplx32_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(inout)        :: array(:,:,:,:,:,:,:)
         complex(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i, j, k, l, m, n, o
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do o=lbounds_(7), ubounds_(7)
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx32_7

      module subroutine omp_target_init_cmplx64_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(inout)        :: array(:)
         complex(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do i=lbounds_, ubounds_
            array(i) = val
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx64_1

      module subroutine omp_target_init_cmplx64_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(inout)        :: array(:,:)
         complex(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i, j
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do j=lbounds_(2), ubounds_(2)
            do i=lbounds_(1), ubounds_(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx64_2

      module subroutine omp_target_init_cmplx64_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(inout)        :: array(:,:,:)
         complex(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i, j, k
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do k=lbounds_(3), ubounds_(3)
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx64_3

      module subroutine omp_target_init_cmplx64_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(inout)        :: array(:,:,:,:)
         complex(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i, j, k, l
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do l=lbounds_(4), ubounds_(4)
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx64_4

      module subroutine omp_target_init_cmplx64_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(inout)        :: array(:,:,:,:,:)
         complex(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i, j, k, l, m
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do m=lbounds_(5), ubounds_(5)
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx64_5

      module subroutine omp_target_init_cmplx64_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(inout)        :: array(:,:,:,:,:,:)
         complex(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i, j, k, l, m, n
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do n=lbounds_(6), ubounds_(6)
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx64_6

      module subroutine omp_target_init_cmplx64_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(inout)        :: array(:,:,:,:,:,:,:)
         complex(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i, j, k, l, m, n, o
         integer(I8P)                       :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do o=lbounds_(7), ubounds_(7)
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx64_7

#if defined _real128
      module subroutine omp_target_init_cmplx128_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(inout)        :: array(:)
         complex(R16P), intent(in)           :: val
         integer(I4P),  intent(in), optional :: omp_dev
         integer(I8P),  intent(in), optional :: lbounds, ubounds
         integer(I8P)                        :: lbounds_, ubounds_
         integer(I8P)                        :: i
         integer(I8P)                        :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do i=lbounds_, ubounds_
            array(i) = val
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx128_1

      module subroutine omp_target_init_cmplx128_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(inout)        :: array(:,:)
         complex(R16P), intent(in)           :: val
         integer(I4P),  intent(in), optional :: omp_dev
         integer(I8P),  intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                        :: lbounds_(2), ubounds_(2)
         integer(I8P)                        :: i, j
         integer(I8P)                        :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do j=lbounds_(2), ubounds_(2)
            do i=lbounds_(1), ubounds_(1)
               array(i,j) = val
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx128_2

      module subroutine omp_target_init_cmplx128_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(inout)        :: array(:,:,:)
         complex(R16P), intent(in)           :: val
         integer(I4P),  intent(in), optional :: omp_dev
         integer(I8P),  intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                        :: lbounds_(3), ubounds_(3)
         integer(I8P)                        :: i, j, k
         integer(I8P)                        :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do k=lbounds_(3), ubounds_(3)
            do j=lbounds_(2), ubounds_(2)
               do i=lbounds_(1), ubounds_(1)
                  array(i,j,k) = val
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx128_3

      module subroutine omp_target_init_cmplx128_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(inout)        :: array(:,:,:,:)
         complex(R16P), intent(in)           :: val
         integer(I4P),  intent(in), optional :: omp_dev
         integer(I8P),  intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                        :: lbounds_(4), ubounds_(4)
         integer(I8P)                        :: i, j, k, l
         integer(I8P)                        :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do l=lbounds_(4), ubounds_(4)
            do k=lbounds_(3), ubounds_(3)
               do j=lbounds_(2), ubounds_(2)
                  do i=lbounds_(1), ubounds_(1)
                     array(i,j,k,l) = val
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx128_4

      module subroutine omp_target_init_cmplx128_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(inout)        :: array(:,:,:,:,:)
         complex(R16P), intent(in)           :: val
         integer(I4P),  intent(in), optional :: omp_dev
         integer(I8P),  intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                        :: lbounds_(5), ubounds_(5)
         integer(I8P)                        :: i, j, k, l, m
         integer(I8P)                        :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do m=lbounds_(5), ubounds_(5)
            do l=lbounds_(4), ubounds_(4)
               do k=lbounds_(3), ubounds_(3)
                  do j=lbounds_(2), ubounds_(2)
                     do i=lbounds_(1), ubounds_(1)
                        array(i,j,k,l,m) = val
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx128_5

      module subroutine omp_target_init_cmplx128_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(inout)        :: array(:,:,:,:,:,:)
         complex(R16P), intent(in)           :: val
         integer(I4P),  intent(in), optional :: omp_dev
         integer(I8P),  intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                        :: lbounds_(6), ubounds_(6)
         integer(I8P)                        :: i, j, k, l, m, n
         integer(I8P)                        :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do n=lbounds_(6), ubounds_(6)
            do m=lbounds_(5), ubounds_(5)
               do l=lbounds_(4), ubounds_(4)
                  do k=lbounds_(3), ubounds_(3)
                     do j=lbounds_(2), ubounds_(2)
                        do i=lbounds_(1), ubounds_(1)
                           array(i,j,k,l,m,n) = val
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx128_6

      module subroutine omp_target_init_cmplx128_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(inout)        :: array(:,:,:,:,:,:,:)
         complex(R16P), intent(in)           :: val
         integer(I4P),  intent(in), optional :: omp_dev
         integer(I8P),  intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                        :: lbounds_(7), ubounds_(7)
         integer(I8P)                        :: i, j, k, l, m, n, o
         integer(I8P)                        :: omp_dev_

         if (present(omp_dev)) then
            omp_dev_ = omp_dev
         else
            omp_dev_ = omp_get_default_device()
         endif
         if (present(lbounds)) then
            lbounds_ = lbounds
         else
            lbounds_ = 1_I8P
         endif
         if (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array,1)
         endif
#if defined _OpenMP_5_1
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) has_device_addr(array) map(to:lbounds_, ubounds_)
#else
         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array) map(to:lbounds_, ubounds_)
#endif
         do o=lbounds_(7), ubounds_(7)
            do n=lbounds_(6), ubounds_(6)
               do m=lbounds_(5), ubounds_(5)
                  do l=lbounds_(4), ubounds_(4)
                     do k=lbounds_(3), ubounds_(3)
                        do j=lbounds_(2), ubounds_(2)
                           do i=lbounds_(1), ubounds_(1)
                              array(i,j,k,l,m,n,o) = val
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_target_init_cmplx128_7
#endif
#endif

endsubmodule dmr_target_init
