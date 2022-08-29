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

submodule (dmr) dmr_device_memcpy

   implicit none

   contains

#if defined _F2018
      ! DMR Device Memcpy Integer F2018 Routines
      module subroutine omp_device_memcpy_int8(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         integer(I1P), intent(out)          :: array_dst(..)
         integer(I1P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
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
            lbounds_(1:rank(array_dst)) = 1_I8P
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array_dst)
         endif

         select rank(array_dst)
         rank(1)
            select rank(array_src)
            rank(1)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1) = array_src(i1)
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(2)
            select rank(array_src)
            rank(2)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2) = array_src(i1,i2)
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(3)
            select rank(array_src)
            rank(3)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3) = array_src(i1,i2,i3)
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(4)
            select rank(array_src)
            rank(4)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(5)
            select rank(array_src)
            rank(5)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(6)
            select rank(array_src)
            rank(6)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i6 = lbounds_(6), ubounds_(6)
                  do i5 = lbounds_(5), ubounds_(5)
                     do i4 = lbounds_(4), ubounds_(4)
                        do i3 = lbounds_(3), ubounds_(3)
                           do i2 = lbounds_(2), ubounds_(2)
                              do i1 = lbounds_(1), ubounds_(1)
                                 array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(7)
            select rank(array_src)
            rank(7)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i7 = lbounds_(7), ubounds_(7)
                  do i6 = lbounds_(6), ubounds_(6)
                     do i5 = lbounds_(5), ubounds_(5)
                        do i4 = lbounds_(4), ubounds_(4)
                           do i3 = lbounds_(3), ubounds_(3)
                              do i2 = lbounds_(2), ubounds_(2)
                                 do i1 = lbounds_(1), ubounds_(1)
                                    array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                                 enddo
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         endselect
      endsubroutine omp_device_memcpy_int8

      module subroutine omp_device_memcpy_int16(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         integer(I2P), intent(out)          :: array_dst(..)
         integer(I2P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
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
            lbounds_(1:rank(array_dst)) = 1_I8P
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array_dst)
         endif

         select rank(array_dst)
         rank(1)
            select rank(array_src)
            rank(1)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1) = array_src(i1)
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(2)
            select rank(array_src)
            rank(2)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2) = array_src(i1,i2)
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(3)
            select rank(array_src)
            rank(3)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3) = array_src(i1,i2,i3)
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(4)
            select rank(array_src)
            rank(4)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(5)
            select rank(array_src)
            rank(5)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(6)
            select rank(array_src)
            rank(6)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i6 = lbounds_(6), ubounds_(6)
                  do i5 = lbounds_(5), ubounds_(5)
                     do i4 = lbounds_(4), ubounds_(4)
                        do i3 = lbounds_(3), ubounds_(3)
                           do i2 = lbounds_(2), ubounds_(2)
                              do i1 = lbounds_(1), ubounds_(1)
                                 array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(7)
            select rank(array_src)
            rank(7)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i7 = lbounds_(7), ubounds_(7)
                  do i6 = lbounds_(6), ubounds_(6)
                     do i5 = lbounds_(5), ubounds_(5)
                        do i4 = lbounds_(4), ubounds_(4)
                           do i3 = lbounds_(3), ubounds_(3)
                              do i2 = lbounds_(2), ubounds_(2)
                                 do i1 = lbounds_(1), ubounds_(1)
                                    array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                                 enddo
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         endselect
      endsubroutine omp_device_memcpy_int16

      module subroutine omp_device_memcpy_int32(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         integer(I4P), intent(out)          :: array_dst(..)
         integer(I4P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
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
            lbounds_(1:rank(array_dst)) = 1_I8P
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array_dst)
         endif

         select rank(array_dst)
         rank(1)
            select rank(array_src)
            rank(1)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1) = array_src(i1)
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(2)
            select rank(array_src)
            rank(2)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2) = array_src(i1,i2)
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(3)
            select rank(array_src)
            rank(3)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3) = array_src(i1,i2,i3)
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(4)
            select rank(array_src)
            rank(4)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(5)
            select rank(array_src)
            rank(5)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(6)
            select rank(array_src)
            rank(6)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i6 = lbounds_(6), ubounds_(6)
                  do i5 = lbounds_(5), ubounds_(5)
                     do i4 = lbounds_(4), ubounds_(4)
                        do i3 = lbounds_(3), ubounds_(3)
                           do i2 = lbounds_(2), ubounds_(2)
                              do i1 = lbounds_(1), ubounds_(1)
                                 array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(7)
            select rank(array_src)
            rank(7)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i7 = lbounds_(7), ubounds_(7)
                  do i6 = lbounds_(6), ubounds_(6)
                     do i5 = lbounds_(5), ubounds_(5)
                        do i4 = lbounds_(4), ubounds_(4)
                           do i3 = lbounds_(3), ubounds_(3)
                              do i2 = lbounds_(2), ubounds_(2)
                                 do i1 = lbounds_(1), ubounds_(1)
                                    array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                                 enddo
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         endselect
      endsubroutine omp_device_memcpy_int32

      module subroutine omp_device_memcpy_int64(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         integer(I8P), intent(out)          :: array_dst(..)
         integer(I8P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
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
            lbounds_(1:rank(array_dst)) = 1_I8P
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array_dst)
         endif

         select rank(array_dst)
         rank(1)
            select rank(array_src)
            rank(1)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1) = array_src(i1)
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(2)
            select rank(array_src)
            rank(2)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2) = array_src(i1,i2)
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(3)
            select rank(array_src)
            rank(3)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3) = array_src(i1,i2,i3)
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(4)
            select rank(array_src)
            rank(4)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(5)
            select rank(array_src)
            rank(5)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(6)
            select rank(array_src)
            rank(6)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i6 = lbounds_(6), ubounds_(6)
                  do i5 = lbounds_(5), ubounds_(5)
                     do i4 = lbounds_(4), ubounds_(4)
                        do i3 = lbounds_(3), ubounds_(3)
                           do i2 = lbounds_(2), ubounds_(2)
                              do i1 = lbounds_(1), ubounds_(1)
                                 array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(7)
            select rank(array_src)
            rank(7)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i7 = lbounds_(7), ubounds_(7)
                  do i6 = lbounds_(6), ubounds_(6)
                     do i5 = lbounds_(5), ubounds_(5)
                        do i4 = lbounds_(4), ubounds_(4)
                           do i3 = lbounds_(3), ubounds_(3)
                              do i2 = lbounds_(2), ubounds_(2)
                                 do i1 = lbounds_(1), ubounds_(1)
                                    array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                                 enddo
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         endselect
      endsubroutine omp_device_memcpy_int64

      ! DMR Device Memcpy Real F2018 Routines
      module subroutine omp_device_memcpy_real32(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         real(R4P), intent(out)          :: array_dst(..)
         real(R4P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
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
            lbounds_(1:rank(array_dst)) = 1_I8P
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array_dst)
         endif

         select rank(array_dst)
         rank(1)
            select rank(array_src)
            rank(1)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1) = array_src(i1)
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(2)
            select rank(array_src)
            rank(2)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2) = array_src(i1,i2)
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(3)
            select rank(array_src)
            rank(3)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3) = array_src(i1,i2,i3)
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(4)
            select rank(array_src)
            rank(4)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(5)
            select rank(array_src)
            rank(5)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(6)
            select rank(array_src)
            rank(6)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i6 = lbounds_(6), ubounds_(6)
                  do i5 = lbounds_(5), ubounds_(5)
                     do i4 = lbounds_(4), ubounds_(4)
                        do i3 = lbounds_(3), ubounds_(3)
                           do i2 = lbounds_(2), ubounds_(2)
                              do i1 = lbounds_(1), ubounds_(1)
                                 array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(7)
            select rank(array_src)
            rank(7)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i7 = lbounds_(7), ubounds_(7)
                  do i6 = lbounds_(6), ubounds_(6)
                     do i5 = lbounds_(5), ubounds_(5)
                        do i4 = lbounds_(4), ubounds_(4)
                           do i3 = lbounds_(3), ubounds_(3)
                              do i2 = lbounds_(2), ubounds_(2)
                                 do i1 = lbounds_(1), ubounds_(1)
                                    array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                                 enddo
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         endselect
      endsubroutine omp_device_memcpy_real32

      module subroutine omp_device_memcpy_real64(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         real(R8P), intent(out)          :: array_dst(..)
         real(R8P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
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
            lbounds_(1:rank(array_dst)) = 1_I8P
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array_dst)
         endif

         select rank(array_dst)
         rank(1)
            select rank(array_src)
            rank(1)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1) = array_src(i1)
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(2)
            select rank(array_src)
            rank(2)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2) = array_src(i1,i2)
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(3)
            select rank(array_src)
            rank(3)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3) = array_src(i1,i2,i3)
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(4)
            select rank(array_src)
            rank(4)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(5)
            select rank(array_src)
            rank(5)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(6)
            select rank(array_src)
            rank(6)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i6 = lbounds_(6), ubounds_(6)
                  do i5 = lbounds_(5), ubounds_(5)
                     do i4 = lbounds_(4), ubounds_(4)
                        do i3 = lbounds_(3), ubounds_(3)
                           do i2 = lbounds_(2), ubounds_(2)
                              do i1 = lbounds_(1), ubounds_(1)
                                 array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(7)
            select rank(array_src)
            rank(7)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i7 = lbounds_(7), ubounds_(7)
                  do i6 = lbounds_(6), ubounds_(6)
                     do i5 = lbounds_(5), ubounds_(5)
                        do i4 = lbounds_(4), ubounds_(4)
                           do i3 = lbounds_(3), ubounds_(3)
                              do i2 = lbounds_(2), ubounds_(2)
                                 do i1 = lbounds_(1), ubounds_(1)
                                    array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                                 enddo
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         endselect
      endsubroutine omp_device_memcpy_real64

#if defined _real128
      module subroutine omp_device_memcpy_real128(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         real(R16P), intent(out)          :: array_dst(..)
         real(R16P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
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
            lbounds_(1:rank(array_dst)) = 1_I8P
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array_dst)
         endif

         select rank(array_dst)
         rank(1)
            select rank(array_src)
            rank(1)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1) = array_src(i1)
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(2)
            select rank(array_src)
            rank(2)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2) = array_src(i1,i2)
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(3)
            select rank(array_src)
            rank(3)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3) = array_src(i1,i2,i3)
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(4)
            select rank(array_src)
            rank(4)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(5)
            select rank(array_src)
            rank(5)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(6)
            select rank(array_src)
            rank(6)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i6 = lbounds_(6), ubounds_(6)
                  do i5 = lbounds_(5), ubounds_(5)
                     do i4 = lbounds_(4), ubounds_(4)
                        do i3 = lbounds_(3), ubounds_(3)
                           do i2 = lbounds_(2), ubounds_(2)
                              do i1 = lbounds_(1), ubounds_(1)
                                 array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(7)
            select rank(array_src)
            rank(7)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i7 = lbounds_(7), ubounds_(7)
                  do i6 = lbounds_(6), ubounds_(6)
                     do i5 = lbounds_(5), ubounds_(5)
                        do i4 = lbounds_(4), ubounds_(4)
                           do i3 = lbounds_(3), ubounds_(3)
                              do i2 = lbounds_(2), ubounds_(2)
                                 do i1 = lbounds_(1), ubounds_(1)
                                    array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                                 enddo
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         endselect
      endsubroutine omp_device_memcpy_real128
#endif

      ! DMR Device Memcpy Complex F2018 Routines
      module subroutine omp_device_memcpy_cmplx32(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         complex(R4P), intent(out)          :: array_dst(..)
         complex(R4P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
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
            lbounds_(1:rank(array_dst)) = 1_I8P
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array_dst)
         endif

         select rank(array_dst)
         rank(1)
            select rank(array_src)
            rank(1)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1) = array_src(i1)
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(2)
            select rank(array_src)
            rank(2)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2) = array_src(i1,i2)
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(3)
            select rank(array_src)
            rank(3)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3) = array_src(i1,i2,i3)
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(4)
            select rank(array_src)
            rank(4)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(5)
            select rank(array_src)
            rank(5)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(6)
            select rank(array_src)
            rank(6)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i6 = lbounds_(6), ubounds_(6)
                  do i5 = lbounds_(5), ubounds_(5)
                     do i4 = lbounds_(4), ubounds_(4)
                        do i3 = lbounds_(3), ubounds_(3)
                           do i2 = lbounds_(2), ubounds_(2)
                              do i1 = lbounds_(1), ubounds_(1)
                                 array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(7)
            select rank(array_src)
            rank(7)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i7 = lbounds_(7), ubounds_(7)
                  do i6 = lbounds_(6), ubounds_(6)
                     do i5 = lbounds_(5), ubounds_(5)
                        do i4 = lbounds_(4), ubounds_(4)
                           do i3 = lbounds_(3), ubounds_(3)
                              do i2 = lbounds_(2), ubounds_(2)
                                 do i1 = lbounds_(1), ubounds_(1)
                                    array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                                 enddo
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         endselect
      endsubroutine omp_device_memcpy_cmplx32

      module subroutine omp_device_memcpy_cmplx64(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         complex(R8P), intent(out)          :: array_dst(..)
         complex(R8P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
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
            lbounds_(1:rank(array_dst)) = 1_I8P
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array_dst)
         endif

         select rank(array_dst)
         rank(1)
            select rank(array_src)
            rank(1)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1) = array_src(i1)
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(2)
            select rank(array_src)
            rank(2)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2) = array_src(i1,i2)
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(3)
            select rank(array_src)
            rank(3)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3) = array_src(i1,i2,i3)
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(4)
            select rank(array_src)
            rank(4)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(5)
            select rank(array_src)
            rank(5)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(6)
            select rank(array_src)
            rank(6)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i6 = lbounds_(6), ubounds_(6)
                  do i5 = lbounds_(5), ubounds_(5)
                     do i4 = lbounds_(4), ubounds_(4)
                        do i3 = lbounds_(3), ubounds_(3)
                           do i2 = lbounds_(2), ubounds_(2)
                              do i1 = lbounds_(1), ubounds_(1)
                                 array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(7)
            select rank(array_src)
            rank(7)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i7 = lbounds_(7), ubounds_(7)
                  do i6 = lbounds_(6), ubounds_(6)
                     do i5 = lbounds_(5), ubounds_(5)
                        do i4 = lbounds_(4), ubounds_(4)
                           do i3 = lbounds_(3), ubounds_(3)
                              do i2 = lbounds_(2), ubounds_(2)
                                 do i1 = lbounds_(1), ubounds_(1)
                                    array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                                 enddo
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         endselect
      endsubroutine omp_device_memcpy_cmplx64

#if defined _real128
      module subroutine omp_device_memcpy_cmplx128(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         complex(R16P), intent(out)          :: array_dst(..)
         complex(R16P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
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
            lbounds_(1:rank(array_dst)) = 1_I8P
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array_dst)
         endif

         select rank(array_dst)
         rank(1)
            select rank(array_src)
            rank(1)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1) = array_src(i1)
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(2)
            select rank(array_src)
            rank(2)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2) = array_src(i1,i2)
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(3)
            select rank(array_src)
            rank(3)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3) = array_src(i1,i2,i3)
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(4)
            select rank(array_src)
            rank(4)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(5)
            select rank(array_src)
            rank(5)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(6)
            select rank(array_src)
            rank(6)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i6 = lbounds_(6), ubounds_(6)
                  do i5 = lbounds_(5), ubounds_(5)
                     do i4 = lbounds_(4), ubounds_(4)
                        do i3 = lbounds_(3), ubounds_(3)
                           do i2 = lbounds_(2), ubounds_(2)
                              do i1 = lbounds_(1), ubounds_(1)
                                 array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(7)
            select rank(array_src)
            rank(7)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i7 = lbounds_(7), ubounds_(7)
                  do i6 = lbounds_(6), ubounds_(6)
                     do i5 = lbounds_(5), ubounds_(5)
                        do i4 = lbounds_(4), ubounds_(4)
                           do i3 = lbounds_(3), ubounds_(3)
                              do i2 = lbounds_(2), ubounds_(2)
                                 do i1 = lbounds_(1), ubounds_(1)
                                    array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                                 enddo
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         endselect
      endsubroutine omp_device_memcpy_cmplx128
#endif

      ! DMR Device Memcpy Logical F2018 Routines
      module subroutine omp_device_memcpy_lgcl32(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         logical(I4P), intent(out)          :: array_dst(..)
         logical(I4P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
         integer(I8P), allocatable          :: lbounds_(:), ubounds_(:)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
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
            lbounds_(1:rank(array_dst)) = 1_I8P
         endif

         if (present(ubound_s)) then
            allocate(ubounds_(1))
            ubounds_(1) = ubound_s
         elseif (present(ubounds)) then
            ubounds_ = ubounds
         else
            ubounds_ = ubound(array_dst)
         endif

         select rank(array_dst)
         rank(1)
            select rank(array_src)
            rank(1)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(1) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1) = array_src(i1)
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(2)
            select rank(array_src)
            rank(2)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(2) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2) = array_src(i1,i2)
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(3)
            select rank(array_src)
            rank(3)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(3) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3) = array_src(i1,i2,i3)
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(4)
            select rank(array_src)
            rank(4)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(4) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(5)
            select rank(array_src)
            rank(5)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(5) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(6)
            select rank(array_src)
            rank(6)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(6) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i6 = lbounds_(6), ubounds_(6)
                  do i5 = lbounds_(5), ubounds_(5)
                     do i4 = lbounds_(4), ubounds_(4)
                        do i3 = lbounds_(3), ubounds_(3)
                           do i2 = lbounds_(2), ubounds_(2)
                              do i1 = lbounds_(1), ubounds_(1)
                                 array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         rank(7)
            select rank(array_src)
            rank(7)
!#if defined _OpenMP_5_1
!               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
               !$omp target teams distribute parallel do collapse(7) device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
               do i7 = lbounds_(7), ubounds_(7)
                  do i6 = lbounds_(6), ubounds_(6)
                     do i5 = lbounds_(5), ubounds_(5)
                        do i4 = lbounds_(4), ubounds_(4)
                           do i3 = lbounds_(3), ubounds_(3)
                              do i2 = lbounds_(2), ubounds_(2)
                                 do i1 = lbounds_(1), ubounds_(1)
                                    array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                                 enddo
                              enddo
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
               !$omp end target teams distribute parallel do
            rank default
               error stop 'Rank mismatch between source and destination'
            end select

         endselect
      endsubroutine omp_device_memcpy_lgcl32


#else
      ! DMR Device Memcpy Integer Routines
      module subroutine omp_device_memcpy_int8_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(out)          :: array_dst(:)
         integer(I1P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i1
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst,1)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i1=lbounds_, ubounds_
            array_dst(i1) = array_src(i1)
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int8_1

      module subroutine omp_device_memcpy_int8_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(out)          :: array_dst(:,:)
         integer(I1P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i1, i2
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i2 = lbounds_(2), ubounds_(2)
            do i1 = lbounds_(1), ubounds_(1)
               array_dst(i1,i2) = array_src(i1,i2)
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int8_2

      module subroutine omp_device_memcpy_int8_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(out)          :: array_dst(:,:,:)
         integer(I1P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i1, i2, i3
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i3 = lbounds_(3), ubounds_(3)
            do i2 = lbounds_(2), ubounds_(2)
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1,i2,i3) = array_src(i1,i2,i3)
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int8_3

      module subroutine omp_device_memcpy_int8_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(out)          :: array_dst(:,:,:,:)
         integer(I1P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i1, i2, i3, i4
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i4 = lbounds_(4), ubounds_(4)
            do i3 = lbounds_(3), ubounds_(3)
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int8_4

      module subroutine omp_device_memcpy_int8_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(out)          :: array_dst(:,:,:,:,:)
         integer(I1P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i5 = lbounds_(5), ubounds_(5)
            do i4 = lbounds_(4), ubounds_(4)
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int8_5

      module subroutine omp_device_memcpy_int8_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(out)          :: array_dst(:,:,:,:,:,:)
         integer(I1P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i6 = lbounds_(6), ubounds_(6)
            do i5 = lbounds_(5), ubounds_(5)
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int8_6

      module subroutine omp_device_memcpy_int8_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         integer(I1P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i7 = lbounds_(7), ubounds_(7)
            do i6 = lbounds_(6), ubounds_(6)
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int8_7

      module subroutine omp_device_memcpy_int16_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(out)          :: array_dst(:)
         integer(I2P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i1
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst,1)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i1=lbounds_, ubounds_
            array_dst(i1) = array_src(i1)
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int16_1

      module subroutine omp_device_memcpy_int16_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(out)          :: array_dst(:,:)
         integer(I2P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i1, i2
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i2 = lbounds_(2), ubounds_(2)
            do i1 = lbounds_(1), ubounds_(1)
               array_dst(i1,i2) = array_src(i1,i2)
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int16_2

      module subroutine omp_device_memcpy_int16_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(out)          :: array_dst(:,:,:)
         integer(I2P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i1, i2, i3
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i3 = lbounds_(3), ubounds_(3)
            do i2 = lbounds_(2), ubounds_(2)
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1,i2,i3) = array_src(i1,i2,i3)
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int16_3

      module subroutine omp_device_memcpy_int16_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(out)          :: array_dst(:,:,:,:)
         integer(I2P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i1, i2, i3, i4
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i4 = lbounds_(4), ubounds_(4)
            do i3 = lbounds_(3), ubounds_(3)
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int16_4

      module subroutine omp_device_memcpy_int16_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(out)          :: array_dst(:,:,:,:,:)
         integer(I2P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i5 = lbounds_(5), ubounds_(5)
            do i4 = lbounds_(4), ubounds_(4)
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int16_5

      module subroutine omp_device_memcpy_int16_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(out)          :: array_dst(:,:,:,:,:,:)
         integer(I2P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i6 = lbounds_(6), ubounds_(6)
            do i5 = lbounds_(5), ubounds_(5)
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int16_6

      module subroutine omp_device_memcpy_int16_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         integer(I2P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i7 = lbounds_(7), ubounds_(7)
            do i6 = lbounds_(6), ubounds_(6)
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int16_7

      module subroutine omp_device_memcpy_int32_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(out)          :: array_dst(:)
         integer(I4P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i1
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst,1)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i1=lbounds_, ubounds_
            array_dst(i1) = array_src(i1)
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int32_1

      module subroutine omp_device_memcpy_int32_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(out)          :: array_dst(:,:)
         integer(I4P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i1, i2
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i2 = lbounds_(2), ubounds_(2)
            do i1 = lbounds_(1), ubounds_(1)
               array_dst(i1,i2) = array_src(i1,i2)
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int32_2

      module subroutine omp_device_memcpy_int32_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(out)          :: array_dst(:,:,:)
         integer(I4P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i1, i2, i3
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i3 = lbounds_(3), ubounds_(3)
            do i2 = lbounds_(2), ubounds_(2)
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1,i2,i3) = array_src(i1,i2,i3)
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int32_3

      module subroutine omp_device_memcpy_int32_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(out)          :: array_dst(:,:,:,:)
         integer(I4P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i1, i2, i3, i4
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i4 = lbounds_(4), ubounds_(4)
            do i3 = lbounds_(3), ubounds_(3)
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int32_4

      module subroutine omp_device_memcpy_int32_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(out)          :: array_dst(:,:,:,:,:)
         integer(I4P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i5 = lbounds_(5), ubounds_(5)
            do i4 = lbounds_(4), ubounds_(4)
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int32_5

      module subroutine omp_device_memcpy_int32_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(out)          :: array_dst(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i6 = lbounds_(6), ubounds_(6)
            do i5 = lbounds_(5), ubounds_(5)
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int32_6

      module subroutine omp_device_memcpy_int32_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i7 = lbounds_(7), ubounds_(7)
            do i6 = lbounds_(6), ubounds_(6)
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int32_7

      module subroutine omp_device_memcpy_int64_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(out)          :: array_dst(:)
         integer(I8P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i1
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst,1)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i1=lbounds_, ubounds_
            array_dst(i1) = array_src(i1)
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int64_1

      module subroutine omp_device_memcpy_int64_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(out)          :: array_dst(:,:)
         integer(I8P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i1, i2
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i2 = lbounds_(2), ubounds_(2)
            do i1 = lbounds_(1), ubounds_(1)
               array_dst(i1,i2) = array_src(i1,i2)
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int64_2

      module subroutine omp_device_memcpy_int64_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(out)          :: array_dst(:,:,:)
         integer(I8P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i1, i2, i3
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i3 = lbounds_(3), ubounds_(3)
            do i2 = lbounds_(2), ubounds_(2)
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1,i2,i3) = array_src(i1,i2,i3)
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int64_3

      module subroutine omp_device_memcpy_int64_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(out)          :: array_dst(:,:,:,:)
         integer(I8P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i1, i2, i3, i4
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i4 = lbounds_(4), ubounds_(4)
            do i3 = lbounds_(3), ubounds_(3)
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int64_4

      module subroutine omp_device_memcpy_int64_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(out)          :: array_dst(:,:,:,:,:)
         integer(I8P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i5 = lbounds_(5), ubounds_(5)
            do i4 = lbounds_(4), ubounds_(4)
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int64_5

      module subroutine omp_device_memcpy_int64_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(out)          :: array_dst(:,:,:,:,:,:)
         integer(I8P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i6 = lbounds_(6), ubounds_(6)
            do i5 = lbounds_(5), ubounds_(5)
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int64_6

      module subroutine omp_device_memcpy_int64_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         integer(I8P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i7 = lbounds_(7), ubounds_(7)
            do i6 = lbounds_(6), ubounds_(6)
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_int64_7


      ! DMR Device Memcpy Real Routines
      module subroutine omp_device_memcpy_real32_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(out)          :: array_dst(:)
         real(R4P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i1
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst,1)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i1=lbounds_, ubounds_
            array_dst(i1) = array_src(i1)
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real32_1

      module subroutine omp_device_memcpy_real32_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(out)          :: array_dst(:,:)
         real(R4P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i1, i2
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i2 = lbounds_(2), ubounds_(2)
            do i1 = lbounds_(1), ubounds_(1)
               array_dst(i1,i2) = array_src(i1,i2)
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real32_2

      module subroutine omp_device_memcpy_real32_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(out)          :: array_dst(:,:,:)
         real(R4P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i1, i2, i3
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i3 = lbounds_(3), ubounds_(3)
            do i2 = lbounds_(2), ubounds_(2)
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1,i2,i3) = array_src(i1,i2,i3)
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real32_3

      module subroutine omp_device_memcpy_real32_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(out)          :: array_dst(:,:,:,:)
         real(R4P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i1, i2, i3, i4
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i4 = lbounds_(4), ubounds_(4)
            do i3 = lbounds_(3), ubounds_(3)
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real32_4

      module subroutine omp_device_memcpy_real32_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(out)          :: array_dst(:,:,:,:,:)
         real(R4P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i5 = lbounds_(5), ubounds_(5)
            do i4 = lbounds_(4), ubounds_(4)
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real32_5

      module subroutine omp_device_memcpy_real32_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(out)          :: array_dst(:,:,:,:,:,:)
         real(R4P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i6 = lbounds_(6), ubounds_(6)
            do i5 = lbounds_(5), ubounds_(5)
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real32_6

      module subroutine omp_device_memcpy_real32_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         real(R4P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i7 = lbounds_(7), ubounds_(7)
            do i6 = lbounds_(6), ubounds_(6)
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real32_7

      module subroutine omp_device_memcpy_real64_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(out)          :: array_dst(:)
         real(R8P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i1
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst,1)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i1=lbounds_, ubounds_
            array_dst(i1) = array_src(i1)
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real64_1

      module subroutine omp_device_memcpy_real64_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(out)          :: array_dst(:,:)
         real(R8P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i1, i2
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i2 = lbounds_(2), ubounds_(2)
            do i1 = lbounds_(1), ubounds_(1)
               array_dst(i1,i2) = array_src(i1,i2)
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real64_2

      module subroutine omp_device_memcpy_real64_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(out)          :: array_dst(:,:,:)
         real(R8P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i1, i2, i3
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i3 = lbounds_(3), ubounds_(3)
            do i2 = lbounds_(2), ubounds_(2)
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1,i2,i3) = array_src(i1,i2,i3)
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real64_3

      module subroutine omp_device_memcpy_real64_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(out)          :: array_dst(:,:,:,:)
         real(R8P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i1, i2, i3, i4
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i4 = lbounds_(4), ubounds_(4)
            do i3 = lbounds_(3), ubounds_(3)
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real64_4

      module subroutine omp_device_memcpy_real64_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(out)          :: array_dst(:,:,:,:,:)
         real(R8P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i5 = lbounds_(5), ubounds_(5)
            do i4 = lbounds_(4), ubounds_(4)
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real64_5

      module subroutine omp_device_memcpy_real64_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(out)          :: array_dst(:,:,:,:,:,:)
         real(R8P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i6 = lbounds_(6), ubounds_(6)
            do i5 = lbounds_(5), ubounds_(5)
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real64_6

      module subroutine omp_device_memcpy_real64_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         real(R8P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i7 = lbounds_(7), ubounds_(7)
            do i6 = lbounds_(6), ubounds_(6)
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real64_7

#if defined _real128
      module subroutine omp_device_memcpy_real128_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(out)          :: array_dst(:)
         real(R16P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i1
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst,1)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i1=lbounds_, ubounds_
            array_dst(i1) = array_src(i1)
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real128_1

      module subroutine omp_device_memcpy_real128_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(out)          :: array_dst(:,:)
         real(R16P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i1, i2
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i2 = lbounds_(2), ubounds_(2)
            do i1 = lbounds_(1), ubounds_(1)
               array_dst(i1,i2) = array_src(i1,i2)
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real128_2

      module subroutine omp_device_memcpy_real128_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(out)          :: array_dst(:,:,:)
         real(R16P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i1, i2, i3
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i3 = lbounds_(3), ubounds_(3)
            do i2 = lbounds_(2), ubounds_(2)
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1,i2,i3) = array_src(i1,i2,i3)
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real128_3

      module subroutine omp_device_memcpy_real128_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(out)          :: array_dst(:,:,:,:)
         real(R16P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i1, i2, i3, i4
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i4 = lbounds_(4), ubounds_(4)
            do i3 = lbounds_(3), ubounds_(3)
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real128_4

      module subroutine omp_device_memcpy_real128_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(out)          :: array_dst(:,:,:,:,:)
         real(R16P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i5 = lbounds_(5), ubounds_(5)
            do i4 = lbounds_(4), ubounds_(4)
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real128_5

      module subroutine omp_device_memcpy_real128_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(out)          :: array_dst(:,:,:,:,:,:)
         real(R16P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i6 = lbounds_(6), ubounds_(6)
            do i5 = lbounds_(5), ubounds_(5)
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real128_6

      module subroutine omp_device_memcpy_real128_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         real(R16P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i7 = lbounds_(7), ubounds_(7)
            do i6 = lbounds_(6), ubounds_(6)
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_real128_7

#endif

      ! DMR Device Memcpy Complex Routines
      module subroutine omp_device_memcpy_cmplx32_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(out)          :: array_dst(:)
         complex(R4P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i1
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst,1)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i1=lbounds_, ubounds_
            array_dst(i1) = array_src(i1)
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx32_1

      module subroutine omp_device_memcpy_cmplx32_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(out)          :: array_dst(:,:)
         complex(R4P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i1, i2
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i2 = lbounds_(2), ubounds_(2)
            do i1 = lbounds_(1), ubounds_(1)
               array_dst(i1,i2) = array_src(i1,i2)
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx32_2

      module subroutine omp_device_memcpy_cmplx32_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(out)          :: array_dst(:,:,:)
         complex(R4P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i1, i2, i3
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i3 = lbounds_(3), ubounds_(3)
            do i2 = lbounds_(2), ubounds_(2)
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1,i2,i3) = array_src(i1,i2,i3)
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx32_3

      module subroutine omp_device_memcpy_cmplx32_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(out)          :: array_dst(:,:,:,:)
         complex(R4P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i1, i2, i3, i4
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i4 = lbounds_(4), ubounds_(4)
            do i3 = lbounds_(3), ubounds_(3)
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx32_4

      module subroutine omp_device_memcpy_cmplx32_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(out)          :: array_dst(:,:,:,:,:)
         complex(R4P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i5 = lbounds_(5), ubounds_(5)
            do i4 = lbounds_(4), ubounds_(4)
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx32_5

      module subroutine omp_device_memcpy_cmplx32_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(out)          :: array_dst(:,:,:,:,:,:)
         complex(R4P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i6 = lbounds_(6), ubounds_(6)
            do i5 = lbounds_(5), ubounds_(5)
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx32_6

      module subroutine omp_device_memcpy_cmplx32_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         complex(R4P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i7 = lbounds_(7), ubounds_(7)
            do i6 = lbounds_(6), ubounds_(6)
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx32_7

      module subroutine omp_device_memcpy_cmplx64_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(out)          :: array_dst(:)
         complex(R8P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i1
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst,1)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i1=lbounds_, ubounds_
            array_dst(i1) = array_src(i1)
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx64_1

      module subroutine omp_device_memcpy_cmplx64_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(out)          :: array_dst(:,:)
         complex(R8P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i1, i2
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i2 = lbounds_(2), ubounds_(2)
            do i1 = lbounds_(1), ubounds_(1)
               array_dst(i1,i2) = array_src(i1,i2)
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx64_2

      module subroutine omp_device_memcpy_cmplx64_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(out)          :: array_dst(:,:,:)
         complex(R8P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i1, i2, i3
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i3 = lbounds_(3), ubounds_(3)
            do i2 = lbounds_(2), ubounds_(2)
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1,i2,i3) = array_src(i1,i2,i3)
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx64_3

      module subroutine omp_device_memcpy_cmplx64_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(out)          :: array_dst(:,:,:,:)
         complex(R8P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i1, i2, i3, i4
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i4 = lbounds_(4), ubounds_(4)
            do i3 = lbounds_(3), ubounds_(3)
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx64_4

      module subroutine omp_device_memcpy_cmplx64_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(out)          :: array_dst(:,:,:,:,:)
         complex(R8P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i5 = lbounds_(5), ubounds_(5)
            do i4 = lbounds_(4), ubounds_(4)
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx64_5

      module subroutine omp_device_memcpy_cmplx64_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(out)          :: array_dst(:,:,:,:,:,:)
         complex(R8P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i6 = lbounds_(6), ubounds_(6)
            do i5 = lbounds_(5), ubounds_(5)
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx64_6

      module subroutine omp_device_memcpy_cmplx64_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         complex(R8P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i7 = lbounds_(7), ubounds_(7)
            do i6 = lbounds_(6), ubounds_(6)
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx64_7

#if defined _real128
      module subroutine omp_device_memcpy_cmplx128_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(out)          :: array_dst(:)
         complex(R16P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i1
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst,1)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i1=lbounds_, ubounds_
            array_dst(i1) = array_src(i1)
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx128_1

      module subroutine omp_device_memcpy_cmplx128_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(out)          :: array_dst(:,:)
         complex(R16P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i1, i2
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i2 = lbounds_(2), ubounds_(2)
            do i1 = lbounds_(1), ubounds_(1)
               array_dst(i1,i2) = array_src(i1,i2)
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx128_2

      module subroutine omp_device_memcpy_cmplx128_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(out)          :: array_dst(:,:,:)
         complex(R16P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i1, i2, i3
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i3 = lbounds_(3), ubounds_(3)
            do i2 = lbounds_(2), ubounds_(2)
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1,i2,i3) = array_src(i1,i2,i3)
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx128_3

      module subroutine omp_device_memcpy_cmplx128_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(out)          :: array_dst(:,:,:,:)
         complex(R16P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i1, i2, i3, i4
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i4 = lbounds_(4), ubounds_(4)
            do i3 = lbounds_(3), ubounds_(3)
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx128_4

      module subroutine omp_device_memcpy_cmplx128_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(out)          :: array_dst(:,:,:,:,:)
         complex(R16P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i5 = lbounds_(5), ubounds_(5)
            do i4 = lbounds_(4), ubounds_(4)
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx128_5

      module subroutine omp_device_memcpy_cmplx128_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(out)          :: array_dst(:,:,:,:,:,:)
         complex(R16P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i6 = lbounds_(6), ubounds_(6)
            do i5 = lbounds_(5), ubounds_(5)
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx128_6

      module subroutine omp_device_memcpy_cmplx128_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         complex(R16P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i7 = lbounds_(7), ubounds_(7)
            do i6 = lbounds_(6), ubounds_(6)
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_cmplx128_7

#endif

      ! DMR Device Memcpy Logical Routines
      module subroutine omp_device_memcpy_lgcl32_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(out)          :: array_dst(:)
         logical(I4P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: lbounds_, ubounds_
         integer(I8P)                       :: i1
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst,1)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(1) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i1=lbounds_, ubounds_
            array_dst(i1) = array_src(i1)
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_lgcl32_1

      module subroutine omp_device_memcpy_lgcl32_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(out)          :: array_dst(:,:)
         logical(I4P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: lbounds_(2), ubounds_(2)
         integer(I8P)                       :: i1, i2
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(2) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i2 = lbounds_(2), ubounds_(2)
            do i1 = lbounds_(1), ubounds_(1)
               array_dst(i1,i2) = array_src(i1,i2)
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_lgcl32_2

      module subroutine omp_device_memcpy_lgcl32_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(out)          :: array_dst(:,:,:)
         logical(I4P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: lbounds_(3), ubounds_(3)
         integer(I8P)                       :: i1, i2, i3
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(3) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i3 = lbounds_(3), ubounds_(3)
            do i2 = lbounds_(2), ubounds_(2)
               do i1 = lbounds_(1), ubounds_(1)
                  array_dst(i1,i2,i3) = array_src(i1,i2,i3)
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_lgcl32_3

      module subroutine omp_device_memcpy_lgcl32_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(out)          :: array_dst(:,:,:,:)
         logical(I4P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: lbounds_(4), ubounds_(4)
         integer(I8P)                       :: i1, i2, i3, i4
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(4) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i4 = lbounds_(4), ubounds_(4)
            do i3 = lbounds_(3), ubounds_(3)
               do i2 = lbounds_(2), ubounds_(2)
                  do i1 = lbounds_(1), ubounds_(1)
                     array_dst(i1,i2,i3,i4) = array_src(i1,i2,i3,i4)
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_lgcl32_4

      module subroutine omp_device_memcpy_lgcl32_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(out)          :: array_dst(:,:,:,:,:)
         logical(I4P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: lbounds_(5), ubounds_(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(5) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i5 = lbounds_(5), ubounds_(5)
            do i4 = lbounds_(4), ubounds_(4)
               do i3 = lbounds_(3), ubounds_(3)
                  do i2 = lbounds_(2), ubounds_(2)
                     do i1 = lbounds_(1), ubounds_(1)
                        array_dst(i1,i2,i3,i4,i5) = array_src(i1,i2,i3,i4,i5)
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_lgcl32_5

      module subroutine omp_device_memcpy_lgcl32_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(out)          :: array_dst(:,:,:,:,:,:)
         logical(I4P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: lbounds_(6), ubounds_(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(6) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i6 = lbounds_(6), ubounds_(6)
            do i5 = lbounds_(5), ubounds_(5)
               do i4 = lbounds_(4), ubounds_(4)
                  do i3 = lbounds_(3), ubounds_(3)
                     do i2 = lbounds_(2), ubounds_(2)
                        do i1 = lbounds_(1), ubounds_(1)
                           array_dst(i1,i2,i3,i4,i5,i6) = array_src(i1,i2,i3,i4,i5,i6)
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_lgcl32_6

      module subroutine omp_device_memcpy_lgcl32_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         logical(I4P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: lbounds_(7), ubounds_(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
         integer(I4P)                       :: omp_dev_

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
            ubounds_ = ubound(array_dst)
         endif
!#if defined _OpenMP_5_1
!         !$omp target teams distribute parallel do collapse(7) device(omp_dev_) map(present, alloc:array_dst, array_src) map(to:lbounds_, ubounds_)
!#else
         !$omp target teams distribute parallel do device(omp_dev_) is_device_ptr(array_dst, array_src) map(to:lbounds_, ubounds_)
!#endif
         do i7 = lbounds_(7), ubounds_(7)
            do i6 = lbounds_(6), ubounds_(6)
               do i5 = lbounds_(5), ubounds_(5)
                  do i4 = lbounds_(4), ubounds_(4)
                     do i3 = lbounds_(3), ubounds_(3)
                        do i2 = lbounds_(2), ubounds_(2)
                           do i1 = lbounds_(1), ubounds_(1)
                              array_dst(i1,i2,i3,i4,i5,i6,i7) = array_src(i1,i2,i3,i4,i5,i6,i7)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
         !$omp end target teams distribute parallel do
      endsubroutine omp_device_memcpy_lgcl32_7



#endif
endsubmodule dmr_device_memcpy