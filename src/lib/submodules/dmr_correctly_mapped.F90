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

submodule (dmr) dmr_correctly_mapped
   use, intrinsic :: iso_c_binding
   use dmr_environment

   implicit none

   contains

#if defined _F2018
      ! DMR Correctly Mapped Integer F2018 Routines
      module function omp_correctly_mapped_int8(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int8
         integer(I1P), intent(in)  :: array(..)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P), allocatable :: size_host(:)
         integer(I4P)              :: i, array_rank

         select rank(array_dst)
         rank(1)
            array_rank=1
         rank(2)
            array_rank=2
         rank(3)
            array_rank=3
         rank(4)
            array_rank=4
         rank(5)
            array_rank=5
         rank(6)
            array_rank=6
         rank(7)
            array_rank=7
         end select
         allocate(size_host(1:array_rank))
         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int8 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int8) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int8)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int8 = omp_correctly_mapped_int8 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
         deallocate(size_host)
      endfunction omp_correctly_mapped_int8

      module function omp_correctly_mapped_int16(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int16
         integer(I2P), intent(in)  :: array(..)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P), allocatable :: size_host(:)
         integer(I4P)              :: i, array_rank

         select rank(array_dst)
         rank(1)
            array_rank=1
         rank(2)
            array_rank=2
         rank(3)
            array_rank=3
         rank(4)
            array_rank=4
         rank(5)
            array_rank=5
         rank(6)
            array_rank=6
         rank(7)
            array_rank=7
         end select
         allocate(size_host(1:array_rank))
         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int16 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int16) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int16)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int16 = omp_correctly_mapped_int16 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
         deallocate(size_host)
      endfunction omp_correctly_mapped_int16

      module function omp_correctly_mapped_int32(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int32
         integer(I4P), intent(in)  :: array(..)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P), allocatable :: size_host(:)
         integer(I4P)              :: i, array_rank

         select rank(array_dst)
         rank(1)
            array_rank=1
         rank(2)
            array_rank=2
         rank(3)
            array_rank=3
         rank(4)
            array_rank=4
         rank(5)
            array_rank=5
         rank(6)
            array_rank=6
         rank(7)
            array_rank=7
         end select
         allocate(size_host(1:array_rank))
         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int32 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int32) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int32)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int32 = omp_correctly_mapped_int32 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
         deallocate(size_host)
      endfunction omp_correctly_mapped_int32

      module function omp_correctly_mapped_int64(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int64
         integer(I8P), intent(in)  :: array(..)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P), allocatable :: size_host(:)
         integer(I4P)              :: i, array_rank

         select rank(array_dst)
         rank(1)
            array_rank=1
         rank(2)
            array_rank=2
         rank(3)
            array_rank=3
         rank(4)
            array_rank=4
         rank(5)
            array_rank=5
         rank(6)
            array_rank=6
         rank(7)
            array_rank=7
         end select
         allocate(size_host(1:array_rank))
         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int64 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int64) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int64)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int64 = omp_correctly_mapped_int64 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
         deallocate(size_host)
      endfunction omp_correctly_mapped_int64

      ! DMR Correctly Mapped Real F2018 Routines
      module function omp_correctly_mapped_real32(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real32
         real(R4P), intent(in)  :: array(..)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P), allocatable :: size_host(:)
         integer(I4P)              :: i, array_rank

         select rank(array_dst)
         rank(1)
            array_rank=1
         rank(2)
            array_rank=2
         rank(3)
            array_rank=3
         rank(4)
            array_rank=4
         rank(5)
            array_rank=5
         rank(6)
            array_rank=6
         rank(7)
            array_rank=7
         end select
         allocate(size_host(1:array_rank))
         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real32 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real32) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real32)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real32 = omp_correctly_mapped_real32 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
         deallocate(size_host)
      endfunction omp_correctly_mapped_real32

      module function omp_correctly_mapped_real64(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real64
         real(R8P), intent(in)  :: array(..)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P), allocatable :: size_host(:)
         integer(I4P)              :: i, array_rank

         select rank(array_dst)
         rank(1)
            array_rank=1
         rank(2)
            array_rank=2
         rank(3)
            array_rank=3
         rank(4)
            array_rank=4
         rank(5)
            array_rank=5
         rank(6)
            array_rank=6
         rank(7)
            array_rank=7
         end select
         allocate(size_host(1:array_rank))
         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real64 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real64) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real64)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real64 = omp_correctly_mapped_real64 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
         deallocate(size_host)
      endfunction omp_correctly_mapped_real64

#if defined _real128
      module function omp_correctly_mapped_real128(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real128
         real(R16P), intent(in)  :: array(..)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P), allocatable :: size_host(:)
         integer(I4P)              :: i, array_rank

         select rank(array_dst)
         rank(1)
            array_rank=1
         rank(2)
            array_rank=2
         rank(3)
            array_rank=3
         rank(4)
            array_rank=4
         rank(5)
            array_rank=5
         rank(6)
            array_rank=6
         rank(7)
            array_rank=7
         end select
         allocate(size_host(1:array_rank))
         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real128 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real128) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real128)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real128 = omp_correctly_mapped_real128 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
         deallocate(size_host)
      endfunction omp_correctly_mapped_real128
#endif

      ! DMR Correctly Mapped Complex F2018 Routines
      module function omp_correctly_mapped_cmplx32(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx32
         complex(R4P), intent(in)  :: array(..)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P), allocatable :: size_host(:)
         integer(I4P)              :: i, array_rank

         select rank(array_dst)
         rank(1)
            array_rank=1
         rank(2)
            array_rank=2
         rank(3)
            array_rank=3
         rank(4)
            array_rank=4
         rank(5)
            array_rank=5
         rank(6)
            array_rank=6
         rank(7)
            array_rank=7
         end select
         allocate(size_host(1:array_rank))
         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx32 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx32) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx32)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx32 = omp_correctly_mapped_cmplx32 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
         deallocate(size_host)
      endfunction omp_correctly_mapped_cmplx32

      module function omp_correctly_mapped_cmplx64(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx64
         complex(R8P), intent(in)  :: array(..)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P), allocatable :: size_host(:)
         integer(I4P)              :: i, array_rank

         select rank(array_dst)
         rank(1)
            array_rank=1
         rank(2)
            array_rank=2
         rank(3)
            array_rank=3
         rank(4)
            array_rank=4
         rank(5)
            array_rank=5
         rank(6)
            array_rank=6
         rank(7)
            array_rank=7
         end select
         allocate(size_host(1:array_rank))
         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx64 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx64) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx64)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx64 = omp_correctly_mapped_cmplx64 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
         deallocate(size_host)
      endfunction omp_correctly_mapped_cmplx64

#if defined _real128
      module function omp_correctly_mapped_cmplx128(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx128
         complex(R16P), intent(in)  :: array(..)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P), allocatable :: size_host(:)
         integer(I4P)              :: i, array_rank

         select rank(array_dst)
         rank(1)
            array_rank=1
         rank(2)
            array_rank=2
         rank(3)
            array_rank=3
         rank(4)
            array_rank=4
         rank(5)
            array_rank=5
         rank(6)
            array_rank=6
         rank(7)
            array_rank=7
         end select
         allocate(size_host(1:array_rank))
         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx128 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx128) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx128)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx128 = omp_correctly_mapped_cmplx128 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
         deallocate(size_host)
      endfunction omp_correctly_mapped_cmplx128
#endif

      ! DMR Correctly Mapped Logical F2018 Routines
      module function omp_correctly_mapped_lgcl32(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_lgcl32
         logical(I4P), intent(in)  :: array(..)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P), allocatable :: size_host(:)
         integer(I4P)              :: i, array_rank

         select rank(array_dst)
         rank(1)
            array_rank=1
         rank(2)
            array_rank=2
         rank(3)
            array_rank=3
         rank(4)
            array_rank=4
         rank(5)
            array_rank=5
         rank(6)
            array_rank=6
         rank(7)
            array_rank=7
         end select
         allocate(size_host(1:array_rank))
         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_lgcl32 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_lgcl32) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_lgcl32)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_lgcl32 = omp_correctly_mapped_lgcl32 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
         deallocate(size_host)
      endfunction omp_correctly_mapped_lgcl32


#else
      ! OpenMP Target Alloc Integer Routines
      module function omp_correctly_mapped_int8_1(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int8_1
         integer(I1P), intent(in)  :: array(:)
         integer(I4P)              :: size_host
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         size_host = size(array,1)

         omp_correctly_mapped_int8_1 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int8_1) map(to:size_host)
         if (size(array,1)/=size_host) omp_correctly_mapped_int8_1 = omp_correctly_mapped_int8_1 .and. .false.
!$omp end target
      endfunction omp_correctly_mapped_int8_1

      module function omp_correctly_mapped_int8_2(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int8_2
         integer(I1P), intent(in)  :: array(:,:)
         integer(I4P), parameter   :: array_rank=2_I4P
         integer(I4P)              :: size_host(2)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int8_2 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int8_2) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int8_2)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int8_2 = omp_correctly_mapped_int8_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int8_2

      module function omp_correctly_mapped_int8_3(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int8_3
         integer(I1P), intent(in)  :: array(:,:,:)
         integer(I4P), parameter   :: array_rank=3_I4P
         integer(I4P)              :: size_host(3)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int8_3 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int8_3) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int8_3)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int8_3 = omp_correctly_mapped_int8_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int8_3

      module function omp_correctly_mapped_int8_4(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int8_4
         integer(I1P), intent(in)  :: array(:,:,:,:)
         integer(I4P), parameter   :: array_rank=4_I4P
         integer(I4P)              :: size_host(4)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int8_4 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int8_4) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int8_4)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int8_4 = omp_correctly_mapped_int8_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int8_4

      module function omp_correctly_mapped_int8_5(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int8_5
         integer(I1P), intent(in)  :: array(:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=5_I4P
         integer(I4P)              :: size_host(5)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int8_5 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int8_5) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int8_5)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int8_5 = omp_correctly_mapped_int8_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int8_5

      module function omp_correctly_mapped_int8_6(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int8_6
         integer(I1P), intent(in)  :: array(:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=6_I4P
         integer(I4P)              :: size_host(6)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int8_6 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int8_6) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int8_6)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int8_6 = omp_correctly_mapped_int8_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int8_6

      module function omp_correctly_mapped_int8_7(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int8_7
         integer(I1P), intent(in)  :: array(:,:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=7_I4P
         integer(I4P)              :: size_host(7)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int8_7 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int8_7) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int8_7)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int8_7 = omp_correctly_mapped_int8_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int8_7

      module function omp_correctly_mapped_int16_1(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int16_1
         integer(I2P), intent(in)  :: array(:)
         integer(I4P)              :: size_host
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         size_host = size(array,1)

         omp_correctly_mapped_int16_1 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int16_1) map(to:size_host)
         if (size(array,1)/=size_host) omp_correctly_mapped_int16_1 = omp_correctly_mapped_int16_1 .and. .false.
!$omp end target
      endfunction omp_correctly_mapped_int16_1

      module function omp_correctly_mapped_int16_2(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int16_2
         integer(I2P), intent(in)  :: array(:,:)
         integer(I4P), parameter   :: array_rank=2_I4P
         integer(I4P)              :: size_host(2)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int16_2 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int16_2) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int16_2)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int16_2 = omp_correctly_mapped_int16_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int16_2

      module function omp_correctly_mapped_int16_3(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int16_3
         integer(I2P), intent(in)  :: array(:,:,:)
         integer(I4P), parameter   :: array_rank=3_I4P
         integer(I4P)              :: size_host(3)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int16_3 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int16_3) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int16_3)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int16_3 = omp_correctly_mapped_int16_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int16_3

      module function omp_correctly_mapped_int16_4(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int16_4
         integer(I2P), intent(in)  :: array(:,:,:,:)
         integer(I4P), parameter   :: array_rank=4_I4P
         integer(I4P)              :: size_host(4)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int16_4 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int16_4) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int16_4)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int16_4 = omp_correctly_mapped_int16_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int16_4

      module function omp_correctly_mapped_int16_5(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int16_5
         integer(I2P), intent(in)  :: array(:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=5_I4P
         integer(I4P)              :: size_host(5)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int16_5 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int16_5) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int16_5)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int16_5 = omp_correctly_mapped_int16_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int16_5

      module function omp_correctly_mapped_int16_6(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int16_6
         integer(I2P), intent(in)  :: array(:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=6_I4P
         integer(I4P)              :: size_host(6)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int16_6 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int16_6) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int16_6)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int16_6 = omp_correctly_mapped_int16_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int16_6

      module function omp_correctly_mapped_int16_7(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int16_7
         integer(I2P), intent(in)  :: array(:,:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=7_I4P
         integer(I4P)              :: size_host(7)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int16_7 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int16_7) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int16_7)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int16_7 = omp_correctly_mapped_int16_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int16_7

      module function omp_correctly_mapped_int32_1(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int32_1
         integer(I4P), intent(in)  :: array(:)
         integer(I4P)              :: size_host
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         size_host = size(array,1)

         omp_correctly_mapped_int32_1 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int32_1) map(to:size_host)
         if (size(array,1)/=size_host) omp_correctly_mapped_int32_1 = omp_correctly_mapped_int32_1 .and. .false.
!$omp end target
      endfunction omp_correctly_mapped_int32_1

      module function omp_correctly_mapped_int32_2(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int32_2
         integer(I4P), intent(in)  :: array(:,:)
         integer(I4P), parameter   :: array_rank=2_I4P
         integer(I4P)              :: size_host(2)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int32_2 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int32_2) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int32_2)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int32_2 = omp_correctly_mapped_int32_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int32_2

      module function omp_correctly_mapped_int32_3(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int32_3
         integer(I4P), intent(in)  :: array(:,:,:)
         integer(I4P), parameter   :: array_rank=3_I4P
         integer(I4P)              :: size_host(3)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int32_3 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int32_3) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int32_3)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int32_3 = omp_correctly_mapped_int32_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int32_3

      module function omp_correctly_mapped_int32_4(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int32_4
         integer(I4P), intent(in)  :: array(:,:,:,:)
         integer(I4P), parameter   :: array_rank=4_I4P
         integer(I4P)              :: size_host(4)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int32_4 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int32_4) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int32_4)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int32_4 = omp_correctly_mapped_int32_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int32_4

      module function omp_correctly_mapped_int32_5(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int32_5
         integer(I4P), intent(in)  :: array(:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=5_I4P
         integer(I4P)              :: size_host(5)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int32_5 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int32_5) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int32_5)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int32_5 = omp_correctly_mapped_int32_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int32_5

      module function omp_correctly_mapped_int32_6(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int32_6
         integer(I4P), intent(in)  :: array(:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=6_I4P
         integer(I4P)              :: size_host(6)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int32_6 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int32_6) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int32_6)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int32_6 = omp_correctly_mapped_int32_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int32_6

      module function omp_correctly_mapped_int32_7(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int32_7
         integer(I4P), intent(in)  :: array(:,:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=7_I4P
         integer(I4P)              :: size_host(7)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int32_7 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int32_7) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int32_7)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int32_7 = omp_correctly_mapped_int32_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int32_7

      module function omp_correctly_mapped_int64_1(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int64_1
         integer(I8P), intent(in)  :: array(:)
         integer(I4P)              :: size_host
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         size_host = size(array,1)

         omp_correctly_mapped_int64_1 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int64_1) map(to:size_host)
         if (size(array,1)/=size_host) omp_correctly_mapped_int64_1 = omp_correctly_mapped_int64_1 .and. .false.
!$omp end target
      endfunction omp_correctly_mapped_int64_1

      module function omp_correctly_mapped_int64_2(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int64_2
         integer(I8P), intent(in)  :: array(:,:)
         integer(I4P), parameter   :: array_rank=2_I4P
         integer(I4P)              :: size_host(2)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int64_2 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int64_2) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int64_2)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int64_2 = omp_correctly_mapped_int64_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int64_2

      module function omp_correctly_mapped_int64_3(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int64_3
         integer(I8P), intent(in)  :: array(:,:,:)
         integer(I4P), parameter   :: array_rank=3_I4P
         integer(I4P)              :: size_host(3)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int64_3 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int64_3) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int64_3)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int64_3 = omp_correctly_mapped_int64_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int64_3

      module function omp_correctly_mapped_int64_4(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int64_4
         integer(I8P), intent(in)  :: array(:,:,:,:)
         integer(I4P), parameter   :: array_rank=4_I4P
         integer(I4P)              :: size_host(4)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int64_4 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int64_4) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int64_4)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int64_4 = omp_correctly_mapped_int64_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int64_4

      module function omp_correctly_mapped_int64_5(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int64_5
         integer(I8P), intent(in)  :: array(:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=5_I4P
         integer(I4P)              :: size_host(5)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int64_5 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int64_5) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int64_5)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int64_5 = omp_correctly_mapped_int64_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int64_5

      module function omp_correctly_mapped_int64_6(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int64_6
         integer(I8P), intent(in)  :: array(:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=6_I4P
         integer(I4P)              :: size_host(6)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int64_6 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int64_6) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int64_6)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int64_6 = omp_correctly_mapped_int64_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int64_6

      module function omp_correctly_mapped_int64_7(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_int64_7
         integer(I8P), intent(in)  :: array(:,:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=7_I4P
         integer(I4P)              :: size_host(7)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_int64_7 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_int64_7) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_int64_7)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_int64_7 = omp_correctly_mapped_int64_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_int64_7


      ! OpenMP Target Alloc Real Routines
      module function omp_correctly_mapped_real32_1(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real32_1
         real(R4P), intent(in)     :: array(:)
         integer(I4P)              :: size_host
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         size_host = size(array,1)

         omp_correctly_mapped_real32_1 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real32_1) map(to:size_host)
         if (size(array,1)/=size_host) omp_correctly_mapped_real32_1 = omp_correctly_mapped_real32_1 .and. .false.
!$omp end target
      endfunction omp_correctly_mapped_real32_1

      module function omp_correctly_mapped_real32_2(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real32_2
         real(R4P), intent(in)     :: array(:,:)
         integer(I4P), parameter   :: array_rank=2_I4P
         integer(I4P)              :: size_host(2)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real32_2 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real32_2) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real32_2)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real32_2 = omp_correctly_mapped_real32_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_real32_2

      module function omp_correctly_mapped_real32_3(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real32_3
         real(R4P), intent(in)     :: array(:,:,:)
         integer(I4P), parameter   :: array_rank=3_I4P
         integer(I4P)              :: size_host(3)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real32_3 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real32_3) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real32_3)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real32_3 = omp_correctly_mapped_real32_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_real32_3

      module function omp_correctly_mapped_real32_4(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real32_4
         real(R4P), intent(in)     :: array(:,:,:,:)
         integer(I4P), parameter   :: array_rank=4_I4P
         integer(I4P)              :: size_host(4)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real32_4 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real32_4) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real32_4)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real32_4 = omp_correctly_mapped_real32_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_real32_4

      module function omp_correctly_mapped_real32_5(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real32_5
         real(R4P), intent(in)     :: array(:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=5_I4P
         integer(I4P)              :: size_host(5)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real32_5 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real32_5) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real32_5)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real32_5 = omp_correctly_mapped_real32_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_real32_5

      module function omp_correctly_mapped_real32_6(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real32_6
         real(R4P), intent(in)     :: array(:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=6_I4P
         integer(I4P)              :: size_host(6)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real32_6 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real32_6) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real32_6)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real32_6 = omp_correctly_mapped_real32_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_real32_6

      module function omp_correctly_mapped_real32_7(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real32_7
         real(R4P), intent(in)     :: array(:,:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=7_I4P
         integer(I4P)              :: size_host(7)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real32_7 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real32_7) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real32_7)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real32_7 = omp_correctly_mapped_real32_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_real32_7

      module function omp_correctly_mapped_real64_1(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real64_1
         real(R8P), intent(in)     :: array(:)
         integer(I4P)              :: size_host
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         size_host = size(array,1)

         omp_correctly_mapped_real64_1 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real64_1) map(to:size_host)
         if (size(array,1)/=size_host) omp_correctly_mapped_real64_1 = omp_correctly_mapped_real64_1 .and. .false.
!$omp end target
      endfunction omp_correctly_mapped_real64_1

      module function omp_correctly_mapped_real64_2(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real64_2
         real(R8P), intent(in)     :: array(:,:)
         integer(I4P), parameter   :: array_rank=2_I4P
         integer(I4P)              :: size_host(2)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real64_2 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real64_2) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real64_2)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real64_2 = omp_correctly_mapped_real64_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_real64_2

      module function omp_correctly_mapped_real64_3(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real64_3
         real(R8P), intent(in)     :: array(:,:,:)
         integer(I4P), parameter   :: array_rank=3_I4P
         integer(I4P)              :: size_host(3)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real64_3 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real64_3) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real64_3)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real64_3 = omp_correctly_mapped_real64_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_real64_3

      module function omp_correctly_mapped_real64_4(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real64_4
         real(R8P), intent(in)     :: array(:,:,:,:)
         integer(I4P), parameter   :: array_rank=4_I4P
         integer(I4P)              :: size_host(4)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real64_4 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real64_4) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real64_4)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real64_4 = omp_correctly_mapped_real64_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_real64_4

      module function omp_correctly_mapped_real64_5(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real64_5
         real(R8P), intent(in)     :: array(:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=5_I4P
         integer(I4P)              :: size_host(5)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real64_5 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real64_5) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real64_5)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real64_5 = omp_correctly_mapped_real64_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_real64_5

      module function omp_correctly_mapped_real64_6(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real64_6
         real(R8P), intent(in)     :: array(:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=6_I4P
         integer(I4P)              :: size_host(6)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real64_6 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real64_6) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real64_6)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real64_6 = omp_correctly_mapped_real64_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_real64_6

      module function omp_correctly_mapped_real64_7(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real64_7
         real(R8P), intent(in)     :: array(:,:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=7_I4P
         integer(I4P)              :: size_host(7)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real64_7 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real64_7) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real64_7)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real64_7 = omp_correctly_mapped_real64_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_real64_7

#if defined _real128
      module function omp_correctly_mapped_real128_1(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real128_1
         real(R16P), intent(in)    :: array(:)
         integer(I4P)              :: size_host
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         size_host = size(array,1)

         omp_correctly_mapped_real128_1 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real128_1) map(to:size_host)
         if (size(array,1)/=size_host) omp_correctly_mapped_real128_1 = omp_correctly_mapped_real128_1 .and. .false.
!$omp end target
      endfunction omp_correctly_mapped_real128_1

      module function omp_correctly_mapped_real128_2(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real128_2
         real(R16P), intent(in)    :: array(:,:)
         integer(I4P), parameter   :: array_rank=2_I4P
         integer(I4P)              :: size_host(2)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real128_2 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real128_2) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real128_2)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real128_2 = omp_correctly_mapped_real128_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_real128_2

      module function omp_correctly_mapped_real128_3(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real128_3
         real(R16P), intent(in)    :: array(:,:,:)
         integer(I4P), parameter   :: array_rank=3_I4P
         integer(I4P)              :: size_host(3)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real128_3 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real128_3) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real128_3)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real128_3 = omp_correctly_mapped_real128_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_real128_3

      module function omp_correctly_mapped_real128_4(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real128_4
         real(R16P), intent(in)    :: array(:,:,:,:)
         integer(I4P), parameter   :: array_rank=4_I4P
         integer(I4P)              :: size_host(4)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real128_4 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real128_4) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real128_4)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real128_4 = omp_correctly_mapped_real128_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_real128_4

      module function omp_correctly_mapped_real128_5(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real128_5
         real(R16P), intent(in)    :: array(:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=5_I4P
         integer(I4P)              :: size_host(5)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real128_5 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real128_5) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real128_5)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real128_5 = omp_correctly_mapped_real128_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_real128_5

      module function omp_correctly_mapped_real128_6(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real128_6
         real(R16P), intent(in)    :: array(:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=6_I4P
         integer(I4P)              :: size_host(6)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real128_6 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real128_6) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real128_6)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real128_6 = omp_correctly_mapped_real128_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_real128_6

      module function omp_correctly_mapped_real128_7(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_real128_7
         real(R16P), intent(in)    :: array(:,:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=7_I4P
         integer(I4P)              :: size_host(7)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_real128_7 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_real128_7) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_real128_7)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_real128_7 = omp_correctly_mapped_real128_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_real128_7

#endif

      ! OpenMP Target Alloc Complex Routines
      module function omp_correctly_mapped_cmplx32_1(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx32_1
         complex(R4P), intent(in)  :: array(:)
         integer(I4P)              :: size_host
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         size_host = size(array,1)

         omp_correctly_mapped_cmplx32_1 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx32_1) map(to:size_host)
         if (size(array,1)/=size_host) omp_correctly_mapped_cmplx32_1 = omp_correctly_mapped_cmplx32_1 .and. .false.
!$omp end target
      endfunction omp_correctly_mapped_cmplx32_1

      module function omp_correctly_mapped_cmplx32_2(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx32_2
         complex(R4P), intent(in)  :: array(:,:)
         integer(I4P), parameter   :: array_rank=2_I4P
         integer(I4P)              :: size_host(2)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx32_2 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx32_2) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx32_2)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx32_2 = omp_correctly_mapped_cmplx32_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_cmplx32_2

      module function omp_correctly_mapped_cmplx32_3(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx32_3
         complex(R4P), intent(in)  :: array(:,:,:)
         integer(I4P), parameter   :: array_rank=3_I4P
         integer(I4P)              :: size_host(3)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx32_3 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx32_3) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx32_3)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx32_3 = omp_correctly_mapped_cmplx32_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_cmplx32_3

      module function omp_correctly_mapped_cmplx32_4(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx32_4
         complex(R4P), intent(in)  :: array(:,:,:,:)
         integer(I4P), parameter   :: array_rank=4_I4P
         integer(I4P)              :: size_host(4)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx32_4 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx32_4) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx32_4)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx32_4 = omp_correctly_mapped_cmplx32_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_cmplx32_4

      module function omp_correctly_mapped_cmplx32_5(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx32_5
         complex(R4P), intent(in)  :: array(:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=5_I4P
         integer(I4P)              :: size_host(5)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx32_5 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx32_5) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx32_5)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx32_5 = omp_correctly_mapped_cmplx32_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_cmplx32_5

      module function omp_correctly_mapped_cmplx32_6(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx32_6
         complex(R4P), intent(in)  :: array(:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=6_I4P
         integer(I4P)              :: size_host(6)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx32_6 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx32_6) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx32_6)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx32_6 = omp_correctly_mapped_cmplx32_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_cmplx32_6

      module function omp_correctly_mapped_cmplx32_7(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx32_7
         complex(R4P), intent(in)  :: array(:,:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=7_I4P
         integer(I4P)              :: size_host(7)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx32_7 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx32_7) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx32_7)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx32_7 = omp_correctly_mapped_cmplx32_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_cmplx32_7

      module function omp_correctly_mapped_cmplx64_1(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx64_1
         complex(R8P), intent(in)  :: array(:)
         integer(I4P)              :: size_host
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         size_host = size(array,1)

         omp_correctly_mapped_cmplx64_1 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx64_1) map(to:size_host)
         if (size(array,1)/=size_host) omp_correctly_mapped_cmplx64_1 = omp_correctly_mapped_cmplx64_1 .and. .false.
!$omp end target
      endfunction omp_correctly_mapped_cmplx64_1

      module function omp_correctly_mapped_cmplx64_2(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx64_2
         complex(R8P), intent(in)  :: array(:,:)
         integer(I4P), parameter   :: array_rank=2_I4P
         integer(I4P)              :: size_host(2)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx64_2 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx64_2) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx64_2)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx64_2 = omp_correctly_mapped_cmplx64_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_cmplx64_2

      module function omp_correctly_mapped_cmplx64_3(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx64_3
         complex(R8P), intent(in)  :: array(:,:,:)
         integer(I4P), parameter   :: array_rank=3_I4P
         integer(I4P)              :: size_host(3)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx64_3 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx64_3) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx64_3)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx64_3 = omp_correctly_mapped_cmplx64_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_cmplx64_3

      module function omp_correctly_mapped_cmplx64_4(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx64_4
         complex(R8P), intent(in)  :: array(:,:,:,:)
         integer(I4P), parameter   :: array_rank=4_I4P
         integer(I4P)              :: size_host(4)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx64_4 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx64_4) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx64_4)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx64_4 = omp_correctly_mapped_cmplx64_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_cmplx64_4

      module function omp_correctly_mapped_cmplx64_5(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx64_5
         complex(R8P), intent(in)  :: array(:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=5_I4P
         integer(I4P)              :: size_host(5)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx64_5 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx64_5) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx64_5)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx64_5 = omp_correctly_mapped_cmplx64_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_cmplx64_5

      module function omp_correctly_mapped_cmplx64_6(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx64_6
         complex(R8P), intent(in)  :: array(:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=6_I4P
         integer(I4P)              :: size_host(6)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx64_6 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx64_6) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx64_6)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx64_6 = omp_correctly_mapped_cmplx64_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_cmplx64_6

      module function omp_correctly_mapped_cmplx64_7(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx64_7
         complex(R8P), intent(in)  :: array(:,:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=7_I4P
         integer(I4P)              :: size_host(7)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx64_7 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx64_7) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx64_7)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx64_7 = omp_correctly_mapped_cmplx64_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_cmplx64_7

#if defined _real128
      module function omp_correctly_mapped_cmplx128_1(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx128_1
         complex(R16P), intent(in) :: array(:)
         integer(I4P)              :: size_host
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         size_host = size(array,1)

         omp_correctly_mapped_cmplx128_1 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx128_1) map(to:size_host)
         if (size(array,1)/=size_host) omp_correctly_mapped_cmplx128_1 = omp_correctly_mapped_cmplx128_1 .and. .false.
!$omp end target
      endfunction omp_correctly_mapped_cmplx128_1

      module function omp_correctly_mapped_cmplx128_2(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx128_2
         complex(R16P), intent(in) :: array(:,:)
         integer(I4P), parameter   :: array_rank=2_I4P
         integer(I4P)              :: size_host(2)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx128_2 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx128_2) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx128_2)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx128_2 = omp_correctly_mapped_cmplx128_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_cmplx128_2

      module function omp_correctly_mapped_cmplx128_3(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx128_3
         complex(R16P), intent(in) :: array(:,:,:)
         integer(I4P), parameter   :: array_rank=3_I4P
         integer(I4P)              :: size_host(3)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx128_3 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx128_3) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx128_3)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx128_3 = omp_correctly_mapped_cmplx128_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_cmplx128_3

      module function omp_correctly_mapped_cmplx128_4(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx128_4
         complex(R16P), intent(in) :: array(:,:,:,:)
         integer(I4P), parameter   :: array_rank=4_I4P
         integer(I4P)              :: size_host(4)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx128_4 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx128_4) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx128_4)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx128_4 = omp_correctly_mapped_cmplx128_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_cmplx128_4

      module function omp_correctly_mapped_cmplx128_5(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx128_5
         complex(R16P), intent(in) :: array(:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=5_I4P
         integer(I4P)              :: size_host(5)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx128_5 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx128_5) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx128_5)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx128_5 = omp_correctly_mapped_cmplx128_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_cmplx128_5

      module function omp_correctly_mapped_cmplx128_6(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx128_6
         complex(R16P), intent(in) :: array(:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=6_I4P
         integer(I4P)              :: size_host(6)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx128_6 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx128_6) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx128_6)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx128_6 = omp_correctly_mapped_cmplx128_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_cmplx128_6

      module function omp_correctly_mapped_cmplx128_7(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_cmplx128_7
         complex(R16P), intent(in) :: array(:,:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=7_I4P
         integer(I4P)              :: size_host(7)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_cmplx128_7 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_cmplx128_7) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_cmplx128_7)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_cmplx128_7 = omp_correctly_mapped_cmplx128_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_cmplx128_7

#endif

      ! OpenMP Target Alloc Logical Routines
      module function omp_correctly_mapped_lgcl32_1(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_lgcl32_1
         logical(I4P), intent(in)  :: array(:)
         integer(I4P)              :: size_host
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         size_host = size(array,1)

         omp_correctly_mapped_lgcl32_1 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_lgcl32_1) map(to:size_host)
         if (size(array,1)/=size_host) omp_correctly_mapped_lgcl32_1 = omp_correctly_mapped_lgcl32_1 .and. .false.
!$omp end target
      endfunction omp_correctly_mapped_lgcl32_1

      module function omp_correctly_mapped_lgcl32_2(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_lgcl32_2
         logical(I4P), intent(in)  :: array(:,:)
         integer(I4P), parameter   :: array_rank=2_I4P
         integer(I4P)              :: size_host(2)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_lgcl32_2 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_lgcl32_2) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_lgcl32_2)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_lgcl32_2 = omp_correctly_mapped_lgcl32_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_lgcl32_2

      module function omp_correctly_mapped_lgcl32_3(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_lgcl32_3
         logical(I4P), intent(in)  :: array(:,:,:)
         integer(I4P), parameter   :: array_rank=3_I4P
         integer(I4P)              :: size_host(3)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_lgcl32_3 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_lgcl32_3) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_lgcl32_3)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_lgcl32_3 = omp_correctly_mapped_lgcl32_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_lgcl32_3

      module function omp_correctly_mapped_lgcl32_4(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_lgcl32_4
         logical(I4P), intent(in)  :: array(:,:,:,:)
         integer(I4P), parameter   :: array_rank=4_I4P
         integer(I4P)              :: size_host(4)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_lgcl32_4 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_lgcl32_4) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_lgcl32_4)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_lgcl32_4 = omp_correctly_mapped_lgcl32_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_lgcl32_4

      module function omp_correctly_mapped_lgcl32_5(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_lgcl32_5
         logical(I4P), intent(in)  :: array(:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=5_I4P
         integer(I4P)              :: size_host(5)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_lgcl32_5 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_lgcl32_5) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_lgcl32_5)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_lgcl32_5 = omp_correctly_mapped_lgcl32_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_lgcl32_5

      module function omp_correctly_mapped_lgcl32_6(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_lgcl32_6
         logical(I4P), intent(in)  :: array(:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=6_I4P
         integer(I4P)              :: size_host(6)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_lgcl32_6 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_lgcl32_6) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_lgcl32_6)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_lgcl32_6 = omp_correctly_mapped_lgcl32_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_lgcl32_6

      module function omp_correctly_mapped_lgcl32_7(array, omp_dev)
         implicit none
         logical                   :: omp_correctly_mapped_lgcl32_7
         logical(I4P), intent(in)  :: array(:,:,:,:,:,:,:)
         integer(I4P), parameter   :: array_rank=7_I4P
         integer(I4P)              :: size_host(7)
         integer(I4P), intent(in)  :: omp_dev
         integer(I4P)              :: i

         do i=1, array_rank
            size_host(i) = size(array,i)
         enddo

         omp_correctly_mapped_lgcl32_7 = .true.
!$omp target device(omp_dev) map(tofrom:omp_correctly_mapped_lgcl32_7) map(to:size_host)
!$omp teams distribute parallel do reduction(.and.:omp_correctly_mapped_lgcl32_7)
         do i=1, array_rank
            if (size(array,i)/=size_host(i)) omp_correctly_mapped_lgcl32_7 = omp_correctly_mapped_lgcl32_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction omp_correctly_mapped_lgcl32_7



#endif

endsubmodule dmr_correctly_mapped