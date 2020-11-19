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
   use omp_lib
   use dmr_environment
   use dmr_c_functions

   implicit none

   contains

      ! OpenMP Check Mapped Integer Routines
      module function dmr_correctly_mapped_int8_1(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int8_1
         integer(I1P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int8_1 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int8_1, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int8_1)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int8_1 = dmr_correctly_mapped_int8_1 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int8_1

      module function dmr_correctly_mapped_int8_2(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int8_2
         integer(I1P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int8_2 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int8_2, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int8_2)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int8_2 = dmr_correctly_mapped_int8_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int8_2

      module function dmr_correctly_mapped_int8_3(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int8_3
         integer(I1P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int8_3 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int8_3, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int8_3)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int8_3 = dmr_correctly_mapped_int8_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int8_3

      module function dmr_correctly_mapped_int8_4(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int8_4
         integer(I1P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int8_4 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int8_4, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int8_4)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int8_4 = dmr_correctly_mapped_int8_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int8_4

      module function dmr_correctly_mapped_int8_5(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int8_5
         integer(I1P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int8_5 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int8_5, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int8_5)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int8_5 = dmr_correctly_mapped_int8_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int8_5

      module function dmr_correctly_mapped_int8_6(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int8_6
         integer(I1P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int8_6 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int8_6, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int8_6)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int8_6 = dmr_correctly_mapped_int8_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int8_6

      module function dmr_correctly_mapped_int8_7(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int8_7
         integer(I1P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int8_7 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int8_7, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int8_7)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int8_7 = dmr_correctly_mapped_int8_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int8_7

      module function dmr_correctly_mapped_int16_1(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int16_1
         integer(I2P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int16_1 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int16_1, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int16_1)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int16_1 = dmr_correctly_mapped_int16_1 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int16_1

      module function dmr_correctly_mapped_int16_2(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int16_2
         integer(I2P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int16_2 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int16_2, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int16_2)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int16_2 = dmr_correctly_mapped_int16_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int16_2

      module function dmr_correctly_mapped_int16_3(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int16_3
         integer(I2P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int16_3 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int16_3, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int16_3)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int16_3 = dmr_correctly_mapped_int16_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int16_3

      module function dmr_correctly_mapped_int16_4(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int16_4
         integer(I2P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int16_4 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int16_4, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int16_4)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int16_4 = dmr_correctly_mapped_int16_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int16_4

      module function dmr_correctly_mapped_int16_5(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int16_5
         integer(I2P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int16_5 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int16_5, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int16_5)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int16_5 = dmr_correctly_mapped_int16_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int16_5

      module function dmr_correctly_mapped_int16_6(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int16_6
         integer(I2P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int16_6 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int16_6, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int16_6)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int16_6 = dmr_correctly_mapped_int16_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int16_6

      module function dmr_correctly_mapped_int16_7(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int16_7
         integer(I2P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int16_7 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int16_7, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int16_7)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int16_7 = dmr_correctly_mapped_int16_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int16_7

      module function dmr_correctly_mapped_int32_1(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int32_1
         integer(I4P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int32_1 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int32_1, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int32_1)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int32_1 = dmr_correctly_mapped_int32_1 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int32_1

      module function dmr_correctly_mapped_int32_2(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int32_2
         integer(I4P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int32_2 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int32_2, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int32_2)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int32_2 = dmr_correctly_mapped_int32_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int32_2

      module function dmr_correctly_mapped_int32_3(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int32_3
         integer(I4P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int32_3 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int32_3, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int32_3)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int32_3 = dmr_correctly_mapped_int32_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int32_3

      module function dmr_correctly_mapped_int32_4(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int32_4
         integer(I4P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int32_4 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int32_4, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int32_4)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int32_4 = dmr_correctly_mapped_int32_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int32_4

      module function dmr_correctly_mapped_int32_5(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int32_5
         integer(I4P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int32_5 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int32_5, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int32_5)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int32_5 = dmr_correctly_mapped_int32_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int32_5

      module function dmr_correctly_mapped_int32_6(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int32_6
         integer(I4P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int32_6 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int32_6, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int32_6)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int32_6 = dmr_correctly_mapped_int32_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int32_6

      module function dmr_correctly_mapped_int32_7(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int32_7
         integer(I4P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int32_7 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int32_7, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int32_7)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int32_7 = dmr_correctly_mapped_int32_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int32_7

      module function dmr_correctly_mapped_int64_1(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int64_1
         integer(I8P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int64_1 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int64_1, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int64_1)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int64_1 = dmr_correctly_mapped_int64_1 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int64_1

      module function dmr_correctly_mapped_int64_2(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int64_2
         integer(I8P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int64_2 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int64_2, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int64_2)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int64_2 = dmr_correctly_mapped_int64_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int64_2

      module function dmr_correctly_mapped_int64_3(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int64_3
         integer(I8P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int64_3 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int64_3, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int64_3)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int64_3 = dmr_correctly_mapped_int64_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int64_3

      module function dmr_correctly_mapped_int64_4(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int64_4
         integer(I8P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int64_4 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int64_4, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int64_4)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int64_4 = dmr_correctly_mapped_int64_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int64_4

      module function dmr_correctly_mapped_int64_5(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int64_5
         integer(I8P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int64_5 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int64_5, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int64_5)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int64_5 = dmr_correctly_mapped_int64_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int64_5

      module function dmr_correctly_mapped_int64_6(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int64_6
         integer(I8P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int64_6 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int64_6, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int64_6)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int64_6 = dmr_correctly_mapped_int64_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int64_6

      module function dmr_correctly_mapped_int64_7(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int64_7
         integer(I8P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_int64_7 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_int64_7, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_int64_7)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_int64_7 = dmr_correctly_mapped_int64_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_int64_7

      ! OpenMP Check Mapped Real Routines
      module function dmr_correctly_mapped_real32_1(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real32_1
         real(R4P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real32_1 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real32_1, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real32_1)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real32_1 = dmr_correctly_mapped_real32_1 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real32_1

      module function dmr_correctly_mapped_real32_2(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real32_2
         real(R4P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real32_2 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real32_2, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real32_2)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real32_2 = dmr_correctly_mapped_real32_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real32_2

      module function dmr_correctly_mapped_real32_3(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real32_3
         real(R4P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real32_3 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real32_3, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real32_3)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real32_3 = dmr_correctly_mapped_real32_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real32_3

      module function dmr_correctly_mapped_real32_4(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real32_4
         real(R4P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real32_4 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real32_4, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real32_4)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real32_4 = dmr_correctly_mapped_real32_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real32_4

      module function dmr_correctly_mapped_real32_5(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real32_5
         real(R4P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real32_5 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real32_5, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real32_5)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real32_5 = dmr_correctly_mapped_real32_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real32_5

      module function dmr_correctly_mapped_real32_6(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real32_6
         real(R4P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real32_6 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real32_6, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real32_6)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real32_6 = dmr_correctly_mapped_real32_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real32_6

      module function dmr_correctly_mapped_real32_7(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real32_7
         real(R4P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real32_7 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real32_7, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real32_7)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real32_7 = dmr_correctly_mapped_real32_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real32_7

      module function dmr_correctly_mapped_real64_1(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real64_1
         real(R8P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real64_1 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real64_1, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real64_1)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real64_1 = dmr_correctly_mapped_real64_1 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real64_1

      module function dmr_correctly_mapped_real64_2(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real64_2
         real(R8P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real64_2 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real64_2, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real64_2)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real64_2 = dmr_correctly_mapped_real64_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real64_2

      module function dmr_correctly_mapped_real64_3(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real64_3
         real(R8P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real64_3 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real64_3, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real64_3)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real64_3 = dmr_correctly_mapped_real64_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real64_3

      module function dmr_correctly_mapped_real64_4(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real64_4
         real(R8P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real64_4 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real64_4, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real64_4)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real64_4 = dmr_correctly_mapped_real64_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real64_4

      module function dmr_correctly_mapped_real64_5(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real64_5
         real(R8P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real64_5 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real64_5, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real64_5)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real64_5 = dmr_correctly_mapped_real64_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real64_5

      module function dmr_correctly_mapped_real64_6(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real64_6
         real(R8P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real64_6 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real64_6, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real64_6)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real64_6 = dmr_correctly_mapped_real64_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real64_6

      module function dmr_correctly_mapped_real64_7(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real64_7
         real(R8P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real64_7 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real64_7, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real64_7)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real64_7 = dmr_correctly_mapped_real64_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real64_7

#if defined _real128
      module function dmr_correctly_mapped_real128_1(array, omp_dev)
         implicit none
         logical                 :: dmr_correctly_mapped_real128_1
         real(R16P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real128_1 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real128_1, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real128_1)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real128_1 = dmr_correctly_mapped_real128_1 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real128_1

      module function dmr_correctly_mapped_real128_2(array, omp_dev)
         implicit none
         logical                 :: dmr_correctly_mapped_real128_2
         real(R16P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real128_2 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real128_2, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real128_2)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real128_2 = dmr_correctly_mapped_real128_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real128_2

      module function dmr_correctly_mapped_real128_3(array, omp_dev)
         implicit none
         logical                 :: dmr_correctly_mapped_real128_3
         real(R16P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real128_3 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real128_3, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real128_3)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real128_3 = dmr_correctly_mapped_real128_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real128_3

      module function dmr_correctly_mapped_real128_4(array, omp_dev)
         implicit none
         logical                 :: dmr_correctly_mapped_real128_4
         real(R16P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real128_4 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real128_4, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real128_4)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real128_4 = dmr_correctly_mapped_real128_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real128_4

      module function dmr_correctly_mapped_real128_5(array, omp_dev)
         implicit none
         logical                 :: dmr_correctly_mapped_real128_5
         real(R16P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real128_5 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real128_5, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real128_5)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real128_5 = dmr_correctly_mapped_real128_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real128_5

      module function dmr_correctly_mapped_real128_6(array, omp_dev)
         implicit none
         logical                 :: dmr_correctly_mapped_real128_6
         real(R16P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real128_6 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real128_6, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real128_6)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real128_6 = dmr_correctly_mapped_real128_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real128_6

      module function dmr_correctly_mapped_real128_7(array, omp_dev)
         implicit none
         logical                 :: dmr_correctly_mapped_real128_7
         real(R16P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_real128_7 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_real128_7, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_real128_7)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_real128_7 = dmr_correctly_mapped_real128_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_real128_7
#endif

      ! OpenMP Check Mapped Complex Routines
      module function dmr_correctly_mapped_cmplx32_1(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx32_1
         complex(R4P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx32_1 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx32_1, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx32_1)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx32_1 = dmr_correctly_mapped_cmplx32_1 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx32_1

      module function dmr_correctly_mapped_cmplx32_2(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx32_2
         complex(R4P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx32_2 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx32_2, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx32_2)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx32_2 = dmr_correctly_mapped_cmplx32_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx32_2

      module function dmr_correctly_mapped_cmplx32_3(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx32_3
         complex(R4P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx32_3 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx32_3, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx32_3)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx32_3 = dmr_correctly_mapped_cmplx32_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx32_3

      module function dmr_correctly_mapped_cmplx32_4(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx32_4
         complex(R4P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx32_4 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx32_4, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx32_4)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx32_4 = dmr_correctly_mapped_cmplx32_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx32_4

      module function dmr_correctly_mapped_cmplx32_5(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx32_5
         complex(R4P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx32_5 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx32_5, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx32_5)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx32_5 = dmr_correctly_mapped_cmplx32_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx32_5

      module function dmr_correctly_mapped_cmplx32_6(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx32_6
         complex(R4P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx32_6 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx32_6, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx32_6)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx32_6 = dmr_correctly_mapped_cmplx32_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx32_6

      module function dmr_correctly_mapped_cmplx32_7(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx32_7
         complex(R4P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx32_7 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx32_7, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx32_7)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx32_7 = dmr_correctly_mapped_cmplx32_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx32_7

      module function dmr_correctly_mapped_cmplx64_1(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx64_1
         complex(R8P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx64_1 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx64_1, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx64_1)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx64_1 = dmr_correctly_mapped_cmplx64_1 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx64_1

      module function dmr_correctly_mapped_cmplx64_2(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx64_2
         complex(R8P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx64_2 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx64_2, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx64_2)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx64_2 = dmr_correctly_mapped_cmplx64_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx64_2

      module function dmr_correctly_mapped_cmplx64_3(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx64_3
         complex(R8P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx64_3 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx64_3, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx64_3)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx64_3 = dmr_correctly_mapped_cmplx64_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx64_3

      module function dmr_correctly_mapped_cmplx64_4(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx64_4
         complex(R8P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx64_4 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx64_4, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx64_4)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx64_4 = dmr_correctly_mapped_cmplx64_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx64_4

      module function dmr_correctly_mapped_cmplx64_5(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx64_5
         complex(R8P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx64_5 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx64_5, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx64_5)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx64_5 = dmr_correctly_mapped_cmplx64_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx64_5

      module function dmr_correctly_mapped_cmplx64_6(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx64_6
         complex(R8P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx64_6 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx64_6, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx64_6)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx64_6 = dmr_correctly_mapped_cmplx64_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx64_6

      module function dmr_correctly_mapped_cmplx64_7(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx64_7
         complex(R8P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx64_7 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx64_7, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx64_7)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx64_7 = dmr_correctly_mapped_cmplx64_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx64_7

#if defined _real128
      module function dmr_correctly_mapped_cmplx128_1(array, omp_dev)
         implicit none
         logical                    :: dmr_correctly_mapped_cmplx128_1
         complex(R16P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx128_1 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx128_1, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx128_1)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx128_1 = dmr_correctly_mapped_cmplx128_1 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx128_1

      module function dmr_correctly_mapped_cmplx128_2(array, omp_dev)
         implicit none
         logical                    :: dmr_correctly_mapped_cmplx128_2
         complex(R16P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx128_2 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx128_2, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx128_2)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx128_2 = dmr_correctly_mapped_cmplx128_2 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx128_2

      module function dmr_correctly_mapped_cmplx128_3(array, omp_dev)
         implicit none
         logical                    :: dmr_correctly_mapped_cmplx128_3
         complex(R16P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx128_3 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx128_3, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx128_3)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx128_3 = dmr_correctly_mapped_cmplx128_3 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx128_3

      module function dmr_correctly_mapped_cmplx128_4(array, omp_dev)
         implicit none
         logical                    :: dmr_correctly_mapped_cmplx128_4
         complex(R16P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx128_4 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx128_4, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx128_4)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx128_4 = dmr_correctly_mapped_cmplx128_4 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx128_4

      module function dmr_correctly_mapped_cmplx128_5(array, omp_dev)
         implicit none
         logical                    :: dmr_correctly_mapped_cmplx128_5
         complex(R16P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx128_5 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx128_5, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx128_5)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx128_5 = dmr_correctly_mapped_cmplx128_5 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx128_5

      module function dmr_correctly_mapped_cmplx128_6(array, omp_dev)
         implicit none
         logical                    :: dmr_correctly_mapped_cmplx128_6
         complex(R16P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx128_6 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx128_6, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx128_6)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx128_6 = dmr_correctly_mapped_cmplx128_6 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx128_6

      module function dmr_correctly_mapped_cmplx128_7(array, omp_dev)
         implicit none
         logical                    :: dmr_correctly_mapped_cmplx128_7
         complex(R16P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"

         allocate(size_host(1:rank(array)))
         do i=1, rank(array)
            size_host(i) = size(array,i)
         enddo

         dmr_correctly_mapped_cmplx128_7 = .true.
!$omp target device(omp_dev) map(tofrom:dmr_correctly_mapped_cmplx128_7, size_host)
!$omp teams distribute parallel do reduction(.and.:dmr_correctly_mapped_cmplx128_7)
         do i=1, rank(array)
            if (size(array,i)/=size_host(i)) dmr_correctly_mapped_cmplx128_7 = dmr_correctly_mapped_cmplx128_7 .and. .false.
         enddo
!$omp end teams distribute parallel do
!$omp end target
      endfunction dmr_correctly_mapped_cmplx128_7
#endif

endsubmodule dmr_correctly_mapped
