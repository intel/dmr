!* ========================================================================== *
!*                                                                            *
!* Copyright (C) 2022 Intel Corporation                                       *
!* This file is part of the DMR library.                                      *
!*                                                                            *
!* For information on the license, see the LICENSE file.                      *
!* Further information: https://github.com/giacrossi/dmr/                     *
!* SPDX-License-Identifier: BSD-3-Clause                                      *
!*                                                                            *
!* ========================================================================== *
!* Giacomo Rossi (Intel Corporation)                                          *
!* ========================================================================== *

program test_dmr_target_is_present
   use omp_lib,         only : omp_get_default_device
   use dmr,             only : omp_target_is_present_f
   use dmr_environment

   implicit none
   integer(I1P), allocatable :: host_data_int8_1(:),  host_data_int8_2(:,:),  host_data_int8_3(:,:,:),  host_data_int8_4(:,:,:,:), &
                                        host_data_int8_5(:,:,:,:,:), host_data_int8_6(:,:,:,:,:,:), host_data_int8_7(:,:,:,:,:,:,:)
   integer(I2P), allocatable :: host_data_int16_1(:),  host_data_int16_2(:,:),  host_data_int16_3(:,:,:),  host_data_int16_4(:,:,:,:), &
                                        host_data_int16_5(:,:,:,:,:), host_data_int16_6(:,:,:,:,:,:), host_data_int16_7(:,:,:,:,:,:,:)
   integer(I4P), allocatable :: host_data_int32_1(:),  host_data_int32_2(:,:),  host_data_int32_3(:,:,:),  host_data_int32_4(:,:,:,:), &
                                        host_data_int32_5(:,:,:,:,:), host_data_int32_6(:,:,:,:,:,:), host_data_int32_7(:,:,:,:,:,:,:)
   integer(I8P), allocatable :: host_data_int64_1(:),  host_data_int64_2(:,:),  host_data_int64_3(:,:,:),  host_data_int64_4(:,:,:,:), &
                                        host_data_int64_5(:,:,:,:,:), host_data_int64_6(:,:,:,:,:,:), host_data_int64_7(:,:,:,:,:,:,:)
   real(R4P), allocatable :: host_data_real32_1(:),  host_data_real32_2(:,:),  host_data_real32_3(:,:,:),  host_data_real32_4(:,:,:,:), &
                                        host_data_real32_5(:,:,:,:,:), host_data_real32_6(:,:,:,:,:,:), host_data_real32_7(:,:,:,:,:,:,:)
   real(R8P), allocatable :: host_data_real64_1(:),  host_data_real64_2(:,:),  host_data_real64_3(:,:,:),  host_data_real64_4(:,:,:,:), &
                                        host_data_real64_5(:,:,:,:,:), host_data_real64_6(:,:,:,:,:,:), host_data_real64_7(:,:,:,:,:,:,:)
#if defined _real128
   real(R16P), allocatable :: host_data_real128_1(:),  host_data_real128_2(:,:),  host_data_real128_3(:,:,:),  host_data_real128_4(:,:,:,:), &
                                        host_data_real128_5(:,:,:,:,:), host_data_real128_6(:,:,:,:,:,:), host_data_real128_7(:,:,:,:,:,:,:)
#endif
   complex(R4P), allocatable :: host_data_cmplx32_1(:),  host_data_cmplx32_2(:,:),  host_data_cmplx32_3(:,:,:),  host_data_cmplx32_4(:,:,:,:), &
                                        host_data_cmplx32_5(:,:,:,:,:), host_data_cmplx32_6(:,:,:,:,:,:), host_data_cmplx32_7(:,:,:,:,:,:,:)
   complex(R8P), allocatable :: host_data_cmplx64_1(:),  host_data_cmplx64_2(:,:),  host_data_cmplx64_3(:,:,:),  host_data_cmplx64_4(:,:,:,:), &
                                        host_data_cmplx64_5(:,:,:,:,:), host_data_cmplx64_6(:,:,:,:,:,:), host_data_cmplx64_7(:,:,:,:,:,:,:)
#if defined _real128
   complex(R16P), allocatable :: host_data_cmplx128_1(:),  host_data_cmplx128_2(:,:),  host_data_cmplx128_3(:,:,:),  host_data_cmplx128_4(:,:,:,:), &
                                        host_data_cmplx128_5(:,:,:,:,:), host_data_cmplx128_6(:,:,:,:,:,:), host_data_cmplx128_7(:,:,:,:,:,:,:)
#endif
   logical(I4P), allocatable :: host_data_lgcl32_1(:),  host_data_lgcl32_2(:,:),  host_data_lgcl32_3(:,:,:),  host_data_lgcl32_4(:,:,:,:), &
                                        host_data_lgcl32_5(:,:,:,:,:), host_data_lgcl32_6(:,:,:,:,:,:), host_data_lgcl32_7(:,:,:,:,:,:,:)

   integer(I4P), parameter :: sz = 10
   integer(I4P) :: ierr=0, test_tot=0, test_not=0, nfail=0
   logical      :: pres

   !!! DMR Target Is Present Integer tests
   allocate(host_data_int8_1(sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int8_1, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int8_1)
      pres = omp_target_is_present_f(host_data=host_data_int8_1, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int8_1, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int8_1"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int8_2(sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int8_2, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int8_2)
      pres = omp_target_is_present_f(host_data=host_data_int8_2, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int8_2, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int8_2"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int8_3(sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int8_3, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int8_3)
      pres = omp_target_is_present_f(host_data=host_data_int8_3, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int8_3, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int8_3"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int8_4(sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int8_4, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int8_4)
      pres = omp_target_is_present_f(host_data=host_data_int8_4, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int8_4, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int8_4"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int8_5(sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int8_5, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int8_5)
      pres = omp_target_is_present_f(host_data=host_data_int8_5, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int8_5, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int8_5"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int8_6(sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int8_6, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int8_6)
      pres = omp_target_is_present_f(host_data=host_data_int8_6, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int8_6, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int8_6"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int8_7(sz,sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int8_7, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int8_7)
      pres = omp_target_is_present_f(host_data=host_data_int8_7, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int8_7, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int8_7"
         nfail = nfail + 1
      endif
   endif

   !!! DMR Target Is Present Integer tests
   allocate(host_data_int16_1(sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int16_1, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int16_1)
      pres = omp_target_is_present_f(host_data=host_data_int16_1, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int16_1, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int16_1"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int16_2(sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int16_2, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int16_2)
      pres = omp_target_is_present_f(host_data=host_data_int16_2, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int16_2, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int16_2"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int16_3(sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int16_3, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int16_3)
      pres = omp_target_is_present_f(host_data=host_data_int16_3, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int16_3, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int16_3"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int16_4(sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int16_4, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int16_4)
      pres = omp_target_is_present_f(host_data=host_data_int16_4, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int16_4, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int16_4"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int16_5(sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int16_5, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int16_5)
      pres = omp_target_is_present_f(host_data=host_data_int16_5, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int16_5, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int16_5"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int16_6(sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int16_6, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int16_6)
      pres = omp_target_is_present_f(host_data=host_data_int16_6, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int16_6, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int16_6"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int16_7(sz,sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int16_7, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int16_7)
      pres = omp_target_is_present_f(host_data=host_data_int16_7, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int16_7, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int16_7"
         nfail = nfail + 1
      endif
   endif

   !!! DMR Target Is Present Integer tests
   allocate(host_data_int32_1(sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int32_1, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int32_1)
      pres = omp_target_is_present_f(host_data=host_data_int32_1, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int32_1, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int32_1"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int32_2(sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int32_2, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int32_2)
      pres = omp_target_is_present_f(host_data=host_data_int32_2, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int32_2, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int32_2"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int32_3(sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int32_3, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int32_3)
      pres = omp_target_is_present_f(host_data=host_data_int32_3, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int32_3, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int32_3"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int32_4(sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int32_4, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int32_4)
      pres = omp_target_is_present_f(host_data=host_data_int32_4, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int32_4, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int32_4"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int32_5(sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int32_5, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int32_5)
      pres = omp_target_is_present_f(host_data=host_data_int32_5, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int32_5, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int32_5"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int32_6(sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int32_6, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int32_6)
      pres = omp_target_is_present_f(host_data=host_data_int32_6, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int32_6, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int32_6"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int32_7(sz,sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int32_7, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int32_7)
      pres = omp_target_is_present_f(host_data=host_data_int32_7, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int32_7, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int32_7"
         nfail = nfail + 1
      endif
   endif

   !!! DMR Target Is Present Integer tests
   allocate(host_data_int64_1(sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int64_1, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int64_1)
      pres = omp_target_is_present_f(host_data=host_data_int64_1, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int64_1, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int64_1"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int64_2(sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int64_2, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int64_2)
      pres = omp_target_is_present_f(host_data=host_data_int64_2, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int64_2, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int64_2"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int64_3(sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int64_3, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int64_3)
      pres = omp_target_is_present_f(host_data=host_data_int64_3, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int64_3, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int64_3"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int64_4(sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int64_4, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int64_4)
      pres = omp_target_is_present_f(host_data=host_data_int64_4, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int64_4, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int64_4"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int64_5(sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int64_5, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int64_5)
      pres = omp_target_is_present_f(host_data=host_data_int64_5, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int64_5, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int64_5"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int64_6(sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int64_6, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int64_6)
      pres = omp_target_is_present_f(host_data=host_data_int64_6, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int64_6, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int64_6"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_int64_7(sz,sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_int64_7, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_int64_7)
      pres = omp_target_is_present_f(host_data=host_data_int64_7, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_int64_7, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_int64_7"
         nfail = nfail + 1
      endif
   endif

   !!! DMR Target Is Present Real tests
   allocate(host_data_real32_1(sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real32_1, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real32_1)
      pres = omp_target_is_present_f(host_data=host_data_real32_1, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real32_1, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real32_1"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_real32_2(sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real32_2, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real32_2)
      pres = omp_target_is_present_f(host_data=host_data_real32_2, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real32_2, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real32_2"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_real32_3(sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real32_3, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real32_3)
      pres = omp_target_is_present_f(host_data=host_data_real32_3, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real32_3, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real32_3"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_real32_4(sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real32_4, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real32_4)
      pres = omp_target_is_present_f(host_data=host_data_real32_4, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real32_4, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real32_4"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_real32_5(sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real32_5, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real32_5)
      pres = omp_target_is_present_f(host_data=host_data_real32_5, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real32_5, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real32_5"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_real32_6(sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real32_6, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real32_6)
      pres = omp_target_is_present_f(host_data=host_data_real32_6, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real32_6, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real32_6"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_real32_7(sz,sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real32_7, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real32_7)
      pres = omp_target_is_present_f(host_data=host_data_real32_7, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real32_7, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real32_7"
         nfail = nfail + 1
      endif
   endif

   !!! DMR Target Is Present Real tests
   allocate(host_data_real64_1(sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real64_1, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real64_1)
      pres = omp_target_is_present_f(host_data=host_data_real64_1, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real64_1, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real64_1"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_real64_2(sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real64_2, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real64_2)
      pres = omp_target_is_present_f(host_data=host_data_real64_2, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real64_2, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real64_2"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_real64_3(sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real64_3, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real64_3)
      pres = omp_target_is_present_f(host_data=host_data_real64_3, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real64_3, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real64_3"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_real64_4(sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real64_4, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real64_4)
      pres = omp_target_is_present_f(host_data=host_data_real64_4, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real64_4, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real64_4"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_real64_5(sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real64_5, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real64_5)
      pres = omp_target_is_present_f(host_data=host_data_real64_5, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real64_5, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real64_5"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_real64_6(sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real64_6, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real64_6)
      pres = omp_target_is_present_f(host_data=host_data_real64_6, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real64_6, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real64_6"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_real64_7(sz,sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real64_7, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real64_7)
      pres = omp_target_is_present_f(host_data=host_data_real64_7, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real64_7, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real64_7"
         nfail = nfail + 1
      endif
   endif

   !!! DMR Target Is Present Real tests
#if defined _real128
   allocate(host_data_real128_1(sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real128_1, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real128_1)
      pres = omp_target_is_present_f(host_data=host_data_real128_1, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real128_1, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real128_1"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_real128_2(sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real128_2, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real128_2)
      pres = omp_target_is_present_f(host_data=host_data_real128_2, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real128_2, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real128_2"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_real128_3(sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real128_3, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real128_3)
      pres = omp_target_is_present_f(host_data=host_data_real128_3, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real128_3, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real128_3"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_real128_4(sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real128_4, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real128_4)
      pres = omp_target_is_present_f(host_data=host_data_real128_4, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real128_4, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real128_4"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_real128_5(sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real128_5, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real128_5)
      pres = omp_target_is_present_f(host_data=host_data_real128_5, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real128_5, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real128_5"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_real128_6(sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real128_6, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real128_6)
      pres = omp_target_is_present_f(host_data=host_data_real128_6, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real128_6, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real128_6"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_real128_7(sz,sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_real128_7, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_real128_7)
      pres = omp_target_is_present_f(host_data=host_data_real128_7, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_real128_7, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_real128_7"
         nfail = nfail + 1
      endif
   endif
#endif

   !!! DMR Target Is Present Complex tests
   allocate(host_data_cmplx32_1(sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx32_1, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx32_1)
      pres = omp_target_is_present_f(host_data=host_data_cmplx32_1, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx32_1, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx32_1"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_cmplx32_2(sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx32_2, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx32_2)
      pres = omp_target_is_present_f(host_data=host_data_cmplx32_2, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx32_2, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx32_2"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_cmplx32_3(sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx32_3, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx32_3)
      pres = omp_target_is_present_f(host_data=host_data_cmplx32_3, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx32_3, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx32_3"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_cmplx32_4(sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx32_4, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx32_4)
      pres = omp_target_is_present_f(host_data=host_data_cmplx32_4, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx32_4, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx32_4"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_cmplx32_5(sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx32_5, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx32_5)
      pres = omp_target_is_present_f(host_data=host_data_cmplx32_5, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx32_5, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx32_5"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_cmplx32_6(sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx32_6, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx32_6)
      pres = omp_target_is_present_f(host_data=host_data_cmplx32_6, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx32_6, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx32_6"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_cmplx32_7(sz,sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx32_7, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx32_7)
      pres = omp_target_is_present_f(host_data=host_data_cmplx32_7, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx32_7, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx32_7"
         nfail = nfail + 1
      endif
   endif

   !!! DMR Target Is Present Complex tests
   allocate(host_data_cmplx64_1(sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx64_1, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx64_1)
      pres = omp_target_is_present_f(host_data=host_data_cmplx64_1, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx64_1, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx64_1"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_cmplx64_2(sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx64_2, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx64_2)
      pres = omp_target_is_present_f(host_data=host_data_cmplx64_2, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx64_2, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx64_2"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_cmplx64_3(sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx64_3, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx64_3)
      pres = omp_target_is_present_f(host_data=host_data_cmplx64_3, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx64_3, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx64_3"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_cmplx64_4(sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx64_4, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx64_4)
      pres = omp_target_is_present_f(host_data=host_data_cmplx64_4, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx64_4, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx64_4"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_cmplx64_5(sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx64_5, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx64_5)
      pres = omp_target_is_present_f(host_data=host_data_cmplx64_5, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx64_5, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx64_5"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_cmplx64_6(sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx64_6, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx64_6)
      pres = omp_target_is_present_f(host_data=host_data_cmplx64_6, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx64_6, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx64_6"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_cmplx64_7(sz,sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx64_7, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx64_7)
      pres = omp_target_is_present_f(host_data=host_data_cmplx64_7, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx64_7, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx64_7"
         nfail = nfail + 1
      endif
   endif

   !!! DMR Target Is Present Complex tests
#if defined _real128
   allocate(host_data_cmplx128_1(sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx128_1, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx128_1)
      pres = omp_target_is_present_f(host_data=host_data_cmplx128_1, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx128_1, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx128_1"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_cmplx128_2(sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx128_2, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx128_2)
      pres = omp_target_is_present_f(host_data=host_data_cmplx128_2, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx128_2, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx128_2"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_cmplx128_3(sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx128_3, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx128_3)
      pres = omp_target_is_present_f(host_data=host_data_cmplx128_3, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx128_3, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx128_3"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_cmplx128_4(sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx128_4, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx128_4)
      pres = omp_target_is_present_f(host_data=host_data_cmplx128_4, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx128_4, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx128_4"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_cmplx128_5(sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx128_5, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx128_5)
      pres = omp_target_is_present_f(host_data=host_data_cmplx128_5, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx128_5, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx128_5"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_cmplx128_6(sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx128_6, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx128_6)
      pres = omp_target_is_present_f(host_data=host_data_cmplx128_6, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx128_6, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx128_6"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_cmplx128_7(sz,sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_cmplx128_7, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_cmplx128_7)
      pres = omp_target_is_present_f(host_data=host_data_cmplx128_7, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_cmplx128_7, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_cmplx128_7"
         nfail = nfail + 1
      endif
   endif
#endif

   !!! DMR Target Is Present Logical tests
   allocate(host_data_lgcl32_1(sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_lgcl32_1, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_lgcl32_1)
      pres = omp_target_is_present_f(host_data=host_data_lgcl32_1, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_lgcl32_1, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_lgcl32_1"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_lgcl32_2(sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_lgcl32_2, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_lgcl32_2)
      pres = omp_target_is_present_f(host_data=host_data_lgcl32_2, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_lgcl32_2, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_lgcl32_2"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_lgcl32_3(sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_lgcl32_3, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_lgcl32_3)
      pres = omp_target_is_present_f(host_data=host_data_lgcl32_3, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_lgcl32_3, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_lgcl32_3"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_lgcl32_4(sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_lgcl32_4, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_lgcl32_4)
      pres = omp_target_is_present_f(host_data=host_data_lgcl32_4, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_lgcl32_4, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_lgcl32_4"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_lgcl32_5(sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_lgcl32_5, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_lgcl32_5)
      pres = omp_target_is_present_f(host_data=host_data_lgcl32_5, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_lgcl32_5, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_lgcl32_5"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_lgcl32_6(sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_lgcl32_6, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_lgcl32_6)
      pres = omp_target_is_present_f(host_data=host_data_lgcl32_6, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_lgcl32_6, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_lgcl32_6"
         nfail = nfail + 1
      endif
   endif
   allocate(host_data_lgcl32_7(sz,sz,sz,sz,sz,sz,sz), stat=ierr)
   if (ierr/=0) then
      write(0,"(/,a,i5)") "Allocation failed on the host for host_data_lgcl32_7, ierr:", ierr
      write(0,"(/,a)") "DMR Target Is Present test will not be executed"
   else
      !$omp target data map(alloc:host_data_lgcl32_7)
      pres = omp_target_is_present_f(host_data=host_data_lgcl32_7, omp_dev=omp_get_default_device())
      !$omp end target data
      if (pres .and. (.not. omp_target_is_present_f(host_data=host_data_lgcl32_7, omp_dev=omp_get_default_device()))) then
         test_tot = test_tot + 1
      else
         write(0,"(/,a,i5)") "Test failed for dev_ptr_lgcl32_7"
         nfail = nfail + 1
      endif
   endif

  write(0,"(/,a)") "DMR Target Is Present Test SUMMARY:"
  write(0,"(3x,a,i5)") "# tests: ", test_tot
  write(0,"(3x,a,i5)") "# passed: ", test_tot - nfail
  write(0,"(3x,a,i5)") "# failed: ", nfail
  write(0,"(3x,a,i5)") "# not executed for host allocation issues ", test_not
  write(0,"()")
endprogram test_dmr_target_is_present