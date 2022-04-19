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

program test_dmr_target_alloc
   use omp_lib,         only : omp_get_default_device
   use dmr,             only : omp_target_alloc_f, omp_target_free_f
   use dmr_environment

   implicit none
   integer(I1P), pointer, contiguous :: dev_ptr_int8_1(:),  dev_ptr_int8_2(:,:),  dev_ptr_int8_3(:,:,:),  dev_ptr_int8_4(:,:,:,:), &
                                        dev_ptr_int8_5(:,:,:,:,:), dev_ptr_int8_6(:,:,:,:,:,:), dev_ptr_int8_7(:,:,:,:,:,:,:)
   integer(I2P), pointer, contiguous :: dev_ptr_int16_1(:),  dev_ptr_int16_2(:,:),  dev_ptr_int16_3(:,:,:),  dev_ptr_int16_4(:,:,:,:), &
                                        dev_ptr_int16_5(:,:,:,:,:), dev_ptr_int16_6(:,:,:,:,:,:), dev_ptr_int16_7(:,:,:,:,:,:,:)
   integer(I4P), pointer, contiguous :: dev_ptr_int32_1(:),  dev_ptr_int32_2(:,:),  dev_ptr_int32_3(:,:,:),  dev_ptr_int32_4(:,:,:,:), &
                                        dev_ptr_int32_5(:,:,:,:,:), dev_ptr_int32_6(:,:,:,:,:,:), dev_ptr_int32_7(:,:,:,:,:,:,:)
   integer(I8P), pointer, contiguous :: dev_ptr_int64_1(:),  dev_ptr_int64_2(:,:),  dev_ptr_int64_3(:,:,:),  dev_ptr_int64_4(:,:,:,:), &
                                        dev_ptr_int64_5(:,:,:,:,:), dev_ptr_int64_6(:,:,:,:,:,:), dev_ptr_int64_7(:,:,:,:,:,:,:)
   real(R4P), pointer, contiguous :: dev_ptr_real32_1(:),  dev_ptr_real32_2(:,:),  dev_ptr_real32_3(:,:,:),  dev_ptr_real32_4(:,:,:,:), &
                                        dev_ptr_real32_5(:,:,:,:,:), dev_ptr_real32_6(:,:,:,:,:,:), dev_ptr_real32_7(:,:,:,:,:,:,:)
   real(R8P), pointer, contiguous :: dev_ptr_real64_1(:),  dev_ptr_real64_2(:,:),  dev_ptr_real64_3(:,:,:),  dev_ptr_real64_4(:,:,:,:), &
                                        dev_ptr_real64_5(:,:,:,:,:), dev_ptr_real64_6(:,:,:,:,:,:), dev_ptr_real64_7(:,:,:,:,:,:,:)
#if defined _real128
   real(R16P), pointer, contiguous :: dev_ptr_real128_1(:),  dev_ptr_real128_2(:,:),  dev_ptr_real128_3(:,:,:),  dev_ptr_real128_4(:,:,:,:), &
                                        dev_ptr_real128_5(:,:,:,:,:), dev_ptr_real128_6(:,:,:,:,:,:), dev_ptr_real128_7(:,:,:,:,:,:,:)
#endif
   complex(R4P), pointer, contiguous :: dev_ptr_cmplx32_1(:),  dev_ptr_cmplx32_2(:,:),  dev_ptr_cmplx32_3(:,:,:),  dev_ptr_cmplx32_4(:,:,:,:), &
                                        dev_ptr_cmplx32_5(:,:,:,:,:), dev_ptr_cmplx32_6(:,:,:,:,:,:), dev_ptr_cmplx32_7(:,:,:,:,:,:,:)
   complex(R8P), pointer, contiguous :: dev_ptr_cmplx64_1(:),  dev_ptr_cmplx64_2(:,:),  dev_ptr_cmplx64_3(:,:,:),  dev_ptr_cmplx64_4(:,:,:,:), &
                                        dev_ptr_cmplx64_5(:,:,:,:,:), dev_ptr_cmplx64_6(:,:,:,:,:,:), dev_ptr_cmplx64_7(:,:,:,:,:,:,:)
#if defined _real128
   complex(R16P), pointer, contiguous :: dev_ptr_cmplx128_1(:),  dev_ptr_cmplx128_2(:,:),  dev_ptr_cmplx128_3(:,:,:),  dev_ptr_cmplx128_4(:,:,:,:), &
                                        dev_ptr_cmplx128_5(:,:,:,:,:), dev_ptr_cmplx128_6(:,:,:,:,:,:), dev_ptr_cmplx128_7(:,:,:,:,:,:,:)
#endif
   logical(I4P), pointer, contiguous :: dev_ptr_lgcl32_1(:),  dev_ptr_lgcl32_2(:,:),  dev_ptr_lgcl32_3(:,:,:),  dev_ptr_lgcl32_4(:,:,:,:), &
                                        dev_ptr_lgcl32_5(:,:,:,:,:), dev_ptr_lgcl32_6(:,:,:,:,:,:), dev_ptr_lgcl32_7(:,:,:,:,:,:,:)

   integer(I4P) :: i1, i2, i3, i4, i5, i6, i7
   integer(I4P), parameter :: sz = 10
   integer(I4P) :: ierr=0, test_tot=0, nfail=0

   !!! DMR Target Alloc 8 bits Integer tests
   nullify( dev_ptr_int8_1, dev_ptr_int8_2, dev_ptr_int8_3, dev_ptr_int8_4, dev_ptr_int8_5, dev_ptr_int8_6, dev_ptr_int8_7)
   nullify( dev_ptr_int16_1, dev_ptr_int16_2, dev_ptr_int16_3, dev_ptr_int16_4, dev_ptr_int16_5, dev_ptr_int16_6, dev_ptr_int16_7)
   nullify( dev_ptr_int32_1, dev_ptr_int32_2, dev_ptr_int32_3, dev_ptr_int32_4, dev_ptr_int32_5, dev_ptr_int32_6, dev_ptr_int32_7)
   nullify( dev_ptr_int64_1, dev_ptr_int64_2, dev_ptr_int64_3, dev_ptr_int64_4, dev_ptr_int64_5, dev_ptr_int64_6, dev_ptr_int64_7)
   nullify( dev_ptr_real32_1, dev_ptr_real32_2, dev_ptr_real32_3, dev_ptr_real32_4, dev_ptr_real32_5, dev_ptr_real32_6, dev_ptr_real32_7)
   nullify( dev_ptr_real64_1, dev_ptr_real64_2, dev_ptr_real64_3, dev_ptr_real64_4, dev_ptr_real64_5, dev_ptr_real64_6, dev_ptr_real64_7)
#if defined _real128
   nullify( dev_ptr_real128_1, dev_ptr_real128_2, dev_ptr_real128_3, dev_ptr_real128_4, dev_ptr_real128_5, dev_ptr_real128_6, dev_ptr_real128_7)
#endif
   nullify( dev_ptr_cmplx32_1, dev_ptr_cmplx32_2, dev_ptr_cmplx32_3, dev_ptr_cmplx32_4, dev_ptr_cmplx32_5, dev_ptr_cmplx32_6, dev_ptr_cmplx32_7)
   nullify( dev_ptr_cmplx64_1, dev_ptr_cmplx64_2, dev_ptr_cmplx64_3, dev_ptr_cmplx64_4, dev_ptr_cmplx64_5, dev_ptr_cmplx64_6, dev_ptr_cmplx64_7)
#if defined _real128
   nullify( dev_ptr_cmplx128_1, dev_ptr_cmplx128_2, dev_ptr_cmplx128_3, dev_ptr_cmplx128_4, dev_ptr_cmplx128_5, dev_ptr_cmplx128_6, dev_ptr_cmplx128_7)
#endif
   nullify( dev_ptr_lgcl32_1, dev_ptr_lgcl32_2, dev_ptr_lgcl32_3, dev_ptr_lgcl32_4, dev_ptr_lgcl32_5, dev_ptr_lgcl32_6, dev_ptr_lgcl32_7)

   call omp_target_alloc_f(fptr_dev=dev_ptr_int8_1, dimensions=sz, omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int8_1)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int8_1, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int8_1, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int8_2, dimensions=[sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int8_2)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int8_2, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int8_2, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int8_3, dimensions=[sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int8_3)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int8_3, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int8_3, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int8_4, dimensions=[sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int8_4)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int8_4, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int8_4, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int8_5, dimensions=[sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int8_5)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int8_5, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int8_5, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int8_6, dimensions=[sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int8_6)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int8_6, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int8_6, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int8_7, dimensions=[sz,sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int8_7)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int8_7, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int8_7, omp_dev=omp_get_default_device())
   endif

   !!! DMR Target Alloc 16 bits Integer tests
   nullify( dev_ptr_int8_1, dev_ptr_int8_2, dev_ptr_int8_3, dev_ptr_int8_4, dev_ptr_int8_5, dev_ptr_int8_6, dev_ptr_int8_7)
   nullify( dev_ptr_int16_1, dev_ptr_int16_2, dev_ptr_int16_3, dev_ptr_int16_4, dev_ptr_int16_5, dev_ptr_int16_6, dev_ptr_int16_7)
   nullify( dev_ptr_int32_1, dev_ptr_int32_2, dev_ptr_int32_3, dev_ptr_int32_4, dev_ptr_int32_5, dev_ptr_int32_6, dev_ptr_int32_7)
   nullify( dev_ptr_int64_1, dev_ptr_int64_2, dev_ptr_int64_3, dev_ptr_int64_4, dev_ptr_int64_5, dev_ptr_int64_6, dev_ptr_int64_7)
   nullify( dev_ptr_real32_1, dev_ptr_real32_2, dev_ptr_real32_3, dev_ptr_real32_4, dev_ptr_real32_5, dev_ptr_real32_6, dev_ptr_real32_7)
   nullify( dev_ptr_real64_1, dev_ptr_real64_2, dev_ptr_real64_3, dev_ptr_real64_4, dev_ptr_real64_5, dev_ptr_real64_6, dev_ptr_real64_7)
#if defined _real128
   nullify( dev_ptr_real128_1, dev_ptr_real128_2, dev_ptr_real128_3, dev_ptr_real128_4, dev_ptr_real128_5, dev_ptr_real128_6, dev_ptr_real128_7)
#endif
   nullify( dev_ptr_cmplx32_1, dev_ptr_cmplx32_2, dev_ptr_cmplx32_3, dev_ptr_cmplx32_4, dev_ptr_cmplx32_5, dev_ptr_cmplx32_6, dev_ptr_cmplx32_7)
   nullify( dev_ptr_cmplx64_1, dev_ptr_cmplx64_2, dev_ptr_cmplx64_3, dev_ptr_cmplx64_4, dev_ptr_cmplx64_5, dev_ptr_cmplx64_6, dev_ptr_cmplx64_7)
#if defined _real128
   nullify( dev_ptr_cmplx128_1, dev_ptr_cmplx128_2, dev_ptr_cmplx128_3, dev_ptr_cmplx128_4, dev_ptr_cmplx128_5, dev_ptr_cmplx128_6, dev_ptr_cmplx128_7)
#endif
   nullify( dev_ptr_lgcl32_1, dev_ptr_lgcl32_2, dev_ptr_lgcl32_3, dev_ptr_lgcl32_4, dev_ptr_lgcl32_5, dev_ptr_lgcl32_6, dev_ptr_lgcl32_7)

   call omp_target_alloc_f(fptr_dev=dev_ptr_int16_1, dimensions=sz, omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int16_1)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int16_1, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int16_1, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int16_2, dimensions=[sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int16_2)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int16_2, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int16_2, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int16_3, dimensions=[sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int16_3)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int16_3, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int16_3, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int16_4, dimensions=[sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int16_4)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int16_4, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int16_4, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int16_5, dimensions=[sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int16_5)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int16_5, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int16_5, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int16_6, dimensions=[sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int16_6)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int16_6, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int16_6, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int16_7, dimensions=[sz,sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int16_7)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int16_7, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int16_7, omp_dev=omp_get_default_device())
   endif

   !!! DMR Target Alloc 32 bits Integer tests
   nullify( dev_ptr_int8_1, dev_ptr_int8_2, dev_ptr_int8_3, dev_ptr_int8_4, dev_ptr_int8_5, dev_ptr_int8_6, dev_ptr_int8_7)
   nullify( dev_ptr_int16_1, dev_ptr_int16_2, dev_ptr_int16_3, dev_ptr_int16_4, dev_ptr_int16_5, dev_ptr_int16_6, dev_ptr_int16_7)
   nullify( dev_ptr_int32_1, dev_ptr_int32_2, dev_ptr_int32_3, dev_ptr_int32_4, dev_ptr_int32_5, dev_ptr_int32_6, dev_ptr_int32_7)
   nullify( dev_ptr_int64_1, dev_ptr_int64_2, dev_ptr_int64_3, dev_ptr_int64_4, dev_ptr_int64_5, dev_ptr_int64_6, dev_ptr_int64_7)
   nullify( dev_ptr_real32_1, dev_ptr_real32_2, dev_ptr_real32_3, dev_ptr_real32_4, dev_ptr_real32_5, dev_ptr_real32_6, dev_ptr_real32_7)
   nullify( dev_ptr_real64_1, dev_ptr_real64_2, dev_ptr_real64_3, dev_ptr_real64_4, dev_ptr_real64_5, dev_ptr_real64_6, dev_ptr_real64_7)
#if defined _real128
   nullify( dev_ptr_real128_1, dev_ptr_real128_2, dev_ptr_real128_3, dev_ptr_real128_4, dev_ptr_real128_5, dev_ptr_real128_6, dev_ptr_real128_7)
#endif
   nullify( dev_ptr_cmplx32_1, dev_ptr_cmplx32_2, dev_ptr_cmplx32_3, dev_ptr_cmplx32_4, dev_ptr_cmplx32_5, dev_ptr_cmplx32_6, dev_ptr_cmplx32_7)
   nullify( dev_ptr_cmplx64_1, dev_ptr_cmplx64_2, dev_ptr_cmplx64_3, dev_ptr_cmplx64_4, dev_ptr_cmplx64_5, dev_ptr_cmplx64_6, dev_ptr_cmplx64_7)
#if defined _real128
   nullify( dev_ptr_cmplx128_1, dev_ptr_cmplx128_2, dev_ptr_cmplx128_3, dev_ptr_cmplx128_4, dev_ptr_cmplx128_5, dev_ptr_cmplx128_6, dev_ptr_cmplx128_7)
#endif
   nullify( dev_ptr_lgcl32_1, dev_ptr_lgcl32_2, dev_ptr_lgcl32_3, dev_ptr_lgcl32_4, dev_ptr_lgcl32_5, dev_ptr_lgcl32_6, dev_ptr_lgcl32_7)

   call omp_target_alloc_f(fptr_dev=dev_ptr_int32_1, dimensions=sz, omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int32_1)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int32_1, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int32_1, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int32_2, dimensions=[sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int32_2)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int32_2, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int32_2, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int32_3, dimensions=[sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int32_3)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int32_3, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int32_3, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int32_4, dimensions=[sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int32_4)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int32_4, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int32_4, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int32_5, dimensions=[sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int32_5)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int32_5, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int32_5, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int32_6, dimensions=[sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int32_6)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int32_6, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int32_6, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int32_7, dimensions=[sz,sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int32_7)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int32_7, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int32_7, omp_dev=omp_get_default_device())
   endif

   !!! DMR Target Alloc 64 bits Integer tests
   nullify( dev_ptr_int8_1, dev_ptr_int8_2, dev_ptr_int8_3, dev_ptr_int8_4, dev_ptr_int8_5, dev_ptr_int8_6, dev_ptr_int8_7)
   nullify( dev_ptr_int16_1, dev_ptr_int16_2, dev_ptr_int16_3, dev_ptr_int16_4, dev_ptr_int16_5, dev_ptr_int16_6, dev_ptr_int16_7)
   nullify( dev_ptr_int32_1, dev_ptr_int32_2, dev_ptr_int32_3, dev_ptr_int32_4, dev_ptr_int32_5, dev_ptr_int32_6, dev_ptr_int32_7)
   nullify( dev_ptr_int64_1, dev_ptr_int64_2, dev_ptr_int64_3, dev_ptr_int64_4, dev_ptr_int64_5, dev_ptr_int64_6, dev_ptr_int64_7)
   nullify( dev_ptr_real32_1, dev_ptr_real32_2, dev_ptr_real32_3, dev_ptr_real32_4, dev_ptr_real32_5, dev_ptr_real32_6, dev_ptr_real32_7)
   nullify( dev_ptr_real64_1, dev_ptr_real64_2, dev_ptr_real64_3, dev_ptr_real64_4, dev_ptr_real64_5, dev_ptr_real64_6, dev_ptr_real64_7)
#if defined _real128
   nullify( dev_ptr_real128_1, dev_ptr_real128_2, dev_ptr_real128_3, dev_ptr_real128_4, dev_ptr_real128_5, dev_ptr_real128_6, dev_ptr_real128_7)
#endif
   nullify( dev_ptr_cmplx32_1, dev_ptr_cmplx32_2, dev_ptr_cmplx32_3, dev_ptr_cmplx32_4, dev_ptr_cmplx32_5, dev_ptr_cmplx32_6, dev_ptr_cmplx32_7)
   nullify( dev_ptr_cmplx64_1, dev_ptr_cmplx64_2, dev_ptr_cmplx64_3, dev_ptr_cmplx64_4, dev_ptr_cmplx64_5, dev_ptr_cmplx64_6, dev_ptr_cmplx64_7)
#if defined _real128
   nullify( dev_ptr_cmplx128_1, dev_ptr_cmplx128_2, dev_ptr_cmplx128_3, dev_ptr_cmplx128_4, dev_ptr_cmplx128_5, dev_ptr_cmplx128_6, dev_ptr_cmplx128_7)
#endif
   nullify( dev_ptr_lgcl32_1, dev_ptr_lgcl32_2, dev_ptr_lgcl32_3, dev_ptr_lgcl32_4, dev_ptr_lgcl32_5, dev_ptr_lgcl32_6, dev_ptr_lgcl32_7)

   call omp_target_alloc_f(fptr_dev=dev_ptr_int64_1, dimensions=sz, omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int64_1)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int64_1, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int64_1, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int64_2, dimensions=[sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int64_2)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int64_2, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int64_2, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int64_3, dimensions=[sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int64_3)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int64_3, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int64_3, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int64_4, dimensions=[sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int64_4)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int64_4, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int64_4, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int64_5, dimensions=[sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int64_5)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int64_5, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int64_5, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int64_6, dimensions=[sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int64_6)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int64_6, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int64_6, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_int64_7, dimensions=[sz,sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_int64_7)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_int64_7, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_int64_7, omp_dev=omp_get_default_device())
   endif

   !!! DMR Target Alloc 32 bits Real tests
   nullify( dev_ptr_int8_1, dev_ptr_int8_2, dev_ptr_int8_3, dev_ptr_int8_4, dev_ptr_int8_5, dev_ptr_int8_6, dev_ptr_int8_7)
   nullify( dev_ptr_int16_1, dev_ptr_int16_2, dev_ptr_int16_3, dev_ptr_int16_4, dev_ptr_int16_5, dev_ptr_int16_6, dev_ptr_int16_7)
   nullify( dev_ptr_int32_1, dev_ptr_int32_2, dev_ptr_int32_3, dev_ptr_int32_4, dev_ptr_int32_5, dev_ptr_int32_6, dev_ptr_int32_7)
   nullify( dev_ptr_int64_1, dev_ptr_int64_2, dev_ptr_int64_3, dev_ptr_int64_4, dev_ptr_int64_5, dev_ptr_int64_6, dev_ptr_int64_7)
   nullify( dev_ptr_real32_1, dev_ptr_real32_2, dev_ptr_real32_3, dev_ptr_real32_4, dev_ptr_real32_5, dev_ptr_real32_6, dev_ptr_real32_7)
   nullify( dev_ptr_real64_1, dev_ptr_real64_2, dev_ptr_real64_3, dev_ptr_real64_4, dev_ptr_real64_5, dev_ptr_real64_6, dev_ptr_real64_7)
#if defined _real128
   nullify( dev_ptr_real128_1, dev_ptr_real128_2, dev_ptr_real128_3, dev_ptr_real128_4, dev_ptr_real128_5, dev_ptr_real128_6, dev_ptr_real128_7)
#endif
   nullify( dev_ptr_cmplx32_1, dev_ptr_cmplx32_2, dev_ptr_cmplx32_3, dev_ptr_cmplx32_4, dev_ptr_cmplx32_5, dev_ptr_cmplx32_6, dev_ptr_cmplx32_7)
   nullify( dev_ptr_cmplx64_1, dev_ptr_cmplx64_2, dev_ptr_cmplx64_3, dev_ptr_cmplx64_4, dev_ptr_cmplx64_5, dev_ptr_cmplx64_6, dev_ptr_cmplx64_7)
#if defined _real128
   nullify( dev_ptr_cmplx128_1, dev_ptr_cmplx128_2, dev_ptr_cmplx128_3, dev_ptr_cmplx128_4, dev_ptr_cmplx128_5, dev_ptr_cmplx128_6, dev_ptr_cmplx128_7)
#endif
   nullify( dev_ptr_lgcl32_1, dev_ptr_lgcl32_2, dev_ptr_lgcl32_3, dev_ptr_lgcl32_4, dev_ptr_lgcl32_5, dev_ptr_lgcl32_6, dev_ptr_lgcl32_7)

   call omp_target_alloc_f(fptr_dev=dev_ptr_real32_1, dimensions=sz, omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real32_1)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real32_1, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real32_1, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_real32_2, dimensions=[sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real32_2)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real32_2, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real32_2, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_real32_3, dimensions=[sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real32_3)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real32_3, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real32_3, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_real32_4, dimensions=[sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real32_4)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real32_4, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real32_4, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_real32_5, dimensions=[sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real32_5)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real32_5, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real32_5, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_real32_6, dimensions=[sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real32_6)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real32_6, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real32_6, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_real32_7, dimensions=[sz,sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real32_7)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real32_7, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real32_7, omp_dev=omp_get_default_device())
   endif

   !!! DMR Target Alloc 64 bits Real tests
   nullify( dev_ptr_int8_1, dev_ptr_int8_2, dev_ptr_int8_3, dev_ptr_int8_4, dev_ptr_int8_5, dev_ptr_int8_6, dev_ptr_int8_7)
   nullify( dev_ptr_int16_1, dev_ptr_int16_2, dev_ptr_int16_3, dev_ptr_int16_4, dev_ptr_int16_5, dev_ptr_int16_6, dev_ptr_int16_7)
   nullify( dev_ptr_int32_1, dev_ptr_int32_2, dev_ptr_int32_3, dev_ptr_int32_4, dev_ptr_int32_5, dev_ptr_int32_6, dev_ptr_int32_7)
   nullify( dev_ptr_int64_1, dev_ptr_int64_2, dev_ptr_int64_3, dev_ptr_int64_4, dev_ptr_int64_5, dev_ptr_int64_6, dev_ptr_int64_7)
   nullify( dev_ptr_real32_1, dev_ptr_real32_2, dev_ptr_real32_3, dev_ptr_real32_4, dev_ptr_real32_5, dev_ptr_real32_6, dev_ptr_real32_7)
   nullify( dev_ptr_real64_1, dev_ptr_real64_2, dev_ptr_real64_3, dev_ptr_real64_4, dev_ptr_real64_5, dev_ptr_real64_6, dev_ptr_real64_7)
#if defined _real128
   nullify( dev_ptr_real128_1, dev_ptr_real128_2, dev_ptr_real128_3, dev_ptr_real128_4, dev_ptr_real128_5, dev_ptr_real128_6, dev_ptr_real128_7)
#endif
   nullify( dev_ptr_cmplx32_1, dev_ptr_cmplx32_2, dev_ptr_cmplx32_3, dev_ptr_cmplx32_4, dev_ptr_cmplx32_5, dev_ptr_cmplx32_6, dev_ptr_cmplx32_7)
   nullify( dev_ptr_cmplx64_1, dev_ptr_cmplx64_2, dev_ptr_cmplx64_3, dev_ptr_cmplx64_4, dev_ptr_cmplx64_5, dev_ptr_cmplx64_6, dev_ptr_cmplx64_7)
#if defined _real128
   nullify( dev_ptr_cmplx128_1, dev_ptr_cmplx128_2, dev_ptr_cmplx128_3, dev_ptr_cmplx128_4, dev_ptr_cmplx128_5, dev_ptr_cmplx128_6, dev_ptr_cmplx128_7)
#endif
   nullify( dev_ptr_lgcl32_1, dev_ptr_lgcl32_2, dev_ptr_lgcl32_3, dev_ptr_lgcl32_4, dev_ptr_lgcl32_5, dev_ptr_lgcl32_6, dev_ptr_lgcl32_7)

   call omp_target_alloc_f(fptr_dev=dev_ptr_real64_1, dimensions=sz, omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real64_1)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real64_1, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real64_1, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_real64_2, dimensions=[sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real64_2)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real64_2, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real64_2, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_real64_3, dimensions=[sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real64_3)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real64_3, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real64_3, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_real64_4, dimensions=[sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real64_4)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real64_4, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real64_4, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_real64_5, dimensions=[sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real64_5)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real64_5, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real64_5, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_real64_6, dimensions=[sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real64_6)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real64_6, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real64_6, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_real64_7, dimensions=[sz,sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real64_7)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real64_7, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real64_7, omp_dev=omp_get_default_device())
   endif

   !!! DMR Target Alloc 128 bits Real tests
   nullify( dev_ptr_int8_1, dev_ptr_int8_2, dev_ptr_int8_3, dev_ptr_int8_4, dev_ptr_int8_5, dev_ptr_int8_6, dev_ptr_int8_7)
   nullify( dev_ptr_int16_1, dev_ptr_int16_2, dev_ptr_int16_3, dev_ptr_int16_4, dev_ptr_int16_5, dev_ptr_int16_6, dev_ptr_int16_7)
   nullify( dev_ptr_int32_1, dev_ptr_int32_2, dev_ptr_int32_3, dev_ptr_int32_4, dev_ptr_int32_5, dev_ptr_int32_6, dev_ptr_int32_7)
   nullify( dev_ptr_int64_1, dev_ptr_int64_2, dev_ptr_int64_3, dev_ptr_int64_4, dev_ptr_int64_5, dev_ptr_int64_6, dev_ptr_int64_7)
   nullify( dev_ptr_real32_1, dev_ptr_real32_2, dev_ptr_real32_3, dev_ptr_real32_4, dev_ptr_real32_5, dev_ptr_real32_6, dev_ptr_real32_7)
   nullify( dev_ptr_real64_1, dev_ptr_real64_2, dev_ptr_real64_3, dev_ptr_real64_4, dev_ptr_real64_5, dev_ptr_real64_6, dev_ptr_real64_7)
#if defined _real128
   nullify( dev_ptr_real128_1, dev_ptr_real128_2, dev_ptr_real128_3, dev_ptr_real128_4, dev_ptr_real128_5, dev_ptr_real128_6, dev_ptr_real128_7)
#endif
   nullify( dev_ptr_cmplx32_1, dev_ptr_cmplx32_2, dev_ptr_cmplx32_3, dev_ptr_cmplx32_4, dev_ptr_cmplx32_5, dev_ptr_cmplx32_6, dev_ptr_cmplx32_7)
   nullify( dev_ptr_cmplx64_1, dev_ptr_cmplx64_2, dev_ptr_cmplx64_3, dev_ptr_cmplx64_4, dev_ptr_cmplx64_5, dev_ptr_cmplx64_6, dev_ptr_cmplx64_7)
#if defined _real128
   nullify( dev_ptr_cmplx128_1, dev_ptr_cmplx128_2, dev_ptr_cmplx128_3, dev_ptr_cmplx128_4, dev_ptr_cmplx128_5, dev_ptr_cmplx128_6, dev_ptr_cmplx128_7)
#endif
   nullify( dev_ptr_lgcl32_1, dev_ptr_lgcl32_2, dev_ptr_lgcl32_3, dev_ptr_lgcl32_4, dev_ptr_lgcl32_5, dev_ptr_lgcl32_6, dev_ptr_lgcl32_7)
#if defined _real128

   call omp_target_alloc_f(fptr_dev=dev_ptr_real128_1, dimensions=sz, omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real128_1)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real128_1, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real128_1, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_real128_2, dimensions=[sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real128_2)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real128_2, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real128_2, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_real128_3, dimensions=[sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real128_3)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real128_3, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real128_3, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_real128_4, dimensions=[sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real128_4)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real128_4, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real128_4, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_real128_5, dimensions=[sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real128_5)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real128_5, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real128_5, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_real128_6, dimensions=[sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real128_6)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real128_6, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real128_6, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_real128_7, dimensions=[sz,sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_real128_7)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_real128_7, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_real128_7, omp_dev=omp_get_default_device())
   endif
#endif

   !!! DMR Target Alloc 32 bits Complex tests
   nullify( dev_ptr_int8_1, dev_ptr_int8_2, dev_ptr_int8_3, dev_ptr_int8_4, dev_ptr_int8_5, dev_ptr_int8_6, dev_ptr_int8_7)
   nullify( dev_ptr_int16_1, dev_ptr_int16_2, dev_ptr_int16_3, dev_ptr_int16_4, dev_ptr_int16_5, dev_ptr_int16_6, dev_ptr_int16_7)
   nullify( dev_ptr_int32_1, dev_ptr_int32_2, dev_ptr_int32_3, dev_ptr_int32_4, dev_ptr_int32_5, dev_ptr_int32_6, dev_ptr_int32_7)
   nullify( dev_ptr_int64_1, dev_ptr_int64_2, dev_ptr_int64_3, dev_ptr_int64_4, dev_ptr_int64_5, dev_ptr_int64_6, dev_ptr_int64_7)
   nullify( dev_ptr_real32_1, dev_ptr_real32_2, dev_ptr_real32_3, dev_ptr_real32_4, dev_ptr_real32_5, dev_ptr_real32_6, dev_ptr_real32_7)
   nullify( dev_ptr_real64_1, dev_ptr_real64_2, dev_ptr_real64_3, dev_ptr_real64_4, dev_ptr_real64_5, dev_ptr_real64_6, dev_ptr_real64_7)
#if defined _real128
   nullify( dev_ptr_real128_1, dev_ptr_real128_2, dev_ptr_real128_3, dev_ptr_real128_4, dev_ptr_real128_5, dev_ptr_real128_6, dev_ptr_real128_7)
#endif
   nullify( dev_ptr_cmplx32_1, dev_ptr_cmplx32_2, dev_ptr_cmplx32_3, dev_ptr_cmplx32_4, dev_ptr_cmplx32_5, dev_ptr_cmplx32_6, dev_ptr_cmplx32_7)
   nullify( dev_ptr_cmplx64_1, dev_ptr_cmplx64_2, dev_ptr_cmplx64_3, dev_ptr_cmplx64_4, dev_ptr_cmplx64_5, dev_ptr_cmplx64_6, dev_ptr_cmplx64_7)
#if defined _real128
   nullify( dev_ptr_cmplx128_1, dev_ptr_cmplx128_2, dev_ptr_cmplx128_3, dev_ptr_cmplx128_4, dev_ptr_cmplx128_5, dev_ptr_cmplx128_6, dev_ptr_cmplx128_7)
#endif
   nullify( dev_ptr_lgcl32_1, dev_ptr_lgcl32_2, dev_ptr_lgcl32_3, dev_ptr_lgcl32_4, dev_ptr_lgcl32_5, dev_ptr_lgcl32_6, dev_ptr_lgcl32_7)

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx32_1, dimensions=sz, omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx32_1)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx32_1, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx32_1, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx32_2, dimensions=[sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx32_2)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx32_2, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx32_2, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx32_3, dimensions=[sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx32_3)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx32_3, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx32_3, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx32_4, dimensions=[sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx32_4)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx32_4, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx32_4, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx32_5, dimensions=[sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx32_5)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx32_5, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx32_5, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx32_6, dimensions=[sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx32_6)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx32_6, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx32_6, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx32_7, dimensions=[sz,sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx32_7)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx32_7, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx32_7, omp_dev=omp_get_default_device())
   endif

   !!! DMR Target Alloc 64 bits Complex tests
   nullify( dev_ptr_int8_1, dev_ptr_int8_2, dev_ptr_int8_3, dev_ptr_int8_4, dev_ptr_int8_5, dev_ptr_int8_6, dev_ptr_int8_7)
   nullify( dev_ptr_int16_1, dev_ptr_int16_2, dev_ptr_int16_3, dev_ptr_int16_4, dev_ptr_int16_5, dev_ptr_int16_6, dev_ptr_int16_7)
   nullify( dev_ptr_int32_1, dev_ptr_int32_2, dev_ptr_int32_3, dev_ptr_int32_4, dev_ptr_int32_5, dev_ptr_int32_6, dev_ptr_int32_7)
   nullify( dev_ptr_int64_1, dev_ptr_int64_2, dev_ptr_int64_3, dev_ptr_int64_4, dev_ptr_int64_5, dev_ptr_int64_6, dev_ptr_int64_7)
   nullify( dev_ptr_real32_1, dev_ptr_real32_2, dev_ptr_real32_3, dev_ptr_real32_4, dev_ptr_real32_5, dev_ptr_real32_6, dev_ptr_real32_7)
   nullify( dev_ptr_real64_1, dev_ptr_real64_2, dev_ptr_real64_3, dev_ptr_real64_4, dev_ptr_real64_5, dev_ptr_real64_6, dev_ptr_real64_7)
#if defined _real128
   nullify( dev_ptr_real128_1, dev_ptr_real128_2, dev_ptr_real128_3, dev_ptr_real128_4, dev_ptr_real128_5, dev_ptr_real128_6, dev_ptr_real128_7)
#endif
   nullify( dev_ptr_cmplx32_1, dev_ptr_cmplx32_2, dev_ptr_cmplx32_3, dev_ptr_cmplx32_4, dev_ptr_cmplx32_5, dev_ptr_cmplx32_6, dev_ptr_cmplx32_7)
   nullify( dev_ptr_cmplx64_1, dev_ptr_cmplx64_2, dev_ptr_cmplx64_3, dev_ptr_cmplx64_4, dev_ptr_cmplx64_5, dev_ptr_cmplx64_6, dev_ptr_cmplx64_7)
#if defined _real128
   nullify( dev_ptr_cmplx128_1, dev_ptr_cmplx128_2, dev_ptr_cmplx128_3, dev_ptr_cmplx128_4, dev_ptr_cmplx128_5, dev_ptr_cmplx128_6, dev_ptr_cmplx128_7)
#endif
   nullify( dev_ptr_lgcl32_1, dev_ptr_lgcl32_2, dev_ptr_lgcl32_3, dev_ptr_lgcl32_4, dev_ptr_lgcl32_5, dev_ptr_lgcl32_6, dev_ptr_lgcl32_7)

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx64_1, dimensions=sz, omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx64_1)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx64_1, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx64_1, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx64_2, dimensions=[sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx64_2)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx64_2, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx64_2, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx64_3, dimensions=[sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx64_3)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx64_3, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx64_3, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx64_4, dimensions=[sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx64_4)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx64_4, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx64_4, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx64_5, dimensions=[sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx64_5)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx64_5, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx64_5, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx64_6, dimensions=[sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx64_6)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx64_6, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx64_6, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx64_7, dimensions=[sz,sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx64_7)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx64_7, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx64_7, omp_dev=omp_get_default_device())
   endif

   !!! DMR Target Alloc 128 bits Complex tests
   nullify( dev_ptr_int8_1, dev_ptr_int8_2, dev_ptr_int8_3, dev_ptr_int8_4, dev_ptr_int8_5, dev_ptr_int8_6, dev_ptr_int8_7)
   nullify( dev_ptr_int16_1, dev_ptr_int16_2, dev_ptr_int16_3, dev_ptr_int16_4, dev_ptr_int16_5, dev_ptr_int16_6, dev_ptr_int16_7)
   nullify( dev_ptr_int32_1, dev_ptr_int32_2, dev_ptr_int32_3, dev_ptr_int32_4, dev_ptr_int32_5, dev_ptr_int32_6, dev_ptr_int32_7)
   nullify( dev_ptr_int64_1, dev_ptr_int64_2, dev_ptr_int64_3, dev_ptr_int64_4, dev_ptr_int64_5, dev_ptr_int64_6, dev_ptr_int64_7)
   nullify( dev_ptr_real32_1, dev_ptr_real32_2, dev_ptr_real32_3, dev_ptr_real32_4, dev_ptr_real32_5, dev_ptr_real32_6, dev_ptr_real32_7)
   nullify( dev_ptr_real64_1, dev_ptr_real64_2, dev_ptr_real64_3, dev_ptr_real64_4, dev_ptr_real64_5, dev_ptr_real64_6, dev_ptr_real64_7)
#if defined _real128
   nullify( dev_ptr_real128_1, dev_ptr_real128_2, dev_ptr_real128_3, dev_ptr_real128_4, dev_ptr_real128_5, dev_ptr_real128_6, dev_ptr_real128_7)
#endif
   nullify( dev_ptr_cmplx32_1, dev_ptr_cmplx32_2, dev_ptr_cmplx32_3, dev_ptr_cmplx32_4, dev_ptr_cmplx32_5, dev_ptr_cmplx32_6, dev_ptr_cmplx32_7)
   nullify( dev_ptr_cmplx64_1, dev_ptr_cmplx64_2, dev_ptr_cmplx64_3, dev_ptr_cmplx64_4, dev_ptr_cmplx64_5, dev_ptr_cmplx64_6, dev_ptr_cmplx64_7)
#if defined _real128
   nullify( dev_ptr_cmplx128_1, dev_ptr_cmplx128_2, dev_ptr_cmplx128_3, dev_ptr_cmplx128_4, dev_ptr_cmplx128_5, dev_ptr_cmplx128_6, dev_ptr_cmplx128_7)
#endif
   nullify( dev_ptr_lgcl32_1, dev_ptr_lgcl32_2, dev_ptr_lgcl32_3, dev_ptr_lgcl32_4, dev_ptr_lgcl32_5, dev_ptr_lgcl32_6, dev_ptr_lgcl32_7)
#if defined _real128

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx128_1, dimensions=sz, omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx128_1)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx128_1, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx128_1, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx128_2, dimensions=[sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx128_2)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx128_2, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx128_2, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx128_3, dimensions=[sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx128_3)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx128_3, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx128_3, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx128_4, dimensions=[sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx128_4)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx128_4, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx128_4, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx128_5, dimensions=[sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx128_5)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx128_5, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx128_5, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx128_6, dimensions=[sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx128_6)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx128_6, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx128_6, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_cmplx128_7, dimensions=[sz,sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_cmplx128_7)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_cmplx128_7, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_cmplx128_7, omp_dev=omp_get_default_device())
   endif
#endif

   !!! DMR Target Alloc 32 bits Logical tests
   nullify( dev_ptr_int8_1, dev_ptr_int8_2, dev_ptr_int8_3, dev_ptr_int8_4, dev_ptr_int8_5, dev_ptr_int8_6, dev_ptr_int8_7)
   nullify( dev_ptr_int16_1, dev_ptr_int16_2, dev_ptr_int16_3, dev_ptr_int16_4, dev_ptr_int16_5, dev_ptr_int16_6, dev_ptr_int16_7)
   nullify( dev_ptr_int32_1, dev_ptr_int32_2, dev_ptr_int32_3, dev_ptr_int32_4, dev_ptr_int32_5, dev_ptr_int32_6, dev_ptr_int32_7)
   nullify( dev_ptr_int64_1, dev_ptr_int64_2, dev_ptr_int64_3, dev_ptr_int64_4, dev_ptr_int64_5, dev_ptr_int64_6, dev_ptr_int64_7)
   nullify( dev_ptr_real32_1, dev_ptr_real32_2, dev_ptr_real32_3, dev_ptr_real32_4, dev_ptr_real32_5, dev_ptr_real32_6, dev_ptr_real32_7)
   nullify( dev_ptr_real64_1, dev_ptr_real64_2, dev_ptr_real64_3, dev_ptr_real64_4, dev_ptr_real64_5, dev_ptr_real64_6, dev_ptr_real64_7)
#if defined _real128
   nullify( dev_ptr_real128_1, dev_ptr_real128_2, dev_ptr_real128_3, dev_ptr_real128_4, dev_ptr_real128_5, dev_ptr_real128_6, dev_ptr_real128_7)
#endif
   nullify( dev_ptr_cmplx32_1, dev_ptr_cmplx32_2, dev_ptr_cmplx32_3, dev_ptr_cmplx32_4, dev_ptr_cmplx32_5, dev_ptr_cmplx32_6, dev_ptr_cmplx32_7)
   nullify( dev_ptr_cmplx64_1, dev_ptr_cmplx64_2, dev_ptr_cmplx64_3, dev_ptr_cmplx64_4, dev_ptr_cmplx64_5, dev_ptr_cmplx64_6, dev_ptr_cmplx64_7)
#if defined _real128
   nullify( dev_ptr_cmplx128_1, dev_ptr_cmplx128_2, dev_ptr_cmplx128_3, dev_ptr_cmplx128_4, dev_ptr_cmplx128_5, dev_ptr_cmplx128_6, dev_ptr_cmplx128_7)
#endif
   nullify( dev_ptr_lgcl32_1, dev_ptr_lgcl32_2, dev_ptr_lgcl32_3, dev_ptr_lgcl32_4, dev_ptr_lgcl32_5, dev_ptr_lgcl32_6, dev_ptr_lgcl32_7)

   call omp_target_alloc_f(fptr_dev=dev_ptr_lgcl32_1, dimensions=sz, omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_lgcl32_1)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_lgcl32_1, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_lgcl32_1, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_lgcl32_2, dimensions=[sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_lgcl32_2)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_lgcl32_2, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_lgcl32_2, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_lgcl32_3, dimensions=[sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_lgcl32_3)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_lgcl32_3, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_lgcl32_3, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_lgcl32_4, dimensions=[sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_lgcl32_4)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_lgcl32_4, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_lgcl32_4, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_lgcl32_5, dimensions=[sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_lgcl32_5)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_lgcl32_5, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_lgcl32_5, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_lgcl32_6, dimensions=[sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_lgcl32_6)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_lgcl32_6, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_lgcl32_6, omp_dev=omp_get_default_device())
   endif

   call omp_target_alloc_f(fptr_dev=dev_ptr_lgcl32_7, dimensions=[sz,sz,sz,sz,sz,sz,sz], omp_dev=omp_get_default_device(), ierr=ierr)

   test_tot = test_tot + 1
   if (.not.associated(dev_ptr_lgcl32_7)) then
      write(0,"(/,a,i5)") "Error testing allocation of dev_ptr_lgcl32_7, ierr:", ierr
      nfail = nfail + 1
   else
      call omp_target_free_f(fptr_dev=dev_ptr_lgcl32_7, omp_dev=omp_get_default_device())
   endif

  write(0,"(/,a)") "DMR Target Alloc Test SUMMARY:"
  write(0,"(3x,a,i5)") "# tests: ", test_tot
  write(0,"(3x,a,i5)") "# passed: ", test_tot - nfail
  write(0,"(3x,a,i5)") "# failed: ", nfail
  write(0,"()")
endprogram test_dmr_target_alloc