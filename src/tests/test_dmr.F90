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

program test_dmr
   use omp_lib
   use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, &
#if defined _real128
                                             real128, &
#endif
                                             real32, real64
   use dmr
   use dmr_c_functions
   use init_device_pointers
   use matmul_device_pointers
   use, intrinsic :: iso_c_binding

   implicit none

   integer(kind=int8),    pointer, contiguous :: fptr_dev_int8   (:),                fptr_dev_int8_2 (:,:),         &
                                                 fptr_dev_int8_3 (:,:,:),            fptr_dev_int8_4 (:,:,:,:),     &
                                                 fptr_dev_int8_5 (:,:,:,:,:),        fptr_dev_int8_6 (:,:,:,:,:,:), &
                                                 fptr_dev_int8_7 (:,:,:,:,:,:,:)
   integer(kind=int8),    target, allocatable :: fptr_hos_int8   (:),                fptr_hos_int8_2 (:,:),         &
                                                 fptr_hos_int8_3 (:,:,:),            fptr_hos_int8_4 (:,:,:,:),     &
                                                 fptr_hos_int8_5 (:,:,:,:,:),        fptr_hos_int8_6 (:,:,:,:,:,:), &
                                                 fptr_hos_int8_7 (:,:,:,:,:,:,:)
   integer(kind=int16),   pointer, contiguous :: fptr_dev_int16   (:),               fptr_dev_int16_2 (:,:),         &
                                                 fptr_dev_int16_3 (:,:,:),           fptr_dev_int16_4 (:,:,:,:),     &
                                                 fptr_dev_int16_5 (:,:,:,:,:),       fptr_dev_int16_6 (:,:,:,:,:,:), &
                                                 fptr_dev_int16_7 (:,:,:,:,:,:,:)
   integer(kind=int16),   target, allocatable :: fptr_hos_int16   (:),               fptr_hos_int16_2 (:,:),         &
                                                 fptr_hos_int16_3 (:,:,:),           fptr_hos_int16_4 (:,:,:,:),     &
                                                 fptr_hos_int16_5 (:,:,:,:,:),       fptr_hos_int16_6 (:,:,:,:,:,:), &
                                                 fptr_hos_int16_7 (:,:,:,:,:,:,:)
   integer(kind=int32),   pointer, contiguous :: fptr_dev_int32   (:),               fptr_dev_int32_2 (:,:),         &
                                                 fptr_dev_int32_3 (:,:,:),           fptr_dev_int32_4 (:,:,:,:),     &
                                                 fptr_dev_int32_5 (:,:,:,:,:),       fptr_dev_int32_6 (:,:,:,:,:,:), &
                                                 fptr_dev_int32_7 (:,:,:,:,:,:,:)
   integer(kind=int32),   target, allocatable :: fptr_hos_int32   (:),               fptr_hos_int32_2 (:,:),         &
                                                 fptr_hos_int32_3 (:,:,:),           fptr_hos_int32_4 (:,:,:,:),     &
                                                 fptr_hos_int32_5 (:,:,:,:,:),       fptr_hos_int32_6 (:,:,:,:,:,:), &
                                                 fptr_hos_int32_7 (:,:,:,:,:,:,:)
   integer(kind=int64),   pointer, contiguous :: fptr_dev_int64   (:),               fptr_dev_int64_2 (:,:),         &
                                                 fptr_dev_int64_3 (:,:,:),           fptr_dev_int64_4 (:,:,:,:),     &
                                                 fptr_dev_int64_5 (:,:,:,:,:),       fptr_dev_int64_6 (:,:,:,:,:,:), &
                                                 fptr_dev_int64_7 (:,:,:,:,:,:,:)
   integer(kind=int64),   target, allocatable :: fptr_hos_int64   (:),               fptr_hos_int64_2 (:,:),         &
                                                 fptr_hos_int64_3 (:,:,:),           fptr_hos_int64_4 (:,:,:,:),     &
                                                 fptr_hos_int64_5 (:,:,:,:,:),       fptr_hos_int64_6 (:,:,:,:,:,:), &
                                                 fptr_hos_int64_7 (:,:,:,:,:,:,:)
   real(kind=real32),     pointer, contiguous :: fptr_dev_real32   (:),              fptr_dev_real32_2 (:,:),         &
                                                 fptr_dev_real32_3 (:,:,:),          fptr_dev_real32_4 (:,:,:,:),     &
                                                 fptr_dev_real32_5 (:,:,:,:,:),      fptr_dev_real32_6 (:,:,:,:,:,:), &
                                                 fptr_dev_real32_7 (:,:,:,:,:,:,:)
   real(kind=real32),     target, allocatable :: fptr_hos_real32   (:),              fptr_hos_real32_2 (:,:),         &
                                                 fptr_hos_real32_3 (:,:,:),          fptr_hos_real32_4 (:,:,:,:),     &
                                                 fptr_hos_real32_5 (:,:,:,:,:),      fptr_hos_real32_6 (:,:,:,:,:,:), &
                                                 fptr_hos_real32_7 (:,:,:,:,:,:,:)
   real(kind=real64),     pointer, contiguous :: fptr_dev_real64   (:),              fptr_dev_real64_2 (:,:),         &
                                                 fptr_dev_real64_3 (:,:,:),          fptr_dev_real64_4 (:,:,:,:),     &
                                                 fptr_dev_real64_5 (:,:,:,:,:),      fptr_dev_real64_6 (:,:,:,:,:,:), &
                                                 fptr_dev_real64_7 (:,:,:,:,:,:,:)
   real(kind=real64),     target, allocatable :: fptr_hos_real64   (:),              fptr_hos_real64_2 (:,:),         &
                                                 fptr_hos_real64_3 (:,:,:),          fptr_hos_real64_4 (:,:,:,:),     &
                                                 fptr_hos_real64_5 (:,:,:,:,:),      fptr_hos_real64_6 (:,:,:,:,:,:), &
                                                 fptr_hos_real64_7 (:,:,:,:,:,:,:)
#if defined _real128
   real(kind=real128),    pointer, contiguous :: fptr_dev_real128  (:),              fptr_dev_real128_2(:,:),         &
                                                 fptr_dev_real128_3(:,:,:),          fptr_dev_real128_4(:,:,:,:),     &
                                                 fptr_dev_real128_5(:,:,:,:,:),      fptr_dev_real128_6(:,:,:,:,:,:), &
                                                 fptr_dev_real128_7(:,:,:,:,:,:,:)
   real(kind=real128),    target, allocatable :: fptr_hos_real128  (:),              fptr_hos_real128_2(:,:),         &
                                                 fptr_hos_real128_3(:,:,:),          fptr_hos_real128_4(:,:,:,:),     &
                                                 fptr_hos_real128_5(:,:,:,:,:),      fptr_hos_real128_6(:,:,:,:,:,:), &
                                                 fptr_hos_real128_7(:,:,:,:,:,:,:)
#endif
   complex(kind=real32),  pointer, contiguous :: fptr_dev_cmplx32   (:),             fptr_dev_cmplx32_2 (:,:),         &
                                                 fptr_dev_cmplx32_3 (:,:,:),         fptr_dev_cmplx32_4 (:,:,:,:),     &
                                                 fptr_dev_cmplx32_5 (:,:,:,:,:),     fptr_dev_cmplx32_6 (:,:,:,:,:,:), &
                                                 fptr_dev_cmplx32_7 (:,:,:,:,:,:,:)
   complex(kind=real32),  target, allocatable :: fptr_hos_cmplx32   (:),             fptr_hos_cmplx32_2 (:,:),         &
                                                 fptr_hos_cmplx32_3 (:,:,:),         fptr_hos_cmplx32_4 (:,:,:,:),     &
                                                 fptr_hos_cmplx32_5 (:,:,:,:,:),     fptr_hos_cmplx32_6 (:,:,:,:,:,:), &
                                                 fptr_hos_cmplx32_7 (:,:,:,:,:,:,:)
   complex(kind=real64),  pointer, contiguous :: fptr_dev_cmplx64   (:),             fptr_dev_cmplx64_2 (:,:),         &
                                                 fptr_dev_cmplx64_3 (:,:,:),         fptr_dev_cmplx64_4 (:,:,:,:),     &
                                                 fptr_dev_cmplx64_5 (:,:,:,:,:),     fptr_dev_cmplx64_6 (:,:,:,:,:,:), &
                                                 fptr_dev_cmplx64_7 (:,:,:,:,:,:,:)
   complex(kind=real64),  target, allocatable :: fptr_hos_cmplx64   (:),             fptr_hos_cmplx64_2 (:,:),         &
                                                 fptr_hos_cmplx64_3 (:,:,:),         fptr_hos_cmplx64_4 (:,:,:,:),     &
                                                 fptr_hos_cmplx64_5 (:,:,:,:,:),     fptr_hos_cmplx64_6 (:,:,:,:,:,:), &
                                                 fptr_hos_cmplx64_7 (:,:,:,:,:,:,:)
#if defined _real128
   complex(kind=real128), pointer, contiguous :: fptr_dev_cmplx128  (:),             fptr_dev_cmplx128_2(:,:),         &
                                                 fptr_dev_cmplx128_3(:,:,:),         fptr_dev_cmplx128_4(:,:,:,:),     &
                                                 fptr_dev_cmplx128_5(:,:,:,:,:),     fptr_dev_cmplx128_6(:,:,:,:,:,:), &
                                                 fptr_dev_cmplx128_7(:,:,:,:,:,:,:)
   complex(kind=real128), target, allocatable :: fptr_hos_cmplx128  (:),             fptr_hos_cmplx128_2(:,:),         &
                                                 fptr_hos_cmplx128_3(:,:,:),         fptr_hos_cmplx128_4(:,:,:,:),     &
                                                 fptr_hos_cmplx128_5(:,:,:,:,:),     fptr_hos_cmplx128_6(:,:,:,:,:,:), &
                                                 fptr_hos_cmplx128_7(:,:,:,:,:,:,:)
#endif

   integer(kind=int64), parameter :: i = 20_int64
   integer(kind=int64), parameter :: j = 10_int64
   integer(kind=int64)            :: siz2(2)=i, siz3(3)=i, siz4(4)=i, siz5(5)=i, siz6(6)=i, siz7(7)=i
   integer(kind=int64)            :: dims(2), k
   integer(kind=int32)            :: ierr
   integer(kind=int32)            :: omp_initial, omp_default
   type(c_ptr)                    :: cptr_dev, cptr_hos
   integer(kind=c_int)            :: errr

   omp_default = omp_get_default_device()
   omp_initial = omp_get_initial_device_c()

   dims(1) = 2_int64
   dims(2) = 4_int64

   print *, '*****************************************************************'
   print *, '                    Start DMR library testing                    '
   print *, '*****************************************************************'
   print *, ''
   print *, ' +++++++++++++++++++ Testing rank one arrays +++++++++++++++++++ '
   print *, ''
   print *, '- - - - - - - - - - - - -Integer arrays- - - - - - - - - - - - - '
   print *, ''
   print *, '                       I1P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_int8, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_int8)
   call init_I(fptr_dev_int8, i)
   print *, 'Device pointer initialization completed'
   call matmul_I(fptr_dev_int8, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_int8(i)); fptr_hos_int8 = 0_int8
   call omp_target_memcpy_f(fptr_hos_int8, fptr_dev_int8, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_int8(1), fptr_hos_int8(i)
   call omp_target_free_f(fptr_dev_int8, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_int8)

   print *, ''
   print *, '                       I1P Host to Device                        '
   print *, ''
   allocate(fptr_hos_int8(i)); fptr_hos_int8 = 5_int8
   call omp_target_alloc_f(fptr_dev_int8, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_int8)
   call omp_target_memcpy_f(fptr_dev_int8, fptr_hos_int8, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_int8 = 0_int8
   call omp_target_memcpy_f(fptr_hos_int8, fptr_dev_int8, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_int8(1), fptr_hos_int8(i)
   call omp_target_free_f(fptr_dev_int8, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_int8)

   print *, ''
   print *, '                       I2P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_int16, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_int16)
   call init_I(fptr_dev_int16, i)
   print *, 'Device pointer initialization completed'
   call matmul_I(fptr_dev_int16, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_int16(i)); fptr_hos_int16 = 0_int16
   call omp_target_memcpy_f(fptr_hos_int16, fptr_dev_int16, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_int16(1), fptr_hos_int16(i)
   call omp_target_free_f(fptr_dev_int16, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_int16)

   print *, ''
   print *, '                       I2P Host to Device                        '
   print *, ''
   allocate(fptr_hos_int16(i)); fptr_hos_int16 = 5_int16
   call omp_target_alloc_f(fptr_dev_int16, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_int16)
   call omp_target_memcpy_f(fptr_dev_int16, fptr_hos_int16, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_int16 = 0_int16
   call omp_target_memcpy_f(fptr_hos_int16, fptr_dev_int16, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_int16(1), fptr_hos_int16(i)
   call omp_target_free_f(fptr_dev_int16, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_int16)

   print *, ''
   print *, '                       I4P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_int32, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_int32)
   call init_I(fptr_dev_int32, i)
   print *, 'Device pointer initialization completed'
   call matmul_I(fptr_dev_int32, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_int32(i)); fptr_hos_int32 = 0_int32
   call omp_target_memcpy_f(fptr_hos_int32, fptr_dev_int32, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_int32(1), fptr_hos_int32(i)
   call omp_target_free_f(fptr_dev_int32, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_int32)

   print *, ''
   print *, '                       I4P Host to Device                        '
   print *, ''
   allocate(fptr_hos_int32(i)); fptr_hos_int32 = 5_int32
   call omp_target_alloc_f(fptr_dev_int32, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_int32)
   call omp_target_memcpy_f(fptr_dev_int32, fptr_hos_int32, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_int32 = 0_int32
   call omp_target_memcpy_f(fptr_hos_int32, fptr_dev_int32, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_int32(1), fptr_hos_int32(i)
   call omp_target_free_f(fptr_dev_int32, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_int32)

   print *, ''
   print *, '                       I8P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_int64, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_int64)
   call init_I(fptr_dev_int64, i)
   print *, 'Device pointer initialization completed'
   call matmul_I(fptr_dev_int64, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_int64(i)); fptr_hos_int64 = 0_int64
   call omp_target_memcpy_f(fptr_hos_int64, fptr_dev_int64, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_int64(1), fptr_hos_int64(i)
   call omp_target_free_f(fptr_dev_int64, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_int64)

   print *, ''
   print *, '                       I8P Host to Device                        '
   print *, ''
   allocate(fptr_hos_int64(i)); fptr_hos_int64 = 5_int64
   call omp_target_alloc_f(fptr_dev_int64, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_int64)
   call omp_target_memcpy_f(fptr_dev_int64, fptr_hos_int64, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_int64 = 0_int64
   call omp_target_memcpy_f(fptr_hos_int64, fptr_dev_int64, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_int64(1), fptr_hos_int64(i)
   call omp_target_free_f(fptr_dev_int64, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_int64)

   print *, ''
   print *, '- - - - - - - - - - - - - -Real arrays- - - - - - - - - - - - - -'
   print *, ''
   print *, '                       R4P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_real32, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_real32)
   call init_R(fptr_dev_real32, i)
   print *, 'Device pointer initialization completed'
   call matmul_R(fptr_dev_real32, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_real32(i)); fptr_hos_real32 = 0_real32
   call omp_target_memcpy_f(fptr_hos_real32, fptr_dev_real32, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_real32(1), fptr_hos_real32(i)
   call omp_target_free_f(fptr_dev_real32, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_real32)

   print *, ''
   print *, '                       R4P Host to Device                        '
   print *, ''
   allocate(fptr_hos_real32(i)); fptr_hos_real32 = 5.0_real32
   call omp_target_alloc_f(fptr_dev_real32, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_real32)
   call omp_target_memcpy_f(fptr_dev_real32, fptr_hos_real32, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_real32 = 0.0_real32
   call omp_target_memcpy_f(fptr_hos_real32, fptr_dev_real32, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_real32(1), fptr_hos_real32(i)
   call omp_target_free_f(fptr_dev_real32, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_real32)

   print *, ''
   print *, '                       R8P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_real64, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_real64)
   call init_R(fptr_dev_real64, i)
   print *, 'Device pointer initialization completed'
   call matmul_R(fptr_dev_real64, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_real64(i)); fptr_hos_real64 = 0.0_real64
   call omp_target_memcpy_f(fptr_hos_real64, fptr_dev_real64, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_real64(1), fptr_hos_real64(i)
   call omp_target_free_f(fptr_dev_real64, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_real64)

   print *, ''
   print *, '                       R8P Host to Device                        '
   print *, ''
   allocate(fptr_hos_real64(i)); fptr_hos_real64 = 5.0_real64
   call omp_target_alloc_f(fptr_dev_real64, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_real64)
   call omp_target_memcpy_f(fptr_dev_real64, fptr_hos_real64, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_real64 = 0.0_real64
   call omp_target_memcpy_f(fptr_hos_real64, fptr_dev_real64, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_real64(1), fptr_hos_real64(i)
   call omp_target_free_f(fptr_dev_real64, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_real64)

#if defined _real128
   print *, ''
   print *, '                       R16P Device to Host                       '
   print *, ''
   call omp_target_alloc_f(fptr_dev_real128, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_real128)
   call init_R(fptr_dev_real128, i)
   print *, 'Device pointer initialization completed'
   call matmul_R(fptr_dev_real128, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_real128(i)); fptr_hos_real128 = 0.0_real128
   call omp_target_memcpy_f(fptr_hos_real128, fptr_dev_real128, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_real128(1), fptr_hos_real128(i)
   call omp_target_free_f(fptr_dev_real128, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_real128)

   print *, ''
   print *, '                       R16P Host to Device                       '
   print *, ''
   allocate(fptr_hos_real128(i)); fptr_hos_real128 = 5.0_real128
   call omp_target_alloc_f(fptr_dev_real128, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_real128)
   call omp_target_memcpy_f(fptr_dev_real128, fptr_hos_real128, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_real128 = 0.0_real128
   call omp_target_memcpy_f(fptr_hos_real128, fptr_dev_real128, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_real128(1), fptr_hos_real128(i)
   call omp_target_free_f(fptr_dev_real128, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_real128)
#endif

   print *, ''
   print *, '- - - - - - - - - - - - -Complex arrays- - - - - - - - - - - - - '
   print *, ''
   print *, '                       C4P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_cmplx32, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_cmplx32)
   call init_C(fptr_dev_cmplx32, i)
   print *, 'Device pointer initialization completed'
   call matmul_C(fptr_dev_cmplx32, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_cmplx32(i)); fptr_hos_cmplx32 = (0.0_real32, 0.0_real32)
   call omp_target_memcpy_f(fptr_hos_cmplx32, fptr_dev_cmplx32, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_cmplx32(1), fptr_hos_cmplx32(i)
   call omp_target_free_f(fptr_dev_cmplx32, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_cmplx32)

   print *, ''
   print *, '                       C4P Host to Device                        '
   print *, ''
   allocate(fptr_hos_cmplx32(i)); fptr_hos_cmplx32 = (5.0_real32, 5.0_real32)
   call omp_target_alloc_f(fptr_dev_cmplx32, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_cmplx32)
   call omp_target_memcpy_f(fptr_dev_cmplx32, fptr_hos_cmplx32, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_cmplx32 = (0.0_real32, 0.0_real32)
   call omp_target_memcpy_f(fptr_hos_cmplx32, fptr_dev_cmplx32, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_cmplx32(1), fptr_hos_cmplx32(i)
   call omp_target_free_f(fptr_dev_cmplx32, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_cmplx32)

   print *, ''
   print *, '                       C8P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_cmplx64, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_cmplx64)
   call init_C(fptr_dev_cmplx64, i)
   print *, 'Device pointer initialization completed'
   call matmul_C(fptr_dev_cmplx64, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_cmplx64(i)); fptr_hos_cmplx64 = (0.0_real64, 0.0_real64)
   call omp_target_memcpy_f(fptr_hos_cmplx64, fptr_dev_cmplx64, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_cmplx64(1), fptr_hos_cmplx64(i)
   call omp_target_free_f(fptr_dev_cmplx64, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_cmplx64)

   print *, ''
   print *, '                       C8P Host to Device                        '
   print *, ''
   allocate(fptr_hos_cmplx64(i)); fptr_hos_cmplx64 = (5.0_real64, 5.0_real64)
   call omp_target_alloc_f(fptr_dev_cmplx64, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_cmplx64)
   call omp_target_memcpy_f(fptr_dev_cmplx64, fptr_hos_cmplx64, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_cmplx64 = (0.0_real64, 0.0_real64)
   call omp_target_memcpy_f(fptr_hos_cmplx64, fptr_dev_cmplx64, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_cmplx64(1), fptr_hos_cmplx64(i)
   call omp_target_free_f(fptr_dev_cmplx64, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_cmplx64)

#if defined _real128
   print *, ''
   print *, '                       C16P Device to Host                       '
   print *, ''
   call omp_target_alloc_f(fptr_dev_cmplx128, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_cmplx128)
   call init_C(fptr_dev_cmplx128, i)
   print *, 'Device pointer initialization completed'
   call matmul_C(fptr_dev_cmplx128, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_cmplx128(i)); fptr_hos_cmplx128 = (0.0_real128, 0.0_real128)
   call omp_target_memcpy_f(fptr_hos_cmplx128, fptr_dev_cmplx128, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_cmplx128(1), fptr_hos_cmplx128(i)
   call omp_target_free_f(fptr_dev_cmplx128, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_cmplx128)

   print *, ''
   print *, '                       C16P Host to Device                       '
   print *, ''
   allocate(fptr_hos_cmplx128(i)); fptr_hos_cmplx128 = (5.0_real128, 5.0_real128)
   call omp_target_alloc_f(fptr_dev_cmplx128, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_cmplx128)
   call omp_target_memcpy_f(fptr_dev_cmplx128, fptr_hos_cmplx128, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_cmplx128 = (0.0_real128, 0.0_real128)
   call omp_target_memcpy_f(fptr_hos_cmplx128, fptr_dev_cmplx128, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_cmplx128(1), fptr_hos_cmplx128(i)
   call omp_target_free_f(fptr_dev_cmplx128, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_cmplx128)
#endif

   print *, ''
   print *, ' +++++++++++++++++++ Testing rank two arrays +++++++++++++++++++ '
   print *, ''
   print *, '- - - - - - - - - - - - -Integer arrays- - - - - - - - - - - - - '
   print *, ''
   print *, '                       I1P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_int8_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_int8_2)
   call init_I(fptr_dev_int8_2, i)
   print *, 'F pointer initialization completed'
   call matmul_I(fptr_dev_int8_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_int8_2(i,i)); fptr_hos_int8_2 = 0_int8
   call omp_target_memcpy_f(fptr_hos_int8_2, fptr_dev_int8_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_int8_2(1,1), fptr_hos_int8_2(i,i)
   call omp_target_free_f(fptr_dev_int8_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_int8_2)

   print *, ''
   print *, '                     I1P Device to Host RECT                     '
   print *, ''
   call omp_target_alloc_f(fptr_dev_int8_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_int8_2)
   call init_I(fptr_dev_int8_2, i)
   print *, 'F pointer initialization completed'
   call matmul_I(fptr_dev_int8_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_int8_2(i,i)); fptr_hos_int8_2 = 0_int8
   call omp_target_memcpy_f(fptr_hos_int8_2, fptr_dev_int8_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device pointer has values:'
   do k=1,i
      print *, fptr_hos_int8_2(k,:)
   enddo
   fptr_hos_int8_2 = 0_int8
   call omp_target_memcpy_rect_f(fptr_hos_int8_2, fptr_dev_int8_2, dims, ierr, [1_int32, 1_int32], &
                            [6_int32, 6_int32], omp_initial, omp_default)
   print *, 'Host pointer has values:'
   do k=1,i
      print *, fptr_hos_int8_2(k,:)
   enddo
   call omp_target_free_f(fptr_dev_int8_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_int8_2)

   print *, ''
   print *, '                       I1P Host to Device                        '
   print *, ''
   allocate(fptr_hos_int8_2(i,i)); fptr_hos_int8_2 = 5_int8
   call omp_target_alloc_f(fptr_dev_int8_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_int8_2)
   call omp_target_memcpy_f(fptr_dev_int8_2, fptr_hos_int8_2, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_int8_2 = 0_int8
   call omp_target_memcpy_f(fptr_hos_int8_2, fptr_dev_int8_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_int8_2(1,1), fptr_hos_int8_2(i,i)
   call omp_target_free_f(fptr_dev_int8_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_int8_2)

   print *, ''
   print *, '                       I2P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_int16_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_int16_2)
   call init_I(fptr_dev_int16_2, i)
   print *, 'F pointer initialization completed'
   call matmul_I(fptr_dev_int16_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_int16_2(i,i)); fptr_hos_int16_2 = 0_int8
   call omp_target_memcpy_f(fptr_hos_int16_2, fptr_dev_int16_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_int16_2(1,1), fptr_hos_int16_2(i,i)
   call omp_target_free_f(fptr_dev_int16_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_int16_2)

   print *, ''
   print *, '                       I2P Host to Device                        '
   print *, ''
   allocate(fptr_hos_int16_2(i,i)); fptr_hos_int16_2 = 5_int16
   call omp_target_alloc_f(fptr_dev_int16_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_int16_2)
   call omp_target_memcpy_f(fptr_dev_int16_2, fptr_hos_int16_2, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_int16_2 = 0_int16
   call omp_target_memcpy_f(fptr_hos_int16_2, fptr_dev_int16_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_int16_2(1,1), fptr_hos_int16_2(i,i)
   call omp_target_free_f(fptr_dev_int16_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_int16_2)

   print *, ''
   print *, '                       I4P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_int32_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_int32_2)
   call init_I(fptr_dev_int32_2, i)
   print *, 'F pointer initialization completed'
   call matmul_I(fptr_dev_int32_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_int32_2(i,i)); fptr_hos_int32_2 = 0_int8
   call omp_target_memcpy_f(fptr_hos_int32_2, fptr_dev_int32_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_int32_2(1,1), fptr_hos_int32_2(i,i)
   call omp_target_free_f(fptr_dev_int32_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_int32_2)

   print *, ''
   print *, '                       I4P Host to Device                        '
   print *, ''
   allocate(fptr_hos_int32_2(i,i)); fptr_hos_int32_2 = 5_int32
   call omp_target_alloc_f(fptr_dev_int32_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_int32_2)
   call omp_target_memcpy_f(fptr_dev_int32_2, fptr_hos_int32_2, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_int32_2 = 0_int32
   call omp_target_memcpy_f(fptr_hos_int32_2, fptr_dev_int32_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_int32_2(1,1), fptr_hos_int32_2(i,i)
   call omp_target_free_f(fptr_dev_int32_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_int32_2)

   print *, ''
   print *, '                       I8P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_int64_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_int64_2)
   call init_I(fptr_dev_int64_2, i)
   print *, 'F pointer initialization completed'
   call matmul_I(fptr_dev_int64_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_int64_2(i,i)); fptr_hos_int64_2 = 0_int8
   call omp_target_memcpy_f(fptr_hos_int64_2, fptr_dev_int64_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_int64_2(1,1), fptr_hos_int64_2(i,i)
   call omp_target_free_f(fptr_dev_int64_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_int64_2)

   print *, ''
   print *, '                       I8P Host to Device                        '
   print *, ''
   allocate(fptr_hos_int64_2(i,i)); fptr_hos_int64_2 = 5_int64
   call omp_target_alloc_f(fptr_dev_int64_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_int64_2)
   call omp_target_memcpy_f(fptr_dev_int64_2, fptr_hos_int64_2, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_int64_2 = 0_int64
   call omp_target_memcpy_f(fptr_hos_int64_2, fptr_dev_int64_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_int64_2(1,1), fptr_hos_int64_2(i,i)
   call omp_target_free_f(fptr_dev_int64_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_int64_2)

   print *, ''
   print *, '- - - - - - - - - - - - - -Real arrays- - - - - - - - - - - - - -'
   print *, ''
   print *, '                       R4P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_real32_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_real32_2)
   call init_R(fptr_dev_real32_2, i)
   print *, 'F pointer initialization completed'
   call matmul_R(fptr_dev_real32_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_real32_2(i,i)); fptr_hos_real32_2 = 0_int8
   call omp_target_memcpy_f(fptr_hos_real32_2, fptr_dev_real32_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_real32_2(1,1), fptr_hos_real32_2(i,i)
   call omp_target_free_f(fptr_dev_real32_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_real32_2)

   print *, ''
   print *, '                       R4P Host to Device                        '
   print *, ''
   allocate(fptr_hos_real32_2(i,i)); fptr_hos_real32_2 = 5.0_real32
   call omp_target_alloc_f(fptr_dev_real32_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_real32_2)
   call omp_target_memcpy_f(fptr_dev_real32_2, fptr_hos_real32_2, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_real32_2 = 0.0_real32
   call omp_target_memcpy_f(fptr_hos_real32_2, fptr_dev_real32_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_real32_2(1,1), fptr_hos_real32_2(i,i)
   call omp_target_free_f(fptr_dev_real32_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_real32_2)

   print *, ''
   print *, '                       R8P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_real64_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_real64_2)
   call init_R(fptr_dev_real64_2, i)
   print *, 'F pointer initialization completed'
   call matmul_R(fptr_dev_real64_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_real64_2(i,i)); fptr_hos_real64_2 = 0_int8
   call omp_target_memcpy_f(fptr_hos_real64_2, fptr_dev_real64_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_real64_2(1,1), fptr_hos_real64_2(i,i)
   call omp_target_free_f(fptr_dev_real64_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_real64_2)

   print *, ''
   print *, '                       R8P Host to Device                        '
   print *, ''
   allocate(fptr_hos_real64_2(i,i)); fptr_hos_real64_2 = 5.0_real64
   call omp_target_alloc_f(fptr_dev_real64_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_real64_2)
   call omp_target_memcpy_f(fptr_dev_real64_2, fptr_hos_real64_2, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_real64_2 = 0.0_real64
   call omp_target_memcpy_f(fptr_hos_real64_2, fptr_dev_real64_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_real64_2(1,1), fptr_hos_real64_2(i,i)
   call omp_target_free_f(fptr_dev_real64_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_real64_2)

#if defined _real128
   print *, ''
   print *, '                       R16P Device to Host                       '
   print *, ''
   call omp_target_alloc_f(fptr_dev_real128_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_real128_2)
   call init_R(fptr_dev_real128_2, i)
   print *, 'F pointer initialization completed'
   call matmul_R(fptr_dev_real128_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_real128_2(i,i)); fptr_hos_real128_2 = 0_int8
   call omp_target_memcpy_f(fptr_hos_real128_2, fptr_dev_real128_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_real128_2(1,1), fptr_hos_real128_2(i,i)
   call omp_target_free_f(fptr_dev_real128_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_real128_2)

   print *, ''
   print *, '                       R16P Host to Device                       '
   print *, ''
   allocate(fptr_hos_real128_2(i,i)); fptr_hos_real128_2 = 5.0_real128
   call omp_target_alloc_f(fptr_dev_real128_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_real128_2)
   call omp_target_memcpy_f(fptr_dev_real128_2, fptr_hos_real128_2, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_real128_2 = 0.0_real128
   call omp_target_memcpy_f(fptr_hos_real128_2, fptr_dev_real128_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_real128_2(1,1), fptr_hos_real128_2(i,i)
   call omp_target_free_f(fptr_dev_real128_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_real128_2)
#endif

   print *, ''
   print *, '- - - - - - - - - - - - -Complex arrays- - - - - - - - - - - - - '
   print *, ''
   print *, '                       C4P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_cmplx32_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_cmplx32_2)
   call init_C(fptr_dev_cmplx32_2, i)
   print *, 'F pointer initialization completed'
   call matmul_C(fptr_dev_cmplx32_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_cmplx32_2(i,i)); fptr_hos_cmplx32_2 = 0_int8
   call omp_target_memcpy_f(fptr_hos_cmplx32_2, fptr_dev_cmplx32_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_cmplx32_2(1,1), fptr_hos_cmplx32_2(i,i)
   call omp_target_free_f(fptr_dev_cmplx32_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_cmplx32_2)

   print *, ''
   print *, '                       C4P Host to Device                        '
   print *, ''
   allocate(fptr_hos_cmplx32_2(i,i)); fptr_hos_cmplx32_2 = (5.0_real32, 5.0_real32)
   call omp_target_alloc_f(fptr_dev_cmplx32_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_cmplx32_2)
   call omp_target_memcpy_f(fptr_dev_cmplx32_2, fptr_hos_cmplx32_2, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_cmplx32_2 = (0.0_real32, 0.0_real32)
   call omp_target_memcpy_f(fptr_hos_cmplx32_2, fptr_dev_cmplx32_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_cmplx32_2(1,1), fptr_hos_cmplx32_2(i,i)
   call omp_target_free_f(fptr_dev_cmplx32_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_cmplx32_2)

   print *, ''
   print *, '                       C8P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_cmplx64_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_cmplx64_2)
   call init_C(fptr_dev_cmplx64_2, i)
   print *, 'F pointer initialization completed'
   call matmul_C(fptr_dev_cmplx64_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_cmplx64_2(i,i)); fptr_hos_cmplx64_2 = 0_int8
   call omp_target_memcpy_f(fptr_hos_cmplx64_2, fptr_dev_cmplx64_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_cmplx64_2(1,1), fptr_hos_cmplx64_2(i,i)
   call omp_target_free_f(fptr_dev_cmplx64_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_cmplx64_2)

   print *, ''
   print *, '                       C8P Host to Device                        '
   print *, ''
   allocate(fptr_hos_cmplx64_2(i,i)); fptr_hos_cmplx64_2 = (5.0_real64, 5.0_real64)
   call omp_target_alloc_f(fptr_dev_cmplx64_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_cmplx64_2)
   call omp_target_memcpy_f(fptr_dev_cmplx64_2, fptr_hos_cmplx64_2, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_cmplx64_2 = (0.0_real64, 0.0_real64)
   call omp_target_memcpy_f(fptr_hos_cmplx64_2, fptr_dev_cmplx64_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_cmplx64_2(1,1), fptr_hos_cmplx64_2(i,i)
   call omp_target_free_f(fptr_dev_cmplx64_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_cmplx64_2)

#if defined _real128
   print *, ''
   print *, '                       C16P Device to Host                       '
   print *, ''
   call omp_target_alloc_f(fptr_dev_cmplx128_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_cmplx128_2)
   call init_C(fptr_dev_cmplx128_2, i)
   print *, 'F pointer initialization completed'
   call matmul_C(fptr_dev_cmplx128_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_cmplx128_2(i,i)); fptr_hos_cmplx128_2 = 0_int8
   call omp_target_memcpy_f(fptr_hos_cmplx128_2, fptr_dev_cmplx128_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_cmplx128_2(1,1), fptr_hos_cmplx128_2(i,i)
   call omp_target_free_f(fptr_dev_cmplx128_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_cmplx128_2)

   print *, ''
   print *, '                       C16P Host to Device                       '
   print *, ''
   allocate(fptr_hos_cmplx128_2(i,i)); fptr_hos_cmplx128_2 = (5.0_real128, 5.0_real128)
   call omp_target_alloc_f(fptr_dev_cmplx128_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_cmplx128_2)
   call omp_target_memcpy_f(fptr_dev_cmplx128_2, fptr_hos_cmplx128_2, ierr, 0_int32, 0_int32, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_cmplx128_2 = (0.0_real128, 0.0_real128)
   call omp_target_memcpy_f(fptr_hos_cmplx128_2, fptr_dev_cmplx128_2, ierr, 0_int32, 0_int32, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_cmplx128_2(1,1), fptr_hos_cmplx128_2(i,i)
   call omp_target_free_f(fptr_dev_cmplx128_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_cmplx128_2)
#endif

endprogram test_dmr
