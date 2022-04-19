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

module dmr
   use, intrinsic :: iso_c_binding
   use omp_lib
   use dmr_environment

   implicit none

   interface omp_target_is_present_f
      module procedure &
                       omp_target_is_present_f_lgcl32_1, omp_target_is_present_f_lgcl32_2, omp_target_is_present_f_lgcl32_3, &
                       omp_target_is_present_f_lgcl32_4, omp_target_is_present_f_lgcl32_5, omp_target_is_present_f_lgcl32_6, &
                       omp_target_is_present_f_lgcl32_7, &
                       omp_target_is_present_f_cmplx32_1, omp_target_is_present_f_cmplx32_2, omp_target_is_present_f_cmplx32_3, &
                       omp_target_is_present_f_cmplx32_4, omp_target_is_present_f_cmplx32_5, omp_target_is_present_f_cmplx32_6, &
                       omp_target_is_present_f_cmplx32_7, &
                       omp_target_is_present_f_cmplx64_1, omp_target_is_present_f_cmplx64_2, omp_target_is_present_f_cmplx64_3, &
                       omp_target_is_present_f_cmplx64_4, omp_target_is_present_f_cmplx64_5, omp_target_is_present_f_cmplx64_6, &
                       omp_target_is_present_f_cmplx64_7, &
#if defined _real128
                       omp_target_is_present_f_cmplx128_1, omp_target_is_present_f_cmplx128_2, omp_target_is_present_f_cmplx128_3, &
                       omp_target_is_present_f_cmplx128_4, omp_target_is_present_f_cmplx128_5, omp_target_is_present_f_cmplx128_6, &
                       omp_target_is_present_f_cmplx128_7, &
#endif
                       omp_target_is_present_f_real32_1, omp_target_is_present_f_real32_2, omp_target_is_present_f_real32_3, &
                       omp_target_is_present_f_real32_4, omp_target_is_present_f_real32_5, omp_target_is_present_f_real32_6, &
                       omp_target_is_present_f_real32_7, &
                       omp_target_is_present_f_real64_1, omp_target_is_present_f_real64_2, omp_target_is_present_f_real64_3, &
                       omp_target_is_present_f_real64_4, omp_target_is_present_f_real64_5, omp_target_is_present_f_real64_6, &
                       omp_target_is_present_f_real64_7, &
#if defined _real128
                       omp_target_is_present_f_real128_1, omp_target_is_present_f_real128_2, omp_target_is_present_f_real128_3, &
                       omp_target_is_present_f_real128_4, omp_target_is_present_f_real128_5, omp_target_is_present_f_real128_6, &
                       omp_target_is_present_f_real128_7, &
#endif
                       omp_target_is_present_f_int8_1, omp_target_is_present_f_int8_2, omp_target_is_present_f_int8_3, &
                       omp_target_is_present_f_int8_4, omp_target_is_present_f_int8_5, omp_target_is_present_f_int8_6, &
                       omp_target_is_present_f_int8_7, &
                       omp_target_is_present_f_int16_1, omp_target_is_present_f_int16_2, omp_target_is_present_f_int16_3, &
                       omp_target_is_present_f_int16_4, omp_target_is_present_f_int16_5, omp_target_is_present_f_int16_6, &
                       omp_target_is_present_f_int16_7, &
                       omp_target_is_present_f_int32_1, omp_target_is_present_f_int32_2, omp_target_is_present_f_int32_3, &
                       omp_target_is_present_f_int32_4, omp_target_is_present_f_int32_5, omp_target_is_present_f_int32_6, &
                       omp_target_is_present_f_int32_7, &
                       omp_target_is_present_f_int64_1, omp_target_is_present_f_int64_2, omp_target_is_present_f_int64_3, &
                       omp_target_is_present_f_int64_4, omp_target_is_present_f_int64_5, omp_target_is_present_f_int64_6, &
                       omp_target_is_present_f_int64_7
   endinterface omp_target_is_present_f

   interface omp_target_free_f
      module procedure &
                       omp_target_free_f_lgcl32_1, omp_target_free_f_lgcl32_2, omp_target_free_f_lgcl32_3, &
                       omp_target_free_f_lgcl32_4, omp_target_free_f_lgcl32_5, omp_target_free_f_lgcl32_6, &
                       omp_target_free_f_lgcl32_7, &
                       omp_target_free_f_cmplx32_1, omp_target_free_f_cmplx32_2, omp_target_free_f_cmplx32_3, &
                       omp_target_free_f_cmplx32_4, omp_target_free_f_cmplx32_5, omp_target_free_f_cmplx32_6, &
                       omp_target_free_f_cmplx32_7, &
                       omp_target_free_f_cmplx64_1, omp_target_free_f_cmplx64_2, omp_target_free_f_cmplx64_3, &
                       omp_target_free_f_cmplx64_4, omp_target_free_f_cmplx64_5, omp_target_free_f_cmplx64_6, &
                       omp_target_free_f_cmplx64_7, &
#if defined _real128
                       omp_target_free_f_cmplx128_1, omp_target_free_f_cmplx128_2, omp_target_free_f_cmplx128_3, &
                       omp_target_free_f_cmplx128_4, omp_target_free_f_cmplx128_5, omp_target_free_f_cmplx128_6, &
                       omp_target_free_f_cmplx128_7, &
#endif
                       omp_target_free_f_real32_1, omp_target_free_f_real32_2, omp_target_free_f_real32_3, &
                       omp_target_free_f_real32_4, omp_target_free_f_real32_5, omp_target_free_f_real32_6, &
                       omp_target_free_f_real32_7, &
                       omp_target_free_f_real64_1, omp_target_free_f_real64_2, omp_target_free_f_real64_3, &
                       omp_target_free_f_real64_4, omp_target_free_f_real64_5, omp_target_free_f_real64_6, &
                       omp_target_free_f_real64_7, &
#if defined _real128
                       omp_target_free_f_real128_1, omp_target_free_f_real128_2, omp_target_free_f_real128_3, &
                       omp_target_free_f_real128_4, omp_target_free_f_real128_5, omp_target_free_f_real128_6, &
                       omp_target_free_f_real128_7, &
#endif
                       omp_target_free_f_int8_1, omp_target_free_f_int8_2, omp_target_free_f_int8_3, &
                       omp_target_free_f_int8_4, omp_target_free_f_int8_5, omp_target_free_f_int8_6, &
                       omp_target_free_f_int8_7, &
                       omp_target_free_f_int16_1, omp_target_free_f_int16_2, omp_target_free_f_int16_3, &
                       omp_target_free_f_int16_4, omp_target_free_f_int16_5, omp_target_free_f_int16_6, &
                       omp_target_free_f_int16_7, &
                       omp_target_free_f_int32_1, omp_target_free_f_int32_2, omp_target_free_f_int32_3, &
                       omp_target_free_f_int32_4, omp_target_free_f_int32_5, omp_target_free_f_int32_6, &
                       omp_target_free_f_int32_7, &
                       omp_target_free_f_int64_1, omp_target_free_f_int64_2, omp_target_free_f_int64_3, &
                       omp_target_free_f_int64_4, omp_target_free_f_int64_5, omp_target_free_f_int64_6, &
                       omp_target_free_f_int64_7
   endinterface omp_target_free_f

   interface omp_target_alloc_f
      module procedure &
                       omp_target_alloc_f_lgcl32_1_32bit, omp_target_alloc_f_lgcl32_2_32bit, omp_target_alloc_f_lgcl32_3_32bit, &
                       omp_target_alloc_f_lgcl32_4_32bit, omp_target_alloc_f_lgcl32_5_32bit, omp_target_alloc_f_lgcl32_6_32bit, &
                       omp_target_alloc_f_lgcl32_7_32bit, &
                       omp_target_alloc_f_cmplx32_1_32bit, omp_target_alloc_f_cmplx32_2_32bit, omp_target_alloc_f_cmplx32_3_32bit, &
                       omp_target_alloc_f_cmplx32_4_32bit, omp_target_alloc_f_cmplx32_5_32bit, omp_target_alloc_f_cmplx32_6_32bit, &
                       omp_target_alloc_f_cmplx32_7_32bit, &
                       omp_target_alloc_f_cmplx64_1_32bit, omp_target_alloc_f_cmplx64_2_32bit, omp_target_alloc_f_cmplx64_3_32bit, &
                       omp_target_alloc_f_cmplx64_4_32bit, omp_target_alloc_f_cmplx64_5_32bit, omp_target_alloc_f_cmplx64_6_32bit, &
                       omp_target_alloc_f_cmplx64_7_32bit, &
#if defined _real128
                       omp_target_alloc_f_cmplx128_1_32bit, omp_target_alloc_f_cmplx128_2_32bit, omp_target_alloc_f_cmplx128_3_32bit, &
                       omp_target_alloc_f_cmplx128_4_32bit, omp_target_alloc_f_cmplx128_5_32bit, omp_target_alloc_f_cmplx128_6_32bit, &
                       omp_target_alloc_f_cmplx128_7_32bit, &
#endif
                       omp_target_alloc_f_real32_1_32bit, omp_target_alloc_f_real32_2_32bit, omp_target_alloc_f_real32_3_32bit, &
                       omp_target_alloc_f_real32_4_32bit, omp_target_alloc_f_real32_5_32bit, omp_target_alloc_f_real32_6_32bit, &
                       omp_target_alloc_f_real32_7_32bit, &
                       omp_target_alloc_f_real64_1_32bit, omp_target_alloc_f_real64_2_32bit, omp_target_alloc_f_real64_3_32bit, &
                       omp_target_alloc_f_real64_4_32bit, omp_target_alloc_f_real64_5_32bit, omp_target_alloc_f_real64_6_32bit, &
                       omp_target_alloc_f_real64_7_32bit, &
#if defined _real128
                       omp_target_alloc_f_real128_1_32bit, omp_target_alloc_f_real128_2_32bit, omp_target_alloc_f_real128_3_32bit, &
                       omp_target_alloc_f_real128_4_32bit, omp_target_alloc_f_real128_5_32bit, omp_target_alloc_f_real128_6_32bit, &
                       omp_target_alloc_f_real128_7_32bit, &
#endif
                       omp_target_alloc_f_int8_1_32bit, omp_target_alloc_f_int8_2_32bit, omp_target_alloc_f_int8_3_32bit, &
                       omp_target_alloc_f_int8_4_32bit, omp_target_alloc_f_int8_5_32bit, omp_target_alloc_f_int8_6_32bit, &
                       omp_target_alloc_f_int8_7_32bit, &
                       omp_target_alloc_f_int16_1_32bit, omp_target_alloc_f_int16_2_32bit, omp_target_alloc_f_int16_3_32bit, &
                       omp_target_alloc_f_int16_4_32bit, omp_target_alloc_f_int16_5_32bit, omp_target_alloc_f_int16_6_32bit, &
                       omp_target_alloc_f_int16_7_32bit, &
                       omp_target_alloc_f_int32_1_32bit, omp_target_alloc_f_int32_2_32bit, omp_target_alloc_f_int32_3_32bit, &
                       omp_target_alloc_f_int32_4_32bit, omp_target_alloc_f_int32_5_32bit, omp_target_alloc_f_int32_6_32bit, &
                       omp_target_alloc_f_int32_7_32bit, &
                       omp_target_alloc_f_int64_1_32bit, omp_target_alloc_f_int64_2_32bit, omp_target_alloc_f_int64_3_32bit, &
                       omp_target_alloc_f_int64_4_32bit, omp_target_alloc_f_int64_5_32bit, omp_target_alloc_f_int64_6_32bit, &
                       omp_target_alloc_f_int64_7_32bit
      module procedure &
                       omp_target_alloc_f_lgcl32_1_64bit, omp_target_alloc_f_lgcl32_2_64bit, omp_target_alloc_f_lgcl32_3_64bit, &
                       omp_target_alloc_f_lgcl32_4_64bit, omp_target_alloc_f_lgcl32_5_64bit, omp_target_alloc_f_lgcl32_6_64bit, &
                       omp_target_alloc_f_lgcl32_7_64bit, &
                       omp_target_alloc_f_cmplx32_1_64bit, omp_target_alloc_f_cmplx32_2_64bit, omp_target_alloc_f_cmplx32_3_64bit, &
                       omp_target_alloc_f_cmplx32_4_64bit, omp_target_alloc_f_cmplx32_5_64bit, omp_target_alloc_f_cmplx32_6_64bit, &
                       omp_target_alloc_f_cmplx32_7_64bit, &
                       omp_target_alloc_f_cmplx64_1_64bit, omp_target_alloc_f_cmplx64_2_64bit, omp_target_alloc_f_cmplx64_3_64bit, &
                       omp_target_alloc_f_cmplx64_4_64bit, omp_target_alloc_f_cmplx64_5_64bit, omp_target_alloc_f_cmplx64_6_64bit, &
                       omp_target_alloc_f_cmplx64_7_64bit, &
#if defined _real128
                       omp_target_alloc_f_cmplx128_1_64bit, omp_target_alloc_f_cmplx128_2_64bit, omp_target_alloc_f_cmplx128_3_64bit, &
                       omp_target_alloc_f_cmplx128_4_64bit, omp_target_alloc_f_cmplx128_5_64bit, omp_target_alloc_f_cmplx128_6_64bit, &
                       omp_target_alloc_f_cmplx128_7_64bit, &
#endif
                       omp_target_alloc_f_real32_1_64bit, omp_target_alloc_f_real32_2_64bit, omp_target_alloc_f_real32_3_64bit, &
                       omp_target_alloc_f_real32_4_64bit, omp_target_alloc_f_real32_5_64bit, omp_target_alloc_f_real32_6_64bit, &
                       omp_target_alloc_f_real32_7_64bit, &
                       omp_target_alloc_f_real64_1_64bit, omp_target_alloc_f_real64_2_64bit, omp_target_alloc_f_real64_3_64bit, &
                       omp_target_alloc_f_real64_4_64bit, omp_target_alloc_f_real64_5_64bit, omp_target_alloc_f_real64_6_64bit, &
                       omp_target_alloc_f_real64_7_64bit, &
#if defined _real128
                       omp_target_alloc_f_real128_1_64bit, omp_target_alloc_f_real128_2_64bit, omp_target_alloc_f_real128_3_64bit, &
                       omp_target_alloc_f_real128_4_64bit, omp_target_alloc_f_real128_5_64bit, omp_target_alloc_f_real128_6_64bit, &
                       omp_target_alloc_f_real128_7_64bit, &
#endif
                       omp_target_alloc_f_int8_1_64bit, omp_target_alloc_f_int8_2_64bit, omp_target_alloc_f_int8_3_64bit, &
                       omp_target_alloc_f_int8_4_64bit, omp_target_alloc_f_int8_5_64bit, omp_target_alloc_f_int8_6_64bit, &
                       omp_target_alloc_f_int8_7_64bit, &
                       omp_target_alloc_f_int16_1_64bit, omp_target_alloc_f_int16_2_64bit, omp_target_alloc_f_int16_3_64bit, &
                       omp_target_alloc_f_int16_4_64bit, omp_target_alloc_f_int16_5_64bit, omp_target_alloc_f_int16_6_64bit, &
                       omp_target_alloc_f_int16_7_64bit, &
                       omp_target_alloc_f_int32_1_64bit, omp_target_alloc_f_int32_2_64bit, omp_target_alloc_f_int32_3_64bit, &
                       omp_target_alloc_f_int32_4_64bit, omp_target_alloc_f_int32_5_64bit, omp_target_alloc_f_int32_6_64bit, &
                       omp_target_alloc_f_int32_7_64bit, &
                       omp_target_alloc_f_int64_1_64bit, omp_target_alloc_f_int64_2_64bit, omp_target_alloc_f_int64_3_64bit, &
                       omp_target_alloc_f_int64_4_64bit, omp_target_alloc_f_int64_5_64bit, omp_target_alloc_f_int64_6_64bit, &
                       omp_target_alloc_f_int64_7_64bit
   endinterface omp_target_alloc_f

   interface omp_target_memcpy_f
      module procedure &
                       omp_target_memcpy_f_lgcl32, &
                       omp_target_memcpy_f_cmplx32, &
                       omp_target_memcpy_f_cmplx64, &
#if defined _real128
                       omp_target_memcpy_f_cmplx128, &
#endif
                       omp_target_memcpy_f_real32, &
                       omp_target_memcpy_f_real64, &
#if defined _real128
                       omp_target_memcpy_f_real128, &
#endif
                       omp_target_memcpy_f_int8, &
                       omp_target_memcpy_f_int16, &
                       omp_target_memcpy_f_int32, &
                       omp_target_memcpy_f_int64
   endinterface omp_target_memcpy_f

   interface omp_target_memcpy_rect_f
      module procedure &
                       omp_target_memcpy_rect_f_lgcl32, &
                       omp_target_memcpy_rect_f_cmplx32, &
                       omp_target_memcpy_rect_f_cmplx64, &
#if defined _real128
                       omp_target_memcpy_rect_f_cmplx128, &
#endif
                       omp_target_memcpy_rect_f_real32, &
                       omp_target_memcpy_rect_f_real64, &
#if defined _real128
                       omp_target_memcpy_rect_f_real128, &
#endif
                       omp_target_memcpy_rect_f_int8, &
                       omp_target_memcpy_rect_f_int16, &
                       omp_target_memcpy_rect_f_int32, &
                       omp_target_memcpy_rect_f_int64
   endinterface omp_target_memcpy_rect_f

   interface omp_target_init
      module procedure &
#if defined _F2008
                       omp_target_init_lgcl32, &
                       omp_target_init_cmplx32, &
                       omp_target_init_cmplx64, &
#if defined _real128
                       omp_target_init_cmplx128, &
#endif
                       omp_target_init_real32, &
                       omp_target_init_real64, &
#if defined _real128
                       omp_target_init_real128, &
#endif
                       omp_target_init_int8, &
                       omp_target_init_int16, &
                       omp_target_init_int32, &
                       omp_target_init_int64
#else
                       omp_target_init_lgcl32_1, omp_target_init_lgcl32_2, omp_target_init_lgcl32_3, &
                       omp_target_init_lgcl32_4, omp_target_init_lgcl32_5, omp_target_init_lgcl32_6, &
                       omp_target_init_lgcl32_7, &
                       omp_target_init_cmplx32_1, omp_target_init_cmplx32_2, omp_target_init_cmplx32_3, &
                       omp_target_init_cmplx32_4, omp_target_init_cmplx32_5, omp_target_init_cmplx32_6, &
                       omp_target_init_cmplx32_7, &
                       omp_target_init_cmplx64_1, omp_target_init_cmplx64_2, omp_target_init_cmplx64_3, &
                       omp_target_init_cmplx64_4, omp_target_init_cmplx64_5, omp_target_init_cmplx64_6, &
                       omp_target_init_cmplx64_7, &
#if defined _real128
                       omp_target_init_cmplx128_1, omp_target_init_cmplx128_2, omp_target_init_cmplx128_3, &
                       omp_target_init_cmplx128_4, omp_target_init_cmplx128_5, omp_target_init_cmplx128_6, &
                       omp_target_init_cmplx128_7, &
#endif
                       omp_target_init_real32_1, omp_target_init_real32_2, omp_target_init_real32_3, &
                       omp_target_init_real32_4, omp_target_init_real32_5, omp_target_init_real32_6, &
                       omp_target_init_real32_7, &
                       omp_target_init_real64_1, omp_target_init_real64_2, omp_target_init_real64_3, &
                       omp_target_init_real64_4, omp_target_init_real64_5, omp_target_init_real64_6, &
                       omp_target_init_real64_7, &
#if defined _real128
                       omp_target_init_real128_1, omp_target_init_real128_2, omp_target_init_real128_3, &
                       omp_target_init_real128_4, omp_target_init_real128_5, omp_target_init_real128_6, &
                       omp_target_init_real128_7, &
#endif
                       omp_target_init_int8_1, omp_target_init_int8_2, omp_target_init_int8_3, &
                       omp_target_init_int8_4, omp_target_init_int8_5, omp_target_init_int8_6, &
                       omp_target_init_int8_7, &
                       omp_target_init_int16_1, omp_target_init_int16_2, omp_target_init_int16_3, &
                       omp_target_init_int16_4, omp_target_init_int16_5, omp_target_init_int16_6, &
                       omp_target_init_int16_7, &
                       omp_target_init_int32_1, omp_target_init_int32_2, omp_target_init_int32_3, &
                       omp_target_init_int32_4, omp_target_init_int32_5, omp_target_init_int32_6, &
                       omp_target_init_int32_7, &
                       omp_target_init_int64_1, omp_target_init_int64_2, omp_target_init_int64_3, &
                       omp_target_init_int64_4, omp_target_init_int64_5, omp_target_init_int64_6, &
                       omp_target_init_int64_7
#endif
   endinterface omp_target_init

   interface omp_device_memcpy
      module procedure &
#if defined _F2008
                       omp_device_memcpy_lgcl32, &
                       omp_device_memcpy_cmplx32, &
                       omp_device_memcpy_cmplx64, &
#if defined _real128
                       omp_device_memcpy_cmplx128, &
#endif
                       omp_device_memcpy_real32, &
                       omp_device_memcpy_real64, &
#if defined _real128
                       omp_device_memcpy_real128, &
#endif
                       omp_device_memcpy_int8, &
                       omp_device_memcpy_int16, &
                       omp_device_memcpy_int32, &
                       omp_device_memcpy_int64
#else
                       omp_device_memcpy_lgcl32_1, omp_device_memcpy_lgcl32_2, omp_device_memcpy_lgcl32_3, &
                       omp_device_memcpy_lgcl32_4, omp_device_memcpy_lgcl32_5, omp_device_memcpy_lgcl32_6, &
                       omp_device_memcpy_lgcl32_7, &
                       omp_device_memcpy_cmplx32_1, omp_device_memcpy_cmplx32_2, omp_device_memcpy_cmplx32_3, &
                       omp_device_memcpy_cmplx32_4, omp_device_memcpy_cmplx32_5, omp_device_memcpy_cmplx32_6, &
                       omp_device_memcpy_cmplx32_7, &
                       omp_device_memcpy_cmplx64_1, omp_device_memcpy_cmplx64_2, omp_device_memcpy_cmplx64_3, &
                       omp_device_memcpy_cmplx64_4, omp_device_memcpy_cmplx64_5, omp_device_memcpy_cmplx64_6, &
                       omp_device_memcpy_cmplx64_7, &
#if defined _real128
                       omp_device_memcpy_cmplx128_1, omp_device_memcpy_cmplx128_2, omp_device_memcpy_cmplx128_3, &
                       omp_device_memcpy_cmplx128_4, omp_device_memcpy_cmplx128_5, omp_device_memcpy_cmplx128_6, &
                       omp_device_memcpy_cmplx128_7, &
#endif
                       omp_device_memcpy_real32_1, omp_device_memcpy_real32_2, omp_device_memcpy_real32_3, &
                       omp_device_memcpy_real32_4, omp_device_memcpy_real32_5, omp_device_memcpy_real32_6, &
                       omp_device_memcpy_real32_7, &
                       omp_device_memcpy_real64_1, omp_device_memcpy_real64_2, omp_device_memcpy_real64_3, &
                       omp_device_memcpy_real64_4, omp_device_memcpy_real64_5, omp_device_memcpy_real64_6, &
                       omp_device_memcpy_real64_7, &
#if defined _real128
                       omp_device_memcpy_real128_1, omp_device_memcpy_real128_2, omp_device_memcpy_real128_3, &
                       omp_device_memcpy_real128_4, omp_device_memcpy_real128_5, omp_device_memcpy_real128_6, &
                       omp_device_memcpy_real128_7, &
#endif
                       omp_device_memcpy_int8_1, omp_device_memcpy_int8_2, omp_device_memcpy_int8_3, &
                       omp_device_memcpy_int8_4, omp_device_memcpy_int8_5, omp_device_memcpy_int8_6, &
                       omp_device_memcpy_int8_7, &
                       omp_device_memcpy_int16_1, omp_device_memcpy_int16_2, omp_device_memcpy_int16_3, &
                       omp_device_memcpy_int16_4, omp_device_memcpy_int16_5, omp_device_memcpy_int16_6, &
                       omp_device_memcpy_int16_7, &
                       omp_device_memcpy_int32_1, omp_device_memcpy_int32_2, omp_device_memcpy_int32_3, &
                       omp_device_memcpy_int32_4, omp_device_memcpy_int32_5, omp_device_memcpy_int32_6, &
                       omp_device_memcpy_int32_7, &
                       omp_device_memcpy_int64_1, omp_device_memcpy_int64_2, omp_device_memcpy_int64_3, &
                       omp_device_memcpy_int64_4, omp_device_memcpy_int64_5, omp_device_memcpy_int64_6, &
                       omp_device_memcpy_int64_7
#endif
   endinterface omp_device_memcpy

#if defined _OpenMP_5_1
   interface omp_target_associate_ptr_f
      module procedure &
                       omp_target_associate_ptr_f_lgcl32_1, omp_target_associate_ptr_f_lgcl32_2, omp_target_associate_ptr_f_lgcl32_3, &
                       omp_target_associate_ptr_f_lgcl32_4, omp_target_associate_ptr_f_lgcl32_5, omp_target_associate_ptr_f_lgcl32_6, &
                       omp_target_associate_ptr_f_lgcl32_7, &
                       omp_target_associate_ptr_f_cmplx32_1, omp_target_associate_ptr_f_cmplx32_2, omp_target_associate_ptr_f_cmplx32_3, &
                       omp_target_associate_ptr_f_cmplx32_4, omp_target_associate_ptr_f_cmplx32_5, omp_target_associate_ptr_f_cmplx32_6, &
                       omp_target_associate_ptr_f_cmplx32_7, &
                       omp_target_associate_ptr_f_cmplx64_1, omp_target_associate_ptr_f_cmplx64_2, omp_target_associate_ptr_f_cmplx64_3, &
                       omp_target_associate_ptr_f_cmplx64_4, omp_target_associate_ptr_f_cmplx64_5, omp_target_associate_ptr_f_cmplx64_6, &
                       omp_target_associate_ptr_f_cmplx64_7, &
#if defined _real128
                       omp_target_associate_ptr_f_cmplx128_1, omp_target_associate_ptr_f_cmplx128_2, omp_target_associate_ptr_f_cmplx128_3, &
                       omp_target_associate_ptr_f_cmplx128_4, omp_target_associate_ptr_f_cmplx128_5, omp_target_associate_ptr_f_cmplx128_6, &
                       omp_target_associate_ptr_f_cmplx128_7, &
#endif
                       omp_target_associate_ptr_f_real32_1, omp_target_associate_ptr_f_real32_2, omp_target_associate_ptr_f_real32_3, &
                       omp_target_associate_ptr_f_real32_4, omp_target_associate_ptr_f_real32_5, omp_target_associate_ptr_f_real32_6, &
                       omp_target_associate_ptr_f_real32_7, &
                       omp_target_associate_ptr_f_real64_1, omp_target_associate_ptr_f_real64_2, omp_target_associate_ptr_f_real64_3, &
                       omp_target_associate_ptr_f_real64_4, omp_target_associate_ptr_f_real64_5, omp_target_associate_ptr_f_real64_6, &
                       omp_target_associate_ptr_f_real64_7, &
#if defined _real128
                       omp_target_associate_ptr_f_real128_1, omp_target_associate_ptr_f_real128_2, omp_target_associate_ptr_f_real128_3, &
                       omp_target_associate_ptr_f_real128_4, omp_target_associate_ptr_f_real128_5, omp_target_associate_ptr_f_real128_6, &
                       omp_target_associate_ptr_f_real128_7, &
#endif
                       omp_target_associate_ptr_f_int8_1, omp_target_associate_ptr_f_int8_2, omp_target_associate_ptr_f_int8_3, &
                       omp_target_associate_ptr_f_int8_4, omp_target_associate_ptr_f_int8_5, omp_target_associate_ptr_f_int8_6, &
                       omp_target_associate_ptr_f_int8_7, &
                       omp_target_associate_ptr_f_int16_1, omp_target_associate_ptr_f_int16_2, omp_target_associate_ptr_f_int16_3, &
                       omp_target_associate_ptr_f_int16_4, omp_target_associate_ptr_f_int16_5, omp_target_associate_ptr_f_int16_6, &
                       omp_target_associate_ptr_f_int16_7, &
                       omp_target_associate_ptr_f_int32_1, omp_target_associate_ptr_f_int32_2, omp_target_associate_ptr_f_int32_3, &
                       omp_target_associate_ptr_f_int32_4, omp_target_associate_ptr_f_int32_5, omp_target_associate_ptr_f_int32_6, &
                       omp_target_associate_ptr_f_int32_7, &
                       omp_target_associate_ptr_f_int64_1, omp_target_associate_ptr_f_int64_2, omp_target_associate_ptr_f_int64_3, &
                       omp_target_associate_ptr_f_int64_4, omp_target_associate_ptr_f_int64_5, omp_target_associate_ptr_f_int64_6, &
                       omp_target_associate_ptr_f_int64_7
   endinterface omp_target_associate_ptr_f

   interface omp_target_disassociate_ptr_f
      module procedure &
                       omp_target_disassociate_ptr_f_lgcl32_1, omp_target_disassociate_ptr_f_lgcl32_2, omp_target_disassociate_ptr_f_lgcl32_3, &
                       omp_target_disassociate_ptr_f_lgcl32_4, omp_target_disassociate_ptr_f_lgcl32_5, omp_target_disassociate_ptr_f_lgcl32_6, &
                       omp_target_disassociate_ptr_f_lgcl32_7, &
                       omp_target_disassociate_ptr_f_cmplx32_1, omp_target_disassociate_ptr_f_cmplx32_2, omp_target_disassociate_ptr_f_cmplx32_3, &
                       omp_target_disassociate_ptr_f_cmplx32_4, omp_target_disassociate_ptr_f_cmplx32_5, omp_target_disassociate_ptr_f_cmplx32_6, &
                       omp_target_disassociate_ptr_f_cmplx32_7, &
                       omp_target_disassociate_ptr_f_cmplx64_1, omp_target_disassociate_ptr_f_cmplx64_2, omp_target_disassociate_ptr_f_cmplx64_3, &
                       omp_target_disassociate_ptr_f_cmplx64_4, omp_target_disassociate_ptr_f_cmplx64_5, omp_target_disassociate_ptr_f_cmplx64_6, &
                       omp_target_disassociate_ptr_f_cmplx64_7, &
#if defined _real128
                       omp_target_disassociate_ptr_f_cmplx128_1, omp_target_disassociate_ptr_f_cmplx128_2, omp_target_disassociate_ptr_f_cmplx128_3, &
                       omp_target_disassociate_ptr_f_cmplx128_4, omp_target_disassociate_ptr_f_cmplx128_5, omp_target_disassociate_ptr_f_cmplx128_6, &
                       omp_target_disassociate_ptr_f_cmplx128_7, &
#endif
                       omp_target_disassociate_ptr_f_real32_1, omp_target_disassociate_ptr_f_real32_2, omp_target_disassociate_ptr_f_real32_3, &
                       omp_target_disassociate_ptr_f_real32_4, omp_target_disassociate_ptr_f_real32_5, omp_target_disassociate_ptr_f_real32_6, &
                       omp_target_disassociate_ptr_f_real32_7, &
                       omp_target_disassociate_ptr_f_real64_1, omp_target_disassociate_ptr_f_real64_2, omp_target_disassociate_ptr_f_real64_3, &
                       omp_target_disassociate_ptr_f_real64_4, omp_target_disassociate_ptr_f_real64_5, omp_target_disassociate_ptr_f_real64_6, &
                       omp_target_disassociate_ptr_f_real64_7, &
#if defined _real128
                       omp_target_disassociate_ptr_f_real128_1, omp_target_disassociate_ptr_f_real128_2, omp_target_disassociate_ptr_f_real128_3, &
                       omp_target_disassociate_ptr_f_real128_4, omp_target_disassociate_ptr_f_real128_5, omp_target_disassociate_ptr_f_real128_6, &
                       omp_target_disassociate_ptr_f_real128_7, &
#endif
                       omp_target_disassociate_ptr_f_int8_1, omp_target_disassociate_ptr_f_int8_2, omp_target_disassociate_ptr_f_int8_3, &
                       omp_target_disassociate_ptr_f_int8_4, omp_target_disassociate_ptr_f_int8_5, omp_target_disassociate_ptr_f_int8_6, &
                       omp_target_disassociate_ptr_f_int8_7, &
                       omp_target_disassociate_ptr_f_int16_1, omp_target_disassociate_ptr_f_int16_2, omp_target_disassociate_ptr_f_int16_3, &
                       omp_target_disassociate_ptr_f_int16_4, omp_target_disassociate_ptr_f_int16_5, omp_target_disassociate_ptr_f_int16_6, &
                       omp_target_disassociate_ptr_f_int16_7, &
                       omp_target_disassociate_ptr_f_int32_1, omp_target_disassociate_ptr_f_int32_2, omp_target_disassociate_ptr_f_int32_3, &
                       omp_target_disassociate_ptr_f_int32_4, omp_target_disassociate_ptr_f_int32_5, omp_target_disassociate_ptr_f_int32_6, &
                       omp_target_disassociate_ptr_f_int32_7, &
                       omp_target_disassociate_ptr_f_int64_1, omp_target_disassociate_ptr_f_int64_2, omp_target_disassociate_ptr_f_int64_3, &
                       omp_target_disassociate_ptr_f_int64_4, omp_target_disassociate_ptr_f_int64_5, omp_target_disassociate_ptr_f_int64_6, &
                       omp_target_disassociate_ptr_f_int64_7
   endinterface omp_target_disassociate_ptr_f

   interface omp_get_mapped_ptr_f
      module procedure &
#if defined _F2008
                       omp_get_mapped_ptr_f_lgcl32, &
                       omp_get_mapped_ptr_f_cmplx32, &
                       omp_get_mapped_ptr_f_cmplx64, &
#if defined _real128
                       omp_get_mapped_ptr_f_cmplx128, &
#endif
                       omp_get_mapped_ptr_f_real32, &
                       omp_get_mapped_ptr_f_real64, &
#if defined _real128
                       omp_get_mapped_ptr_f_real128, &
#endif
                       omp_get_mapped_ptr_f_int8, &
                       omp_get_mapped_ptr_f_int16, &
                       omp_get_mapped_ptr_f_int32, &
                       omp_get_mapped_ptr_f_int64
#else
                       omp_get_mapped_ptr_f_lgcl32_1, omp_get_mapped_ptr_f_lgcl32_2, omp_get_mapped_ptr_f_lgcl32_3, &
                       omp_get_mapped_ptr_f_lgcl32_4, omp_get_mapped_ptr_f_lgcl32_5, omp_get_mapped_ptr_f_lgcl32_6, &
                       omp_get_mapped_ptr_f_lgcl32_7, &
                       omp_get_mapped_ptr_f_cmplx32_1, omp_get_mapped_ptr_f_cmplx32_2, omp_get_mapped_ptr_f_cmplx32_3, &
                       omp_get_mapped_ptr_f_cmplx32_4, omp_get_mapped_ptr_f_cmplx32_5, omp_get_mapped_ptr_f_cmplx32_6, &
                       omp_get_mapped_ptr_f_cmplx32_7, &
                       omp_get_mapped_ptr_f_cmplx64_1, omp_get_mapped_ptr_f_cmplx64_2, omp_get_mapped_ptr_f_cmplx64_3, &
                       omp_get_mapped_ptr_f_cmplx64_4, omp_get_mapped_ptr_f_cmplx64_5, omp_get_mapped_ptr_f_cmplx64_6, &
                       omp_get_mapped_ptr_f_cmplx64_7, &
#if defined _real128
                       omp_get_mapped_ptr_f_cmplx128_1, omp_get_mapped_ptr_f_cmplx128_2, omp_get_mapped_ptr_f_cmplx128_3, &
                       omp_get_mapped_ptr_f_cmplx128_4, omp_get_mapped_ptr_f_cmplx128_5, omp_get_mapped_ptr_f_cmplx128_6, &
                       omp_get_mapped_ptr_f_cmplx128_7, &
#endif
                       omp_get_mapped_ptr_f_real32_1, omp_get_mapped_ptr_f_real32_2, omp_get_mapped_ptr_f_real32_3, &
                       omp_get_mapped_ptr_f_real32_4, omp_get_mapped_ptr_f_real32_5, omp_get_mapped_ptr_f_real32_6, &
                       omp_get_mapped_ptr_f_real32_7, &
                       omp_get_mapped_ptr_f_real64_1, omp_get_mapped_ptr_f_real64_2, omp_get_mapped_ptr_f_real64_3, &
                       omp_get_mapped_ptr_f_real64_4, omp_get_mapped_ptr_f_real64_5, omp_get_mapped_ptr_f_real64_6, &
                       omp_get_mapped_ptr_f_real64_7, &
#if defined _real128
                       omp_get_mapped_ptr_f_real128_1, omp_get_mapped_ptr_f_real128_2, omp_get_mapped_ptr_f_real128_3, &
                       omp_get_mapped_ptr_f_real128_4, omp_get_mapped_ptr_f_real128_5, omp_get_mapped_ptr_f_real128_6, &
                       omp_get_mapped_ptr_f_real128_7, &
#endif
                       omp_get_mapped_ptr_f_int8_1, omp_get_mapped_ptr_f_int8_2, omp_get_mapped_ptr_f_int8_3, &
                       omp_get_mapped_ptr_f_int8_4, omp_get_mapped_ptr_f_int8_5, omp_get_mapped_ptr_f_int8_6, &
                       omp_get_mapped_ptr_f_int8_7, &
                       omp_get_mapped_ptr_f_int16_1, omp_get_mapped_ptr_f_int16_2, omp_get_mapped_ptr_f_int16_3, &
                       omp_get_mapped_ptr_f_int16_4, omp_get_mapped_ptr_f_int16_5, omp_get_mapped_ptr_f_int16_6, &
                       omp_get_mapped_ptr_f_int16_7, &
                       omp_get_mapped_ptr_f_int32_1, omp_get_mapped_ptr_f_int32_2, omp_get_mapped_ptr_f_int32_3, &
                       omp_get_mapped_ptr_f_int32_4, omp_get_mapped_ptr_f_int32_5, omp_get_mapped_ptr_f_int32_6, &
                       omp_get_mapped_ptr_f_int32_7, &
                       omp_get_mapped_ptr_f_int64_1, omp_get_mapped_ptr_f_int64_2, omp_get_mapped_ptr_f_int64_3, &
                       omp_get_mapped_ptr_f_int64_4, omp_get_mapped_ptr_f_int64_5, omp_get_mapped_ptr_f_int64_6, &
                       omp_get_mapped_ptr_f_int64_7
#endif
   endinterface omp_get_mapped_ptr_f
#endif

! OpenMP Target Is Present Routines
   interface
      ! OpenMP Target Is Present Integer Routines
      module function omp_target_is_present_f_int8_1(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int8_1
         integer(I1P), target, intent(in)  :: host_data(:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int8_1
      module function omp_target_is_present_f_int8_2(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int8_2
         integer(I1P), target, intent(in)  :: host_data(:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int8_2
      module function omp_target_is_present_f_int8_3(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int8_3
         integer(I1P), target, intent(in)  :: host_data(:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int8_3
      module function omp_target_is_present_f_int8_4(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int8_4
         integer(I1P), target, intent(in)  :: host_data(:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int8_4
      module function omp_target_is_present_f_int8_5(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int8_5
         integer(I1P), target, intent(in)  :: host_data(:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int8_5
      module function omp_target_is_present_f_int8_6(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int8_6
         integer(I1P), target, intent(in)  :: host_data(:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int8_6
      module function omp_target_is_present_f_int8_7(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int8_7
         integer(I1P), target, intent(in)  :: host_data(:,:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int8_7
!
      module function omp_target_is_present_f_int16_1(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int16_1
         integer(I2P), target, intent(in)  :: host_data(:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int16_1
      module function omp_target_is_present_f_int16_2(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int16_2
         integer(I2P), target, intent(in)  :: host_data(:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int16_2
      module function omp_target_is_present_f_int16_3(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int16_3
         integer(I2P), target, intent(in)  :: host_data(:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int16_3
      module function omp_target_is_present_f_int16_4(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int16_4
         integer(I2P), target, intent(in)  :: host_data(:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int16_4
      module function omp_target_is_present_f_int16_5(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int16_5
         integer(I2P), target, intent(in)  :: host_data(:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int16_5
      module function omp_target_is_present_f_int16_6(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int16_6
         integer(I2P), target, intent(in)  :: host_data(:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int16_6
      module function omp_target_is_present_f_int16_7(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int16_7
         integer(I2P), target, intent(in)  :: host_data(:,:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int16_7
!
      module function omp_target_is_present_f_int32_1(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int32_1
         integer(I4P), target, intent(in)  :: host_data(:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int32_1
      module function omp_target_is_present_f_int32_2(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int32_2
         integer(I4P), target, intent(in)  :: host_data(:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int32_2
      module function omp_target_is_present_f_int32_3(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int32_3
         integer(I4P), target, intent(in)  :: host_data(:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int32_3
      module function omp_target_is_present_f_int32_4(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int32_4
         integer(I4P), target, intent(in)  :: host_data(:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int32_4
      module function omp_target_is_present_f_int32_5(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int32_5
         integer(I4P), target, intent(in)  :: host_data(:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int32_5
      module function omp_target_is_present_f_int32_6(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int32_6
         integer(I4P), target, intent(in)  :: host_data(:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int32_6
      module function omp_target_is_present_f_int32_7(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int32_7
         integer(I4P), target, intent(in)  :: host_data(:,:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int32_7
!
      module function omp_target_is_present_f_int64_1(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int64_1
         integer(I8P), target, intent(in)  :: host_data(:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int64_1
      module function omp_target_is_present_f_int64_2(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int64_2
         integer(I8P), target, intent(in)  :: host_data(:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int64_2
      module function omp_target_is_present_f_int64_3(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int64_3
         integer(I8P), target, intent(in)  :: host_data(:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int64_3
      module function omp_target_is_present_f_int64_4(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int64_4
         integer(I8P), target, intent(in)  :: host_data(:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int64_4
      module function omp_target_is_present_f_int64_5(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int64_5
         integer(I8P), target, intent(in)  :: host_data(:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int64_5
      module function omp_target_is_present_f_int64_6(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int64_6
         integer(I8P), target, intent(in)  :: host_data(:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int64_6
      module function omp_target_is_present_f_int64_7(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_int64_7
         integer(I8P), target, intent(in)  :: host_data(:,:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_int64_7
!
!
      ! OpenMP Target Is Present Real Routines
      module function omp_target_is_present_f_real32_1(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real32_1
         real(R4P), target, intent(in)     :: host_data(:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real32_1
      module function omp_target_is_present_f_real32_2(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real32_2
         real(R4P), target, intent(in)     :: host_data(:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real32_2
      module function omp_target_is_present_f_real32_3(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real32_3
         real(R4P), target, intent(in)     :: host_data(:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real32_3
      module function omp_target_is_present_f_real32_4(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real32_4
         real(R4P), target, intent(in)     :: host_data(:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real32_4
      module function omp_target_is_present_f_real32_5(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real32_5
         real(R4P), target, intent(in)     :: host_data(:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real32_5
      module function omp_target_is_present_f_real32_6(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real32_6
         real(R4P), target, intent(in)     :: host_data(:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real32_6
      module function omp_target_is_present_f_real32_7(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real32_7
         real(R4P), target, intent(in)     :: host_data(:,:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real32_7
!
      module function omp_target_is_present_f_real64_1(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real64_1
         real(R8P), target, intent(in)     :: host_data(:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real64_1
      module function omp_target_is_present_f_real64_2(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real64_2
         real(R8P), target, intent(in)     :: host_data(:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real64_2
      module function omp_target_is_present_f_real64_3(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real64_3
         real(R8P), target, intent(in)     :: host_data(:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real64_3
      module function omp_target_is_present_f_real64_4(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real64_4
         real(R8P), target, intent(in)     :: host_data(:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real64_4
      module function omp_target_is_present_f_real64_5(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real64_5
         real(R8P), target, intent(in)     :: host_data(:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real64_5
      module function omp_target_is_present_f_real64_6(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real64_6
         real(R8P), target, intent(in)     :: host_data(:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real64_6
      module function omp_target_is_present_f_real64_7(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real64_7
         real(R8P), target, intent(in)     :: host_data(:,:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real64_7
!
#if defined _real128
      module function omp_target_is_present_f_real128_1(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real128_1
         real(R16P), target, intent(in)    :: host_data(:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real128_1
      module function omp_target_is_present_f_real128_2(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real128_2
         real(R16P), target, intent(in)    :: host_data(:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real128_2
      module function omp_target_is_present_f_real128_3(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real128_3
         real(R16P), target, intent(in)    :: host_data(:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real128_3
      module function omp_target_is_present_f_real128_4(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real128_4
         real(R16P), target, intent(in)    :: host_data(:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real128_4
      module function omp_target_is_present_f_real128_5(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real128_5
         real(R16P), target, intent(in)    :: host_data(:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real128_5
      module function omp_target_is_present_f_real128_6(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real128_6
         real(R16P), target, intent(in)    :: host_data(:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real128_6
      module function omp_target_is_present_f_real128_7(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_real128_7
         real(R16P), target, intent(in)    :: host_data(:,:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_real128_7
#endif
!
!
      ! OpenMP Target Is Present Complex Routines
      module function omp_target_is_present_f_cmplx32_1(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx32_1
         complex(R4P), target, intent(in)  :: host_data(:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx32_1
      module function omp_target_is_present_f_cmplx32_2(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx32_2
         complex(R4P), target, intent(in)  :: host_data(:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx32_2
      module function omp_target_is_present_f_cmplx32_3(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx32_3
         complex(R4P), target, intent(in)  :: host_data(:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx32_3
      module function omp_target_is_present_f_cmplx32_4(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx32_4
         complex(R4P), target, intent(in)  :: host_data(:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx32_4
      module function omp_target_is_present_f_cmplx32_5(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx32_5
         complex(R4P), target, intent(in)  :: host_data(:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx32_5
      module function omp_target_is_present_f_cmplx32_6(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx32_6
         complex(R4P), target, intent(in)  :: host_data(:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx32_6
      module function omp_target_is_present_f_cmplx32_7(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx32_7
         complex(R4P), target, intent(in)  :: host_data(:,:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx32_7
!
      module function omp_target_is_present_f_cmplx64_1(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx64_1
         complex(R8P), target, intent(in)  :: host_data(:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx64_1
      module function omp_target_is_present_f_cmplx64_2(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx64_2
         complex(R8P), target, intent(in)  :: host_data(:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx64_2
      module function omp_target_is_present_f_cmplx64_3(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx64_3
         complex(R8P), target, intent(in)  :: host_data(:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx64_3
      module function omp_target_is_present_f_cmplx64_4(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx64_4
         complex(R8P), target, intent(in)  :: host_data(:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx64_4
      module function omp_target_is_present_f_cmplx64_5(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx64_5
         complex(R8P), target, intent(in)  :: host_data(:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx64_5
      module function omp_target_is_present_f_cmplx64_6(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx64_6
         complex(R8P), target, intent(in)  :: host_data(:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx64_6
      module function omp_target_is_present_f_cmplx64_7(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx64_7
         complex(R8P), target, intent(in)  :: host_data(:,:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx64_7
!
#if defined _real128
      module function omp_target_is_present_f_cmplx128_1(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx128_1
         complex(R16P), target, intent(in) :: host_data(:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx128_1
      module function omp_target_is_present_f_cmplx128_2(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx128_2
         complex(R16P), target, intent(in) :: host_data(:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx128_2
      module function omp_target_is_present_f_cmplx128_3(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx128_3
         complex(R16P), target, intent(in) :: host_data(:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx128_3
      module function omp_target_is_present_f_cmplx128_4(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx128_4
         complex(R16P), target, intent(in) :: host_data(:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx128_4
      module function omp_target_is_present_f_cmplx128_5(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx128_5
         complex(R16P), target, intent(in) :: host_data(:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx128_5
      module function omp_target_is_present_f_cmplx128_6(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx128_6
         complex(R16P), target, intent(in) :: host_data(:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx128_6
      module function omp_target_is_present_f_cmplx128_7(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_cmplx128_7
         complex(R16P), target, intent(in) :: host_data(:,:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_cmplx128_7
#endif
!
!
      ! OpenMP Target Is Present Logical Routines
      module function omp_target_is_present_f_lgcl32_1(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_lgcl32_1
         logical(I4P), target, intent(in)  :: host_data(:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_lgcl32_1
      module function omp_target_is_present_f_lgcl32_2(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_lgcl32_2
         logical(I4P), target, intent(in)  :: host_data(:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_lgcl32_2
      module function omp_target_is_present_f_lgcl32_3(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_lgcl32_3
         logical(I4P), target, intent(in)  :: host_data(:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_lgcl32_3
      module function omp_target_is_present_f_lgcl32_4(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_lgcl32_4
         logical(I4P), target, intent(in)  :: host_data(:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_lgcl32_4
      module function omp_target_is_present_f_lgcl32_5(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_lgcl32_5
         logical(I4P), target, intent(in)  :: host_data(:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_lgcl32_5
      module function omp_target_is_present_f_lgcl32_6(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_lgcl32_6
         logical(I4P), target, intent(in)  :: host_data(:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_lgcl32_6
      module function omp_target_is_present_f_lgcl32_7(host_data, omp_dev)
         implicit none
         logical                                       :: omp_target_is_present_f_lgcl32_7
         logical(I4P), target, intent(in)  :: host_data(:,:,:,:,:,:,:)
         integer(I4P), intent(in)          :: omp_dev
      endfunction omp_target_is_present_f_lgcl32_7
!
!
   endinterface

! OpenMP Target Alloc Routines
   interface
      ! OpenMP Target Alloc Integer 32 Bits Dimenstions Routines
      module subroutine omp_target_alloc_f_int8_1_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_int8_1_32bit

      module subroutine omp_target_alloc_f_int8_2_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_int8_2_32bit

      module subroutine omp_target_alloc_f_int8_3_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_int8_3_32bit

      module subroutine omp_target_alloc_f_int8_4_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_int8_4_32bit

      module subroutine omp_target_alloc_f_int8_5_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_int8_5_32bit

      module subroutine omp_target_alloc_f_int8_6_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_int8_6_32bit

      module subroutine omp_target_alloc_f_int8_7_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_int8_7_32bit

      module subroutine omp_target_alloc_f_int16_1_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_int16_1_32bit

      module subroutine omp_target_alloc_f_int16_2_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_int16_2_32bit

      module subroutine omp_target_alloc_f_int16_3_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_int16_3_32bit

      module subroutine omp_target_alloc_f_int16_4_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_int16_4_32bit

      module subroutine omp_target_alloc_f_int16_5_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_int16_5_32bit

      module subroutine omp_target_alloc_f_int16_6_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_int16_6_32bit

      module subroutine omp_target_alloc_f_int16_7_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_int16_7_32bit

      module subroutine omp_target_alloc_f_int32_1_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_int32_1_32bit

      module subroutine omp_target_alloc_f_int32_2_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_int32_2_32bit

      module subroutine omp_target_alloc_f_int32_3_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_int32_3_32bit

      module subroutine omp_target_alloc_f_int32_4_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_int32_4_32bit

      module subroutine omp_target_alloc_f_int32_5_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_int32_5_32bit

      module subroutine omp_target_alloc_f_int32_6_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_int32_6_32bit

      module subroutine omp_target_alloc_f_int32_7_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_int32_7_32bit

      module subroutine omp_target_alloc_f_int64_1_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_int64_1_32bit

      module subroutine omp_target_alloc_f_int64_2_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_int64_2_32bit

      module subroutine omp_target_alloc_f_int64_3_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_int64_3_32bit

      module subroutine omp_target_alloc_f_int64_4_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_int64_4_32bit

      module subroutine omp_target_alloc_f_int64_5_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_int64_5_32bit

      module subroutine omp_target_alloc_f_int64_6_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_int64_6_32bit

      module subroutine omp_target_alloc_f_int64_7_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_int64_7_32bit


      ! OpenMP Target Alloc Real 32 Bits Dimenstions Routines
      module subroutine omp_target_alloc_f_real32_1_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_real32_1_32bit

      module subroutine omp_target_alloc_f_real32_2_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_real32_2_32bit

      module subroutine omp_target_alloc_f_real32_3_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_real32_3_32bit

      module subroutine omp_target_alloc_f_real32_4_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_real32_4_32bit

      module subroutine omp_target_alloc_f_real32_5_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_real32_5_32bit

      module subroutine omp_target_alloc_f_real32_6_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_real32_6_32bit

      module subroutine omp_target_alloc_f_real32_7_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_real32_7_32bit

      module subroutine omp_target_alloc_f_real64_1_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_real64_1_32bit

      module subroutine omp_target_alloc_f_real64_2_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_real64_2_32bit

      module subroutine omp_target_alloc_f_real64_3_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_real64_3_32bit

      module subroutine omp_target_alloc_f_real64_4_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_real64_4_32bit

      module subroutine omp_target_alloc_f_real64_5_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_real64_5_32bit

      module subroutine omp_target_alloc_f_real64_6_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_real64_6_32bit

      module subroutine omp_target_alloc_f_real64_7_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_real64_7_32bit

#if defined _real128
      module subroutine omp_target_alloc_f_real128_1_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_real128_1_32bit

      module subroutine omp_target_alloc_f_real128_2_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_real128_2_32bit

      module subroutine omp_target_alloc_f_real128_3_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_real128_3_32bit

      module subroutine omp_target_alloc_f_real128_4_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_real128_4_32bit

      module subroutine omp_target_alloc_f_real128_5_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_real128_5_32bit

      module subroutine omp_target_alloc_f_real128_6_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_real128_6_32bit

      module subroutine omp_target_alloc_f_real128_7_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_real128_7_32bit

#endif

      ! OpenMP Target Alloc Complex 32 Bits Dimenstions Routines
      module subroutine omp_target_alloc_f_cmplx32_1_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_cmplx32_1_32bit

      module subroutine omp_target_alloc_f_cmplx32_2_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_cmplx32_2_32bit

      module subroutine omp_target_alloc_f_cmplx32_3_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_cmplx32_3_32bit

      module subroutine omp_target_alloc_f_cmplx32_4_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_cmplx32_4_32bit

      module subroutine omp_target_alloc_f_cmplx32_5_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_cmplx32_5_32bit

      module subroutine omp_target_alloc_f_cmplx32_6_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_cmplx32_6_32bit

      module subroutine omp_target_alloc_f_cmplx32_7_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_cmplx32_7_32bit

      module subroutine omp_target_alloc_f_cmplx64_1_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_cmplx64_1_32bit

      module subroutine omp_target_alloc_f_cmplx64_2_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_cmplx64_2_32bit

      module subroutine omp_target_alloc_f_cmplx64_3_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_cmplx64_3_32bit

      module subroutine omp_target_alloc_f_cmplx64_4_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_cmplx64_4_32bit

      module subroutine omp_target_alloc_f_cmplx64_5_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_cmplx64_5_32bit

      module subroutine omp_target_alloc_f_cmplx64_6_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_cmplx64_6_32bit

      module subroutine omp_target_alloc_f_cmplx64_7_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_cmplx64_7_32bit

#if defined _real128
      module subroutine omp_target_alloc_f_cmplx128_1_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_cmplx128_1_32bit

      module subroutine omp_target_alloc_f_cmplx128_2_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_cmplx128_2_32bit

      module subroutine omp_target_alloc_f_cmplx128_3_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_cmplx128_3_32bit

      module subroutine omp_target_alloc_f_cmplx128_4_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_cmplx128_4_32bit

      module subroutine omp_target_alloc_f_cmplx128_5_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_cmplx128_5_32bit

      module subroutine omp_target_alloc_f_cmplx128_6_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_cmplx128_6_32bit

      module subroutine omp_target_alloc_f_cmplx128_7_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_cmplx128_7_32bit

#endif

      ! OpenMP Target Alloc Logical 32 Bits Dimenstions Routines
      module subroutine omp_target_alloc_f_lgcl32_1_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_lgcl32_1_32bit

      module subroutine omp_target_alloc_f_lgcl32_2_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_lgcl32_2_32bit

      module subroutine omp_target_alloc_f_lgcl32_3_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_lgcl32_3_32bit

      module subroutine omp_target_alloc_f_lgcl32_4_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_lgcl32_4_32bit

      module subroutine omp_target_alloc_f_lgcl32_5_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_lgcl32_5_32bit

      module subroutine omp_target_alloc_f_lgcl32_6_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_lgcl32_6_32bit

      module subroutine omp_target_alloc_f_lgcl32_7_32bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I4P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_lgcl32_7_32bit


      ! OpenMP Target Alloc Integer 64 Bits Dimenstions Routines
      module subroutine omp_target_alloc_f_int8_1_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_int8_1_64bit

      module subroutine omp_target_alloc_f_int8_2_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_int8_2_64bit

      module subroutine omp_target_alloc_f_int8_3_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_int8_3_64bit

      module subroutine omp_target_alloc_f_int8_4_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_int8_4_64bit

      module subroutine omp_target_alloc_f_int8_5_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_int8_5_64bit

      module subroutine omp_target_alloc_f_int8_6_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_int8_6_64bit

      module subroutine omp_target_alloc_f_int8_7_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_int8_7_64bit

      module subroutine omp_target_alloc_f_int16_1_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_int16_1_64bit

      module subroutine omp_target_alloc_f_int16_2_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_int16_2_64bit

      module subroutine omp_target_alloc_f_int16_3_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_int16_3_64bit

      module subroutine omp_target_alloc_f_int16_4_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_int16_4_64bit

      module subroutine omp_target_alloc_f_int16_5_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_int16_5_64bit

      module subroutine omp_target_alloc_f_int16_6_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_int16_6_64bit

      module subroutine omp_target_alloc_f_int16_7_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_int16_7_64bit

      module subroutine omp_target_alloc_f_int32_1_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_int32_1_64bit

      module subroutine omp_target_alloc_f_int32_2_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_int32_2_64bit

      module subroutine omp_target_alloc_f_int32_3_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_int32_3_64bit

      module subroutine omp_target_alloc_f_int32_4_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_int32_4_64bit

      module subroutine omp_target_alloc_f_int32_5_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_int32_5_64bit

      module subroutine omp_target_alloc_f_int32_6_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_int32_6_64bit

      module subroutine omp_target_alloc_f_int32_7_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_int32_7_64bit

      module subroutine omp_target_alloc_f_int64_1_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_int64_1_64bit

      module subroutine omp_target_alloc_f_int64_2_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_int64_2_64bit

      module subroutine omp_target_alloc_f_int64_3_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_int64_3_64bit

      module subroutine omp_target_alloc_f_int64_4_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_int64_4_64bit

      module subroutine omp_target_alloc_f_int64_5_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_int64_5_64bit

      module subroutine omp_target_alloc_f_int64_6_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_int64_6_64bit

      module subroutine omp_target_alloc_f_int64_7_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_int64_7_64bit


      ! OpenMP Target Alloc Real 64 Bits Dimenstions Routines
      module subroutine omp_target_alloc_f_real32_1_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_real32_1_64bit

      module subroutine omp_target_alloc_f_real32_2_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_real32_2_64bit

      module subroutine omp_target_alloc_f_real32_3_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_real32_3_64bit

      module subroutine omp_target_alloc_f_real32_4_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_real32_4_64bit

      module subroutine omp_target_alloc_f_real32_5_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_real32_5_64bit

      module subroutine omp_target_alloc_f_real32_6_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_real32_6_64bit

      module subroutine omp_target_alloc_f_real32_7_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_real32_7_64bit

      module subroutine omp_target_alloc_f_real64_1_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_real64_1_64bit

      module subroutine omp_target_alloc_f_real64_2_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_real64_2_64bit

      module subroutine omp_target_alloc_f_real64_3_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_real64_3_64bit

      module subroutine omp_target_alloc_f_real64_4_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_real64_4_64bit

      module subroutine omp_target_alloc_f_real64_5_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_real64_5_64bit

      module subroutine omp_target_alloc_f_real64_6_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_real64_6_64bit

      module subroutine omp_target_alloc_f_real64_7_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_real64_7_64bit

#if defined _real128
      module subroutine omp_target_alloc_f_real128_1_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_real128_1_64bit

      module subroutine omp_target_alloc_f_real128_2_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_real128_2_64bit

      module subroutine omp_target_alloc_f_real128_3_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_real128_3_64bit

      module subroutine omp_target_alloc_f_real128_4_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_real128_4_64bit

      module subroutine omp_target_alloc_f_real128_5_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_real128_5_64bit

      module subroutine omp_target_alloc_f_real128_6_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_real128_6_64bit

      module subroutine omp_target_alloc_f_real128_7_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_real128_7_64bit

#endif

      ! OpenMP Target Alloc Complex 64 Bits Dimenstions Routines
      module subroutine omp_target_alloc_f_cmplx32_1_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_cmplx32_1_64bit

      module subroutine omp_target_alloc_f_cmplx32_2_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_cmplx32_2_64bit

      module subroutine omp_target_alloc_f_cmplx32_3_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_cmplx32_3_64bit

      module subroutine omp_target_alloc_f_cmplx32_4_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_cmplx32_4_64bit

      module subroutine omp_target_alloc_f_cmplx32_5_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_cmplx32_5_64bit

      module subroutine omp_target_alloc_f_cmplx32_6_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_cmplx32_6_64bit

      module subroutine omp_target_alloc_f_cmplx32_7_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_cmplx32_7_64bit

      module subroutine omp_target_alloc_f_cmplx64_1_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_cmplx64_1_64bit

      module subroutine omp_target_alloc_f_cmplx64_2_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_cmplx64_2_64bit

      module subroutine omp_target_alloc_f_cmplx64_3_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_cmplx64_3_64bit

      module subroutine omp_target_alloc_f_cmplx64_4_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_cmplx64_4_64bit

      module subroutine omp_target_alloc_f_cmplx64_5_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_cmplx64_5_64bit

      module subroutine omp_target_alloc_f_cmplx64_6_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_cmplx64_6_64bit

      module subroutine omp_target_alloc_f_cmplx64_7_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_cmplx64_7_64bit

#if defined _real128
      module subroutine omp_target_alloc_f_cmplx128_1_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_cmplx128_1_64bit

      module subroutine omp_target_alloc_f_cmplx128_2_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_cmplx128_2_64bit

      module subroutine omp_target_alloc_f_cmplx128_3_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_cmplx128_3_64bit

      module subroutine omp_target_alloc_f_cmplx128_4_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_cmplx128_4_64bit

      module subroutine omp_target_alloc_f_cmplx128_5_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_cmplx128_5_64bit

      module subroutine omp_target_alloc_f_cmplx128_6_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_cmplx128_6_64bit

      module subroutine omp_target_alloc_f_cmplx128_7_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_cmplx128_7_64bit

#endif

      ! OpenMP Target Alloc Logical 64 Bits Dimenstions Routines
      module subroutine omp_target_alloc_f_lgcl32_1_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I8P), intent(in)                        :: dimensions
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(2)
      endsubroutine omp_target_alloc_f_lgcl32_1_64bit

      module subroutine omp_target_alloc_f_lgcl32_2_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I8P), intent(in)                        :: dimensions(2)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(4)
      endsubroutine omp_target_alloc_f_lgcl32_2_64bit

      module subroutine omp_target_alloc_f_lgcl32_3_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                        :: dimensions(3)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(6)
      endsubroutine omp_target_alloc_f_lgcl32_3_64bit

      module subroutine omp_target_alloc_f_lgcl32_4_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(4)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(8)
      endsubroutine omp_target_alloc_f_lgcl32_4_64bit

      module subroutine omp_target_alloc_f_lgcl32_5_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(5)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(10)
      endsubroutine omp_target_alloc_f_lgcl32_5_64bit

      module subroutine omp_target_alloc_f_lgcl32_6_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(6)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(12)
      endsubroutine omp_target_alloc_f_lgcl32_6_64bit

      module subroutine omp_target_alloc_f_lgcl32_7_64bit(fptr_dev, dimensions, omp_dev, ierr, bounds)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                        :: dimensions(7)
         integer(I4P), intent(in)                        :: omp_dev
         integer(I4P), intent(out)                       :: ierr
         integer(I8P), intent(in), optional              :: bounds(14)
      endsubroutine omp_target_alloc_f_lgcl32_7_64bit


   endinterface

! OpenMP Target Free Routines
   interface
      ! OpenMP Target Free Integer Routines
      module subroutine omp_target_free_f_int8_1(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int8_1

      module subroutine omp_target_free_f_int8_2(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int8_2

      module subroutine omp_target_free_f_int8_3(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int8_3

      module subroutine omp_target_free_f_int8_4(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int8_4

      module subroutine omp_target_free_f_int8_5(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int8_5

      module subroutine omp_target_free_f_int8_6(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int8_6

      module subroutine omp_target_free_f_int8_7(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int8_7

      module subroutine omp_target_free_f_int16_1(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int16_1

      module subroutine omp_target_free_f_int16_2(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int16_2

      module subroutine omp_target_free_f_int16_3(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int16_3

      module subroutine omp_target_free_f_int16_4(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int16_4

      module subroutine omp_target_free_f_int16_5(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int16_5

      module subroutine omp_target_free_f_int16_6(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int16_6

      module subroutine omp_target_free_f_int16_7(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int16_7

      module subroutine omp_target_free_f_int32_1(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int32_1

      module subroutine omp_target_free_f_int32_2(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int32_2

      module subroutine omp_target_free_f_int32_3(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int32_3

      module subroutine omp_target_free_f_int32_4(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int32_4

      module subroutine omp_target_free_f_int32_5(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int32_5

      module subroutine omp_target_free_f_int32_6(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int32_6

      module subroutine omp_target_free_f_int32_7(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int32_7

      module subroutine omp_target_free_f_int64_1(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int64_1

      module subroutine omp_target_free_f_int64_2(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int64_2

      module subroutine omp_target_free_f_int64_3(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int64_3

      module subroutine omp_target_free_f_int64_4(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int64_4

      module subroutine omp_target_free_f_int64_5(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int64_5

      module subroutine omp_target_free_f_int64_6(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int64_6

      module subroutine omp_target_free_f_int64_7(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_int64_7


      ! OpenMP Target Free Real Routines
      module subroutine omp_target_free_f_real32_1(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real32_1

      module subroutine omp_target_free_f_real32_2(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real32_2

      module subroutine omp_target_free_f_real32_3(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real32_3

      module subroutine omp_target_free_f_real32_4(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real32_4

      module subroutine omp_target_free_f_real32_5(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real32_5

      module subroutine omp_target_free_f_real32_6(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real32_6

      module subroutine omp_target_free_f_real32_7(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real32_7

      module subroutine omp_target_free_f_real64_1(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real64_1

      module subroutine omp_target_free_f_real64_2(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real64_2

      module subroutine omp_target_free_f_real64_3(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real64_3

      module subroutine omp_target_free_f_real64_4(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real64_4

      module subroutine omp_target_free_f_real64_5(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real64_5

      module subroutine omp_target_free_f_real64_6(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real64_6

      module subroutine omp_target_free_f_real64_7(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real64_7

#if defined _real128
      module subroutine omp_target_free_f_real128_1(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real128_1

      module subroutine omp_target_free_f_real128_2(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real128_2

      module subroutine omp_target_free_f_real128_3(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real128_3

      module subroutine omp_target_free_f_real128_4(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real128_4

      module subroutine omp_target_free_f_real128_5(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real128_5

      module subroutine omp_target_free_f_real128_6(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real128_6

      module subroutine omp_target_free_f_real128_7(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_real128_7

#endif

      ! OpenMP Target Free Complex Routines
      module subroutine omp_target_free_f_cmplx32_1(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx32_1

      module subroutine omp_target_free_f_cmplx32_2(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx32_2

      module subroutine omp_target_free_f_cmplx32_3(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx32_3

      module subroutine omp_target_free_f_cmplx32_4(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx32_4

      module subroutine omp_target_free_f_cmplx32_5(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx32_5

      module subroutine omp_target_free_f_cmplx32_6(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx32_6

      module subroutine omp_target_free_f_cmplx32_7(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx32_7

      module subroutine omp_target_free_f_cmplx64_1(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx64_1

      module subroutine omp_target_free_f_cmplx64_2(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx64_2

      module subroutine omp_target_free_f_cmplx64_3(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx64_3

      module subroutine omp_target_free_f_cmplx64_4(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx64_4

      module subroutine omp_target_free_f_cmplx64_5(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx64_5

      module subroutine omp_target_free_f_cmplx64_6(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx64_6

      module subroutine omp_target_free_f_cmplx64_7(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx64_7

#if defined _real128
      module subroutine omp_target_free_f_cmplx128_1(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx128_1

      module subroutine omp_target_free_f_cmplx128_2(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx128_2

      module subroutine omp_target_free_f_cmplx128_3(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx128_3

      module subroutine omp_target_free_f_cmplx128_4(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx128_4

      module subroutine omp_target_free_f_cmplx128_5(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx128_5

      module subroutine omp_target_free_f_cmplx128_6(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx128_6

      module subroutine omp_target_free_f_cmplx128_7(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_cmplx128_7

#endif

      ! OpenMP Target Free Logical Routines
      module subroutine omp_target_free_f_lgcl32_1(fptr_dev, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_lgcl32_1

      module subroutine omp_target_free_f_lgcl32_2(fptr_dev, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_lgcl32_2

      module subroutine omp_target_free_f_lgcl32_3(fptr_dev, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_lgcl32_3

      module subroutine omp_target_free_f_lgcl32_4(fptr_dev, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_lgcl32_4

      module subroutine omp_target_free_f_lgcl32_5(fptr_dev, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_lgcl32_5

      module subroutine omp_target_free_f_lgcl32_6(fptr_dev, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_lgcl32_6

      module subroutine omp_target_free_f_lgcl32_7(fptr_dev, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_target_free_f_lgcl32_7


   endinterface

! OpenMP Target Memcpy Routines
   interface
      ! OpenMP Target Memcpy Integer Routines
      module function omp_target_memcpy_f_int8(fptr_dst, fptr_src, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_f_int8
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(..)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                      :: dst_off, src_off
      endfunction omp_target_memcpy_f_int8

      module function omp_target_memcpy_f_int16(fptr_dst, fptr_src, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_f_int16
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(..)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                      :: dst_off, src_off
      endfunction omp_target_memcpy_f_int16

      module function omp_target_memcpy_f_int32(fptr_dst, fptr_src, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_f_int32
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(..)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                      :: dst_off, src_off
      endfunction omp_target_memcpy_f_int32

      module function omp_target_memcpy_f_int64(fptr_dst, fptr_src, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_f_int64
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(..)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                      :: dst_off, src_off
      endfunction omp_target_memcpy_f_int64

      ! OpenMP Target Memcpy Real Routines
      module function omp_target_memcpy_f_real32(fptr_dst, fptr_src, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_f_real32
         real(R4P), contiguous, target, intent(out) :: fptr_dst(..)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                      :: dst_off, src_off
      endfunction omp_target_memcpy_f_real32

      module function omp_target_memcpy_f_real64(fptr_dst, fptr_src, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_f_real64
         real(R8P), contiguous, target, intent(out) :: fptr_dst(..)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                      :: dst_off, src_off
      endfunction omp_target_memcpy_f_real64

#if defined _real128
      module function omp_target_memcpy_f_real128(fptr_dst, fptr_src, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_f_real128
         real(R16P), contiguous, target, intent(out) :: fptr_dst(..)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                      :: dst_off, src_off
      endfunction omp_target_memcpy_f_real128
#endif

      ! OpenMP Target Memcpy Complex Routines
      module function omp_target_memcpy_f_cmplx32(fptr_dst, fptr_src, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_f_cmplx32
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(..)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                      :: dst_off, src_off
      endfunction omp_target_memcpy_f_cmplx32

      module function omp_target_memcpy_f_cmplx64(fptr_dst, fptr_src, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_f_cmplx64
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(..)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                      :: dst_off, src_off
      endfunction omp_target_memcpy_f_cmplx64

#if defined _real128
      module function omp_target_memcpy_f_cmplx128(fptr_dst, fptr_src, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_f_cmplx128
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(..)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                      :: dst_off, src_off
      endfunction omp_target_memcpy_f_cmplx128
#endif

      ! OpenMP Target Memcpy Logical Routines
      module function omp_target_memcpy_f_lgcl32(fptr_dst, fptr_src, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_f_lgcl32
         logical(I4P), contiguous, target, intent(out) :: fptr_dst(..)
         logical(I4P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
         integer(I4P), intent(in)                      :: dst_off, src_off
      endfunction omp_target_memcpy_f_lgcl32


   end interface

! OpenMP Target Memcpy Rect Routines
   interface
      ! OpenMP Target Memcpy Rect Integer Routines
      module function omp_target_memcpy_rect_f_int8(fptr_dst, fptr_src, cpy_dims, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_rect_f_int8
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(..)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I8P), intent(in)                      :: cpy_dims(:)
         integer(I4P), intent(in)                      :: dst_offs(:), src_offs(:)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
      endfunction omp_target_memcpy_rect_f_int8

      module function omp_target_memcpy_rect_f_int16(fptr_dst, fptr_src, cpy_dims, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_rect_f_int16
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(..)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I8P), intent(in)                      :: cpy_dims(:)
         integer(I4P), intent(in)                      :: dst_offs(:), src_offs(:)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
      endfunction omp_target_memcpy_rect_f_int16

      module function omp_target_memcpy_rect_f_int32(fptr_dst, fptr_src, cpy_dims, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_rect_f_int32
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(..)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I8P), intent(in)                      :: cpy_dims(:)
         integer(I4P), intent(in)                      :: dst_offs(:), src_offs(:)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
      endfunction omp_target_memcpy_rect_f_int32

      module function omp_target_memcpy_rect_f_int64(fptr_dst, fptr_src, cpy_dims, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_rect_f_int64
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(..)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I8P), intent(in)                      :: cpy_dims(:)
         integer(I4P), intent(in)                      :: dst_offs(:), src_offs(:)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
      endfunction omp_target_memcpy_rect_f_int64

      ! OpenMP Target Memcpy Rect Real Routines
      module function omp_target_memcpy_rect_f_real32(fptr_dst, fptr_src, cpy_dims, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_rect_f_real32
         real(R4P), contiguous, target, intent(out) :: fptr_dst(..)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I8P), intent(in)                      :: cpy_dims(:)
         integer(I4P), intent(in)                      :: dst_offs(:), src_offs(:)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
      endfunction omp_target_memcpy_rect_f_real32

      module function omp_target_memcpy_rect_f_real64(fptr_dst, fptr_src, cpy_dims, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_rect_f_real64
         real(R8P), contiguous, target, intent(out) :: fptr_dst(..)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I8P), intent(in)                      :: cpy_dims(:)
         integer(I4P), intent(in)                      :: dst_offs(:), src_offs(:)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
      endfunction omp_target_memcpy_rect_f_real64

#if defined _real128
      module function omp_target_memcpy_rect_f_real128(fptr_dst, fptr_src, cpy_dims, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_rect_f_real128
         real(R16P), contiguous, target, intent(out) :: fptr_dst(..)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I8P), intent(in)                      :: cpy_dims(:)
         integer(I4P), intent(in)                      :: dst_offs(:), src_offs(:)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
      endfunction omp_target_memcpy_rect_f_real128
#endif

      ! OpenMP Target Memcpy Rect Complex Routines
      module function omp_target_memcpy_rect_f_cmplx32(fptr_dst, fptr_src, cpy_dims, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_rect_f_cmplx32
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(..)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I8P), intent(in)                      :: cpy_dims(:)
         integer(I4P), intent(in)                      :: dst_offs(:), src_offs(:)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
      endfunction omp_target_memcpy_rect_f_cmplx32

      module function omp_target_memcpy_rect_f_cmplx64(fptr_dst, fptr_src, cpy_dims, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_rect_f_cmplx64
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(..)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I8P), intent(in)                      :: cpy_dims(:)
         integer(I4P), intent(in)                      :: dst_offs(:), src_offs(:)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
      endfunction omp_target_memcpy_rect_f_cmplx64

#if defined _real128
      module function omp_target_memcpy_rect_f_cmplx128(fptr_dst, fptr_src, cpy_dims, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_rect_f_cmplx128
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(..)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I8P), intent(in)                      :: cpy_dims(:)
         integer(I4P), intent(in)                      :: dst_offs(:), src_offs(:)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
      endfunction omp_target_memcpy_rect_f_cmplx128
#endif

      ! OpenMP Target Memcpy Rect Logical Routines
      module function omp_target_memcpy_rect_f_lgcl32(fptr_dst, fptr_src, cpy_dims, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P)                                  :: omp_target_memcpy_rect_f_lgcl32
         logical(I4P), contiguous, target, intent(out) :: fptr_dst(..)
         logical(I4P), contiguous, target, intent(in)  :: fptr_src(..)
         integer(I8P), intent(in)                      :: cpy_dims(:)
         integer(I4P), intent(in)                      :: dst_offs(:), src_offs(:)
         integer(I4P), intent(in)                      :: omp_dst_dev, omp_src_dev
      endfunction omp_target_memcpy_rect_f_lgcl32

   endinterface

! DMR Init Routines
   interface
#if defined _F2008
      ! DMR Target Init Integer F2018 Routines
      module subroutine omp_target_init_int8(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         integer(I1P), intent(inout)        :: array(..)
         integer(I1P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
      endsubroutine omp_target_init_int8

      module subroutine omp_target_init_int16(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         integer(I2P), intent(inout)        :: array(..)
         integer(I2P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
      endsubroutine omp_target_init_int16

      module subroutine omp_target_init_int32(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         integer(I4P), intent(inout)        :: array(..)
         integer(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
      endsubroutine omp_target_init_int32

      module subroutine omp_target_init_int64(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         integer(I8P), intent(inout)        :: array(..)
         integer(I8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
      endsubroutine omp_target_init_int64

      ! DMR Target Init Real F2018 Routines
      module subroutine omp_target_init_real32(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         real(R4P), intent(inout)        :: array(..)
         real(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
      endsubroutine omp_target_init_real32

      module subroutine omp_target_init_real64(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         real(R8P), intent(inout)        :: array(..)
         real(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
      endsubroutine omp_target_init_real64

#if defined _real128
      module subroutine omp_target_init_real128(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         real(R16P), intent(inout)        :: array(..)
         real(R16P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
      endsubroutine omp_target_init_real128
#endif

      ! DMR Target Init Complex F2018 Routines
      module subroutine omp_target_init_cmplx32(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         complex(R4P), intent(inout)        :: array(..)
         complex(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
      endsubroutine omp_target_init_cmplx32

      module subroutine omp_target_init_cmplx64(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         complex(R8P), intent(inout)        :: array(..)
         complex(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
      endsubroutine omp_target_init_cmplx64

#if defined _real128
      module subroutine omp_target_init_cmplx128(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         complex(R16P), intent(inout)        :: array(..)
         complex(R16P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
      endsubroutine omp_target_init_cmplx128
#endif

      ! DMR Target Init Logical F2018 Routines
      module subroutine omp_target_init_lgcl32(array, val, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         logical(I4P), intent(inout)        :: array(..)
         logical(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
      endsubroutine omp_target_init_lgcl32

#else
      ! DMR Target Init Integer Routines
      module subroutine omp_target_init_int8_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(inout)        :: array(:)
         integer(I1P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
      endsubroutine omp_target_init_int8_1

      module subroutine omp_target_init_int8_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(inout)        :: array(:,:)
         integer(I1P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
      endsubroutine omp_target_init_int8_2

      module subroutine omp_target_init_int8_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(inout)        :: array(:,:,:)
         integer(I1P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
      endsubroutine omp_target_init_int8_3

      module subroutine omp_target_init_int8_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(inout)        :: array(:,:,:,:)
         integer(I1P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
      endsubroutine omp_target_init_int8_4

      module subroutine omp_target_init_int8_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(inout)        :: array(:,:,:,:,:)
         integer(I1P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
      endsubroutine omp_target_init_int8_5

      module subroutine omp_target_init_int8_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(inout)        :: array(:,:,:,:,:,:)
         integer(I1P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
      endsubroutine omp_target_init_int8_6

      module subroutine omp_target_init_int8_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(inout)        :: array(:,:,:,:,:,:,:)
         integer(I1P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
      endsubroutine omp_target_init_int8_7

      module subroutine omp_target_init_int16_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(inout)        :: array(:)
         integer(I2P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
      endsubroutine omp_target_init_int16_1

      module subroutine omp_target_init_int16_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(inout)        :: array(:,:)
         integer(I2P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
      endsubroutine omp_target_init_int16_2

      module subroutine omp_target_init_int16_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(inout)        :: array(:,:,:)
         integer(I2P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
      endsubroutine omp_target_init_int16_3

      module subroutine omp_target_init_int16_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(inout)        :: array(:,:,:,:)
         integer(I2P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
      endsubroutine omp_target_init_int16_4

      module subroutine omp_target_init_int16_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(inout)        :: array(:,:,:,:,:)
         integer(I2P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
      endsubroutine omp_target_init_int16_5

      module subroutine omp_target_init_int16_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(inout)        :: array(:,:,:,:,:,:)
         integer(I2P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
      endsubroutine omp_target_init_int16_6

      module subroutine omp_target_init_int16_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(inout)        :: array(:,:,:,:,:,:,:)
         integer(I2P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
      endsubroutine omp_target_init_int16_7

      module subroutine omp_target_init_int32_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(inout)        :: array(:)
         integer(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
      endsubroutine omp_target_init_int32_1

      module subroutine omp_target_init_int32_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(inout)        :: array(:,:)
         integer(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
      endsubroutine omp_target_init_int32_2

      module subroutine omp_target_init_int32_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(inout)        :: array(:,:,:)
         integer(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
      endsubroutine omp_target_init_int32_3

      module subroutine omp_target_init_int32_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(inout)        :: array(:,:,:,:)
         integer(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
      endsubroutine omp_target_init_int32_4

      module subroutine omp_target_init_int32_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(inout)        :: array(:,:,:,:,:)
         integer(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
      endsubroutine omp_target_init_int32_5

      module subroutine omp_target_init_int32_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(inout)        :: array(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
      endsubroutine omp_target_init_int32_6

      module subroutine omp_target_init_int32_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(inout)        :: array(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
      endsubroutine omp_target_init_int32_7

      module subroutine omp_target_init_int64_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(inout)        :: array(:)
         integer(I8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
      endsubroutine omp_target_init_int64_1

      module subroutine omp_target_init_int64_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(inout)        :: array(:,:)
         integer(I8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
      endsubroutine omp_target_init_int64_2

      module subroutine omp_target_init_int64_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(inout)        :: array(:,:,:)
         integer(I8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
      endsubroutine omp_target_init_int64_3

      module subroutine omp_target_init_int64_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(inout)        :: array(:,:,:,:)
         integer(I8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
      endsubroutine omp_target_init_int64_4

      module subroutine omp_target_init_int64_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(inout)        :: array(:,:,:,:,:)
         integer(I8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
      endsubroutine omp_target_init_int64_5

      module subroutine omp_target_init_int64_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(inout)        :: array(:,:,:,:,:,:)
         integer(I8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
      endsubroutine omp_target_init_int64_6

      module subroutine omp_target_init_int64_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(inout)        :: array(:,:,:,:,:,:,:)
         integer(I8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
      endsubroutine omp_target_init_int64_7


      ! DMR Target Init Real Routines
      module subroutine omp_target_init_real32_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(inout)        :: array(:)
         real(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
      endsubroutine omp_target_init_real32_1

      module subroutine omp_target_init_real32_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(inout)        :: array(:,:)
         real(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
      endsubroutine omp_target_init_real32_2

      module subroutine omp_target_init_real32_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(inout)        :: array(:,:,:)
         real(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
      endsubroutine omp_target_init_real32_3

      module subroutine omp_target_init_real32_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(inout)        :: array(:,:,:,:)
         real(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
      endsubroutine omp_target_init_real32_4

      module subroutine omp_target_init_real32_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(inout)        :: array(:,:,:,:,:)
         real(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
      endsubroutine omp_target_init_real32_5

      module subroutine omp_target_init_real32_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(inout)        :: array(:,:,:,:,:,:)
         real(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
      endsubroutine omp_target_init_real32_6

      module subroutine omp_target_init_real32_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(inout)        :: array(:,:,:,:,:,:,:)
         real(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
      endsubroutine omp_target_init_real32_7

      module subroutine omp_target_init_real64_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(inout)        :: array(:)
         real(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
      endsubroutine omp_target_init_real64_1

      module subroutine omp_target_init_real64_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(inout)        :: array(:,:)
         real(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
      endsubroutine omp_target_init_real64_2

      module subroutine omp_target_init_real64_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(inout)        :: array(:,:,:)
         real(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
      endsubroutine omp_target_init_real64_3

      module subroutine omp_target_init_real64_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(inout)        :: array(:,:,:,:)
         real(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
      endsubroutine omp_target_init_real64_4

      module subroutine omp_target_init_real64_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(inout)        :: array(:,:,:,:,:)
         real(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
      endsubroutine omp_target_init_real64_5

      module subroutine omp_target_init_real64_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(inout)        :: array(:,:,:,:,:,:)
         real(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
      endsubroutine omp_target_init_real64_6

      module subroutine omp_target_init_real64_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(inout)        :: array(:,:,:,:,:,:,:)
         real(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
      endsubroutine omp_target_init_real64_7

#if defined _real128
      module subroutine omp_target_init_real128_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(inout)        :: array(:)
         real(R16P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
      endsubroutine omp_target_init_real128_1

      module subroutine omp_target_init_real128_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(inout)        :: array(:,:)
         real(R16P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
      endsubroutine omp_target_init_real128_2

      module subroutine omp_target_init_real128_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(inout)        :: array(:,:,:)
         real(R16P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
      endsubroutine omp_target_init_real128_3

      module subroutine omp_target_init_real128_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(inout)        :: array(:,:,:,:)
         real(R16P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
      endsubroutine omp_target_init_real128_4

      module subroutine omp_target_init_real128_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(inout)        :: array(:,:,:,:,:)
         real(R16P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
      endsubroutine omp_target_init_real128_5

      module subroutine omp_target_init_real128_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(inout)        :: array(:,:,:,:,:,:)
         real(R16P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
      endsubroutine omp_target_init_real128_6

      module subroutine omp_target_init_real128_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(inout)        :: array(:,:,:,:,:,:,:)
         real(R16P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
      endsubroutine omp_target_init_real128_7

#endif

      ! DMR Target Init Complex Routines
      module subroutine omp_target_init_cmplx32_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(inout)        :: array(:)
         complex(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
      endsubroutine omp_target_init_cmplx32_1

      module subroutine omp_target_init_cmplx32_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(inout)        :: array(:,:)
         complex(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
      endsubroutine omp_target_init_cmplx32_2

      module subroutine omp_target_init_cmplx32_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(inout)        :: array(:,:,:)
         complex(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
      endsubroutine omp_target_init_cmplx32_3

      module subroutine omp_target_init_cmplx32_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(inout)        :: array(:,:,:,:)
         complex(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
      endsubroutine omp_target_init_cmplx32_4

      module subroutine omp_target_init_cmplx32_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(inout)        :: array(:,:,:,:,:)
         complex(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
      endsubroutine omp_target_init_cmplx32_5

      module subroutine omp_target_init_cmplx32_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(inout)        :: array(:,:,:,:,:,:)
         complex(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
      endsubroutine omp_target_init_cmplx32_6

      module subroutine omp_target_init_cmplx32_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(inout)        :: array(:,:,:,:,:,:,:)
         complex(R4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
      endsubroutine omp_target_init_cmplx32_7

      module subroutine omp_target_init_cmplx64_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(inout)        :: array(:)
         complex(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
      endsubroutine omp_target_init_cmplx64_1

      module subroutine omp_target_init_cmplx64_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(inout)        :: array(:,:)
         complex(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
      endsubroutine omp_target_init_cmplx64_2

      module subroutine omp_target_init_cmplx64_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(inout)        :: array(:,:,:)
         complex(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
      endsubroutine omp_target_init_cmplx64_3

      module subroutine omp_target_init_cmplx64_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(inout)        :: array(:,:,:,:)
         complex(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
      endsubroutine omp_target_init_cmplx64_4

      module subroutine omp_target_init_cmplx64_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(inout)        :: array(:,:,:,:,:)
         complex(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
      endsubroutine omp_target_init_cmplx64_5

      module subroutine omp_target_init_cmplx64_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(inout)        :: array(:,:,:,:,:,:)
         complex(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
      endsubroutine omp_target_init_cmplx64_6

      module subroutine omp_target_init_cmplx64_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(inout)        :: array(:,:,:,:,:,:,:)
         complex(R8P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
      endsubroutine omp_target_init_cmplx64_7

#if defined _real128
      module subroutine omp_target_init_cmplx128_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(inout)        :: array(:)
         complex(R16P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
      endsubroutine omp_target_init_cmplx128_1

      module subroutine omp_target_init_cmplx128_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(inout)        :: array(:,:)
         complex(R16P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
      endsubroutine omp_target_init_cmplx128_2

      module subroutine omp_target_init_cmplx128_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(inout)        :: array(:,:,:)
         complex(R16P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
      endsubroutine omp_target_init_cmplx128_3

      module subroutine omp_target_init_cmplx128_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(inout)        :: array(:,:,:,:)
         complex(R16P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
      endsubroutine omp_target_init_cmplx128_4

      module subroutine omp_target_init_cmplx128_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(inout)        :: array(:,:,:,:,:)
         complex(R16P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
      endsubroutine omp_target_init_cmplx128_5

      module subroutine omp_target_init_cmplx128_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(inout)        :: array(:,:,:,:,:,:)
         complex(R16P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
      endsubroutine omp_target_init_cmplx128_6

      module subroutine omp_target_init_cmplx128_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(inout)        :: array(:,:,:,:,:,:,:)
         complex(R16P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
      endsubroutine omp_target_init_cmplx128_7

#endif

      ! DMR Target Init Logical Routines
      module subroutine omp_target_init_lgcl32_1(array, val, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(inout)        :: array(:)
         logical(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
      endsubroutine omp_target_init_lgcl32_1

      module subroutine omp_target_init_lgcl32_2(array, val, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(inout)        :: array(:,:)
         logical(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
      endsubroutine omp_target_init_lgcl32_2

      module subroutine omp_target_init_lgcl32_3(array, val, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(inout)        :: array(:,:,:)
         logical(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
      endsubroutine omp_target_init_lgcl32_3

      module subroutine omp_target_init_lgcl32_4(array, val, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(inout)        :: array(:,:,:,:)
         logical(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
      endsubroutine omp_target_init_lgcl32_4

      module subroutine omp_target_init_lgcl32_5(array, val, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(inout)        :: array(:,:,:,:,:)
         logical(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
      endsubroutine omp_target_init_lgcl32_5

      module subroutine omp_target_init_lgcl32_6(array, val, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(inout)        :: array(:,:,:,:,:,:)
         logical(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
      endsubroutine omp_target_init_lgcl32_6

      module subroutine omp_target_init_lgcl32_7(array, val, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(inout)        :: array(:,:,:,:,:,:,:)
         logical(I4P), intent(in)           :: val
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
      endsubroutine omp_target_init_lgcl32_7


#endif
   endinterface

! DMR Device Memcpy Routines
   interface
#if defined _F2008
      ! DMR Device Memcpy Integer F2018 Routines
      module subroutine omp_device_memcpy_int8(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         integer(I1P), intent(out)          :: array_dst(..)
         integer(I1P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
      endsubroutine omp_device_memcpy_int8

      module subroutine omp_device_memcpy_int16(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         integer(I2P), intent(out)          :: array_dst(..)
         integer(I2P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
      endsubroutine omp_device_memcpy_int16

      module subroutine omp_device_memcpy_int32(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         integer(I4P), intent(out)          :: array_dst(..)
         integer(I4P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
      endsubroutine omp_device_memcpy_int32

      module subroutine omp_device_memcpy_int64(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         integer(I8P), intent(out)          :: array_dst(..)
         integer(I8P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
      endsubroutine omp_device_memcpy_int64

      ! DMR Device Memcpy Real F2018 Routines
      module subroutine omp_device_memcpy_real32(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         real(R4P), intent(out)          :: array_dst(..)
         real(R4P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
      endsubroutine omp_device_memcpy_real32

      module subroutine omp_device_memcpy_real64(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         real(R8P), intent(out)          :: array_dst(..)
         real(R8P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
      endsubroutine omp_device_memcpy_real64

#if defined _real128
      module subroutine omp_device_memcpy_real128(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         real(R16P), intent(out)          :: array_dst(..)
         real(R16P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
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
      endsubroutine omp_device_memcpy_cmplx32

      module subroutine omp_device_memcpy_cmplx64(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         complex(R8P), intent(out)          :: array_dst(..)
         complex(R8P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
      endsubroutine omp_device_memcpy_cmplx64

#if defined _real128
      module subroutine omp_device_memcpy_cmplx128(array_dst, array_src, omp_dev, lbound_s, ubound_s, lbounds, ubounds)
         implicit none
         complex(R16P), intent(out)          :: array_dst(..)
         complex(R16P), intent(in)           :: array_src(..)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbound_s, ubound_s
         integer(I8P), intent(in), optional :: lbounds(:), ubounds(:)
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
      endsubroutine omp_device_memcpy_lgcl32

#else
      ! DMR Device Memcpy Integer Routines
      module subroutine omp_device_memcpy_int8_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(out)          :: array_dst(:)
         integer(I1P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: i1
      endsubroutine omp_device_memcpy_int8_1

      module subroutine omp_device_memcpy_int8_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(out)          :: array_dst(:,:)
         integer(I1P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: i1, i2
      endsubroutine omp_device_memcpy_int8_2

      module subroutine omp_device_memcpy_int8_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(out)          :: array_dst(:,:,:)
         integer(I1P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: i1, i2, i3
      endsubroutine omp_device_memcpy_int8_3

      module subroutine omp_device_memcpy_int8_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(out)          :: array_dst(:,:,:,:)
         integer(I1P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: i1, i2, i3, i4
      endsubroutine omp_device_memcpy_int8_4

      module subroutine omp_device_memcpy_int8_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(out)          :: array_dst(:,:,:,:,:)
         integer(I1P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
      endsubroutine omp_device_memcpy_int8_5

      module subroutine omp_device_memcpy_int8_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(out)          :: array_dst(:,:,:,:,:,:)
         integer(I1P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
      endsubroutine omp_device_memcpy_int8_6

      module subroutine omp_device_memcpy_int8_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I1P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         integer(I1P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
      endsubroutine omp_device_memcpy_int8_7

      module subroutine omp_device_memcpy_int16_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(out)          :: array_dst(:)
         integer(I2P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: i1
      endsubroutine omp_device_memcpy_int16_1

      module subroutine omp_device_memcpy_int16_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(out)          :: array_dst(:,:)
         integer(I2P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: i1, i2
      endsubroutine omp_device_memcpy_int16_2

      module subroutine omp_device_memcpy_int16_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(out)          :: array_dst(:,:,:)
         integer(I2P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: i1, i2, i3
      endsubroutine omp_device_memcpy_int16_3

      module subroutine omp_device_memcpy_int16_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(out)          :: array_dst(:,:,:,:)
         integer(I2P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: i1, i2, i3, i4
      endsubroutine omp_device_memcpy_int16_4

      module subroutine omp_device_memcpy_int16_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(out)          :: array_dst(:,:,:,:,:)
         integer(I2P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
      endsubroutine omp_device_memcpy_int16_5

      module subroutine omp_device_memcpy_int16_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(out)          :: array_dst(:,:,:,:,:,:)
         integer(I2P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
      endsubroutine omp_device_memcpy_int16_6

      module subroutine omp_device_memcpy_int16_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I2P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         integer(I2P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
      endsubroutine omp_device_memcpy_int16_7

      module subroutine omp_device_memcpy_int32_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(out)          :: array_dst(:)
         integer(I4P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: i1
      endsubroutine omp_device_memcpy_int32_1

      module subroutine omp_device_memcpy_int32_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(out)          :: array_dst(:,:)
         integer(I4P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: i1, i2
      endsubroutine omp_device_memcpy_int32_2

      module subroutine omp_device_memcpy_int32_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(out)          :: array_dst(:,:,:)
         integer(I4P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: i1, i2, i3
      endsubroutine omp_device_memcpy_int32_3

      module subroutine omp_device_memcpy_int32_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(out)          :: array_dst(:,:,:,:)
         integer(I4P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: i1, i2, i3, i4
      endsubroutine omp_device_memcpy_int32_4

      module subroutine omp_device_memcpy_int32_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(out)          :: array_dst(:,:,:,:,:)
         integer(I4P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
      endsubroutine omp_device_memcpy_int32_5

      module subroutine omp_device_memcpy_int32_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(out)          :: array_dst(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
      endsubroutine omp_device_memcpy_int32_6

      module subroutine omp_device_memcpy_int32_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I4P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
      endsubroutine omp_device_memcpy_int32_7

      module subroutine omp_device_memcpy_int64_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(out)          :: array_dst(:)
         integer(I8P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: i1
      endsubroutine omp_device_memcpy_int64_1

      module subroutine omp_device_memcpy_int64_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(out)          :: array_dst(:,:)
         integer(I8P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: i1, i2
      endsubroutine omp_device_memcpy_int64_2

      module subroutine omp_device_memcpy_int64_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(out)          :: array_dst(:,:,:)
         integer(I8P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: i1, i2, i3
      endsubroutine omp_device_memcpy_int64_3

      module subroutine omp_device_memcpy_int64_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(out)          :: array_dst(:,:,:,:)
         integer(I8P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: i1, i2, i3, i4
      endsubroutine omp_device_memcpy_int64_4

      module subroutine omp_device_memcpy_int64_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(out)          :: array_dst(:,:,:,:,:)
         integer(I8P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
      endsubroutine omp_device_memcpy_int64_5

      module subroutine omp_device_memcpy_int64_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(out)          :: array_dst(:,:,:,:,:,:)
         integer(I8P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
      endsubroutine omp_device_memcpy_int64_6

      module subroutine omp_device_memcpy_int64_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         integer(I8P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         integer(I8P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
      endsubroutine omp_device_memcpy_int64_7


      ! DMR Device Memcpy Real Routines
      module subroutine omp_device_memcpy_real32_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(out)          :: array_dst(:)
         real(R4P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: i1
      endsubroutine omp_device_memcpy_real32_1

      module subroutine omp_device_memcpy_real32_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(out)          :: array_dst(:,:)
         real(R4P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: i1, i2
      endsubroutine omp_device_memcpy_real32_2

      module subroutine omp_device_memcpy_real32_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(out)          :: array_dst(:,:,:)
         real(R4P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: i1, i2, i3
      endsubroutine omp_device_memcpy_real32_3

      module subroutine omp_device_memcpy_real32_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(out)          :: array_dst(:,:,:,:)
         real(R4P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: i1, i2, i3, i4
      endsubroutine omp_device_memcpy_real32_4

      module subroutine omp_device_memcpy_real32_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(out)          :: array_dst(:,:,:,:,:)
         real(R4P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
      endsubroutine omp_device_memcpy_real32_5

      module subroutine omp_device_memcpy_real32_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(out)          :: array_dst(:,:,:,:,:,:)
         real(R4P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
      endsubroutine omp_device_memcpy_real32_6

      module subroutine omp_device_memcpy_real32_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R4P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         real(R4P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
      endsubroutine omp_device_memcpy_real32_7

      module subroutine omp_device_memcpy_real64_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(out)          :: array_dst(:)
         real(R8P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: i1
      endsubroutine omp_device_memcpy_real64_1

      module subroutine omp_device_memcpy_real64_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(out)          :: array_dst(:,:)
         real(R8P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: i1, i2
      endsubroutine omp_device_memcpy_real64_2

      module subroutine omp_device_memcpy_real64_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(out)          :: array_dst(:,:,:)
         real(R8P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: i1, i2, i3
      endsubroutine omp_device_memcpy_real64_3

      module subroutine omp_device_memcpy_real64_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(out)          :: array_dst(:,:,:,:)
         real(R8P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: i1, i2, i3, i4
      endsubroutine omp_device_memcpy_real64_4

      module subroutine omp_device_memcpy_real64_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(out)          :: array_dst(:,:,:,:,:)
         real(R8P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
      endsubroutine omp_device_memcpy_real64_5

      module subroutine omp_device_memcpy_real64_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(out)          :: array_dst(:,:,:,:,:,:)
         real(R8P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
      endsubroutine omp_device_memcpy_real64_6

      module subroutine omp_device_memcpy_real64_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R8P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         real(R8P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
      endsubroutine omp_device_memcpy_real64_7

#if defined _real128
      module subroutine omp_device_memcpy_real128_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(out)          :: array_dst(:)
         real(R16P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: i1
      endsubroutine omp_device_memcpy_real128_1

      module subroutine omp_device_memcpy_real128_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(out)          :: array_dst(:,:)
         real(R16P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: i1, i2
      endsubroutine omp_device_memcpy_real128_2

      module subroutine omp_device_memcpy_real128_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(out)          :: array_dst(:,:,:)
         real(R16P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: i1, i2, i3
      endsubroutine omp_device_memcpy_real128_3

      module subroutine omp_device_memcpy_real128_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(out)          :: array_dst(:,:,:,:)
         real(R16P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: i1, i2, i3, i4
      endsubroutine omp_device_memcpy_real128_4

      module subroutine omp_device_memcpy_real128_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(out)          :: array_dst(:,:,:,:,:)
         real(R16P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
      endsubroutine omp_device_memcpy_real128_5

      module subroutine omp_device_memcpy_real128_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(out)          :: array_dst(:,:,:,:,:,:)
         real(R16P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
      endsubroutine omp_device_memcpy_real128_6

      module subroutine omp_device_memcpy_real128_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         real(R16P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         real(R16P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
      endsubroutine omp_device_memcpy_real128_7

#endif

      ! DMR Device Memcpy Complex Routines
      module subroutine omp_device_memcpy_cmplx32_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(out)          :: array_dst(:)
         complex(R4P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: i1
      endsubroutine omp_device_memcpy_cmplx32_1

      module subroutine omp_device_memcpy_cmplx32_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(out)          :: array_dst(:,:)
         complex(R4P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: i1, i2
      endsubroutine omp_device_memcpy_cmplx32_2

      module subroutine omp_device_memcpy_cmplx32_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(out)          :: array_dst(:,:,:)
         complex(R4P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: i1, i2, i3
      endsubroutine omp_device_memcpy_cmplx32_3

      module subroutine omp_device_memcpy_cmplx32_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(out)          :: array_dst(:,:,:,:)
         complex(R4P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: i1, i2, i3, i4
      endsubroutine omp_device_memcpy_cmplx32_4

      module subroutine omp_device_memcpy_cmplx32_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(out)          :: array_dst(:,:,:,:,:)
         complex(R4P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
      endsubroutine omp_device_memcpy_cmplx32_5

      module subroutine omp_device_memcpy_cmplx32_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(out)          :: array_dst(:,:,:,:,:,:)
         complex(R4P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
      endsubroutine omp_device_memcpy_cmplx32_6

      module subroutine omp_device_memcpy_cmplx32_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R4P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         complex(R4P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
      endsubroutine omp_device_memcpy_cmplx32_7

      module subroutine omp_device_memcpy_cmplx64_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(out)          :: array_dst(:)
         complex(R8P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: i1
      endsubroutine omp_device_memcpy_cmplx64_1

      module subroutine omp_device_memcpy_cmplx64_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(out)          :: array_dst(:,:)
         complex(R8P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: i1, i2
      endsubroutine omp_device_memcpy_cmplx64_2

      module subroutine omp_device_memcpy_cmplx64_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(out)          :: array_dst(:,:,:)
         complex(R8P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: i1, i2, i3
      endsubroutine omp_device_memcpy_cmplx64_3

      module subroutine omp_device_memcpy_cmplx64_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(out)          :: array_dst(:,:,:,:)
         complex(R8P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: i1, i2, i3, i4
      endsubroutine omp_device_memcpy_cmplx64_4

      module subroutine omp_device_memcpy_cmplx64_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(out)          :: array_dst(:,:,:,:,:)
         complex(R8P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
      endsubroutine omp_device_memcpy_cmplx64_5

      module subroutine omp_device_memcpy_cmplx64_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(out)          :: array_dst(:,:,:,:,:,:)
         complex(R8P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
      endsubroutine omp_device_memcpy_cmplx64_6

      module subroutine omp_device_memcpy_cmplx64_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R8P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         complex(R8P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
      endsubroutine omp_device_memcpy_cmplx64_7

#if defined _real128
      module subroutine omp_device_memcpy_cmplx128_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(out)          :: array_dst(:)
         complex(R16P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: i1
      endsubroutine omp_device_memcpy_cmplx128_1

      module subroutine omp_device_memcpy_cmplx128_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(out)          :: array_dst(:,:)
         complex(R16P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: i1, i2
      endsubroutine omp_device_memcpy_cmplx128_2

      module subroutine omp_device_memcpy_cmplx128_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(out)          :: array_dst(:,:,:)
         complex(R16P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: i1, i2, i3
      endsubroutine omp_device_memcpy_cmplx128_3

      module subroutine omp_device_memcpy_cmplx128_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(out)          :: array_dst(:,:,:,:)
         complex(R16P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: i1, i2, i3, i4
      endsubroutine omp_device_memcpy_cmplx128_4

      module subroutine omp_device_memcpy_cmplx128_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(out)          :: array_dst(:,:,:,:,:)
         complex(R16P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
      endsubroutine omp_device_memcpy_cmplx128_5

      module subroutine omp_device_memcpy_cmplx128_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(out)          :: array_dst(:,:,:,:,:,:)
         complex(R16P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
      endsubroutine omp_device_memcpy_cmplx128_6

      module subroutine omp_device_memcpy_cmplx128_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         complex(R16P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         complex(R16P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
      endsubroutine omp_device_memcpy_cmplx128_7

#endif

      ! DMR Device Memcpy Logical Routines
      module subroutine omp_device_memcpy_lgcl32_1(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(out)          :: array_dst(:)
         logical(I4P), intent(in)           :: array_src(:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds, ubounds
         integer(I8P)                       :: i1
      endsubroutine omp_device_memcpy_lgcl32_1

      module subroutine omp_device_memcpy_lgcl32_2(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(out)          :: array_dst(:,:)
         logical(I4P), intent(in)           :: array_src(:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(2), ubounds(2)
         integer(I8P)                       :: i1, i2
      endsubroutine omp_device_memcpy_lgcl32_2

      module subroutine omp_device_memcpy_lgcl32_3(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(out)          :: array_dst(:,:,:)
         logical(I4P), intent(in)           :: array_src(:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(3), ubounds(3)
         integer(I8P)                       :: i1, i2, i3
      endsubroutine omp_device_memcpy_lgcl32_3

      module subroutine omp_device_memcpy_lgcl32_4(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(out)          :: array_dst(:,:,:,:)
         logical(I4P), intent(in)           :: array_src(:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(4), ubounds(4)
         integer(I8P)                       :: i1, i2, i3, i4
      endsubroutine omp_device_memcpy_lgcl32_4

      module subroutine omp_device_memcpy_lgcl32_5(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(out)          :: array_dst(:,:,:,:,:)
         logical(I4P), intent(in)           :: array_src(:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(5), ubounds(5)
         integer(I8P)                       :: i1, i2, i3, i4, i5
      endsubroutine omp_device_memcpy_lgcl32_5

      module subroutine omp_device_memcpy_lgcl32_6(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(out)          :: array_dst(:,:,:,:,:,:)
         logical(I4P), intent(in)           :: array_src(:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(6), ubounds(6)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6
      endsubroutine omp_device_memcpy_lgcl32_6

      module subroutine omp_device_memcpy_lgcl32_7(array_dst, array_src, omp_dev, lbounds, ubounds)
         implicit none
         logical(I4P), intent(out)          :: array_dst(:,:,:,:,:,:,:)
         logical(I4P), intent(in)           :: array_src(:,:,:,:,:,:,:)
         integer(I4P), intent(in), optional :: omp_dev
         integer(I8P), intent(in), optional :: lbounds(7), ubounds(7)
         integer(I8P)                       :: i1, i2, i3, i4, i5, i6, i7
      endsubroutine omp_device_memcpy_lgcl32_7


#endif
   endinterface

#if defined _OpenMP_5_1
! DMR Target Associate Pointer Routines
   interface
      ! DMR Target Associate Pointer Integer Routines
      module function omp_target_associate_ptr_f_int8_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int8_1
         integer(I1P), target,  intent(out) :: fptr_hos(:)
         integer(I1P), pointer, intent(in)  :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int8_1

      module function omp_target_associate_ptr_f_int8_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int8_2
         integer(I1P), target,  intent(out) :: fptr_hos(:,:)
         integer(I1P), pointer, intent(in)  :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int8_2

      module function omp_target_associate_ptr_f_int8_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int8_3
         integer(I1P), target,  intent(out) :: fptr_hos(:,:,:)
         integer(I1P), pointer, intent(in)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int8_3

      module function omp_target_associate_ptr_f_int8_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int8_4
         integer(I1P), target,  intent(out) :: fptr_hos(:,:,:,:)
         integer(I1P), pointer, intent(in)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int8_4

      module function omp_target_associate_ptr_f_int8_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int8_5
         integer(I1P), target,  intent(out) :: fptr_hos(:,:,:,:,:)
         integer(I1P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int8_5

      module function omp_target_associate_ptr_f_int8_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int8_6
         integer(I1P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:)
         integer(I1P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int8_6

      module function omp_target_associate_ptr_f_int8_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int8_7
         integer(I1P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:,:)
         integer(I1P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int8_7


      module function omp_target_associate_ptr_f_int16_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int16_1
         integer(I2P), target,  intent(out) :: fptr_hos(:)
         integer(I2P), pointer, intent(in)  :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int16_1

      module function omp_target_associate_ptr_f_int16_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int16_2
         integer(I2P), target,  intent(out) :: fptr_hos(:,:)
         integer(I2P), pointer, intent(in)  :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int16_2

      module function omp_target_associate_ptr_f_int16_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int16_3
         integer(I2P), target,  intent(out) :: fptr_hos(:,:,:)
         integer(I2P), pointer, intent(in)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int16_3

      module function omp_target_associate_ptr_f_int16_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int16_4
         integer(I2P), target,  intent(out) :: fptr_hos(:,:,:,:)
         integer(I2P), pointer, intent(in)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int16_4

      module function omp_target_associate_ptr_f_int16_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int16_5
         integer(I2P), target,  intent(out) :: fptr_hos(:,:,:,:,:)
         integer(I2P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int16_5

      module function omp_target_associate_ptr_f_int16_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int16_6
         integer(I2P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:)
         integer(I2P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int16_6

      module function omp_target_associate_ptr_f_int16_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int16_7
         integer(I2P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:,:)
         integer(I2P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int16_7


      module function omp_target_associate_ptr_f_int32_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int32_1
         integer(I4P), target,  intent(out) :: fptr_hos(:)
         integer(I4P), pointer, intent(in)  :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int32_1

      module function omp_target_associate_ptr_f_int32_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int32_2
         integer(I4P), target,  intent(out) :: fptr_hos(:,:)
         integer(I4P), pointer, intent(in)  :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int32_2

      module function omp_target_associate_ptr_f_int32_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int32_3
         integer(I4P), target,  intent(out) :: fptr_hos(:,:,:)
         integer(I4P), pointer, intent(in)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int32_3

      module function omp_target_associate_ptr_f_int32_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int32_4
         integer(I4P), target,  intent(out) :: fptr_hos(:,:,:,:)
         integer(I4P), pointer, intent(in)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int32_4

      module function omp_target_associate_ptr_f_int32_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int32_5
         integer(I4P), target,  intent(out) :: fptr_hos(:,:,:,:,:)
         integer(I4P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int32_5

      module function omp_target_associate_ptr_f_int32_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int32_6
         integer(I4P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int32_6

      module function omp_target_associate_ptr_f_int32_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int32_7
         integer(I4P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int32_7


      module function omp_target_associate_ptr_f_int64_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int64_1
         integer(I8P), target,  intent(out) :: fptr_hos(:)
         integer(I8P), pointer, intent(in)  :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int64_1

      module function omp_target_associate_ptr_f_int64_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int64_2
         integer(I8P), target,  intent(out) :: fptr_hos(:,:)
         integer(I8P), pointer, intent(in)  :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int64_2

      module function omp_target_associate_ptr_f_int64_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int64_3
         integer(I8P), target,  intent(out) :: fptr_hos(:,:,:)
         integer(I8P), pointer, intent(in)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int64_3

      module function omp_target_associate_ptr_f_int64_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int64_4
         integer(I8P), target,  intent(out) :: fptr_hos(:,:,:,:)
         integer(I8P), pointer, intent(in)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int64_4

      module function omp_target_associate_ptr_f_int64_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int64_5
         integer(I8P), target,  intent(out) :: fptr_hos(:,:,:,:,:)
         integer(I8P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int64_5

      module function omp_target_associate_ptr_f_int64_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int64_6
         integer(I8P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:)
         integer(I8P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int64_6

      module function omp_target_associate_ptr_f_int64_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_int64_7
         integer(I8P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:,:)
         integer(I8P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_int64_7


      ! DMR Target Associate Pointer Real Routines
      module function omp_target_associate_ptr_f_real32_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real32_1
         real(R4P), target,  intent(out)    :: fptr_hos(:)
         real(R4P), pointer, intent(in)     :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real32_1

      module function omp_target_associate_ptr_f_real32_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real32_2
         real(R4P), target,  intent(out)    :: fptr_hos(:,:)
         real(R4P), pointer, intent(in)     :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real32_2

      module function omp_target_associate_ptr_f_real32_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real32_3
         real(R4P), target,  intent(out)    :: fptr_hos(:,:,:)
         real(R4P), pointer, intent(in)     :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real32_3

      module function omp_target_associate_ptr_f_real32_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real32_4
         real(R4P), target,  intent(out)    :: fptr_hos(:,:,:,:)
         real(R4P), pointer, intent(in)     :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real32_4

      module function omp_target_associate_ptr_f_real32_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real32_5
         real(R4P), target,  intent(out)    :: fptr_hos(:,:,:,:,:)
         real(R4P), pointer, intent(in)     :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real32_5

      module function omp_target_associate_ptr_f_real32_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real32_6
         real(R4P), target,  intent(out)    :: fptr_hos(:,:,:,:,:,:)
         real(R4P), pointer, intent(in)     :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real32_6

      module function omp_target_associate_ptr_f_real32_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real32_7
         real(R4P), target,  intent(out)    :: fptr_hos(:,:,:,:,:,:,:)
         real(R4P), pointer, intent(in)     :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real32_7


      module function omp_target_associate_ptr_f_real64_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real64_1
         real(R8P), target,  intent(out)    :: fptr_hos(:)
         real(R8P), pointer, intent(in)     :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real64_1

      module function omp_target_associate_ptr_f_real64_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real64_2
         real(R8P), target,  intent(out)    :: fptr_hos(:,:)
         real(R8P), pointer, intent(in)     :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real64_2

      module function omp_target_associate_ptr_f_real64_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real64_3
         real(R8P), target,  intent(out)    :: fptr_hos(:,:,:)
         real(R8P), pointer, intent(in)     :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real64_3

      module function omp_target_associate_ptr_f_real64_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real64_4
         real(R8P), target,  intent(out)    :: fptr_hos(:,:,:,:)
         real(R8P), pointer, intent(in)     :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real64_4

      module function omp_target_associate_ptr_f_real64_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real64_5
         real(R8P), target,  intent(out)    :: fptr_hos(:,:,:,:,:)
         real(R8P), pointer, intent(in)     :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real64_5

      module function omp_target_associate_ptr_f_real64_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real64_6
         real(R8P), target,  intent(out)    :: fptr_hos(:,:,:,:,:,:)
         real(R8P), pointer, intent(in)     :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real64_6

      module function omp_target_associate_ptr_f_real64_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real64_7
         real(R8P), target,  intent(out)    :: fptr_hos(:,:,:,:,:,:,:)
         real(R8P), pointer, intent(in)     :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real64_7


#if defined _real128
      module function omp_target_associate_ptr_f_real128_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real128_1
         real(R16P), target,  intent(out)   :: fptr_hos(:)
         real(R16P), pointer, intent(in)    :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real128_1

      module function omp_target_associate_ptr_f_real128_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real128_2
         real(R16P), target,  intent(out)   :: fptr_hos(:,:)
         real(R16P), pointer, intent(in)    :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real128_2

      module function omp_target_associate_ptr_f_real128_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real128_3
         real(R16P), target,  intent(out)   :: fptr_hos(:,:,:)
         real(R16P), pointer, intent(in)    :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real128_3

      module function omp_target_associate_ptr_f_real128_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real128_4
         real(R16P), target,  intent(out)   :: fptr_hos(:,:,:,:)
         real(R16P), pointer, intent(in)    :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real128_4

      module function omp_target_associate_ptr_f_real128_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real128_5
         real(R16P), target,  intent(out)   :: fptr_hos(:,:,:,:,:)
         real(R16P), pointer, intent(in)    :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real128_5

      module function omp_target_associate_ptr_f_real128_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real128_6
         real(R16P), target,  intent(out)   :: fptr_hos(:,:,:,:,:,:)
         real(R16P), pointer, intent(in)    :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real128_6

      module function omp_target_associate_ptr_f_real128_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_real128_7
         real(R16P), target,  intent(out)   :: fptr_hos(:,:,:,:,:,:,:)
         real(R16P), pointer, intent(in)    :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_real128_7

#endif

      ! DMR Target Associate Pointer Complex Routines
      module function omp_target_associate_ptr_f_cmplx32_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx32_1
         complex(R4P), target,  intent(out) :: fptr_hos(:)
         complex(R4P), pointer, intent(in)  :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx32_1

      module function omp_target_associate_ptr_f_cmplx32_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx32_2
         complex(R4P), target,  intent(out) :: fptr_hos(:,:)
         complex(R4P), pointer, intent(in)  :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx32_2

      module function omp_target_associate_ptr_f_cmplx32_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx32_3
         complex(R4P), target,  intent(out) :: fptr_hos(:,:,:)
         complex(R4P), pointer, intent(in)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx32_3

      module function omp_target_associate_ptr_f_cmplx32_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx32_4
         complex(R4P), target,  intent(out) :: fptr_hos(:,:,:,:)
         complex(R4P), pointer, intent(in)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx32_4

      module function omp_target_associate_ptr_f_cmplx32_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx32_5
         complex(R4P), target,  intent(out) :: fptr_hos(:,:,:,:,:)
         complex(R4P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx32_5

      module function omp_target_associate_ptr_f_cmplx32_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx32_6
         complex(R4P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:)
         complex(R4P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx32_6

      module function omp_target_associate_ptr_f_cmplx32_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx32_7
         complex(R4P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:,:)
         complex(R4P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx32_7


      module function omp_target_associate_ptr_f_cmplx64_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx64_1
         complex(R8P), target,  intent(out) :: fptr_hos(:)
         complex(R8P), pointer, intent(in)  :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx64_1

      module function omp_target_associate_ptr_f_cmplx64_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx64_2
         complex(R8P), target,  intent(out) :: fptr_hos(:,:)
         complex(R8P), pointer, intent(in)  :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx64_2

      module function omp_target_associate_ptr_f_cmplx64_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx64_3
         complex(R8P), target,  intent(out) :: fptr_hos(:,:,:)
         complex(R8P), pointer, intent(in)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx64_3

      module function omp_target_associate_ptr_f_cmplx64_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx64_4
         complex(R8P), target,  intent(out) :: fptr_hos(:,:,:,:)
         complex(R8P), pointer, intent(in)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx64_4

      module function omp_target_associate_ptr_f_cmplx64_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx64_5
         complex(R8P), target,  intent(out) :: fptr_hos(:,:,:,:,:)
         complex(R8P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx64_5

      module function omp_target_associate_ptr_f_cmplx64_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx64_6
         complex(R8P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:)
         complex(R8P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx64_6

      module function omp_target_associate_ptr_f_cmplx64_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx64_7
         complex(R8P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:,:)
         complex(R8P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx64_7


#if defined _real128
      module function omp_target_associate_ptr_f_cmplx128_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx128_1
         complex(R16P), target,  intent(out):: fptr_hos(:)
         complex(R16P), pointer, intent(in) :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx128_1

      module function omp_target_associate_ptr_f_cmplx128_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx128_2
         complex(R16P), target,  intent(out):: fptr_hos(:,:)
         complex(R16P), pointer, intent(in) :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx128_2

      module function omp_target_associate_ptr_f_cmplx128_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx128_3
         complex(R16P), target,  intent(out):: fptr_hos(:,:,:)
         complex(R16P), pointer, intent(in) :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx128_3

      module function omp_target_associate_ptr_f_cmplx128_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx128_4
         complex(R16P), target,  intent(out):: fptr_hos(:,:,:,:)
         complex(R16P), pointer, intent(in) :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx128_4

      module function omp_target_associate_ptr_f_cmplx128_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx128_5
         complex(R16P), target,  intent(out):: fptr_hos(:,:,:,:,:)
         complex(R16P), pointer, intent(in) :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx128_5

      module function omp_target_associate_ptr_f_cmplx128_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx128_6
         complex(R16P), target,  intent(out):: fptr_hos(:,:,:,:,:,:)
         complex(R16P), pointer, intent(in) :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx128_6

      module function omp_target_associate_ptr_f_cmplx128_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_cmplx128_7
         complex(R16P), target,  intent(out):: fptr_hos(:,:,:,:,:,:,:)
         complex(R16P), pointer, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_cmplx128_7

#endif

      ! DMR Target Associate Pointer Logical Routines
      module function omp_target_associate_ptr_f_lgcl32_1(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_lgcl32_1
         logical(I4P), target,  intent(out) :: fptr_hos(:)
         logical(I4P), pointer, intent(in)  :: fptr_dev(:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_lgcl32_1

      module function omp_target_associate_ptr_f_lgcl32_2(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_lgcl32_2
         logical(I4P), target,  intent(out) :: fptr_hos(:,:)
         logical(I4P), pointer, intent(in)  :: fptr_dev(:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_lgcl32_2

      module function omp_target_associate_ptr_f_lgcl32_3(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_lgcl32_3
         logical(I4P), target,  intent(out) :: fptr_hos(:,:,:)
         logical(I4P), pointer, intent(in)  :: fptr_dev(:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_lgcl32_3

      module function omp_target_associate_ptr_f_lgcl32_4(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_lgcl32_4
         logical(I4P), target,  intent(out) :: fptr_hos(:,:,:,:)
         logical(I4P), pointer, intent(in)  :: fptr_dev(:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_lgcl32_4

      module function omp_target_associate_ptr_f_lgcl32_5(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_lgcl32_5
         logical(I4P), target,  intent(out) :: fptr_hos(:,:,:,:,:)
         logical(I4P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_lgcl32_5

      module function omp_target_associate_ptr_f_lgcl32_6(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_lgcl32_6
         logical(I4P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:)
         logical(I4P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_lgcl32_6

      module function omp_target_associate_ptr_f_lgcl32_7(fptr_hos, fptr_dev, dev_off, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_associate_ptr_f_lgcl32_7
         logical(I4P), target,  intent(out) :: fptr_hos(:,:,:,:,:,:,:)
         logical(I4P), pointer, intent(in)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: dev_off
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_associate_ptr_f_lgcl32_7


   endinterface

! DMR Target Disassociate Pointer Routines
   interface
      ! DMR Target Disassociate Pointer Integer Routines
      module function omp_target_disassociate_ptr_f_int8_1(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int8_1
         integer(I1P), target,  intent(in) :: fptr_hos(:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int8_1

      module function omp_target_disassociate_ptr_f_int8_2(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int8_2
         integer(I1P), target,  intent(in) :: fptr_hos(:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int8_2

      module function omp_target_disassociate_ptr_f_int8_3(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int8_3
         integer(I1P), target,  intent(in) :: fptr_hos(:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int8_3

      module function omp_target_disassociate_ptr_f_int8_4(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int8_4
         integer(I1P), target,  intent(in) :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int8_4

      module function omp_target_disassociate_ptr_f_int8_5(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int8_5
         integer(I1P), target,  intent(in) :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int8_5

      module function omp_target_disassociate_ptr_f_int8_6(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int8_6
         integer(I1P), target,  intent(in) :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int8_6

      module function omp_target_disassociate_ptr_f_int8_7(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int8_7
         integer(I1P), target,  intent(in) :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int8_7


      module function omp_target_disassociate_ptr_f_int16_1(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int16_1
         integer(I2P), target,  intent(in) :: fptr_hos(:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int16_1

      module function omp_target_disassociate_ptr_f_int16_2(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int16_2
         integer(I2P), target,  intent(in) :: fptr_hos(:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int16_2

      module function omp_target_disassociate_ptr_f_int16_3(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int16_3
         integer(I2P), target,  intent(in) :: fptr_hos(:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int16_3

      module function omp_target_disassociate_ptr_f_int16_4(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int16_4
         integer(I2P), target,  intent(in) :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int16_4

      module function omp_target_disassociate_ptr_f_int16_5(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int16_5
         integer(I2P), target,  intent(in) :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int16_5

      module function omp_target_disassociate_ptr_f_int16_6(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int16_6
         integer(I2P), target,  intent(in) :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int16_6

      module function omp_target_disassociate_ptr_f_int16_7(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int16_7
         integer(I2P), target,  intent(in) :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int16_7


      module function omp_target_disassociate_ptr_f_int32_1(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int32_1
         integer(I4P), target,  intent(in) :: fptr_hos(:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int32_1

      module function omp_target_disassociate_ptr_f_int32_2(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int32_2
         integer(I4P), target,  intent(in) :: fptr_hos(:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int32_2

      module function omp_target_disassociate_ptr_f_int32_3(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int32_3
         integer(I4P), target,  intent(in) :: fptr_hos(:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int32_3

      module function omp_target_disassociate_ptr_f_int32_4(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int32_4
         integer(I4P), target,  intent(in) :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int32_4

      module function omp_target_disassociate_ptr_f_int32_5(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int32_5
         integer(I4P), target,  intent(in) :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int32_5

      module function omp_target_disassociate_ptr_f_int32_6(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int32_6
         integer(I4P), target,  intent(in) :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int32_6

      module function omp_target_disassociate_ptr_f_int32_7(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int32_7
         integer(I4P), target,  intent(in) :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int32_7


      module function omp_target_disassociate_ptr_f_int64_1(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int64_1
         integer(I8P), target,  intent(in) :: fptr_hos(:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int64_1

      module function omp_target_disassociate_ptr_f_int64_2(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int64_2
         integer(I8P), target,  intent(in) :: fptr_hos(:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int64_2

      module function omp_target_disassociate_ptr_f_int64_3(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int64_3
         integer(I8P), target,  intent(in) :: fptr_hos(:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int64_3

      module function omp_target_disassociate_ptr_f_int64_4(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int64_4
         integer(I8P), target,  intent(in) :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int64_4

      module function omp_target_disassociate_ptr_f_int64_5(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int64_5
         integer(I8P), target,  intent(in) :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int64_5

      module function omp_target_disassociate_ptr_f_int64_6(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int64_6
         integer(I8P), target,  intent(in) :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int64_6

      module function omp_target_disassociate_ptr_f_int64_7(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_int64_7
         integer(I8P), target,  intent(in) :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_int64_7


      ! DMR Target Disassociate Pointer Real Routines
      module function omp_target_disassociate_ptr_f_real32_1(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real32_1
         real(R4P), target,  intent(in)    :: fptr_hos(:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real32_1

      module function omp_target_disassociate_ptr_f_real32_2(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real32_2
         real(R4P), target,  intent(in)    :: fptr_hos(:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real32_2

      module function omp_target_disassociate_ptr_f_real32_3(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real32_3
         real(R4P), target,  intent(in)    :: fptr_hos(:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real32_3

      module function omp_target_disassociate_ptr_f_real32_4(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real32_4
         real(R4P), target,  intent(in)    :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real32_4

      module function omp_target_disassociate_ptr_f_real32_5(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real32_5
         real(R4P), target,  intent(in)    :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real32_5

      module function omp_target_disassociate_ptr_f_real32_6(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real32_6
         real(R4P), target,  intent(in)    :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real32_6

      module function omp_target_disassociate_ptr_f_real32_7(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real32_7
         real(R4P), target,  intent(in)    :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real32_7


      module function omp_target_disassociate_ptr_f_real64_1(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real64_1
         real(R8P), target,  intent(in)    :: fptr_hos(:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real64_1

      module function omp_target_disassociate_ptr_f_real64_2(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real64_2
         real(R8P), target,  intent(in)    :: fptr_hos(:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real64_2

      module function omp_target_disassociate_ptr_f_real64_3(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real64_3
         real(R8P), target,  intent(in)    :: fptr_hos(:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real64_3

      module function omp_target_disassociate_ptr_f_real64_4(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real64_4
         real(R8P), target,  intent(in)    :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real64_4

      module function omp_target_disassociate_ptr_f_real64_5(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real64_5
         real(R8P), target,  intent(in)    :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real64_5

      module function omp_target_disassociate_ptr_f_real64_6(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real64_6
         real(R8P), target,  intent(in)    :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real64_6

      module function omp_target_disassociate_ptr_f_real64_7(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real64_7
         real(R8P), target,  intent(in)    :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real64_7


#if defined _real128
      module function omp_target_disassociate_ptr_f_real128_1(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real128_1
         real(R16P), target,  intent(in)   :: fptr_hos(:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real128_1

      module function omp_target_disassociate_ptr_f_real128_2(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real128_2
         real(R16P), target,  intent(in)   :: fptr_hos(:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real128_2

      module function omp_target_disassociate_ptr_f_real128_3(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real128_3
         real(R16P), target,  intent(in)   :: fptr_hos(:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real128_3

      module function omp_target_disassociate_ptr_f_real128_4(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real128_4
         real(R16P), target,  intent(in)   :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real128_4

      module function omp_target_disassociate_ptr_f_real128_5(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real128_5
         real(R16P), target,  intent(in)   :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real128_5

      module function omp_target_disassociate_ptr_f_real128_6(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real128_6
         real(R16P), target,  intent(in)   :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real128_6

      module function omp_target_disassociate_ptr_f_real128_7(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_real128_7
         real(R16P), target,  intent(in)   :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_real128_7

#endif

      ! DMR Target Disassociate Pointer Complex Routines
      module function omp_target_disassociate_ptr_f_cmplx32_1(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx32_1
         complex(R4P), target,  intent(in) :: fptr_hos(:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx32_1

      module function omp_target_disassociate_ptr_f_cmplx32_2(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx32_2
         complex(R4P), target,  intent(in) :: fptr_hos(:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx32_2

      module function omp_target_disassociate_ptr_f_cmplx32_3(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx32_3
         complex(R4P), target,  intent(in) :: fptr_hos(:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx32_3

      module function omp_target_disassociate_ptr_f_cmplx32_4(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx32_4
         complex(R4P), target,  intent(in) :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx32_4

      module function omp_target_disassociate_ptr_f_cmplx32_5(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx32_5
         complex(R4P), target,  intent(in) :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx32_5

      module function omp_target_disassociate_ptr_f_cmplx32_6(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx32_6
         complex(R4P), target,  intent(in) :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx32_6

      module function omp_target_disassociate_ptr_f_cmplx32_7(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx32_7
         complex(R4P), target,  intent(in) :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx32_7


      module function omp_target_disassociate_ptr_f_cmplx64_1(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx64_1
         complex(R8P), target,  intent(in) :: fptr_hos(:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx64_1

      module function omp_target_disassociate_ptr_f_cmplx64_2(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx64_2
         complex(R8P), target,  intent(in) :: fptr_hos(:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx64_2

      module function omp_target_disassociate_ptr_f_cmplx64_3(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx64_3
         complex(R8P), target,  intent(in) :: fptr_hos(:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx64_3

      module function omp_target_disassociate_ptr_f_cmplx64_4(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx64_4
         complex(R8P), target,  intent(in) :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx64_4

      module function omp_target_disassociate_ptr_f_cmplx64_5(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx64_5
         complex(R8P), target,  intent(in) :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx64_5

      module function omp_target_disassociate_ptr_f_cmplx64_6(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx64_6
         complex(R8P), target,  intent(in) :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx64_6

      module function omp_target_disassociate_ptr_f_cmplx64_7(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx64_7
         complex(R8P), target,  intent(in) :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx64_7


#if defined _real128
      module function omp_target_disassociate_ptr_f_cmplx128_1(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx128_1
         complex(R16P), target,  intent(in):: fptr_hos(:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx128_1

      module function omp_target_disassociate_ptr_f_cmplx128_2(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx128_2
         complex(R16P), target,  intent(in):: fptr_hos(:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx128_2

      module function omp_target_disassociate_ptr_f_cmplx128_3(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx128_3
         complex(R16P), target,  intent(in):: fptr_hos(:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx128_3

      module function omp_target_disassociate_ptr_f_cmplx128_4(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx128_4
         complex(R16P), target,  intent(in):: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx128_4

      module function omp_target_disassociate_ptr_f_cmplx128_5(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx128_5
         complex(R16P), target,  intent(in):: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx128_5

      module function omp_target_disassociate_ptr_f_cmplx128_6(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx128_6
         complex(R16P), target,  intent(in):: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx128_6

      module function omp_target_disassociate_ptr_f_cmplx128_7(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_cmplx128_7
         complex(R16P), target,  intent(in):: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_cmplx128_7

#endif

      ! DMR Target Disassociate Pointer Logical Routines
      module function omp_target_disassociate_ptr_f_lgcl32_1(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_lgcl32_1
         logical(I4P), target,  intent(in) :: fptr_hos(:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_lgcl32_1

      module function omp_target_disassociate_ptr_f_lgcl32_2(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_lgcl32_2
         logical(I4P), target,  intent(in) :: fptr_hos(:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_lgcl32_2

      module function omp_target_disassociate_ptr_f_lgcl32_3(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_lgcl32_3
         logical(I4P), target,  intent(in) :: fptr_hos(:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_lgcl32_3

      module function omp_target_disassociate_ptr_f_lgcl32_4(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_lgcl32_4
         logical(I4P), target,  intent(in) :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_lgcl32_4

      module function omp_target_disassociate_ptr_f_lgcl32_5(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_lgcl32_5
         logical(I4P), target,  intent(in) :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_lgcl32_5

      module function omp_target_disassociate_ptr_f_lgcl32_6(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_lgcl32_6
         logical(I4P), target,  intent(in) :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_lgcl32_6

      module function omp_target_disassociate_ptr_f_lgcl32_7(fptr_hos, omp_dev)
         implicit none
         integer(I4P)                       :: omp_target_disassociate_ptr_f_lgcl32_7
         logical(I4P), target,  intent(in) :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)           :: omp_dev
      endfunction omp_target_disassociate_ptr_f_lgcl32_7


   endinterface

! DMR Get Mapped Pointer Routines
   interface
#if defined _F2018
      ! DMR Get Mapped Pointer Integer F2018 Routines
      module subroutine omp_get_mapped_ptr_f_int8(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I1P), pointer, intent(out) :: fptr_dev(..)
         integer(I1P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)           :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int8

      module subroutine omp_get_mapped_ptr_f_int16(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I2P), pointer, intent(out) :: fptr_dev(..)
         integer(I2P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)           :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int16

      module subroutine omp_get_mapped_ptr_f_int32(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I4P), pointer, intent(out) :: fptr_dev(..)
         integer(I4P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)           :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int32

      module subroutine omp_get_mapped_ptr_f_int64(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I8P), pointer, intent(out) :: fptr_dev(..)
         integer(I8P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)           :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int64

      ! DMR Get Mapped Pointer Real F2018 Routines
      module subroutine omp_get_mapped_ptr_f_real32(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R4P), pointer, intent(out) :: fptr_dev(..)
         real(R4P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)           :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real32

      module subroutine omp_get_mapped_ptr_f_real64(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R8P), pointer, intent(out) :: fptr_dev(..)
         real(R8P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)           :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real64

#if defined _real128
      module subroutine omp_get_mapped_ptr_f_real128(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R16P), pointer, intent(out) :: fptr_dev(..)
         real(R16P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)           :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real128
#endif

      ! DMR Get Mapped Pointer Complex F2018 Routines
      module subroutine omp_get_mapped_ptr_f_cmplx32(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R4P), pointer, intent(out) :: fptr_dev(..)
         complex(R4P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)           :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx32

      module subroutine omp_get_mapped_ptr_f_cmplx64(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R8P), pointer, intent(out) :: fptr_dev(..)
         complex(R8P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)           :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx64

#if defined _real128
      module subroutine omp_get_mapped_ptr_f_cmplx128(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R16P), pointer, intent(out) :: fptr_dev(..)
         complex(R16P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)           :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx128
#endif

      ! DMR Get Mapped Pointer Logical F2018 Routines
      module subroutine omp_get_mapped_ptr_f_lgcl32(fptr_dev, fptr_hos, omp_dev)
         implicit none
         logical(I4P), pointer, intent(out) :: fptr_dev(..)
         logical(I4P), target,  intent(in)  :: fptr_hos(..)
         integer(I4P), intent(in)           :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_lgcl32

#else
      ! DMR Get Mapped Pointer Integer Routines
      module subroutine omp_get_mapped_ptr_f_int8_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I1P), target,  contiguous, intent(in)   :: fptr_hos(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int8_1

      module subroutine omp_get_mapped_ptr_f_int8_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I1P), target,  contiguous, intent(in)   :: fptr_hos(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int8_2

      module subroutine omp_get_mapped_ptr_f_int8_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I1P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int8_3

      module subroutine omp_get_mapped_ptr_f_int8_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I1P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int8_4

      module subroutine omp_get_mapped_ptr_f_int8_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I1P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int8_5

      module subroutine omp_get_mapped_ptr_f_int8_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I1P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int8_6

      module subroutine omp_get_mapped_ptr_f_int8_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I1P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int8_7

      module subroutine omp_get_mapped_ptr_f_int16_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I2P), target,  contiguous, intent(in)   :: fptr_hos(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int16_1

      module subroutine omp_get_mapped_ptr_f_int16_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I2P), target,  contiguous, intent(in)   :: fptr_hos(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int16_2

      module subroutine omp_get_mapped_ptr_f_int16_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I2P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int16_3

      module subroutine omp_get_mapped_ptr_f_int16_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I2P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int16_4

      module subroutine omp_get_mapped_ptr_f_int16_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I2P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int16_5

      module subroutine omp_get_mapped_ptr_f_int16_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I2P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int16_6

      module subroutine omp_get_mapped_ptr_f_int16_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I2P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int16_7

      module subroutine omp_get_mapped_ptr_f_int32_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I4P), target,  contiguous, intent(in)   :: fptr_hos(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int32_1

      module subroutine omp_get_mapped_ptr_f_int32_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I4P), target,  contiguous, intent(in)   :: fptr_hos(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int32_2

      module subroutine omp_get_mapped_ptr_f_int32_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I4P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int32_3

      module subroutine omp_get_mapped_ptr_f_int32_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I4P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int32_4

      module subroutine omp_get_mapped_ptr_f_int32_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I4P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int32_5

      module subroutine omp_get_mapped_ptr_f_int32_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I4P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int32_6

      module subroutine omp_get_mapped_ptr_f_int32_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I4P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int32_7

      module subroutine omp_get_mapped_ptr_f_int64_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         integer(I8P), target,  contiguous, intent(in)   :: fptr_hos(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int64_1

      module subroutine omp_get_mapped_ptr_f_int64_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         integer(I8P), target,  contiguous, intent(in)   :: fptr_hos(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int64_2

      module subroutine omp_get_mapped_ptr_f_int64_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         integer(I8P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int64_3

      module subroutine omp_get_mapped_ptr_f_int64_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         integer(I8P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int64_4

      module subroutine omp_get_mapped_ptr_f_int64_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         integer(I8P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int64_5

      module subroutine omp_get_mapped_ptr_f_int64_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int64_6

      module subroutine omp_get_mapped_ptr_f_int64_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_int64_7


      ! DMR Get Mapped Pointer Real Routines
      module subroutine omp_get_mapped_ptr_f_real32_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:)
         real(R4P), target,  contiguous, intent(in)      :: fptr_hos(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real32_1

      module subroutine omp_get_mapped_ptr_f_real32_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:)
         real(R4P), target,  contiguous, intent(in)      :: fptr_hos(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real32_2

      module subroutine omp_get_mapped_ptr_f_real32_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:)
         real(R4P), target,  contiguous, intent(in)      :: fptr_hos(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real32_3

      module subroutine omp_get_mapped_ptr_f_real32_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:)
         real(R4P), target,  contiguous, intent(in)      :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real32_4

      module subroutine omp_get_mapped_ptr_f_real32_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:)
         real(R4P), target,  contiguous, intent(in)      :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real32_5

      module subroutine omp_get_mapped_ptr_f_real32_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:)
         real(R4P), target,  contiguous, intent(in)      :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real32_6

      module subroutine omp_get_mapped_ptr_f_real32_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:,:)
         real(R4P), target,  contiguous, intent(in)      :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real32_7

      module subroutine omp_get_mapped_ptr_f_real64_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:)
         real(R8P), target,  contiguous, intent(in)      :: fptr_hos(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real64_1

      module subroutine omp_get_mapped_ptr_f_real64_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:)
         real(R8P), target,  contiguous, intent(in)      :: fptr_hos(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real64_2

      module subroutine omp_get_mapped_ptr_f_real64_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:)
         real(R8P), target,  contiguous, intent(in)      :: fptr_hos(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real64_3

      module subroutine omp_get_mapped_ptr_f_real64_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:)
         real(R8P), target,  contiguous, intent(in)      :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real64_4

      module subroutine omp_get_mapped_ptr_f_real64_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:)
         real(R8P), target,  contiguous, intent(in)      :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real64_5

      module subroutine omp_get_mapped_ptr_f_real64_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:)
         real(R8P), target,  contiguous, intent(in)      :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real64_6

      module subroutine omp_get_mapped_ptr_f_real64_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(out)     :: fptr_dev(:,:,:,:,:,:,:)
         real(R8P), target,  contiguous, intent(in)      :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real64_7

#if defined _real128
      module subroutine omp_get_mapped_ptr_f_real128_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:)
         real(R16P), target,  contiguous, intent(in)     :: fptr_hos(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real128_1

      module subroutine omp_get_mapped_ptr_f_real128_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:)
         real(R16P), target,  contiguous, intent(in)     :: fptr_hos(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real128_2

      module subroutine omp_get_mapped_ptr_f_real128_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:)
         real(R16P), target,  contiguous, intent(in)     :: fptr_hos(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real128_3

      module subroutine omp_get_mapped_ptr_f_real128_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:)
         real(R16P), target,  contiguous, intent(in)     :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real128_4

      module subroutine omp_get_mapped_ptr_f_real128_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:)
         real(R16P), target,  contiguous, intent(in)     :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real128_5

      module subroutine omp_get_mapped_ptr_f_real128_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:,:)
         real(R16P), target,  contiguous, intent(in)     :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real128_6

      module subroutine omp_get_mapped_ptr_f_real128_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(out)    :: fptr_dev(:,:,:,:,:,:,:)
         real(R16P), target,  contiguous, intent(in)     :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_real128_7

#endif

      ! DMR Get Mapped Pointer Complex Routines
      module subroutine omp_get_mapped_ptr_f_cmplx32_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         complex(R4P), target,  contiguous, intent(in)   :: fptr_hos(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx32_1

      module subroutine omp_get_mapped_ptr_f_cmplx32_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         complex(R4P), target,  contiguous, intent(in)   :: fptr_hos(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx32_2

      module subroutine omp_get_mapped_ptr_f_cmplx32_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         complex(R4P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx32_3

      module subroutine omp_get_mapped_ptr_f_cmplx32_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         complex(R4P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx32_4

      module subroutine omp_get_mapped_ptr_f_cmplx32_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         complex(R4P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx32_5

      module subroutine omp_get_mapped_ptr_f_cmplx32_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         complex(R4P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx32_6

      module subroutine omp_get_mapped_ptr_f_cmplx32_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         complex(R4P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx32_7

      module subroutine omp_get_mapped_ptr_f_cmplx64_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         complex(R8P), target,  contiguous, intent(in)   :: fptr_hos(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx64_1

      module subroutine omp_get_mapped_ptr_f_cmplx64_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         complex(R8P), target,  contiguous, intent(in)   :: fptr_hos(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx64_2

      module subroutine omp_get_mapped_ptr_f_cmplx64_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         complex(R8P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx64_3

      module subroutine omp_get_mapped_ptr_f_cmplx64_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         complex(R8P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx64_4

      module subroutine omp_get_mapped_ptr_f_cmplx64_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         complex(R8P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx64_5

      module subroutine omp_get_mapped_ptr_f_cmplx64_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         complex(R8P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx64_6

      module subroutine omp_get_mapped_ptr_f_cmplx64_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         complex(R8P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx64_7

#if defined _real128
      module subroutine omp_get_mapped_ptr_f_cmplx128_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:)
         complex(R16P), target,  contiguous, intent(in)  :: fptr_hos(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx128_1

      module subroutine omp_get_mapped_ptr_f_cmplx128_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:)
         complex(R16P), target,  contiguous, intent(in)  :: fptr_hos(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx128_2

      module subroutine omp_get_mapped_ptr_f_cmplx128_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:)
         complex(R16P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx128_3

      module subroutine omp_get_mapped_ptr_f_cmplx128_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:)
         complex(R16P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx128_4

      module subroutine omp_get_mapped_ptr_f_cmplx128_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:)
         complex(R16P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx128_5

      module subroutine omp_get_mapped_ptr_f_cmplx128_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:)
         complex(R16P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx128_6

      module subroutine omp_get_mapped_ptr_f_cmplx128_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(out) :: fptr_dev(:,:,:,:,:,:,:)
         complex(R16P), target,  contiguous, intent(in)  :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_cmplx128_7

#endif

      ! DMR Get Mapped Pointer Logical Routines
      module subroutine omp_get_mapped_ptr_f_lgcl32_1(fptr_dev, fptr_hos, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:)
         logical(I4P), target,  contiguous, intent(in)   :: fptr_hos(:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_lgcl32_1

      module subroutine omp_get_mapped_ptr_f_lgcl32_2(fptr_dev, fptr_hos, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:)
         logical(I4P), target,  contiguous, intent(in)   :: fptr_hos(:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_lgcl32_2

      module subroutine omp_get_mapped_ptr_f_lgcl32_3(fptr_dev, fptr_hos, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:)
         logical(I4P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_lgcl32_3

      module subroutine omp_get_mapped_ptr_f_lgcl32_4(fptr_dev, fptr_hos, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:)
         logical(I4P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_lgcl32_4

      module subroutine omp_get_mapped_ptr_f_lgcl32_5(fptr_dev, fptr_hos, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:)
         logical(I4P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_lgcl32_5

      module subroutine omp_get_mapped_ptr_f_lgcl32_6(fptr_dev, fptr_hos, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:)
         logical(I4P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_lgcl32_6

      module subroutine omp_get_mapped_ptr_f_lgcl32_7(fptr_dev, fptr_hos, omp_dev)
         implicit none
         logical(I4P), pointer, contiguous, intent(out)  :: fptr_dev(:,:,:,:,:,:,:)
         logical(I4P), target,  contiguous, intent(in)   :: fptr_hos(:,:,:,:,:,:,:)
         integer(I4P), intent(in)                        :: omp_dev
      endsubroutine omp_get_mapped_ptr_f_lgcl32_7


#endif
   endinterface
#endif

endmodule dmr