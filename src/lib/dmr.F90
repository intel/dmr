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
   use dmr_c_functions

   implicit none

   save

   interface omp_target_is_present_f
      module procedure &
                       omp_target_is_present_f_int8_1,     omp_target_is_present_f_int8_2,     omp_target_is_present_f_int8_3,  &
                       omp_target_is_present_f_int8_4,     omp_target_is_present_f_int8_5,     omp_target_is_present_f_int8_6,  &
                       omp_target_is_present_f_int8_7,     &
                       omp_target_is_present_f_int16_1,    omp_target_is_present_f_int16_2,    omp_target_is_present_f_int16_3,  &
                       omp_target_is_present_f_int16_4,    omp_target_is_present_f_int16_5,    omp_target_is_present_f_int16_6,  &
                       omp_target_is_present_f_int16_7,    &
                       omp_target_is_present_f_int32_1,    omp_target_is_present_f_int32_2,    omp_target_is_present_f_int32_3,  &
                       omp_target_is_present_f_int32_4,    omp_target_is_present_f_int32_5,    omp_target_is_present_f_int32_6,  &
                       omp_target_is_present_f_int32_7,    &
                       omp_target_is_present_f_int64_1,    omp_target_is_present_f_int64_2,    omp_target_is_present_f_int64_3,  &
                       omp_target_is_present_f_int64_4,    omp_target_is_present_f_int64_5,    omp_target_is_present_f_int64_6,  &
                       omp_target_is_present_f_int64_7,    &
#if defined _real128
                       omp_target_is_present_f_real128_1,  omp_target_is_present_f_real128_2,  omp_target_is_present_f_real128_3, &
                       omp_target_is_present_f_real128_4,  omp_target_is_present_f_real128_5,  omp_target_is_present_f_real128_6, &
                       omp_target_is_present_f_real128_7,  &
#endif
                       omp_target_is_present_f_real32_1,   omp_target_is_present_f_real32_2,   omp_target_is_present_f_real32_3,  &
                       omp_target_is_present_f_real32_4,   omp_target_is_present_f_real32_5,   omp_target_is_present_f_real32_6,  &
                       omp_target_is_present_f_real32_7,   &
                       omp_target_is_present_f_real64_1,   omp_target_is_present_f_real64_2,   omp_target_is_present_f_real64_3,  &
                       omp_target_is_present_f_real64_4,   omp_target_is_present_f_real64_5,   omp_target_is_present_f_real64_6,  &
                       omp_target_is_present_f_real64_7,   &
#if defined _real128
                       omp_target_is_present_f_cmplx128_1, omp_target_is_present_f_cmplx128_2, omp_target_is_present_f_cmplx128_3, &
                       omp_target_is_present_f_cmplx128_4, omp_target_is_present_f_cmplx128_5, omp_target_is_present_f_cmplx128_6, &
                       omp_target_is_present_f_cmplx128_7,  &
#endif
                       omp_target_is_present_f_cmplx32_1,  omp_target_is_present_f_cmplx32_2,  omp_target_is_present_f_cmplx32_3,  &
                       omp_target_is_present_f_cmplx32_4,  omp_target_is_present_f_cmplx32_5,  omp_target_is_present_f_cmplx32_6,  &
                       omp_target_is_present_f_cmplx32_7,  &
                       omp_target_is_present_f_cmplx64_1,  omp_target_is_present_f_cmplx64_2,  omp_target_is_present_f_cmplx64_3,  &
                       omp_target_is_present_f_cmplx64_4,  omp_target_is_present_f_cmplx64_5,  omp_target_is_present_f_cmplx64_6,  &
                       omp_target_is_present_f_cmplx64_7
   endinterface omp_target_is_present_f

   interface omp_get_mapped_ptr_f
      module procedure &
                       omp_get_mapped_ptr_f_int8_1,     omp_get_mapped_ptr_f_int8_2,     omp_get_mapped_ptr_f_int8_3,  &
                       omp_get_mapped_ptr_f_int8_4,     omp_get_mapped_ptr_f_int8_5,     omp_get_mapped_ptr_f_int8_6,  &
                       omp_get_mapped_ptr_f_int8_7,     &
                       omp_get_mapped_ptr_f_int16_1,    omp_get_mapped_ptr_f_int16_2,    omp_get_mapped_ptr_f_int16_3,  &
                       omp_get_mapped_ptr_f_int16_4,    omp_get_mapped_ptr_f_int16_5,    omp_get_mapped_ptr_f_int16_6,  &
                       omp_get_mapped_ptr_f_int16_7,    &
                       omp_get_mapped_ptr_f_int32_1,    omp_get_mapped_ptr_f_int32_2,    omp_get_mapped_ptr_f_int32_3,  &
                       omp_get_mapped_ptr_f_int32_4,    omp_get_mapped_ptr_f_int32_5,    omp_get_mapped_ptr_f_int32_6,  &
                       omp_get_mapped_ptr_f_int32_7,    &
                       omp_get_mapped_ptr_f_int64_1,    omp_get_mapped_ptr_f_int64_2,    omp_get_mapped_ptr_f_int64_3,  &
                       omp_get_mapped_ptr_f_int64_4,    omp_get_mapped_ptr_f_int64_5,    omp_get_mapped_ptr_f_int64_6,  &
                       omp_get_mapped_ptr_f_int64_7,    &
#if defined _real128
                       omp_get_mapped_ptr_f_real128_1,  omp_get_mapped_ptr_f_real128_2,  omp_get_mapped_ptr_f_real128_3, &
                       omp_get_mapped_ptr_f_real128_4,  omp_get_mapped_ptr_f_real128_5,  omp_get_mapped_ptr_f_real128_6, &
                       omp_get_mapped_ptr_f_real128_7,  &
#endif
                       omp_get_mapped_ptr_f_real32_1,   omp_get_mapped_ptr_f_real32_2,   omp_get_mapped_ptr_f_real32_3,  &
                       omp_get_mapped_ptr_f_real32_4,   omp_get_mapped_ptr_f_real32_5,   omp_get_mapped_ptr_f_real32_6,  &
                       omp_get_mapped_ptr_f_real32_7,   &
                       omp_get_mapped_ptr_f_real64_1,   omp_get_mapped_ptr_f_real64_2,   omp_get_mapped_ptr_f_real64_3,  &
                       omp_get_mapped_ptr_f_real64_4,   omp_get_mapped_ptr_f_real64_5,   omp_get_mapped_ptr_f_real64_6,  &
                       omp_get_mapped_ptr_f_real64_7,   &
#if defined _real128
                       omp_get_mapped_ptr_f_cmplx128_1, omp_get_mapped_ptr_f_cmplx128_2, omp_get_mapped_ptr_f_cmplx128_3, &
                       omp_get_mapped_ptr_f_cmplx128_4, omp_get_mapped_ptr_f_cmplx128_5, omp_get_mapped_ptr_f_cmplx128_6, &
                       omp_get_mapped_ptr_f_cmplx128_7,  &
#endif
                       omp_get_mapped_ptr_f_cmplx32_1,  omp_get_mapped_ptr_f_cmplx32_2,  omp_get_mapped_ptr_f_cmplx32_3,  &
                       omp_get_mapped_ptr_f_cmplx32_4,  omp_get_mapped_ptr_f_cmplx32_5,  omp_get_mapped_ptr_f_cmplx32_6,  &
                       omp_get_mapped_ptr_f_cmplx32_7,  &
                       omp_get_mapped_ptr_f_cmplx64_1,  omp_get_mapped_ptr_f_cmplx64_2,  omp_get_mapped_ptr_f_cmplx64_3,  &
                       omp_get_mapped_ptr_f_cmplx64_4,  omp_get_mapped_ptr_f_cmplx64_5,  omp_get_mapped_ptr_f_cmplx64_6,  &
                       omp_get_mapped_ptr_f_cmplx64_7
   endinterface omp_get_mapped_ptr_f

   interface omp_target_free_f
      module procedure &
                       omp_target_free_f_int8_1,     omp_target_free_f_int8_2,     omp_target_free_f_int8_3,  &
                       omp_target_free_f_int8_4,     omp_target_free_f_int8_5,     omp_target_free_f_int8_6,  &
                       omp_target_free_f_int8_7,     &
                       omp_target_free_f_int16_1,    omp_target_free_f_int16_2,    omp_target_free_f_int16_3,  &
                       omp_target_free_f_int16_4,    omp_target_free_f_int16_5,    omp_target_free_f_int16_6,  &
                       omp_target_free_f_int16_7,    &
                       omp_target_free_f_int32_1,    omp_target_free_f_int32_2,    omp_target_free_f_int32_3,  &
                       omp_target_free_f_int32_4,    omp_target_free_f_int32_5,    omp_target_free_f_int32_6,  &
                       omp_target_free_f_int32_7,    &
                       omp_target_free_f_int64_1,    omp_target_free_f_int64_2,    omp_target_free_f_int64_3,  &
                       omp_target_free_f_int64_4,    omp_target_free_f_int64_5,    omp_target_free_f_int64_6,  &
                       omp_target_free_f_int64_7,    &
#if defined _real128
                       omp_target_free_f_real128_1,  omp_target_free_f_real128_2,  omp_target_free_f_real128_3, &
                       omp_target_free_f_real128_4,  omp_target_free_f_real128_5,  omp_target_free_f_real128_6, &
                       omp_target_free_f_real128_7,  &
#endif
                       omp_target_free_f_real32_1,   omp_target_free_f_real32_2,   omp_target_free_f_real32_3,  &
                       omp_target_free_f_real32_4,   omp_target_free_f_real32_5,   omp_target_free_f_real32_6,  &
                       omp_target_free_f_real32_7,   &
                       omp_target_free_f_real64_1,   omp_target_free_f_real64_2,   omp_target_free_f_real64_3,  &
                       omp_target_free_f_real64_4,   omp_target_free_f_real64_5,   omp_target_free_f_real64_6,  &
                       omp_target_free_f_real64_7,   &
#if defined _real128
                       omp_target_free_f_cmplx128_1, omp_target_free_f_cmplx128_2, omp_target_free_f_cmplx128_3, &
                       omp_target_free_f_cmplx128_4, omp_target_free_f_cmplx128_5, omp_target_free_f_cmplx128_6, &
                       omp_target_free_f_cmplx128_7,  &
#endif
                       omp_target_free_f_cmplx32_1,  omp_target_free_f_cmplx32_2,  omp_target_free_f_cmplx32_3,  &
                       omp_target_free_f_cmplx32_4,  omp_target_free_f_cmplx32_5,  omp_target_free_f_cmplx32_6,  &
                       omp_target_free_f_cmplx32_7,  &
                       omp_target_free_f_cmplx64_1,  omp_target_free_f_cmplx64_2,  omp_target_free_f_cmplx64_3,  &
                       omp_target_free_f_cmplx64_4,  omp_target_free_f_cmplx64_5,  omp_target_free_f_cmplx64_6,  &
                       omp_target_free_f_cmplx64_7
   endinterface omp_target_free_f

   interface omp_target_alloc_f
      module procedure &
                       omp_target_alloc_f_int8_1,     omp_target_alloc_f_int8_2,     omp_target_alloc_f_int8_3,  &
                       omp_target_alloc_f_int8_4,     omp_target_alloc_f_int8_5,     omp_target_alloc_f_int8_6,  &
                       omp_target_alloc_f_int8_7,     &
                       omp_target_alloc_f_int16_1,    omp_target_alloc_f_int16_2,    omp_target_alloc_f_int16_3,  &
                       omp_target_alloc_f_int16_4,    omp_target_alloc_f_int16_5,    omp_target_alloc_f_int16_6,  &
                       omp_target_alloc_f_int16_7,    &
                       omp_target_alloc_f_int32_1,    omp_target_alloc_f_int32_2,    omp_target_alloc_f_int32_3,  &
                       omp_target_alloc_f_int32_4,    omp_target_alloc_f_int32_5,    omp_target_alloc_f_int32_6,  &
                       omp_target_alloc_f_int32_7,    &
                       omp_target_alloc_f_int64_1,    omp_target_alloc_f_int64_2,    omp_target_alloc_f_int64_3,  &
                       omp_target_alloc_f_int64_4,    omp_target_alloc_f_int64_5,    omp_target_alloc_f_int64_6,  &
                       omp_target_alloc_f_int64_7,    &
#if defined _real128
                       omp_target_alloc_f_real128_1,  omp_target_alloc_f_real128_2,  omp_target_alloc_f_real128_3, &
                       omp_target_alloc_f_real128_4,  omp_target_alloc_f_real128_5,  omp_target_alloc_f_real128_6, &
                       omp_target_alloc_f_real128_7,  &
#endif
                       omp_target_alloc_f_real32_1,   omp_target_alloc_f_real32_2,   omp_target_alloc_f_real32_3,  &
                       omp_target_alloc_f_real32_4,   omp_target_alloc_f_real32_5,   omp_target_alloc_f_real32_6,  &
                       omp_target_alloc_f_real32_7,   &
                       omp_target_alloc_f_real64_1,   omp_target_alloc_f_real64_2,   omp_target_alloc_f_real64_3,  &
                       omp_target_alloc_f_real64_4,   omp_target_alloc_f_real64_5,   omp_target_alloc_f_real64_6,  &
                       omp_target_alloc_f_real64_7,   &
#if defined _real128
                       omp_target_alloc_f_cmplx128_1, omp_target_alloc_f_cmplx128_2, omp_target_alloc_f_cmplx128_3, &
                       omp_target_alloc_f_cmplx128_4, omp_target_alloc_f_cmplx128_5, omp_target_alloc_f_cmplx128_6, &
                       omp_target_alloc_f_cmplx128_7,  &
#endif
                       omp_target_alloc_f_cmplx32_1,  omp_target_alloc_f_cmplx32_2,  omp_target_alloc_f_cmplx32_3,  &
                       omp_target_alloc_f_cmplx32_4,  omp_target_alloc_f_cmplx32_5,  omp_target_alloc_f_cmplx32_6,  &
                       omp_target_alloc_f_cmplx32_7,  &
                       omp_target_alloc_f_cmplx64_1,  omp_target_alloc_f_cmplx64_2,  omp_target_alloc_f_cmplx64_3,  &
                       omp_target_alloc_f_cmplx64_4,  omp_target_alloc_f_cmplx64_5,  omp_target_alloc_f_cmplx64_6,  &
                       omp_target_alloc_f_cmplx64_7
   endinterface omp_target_alloc_f

   interface omp_target_memcpy_f
      module procedure &
                       omp_target_memcpy_f_int8_1,     omp_target_memcpy_f_int8_2,     omp_target_memcpy_f_int8_3,  &
                       omp_target_memcpy_f_int8_4,     omp_target_memcpy_f_int8_5,     omp_target_memcpy_f_int8_6,  &
                       omp_target_memcpy_f_int8_7,     &
                       omp_target_memcpy_f_int16_1,    omp_target_memcpy_f_int16_2,    omp_target_memcpy_f_int16_3,  &
                       omp_target_memcpy_f_int16_4,    omp_target_memcpy_f_int16_5,    omp_target_memcpy_f_int16_6,  &
                       omp_target_memcpy_f_int16_7,    &
                       omp_target_memcpy_f_int32_1,    omp_target_memcpy_f_int32_2,    omp_target_memcpy_f_int32_3,  &
                       omp_target_memcpy_f_int32_4,    omp_target_memcpy_f_int32_5,    omp_target_memcpy_f_int32_6,  &
                       omp_target_memcpy_f_int32_7,    &
                       omp_target_memcpy_f_int64_1,    omp_target_memcpy_f_int64_2,    omp_target_memcpy_f_int64_3,  &
                       omp_target_memcpy_f_int64_4,    omp_target_memcpy_f_int64_5,    omp_target_memcpy_f_int64_6,  &
                       omp_target_memcpy_f_int64_7,    &
#if defined _real128
                       omp_target_memcpy_f_real128_1,  omp_target_memcpy_f_real128_2,  omp_target_memcpy_f_real128_3, &
                       omp_target_memcpy_f_real128_4,  omp_target_memcpy_f_real128_5,  omp_target_memcpy_f_real128_6, &
                       omp_target_memcpy_f_real128_7,  &
#endif
                       omp_target_memcpy_f_real32_1,   omp_target_memcpy_f_real32_2,   omp_target_memcpy_f_real32_3,  &
                       omp_target_memcpy_f_real32_4,   omp_target_memcpy_f_real32_5,   omp_target_memcpy_f_real32_6,  &
                       omp_target_memcpy_f_real32_7,   &
                       omp_target_memcpy_f_real64_1,   omp_target_memcpy_f_real64_2,   omp_target_memcpy_f_real64_3,  &
                       omp_target_memcpy_f_real64_4,   omp_target_memcpy_f_real64_5,   omp_target_memcpy_f_real64_6,  &
                       omp_target_memcpy_f_real64_7,   &
#if defined _real128
                       omp_target_memcpy_f_cmplx128_1, omp_target_memcpy_f_cmplx128_2, omp_target_memcpy_f_cmplx128_3, &
                       omp_target_memcpy_f_cmplx128_4, omp_target_memcpy_f_cmplx128_5, omp_target_memcpy_f_cmplx128_6, &
                       omp_target_memcpy_f_cmplx128_7,  &
#endif
                       omp_target_memcpy_f_cmplx32_1,  omp_target_memcpy_f_cmplx32_2,  omp_target_memcpy_f_cmplx32_3,  &
                       omp_target_memcpy_f_cmplx32_4,  omp_target_memcpy_f_cmplx32_5,  omp_target_memcpy_f_cmplx32_6,  &
                       omp_target_memcpy_f_cmplx32_7,  &
                       omp_target_memcpy_f_cmplx64_1,  omp_target_memcpy_f_cmplx64_2,  omp_target_memcpy_f_cmplx64_3,  &
                       omp_target_memcpy_f_cmplx64_4,  omp_target_memcpy_f_cmplx64_5,  omp_target_memcpy_f_cmplx64_6,  &
                       omp_target_memcpy_f_cmplx64_7
   endinterface omp_target_memcpy_f

   interface omp_target_memcpy_rect_f
      module procedure &
                       omp_target_memcpy_rect_f_int8_2,  omp_target_memcpy_rect_f_int8_3,  &
                       omp_target_memcpy_rect_f_int8_4,  omp_target_memcpy_rect_f_int8_5,  omp_target_memcpy_rect_f_int8_6,  &
                       omp_target_memcpy_rect_f_int8_7,  &
                       omp_target_memcpy_rect_f_int16_2,  omp_target_memcpy_rect_f_int16_3,  &
                       omp_target_memcpy_rect_f_int16_4,  omp_target_memcpy_rect_f_int16_5,  omp_target_memcpy_rect_f_int16_6,  &
                       omp_target_memcpy_rect_f_int16_7,  &
                       omp_target_memcpy_rect_f_int32_2,  omp_target_memcpy_rect_f_int32_3,  &
                       omp_target_memcpy_rect_f_int32_4,  omp_target_memcpy_rect_f_int32_5,  omp_target_memcpy_rect_f_int32_6,  &
                       omp_target_memcpy_rect_f_int32_7,  &
                       omp_target_memcpy_rect_f_int64_2,  omp_target_memcpy_rect_f_int64_3,  &
                       omp_target_memcpy_rect_f_int64_4,  omp_target_memcpy_rect_f_int64_5,  omp_target_memcpy_rect_f_int64_6,  &
                       omp_target_memcpy_rect_f_int64_7,  &
#if defined _real128
                       omp_target_memcpy_rect_f_real128_2, omp_target_memcpy_rect_f_real128_3, &
                       omp_target_memcpy_rect_f_real128_4, omp_target_memcpy_rect_f_real128_5, omp_target_memcpy_rect_f_real128_6, &
                       omp_target_memcpy_rect_f_real128_7, &
#endif
                       omp_target_memcpy_rect_f_real32_2,  omp_target_memcpy_rect_f_real32_3,  &
                       omp_target_memcpy_rect_f_real32_4,  omp_target_memcpy_rect_f_real32_5,  omp_target_memcpy_rect_f_real32_6,  &
                       omp_target_memcpy_rect_f_real32_7,  &
                       omp_target_memcpy_rect_f_real64_2,  omp_target_memcpy_rect_f_real64_3,  &
                       omp_target_memcpy_rect_f_real64_4,  omp_target_memcpy_rect_f_real64_5,  omp_target_memcpy_rect_f_real64_6,  &
                       omp_target_memcpy_rect_f_real64_7,  &
#if defined _real128
                       omp_target_memcpy_rect_f_cmplx128_2, omp_target_memcpy_rect_f_cmplx128_3, &
                       omp_target_memcpy_rect_f_cmplx128_4, omp_target_memcpy_rect_f_cmplx128_5, omp_target_memcpy_rect_f_cmplx128_6, &
                       omp_target_memcpy_rect_f_cmplx128_7,  &
#endif
                       omp_target_memcpy_rect_f_cmplx32_2,  omp_target_memcpy_rect_f_cmplx32_3,  &
                       omp_target_memcpy_rect_f_cmplx32_4,  omp_target_memcpy_rect_f_cmplx32_5,  omp_target_memcpy_rect_f_cmplx32_6,  &
                       omp_target_memcpy_rect_f_cmplx32_7,  &
                       omp_target_memcpy_rect_f_cmplx64_2,  omp_target_memcpy_rect_f_cmplx64_3,  &
                       omp_target_memcpy_rect_f_cmplx64_4,  omp_target_memcpy_rect_f_cmplx64_5,  omp_target_memcpy_rect_f_cmplx64_6,  &
                       omp_target_memcpy_rect_f_cmplx64_7
   endinterface omp_target_memcpy_rect_f

   public omp_target_is_present_f, omp_target_alloc_f, omp_target_free_f, omp_target_memcpy_f, omp_target_memcpy_rect_f, &
          omp_get_mapped_ptr_f

   private

   contains

      ! OpenMP Target Is Present Integer Routines
      function omp_target_is_present_f_int8_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_1
         integer(I1P), target, intent(in) :: fptr_dev(:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int8_1 = .true.
         else
            omp_target_is_present_f_int8_1 = .false.
         endif

       endfunction omp_target_is_present_f_int8_1

      function omp_target_is_present_f_int8_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_2
         integer(I1P), target, intent(in) :: fptr_dev(:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int8_2 = .true.
         else
            omp_target_is_present_f_int8_2 = .false.
         endif

      endfunction omp_target_is_present_f_int8_2

      function omp_target_is_present_f_int8_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_3
         integer(I1P), target, intent(in) :: fptr_dev(:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int8_3 = .true.
         else
            omp_target_is_present_f_int8_3 = .false.
         endif

       endfunction omp_target_is_present_f_int8_3

      function omp_target_is_present_f_int8_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_4
         integer(I1P), target, intent(in) :: fptr_dev(:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int8_4 = .true.
         else
            omp_target_is_present_f_int8_4 = .false.
         endif

      endfunction omp_target_is_present_f_int8_4

      function omp_target_is_present_f_int8_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_5
         integer(I1P), target, intent(in) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int8_5 = .true.
         else
            omp_target_is_present_f_int8_5 = .false.
         endif

      endfunction omp_target_is_present_f_int8_5

      function omp_target_is_present_f_int8_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_6
         integer(I1P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int8_6 = .true.
         else
            omp_target_is_present_f_int8_6 = .false.
         endif

      endfunction omp_target_is_present_f_int8_6

      function omp_target_is_present_f_int8_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_7
         integer(I1P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int8_7 = .true.
         else
            omp_target_is_present_f_int8_7 = .false.
         endif

      endfunction omp_target_is_present_f_int8_7

      function omp_target_is_present_f_int16_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_1
         integer(I2P), target, intent(in) :: fptr_dev(:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int16_1 = .true.
         else
            omp_target_is_present_f_int16_1 = .false.
         endif

       endfunction omp_target_is_present_f_int16_1

      function omp_target_is_present_f_int16_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_2
         integer(I2P), target, intent(in) :: fptr_dev(:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int16_2 = .true.
         else
            omp_target_is_present_f_int16_2 = .false.
         endif

      endfunction omp_target_is_present_f_int16_2

      function omp_target_is_present_f_int16_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_3
         integer(I2P), target, intent(in) :: fptr_dev(:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int16_3 = .true.
         else
            omp_target_is_present_f_int16_3 = .false.
         endif

       endfunction omp_target_is_present_f_int16_3

      function omp_target_is_present_f_int16_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_4
         integer(I2P), target, intent(in) :: fptr_dev(:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int16_4 = .true.
         else
            omp_target_is_present_f_int16_4 = .false.
         endif

      endfunction omp_target_is_present_f_int16_4

      function omp_target_is_present_f_int16_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_5
         integer(I2P), target, intent(in) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int16_5 = .true.
         else
            omp_target_is_present_f_int16_5 = .false.
         endif

      endfunction omp_target_is_present_f_int16_5

      function omp_target_is_present_f_int16_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_6
         integer(I2P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int16_6 = .true.
         else
            omp_target_is_present_f_int16_6 = .false.
         endif

      endfunction omp_target_is_present_f_int16_6

      function omp_target_is_present_f_int16_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_7
         integer(I2P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int16_7 = .true.
         else
            omp_target_is_present_f_int16_7 = .false.
         endif

      endfunction omp_target_is_present_f_int16_7

      function omp_target_is_present_f_int32_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_1
         integer(I4P), target, intent(in) :: fptr_dev(:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int32_1 = .true.
         else
            omp_target_is_present_f_int32_1 = .false.
         endif

       endfunction omp_target_is_present_f_int32_1

      function omp_target_is_present_f_int32_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_2
         integer(I4P), target, intent(in) :: fptr_dev(:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int32_2 = .true.
         else
            omp_target_is_present_f_int32_2 = .false.
         endif

      endfunction omp_target_is_present_f_int32_2

      function omp_target_is_present_f_int32_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_3
         integer(I4P), target, intent(in) :: fptr_dev(:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int32_3 = .true.
         else
            omp_target_is_present_f_int32_3 = .false.
         endif

       endfunction omp_target_is_present_f_int32_3

      function omp_target_is_present_f_int32_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_4
         integer(I4P), target, intent(in) :: fptr_dev(:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int32_4 = .true.
         else
            omp_target_is_present_f_int32_4 = .false.
         endif

      endfunction omp_target_is_present_f_int32_4

      function omp_target_is_present_f_int32_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_5
         integer(I4P), target, intent(in) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int32_5 = .true.
         else
            omp_target_is_present_f_int32_5 = .false.
         endif

      endfunction omp_target_is_present_f_int32_5

      function omp_target_is_present_f_int32_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_6
         integer(I4P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int32_6 = .true.
         else
            omp_target_is_present_f_int32_6 = .false.
         endif

      endfunction omp_target_is_present_f_int32_6

      function omp_target_is_present_f_int32_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_7
         integer(I4P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_int32_7 = .true.
         else
            omp_target_is_present_f_int32_7 = .false.
         endif

      endfunction omp_target_is_present_f_int32_7

      function omp_target_is_present_f_int64_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_1
         integer(I8P), target, intent(in) :: fptr_dev(:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I8P) /= 0_I8P) then
            omp_target_is_present_f_int64_1 = .true.
         else
            omp_target_is_present_f_int64_1 = .false.
         endif

       endfunction omp_target_is_present_f_int64_1

      function omp_target_is_present_f_int64_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_2
         integer(I8P), target, intent(in) :: fptr_dev(:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I8P) /= 0_I8P) then
            omp_target_is_present_f_int64_2 = .true.
         else
            omp_target_is_present_f_int64_2 = .false.
         endif

      endfunction omp_target_is_present_f_int64_2

      function omp_target_is_present_f_int64_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_3
         integer(I8P), target, intent(in) :: fptr_dev(:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I8P) /= 0_I8P) then
            omp_target_is_present_f_int64_3 = .true.
         else
            omp_target_is_present_f_int64_3 = .false.
         endif

       endfunction omp_target_is_present_f_int64_3

      function omp_target_is_present_f_int64_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_4
         integer(I8P), target, intent(in) :: fptr_dev(:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I8P) /= 0_I8P) then
            omp_target_is_present_f_int64_4 = .true.
         else
            omp_target_is_present_f_int64_4 = .false.
         endif

      endfunction omp_target_is_present_f_int64_4

      function omp_target_is_present_f_int64_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_5
         integer(I8P), target, intent(in) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I8P) /= 0_I8P) then
            omp_target_is_present_f_int64_5 = .true.
         else
            omp_target_is_present_f_int64_5 = .false.
         endif

      endfunction omp_target_is_present_f_int64_5

      function omp_target_is_present_f_int64_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_6
         integer(I8P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I8P) /= 0_I8P) then
            omp_target_is_present_f_int64_6 = .true.
         else
            omp_target_is_present_f_int64_6 = .false.
         endif

      endfunction omp_target_is_present_f_int64_6

      function omp_target_is_present_f_int64_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_7
         integer(I8P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I8P) /= 0_I8P) then
            omp_target_is_present_f_int64_7 = .true.
         else
            omp_target_is_present_f_int64_7 = .false.
         endif

      endfunction omp_target_is_present_f_int64_7

      ! OpenMP Get Mapped Pointer Integer Routines
      function omp_get_mapped_ptr_f_int8_1(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int8_1
         integer(I1P), target, intent(inout) :: fptr_dev(:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int8_1 = .true.
         else
            omp_get_mapped_ptr_f_int8_1 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int8_1

      function omp_get_mapped_ptr_f_int8_2(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int8_2
         integer(I1P), target, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int8_2 = .true.
         else
            omp_get_mapped_ptr_f_int8_2 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int8_2

      function omp_get_mapped_ptr_f_int8_3(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int8_3
         integer(I1P), target, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int8_3 = .true.
         else
            omp_get_mapped_ptr_f_int8_3 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int8_3

      function omp_get_mapped_ptr_f_int8_4(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int8_4
         integer(I1P), target, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int8_4 = .true.
         else
            omp_get_mapped_ptr_f_int8_4 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int8_4

      function omp_get_mapped_ptr_f_int8_5(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int8_5
         integer(I1P), target, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int8_5 = .true.
         else
            omp_get_mapped_ptr_f_int8_5 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int8_5

      function omp_get_mapped_ptr_f_int8_6(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int8_6
         integer(I1P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int8_6 = .true.
         else
            omp_get_mapped_ptr_f_int8_6 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int8_6

      function omp_get_mapped_ptr_f_int8_7(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int8_7
         integer(I1P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int8_7 = .true.
         else
            omp_get_mapped_ptr_f_int8_7 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int8_7

      function omp_get_mapped_ptr_f_int16_1(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int16_1
         integer(I2P), target, intent(inout) :: fptr_dev(:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int16_1 = .true.
         else
            omp_get_mapped_ptr_f_int16_1 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int16_1

      function omp_get_mapped_ptr_f_int16_2(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int16_2
         integer(I2P), target, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int16_2 = .true.
         else
            omp_get_mapped_ptr_f_int16_2 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int16_2

      function omp_get_mapped_ptr_f_int16_3(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int16_3
         integer(I2P), target, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int16_3 = .true.
         else
            omp_get_mapped_ptr_f_int16_3 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int16_3

      function omp_get_mapped_ptr_f_int16_4(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int16_4
         integer(I2P), target, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int16_4 = .true.
         else
            omp_get_mapped_ptr_f_int16_4 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int16_4

      function omp_get_mapped_ptr_f_int16_5(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int16_5
         integer(I2P), target, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int16_5 = .true.
         else
            omp_get_mapped_ptr_f_int16_5 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int16_5

      function omp_get_mapped_ptr_f_int16_6(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int16_6
         integer(I2P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int16_6 = .true.
         else
            omp_get_mapped_ptr_f_int16_6 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int16_6

      function omp_get_mapped_ptr_f_int16_7(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int16_7
         integer(I2P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int16_7 = .true.
         else
            omp_get_mapped_ptr_f_int16_7 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int16_7

      function omp_get_mapped_ptr_f_int32_1(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int32_1
         integer(I4P), target, intent(inout) :: fptr_dev(:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int32_1 = .true.
         else
            omp_get_mapped_ptr_f_int32_1 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int32_1

      function omp_get_mapped_ptr_f_int32_2(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int32_2
         integer(I4P), target, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int32_2 = .true.
         else
            omp_get_mapped_ptr_f_int32_2 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int32_2

      function omp_get_mapped_ptr_f_int32_3(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int32_3
         integer(I4P), target, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int32_3 = .true.
         else
            omp_get_mapped_ptr_f_int32_3 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int32_3

      function omp_get_mapped_ptr_f_int32_4(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int32_4
         integer(I4P), target, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int32_4 = .true.
         else
            omp_get_mapped_ptr_f_int32_4 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int32_4

      function omp_get_mapped_ptr_f_int32_5(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int32_5
         integer(I4P), target, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int32_5 = .true.
         else
            omp_get_mapped_ptr_f_int32_5 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int32_5

      function omp_get_mapped_ptr_f_int32_6(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int32_6
         integer(I4P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int32_6 = .true.
         else
            omp_get_mapped_ptr_f_int32_6 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int32_6

      function omp_get_mapped_ptr_f_int32_7(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int32_7
         integer(I4P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int32_7 = .true.
         else
            omp_get_mapped_ptr_f_int32_7 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int32_7

      function omp_get_mapped_ptr_f_int64_1(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int64_1
         integer(I8P), target, intent(inout) :: fptr_dev(:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int64_1 = .true.
         else
            omp_get_mapped_ptr_f_int64_1 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int64_1

      function omp_get_mapped_ptr_f_int64_2(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int64_2
         integer(I8P), target, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int64_2 = .true.
         else
            omp_get_mapped_ptr_f_int64_2 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int64_2

      function omp_get_mapped_ptr_f_int64_3(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int64_3
         integer(I8P), target, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int64_3 = .true.
         else
            omp_get_mapped_ptr_f_int64_3 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int64_3

      function omp_get_mapped_ptr_f_int64_4(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int64_4
         integer(I8P), target, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int64_4 = .true.
         else
            omp_get_mapped_ptr_f_int64_4 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int64_4

      function omp_get_mapped_ptr_f_int64_5(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int64_5
         integer(I8P), target, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int64_5 = .true.
         else
            omp_get_mapped_ptr_f_int64_5 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int64_5

      function omp_get_mapped_ptr_f_int64_6(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int64_6
         integer(I8P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int64_6 = .true.
         else
            omp_get_mapped_ptr_f_int64_6 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int64_6

      function omp_get_mapped_ptr_f_int64_7(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int64_7
         integer(I8P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_int64_7 = .true.
         else
            omp_get_mapped_ptr_f_int64_7 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_int64_7

      ! OpenMP Target Free Integer Routines
      subroutine omp_target_free_f_int8_1(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int8_1

      subroutine omp_target_free_f_int8_2(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int8_2

      subroutine omp_target_free_f_int8_3(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int8_3

      subroutine omp_target_free_f_int8_4(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int8_4

      subroutine omp_target_free_f_int8_5(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int8_5

      subroutine omp_target_free_f_int8_6(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int8_6

      subroutine omp_target_free_f_int8_7(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int8_7

      subroutine omp_target_free_f_int16_1(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int16_1

      subroutine omp_target_free_f_int16_2(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int16_2

      subroutine omp_target_free_f_int16_3(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int16_3

      subroutine omp_target_free_f_int16_4(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int16_4

      subroutine omp_target_free_f_int16_5(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int16_5

      subroutine omp_target_free_f_int16_6(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int16_6

      subroutine omp_target_free_f_int16_7(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int16_7

      subroutine omp_target_free_f_int32_1(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int32_1

      subroutine omp_target_free_f_int32_2(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int32_2

      subroutine omp_target_free_f_int32_3(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int32_3

      subroutine omp_target_free_f_int32_4(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int32_4

      subroutine omp_target_free_f_int32_5(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int32_5

      subroutine omp_target_free_f_int32_6(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int32_6

      subroutine omp_target_free_f_int32_7(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int32_7

      subroutine omp_target_free_f_int64_1(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int64_1

      subroutine omp_target_free_f_int64_2(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int64_2

      subroutine omp_target_free_f_int64_3(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int64_3

      subroutine omp_target_free_f_int64_4(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int64_4

      subroutine omp_target_free_f_int64_5(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int64_5

      subroutine omp_target_free_f_int64_6(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int64_6

      subroutine omp_target_free_f_int64_7(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_int64_7

      ! OpenMP Target Alloc Integer Routines
      subroutine omp_target_alloc_f_int8_1(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_int8_1

      subroutine omp_target_alloc_f_int8_2(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_int8_2

      subroutine omp_target_alloc_f_int8_3(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_int8_3

      subroutine omp_target_alloc_f_int8_4(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_int8_4

      subroutine omp_target_alloc_f_int8_5(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_int8_5

      subroutine omp_target_alloc_f_int8_6(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_int8_6

      subroutine omp_target_alloc_f_int8_7(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I1P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_int8_7

      subroutine omp_target_alloc_f_int16_1(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_int16_1

      subroutine omp_target_alloc_f_int16_2(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_int16_2

      subroutine omp_target_alloc_f_int16_3(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_int16_3

      subroutine omp_target_alloc_f_int16_4(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_int16_4

      subroutine omp_target_alloc_f_int16_5(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_int16_5

      subroutine omp_target_alloc_f_int16_6(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_int16_6

      subroutine omp_target_alloc_f_int16_7(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I2P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_int16_7

      subroutine omp_target_alloc_f_int32_1(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_int32_1

      subroutine omp_target_alloc_f_int32_2(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_int32_2

      subroutine omp_target_alloc_f_int32_3(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_int32_3

      subroutine omp_target_alloc_f_int32_4(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_int32_4

      subroutine omp_target_alloc_f_int32_5(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_int32_5

      subroutine omp_target_alloc_f_int32_6(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_int32_6

      subroutine omp_target_alloc_f_int32_7(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_int32_7

      subroutine omp_target_alloc_f_int64_1(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_int64_1

      subroutine omp_target_alloc_f_int64_2(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_int64_2

      subroutine omp_target_alloc_f_int64_3(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_int64_3

      subroutine omp_target_alloc_f_int64_4(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_int64_4

      subroutine omp_target_alloc_f_int64_5(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_int64_5

      subroutine omp_target_alloc_f_int64_6(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_int64_6

      subroutine omp_target_alloc_f_int64_7(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1_I8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_int64_7

      ! OpenMP Target Memcpy Integer Routines
      subroutine omp_target_memcpy_f_int8_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int8_1

      subroutine omp_target_memcpy_f_int8_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int8_2

      subroutine omp_target_memcpy_f_int8_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int8_3

      subroutine omp_target_memcpy_f_int8_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int8_4

      subroutine omp_target_memcpy_f_int8_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int8_5

      subroutine omp_target_memcpy_f_int8_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int8_6

      subroutine omp_target_memcpy_f_int8_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int8_7

      subroutine omp_target_memcpy_f_int16_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int16_1

      subroutine omp_target_memcpy_f_int16_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int16_2

      subroutine omp_target_memcpy_f_int16_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int16_3

      subroutine omp_target_memcpy_f_int16_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int16_4

      subroutine omp_target_memcpy_f_int16_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int16_5

      subroutine omp_target_memcpy_f_int16_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int16_6

      subroutine omp_target_memcpy_f_int16_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int16_7

      subroutine omp_target_memcpy_f_int32_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int32_1

      subroutine omp_target_memcpy_f_int32_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int32_2

      subroutine omp_target_memcpy_f_int32_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int32_3

      subroutine omp_target_memcpy_f_int32_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int32_4

      subroutine omp_target_memcpy_f_int32_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int32_5

      subroutine omp_target_memcpy_f_int32_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int32_6

      subroutine omp_target_memcpy_f_int32_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int32_7

      subroutine omp_target_memcpy_f_int64_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int64_1

      subroutine omp_target_memcpy_f_int64_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int64_2

      subroutine omp_target_memcpy_f_int64_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int64_3

      subroutine omp_target_memcpy_f_int64_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int64_4

      subroutine omp_target_memcpy_f_int64_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int64_5

      subroutine omp_target_memcpy_f_int64_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int64_6

      subroutine omp_target_memcpy_f_int64_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_int64_7

     ! OpenMP Target Memcpy Rect Integer Routines
      subroutine omp_target_memcpy_rect_f_int8_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int8_2

      subroutine omp_target_memcpy_rect_f_int8_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int8_3

      subroutine omp_target_memcpy_rect_f_int8_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int8_4

      subroutine omp_target_memcpy_rect_f_int8_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int8_5

      subroutine omp_target_memcpy_rect_f_int8_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int8_6

      subroutine omp_target_memcpy_rect_f_int8_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I1P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int8_7

      subroutine omp_target_memcpy_rect_f_int16_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int16_2

      subroutine omp_target_memcpy_rect_f_int16_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int16_3

      subroutine omp_target_memcpy_rect_f_int16_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int16_4

      subroutine omp_target_memcpy_rect_f_int16_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int16_5

      subroutine omp_target_memcpy_rect_f_int16_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int16_6

      subroutine omp_target_memcpy_rect_f_int16_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I2P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int16_7

      subroutine omp_target_memcpy_rect_f_int32_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int32_2

      subroutine omp_target_memcpy_rect_f_int32_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int32_3

      subroutine omp_target_memcpy_rect_f_int32_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int32_4

      subroutine omp_target_memcpy_rect_f_int32_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int32_5

      subroutine omp_target_memcpy_rect_f_int32_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int32_6

      subroutine omp_target_memcpy_rect_f_int32_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int32_7

      subroutine omp_target_memcpy_rect_f_int64_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int64_2

      subroutine omp_target_memcpy_rect_f_int64_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int64_3

      subroutine omp_target_memcpy_rect_f_int64_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int64_4

      subroutine omp_target_memcpy_rect_f_int64_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int64_5

      subroutine omp_target_memcpy_rect_f_int64_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int64_6

      subroutine omp_target_memcpy_rect_f_int64_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1_I8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_int64_7

      ! OpenMP Target Is Present Real Routines
      function omp_target_is_present_f_real32_1(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_1
         real(R4P), target, intent(in) :: fptr_dev(:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real32_1 = .true.
         else
            omp_target_is_present_f_real32_1 = .false.
         endif

       endfunction omp_target_is_present_f_real32_1

      function omp_target_is_present_f_real32_2(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_2
         real(R4P), target, intent(in) :: fptr_dev(:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real32_2 = .true.
         else
            omp_target_is_present_f_real32_2 = .false.
         endif

      endfunction omp_target_is_present_f_real32_2

      function omp_target_is_present_f_real32_3(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_3
         real(R4P), target, intent(in) :: fptr_dev(:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real32_3 = .true.
         else
            omp_target_is_present_f_real32_3 = .false.
         endif

       endfunction omp_target_is_present_f_real32_3

      function omp_target_is_present_f_real32_4(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_4
         real(R4P), target, intent(in) :: fptr_dev(:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real32_4 = .true.
         else
            omp_target_is_present_f_real32_4 = .false.
         endif

      endfunction omp_target_is_present_f_real32_4

      function omp_target_is_present_f_real32_5(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_5
         real(R4P), target, intent(in) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real32_5 = .true.
         else
            omp_target_is_present_f_real32_5 = .false.
         endif

      endfunction omp_target_is_present_f_real32_5

      function omp_target_is_present_f_real32_6(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_6
         real(R4P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real32_6 = .true.
         else
            omp_target_is_present_f_real32_6 = .false.
         endif

      endfunction omp_target_is_present_f_real32_6

      function omp_target_is_present_f_real32_7(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_7
         real(R4P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real32_7 = .true.
         else
            omp_target_is_present_f_real32_7 = .false.
         endif

      endfunction omp_target_is_present_f_real32_7

      function omp_target_is_present_f_real64_1(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_1
         real(R8P), target, intent(in) :: fptr_dev(:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real64_1 = .true.
         else
            omp_target_is_present_f_real64_1 = .false.
         endif

       endfunction omp_target_is_present_f_real64_1

      function omp_target_is_present_f_real64_2(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_2
         real(R8P), target, intent(in) :: fptr_dev(:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real64_2 = .true.
         else
            omp_target_is_present_f_real64_2 = .false.
         endif

      endfunction omp_target_is_present_f_real64_2

      function omp_target_is_present_f_real64_3(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_3
         real(R8P), target, intent(in) :: fptr_dev(:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real64_3 = .true.
         else
            omp_target_is_present_f_real64_3 = .false.
         endif

       endfunction omp_target_is_present_f_real64_3

      function omp_target_is_present_f_real64_4(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_4
         real(R8P), target, intent(in) :: fptr_dev(:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real64_4 = .true.
         else
            omp_target_is_present_f_real64_4 = .false.
         endif

      endfunction omp_target_is_present_f_real64_4

      function omp_target_is_present_f_real64_5(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_5
         real(R8P), target, intent(in) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real64_5 = .true.
         else
            omp_target_is_present_f_real64_5 = .false.
         endif

      endfunction omp_target_is_present_f_real64_5

      function omp_target_is_present_f_real64_6(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_6
         real(R8P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real64_6 = .true.
         else
            omp_target_is_present_f_real64_6 = .false.
         endif

      endfunction omp_target_is_present_f_real64_6

      function omp_target_is_present_f_real64_7(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_7
         real(R8P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real64_7 = .true.
         else
            omp_target_is_present_f_real64_7 = .false.
         endif

      endfunction omp_target_is_present_f_real64_7

#if defined _real128
      function omp_target_is_present_f_real128_1(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_1
         real(R16P), target, intent(in) :: fptr_dev(:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real128_1 = .true.
         else
            omp_target_is_present_f_real128_1 = .false.
         endif

       endfunction omp_target_is_present_f_real128_1

      function omp_target_is_present_f_real128_2(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_2
         real(R16P), target, intent(in) :: fptr_dev(:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real128_2 = .true.
         else
            omp_target_is_present_f_real128_2 = .false.
         endif

      endfunction omp_target_is_present_f_real128_2

      function omp_target_is_present_f_real128_3(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_3
         real(R16P), target, intent(in) :: fptr_dev(:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real128_3 = .true.
         else
            omp_target_is_present_f_real128_3 = .false.
         endif

       endfunction omp_target_is_present_f_real128_3

      function omp_target_is_present_f_real128_4(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_4
         real(R16P), target, intent(in) :: fptr_dev(:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real128_4 = .true.
         else
            omp_target_is_present_f_real128_4 = .false.
         endif

      endfunction omp_target_is_present_f_real128_4

      function omp_target_is_present_f_real128_5(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_5
         real(R16P), target, intent(in) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real128_5 = .true.
         else
            omp_target_is_present_f_real128_5 = .false.
         endif

      endfunction omp_target_is_present_f_real128_5

      function omp_target_is_present_f_real128_6(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_6
         real(R16P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real128_6 = .true.
         else
            omp_target_is_present_f_real128_6 = .false.
         endif

      endfunction omp_target_is_present_f_real128_6

      function omp_target_is_present_f_real128_7(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_7
         real(R16P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_real128_7 = .true.
         else
            omp_target_is_present_f_real128_7 = .false.
         endif

      endfunction omp_target_is_present_f_real128_7
#endif

      ! OpenMP Get Mapped Pointer Real Routines
      function omp_get_mapped_ptr_f_real32_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real32_1
         real(R4P), target, intent(inout) :: fptr_dev(:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real32_1 = .true.
         else
            omp_get_mapped_ptr_f_real32_1 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real32_1

      function omp_get_mapped_ptr_f_real32_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real32_2
         real(R4P), target, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real32_2 = .true.
         else
            omp_get_mapped_ptr_f_real32_2 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real32_2

      function omp_get_mapped_ptr_f_real32_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real32_3
         real(R4P), target, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real32_3 = .true.
         else
            omp_get_mapped_ptr_f_real32_3 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real32_3

      function omp_get_mapped_ptr_f_real32_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real32_4
         real(R4P), target, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real32_4 = .true.
         else
            omp_get_mapped_ptr_f_real32_4 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real32_4

      function omp_get_mapped_ptr_f_real32_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real32_5
         real(R4P), target, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real32_5 = .true.
         else
            omp_get_mapped_ptr_f_real32_5 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real32_5

      function omp_get_mapped_ptr_f_real32_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real32_6
         real(R4P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real32_6 = .true.
         else
            omp_get_mapped_ptr_f_real32_6 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real32_6

      function omp_get_mapped_ptr_f_real32_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real32_7
         real(R4P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real32_7 = .true.
         else
            omp_get_mapped_ptr_f_real32_7 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real32_7

      function omp_get_mapped_ptr_f_real64_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real64_1
         real(R8P), target, intent(inout) :: fptr_dev(:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real64_1 = .true.
         else
            omp_get_mapped_ptr_f_real64_1 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real64_1

      function omp_get_mapped_ptr_f_real64_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real64_2
         real(R8P), target, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real64_2 = .true.
         else
            omp_get_mapped_ptr_f_real64_2 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real64_2

      function omp_get_mapped_ptr_f_real64_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real64_3
         real(R8P), target, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real64_3 = .true.
         else
            omp_get_mapped_ptr_f_real64_3 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real64_3

      function omp_get_mapped_ptr_f_real64_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real64_4
         real(R8P), target, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real64_4 = .true.
         else
            omp_get_mapped_ptr_f_real64_4 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real64_4

      function omp_get_mapped_ptr_f_real64_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real64_5
         real(R8P), target, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real64_5 = .true.
         else
            omp_get_mapped_ptr_f_real64_5 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real64_5

      function omp_get_mapped_ptr_f_real64_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real64_6
         real(R8P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real64_6 = .true.
         else
            omp_get_mapped_ptr_f_real64_6 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real64_6

      function omp_get_mapped_ptr_f_real64_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real64_7
         real(R8P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real64_7 = .true.
         else
            omp_get_mapped_ptr_f_real64_7 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real64_7

#if defined _real128
      function omp_get_mapped_ptr_f_real128_1(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_get_mapped_ptr_f_real128_1
         real(R16P), target, intent(inout) :: fptr_dev(:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real128_1 = .true.
         else
            omp_get_mapped_ptr_f_real128_1 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real128_1

      function omp_get_mapped_ptr_f_real128_2(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_get_mapped_ptr_f_real128_2
         real(R16P), target, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real128_2 = .true.
         else
            omp_get_mapped_ptr_f_real128_2 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real128_2

      function omp_get_mapped_ptr_f_real128_3(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_get_mapped_ptr_f_real128_3
         real(R16P), target, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real128_3 = .true.
         else
            omp_get_mapped_ptr_f_real128_3 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real128_3

      function omp_get_mapped_ptr_f_real128_4(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_get_mapped_ptr_f_real128_4
         real(R16P), target, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real128_4 = .true.
         else
            omp_get_mapped_ptr_f_real128_4 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real128_4

      function omp_get_mapped_ptr_f_real128_5(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_get_mapped_ptr_f_real128_5
         real(R16P), target, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real128_5 = .true.
         else
            omp_get_mapped_ptr_f_real128_5 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real128_5

      function omp_get_mapped_ptr_f_real128_6(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_get_mapped_ptr_f_real128_6
         real(R16P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real128_6 = .true.
         else
            omp_get_mapped_ptr_f_real128_6 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real128_6

      function omp_get_mapped_ptr_f_real128_7(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_get_mapped_ptr_f_real128_7
         real(R16P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_real128_7 = .true.
         else
            omp_get_mapped_ptr_f_real128_7 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_real128_7
#endif

      ! OpenMP Target Free Real Routines
      subroutine omp_target_free_f_real32_1(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real32_1

      subroutine omp_target_free_f_real32_2(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real32_2

      subroutine omp_target_free_f_real32_3(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real32_3

      subroutine omp_target_free_f_real32_4(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real32_4

      subroutine omp_target_free_f_real32_5(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real32_5

      subroutine omp_target_free_f_real32_6(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real32_6

      subroutine omp_target_free_f_real32_7(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real32_7

      subroutine omp_target_free_f_real64_1(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real64_1

      subroutine omp_target_free_f_real64_2(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real64_2

      subroutine omp_target_free_f_real64_3(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real64_3

      subroutine omp_target_free_f_real64_4(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real64_4

      subroutine omp_target_free_f_real64_5(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real64_5

      subroutine omp_target_free_f_real64_6(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real64_6

      subroutine omp_target_free_f_real64_7(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real64_7

#if defined _real128
      subroutine omp_target_free_f_real128_1(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real128_1

      subroutine omp_target_free_f_real128_2(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real128_2

      subroutine omp_target_free_f_real128_3(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real128_3

      subroutine omp_target_free_f_real128_4(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real128_4

      subroutine omp_target_free_f_real128_5(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real128_5

      subroutine omp_target_free_f_real128_6(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real128_6

      subroutine omp_target_free_f_real128_7(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_real128_7
#endif

      ! OpenMP Target Alloc Real Routines
      subroutine omp_target_alloc_f_real32_1(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)          :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_real32_1

      subroutine omp_target_alloc_f_real32_2(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)          :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_real32_2

      subroutine omp_target_alloc_f_real32_3(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)          :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_real32_3

      subroutine omp_target_alloc_f_real32_4(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_real32_4

      subroutine omp_target_alloc_f_real32_5(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_real32_5

      subroutine omp_target_alloc_f_real32_6(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_real32_6

      subroutine omp_target_alloc_f_real32_7(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_real32_7

      subroutine omp_target_alloc_f_real64_1(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)          :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_real64_1

      subroutine omp_target_alloc_f_real64_2(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)          :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_real64_2

      subroutine omp_target_alloc_f_real64_3(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)          :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_real64_3

      subroutine omp_target_alloc_f_real64_4(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_real64_4

      subroutine omp_target_alloc_f_real64_5(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_real64_5

      subroutine omp_target_alloc_f_real64_6(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_real64_6

      subroutine omp_target_alloc_f_real64_7(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_real64_7

#if defined _real128
      subroutine omp_target_alloc_f_real128_1(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)           :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(dimensions, c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_real128_1

      subroutine omp_target_alloc_f_real128_2(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)           :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_real128_2

      subroutine omp_target_alloc_f_real128_3(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)           :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_real128_3

      subroutine omp_target_alloc_f_real128_4(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)           :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_real128_4

      subroutine omp_target_alloc_f_real128_5(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)           :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_real128_5

      subroutine omp_target_alloc_f_real128_6(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)           :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_real128_6

      subroutine omp_target_alloc_f_real128_7(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)           :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_real128_7
#endif

      ! OpenMP Target Memcpy Real Routines
      subroutine omp_target_memcpy_f_real32_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real32_1

      subroutine omp_target_memcpy_f_real32_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real32_2

      subroutine omp_target_memcpy_f_real32_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real32_3

      subroutine omp_target_memcpy_f_real32_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real32_4

      subroutine omp_target_memcpy_f_real32_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real32_5

      subroutine omp_target_memcpy_f_real32_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real32_6

      subroutine omp_target_memcpy_f_real32_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real32_7

      subroutine omp_target_memcpy_f_real64_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real64_1

      subroutine omp_target_memcpy_f_real64_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real64_2

      subroutine omp_target_memcpy_f_real64_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real64_3

      subroutine omp_target_memcpy_f_real64_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real64_4

      subroutine omp_target_memcpy_f_real64_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real64_5

      subroutine omp_target_memcpy_f_real64_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real64_6

      subroutine omp_target_memcpy_f_real64_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real64_7

#if defined _real128
      subroutine omp_target_memcpy_f_real128_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real128_1

      subroutine omp_target_memcpy_f_real128_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real128_2

      subroutine omp_target_memcpy_f_real128_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real128_3

      subroutine omp_target_memcpy_f_real128_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real128_4

      subroutine omp_target_memcpy_f_real128_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real128_5

      subroutine omp_target_memcpy_f_real128_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real128_6

      subroutine omp_target_memcpy_f_real128_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_real128_7
#endif

     ! OpenMP Target Memcpy Rect Real Routines
      subroutine omp_target_memcpy_rect_f_real32_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R4P),    contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_real32_2

      subroutine omp_target_memcpy_rect_f_real32_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R4P),    contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_real32_3

      subroutine omp_target_memcpy_rect_f_real32_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R4P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_real32_4

      subroutine omp_target_memcpy_rect_f_real32_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R4P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_real32_5

      subroutine omp_target_memcpy_rect_f_real32_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R4P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_real32_6

      subroutine omp_target_memcpy_rect_f_real32_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R4P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_real32_7

      subroutine omp_target_memcpy_rect_f_real64_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R8P),    contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_real64_2

      subroutine omp_target_memcpy_rect_f_real64_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R8P),    contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_real64_3

      subroutine omp_target_memcpy_rect_f_real64_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R8P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_real64_4

      subroutine omp_target_memcpy_rect_f_real64_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R8P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_real64_5

      subroutine omp_target_memcpy_rect_f_real64_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R8P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_real64_6

      subroutine omp_target_memcpy_rect_f_real64_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R8P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_real64_7

#if defined _real128
      subroutine omp_target_memcpy_rect_f_real128_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R16P),   contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_real128_2

      subroutine omp_target_memcpy_rect_f_real128_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R16P),   contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_real128_3

      subroutine omp_target_memcpy_rect_f_real128_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R16P),   contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_real128_4

      subroutine omp_target_memcpy_rect_f_real128_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R16P),   contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_real128_5

      subroutine omp_target_memcpy_rect_f_real128_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R16P),   contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_real128_6

      subroutine omp_target_memcpy_rect_f_real128_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R16P),   contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_real128_7
#endif

      ! OpenMP Target Is Present Complex Routines
      function omp_target_is_present_f_cmplx32_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_1
         complex(R4P), target, intent(in) :: fptr_dev(:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx32_1 = .true.
         else
            omp_target_is_present_f_cmplx32_1 = .false.
         endif

       endfunction omp_target_is_present_f_cmplx32_1

      function omp_target_is_present_f_cmplx32_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_2
         complex(R4P), target, intent(in) :: fptr_dev(:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx32_2 = .true.
         else
            omp_target_is_present_f_cmplx32_2 = .false.
         endif

      endfunction omp_target_is_present_f_cmplx32_2

      function omp_target_is_present_f_cmplx32_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_3
         complex(R4P), target, intent(in) :: fptr_dev(:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx32_3 = .true.
         else
            omp_target_is_present_f_cmplx32_3 = .false.
         endif

       endfunction omp_target_is_present_f_cmplx32_3

      function omp_target_is_present_f_cmplx32_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_4
         complex(R4P), target, intent(in) :: fptr_dev(:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx32_4 = .true.
         else
            omp_target_is_present_f_cmplx32_4 = .false.
         endif

      endfunction omp_target_is_present_f_cmplx32_4

      function omp_target_is_present_f_cmplx32_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_5
         complex(R4P), target, intent(in) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx32_5 = .true.
         else
            omp_target_is_present_f_cmplx32_5 = .false.
         endif

      endfunction omp_target_is_present_f_cmplx32_5

      function omp_target_is_present_f_cmplx32_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_6
         complex(R4P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx32_6 = .true.
         else
            omp_target_is_present_f_cmplx32_6 = .false.
         endif

      endfunction omp_target_is_present_f_cmplx32_6

      function omp_target_is_present_f_cmplx32_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_7
         complex(R4P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx32_7 = .true.
         else
            omp_target_is_present_f_cmplx32_7 = .false.
         endif

      endfunction omp_target_is_present_f_cmplx32_7

      function omp_target_is_present_f_cmplx64_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_1
         complex(R8P), target, intent(in) :: fptr_dev(:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx64_1 = .true.
         else
            omp_target_is_present_f_cmplx64_1 = .false.
         endif

       endfunction omp_target_is_present_f_cmplx64_1

      function omp_target_is_present_f_cmplx64_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_2
         complex(R8P), target, intent(in) :: fptr_dev(:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx64_2 = .true.
         else
            omp_target_is_present_f_cmplx64_2 = .false.
         endif

      endfunction omp_target_is_present_f_cmplx64_2

      function omp_target_is_present_f_cmplx64_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_3
         complex(R8P), target, intent(in) :: fptr_dev(:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx64_3 = .true.
         else
            omp_target_is_present_f_cmplx64_3 = .false.
         endif

       endfunction omp_target_is_present_f_cmplx64_3

      function omp_target_is_present_f_cmplx64_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_4
         complex(R8P), target, intent(in) :: fptr_dev(:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx64_4 = .true.
         else
            omp_target_is_present_f_cmplx64_4 = .false.
         endif

      endfunction omp_target_is_present_f_cmplx64_4

      function omp_target_is_present_f_cmplx64_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_5
         complex(R8P), target, intent(in) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx64_5 = .true.
         else
            omp_target_is_present_f_cmplx64_5 = .false.
         endif

      endfunction omp_target_is_present_f_cmplx64_5

      function omp_target_is_present_f_cmplx64_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_6
         complex(R8P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx64_6 = .true.
         else
            omp_target_is_present_f_cmplx64_6 = .false.
         endif

      endfunction omp_target_is_present_f_cmplx64_6

      function omp_target_is_present_f_cmplx64_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_7
         complex(R8P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx64_7 = .true.
         else
            omp_target_is_present_f_cmplx64_7 = .false.
         endif

      endfunction omp_target_is_present_f_cmplx64_7

#if defined _real128
      function omp_target_is_present_f_cmplx128_1(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_1
         complex(R16P), target, intent(in) :: fptr_dev(:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx128_1 = .true.
         else
            omp_target_is_present_f_cmplx128_1 = .false.
         endif

       endfunction omp_target_is_present_f_cmplx128_1

      function omp_target_is_present_f_cmplx128_2(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_2
         complex(R16P), target, intent(in) :: fptr_dev(:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx128_2 = .true.
         else
            omp_target_is_present_f_cmplx128_2 = .false.
         endif

      endfunction omp_target_is_present_f_cmplx128_2

      function omp_target_is_present_f_cmplx128_3(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_3
         complex(R16P), target, intent(in) :: fptr_dev(:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx128_3 = .true.
         else
            omp_target_is_present_f_cmplx128_3 = .false.
         endif

       endfunction omp_target_is_present_f_cmplx128_3

      function omp_target_is_present_f_cmplx128_4(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_4
         complex(R16P), target, intent(in) :: fptr_dev(:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx128_4 = .true.
         else
            omp_target_is_present_f_cmplx128_4 = .false.
         endif

      endfunction omp_target_is_present_f_cmplx128_4

      function omp_target_is_present_f_cmplx128_5(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_5
         complex(R16P), target, intent(in) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx128_5 = .true.
         else
            omp_target_is_present_f_cmplx128_5 = .false.
         endif

      endfunction omp_target_is_present_f_cmplx128_5

      function omp_target_is_present_f_cmplx128_6(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_6
         complex(R16P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx128_6 = .true.
         else
            omp_target_is_present_f_cmplx128_6 = .false.
         endif

      endfunction omp_target_is_present_f_cmplx128_6

      function omp_target_is_present_f_cmplx128_7(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_7
         complex(R16P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/is_present.i90"

         if (int(omp_target_is_present_c(cptr_dev, omp_device), 1_I4P) /= 0_I4P) then
            omp_target_is_present_f_cmplx128_7 = .true.
         else
            omp_target_is_present_f_cmplx128_7 = .false.
         endif

      endfunction omp_target_is_present_f_cmplx128_7
#endif

      ! OpenMP Get Mapped Pointer Complex Routines
      function omp_get_mapped_ptr_f_cmplx32_1(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx32_1
         complex(R4P), target, intent(inout) :: fptr_dev(:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx32_1 = .true.
         else
            omp_get_mapped_ptr_f_cmplx32_1 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx32_1

      function omp_get_mapped_ptr_f_cmplx32_2(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx32_2
         complex(R4P), target, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx32_2 = .true.
         else
            omp_get_mapped_ptr_f_cmplx32_2 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx32_2

      function omp_get_mapped_ptr_f_cmplx32_3(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx32_3
         complex(R4P), target, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx32_3 = .true.
         else
            omp_get_mapped_ptr_f_cmplx32_3 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx32_3

      function omp_get_mapped_ptr_f_cmplx32_4(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx32_4
         complex(R4P), target, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx32_4 = .true.
         else
            omp_get_mapped_ptr_f_cmplx32_4 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx32_4

      function omp_get_mapped_ptr_f_cmplx32_5(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx32_5
         complex(R4P), target, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx32_5 = .true.
         else
            omp_get_mapped_ptr_f_cmplx32_5 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx32_5

      function omp_get_mapped_ptr_f_cmplx32_6(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx32_6
         complex(R4P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx32_6 = .true.
         else
            omp_get_mapped_ptr_f_cmplx32_6 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx32_6

      function omp_get_mapped_ptr_f_cmplx32_7(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx32_7
         complex(R4P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx32_7 = .true.
         else
            omp_get_mapped_ptr_f_cmplx32_7 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx32_7

      function omp_get_mapped_ptr_f_cmplx64_1(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx64_1
         complex(R8P), target, intent(inout) :: fptr_dev(:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx64_1 = .true.
         else
            omp_get_mapped_ptr_f_cmplx64_1 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx64_1

      function omp_get_mapped_ptr_f_cmplx64_2(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx64_2
         complex(R8P), target, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx64_2 = .true.
         else
            omp_get_mapped_ptr_f_cmplx64_2 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx64_2

      function omp_get_mapped_ptr_f_cmplx64_3(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx64_3
         complex(R8P), target, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx64_3 = .true.
         else
            omp_get_mapped_ptr_f_cmplx64_3 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx64_3

      function omp_get_mapped_ptr_f_cmplx64_4(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx64_4
         complex(R8P), target, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx64_4 = .true.
         else
            omp_get_mapped_ptr_f_cmplx64_4 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx64_4

      function omp_get_mapped_ptr_f_cmplx64_5(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx64_5
         complex(R8P), target, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx64_5 = .true.
         else
            omp_get_mapped_ptr_f_cmplx64_5 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx64_5

      function omp_get_mapped_ptr_f_cmplx64_6(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx64_6
         complex(R8P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx64_6 = .true.
         else
            omp_get_mapped_ptr_f_cmplx64_6 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx64_6

      function omp_get_mapped_ptr_f_cmplx64_7(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx64_7
         complex(R8P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx64_7 = .true.
         else
            omp_get_mapped_ptr_f_cmplx64_7 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx64_7

#if defined _real128
      function omp_get_mapped_ptr_f_cmplx128_1(fptr_dev, omp_dev)
         implicit none
         logical                              :: omp_get_mapped_ptr_f_cmplx128_1
         complex(R16P), target, intent(inout) :: fptr_dev(:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx128_1 = .true.
         else
            omp_get_mapped_ptr_f_cmplx128_1 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx128_1

      function omp_get_mapped_ptr_f_cmplx128_2(fptr_dev, omp_dev)
         implicit none
         logical                              :: omp_get_mapped_ptr_f_cmplx128_2
         complex(R16P), target, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx128_2 = .true.
         else
            omp_get_mapped_ptr_f_cmplx128_2 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx128_2

      function omp_get_mapped_ptr_f_cmplx128_3(fptr_dev, omp_dev)
         implicit none
         logical                              :: omp_get_mapped_ptr_f_cmplx128_3
         complex(R16P), target, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx128_3 = .true.
         else
            omp_get_mapped_ptr_f_cmplx128_3 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx128_3

      function omp_get_mapped_ptr_f_cmplx128_4(fptr_dev, omp_dev)
         implicit none
         logical                              :: omp_get_mapped_ptr_f_cmplx128_4
         complex(R16P), target, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx128_4 = .true.
         else
            omp_get_mapped_ptr_f_cmplx128_4 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx128_4

      function omp_get_mapped_ptr_f_cmplx128_5(fptr_dev, omp_dev)
         implicit none
         logical                              :: omp_get_mapped_ptr_f_cmplx128_5
         complex(R16P), target, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx128_5 = .true.
         else
            omp_get_mapped_ptr_f_cmplx128_5 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx128_5

      function omp_get_mapped_ptr_f_cmplx128_6(fptr_dev, omp_dev)
         implicit none
         logical                              :: omp_get_mapped_ptr_f_cmplx128_6
         complex(R16P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx128_6 = .true.
         else
            omp_get_mapped_ptr_f_cmplx128_6 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx128_6

      function omp_get_mapped_ptr_f_cmplx128_7(fptr_dev, omp_dev)
         implicit none
         logical                              :: omp_get_mapped_ptr_f_cmplx128_7
         complex(R16P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/get_mapped_ptr.i90"

         if (c_associated(cptr_dev)) then
            omp_get_mapped_ptr_f_cmplx128_7 = .true.
         else
            omp_get_mapped_ptr_f_cmplx128_7 = .false.
         endif

      endfunction omp_get_mapped_ptr_f_cmplx128_7
#endif

      ! OpenMP Target Free Complex Routines
      subroutine omp_target_free_f_cmplx32_1(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx32_1

      subroutine omp_target_free_f_cmplx32_2(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx32_2

      subroutine omp_target_free_f_cmplx32_3(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx32_3

      subroutine omp_target_free_f_cmplx32_4(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx32_4

      subroutine omp_target_free_f_cmplx32_5(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx32_5

      subroutine omp_target_free_f_cmplx32_6(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx32_6

      subroutine omp_target_free_f_cmplx32_7(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx32_7

      subroutine omp_target_free_f_cmplx64_1(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx64_1

      subroutine omp_target_free_f_cmplx64_2(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx64_2

      subroutine omp_target_free_f_cmplx64_3(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx64_3

      subroutine omp_target_free_f_cmplx64_4(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx64_4

      subroutine omp_target_free_f_cmplx64_5(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx64_5

      subroutine omp_target_free_f_cmplx64_6(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx64_6

      subroutine omp_target_free_f_cmplx64_7(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx64_7

#if defined _real128
      subroutine omp_target_free_f_cmplx128_1(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx128_1

      subroutine omp_target_free_f_cmplx128_2(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx128_2

      subroutine omp_target_free_f_cmplx128_3(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx128_3

      subroutine omp_target_free_f_cmplx128_4(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx128_4

      subroutine omp_target_free_f_cmplx128_5(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx128_5

      subroutine omp_target_free_f_cmplx128_6(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx128_6

      subroutine omp_target_free_f_cmplx128_7(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)

         include "src/lib/include/free_fptr_dev.i90"

      endsubroutine omp_target_free_f_cmplx128_7
#endif

      ! OpenMP Target Alloc Complex Routines
      subroutine omp_target_alloc_f_cmplx32_1(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)             :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * dimensions, c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_cmplx32_1

      subroutine omp_target_alloc_f_cmplx32_2(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)          :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_cmplx32_2

      subroutine omp_target_alloc_f_cmplx32_3(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)          :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_cmplx32_3

      subroutine omp_target_alloc_f_cmplx32_4(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_cmplx32_4

      subroutine omp_target_alloc_f_cmplx32_5(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_cmplx32_5

      subroutine omp_target_alloc_f_cmplx32_6(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_cmplx32_6

      subroutine omp_target_alloc_f_cmplx32_7(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)          :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R4P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_cmplx32_7

      subroutine omp_target_alloc_f_cmplx64_1(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)             :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * dimensions, c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_cmplx64_1

      subroutine omp_target_alloc_f_cmplx64_2(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)             :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_cmplx64_2

      subroutine omp_target_alloc_f_cmplx64_3(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)             :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_cmplx64_3

      subroutine omp_target_alloc_f_cmplx64_4(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)             :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_cmplx64_4

      subroutine omp_target_alloc_f_cmplx64_5(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)             :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_cmplx64_5

      subroutine omp_target_alloc_f_cmplx64_6(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)             :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_cmplx64_6

      subroutine omp_target_alloc_f_cmplx64_7(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)             :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R8P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_cmplx64_7

#if defined _real128
      subroutine omp_target_alloc_f_cmplx128_1(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)              :: dimensions

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * dimensions, c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions])

      endsubroutine omp_target_alloc_f_cmplx128_1

      subroutine omp_target_alloc_f_cmplx128_2(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)              :: dimensions(2)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2)])

      endsubroutine omp_target_alloc_f_cmplx128_2

      subroutine omp_target_alloc_f_cmplx128_3(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)              :: dimensions(3)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3)])

      endsubroutine omp_target_alloc_f_cmplx128_3

      subroutine omp_target_alloc_f_cmplx128_4(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)              :: dimensions(4)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4)])

      endsubroutine omp_target_alloc_f_cmplx128_4

      subroutine omp_target_alloc_f_cmplx128_5(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)              :: dimensions(5)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5)])

      endsubroutine omp_target_alloc_f_cmplx128_5

      subroutine omp_target_alloc_f_cmplx128_6(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)              :: dimensions(6)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6)])

      endsubroutine omp_target_alloc_f_cmplx128_6

      subroutine omp_target_alloc_f_cmplx128_7(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)              :: dimensions(7)

         include "src/lib/include/alloc_fptr_dev.i90"

         cptr_dev = omp_target_alloc_c(int(2_I8P * product(dimensions), c_size_t) * byte_size(1._R16P), omp_device)

         if (c_associated(cptr_dev)) call c_f_pointer(cptr_dev, fptr_dev, [dimensions(1), dimensions(2), dimensions(3), dimensions(4), &
                                               dimensions(5), dimensions(6), dimensions(7)])

      endsubroutine omp_target_alloc_f_cmplx128_7
#endif

      ! OpenMP Target Memcpy Complex Routines
      subroutine omp_target_memcpy_f_cmplx32_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx32_1

      subroutine omp_target_memcpy_f_cmplx32_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx32_2

      subroutine omp_target_memcpy_f_cmplx32_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx32_3

      subroutine omp_target_memcpy_f_cmplx32_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx32_4

      subroutine omp_target_memcpy_f_cmplx32_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx32_5

      subroutine omp_target_memcpy_f_cmplx32_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx32_6

      subroutine omp_target_memcpy_f_cmplx32_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx32_7

      subroutine omp_target_memcpy_f_cmplx64_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx64_1

      subroutine omp_target_memcpy_f_cmplx64_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx64_2

      subroutine omp_target_memcpy_f_cmplx64_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx64_3

      subroutine omp_target_memcpy_f_cmplx64_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx64_4

      subroutine omp_target_memcpy_f_cmplx64_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx64_5

      subroutine omp_target_memcpy_f_cmplx64_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx64_6

      subroutine omp_target_memcpy_f_cmplx64_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx64_7

#if defined _real128
      subroutine omp_target_memcpy_f_cmplx128_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx128_1

      subroutine omp_target_memcpy_f_cmplx128_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx128_2

      subroutine omp_target_memcpy_f_cmplx128_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx128_3

      subroutine omp_target_memcpy_f_cmplx128_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx128_4

      subroutine omp_target_memcpy_f_cmplx128_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx128_5

      subroutine omp_target_memcpy_f_cmplx128_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx128_6

      subroutine omp_target_memcpy_f_cmplx128_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)

         include "src/lib/include/memcpy.i90"

         total_dim = int(2_I8P * n_elements * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_c(cptr_dst, cptr_src, total_dim, omp_dst_offset, omp_src_offset, &
            omp_dst_device, omp_src_device), I4P)

      endsubroutine omp_target_memcpy_f_cmplx128_7
#endif

     ! OpenMP Target Memcpy Rect Complex Routines
      subroutine omp_target_memcpy_rect_f_cmplx32_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(2_I8P * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_cmplx32_2

      subroutine omp_target_memcpy_rect_f_cmplx32_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(2_I8P * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_cmplx32_3

      subroutine omp_target_memcpy_rect_f_cmplx32_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(2_I8P * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_cmplx32_4

      subroutine omp_target_memcpy_rect_f_cmplx32_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(2_I8P * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_cmplx32_5

      subroutine omp_target_memcpy_rect_f_cmplx32_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(2_I8P * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_cmplx32_6

      subroutine omp_target_memcpy_rect_f_cmplx32_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(2_I8P * byte_size(1._R4P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_cmplx32_7

      subroutine omp_target_memcpy_rect_f_cmplx64_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(2_I8P * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_cmplx64_2

      subroutine omp_target_memcpy_rect_f_cmplx64_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(2_I8P * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_cmplx64_3

      subroutine omp_target_memcpy_rect_f_cmplx64_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(2_I8P * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_cmplx64_4

      subroutine omp_target_memcpy_rect_f_cmplx64_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(2_I8P * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_cmplx64_5

      subroutine omp_target_memcpy_rect_f_cmplx64_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(2_I8P * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_cmplx64_6

      subroutine omp_target_memcpy_rect_f_cmplx64_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(2_I8P * byte_size(1._R8P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_cmplx64_7

#if defined _real128
      subroutine omp_target_memcpy_rect_f_cmplx128_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P),  parameter                       :: fptr_dims = 2_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(2_I8P * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,        &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_cmplx128_2

      subroutine omp_target_memcpy_rect_f_cmplx128_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P),  parameter                       :: fptr_dims = 3_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(2_I8P * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_cmplx128_3

      subroutine omp_target_memcpy_rect_f_cmplx128_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P),  parameter                       :: fptr_dims = 4_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(2_I8P * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_cmplx128_4

      subroutine omp_target_memcpy_rect_f_cmplx128_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P),  parameter                       :: fptr_dims = 5_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(2_I8P * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_cmplx128_5

      subroutine omp_target_memcpy_rect_f_cmplx128_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P),  parameter                       :: fptr_dims = 6_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(2_I8P * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_cmplx128_6

      subroutine omp_target_memcpy_rect_f_cmplx128_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P),  parameter                       :: fptr_dims = 7_I4P

         include "src/lib/include/memcpy_rect.i90"

         elem_dim = int(2_I8P * byte_size(1._R16P), c_size_t)

         ierr = int(omp_target_memcpy_rect_c(cptr_dst, cptr_src, elem_dim, fptr_rank, volume_dims, &
            omp_dst_offsets, omp_src_offsets, cptr_dst_dims, cptr_src_dims, omp_dst_device,     &
            omp_src_device), I4P)

      endsubroutine omp_target_memcpy_rect_f_cmplx128_7
#endif

endmodule dmr
