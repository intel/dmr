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
                       omp_target_alloc_f_int8_1,     omp_target_alloc_f_int8_2,     omp_target_alloc_f_int8_3,     &
                       omp_target_alloc_f_int8_4,     omp_target_alloc_f_int8_5,     omp_target_alloc_f_int8_6,     &
                       omp_target_alloc_f_int8_7,     &
                       omp_target_alloc_f_int16_1,    omp_target_alloc_f_int16_2,    omp_target_alloc_f_int16_3,    &
                       omp_target_alloc_f_int16_4,    omp_target_alloc_f_int16_5,    omp_target_alloc_f_int16_6,    &
                       omp_target_alloc_f_int16_7,    &
                       omp_target_alloc_f_int32_1,    omp_target_alloc_f_int32_2,    omp_target_alloc_f_int32_3,    &
                       omp_target_alloc_f_int32_4,    omp_target_alloc_f_int32_5,    omp_target_alloc_f_int32_6,    &
                       omp_target_alloc_f_int32_7,    &
                       omp_target_alloc_f_int64_1,    omp_target_alloc_f_int64_2,    omp_target_alloc_f_int64_3,    &
                       omp_target_alloc_f_int64_4,    omp_target_alloc_f_int64_5,    omp_target_alloc_f_int64_6,    &
                       omp_target_alloc_f_int64_7,    &
#if defined _real128
                       omp_target_alloc_f_real128_1,  omp_target_alloc_f_real128_2,  omp_target_alloc_f_real128_3,  &
                       omp_target_alloc_f_real128_4,  omp_target_alloc_f_real128_5,  omp_target_alloc_f_real128_6,  &
                       omp_target_alloc_f_real128_7,  &
#endif
                       omp_target_alloc_f_real32_1,   omp_target_alloc_f_real32_2,   omp_target_alloc_f_real32_3,   &
                       omp_target_alloc_f_real32_4,   omp_target_alloc_f_real32_5,   omp_target_alloc_f_real32_6,   &
                       omp_target_alloc_f_real32_7,   &
                       omp_target_alloc_f_real64_1,   omp_target_alloc_f_real64_2,   omp_target_alloc_f_real64_3,   &
                       omp_target_alloc_f_real64_4,   omp_target_alloc_f_real64_5,   omp_target_alloc_f_real64_6,   &
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

   interface dmr_correctly_mapped
      module procedure &
                       dmr_correctly_mapped_int8_1,     dmr_correctly_mapped_int8_2,     dmr_correctly_mapped_int8_3,     &
                       dmr_correctly_mapped_int8_4,     dmr_correctly_mapped_int8_5,     dmr_correctly_mapped_int8_6,     &
                       dmr_correctly_mapped_int8_7,     &
                       dmr_correctly_mapped_int16_1,    dmr_correctly_mapped_int16_2,    dmr_correctly_mapped_int16_3,    &
                       dmr_correctly_mapped_int16_4,    dmr_correctly_mapped_int16_5,    dmr_correctly_mapped_int16_6,    &
                       dmr_correctly_mapped_int16_7,    &
                       dmr_correctly_mapped_int32_1,    dmr_correctly_mapped_int32_2,    dmr_correctly_mapped_int32_3,    &
                       dmr_correctly_mapped_int32_4,    dmr_correctly_mapped_int32_5,    dmr_correctly_mapped_int32_6,    &
                       dmr_correctly_mapped_int32_7,    &
                       dmr_correctly_mapped_int64_1,    dmr_correctly_mapped_int64_2,    dmr_correctly_mapped_int64_3,    &
                       dmr_correctly_mapped_int64_4,    dmr_correctly_mapped_int64_5,    dmr_correctly_mapped_int64_6,    &
                       dmr_correctly_mapped_int64_7,    &
#if defined _real128
                       dmr_correctly_mapped_real128_1,  dmr_correctly_mapped_real128_2,  dmr_correctly_mapped_real128_3,  &
                       dmr_correctly_mapped_real128_4,  dmr_correctly_mapped_real128_5,  dmr_correctly_mapped_real128_6,  &
                       dmr_correctly_mapped_real128_7,  &
#endif
                       dmr_correctly_mapped_real32_1,   dmr_correctly_mapped_real32_2,   dmr_correctly_mapped_real32_3,   &
                       dmr_correctly_mapped_real32_4,   dmr_correctly_mapped_real32_5,   dmr_correctly_mapped_real32_6,   &
                       dmr_correctly_mapped_real32_7,   &
                       dmr_correctly_mapped_real64_1,   dmr_correctly_mapped_real64_2,   dmr_correctly_mapped_real64_3,   &
                       dmr_correctly_mapped_real64_4,   dmr_correctly_mapped_real64_5,   dmr_correctly_mapped_real64_6,   &
                       dmr_correctly_mapped_real64_7,   &
#if defined _real128
                       dmr_correctly_mapped_cmplx128_1, dmr_correctly_mapped_cmplx128_2, dmr_correctly_mapped_cmplx128_3, &
                       dmr_correctly_mapped_cmplx128_4, dmr_correctly_mapped_cmplx128_5, dmr_correctly_mapped_cmplx128_6, &
                       dmr_correctly_mapped_cmplx128_7,  &
#endif
                       dmr_correctly_mapped_cmplx32_1,  dmr_correctly_mapped_cmplx32_2,  dmr_correctly_mapped_cmplx32_3,  &
                       dmr_correctly_mapped_cmplx32_4,  dmr_correctly_mapped_cmplx32_5,  dmr_correctly_mapped_cmplx32_6,  &
                       dmr_correctly_mapped_cmplx32_7,  &
                       dmr_correctly_mapped_cmplx64_1,  dmr_correctly_mapped_cmplx64_2,  dmr_correctly_mapped_cmplx64_3,  &
                       dmr_correctly_mapped_cmplx64_4,  dmr_correctly_mapped_cmplx64_5,  dmr_correctly_mapped_cmplx64_6,  &
                       dmr_correctly_mapped_cmplx64_7
   endinterface dmr_correctly_mapped

#if defined _OpenMP_TR9
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
#endif

! OpenMP Target Is Present Routines
   interface
      ! OpenMP Target Is Present Integer Routines
      module function omp_target_is_present_f_int8_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_1
         integer(I1P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_int8_1

      module function omp_target_is_present_f_int8_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_2
         integer(I1P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int8_2

      module function omp_target_is_present_f_int8_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_3
         integer(I1P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_int8_3

      module function omp_target_is_present_f_int8_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_4
         integer(I1P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int8_4

      module function omp_target_is_present_f_int8_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_5
         integer(I1P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int8_5

      module function omp_target_is_present_f_int8_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_6
         integer(I1P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int8_6

      module function omp_target_is_present_f_int8_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int8_7
         integer(I1P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int8_7

      module function omp_target_is_present_f_int16_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_1
         integer(I2P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_int16_1

      module function omp_target_is_present_f_int16_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_2
         integer(I2P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int16_2

      module function omp_target_is_present_f_int16_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_3
         integer(I2P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_int16_3

      module function omp_target_is_present_f_int16_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_4
         integer(I2P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int16_4

      module function omp_target_is_present_f_int16_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_5
         integer(I2P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int16_5

      module function omp_target_is_present_f_int16_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_6
         integer(I2P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int16_6

      module function omp_target_is_present_f_int16_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int16_7
         integer(I2P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int16_7

      module function omp_target_is_present_f_int32_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_1
         integer(I4P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_int32_1

      module function omp_target_is_present_f_int32_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_2
         integer(I4P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int32_2

      module function omp_target_is_present_f_int32_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_3
         integer(I4P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_int32_3

      module function omp_target_is_present_f_int32_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_4
         integer(I4P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int32_4

      module function omp_target_is_present_f_int32_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_5
         integer(I4P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int32_5

      module function omp_target_is_present_f_int32_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_6
         integer(I4P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int32_6

      module function omp_target_is_present_f_int32_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int32_7
         integer(I4P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int32_7

      module function omp_target_is_present_f_int64_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_1
         integer(I8P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_int64_1

      module function omp_target_is_present_f_int64_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_2
         integer(I8P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int64_2

      module function omp_target_is_present_f_int64_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_3
         integer(I8P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_int64_3

      module function omp_target_is_present_f_int64_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_4
         integer(I8P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int64_4

      module function omp_target_is_present_f_int64_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_5
         integer(I8P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int64_5

      module function omp_target_is_present_f_int64_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_6
         integer(I8P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int64_6

      module function omp_target_is_present_f_int64_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_int64_7
         integer(I8P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_int64_7

      ! OpenMP Target Is Present Real Routines
      module function omp_target_is_present_f_real32_1(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_1
         real(R4P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_real32_1

      module function omp_target_is_present_f_real32_2(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_2
         real(R4P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_real32_2

      module function omp_target_is_present_f_real32_3(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_3
         real(R4P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_real32_3

      module function omp_target_is_present_f_real32_4(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_4
         real(R4P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_real32_4

      module function omp_target_is_present_f_real32_5(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_5
         real(R4P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_real32_5

      module function omp_target_is_present_f_real32_6(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_6
         real(R4P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_real32_6

      module function omp_target_is_present_f_real32_7(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real32_7
         real(R4P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_real32_7

      module function omp_target_is_present_f_real64_1(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_1
         real(R8P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_real64_1

      module function omp_target_is_present_f_real64_2(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_2
         real(R8P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_real64_2

      module function omp_target_is_present_f_real64_3(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_3
         real(R8P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_real64_3

      module function omp_target_is_present_f_real64_4(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_4
         real(R8P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_real64_4

      module function omp_target_is_present_f_real64_5(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_5
         real(R8P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_real64_5

      module function omp_target_is_present_f_real64_6(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_6
         real(R8P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_real64_6

      module function omp_target_is_present_f_real64_7(fptr_dev, omp_dev)
         implicit none
         logical                       :: omp_target_is_present_f_real64_7
         real(R8P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_real64_7

#if defined _real128
      module function omp_target_is_present_f_real128_1(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_1
         real(R16P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_real128_1

      module function omp_target_is_present_f_real128_2(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_2
         real(R16P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_real128_2

      module function omp_target_is_present_f_real128_3(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_3
         real(R16P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_real128_3

      module function omp_target_is_present_f_real128_4(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_4
         real(R16P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_real128_4

      module function omp_target_is_present_f_real128_5(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_5
         real(R16P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_real128_5

      module function omp_target_is_present_f_real128_6(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_6
         real(R16P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_real128_6

      module function omp_target_is_present_f_real128_7(fptr_dev, omp_dev)
         implicit none
         logical                        :: omp_target_is_present_f_real128_7
         real(R16P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_real128_7
#endif

      ! OpenMP Target Is Present Complex Routines
      module function omp_target_is_present_f_cmplx32_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_1
         complex(R4P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_cmplx32_1

      module function omp_target_is_present_f_cmplx32_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_2
         complex(R4P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_cmplx32_2

      module function omp_target_is_present_f_cmplx32_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_3
         complex(R4P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_cmplx32_3

      module function omp_target_is_present_f_cmplx32_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_4
         complex(R4P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_cmplx32_4

      module function omp_target_is_present_f_cmplx32_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_5
         complex(R4P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_cmplx32_5

      module function omp_target_is_present_f_cmplx32_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_6
         complex(R4P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_cmplx32_6

      module function omp_target_is_present_f_cmplx32_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx32_7
         complex(R4P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_cmplx32_7

      module function omp_target_is_present_f_cmplx64_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_1
         complex(R8P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_cmplx64_1

      module function omp_target_is_present_f_cmplx64_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_2
         complex(R8P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_cmplx64_2

      module function omp_target_is_present_f_cmplx64_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_3
         complex(R8P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_cmplx64_3

      module function omp_target_is_present_f_cmplx64_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_4
         complex(R8P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_cmplx64_4

      module function omp_target_is_present_f_cmplx64_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_5
         complex(R8P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_cmplx64_5

      module function omp_target_is_present_f_cmplx64_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_6
         complex(R8P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_cmplx64_6

      module function omp_target_is_present_f_cmplx64_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_target_is_present_f_cmplx64_7
         complex(R8P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_cmplx64_7

#if defined _real128
      module function omp_target_is_present_f_cmplx128_1(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_1
         complex(R16P), target, intent(in) :: fptr_dev(:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_cmplx128_1

      module function omp_target_is_present_f_cmplx128_2(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_2
         complex(R16P), target, intent(in) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_cmplx128_2

      module function omp_target_is_present_f_cmplx128_3(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_3
         complex(R16P), target, intent(in) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
       endfunction omp_target_is_present_f_cmplx128_3

      module function omp_target_is_present_f_cmplx128_4(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_4
         complex(R16P), target, intent(in) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_cmplx128_4

      module function omp_target_is_present_f_cmplx128_5(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_5
         complex(R16P), target, intent(in) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_cmplx128_5

      module function omp_target_is_present_f_cmplx128_6(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_6
         complex(R16P), target, intent(in) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_cmplx128_6

      module function omp_target_is_present_f_cmplx128_7(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_target_is_present_f_cmplx128_7
         complex(R16P), target, intent(in) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_is_present.i90"
      endfunction omp_target_is_present_f_cmplx128_7
#endif
   endinterface

! OpenMP Target Alloc Routines
   interface
      ! OpenMP Target Alloc Integer Routines
      module subroutine omp_target_alloc_f_int8_1(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                         :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int8_1

      module subroutine omp_target_alloc_f_int8_2(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                         :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int8_2

      module subroutine omp_target_alloc_f_int8_3(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                         :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int8_3

      module subroutine omp_target_alloc_f_int8_4(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int8_4

      module subroutine omp_target_alloc_f_int8_5(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int8_5

      module subroutine omp_target_alloc_f_int8_6(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int8_6

      module subroutine omp_target_alloc_f_int8_7(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int8_7

      module subroutine omp_target_alloc_f_int16_1(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                         :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int16_1

      module subroutine omp_target_alloc_f_int16_2(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                         :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int16_2

      module subroutine omp_target_alloc_f_int16_3(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                         :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int16_3

      module subroutine omp_target_alloc_f_int16_4(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int16_4

      module subroutine omp_target_alloc_f_int16_5(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int16_5

      module subroutine omp_target_alloc_f_int16_6(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int16_6

      module subroutine omp_target_alloc_f_int16_7(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int16_7

      module subroutine omp_target_alloc_f_int32_1(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                         :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int32_1

      module subroutine omp_target_alloc_f_int32_2(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                         :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int32_2

      module subroutine omp_target_alloc_f_int32_3(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                         :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int32_3

      module subroutine omp_target_alloc_f_int32_4(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int32_4

      module subroutine omp_target_alloc_f_int32_5(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int32_5

      module subroutine omp_target_alloc_f_int32_6(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int32_6

      module subroutine omp_target_alloc_f_int32_7(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int32_7

      module subroutine omp_target_alloc_f_int64_1(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                         :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int64_1

      module subroutine omp_target_alloc_f_int64_2(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                         :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int64_2

      module subroutine omp_target_alloc_f_int64_3(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                         :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int64_3

      module subroutine omp_target_alloc_f_int64_4(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int64_4

      module subroutine omp_target_alloc_f_int64_5(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int64_5

      module subroutine omp_target_alloc_f_int64_6(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int64_6

      module subroutine omp_target_alloc_f_int64_7(fptr_dev, dimensions, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_int64_7

      ! OpenMP Target Alloc Real Routines
      module subroutine omp_target_alloc_f_real32_1(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                      :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real32_1

      module subroutine omp_target_alloc_f_real32_2(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                      :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real32_2

      module subroutine omp_target_alloc_f_real32_3(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                      :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real32_3

      module subroutine omp_target_alloc_f_real32_4(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                      :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real32_4

      module subroutine omp_target_alloc_f_real32_5(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                      :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"

      endsubroutine omp_target_alloc_f_real32_5

      module subroutine omp_target_alloc_f_real32_6(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                      :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real32_6

      module subroutine omp_target_alloc_f_real32_7(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                      :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real32_7

      module subroutine omp_target_alloc_f_real64_1(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                      :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real64_1

      module subroutine omp_target_alloc_f_real64_2(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                      :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real64_2

      module subroutine omp_target_alloc_f_real64_3(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                      :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real64_3

      module subroutine omp_target_alloc_f_real64_4(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                      :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real64_4

      module subroutine omp_target_alloc_f_real64_5(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                      :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real64_5

      module subroutine omp_target_alloc_f_real64_6(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                      :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real64_6

      module subroutine omp_target_alloc_f_real64_7(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                      :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real64_7

#if defined _real128
      module subroutine omp_target_alloc_f_real128_1(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                       :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real128_1

      module subroutine omp_target_alloc_f_real128_2(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                       :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real128_2

      module subroutine omp_target_alloc_f_real128_3(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                       :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real128_3

      module subroutine omp_target_alloc_f_real128_4(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real128_4

      module subroutine omp_target_alloc_f_real128_5(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real128_5

      module subroutine omp_target_alloc_f_real128_6(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real128_6

      module subroutine omp_target_alloc_f_real128_7(fptr_dev, dimensions, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                       :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_real128_7
#endif

      ! OpenMP Target Alloc Complex Routines
      module subroutine omp_target_alloc_f_cmplx32_1(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                         :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx32_1

      module subroutine omp_target_alloc_f_cmplx32_2(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                         :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx32_2

      module subroutine omp_target_alloc_f_cmplx32_3(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                         :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx32_3

      module subroutine omp_target_alloc_f_cmplx32_4(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx32_4

      module subroutine omp_target_alloc_f_cmplx32_5(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx32_5

      module subroutine omp_target_alloc_f_cmplx32_6(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx32_6

      module subroutine omp_target_alloc_f_cmplx32_7(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx32_7

      module subroutine omp_target_alloc_f_cmplx64_1(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                         :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx64_1

      module subroutine omp_target_alloc_f_cmplx64_2(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                         :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx64_2

      module subroutine omp_target_alloc_f_cmplx64_3(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                         :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx64_3

      module subroutine omp_target_alloc_f_cmplx64_4(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx64_4

      module subroutine omp_target_alloc_f_cmplx64_5(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx64_5

      module subroutine omp_target_alloc_f_cmplx64_6(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx64_6

      module subroutine omp_target_alloc_f_cmplx64_7(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                         :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx64_7

#if defined _real128
      module subroutine omp_target_alloc_f_cmplx128_1(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         integer(I8P), intent(in)                          :: dimensions
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx128_1

      module subroutine omp_target_alloc_f_cmplx128_2(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         integer(I8P), intent(in)                          :: dimensions(2)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx128_2

      module subroutine omp_target_alloc_f_cmplx128_3(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         integer(I8P), intent(in)                          :: dimensions(3)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx128_3

      module subroutine omp_target_alloc_f_cmplx128_4(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         integer(I8P), intent(in)                          :: dimensions(4)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx128_4

      module subroutine omp_target_alloc_f_cmplx128_5(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         integer(I8P), intent(in)                          :: dimensions(5)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx128_5

      module subroutine omp_target_alloc_f_cmplx128_6(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         integer(I8P), intent(in)                          :: dimensions(6)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx128_6

      module subroutine omp_target_alloc_f_cmplx128_7(fptr_dev, dimensions, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         integer(I8P), intent(in)                          :: dimensions(7)
         include "src/lib/include/dmr_target_alloc.i90"
      endsubroutine omp_target_alloc_f_cmplx128_7
#endif
   endinterface

! OpenMP Target Free Routines
   interface
      ! OpenMP Target Free Integer Routines
      module subroutine omp_target_free_f_int8_1(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int8_1

      module subroutine omp_target_free_f_int8_2(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int8_2

      module subroutine omp_target_free_f_int8_3(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int8_3

      module subroutine omp_target_free_f_int8_4(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int8_4

      module subroutine omp_target_free_f_int8_5(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int8_5

      module subroutine omp_target_free_f_int8_6(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int8_6

      module subroutine omp_target_free_f_int8_7(fptr_dev, omp_dev)
         implicit none
         integer(I1P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int8_7

      module subroutine omp_target_free_f_int16_1(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int16_1

      module subroutine omp_target_free_f_int16_2(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int16_2

      module subroutine omp_target_free_f_int16_3(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int16_3

      module subroutine omp_target_free_f_int16_4(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int16_4

      module subroutine omp_target_free_f_int16_5(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int16_5

      module subroutine omp_target_free_f_int16_6(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int16_6

      module subroutine omp_target_free_f_int16_7(fptr_dev, omp_dev)
         implicit none
         integer(I2P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int16_7

      module subroutine omp_target_free_f_int32_1(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int32_1

      module subroutine omp_target_free_f_int32_2(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int32_2

      module subroutine omp_target_free_f_int32_3(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int32_3

      module subroutine omp_target_free_f_int32_4(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int32_4

      module subroutine omp_target_free_f_int32_5(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int32_5

      module subroutine omp_target_free_f_int32_6(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int32_6

      module subroutine omp_target_free_f_int32_7(fptr_dev, omp_dev)
         implicit none
         integer(I4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int32_7

      module subroutine omp_target_free_f_int64_1(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int64_1

      module subroutine omp_target_free_f_int64_2(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int64_2

      module subroutine omp_target_free_f_int64_3(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int64_3

      module subroutine omp_target_free_f_int64_4(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int64_4

      module subroutine omp_target_free_f_int64_5(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int64_5

      module subroutine omp_target_free_f_int64_6(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int64_6

      module subroutine omp_target_free_f_int64_7(fptr_dev, omp_dev)
         implicit none
         integer(I8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_int64_7

      ! OpenMP Target Free Real Routines
      module subroutine omp_target_free_f_real32_1(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real32_1

      module subroutine omp_target_free_f_real32_2(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real32_2

      module subroutine omp_target_free_f_real32_3(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real32_3

      module subroutine omp_target_free_f_real32_4(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real32_4

      module subroutine omp_target_free_f_real32_5(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real32_5

      module subroutine omp_target_free_f_real32_6(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real32_6

      module subroutine omp_target_free_f_real32_7(fptr_dev, omp_dev)
         implicit none
         real(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real32_7

      module subroutine omp_target_free_f_real64_1(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real64_1

      module subroutine omp_target_free_f_real64_2(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real64_2

      module subroutine omp_target_free_f_real64_3(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real64_3

      module subroutine omp_target_free_f_real64_4(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real64_4

      module subroutine omp_target_free_f_real64_5(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real64_5

      module subroutine omp_target_free_f_real64_6(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real64_6

      module subroutine omp_target_free_f_real64_7(fptr_dev, omp_dev)
         implicit none
         real(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real64_7

#if defined _real128
      module subroutine omp_target_free_f_real128_1(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real128_1

      module subroutine omp_target_free_f_real128_2(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real128_2

      module subroutine omp_target_free_f_real128_3(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real128_3

      module subroutine omp_target_free_f_real128_4(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real128_4

      module subroutine omp_target_free_f_real128_5(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real128_5

      module subroutine omp_target_free_f_real128_6(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real128_6

      module subroutine omp_target_free_f_real128_7(fptr_dev, omp_dev)
         implicit none
         real(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_real128_7
#endif

      ! OpenMP Target Free Complex Routines
      module subroutine omp_target_free_f_cmplx32_1(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx32_1

      module subroutine omp_target_free_f_cmplx32_2(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx32_2

      module subroutine omp_target_free_f_cmplx32_3(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx32_3

      module subroutine omp_target_free_f_cmplx32_4(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx32_4

      module subroutine omp_target_free_f_cmplx32_5(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx32_5

      module subroutine omp_target_free_f_cmplx32_6(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx32_6

      module subroutine omp_target_free_f_cmplx32_7(fptr_dev, omp_dev)
         implicit none
         complex(R4P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx32_7

      module subroutine omp_target_free_f_cmplx64_1(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx64_1

      module subroutine omp_target_free_f_cmplx64_2(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx64_2

      module subroutine omp_target_free_f_cmplx64_3(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx64_3

      module subroutine omp_target_free_f_cmplx64_4(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx64_4

      module subroutine omp_target_free_f_cmplx64_5(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx64_5

      module subroutine omp_target_free_f_cmplx64_6(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx64_6

      module subroutine omp_target_free_f_cmplx64_7(fptr_dev, omp_dev)
         implicit none
         complex(R8P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx64_7

#if defined _real128
      module subroutine omp_target_free_f_cmplx128_1(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx128_1

      module subroutine omp_target_free_f_cmplx128_2(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx128_2

      module subroutine omp_target_free_f_cmplx128_3(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx128_3

      module subroutine omp_target_free_f_cmplx128_4(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx128_4

      module subroutine omp_target_free_f_cmplx128_5(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx128_5

      module subroutine omp_target_free_f_cmplx128_6(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx128_6

      module subroutine omp_target_free_f_cmplx128_7(fptr_dev, omp_dev)
         implicit none
         complex(R16P), pointer, contiguous, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_free.i90"
      endsubroutine omp_target_free_f_cmplx128_7
#endif
   endinterface

! OpenMP Target Memcpy Routines
   interface
      ! OpenMP Target Memcpy Integer Routines
      module subroutine omp_target_memcpy_f_int8_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int8_1

      module subroutine omp_target_memcpy_f_int8_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int8_2

      module subroutine omp_target_memcpy_f_int8_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int8_3

      module subroutine omp_target_memcpy_f_int8_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int8_4

      module subroutine omp_target_memcpy_f_int8_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int8_5

      module subroutine omp_target_memcpy_f_int8_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int8_6

      module subroutine omp_target_memcpy_f_int8_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int8_7

      module subroutine omp_target_memcpy_f_int16_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int16_1

      module subroutine omp_target_memcpy_f_int16_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int16_2

      module subroutine omp_target_memcpy_f_int16_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int16_3

      module subroutine omp_target_memcpy_f_int16_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int16_4

      module subroutine omp_target_memcpy_f_int16_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int16_5

      module subroutine omp_target_memcpy_f_int16_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int16_6

      module subroutine omp_target_memcpy_f_int16_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int16_7

      module subroutine omp_target_memcpy_f_int32_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int32_1

      module subroutine omp_target_memcpy_f_int32_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int32_2

      module subroutine omp_target_memcpy_f_int32_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int32_3

      module subroutine omp_target_memcpy_f_int32_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int32_4

      module subroutine omp_target_memcpy_f_int32_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int32_5

      module subroutine omp_target_memcpy_f_int32_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int32_6

      module subroutine omp_target_memcpy_f_int32_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int32_7

      module subroutine omp_target_memcpy_f_int64_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int64_1

      module subroutine omp_target_memcpy_f_int64_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int64_2

      module subroutine omp_target_memcpy_f_int64_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int64_3

      module subroutine omp_target_memcpy_f_int64_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int64_4

      module subroutine omp_target_memcpy_f_int64_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int64_5

      module subroutine omp_target_memcpy_f_int64_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int64_6

      module subroutine omp_target_memcpy_f_int64_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_int64_7

      ! OpenMP Target Memcpy Real Routines
      module subroutine omp_target_memcpy_f_real32_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real32_1

      module subroutine omp_target_memcpy_f_real32_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real32_2

      module subroutine omp_target_memcpy_f_real32_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real32_3

      module subroutine omp_target_memcpy_f_real32_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real32_4

      module subroutine omp_target_memcpy_f_real32_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real32_5

      module subroutine omp_target_memcpy_f_real32_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real32_6

      module subroutine omp_target_memcpy_f_real32_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real32_7

      module subroutine omp_target_memcpy_f_real64_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real64_1

      module subroutine omp_target_memcpy_f_real64_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real64_2

      module subroutine omp_target_memcpy_f_real64_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real64_3

      module subroutine omp_target_memcpy_f_real64_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real64_4

      module subroutine omp_target_memcpy_f_real64_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real64_5

      module subroutine omp_target_memcpy_f_real64_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real64_6

      module subroutine omp_target_memcpy_f_real64_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real64_7

#if defined _real128
      module subroutine omp_target_memcpy_f_real128_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real128_1

      module subroutine omp_target_memcpy_f_real128_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real128_2

      module subroutine omp_target_memcpy_f_real128_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real128_3

      module subroutine omp_target_memcpy_f_real128_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real128_4

      module subroutine omp_target_memcpy_f_real128_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real128_5

      module subroutine omp_target_memcpy_f_real128_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real128_6

      module subroutine omp_target_memcpy_f_real128_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_real128_7
#endif

      ! OpenMP Target Memcpy Complex Routines
      module subroutine omp_target_memcpy_f_cmplx32_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx32_1

      module subroutine omp_target_memcpy_f_cmplx32_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx32_2

      module subroutine omp_target_memcpy_f_cmplx32_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx32_3

      module subroutine omp_target_memcpy_f_cmplx32_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx32_4

      module subroutine omp_target_memcpy_f_cmplx32_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx32_5

      module subroutine omp_target_memcpy_f_cmplx32_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx32_6

      module subroutine omp_target_memcpy_f_cmplx32_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx32_7

      module subroutine omp_target_memcpy_f_cmplx64_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx64_1

      module subroutine omp_target_memcpy_f_cmplx64_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx64_2

      module subroutine omp_target_memcpy_f_cmplx64_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx64_3

      module subroutine omp_target_memcpy_f_cmplx64_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx64_4

      module subroutine omp_target_memcpy_f_cmplx64_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx64_5

      module subroutine omp_target_memcpy_f_cmplx64_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx64_6

      module subroutine omp_target_memcpy_f_cmplx64_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx64_7

#if defined _real128
      module subroutine omp_target_memcpy_f_cmplx128_1(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx128_1

      module subroutine omp_target_memcpy_f_cmplx128_2(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx128_2

      module subroutine omp_target_memcpy_f_cmplx128_3(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx128_3

      module subroutine omp_target_memcpy_f_cmplx128_4(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx128_4

      module subroutine omp_target_memcpy_f_cmplx128_5(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx128_5

      module subroutine omp_target_memcpy_f_cmplx128_6(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx128_6

      module subroutine omp_target_memcpy_f_cmplx128_7(fptr_dst, fptr_src, ierr, dst_off, src_off, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_target_memcpy.i90"
      endsubroutine omp_target_memcpy_f_cmplx128_7
#endif
   end interface

! OpenMP Target Memcpy Rect Routines
   interface
     ! OpenMP Target Memcpy Rect Integer Routines
      module subroutine omp_target_memcpy_rect_f_int8_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_int8_2

      module subroutine omp_target_memcpy_rect_f_int8_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int8_3

      module subroutine omp_target_memcpy_rect_f_int8_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int8_4

      module subroutine omp_target_memcpy_rect_f_int8_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int8_5

      module subroutine omp_target_memcpy_rect_f_int8_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int8_6

      module subroutine omp_target_memcpy_rect_f_int8_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I1P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int8_7

      module subroutine omp_target_memcpy_rect_f_int16_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int16_2

      module subroutine omp_target_memcpy_rect_f_int16_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int16_3

      module subroutine omp_target_memcpy_rect_f_int16_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int16_4

      module subroutine omp_target_memcpy_rect_f_int16_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int16_5

      module subroutine omp_target_memcpy_rect_f_int16_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int16_6

      module subroutine omp_target_memcpy_rect_f_int16_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I2P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int16_7

      module subroutine omp_target_memcpy_rect_f_int32_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int32_2

      module subroutine omp_target_memcpy_rect_f_int32_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int32_3

      module subroutine omp_target_memcpy_rect_f_int32_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int32_4

      module subroutine omp_target_memcpy_rect_f_int32_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int32_5

      module subroutine omp_target_memcpy_rect_f_int32_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int32_6

      module subroutine omp_target_memcpy_rect_f_int32_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int32_7

      module subroutine omp_target_memcpy_rect_f_int64_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int64_2

      module subroutine omp_target_memcpy_rect_f_int64_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int64_3

      module subroutine omp_target_memcpy_rect_f_int64_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int64_4

      module subroutine omp_target_memcpy_rect_f_int64_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int64_5

      module subroutine omp_target_memcpy_rect_f_int64_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int64_6

      module subroutine omp_target_memcpy_rect_f_int64_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         integer(I8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_int64_7

     ! OpenMP Target Memcpy Rect Real Routines
      module subroutine omp_target_memcpy_rect_f_real32_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R4P),    contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_real32_2

      module subroutine omp_target_memcpy_rect_f_real32_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R4P),    contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_real32_3

      module subroutine omp_target_memcpy_rect_f_real32_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R4P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_real32_4

      module subroutine omp_target_memcpy_rect_f_real32_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R4P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_real32_5

      module subroutine omp_target_memcpy_rect_f_real32_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R4P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_real32_6

      module subroutine omp_target_memcpy_rect_f_real32_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R4P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_real32_7

      module subroutine omp_target_memcpy_rect_f_real64_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R8P),    contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_real64_2

      module subroutine omp_target_memcpy_rect_f_real64_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R8P),    contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_real64_3

      module subroutine omp_target_memcpy_rect_f_real64_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R8P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_real64_4

      module subroutine omp_target_memcpy_rect_f_real64_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R8P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_real64_5

      module subroutine omp_target_memcpy_rect_f_real64_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R8P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_real64_6

      module subroutine omp_target_memcpy_rect_f_real64_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R8P),    contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_real64_7

#if defined _real128
      module subroutine omp_target_memcpy_rect_f_real128_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   contiguous, target, intent(out) :: fptr_dst(:,:)
         real(R16P),   contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_real128_2

      module subroutine omp_target_memcpy_rect_f_real128_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   contiguous, target, intent(out) :: fptr_dst(:,:,:)
         real(R16P),   contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_real128_3

      module subroutine omp_target_memcpy_rect_f_real128_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         real(R16P),   contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      end subroutine omp_target_memcpy_rect_f_real128_4

      module subroutine omp_target_memcpy_rect_f_real128_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         real(R16P),   contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_real128_5

      module subroutine omp_target_memcpy_rect_f_real128_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         real(R16P),   contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_real128_6

      module subroutine omp_target_memcpy_rect_f_real128_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         real(R16P),   contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_real128_7
#endif

     ! OpenMP Target Memcpy Rect Complex Routines
      module subroutine omp_target_memcpy_rect_f_cmplx32_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_cmplx32_2

      module subroutine omp_target_memcpy_rect_f_cmplx32_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_cmplx32_3

      module subroutine omp_target_memcpy_rect_f_cmplx32_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_cmplx32_4

      module subroutine omp_target_memcpy_rect_f_cmplx32_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_cmplx32_5

      module subroutine omp_target_memcpy_rect_f_cmplx32_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_cmplx32_6

      module subroutine omp_target_memcpy_rect_f_cmplx32_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R4P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_cmplx32_7

      module subroutine omp_target_memcpy_rect_f_cmplx64_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P), parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_cmplx64_2

      module subroutine omp_target_memcpy_rect_f_cmplx64_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_cmplx64_3

      module subroutine omp_target_memcpy_rect_f_cmplx64_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_cmplx64_4

      module subroutine omp_target_memcpy_rect_f_cmplx64_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_cmplx64_5

      module subroutine omp_target_memcpy_rect_f_cmplx64_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_cmplx64_6

      module subroutine omp_target_memcpy_rect_f_cmplx64_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R8P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P), parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_cmplx64_7

#if defined _real128
      module subroutine omp_target_memcpy_rect_f_cmplx128_2(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:)
         integer(I4P),  parameter                       :: fptr_dims = 2_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_cmplx128_2

      module subroutine omp_target_memcpy_rect_f_cmplx128_3(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:)
         integer(I4P),  parameter                       :: fptr_dims = 3_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_cmplx128_3

      module subroutine omp_target_memcpy_rect_f_cmplx128_4(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:)
         integer(I4P),  parameter                       :: fptr_dims = 4_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_cmplx128_4

      module subroutine omp_target_memcpy_rect_f_cmplx128_5(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:)
         integer(I4P),  parameter                       :: fptr_dims = 5_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_cmplx128_5

      module subroutine omp_target_memcpy_rect_f_cmplx128_6(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:)
         integer(I4P),  parameter                       :: fptr_dims = 6_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_cmplx128_6

      module subroutine omp_target_memcpy_rect_f_cmplx128_7(fptr_dst, fptr_src, cpy_dims, ierr, dst_offs, src_offs, &
            omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), contiguous, target, intent(out) :: fptr_dst(:,:,:,:,:,:,:)
         complex(R16P), contiguous, target, intent(in)  :: fptr_src(:,:,:,:,:,:,:)
         integer(I4P),  parameter                       :: fptr_dims = 7_I4P
         include "src/lib/include/dmr_target_memcpy_rect.i90"
      endsubroutine omp_target_memcpy_rect_f_cmplx128_7
#endif
   endinterface

! DMR Correctly Mapped Routines
   interface
     ! DMR Correctly Mapped Integer Routines
      module function dmr_correctly_mapped_int8_1(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int8_1
         integer(I1P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"
       endfunction dmr_correctly_mapped_int8_1

      module function dmr_correctly_mapped_int8_2(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int8_2
         integer(I1P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int8_2

      module function dmr_correctly_mapped_int8_3(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int8_3
         integer(I1P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int8_3

      module function dmr_correctly_mapped_int8_4(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int8_4
         integer(I1P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int8_4

      module function dmr_correctly_mapped_int8_5(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int8_5
         integer(I1P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int8_5

      module function dmr_correctly_mapped_int8_6(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int8_6
         integer(I1P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int8_6

      module function dmr_correctly_mapped_int8_7(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int8_7
         integer(I1P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int8_7

      module function dmr_correctly_mapped_int16_1(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int16_1
         integer(I2P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int16_1

      module function dmr_correctly_mapped_int16_2(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int16_2
         integer(I2P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int16_2

      module function dmr_correctly_mapped_int16_3(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int16_3
         integer(I2P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int16_3

      module function dmr_correctly_mapped_int16_4(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int16_4
         integer(I2P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int16_4

      module function dmr_correctly_mapped_int16_5(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int16_5
         integer(I2P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int16_5

      module function dmr_correctly_mapped_int16_6(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int16_6
         integer(I2P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int16_6

      module function dmr_correctly_mapped_int16_7(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int16_7
         integer(I2P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int16_7

      module function dmr_correctly_mapped_int32_1(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int32_1
         integer(I4P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int32_1

      module function dmr_correctly_mapped_int32_2(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int32_2
         integer(I4P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int32_2

      module function dmr_correctly_mapped_int32_3(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int32_3
         integer(I4P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int32_3

      module function dmr_correctly_mapped_int32_4(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int32_4
         integer(I4P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int32_4

      module function dmr_correctly_mapped_int32_5(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int32_5
         integer(I4P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int32_5

      module function dmr_correctly_mapped_int32_6(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int32_6
         integer(I4P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int32_6

      module function dmr_correctly_mapped_int32_7(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int32_7
         integer(I4P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int32_7

      module function dmr_correctly_mapped_int64_1(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int64_1
         integer(I8P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int64_1

      module function dmr_correctly_mapped_int64_2(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int64_2
         integer(I8P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int64_2

      module function dmr_correctly_mapped_int64_3(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int64_3
         integer(I8P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int64_3

      module function dmr_correctly_mapped_int64_4(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int64_4
         integer(I8P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int64_4

      module function dmr_correctly_mapped_int64_5(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int64_5
         integer(I8P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int64_5

      module function dmr_correctly_mapped_int64_6(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int64_6
         integer(I8P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int64_6

      module function dmr_correctly_mapped_int64_7(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_int64_7
         integer(I8P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_int64_7

      ! OpenMP Check Mapped Real Routines
      module function dmr_correctly_mapped_real32_1(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real32_1
         real(R4P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real32_1

      module function dmr_correctly_mapped_real32_2(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real32_2
         real(R4P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real32_2

      module function dmr_correctly_mapped_real32_3(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real32_3
         real(R4P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real32_3

      module function dmr_correctly_mapped_real32_4(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real32_4
         real(R4P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real32_4

      module function dmr_correctly_mapped_real32_5(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real32_5
         real(R4P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real32_5

      module function dmr_correctly_mapped_real32_6(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real32_6
         real(R4P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real32_6

      module function dmr_correctly_mapped_real32_7(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real32_7
         real(R4P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real32_7

      module function dmr_correctly_mapped_real64_1(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real64_1
         real(R8P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real64_1

      module function dmr_correctly_mapped_real64_2(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real64_2
         real(R8P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real64_2

      module function dmr_correctly_mapped_real64_3(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real64_3
         real(R8P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real64_3

      module function dmr_correctly_mapped_real64_4(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real64_4
         real(R8P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real64_4

      module function dmr_correctly_mapped_real64_5(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real64_5
         real(R8P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real64_5

      module function dmr_correctly_mapped_real64_6(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real64_6
         real(R8P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real64_6

      module function dmr_correctly_mapped_real64_7(array, omp_dev)
         implicit none
         logical                :: dmr_correctly_mapped_real64_7
         real(R8P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real64_7

#if defined _real128
      module function dmr_correctly_mapped_real128_1(array, omp_dev)
         implicit none
         logical                 :: dmr_correctly_mapped_real128_1
         real(R16P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real128_1

      module function dmr_correctly_mapped_real128_2(array, omp_dev)
         implicit none
         logical                 :: dmr_correctly_mapped_real128_2
         real(R16P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real128_2

      module function dmr_correctly_mapped_real128_3(array, omp_dev)
         implicit none
         logical                 :: dmr_correctly_mapped_real128_3
         real(R16P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real128_3

      module function dmr_correctly_mapped_real128_4(array, omp_dev)
         implicit none
         logical                 :: dmr_correctly_mapped_real128_4
         real(R16P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real128_4

      module function dmr_correctly_mapped_real128_5(array, omp_dev)
         implicit none
         logical                 :: dmr_correctly_mapped_real128_5
         real(R16P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real128_5

      module function dmr_correctly_mapped_real128_6(array, omp_dev)
         implicit none
         logical                 :: dmr_correctly_mapped_real128_6
         real(R16P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real128_6

      module function dmr_correctly_mapped_real128_7(array, omp_dev)
         implicit none
         logical                 :: dmr_correctly_mapped_real128_7
         real(R16P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_real128_7
#endif

      ! OpenMP Check Mapped Complex Routines
      module function dmr_correctly_mapped_cmplx32_1(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx32_1
         complex(R4P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx32_1

      module function dmr_correctly_mapped_cmplx32_2(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx32_2
         complex(R4P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx32_2

      module function dmr_correctly_mapped_cmplx32_3(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx32_3
         complex(R4P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx32_3

      module function dmr_correctly_mapped_cmplx32_4(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx32_4
         complex(R4P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx32_4

      module function dmr_correctly_mapped_cmplx32_5(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx32_5
         complex(R4P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx32_5

      module function dmr_correctly_mapped_cmplx32_6(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx32_6
         complex(R4P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx32_6

      module function dmr_correctly_mapped_cmplx32_7(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx32_7
         complex(R4P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx32_7

      module function dmr_correctly_mapped_cmplx64_1(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx64_1
         complex(R8P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx64_1

      module function dmr_correctly_mapped_cmplx64_2(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx64_2
         complex(R8P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx64_2

      module function dmr_correctly_mapped_cmplx64_3(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx64_3
         complex(R8P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx64_3

      module function dmr_correctly_mapped_cmplx64_4(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx64_4
         complex(R8P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx64_4

      module function dmr_correctly_mapped_cmplx64_5(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx64_5
         complex(R8P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx64_5

      module function dmr_correctly_mapped_cmplx64_6(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx64_6
         complex(R8P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx64_6

      module function dmr_correctly_mapped_cmplx64_7(array, omp_dev)
         implicit none
         logical                   :: dmr_correctly_mapped_cmplx64_7
         complex(R8P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx64_7

#if defined _real128
      module function dmr_correctly_mapped_cmplx128_1(array, omp_dev)
         implicit none
         logical                    :: dmr_correctly_mapped_cmplx128_1
         complex(R16P), intent(in)  :: array(:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx128_1

      module function dmr_correctly_mapped_cmplx128_2(array, omp_dev)
         implicit none
         logical                    :: dmr_correctly_mapped_cmplx128_2
         complex(R16P), intent(in)  :: array(:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx128_2

      module function dmr_correctly_mapped_cmplx128_3(array, omp_dev)
         implicit none
         logical                    :: dmr_correctly_mapped_cmplx128_3
         complex(R16P), intent(in)  :: array(:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx128_3

      module function dmr_correctly_mapped_cmplx128_4(array, omp_dev)
         implicit none
         logical                    :: dmr_correctly_mapped_cmplx128_4
         complex(R16P), intent(in)  :: array(:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx128_4

      module function dmr_correctly_mapped_cmplx128_5(array, omp_dev)
         implicit none
         logical                    :: dmr_correctly_mapped_cmplx128_5
         complex(R16P), intent(in)  :: array(:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx128_5

      module function dmr_correctly_mapped_cmplx128_6(array, omp_dev)
         implicit none
         logical                    :: dmr_correctly_mapped_cmplx128_6
         complex(R16P), intent(in)  :: array(:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx128_6

      module function dmr_correctly_mapped_cmplx128_7(array, omp_dev)
         implicit none
         logical                    :: dmr_correctly_mapped_cmplx128_7
         complex(R16P), intent(in)  :: array(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_correctly_mapped.i90"
      endfunction dmr_correctly_mapped_cmplx128_7
#endif
   endinterface

#if defined _OpenMP_TR9
! OpenMP Get Mapped Pointer Routines
   interface
      ! OpenMP Get Mapped Pointer Integer Routines
       module function omp_get_mapped_ptr_f_int8_1(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int8_1
         integer(I1P), target, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int8_1

       module function omp_get_mapped_ptr_f_int8_2(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int8_2
         integer(I1P), target, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int8_2

       module function omp_get_mapped_ptr_f_int8_3(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int8_3
         integer(I1P), target, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int8_3

       module function omp_get_mapped_ptr_f_int8_4(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int8_4
         integer(I1P), target, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int8_4

       module function omp_get_mapped_ptr_f_int8_5(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int8_5
         integer(I1P), target, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int8_5

       module function omp_get_mapped_ptr_f_int8_6(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int8_6
         integer(I1P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int8_6

       module function omp_get_mapped_ptr_f_int8_7(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int8_7
         integer(I1P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int8_7

       module function omp_get_mapped_ptr_f_int16_1(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int16_1
         integer(I2P), target, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int16_1

       module function omp_get_mapped_ptr_f_int16_2(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int16_2
         integer(I2P), target, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int16_2

       module function omp_get_mapped_ptr_f_int16_3(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int16_3
         integer(I2P), target, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int16_3

       module function omp_get_mapped_ptr_f_int16_4(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int16_4
         integer(I2P), target, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int16_4

       module function omp_get_mapped_ptr_f_int16_5(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int16_5
         integer(I2P), target, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int16_5

       module function omp_get_mapped_ptr_f_int16_6(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int16_6
         integer(I2P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int16_6

       module function omp_get_mapped_ptr_f_int16_7(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int16_7
         integer(I2P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int16_7

       module function omp_get_mapped_ptr_f_int32_1(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int32_1
         integer(I4P), target, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int32_1

       module function omp_get_mapped_ptr_f_int32_2(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int32_2
         integer(I4P), target, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int32_2

       module function omp_get_mapped_ptr_f_int32_3(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int32_3
         integer(I4P), target, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int32_3

       module function omp_get_mapped_ptr_f_int32_4(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int32_4
         integer(I4P), target, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int32_4

       module function omp_get_mapped_ptr_f_int32_5(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int32_5
         integer(I4P), target, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int32_5

       module function omp_get_mapped_ptr_f_int32_6(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int32_6
         integer(I4P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int32_6

       module function omp_get_mapped_ptr_f_int32_7(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int32_7
         integer(I4P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int32_7

       module function omp_get_mapped_ptr_f_int64_1(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int64_1
         integer(I8P), target, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int64_1

       module function omp_get_mapped_ptr_f_int64_2(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int64_2
         integer(I8P), target, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int64_2

       module function omp_get_mapped_ptr_f_int64_3(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int64_3
         integer(I8P), target, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int64_3

       module function omp_get_mapped_ptr_f_int64_4(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int64_4
         integer(I8P), target, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int64_4

       module function omp_get_mapped_ptr_f_int64_5(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int64_5
         integer(I8P), target, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int64_5

       module function omp_get_mapped_ptr_f_int64_6(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int64_6
         integer(I8P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int64_6

       module function omp_get_mapped_ptr_f_int64_7(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_int64_7
         integer(I8P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_int64_7

      ! OpenMP Get Mapped Pointer Real Routines
       module function omp_get_mapped_ptr_f_real32_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real32_1
         real(R4P), target, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real32_1

       module function omp_get_mapped_ptr_f_real32_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real32_2
         real(R4P), target, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real32_2

       module function omp_get_mapped_ptr_f_real32_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real32_3
         real(R4P), target, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real32_3

       module function omp_get_mapped_ptr_f_real32_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real32_4
         real(R4P), target, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real32_4

       module function omp_get_mapped_ptr_f_real32_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real32_5
         real(R4P), target, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real32_5

       module function omp_get_mapped_ptr_f_real32_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real32_6
         real(R4P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real32_6

       module function omp_get_mapped_ptr_f_real32_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real32_7
         real(R4P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real32_7

       module function omp_get_mapped_ptr_f_real64_1(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real64_1
         real(R8P), target, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real64_1

       module function omp_get_mapped_ptr_f_real64_2(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real64_2
         real(R8P), target, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real64_2

       module function omp_get_mapped_ptr_f_real64_3(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real64_3
         real(R8P), target, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real64_3

       module function omp_get_mapped_ptr_f_real64_4(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real64_4
         real(R8P), target, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real64_4

       module function omp_get_mapped_ptr_f_real64_5(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real64_5
         real(R8P), target, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real64_5

       module function omp_get_mapped_ptr_f_real64_6(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real64_6
         real(R8P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real64_6

       module function omp_get_mapped_ptr_f_real64_7(fptr_dev, omp_dev)
         implicit none
         logical                          :: omp_get_mapped_ptr_f_real64_7
         real(R8P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real64_7

#if defined _real128
       module function omp_get_mapped_ptr_f_real128_1(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_get_mapped_ptr_f_real128_1
         real(R16P), target, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real128_1

       module function omp_get_mapped_ptr_f_real128_2(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_get_mapped_ptr_f_real128_2
         real(R16P), target, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real128_2

       module function omp_get_mapped_ptr_f_real128_3(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_get_mapped_ptr_f_real128_3
         real(R16P), target, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real128_3

       module function omp_get_mapped_ptr_f_real128_4(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_get_mapped_ptr_f_real128_4
         real(R16P), target, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real128_4

       module function omp_get_mapped_ptr_f_real128_5(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_get_mapped_ptr_f_real128_5
         real(R16P), target, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real128_5

       module function omp_get_mapped_ptr_f_real128_6(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_get_mapped_ptr_f_real128_6
         real(R16P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real128_6

       module function omp_get_mapped_ptr_f_real128_7(fptr_dev, omp_dev)
         implicit none
         logical                           :: omp_get_mapped_ptr_f_real128_7
         real(R16P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_real128_7
#endif

      ! OpenMP Get Mapped Pointer Complex Routines
       module function omp_get_mapped_ptr_f_cmplx32_1(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx32_1
         complex(R4P), target, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx32_1

       module function omp_get_mapped_ptr_f_cmplx32_2(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx32_2
         complex(R4P), target, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx32_2

       module function omp_get_mapped_ptr_f_cmplx32_3(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx32_3
         complex(R4P), target, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx32_3

       module function omp_get_mapped_ptr_f_cmplx32_4(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx32_4
         complex(R4P), target, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx32_4

       module function omp_get_mapped_ptr_f_cmplx32_5(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx32_5
         complex(R4P), target, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx32_5

       module function omp_get_mapped_ptr_f_cmplx32_6(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx32_6
         complex(R4P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx32_6

       module function omp_get_mapped_ptr_f_cmplx32_7(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx32_7
         complex(R4P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx32_7

       module function omp_get_mapped_ptr_f_cmplx64_1(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx64_1
         complex(R8P), target, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx64_1

       module function omp_get_mapped_ptr_f_cmplx64_2(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx64_2
         complex(R8P), target, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx64_2

       module function omp_get_mapped_ptr_f_cmplx64_3(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx64_3
         complex(R8P), target, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx64_3

       module function omp_get_mapped_ptr_f_cmplx64_4(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx64_4
         complex(R8P), target, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx64_4

       module function omp_get_mapped_ptr_f_cmplx64_5(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx64_5
         complex(R8P), target, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx64_5

       module function omp_get_mapped_ptr_f_cmplx64_6(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx64_6
         complex(R8P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx64_6

       module function omp_get_mapped_ptr_f_cmplx64_7(fptr_dev, omp_dev)
         implicit none
         logical                             :: omp_get_mapped_ptr_f_cmplx64_7
         complex(R8P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx64_7

#if defined _real128
       module function omp_get_mapped_ptr_f_cmplx128_1(fptr_dev, omp_dev)
         implicit none
         logical                              :: omp_get_mapped_ptr_f_cmplx128_1
         complex(R16P), target, intent(inout) :: fptr_dev(:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx128_1

       module function omp_get_mapped_ptr_f_cmplx128_2(fptr_dev, omp_dev)
         implicit none
         logical                              :: omp_get_mapped_ptr_f_cmplx128_2
         complex(R16P), target, intent(inout) :: fptr_dev(:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx128_2

       module function omp_get_mapped_ptr_f_cmplx128_3(fptr_dev, omp_dev)
         implicit none
         logical                              :: omp_get_mapped_ptr_f_cmplx128_3
         complex(R16P), target, intent(inout) :: fptr_dev(:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx128_3

       module function omp_get_mapped_ptr_f_cmplx128_4(fptr_dev, omp_dev)
         implicit none
         logical                              :: omp_get_mapped_ptr_f_cmplx128_4
         complex(R16P), target, intent(inout) :: fptr_dev(:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx128_4

       module function omp_get_mapped_ptr_f_cmplx128_5(fptr_dev, omp_dev)
         implicit none
         logical                              :: omp_get_mapped_ptr_f_cmplx128_5
         complex(R16P), target, intent(inout) :: fptr_dev(:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx128_5

       module function omp_get_mapped_ptr_f_cmplx128_6(fptr_dev, omp_dev)
         implicit none
         logical                              :: omp_get_mapped_ptr_f_cmplx128_6
         complex(R16P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx128_6

       module function omp_get_mapped_ptr_f_cmplx128_7(fptr_dev, omp_dev)
         implicit none
         logical                              :: omp_get_mapped_ptr_f_cmplx128_7
         complex(R16P), target, intent(inout) :: fptr_dev(:,:,:,:,:,:,:)
         include "src/lib/include/dmr_get_mapped_ptr.i90"
      endfunction omp_get_mapped_ptr_f_cmplx128_7
#endif
   endinterface
#endif

endmodule dmr
