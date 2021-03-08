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

submodule (dmr) dmr_target_memcpy_scalar
   use, intrinsic :: iso_c_binding
   use omp_lib
   use dmr_environment
   use dmr_c_functions

   implicit none

   contains
      ! OpenMP Target Memcpy Integer Routines
#if defined _OpenMP_5_1
      module subroutine omp_target_memcpy_f_int8_scalar(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         integer(I1P), intent(out) :: sc_dst
         integer(I1P), intent(in)  :: sc_src
         integer(I4P), intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(to:sc_src) has_device_addr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(from:sc_dst) has_device_addr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_int8_scalar

      module subroutine omp_target_memcpy_f_int16_scalar(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         integer(I2P), intent(out) :: sc_dst
         integer(I2P), intent(in)  :: sc_src
         integer(I4P), intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(to:sc_src) has_device_addr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(from:sc_dst) has_device_addr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_int16_scalar

      module subroutine omp_target_memcpy_f_int32_scalar(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         integer(I4P), intent(out) :: sc_dst
         integer(I4P), intent(in)  :: sc_src
         integer(I4P), intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(to:sc_src) has_device_addr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(from:sc_dst) has_device_addr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_int32_scalar

      module subroutine omp_target_memcpy_f_int64_scalar(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         integer(I8P), intent(out) :: sc_dst
         integer(I8P), intent(in)  :: sc_src
         integer(I4P), intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(to:sc_src) has_device_addr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(from:sc_dst) has_device_addr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_int64_scalar

      ! OpenMP Target Memcpy Real Routines
      module subroutine omp_target_memcpy_f_real32_scalar(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         real(R4P),    intent(out) :: sc_dst
         real(R4P),    intent(in)  :: sc_src
         integer(I4P), intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(to:sc_src) has_device_addr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(from:sc_dst) has_device_addr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_real32_scalar

      module subroutine omp_target_memcpy_f_real64_scalar(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         real(R8P),    intent(out) :: sc_dst
         real(R8P),    intent(in)  :: sc_src
         integer(I4P), intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(to:sc_src) has_device_addr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(from:sc_dst) has_device_addr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_real64_scalar

#if defined _real128
      module subroutine omp_target_memcpy_f_real128_scalar(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         real(R16P),   intent(out) :: sc_dst
         real(R16P),   intent(in)  :: sc_src
         integer(I4P), intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(to:sc_src) has_device_addr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(from:sc_dst) has_device_addr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_real128_scalar
#endif

      ! OpenMP Target Memcpy Complex Routines
      module subroutine omp_target_memcpy_f_cmplx32_scalar(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         complex(R4P), intent(out) :: sc_dst
         complex(R4P), intent(in)  :: sc_src
         integer(I4P), intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(to:sc_src) has_device_addr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(from:sc_dst) has_device_addr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_cmplx32_scalar

      module subroutine omp_target_memcpy_f_cmplx64_scalar(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         complex(R8P), intent(out) :: sc_dst
         complex(R8P), intent(in)  :: sc_src
         integer(I4P), intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(to:sc_src) has_device_addr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(from:sc_dst) has_device_addr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_cmplx64_scalar

#if defined _real128
      module subroutine omp_target_memcpy_f_cmplx128_scalar(sc_dst, sc_src, omp_dst_dev, omp_src_dev)
         implicit none
         complex(R16P), intent(out) :: sc_dst
         complex(R16P), intent(in)  :: sc_src
         integer(I4P),  intent(in)  :: omp_dst_dev, omp_src_dev

         if (omp_src_dev==omp_get_initial_device()) then
            !$omp target map(to:sc_src) has_device_addr(sc_dst) device(omp_dst_dev)
            sc_dst = sc_src
            !$omp end target
         else
            !$omp target map(from:sc_dst) has_device_addr(sc_src) device(omp_src_dev)
            sc_dst = sc_src
            !$omp end target
         endif
      endsubroutine omp_target_memcpy_f_cmplx128_scalar
#endif
#endif

endsubmodule dmr_target_memcpy_scalar
