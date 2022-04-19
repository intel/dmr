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

module dmr_environment

   implicit none

   private

   save

   public :: byte_size, &
         R4P, &
         R8P, &
#if defined _real128
         R16P, &
#endif
         I1P, &
         I2P, &
         I4P, &
         I8P

   integer, parameter :: I1P  = selected_int_kind(2)        !< Range \([-2^{7} ,+2^{7}  - 1]\), 3  digits plus sign; 8  bits.
   integer, parameter :: I2P  = selected_int_kind(4)        !< Range \([-2^{15},+2^{15} - 1]\), 5  digits plus sign; 16 bits.
   integer, parameter :: I4P  = selected_int_kind(9)        !< Range \([-2^{31},+2^{31} - 1]\), 10 digits plus sign; 32 bits.
   integer, parameter :: I8P  = selected_int_kind(18)       !< Range \([-2^{63},+2^{63} - 1]\), 19 digits plus sign; 64 bits.

   integer, parameter :: R4P  = selected_real_kind(6,37)    !< 6  digits, range \([10^{-37}  , 10^{+37}   - 1]\); 32 bits.
   integer, parameter :: R8P  = selected_real_kind(15,307)  !< 15 digits, range \([10^{-307} , 10^{+307}  - 1]\); 64 bits.
#if defined _real128
   integer, parameter :: R16P = selected_real_kind(33,4931) !< 33 digits, range \([10^{-4931}, 10^{+4931} - 1]\); 128 bits.
#endif

   interface byte_size
      module procedure &
         byte_size_real32, &
         byte_size_real64, &
#if defined _real128
         byte_size_real128, &
#endif
         byte_size_int8, &
         byte_size_int16, &
         byte_size_int32, &
         byte_size_int64
   endinterface

   contains
      elemental function byte_size_int8(x) result(bytes)
         integer(I1P), intent(in) :: x
         integer(I1P)             :: bytes

         bytes = storage_size(x) / 8_I1P
      endfunction byte_size_int8
!
      elemental function byte_size_int16(x) result(bytes)
         integer(I2P), intent(in) :: x
         integer(I1P)             :: bytes

         bytes = storage_size(x) / 8_I1P
      endfunction byte_size_int16
!
      elemental function byte_size_int32(x) result(bytes)
         integer(I4P), intent(in) :: x
         integer(I1P)             :: bytes

         bytes = storage_size(x) / 8_I1P
      endfunction byte_size_int32
!
      elemental function byte_size_int64(x) result(bytes)
         integer(I8P), intent(in) :: x
         integer(I1P)             :: bytes

         bytes = storage_size(x) / 8_I1P
      endfunction byte_size_int64
!
!
      elemental function byte_size_real32(x) result(bytes)
         real(R4P), intent(in) :: x
         integer(I1P)             :: bytes

         bytes = storage_size(x) / 8_I1P
      endfunction byte_size_real32
!
      elemental function byte_size_real64(x) result(bytes)
         real(R8P), intent(in) :: x
         integer(I1P)             :: bytes

         bytes = storage_size(x) / 8_I1P
      endfunction byte_size_real64
!
#if defined _real128
      elemental function byte_size_real128(x) result(bytes)
         real(R16P), intent(in) :: x
         integer(I1P)             :: bytes

         bytes = storage_size(x) / 8_I1P
      endfunction byte_size_real128
#endif
!
!

endmodule dmr_environment