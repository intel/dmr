!* ========================================================================== *
!*                                                                            *
!* Copyright (C) 2020 Intel Corporation                                       *
!* This file is part of the FALCO library.                                    *
!*                                                                            *
!* For information on the license, see the LICENSE file.                      *
!* Further information: https://github.com/giacrossi/FALCO/                   *
!* SPDX-License-Identifier: BSD-3-Clause                                      *
!*                                                                            *
!* ========================================================================== *
!* Giacomo Rossi (Intel Corporation)                                          *
!* ========================================================================== *

module falco_environment

   implicit none

   public

   save

   integer, parameter :: I8P  = selected_int_kind(18)       !< Range \([-2^{63},+2^{63} - 1]\), 19 digits plus sign; 64 bits.
   integer, parameter :: I4P  = selected_int_kind(9)        !< Range \([-2^{31},+2^{31} - 1]\), 10 digits plus sign; 32 bits.
   integer, parameter :: I2P  = selected_int_kind(4)        !< Range \([-2^{15},+2^{15} - 1]\), 5  digits plus sign; 16 bits.
   integer, parameter :: I1P  = selected_int_kind(2)        !< Range \([-2^{7} ,+2^{7}  - 1]\), 3  digits plus sign; 8  bits.

   integer, parameter :: R8P  = selected_real_kind(15,307)  !< 15 digits, range \([10^{-307} , 10^{+307}  - 1]\); 64 bits.
   integer, parameter :: R4P  = selected_real_kind(6,37)    !< 6  digits, range \([10^{-37}  , 10^{+37}   - 1]\); 32 bits.
#if defined _real128
   integer, parameter :: R16P = selected_real_kind(33,4931) !< 33 digits, range \([10^{-4931}, 10^{+4931} - 1]\); 128 bits.
#endif

endmodule falco_environment
