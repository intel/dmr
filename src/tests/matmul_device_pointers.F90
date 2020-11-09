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

module matmul_device_pointers
   use omp_lib
   use falco_environment

   implicit none

   interface matmul_I
      module procedure matmul_int8,   matmul_int16,   matmul_int32,   matmul_int64,   &
                       matmul_int8_2, matmul_int16_2, matmul_int32_2, matmul_int64_2
   endinterface matmul_I

   interface matmul_R
      module procedure &
#if defined _real128
                       matmul_real128, matmul_real128_2, &
#endif
                       matmul_real32,  matmul_real32_2,  matmul_real64,   matmul_real64_2
   endinterface matmul_R

   interface matmul_C
      module procedure &
#if defined _real128
                       matmul_cmplx128, matmul_C16P_2, &
#endif
                       matmul_cmplx32,  matmul_C4P_2,  matmul_cmplx64,   matmul_C8P_2
   endinterface matmul_C

   public matmul_I, matmul_R, matmul_C

   private

   contains

      ! Init rank one device pointers
      subroutine matmul_int8(fptr_dev, i)
         integer(I1P), intent(inout) :: fptr_dev(:)
         integer(I1P)                :: k=2_I1P
         include "src/tests/include/matmul_rank_1.i90"
      endsubroutine matmul_int8

      subroutine matmul_int16(fptr_dev, i)
         integer(I2P), intent(inout) :: fptr_dev(:)
         integer(I2P)                :: k=2_I2P
         include "src/tests/include/matmul_rank_1.i90"
      endsubroutine matmul_int16

      subroutine matmul_int32(fptr_dev, i)
         integer(I4P), intent(inout) :: fptr_dev(:)
         integer(I4P)                :: k=2_I4P
         include "src/tests/include/matmul_rank_1.i90"
      endsubroutine matmul_int32

      subroutine matmul_int64(fptr_dev, i)
         integer(I8P), intent(inout) :: fptr_dev(:)
         integer(I8P)                :: k=2_I8P
         include "src/tests/include/matmul_rank_1.i90"
      endsubroutine matmul_int64

      subroutine matmul_real32(fptr_dev, i)
         real(R4P), intent(inout) :: fptr_dev(:)
         real(R4P)                :: k=2.0_R4P
         include "src/tests/include/matmul_rank_1.i90"
      endsubroutine matmul_real32

      subroutine matmul_real64(fptr_dev, i)
         real(R8P), intent(inout) :: fptr_dev(:)
         real(R8P)                :: k=2.0_R8P
         include "src/tests/include/matmul_rank_1.i90"
      endsubroutine matmul_real64

#if defined _real128
      subroutine matmul_real128(fptr_dev, i)
         real(R16P), intent(inout) :: fptr_dev(:)
         real(R16P)                :: k=2.0_R16P
         include "src/tests/include/matmul_rank_1.i90"
      endsubroutine matmul_real128
#endif

      subroutine matmul_cmplx32(fptr_dev, i)
         complex(R4P), intent(inout) :: fptr_dev(:)
         complex(R4P)                :: k=(2.0_R4P, 0.0_R4P)
         include "src/tests/include/matmul_rank_1.i90"
      endsubroutine matmul_cmplx32

      subroutine matmul_cmplx64(fptr_dev, i)
         complex(R8P), intent(inout) :: fptr_dev(:)
         complex(R8P)                :: k=(2.0_R8P, 0.0_R8P)
         include "src/tests/include/matmul_rank_1.i90"
      endsubroutine matmul_cmplx64

#if defined _real128
      subroutine matmul_cmplx128(fptr_dev, i)
         complex(R16P), intent(inout) :: fptr_dev(:)
         complex(R16P)                :: k=(2.0_R16P, 0.0_R16P)
         include "src/tests/include/matmul_rank_1.i90"
      endsubroutine matmul_cmplx128
#endif

      ! Init rank two device pointers
      subroutine matmul_int8_2(fptr_dev, i)
         integer(I1P), intent(inout) :: fptr_dev(:,:)
         integer(I1P)                :: k=2_I1P
         include "src/tests/include/matmul_rank_2.i90"
      endsubroutine matmul_int8_2

      subroutine matmul_int16_2(fptr_dev, i)
         integer(I2P), intent(inout) :: fptr_dev(:,:)
         integer(I2P)                :: k=2_I2P
         include "src/tests/include/matmul_rank_2.i90"
      endsubroutine matmul_int16_2

      subroutine matmul_int32_2(fptr_dev, i)
         integer(I4P), intent(inout) :: fptr_dev(:,:)
         integer(I4P)                :: k=2_I4P
         include "src/tests/include/matmul_rank_2.i90"
      endsubroutine matmul_int32_2

      subroutine matmul_int64_2(fptr_dev, i)
         integer(I8P), intent(inout) :: fptr_dev(:,:)
         integer(I8P)                :: k=2_I8P
         include "src/tests/include/matmul_rank_2.i90"
      endsubroutine matmul_int64_2

      subroutine matmul_real32_2(fptr_dev, i)
         real(R4P), intent(inout) :: fptr_dev(:,:)
         real(R4P)                :: k=2.0_R4P
         include "src/tests/include/matmul_rank_2.i90"
      endsubroutine matmul_real32_2

      subroutine matmul_real64_2(fptr_dev, i)
         real(R8P), intent(inout) :: fptr_dev(:,:)
         real(R8P)                :: k=2.0_R8P
         include "src/tests/include/matmul_rank_2.i90"
      endsubroutine matmul_real64_2

#if defined _real128
      subroutine matmul_real128_2(fptr_dev, i)
         real(R16P), intent(inout) :: fptr_dev(:,:)
         real(R16P)                :: k=2.0_R16P
         include "src/tests/include/matmul_rank_2.i90"
      endsubroutine matmul_real128_2
#endif

      subroutine matmul_cmplx32_2(fptr_dev, i)
         complex(R4P), intent(inout) :: fptr_dev(:,:)
         complex(R4P)                :: k=(2.0_R4P, 0.0_R4P)
         include "src/tests/include/matmul_rank_2.i90"
      endsubroutine matmul_cmplx32_2

      subroutine matmul_cmplx64_2(fptr_dev, i)
         complex(R8P), intent(inout) :: fptr_dev(:,:)
         complex(R8P)                :: k=(2.0_R8P, 0.0_R8P)
         include "src/tests/include/matmul_rank_2.i90"
      endsubroutine matmul_cmplx64_2

#if defined _real128
      subroutine matmul_cmplx128_2(fptr_dev, i)
         complex(R16P), intent(inout) :: fptr_dev(:,:)
         complex(R16P)                 :: k=(2.0_R16P, 0.0_R16P)
         include "src/tests/include/matmul_rank_2.i90"
      endsubroutine matmul_cmplx128_2
#endif
endmodule matmul_device_pointers
