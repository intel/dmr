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

module init_device_pointers
   use omp_lib
   use PENF, only : I1P, I2P, I4P, I8P, &
#if defined _real128
                    R16P, &
#endif
                    R4P, R8P

   implicit none

   interface init_I
      module procedure init_int8,   init_int16,   init_int32,   init_int64,   &
                       init_int8_2, init_int16_2, init_int32_2, init_int64_2
   endinterface init_I

   interface init_R
      module procedure &
#if defined _real128
                       init_real128, init_real128_2, &
#endif
                       init_real32,  init_real32_2,  init_real64,   init_real64_2
   endinterface init_R

   interface init_C
      module procedure &
#if defined _real128
                       init_cmplx128, init_cmplx128_2, &
#endif
                       init_cmplx32,  init_cmplx32_2,  init_cmplx64,   init_cmplx64_2
   endinterface init_C

   public init_I, init_R, init_C

   private

   contains

      ! Init rank one device pointers
      subroutine init_int8(fptr_dev, i)
         integer(I1P), intent(inout) :: fptr_dev(:)
         integer(I1P), parameter     :: const = 1_I1P
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_int8

      subroutine init_int16(fptr_dev, i)
         integer(I2P), intent(inout) :: fptr_dev(:)
         integer(I2P), parameter     :: const = 1_I2P
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_int16

      subroutine init_int32(fptr_dev, i)
         integer(I4P), intent(inout) :: fptr_dev(:)
         integer(I4P), parameter     :: const = 1_I4P
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_int32

      subroutine init_int64(fptr_dev, i)
         integer(I8P), intent(inout) :: fptr_dev(:)
         integer(I8P), parameter     :: const = 1_I8P
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_int64

      subroutine init_real32(fptr_dev, i)
         real(R4P), intent(inout) :: fptr_dev(:)
         real(R4P), parameter     :: const = 1.0_R4P
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_real32

      subroutine init_real64(fptr_dev, i)
         real(R8P), intent(inout) :: fptr_dev(:)
         real(R8P), parameter     :: const = 1.0_R8P
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_real64

#if defined _real128
      subroutine init_real128(fptr_dev, i)
         real(R16P), intent(inout) :: fptr_dev(:)
         real(R16P), parameter     :: const = 1.0_R16P
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_real128
#endif

      subroutine init_cmplx32(fptr_dev, i)
         complex(R4P), intent(inout) :: fptr_dev(:)
         complex(R4P), parameter     :: const = (1.0_R4P, 0.0_R4P)
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_cmplx32

      subroutine init_cmplx64(fptr_dev, i)
         complex(R8P), intent(inout) :: fptr_dev(:)
         complex(R8P), parameter     :: const = (1.0_R8P, 0.0_R8P)
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_cmplx64

#if defined _real128
      subroutine init_cmplx128(fptr_dev, i)
         complex(R16P), intent(inout) :: fptr_dev(:)
         complex(R16P), parameter     :: const = (1.0_R16P, 0.0_R16P)
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_cmplx128
#endif

      ! Init rank two device pointers
      subroutine init_int8_2(fptr_dev, i)
         integer(I1P), intent(inout) :: fptr_dev(:,:)
         integer(I1P), parameter     :: const = 1_I1P
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_int8_2

      subroutine init_int16_2(fptr_dev, i)
         integer(I2P), intent(inout) :: fptr_dev(:,:)
         integer(I2P), parameter     :: const = 1_I2P
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_int16_2

      subroutine init_int32_2(fptr_dev, i)
         integer(I4P), intent(inout) :: fptr_dev(:,:)
         integer(I4P), parameter     :: const = 1_I4P
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_int32_2

      subroutine init_int64_2(fptr_dev, i)
         integer(I8P), intent(inout) :: fptr_dev(:,:)
         integer(I8P), parameter     :: const = 1_I8P
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_int64_2

      subroutine init_real32_2(fptr_dev, i)
         real(R4P), intent(inout) :: fptr_dev(:,:)
         real(R4P), parameter     :: const = 1.0_R4P
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_real32_2

      subroutine init_real64_2(fptr_dev, i)
         real(R8P), intent(inout) :: fptr_dev(:,:)
         real(R8P), parameter     :: const = 1.0_R8P
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_real64_2

#if defined _real128
      subroutine init_real128_2(fptr_dev, i)
         real(R16P), intent(inout) :: fptr_dev(:,:)
         real(R16P), parameter     :: const = 1.0_R16P
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_real128_2
#endif

      subroutine init_cmplx32_2(fptr_dev, i)
         complex(R4P), intent(inout) :: fptr_dev(:,:)
         complex(R4P), parameter     :: const = (1.0_R4P, 0.0_R4P)
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_cmplx32_2

      subroutine init_cmplx64_2(fptr_dev, i)
         complex(R8P), intent(inout) :: fptr_dev(:,:)
         complex(R8P), parameter     :: const = (1.0_R8P, 0.0_R8P)
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_cmplx64_2

#if defined _real128
      subroutine init_cmplx128_2(fptr_dev, i)
         complex(R16P), intent(inout) :: fptr_dev(:,:)
         complex(R16P), parameter     :: const = (1.0_R16P, 0.0_R16P)
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_cmplx128_2
#endif
endmodule init_device_pointers
