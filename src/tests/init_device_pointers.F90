module init_device_pointers
   use PENF
   use omp_lib

   implicit none

   interface init_I
      module procedure init_I1P,   init_I2P,   init_I4P,   init_I8P,   &
                       init_I1P_2, init_I2P_2, init_I4P_2, init_I8P_2
   endinterface init_I

   interface init_R
      module procedure &
#if defined _R16P
                       init_R16P, init_R16P_2, &
#endif
                       init_R4P,  init_R4P_2,  init_R8P,   init_R8P_2
   endinterface init_R

   interface init_C
      module procedure &
#if defined _R16P
                       init_C16P, init_C16P_2, &
#endif
                       init_C4P,  init_C4P_2,  init_C8P,   init_C8P_2
   endinterface init_C

   public init_I, init_R, init_C

   private

   contains

      ! Init rank one device pointers
      subroutine init_I1P(fptr_dev, i)
         integer(I1P), intent(inout) :: fptr_dev(:)
         integer(I1P), parameter     :: const = 1_I1P
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_I1P

      subroutine init_I2P(fptr_dev, i)
         integer(I2P), intent(inout) :: fptr_dev(:)
         integer(I2P), parameter     :: const = 1_I2P
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_I2P

      subroutine init_I4P(fptr_dev, i)
         integer(I4P), intent(inout) :: fptr_dev(:)
         integer(I4P), parameter     :: const = 1_I4P
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_I4P

      subroutine init_I8P(fptr_dev, i)
         integer(I8P), intent(inout) :: fptr_dev(:)
         integer(I8P), parameter     :: const = 1_I8P
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_I8P

      subroutine init_R4P(fptr_dev, i)
         real(R4P), intent(inout) :: fptr_dev(:)
         real(R4P), parameter     :: const = 1.0_R4P
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_R4P

      subroutine init_R8P(fptr_dev, i)
         real(R8P), intent(inout) :: fptr_dev(:)
         real(R8P), parameter     :: const = 1.0_R8P
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_R8P

#if defined _R16P
      subroutine init_R16P(fptr_dev, i)
         real(R16P), intent(inout) :: fptr_dev(:)
         real(R16P), parameter     :: const = 1.0_R16P
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_R16P
#endif

      subroutine init_C4P(fptr_dev, i)
         complex(R4P), intent(inout) :: fptr_dev(:)
         complex(R4P), parameter     :: const = (1.0_R4P, 0.0_R4P)
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_C4P

      subroutine init_C8P(fptr_dev, i)
         complex(R8P), intent(inout) :: fptr_dev(:)
         complex(R8P), parameter     :: const = (1.0_R8P, 0.0_R8P)
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_C8P

#if defined _R16P
      subroutine init_C16P(fptr_dev, i)
         complex(R16P), intent(inout) :: fptr_dev(:)
         complex(R16P), parameter     :: const = (1.0_R16P, 0.0_R16P)
         include "src/tests/include/init_rank_1.i90"
      endsubroutine init_C16P
#endif

      ! Init rank two device pointers
      subroutine init_I1P_2(fptr_dev, i)
         integer(I1P), intent(inout) :: fptr_dev(:,:)
         integer(I1P), parameter     :: const = 1_I1P
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_I1P_2

      subroutine init_I2P_2(fptr_dev, i)
         integer(I2P), intent(inout) :: fptr_dev(:,:)
         integer(I2P), parameter     :: const = 1_I2P
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_I2P_2

      subroutine init_I4P_2(fptr_dev, i)
         integer(I4P), intent(inout) :: fptr_dev(:,:)
         integer(I4P), parameter     :: const = 1_I4P
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_I4P_2

      subroutine init_I8P_2(fptr_dev, i)
         integer(I8P), intent(inout) :: fptr_dev(:,:)
         integer(I8P), parameter     :: const = 1_I8P
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_I8P_2

      subroutine init_R4P_2(fptr_dev, i)
         real(R4P), intent(inout) :: fptr_dev(:,:)
         real(R4P), parameter     :: const = 1.0_R4P
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_R4P_2

      subroutine init_R8P_2(fptr_dev, i)
         real(R8P), intent(inout) :: fptr_dev(:,:)
         real(R8P), parameter     :: const = 1.0_R8P
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_R8P_2

#if defined _R16P
      subroutine init_R16P_2(fptr_dev, i)
         real(R16P), intent(inout) :: fptr_dev(:,:)
         real(R16P), parameter     :: const = 1.0_R16P
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_R16P_2
#endif

      subroutine init_C4P_2(fptr_dev, i)
         complex(R4P), intent(inout) :: fptr_dev(:,:)
         complex(R4P), parameter     :: const = (1.0_R4P, 0.0_R4P)
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_C4P_2

      subroutine init_C8P_2(fptr_dev, i)
         complex(R8P), intent(inout) :: fptr_dev(:,:)
         complex(R8P), parameter     :: const = (1.0_R8P, 0.0_R8P)
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_C8P_2

#if defined _R16P
      subroutine init_C16P_2(fptr_dev, i)
         complex(R16P), intent(inout) :: fptr_dev(:,:)
         complex(R16P), parameter     :: const = (1.0_R16P, 0.0_R16P)
         include "src/tests/include/init_rank_2.i90"
      endsubroutine init_C16P_2
#endif
endmodule init_device_pointers
