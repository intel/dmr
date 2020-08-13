module matmul_device_pointers
   use PENF

   implicit none

   interface matmul_I
      module procedure matmul_I1P,   matmul_I2P,   matmul_I4P,   matmul_I8P,   &
                       matmul_I1P_2, matmul_I2P_2, matmul_I4P_2, matmul_I8P_2
   endinterface matmul_I

   interface matmul_R
      module procedure matmul_R4P,   matmul_R8P,   matmul_R16P,   &
                       matmul_R4P_2, matmul_R8P_2, matmul_R16P_2
   endinterface matmul_R

   interface matmul_C
      module procedure matmul_C4P,   matmul_C8P,   matmul_C16P,   &
                       matmul_C4P_2, matmul_C8P_2, matmul_C16P_2
   endinterface matmul_C

   public matmul_I, matmul_R, matmul_C

   private

   contains

      ! Init rank one device pointers
      subroutine matmul_I1P(fptr_dev, i)
         integer(I1P), intent(inout) :: fptr_dev(:)
         integer(I1P)                :: k=2_I1P
         include 'include/matmul_rank_1.i90'
      endsubroutine matmul_I1P

      subroutine matmul_I2P(fptr_dev, i)
         integer(I2P), intent(inout) :: fptr_dev(:)
         integer(I2P)                :: k=2_I2P
         include 'include/matmul_rank_1.i90'
      endsubroutine matmul_I2P

      subroutine matmul_I4P(fptr_dev, i)
         integer(I4P), intent(inout) :: fptr_dev(:)
         integer(I4P)                :: k=2_I4P
         include 'include/matmul_rank_1.i90'
      endsubroutine matmul_I4P

      subroutine matmul_I8P(fptr_dev, i)
         integer(I8P), intent(inout) :: fptr_dev(:)
         integer(I8P)                :: k=2_I8P
         include 'include/matmul_rank_1.i90'
      endsubroutine matmul_I8P

      subroutine matmul_R4P(fptr_dev, i)
         real(R4P), intent(inout) :: fptr_dev(:)
         real(R4P)                :: k=2.0_R4P
         include 'include/matmul_rank_1.i90'
      endsubroutine matmul_R4P

      subroutine matmul_R8P(fptr_dev, i)
         real(R8P), intent(inout) :: fptr_dev(:)
         real(R8P)                :: k=2.0_R8P
         include 'include/matmul_rank_1.i90'
      endsubroutine matmul_R8P

      subroutine matmul_R16P(fptr_dev, i)
         real(R16P), intent(inout) :: fptr_dev(:)
         real(R16P)                :: k=2.0_R16P
         include 'include/matmul_rank_1.i90'
      endsubroutine matmul_R16P

      subroutine matmul_C4P(fptr_dev, i)
         complex(R4P), intent(inout) :: fptr_dev(:)
         complex(R4P)                :: k=(2.0_R4P, 0.0_R4P)
         include 'include/matmul_rank_1.i90'
      endsubroutine matmul_C4P

      subroutine matmul_C8P(fptr_dev, i)
         complex(R8P), intent(inout) :: fptr_dev(:)
         complex(R8P)                :: k=(2.0_R8P, 0.0_R8P)
         include 'include/matmul_rank_1.i90'
      endsubroutine matmul_C8P

      subroutine matmul_C16P(fptr_dev, i)
         complex(R16P), intent(inout) :: fptr_dev(:)
         complex(R16P)                :: k=(2.0_R16P, 0.0_R16P)
         include 'include/matmul_rank_1.i90'
      endsubroutine matmul_C16P

      ! Init rank two device pointers
      subroutine matmul_I1P_2(fptr_dev, i)
         integer(I1P), intent(inout) :: fptr_dev(:,:)
         integer(I1P)                :: k=2_I1P
         include 'include/matmul_rank_2.i90'
      endsubroutine matmul_I1P_2

      subroutine matmul_I2P_2(fptr_dev, i)
         integer(I2P), intent(inout) :: fptr_dev(:,:)
         integer(I2P)                :: k=2_I2P
         include 'include/matmul_rank_2.i90'
      endsubroutine matmul_I2P_2

      subroutine matmul_I4P_2(fptr_dev, i)
         integer(I4P), intent(inout) :: fptr_dev(:,:)
         integer(I4P)                :: k=2_I4P
         include 'include/matmul_rank_2.i90'
      endsubroutine matmul_I4P_2

      subroutine matmul_I8P_2(fptr_dev, i)
         integer(I8P), intent(inout) :: fptr_dev(:,:)
         integer(I8P)                :: k=2_I8P
         include 'include/matmul_rank_2.i90'
      endsubroutine matmul_I8P_2

      subroutine matmul_R4P_2(fptr_dev, i)
         real(R4P), intent(inout) :: fptr_dev(:,:)
         real(R4P)                :: k=2.0_R4P
         include 'include/matmul_rank_2.i90'
      endsubroutine matmul_R4P_2

      subroutine matmul_R8P_2(fptr_dev, i)
         real(R8P), intent(inout) :: fptr_dev(:,:)
         real(R8P)                :: k=2.0_R8P
         include 'include/matmul_rank_2.i90'
      endsubroutine matmul_R8P_2

      subroutine matmul_R16P_2(fptr_dev, i)
         real(R16P), intent(inout) :: fptr_dev(:,:)
         real(R16P)                :: k=2.0_R16P
         include 'include/matmul_rank_2.i90'
      endsubroutine matmul_R16P_2

      subroutine matmul_C4P_2(fptr_dev, i)
         complex(R4P), intent(inout) :: fptr_dev(:,:)
         complex(R4P)                :: k=(2.0_R4P, 0.0_R4P)
         include 'include/matmul_rank_2.i90'
      endsubroutine matmul_C4P_2

      subroutine matmul_C8P_2(fptr_dev, i)
         complex(R8P), intent(inout) :: fptr_dev(:,:)
         complex(R8P)                :: k=(2.0_R8P, 0.0_R8P)
         include 'include/matmul_rank_2.i90'
      endsubroutine matmul_C8P_2

      subroutine matmul_C16P_2(fptr_dev, i)
         complex(R16P), intent(inout) :: fptr_dev(:,:)
         complex(R16P)                 :: k=(2.0_R16P, 0.0_R16P)
         include 'include/matmul_rank_2.i90'
      endsubroutine matmul_C16P_2
endmodule matmul_device_pointers
