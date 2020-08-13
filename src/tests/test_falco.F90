program test_falco
   use falco
   use penf
   use omp_lib
   use init_device_pointers
   use matmul_device_pointers
   use, intrinsic :: iso_c_binding

   implicit none

   integer(I1P),  pointer, contiguous :: fptr_dev_I1P   (:),             fptr_dev_I1P_2 (:,:),         &
                                         fptr_dev_I1P_3 (:,:,:),         fptr_dev_I1P_4 (:,:,:,:),     &
                                         fptr_dev_I1P_5 (:,:,:,:,:),     fptr_dev_I1P_6 (:,:,:,:,:,:), &
                                         fptr_dev_I1P_7 (:,:,:,:,:,:,:)
   integer(I1P),  target, allocatable :: fptr_hos_I1P   (:),             fptr_hos_I1P_2 (:,:),         &
                                         fptr_hos_I1P_3 (:,:,:),         fptr_hos_I1P_4 (:,:,:,:),     &
                                         fptr_hos_I1P_5 (:,:,:,:,:),     fptr_hos_I1P_6 (:,:,:,:,:,:), &
                                         fptr_hos_I1P_7 (:,:,:,:,:,:,:)
   integer(I2P),  pointer, contiguous :: fptr_dev_I2P   (:),             fptr_dev_I2P_2 (:,:),         &
                                         fptr_dev_I2P_3 (:,:,:),         fptr_dev_I2P_4 (:,:,:,:),     &
                                         fptr_dev_I2P_5 (:,:,:,:,:),     fptr_dev_I2P_6 (:,:,:,:,:,:), &
                                         fptr_dev_I2P_7 (:,:,:,:,:,:,:)
   integer(I2P),  target, allocatable :: fptr_hos_I2P   (:),             fptr_hos_I2P_2 (:,:),         &
                                         fptr_hos_I2P_3 (:,:,:),         fptr_hos_I2P_4 (:,:,:,:),     &
                                         fptr_hos_I2P_5 (:,:,:,:,:),     fptr_hos_I2P_6 (:,:,:,:,:,:), &
                                         fptr_hos_I2P_7 (:,:,:,:,:,:,:)
   integer(I4P),  pointer, contiguous :: fptr_dev_I4P   (:),             fptr_dev_I4P_2 (:,:),         &
                                         fptr_dev_I4P_3 (:,:,:),         fptr_dev_I4P_4 (:,:,:,:),     &
                                         fptr_dev_I4P_5 (:,:,:,:,:),     fptr_dev_I4P_6 (:,:,:,:,:,:), &
                                         fptr_dev_I4P_7 (:,:,:,:,:,:,:)
   integer(I4P),  target, allocatable :: fptr_hos_I4P   (:),             fptr_hos_I4P_2 (:,:),         &
                                         fptr_hos_I4P_3 (:,:,:),         fptr_hos_I4P_4 (:,:,:,:),     &
                                         fptr_hos_I4P_5 (:,:,:,:,:),     fptr_hos_I4P_6 (:,:,:,:,:,:), &
                                         fptr_hos_I4P_7 (:,:,:,:,:,:,:)
   integer(I8P),  pointer, contiguous :: fptr_dev_I8P   (:),             fptr_dev_I8P_2 (:,:),         &
                                         fptr_dev_I8P_3 (:,:,:),         fptr_dev_I8P_4 (:,:,:,:),     &
                                         fptr_dev_I8P_5 (:,:,:,:,:),     fptr_dev_I8P_6 (:,:,:,:,:,:), &
                                         fptr_dev_I8P_7 (:,:,:,:,:,:,:)
   integer(I8P),  target, allocatable :: fptr_hos_I8P   (:),             fptr_hos_I8P_2 (:,:),         &
                                         fptr_hos_I8P_3 (:,:,:),         fptr_hos_I8P_4 (:,:,:,:),     &
                                         fptr_hos_I8P_5 (:,:,:,:,:),     fptr_hos_I8P_6 (:,:,:,:,:,:), &
                                         fptr_hos_I8P_7 (:,:,:,:,:,:,:)
   real(R4P),     pointer, contiguous :: fptr_dev_R4P   (:),             fptr_dev_R4P_2 (:,:),         &
                                         fptr_dev_R4P_3 (:,:,:),         fptr_dev_R4P_4 (:,:,:,:),     &
                                         fptr_dev_R4P_5 (:,:,:,:,:),     fptr_dev_R4P_6 (:,:,:,:,:,:), &
                                         fptr_dev_R4P_7 (:,:,:,:,:,:,:)
   real(R4P),     target, allocatable :: fptr_hos_R4P   (:),             fptr_hos_R4P_2 (:,:),         &
                                         fptr_hos_R4P_3 (:,:,:),         fptr_hos_R4P_4 (:,:,:,:),     &
                                         fptr_hos_R4P_5 (:,:,:,:,:),     fptr_hos_R4P_6 (:,:,:,:,:,:), &
                                         fptr_hos_R4P_7 (:,:,:,:,:,:,:)
   real(R8P),     pointer, contiguous :: fptr_dev_R8P   (:),             fptr_dev_R8P_2 (:,:),         &
                                         fptr_dev_R8P_3 (:,:,:),         fptr_dev_R8P_4 (:,:,:,:),     &
                                         fptr_dev_R8P_5 (:,:,:,:,:),     fptr_dev_R8P_6 (:,:,:,:,:,:), &
                                         fptr_dev_R8P_7 (:,:,:,:,:,:,:)
   real(R8P),     target, allocatable :: fptr_hos_R8P   (:),             fptr_hos_R8P_2 (:,:),         &
                                         fptr_hos_R8P_3 (:,:,:),         fptr_hos_R8P_4 (:,:,:,:),     &
                                         fptr_hos_R8P_5 (:,:,:,:,:),     fptr_hos_R8P_6 (:,:,:,:,:,:), &
                                         fptr_hos_R8P_7 (:,:,:,:,:,:,:)
   real(R16P),    pointer, contiguous :: fptr_dev_R16P  (:),             fptr_dev_R16P_2(:,:),         &
                                         fptr_dev_R16P_3(:,:,:),         fptr_dev_R16P_4(:,:,:,:),     &
                                         fptr_dev_R16P_5(:,:,:,:,:),     fptr_dev_R16P_6(:,:,:,:,:,:), &
                                         fptr_dev_R16P_7(:,:,:,:,:,:,:)
   real(R16P),    target, allocatable :: fptr_hos_R16P  (:),             fptr_hos_R16P_2(:,:),         &
                                         fptr_hos_R16P_3(:,:,:),         fptr_hos_R16P_4(:,:,:,:),     &
                                         fptr_hos_R16P_5(:,:,:,:,:),     fptr_hos_R16P_6(:,:,:,:,:,:), &
                                         fptr_hos_R16P_7(:,:,:,:,:,:,:)
   complex(R4P),  pointer, contiguous :: fptr_dev_C4P   (:),             fptr_dev_C4P_2 (:,:),         &
                                         fptr_dev_C4P_3 (:,:,:),         fptr_dev_C4P_4 (:,:,:,:),     &
                                         fptr_dev_C4P_5 (:,:,:,:,:),     fptr_dev_C4P_6 (:,:,:,:,:,:), &
                                         fptr_dev_C4P_7 (:,:,:,:,:,:,:)
   complex(R4P),  target, allocatable :: fptr_hos_C4P   (:),             fptr_hos_C4P_2 (:,:),         &
                                         fptr_hos_C4P_3 (:,:,:),         fptr_hos_C4P_4 (:,:,:,:),     &
                                         fptr_hos_C4P_5 (:,:,:,:,:),     fptr_hos_C4P_6 (:,:,:,:,:,:), &
                                         fptr_hos_C4P_7 (:,:,:,:,:,:,:)
   complex(R8P),  pointer, contiguous :: fptr_dev_C8P   (:),             fptr_dev_C8P_2 (:,:),         &
                                         fptr_dev_C8P_3 (:,:,:),         fptr_dev_C8P_4 (:,:,:,:),     &
                                         fptr_dev_C8P_5 (:,:,:,:,:),     fptr_dev_C8P_6 (:,:,:,:,:,:), &
                                         fptr_dev_C8P_7 (:,:,:,:,:,:,:)
   complex(R8P),  target, allocatable :: fptr_hos_C8P   (:),             fptr_hos_C8P_2 (:,:),         &
                                         fptr_hos_C8P_3 (:,:,:),         fptr_hos_C8P_4 (:,:,:,:),     &
                                         fptr_hos_C8P_5 (:,:,:,:,:),     fptr_hos_C8P_6 (:,:,:,:,:,:), &
                                         fptr_hos_C8P_7 (:,:,:,:,:,:,:)
   complex(R16P), pointer, contiguous :: fptr_dev_C16P  (:),             fptr_dev_C16P_2(:,:),         &
                                         fptr_dev_C16P_3(:,:,:),         fptr_dev_C16P_4(:,:,:,:),     &
                                         fptr_dev_C16P_5(:,:,:,:,:),     fptr_dev_C16P_6(:,:,:,:,:,:), &
                                         fptr_dev_C16P_7(:,:,:,:,:,:,:)
   complex(R16P), target, allocatable :: fptr_hos_C16P  (:),             fptr_hos_C16P_2(:,:),         &
                                         fptr_hos_C16P_3(:,:,:),         fptr_hos_C16P_4(:,:,:,:),     &
                                         fptr_hos_C16P_5(:,:,:,:,:),     fptr_hos_C16P_6(:,:,:,:,:,:), &
                                         fptr_hos_C16P_7(:,:,:,:,:,:,:)

   integer(I8P), parameter            :: i = 20_I8P
   integer(I8P), parameter            :: j = 10_I8P
   integer(I8P)                       :: siz2(2)=i, siz3(3)=i, siz4(4)=i, siz5(5)=i, siz6(6)=i, siz7(7)=i
   integer(I4P)                       :: ierr
   integer(I4P)                       :: omp_initial, omp_default
   type(c_ptr)                        :: cptr_dev, cptr_hos
   integer(kind=c_int)                :: errr

   omp_initial = omp_get_initial_device()
   omp_default = omp_get_default_device()

   print *, '*****************************************************************'
   print *, '                   Start FALCO library testing                   '
   print *, '*****************************************************************'
   print *, ''
   print *, '                     Testing rank one arrays                     '
   print *, ''
   print *, 'Integers arrays'
   print *, ''
   call omp_target_alloc_f(fptr_dev_I1P, i, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I1P)
   call init_I(fptr_dev_I1P, i)
   print *, 'F pointer initialization complete'
   call matmul_I(fptr_dev_I1P, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_I1P(i))
   call omp_target_memcpy_f(fptr_hos_I1P, fptr_dev_I1P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_I1P(1), fptr_hos_I1P(i)
   call omp_target_free_f(fptr_dev_I1P, omp_default)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_I1P)

   call omp_target_alloc_f(fptr_dev_I1P, i, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I1P)
   allocate(fptr_hos_I1P(i))
   fptr_hos_I1P = 2_I1P
   print *, 'F host pointer initialization complete'
   call omp_target_memcpy_f(fptr_dev_I1P, fptr_hos_I1P, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_I1P(1), fptr_hos_I1P(i)
   print *, 'F pointer on the device has values: ', fptr_dev_I1P(1), fptr_dev_I1P(i)
   call omp_target_free_f(fptr_dev_I1P, omp_default)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_I1P)

   call omp_target_alloc_f(fptr_dev_I2P, i, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I2P)
   call init_I(fptr_dev_I2P, i)
   print *, 'F pointer initialization complete'
   call matmul_I(fptr_dev_I2P, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_I2P(i))
   call omp_target_memcpy_f(fptr_hos_I2P, fptr_dev_I2P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_I2P(1), fptr_hos_I2P(i)
   call omp_target_free_f(fptr_dev_I2P)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_I2P)

   call omp_target_alloc_f(fptr_dev_I4P, i, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I4P)
   call init_I(fptr_dev_I4P, i)
   print *, 'F pointer initialization complete'
   call matmul_I(fptr_dev_I4P, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_I4P(i))
   call omp_target_memcpy_f(fptr_hos_I4P, fptr_dev_I4P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_I4P(1), fptr_hos_I4P(i)
   call omp_target_free_f(fptr_dev_I4P)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_I4P)

   call omp_target_alloc_f(fptr_dev_I8P, i, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I8P)
   call init_I(fptr_dev_I8P, i)
   print *, 'F pointer initialization complete'
   call matmul_I(fptr_dev_I8P, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_I8P(i))
   call omp_target_memcpy_f(fptr_hos_I8P, fptr_dev_I8P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_I8P(1), fptr_hos_I8P(i)
   call omp_target_free_f(fptr_dev_I8P)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_I8P)

   print *, ''
   print *, 'Real arrays'
   print *, ''
   call omp_target_alloc_f(fptr_dev_R4P, i, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_R4P)
   call init_R(fptr_dev_R4P, i)
   print *, 'F pointer initialization complete'
   call matmul_R(fptr_dev_R4P, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_R4P(i))
   call omp_target_memcpy_f(fptr_hos_R4P, fptr_dev_R4P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_R4P(1), fptr_hos_R4P(i)
   call omp_target_free_f(fptr_dev_R4P, omp_default)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_R4P)

   call omp_target_alloc_f(fptr_dev_R8P, i, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_R8P)
   call init_R(fptr_dev_R8P, i)
   print *, 'F pointer initialization complete'
   call matmul_R(fptr_dev_R8P, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_R8P(i))
   call omp_target_memcpy_f(fptr_hos_R8P, fptr_dev_R8P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_R8P(1), fptr_hos_R8P(i)
   call omp_target_free_f(fptr_dev_R8P)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_R8P)

   call omp_target_alloc_f(fptr_dev_R16P, i, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_R16P)
   call init_R(fptr_dev_R16P, i)
   print *, 'F pointer initialization complete'
   call matmul_R(fptr_dev_R16P, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_R16P(i))
   call omp_target_memcpy_f(fptr_hos_R16P, fptr_dev_R16P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_R16P(1), fptr_hos_R16P(i)
   call omp_target_free_f(fptr_dev_R16P)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_R16P)

   print *, ''
   print *, 'Complex arrays'
   print *, ''
   call omp_target_alloc_f(fptr_dev_C4P, i, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_C4P)
   call init_C(fptr_dev_C4P, i)
   print *, 'F pointer initialization complete'
   call matmul_C(fptr_dev_C4P, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_C4P(i))
   call omp_target_memcpy_f(fptr_hos_C4P, fptr_dev_C4P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_C4P(1), fptr_hos_C4P(i)
   call omp_target_free_f(fptr_dev_C4P, omp_default)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_C4P)

   call omp_target_alloc_f(fptr_dev_C8P, i, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_C8P)
   call init_C(fptr_dev_C8P, i)
   print *, 'F pointer initialization complete'
   call matmul_C(fptr_dev_C8P, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_C8P(i))
   call omp_target_memcpy_f(fptr_hos_C8P, fptr_dev_C8P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_C8P(1), fptr_hos_C8P(i)
   call omp_target_free_f(fptr_dev_C8P)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_C8P)

   call omp_target_alloc_f(fptr_dev_C16P, i, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_C16P)
   call init_C(fptr_dev_C16P, i)
   print *, 'F pointer initialization complete'
   call matmul_C(fptr_dev_C16P, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_C16P(i))
   call omp_target_memcpy_f(fptr_hos_C16P, fptr_dev_C16P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_C16P(1), fptr_hos_C16P(i)
   call omp_target_free_f(fptr_dev_C16P)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_C16P)

   print *, ''
   print *, '                     Testing rank two arrays                     '
   print *, ''
   print *, 'Integers arrays'
   print *, ''
   call omp_target_alloc_f(fptr_dev_I1P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I1P_2)
   call init_I(fptr_dev_I1P_2, i)
   print *, 'F pointer initialization complete'
   call matmul_I(fptr_dev_I1P_2, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_I1P_2(i,i))
   call omp_target_memcpy_f(fptr_hos_I1P_2, fptr_dev_I1P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_I1P_2(1,1), fptr_hos_I1P_2(i,i)
   call omp_target_free_f(fptr_dev_I1P_2, omp_default)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_I1P_2)

   call omp_target_alloc_f(fptr_dev_I2P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I2P_2)
   call init_I(fptr_dev_I2P_2, i)
   print *, 'F pointer initialization complete'
   call matmul_I(fptr_dev_I2P_2, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_I2P_2(i,i))
   call omp_target_memcpy_f(fptr_hos_I2P_2, fptr_dev_I2P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_I2P_2(1,1), fptr_hos_I2P_2(i,i)
   call omp_target_free_f(fptr_dev_I2P_2)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_I2P_2)

   call omp_target_alloc_f(fptr_dev_I4P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I4P_2)
   call init_I(fptr_dev_I4P_2, i)
   print *, 'F pointer initialization complete'
   call matmul_I(fptr_dev_I4P_2, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_I4P_2(i,i))
   call omp_target_memcpy_f(fptr_hos_I4P_2, fptr_dev_I4P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_I4P_2(1,1), fptr_hos_I4P_2(i,i)
   call omp_target_free_f(fptr_dev_I4P_2)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_I4P_2)

   call omp_target_alloc_f(fptr_dev_I8P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I8P_2)
   call init_I(fptr_dev_I8P_2, i)
   print *, 'F pointer initialization complete'
   call matmul_I(fptr_dev_I8P_2, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_I8P_2(i,i))
   call omp_target_memcpy_f(fptr_hos_I8P_2, fptr_dev_I8P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_I8P_2(1,1), fptr_hos_I8P_2(i,i)
   call omp_target_free_f(fptr_dev_I8P_2)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_I8P_2)

   print *, ''
   print *, 'Real arrays'
   print *, ''
   call omp_target_alloc_f(fptr_dev_R4P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_R4P_2)
   call init_R(fptr_dev_R4P_2, i)
   print *, 'F pointer initialization complete'
   call matmul_R(fptr_dev_R4P_2, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_R4P_2(i,i))
   call omp_target_memcpy_f(fptr_hos_R4P_2, fptr_dev_R4P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_R4P_2(1,1), fptr_hos_R4P_2(i,i)
   call omp_target_free_f(fptr_dev_R4P_2, omp_default)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_R4P_2)

   call omp_target_alloc_f(fptr_dev_R8P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_R8P_2)
   call init_R(fptr_dev_R8P_2, i)
   print *, 'F pointer initialization complete'
   call matmul_R(fptr_dev_R8P_2, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_R8P_2(i,i))
   call omp_target_memcpy_f(fptr_hos_R8P_2, fptr_dev_R8P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_R8P_2(1,1), fptr_hos_R8P_2(i,i)
   call omp_target_free_f(fptr_dev_R8P_2)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_R8P_2)

   call omp_target_alloc_f(fptr_dev_R16P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_R16P_2)
   call init_R(fptr_dev_R16P_2, i)
   print *, 'F pointer initialization complete'
   call matmul_R(fptr_dev_R16P_2, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_R16P_2(i,i))
   call omp_target_memcpy_f(fptr_hos_R16P_2, fptr_dev_R16P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_R16P_2(1,1), fptr_hos_R16P_2(i,i)
   call omp_target_free_f(fptr_dev_R16P_2)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_R16P_2)

   print *, ''
   print *, 'Complex arrays'
   print *, ''
   call omp_target_alloc_f(fptr_dev_C4P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_C4P_2)
   call init_C(fptr_dev_C4P_2, i)
   print *, 'F pointer initialization complete'
   call matmul_C(fptr_dev_C4P_2, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_C4P_2(i,i))
   call omp_target_memcpy_f(fptr_hos_C4P_2, fptr_dev_C4P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_C4P_2(1,1), fptr_hos_C4P_2(i,i)
   call omp_target_free_f(fptr_dev_C4P_2, omp_default)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_C4P_2)

   call omp_target_alloc_f(fptr_dev_C8P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_C8P_2)
   call init_C(fptr_dev_C8P_2, i)
   print *, 'F pointer initialization complete'
   call matmul_C(fptr_dev_C8P_2, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_C8P_2(i,i))
   call omp_target_memcpy_f(fptr_hos_C8P_2, fptr_dev_C8P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_C8P_2(1,1), fptr_hos_C8P_2(i,i)
   call omp_target_free_f(fptr_dev_C8P_2)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_C8P_2)

   call omp_target_alloc_f(fptr_dev_C16P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_C16P_2)
   call init_C(fptr_dev_C16P_2, i)
   print *, 'F pointer initialization complete'
   call matmul_C(fptr_dev_C16P_2, i)
   print *, 'F pointer multiplication complete'
   allocate(fptr_hos_C16P_2(i,i))
   call omp_target_memcpy_f(fptr_hos_C16P_2, fptr_dev_C16P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'F pointer copy complete'
   print *, 'F pointer on the host has values: ', fptr_hos_C16P_2(1,1), fptr_hos_C16P_2(i,i)
   call omp_target_free_f(fptr_dev_C16P_2)
   print *, 'F pointer deallocated from the device'
   deallocate(fptr_hos_C16P_2)

endprogram test_falco
