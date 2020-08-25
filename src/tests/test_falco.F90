program test_falco
   use omp_lib
   use falco
   use penf
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
#if defined _R16P
   real(R16P),    pointer, contiguous :: fptr_dev_R16P  (:),             fptr_dev_R16P_2(:,:),         &
                                         fptr_dev_R16P_3(:,:,:),         fptr_dev_R16P_4(:,:,:,:),     &
                                         fptr_dev_R16P_5(:,:,:,:,:),     fptr_dev_R16P_6(:,:,:,:,:,:), &
                                         fptr_dev_R16P_7(:,:,:,:,:,:,:)
   real(R16P),    target, allocatable :: fptr_hos_R16P  (:),             fptr_hos_R16P_2(:,:),         &
                                         fptr_hos_R16P_3(:,:,:),         fptr_hos_R16P_4(:,:,:,:),     &
                                         fptr_hos_R16P_5(:,:,:,:,:),     fptr_hos_R16P_6(:,:,:,:,:,:), &
                                         fptr_hos_R16P_7(:,:,:,:,:,:,:)
#endif
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
#if defined _R16P
   complex(R16P), pointer, contiguous :: fptr_dev_C16P  (:),             fptr_dev_C16P_2(:,:),         &
                                         fptr_dev_C16P_3(:,:,:),         fptr_dev_C16P_4(:,:,:,:),     &
                                         fptr_dev_C16P_5(:,:,:,:,:),     fptr_dev_C16P_6(:,:,:,:,:,:), &
                                         fptr_dev_C16P_7(:,:,:,:,:,:,:)
   complex(R16P), target, allocatable :: fptr_hos_C16P  (:),             fptr_hos_C16P_2(:,:),         &
                                         fptr_hos_C16P_3(:,:,:),         fptr_hos_C16P_4(:,:,:,:),     &
                                         fptr_hos_C16P_5(:,:,:,:,:),     fptr_hos_C16P_6(:,:,:,:,:,:), &
                                         fptr_hos_C16P_7(:,:,:,:,:,:,:)
#endif

   integer(I8P), parameter            :: i = 20000_I8P
   integer(I8P), parameter            :: j = 10_I8P
   integer(I8P)                       :: siz2(2)=i, siz3(3)=i, siz4(4)=i, siz5(5)=i, siz6(6)=i, siz7(7)=i
   integer(I4P)                       :: ierr
   integer(I4P)                       :: omp_initial, omp_default
   type(c_ptr)                        :: cptr_dev, cptr_hos
   integer(kind=c_int)                :: errr

   omp_default = omp_get_default_device()
   omp_initial = omp_get_initial_device_c()

   print *, '*****************************************************************'
   print *, '                   Start FALCO library testing                   '
   print *, '*****************************************************************'
   print *, ''
   print *, ' +++++++++++++++++++ Testing rank one arrays +++++++++++++++++++ '
   print *, ''
   print *, '- - - - - - - - - - - - -Integer arrays- - - - - - - - - - - - - '
   print *, ''
   print *, '                       I1P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_I1P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I1P)
   call init_I(fptr_dev_I1P, i)
   print *, 'Device pointer initialization completed'
   call matmul_I(fptr_dev_I1P, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_I1P(i)); fptr_hos_I1P = 0_I1P
   call omp_target_memcpy_f(fptr_hos_I1P, fptr_dev_I1P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_I1P(1), fptr_hos_I1P(i)
   call omp_target_free_f(fptr_dev_I1P, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_I1P)

   print *, ''
   print *, '                       I1P Host to Device                        '
   print *, ''
   allocate(fptr_hos_I1P(i)); fptr_hos_I1P = 5_I1P
   call omp_target_alloc_f(fptr_dev_I1P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I1P)
   call omp_target_memcpy_f(fptr_dev_I1P, fptr_hos_I1P, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_I1P = 0_I1P
   call omp_target_memcpy_f(fptr_hos_I1P, fptr_dev_I1P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_I1P(1), fptr_hos_I1P(i)
   call omp_target_free_f(fptr_dev_I1P, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_I1P)

   print *, ''
   print *, '                       I2P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_I2P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I2P)
   call init_I(fptr_dev_I2P, i)
   print *, 'Device pointer initialization completed'
   call matmul_I(fptr_dev_I2P, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_I2P(i)); fptr_hos_I2P = 0_I2P
   call omp_target_memcpy_f(fptr_hos_I2P, fptr_dev_I2P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_I2P(1), fptr_hos_I2P(i)
   call omp_target_free_f(fptr_dev_I2P)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_I2P)

   print *, ''
   print *, '                       I2P Host to Device                        '
   print *, ''
   allocate(fptr_hos_I2P(i)); fptr_hos_I2P = 5_I2P
   call omp_target_alloc_f(fptr_dev_I2P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I2P)
   call omp_target_memcpy_f(fptr_dev_I2P, fptr_hos_I2P, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_I2P = 0_I2P
   call omp_target_memcpy_f(fptr_hos_I2P, fptr_dev_I2P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_I2P(1), fptr_hos_I2P(i)
   call omp_target_free_f(fptr_dev_I2P, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_I2P)

   print *, ''
   print *, '                       I4P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_I4P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I4P)
   call init_I(fptr_dev_I4P, i)
   print *, 'Device pointer initialization completed'
   call matmul_I(fptr_dev_I4P, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_I4P(i)); fptr_hos_I4P = 0_I4P
   call omp_target_memcpy_f(fptr_hos_I4P, fptr_dev_I4P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_I4P(1), fptr_hos_I4P(i)
   call omp_target_free_f(fptr_dev_I4P)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_I4P)

   print *, ''
   print *, '                       I4P Host to Device                        '
   print *, ''
   allocate(fptr_hos_I4P(i)); fptr_hos_I4P = 5_I4P
   call omp_target_alloc_f(fptr_dev_I4P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I4P)
   call omp_target_memcpy_f(fptr_dev_I4P, fptr_hos_I4P, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_I4P = 0_I4P
   call omp_target_memcpy_f(fptr_hos_I4P, fptr_dev_I4P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_I4P(1), fptr_hos_I4P(i)
   call omp_target_free_f(fptr_dev_I4P, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_I4P)

   print *, ''
   print *, '                       I8P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_I8P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I8P)
   call init_I(fptr_dev_I8P, i)
   print *, 'Device pointer initialization completed'
   call matmul_I(fptr_dev_I8P, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_I8P(i)); fptr_hos_I8P = 0_I8P
   call omp_target_memcpy_f(fptr_hos_I8P, fptr_dev_I8P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_I8P(1), fptr_hos_I8P(i)
   call omp_target_free_f(fptr_dev_I8P)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_I8P)

   print *, ''
   print *, '                       I8P Host to Device                        '
   print *, ''
   allocate(fptr_hos_I8P(i)); fptr_hos_I8P = 5_I8P
   call omp_target_alloc_f(fptr_dev_I8P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I8P)
   call omp_target_memcpy_f(fptr_dev_I8P, fptr_hos_I8P, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_I8P = 0_I8P
   call omp_target_memcpy_f(fptr_hos_I8P, fptr_dev_I8P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_I8P(1), fptr_hos_I8P(i)
   call omp_target_free_f(fptr_dev_I8P, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_I8P)

   print *, ''
   print *, '- - - - - - - - - - - - - -Real arrays- - - - - - - - - - - - - -'
   print *, ''
   print *, '                       R4P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_R4P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_R4P)
   call init_R(fptr_dev_R4P, i)
   print *, 'Device pointer initialization completed'
   call matmul_R(fptr_dev_R4P, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_R4P(i)); fptr_hos_R4P = 0_R4P
   call omp_target_memcpy_f(fptr_hos_R4P, fptr_dev_R4P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_R4P(1), fptr_hos_R4P(i)
   call omp_target_free_f(fptr_dev_R4P, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_R4P)

   print *, ''
   print *, '                       R4P Host to Device                        '
   print *, ''
   allocate(fptr_hos_R4P(i)); fptr_hos_R4P = 5.0_R4P
   call omp_target_alloc_f(fptr_dev_R4P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_R4P)
   call omp_target_memcpy_f(fptr_dev_R4P, fptr_hos_R4P, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_R4P = 0.0_R4P
   call omp_target_memcpy_f(fptr_hos_R4P, fptr_dev_R4P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_R4P(1), fptr_hos_R4P(i)
   call omp_target_free_f(fptr_dev_R4P, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_R4P)

   print *, ''
   print *, '                       R8P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_R8P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_R8P)
   call init_R(fptr_dev_R8P, i)
   print *, 'Device pointer initialization completed'
   call matmul_R(fptr_dev_R8P, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_R8P(i)); fptr_hos_R8P = 0.0_R8P
   call omp_target_memcpy_f(fptr_hos_R8P, fptr_dev_R8P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_R8P(1), fptr_hos_R8P(i)
   call omp_target_free_f(fptr_dev_R8P)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_R8P)

   print *, ''
   print *, '                       R8P Host to Device                        '
   print *, ''
   allocate(fptr_hos_R8P(i)); fptr_hos_R8P = 5.0_R8P
   call omp_target_alloc_f(fptr_dev_R8P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_R8P)
   call omp_target_memcpy_f(fptr_dev_R8P, fptr_hos_R8P, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_R8P = 0.0_R8P
   call omp_target_memcpy_f(fptr_hos_R8P, fptr_dev_R8P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_R8P(1), fptr_hos_R8P(i)
   call omp_target_free_f(fptr_dev_R8P, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_R8P)

#if defined _R16P
   print *, ''
   print *, '                       R16P Device to Host                       '
   print *, ''
   call omp_target_alloc_f(fptr_dev_R16P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_R16P)
   call init_R(fptr_dev_R16P, i)
   print *, 'Device pointer initialization completed'
   call matmul_R(fptr_dev_R16P, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_R16P(i)); fptr_hos_R16P = 0.0_R16P
   call omp_target_memcpy_f(fptr_hos_R16P, fptr_dev_R16P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_R16P(1), fptr_hos_R16P(i)
   call omp_target_free_f(fptr_dev_R16P)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_R16P)

   print *, ''
   print *, '                       R16P Host to Device                       '
   print *, ''
   allocate(fptr_hos_R16P(i)); fptr_hos_R16P = 5.0_R16P
   call omp_target_alloc_f(fptr_dev_R16P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_R16P)
   call omp_target_memcpy_f(fptr_dev_R16P, fptr_hos_R16P, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_R16P = 0.0_R16P
   call omp_target_memcpy_f(fptr_hos_R16P, fptr_dev_R16P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_R16P(1), fptr_hos_R16P(i)
   call omp_target_free_f(fptr_dev_R16P, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_R16P)
#endif

   print *, ''
   print *, '- - - - - - - - - - - - -Complex arrays- - - - - - - - - - - - - '
   print *, ''
   print *, '                       C4P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_C4P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_C4P)
   call init_C(fptr_dev_C4P, i)
   print *, 'Device pointer initialization completed'
   call matmul_C(fptr_dev_C4P, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_C4P(i)); fptr_hos_C4P = (0.0_R4P, 0.0_R4P)
   call omp_target_memcpy_f(fptr_hos_C4P, fptr_dev_C4P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_C4P(1), fptr_hos_C4P(i)
   call omp_target_free_f(fptr_dev_C4P, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_C4P)

   print *, ''
   print *, '                       C4P Host to Device                        '
   print *, ''
   allocate(fptr_hos_C4P(i)); fptr_hos_C4P = (5.0_R4P, 5.0_R4P)
   call omp_target_alloc_f(fptr_dev_C4P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_C4P)
   call omp_target_memcpy_f(fptr_dev_C4P, fptr_hos_C4P, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_C4P = (0.0_R4P, 0.0_R4P)
   call omp_target_memcpy_f(fptr_hos_C4P, fptr_dev_C4P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_C4P(1), fptr_hos_C4P(i)
   call omp_target_free_f(fptr_dev_C4P, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_C4P)

   print *, ''
   print *, '                       C8P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_C8P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_C8P)
   call init_C(fptr_dev_C8P, i)
   print *, 'Device pointer initialization completed'
   call matmul_C(fptr_dev_C8P, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_C8P(i)); fptr_hos_C8P = (0.0_R8P, 0.0_R8P)
   call omp_target_memcpy_f(fptr_hos_C8P, fptr_dev_C8P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_C8P(1), fptr_hos_C8P(i)
   call omp_target_free_f(fptr_dev_C8P)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_C8P)

   print *, ''
   print *, '                       C8P Host to Device                        '
   print *, ''
   allocate(fptr_hos_C8P(i)); fptr_hos_C8P = (5.0_R8P, 5.0_R8P)
   call omp_target_alloc_f(fptr_dev_C8P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_C8P)
   call omp_target_memcpy_f(fptr_dev_C8P, fptr_hos_C8P, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_C8P = (0.0_R8P, 0.0_R8P)
   call omp_target_memcpy_f(fptr_hos_C8P, fptr_dev_C8P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_C8P(1), fptr_hos_C8P(i)
   call omp_target_free_f(fptr_dev_C8P, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_C8P)

#if defined _R16P
   print *, ''
   print *, '                       C16P Device to Host                       '
   print *, ''
   call omp_target_alloc_f(fptr_dev_C16P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_C16P)
   call init_C(fptr_dev_C16P, i)
   print *, 'Device pointer initialization completed'
   call matmul_C(fptr_dev_C16P, i)
   print *, 'Device pointer multiplication completed'
   allocate(fptr_hos_C16P(i)); fptr_hos_C16P = (0.0_R16P, 0.0_R16P)
   call omp_target_memcpy_f(fptr_hos_C16P, fptr_dev_C16P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_C16P(1), fptr_hos_C16P(i)
   call omp_target_free_f(fptr_dev_C16P)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_C16P)

   print *, ''
   print *, '                       C16P Host to Device                       '
   print *, ''
   allocate(fptr_hos_C16P(i)); fptr_hos_C16P = (5.0_R16P, 5.0_R16P)
   call omp_target_alloc_f(fptr_dev_C16P, i, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_C16P)
   call omp_target_memcpy_f(fptr_dev_C16P, fptr_hos_C16P, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_C16P = (0.0_R16P, 0.0_R16P)
   call omp_target_memcpy_f(fptr_hos_C16P, fptr_dev_C16P, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_C16P(1), fptr_hos_C16P(i)
   call omp_target_free_f(fptr_dev_C16P, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_C16P)
#endif

   print *, ''
   print *, ' +++++++++++++++++++ Testing rank two arrays +++++++++++++++++++ '
   print *, ''
   print *, '- - - - - - - - - - - - -Integer arrays- - - - - - - - - - - - - '
   print *, ''
   print *, '                       I1P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_I1P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I1P_2)
   call init_I(fptr_dev_I1P_2, i)
   print *, 'F pointer initialization completed'
   call matmul_I(fptr_dev_I1P_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_I1P_2(i,i)); fptr_hos_I1P_2 = 0_I1P
   call omp_target_memcpy_f(fptr_hos_I1P_2, fptr_dev_I1P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_I1P_2(1,1), fptr_hos_I1P_2(i,i)
   call omp_target_free_f(fptr_dev_I1P_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_I1P_2)

   print *, ''
   print *, '                       I1P Host to Device                        '
   print *, ''
   allocate(fptr_hos_I1P_2(i,i)); fptr_hos_I1P_2 = 5_I1P
   call omp_target_alloc_f(fptr_dev_I1P_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I1P_2)
   call omp_target_memcpy_f(fptr_dev_I1P_2, fptr_hos_I1P_2, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_I1P_2 = 0_I1P
   call omp_target_memcpy_f(fptr_hos_I1P_2, fptr_dev_I1P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_I1P_2(1,1), fptr_hos_I1P_2(i,i)
   call omp_target_free_f(fptr_dev_I1P_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_I1P_2)

   print *, ''
   print *, '                       I2P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_I2P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I2P_2)
   call init_I(fptr_dev_I2P_2, i)
   print *, 'F pointer initialization completed'
   call matmul_I(fptr_dev_I2P_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_I2P_2(i,i)); fptr_hos_I2P_2 = 0_I1P
   call omp_target_memcpy_f(fptr_hos_I2P_2, fptr_dev_I2P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_I2P_2(1,1), fptr_hos_I2P_2(i,i)
   call omp_target_free_f(fptr_dev_I2P_2)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_I2P_2)

   print *, ''
   print *, '                       I2P Host to Device                        '
   print *, ''
   allocate(fptr_hos_I2P_2(i,i)); fptr_hos_I2P_2 = 5_I2P
   call omp_target_alloc_f(fptr_dev_I2P_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I2P_2)
   call omp_target_memcpy_f(fptr_dev_I2P_2, fptr_hos_I2P_2, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_I2P_2 = 0_I2P
   call omp_target_memcpy_f(fptr_hos_I2P_2, fptr_dev_I2P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_I2P_2(1,1), fptr_hos_I2P_2(i,i)
   call omp_target_free_f(fptr_dev_I2P_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_I2P_2)

   print *, ''
   print *, '                       I4P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_I4P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I4P_2)
   call init_I(fptr_dev_I4P_2, i)
   print *, 'F pointer initialization completed'
   call matmul_I(fptr_dev_I4P_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_I4P_2(i,i)); fptr_hos_I4P_2 = 0_I1P
   call omp_target_memcpy_f(fptr_hos_I4P_2, fptr_dev_I4P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_I4P_2(1,1), fptr_hos_I4P_2(i,i)
   call omp_target_free_f(fptr_dev_I4P_2)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_I4P_2)

   print *, ''
   print *, '                       I4P Host to Device                        '
   print *, ''
   allocate(fptr_hos_I4P_2(i,i)); fptr_hos_I4P_2 = 5_I4P
   call omp_target_alloc_f(fptr_dev_I4P_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I4P_2)
   call omp_target_memcpy_f(fptr_dev_I4P_2, fptr_hos_I4P_2, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_I4P_2 = 0_I4P
   call omp_target_memcpy_f(fptr_hos_I4P_2, fptr_dev_I4P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_I4P_2(1,1), fptr_hos_I4P_2(i,i)
   call omp_target_free_f(fptr_dev_I4P_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_I4P_2)

   print *, ''
   print *, '                       I8P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_I8P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I8P_2)
   call init_I(fptr_dev_I8P_2, i)
   print *, 'F pointer initialization completed'
   call matmul_I(fptr_dev_I8P_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_I8P_2(i,i)); fptr_hos_I8P_2 = 0_I1P
   call omp_target_memcpy_f(fptr_hos_I8P_2, fptr_dev_I8P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_I8P_2(1,1), fptr_hos_I8P_2(i,i)
   call omp_target_free_f(fptr_dev_I8P_2)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_I8P_2)

   print *, ''
   print *, '                       I8P Host to Device                        '
   print *, ''
   allocate(fptr_hos_I8P_2(i,i)); fptr_hos_I8P_2 = 5_I8P
   call omp_target_alloc_f(fptr_dev_I8P_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_I8P_2)
   call omp_target_memcpy_f(fptr_dev_I8P_2, fptr_hos_I8P_2, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_I8P_2 = 0_I8P
   call omp_target_memcpy_f(fptr_hos_I8P_2, fptr_dev_I8P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_I8P_2(1,1), fptr_hos_I8P_2(i,i)
   call omp_target_free_f(fptr_dev_I8P_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_I8P_2)

   print *, ''
   print *, '- - - - - - - - - - - - - -Real arrays- - - - - - - - - - - - - -'
   print *, ''
   print *, '                       R4P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_R4P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_R4P_2)
   call init_R(fptr_dev_R4P_2, i)
   print *, 'F pointer initialization completed'
   call matmul_R(fptr_dev_R4P_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_R4P_2(i,i)); fptr_hos_R4P_2 = 0_I1P
   call omp_target_memcpy_f(fptr_hos_R4P_2, fptr_dev_R4P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_R4P_2(1,1), fptr_hos_R4P_2(i,i)
   call omp_target_free_f(fptr_dev_R4P_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_R4P_2)

   print *, ''
   print *, '                       R4P Host to Device                        '
   print *, ''
   allocate(fptr_hos_R4P_2(i,i)); fptr_hos_R4P_2 = 5.0_R4P
   call omp_target_alloc_f(fptr_dev_R4P_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_R4P_2)
   call omp_target_memcpy_f(fptr_dev_R4P_2, fptr_hos_R4P_2, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_R4P_2 = 0.0_R4P
   call omp_target_memcpy_f(fptr_hos_R4P_2, fptr_dev_R4P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_R4P_2(1,1), fptr_hos_R4P_2(i,i)
   call omp_target_free_f(fptr_dev_R4P_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_R4P_2)

   print *, ''
   print *, '                       R8P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_R8P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_R8P_2)
   call init_R(fptr_dev_R8P_2, i)
   print *, 'F pointer initialization completed'
   call matmul_R(fptr_dev_R8P_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_R8P_2(i,i)); fptr_hos_R8P_2 = 0_I1P
   call omp_target_memcpy_f(fptr_hos_R8P_2, fptr_dev_R8P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_R8P_2(1,1), fptr_hos_R8P_2(i,i)
   call omp_target_free_f(fptr_dev_R8P_2)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_R8P_2)

   print *, ''
   print *, '                       R8P Host to Device                        '
   print *, ''
   allocate(fptr_hos_R8P_2(i,i)); fptr_hos_R8P_2 = 5.0_R8P
   call omp_target_alloc_f(fptr_dev_R8P_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_R8P_2)
   call omp_target_memcpy_f(fptr_dev_R8P_2, fptr_hos_R8P_2, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_R8P_2 = 0.0_R8P
   call omp_target_memcpy_f(fptr_hos_R8P_2, fptr_dev_R8P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_R8P_2(1,1), fptr_hos_R8P_2(i,i)
   call omp_target_free_f(fptr_dev_R8P_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_R8P_2)

#if defined _R16P
   print *, ''
   print *, '                       R16P Device to Host                       '
   print *, ''
   call omp_target_alloc_f(fptr_dev_R16P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_R16P_2)
   call init_R(fptr_dev_R16P_2, i)
   print *, 'F pointer initialization completed'
   call matmul_R(fptr_dev_R16P_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_R16P_2(i,i)); fptr_hos_R16P_2 = 0_I1P
   call omp_target_memcpy_f(fptr_hos_R16P_2, fptr_dev_R16P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_R16P_2(1,1), fptr_hos_R16P_2(i,i)
   call omp_target_free_f(fptr_dev_R16P_2)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_R16P_2)

   print *, ''
   print *, '                       R16P Host to Device                       '
   print *, ''
   allocate(fptr_hos_R16P_2(i,i)); fptr_hos_R16P_2 = 5.0_R16P
   call omp_target_alloc_f(fptr_dev_R16P_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_R16P_2)
   call omp_target_memcpy_f(fptr_dev_R16P_2, fptr_hos_R16P_2, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_R16P_2 = 0.0_R16P
   call omp_target_memcpy_f(fptr_hos_R16P_2, fptr_dev_R16P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_R16P_2(1,1), fptr_hos_R16P_2(i,i)
   call omp_target_free_f(fptr_dev_R16P_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_R16P_2)
#endif

   print *, ''
   print *, '- - - - - - - - - - - - -Complex arrays- - - - - - - - - - - - - '
   print *, ''
   print *, '                       C4P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_C4P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_C4P_2)
   call init_C(fptr_dev_C4P_2, i)
   print *, 'F pointer initialization completed'
   call matmul_C(fptr_dev_C4P_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_C4P_2(i,i)); fptr_hos_C4P_2 = 0_I1P
   call omp_target_memcpy_f(fptr_hos_C4P_2, fptr_dev_C4P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_C4P_2(1,1), fptr_hos_C4P_2(i,i)
   call omp_target_free_f(fptr_dev_C4P_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_C4P_2)

   print *, ''
   print *, '                       C4P Host to Device                        '
   print *, ''
   allocate(fptr_hos_C4P_2(i,i)); fptr_hos_C4P_2 = (5.0_R4P, 5.0_R4P)
   call omp_target_alloc_f(fptr_dev_C4P_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_C4P_2)
   call omp_target_memcpy_f(fptr_dev_C4P_2, fptr_hos_C4P_2, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_C4P_2 = (0.0_R4P, 0.0_R4P)
   call omp_target_memcpy_f(fptr_hos_C4P_2, fptr_dev_C4P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_C4P_2(1,1), fptr_hos_C4P_2(i,i)
   call omp_target_free_f(fptr_dev_C4P_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_C4P_2)

   print *, ''
   print *, '                       C8P Device to Host                        '
   print *, ''
   call omp_target_alloc_f(fptr_dev_C8P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_C8P_2)
   call init_C(fptr_dev_C8P_2, i)
   print *, 'F pointer initialization completed'
   call matmul_C(fptr_dev_C8P_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_C8P_2(i,i)); fptr_hos_C8P_2 = 0_I1P
   call omp_target_memcpy_f(fptr_hos_C8P_2, fptr_dev_C8P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_C8P_2(1,1), fptr_hos_C8P_2(i,i)
   call omp_target_free_f(fptr_dev_C8P_2)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_C8P_2)

   print *, ''
   print *, '                       C8P Host to Device                        '
   print *, ''
   allocate(fptr_hos_C8P_2(i,i)); fptr_hos_C8P_2 = (5.0_R8P, 5.0_R8P)
   call omp_target_alloc_f(fptr_dev_C8P_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_C8P_2)
   call omp_target_memcpy_f(fptr_dev_C8P_2, fptr_hos_C8P_2, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_C8P_2 = (0.0_R8P, 0.0_R8P)
   call omp_target_memcpy_f(fptr_hos_C8P_2, fptr_dev_C8P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_C8P_2(1,1), fptr_hos_C8P_2(i,i)
   call omp_target_free_f(fptr_dev_C8P_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_C8P_2)

#if defined _R16P
   print *, ''
   print *, '                       C16P Device to Host                       '
   print *, ''
   call omp_target_alloc_f(fptr_dev_C16P_2, siz2, omp_default)
   print *, 'Is the pointer associated after omp_target_alloc_f? ', associated(fptr_dev_C16P_2)
   call init_C(fptr_dev_C16P_2, i)
   print *, 'F pointer initialization completed'
   call matmul_C(fptr_dev_C16P_2, i)
   print *, 'F pointer multiplication completed'
   allocate(fptr_hos_C16P_2(i,i)); fptr_hos_C16P_2 = 0_I1P
   call omp_target_memcpy_f(fptr_hos_C16P_2, fptr_dev_C16P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Device to host copy completed'
   print *, 'Host pointer has values: ', fptr_hos_C16P_2(1,1), fptr_hos_C16P_2(i,i)
   call omp_target_free_f(fptr_dev_C16P_2)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_C16P_2)

   print *, ''
   print *, '                       C16P Host to Device                       '
   print *, ''
   allocate(fptr_hos_C16P_2(i,i)); fptr_hos_C16P_2 = (5.0_R16P, 5.0_R16P)
   call omp_target_alloc_f(fptr_dev_C16P_2, siz2, omp_default)
   print *, 'Is the device pointer associated after omp_target_alloc_f? ', associated(fptr_dev_C16P_2)
   call omp_target_memcpy_f(fptr_dev_C16P_2, fptr_hos_C16P_2, ierr, 0_I4P, 0_I4P, &
                            omp_default, omp_initial)
   print *, 'Host to device copy completed'
   fptr_hos_C16P_2 = (0.0_R16P, 0.0_R16P)
   call omp_target_memcpy_f(fptr_hos_C16P_2, fptr_dev_C16P_2, ierr, 0_I4P, 0_I4P, &
                            omp_initial, omp_default)
   print *, 'Host pointer has values: ', fptr_hos_C16P_2(1,1), fptr_hos_C16P_2(i,i)
   call omp_target_free_f(fptr_dev_C16P_2, omp_default)
   print *, 'Device pointer deallocation completed'
   deallocate(fptr_hos_C16P_2)
#endif

endprogram test_falco
