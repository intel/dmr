#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

void *omp_target_alloc_c(size_t byte_dimension, int device_number)
{
    float * ptr = omp_target_alloc(byte_dimension, device_number);
    if (ptr == NULL) {
        printf("Memory not allocated.\n");
        exit(0);
    }
    else {
        #pragma omp target is_device_ptr(ptr)
        #pragma omp teams distribute parallel for
        for (i = 0; i < n; ++i) {
            ptr[i] = 1.0;
        }
    }
    return ptr;
}

int omp_tgt_memcopy(void *host, void *device, int n_values, int hov, int dov)
{
    int i_device, i_host, status;
    size_t length, h_off, d_off;
    i_device = omp_get_default_device();
    i_host   = omp_get_initial_device();
    length   = n_values * sizeof(float);
    h_off    = hov      * sizeof(float);
    d_off    = dov      * sizeof(float);
    status = omp_target_memcpy(host, device, length, h_off, d_off, i_host, i_device);
    return status;
}
