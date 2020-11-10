/* ========================================================================= *
 * Copyright (C) 2020 Intel Corporation                                      *
 * This file is part of the FALCO library.                                   *
 *                                                                           *
 * For information on the license, see the LICENSE file.                     *
 * Further information: https://github.com/giacrossi/FALCO/                  *
 * SPDX-License-Identifier: BSD-3-Clause                                     *
 *                                                                           *
 * ========================================================================= *
 * Giacomo Rossi (Intel Corporation)                                         *
 * ========================================================================= */

#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

int omp_get_default_device_c(void)
{
  return omp_get_default_device();
}

int omp_get_initial_device_c(void)
{
  return omp_get_initial_device();
}

void *omp_target_alloc_c(size_t byte_dimension, int dev_id)
{
    void * ptr = omp_target_alloc(byte_dimension, dev_id);
    return ptr;
}

void omp_target_free_c(void * dev_ptr, int dev_id)
{
    omp_target_free(dev_ptr, dev_id);
}

int omp_target_is_present_c(void *ptr, int dev_id)
{
    return omp_target_is_present(ptr, dev_id);
}

void *omp_get_mapped_ptr_c(void *ptr, int dev_id)
{
    void * dev_ptr = omp_get_mapped_ptr(ptr, dev_id);
    return dev_ptr;
}

int omp_target_memcopy_c(void *dst, void *src, size_t length, size_t dst_off, size_t src_off,
                         int dst_dev_id, int src_dev_id)
{
    return omp_target_memcpy(dst, src, length, dst_off, src_off, dst_dev_id, src_dev_id);
}

int omp_target_memcopy_rect_c(void *dst, void *src, size_t elem_dim, int dims, size_t * volume,
                              size_t * dst_off, size_t * src_off, size_t * dst_dims, size_t * src_dims,
                              int dst_dev_id, int src_dev_id)
{
    return omp_target_memcpy_rect(dst, src, elem_dim, dims, volume, dst_off, src_off,
                                  dst_dims, src_dims, dst_dev_id, src_dev_id);
}

int omp_target_associate_ptr_c(void *hst_ptr, void *dev_ptr, size_t total_dim, size_t dev_off, int dev_id)
{
    return omp_target_associate_ptr(hst_ptr, dev_ptr, total_dim, dev_off, dev_id);
}

int omp_target_disassociate_ptr_c(void *hst_ptr, int dev_id)
{
    return omp_target_disassociate_ptr(hst_ptr, dev_id);
}
