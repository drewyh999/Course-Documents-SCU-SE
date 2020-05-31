#include <stdio.h>
#include <stdlib.h>
#include "defs.h"
#include "cache.h"


/* Here is an our naive implementation */
char rotate_descr[] = "Naive Row-wise Traversal of src";
void rotate(int dim, pixel *src, pixel *dst) {
    int base_size = 4;
    int sub_mat_i, j, sub_mat_l, n;

    for(n = 0;n < dim;n += base_size)
        for(sub_mat_l = 0;sub_mat_l<dim;sub_mat_l += base_size)
            for(j = n + base_size - 1;j >= n;j --)
                for(sub_mat_i = sub_mat_l;sub_mat_i < sub_mat_l + base_size;sub_mat_i ++)
                    COPY(&dst[PIXEL(dim-1-j,sub_mat_i,dim)], &src[PIXEL(sub_mat_i,j,dim)]);

	return;
}


/* Add additional functions to test here */
void register_rotate_functions() {
	add_rotate_function(&rotate, rotate_descr);
	
}

