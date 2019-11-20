/*
 * YAFFS: Yet another FFS. A NAND-flash specific file system. 
 * yaffs_ramdisk.c: yaffs ram disk component
 *
 * Copyright (C) 2002 Aleph One Ltd.
 *
 * Created by Charles Manning <charles@aleph1.co.uk>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 */

#ifndef __FILEEM2K_H__
#define __FILEEM2K_H__

#if 1

#define SIZE_IN_MB 128
//#define SIZE_IN_MB 8000
#define PAGE_DATA_SIZE (2048)
#define PAGE_SPARE_SIZE  (64)
#define PAGE_SIZE  (PAGE_DATA_SIZE + PAGE_SPARE_SIZE)
#define PAGES_PER_BLOCK (64)
#define BLOCK_DATA_SIZE (PAGE_DATA_SIZE * PAGES_PER_BLOCK)
#define BLOCK_SIZE (PAGES_PER_BLOCK * (PAGE_SIZE))
#define BLOCKS_PER_MB ((1024*1024)/BLOCK_DATA_SIZE)
#define SIZE_IN_BLOCKS (BLOCKS_PER_MB * SIZE_IN_MB)

#else

#define SIZE_IN_MB 128
#define PAGE_DATA_SIZE (512)
#define SPARE_SIZE  (16)
#define PAGE_SIZE  (PAGE_DATA_SIZE + SPARE_SIZE)
#define PAGES_PER_BLOCK (32)
#define BLOCK_DATA_SIZE (PAGE_SIZE * PAGES_PER_BLOCK)
#define BLOCK_SIZE (PAGES_PER_BLOCK * (PAGE_SIZE))
#define BLOCKS_PER_MB ((1024*1024)/BLOCK_DATA_SIZE)
#define SIZE_IN_BLOCKS (BLOCKS_PER_MB * SIZE_IN_MB)

#endif


int yflash_GetNumberOfBlocks(void);

#endif

