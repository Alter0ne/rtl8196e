/*
 * This file is subject to the terms and conditions of the GNU General Public
 * License.  See the file "COPYING" in the main directory of this archive
 * for more details.
 *
 * Copyright (C) 1994 - 2000 Ralf Baechle
 * Copyright (C) 1999, 2000 Silicon Graphics, Inc.
 * Kevin D. Kissell, kevink@mips.com and Carsten Langgaard, carstenl@mips.com
 * Copyright (C) 2000 MIPS Technologies, Inc.  All rights reserved.
 *
 * Modified for RLX processors
 * Copyright (C) 2008-2011 Tony Wu (tonywu@realtek.com)
 */
#include <linux/bug.h>
#include <linux/init.h>
#include <linux/module.h>
#include <linux/signal.h>
#include <linux/sched.h>
#include <linux/smp.h>
#include <linux/kernel.h>
#include <linux/errno.h>
#include <linux/string.h>
#include <linux/types.h>
#include <linux/pagemap.h>
#include <linux/ptrace.h>
#include <linux/mman.h>
#include <linux/mm.h>
#include <linux/bootmem.h>
#include <linux/highmem.h>
#include <linux/swap.h>
#include <linux/proc_fs.h>
#include <linux/pfn.h>
#include <linux/hardirq.h>
#include <linux/gfp.h>
#include <linux/kcore.h>

#include <asm/asm-offsets.h>
#include <asm/bootinfo.h>
#include <asm/cachectl.h>
#include <asm/cpu.h>
#include <asm/dma.h>
#include <asm/kmap_types.h>
#include <asm/mmu_context.h>
#include <asm/sections.h>
#include <asm/pgtable.h>
#include <asm/pgalloc.h>
#include <asm/tlb.h>
#include <asm/fixmap.h>

#define ENTER_CRITICAL(flags) local_irq_save(flags)
#define EXIT_CRITICAL(flags) local_irq_restore(flags)

/*
 * We have up to 8 empty zeroed pages so we can map one of the right colour
 * when needed.	 This is necessary only on R4000 / R4400 SC and MC versions
 * where we have to avoid VCED / VECI exceptions for good performance at
 * any price.  Since page is never written to after the initialization we
 * don't have to care about aliases on other CPUs.
 */
unsigned long empty_zero_page, zero_page_mask;
EXPORT_SYMBOL_GPL(empty_zero_page);

/*
 * Not static inline because used by IP27 special magic initialization code
 */
void setup_zero_pages(void)
{
	unsigned int order, i;
	struct page *page;

	order = 0;
	empty_zero_page = __get_free_pages(GFP_KERNEL | __GFP_ZERO, order);
	if (!empty_zero_page)
		panic("Oh boy, that early out of memory?");

	page = virt_to_page((void *)empty_zero_page);
	split_page(page, order);
	for (i = 0; i < (1 << order); i++, page++)
		mark_page_reserved(page);

	zero_page_mask = ((PAGE_SIZE << order) - 1) & PAGE_MASK;
}

static inline void kmap_coherent_init(void) {}

void copy_user_highpage(struct page *to, struct page *from,
	unsigned long vaddr, struct vm_area_struct *vma)
{
	void *vfrom, *vto;

	vto = kmap_atomic(to);
	vfrom = kmap_atomic(from);
	copy_page(vto, vfrom);
	kunmap_atomic(vfrom);
	flush_data_cache_page((unsigned long)vto);
	kunmap_atomic(vto);
	/* Make sure this page is cleared on other CPU's too before using it */
	smp_wmb();
}

void copy_to_user_page(struct vm_area_struct *vma,
	struct page *page, unsigned long vaddr, void *dst, const void *src,
	unsigned long len)
{
	memcpy(dst, src, len);
	if ((vma->vm_flags & VM_EXEC))
		flush_cache_page(vma, vaddr, page_to_pfn(page));
}

void copy_from_user_page(struct vm_area_struct *vma,
	struct page *page, unsigned long vaddr, void *dst, const void *src,
	unsigned long len)
{
	memcpy(dst, src, len);
}

void __init fixrange_init(unsigned long start, unsigned long end,
	pgd_t *pgd_base)
{
#if defined(CONFIG_HIGHMEM)
	pgd_t *pgd;
	pud_t *pud;
	pmd_t *pmd;
	pte_t *pte;
	int i, j, k;
	unsigned long vaddr;

	vaddr = start;
	i = __pgd_offset(vaddr);
	j = __pud_offset(vaddr);
	k = __pmd_offset(vaddr);
	pgd = pgd_base + i;

	for ( ; (i < PTRS_PER_PGD) && (vaddr < end); pgd++, i++) {
		pud = (pud_t *)pgd;
		for ( ; (j < PTRS_PER_PUD) && (vaddr < end); pud++, j++) {
			pmd = (pmd_t *)pud;
			for (; (k < PTRS_PER_PMD) && (vaddr < end); pmd++, k++) {
				if (pmd_none(*pmd)) {
					pte = (pte_t *) alloc_bootmem_low_pages(PAGE_SIZE);
					set_pmd(pmd, __pmd((unsigned long)pte));
					BUG_ON(pte != pte_offset_kernel(pmd, 0));
				}
				vaddr += PMD_SIZE;
			}
			k = 0;
		}
		j = 0;
	}
#endif
}

#ifndef CONFIG_NEED_MULTIPLE_NODES
int page_is_ram(unsigned long pagenr)
{
	int i;

	for (i = 0; i < boot_mem_map.nr_map; i++) {
		unsigned long addr, end;

		switch (boot_mem_map.map[i].type) {
		case BOOT_MEM_RAM:
		case BOOT_MEM_INIT_RAM:
			break;
		default:
			/* not usable memory */
			continue;
		}

		addr = PFN_UP(boot_mem_map.map[i].addr);
		end = PFN_DOWN(boot_mem_map.map[i].addr +
			       boot_mem_map.map[i].size);

		if (pagenr >= addr && pagenr < end)
			return 1;
	}

	return 0;
}

void __init paging_init(void)
{
	unsigned long max_zone_pfns[MAX_NR_ZONES];
	unsigned long lastpfn __maybe_unused;

	pagetable_init();

#ifdef CONFIG_HIGHMEM
	kmap_init();
#endif
	kmap_coherent_init();

#ifdef CONFIG_ZONE_DMA
	max_zone_pfns[ZONE_DMA] = MAX_DMA_PFN;
#endif
#ifdef CONFIG_ZONE_DMA32
	max_zone_pfns[ZONE_DMA32] = MAX_DMA32_PFN;
#endif
	max_zone_pfns[ZONE_NORMAL] = max_low_pfn;
	lastpfn = max_low_pfn;
#ifdef CONFIG_HIGHMEM
	max_zone_pfns[ZONE_HIGHMEM] = highend_pfn;
	lastpfn = highend_pfn;
#endif

	free_area_init_nodes(max_zone_pfns);
}

void __init mem_init(void)
{
	unsigned long codesize, reservedpages, datasize, initsize;
	unsigned long tmp, ram;

#ifdef CONFIG_HIGHMEM
#ifdef CONFIG_DISCONTIGMEM
#error "CONFIG_HIGHMEM and CONFIG_DISCONTIGMEM dont work together yet"
#endif
	max_mapnr = highend_pfn ? highend_pfn : max_low_pfn;
#else
	max_mapnr = max_low_pfn;
#endif
	high_memory = (void *) __va(max_low_pfn << PAGE_SHIFT);

	totalram_pages += free_all_bootmem();
	setup_zero_pages();	/* Setup zeroed pages.  */

	reservedpages = ram = 0;
	for (tmp = 0; tmp < max_low_pfn; tmp++)
		if (page_is_ram(tmp) && pfn_valid(tmp)) {
			ram++;
			if (PageReserved(pfn_to_page(tmp)))
				reservedpages++;
		}
	num_physpages = ram;

#ifdef CONFIG_HIGHMEM
	for (tmp = highstart_pfn; tmp < highend_pfn; tmp++) {
		struct page *page = pfn_to_page(tmp);

		if (!page_is_ram(tmp)) {
			SetPageReserved(page);
			continue;
		}
		free_highmem_page(page);
	}
	num_physpages += totalhigh_pages;
#endif

	codesize =  (unsigned long) &_etext - (unsigned long) &_text;
	datasize =  (unsigned long) &_edata - (unsigned long) &_etext;
	initsize =  (unsigned long) &__init_end - (unsigned long) &__init_begin;

	printk(KERN_INFO "Memory: %luk/%luk available (%ldk kernel code, "
	       "%ldk reserved, %ldk data, %ldk init, %ldk highmem)\n",
	       nr_free_pages() << (PAGE_SHIFT-10),
	       ram << (PAGE_SHIFT-10),
	       codesize >> 10,
	       reservedpages << (PAGE_SHIFT-10),
	       datasize >> 10,
	       initsize >> 10,
	       totalhigh_pages << (PAGE_SHIFT-10));
}
#endif /* !CONFIG_NEED_MULTIPLE_NODES */

void free_init_pages(const char *what, unsigned long begin, unsigned long end)
{
	unsigned long pfn;

	for (pfn = PFN_UP(begin); pfn < PFN_DOWN(end); pfn++) {
		struct page *page = pfn_to_page(pfn);
		void *addr = phys_to_virt(PFN_PHYS(pfn));

		memset(addr, POISON_FREE_INITMEM, PAGE_SIZE);
		free_reserved_page(page);
	}
	printk(KERN_INFO "Freeing %s: %ldk freed\n", what, (end - begin) >> 10);
}

#ifdef CONFIG_BLK_DEV_INITRD
void free_initrd_mem(unsigned long start, unsigned long end)
{
	free_reserved_area(start, end, POISON_FREE_INITMEM, "initrd");
}
#endif

void __init_refok free_initmem(void)
{
	extern void bsp_free_prom_memory(void);
	bsp_free_prom_memory();
	free_initmem_default(POISON_FREE_INITMEM);
}

#ifndef CONFIG_MIPS_PGD_C0_CONTEXT
unsigned long pgd_current[NR_CPUS];
#endif

/*
 * gcc 3.3 and older have trouble determining that PTRS_PER_PGD and PGD_ORDER
 * are constants.  So we use the variants from asm-offset.h until that gcc
 * will officially be retired.
 *
 * Align swapper_pg_dir in to 64K, allows its address to be loaded
 * with a single LUI instruction in the TLB handlers.  If we used
 * __aligned(64K), its size would get rounded up to the alignment
 * size, and waste space.  So we place it in its own section and align
 * it in the linker script.
 */
pgd_t swapper_pg_dir[_PTRS_PER_PGD] __section(.bss..swapper_pg_dir);
#ifndef __PAGETABLE_PMD_FOLDED
pmd_t invalid_pmd_table[PTRS_PER_PMD] __page_aligned_bss;
#endif
pte_t invalid_pte_table[PTRS_PER_PTE] __page_aligned_bss;
