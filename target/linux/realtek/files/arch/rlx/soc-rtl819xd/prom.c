/*
 * Copyright 2006, Realtek Semiconductor Corp.
 *
 * arch/rlx/rlxocp/prom.c
 *   Early initialization code for the RTL819xD
 *
 * Tony Wu (tonywu@realtek.com.tw)
 * Nov. 7, 2006
 */
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/string.h>
#include <asm/bootinfo.h>
#include <asm/addrspace.h>
#include <asm/page.h>
#include <asm/cpu.h>

//#include <asm/rlxbsp.h>

#include "bspcpu.h"

#if defined(CONFIG_RTL_819X)
#include "bspchip.h"
#endif

extern char arcs_cmdline[];

#ifdef CONFIG_EARLY_PRINTK
static int promcons_output __initdata = 0;
                                                                                                    
void unregister_prom_console(void)
{
    if (promcons_output) {
        promcons_output = 0;
    }
}
                                                                                                    
void disable_early_printk(void)
    __attribute__ ((alias("unregister_prom_console")));
#endif
                                                                                                    

const char *get_system_type(void)
{
    return "RTL819xD";
}

static __init void rtl819xd_init_cmdline(int argc, char **argv)
{
	int i;

	for (i = 0; i < argc; i++)
	{
		strlcat(arcs_cmdline, " ", sizeof(arcs_cmdline));
		strlcat(arcs_cmdline, argv[i], sizeof(arcs_cmdline));
	}
}

/* Do basic initialization */
void __init bsp_init(void)
{
    u_long mem_size;

    /*user CMLLINE created by menuconfig*/
    /*
    arcs_cmdline[0] = '\0';
    strcpy(arcs_cmdline, "console=ttyS0,38400");
    */

	rtl819xd_init_cmdline(fw_arg0, (char **)fw_arg1);

// customize cpu_mem_size in bspcpu.h
#ifdef cpu_mem_size
	mem_size = cpu_mem_size;
#elif defined(RTL_8198_NFBI_BOARD)
	mem_size = ((7 << 20)-16); //reserve 16 byte for firmware header;
#else
       	/*now: alway believe DRAM configuration register*/
        {
                unsigned int DCRvalue = 0;
                unsigned int bus_width = 0, chip_sel = 0, row_cnt = 0, col_cnt = 0,bank_cnt = 0;
 
                DCRvalue = ( (*(volatile unsigned int *)BSP_MC_MTCR0));
 
                /*bit 19,0:2 bank; 1: 4 bank*/
                switch(DCRvalue & 0x080000)
                {
                        case 0x0:
                                bank_cnt = 2;
                                break;
                        case 0x080000:
                                bank_cnt = 4;
                                break;
                        default:
                                bank_cnt = 0;
                                break;
                }
 
                /*bit 22~24: colomn count*/
                switch(DCRvalue & 0x01C00000)
                {
                        case 0x00000000:
                                col_cnt = 256;
                                break;
                        case 0x00400000:
                                col_cnt = 512;
                                break;
                        case 0x00800000:
                                col_cnt = 1024;
                                break;
                        case 0x00C00000:
                                col_cnt = 2048;
                                break;
                        case 0x01000000:
                                col_cnt = 4096;
                                break;
                        default:
                                printk("unknow colomn count(0x%x)\n",DCRvalue & 0x01C00000);
                                break;
                }
 
                /*bit 25~26: row count*/
                switch(DCRvalue & 0x06000000)
                {
                        case 0x00000000:
                                row_cnt = 2048;
                                break;
                        case 0x02000000:
                                row_cnt = 4096;
                                break;
                        case 0x04000000:
                                row_cnt = 8192;
                                break;
                        case 0x06000000:
                                row_cnt = 16384;
                                break;
			default:
				printk("unknow row count(0x%x)\n",DCRvalue & 0x06000000);
				break;
                }
 
                /*bit 27: chip select*/
                switch(DCRvalue & 0x08000000)
                {
                        case 0x0:
                                chip_sel = 1;
                                break;
                        case 0x08000000:
                                chip_sel = 2;
                                break;
			default:
				printk("unknow chip select(0x%x)\n",DCRvalue & 0x08000000);
				break;
                }
 
                /*bit 28~29: bus width*/
                switch(DCRvalue & 0x30000000)
                {
                        case 0x0:
                                bus_width = 8;
                                break;
                        case 0x10000000:
                                bus_width = 16;
                                break;
                        case 0x20000000:
                                bus_width = 32;
                                break;
                        default:
                                printk("bus width is reseved!\n");
                                break;
                }
 		
                /*total size(Byte)*/
                if((REG32(0xb800100C)&0x40000000)==0x40000000)
		{
			mem_size = (row_cnt * col_cnt *bank_cnt) * (bus_width >> 3) * chip_sel*2;
		}
		else
		mem_size = (row_cnt * col_cnt *bank_cnt) * (bus_width >> 3) * chip_sel;     
        }
#endif
    if(mem_size == (256 << 20))
    {
                mem_size =(256 << 20) -32;
    }

    add_memory_region(0, mem_size, BOOT_MEM_RAM);
}

void __init bsp_free_prom_memory(void)
{
  return;
}
