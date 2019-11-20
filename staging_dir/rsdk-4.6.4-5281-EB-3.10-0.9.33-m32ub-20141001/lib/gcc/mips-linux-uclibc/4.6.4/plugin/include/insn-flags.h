/* Generated automatically by the program `genflags'
   from the machine description file `md'.  */

#ifndef GCC_INSN_FLAGS_H
#define GCC_INSN_FLAGS_H

#define HAVE_ls2_alu1_turn_enabled_insn (TUNE_LOONGSON_2EF)
#define HAVE_ls2_alu2_turn_enabled_insn (TUNE_LOONGSON_2EF)
#define HAVE_ls2_falu1_turn_enabled_insn (TUNE_LOONGSON_2EF)
#define HAVE_ls2_falu2_turn_enabled_insn (TUNE_LOONGSON_2EF)
#define HAVE_madsi_4x8x ((TARGET_RLX_NORAD && !TARGET_MIPS16) && TARGET_MAC)
#define HAVE_madsidi3_32bit_4x8x ((TARGET_RLX_NORAD && !TARGET_MIPS16) && TARGET_MAC)
#define HAVE_umadsidi3_32bit_4x8x ((TARGET_RLX_NORAD && !TARGET_MIPS16) && TARGET_MAC)
#define HAVE_madhl_4x8x ((TARGET_RLX_NORAD && !TARGET_MIPS16) && TARGET_MAC)
#define HAVE_msubsi_4x8x ((TARGET_RLX_NORAD && !TARGET_MIPS16) && TARGET_MAC)
#define HAVE_msubsidi3_32bit_4x8x ((TARGET_RLX_NORAD && !TARGET_MIPS16) && TARGET_MAC)
#define HAVE_umsubsidi3_32bit_4x8x ((TARGET_RLX_NORAD && !TARGET_MIPS16) && TARGET_MAC)
#define HAVE_msbhl_4x8x ((TARGET_RLX_NORAD && !TARGET_MIPS16) && TARGET_MAC)
#define HAVE_mszhl_4x8x ((TARGET_RLX_NORAD && !TARGET_MIPS16) && GET_CODE(operands[3]) == GET_CODE(operands[4]) && TARGET_MAC)
#define HAVE_mazhl_4x8x ((TARGET_RLX_NORAD && !TARGET_MIPS16) && GET_CODE(operands[3]) == GET_CODE(operands[4]) && TARGET_MAC)
#define HAVE_abssi2 (TARGET_RLX_RAD && !TARGET_MIPS16)
#define HAVE_sminsi3_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX)
#define HAVE_smaxsi3_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX)
#define HAVE_addrsi3_saturation_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX)
#define HAVE_addrsi3_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX)
#define HAVE_subrsi3_saturation_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX)
#define HAVE_subrsi3_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX)
#define HAVE_multasi3_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX)
#define HAVE_multadi3_5x8x (((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX) && (TARGET_64BIT))
#define HAVE_multasi3_highpart_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX)
#define HAVE_multausi3_highpart_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX)
#define HAVE_multasidi3_32bit_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX)
#define HAVE_multausidi3_32bit_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX)
#define HAVE_mazhl_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && GET_CODE(operands[3]) == GET_CODE(operands[4]) && TARGET_MAC)
#define HAVE_mszhl_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && GET_CODE(operands[3]) == GET_CODE(operands[4]) && TARGET_MAC)
#define HAVE_divamodsi4_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX)
#define HAVE_divaumodsi4_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX)
#define HAVE_maddasi_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX)
#define HAVE_maddasidi3_32bit_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX)
#define HAVE_maddausidi3_32bit_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX)
#define HAVE_madhl_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_MAC)
#define HAVE_msubasi_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX)
#define HAVE_msubasidi3_32bit_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX)
#define HAVE_msubausidi3_32bit_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_RADIAX)
#define HAVE_msbhl_5x8x ((TARGET_RLX_RAD && !TARGET_MIPS16)  && TARGET_MAC)
#define HAVE_init_lps0_lpe0 ((TARGET_RLX_RAD && !TARGET_MIPS16) \
    && (TARGET_ZERO_OVERHEAD || TARGET_UNSAFE_ZERO_OVERHEAD) \
    && flag_ivopts)
#define HAVE_init_lpc0 ((TARGET_RLX_RAD && !TARGET_MIPS16) \
    && (TARGET_ZERO_OVERHEAD || TARGET_UNSAFE_ZERO_OVERHEAD) \
    && flag_ivopts)
#define HAVE_doloop_begin_pattern ((TARGET_RLX_RAD && !TARGET_MIPS16) \
    && (TARGET_ZERO_OVERHEAD || TARGET_UNSAFE_ZERO_OVERHEAD) \
    && flag_ivopts)
#define HAVE_doloop_end_pattern ((TARGET_RLX_RAD && !TARGET_MIPS16) \
    && (TARGET_ZERO_OVERHEAD || TARGET_UNSAFE_ZERO_OVERHEAD) \
    && flag_ivopts)
#define HAVE_rlx_movv2hi ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_addr2 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_subr2 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_absr2 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_min2 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_max2 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_multa2_internal1 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_multa2_internal2 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_multa2_internal3 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_mulna2_internal1 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_mulna2_internal2 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_mulna2_internal3 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_madda2_internal1 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_madda2_internal2 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_madda2_internal3 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_msuba2_internal1 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_msuba2_internal2 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_msuba2_internal3 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_sllv2 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_srlv2 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_srav2 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_sltr2 ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_trap 1
#define HAVE_addsf3 (TARGET_HARD_FLOAT)
#define HAVE_adddf3 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_addv2sf3 (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_subsf3 (TARGET_HARD_FLOAT)
#define HAVE_subdf3 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_subv2sf3 (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_subsi3 1
#define HAVE_subdi3 (TARGET_64BIT)
#define HAVE_mulv2sf3 (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mulsi3_mul3_loongson (TARGET_LOONGSON_2EF || TARGET_LOONGSON_3A)
#define HAVE_muldi3_mul3_loongson ((TARGET_LOONGSON_2EF || TARGET_LOONGSON_3A) && (TARGET_64BIT))
#define HAVE_mulsi3_mul3 (ISA_HAS_MUL3)
#define HAVE_muldi3_mul3 ((ISA_HAS_DMUL3) && (TARGET_64BIT))
#define HAVE_mulsi3_internal 1
#define HAVE_muldi3_internal (TARGET_64BIT)
#define HAVE_mulsidi3_32bit (!TARGET_64BIT)
#define HAVE_umulsidi3_32bit (!TARGET_64BIT)
#define HAVE_mulsidi3_64bit (TARGET_64BIT && !ISA_HAS_DMUL3)
#define HAVE_umulsidi3_64bit (TARGET_64BIT && !ISA_HAS_DMUL3)
#define HAVE_mulsidi3_64bit_hilo (TARGET_64BIT)
#define HAVE_umulsidi3_64bit_hilo (TARGET_64BIT)
#define HAVE_mulsidi3_64bit_dmul (TARGET_64BIT && ISA_HAS_DMUL3)
#define HAVE_msubsidi4 (!TARGET_64BIT && (ISA_HAS_MSAC || GENERATE_MADD_MSUB || ISA_HAS_DSP))
#define HAVE_umsubsidi4 (!TARGET_64BIT && (ISA_HAS_MSAC || GENERATE_MADD_MSUB || ISA_HAS_DSP))
#define HAVE_smulsi3_highpart_internal (!ISA_HAS_MULHI)
#define HAVE_umulsi3_highpart_internal (!ISA_HAS_MULHI)
#define HAVE_smulsi3_highpart_mulhi_internal (ISA_HAS_MULHI)
#define HAVE_umulsi3_highpart_mulhi_internal (ISA_HAS_MULHI)
#define HAVE_smuldi3_highpart (TARGET_64BIT)
#define HAVE_umuldi3_highpart (TARGET_64BIT)
#define HAVE_mulditi3_internal (TARGET_64BIT)
#define HAVE_umulditi3_internal (TARGET_64BIT)
#define HAVE_madsi (TARGET_MAD)
#define HAVE_maddsidi4 ((TARGET_MAD || ISA_HAS_MACC || GENERATE_MADD_MSUB || ISA_HAS_DSP) \
   && !TARGET_64BIT)
#define HAVE_umaddsidi4 ((TARGET_MAD || ISA_HAS_MACC || GENERATE_MADD_MSUB || ISA_HAS_DSP) \
   && !TARGET_64BIT)
#define HAVE_divmodsi4 1
#define HAVE_divmoddi4 (TARGET_64BIT)
#define HAVE_udivmodsi4 1
#define HAVE_udivmoddi4 (TARGET_64BIT)
#define HAVE_divmodsi4_hilo_di (!TARGET_64BIT)
#define HAVE_udivmodsi4_hilo_di (!TARGET_64BIT)
#define HAVE_divmodsi4_hilo_ti (TARGET_64BIT)
#define HAVE_udivmodsi4_hilo_ti (TARGET_64BIT)
#define HAVE_divmoddi4_hilo_ti (TARGET_64BIT)
#define HAVE_udivmoddi4_hilo_ti (TARGET_64BIT)
#define HAVE_sqrtsf2 ((!ISA_MIPS1 || TARGET_RLX_FPU) && (TARGET_HARD_FLOAT))
#define HAVE_sqrtdf2 ((!ISA_MIPS1 || TARGET_RLX_FPU) && (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
#define HAVE_sqrtv2sf2 ((TARGET_SB1) && (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT))
#define HAVE_abssf2 ((!HONOR_NANS (SFmode)) && (TARGET_HARD_FLOAT))
#define HAVE_absdf2 ((!HONOR_NANS (DFmode)) && (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
#define HAVE_absv2sf2 ((!HONOR_NANS (V2SFmode)) && (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT))
#define HAVE_clzsi2 (ISA_HAS_CLZ_CLO)
#define HAVE_clzdi2 ((ISA_HAS_CLZ_CLO) && (TARGET_64BIT))
#define HAVE_popcountsi2 (ISA_HAS_POP)
#define HAVE_popcountdi2 ((ISA_HAS_POP) && (TARGET_64BIT))
#define HAVE_negsi2 1
#define HAVE_negdi2 (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_negsf2 ((!HONOR_NANS (SFmode)) && (TARGET_HARD_FLOAT))
#define HAVE_negdf2 ((!HONOR_NANS (DFmode)) && (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
#define HAVE_negv2sf2 ((!HONOR_NANS (V2SFmode)) && (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT))
#define HAVE_one_cmplsi2 1
#define HAVE_one_cmpldi2 (TARGET_64BIT)
#define HAVE_truncdfsf2 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_truncdiqi2 (TARGET_64BIT)
#define HAVE_truncdihi2 (TARGET_64BIT)
#define HAVE_truncdisi2 (TARGET_64BIT)
#define HAVE_extendsidi2 (TARGET_64BIT)
#define HAVE_extendsfdf2 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_fix_truncdfsi2_insn (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && ISA_HAS_TRUNC_W)
#define HAVE_fix_truncdfsi2_sat (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && ISA_HAS_TRUNC_W)
#define HAVE_fix_truncdfsi2_macro (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && !ISA_HAS_TRUNC_W)
#define HAVE_fix_truncsfsi2_insn (TARGET_HARD_FLOAT && ISA_HAS_TRUNC_W)
#define HAVE_fix_truncsfsi2_macro (TARGET_HARD_FLOAT && !ISA_HAS_TRUNC_W)
#define HAVE_fix_truncdfdi2 (TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT)
#define HAVE_fix_truncsfdi2 (TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT)
#define HAVE_floatsidf2 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_floatdidf2 (TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT)
#define HAVE_floatsisf2 (TARGET_HARD_FLOAT)
#define HAVE_floatdisf2 (TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT)
#define HAVE_extvsi (ISA_HAS_EXTS && UINTVAL (operands[2]) <= 32)
#define HAVE_extvdi ((ISA_HAS_EXTS && UINTVAL (operands[2]) <= 32) && (TARGET_64BIT))
#define HAVE_extzvsi (mips_use_ins_ext_p (operands[1], INTVAL (operands[2]), \
		       INTVAL (operands[3])))
#define HAVE_extzvdi ((mips_use_ins_ext_p (operands[1], INTVAL (operands[2]), \
		       INTVAL (operands[3]))) && (TARGET_64BIT))
#define HAVE_insvsi (mips_use_ins_ext_p (operands[0], INTVAL (operands[1]), \
		       INTVAL (operands[2])))
#define HAVE_insvdi ((mips_use_ins_ext_p (operands[0], INTVAL (operands[1]), \
		       INTVAL (operands[2]))) && (TARGET_64BIT))
#define HAVE_mov_lwl (!TARGET_MIPS16 && mips_mem_fits_mode_p (SImode, operands[1]))
#define HAVE_mov_ldl ((!TARGET_MIPS16 && mips_mem_fits_mode_p (DImode, operands[1])) && (TARGET_64BIT))
#define HAVE_mov_lwr (!TARGET_MIPS16 && mips_mem_fits_mode_p (SImode, operands[1]))
#define HAVE_mov_ldr ((!TARGET_MIPS16 && mips_mem_fits_mode_p (DImode, operands[1])) && (TARGET_64BIT))
#define HAVE_mov_swl (!TARGET_MIPS16 && mips_mem_fits_mode_p (SImode, operands[0]))
#define HAVE_mov_sdl ((!TARGET_MIPS16 && mips_mem_fits_mode_p (DImode, operands[0])) && (TARGET_64BIT))
#define HAVE_mov_swr (!TARGET_MIPS16 && mips_mem_fits_mode_p (SImode, operands[0]))
#define HAVE_mov_sdr ((!TARGET_MIPS16 && mips_mem_fits_mode_p (DImode, operands[0])) && (TARGET_64BIT))
#define HAVE_load_gotsi (Pmode == SImode)
#define HAVE_load_gotdi (Pmode == DImode)
#define HAVE_movcc (ISA_HAS_8CC && TARGET_HARD_FLOAT)
#define HAVE_mfhisi_di (!TARGET_64BIT)
#define HAVE_mfhisi_ti (TARGET_64BIT)
#define HAVE_mfhidi_ti (TARGET_64BIT)
#define HAVE_mthisi_di (!TARGET_64BIT)
#define HAVE_mthisi_ti (TARGET_64BIT)
#define HAVE_mthidi_ti (TARGET_64BIT)
#define HAVE_load_lowdf ((TARGET_HARD_FLOAT) && (!TARGET_64BIT && TARGET_DOUBLE_FLOAT))
#define HAVE_load_lowdi ((TARGET_HARD_FLOAT) && (!TARGET_64BIT && TARGET_DOUBLE_FLOAT))
#define HAVE_load_lowv2sf ((TARGET_HARD_FLOAT) && (!TARGET_64BIT && TARGET_PAIRED_SINGLE_FLOAT))
#define HAVE_load_lowv2si ((TARGET_HARD_FLOAT) && (!TARGET_64BIT && TARGET_LOONGSON_VECTORS))
#define HAVE_load_lowv4hi ((TARGET_HARD_FLOAT) && (!TARGET_64BIT && TARGET_LOONGSON_VECTORS))
#define HAVE_load_lowv8qi ((TARGET_HARD_FLOAT) && (!TARGET_64BIT && TARGET_LOONGSON_VECTORS))
#define HAVE_load_lowtf ((TARGET_HARD_FLOAT) && (TARGET_64BIT && TARGET_FLOAT64))
#define HAVE_load_highdf ((TARGET_HARD_FLOAT) && (!TARGET_64BIT && TARGET_DOUBLE_FLOAT))
#define HAVE_load_highdi ((TARGET_HARD_FLOAT) && (!TARGET_64BIT && TARGET_DOUBLE_FLOAT))
#define HAVE_load_highv2sf ((TARGET_HARD_FLOAT) && (!TARGET_64BIT && TARGET_PAIRED_SINGLE_FLOAT))
#define HAVE_load_highv2si ((TARGET_HARD_FLOAT) && (!TARGET_64BIT && TARGET_LOONGSON_VECTORS))
#define HAVE_load_highv4hi ((TARGET_HARD_FLOAT) && (!TARGET_64BIT && TARGET_LOONGSON_VECTORS))
#define HAVE_load_highv8qi ((TARGET_HARD_FLOAT) && (!TARGET_64BIT && TARGET_LOONGSON_VECTORS))
#define HAVE_load_hightf ((TARGET_HARD_FLOAT) && (TARGET_64BIT && TARGET_FLOAT64))
#define HAVE_store_worddf ((TARGET_HARD_FLOAT) && (!TARGET_64BIT && TARGET_DOUBLE_FLOAT))
#define HAVE_store_worddi ((TARGET_HARD_FLOAT) && (!TARGET_64BIT && TARGET_DOUBLE_FLOAT))
#define HAVE_store_wordv2sf ((TARGET_HARD_FLOAT) && (!TARGET_64BIT && TARGET_PAIRED_SINGLE_FLOAT))
#define HAVE_store_wordv2si ((TARGET_HARD_FLOAT) && (!TARGET_64BIT && TARGET_LOONGSON_VECTORS))
#define HAVE_store_wordv4hi ((TARGET_HARD_FLOAT) && (!TARGET_64BIT && TARGET_LOONGSON_VECTORS))
#define HAVE_store_wordv8qi ((TARGET_HARD_FLOAT) && (!TARGET_64BIT && TARGET_LOONGSON_VECTORS))
#define HAVE_store_wordtf ((TARGET_HARD_FLOAT) && (TARGET_64BIT && TARGET_FLOAT64))
#define HAVE_mthc1df ((TARGET_HARD_FLOAT && ISA_HAS_MXHC1) && (!TARGET_64BIT && TARGET_DOUBLE_FLOAT))
#define HAVE_mthc1di ((TARGET_HARD_FLOAT && ISA_HAS_MXHC1) && (!TARGET_64BIT && TARGET_DOUBLE_FLOAT))
#define HAVE_mthc1v2sf ((TARGET_HARD_FLOAT && ISA_HAS_MXHC1) && (!TARGET_64BIT && TARGET_PAIRED_SINGLE_FLOAT))
#define HAVE_mthc1v2si ((TARGET_HARD_FLOAT && ISA_HAS_MXHC1) && (!TARGET_64BIT && TARGET_LOONGSON_VECTORS))
#define HAVE_mthc1v4hi ((TARGET_HARD_FLOAT && ISA_HAS_MXHC1) && (!TARGET_64BIT && TARGET_LOONGSON_VECTORS))
#define HAVE_mthc1v8qi ((TARGET_HARD_FLOAT && ISA_HAS_MXHC1) && (!TARGET_64BIT && TARGET_LOONGSON_VECTORS))
#define HAVE_mthc1tf ((TARGET_HARD_FLOAT && ISA_HAS_MXHC1) && (TARGET_64BIT && TARGET_FLOAT64))
#define HAVE_mfhc1df ((TARGET_HARD_FLOAT && ISA_HAS_MXHC1) && (!TARGET_64BIT && TARGET_DOUBLE_FLOAT))
#define HAVE_mfhc1di ((TARGET_HARD_FLOAT && ISA_HAS_MXHC1) && (!TARGET_64BIT && TARGET_DOUBLE_FLOAT))
#define HAVE_mfhc1v2sf ((TARGET_HARD_FLOAT && ISA_HAS_MXHC1) && (!TARGET_64BIT && TARGET_PAIRED_SINGLE_FLOAT))
#define HAVE_mfhc1v2si ((TARGET_HARD_FLOAT && ISA_HAS_MXHC1) && (!TARGET_64BIT && TARGET_LOONGSON_VECTORS))
#define HAVE_mfhc1v4hi ((TARGET_HARD_FLOAT && ISA_HAS_MXHC1) && (!TARGET_64BIT && TARGET_LOONGSON_VECTORS))
#define HAVE_mfhc1v8qi ((TARGET_HARD_FLOAT && ISA_HAS_MXHC1) && (!TARGET_64BIT && TARGET_LOONGSON_VECTORS))
#define HAVE_mfhc1tf ((TARGET_HARD_FLOAT && ISA_HAS_MXHC1) && (TARGET_64BIT && TARGET_FLOAT64))
#define HAVE_loadgp_newabi_si ((mips_current_loadgp_style () == LOADGP_NEWABI) && (Pmode == SImode))
#define HAVE_loadgp_newabi_di ((mips_current_loadgp_style () == LOADGP_NEWABI) && (Pmode == DImode))
#define HAVE_loadgp_absolute_si ((mips_current_loadgp_style () == LOADGP_ABSOLUTE) && (Pmode == SImode))
#define HAVE_loadgp_absolute_di ((mips_current_loadgp_style () == LOADGP_ABSOLUTE) && (Pmode == DImode))
#define HAVE_loadgp_blockage 1
#define HAVE_loadgp_rtp_si ((mips_current_loadgp_style () == LOADGP_RTP) && (Pmode == SImode))
#define HAVE_loadgp_rtp_di ((mips_current_loadgp_style () == LOADGP_RTP) && (Pmode == DImode))
#define HAVE_copygp_mips16 (TARGET_MIPS16)
#define HAVE_potential_cprestore (!TARGET_CPRESTORE_DIRECTIVE || operands[2] == pic_offset_table_rtx)
#define HAVE_cprestore (TARGET_CPRESTORE_DIRECTIVE)
#define HAVE_use_cprestore 1
#define HAVE_sync (GENERATE_SYNC)
#define HAVE_synci (TARGET_SYNCI)
#define HAVE_rdhwr_synci_step_si ((ISA_HAS_SYNCI) && (Pmode == SImode))
#define HAVE_rdhwr_synci_step_di ((ISA_HAS_SYNCI) && (Pmode == DImode))
#define HAVE_clear_hazard_si ((ISA_HAS_SYNCI) && (Pmode == SImode))
#define HAVE_clear_hazard_di ((ISA_HAS_SYNCI) && (Pmode == DImode))
#define HAVE_mips_cache (ISA_HAS_CACHE)
#define HAVE_r10k_cache_barrier (ISA_HAS_CACHE)
#define HAVE_rotrsi3 (ISA_HAS_ROR)
#define HAVE_rotrdi3 ((ISA_HAS_ROR) && (TARGET_64BIT))
#define HAVE_sunordered_sf (TARGET_HARD_FLOAT)
#define HAVE_suneq_sf (TARGET_HARD_FLOAT)
#define HAVE_sunlt_sf (TARGET_HARD_FLOAT)
#define HAVE_sunle_sf (TARGET_HARD_FLOAT)
#define HAVE_seq_sf (TARGET_HARD_FLOAT)
#define HAVE_slt_sf (TARGET_HARD_FLOAT)
#define HAVE_sle_sf (TARGET_HARD_FLOAT)
#define HAVE_sunordered_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_suneq_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_sunlt_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_sunle_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_seq_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_slt_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_sle_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_sge_sf (TARGET_HARD_FLOAT)
#define HAVE_sgt_sf (TARGET_HARD_FLOAT)
#define HAVE_sunge_sf (TARGET_HARD_FLOAT)
#define HAVE_sungt_sf (TARGET_HARD_FLOAT)
#define HAVE_sge_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_sgt_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_sunge_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_sungt_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_indirect_jumpsi (Pmode == SImode)
#define HAVE_indirect_jumpdi (Pmode == DImode)
#define HAVE_tablejumpsi (Pmode == SImode)
#define HAVE_tablejumpdi (Pmode == DImode)
#define HAVE_blockage 1
#define HAVE_return_internal 1
#define HAVE_mips_eret 1
#define HAVE_mips_deret 1
#define HAVE_mips_di 1
#define HAVE_mips_ehb 1
#define HAVE_mips_rdpgpr 1
#define HAVE_cop0_move 1
#define HAVE_eh_set_lr_si (! TARGET_64BIT)
#define HAVE_eh_set_lr_di (TARGET_64BIT)
#define HAVE_restore_gp (TARGET_CALL_CLOBBERED_GP)
#define HAVE_move_gpsi 1
#define HAVE_move_gpdi (TARGET_64BIT)
#define HAVE_load_callsi ((TARGET_USE_GOT) && (Pmode == SImode))
#define HAVE_load_calldi ((TARGET_USE_GOT) && (Pmode == DImode))
#define HAVE_set_got_version (TARGET_USE_GOT)
#define HAVE_update_got_version (TARGET_USE_GOT)
#define HAVE_sibcall_internal (TARGET_SIBCALLS && SIBLING_CALL_P (insn))
#define HAVE_sibcall_value_internal (TARGET_SIBCALLS && SIBLING_CALL_P (insn))
#define HAVE_sibcall_value_multiple_internal (TARGET_SIBCALLS && SIBLING_CALL_P (insn))
#define HAVE_call_internal 1
#define HAVE_call_split (TARGET_SPLIT_CALLS)
#define HAVE_call_internal_direct 1
#define HAVE_call_direct_split (TARGET_SPLIT_CALLS)
#define HAVE_call_value_internal 1
#define HAVE_call_value_split (TARGET_SPLIT_CALLS)
#define HAVE_call_value_internal_direct 1
#define HAVE_call_value_direct_split (TARGET_SPLIT_CALLS)
#define HAVE_call_value_multiple_internal 1
#define HAVE_call_value_multiple_split (TARGET_SPLIT_CALLS)
#define HAVE_prefetch (ISA_HAS_PREFETCH && TARGET_EXPLICIT_RELOCS)
#define HAVE_nop 1
#define HAVE_hazard_nop 1
#define HAVE_consttable_tls_reloc (TARGET_MIPS16_PCREL_LOADS)
#define HAVE_consttable_int (TARGET_MIPS16)
#define HAVE_consttable_float (TARGET_MIPS16)
#define HAVE_align 1
#define HAVE_tls_get_tp_si ((HAVE_AS_TLS && !TARGET_MIPS16) && (Pmode == SImode))
#define HAVE_tls_get_tp_di ((HAVE_AS_TLS && !TARGET_MIPS16) && (Pmode == DImode))
#define HAVE_tls_get_tp_mips16_si ((HAVE_AS_TLS && TARGET_MIPS16 && !TARGET_MIPS16R) && (Pmode == SImode))
#define HAVE_tls_get_tp_mips16_di ((HAVE_AS_TLS && TARGET_MIPS16 && !TARGET_MIPS16R) && (Pmode == DImode))
#define HAVE_tls_get_tp_mips16r_si ((HAVE_AS_TLS && TARGET_MIPS16R) && (Pmode == SImode))
#define HAVE_tls_get_tp_mips16r_di ((HAVE_AS_TLS && TARGET_MIPS16R) && (Pmode == DImode))
#define HAVE_sync_compare_and_swapsi (GENERATE_LL_SC)
#define HAVE_sync_compare_and_swapdi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_compare_and_swap_12 (GENERATE_LL_SC)
#define HAVE_sync_addsi (GENERATE_LL_SC)
#define HAVE_sync_adddi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_sync_add_12 (GENERATE_LL_SC)
#define HAVE_sync_sub_12 (GENERATE_LL_SC)
#define HAVE_sync_ior_12 (GENERATE_LL_SC)
#define HAVE_sync_xor_12 (GENERATE_LL_SC)
#define HAVE_sync_and_12 (GENERATE_LL_SC)
#define HAVE_sync_old_add_12 (GENERATE_LL_SC)
#define HAVE_sync_old_sub_12 (GENERATE_LL_SC)
#define HAVE_sync_old_ior_12 (GENERATE_LL_SC)
#define HAVE_sync_old_xor_12 (GENERATE_LL_SC)
#define HAVE_sync_old_and_12 (GENERATE_LL_SC)
#define HAVE_sync_new_add_12 (GENERATE_LL_SC)
#define HAVE_sync_new_sub_12 (GENERATE_LL_SC)
#define HAVE_sync_new_ior_12 (GENERATE_LL_SC)
#define HAVE_sync_new_xor_12 (GENERATE_LL_SC)
#define HAVE_sync_new_and_12 (GENERATE_LL_SC)
#define HAVE_sync_nand_12 (GENERATE_LL_SC)
#define HAVE_sync_old_nand_12 (GENERATE_LL_SC)
#define HAVE_sync_new_nand_12 (GENERATE_LL_SC)
#define HAVE_sync_subsi (GENERATE_LL_SC)
#define HAVE_sync_subdi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_sync_old_addsi (GENERATE_LL_SC)
#define HAVE_sync_old_adddi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_sync_old_subsi (GENERATE_LL_SC)
#define HAVE_sync_old_subdi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_sync_new_addsi (GENERATE_LL_SC)
#define HAVE_sync_new_adddi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_sync_new_subsi (GENERATE_LL_SC)
#define HAVE_sync_new_subdi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_sync_iorsi (GENERATE_LL_SC)
#define HAVE_sync_xorsi (GENERATE_LL_SC)
#define HAVE_sync_andsi (GENERATE_LL_SC)
#define HAVE_sync_iordi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_sync_xordi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_sync_anddi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_sync_old_iorsi (GENERATE_LL_SC)
#define HAVE_sync_old_xorsi (GENERATE_LL_SC)
#define HAVE_sync_old_andsi (GENERATE_LL_SC)
#define HAVE_sync_old_iordi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_sync_old_xordi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_sync_old_anddi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_sync_new_iorsi (GENERATE_LL_SC)
#define HAVE_sync_new_xorsi (GENERATE_LL_SC)
#define HAVE_sync_new_andsi (GENERATE_LL_SC)
#define HAVE_sync_new_iordi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_sync_new_xordi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_sync_new_anddi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_sync_nandsi (GENERATE_LL_SC)
#define HAVE_sync_nanddi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_sync_old_nandsi (GENERATE_LL_SC)
#define HAVE_sync_old_nanddi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_sync_new_nandsi (GENERATE_LL_SC)
#define HAVE_sync_new_nanddi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_sync_lock_test_and_setsi (GENERATE_LL_SC)
#define HAVE_sync_lock_test_and_setdi ((GENERATE_LL_SC) && (TARGET_64BIT))
#define HAVE_test_and_set_12 (GENERATE_LL_SC)
#define HAVE_mips_cond_move_tf_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_pul_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_puu_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_pll_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_plu_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_vec_initv2sf_internal (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_vec_extractv2sf (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_alnv_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_addr_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_cvt_pw_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_cvt_ps_pw (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_mulr_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_cabs_cond_s ((TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT) && (TARGET_HARD_FLOAT))
#define HAVE_mips_cabs_cond_d ((TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT) && (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
#define HAVE_mips_c_cond_4s (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_cabs_cond_4s (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_c_cond_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_cabs_cond_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_sunordered_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_suneq_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_sunlt_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_sunle_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_seq_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_slt_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_sle_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_sge_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_sgt_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_sunge_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_sungt_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_bc1any4t (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_bc1any4f (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_bc1any2t (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_bc1any2f (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_rsqrt1_s ((TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT) && (TARGET_HARD_FLOAT))
#define HAVE_mips_rsqrt1_d ((TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT) && (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
#define HAVE_mips_rsqrt1_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_rsqrt2_s ((TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT) && (TARGET_HARD_FLOAT))
#define HAVE_mips_rsqrt2_d ((TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT) && (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
#define HAVE_mips_rsqrt2_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_recip1_s ((TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT) && (TARGET_HARD_FLOAT))
#define HAVE_mips_recip1_d ((TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT) && (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
#define HAVE_mips_recip1_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_recip2_s ((TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT) && (TARGET_HARD_FLOAT))
#define HAVE_mips_recip2_d ((TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT) && (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
#define HAVE_mips_recip2_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_addv2hi3 (ISA_HAS_DSP)
#define HAVE_addv4qi3 (ISA_HAS_DSP)
#define HAVE_mips_addq_s_w (ISA_HAS_DSP)
#define HAVE_mips_addq_s_ph (ISA_HAS_DSP)
#define HAVE_mips_addu_s_qb (ISA_HAS_DSP)
#define HAVE_subv2hi3 (ISA_HAS_DSP)
#define HAVE_subv4qi3 (ISA_HAS_DSP)
#define HAVE_mips_subq_s_w (ISA_HAS_DSP)
#define HAVE_mips_subq_s_ph (ISA_HAS_DSP)
#define HAVE_mips_subu_s_qb (ISA_HAS_DSP)
#define HAVE_mips_addsc (ISA_HAS_DSP)
#define HAVE_mips_addwc (ISA_HAS_DSP)
#define HAVE_mips_modsub (ISA_HAS_DSP)
#define HAVE_mips_raddu_w_qb (ISA_HAS_DSP)
#define HAVE_mips_absq_s_w (ISA_HAS_DSP)
#define HAVE_mips_absq_s_ph (ISA_HAS_DSP)
#define HAVE_mips_precrq_qb_ph (ISA_HAS_DSP)
#define HAVE_mips_precrq_ph_w (ISA_HAS_DSP)
#define HAVE_mips_precrq_rs_ph_w (ISA_HAS_DSP)
#define HAVE_mips_precrqu_s_qb_ph (ISA_HAS_DSP)
#define HAVE_mips_preceq_w_phl (ISA_HAS_DSP)
#define HAVE_mips_preceq_w_phr (ISA_HAS_DSP)
#define HAVE_mips_precequ_ph_qbl (ISA_HAS_DSP)
#define HAVE_mips_precequ_ph_qbr (ISA_HAS_DSP)
#define HAVE_mips_precequ_ph_qbla (ISA_HAS_DSP)
#define HAVE_mips_precequ_ph_qbra (ISA_HAS_DSP)
#define HAVE_mips_preceu_ph_qbl (ISA_HAS_DSP)
#define HAVE_mips_preceu_ph_qbr (ISA_HAS_DSP)
#define HAVE_mips_preceu_ph_qbla (ISA_HAS_DSP)
#define HAVE_mips_preceu_ph_qbra (ISA_HAS_DSP)
#define HAVE_mips_shll_ph (ISA_HAS_DSP)
#define HAVE_mips_shll_qb (ISA_HAS_DSP)
#define HAVE_mips_shll_s_w (ISA_HAS_DSP)
#define HAVE_mips_shll_s_ph (ISA_HAS_DSP)
#define HAVE_mips_shrl_qb (ISA_HAS_DSP)
#define HAVE_mips_shra_ph (ISA_HAS_DSP)
#define HAVE_mips_shra_r_w (ISA_HAS_DSP)
#define HAVE_mips_shra_r_ph (ISA_HAS_DSP)
#define HAVE_mips_muleu_s_ph_qbl (ISA_HAS_DSP)
#define HAVE_mips_muleu_s_ph_qbr (ISA_HAS_DSP)
#define HAVE_mips_mulq_rs_ph (ISA_HAS_DSP)
#define HAVE_mips_muleq_s_w_phl (ISA_HAS_DSP)
#define HAVE_mips_muleq_s_w_phr (ISA_HAS_DSP)
#define HAVE_mips_dpau_h_qbl (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_dpau_h_qbr (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_dpsu_h_qbl (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_dpsu_h_qbr (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_dpaq_s_w_ph (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_dpsq_s_w_ph (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_mulsaq_s_w_ph (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_dpaq_sa_l_w (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_dpsq_sa_l_w (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_maq_s_w_phl (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_maq_s_w_phr (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_maq_sa_w_phl (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_maq_sa_w_phr (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_bitrev (ISA_HAS_DSP)
#define HAVE_mips_insv (ISA_HAS_DSP)
#define HAVE_mips_repl_qb (ISA_HAS_DSP)
#define HAVE_mips_repl_ph (ISA_HAS_DSP)
#define HAVE_mips_cmp_eq_ph (ISA_HAS_DSP)
#define HAVE_mips_cmpu_eq_qb (ISA_HAS_DSP)
#define HAVE_mips_cmp_lt_ph (ISA_HAS_DSP)
#define HAVE_mips_cmpu_lt_qb (ISA_HAS_DSP)
#define HAVE_mips_cmp_le_ph (ISA_HAS_DSP)
#define HAVE_mips_cmpu_le_qb (ISA_HAS_DSP)
#define HAVE_mips_cmpgu_eq_qb (ISA_HAS_DSP)
#define HAVE_mips_cmpgu_lt_qb (ISA_HAS_DSP)
#define HAVE_mips_cmpgu_le_qb (ISA_HAS_DSP)
#define HAVE_mips_pick_ph (ISA_HAS_DSP)
#define HAVE_mips_pick_qb (ISA_HAS_DSP)
#define HAVE_mips_packrl_ph (ISA_HAS_DSP)
#define HAVE_mips_extr_w (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_extr_r_w (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_extr_rs_w (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_extr_s_h (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_extp (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_extpdp (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_shilo (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_mthlip (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_wrdsp (ISA_HAS_DSP)
#define HAVE_mips_rddsp (ISA_HAS_DSP)
#define HAVE_mips_lbux_si ((ISA_HAS_DSP) && (Pmode == SImode))
#define HAVE_mips_lbux_di ((ISA_HAS_DSP) && (Pmode == DImode))
#define HAVE_mips_lhx_si ((ISA_HAS_DSP) && (Pmode == SImode))
#define HAVE_mips_lhx_di ((ISA_HAS_DSP) && (Pmode == DImode))
#define HAVE_mips_lwx_si ((ISA_HAS_DSP) && (Pmode == SImode))
#define HAVE_mips_lwx_di ((ISA_HAS_DSP) && (Pmode == DImode))
#define HAVE_mips_bposge (ISA_HAS_DSP)
#define HAVE_mips_absq_s_qb (ISA_HAS_DSPR2)
#define HAVE_mips_addu_ph (ISA_HAS_DSPR2)
#define HAVE_mips_addu_s_ph (ISA_HAS_DSPR2)
#define HAVE_mips_adduh_qb (ISA_HAS_DSPR2)
#define HAVE_mips_adduh_r_qb (ISA_HAS_DSPR2)
#define HAVE_mips_append (ISA_HAS_DSPR2)
#define HAVE_mips_balign (ISA_HAS_DSPR2)
#define HAVE_mips_cmpgdu_eq_qb (ISA_HAS_DSPR2)
#define HAVE_mips_cmpgdu_lt_qb (ISA_HAS_DSPR2)
#define HAVE_mips_cmpgdu_le_qb (ISA_HAS_DSPR2)
#define HAVE_mips_dpa_w_ph (ISA_HAS_DSPR2 && !TARGET_64BIT)
#define HAVE_mips_dps_w_ph (ISA_HAS_DSPR2 && !TARGET_64BIT)
#define HAVE_mulv2hi3 (ISA_HAS_DSPR2)
#define HAVE_mips_mul_s_ph (ISA_HAS_DSPR2)
#define HAVE_mips_mulq_rs_w (ISA_HAS_DSPR2)
#define HAVE_mips_mulq_s_ph (ISA_HAS_DSPR2)
#define HAVE_mips_mulq_s_w (ISA_HAS_DSPR2)
#define HAVE_mips_mulsa_w_ph (ISA_HAS_DSPR2 && !TARGET_64BIT)
#define HAVE_mips_precr_qb_ph (ISA_HAS_DSPR2)
#define HAVE_mips_precr_sra_ph_w (ISA_HAS_DSPR2)
#define HAVE_mips_precr_sra_r_ph_w (ISA_HAS_DSPR2)
#define HAVE_mips_prepend (ISA_HAS_DSPR2)
#define HAVE_mips_shra_qb (ISA_HAS_DSPR2)
#define HAVE_mips_shra_r_qb (ISA_HAS_DSPR2)
#define HAVE_mips_shrl_ph (ISA_HAS_DSPR2)
#define HAVE_mips_subu_ph (ISA_HAS_DSPR2)
#define HAVE_mips_subu_s_ph (ISA_HAS_DSPR2)
#define HAVE_mips_subuh_qb (ISA_HAS_DSPR2)
#define HAVE_mips_subuh_r_qb (ISA_HAS_DSPR2)
#define HAVE_mips_addqh_ph (ISA_HAS_DSPR2)
#define HAVE_mips_addqh_r_ph (ISA_HAS_DSPR2)
#define HAVE_mips_addqh_w (ISA_HAS_DSPR2)
#define HAVE_mips_addqh_r_w (ISA_HAS_DSPR2)
#define HAVE_mips_subqh_ph (ISA_HAS_DSPR2)
#define HAVE_mips_subqh_r_ph (ISA_HAS_DSPR2)
#define HAVE_mips_subqh_w (ISA_HAS_DSPR2)
#define HAVE_mips_subqh_r_w (ISA_HAS_DSPR2)
#define HAVE_mips_dpax_w_ph (ISA_HAS_DSPR2 && !TARGET_64BIT)
#define HAVE_mips_dpsx_w_ph (ISA_HAS_DSPR2 && !TARGET_64BIT)
#define HAVE_mips_dpaqx_s_w_ph (ISA_HAS_DSPR2 && !TARGET_64BIT)
#define HAVE_mips_dpaqx_sa_w_ph (ISA_HAS_DSPR2 && !TARGET_64BIT)
#define HAVE_mips_dpsqx_s_w_ph (ISA_HAS_DSPR2 && !TARGET_64BIT)
#define HAVE_mips_dpsqx_sa_w_ph (ISA_HAS_DSPR2 && !TARGET_64BIT)
#define HAVE_addqq3 1
#define HAVE_addhq3 1
#define HAVE_addsq3 1
#define HAVE_adddq3 (TARGET_64BIT)
#define HAVE_adduqq3 1
#define HAVE_adduhq3 1
#define HAVE_addusq3 1
#define HAVE_addudq3 (TARGET_64BIT)
#define HAVE_addha3 1
#define HAVE_addsa3 1
#define HAVE_addda3 (TARGET_64BIT)
#define HAVE_adduha3 1
#define HAVE_addusa3 1
#define HAVE_adduda3 (TARGET_64BIT)
#define HAVE_usadduqq3 (ISA_HAS_DSP)
#define HAVE_usadduhq3 (ISA_HAS_DSPR2)
#define HAVE_usadduha3 (ISA_HAS_DSPR2)
#define HAVE_usaddv4uqq3 (ISA_HAS_DSP)
#define HAVE_usaddv2uhq3 (ISA_HAS_DSPR2)
#define HAVE_usaddv2uha3 (ISA_HAS_DSPR2)
#define HAVE_ssaddhq3 (ISA_HAS_DSP)
#define HAVE_ssaddsq3 (ISA_HAS_DSP)
#define HAVE_ssaddha3 (ISA_HAS_DSP)
#define HAVE_ssaddsa3 (ISA_HAS_DSP)
#define HAVE_ssaddv2hq3 (ISA_HAS_DSP)
#define HAVE_ssaddv2ha3 (ISA_HAS_DSP)
#define HAVE_subqq3 1
#define HAVE_subhq3 1
#define HAVE_subsq3 1
#define HAVE_subdq3 (TARGET_64BIT)
#define HAVE_subuqq3 1
#define HAVE_subuhq3 1
#define HAVE_subusq3 1
#define HAVE_subudq3 (TARGET_64BIT)
#define HAVE_subha3 1
#define HAVE_subsa3 1
#define HAVE_subda3 (TARGET_64BIT)
#define HAVE_subuha3 1
#define HAVE_subusa3 1
#define HAVE_subuda3 (TARGET_64BIT)
#define HAVE_ussubuqq3 (ISA_HAS_DSP)
#define HAVE_ussubuhq3 (ISA_HAS_DSPR2)
#define HAVE_ussubuha3 (ISA_HAS_DSPR2)
#define HAVE_ussubv4uqq3 (ISA_HAS_DSP)
#define HAVE_ussubv2uhq3 (ISA_HAS_DSPR2)
#define HAVE_ussubv2uha3 (ISA_HAS_DSPR2)
#define HAVE_sssubhq3 (ISA_HAS_DSP)
#define HAVE_sssubsq3 (ISA_HAS_DSP)
#define HAVE_sssubha3 (ISA_HAS_DSP)
#define HAVE_sssubsa3 (ISA_HAS_DSP)
#define HAVE_sssubv2hq3 (ISA_HAS_DSP)
#define HAVE_sssubv2ha3 (ISA_HAS_DSP)
#define HAVE_ssmulv2hq3 (ISA_HAS_DSP)
#define HAVE_ssmulhq3 (ISA_HAS_DSP)
#define HAVE_ssmulsq3 (ISA_HAS_DSPR2)
#define HAVE_ssmaddsqdq4 (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_ssmsubsqdq4 (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_movv2si_internal (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_movv4hi_internal (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_movv8qi_internal (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_vec_pack_ssat_v2si (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_vec_pack_ssat_v4hi (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_vec_pack_usat_v4hi (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_addv2si3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_addv4hi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_addv8qi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_paddd (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_ssaddv4hi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_ssaddv8qi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_usaddv4hi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_usaddv8qi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pandn_w (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pandn_h (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pandn_b (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pandn_d (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pavgh (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pavgb (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pcmpeqw (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pcmpeqh (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pcmpeqb (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pcmpgtw (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pcmpgth (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pcmpgtb (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pextrh (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pinsrh_0 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pinsrh_1 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pinsrh_2 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pinsrh_3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pmaddhw (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_smaxv4hi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_umaxv8qi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_sminv4hi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_uminv8qi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pmovmskb (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_umulv4hi3_highpart (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_smulv4hi3_highpart (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_mulv4hi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pmuluw (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pasubub (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_biadd (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_psadbh (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_pshufh (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_ashlv2si3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_ashlv4hi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_ashrv2si3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_ashrv4hi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_lshrv2si3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_lshrv4hi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_subv2si3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_subv4hi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_subv8qi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_loongson_psubd (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_sssubv4hi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_sssubv8qi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_ussubv4hi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_ussubv8qi3 (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_vec_interleave_highv2si (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_vec_interleave_highv4hi (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_vec_interleave_highv8qi (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_vec_interleave_lowv2si (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_vec_interleave_lowv4hi (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_vec_interleave_lowv8qi (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_divsi3 (TARGET_LOONGSON_2EF || TARGET_LOONGSON_3A)
#define HAVE_udivsi3 (TARGET_LOONGSON_2EF || TARGET_LOONGSON_3A)
#define HAVE_divdi3 ((TARGET_LOONGSON_2EF || TARGET_LOONGSON_3A) && (TARGET_64BIT))
#define HAVE_udivdi3 ((TARGET_LOONGSON_2EF || TARGET_LOONGSON_3A) && (TARGET_64BIT))
#define HAVE_modsi3 (TARGET_LOONGSON_2EF || TARGET_LOONGSON_3A)
#define HAVE_umodsi3 (TARGET_LOONGSON_2EF || TARGET_LOONGSON_3A)
#define HAVE_moddi3 ((TARGET_LOONGSON_2EF || TARGET_LOONGSON_3A) && (TARGET_64BIT))
#define HAVE_umoddi3 ((TARGET_LOONGSON_2EF || TARGET_LOONGSON_3A) && (TARGET_64BIT))
#define HAVE_doloop_begin ((TARGET_RLX_RAD && !TARGET_MIPS16) \
    && (TARGET_ZERO_OVERHEAD || TARGET_UNSAFE_ZERO_OVERHEAD) \
    && flag_ivopts)
#define HAVE_doloop_end ((TARGET_RLX_RAD && !TARGET_MIPS16) \
    && (TARGET_ZERO_OVERHEAD || TARGET_UNSAFE_ZERO_OVERHEAD) \
    && flag_ivopts)
#define HAVE_movv2hi ((TARGET_RLX_RAD && !TARGET_MIPS16) && TARGET_SIMD && TARGET_RADIAX)
#define HAVE_ctrapsi4 (ISA_HAS_COND_TRAP)
#define HAVE_ctrapdi4 ((ISA_HAS_COND_TRAP) && (TARGET_64BIT))
#define HAVE_addsi3 1
#define HAVE_adddi3 (TARGET_64BIT)
#define HAVE_mulsf3 (TARGET_HARD_FLOAT)
#define HAVE_muldf3 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_mulhisi3 ((TARGET_RLX && !TARGET_MIPS16) && TARGET_MAC)
#define HAVE_mulsi3 1
#define HAVE_muldi3 (TARGET_64BIT)
#define HAVE_mulsidi3 (mips_mulsidi3_gen_fn (SIGN_EXTEND) != NULL)
#define HAVE_umulsidi3 (mips_mulsidi3_gen_fn (ZERO_EXTEND) != NULL)
#define HAVE_smulsi3_highpart 1
#define HAVE_umulsi3_highpart 1
#define HAVE_mulditi3 (TARGET_64BIT)
#define HAVE_umulditi3 (TARGET_64BIT)
#define HAVE_divsf3 (TARGET_HARD_FLOAT)
#define HAVE_divdf3 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_divv2sf3 ((TARGET_SB1) && (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT))
#define HAVE_andsi3 1
#define HAVE_anddi3 (TARGET_64BIT)
#define HAVE_iorsi3 1
#define HAVE_iordi3 (TARGET_64BIT)
#define HAVE_xorsi3 1
#define HAVE_xordi3 (TARGET_64BIT)
#define HAVE_zero_extendsidi2 (TARGET_64BIT)
#define HAVE_zero_extendqisi2 1
#define HAVE_zero_extendqidi2 (TARGET_64BIT)
#define HAVE_zero_extendhisi2 1
#define HAVE_zero_extendhidi2 (TARGET_64BIT)
#define HAVE_zero_extendqihi2 1
#define HAVE_extendqisi2 1
#define HAVE_extendqidi2 (TARGET_64BIT)
#define HAVE_extendhisi2 1
#define HAVE_extendhidi2 (TARGET_64BIT)
#define HAVE_extendqihi2 1
#define HAVE_fix_truncdfsi2 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_fix_truncsfsi2 (TARGET_HARD_FLOAT)
#define HAVE_fixuns_truncdfsi2 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_fixuns_truncdfdi2 (TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT)
#define HAVE_fixuns_truncsfsi2 (TARGET_HARD_FLOAT)
#define HAVE_fixuns_truncsfdi2 (TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT)
#define HAVE_extv (!TARGET_MIPS16)
#define HAVE_extzv (!TARGET_MIPS16)
#define HAVE_insv (!TARGET_MIPS16)
#define HAVE_unspec_gotsi (Pmode == SImode)
#define HAVE_unspec_gotdi (Pmode == DImode)
#define HAVE_movdi 1
#define HAVE_movsi 1
#define HAVE_movv4qi (TARGET_DSP)
#define HAVE_movv2hq (TARGET_DSP)
#define HAVE_movv2uhq (TARGET_DSP)
#define HAVE_movv2ha (TARGET_DSP)
#define HAVE_movv2uha (TARGET_DSP)
#define HAVE_movv4qq (TARGET_DSP)
#define HAVE_movv4uqq (TARGET_DSP)
#define HAVE_reload_incc (ISA_HAS_8CC && TARGET_HARD_FLOAT)
#define HAVE_reload_outcc (ISA_HAS_8CC && TARGET_HARD_FLOAT)
#define HAVE_movhi 1
#define HAVE_movqi 1
#define HAVE_movsf 1
#define HAVE_movdf 1
#define HAVE_movti (TARGET_64BIT)
#define HAVE_movtf (TARGET_64BIT)
#define HAVE_movv2sf (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_move_doubleword_fprdf (!TARGET_64BIT && TARGET_DOUBLE_FLOAT)
#define HAVE_move_doubleword_fprdi (!TARGET_64BIT && TARGET_DOUBLE_FLOAT)
#define HAVE_move_doubleword_fprv2sf (!TARGET_64BIT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_move_doubleword_fprv2si (!TARGET_64BIT && TARGET_LOONGSON_VECTORS)
#define HAVE_move_doubleword_fprv4hi (!TARGET_64BIT && TARGET_LOONGSON_VECTORS)
#define HAVE_move_doubleword_fprv8qi (!TARGET_64BIT && TARGET_LOONGSON_VECTORS)
#define HAVE_move_doubleword_fprtf (TARGET_64BIT && TARGET_FLOAT64)
#define HAVE_load_const_gp_si (Pmode == SImode)
#define HAVE_load_const_gp_di (Pmode == DImode)
#define HAVE_clear_cache 1
#define HAVE_movmemsi (!TARGET_MIPS16 && !TARGET_MEMCPY)
#define HAVE_ashlsi3 1
#define HAVE_ashrsi3 1
#define HAVE_lshrsi3 1
#define HAVE_ashldi3 (TARGET_64BIT)
#define HAVE_ashrdi3 (TARGET_64BIT)
#define HAVE_lshrdi3 (TARGET_64BIT)
#define HAVE_cbranchsi4 1
#define HAVE_cbranchdi4 (TARGET_64BIT)
#define HAVE_cbranchsf4 (TARGET_HARD_FLOAT)
#define HAVE_cbranchdf4 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_condjump 1
#define HAVE_cstoresi4 1
#define HAVE_cstoredi4 (TARGET_64BIT)
#define HAVE_jump 1
#define HAVE_indirect_jump 1
#define HAVE_tablejump 1
#define HAVE_builtin_setjmp_setup (TARGET_USE_GOT)
#define HAVE_builtin_longjmp (TARGET_USE_GOT)
#define HAVE_prologue 1
#define HAVE_epilogue 1
#define HAVE_sibcall_epilogue 1
#define HAVE_return (mips_can_use_return_insn ())
#define HAVE_eh_return 1
#define HAVE_exception_receiver (TARGET_USE_GOT)
#define HAVE_nonlocal_goto_receiver (TARGET_USE_GOT)
#define HAVE_sibcall (TARGET_SIBCALLS)
#define HAVE_sibcall_value (TARGET_SIBCALLS)
#define HAVE_call 1
#define HAVE_call_value 1
#define HAVE_untyped_call 1
#define HAVE_movsicc (ISA_HAS_CONDMOVE)
#define HAVE_movdicc ((ISA_HAS_CONDMOVE) && (TARGET_64BIT))
#define HAVE_movsfcc ((ISA_HAS_FP_CONDMOVE) && (TARGET_HARD_FLOAT))
#define HAVE_movdfcc ((ISA_HAS_FP_CONDMOVE) && (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
#define HAVE_memory_barrier (GENERATE_SYNC)
#define HAVE_sync_compare_and_swapqi (GENERATE_LL_SC)
#define HAVE_sync_compare_and_swaphi (GENERATE_LL_SC)
#define HAVE_sync_addqi (GENERATE_LL_SC)
#define HAVE_sync_subqi (GENERATE_LL_SC)
#define HAVE_sync_iorqi (GENERATE_LL_SC)
#define HAVE_sync_xorqi (GENERATE_LL_SC)
#define HAVE_sync_andqi (GENERATE_LL_SC)
#define HAVE_sync_addhi (GENERATE_LL_SC)
#define HAVE_sync_subhi (GENERATE_LL_SC)
#define HAVE_sync_iorhi (GENERATE_LL_SC)
#define HAVE_sync_xorhi (GENERATE_LL_SC)
#define HAVE_sync_andhi (GENERATE_LL_SC)
#define HAVE_sync_old_addqi (GENERATE_LL_SC)
#define HAVE_sync_old_subqi (GENERATE_LL_SC)
#define HAVE_sync_old_iorqi (GENERATE_LL_SC)
#define HAVE_sync_old_xorqi (GENERATE_LL_SC)
#define HAVE_sync_old_andqi (GENERATE_LL_SC)
#define HAVE_sync_old_addhi (GENERATE_LL_SC)
#define HAVE_sync_old_subhi (GENERATE_LL_SC)
#define HAVE_sync_old_iorhi (GENERATE_LL_SC)
#define HAVE_sync_old_xorhi (GENERATE_LL_SC)
#define HAVE_sync_old_andhi (GENERATE_LL_SC)
#define HAVE_sync_new_addqi (GENERATE_LL_SC)
#define HAVE_sync_new_subqi (GENERATE_LL_SC)
#define HAVE_sync_new_iorqi (GENERATE_LL_SC)
#define HAVE_sync_new_xorqi (GENERATE_LL_SC)
#define HAVE_sync_new_andqi (GENERATE_LL_SC)
#define HAVE_sync_new_addhi (GENERATE_LL_SC)
#define HAVE_sync_new_subhi (GENERATE_LL_SC)
#define HAVE_sync_new_iorhi (GENERATE_LL_SC)
#define HAVE_sync_new_xorhi (GENERATE_LL_SC)
#define HAVE_sync_new_andhi (GENERATE_LL_SC)
#define HAVE_sync_nandqi (GENERATE_LL_SC)
#define HAVE_sync_nandhi (GENERATE_LL_SC)
#define HAVE_sync_old_nandqi (GENERATE_LL_SC)
#define HAVE_sync_old_nandhi (GENERATE_LL_SC)
#define HAVE_sync_new_nandqi (GENERATE_LL_SC)
#define HAVE_sync_new_nandhi (GENERATE_LL_SC)
#define HAVE_sync_lock_test_and_setqi (GENERATE_LL_SC)
#define HAVE_sync_lock_test_and_sethi (GENERATE_LL_SC)
#define HAVE_movv2sfcc (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_vec_initv2sf (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_vec_setv2sf (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_cvt_ps_s (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_cvt_s_pl (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_cvt_s_pu (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_abs_ps (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_scc_ps 1
#define HAVE_single_cc 1
#define HAVE_vcondv2sf (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_sminv2sf3 (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_smaxv2sf3 (TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT)
#define HAVE_mips_lbux (ISA_HAS_DSP)
#define HAVE_mips_lhx (ISA_HAS_DSP)
#define HAVE_mips_lwx (ISA_HAS_DSP)
#define HAVE_mips_madd (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_maddu (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_msub (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_mips_msubu (ISA_HAS_DSP && !TARGET_64BIT)
#define HAVE_movv2si (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_movv4hi (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_movv8qi (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_vec_initv2si (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_vec_initv4hi (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
#define HAVE_vec_initv8qi (TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS)
extern rtx        gen_ls2_alu1_turn_enabled_insn      (void);
extern rtx        gen_ls2_alu2_turn_enabled_insn      (void);
extern rtx        gen_ls2_falu1_turn_enabled_insn     (void);
extern rtx        gen_ls2_falu2_turn_enabled_insn     (void);
extern rtx        gen_madsi_4x8x                      (rtx, rtx, rtx);
extern rtx        gen_madsidi3_32bit_4x8x             (rtx, rtx, rtx, rtx);
extern rtx        gen_umadsidi3_32bit_4x8x            (rtx, rtx, rtx, rtx);
extern rtx        gen_madhl_4x8x                      (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_msubsi_4x8x                     (rtx, rtx, rtx);
extern rtx        gen_msubsidi3_32bit_4x8x            (rtx, rtx, rtx, rtx);
extern rtx        gen_umsubsidi3_32bit_4x8x           (rtx, rtx, rtx, rtx);
extern rtx        gen_msbhl_4x8x                      (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_mszhl_4x8x                      (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_mazhl_4x8x                      (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_abssi2                          (rtx, rtx);
extern rtx        gen_sminsi3_5x8x                    (rtx, rtx, rtx);
extern rtx        gen_smaxsi3_5x8x                    (rtx, rtx, rtx);
extern rtx        gen_addrsi3_saturation_5x8x         (rtx, rtx, rtx);
extern rtx        gen_addrsi3_5x8x                    (rtx, rtx, rtx);
extern rtx        gen_subrsi3_saturation_5x8x         (rtx, rtx, rtx);
extern rtx        gen_subrsi3_5x8x                    (rtx, rtx, rtx);
extern rtx        gen_multasi3_5x8x                   (rtx, rtx, rtx);
extern rtx        gen_multadi3_5x8x                   (rtx, rtx, rtx);
extern rtx        gen_multasi3_highpart_5x8x          (rtx, rtx, rtx);
extern rtx        gen_multausi3_highpart_5x8x         (rtx, rtx, rtx);
extern rtx        gen_multasidi3_32bit_5x8x           (rtx, rtx, rtx);
extern rtx        gen_multausidi3_32bit_5x8x          (rtx, rtx, rtx);
extern rtx        gen_mazhl_5x8x                      (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_mszhl_5x8x                      (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_divamodsi4_5x8x                 (rtx, rtx, rtx, rtx);
extern rtx        gen_divaumodsi4_5x8x                (rtx, rtx, rtx, rtx);
extern rtx        gen_maddasi_5x8x                    (rtx, rtx, rtx);
extern rtx        gen_maddasidi3_32bit_5x8x           (rtx, rtx, rtx, rtx);
extern rtx        gen_maddausidi3_32bit_5x8x          (rtx, rtx, rtx, rtx);
extern rtx        gen_madhl_5x8x                      (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_msubasi_5x8x                    (rtx, rtx, rtx);
extern rtx        gen_msubasidi3_32bit_5x8x           (rtx, rtx, rtx, rtx);
extern rtx        gen_msubausidi3_32bit_5x8x          (rtx, rtx, rtx, rtx);
extern rtx        gen_msbhl_5x8x                      (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_init_lps0_lpe0                  (rtx, rtx);
extern rtx        gen_init_lpc0                       (rtx);
extern rtx        gen_doloop_begin_pattern            (rtx);
extern rtx        gen_doloop_end_pattern              (rtx, rtx);
extern rtx        gen_rlx_movv2hi                     (rtx, rtx);
extern rtx        gen_addr2                           (rtx, rtx, rtx);
extern rtx        gen_subr2                           (rtx, rtx, rtx);
extern rtx        gen_absr2                           (rtx, rtx);
extern rtx        gen_min2                            (rtx, rtx, rtx);
extern rtx        gen_max2                            (rtx, rtx, rtx);
extern rtx        gen_multa2_internal1                (rtx, rtx, rtx);
extern rtx        gen_multa2_internal2                (rtx, rtx, rtx);
extern rtx        gen_multa2_internal3                (rtx, rtx, rtx);
extern rtx        gen_mulna2_internal1                (rtx, rtx, rtx);
extern rtx        gen_mulna2_internal2                (rtx, rtx, rtx);
extern rtx        gen_mulna2_internal3                (rtx, rtx, rtx);
extern rtx        gen_madda2_internal1                (rtx, rtx, rtx);
extern rtx        gen_madda2_internal2                (rtx, rtx, rtx);
extern rtx        gen_madda2_internal3                (rtx, rtx, rtx);
extern rtx        gen_msuba2_internal1                (rtx, rtx, rtx);
extern rtx        gen_msuba2_internal2                (rtx, rtx, rtx);
extern rtx        gen_msuba2_internal3                (rtx, rtx, rtx);
extern rtx        gen_sllv2                           (rtx, rtx, rtx);
extern rtx        gen_srlv2                           (rtx, rtx, rtx);
extern rtx        gen_srav2                           (rtx, rtx, rtx);
extern rtx        gen_sltr2                           (rtx, rtx, rtx);
extern rtx        gen_trap                            (void);
extern rtx        gen_addsf3                          (rtx, rtx, rtx);
extern rtx        gen_adddf3                          (rtx, rtx, rtx);
extern rtx        gen_addv2sf3                        (rtx, rtx, rtx);
extern rtx        gen_subsf3                          (rtx, rtx, rtx);
extern rtx        gen_subdf3                          (rtx, rtx, rtx);
extern rtx        gen_subv2sf3                        (rtx, rtx, rtx);
extern rtx        gen_subsi3                          (rtx, rtx, rtx);
extern rtx        gen_subdi3                          (rtx, rtx, rtx);
extern rtx        gen_mulv2sf3                        (rtx, rtx, rtx);
extern rtx        gen_mulsi3_mul3_loongson            (rtx, rtx, rtx);
extern rtx        gen_muldi3_mul3_loongson            (rtx, rtx, rtx);
extern rtx        gen_mulsi3_mul3                     (rtx, rtx, rtx);
extern rtx        gen_muldi3_mul3                     (rtx, rtx, rtx);
extern rtx        gen_mulsi3_internal                 (rtx, rtx, rtx);
extern rtx        gen_muldi3_internal                 (rtx, rtx, rtx);
extern rtx        gen_mulsidi3_32bit                  (rtx, rtx, rtx);
extern rtx        gen_umulsidi3_32bit                 (rtx, rtx, rtx);
extern rtx        gen_mulsidi3_64bit                  (rtx, rtx, rtx);
extern rtx        gen_umulsidi3_64bit                 (rtx, rtx, rtx);
extern rtx        gen_mulsidi3_64bit_hilo             (rtx, rtx, rtx);
extern rtx        gen_umulsidi3_64bit_hilo            (rtx, rtx, rtx);
extern rtx        gen_mulsidi3_64bit_dmul             (rtx, rtx, rtx);
extern rtx        gen_msubsidi4                       (rtx, rtx, rtx, rtx);
extern rtx        gen_umsubsidi4                      (rtx, rtx, rtx, rtx);
extern rtx        gen_smulsi3_highpart_internal       (rtx, rtx, rtx);
extern rtx        gen_umulsi3_highpart_internal       (rtx, rtx, rtx);
extern rtx        gen_smulsi3_highpart_mulhi_internal (rtx, rtx, rtx);
extern rtx        gen_umulsi3_highpart_mulhi_internal (rtx, rtx, rtx);
extern rtx        gen_smuldi3_highpart                (rtx, rtx, rtx);
extern rtx        gen_umuldi3_highpart                (rtx, rtx, rtx);
extern rtx        gen_mulditi3_internal               (rtx, rtx, rtx);
extern rtx        gen_umulditi3_internal              (rtx, rtx, rtx);
extern rtx        gen_madsi                           (rtx, rtx, rtx);
extern rtx        gen_maddsidi4                       (rtx, rtx, rtx, rtx);
extern rtx        gen_umaddsidi4                      (rtx, rtx, rtx, rtx);
extern rtx        gen_divmodsi4                       (rtx, rtx, rtx, rtx);
extern rtx        gen_divmoddi4                       (rtx, rtx, rtx, rtx);
extern rtx        gen_udivmodsi4                      (rtx, rtx, rtx, rtx);
extern rtx        gen_udivmoddi4                      (rtx, rtx, rtx, rtx);
extern rtx        gen_divmodsi4_hilo_di               (rtx, rtx, rtx);
extern rtx        gen_udivmodsi4_hilo_di              (rtx, rtx, rtx);
static inline rtx gen_divmoddi4_hilo_di               (rtx, rtx, rtx);
static inline rtx
gen_divmoddi4_hilo_di(rtx ARG_UNUSED (a), rtx ARG_UNUSED (b), rtx ARG_UNUSED (c))
{
  return 0;
}
static inline rtx gen_udivmoddi4_hilo_di              (rtx, rtx, rtx);
static inline rtx
gen_udivmoddi4_hilo_di(rtx ARG_UNUSED (a), rtx ARG_UNUSED (b), rtx ARG_UNUSED (c))
{
  return 0;
}
extern rtx        gen_divmodsi4_hilo_ti               (rtx, rtx, rtx);
extern rtx        gen_udivmodsi4_hilo_ti              (rtx, rtx, rtx);
extern rtx        gen_divmoddi4_hilo_ti               (rtx, rtx, rtx);
extern rtx        gen_udivmoddi4_hilo_ti              (rtx, rtx, rtx);
extern rtx        gen_sqrtsf2                         (rtx, rtx);
extern rtx        gen_sqrtdf2                         (rtx, rtx);
extern rtx        gen_sqrtv2sf2                       (rtx, rtx);
extern rtx        gen_abssf2                          (rtx, rtx);
extern rtx        gen_absdf2                          (rtx, rtx);
extern rtx        gen_absv2sf2                        (rtx, rtx);
extern rtx        gen_clzsi2                          (rtx, rtx);
extern rtx        gen_clzdi2                          (rtx, rtx);
extern rtx        gen_popcountsi2                     (rtx, rtx);
extern rtx        gen_popcountdi2                     (rtx, rtx);
extern rtx        gen_negsi2                          (rtx, rtx);
extern rtx        gen_negdi2                          (rtx, rtx);
extern rtx        gen_negsf2                          (rtx, rtx);
extern rtx        gen_negdf2                          (rtx, rtx);
extern rtx        gen_negv2sf2                        (rtx, rtx);
extern rtx        gen_one_cmplsi2                     (rtx, rtx);
extern rtx        gen_one_cmpldi2                     (rtx, rtx);
extern rtx        gen_truncdfsf2                      (rtx, rtx);
extern rtx        gen_truncdiqi2                      (rtx, rtx);
extern rtx        gen_truncdihi2                      (rtx, rtx);
extern rtx        gen_truncdisi2                      (rtx, rtx);
extern rtx        gen_extendsidi2                     (rtx, rtx);
extern rtx        gen_extendsfdf2                     (rtx, rtx);
extern rtx        gen_fix_truncdfsi2_insn             (rtx, rtx);
extern rtx        gen_fix_truncdfsi2_sat              (rtx, rtx);
extern rtx        gen_fix_truncdfsi2_macro            (rtx, rtx);
extern rtx        gen_fix_truncsfsi2_insn             (rtx, rtx);
extern rtx        gen_fix_truncsfsi2_macro            (rtx, rtx);
extern rtx        gen_fix_truncdfdi2                  (rtx, rtx);
extern rtx        gen_fix_truncsfdi2                  (rtx, rtx);
extern rtx        gen_floatsidf2                      (rtx, rtx);
extern rtx        gen_floatdidf2                      (rtx, rtx);
extern rtx        gen_floatsisf2                      (rtx, rtx);
extern rtx        gen_floatdisf2                      (rtx, rtx);
extern rtx        gen_extvsi                          (rtx, rtx, rtx, rtx);
extern rtx        gen_extvdi                          (rtx, rtx, rtx, rtx);
extern rtx        gen_extzvsi                         (rtx, rtx, rtx, rtx);
extern rtx        gen_extzvdi                         (rtx, rtx, rtx, rtx);
extern rtx        gen_insvsi                          (rtx, rtx, rtx, rtx);
extern rtx        gen_insvdi                          (rtx, rtx, rtx, rtx);
extern rtx        gen_mov_lwl                         (rtx, rtx, rtx);
extern rtx        gen_mov_ldl                         (rtx, rtx, rtx);
extern rtx        gen_mov_lwr                         (rtx, rtx, rtx, rtx);
extern rtx        gen_mov_ldr                         (rtx, rtx, rtx, rtx);
extern rtx        gen_mov_swl                         (rtx, rtx, rtx);
extern rtx        gen_mov_sdl                         (rtx, rtx, rtx);
extern rtx        gen_mov_swr                         (rtx, rtx, rtx);
extern rtx        gen_mov_sdr                         (rtx, rtx, rtx);
extern rtx        gen_load_gotsi                      (rtx, rtx, rtx);
extern rtx        gen_load_gotdi                      (rtx, rtx, rtx);
extern rtx        gen_movcc                           (rtx, rtx);
extern rtx        gen_mfhisi_di                       (rtx, rtx);
static inline rtx gen_mfhidi_di                       (rtx, rtx);
static inline rtx
gen_mfhidi_di(rtx ARG_UNUSED (a), rtx ARG_UNUSED (b))
{
  return 0;
}
extern rtx        gen_mfhisi_ti                       (rtx, rtx);
extern rtx        gen_mfhidi_ti                       (rtx, rtx);
extern rtx        gen_mthisi_di                       (rtx, rtx, rtx);
static inline rtx gen_mthidi_di                       (rtx, rtx, rtx);
static inline rtx
gen_mthidi_di(rtx ARG_UNUSED (a), rtx ARG_UNUSED (b), rtx ARG_UNUSED (c))
{
  return 0;
}
extern rtx        gen_mthisi_ti                       (rtx, rtx, rtx);
extern rtx        gen_mthidi_ti                       (rtx, rtx, rtx);
extern rtx        gen_load_lowdf                      (rtx, rtx);
extern rtx        gen_load_lowdi                      (rtx, rtx);
extern rtx        gen_load_lowv2sf                    (rtx, rtx);
extern rtx        gen_load_lowv2si                    (rtx, rtx);
extern rtx        gen_load_lowv4hi                    (rtx, rtx);
extern rtx        gen_load_lowv8qi                    (rtx, rtx);
extern rtx        gen_load_lowtf                      (rtx, rtx);
extern rtx        gen_load_highdf                     (rtx, rtx, rtx);
extern rtx        gen_load_highdi                     (rtx, rtx, rtx);
extern rtx        gen_load_highv2sf                   (rtx, rtx, rtx);
extern rtx        gen_load_highv2si                   (rtx, rtx, rtx);
extern rtx        gen_load_highv4hi                   (rtx, rtx, rtx);
extern rtx        gen_load_highv8qi                   (rtx, rtx, rtx);
extern rtx        gen_load_hightf                     (rtx, rtx, rtx);
extern rtx        gen_store_worddf                    (rtx, rtx, rtx);
extern rtx        gen_store_worddi                    (rtx, rtx, rtx);
extern rtx        gen_store_wordv2sf                  (rtx, rtx, rtx);
extern rtx        gen_store_wordv2si                  (rtx, rtx, rtx);
extern rtx        gen_store_wordv4hi                  (rtx, rtx, rtx);
extern rtx        gen_store_wordv8qi                  (rtx, rtx, rtx);
extern rtx        gen_store_wordtf                    (rtx, rtx, rtx);
extern rtx        gen_mthc1df                         (rtx, rtx, rtx);
extern rtx        gen_mthc1di                         (rtx, rtx, rtx);
extern rtx        gen_mthc1v2sf                       (rtx, rtx, rtx);
extern rtx        gen_mthc1v2si                       (rtx, rtx, rtx);
extern rtx        gen_mthc1v4hi                       (rtx, rtx, rtx);
extern rtx        gen_mthc1v8qi                       (rtx, rtx, rtx);
extern rtx        gen_mthc1tf                         (rtx, rtx, rtx);
extern rtx        gen_mfhc1df                         (rtx, rtx);
extern rtx        gen_mfhc1di                         (rtx, rtx);
extern rtx        gen_mfhc1v2sf                       (rtx, rtx);
extern rtx        gen_mfhc1v2si                       (rtx, rtx);
extern rtx        gen_mfhc1v4hi                       (rtx, rtx);
extern rtx        gen_mfhc1v8qi                       (rtx, rtx);
extern rtx        gen_mfhc1tf                         (rtx, rtx);
extern rtx        gen_loadgp_newabi_si                (rtx, rtx, rtx);
extern rtx        gen_loadgp_newabi_di                (rtx, rtx, rtx);
extern rtx        gen_loadgp_absolute_si              (rtx, rtx);
extern rtx        gen_loadgp_absolute_di              (rtx, rtx);
extern rtx        gen_loadgp_blockage                 (void);
extern rtx        gen_loadgp_rtp_si                   (rtx, rtx, rtx);
extern rtx        gen_loadgp_rtp_di                   (rtx, rtx, rtx);
extern rtx        gen_copygp_mips16                   (rtx, rtx);
extern rtx        gen_potential_cprestore             (rtx, rtx, rtx, rtx);
extern rtx        gen_cprestore                       (rtx, rtx);
extern rtx        gen_use_cprestore                   (rtx);
extern rtx        gen_sync                            (void);
extern rtx        gen_synci                           (rtx);
extern rtx        gen_rdhwr_synci_step_si             (rtx);
extern rtx        gen_rdhwr_synci_step_di             (rtx);
extern rtx        gen_clear_hazard_si                 (void);
extern rtx        gen_clear_hazard_di                 (void);
extern rtx        gen_mips_cache                      (rtx, rtx);
extern rtx        gen_r10k_cache_barrier              (void);
extern rtx        gen_rotrsi3                         (rtx, rtx, rtx);
extern rtx        gen_rotrdi3                         (rtx, rtx, rtx);
extern rtx        gen_sunordered_sf                   (rtx, rtx, rtx);
extern rtx        gen_suneq_sf                        (rtx, rtx, rtx);
extern rtx        gen_sunlt_sf                        (rtx, rtx, rtx);
extern rtx        gen_sunle_sf                        (rtx, rtx, rtx);
extern rtx        gen_seq_sf                          (rtx, rtx, rtx);
extern rtx        gen_slt_sf                          (rtx, rtx, rtx);
extern rtx        gen_sle_sf                          (rtx, rtx, rtx);
extern rtx        gen_sunordered_df                   (rtx, rtx, rtx);
extern rtx        gen_suneq_df                        (rtx, rtx, rtx);
extern rtx        gen_sunlt_df                        (rtx, rtx, rtx);
extern rtx        gen_sunle_df                        (rtx, rtx, rtx);
extern rtx        gen_seq_df                          (rtx, rtx, rtx);
extern rtx        gen_slt_df                          (rtx, rtx, rtx);
extern rtx        gen_sle_df                          (rtx, rtx, rtx);
extern rtx        gen_sge_sf                          (rtx, rtx, rtx);
extern rtx        gen_sgt_sf                          (rtx, rtx, rtx);
extern rtx        gen_sunge_sf                        (rtx, rtx, rtx);
extern rtx        gen_sungt_sf                        (rtx, rtx, rtx);
extern rtx        gen_sge_df                          (rtx, rtx, rtx);
extern rtx        gen_sgt_df                          (rtx, rtx, rtx);
extern rtx        gen_sunge_df                        (rtx, rtx, rtx);
extern rtx        gen_sungt_df                        (rtx, rtx, rtx);
extern rtx        gen_indirect_jumpsi                 (rtx);
extern rtx        gen_indirect_jumpdi                 (rtx);
extern rtx        gen_tablejumpsi                     (rtx, rtx);
extern rtx        gen_tablejumpdi                     (rtx, rtx);
extern rtx        gen_blockage                        (void);
extern rtx        gen_return_internal                 (rtx);
extern rtx        gen_mips_eret                       (void);
extern rtx        gen_mips_deret                      (void);
extern rtx        gen_mips_di                         (void);
extern rtx        gen_mips_ehb                        (void);
extern rtx        gen_mips_rdpgpr                     (rtx, rtx);
extern rtx        gen_cop0_move                       (rtx, rtx);
extern rtx        gen_eh_set_lr_si                    (rtx);
extern rtx        gen_eh_set_lr_di                    (rtx);
extern rtx        gen_restore_gp                      (void);
extern rtx        gen_move_gpsi                       (rtx, rtx);
extern rtx        gen_move_gpdi                       (rtx, rtx);
extern rtx        gen_load_callsi                     (rtx, rtx, rtx);
extern rtx        gen_load_calldi                     (rtx, rtx, rtx);
extern rtx        gen_set_got_version                 (void);
extern rtx        gen_update_got_version              (void);
extern rtx        gen_sibcall_internal                (rtx, rtx);
extern rtx        gen_sibcall_value_internal          (rtx, rtx, rtx);
extern rtx        gen_sibcall_value_multiple_internal (rtx, rtx, rtx, rtx);
extern rtx        gen_call_internal                   (rtx, rtx);
extern rtx        gen_call_split                      (rtx, rtx);
extern rtx        gen_call_internal_direct            (rtx, rtx);
extern rtx        gen_call_direct_split               (rtx, rtx);
extern rtx        gen_call_value_internal             (rtx, rtx, rtx);
extern rtx        gen_call_value_split                (rtx, rtx, rtx);
extern rtx        gen_call_value_internal_direct      (rtx, rtx, rtx);
extern rtx        gen_call_value_direct_split         (rtx, rtx, rtx);
extern rtx        gen_call_value_multiple_internal    (rtx, rtx, rtx, rtx);
extern rtx        gen_call_value_multiple_split       (rtx, rtx, rtx, rtx);
extern rtx        gen_prefetch                        (rtx, rtx, rtx);
extern rtx        gen_nop                             (void);
extern rtx        gen_hazard_nop                      (void);
extern rtx        gen_consttable_tls_reloc            (rtx, rtx);
extern rtx        gen_consttable_int                  (rtx, rtx);
extern rtx        gen_consttable_float                (rtx);
extern rtx        gen_align                           (rtx);
extern rtx        gen_tls_get_tp_si                   (rtx);
extern rtx        gen_tls_get_tp_di                   (rtx);
extern rtx        gen_tls_get_tp_mips16_si            (rtx, rtx);
extern rtx        gen_tls_get_tp_mips16_di            (rtx, rtx);
extern rtx        gen_tls_get_tp_mips16r_si           (rtx, rtx);
extern rtx        gen_tls_get_tp_mips16r_di           (rtx, rtx);
extern rtx        gen_sync_compare_and_swapsi         (rtx, rtx, rtx, rtx);
extern rtx        gen_sync_compare_and_swapdi         (rtx, rtx, rtx, rtx);
extern rtx        gen_compare_and_swap_12             (rtx, rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_sync_addsi                      (rtx, rtx);
extern rtx        gen_sync_adddi                      (rtx, rtx);
extern rtx        gen_sync_add_12                     (rtx, rtx, rtx, rtx);
extern rtx        gen_sync_sub_12                     (rtx, rtx, rtx, rtx);
extern rtx        gen_sync_ior_12                     (rtx, rtx, rtx, rtx);
extern rtx        gen_sync_xor_12                     (rtx, rtx, rtx, rtx);
extern rtx        gen_sync_and_12                     (rtx, rtx, rtx, rtx);
extern rtx        gen_sync_old_add_12                 (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_sync_old_sub_12                 (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_sync_old_ior_12                 (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_sync_old_xor_12                 (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_sync_old_and_12                 (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_sync_new_add_12                 (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_sync_new_sub_12                 (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_sync_new_ior_12                 (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_sync_new_xor_12                 (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_sync_new_and_12                 (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_sync_nand_12                    (rtx, rtx, rtx, rtx);
extern rtx        gen_sync_old_nand_12                (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_sync_new_nand_12                (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_sync_subsi                      (rtx, rtx);
extern rtx        gen_sync_subdi                      (rtx, rtx);
extern rtx        gen_sync_old_addsi                  (rtx, rtx, rtx);
extern rtx        gen_sync_old_adddi                  (rtx, rtx, rtx);
extern rtx        gen_sync_old_subsi                  (rtx, rtx, rtx);
extern rtx        gen_sync_old_subdi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_addsi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_adddi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_subsi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_subdi                  (rtx, rtx, rtx);
extern rtx        gen_sync_iorsi                      (rtx, rtx);
extern rtx        gen_sync_xorsi                      (rtx, rtx);
extern rtx        gen_sync_andsi                      (rtx, rtx);
extern rtx        gen_sync_iordi                      (rtx, rtx);
extern rtx        gen_sync_xordi                      (rtx, rtx);
extern rtx        gen_sync_anddi                      (rtx, rtx);
extern rtx        gen_sync_old_iorsi                  (rtx, rtx, rtx);
extern rtx        gen_sync_old_xorsi                  (rtx, rtx, rtx);
extern rtx        gen_sync_old_andsi                  (rtx, rtx, rtx);
extern rtx        gen_sync_old_iordi                  (rtx, rtx, rtx);
extern rtx        gen_sync_old_xordi                  (rtx, rtx, rtx);
extern rtx        gen_sync_old_anddi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_iorsi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_xorsi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_andsi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_iordi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_xordi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_anddi                  (rtx, rtx, rtx);
extern rtx        gen_sync_nandsi                     (rtx, rtx);
extern rtx        gen_sync_nanddi                     (rtx, rtx);
extern rtx        gen_sync_old_nandsi                 (rtx, rtx, rtx);
extern rtx        gen_sync_old_nanddi                 (rtx, rtx, rtx);
extern rtx        gen_sync_new_nandsi                 (rtx, rtx, rtx);
extern rtx        gen_sync_new_nanddi                 (rtx, rtx, rtx);
extern rtx        gen_sync_lock_test_and_setsi        (rtx, rtx, rtx);
extern rtx        gen_sync_lock_test_and_setdi        (rtx, rtx, rtx);
extern rtx        gen_test_and_set_12                 (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_mips_cond_move_tf_ps            (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_pul_ps                     (rtx, rtx, rtx);
extern rtx        gen_mips_puu_ps                     (rtx, rtx, rtx);
extern rtx        gen_mips_pll_ps                     (rtx, rtx, rtx);
extern rtx        gen_mips_plu_ps                     (rtx, rtx, rtx);
extern rtx        gen_vec_initv2sf_internal           (rtx, rtx, rtx);
extern rtx        gen_vec_extractv2sf                 (rtx, rtx, rtx);
extern rtx        gen_mips_alnv_ps                    (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_addr_ps                    (rtx, rtx, rtx);
extern rtx        gen_mips_cvt_pw_ps                  (rtx, rtx);
extern rtx        gen_mips_cvt_ps_pw                  (rtx, rtx);
extern rtx        gen_mips_mulr_ps                    (rtx, rtx, rtx);
extern rtx        gen_mips_cabs_cond_s                (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_cabs_cond_d                (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_c_cond_4s                  (rtx, rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_mips_cabs_cond_4s               (rtx, rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_mips_c_cond_ps                  (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_cabs_cond_ps               (rtx, rtx, rtx, rtx);
extern rtx        gen_sunordered_ps                   (rtx, rtx, rtx);
extern rtx        gen_suneq_ps                        (rtx, rtx, rtx);
extern rtx        gen_sunlt_ps                        (rtx, rtx, rtx);
extern rtx        gen_sunle_ps                        (rtx, rtx, rtx);
extern rtx        gen_seq_ps                          (rtx, rtx, rtx);
extern rtx        gen_slt_ps                          (rtx, rtx, rtx);
extern rtx        gen_sle_ps                          (rtx, rtx, rtx);
extern rtx        gen_sge_ps                          (rtx, rtx, rtx);
extern rtx        gen_sgt_ps                          (rtx, rtx, rtx);
extern rtx        gen_sunge_ps                        (rtx, rtx, rtx);
extern rtx        gen_sungt_ps                        (rtx, rtx, rtx);
extern rtx        gen_bc1any4t                        (rtx, rtx);
extern rtx        gen_bc1any4f                        (rtx, rtx);
extern rtx        gen_bc1any2t                        (rtx, rtx);
extern rtx        gen_bc1any2f                        (rtx, rtx);
extern rtx        gen_mips_rsqrt1_s                   (rtx, rtx);
extern rtx        gen_mips_rsqrt1_d                   (rtx, rtx);
extern rtx        gen_mips_rsqrt1_ps                  (rtx, rtx);
extern rtx        gen_mips_rsqrt2_s                   (rtx, rtx, rtx);
extern rtx        gen_mips_rsqrt2_d                   (rtx, rtx, rtx);
extern rtx        gen_mips_rsqrt2_ps                  (rtx, rtx, rtx);
extern rtx        gen_mips_recip1_s                   (rtx, rtx);
extern rtx        gen_mips_recip1_d                   (rtx, rtx);
extern rtx        gen_mips_recip1_ps                  (rtx, rtx);
extern rtx        gen_mips_recip2_s                   (rtx, rtx, rtx);
extern rtx        gen_mips_recip2_d                   (rtx, rtx, rtx);
extern rtx        gen_mips_recip2_ps                  (rtx, rtx, rtx);
extern rtx        gen_addv2hi3                        (rtx, rtx, rtx);
extern rtx        gen_addv4qi3                        (rtx, rtx, rtx);
extern rtx        gen_mips_addq_s_w                   (rtx, rtx, rtx);
extern rtx        gen_mips_addq_s_ph                  (rtx, rtx, rtx);
extern rtx        gen_mips_addu_s_qb                  (rtx, rtx, rtx);
extern rtx        gen_subv2hi3                        (rtx, rtx, rtx);
extern rtx        gen_subv4qi3                        (rtx, rtx, rtx);
extern rtx        gen_mips_subq_s_w                   (rtx, rtx, rtx);
extern rtx        gen_mips_subq_s_ph                  (rtx, rtx, rtx);
extern rtx        gen_mips_subu_s_qb                  (rtx, rtx, rtx);
extern rtx        gen_mips_addsc                      (rtx, rtx, rtx);
extern rtx        gen_mips_addwc                      (rtx, rtx, rtx);
extern rtx        gen_mips_modsub                     (rtx, rtx, rtx);
extern rtx        gen_mips_raddu_w_qb                 (rtx, rtx);
extern rtx        gen_mips_absq_s_w                   (rtx, rtx);
extern rtx        gen_mips_absq_s_ph                  (rtx, rtx);
extern rtx        gen_mips_precrq_qb_ph               (rtx, rtx, rtx);
extern rtx        gen_mips_precrq_ph_w                (rtx, rtx, rtx);
extern rtx        gen_mips_precrq_rs_ph_w             (rtx, rtx, rtx);
extern rtx        gen_mips_precrqu_s_qb_ph            (rtx, rtx, rtx);
extern rtx        gen_mips_preceq_w_phl               (rtx, rtx);
extern rtx        gen_mips_preceq_w_phr               (rtx, rtx);
extern rtx        gen_mips_precequ_ph_qbl             (rtx, rtx);
extern rtx        gen_mips_precequ_ph_qbr             (rtx, rtx);
extern rtx        gen_mips_precequ_ph_qbla            (rtx, rtx);
extern rtx        gen_mips_precequ_ph_qbra            (rtx, rtx);
extern rtx        gen_mips_preceu_ph_qbl              (rtx, rtx);
extern rtx        gen_mips_preceu_ph_qbr              (rtx, rtx);
extern rtx        gen_mips_preceu_ph_qbla             (rtx, rtx);
extern rtx        gen_mips_preceu_ph_qbra             (rtx, rtx);
extern rtx        gen_mips_shll_ph                    (rtx, rtx, rtx);
extern rtx        gen_mips_shll_qb                    (rtx, rtx, rtx);
extern rtx        gen_mips_shll_s_w                   (rtx, rtx, rtx);
extern rtx        gen_mips_shll_s_ph                  (rtx, rtx, rtx);
extern rtx        gen_mips_shrl_qb                    (rtx, rtx, rtx);
extern rtx        gen_mips_shra_ph                    (rtx, rtx, rtx);
extern rtx        gen_mips_shra_r_w                   (rtx, rtx, rtx);
extern rtx        gen_mips_shra_r_ph                  (rtx, rtx, rtx);
extern rtx        gen_mips_muleu_s_ph_qbl             (rtx, rtx, rtx);
extern rtx        gen_mips_muleu_s_ph_qbr             (rtx, rtx, rtx);
extern rtx        gen_mips_mulq_rs_ph                 (rtx, rtx, rtx);
extern rtx        gen_mips_muleq_s_w_phl              (rtx, rtx, rtx);
extern rtx        gen_mips_muleq_s_w_phr              (rtx, rtx, rtx);
extern rtx        gen_mips_dpau_h_qbl                 (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_dpau_h_qbr                 (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_dpsu_h_qbl                 (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_dpsu_h_qbr                 (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_dpaq_s_w_ph                (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_dpsq_s_w_ph                (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_mulsaq_s_w_ph              (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_dpaq_sa_l_w                (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_dpsq_sa_l_w                (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_maq_s_w_phl                (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_maq_s_w_phr                (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_maq_sa_w_phl               (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_maq_sa_w_phr               (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_bitrev                     (rtx, rtx);
extern rtx        gen_mips_insv                       (rtx, rtx, rtx);
extern rtx        gen_mips_repl_qb                    (rtx, rtx);
extern rtx        gen_mips_repl_ph                    (rtx, rtx);
extern rtx        gen_mips_cmp_eq_ph                  (rtx, rtx);
extern rtx        gen_mips_cmpu_eq_qb                 (rtx, rtx);
extern rtx        gen_mips_cmp_lt_ph                  (rtx, rtx);
extern rtx        gen_mips_cmpu_lt_qb                 (rtx, rtx);
extern rtx        gen_mips_cmp_le_ph                  (rtx, rtx);
extern rtx        gen_mips_cmpu_le_qb                 (rtx, rtx);
extern rtx        gen_mips_cmpgu_eq_qb                (rtx, rtx, rtx);
extern rtx        gen_mips_cmpgu_lt_qb                (rtx, rtx, rtx);
extern rtx        gen_mips_cmpgu_le_qb                (rtx, rtx, rtx);
extern rtx        gen_mips_pick_ph                    (rtx, rtx, rtx);
extern rtx        gen_mips_pick_qb                    (rtx, rtx, rtx);
extern rtx        gen_mips_packrl_ph                  (rtx, rtx, rtx);
extern rtx        gen_mips_extr_w                     (rtx, rtx, rtx);
extern rtx        gen_mips_extr_r_w                   (rtx, rtx, rtx);
extern rtx        gen_mips_extr_rs_w                  (rtx, rtx, rtx);
extern rtx        gen_mips_extr_s_h                   (rtx, rtx, rtx);
extern rtx        gen_mips_extp                       (rtx, rtx, rtx);
extern rtx        gen_mips_extpdp                     (rtx, rtx, rtx);
extern rtx        gen_mips_shilo                      (rtx, rtx, rtx);
extern rtx        gen_mips_mthlip                     (rtx, rtx, rtx);
extern rtx        gen_mips_wrdsp                      (rtx, rtx);
extern rtx        gen_mips_rddsp                      (rtx, rtx);
extern rtx        gen_mips_lbux_si                    (rtx, rtx, rtx);
extern rtx        gen_mips_lbux_di                    (rtx, rtx, rtx);
extern rtx        gen_mips_lhx_si                     (rtx, rtx, rtx);
extern rtx        gen_mips_lhx_di                     (rtx, rtx, rtx);
extern rtx        gen_mips_lwx_si                     (rtx, rtx, rtx);
extern rtx        gen_mips_lwx_di                     (rtx, rtx, rtx);
extern rtx        gen_mips_bposge                     (rtx, rtx);
extern rtx        gen_mips_absq_s_qb                  (rtx, rtx);
extern rtx        gen_mips_addu_ph                    (rtx, rtx, rtx);
extern rtx        gen_mips_addu_s_ph                  (rtx, rtx, rtx);
extern rtx        gen_mips_adduh_qb                   (rtx, rtx, rtx);
extern rtx        gen_mips_adduh_r_qb                 (rtx, rtx, rtx);
extern rtx        gen_mips_append                     (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_balign                     (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_cmpgdu_eq_qb               (rtx, rtx, rtx);
extern rtx        gen_mips_cmpgdu_lt_qb               (rtx, rtx, rtx);
extern rtx        gen_mips_cmpgdu_le_qb               (rtx, rtx, rtx);
extern rtx        gen_mips_dpa_w_ph                   (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_dps_w_ph                   (rtx, rtx, rtx, rtx);
extern rtx        gen_mulv2hi3                        (rtx, rtx, rtx);
extern rtx        gen_mips_mul_s_ph                   (rtx, rtx, rtx);
extern rtx        gen_mips_mulq_rs_w                  (rtx, rtx, rtx);
extern rtx        gen_mips_mulq_s_ph                  (rtx, rtx, rtx);
extern rtx        gen_mips_mulq_s_w                   (rtx, rtx, rtx);
extern rtx        gen_mips_mulsa_w_ph                 (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_precr_qb_ph                (rtx, rtx, rtx);
extern rtx        gen_mips_precr_sra_ph_w             (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_precr_sra_r_ph_w           (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_prepend                    (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_shra_qb                    (rtx, rtx, rtx);
extern rtx        gen_mips_shra_r_qb                  (rtx, rtx, rtx);
extern rtx        gen_mips_shrl_ph                    (rtx, rtx, rtx);
extern rtx        gen_mips_subu_ph                    (rtx, rtx, rtx);
extern rtx        gen_mips_subu_s_ph                  (rtx, rtx, rtx);
extern rtx        gen_mips_subuh_qb                   (rtx, rtx, rtx);
extern rtx        gen_mips_subuh_r_qb                 (rtx, rtx, rtx);
extern rtx        gen_mips_addqh_ph                   (rtx, rtx, rtx);
extern rtx        gen_mips_addqh_r_ph                 (rtx, rtx, rtx);
extern rtx        gen_mips_addqh_w                    (rtx, rtx, rtx);
extern rtx        gen_mips_addqh_r_w                  (rtx, rtx, rtx);
extern rtx        gen_mips_subqh_ph                   (rtx, rtx, rtx);
extern rtx        gen_mips_subqh_r_ph                 (rtx, rtx, rtx);
extern rtx        gen_mips_subqh_w                    (rtx, rtx, rtx);
extern rtx        gen_mips_subqh_r_w                  (rtx, rtx, rtx);
extern rtx        gen_mips_dpax_w_ph                  (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_dpsx_w_ph                  (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_dpaqx_s_w_ph               (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_dpaqx_sa_w_ph              (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_dpsqx_s_w_ph               (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_dpsqx_sa_w_ph              (rtx, rtx, rtx, rtx);
extern rtx        gen_addqq3                          (rtx, rtx, rtx);
extern rtx        gen_addhq3                          (rtx, rtx, rtx);
extern rtx        gen_addsq3                          (rtx, rtx, rtx);
extern rtx        gen_adddq3                          (rtx, rtx, rtx);
extern rtx        gen_adduqq3                         (rtx, rtx, rtx);
extern rtx        gen_adduhq3                         (rtx, rtx, rtx);
extern rtx        gen_addusq3                         (rtx, rtx, rtx);
extern rtx        gen_addudq3                         (rtx, rtx, rtx);
extern rtx        gen_addha3                          (rtx, rtx, rtx);
extern rtx        gen_addsa3                          (rtx, rtx, rtx);
extern rtx        gen_addda3                          (rtx, rtx, rtx);
extern rtx        gen_adduha3                         (rtx, rtx, rtx);
extern rtx        gen_addusa3                         (rtx, rtx, rtx);
extern rtx        gen_adduda3                         (rtx, rtx, rtx);
extern rtx        gen_usadduqq3                       (rtx, rtx, rtx);
extern rtx        gen_usadduhq3                       (rtx, rtx, rtx);
extern rtx        gen_usadduha3                       (rtx, rtx, rtx);
extern rtx        gen_usaddv4uqq3                     (rtx, rtx, rtx);
extern rtx        gen_usaddv2uhq3                     (rtx, rtx, rtx);
extern rtx        gen_usaddv2uha3                     (rtx, rtx, rtx);
extern rtx        gen_ssaddhq3                        (rtx, rtx, rtx);
extern rtx        gen_ssaddsq3                        (rtx, rtx, rtx);
extern rtx        gen_ssaddha3                        (rtx, rtx, rtx);
extern rtx        gen_ssaddsa3                        (rtx, rtx, rtx);
extern rtx        gen_ssaddv2hq3                      (rtx, rtx, rtx);
extern rtx        gen_ssaddv2ha3                      (rtx, rtx, rtx);
extern rtx        gen_subqq3                          (rtx, rtx, rtx);
extern rtx        gen_subhq3                          (rtx, rtx, rtx);
extern rtx        gen_subsq3                          (rtx, rtx, rtx);
extern rtx        gen_subdq3                          (rtx, rtx, rtx);
extern rtx        gen_subuqq3                         (rtx, rtx, rtx);
extern rtx        gen_subuhq3                         (rtx, rtx, rtx);
extern rtx        gen_subusq3                         (rtx, rtx, rtx);
extern rtx        gen_subudq3                         (rtx, rtx, rtx);
extern rtx        gen_subha3                          (rtx, rtx, rtx);
extern rtx        gen_subsa3                          (rtx, rtx, rtx);
extern rtx        gen_subda3                          (rtx, rtx, rtx);
extern rtx        gen_subuha3                         (rtx, rtx, rtx);
extern rtx        gen_subusa3                         (rtx, rtx, rtx);
extern rtx        gen_subuda3                         (rtx, rtx, rtx);
extern rtx        gen_ussubuqq3                       (rtx, rtx, rtx);
extern rtx        gen_ussubuhq3                       (rtx, rtx, rtx);
extern rtx        gen_ussubuha3                       (rtx, rtx, rtx);
extern rtx        gen_ussubv4uqq3                     (rtx, rtx, rtx);
extern rtx        gen_ussubv2uhq3                     (rtx, rtx, rtx);
extern rtx        gen_ussubv2uha3                     (rtx, rtx, rtx);
extern rtx        gen_sssubhq3                        (rtx, rtx, rtx);
extern rtx        gen_sssubsq3                        (rtx, rtx, rtx);
extern rtx        gen_sssubha3                        (rtx, rtx, rtx);
extern rtx        gen_sssubsa3                        (rtx, rtx, rtx);
extern rtx        gen_sssubv2hq3                      (rtx, rtx, rtx);
extern rtx        gen_sssubv2ha3                      (rtx, rtx, rtx);
extern rtx        gen_ssmulv2hq3                      (rtx, rtx, rtx);
extern rtx        gen_ssmulhq3                        (rtx, rtx, rtx);
extern rtx        gen_ssmulsq3                        (rtx, rtx, rtx);
extern rtx        gen_ssmaddsqdq4                     (rtx, rtx, rtx, rtx);
extern rtx        gen_ssmsubsqdq4                     (rtx, rtx, rtx, rtx);
extern rtx        gen_movv2si_internal                (rtx, rtx);
extern rtx        gen_movv4hi_internal                (rtx, rtx);
extern rtx        gen_movv8qi_internal                (rtx, rtx);
extern rtx        gen_vec_pack_ssat_v2si              (rtx, rtx, rtx);
extern rtx        gen_vec_pack_ssat_v4hi              (rtx, rtx, rtx);
extern rtx        gen_vec_pack_usat_v4hi              (rtx, rtx, rtx);
extern rtx        gen_addv2si3                        (rtx, rtx, rtx);
extern rtx        gen_addv4hi3                        (rtx, rtx, rtx);
extern rtx        gen_addv8qi3                        (rtx, rtx, rtx);
extern rtx        gen_loongson_paddd                  (rtx, rtx, rtx);
extern rtx        gen_ssaddv4hi3                      (rtx, rtx, rtx);
extern rtx        gen_ssaddv8qi3                      (rtx, rtx, rtx);
extern rtx        gen_usaddv4hi3                      (rtx, rtx, rtx);
extern rtx        gen_usaddv8qi3                      (rtx, rtx, rtx);
extern rtx        gen_loongson_pandn_w                (rtx, rtx, rtx);
extern rtx        gen_loongson_pandn_h                (rtx, rtx, rtx);
extern rtx        gen_loongson_pandn_b                (rtx, rtx, rtx);
extern rtx        gen_loongson_pandn_d                (rtx, rtx, rtx);
extern rtx        gen_loongson_pavgh                  (rtx, rtx, rtx);
extern rtx        gen_loongson_pavgb                  (rtx, rtx, rtx);
extern rtx        gen_loongson_pcmpeqw                (rtx, rtx, rtx);
extern rtx        gen_loongson_pcmpeqh                (rtx, rtx, rtx);
extern rtx        gen_loongson_pcmpeqb                (rtx, rtx, rtx);
extern rtx        gen_loongson_pcmpgtw                (rtx, rtx, rtx);
extern rtx        gen_loongson_pcmpgth                (rtx, rtx, rtx);
extern rtx        gen_loongson_pcmpgtb                (rtx, rtx, rtx);
extern rtx        gen_loongson_pextrh                 (rtx, rtx, rtx);
extern rtx        gen_loongson_pinsrh_0               (rtx, rtx, rtx);
extern rtx        gen_loongson_pinsrh_1               (rtx, rtx, rtx);
extern rtx        gen_loongson_pinsrh_2               (rtx, rtx, rtx);
extern rtx        gen_loongson_pinsrh_3               (rtx, rtx, rtx);
extern rtx        gen_loongson_pmaddhw                (rtx, rtx, rtx);
extern rtx        gen_smaxv4hi3                       (rtx, rtx, rtx);
extern rtx        gen_umaxv8qi3                       (rtx, rtx, rtx);
extern rtx        gen_sminv4hi3                       (rtx, rtx, rtx);
extern rtx        gen_uminv8qi3                       (rtx, rtx, rtx);
extern rtx        gen_loongson_pmovmskb               (rtx, rtx);
extern rtx        gen_umulv4hi3_highpart              (rtx, rtx, rtx);
extern rtx        gen_smulv4hi3_highpart              (rtx, rtx, rtx);
extern rtx        gen_mulv4hi3                        (rtx, rtx, rtx);
extern rtx        gen_loongson_pmuluw                 (rtx, rtx, rtx);
extern rtx        gen_loongson_pasubub                (rtx, rtx, rtx);
extern rtx        gen_loongson_biadd                  (rtx, rtx);
extern rtx        gen_loongson_psadbh                 (rtx, rtx, rtx);
extern rtx        gen_loongson_pshufh                 (rtx, rtx, rtx, rtx);
extern rtx        gen_ashlv2si3                       (rtx, rtx, rtx);
extern rtx        gen_ashlv4hi3                       (rtx, rtx, rtx);
extern rtx        gen_ashrv2si3                       (rtx, rtx, rtx);
extern rtx        gen_ashrv4hi3                       (rtx, rtx, rtx);
extern rtx        gen_lshrv2si3                       (rtx, rtx, rtx);
extern rtx        gen_lshrv4hi3                       (rtx, rtx, rtx);
extern rtx        gen_subv2si3                        (rtx, rtx, rtx);
extern rtx        gen_subv4hi3                        (rtx, rtx, rtx);
extern rtx        gen_subv8qi3                        (rtx, rtx, rtx);
extern rtx        gen_loongson_psubd                  (rtx, rtx, rtx);
extern rtx        gen_sssubv4hi3                      (rtx, rtx, rtx);
extern rtx        gen_sssubv8qi3                      (rtx, rtx, rtx);
extern rtx        gen_ussubv4hi3                      (rtx, rtx, rtx);
extern rtx        gen_ussubv8qi3                      (rtx, rtx, rtx);
extern rtx        gen_vec_interleave_highv2si         (rtx, rtx, rtx);
extern rtx        gen_vec_interleave_highv4hi         (rtx, rtx, rtx);
extern rtx        gen_vec_interleave_highv8qi         (rtx, rtx, rtx);
extern rtx        gen_vec_interleave_lowv2si          (rtx, rtx, rtx);
extern rtx        gen_vec_interleave_lowv4hi          (rtx, rtx, rtx);
extern rtx        gen_vec_interleave_lowv8qi          (rtx, rtx, rtx);
extern rtx        gen_divsi3                          (rtx, rtx, rtx);
extern rtx        gen_udivsi3                         (rtx, rtx, rtx);
extern rtx        gen_divdi3                          (rtx, rtx, rtx);
extern rtx        gen_udivdi3                         (rtx, rtx, rtx);
extern rtx        gen_modsi3                          (rtx, rtx, rtx);
extern rtx        gen_umodsi3                         (rtx, rtx, rtx);
extern rtx        gen_moddi3                          (rtx, rtx, rtx);
extern rtx        gen_umoddi3                         (rtx, rtx, rtx);
extern rtx        gen_doloop_begin                    (rtx, rtx, rtx, rtx);
extern rtx        gen_doloop_end                      (rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_movv2hi                         (rtx, rtx);
extern rtx        gen_ctrapsi4                        (rtx, rtx, rtx, rtx);
extern rtx        gen_ctrapdi4                        (rtx, rtx, rtx, rtx);
extern rtx        gen_addsi3                          (rtx, rtx, rtx);
extern rtx        gen_adddi3                          (rtx, rtx, rtx);
extern rtx        gen_mulsf3                          (rtx, rtx, rtx);
extern rtx        gen_muldf3                          (rtx, rtx, rtx);
extern rtx        gen_mulhisi3                        (rtx, rtx, rtx);
extern rtx        gen_mulsi3                          (rtx, rtx, rtx);
extern rtx        gen_muldi3                          (rtx, rtx, rtx);
extern rtx        gen_mulsidi3                        (rtx, rtx, rtx);
extern rtx        gen_umulsidi3                       (rtx, rtx, rtx);
extern rtx        gen_smulsi3_highpart                (rtx, rtx, rtx);
extern rtx        gen_umulsi3_highpart                (rtx, rtx, rtx);
extern rtx        gen_mulditi3                        (rtx, rtx, rtx);
extern rtx        gen_umulditi3                       (rtx, rtx, rtx);
extern rtx        gen_divsf3                          (rtx, rtx, rtx);
extern rtx        gen_divdf3                          (rtx, rtx, rtx);
extern rtx        gen_divv2sf3                        (rtx, rtx, rtx);
extern rtx        gen_andsi3                          (rtx, rtx, rtx);
extern rtx        gen_anddi3                          (rtx, rtx, rtx);
extern rtx        gen_iorsi3                          (rtx, rtx, rtx);
extern rtx        gen_iordi3                          (rtx, rtx, rtx);
extern rtx        gen_xorsi3                          (rtx, rtx, rtx);
extern rtx        gen_xordi3                          (rtx, rtx, rtx);
extern rtx        gen_zero_extendsidi2                (rtx, rtx);
extern rtx        gen_zero_extendqisi2                (rtx, rtx);
extern rtx        gen_zero_extendqidi2                (rtx, rtx);
extern rtx        gen_zero_extendhisi2                (rtx, rtx);
extern rtx        gen_zero_extendhidi2                (rtx, rtx);
extern rtx        gen_zero_extendqihi2                (rtx, rtx);
extern rtx        gen_extendqisi2                     (rtx, rtx);
extern rtx        gen_extendqidi2                     (rtx, rtx);
extern rtx        gen_extendhisi2                     (rtx, rtx);
extern rtx        gen_extendhidi2                     (rtx, rtx);
extern rtx        gen_extendqihi2                     (rtx, rtx);
extern rtx        gen_fix_truncdfsi2                  (rtx, rtx);
extern rtx        gen_fix_truncsfsi2                  (rtx, rtx);
extern rtx        gen_fixuns_truncdfsi2               (rtx, rtx);
extern rtx        gen_fixuns_truncdfdi2               (rtx, rtx);
extern rtx        gen_fixuns_truncsfsi2               (rtx, rtx);
extern rtx        gen_fixuns_truncsfdi2               (rtx, rtx);
extern rtx        gen_extv                            (rtx, rtx, rtx, rtx);
extern rtx        gen_extzv                           (rtx, rtx, rtx, rtx);
extern rtx        gen_insv                            (rtx, rtx, rtx, rtx);
extern rtx        gen_unspec_gotsi                    (rtx, rtx);
extern rtx        gen_unspec_gotdi                    (rtx, rtx);
extern rtx        gen_movdi                           (rtx, rtx);
extern rtx        gen_movsi                           (rtx, rtx);
extern rtx        gen_movv4qi                         (rtx, rtx);
extern rtx        gen_movv2hq                         (rtx, rtx);
extern rtx        gen_movv2uhq                        (rtx, rtx);
extern rtx        gen_movv2ha                         (rtx, rtx);
extern rtx        gen_movv2uha                        (rtx, rtx);
extern rtx        gen_movv4qq                         (rtx, rtx);
extern rtx        gen_movv4uqq                        (rtx, rtx);
extern rtx        gen_reload_incc                     (rtx, rtx, rtx);
extern rtx        gen_reload_outcc                    (rtx, rtx, rtx);
extern rtx        gen_movhi                           (rtx, rtx);
extern rtx        gen_movqi                           (rtx, rtx);
extern rtx        gen_movsf                           (rtx, rtx);
extern rtx        gen_movdf                           (rtx, rtx);
extern rtx        gen_movti                           (rtx, rtx);
extern rtx        gen_movtf                           (rtx, rtx);
extern rtx        gen_movv2sf                         (rtx, rtx);
extern rtx        gen_move_doubleword_fprdf           (rtx, rtx);
extern rtx        gen_move_doubleword_fprdi           (rtx, rtx);
extern rtx        gen_move_doubleword_fprv2sf         (rtx, rtx);
extern rtx        gen_move_doubleword_fprv2si         (rtx, rtx);
extern rtx        gen_move_doubleword_fprv4hi         (rtx, rtx);
extern rtx        gen_move_doubleword_fprv8qi         (rtx, rtx);
extern rtx        gen_move_doubleword_fprtf           (rtx, rtx);
extern rtx        gen_load_const_gp_si                (rtx);
extern rtx        gen_load_const_gp_di                (rtx);
extern rtx        gen_clear_cache                     (rtx, rtx);
extern rtx        gen_movmemsi                        (rtx, rtx, rtx, rtx);
extern rtx        gen_ashlsi3                         (rtx, rtx, rtx);
extern rtx        gen_ashrsi3                         (rtx, rtx, rtx);
extern rtx        gen_lshrsi3                         (rtx, rtx, rtx);
extern rtx        gen_ashldi3                         (rtx, rtx, rtx);
extern rtx        gen_ashrdi3                         (rtx, rtx, rtx);
extern rtx        gen_lshrdi3                         (rtx, rtx, rtx);
extern rtx        gen_cbranchsi4                      (rtx, rtx, rtx, rtx);
extern rtx        gen_cbranchdi4                      (rtx, rtx, rtx, rtx);
extern rtx        gen_cbranchsf4                      (rtx, rtx, rtx, rtx);
extern rtx        gen_cbranchdf4                      (rtx, rtx, rtx, rtx);
extern rtx        gen_condjump                        (rtx, rtx);
extern rtx        gen_cstoresi4                       (rtx, rtx, rtx, rtx);
extern rtx        gen_cstoredi4                       (rtx, rtx, rtx, rtx);
extern rtx        gen_jump                            (rtx);
extern rtx        gen_indirect_jump                   (rtx);
extern rtx        gen_tablejump                       (rtx, rtx);
extern rtx        gen_builtin_setjmp_setup            (rtx);
extern rtx        gen_builtin_longjmp                 (rtx);
extern rtx        gen_prologue                        (void);
extern rtx        gen_epilogue                        (void);
extern rtx        gen_sibcall_epilogue                (void);
extern rtx        gen_return                          (void);
extern rtx        gen_eh_return                       (rtx);
extern rtx        gen_exception_receiver              (void);
extern rtx        gen_nonlocal_goto_receiver          (void);
#define GEN_SIBCALL(A, B, C, D) gen_sibcall ((A), (B), (C), (D))
extern rtx        gen_sibcall                         (rtx, rtx, rtx, rtx);
#define GEN_SIBCALL_VALUE(A, B, C, D, E) gen_sibcall_value ((A), (B), (C), (D))
extern rtx        gen_sibcall_value                   (rtx, rtx, rtx, rtx);
#define GEN_CALL(A, B, C, D) gen_call ((A), (B), (C), (D))
extern rtx        gen_call                            (rtx, rtx, rtx, rtx);
#define GEN_CALL_VALUE(A, B, C, D, E) gen_call_value ((A), (B), (C), (D))
extern rtx        gen_call_value                      (rtx, rtx, rtx, rtx);
extern rtx        gen_untyped_call                    (rtx, rtx, rtx);
extern rtx        gen_movsicc                         (rtx, rtx, rtx, rtx);
extern rtx        gen_movdicc                         (rtx, rtx, rtx, rtx);
extern rtx        gen_movsfcc                         (rtx, rtx, rtx, rtx);
extern rtx        gen_movdfcc                         (rtx, rtx, rtx, rtx);
extern rtx        gen_memory_barrier                  (void);
extern rtx        gen_sync_compare_and_swapqi         (rtx, rtx, rtx, rtx);
extern rtx        gen_sync_compare_and_swaphi         (rtx, rtx, rtx, rtx);
extern rtx        gen_sync_addqi                      (rtx, rtx);
extern rtx        gen_sync_subqi                      (rtx, rtx);
extern rtx        gen_sync_iorqi                      (rtx, rtx);
extern rtx        gen_sync_xorqi                      (rtx, rtx);
extern rtx        gen_sync_andqi                      (rtx, rtx);
extern rtx        gen_sync_addhi                      (rtx, rtx);
extern rtx        gen_sync_subhi                      (rtx, rtx);
extern rtx        gen_sync_iorhi                      (rtx, rtx);
extern rtx        gen_sync_xorhi                      (rtx, rtx);
extern rtx        gen_sync_andhi                      (rtx, rtx);
extern rtx        gen_sync_old_addqi                  (rtx, rtx, rtx);
extern rtx        gen_sync_old_subqi                  (rtx, rtx, rtx);
extern rtx        gen_sync_old_iorqi                  (rtx, rtx, rtx);
extern rtx        gen_sync_old_xorqi                  (rtx, rtx, rtx);
extern rtx        gen_sync_old_andqi                  (rtx, rtx, rtx);
extern rtx        gen_sync_old_addhi                  (rtx, rtx, rtx);
extern rtx        gen_sync_old_subhi                  (rtx, rtx, rtx);
extern rtx        gen_sync_old_iorhi                  (rtx, rtx, rtx);
extern rtx        gen_sync_old_xorhi                  (rtx, rtx, rtx);
extern rtx        gen_sync_old_andhi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_addqi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_subqi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_iorqi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_xorqi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_andqi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_addhi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_subhi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_iorhi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_xorhi                  (rtx, rtx, rtx);
extern rtx        gen_sync_new_andhi                  (rtx, rtx, rtx);
extern rtx        gen_sync_nandqi                     (rtx, rtx);
extern rtx        gen_sync_nandhi                     (rtx, rtx);
extern rtx        gen_sync_old_nandqi                 (rtx, rtx, rtx);
extern rtx        gen_sync_old_nandhi                 (rtx, rtx, rtx);
extern rtx        gen_sync_new_nandqi                 (rtx, rtx, rtx);
extern rtx        gen_sync_new_nandhi                 (rtx, rtx, rtx);
extern rtx        gen_sync_lock_test_and_setqi        (rtx, rtx, rtx);
extern rtx        gen_sync_lock_test_and_sethi        (rtx, rtx, rtx);
extern rtx        gen_movv2sfcc                       (rtx, rtx, rtx, rtx);
extern rtx        gen_vec_initv2sf                    (rtx, rtx);
extern rtx        gen_vec_setv2sf                     (rtx, rtx, rtx);
extern rtx        gen_mips_cvt_ps_s                   (rtx, rtx, rtx);
extern rtx        gen_mips_cvt_s_pl                   (rtx, rtx);
extern rtx        gen_mips_cvt_s_pu                   (rtx, rtx);
extern rtx        gen_mips_abs_ps                     (rtx, rtx);
extern rtx        gen_scc_ps                          (rtx, rtx);
extern rtx        gen_single_cc                       (rtx, rtx);
extern rtx        gen_vcondv2sf                       (rtx, rtx, rtx, rtx, rtx, rtx);
extern rtx        gen_sminv2sf3                       (rtx, rtx, rtx);
extern rtx        gen_smaxv2sf3                       (rtx, rtx, rtx);
extern rtx        gen_mips_lbux                       (rtx, rtx, rtx);
extern rtx        gen_mips_lhx                        (rtx, rtx, rtx);
extern rtx        gen_mips_lwx                        (rtx, rtx, rtx);
extern rtx        gen_mips_madd                       (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_maddu                      (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_msub                       (rtx, rtx, rtx, rtx);
extern rtx        gen_mips_msubu                      (rtx, rtx, rtx, rtx);
extern rtx        gen_movv2si                         (rtx, rtx);
extern rtx        gen_movv4hi                         (rtx, rtx);
extern rtx        gen_movv8qi                         (rtx, rtx);
extern rtx        gen_vec_initv2si                    (rtx, rtx);
extern rtx        gen_vec_initv4hi                    (rtx, rtx);
extern rtx        gen_vec_initv8qi                    (rtx, rtx);

#endif /* GCC_INSN_FLAGS_H */
