/****************************************************************************** 
* 
* Copyright(c) 2007 - 2011 Realtek Corporation. All rights reserved. 
* 
* This program is free software; you can redistribute it and/or modify it 
* under the terms of version 2 of the GNU General Public License as 
* published by the Free Software Foundation. 
* 
* This program is distributed in the hope that it will be useful, but WITHOUT 
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for 
* more details. 
* 
* You should have received a copy of the GNU General Public License along with 
* this program; if not, write to the Free Software Foundation, Inc., 
* 51 Franklin Street, Fifth Floor, Boston, MA 02110, USA 
* 
* 
******************************************************************************/

/*Image2HeaderVersion: 2.22*/
#include "mp_precomp.h"
#include "../phydm_precomp.h"

#if (RTL8197F_SUPPORT == 1)
static BOOLEAN
CheckPositive(
	IN  PDM_ODM_T     pDM_Odm,
	IN  const u4Byte  Condition1,
	IN  const u4Byte  Condition2,
	IN	const u4Byte  Condition3,
	IN	const u4Byte  Condition4
)
{
	u1Byte    _BoardType = ((pDM_Odm->BoardType & BIT4) >> 4) << 0 | /* _GLNA*/
				((pDM_Odm->BoardType & BIT3) >> 3) << 1 | /* _GPA*/ 
				((pDM_Odm->BoardType & BIT7) >> 7) << 2 | /* _ALNA*/
				((pDM_Odm->BoardType & BIT6) >> 6) << 3 | /* _APA */
				((pDM_Odm->BoardType & BIT2) >> 2) << 4;  /* _BT*/  

	u4Byte	cond1   = Condition1, cond2 = Condition2, cond3 = Condition3, cond4 = Condition4;
	u4Byte    driver1 = pDM_Odm->CutVersion       << 24 | 
				(pDM_Odm->SupportInterface & 0xF0) << 16 | 
				pDM_Odm->SupportPlatform  << 16 | 
				pDM_Odm->PackageType      << 12 | 
				(pDM_Odm->SupportInterface & 0x0F) << 8  |
				_BoardType;

	u4Byte    driver2 = (pDM_Odm->TypeGLNA & 0xFF) <<  0 |  
				(pDM_Odm->TypeGPA & 0xFF)  <<  8 | 
				(pDM_Odm->TypeALNA & 0xFF) << 16 | 
				(pDM_Odm->TypeAPA & 0xFF)  << 24; 

u4Byte    driver3 = 0;

	u4Byte    driver4 = (pDM_Odm->TypeGLNA & 0xFF00) >>  8 |
				(pDM_Odm->TypeGPA & 0xFF00) |
				(pDM_Odm->TypeALNA & 0xFF00) << 8 |
				(pDM_Odm->TypeAPA & 0xFF00)  << 16;

	ODM_RT_TRACE(pDM_Odm, ODM_COMP_INIT, ODM_DBG_TRACE, 
	("===> CheckPositive (cond1, cond2, cond3, cond4) = (0x%X 0x%X 0x%X 0x%X)\n", cond1, cond2, cond3, cond4));
	ODM_RT_TRACE(pDM_Odm, ODM_COMP_INIT, ODM_DBG_TRACE, 
	("===> CheckPositive (driver1, driver2, driver3, driver4) = (0x%X 0x%X 0x%X 0x%X)\n", driver1, driver2, driver3, driver4));

	ODM_RT_TRACE(pDM_Odm, ODM_COMP_INIT, ODM_DBG_TRACE, 
	("	(Platform, Interface) = (0x%X, 0x%X)\n", pDM_Odm->SupportPlatform, pDM_Odm->SupportInterface));
	ODM_RT_TRACE(pDM_Odm, ODM_COMP_INIT, ODM_DBG_TRACE, 
	("	(Board, Package) = (0x%X, 0x%X)\n", pDM_Odm->BoardType, pDM_Odm->PackageType));


	/*============== Value Defined Check ===============*/
	/*QFN Type [15:12] and Cut Version [27:24] need to do value check*/
	
	if (((cond1 & 0x0000F000) != 0) && ((cond1 & 0x0000F000) != (driver1 & 0x0000F000)))
		return FALSE;
	if (((cond1 & 0x0F000000) != 0) && ((cond1 & 0x0F000000) != (driver1 & 0x0F000000)))
		return FALSE;

	/*=============== Bit Defined Check ================*/
	/* We don't care [31:28] */

	cond1   &= 0x00FF0FFF; 
	driver1 &= 0x00FF0FFF; 

	if ((cond1 & driver1) == cond1) {
		u4Byte bitMask = 0;

		if ((cond1 & 0x0F) == 0) /* BoardType is DONTCARE*/
			return TRUE;

		if ((cond1 & BIT0) != 0) /*GLNA*/
			bitMask |= 0x000000FF;
		if ((cond1 & BIT1) != 0) /*GPA*/
			bitMask |= 0x0000FF00;
		if ((cond1 & BIT2) != 0) /*ALNA*/
			bitMask |= 0x00FF0000;
		if ((cond1 & BIT3) != 0) /*APA*/
			bitMask |= 0xFF000000;

		if (((cond2 & bitMask) == (driver2 & bitMask)) && ((cond4 & bitMask) == (driver4 & bitMask)))  /* BoardType of each RF path is matched*/
			return TRUE;
		else
			return FALSE;
	} else
		return FALSE;
}
static BOOLEAN
CheckNegative(
	IN  PDM_ODM_T     pDM_Odm,
	IN  const u4Byte  Condition1,
	IN  const u4Byte  Condition2
)
{
	return TRUE;
}

/******************************************************************************
*                           RadioA.TXT
******************************************************************************/

u4Byte Array_MP_8197F_RadioA[] = { 
		0x000, 0x00030000,
		0x018, 0x00000C07,
		0x0CA, 0x00002000,
		0x081, 0x0000FC00,
		0x082, 0x000003C0,
		0x084, 0x00000005,
		0x086, 0x000933A5,
		0x087, 0x00000000,
		0x088, 0x0007B030,
		0x08E, 0x00064540,
		0x08F, 0x000A8000,
		0x0DF, 0x00000E00,
		0x051, 0x00000C1C,
		0x052, 0x0009A007,
		0x053, 0x00018C47,
		0x054, 0x00090034,
		0x055, 0x00082000,
		0x056, 0x0000880F,
		0x057, 0x00079000,
		0x058, 0x000603F0,
		0x05A, 0x00050000,
		0x05B, 0x00000006,
		0x05C, 0x00000015,
		0x065, 0x00020000,
		0x06E, 0x00038319,
		0x0DF, 0x00000600,
	0x80001003,	0x00000000,	0x40000000,	0x00000000,
		0x0EF, 0x00000100,
		0x033, 0x00000121,
		0x033, 0x00004123,
		0x033, 0x00008126,
		0x033, 0x0000C129,
		0x033, 0x0001012C,
		0x033, 0x0001414C,
		0x033, 0x0001816C,
		0x033, 0x0001C16F,
		0x033, 0x000201AE,
		0x033, 0x000245AC,
		0x033, 0x00028DAB,
		0x0EF, 0x00000000,
	0x90002008,	0x00000000,	0x40000000,	0x00000000,
		0x0EF, 0x00000100,
		0x033, 0x000000A5,
		0x033, 0x000040A8,
		0x033, 0x000080E8,
		0x033, 0x0000C4E6,
		0x033, 0x00010DE5,
		0x033, 0x00014DE8,
		0x033, 0x00018DEB,
		0x033, 0x0001CDEE,
		0x033, 0x00020DF1,
		0x033, 0x00024DF4,
		0x033, 0x00028DF7,
		0x0EF, 0x00000000,
	0xA0000000,	0x00000000,
		0x0EF, 0x00000100,
		0x033, 0x000000A5,
		0x033, 0x000040A8,
		0x033, 0x000080E8,
		0x033, 0x0000C4E6,
		0x033, 0x00010DE5,
		0x033, 0x00014DE8,
		0x033, 0x00018DEB,
		0x033, 0x0001CDEE,
		0x033, 0x00020DF1,
		0x033, 0x00024DF4,
		0x033, 0x00028DF7,
		0x0EF, 0x00000000,
	0xB0000000,	0x00000000,
		0x0EF, 0x00020000,
		0x033, 0x00030026,
		0x033, 0x00038026,
		0x033, 0x00070026,
		0x033, 0x00078026,
		0x0EF, 0x00000000,
	0x80001003,	0x00000000,	0x40000000,	0x00000000,
		0x0EF, 0x00002000,
		0x033, 0x0006000D,
		0x0EF, 0x00000000,
	0x90002008,	0x00000000,	0x40000000,	0x00000000,
		0x0EF, 0x00020000,
		0x033, 0x00030021,
		0x0EF, 0x00000000,
	0xA0000000,	0x00000000,
		0x0EF, 0x00020000,
		0x033, 0x00030021,
		0x0EF, 0x00000000,
	0xB0000000,	0x00000000,
		0x0C9, 0x00000600,
		0x0B0, 0x000FFFC3,
		0x0B1, 0x00033B9F,
		0x0B2, 0x00033762,
	0x80001003,	0x00000000,	0x40000000,	0x00000000,
		0x0B4, 0x000141F0,
	0x90002008,	0x00000000,	0x40000000,	0x00000000,
		0x0B4, 0x000140A0,
	0xA0000000,	0x00000000,
		0x0B4, 0x000141F0,
	0xB0000000,	0x00000000,
		0x0B5, 0x00014000,
		0x0B6, 0x00012425,
		0x0B7, 0x0001060C,
		0x0B8, 0x00070F3F,
		0x0B9, 0x000C0008,
		0x0BA, 0x00040005,
		0x0C2, 0x00002C01,
		0x0C3, 0x0000000B,
		0x0C4, 0x00081E2F,
		0x0C5, 0x0005C28F,
		0x0C6, 0x000000A0,
		0x0C9, 0x00000600,
		0x0CA, 0x00002000,
		0xFFE, 0x00000000,
		0x018, 0x00008C07,
		0xFFE, 0x00000000,
		0xFFE, 0x00000000,
		0xFFE, 0x00000000,
		0x000, 0x00035DF5,

};

void
ODM_ReadAndConfig_MP_8197F_RadioA(
	IN   PDM_ODM_T  pDM_Odm
)
{
	u4Byte     i         = 0;
	u1Byte     cCond;
	BOOLEAN bMatched = TRUE, bSkipped = FALSE;
	u4Byte     ArrayLen    = sizeof(Array_MP_8197F_RadioA)/sizeof(u4Byte);
	pu4Byte    Array       = Array_MP_8197F_RadioA;
	
	u4Byte	v1 = 0, v2 = 0, pre_v1 = 0, pre_v2 = 0;

	ODM_RT_TRACE(pDM_Odm, ODM_COMP_INIT, ODM_DBG_LOUD, ("===> ODM_ReadAndConfig_MP_8197F_RadioA\n"));

	while ((i + 1) < ArrayLen) {
		v1 = Array[i];
		v2 = Array[i + 1];

		if (v1 & (BIT31 | BIT30)) {/*positive & negative condition*/
			if (v1 & BIT31) {/* positive condition*/
				cCond  = (u1Byte)((v1 & (BIT29|BIT28)) >> 28);
				if (cCond == COND_ENDIF) {/*end*/
					bMatched = TRUE;
					bSkipped = FALSE;
					ODM_RT_TRACE(pDM_Odm, ODM_COMP_INIT, ODM_DBG_LOUD, ("ENDIF\n"));
				} else if (cCond == COND_ELSE) { /*else*/
					bMatched = bSkipped?FALSE:TRUE;
					ODM_RT_TRACE(pDM_Odm, ODM_COMP_INIT, ODM_DBG_LOUD, ("ELSE\n"));
				} else {/*if , else if*/
					pre_v1 = v1;
					pre_v2 = v2;
					ODM_RT_TRACE(pDM_Odm, ODM_COMP_INIT, ODM_DBG_LOUD, ("IF or ELSE IF\n"));
				}
			} else if (v1 & BIT30) { /*negative condition*/
				if (bSkipped == FALSE) {
					if (CheckPositive(pDM_Odm, pre_v1, pre_v2, v1, v2)) {
						bMatched = TRUE;
						bSkipped = TRUE;
					} else {
						bMatched = FALSE;
						bSkipped = FALSE;
					}
				} else
					bMatched = FALSE;
			}
		} else {
			if (bMatched)
				odm_ConfigRF_RadioA_8197F(pDM_Odm, v1, v2);
		}
		i = i + 2;
	}
}

u4Byte
ODM_GetVersion_MP_8197F_RadioA(void)
{
	   return 37;
}

/******************************************************************************
*                           RadioB.TXT
******************************************************************************/

u4Byte Array_MP_8197F_RadioB[] = { 
		0x000, 0x00030000,
		0x081, 0x0000FC00,
		0x082, 0x000003C0,
		0x084, 0x00000005,
		0x086, 0x000933A5,
		0x087, 0x00000000,
		0x088, 0x0007B030,
		0x08E, 0x00064540,
		0x08F, 0x000A8000,
		0x0DF, 0x00000E00,
		0x051, 0x00000C18,
		0x052, 0x0009A007,
		0x053, 0x00018C4B,
		0x054, 0x00090030,
		0x055, 0x00082000,
		0x056, 0x0000880F,
		0x057, 0x00079000,
		0x058, 0x000603F0,
		0x05A, 0x00050000,
		0x05B, 0x00000006,
		0x05C, 0x00000015,
		0x065, 0x00020000,
		0x06E, 0x00038319,
		0x0DF, 0x00000600,
	0x80001003,	0x00000000,	0x40000000,	0x00000000,
		0x0EF, 0x00000100,
		0x033, 0x00000121,
		0x033, 0x00004123,
		0x033, 0x00008126,
		0x033, 0x0000C129,
		0x033, 0x0001012C,
		0x033, 0x0001414C,
		0x033, 0x0001816C,
		0x033, 0x0001C16F,
		0x033, 0x000201AE,
		0x033, 0x000245AC,
		0x033, 0x00028DAB,
		0x0EF, 0x00000000,
	0x90002008,	0x00000000,	0x40000000,	0x00000000,
		0x0EF, 0x00000100,
		0x033, 0x000000A5,
		0x033, 0x000040A8,
		0x033, 0x000080E8,
		0x033, 0x0000C4E6,
		0x033, 0x00010DE5,
		0x033, 0x00014DE8,
		0x033, 0x00018DEB,
		0x033, 0x0001CDEE,
		0x033, 0x00020DF1,
		0x033, 0x00024DF4,
		0x033, 0x00028DF7,
		0x0EF, 0x00000000,
	0xA0000000,	0x00000000,
		0x0EF, 0x00000100,
		0x033, 0x000000A5,
		0x033, 0x000040A8,
		0x033, 0x000080E8,
		0x033, 0x0000C4E6,
		0x033, 0x00010DE5,
		0x033, 0x00014DE8,
		0x033, 0x00018DEB,
		0x033, 0x0001CDEE,
		0x033, 0x00020DF1,
		0x033, 0x00024DF4,
		0x033, 0x00028DF7,
		0x0EF, 0x00000000,
	0xB0000000,	0x00000000,
	0x80001003,	0x00000000,	0x40000000,	0x00000000,
		0x0EF, 0x00002000,
		0x033, 0x0006000D,
		0x0EF, 0x00000000,
	0x90002008,	0x00000000,	0x40000000,	0x00000000,
		0x0EF, 0x00020000,
		0x033, 0x00030021,
		0x0EF, 0x00000000,
	0xA0000000,	0x00000000,
		0x0EF, 0x00020000,
		0x033, 0x00030021,
		0x0EF, 0x00000000,
	0xB0000000,	0x00000000,
		0x000, 0x00035DF5,

};

void
ODM_ReadAndConfig_MP_8197F_RadioB(
	IN   PDM_ODM_T  pDM_Odm
)
{
	u4Byte     i         = 0;
	u1Byte     cCond;
	BOOLEAN bMatched = TRUE, bSkipped = FALSE;
	u4Byte     ArrayLen    = sizeof(Array_MP_8197F_RadioB)/sizeof(u4Byte);
	pu4Byte    Array       = Array_MP_8197F_RadioB;
	
	u4Byte	v1 = 0, v2 = 0, pre_v1 = 0, pre_v2 = 0;

	ODM_RT_TRACE(pDM_Odm, ODM_COMP_INIT, ODM_DBG_LOUD, ("===> ODM_ReadAndConfig_MP_8197F_RadioB\n"));

	while ((i + 1) < ArrayLen) {
		v1 = Array[i];
		v2 = Array[i + 1];

		if (v1 & (BIT31 | BIT30)) {/*positive & negative condition*/
			if (v1 & BIT31) {/* positive condition*/
				cCond  = (u1Byte)((v1 & (BIT29|BIT28)) >> 28);
				if (cCond == COND_ENDIF) {/*end*/
					bMatched = TRUE;
					bSkipped = FALSE;
					ODM_RT_TRACE(pDM_Odm, ODM_COMP_INIT, ODM_DBG_LOUD, ("ENDIF\n"));
				} else if (cCond == COND_ELSE) { /*else*/
					bMatched = bSkipped?FALSE:TRUE;
					ODM_RT_TRACE(pDM_Odm, ODM_COMP_INIT, ODM_DBG_LOUD, ("ELSE\n"));
				} else {/*if , else if*/
					pre_v1 = v1;
					pre_v2 = v2;
					ODM_RT_TRACE(pDM_Odm, ODM_COMP_INIT, ODM_DBG_LOUD, ("IF or ELSE IF\n"));
				}
			} else if (v1 & BIT30) { /*negative condition*/
				if (bSkipped == FALSE) {
					if (CheckPositive(pDM_Odm, pre_v1, pre_v2, v1, v2)) {
						bMatched = TRUE;
						bSkipped = TRUE;
					} else {
						bMatched = FALSE;
						bSkipped = FALSE;
					}
				} else
					bMatched = FALSE;
			}
		} else {
			if (bMatched)
				odm_ConfigRF_RadioB_8197F(pDM_Odm, v1, v2);
		}
		i = i + 2;
	}
}

u4Byte
ODM_GetVersion_MP_8197F_RadioB(void)
{
	   return 37;
}

/******************************************************************************
*                           TxPowerTrack.TXT
******************************************************************************/

u1Byte gDeltaSwingTableIdx_MP_5GB_N_TxPowerTrack_8197F[][DELTA_SWINGIDX_SIZE] = {
	{0, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 11, 12, 12, 13, 13, 14, 14, 14, 14, 14, 14, 14},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 14, 14, 14, 14, 14},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 14, 14, 14, 14, 14},
};
u1Byte gDeltaSwingTableIdx_MP_5GB_P_TxPowerTrack_8197F[][DELTA_SWINGIDX_SIZE] = {
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10, 11, 12, 12, 13, 14, 15, 15, 16, 16, 17, 17, 18, 19, 20, 20, 20},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10, 11, 12, 12, 13, 14, 15, 15, 16, 17, 18, 18, 19, 19, 20, 20, 20},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10, 11, 12, 12, 13, 14, 15, 15, 16, 17, 18, 18, 19, 20, 21, 21, 21},
};
u1Byte gDeltaSwingTableIdx_MP_5GA_N_TxPowerTrack_8197F[][DELTA_SWINGIDX_SIZE] = {
	{0, 1, 2, 3, 3, 4, 4, 5, 5, 6, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 14, 14, 14, 14, 14},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 6, 7, 7, 8, 8, 9, 10, 11, 11, 12, 13, 13, 14, 15, 16, 16, 16, 16, 16, 16, 16},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10, 10, 11, 11, 12, 13, 14, 14, 15, 15, 16, 16, 16, 16, 16, 16, 16},
};
u1Byte gDeltaSwingTableIdx_MP_5GA_P_TxPowerTrack_8197F[][DELTA_SWINGIDX_SIZE] = {
	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10, 11, 12, 12, 13, 14, 15, 15, 16, 17, 18, 18, 19, 20, 21, 21, 21},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10, 11, 12, 12, 13, 14, 15, 15, 16, 17, 18, 18, 19, 20, 21, 21, 21},
};
u1Byte gDeltaSwingTableIdx_MP_2GB_N_TxPowerTrack_8197F[]    = {0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8};
u1Byte gDeltaSwingTableIdx_MP_2GB_P_TxPowerTrack_8197F[]    = {0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8};
u1Byte gDeltaSwingTableIdx_MP_2GA_N_TxPowerTrack_8197F[]    = {0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8};
u1Byte gDeltaSwingTableIdx_MP_2GA_P_TxPowerTrack_8197F[]    = {0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8};
u1Byte gDeltaSwingTableIdx_MP_2GCCKB_N_TxPowerTrack_8197F[] = {0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8};
u1Byte gDeltaSwingTableIdx_MP_2GCCKB_P_TxPowerTrack_8197F[] = {0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8};
u1Byte gDeltaSwingTableIdx_MP_2GCCKA_N_TxPowerTrack_8197F[] = {0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8};
u1Byte gDeltaSwingTableIdx_MP_2GCCKA_P_TxPowerTrack_8197F[] = {0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8};

void
ODM_ReadAndConfig_MP_8197F_TxPowerTrack(
	IN   PDM_ODM_T  pDM_Odm
)
{
	PODM_RF_CAL_T  pRFCalibrateInfo = &(pDM_Odm->RFCalibrateInfo);

	ODM_RT_TRACE(pDM_Odm, ODM_COMP_INIT, ODM_DBG_LOUD, ("===> ODM_ReadAndConfig_MP_MP_8197F\n"));


	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GA_P, gDeltaSwingTableIdx_MP_2GA_P_TxPowerTrack_8197F, DELTA_SWINGIDX_SIZE);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GA_N, gDeltaSwingTableIdx_MP_2GA_N_TxPowerTrack_8197F, DELTA_SWINGIDX_SIZE);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GB_P, gDeltaSwingTableIdx_MP_2GB_P_TxPowerTrack_8197F, DELTA_SWINGIDX_SIZE);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GB_N, gDeltaSwingTableIdx_MP_2GB_N_TxPowerTrack_8197F, DELTA_SWINGIDX_SIZE);

	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GCCKA_P, gDeltaSwingTableIdx_MP_2GCCKA_P_TxPowerTrack_8197F, DELTA_SWINGIDX_SIZE);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GCCKA_N, gDeltaSwingTableIdx_MP_2GCCKA_N_TxPowerTrack_8197F, DELTA_SWINGIDX_SIZE);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GCCKB_P, gDeltaSwingTableIdx_MP_2GCCKB_P_TxPowerTrack_8197F, DELTA_SWINGIDX_SIZE);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GCCKB_N, gDeltaSwingTableIdx_MP_2GCCKB_N_TxPowerTrack_8197F, DELTA_SWINGIDX_SIZE);

	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_5GA_P, gDeltaSwingTableIdx_MP_5GA_P_TxPowerTrack_8197F, DELTA_SWINGIDX_SIZE*3);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_5GA_N, gDeltaSwingTableIdx_MP_5GA_N_TxPowerTrack_8197F, DELTA_SWINGIDX_SIZE*3);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_5GB_P, gDeltaSwingTableIdx_MP_5GB_P_TxPowerTrack_8197F, DELTA_SWINGIDX_SIZE*3);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_5GB_N, gDeltaSwingTableIdx_MP_5GB_N_TxPowerTrack_8197F, DELTA_SWINGIDX_SIZE*3);
}

/******************************************************************************
*                           TxPowerTrack_Type0.TXT
******************************************************************************/

u1Byte gDeltaSwingTableIdx_MP_5GB_N_TxPowerTrack_Type0_8197F[][DELTA_SWINGIDX_SIZE] = {
	{0, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 11, 12, 12, 13, 13, 14, 14, 14, 14, 14, 14, 14},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 14, 14, 14, 14, 14},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 14, 14, 14, 14, 14},
};
u1Byte gDeltaSwingTableIdx_MP_5GB_P_TxPowerTrack_Type0_8197F[][DELTA_SWINGIDX_SIZE] = {
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10, 11, 12, 12, 13, 14, 15, 15, 16, 16, 17, 17, 18, 19, 20, 20, 20},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10, 11, 12, 12, 13, 14, 15, 15, 16, 17, 18, 18, 19, 19, 20, 20, 20},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10, 11, 12, 12, 13, 14, 15, 15, 16, 17, 18, 18, 19, 20, 21, 21, 21},
};
u1Byte gDeltaSwingTableIdx_MP_5GA_N_TxPowerTrack_Type0_8197F[][DELTA_SWINGIDX_SIZE] = {
	{0, 1, 2, 3, 3, 4, 4, 5, 5, 6, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 14, 14, 14, 14, 14},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 6, 7, 7, 8, 8, 9, 10, 11, 11, 12, 13, 13, 14, 15, 16, 16, 16, 16, 16, 16, 16},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10, 10, 11, 11, 12, 13, 14, 14, 15, 15, 16, 16, 16, 16, 16, 16, 16},
};
u1Byte gDeltaSwingTableIdx_MP_5GA_P_TxPowerTrack_Type0_8197F[][DELTA_SWINGIDX_SIZE] = {
	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10, 11, 12, 12, 13, 14, 15, 15, 16, 17, 18, 18, 19, 20, 21, 21, 21},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10, 11, 12, 12, 13, 14, 15, 15, 16, 17, 18, 18, 19, 20, 21, 21, 21},
};
u1Byte gDeltaSwingTableIdx_MP_2GB_N_TxPowerTrack_Type0_8197F[]    = {0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8};
u1Byte gDeltaSwingTableIdx_MP_2GB_P_TxPowerTrack_Type0_8197F[]    = {0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8};
u1Byte gDeltaSwingTableIdx_MP_2GA_N_TxPowerTrack_Type0_8197F[]    = {0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8};
u1Byte gDeltaSwingTableIdx_MP_2GA_P_TxPowerTrack_Type0_8197F[]    = {0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8};
u1Byte gDeltaSwingTableIdx_MP_2GCCKB_N_TxPowerTrack_Type0_8197F[] = {0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8};
u1Byte gDeltaSwingTableIdx_MP_2GCCKB_P_TxPowerTrack_Type0_8197F[] = {0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8};
u1Byte gDeltaSwingTableIdx_MP_2GCCKA_N_TxPowerTrack_Type0_8197F[] = {0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8};
u1Byte gDeltaSwingTableIdx_MP_2GCCKA_P_TxPowerTrack_Type0_8197F[] = {0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8};

void
ODM_ReadAndConfig_MP_8197F_TxPowerTrack_Type0(
	IN   PDM_ODM_T  pDM_Odm
)
{
	PODM_RF_CAL_T  pRFCalibrateInfo = &(pDM_Odm->RFCalibrateInfo);

	ODM_RT_TRACE(pDM_Odm, ODM_COMP_INIT, ODM_DBG_LOUD, ("===> ODM_ReadAndConfig_MP_MP_8197F\n"));


	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GA_P, gDeltaSwingTableIdx_MP_2GA_P_TxPowerTrack_Type0_8197F, DELTA_SWINGIDX_SIZE);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GA_N, gDeltaSwingTableIdx_MP_2GA_N_TxPowerTrack_Type0_8197F, DELTA_SWINGIDX_SIZE);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GB_P, gDeltaSwingTableIdx_MP_2GB_P_TxPowerTrack_Type0_8197F, DELTA_SWINGIDX_SIZE);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GB_N, gDeltaSwingTableIdx_MP_2GB_N_TxPowerTrack_Type0_8197F, DELTA_SWINGIDX_SIZE);

	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GCCKA_P, gDeltaSwingTableIdx_MP_2GCCKA_P_TxPowerTrack_Type0_8197F, DELTA_SWINGIDX_SIZE);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GCCKA_N, gDeltaSwingTableIdx_MP_2GCCKA_N_TxPowerTrack_Type0_8197F, DELTA_SWINGIDX_SIZE);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GCCKB_P, gDeltaSwingTableIdx_MP_2GCCKB_P_TxPowerTrack_Type0_8197F, DELTA_SWINGIDX_SIZE);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GCCKB_N, gDeltaSwingTableIdx_MP_2GCCKB_N_TxPowerTrack_Type0_8197F, DELTA_SWINGIDX_SIZE);

	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_5GA_P, gDeltaSwingTableIdx_MP_5GA_P_TxPowerTrack_Type0_8197F, DELTA_SWINGIDX_SIZE*3);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_5GA_N, gDeltaSwingTableIdx_MP_5GA_N_TxPowerTrack_Type0_8197F, DELTA_SWINGIDX_SIZE*3);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_5GB_P, gDeltaSwingTableIdx_MP_5GB_P_TxPowerTrack_Type0_8197F, DELTA_SWINGIDX_SIZE*3);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_5GB_N, gDeltaSwingTableIdx_MP_5GB_N_TxPowerTrack_Type0_8197F, DELTA_SWINGIDX_SIZE*3);
}

/******************************************************************************
*                           TxPowerTrack_Type1.TXT
******************************************************************************/

u1Byte gDeltaSwingTableIdx_MP_5GB_N_TxPowerTrack_Type1_8197F[][DELTA_SWINGIDX_SIZE] = {
	{0, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 11, 12, 12, 13, 13, 14, 14, 14, 14, 14, 14, 14},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 14, 14, 14, 14, 14},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 14, 14, 14, 14, 14},
};
u1Byte gDeltaSwingTableIdx_MP_5GB_P_TxPowerTrack_Type1_8197F[][DELTA_SWINGIDX_SIZE] = {
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10, 11, 12, 12, 13, 14, 15, 15, 16, 16, 17, 17, 18, 19, 20, 20, 20},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10, 11, 12, 12, 13, 14, 15, 15, 16, 17, 18, 18, 19, 19, 20, 20, 20},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10, 11, 12, 12, 13, 14, 15, 15, 16, 17, 18, 18, 19, 20, 21, 21, 21},
};
u1Byte gDeltaSwingTableIdx_MP_5GA_N_TxPowerTrack_Type1_8197F[][DELTA_SWINGIDX_SIZE] = {
	{0, 1, 2, 3, 3, 4, 4, 5, 5, 6, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 14, 14, 14, 14, 14},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 6, 7, 7, 8, 8, 9, 10, 11, 11, 12, 13, 13, 14, 15, 16, 16, 16, 16, 16, 16, 16},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10, 10, 11, 11, 12, 13, 14, 14, 15, 15, 16, 16, 16, 16, 16, 16, 16},
};
u1Byte gDeltaSwingTableIdx_MP_5GA_P_TxPowerTrack_Type1_8197F[][DELTA_SWINGIDX_SIZE] = {
	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10, 11, 12, 12, 13, 14, 15, 15, 16, 17, 18, 18, 19, 20, 21, 21, 21},
	{0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10, 11, 12, 12, 13, 14, 15, 15, 16, 17, 18, 18, 19, 20, 21, 21, 21},
};
u1Byte gDeltaSwingTableIdx_MP_2GB_N_TxPowerTrack_Type1_8197F[]    = {0, 0, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 10, 11, 11, 11, 12, 12, 12, 13, 13};
u1Byte gDeltaSwingTableIdx_MP_2GB_P_TxPowerTrack_Type1_8197F[]    = {0, 0, 1, 1, 2, 3, 3, 3, 4, 4, 5, 6, 6, 7, 7, 8, 8, 8, 9, 10, 10, 10, 11, 11, 11, 12, 12, 12, 13, 13};
u1Byte gDeltaSwingTableIdx_MP_2GA_N_TxPowerTrack_Type1_8197F[]    = {0, 0, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 10, 11, 11, 11, 12, 12, 12, 13, 13};
u1Byte gDeltaSwingTableIdx_MP_2GA_P_TxPowerTrack_Type1_8197F[]    = {0, 0, 1, 1, 2, 3, 3, 3, 4, 4, 5, 6, 6, 7, 7, 8, 8, 8, 9, 10, 10, 10, 11, 11, 11, 12, 12, 12, 13, 13};
u1Byte gDeltaSwingTableIdx_MP_2GCCKB_N_TxPowerTrack_Type1_8197F[] = {0, 0, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 10, 11, 11, 11, 12, 12, 12, 13, 13};
u1Byte gDeltaSwingTableIdx_MP_2GCCKB_P_TxPowerTrack_Type1_8197F[] = {0, 0, 1, 1, 2, 3, 3, 3, 4, 4, 5, 6, 6, 7, 7, 8, 8, 8, 9, 10, 10, 10, 11, 11, 11, 12, 12, 12, 13, 13};
u1Byte gDeltaSwingTableIdx_MP_2GCCKA_N_TxPowerTrack_Type1_8197F[] = {0, 0, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 10, 11, 11, 11, 12, 12, 12, 13, 13};
u1Byte gDeltaSwingTableIdx_MP_2GCCKA_P_TxPowerTrack_Type1_8197F[] = {0, 0, 1, 1, 2, 3, 3, 3, 4, 4, 5, 6, 6, 7, 7, 8, 8, 8, 9, 10, 10, 10, 11, 11, 11, 12, 12, 12, 13, 13};

void
ODM_ReadAndConfig_MP_8197F_TxPowerTrack_Type1(
	IN   PDM_ODM_T  pDM_Odm
)
{
	PODM_RF_CAL_T  pRFCalibrateInfo = &(pDM_Odm->RFCalibrateInfo);

	ODM_RT_TRACE(pDM_Odm, ODM_COMP_INIT, ODM_DBG_LOUD, ("===> ODM_ReadAndConfig_MP_MP_8197F\n"));


	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GA_P, gDeltaSwingTableIdx_MP_2GA_P_TxPowerTrack_Type1_8197F, DELTA_SWINGIDX_SIZE);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GA_N, gDeltaSwingTableIdx_MP_2GA_N_TxPowerTrack_Type1_8197F, DELTA_SWINGIDX_SIZE);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GB_P, gDeltaSwingTableIdx_MP_2GB_P_TxPowerTrack_Type1_8197F, DELTA_SWINGIDX_SIZE);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GB_N, gDeltaSwingTableIdx_MP_2GB_N_TxPowerTrack_Type1_8197F, DELTA_SWINGIDX_SIZE);

	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GCCKA_P, gDeltaSwingTableIdx_MP_2GCCKA_P_TxPowerTrack_Type1_8197F, DELTA_SWINGIDX_SIZE);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GCCKA_N, gDeltaSwingTableIdx_MP_2GCCKA_N_TxPowerTrack_Type1_8197F, DELTA_SWINGIDX_SIZE);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GCCKB_P, gDeltaSwingTableIdx_MP_2GCCKB_P_TxPowerTrack_Type1_8197F, DELTA_SWINGIDX_SIZE);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_2GCCKB_N, gDeltaSwingTableIdx_MP_2GCCKB_N_TxPowerTrack_Type1_8197F, DELTA_SWINGIDX_SIZE);

	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_5GA_P, gDeltaSwingTableIdx_MP_5GA_P_TxPowerTrack_Type1_8197F, DELTA_SWINGIDX_SIZE*3);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_5GA_N, gDeltaSwingTableIdx_MP_5GA_N_TxPowerTrack_Type1_8197F, DELTA_SWINGIDX_SIZE*3);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_5GB_P, gDeltaSwingTableIdx_MP_5GB_P_TxPowerTrack_Type1_8197F, DELTA_SWINGIDX_SIZE*3);
	ODM_MoveMemory(pDM_Odm, pRFCalibrateInfo->DeltaSwingTableIdx_5GB_N, gDeltaSwingTableIdx_MP_5GB_N_TxPowerTrack_Type1_8197F, DELTA_SWINGIDX_SIZE*3);
}

#endif /* end of HWIMG_SUPPORT*/

