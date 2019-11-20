#include "8192cd.h"
#include "8192cd_cfg.h"
#include "8192cd_util.h"
#include "8192cd_headers.h"
#include "Beamforming.h"
#include "8812_reg.h"
#include "8812_vht_gen.h"



#if (BEAMFORMING_SUPPORT == 1)
VOID
Beamforming_SetBeamFormInit(
	struct rtl8192cd_priv *priv
		)
{
#ifdef CONFIG_WLAN_HAL_8822BE
		if(GET_CHIP_VER(priv) == VERSION_8822B)
			SetBeamformInit8822B(priv);
#endif
}


VOID
Beamforming_SetBeamFormLeave(
	struct rtl8192cd_priv *priv,
	u1Byte				Idx
	)
{

#ifdef CONFIG_RTL_8812_SUPPORT
		if(GET_CHIP_VER(priv) == VERSION_8812E)
			SetBeamformLeave8812(priv, Idx);	
#endif

#ifdef CONFIG_WLAN_HAL_8192EE
		if(GET_CHIP_VER(priv) == VERSION_8192E)
			SetBeamformLeave92E(priv,Idx);
#endif

#ifdef CONFIG_WLAN_HAL_8814AE
		if(GET_CHIP_VER(priv) == VERSION_8814A)
			SetBeamformLeave8814A(priv,Idx);
#endif

#ifdef CONFIG_WLAN_HAL_8881A
		if(GET_CHIP_VER(priv) == VERSION_8881A)
			SetBeamformLeave8881A(priv,Idx);

#endif

#ifdef CONFIG_WLAN_HAL_8197F
		if(GET_CHIP_VER(priv) == VERSION_8197F)
			SetBeamformLeave8197F(priv,Idx);
#endif

#ifdef CONFIG_WLAN_HAL_8822BE
		if(GET_CHIP_VER(priv) == VERSION_8822B)
			SetBeamformLeave8822B(priv,Idx);
#endif

}

VOID
Beamforming_SetBeamFormStatus(
	struct rtl8192cd_priv *priv,
	u1Byte				Idx
	)
{

#ifdef CONFIG_WLAN_HAL_8192EE
		if(GET_CHIP_VER(priv)== VERSION_8192E)
			SetBeamformStatus92E(priv, Idx);		
#endif
#ifdef CONFIG_RTL_8812_SUPPORT
		if(GET_CHIP_VER(priv)== VERSION_8812E)
			SetBeamformStatus8812(priv, Idx);
#endif

#ifdef CONFIG_WLAN_HAL_8814AE
		if(GET_CHIP_VER(priv) == VERSION_8814A)
			SetBeamformStatus8814A(priv,Idx);
#endif

#ifdef CONFIG_WLAN_HAL_8197F
		if(GET_CHIP_VER(priv) == VERSION_8197F)
			SetBeamformStatus8197F(priv,Idx);
#endif

#ifdef CONFIG_WLAN_HAL_8822BE
		if(GET_CHIP_VER(priv) == VERSION_8822B)
			SetBeamformStatus8822B(priv,Idx);
#endif

}

VOID
Beamforming_SetBeamFormEnter(
	struct rtl8192cd_priv *priv,
	u1Byte				Idx
	)
{
	ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s] Enter\n", __FUNCTION__)); 
#ifdef CONFIG_WLAN_HAL_8192EE
		if (GET_CHIP_VER(priv)== VERSION_8192E)	
			SetBeamformEnter92E(priv, Idx);
#endif
#ifdef CONFIG_RTL_8812_SUPPORT
		if (GET_CHIP_VER(priv)== VERSION_8812E)	
			SetBeamformEnter8812(priv, Idx);
#endif
#ifdef CONFIG_WLAN_HAL_8814AE
		if(GET_CHIP_VER(priv) == VERSION_8814A)
			SetBeamformEnter8814A(priv,Idx);
#endif

#ifdef CONFIG_WLAN_HAL_8881A
		if(GET_CHIP_VER(priv)== VERSION_8881A)
			SetBeamformEnter8881A(priv, Idx);
#endif

#ifdef CONFIG_WLAN_HAL_8197F
		if(GET_CHIP_VER(priv) == VERSION_8197F)
			SetBeamformEnter8197F(priv,Idx);
#endif

#ifdef CONFIG_WLAN_HAL_8822BE
		if(GET_CHIP_VER(priv) == VERSION_8822B)
			SetBeamformEnter8822B(priv,Idx);
#endif
}


VOID
Beamforming_NDPARate(
	struct rtl8192cd_priv *priv,
	BOOLEAN		Mode,
	u1Byte		BW,
	u1Byte		Rate
	)
{
#ifdef CONFIG_WLAN_HAL_8192EE
	if(GET_CHIP_VER(priv)== VERSION_8192E){
		Beamforming_NDPARate_92E(priv, Mode, BW, Rate);
	}		
#endif
#ifdef CONFIG_RTL_8812_SUPPORT
	if(GET_CHIP_VER(priv)== VERSION_8812E) {
		Beamforming_NDPARate_8812(priv, Mode, BW, Rate);  //
	}
#endif
#ifdef CONFIG_WLAN_HAL_8814AE
	if(GET_CHIP_VER(priv)== VERSION_8814A){
		Beamforming_NDPARate_8814A(priv, Mode, BW, Rate);
	}		
#endif

#ifdef CONFIG_WLAN_HAL_8197F
	if(GET_CHIP_VER(priv)== VERSION_8197F){
		Beamforming_NDPARate_8197F(priv, Mode, BW, Rate);
	}		
#endif
}

VOID
Beamforming_SetHWTimer(
	struct rtl8192cd_priv *priv,
	u2Byte	t
	)
{
#ifdef CONFIG_WLAN_HAL_8192EE
		if(GET_CHIP_VER(priv)== VERSION_8192E)
		{			
			HW_VAR_HW_REG_TIMER_STOP_92E(priv);
			HW_VAR_HW_REG_TIMER_INIT_92E(priv, t);
			HW_VAR_HW_REG_TIMER_START_92E(priv);
		}
#endif
#ifdef CONFIG_RTL_8812_SUPPORT
		if(GET_CHIP_VER(priv)== VERSION_8812E)
		{
			HW_VAR_HW_REG_TIMER_STOP_8812(priv);
			HW_VAR_HW_REG_TIMER_INIT_8812(priv, t);
			HW_VAR_HW_REG_TIMER_START_8812(priv);
		}
#endif
#ifdef CONFIG_WLAN_HAL_8814AE
		if(GET_CHIP_VER(priv)== VERSION_8814A)
		{			
			HW_VAR_HW_REG_TIMER_STOP_8814A(priv);
			HW_VAR_HW_REG_TIMER_INIT_8814A(priv, t);
			HW_VAR_HW_REG_TIMER_START_8814A(priv);
		}
#endif
}

VOID
Beamforming_StopHWTimer(
	struct rtl8192cd_priv *priv
	)
{
#ifdef CONFIG_WLAN_HAL_8192EE
		if(GET_CHIP_VER(priv)== VERSION_8192E)
		{			
			HW_VAR_HW_REG_TIMER_STOP_92E(priv);
		}
#endif
#ifdef CONFIG_RTL_8812_SUPPORT
		if(GET_CHIP_VER(priv)== VERSION_8812E)
		{
			HW_VAR_HW_REG_TIMER_STOP_8812(priv);
		}
#endif
#ifdef CONFIG_WLAN_HAL_8814AE
		if(GET_CHIP_VER(priv)== VERSION_8814A)
		{			
			HW_VAR_HW_REG_TIMER_STOP_8814A(priv);
		}
#endif
}

u1Byte
Beamforming_GetHTNDPTxRate(
	struct rtl8192cd_priv *priv,
	u1Byte	CompSteeringNumofBFer
)
{
	u1Byte Nr_index = 0;
	u1Byte NDPTxRate;

#ifdef CONFIG_WLAN_HAL_8814AE
	if(GET_CHIP_VER(priv)== VERSION_8814A)
		Nr_index = TxBF_Nr(halTxbf8814A_GetNtx(priv), CompSteeringNumofBFer);		/*find Nr*/
	else
#endif
		Nr_index = TxBF_Nr(1, CompSteeringNumofBFer);		/*find Nr*/
	switch(Nr_index)
	{
		case 1:
		NDPTxRate = _MCS8_RATE_;
		break;

		case 2:
		NDPTxRate = _MCS16_RATE_;
		break;

		case 3:
		NDPTxRate = _MCS24_RATE_;
		break;
			
		default:
		NDPTxRate = _MCS8_RATE_;
		break;
	
	}

return NDPTxRate;

}

u1Byte
Beamforming_GetVHTNDPTxRate(
	struct rtl8192cd_priv *priv,
	u1Byte	CompSteeringNumofBFer
)
{
	u1Byte Nr_index = 0;
	u1Byte NDPTxRate;

#ifdef CONFIG_WLAN_HAL_8814AE
	if(GET_CHIP_VER(priv)== VERSION_8814A)
		Nr_index = TxBF_Nr(halTxbf8814A_GetNtx(priv), CompSteeringNumofBFer);		/*find Nr*/
	else
#endif
		Nr_index = TxBF_Nr(1, CompSteeringNumofBFer);		/*find Nr*/
	
	switch(Nr_index)
	{
		case 1:
		NDPTxRate = _NSS2_MCS0_RATE_;
		break;

		case 2:
		NDPTxRate = _NSS3_MCS0_RATE_;
		break;

		case 3:
		NDPTxRate = _NSS4_MCS0_RATE_;
		break;
			
		default:
		NDPTxRate = _NSS2_MCS0_RATE_;
		break;
	
	}

return NDPTxRate;

}


VOID
PacketAppendData(
	IN	POCTET_STRING	packet,
	IN	OCTET_STRING	data
	)
{
	pu1Byte buf = packet->Octet + packet->Length;
	memcpy( buf, data.Octet, data.Length);
	packet->Length = packet->Length + data.Length;
}



BEAMFORMING_CAP
Beamforming_GetEntryBeamCapByMacId(
	struct rtl8192cd_priv *priv,
	IN	u1Byte		MacId
	)
{
	u1Byte	i = 0;
	PRT_BEAMFORMING_INFO	pBeamformingInfo = &(priv->pshare->BeamformingInfo);
	BEAMFORMING_CAP			BeamformEntryCap = BEAMFORMING_CAP_NONE;
	
	for(i = 0; i < BEAMFORMEE_ENTRY_NUM; i++)
	{
		if(pBeamformingInfo->BeamformeeEntry[i].bUsed &&
			(MacId == pBeamformingInfo->BeamformeeEntry[i].MacId))
		{
			BeamformEntryCap =  pBeamformingInfo->BeamformeeEntry[i].BeamformEntryCap;
			i = BEAMFORMEE_ENTRY_NUM;
		}
	}

	return BeamformEntryCap;
}


PRT_BEAMFORMING_ENTRY
Beamforming_GetBFeeEntryByAddr(
	struct rtl8192cd_priv *priv,
	IN	pu1Byte		RA,
	OUT	pu1Byte		Idx
	)
{
	u1Byte	i = 0;
	PRT_BEAMFORMING_INFO pBeamformingInfo = &(priv->pshare->BeamformingInfo);
	
	for(i = 0; i < BEAMFORMEE_ENTRY_NUM; i++)
	{
		if((pBeamformingInfo->BeamformeeEntry[i].bUsed) && 
			((isEqualMACAddr(RA, pBeamformingInfo->BeamformeeEntry[i].MacAddr))))
		{
			*Idx = i;
			return &(pBeamformingInfo->BeamformeeEntry[i]);
		}
	}
	return NULL;
}

PRT_BEAMFORMER_ENTRY
Beamforming_GetBFerEntryByAddr(
	struct rtl8192cd_priv *priv,
	IN	pu1Byte		RA,
	OUT	pu1Byte		Idx
	)
{
	u1Byte	i = 0;
	PRT_BEAMFORMING_INFO pBeamformingInfo = &(priv->pshare->BeamformingInfo);
	
	for(i = 0; i < BEAMFORMER_ENTRY_NUM; i++)
	{
		if((pBeamformingInfo->BeamformerEntry[i].bUsed) && 
			((isEqualMACAddr(RA, pBeamformingInfo->BeamformerEntry[i].MacAddr))))
		{
			*Idx = i;
			return &(pBeamformingInfo->BeamformerEntry[i]);
		}
	}
	return NULL;
}


PRT_BEAMFORMING_ENTRY
Beamforming_GetEntryByMacId(
	struct rtl8192cd_priv *priv,
	u1Byte		MacId,
	pu1Byte		Idx
	)
{
	u1Byte	i = 0;
	PRT_BEAMFORMING_INFO pBeamformingInfo = &(priv->pshare->BeamformingInfo);
	
	for(i = 0; i < BEAMFORMEE_ENTRY_NUM; i++)
	{
		if(pBeamformingInfo->BeamformeeEntry[i].bUsed && 
			(MacId == pBeamformingInfo->BeamformeeEntry[i].MacId))
		{
			*Idx = i;
			return &(pBeamformingInfo->BeamformeeEntry[i]);
		}
	}

	return NULL;

}

PRT_BEAMFORMING_ENTRY
Beamforming_GetFreeBFeeEntry(
	struct rtl8192cd_priv *priv,
    OUT	pu1Byte		Idx,
    pu1Byte             RA
	)
{
	u1Byte	i = 0;
	PRT_BEAMFORMING_INFO pBeamformingInfo = &(priv->pshare->BeamformingInfo);
	
	ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]%s Start!\n", __FUNCTION__));

	ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]%s, RA= 0x %x%x%x%x%x%x\n",
		__FUNCTION__,
		RA[0],RA[1],RA[2],RA[3],RA[4],RA[5]));

	ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]%s, DelBFeeEntry_Idx0= 0x %x%x%x%x%x%x\n",
	__FUNCTION__,
    pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx0[0],
    pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx0[1],
    pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx0[2],
    pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx0[3],
    pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx0[4],
    pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx0[5]));

    if( (isEqualMACAddr(RA, pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx0)) ){
		ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]@%s, (memcmp(RA, pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx0, MACADDRLEN)) == 1, return NULL!!\n", __FUNCTION__));
        return NULL;
    }
    else if((isEqualMACAddr(RA, pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx1))){
		ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]@%s, (memcmp(RA, pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx1, MACADDRLEN)) == 1, return NULL!!\n", __FUNCTION__));
        return NULL;
    }

	for(i = 0; i < BEAMFORMEE_ENTRY_NUM; i++)
	{
		//DbgPrint("Check Point 1: BeamformeeEntry[%d].bUsed = %d\n", i, pBeamformingInfo->BeamformeeEntry[i].bUsed);
		if(pBeamformingInfo->BeamformeeEntry[i].bUsed == FALSE)
		{
			*Idx = i;
			return &(pBeamformingInfo->BeamformeeEntry[i]);
		}	
	}
	return NULL;
}

PRT_BEAMFORMER_ENTRY
Beamforming_GetFreeBFerEntry(
	struct rtl8192cd_priv *priv,
	OUT	pu1Byte		Idx,
	pu1Byte             RA
	)
{
	u1Byte	i = 0;
	PRT_BEAMFORMING_INFO pBeamformingInfo = &(priv->pshare->BeamformingInfo);
	
	ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]%s Start!\n", __FUNCTION__));
    
    if((isEqualMACAddr(RA, pBeamformingInfo->DelEntryListByMACAddr.BFerEntry_Idx0))){
		ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]@s, (memcmp(RA, pBeamformingInfo->DelEntryListByMACAddr.BFerEntry_Idx0, MACADDRLEN)) == 1, return NULL!!\n", __FUNCTION__));
        return NULL;
    }
    else if((isEqualMACAddr(RA, pBeamformingInfo->DelEntryListByMACAddr.BFerEntry_Idx1))){
		ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]@s, (memcmp(RA, pBeamformingInfo->DelEntryListByMACAddr.BFerEntry_Idx1, MACADDRLEN)) == 1, return NULL!!\n", __FUNCTION__));
        return NULL;
    }
    
	for(i = 0; i < BEAMFORMER_ENTRY_NUM; i++)
	{
		if(pBeamformingInfo->BeamformerEntry[i].bUsed == FALSE)
		{
			*Idx = i;
			return &(pBeamformingInfo->BeamformerEntry[i]);
		}	
	}
	return NULL;
}

VOID
Beamforming_GidPAid(
	struct rtl8192cd_priv *priv,
	struct stat_info	*pstat)
{
//eric-mu ??
#if 1
	if (GET_CHIP_VER(priv) == VERSION_8822B) { //MU-MIMO: to be done
		PRT_BEAMFORMING_INFO pBeamformingInfo = &(priv->pshare->BeamformingInfo);
		PRT_BEAMFORMING_ENTRY	pEntry;
		u1Byte idx;
		
		pstat->g_id = 63;
		for (idx = 0; idx < BEAMFORMEE_ENTRY_NUM; idx++) {
			pEntry = &(pBeamformingInfo->BeamformeeEntry[idx]);
			
			if (pstat->aid == pEntry->AID) {
				pstat->p_aid = pEntry->P_AID;
				break;
			}
		}
	}else
#endif
	if (OPMODE & WIFI_AP_STATE)
	{
		u2Byte	AID = (u2Byte) ((pstat->aid) & 0x1ff); 		//AID[0:8]
		u2Byte	bssid = 0;	

		pstat->g_id = 63;
		
		bssid = ((BSSID[5] & 0xf0) >> 4) ^ (BSSID[5] & 0xf);	// BSSID[44:47] xor BSSID[40:43]
		pstat->p_aid = (AID + bssid * 32) & 0x1ff;		// (dec(A) + dec(B)*32) mod 512		

	}
	else if (OPMODE & WIFI_ADHOC_STATE)
	{
		pstat->p_aid = REMAP_AID(pstat);
		pstat->g_id = 63;

	}
	else if (OPMODE & WIFI_STATION_STATE)
	{
		pstat->g_id = 0;		
		pstat->p_aid = ((int)(pstat->hwaddr[5])<<1) | (pstat->hwaddr[4]>>7);
	}	
}


PRT_BEAMFORMING_ENTRY
Beamforming_AddBFeeEntry(
	struct rtl8192cd_priv *priv,
	struct stat_info	*pSTA,
		pu1Byte				RA,
		u2Byte				AID,
		u2Byte				MacID,
		u1Byte				BW,
		BEAMFORMING_CAP		BeamformCap,
		pu1Byte				Idx,
		u2Byte				CompSteeringNumofBFer
	)
{
	PRT_BEAMFORMING_ENTRY	pEntry;
	pEntry = Beamforming_GetFreeBFeeEntry(priv, Idx,RA);
	PRT_BEAMFORMING_INFO 		pBeamInfo = &(priv->pshare->BeamformingInfo);
	u4Byte i;
#if defined(BEAMFORMING_AUTO) && defined(TXPWR_LMT)
	if(priv->pshare->rf_ft_var.txbf_pwrlmt == TXBF_TXPWRLMT_AUTO) {
		if((BeamformCap & BEAMFORMER_CAP_VHT_SU)) {
			if((Beamforming_GetVHTNDPTxRate(priv, CompSteeringNumofBFer) == _NSS2_MCS0_RATE_) && !priv->pshare->txbferVHT2TX) {
				return NULL;
			}
			#ifdef CONFIG_WLAN_HAL_8814AE  
			if((Beamforming_GetVHTNDPTxRate(priv, CompSteeringNumofBFer) == _NSS3_MCS0_RATE_) && !priv->pshare->txbferVHT3TX) {
				return NULL;
			}
			#endif
		} else if((BeamformCap & BEAMFORMER_CAP_HT_EXPLICIT)) {
			if((Beamforming_GetHTNDPTxRate(priv, CompSteeringNumofBFer) == _MCS8_RATE_) && !priv->pshare->txbferHT2TX) {
				return NULL;
			}
			#ifdef CONFIG_WLAN_HAL_8814AE  
			if((Beamforming_GetHTNDPTxRate(priv, CompSteeringNumofBFer) == _MCS16_RATE_) && !priv->pshare->txbferHT3TX) {
				return NULL;
			}
			#endif
		}
	}
#endif		

    // Check if this MAC address is in DelEntryList
    
	ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]Get BFee Idx = %d\n", __FUNCTION__, *Idx));

	if(pEntry != NULL)
	{	

if(GET_CHIP_VER(priv) == VERSION_8822B){

	pBeamInfo->beamformee_mu_cnt = 0;
	pBeamInfo->beamformee_su_cnt = 0;

	for(i = 0; i < BEAMFORMEE_ENTRY_NUM; i++)
	{
		if(pBeamInfo->BeamformeeEntry[i].bUsed){
			if(pBeamInfo->BeamformeeEntry[i].BeamformEntryCap & BEAMFORM_CAP_VHT_MU_BFER)
				pBeamInfo->beamformee_mu_cnt ++ ;
			else
				pBeamInfo->beamformee_su_cnt ++ ;
		}
	}

	if (pEntry->BeamformEntryCap & BEAMFORM_CAP_VHT_MU_BFER){

		if(pBeamInfo->beamformee_mu_cnt >= MAX_NUM_BEAMFORMEE_MU)
			return NULL;
	}
	else {

		if(pBeamInfo->beamformee_su_cnt && (GET_CHIP_VER_8822(priv) <= 0x2))
			return NULL;

		if(pBeamInfo->beamformee_su_cnt >= MAX_NUM_BEAMFORMEE_SU)
			return NULL;
	}
}
	
		pEntry->bUsed = TRUE;
		pEntry->AID = AID;
		pEntry->MacId = MacID;
		pEntry->BW = BW;

		// AID -> P_AID
		if (OPMODE & WIFI_AP_STATE)
		{
			//eric-mu			
			u2Byte bssid = ((GET_MY_HWADDR[5]>> 4) & 0x0f ) ^ 
							(GET_MY_HWADDR[5] & 0xf);				// BSSID[44:47] xor BSSID[40:43]
			pEntry->P_AID = (AID + (bssid <<5)) & 0x1ff;				// (dec(A) + dec(B)*32) mod 512	

		}
		else if (OPMODE & WIFI_ADHOC_STATE)
		{
//			pEntry->P_AID = AID;
			pEntry->P_AID = 0;
	
		}
		else if (OPMODE & WIFI_STATION_STATE) {
			pEntry->P_AID =  RA[5];						// BSSID[39:47]
			pEntry->P_AID = (pEntry->P_AID << 1) | (RA[4] >> 7 );
		}
		//
			
		memcpy(pEntry->MacAddr, RA, MACADDRLEN);
		pEntry->bTxBF = FALSE;
		pEntry->bSound = FALSE;
		
		pEntry->BeamformEntryCap = BeamformCap;	
		pEntry->BeamformEntryState = BEAMFORMING_ENTRY_STATE_UNINITIALIZE;
		pEntry->LogStatusFailCnt = 0;

		pEntry->CompSteeringNumofBFer = CompSteeringNumofBFer;
		pEntry->pSTA = pSTA;

#if 1
		if(GET_CHIP_VER(priv) == VERSION_8822B) {

		pEntry->HwState = BEAMFORM_ENTRY_HW_STATE_ADD_INIT;

		if (pEntry->BeamformEntryCap & BEAMFORM_CAP_VHT_MU_BFER)
		{
			//panic_printk("eric-mu [%s][%d] P_AID = 0x%x(%d)\n", __FUNCTION__, __LINE__, pEntry->P_AID, pEntry->P_AID);
		
			pBeamInfo->beamformee_mu_cnt ++ ;

			//pEntry->is_mu_sta = TRUE;
			pBeamInfo->FirstMUBFeeIndex = beamform_GetFirstMUBFeeEntryIdx(priv);
			
			// Record HW idx info
			/*
			for (i = 0; i < MAX_NUM_BEAMFORMEE_MU; i++) {
				if ((pBeamInfo->beamformee_mu_reg_maping & BIT(i)) == 0) {
					pBeamInfo->beamformee_mu_reg_maping |= BIT(i);
					pEntry->mu_reg_index = i;				
					break;
				}
			}
			ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("Add BFee entry beamformee_mu_reg_maping=%#X, mu_reg_index=%d\n", pBeamInfo->beamformee_mu_reg_maping, pEntry->mu_reg_index));
			*/
		}
		else 
		{
			pBeamInfo->beamformee_su_cnt ++;
			
			//pEntry->is_mu_sta = FALSE;
			/*
			for (i = 0; i < MAX_NUM_BEAMFORMEE_SU; i++) {
				if ((pBeamInfo->beamformee_su_reg_maping & BIT(i)) == 0) {
					pBeamInfo->beamformee_su_reg_maping |= BIT(i);
					pEntry->su_reg_index = i;
					break;
				}
			}
			ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("Add BFee entry beamformee_su_reg_maping=%#X, su_reg_index=%d\n", pBeamInfo->beamformee_su_reg_maping, pEntry->su_reg_index));
			*/
		}

		}
#endif

#ifdef MBSSID
		if(GET_CHIP_VER(priv) == VERSION_8812E)
			if (GET_MIB(GET_ROOT(priv))->miscEntry.vap_enable)
				rtl8192cd_set_mbssid(priv, RA, *Idx);
#endif
		return pEntry;
	}
	else
		return NULL;
}

PRT_BEAMFORMER_ENTRY
Beamforming_AddBFerEntry(
		struct rtl8192cd_priv *priv,
		pu1Byte				RA,
		u2Byte				AID,
		BEAMFORMING_CAP	BeamformCap,
		pu1Byte				Idx,
		u2Byte				NumofSoundingDim
	)
{
	PRT_BEAMFORMER_ENTRY	pEntry;
	PRT_BEAMFORMING_INFO 		pBeamInfo = &(priv->pshare->BeamformingInfo);
	u4Byte i;
    
	ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]%s Start!\n", __FUNCTION__));

    pEntry = Beamforming_GetFreeBFerEntry(priv, Idx, RA);

	if(pEntry != NULL)
	{	
		pEntry->bUsed = TRUE;			

		// AID -> P_AID
		if (OPMODE & WIFI_AP_STATE)
		{
                u2Byte bssid = ((GET_MY_HWADDR[5]>> 4) & 0x0f ) ^ (GET_MY_HWADDR[5] & 0xf);     // BSSID[44:47] xor BSSID[40:43]
			pEntry->P_AID = (AID + (bssid <<5)) & 0x1ff;				// (dec(A) + dec(B)*32) mod 512	

		}
		else if (OPMODE & WIFI_ADHOC_STATE)
		{
			pEntry->P_AID = 0;
		}
		else if (OPMODE & WIFI_STATION_STATE) {
			pEntry->P_AID =  RA[5];						// BSSID[39:47]
			pEntry->P_AID = (pEntry->P_AID << 1) | (RA[4] >> 7 );
		}

		memcpy(pEntry->MacAddr, RA, MACADDRLEN);
		pEntry->BeamformEntryCap = BeamformCap;	
		pEntry->NumofSoundingDim = NumofSoundingDim;
		
            pEntry->ClockResetTimes = 0;
            pEntry->NDPAPreLogSeq = 0;
            pEntry->NDPALogSeq = 0;
            pEntry->NDPALogRetryCnt = 0;
            pEntry->NDPALogSuccess = 0;
//            pEntry->LogStatusFailCnt = 0;


#if 1 //eric-txbf 
if(GET_CHIP_VER(priv) == VERSION_8822B){
		//panic_printk("\n\n !!pEntry->BeamformEntryCap = 0x%x pBeamInfo->BeamformCap = 0x%x \n\n");

		if ((pBeamInfo->BeamformCap & BEAMFORM_CAP_VHT_MU_BFEE) &&
			(pEntry->BeamformEntryCap & BEAMFORM_CAP_VHT_MU_BFER)) {
			pBeamInfo->beamformer_mu_cnt += 1;
			//pEntry->is_mu_ap = TRUE;
			pEntry->AID = AID;
		} else if ((pBeamInfo->BeamformCap & (BEAMFORMEE_CAP_VHT_SU|BEAMFORMEE_CAP_HT_EXPLICIT)) &&
			(pEntry->BeamformEntryCap & (BEAMFORMER_CAP_VHT_SU|BEAMFORMER_CAP_HT_EXPLICIT))) {
			pBeamInfo->beamformer_su_cnt += 1;

			// Record HW idx info
			for (i = 0; i < MAX_NUM_BEAMFORMER_SU; i++) {
				if ((pBeamInfo->beamformer_su_reg_maping & BIT(i)) == 0) {
					pBeamInfo->beamformer_su_reg_maping |= BIT(i);
					pEntry->su_reg_index = i;
					break;
				}
			}
			ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("Add BFer entry beamformer_su_reg_maping=%#X, su_reg_index=%d\n", pBeamInfo->beamformer_su_reg_maping, pEntry->su_reg_index));
		}
}
#endif
			ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]@%s, BFer Entry= 0x%x\n", __FUNCTION__, pEntry));

		return pEntry;
	}
	else
		return NULL;
}


BOOLEAN
Beamforming_RemoveEntry(
	struct rtl8192cd_priv *priv,
	IN	pu1Byte		RA,
	OUT	pu1Byte		Idx
	)
{
	PRT_BEAMFORMER_ENTRY	pBFerEntry = Beamforming_GetBFerEntryByAddr(priv, RA, Idx);
    PRT_BEAMFORMING_ENTRY   pBFeeEntry = Beamforming_GetBFeeEntryByAddr(priv, RA, Idx);
    PRT_BEAMFORMING_INFO    pBeamInfo = &(priv->pshare->BeamformingInfo);

    u1Byte  i;
    // CurDelBFerBFeeEntrySel had been decided by the function who calls Beamforming_DeInitEntry
    DEL_ENTRY_TYPE_SEL  CurDelBFerBFeeEntrySel=pBeamInfo->CurDelBFerBFeeEntrySel;
    
	BOOLEAN ret = FALSE, bHwStateAddInit = FALSE;

//    if( (CurDelBFerBFeeEntrySel== BFerEntry)||(CurDelBFerBFeeEntrySel == BFerBFeeEntry) )
/*
    if( CurDelBFerBFeeEntrySel == BFerEntry)
    {
        if (pBFerEntry != NULL)
        {
		pBFerEntry->bUsed = FALSE;
		pBFerEntry->BeamformEntryCap = BEAMFORMING_CAP_NONE;

            if(*Idx==0)
                memcpy(&pBeamformingInfo->DelEntryListByMACAddr.BFerEntry_Idx0[0], RA, 6);                
            else
                memcpy(&pBeamformingInfo->DelEntryListByMACAddr.BFerEntry_Idx1[0], RA, 6);
            
		ret = TRUE;
            ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]@%s, Remove BFerEntry idx=%d\n", __FUNCTION__, *Idx));
        }
    }
*/
    if (pBFerEntry != NULL)
    {
        pBFerEntry->bUsed = FALSE;
        pBFerEntry->BeamformEntryCap = BEAMFORMING_CAP_NONE;

        if(CurDelBFerBFeeEntrySel == BFerEntry)
        {
            if(*Idx==0)
		        memcpy(pBeamInfo->DelEntryListByMACAddr.BFerEntry_Idx0, RA, 6);                
            else
		        memcpy(pBeamInfo->DelEntryListByMACAddr.BFerEntry_Idx1, RA, 6);
	}
	
#if 1 //eric-txbf
if(GET_CHIP_VER(priv) == VERSION_8822B){	
		if (pBFerEntry->BeamformEntryCap & BEAMFORM_CAP_VHT_MU_BFEE)
		{
			pBeamInfo->beamformer_mu_cnt -= 1;			
			PlatformZeroMemory(pBFerEntry->gid_valid, 8);
			PlatformZeroMemory(pBFerEntry->user_position, 16);
		}
		else if(pBFerEntry->BeamformEntryCap & (BEAMFORMER_CAP_VHT_SU|BEAMFORMER_CAP_HT_EXPLICIT))
			pBeamInfo->beamformer_su_cnt -= 1;		
		//pBFerEntry->BeamformEntryCap = BEAMFORMING_CAP_NONE;
		bHwStateAddInit = (pBFerEntry->HwState == BEAMFORM_ENTRY_HW_STATE_ADD_INIT)? TRUE:FALSE;
		pBFerEntry->HwState = BEAMFORM_ENTRY_HW_STATE_DELETE_INIT;

#if 0		
		if(pBeamInfo->beamformer_mu_cnt == 0)
			pBeamInfo->BeamformCap &= ~(BEAMFORM_CAP_VHT_MU_BFEE);
		if(pBeamInfo->beamformer_su_cnt == 0)
			pBeamInfo->BeamformCap &= ~(BEAMFORMEE_CAP_VHT_SU|BEAMFORMEE_CAP_HT_EXPLICIT);
#endif
}
#endif
		ret = TRUE;
	}

    if (pBFeeEntry != NULL) 
    {
        pBFeeEntry->bUsed = FALSE;
        pBFeeEntry->BeamformEntryCap = BEAMFORMING_CAP_NONE;

        if(CurDelBFerBFeeEntrySel == BFeeEntry)
        {
            if(*Idx==0)
		        memcpy(&pBeamInfo->DelEntryListByMACAddr.BFeeEntry_Idx0[0], RA, 6);            
            else
		        memcpy(&pBeamInfo->DelEntryListByMACAddr.BFeeEntry_Idx1[0], RA, 6);
		}
#if 1 //eric-txbf
if(GET_CHIP_VER(priv) == VERSION_8822B){
		if (pBFeeEntry->BeamformEntryCap & BEAMFORM_CAP_VHT_MU_BFER) {
			pBeamInfo->beamformee_mu_cnt -= 1;
			pBeamInfo->FirstMUBFeeIndex = beamform_GetFirstMUBFeeEntryIdx(priv);
		} else if(pBFeeEntry->BeamformEntryCap & (BEAMFORMEE_CAP_VHT_SU|BEAMFORMEE_CAP_HT_EXPLICIT)){
			pBeamInfo->beamformee_su_cnt -= 1;
}
#endif
        }
        
#if 1 //eric-txbf
if(GET_CHIP_VER(priv) == VERSION_8822B){
		//pBFeeEntry->BeamformEntryCap = BEAMFORMING_CAP_NONE;
		//pBFeeEntry->bBeamformingInProgress = FALSE;
		bHwStateAddInit = (pBFeeEntry->HwState == BEAMFORM_ENTRY_HW_STATE_ADD_INIT)? TRUE:FALSE;
		pBFeeEntry->HwState = BEAMFORM_ENTRY_HW_STATE_DELETE_INIT;

		//if(pBeamInfo->beamformee_mu_cnt == 0)
			//pBeamInfo->BeamformCap &= ~(BEAMFORM_CAP_VHT_MU_BFER);
		//if(pBeamInfo->beamformee_su_cnt == 0)
			//pBeamInfo->BeamformCap &= ~(BEAMFORMER_CAP_VHT_SU|BEAMFORMER_CAP_HT_EXPLICIT);

		beamform_UpdateMinSoundingPeriod(priv, 0, TRUE);
}
#endif        
        ret = TRUE;    
    }
    
#if 1 //eric-txbf    
if(GET_CHIP_VER(priv) == VERSION_8822B){
	if (ret == TRUE)
	{
		if(bHwStateAddInit)
			pBeamInfo->SetHalBFEnterOnDemandCnt--;
		pBeamInfo->SetHalBFLeaveOnDemandCnt++;
		//Beamform_SetHwConfig(Adapter, BEAMFORM_SET_HW_TYPE_LEAVE);
	}
}
#endif   
/*
    if( (CurDelBFerBFeeEntrySel == BFeeEntry)||(CurDelBFerBFeeEntrySel == BFerBFeeEntry) )
    {
        if (pBFeeEntry != NULL) 
        {
            pBFeeEntry->bUsed = FALSE;
            pBFeeEntry->BeamformEntryCap = BEAMFORMING_CAP_NONE;

            if(*Idx==0)
                memcpy(&pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx0[0], RA, 6);            
            else
                memcpy(&pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx1[0], RA, 6);

            ret = TRUE;
        
            ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]@%s, Remove BFeeEntry idx=%d\n", __FUNCTION__, *Idx));
            ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]@%s, RA=0x%x%x%x%x%x%x\n",
                __FUNCTION__,
                RA[0],RA[1],RA[2],RA[3],RA[4],RA[5]));

            ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]@%s, BFeeEntry_Idx0=0x%x%x%x%x%x%x, BFeeEntry_Idx1=0x%x%x%x%x%x%x\n",
            __FUNCTION__,
            pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx0[0],
            pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx0[1],
            pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx0[2],
            pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx0[3],
            pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx0[4],
            pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx0[5],
            pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx1[0],
            pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx1[1],
            pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx1[2],
            pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx1[3],
            pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx1[4],
            pBeamformingInfo->DelEntryListByMACAddr.BFeeEntry_Idx1[5]
            ));

        }
            
    }
*/
	return ret;
}

BOOLEAN
Beamforming_InitEntry(
	struct rtl8192cd_priv	*priv,
	struct stat_info		*pSTA,
	pu1Byte				BFerBFeeIdx	
	)
{

	PRT_BEAMFORMING_ENTRY	pBeamformEntry = NULL;
	PRT_BEAMFORMER_ENTRY	pBeamformerEntry = NULL;
	pu1Byte					RA; 
	u2Byte					AID, MacID;
	u1Byte					WirelessMode;
	u1Byte					BW = HT_CHANNEL_WIDTH_20;
	BEAMFORMING_CAP		BeamformCap = BEAMFORMING_CAP_NONE;	
	u1Byte					BFerIdx = 0xF, BFeeIdx = 0xF;
	u2Byte					CompSteeringNumofBFer = 0, NumofSoundingDim = 0;

	//DbgPrint("%s => txbf = %d, txbfer = %d, txbfee = %d\n", __FUNCTION__, priv->pmib->dot11RFEntry.txbf, priv->pmib->dot11RFEntry.txbfer, priv->pmib->dot11RFEntry.txbfee);
	

	// The current setting does not support Beaforming
	if (priv->pmib->dot11RFEntry.txbf == 0)
		return FALSE;

	//DbgPrint("%s => 2\n", __FUNCTION__);

	// IBSS, AP mode
	if (pSTA != NULL) {
		AID = pSTA->aid;
		RA  = pSTA->hwaddr;
		MacID = pSTA->aid;

		WirelessMode = pSTA->WirelessMode;
		BW = pSTA->tx_bw;
	} else	// Client mode
		return FALSE;

	if ( WirelessMode < WIRELESS_MODE_N_24G)
		return FALSE;
	else 
	{

// BIT 4 implies capable of sending NDPA (BFER),
// BIT 3 implies capable of receiving NDPA (BFEE),
		if (WirelessMode == WIRELESS_MODE_N_24G || WirelessMode == WIRELESS_MODE_N_5G)
		{
			if(pSTA->ht_cap_len && (cpu_to_le32(pSTA->ht_cap_buf.txbf_cap) & 0x8)&& (priv->pmib->dot11RFEntry.txbfer == 1))	//bfer
			{
				BeamformCap |=BEAMFORMER_CAP_HT_EXPLICIT;
				CompSteeringNumofBFer = (u1Byte)((cpu_to_le32(pSTA->ht_cap_buf.txbf_cap) & (BIT(23)|BIT(24)))>>23);
		//		panic_printk("[%d] BeamformCap = BEAMFORMER_CAP_HT_EXPLICIT \n",__LINE__);
			}
			if (pSTA->ht_cap_len && (cpu_to_le32(pSTA->ht_cap_buf.txbf_cap) & 0x10)&& (priv->pmib->dot11RFEntry.txbfee == 1))	//bfee
			{
				BeamformCap |=BEAMFORMEE_CAP_HT_EXPLICIT;
				NumofSoundingDim = (u1Byte)((cpu_to_le32(pSTA->ht_cap_buf.txbf_cap) & (BIT(27)|BIT(28)))>>27);
		//		panic_printk("[%d] BeamformCap = BEAMFORMEE_CAP_HT_EXPLICIT \n",__LINE__);
			}
		}
#ifdef RTK_AC_SUPPORT			
		if(WirelessMode == WIRELESS_MODE_AC_5G || WirelessMode == WIRELESS_MODE_AC_24G)
		{
			if(pSTA->vht_cap_len && (cpu_to_le32(pSTA->vht_cap_buf.vht_cap_info) & BIT(SU_BFEE_S)) && (priv->pmib->dot11RFEntry.txbfer == 1))  // AC�٨S�̾�bfer or bfee��
			{
			BeamformCap |=BEAMFORMER_CAP_VHT_SU;
			CompSteeringNumofBFer = (u1Byte)((cpu_to_le32(pSTA->vht_cap_buf.vht_cap_info) & (BIT(MAX_ANT_SUPP_S)|BIT(MAX_ANT_SUPP_S+1)|BIT(MAX_ANT_SUPP_E)))>>MAX_ANT_SUPP_S);
			}
			if(pSTA->vht_cap_len && (cpu_to_le32(pSTA->vht_cap_buf.vht_cap_info) & BIT(SU_BFER_S)) && (priv->pmib->dot11RFEntry.txbfee == 1))
			{
			BeamformCap |=BEAMFORMEE_CAP_VHT_SU;
			NumofSoundingDim = (u1Byte)((cpu_to_le32(pSTA->vht_cap_buf.vht_cap_info) & (BIT(SOUNDING_DIMENSIONS_S)|BIT(SOUNDING_DIMENSIONS_S+1)|BIT(SOUNDING_DIMENSIONS_E)))>>SOUNDING_DIMENSIONS_S);
			}
#if 1 //eric-txbf
if((GET_CHIP_VER(priv) == VERSION_8822B) && priv->pmib->dot11RFEntry.txbf_mu){
			if(pSTA->vht_cap_len && (cpu_to_le32(pSTA->vht_cap_buf.vht_cap_info) & BIT(MU_BFEE_S)) && (priv->pmib->dot11RFEntry.txbfer == 1))  // AC�٨S�̾�bfer or bfee��
			{
			BeamformCap |=BEAMFORM_CAP_VHT_MU_BFER;
			CompSteeringNumofBFer = (u1Byte)((cpu_to_le32(pSTA->vht_cap_buf.vht_cap_info) & (BIT(MAX_ANT_SUPP_S)|BIT(MAX_ANT_SUPP_S+1)|BIT(MAX_ANT_SUPP_E)))>>MAX_ANT_SUPP_S);
			}
#if 0 //eric-mu2 disable mu txbfee
			if(pSTA->vht_cap_len && (cpu_to_le32(pSTA->vht_cap_buf.vht_cap_info) & BIT(MU_BFER_S)) && (priv->pmib->dot11RFEntry.txbfee == 1))
			{
			BeamformCap |=BEAMFORM_CAP_VHT_MU_BFEE;
			NumofSoundingDim = (u1Byte)((cpu_to_le32(pSTA->vht_cap_buf.vht_cap_info) & (BIT(SOUNDING_DIMENSIONS_S)|BIT(SOUNDING_DIMENSIONS_S+1)|BIT(SOUNDING_DIMENSIONS_E)))>>SOUNDING_DIMENSIONS_S);
			}
#endif
}
#endif
		}
#endif		
	}

	if(BeamformCap == BEAMFORMING_CAP_NONE)
		return FALSE;

	ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, CompSteeringNumofBFer = %d, NumofSoundingDim = %d\n", __FUNCTION__, CompSteeringNumofBFer, NumofSoundingDim));
// bfme
	if((BeamformCap & BEAMFORMEE_CAP_HT_EXPLICIT) || (BeamformCap & BEAMFORMEE_CAP_VHT_SU))
	{
		pBeamformerEntry = Beamforming_GetBFerEntryByAddr(priv, RA, &BFerIdx);

		if(pBeamformerEntry == NULL)
		{
			pBeamformerEntry = Beamforming_AddBFerEntry(priv, RA, AID, BeamformCap, &BFerIdx, NumofSoundingDim);

			if(pBeamformerEntry == NULL) {
				ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, Not enough BFer entry!\n", __FUNCTION__));
#if defined(UNIVERSAL_REPEATER)
				if(IS_VXD_INTERFACE(priv)) {
					unsigned int reuse_idx = priv->dev->name[4]-'0';

					if(reuse_idx < BEAMFORMEE_ENTRY_NUM) {
						if(!Beamforming_DeInitEntry(priv,priv->pshare->BeamformingInfo.BeamformerEntry[reuse_idx].MacAddr)) {
							printk("[%s]Force remove BeamformerEntry[%d] failed\n",priv->dev->name,reuse_idx);
						} else {
							Beamforming_Notify(priv);
							pBeamformerEntry = Beamforming_AddBFerEntry(priv, RA, AID, BeamformCap, &BFerIdx, NumofSoundingDim);
							DEBUG_INFO("[%s]Reuse BeamformerEntry[%d]\n",priv->dev->name,BFerIdx);
							if(!pBeamformerEntry)
								printk("[%s]Reuse BeamformerEntry[%d] failed\n",priv->dev->name,BFerIdx);
						}
					} else {
						printk("Invalid index(%d) for reuse BeamformerEntry, beamformer is inactive!\n",reuse_idx);
					}
				}
#endif
			}
#if defined(UNIVERSAL_REPEATER)
			else {
				if(IS_VXD_INTERFACE(priv))
					DEBUG_INFO("[%s]Occupy BeamformerEntry[%d]\n",priv->dev->name,BFerIdx);
			}
#endif
		}
	}

// bfer
	if((BeamformCap & BEAMFORMER_CAP_HT_EXPLICIT) || (BeamformCap & BEAMFORMER_CAP_VHT_SU))
	{
		pBeamformEntry = Beamforming_GetBFeeEntryByAddr(priv, RA, &BFeeIdx);
		
		if(pBeamformEntry == NULL)
		{
			pBeamformEntry = Beamforming_AddBFeeEntry(priv, pSTA, RA, AID, MacID, BW, BeamformCap, &BFeeIdx, CompSteeringNumofBFer);

			if(pBeamformEntry == NULL)
			{
#if defined(UNIVERSAL_REPEATER)
				if(IS_VXD_INTERFACE(priv)) {
					unsigned int reuse_idx = priv->dev->name[4]-'0';

					if(reuse_idx < BEAMFORMEE_ENTRY_NUM) {
						if(!Beamforming_DeInitEntry(priv,priv->pshare->BeamformingInfo.BeamformeeEntry[reuse_idx].MacAddr)) {
							printk("[%s]Force remove BeamformeeEntry[%d] failed\n",priv->dev->name,reuse_idx);
						} else {
							Beamforming_Notify(priv);
							pBeamformEntry = Beamforming_AddBFeeEntry(priv, pSTA, RA, AID, MacID, BW, BeamformCap, &BFeeIdx, CompSteeringNumofBFer);
							DEBUG_INFO("[%s]Reuse BeamformeeEntry[%d]\n",priv->dev->name,BFeeIdx);
							if(pBeamformEntry)
								pBeamformEntry->BeamformEntryState = BEAMFORMING_ENTRY_STATE_INITIALIZEING;
							else
								printk("[%s]Reuse BeamformeeEntry[%d] failed\n",priv->dev->name,BFeeIdx);
						}
					} else {
						printk("Invalid index(%d) for reuse BeamformerEntry, beamformee is inactive!\n",reuse_idx);
					}
				} else
#endif
				return NULL;
			}
			else
				pBeamformEntry->BeamformEntryState = BEAMFORMING_ENTRY_STATE_INITIALIZEING;
		}	
		else
		{
#if defined(UNIVERSAL_REPEATER)
			if(IS_VXD_INTERFACE(priv))
				DEBUG_INFO("[%s]Occupy BeamformEntry[%d]\n",priv->dev->name,BFerIdx);
#endif
			// Entry has been created. If entry is initialing or progressing then errors occur.
			if(	pBeamformEntry->BeamformEntryState != BEAMFORMING_ENTRY_STATE_INITIALIZED && 
				pBeamformEntry->BeamformEntryState != BEAMFORMING_ENTRY_STATE_PROGRESSED)
			{
				ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, Error State of Beamforming\n", __FUNCTION__));
				return FALSE;
			}	
			else
				pBeamformEntry->BeamformEntryState = BEAMFORMING_ENTRY_STATE_INITIALIZEING;
		}

		pBeamformEntry->BeamformEntryState = BEAMFORMING_ENTRY_STATE_INITIALIZED;
		Beamforming_AutoTest(priv, BFeeIdx, pBeamformEntry);
	}

	*BFerBFeeIdx = (BFerIdx<<4) | BFeeIdx;
	ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s,  BFerIdx=0x%x, BFeeIdx=0x%x, BFerBFeeIdx=0x%x \n", __FUNCTION__, BFerIdx, BFeeIdx, *BFerBFeeIdx));

	return TRUE;
}

BOOLEAN
Beamforming_DeInitEntry(
	struct rtl8192cd_priv *priv,
	pu1Byte			RA
	)
{
	u1Byte					Idx = 0;

	ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]%s Start!!!\n", __FUNCTION__));
	
	if(Beamforming_RemoveEntry(priv, RA, &Idx) == TRUE)
	{
		Beamforming_SetBeamFormLeave(priv, Idx);

#ifdef CONFIG_RTL_8812_SUPPORT				// 8812 only??
#ifdef MBSSID
		if (GET_MIB(GET_ROOT(priv))->miscEntry.vap_enable)
			rtl8192cd_clear_mbssid(priv, Idx);
#endif
#endif
		
		return TRUE;
	}
	else
	{
		// For AP debug, because when STA disconnect AP, release_stainfo will be triggered many times
		return FALSE;
	}

}

VOID
BeamformingReset(
	struct rtl8192cd_priv *priv
	)
{
	u1Byte		Idx = 0;
	PRT_BEAMFORMING_INFO pBeamformingInfo = &(priv->pshare->BeamformingInfo);

	for(Idx = 0; Idx < BEAMFORMEE_ENTRY_NUM; Idx++)
	{
		if(pBeamformingInfo->BeamformeeEntry[Idx].bUsed == TRUE)
		{
			ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, Reset entry idx=%d\n", __FUNCTION__, Idx));
			pBeamformingInfo->BeamformeeEntry[Idx].bUsed = FALSE;
			pBeamformingInfo->BeamformeeEntry[Idx].BeamformEntryCap = BEAMFORMING_CAP_NONE;
			//pBeamformingInfo->BeamformeeEntry[Idx].BeamformEntryState = BEAMFORMING_ENTRY_STATE_UNINITIALIZE;
			pBeamformingInfo->BeamformeeEntry[Idx].bBeamformingInProgress = FALSE;

			Beamforming_SetBeamFormLeave(priv, Idx);		
		}
	}

	for(Idx = 0; Idx < BEAMFORMER_ENTRY_NUM; Idx++)
	{
		pBeamformingInfo->BeamformerEntry[Idx].bUsed = FALSE;
	}

}



#define FillOctetString(_os,_octet,_len)		\
	(_os).Octet=(pu1Byte)(_octet);			\
	(_os).Length=(_len);

VOID
ConstructHTNDPAPacket(
	struct rtl8192cd_priv *priv,
	pu1Byte				RA,
	pu1Byte				Buffer,
	pu4Byte				pLength,
	u1Byte			 	BW
	)
{
	u2Byte					Duration= 0;
	OCTET_STRING			pNDPAFrame, ActionContent;
	u1Byte					ActionHdr[4] = {ACT_CAT_VENDOR, 0x00, 0xe0, 0x4c};
	int aSifsTime = ((priv->pmib->dot11BssType.net_work_type & WIRELESS_11N) && (priv->pshare->ht_sta_num)) ? 0x10 : 10;


	SET_80211_HDR_FRAME_CONTROL(Buffer,0);
	SET_80211_HDR_ORDER(Buffer, 1);
	SET_80211_HDR_TYPE_AND_SUBTYPE(Buffer,Type_Action_No_Ack);

	memcpy((void *)GetAddr1Ptr(Buffer), RA, MACADDRLEN);
	memcpy((void *)GetAddr2Ptr(Buffer), GET_MY_HWADDR, MACADDRLEN);
	memcpy((void *)GetAddr3Ptr(Buffer), BSSID, MACADDRLEN);

	Duration = 2*aSifsTime + 40;
	
	if(BW== HT_CHANNEL_WIDTH_20_40)
		Duration+= 87;
	else	
		Duration+= 180;

	SET_80211_HDR_DURATION(Buffer, Duration);

	//HT control field
	SET_HT_CTRL_CSI_STEERING(Buffer+sMacHdrLng, 3);
	SET_HT_CTRL_NDP_ANNOUNCEMENT(Buffer+sMacHdrLng, 1);
	
	FillOctetString(pNDPAFrame, Buffer, sMacHdrLng+sHTCLng);

	FillOctetString(ActionContent, ActionHdr, 4);
	PacketAppendData(&pNDPAFrame, ActionContent);	

	*pLength = 32;
}


BOOLEAN
SendHTNDPAPacket(
	struct rtl8192cd_priv *priv,
		pu1Byte				RA,
		u1Byte 				BW,
		u1Byte		NDPTxRate
	)
{
	BOOLEAN					ret = TRUE;
	unsigned char *pbuf 		= get_wlanllchdr_from_poll(priv);
	u4Byte PacketLength;
	DECLARE_TXINSN(txinsn);	

	if(pbuf) 
	{
		memset(pbuf, 0, sizeof(struct wlan_hdr));
		ConstructHTNDPAPacket(
				priv, 
				RA,
				pbuf,
				&PacketLength,
				BW
				);
		
		txinsn.q_num = MGNT_QUEUE;	
		txinsn.fr_type = _PRE_ALLOCLLCHDR_;				

		txinsn.phdr = pbuf;
		txinsn.hdr_len = PacketLength;
		txinsn.fr_len = 0;
		txinsn.tx_rate = NDPTxRate; //_MCS8_RATE_;, According to Nr
		txinsn.fixed_rate = 1;	
		txinsn.ndpa = 1;

		if (rtl8192cd_wlantx(priv, &txinsn) == CONGESTED) {		
			netif_stop_queue(priv->dev);		
			priv->ext_stats.tx_drops++; 	
//			panic_printk("TX DROP: Congested!\n");
			if (txinsn.phdr)
				release_wlanllchdr_to_poll(priv, txinsn.phdr); 			
			if (txinsn.pframe)
				release_mgtbuf_to_poll(priv, txinsn.pframe);			
			return 0;	
		}
	}
	else
		ret = FALSE;

	return ret;
}




VOID
ConstructVHTNDPAPacket(
	struct rtl8192cd_priv *priv,
	pu1Byte			RA,
	u2Byte			AID,
	pu1Byte			Buffer,
	pu4Byte			pLength,
	u1Byte 			BW
	)
{
	u2Byte					Duration= 0;
	u1Byte					Sequence = 0;
	pu1Byte					pNDPAFrame = Buffer;
	u2Byte					tmp16;
	
	RT_NDPA_STA_INFO		STAInfo;
	int aSifsTime = ((priv->pmib->dot11BssType.net_work_type & WIRELESS_11N) && (priv->pshare->ht_sta_num)) ? 0x10 : 10;

	// Frame control.
	SET_80211_HDR_FRAME_CONTROL(pNDPAFrame, 0);
	SET_80211_HDR_TYPE_AND_SUBTYPE(pNDPAFrame, Type_NDPA);

	memcpy((void *)GetAddr1Ptr(pNDPAFrame), RA, MACADDRLEN);
	memcpy((void *)GetAddr2Ptr(pNDPAFrame), GET_MY_HWADDR, MACADDRLEN);

	Duration = 2*aSifsTime + 44;
	
	if(BW == HT_CHANNEL_WIDTH_80)
		Duration += 40;
	else if(BW == HT_CHANNEL_WIDTH_20_40)
		Duration+= 87;
	else	
		Duration+= 180;

	SetDuration(pNDPAFrame, Duration);
	Sequence = GET_HW(priv)->sounding_seq<<2;
	GET_HW(priv)->sounding_seq =  (GET_HW(priv)->sounding_seq+1) & 0xfff;
	 
	memcpy(pNDPAFrame+16, &Sequence,1);

	if (OPMODE & WIFI_ADHOC_STATE)
		AID = 0;

	STAInfo.AID = AID;

	STAInfo.FeedbackType = 0;
	STAInfo.NcIndex = 0;
	
	memcpy(&tmp16, (pu1Byte)&STAInfo, 2);
	tmp16 = cpu_to_le16(tmp16);

	memcpy(pNDPAFrame+17, &tmp16, 2);

	*pLength = 19;
}


BOOLEAN
SendVHTNDPAPacket(
	struct rtl8192cd_priv *priv,
	IN	pu1Byte			RA,
	IN	u2Byte			AID,
	u1Byte 				BW,
	u1Byte		NDPTxRate
	)
{
	BOOLEAN					ret = TRUE;
	u4Byte 					PacketLength;
	unsigned char *pbuf 	= get_wlanllchdr_from_poll(priv);
	DECLARE_TXINSN(txinsn);	

	if(pbuf)
	{
		memset(pbuf, 0, sizeof(struct wlan_hdr));

		ConstructVHTNDPAPacket	(
			priv, 
			RA,
			AID,
			pbuf,
			&PacketLength,
			BW
			);

		txinsn.q_num = MANAGE_QUE_NUM;
		txinsn.fr_type = _PRE_ALLOCLLCHDR_;		
		txinsn.phdr = pbuf;
		txinsn.hdr_len = PacketLength;
		txinsn.fr_len = 0;
		txinsn.fixed_rate = 1;	
		txinsn.tx_rate = NDPTxRate;	// According to Nr	
		txinsn.ndpa = 1;

		if (rtl8192cd_wlantx(priv, &txinsn) == CONGESTED) {		
			netif_stop_queue(priv->dev);		
			priv->ext_stats.tx_drops++; 	
//			panic_printk("TX DROP: Congested!\n");
			if (txinsn.phdr)
				release_wlanllchdr_to_poll(priv, txinsn.phdr); 			
			if (txinsn.pframe)
				release_mgtbuf_to_poll(priv, txinsn.pframe);
			return 0;	
		}
	}
	else
		ret = FALSE;

	return ret;
}



u1Byte
beamforming_SoundingIdx(
	PRT_BEAMFORMING_INFO 			pBeamInfo
	)
{
	u1Byte							Idx = 0;
	PRT_BEAMFORMING_PERIOD_INFO	pBeamPeriodInfo = &(pBeamInfo->BeamformingPeriodInfo);

	if(	pBeamPeriodInfo->Mode == SOUNDING_SW_VHT_TIMER ||pBeamPeriodInfo->Mode == SOUNDING_SW_HT_TIMER ||
		pBeamPeriodInfo->Mode == SOUNDING_HW_VHT_TIMER ||pBeamPeriodInfo->Mode == SOUNDING_HW_HT_TIMER)
		Idx = pBeamPeriodInfo->Idx;
	else
		Idx = 0;

	return Idx;
}

BEAMFORMING_NOTIFY_STATE
beamfomring_bSounding_8822B(
	struct rtl8192cd_priv	*priv,
	PRT_BEAMFORMING_INFO 	pBeamInfo,
	pu1Byte					Idx
	)
{
	BEAMFORMING_NOTIFY_STATE		bSounding = BEAMFORMING_NOTIFY_NONE;
	//RT_BEAMFORMING_ENTRY			Entry = pBeamInfo->BeamformeeEntry[*Idx];
	RT_BEAMFORMING_PERIOD_INFO		BeamPeriodInfo = pBeamInfo->BeamformingPeriodInfo;

	if(BeamPeriodInfo.Mode == SOUNDING_STOP_All_TIMER)
		bSounding = BEAMFORMING_NOTIFY_RESET;
//	else if(BeamPeriodInfo.Mode == SOUNDING_STOP_OID_TIMER && Entry.bTxBF == FALSE)
//		bSounding = BEAMFORMING_NOTIFY_RESET;
	else
	{
		u1Byte i;

		for(i=0;i<BEAMFORMEE_ENTRY_NUM;i++)
		{
			//panic_printk("[David]@%s: BFee Entry %d bUsed=%d, bSound=%d \n", __FUNCTION__, i, pBeamInfo->BeamformeeEntry[i].bUsed, pBeamInfo->BeamformeeEntry[i].bSound);
			if(pBeamInfo->BeamformeeEntry[i].bUsed && (!pBeamInfo->BeamformeeEntry[i].bSound))
			{
				*Idx = i;

				if (pBeamInfo->BeamformeeEntry[i].is_mu_sta)
					bSounding = BEAMFORMEE_NOTIFY_ADD_MU;
				else
					bSounding = BEAMFORMEE_NOTIFY_ADD_SU;

			}

			if((!pBeamInfo->BeamformeeEntry[i].bUsed) && pBeamInfo->BeamformeeEntry[i].bSound)
			{
				*Idx = i;

				if (pBeamInfo->BeamformeeEntry[i].is_mu_sta)
					bSounding = BEAMFORMEE_NOTIFY_DELETE_MU;
				else
					bSounding = BEAMFORMEE_NOTIFY_DELETE_SU;

			}
		}
	}

	return bSounding;
}


BEAMFORMING_NOTIFY_STATE
beamfomring_bSounding(
	struct rtl8192cd_priv	*priv,
	PRT_BEAMFORMING_INFO 	pBeamInfo,
	pu1Byte					Idx
	)
{
	BEAMFORMING_NOTIFY_STATE		bSounding = BEAMFORMING_NOTIFY_NONE;
	//RT_BEAMFORMING_ENTRY			Entry = pBeamInfo->BeamformeeEntry[*Idx];
	RT_BEAMFORMING_PERIOD_INFO		BeamPeriodInfo = pBeamInfo->BeamformingPeriodInfo;

	if(BeamPeriodInfo.Mode == SOUNDING_STOP_All_TIMER)
		bSounding = BEAMFORMING_NOTIFY_RESET;
//	else if(BeamPeriodInfo.Mode == SOUNDING_STOP_OID_TIMER && Entry.bTxBF == FALSE)
//		bSounding = BEAMFORMING_NOTIFY_RESET;
	else
	{
		u1Byte i;

		for(i=0;i<BEAMFORMEE_ENTRY_NUM;i++)
		{
			//panic_printk("[David]@%s: BFee Entry %d bUsed=%d, bSound=%d \n", __FUNCTION__, i, pBeamInfo->BeamformeeEntry[i].bUsed, pBeamInfo->BeamformeeEntry[i].bSound);
			if(pBeamInfo->BeamformeeEntry[i].bUsed && (!pBeamInfo->BeamformeeEntry[i].bSound))
			{
				*Idx = i;
				bSounding = BEAMFORMING_NOTIFY_ADD;
			}

			if((!pBeamInfo->BeamformeeEntry[i].bUsed) && pBeamInfo->BeamformeeEntry[i].bSound)
			{
				*Idx = i;
				bSounding = BEAMFORMING_NOTIFY_DELETE;
			}
		}
	}

	return bSounding;
}


SOUNDING_MODE
beamforming_SoundingMode(
	PRT_BEAMFORMING_INFO 	pBeamInfo,
	u1Byte					Idx
	)
{
	RT_BEAMFORMING_PERIOD_INFO		BeamPeriodInfo = pBeamInfo->BeamformingPeriodInfo;	
	SOUNDING_MODE					Mode = BeamPeriodInfo.Mode;
	RT_BEAMFORMING_ENTRY			Entry = pBeamInfo->BeamformeeEntry[Idx];

	if(	BeamPeriodInfo.Mode == SOUNDING_SW_VHT_TIMER || BeamPeriodInfo.Mode == SOUNDING_SW_HT_TIMER ||
		BeamPeriodInfo.Mode == SOUNDING_HW_VHT_TIMER || BeamPeriodInfo.Mode == SOUNDING_HW_HT_TIMER )
		Mode = BeamPeriodInfo.Mode;
	else	if(Entry.BeamformEntryCap & BEAMFORMER_CAP_VHT_SU)
		Mode = SOUNDING_AUTO_VHT_TIMER;
	else	if(Entry.BeamformEntryCap & BEAMFORMER_CAP_HT_EXPLICIT)
		Mode = SOUNDING_AUTO_HT_TIMER;

	return Mode;
}


u2Byte
beamforming_SoundingTime(
	PRT_BEAMFORMING_INFO 	pBeamInfo,
	SOUNDING_MODE			Mode
	)
{
	u2Byte							SoundingTime = 0xffff;
	RT_BEAMFORMING_PERIOD_INFO		BeamPeriodInfo = pBeamInfo->BeamformingPeriodInfo;

	if(Mode == SOUNDING_HW_HT_TIMER || Mode == SOUNDING_HW_VHT_TIMER)
		SoundingTime = BeamPeriodInfo.BeamPeriod * 32;
	else	if(Mode == SOUNDING_SW_HT_TIMER || Mode == SOUNDING_SW_VHT_TIMER)
		SoundingTime = BeamPeriodInfo.BeamPeriod ;
	else
		SoundingTime = 20*32;

	return SoundingTime;
}


u1Byte
beamforming_SoundingBW(
	PRT_BEAMFORMING_INFO 	pBeamInfo,
	SOUNDING_MODE			Mode,
	u1Byte					Idx
	)
{
	u1Byte							SoundingBW = HT_CHANNEL_WIDTH_20;
	RT_BEAMFORMING_ENTRY			Entry = pBeamInfo->BeamformeeEntry[Idx];
	RT_BEAMFORMING_PERIOD_INFO		BeamPeriodInfo = pBeamInfo->BeamformingPeriodInfo;

	if(Mode == SOUNDING_HW_HT_TIMER || Mode == SOUNDING_HW_VHT_TIMER)
		SoundingBW = BeamPeriodInfo.BW;
	else	if(Mode == SOUNDING_SW_HT_TIMER || Mode == SOUNDING_SW_VHT_TIMER)
		SoundingBW = BeamPeriodInfo.BW;
	else 
		SoundingBW = Entry.BW;

	return SoundingBW;
}


VOID
beamforming_StartPeriod(
	struct rtl8192cd_priv *priv,
	u1Byte				Idx
	)
{

	PRT_BEAMFORMING_INFO 		pBeamInfo = &(priv->pshare->BeamformingInfo);
	PRT_BEAMFORMING_TIMER_INFO	pBeamTimerInfo = &(pBeamInfo->BeamformingTimerInfo[Idx]);	


//	pBeamTimerInfo->Idx = Idx;
	pBeamTimerInfo->Mode = beamforming_SoundingMode(pBeamInfo, Idx);
	pBeamTimerInfo->BW = beamforming_SoundingBW(pBeamInfo, pBeamTimerInfo->Mode, Idx);
	pBeamTimerInfo->BeamPeriod = beamforming_SoundingTime(pBeamInfo, pBeamTimerInfo->Mode);

	if(pBeamTimerInfo->Mode == SOUNDING_SW_VHT_TIMER || pBeamTimerInfo->Mode == SOUNDING_SW_HT_TIMER) 
	{
		ODM_SetTimer(ODMPTR, &pBeamInfo->BeamformingTimer, pBeamTimerInfo->BeamPeriod);
	} 
	else
	{
		Beamforming_SetHWTimer(priv, pBeamTimerInfo->BeamPeriod);
	}	

//	panic_printk ("%s Idx %d Mode %d BW %d Period %d\n", __FUNCTION__, 
//			Idx, pBeamTimerInfo->Mode, pBeamTimerInfo->BW, pBeamTimerInfo->BeamPeriod);
}


VOID
beamforming_EndPeriod_SW(
		struct rtl8192cd_priv *priv,
		u1Byte		Idx
	)
{
//	u1Byte						Idx = 0;
	PRT_BEAMFORMING_ENTRY		pBeamformEntry;
	PRT_BEAMFORMING_INFO 		pBeamInfo = &(priv->pshare->BeamformingInfo);
	PRT_BEAMFORMING_TIMER_INFO	pBeamTimerInfo = &(pBeamInfo->BeamformingTimerInfo[Idx]);

	if(pBeamTimerInfo->Mode == SOUNDING_SW_VHT_TIMER || pBeamTimerInfo->Mode == SOUNDING_SW_HT_TIMER) 
	{
		ODM_CancelTimer(ODMPTR, &pBeamInfo->BeamformingTimer);

		if(GET_CHIP_VER(priv) == VERSION_8822B){
		ODM_CancelTimer(ODMPTR, &pBeamInfo->BFSoundingTimeoutTimer);
			pBeamInfo->SoundingInfoV2.State = SOUNDING_STATE_NONE;
		}
	}
	else
	{
		Beamforming_StopHWTimer(priv);
	}

}

VOID
beamforming_EndPeriod_FW(
		struct rtl8192cd_priv 	*priv,
		u1Byte				Idx
	)
{
	return;
}

VOID
beamforming_ClearEntry_SW(
		struct rtl8192cd_priv 	*priv,
		BOOLEAN				IsDelete,
		u1Byte				DeleteIdx
	)
{
	u1Byte						Idx = 0;
	PRT_BEAMFORMING_ENTRY		pBeamformEntry;
	PRT_BEAMFORMING_INFO 		pBeamInfo = &(priv->pshare->BeamformingInfo);

	if(IsDelete)
	{
		if(DeleteIdx<BEAMFORMEE_ENTRY_NUM)
		{
			pBeamformEntry = pBeamInfo->BeamformeeEntry + DeleteIdx;

			if(!((!pBeamformEntry->bUsed) && pBeamformEntry->bSound))
			{
				ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, SW DeleteIdx is wrong!\n", __FUNCTION__)); 
				return;
			}
		}

		if(pBeamformEntry->BeamformEntryState == BEAMFORMING_ENTRY_STATE_PROGRESSING)
		{
			pBeamformEntry->bBeamformingInProgress = FALSE;
			pBeamformEntry->BeamformEntryState = BEAMFORMING_ENTRY_STATE_UNINITIALIZE;
		}
		else if(pBeamformEntry->BeamformEntryState == BEAMFORMING_ENTRY_STATE_PROGRESSED)
		{
			pBeamformEntry->BeamformEntryState  = BEAMFORMING_ENTRY_STATE_UNINITIALIZE;
			Beamforming_SetBeamFormStatus(priv, DeleteIdx);
		}	
		pBeamformEntry->bSound=FALSE;
		
	}
	else
	{
		for(Idx = 0; Idx < BEAMFORMEE_ENTRY_NUM; Idx++)
		{
			pBeamformEntry = pBeamInfo->BeamformeeEntry+Idx;

			if(pBeamformEntry->bSound)
			{
				ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, SW Reset entry %d\n", __FUNCTION__, Idx)); 

				/*	
				*	If End procedure is 
				*	1. Between (Send NDPA, C2H packet return), reset state to initialized.
				*	After C2H packet return , status bit will be set to zero. 
				*r
				*	2. After C2H packet, then reset state to initialized and clear status bit.
				*/ 
				if(pBeamformEntry->BeamformEntryState == BEAMFORMING_ENTRY_STATE_PROGRESSING)
					{
					Beamforming_End(priv, 0);
					}	
				else if(pBeamformEntry->BeamformEntryState == BEAMFORMING_ENTRY_STATE_PROGRESSED)
				{
					pBeamformEntry->BeamformEntryState  = BEAMFORMING_ENTRY_STATE_INITIALIZED;
					Beamforming_SetBeamFormStatus(priv, Idx);
				}	
				
				pBeamformEntry->bSound=FALSE;
			}
		}			
	}

}

VOID
beamforming_ClearEntry_FW(
		struct rtl8192cd_priv 	*priv,
		BOOLEAN				IsDelete,
		u1Byte				DeleteIdx
	)
{
	return;
}

struct rtl8192cd_priv* 
getBeamEntryDev(struct rtl8192cd_priv *priv, PRT_BEAMFORMING_ENTRY pEntry)
{
	struct stat_info *pstat;
	struct rtl8192cd_priv *vxd_priv;	
	int j;

	pstat = get_stainfo(priv, pEntry->MacAddr);
	if(pstat)
		return priv;
	
#ifdef MBSSID
	  if ((OPMODE & WIFI_AP_STATE) && priv->pmib->miscEntry.vap_enable) 
	  {
		for (j=0; j<RTL8192CD_NUM_VWLAN; j++) 
		{
			if ((priv->pvap_priv[j]->assoc_num > 0) && IS_DRV_OPEN(priv->pvap_priv[j]))
			{
				pstat = get_stainfo(priv->pvap_priv[j], pEntry->MacAddr);
				if(pstat)
					return priv->pvap_priv[j];

			}
		}
	}
#endif			
#ifdef UNIVERSAL_REPEATER
	vxd_priv = GET_VXD_PRIV(priv);
	priv = vxd_priv;
	if((OPMODE & WIFI_STATION_STATE) && (vxd_priv->assoc_num > 0) && IS_DRV_OPEN(vxd_priv)	) 
	{ 	
		pstat = get_stainfo(vxd_priv, pEntry->MacAddr);
		if(pstat)
			return vxd_priv;
	}
#endif
	return NULL;

}


BOOLEAN
BeamformingStart_V2(
	struct rtl8192cd_priv *priv,
	u1Byte			Idx,
	u1Byte			Mode, 
	u1Byte			BW
	)
{
	pu1Byte					RA = NULL;
	PRT_BEAMFORMING_ENTRY	pEntry;
	BOOLEAN					ret = TRUE;
	PRT_BEAMFORMING_INFO 	pBeamformingInfo = &(priv->pshare->BeamformingInfo);
	u1Byte					NDPTxRate;
	pEntry = &(pBeamformingInfo->BeamformeeEntry[Idx]);

	priv = getBeamEntryDev(priv, pEntry);
	if( !priv)
		return FALSE;
 
	if(pEntry->bUsed == FALSE)
	{
		pEntry->bBeamformingInProgress = FALSE;
		return FALSE;
	}
	else
	{
		if(pEntry->bBeamformingInProgress)
			return FALSE;
		pEntry->bBeamformingInProgress = TRUE;			
	
		RA = pEntry->MacAddr;

		{
			int i;
			struct stat_info *pstat = get_stainfo(GET_ROOT(priv), RA);	
#ifdef UNIVERSAL_REPEATER
			if ((pstat == NULL) && IS_DRV_OPEN(GET_VXD_PRIV(GET_ROOT(priv)))) {
				pstat = get_stainfo(GET_VXD_PRIV(priv), RA);
			}
#endif
#ifdef MBSSID
			if (pstat == NULL)	{
				if (GET_ROOT(priv)->pmib->miscEntry.vap_enable) {
					for (i=0; i<RTL8192CD_NUM_VWLAN; i++) {
						if (IS_DRV_OPEN(GET_ROOT(priv)->pvap_priv[i]))
							pstat = get_stainfo(GET_ROOT(priv)->pvap_priv[i], RA);
					}
				}
			}
#endif
			if(pstat && (pstat->state & WIFI_SLEEP_STATE)) {
				pEntry->Sounding_En = 0;
				return TRUE;
			}

		}

		
		if(Mode == SOUNDING_SW_HT_TIMER || Mode == SOUNDING_HW_HT_TIMER || Mode == SOUNDING_AUTO_HT_TIMER)
		{	
			if(!(pEntry->BeamformEntryCap & BEAMFORMER_CAP_HT_EXPLICIT))
			{
//				pBeamformingInfo->bBeamformingInProgress = FALSE;
				pEntry->bBeamformingInProgress = FALSE; 
				return FALSE;
			}
		}
		else if(Mode == SOUNDING_SW_VHT_TIMER || Mode == SOUNDING_HW_VHT_TIMER || Mode == SOUNDING_AUTO_VHT_TIMER)
		{
			if(!(pEntry->BeamformEntryCap & BEAMFORMER_CAP_VHT_SU))
			{
//				pBeamformingInfo->bBeamformingInProgress = FALSE;
				pEntry->bBeamformingInProgress = FALSE; 
				return FALSE;
			}
		}
		
		if(pEntry->BeamformEntryState != BEAMFORMING_ENTRY_STATE_INITIALIZED && pEntry->BeamformEntryState != BEAMFORMING_ENTRY_STATE_PROGRESSED)
		{
//			pBeamformingInfo->bBeamformingInProgress = FALSE;
			pEntry->bBeamformingInProgress = FALSE; 
			return FALSE;
		}	
		else
		{
			pEntry->BeamformEntryState = BEAMFORMING_ENTRY_STATE_PROGRESSING; 
			pEntry->bSound = TRUE;
		}
	}

	pEntry->BW = BW;
	pBeamformingInfo->BeamformeeCurIdx = Idx;
	
	Beamforming_SetBeamFormStatus(priv, (pBeamformingInfo->BeamformeeCurIdx));
	Beamforming_NDPARate(priv, Mode, BW, 0);	// soundingpreiod only for debug, use 0 for all case

//  debug
	if (!priv->pshare->rf_ft_var.soundingEnable){

		pEntry->Sounding_En = 0;
		return TRUE;
	}
#ifdef CONFIG_WLAN_HAL_8192EE
	if ((OPMODE & WIFI_AP_STATE) && (priv->pshare->soundingLock)){

		pEntry->Sounding_En = 0;
		return TRUE;
	}
#endif
//	
	if(Mode == SOUNDING_SW_HT_TIMER || Mode == SOUNDING_HW_HT_TIMER || Mode == SOUNDING_AUTO_HT_TIMER)
	{
		NDPTxRate = Beamforming_GetHTNDPTxRate(priv, pEntry->CompSteeringNumofBFer);
		ret = SendHTNDPAPacket(priv,RA, BW, NDPTxRate);
		ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, HT NDP Rate = %d\n", __FUNCTION__, NDPTxRate));
		pEntry->Sounding_En = 1;
	}
	else
	{
		NDPTxRate = Beamforming_GetVHTNDPTxRate(priv, pEntry->CompSteeringNumofBFer);

#ifdef CONFIG_WLAN_HAL_8814AE		
		if(((pEntry->pSTA->current_tx_rate >=_NSS3_MCS7_RATE_) && (pEntry->pSTA->current_tx_rate <=_NSS3_MCS9_RATE_)) &&
			priv->pshare->rf_ft_var.Nsnding3SS &&(GET_CHIP_VER(priv)== VERSION_8814A) )
		{
			ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, VHT3SS 7,8,9, not sounding!!\n", __FUNCTION__));
			pEntry->Sounding_En = 0;
		}
		else
#endif

#ifdef CONFIG_RTL_8812_SUPPORT
		if((((pEntry->pSTA->current_tx_rate >=_NSS2_MCS0_RATE_) && (pEntry->pSTA->current_tx_rate <=_NSS2_MCS9_RATE_)) || ((pEntry->pSTA->current_tx_rate >= _NSS1_MCS0_RATE_) && (pEntry->pSTA->current_tx_rate <= _NSS1_MCS1_RATE_))) &&
			priv->pshare->rf_ft_var.Nsnding3SS && 
			(GET_CHIP_VER(priv)== VERSION_8812E) )
		{
			ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, VHT2SS 0-9 Not sounding!!\n", __FUNCTION__));
			pEntry->Sounding_En = 0;
		}
		else
#endif

/*
#ifdef CONFIG_WLAN_HAL_8192EE
		if(((pEntry->pSTA->current_tx_rate >=_MCS8_RATE_) && (pEntry->pSTA->current_tx_rate <=_MCS15_RATE_)) &&
			priv->pshare->rf_ft_var.Nsnding3SS && (GET_CHIP_VER(priv)== VERSION_8192E) )
		{
			ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, HT MCS 8-15 Not sounding!!\n", __FUNCTION__));
		}
		else

#endif
*/
		{
			pEntry->Sounding_En = 1;
			
			if(priv->pshare->rf_ft_var.ndpaaid != 0xff)
				ret = SendVHTNDPAPacket(priv,RA, priv->pshare->rf_ft_var.ndpaaid, BW, NDPTxRate);
			else
				ret = SendVHTNDPAPacket(priv,RA, pEntry->AID, BW, NDPTxRate);
		}
		ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, VHT NDP Rate = %d\n", __FUNCTION__, NDPTxRate));  
	}

	if(ret == FALSE)
	{
		ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, Beamforming_RemoveEntry because of failure sending NDPA for addr =\n", __FUNCTION__));  
//		Beamforming_RemoveEntry(priv, RA, &Idx);
		Beamforming_Leave(priv, RA);
//		pBeamformingInfo->bBeamformingInProgress = FALSE;
		pEntry->bBeamformingInProgress = FALSE;
		return FALSE;
	}

	return TRUE;
}

VOID
Beamforming_Notify_8822B(
	struct rtl8192cd_priv *priv
	)
{

		u1Byte						Idx=BEAMFORMEE_ENTRY_NUM;
		BEAMFORMING_NOTIFY_STATE	bSounding = BEAMFORMING_NOTIFY_NONE;
		PRT_BEAMFORMING_INFO		pBeamInfo = &(priv->pshare->BeamformingInfo);
		//PRT_SOUNDING_INFO 		pSoundInfo = &(pBeamInfo->SoundingInfo);
		PRT_BEAMFORMING_TIMER_INFO	pBeamTimerInfo = NULL;	
	
		ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s Start!\n", __func__));
	
#if 1 //eric-mu
		Idx = pBeamInfo->BeamformeeCurIdx;
		bSounding = beamfomring_bSounding_8822B(priv, pBeamInfo, &Idx);
#endif
	
		if(Idx<BEAMFORMEE_ENTRY_NUM)
			pBeamTimerInfo = &(pBeamInfo->BeamformingTimerInfo[Idx]);	
		else if(bSounding == BEAMFORMING_NOTIFY_RESET)
			pBeamTimerInfo = &(pBeamInfo->BeamformingTimerInfo[0]); 
		else
			panic_printk("[%s][%d]\n", __FUNCTION__, __LINE__); //eric-mu
	
#if 1 //eric-mu
		if(!pBeamTimerInfo){
			panic_printk("ERROR [%s][%d] pBeamTimerInfo = NULL !!\n", __FUNCTION__, __LINE__); //eric-mu
			panic_printk("Idx=%d BEAMFORMEE_ENTRY_NUM=%d bSounding = %d\n", Idx, BEAMFORMEE_ENTRY_NUM, bSounding); //eric-mu
			return;
		}
#endif
	
		ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, Before notify, bSounding=%d, Idx=%d\n", __func__, bSounding, Idx));
		ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s: pBeamInfo->beamformee_su_cnt = %d\n", __func__, pBeamInfo->beamformee_su_cnt));
		
	
		switch (bSounding) {
		case BEAMFORMEE_NOTIFY_ADD_SU:
			ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s: BEAMFORMEE_NOTIFY_ADD_SU\n", __func__));
			beamforming_StartPeriod(priv, Idx);
		break;
	
		case BEAMFORMEE_NOTIFY_DELETE_SU:
			ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s: BEAMFORMEE_NOTIFY_DELETE_SU\n", __func__));
			if (pBeamTimerInfo->Mode == SOUNDING_FW_HT_TIMER || pBeamTimerInfo->Mode == SOUNDING_FW_VHT_TIMER) {
				beamforming_ClearEntry_FW(priv, TRUE, Idx);
				if (pBeamInfo->beamformee_su_cnt == 0) { /* For 2->1 entry, we should not cancel SW timer */
					beamforming_EndPeriod_FW(priv, Idx);
					ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s: No BFee left\n", __func__));
				}
			} else {
				beamforming_ClearEntry_SW(priv, TRUE, Idx);
				if (pBeamInfo->beamformee_su_cnt == 0) { /* For 2->1 entry, we should not cancel SW timer */
					beamforming_EndPeriod_SW(priv, Idx);
					ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s: No BFee left\n", __func__));
				}
			}
		break;
#if 0
		case BEAMFORMEE_NOTIFY_ADD_MU:
			ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s: BEAMFORMEE_NOTIFY_ADD_MU\n", __func__));
			if (pBeamInfo->beamformee_mu_cnt == 2) {
				/*if (pSoundInfo->SoundMode == SOUNDING_SW_VHT_TIMER || pSoundInfo->SoundMode == SOUNDING_SW_HT_TIMER)
					ODM_SetTimer(pDM_Odm, &pBeamInfo->BeamformingTimer, pSoundInfo->SoundPeriod);*/
				ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s: Start Sounding\n", __func__));
				ODM_SetTimer(ODMPTR, &pBeamInfo->BeamformingTimer, 1000); /*Do MU sounding every 1sec*/
			} else
				ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s: Less or larger than 2 MU STAs, not to set timer\n", __func__));
		break;
	
		case BEAMFORMEE_NOTIFY_DELETE_MU:
			//ODM_RT_TRACE(pDM_Odm, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s: BEAMFORMEE_NOTIFY_DELETE_MU\n", __func__));
			if (pBeamInfo->beamformee_mu_cnt == 1) {
				/*if (pSoundInfo->SoundMode == SOUNDING_SW_VHT_TIMER || pSoundInfo->SoundMode == SOUNDING_SW_HT_TIMER)*/{
					ODM_CancelTimer(ODMPTR, &pBeamInfo->BeamformingTimer);
					ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s: Less than 2 MU STAs, stop sounding\n", __func__));
				}
			}
		break;
#endif
		case BEAMFORMING_NOTIFY_RESET:
			if (pBeamTimerInfo->Mode == SOUNDING_FW_HT_TIMER || pBeamTimerInfo->Mode == SOUNDING_FW_VHT_TIMER) {	
				beamforming_ClearEntry_FW(priv, FALSE, Idx);
				beamforming_EndPeriod_FW(priv, Idx);
			} else {
				beamforming_ClearEntry_SW(priv, FALSE, Idx);
				beamforming_EndPeriod_SW(priv, Idx);
			}
	
		break;
	
		default:
		break;
		}
		
}


VOID
Beamforming_Notify(
	struct rtl8192cd_priv *priv
	)
{

	u1Byte						Idx=BEAMFORMEE_ENTRY_NUM;
	BEAMFORMING_NOTIFY_STATE	bSounding = BEAMFORMING_NOTIFY_NONE;
	PRT_BEAMFORMING_INFO 		pBeamInfo = &(priv->pshare->BeamformingInfo);
	PRT_BEAMFORMING_TIMER_INFO	pBeamTimerInfo = NULL;	

	if (!(priv->drv_state & DRV_STATE_OPEN))
		return;

	if(GET_CHIP_VER(priv) == VERSION_8822B){
		Beamforming_Notify_8822B(priv);
		return;
	}
		
	bSounding = beamfomring_bSounding(priv, pBeamInfo, &Idx);

	if(Idx<BEAMFORMEE_ENTRY_NUM)
		pBeamTimerInfo = &(pBeamInfo->BeamformingTimerInfo[Idx]);	
	else if(bSounding == BEAMFORMING_NOTIFY_RESET)
		pBeamTimerInfo = &(pBeamInfo->BeamformingTimerInfo[0]);	

	if(pBeamInfo->BeamformState == BEAMFORMING_STATE_END)
	{
		if(bSounding==BEAMFORMING_NOTIFY_ADD)
		{			
			beamforming_StartPeriod(priv, Idx);
			pBeamInfo->BeamformState = BEAMFORMING_STATE_START_1BFee;
		}
	}
	else if(pBeamInfo->BeamformState == BEAMFORMING_STATE_START_1BFee)
	{
		if(bSounding==BEAMFORMING_NOTIFY_ADD)
		{
			beamforming_StartPeriod(priv, Idx);
			pBeamInfo->BeamformState = BEAMFORMING_STATE_START_2BFee;
		}
		else if(bSounding == BEAMFORMING_NOTIFY_DELETE)
		{
			if(pBeamTimerInfo->Mode == SOUNDING_FW_HT_TIMER || pBeamTimerInfo->Mode == SOUNDING_FW_VHT_TIMER)
			{
				beamforming_EndPeriod_FW(priv, Idx);
				beamforming_ClearEntry_FW(priv, TRUE, Idx);
			}
			else
			{
				beamforming_EndPeriod_SW(priv, Idx);
				beamforming_ClearEntry_SW(priv, TRUE, Idx);
			}
			
			pBeamInfo->BeamformState = BEAMFORMING_STATE_END;
		}
		else if(bSounding == BEAMFORMING_NOTIFY_RESET)
		{
			if(pBeamTimerInfo->Mode == SOUNDING_FW_HT_TIMER || pBeamTimerInfo->Mode == SOUNDING_FW_VHT_TIMER)
			{
				beamforming_EndPeriod_FW(priv, Idx);
				beamforming_ClearEntry_FW(priv, FALSE, Idx);
			}
			else
			{
				beamforming_EndPeriod_SW(priv, Idx);
				beamforming_ClearEntry_SW(priv, FALSE, Idx);
			}
			
			pBeamInfo->BeamformState = BEAMFORMING_STATE_END;
		}
	}
	else if(pBeamInfo->BeamformState == BEAMFORMING_STATE_START_2BFee)
	{
		if(bSounding == BEAMFORMING_NOTIFY_ADD)
		{
			ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, should be block\n", __FUNCTION__));  

		}
		else if(bSounding == BEAMFORMING_NOTIFY_DELETE)
		{
			if(pBeamTimerInfo->Mode == SOUNDING_FW_HT_TIMER || pBeamTimerInfo->Mode == SOUNDING_FW_VHT_TIMER)
			{
				beamforming_EndPeriod_FW(priv, Idx);
				beamforming_ClearEntry_FW(priv, TRUE, Idx);
			}
			else
			{
				// For 2->1 entry, we should not cancel SW timer
				beamforming_ClearEntry_SW(priv, TRUE, Idx);
			}
		
			pBeamInfo->BeamformState = BEAMFORMING_STATE_START_1BFee;
		}
		else if(bSounding == BEAMFORMING_NOTIFY_RESET)
		{
			if(pBeamTimerInfo->Mode == SOUNDING_FW_HT_TIMER || pBeamTimerInfo->Mode == SOUNDING_FW_VHT_TIMER)
			{
				beamforming_EndPeriod_FW(priv, Idx);
				beamforming_ClearEntry_FW(priv, FALSE, Idx);
			}
			else
			{
				beamforming_EndPeriod_SW(priv, Idx);
				beamforming_ClearEntry_SW(priv, FALSE, Idx);
			}
			
			pBeamInfo->BeamformState = BEAMFORMING_STATE_END;
		}
	}

}


VOID
Beamforming_AutoTest(
	struct rtl8192cd_priv *priv,
	u1Byte					Idx, 
	PRT_BEAMFORMING_ENTRY	pBeamformEntry
	)
{
	SOUNDING_MODE					Mode;

	BEAMFORMING_CAP				BeamformCap = pBeamformEntry->BeamformEntryCap;
	PRT_BEAMFORMING_INFO 		pBeamInfo = &(priv->pshare->BeamformingInfo);	
	PRT_BEAMFORMING_PERIOD_INFO pBeamPeriodInfo = &(pBeamInfo->BeamformingPeriodInfo);

	if(BeamformCap & BEAMFORMER_CAP_VHT_SU)
		Mode = SOUNDING_SW_VHT_TIMER;
	else if(BeamformCap & BEAMFORMER_CAP_HT_EXPLICIT)
	{
		Mode = SOUNDING_SW_HT_TIMER;		// use sw timer for all IC
	
/*#ifdef CONFIG_WLAN_HAL_8192EE
		if(GET_CHIP_VER(priv)== VERSION_8192E)
			Mode = SOUNDING_SW_HT_TIMER;
#endif
#ifdef CONFIG_RTL_8812_SUPPORT
		if(GET_CHIP_VER(priv)== VERSION_8812E)
			Mode = SOUNDING_HW_HT_TIMER;
#endif
#ifdef CONFIG_WLAN_HAL_8814AE
		if(GET_CHIP_VER(priv)== VERSION_8814A)
			Mode = SOUNDING_HW_HT_TIMER;
#endif*/
	}
	else 
		return;

	pBeamPeriodInfo->Idx = Idx;
	pBeamPeriodInfo->Mode = Mode;
	pBeamPeriodInfo->BW = pBeamformEntry->BW;

	pBeamPeriodInfo->BeamPeriod = priv->pshare->rf_ft_var.soundingPeriod;


}


VOID
Beamforming_End(
	struct rtl8192cd_priv *priv,
	BOOLEAN			Status	
	)
{

	PRT_BEAMFORMING_INFO pBeamformingInfo = &(priv->pshare->BeamformingInfo);
    PRT_BEAMFORMING_ENTRY	pBFeeEntry = &(pBeamformingInfo->BeamformeeEntry[pBeamformingInfo->BeamformeeCurIdx]);

	ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s] Status = %d, BeamformEntryState=%d, pBFeeEntry->bUsed=%d\n",
        __FUNCTION__, Status,
        pBFeeEntry->BeamformEntryState,
        pBFeeEntry->bUsed
        ));

    if((pBFeeEntry->BeamformEntryState != BEAMFORMING_ENTRY_STATE_PROGRESSING)||(pBFeeEntry->bUsed==0)|| (pBFeeEntry->Sounding_En==0))
	{
		return;
	}

    // Because in this case 8814A STOP sounding @BeamformingStart_V2 , so NOT apply V-matrix here.
    if(((pBFeeEntry->pSTA->current_tx_rate >=_NSS3_MCS7_RATE_) && (pBFeeEntry->pSTA->current_tx_rate <=_NSS3_MCS9_RATE_)) &&
		priv->pshare->rf_ft_var.Nsnding3SS)
	{
		ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, VHT3SS 7,8,9, do not apply V matrix.\n", __FUNCTION__));
        pBFeeEntry->BeamformEntryState = BEAMFORMING_ENTRY_STATE_INITIALIZED;
		Beamforming_SetBeamFormStatus(priv, (pBeamformingInfo->BeamformeeCurIdx));
	}
    else if(Status == 1)        // Receive CSI successful
	{
        pBFeeEntry->LogStatusFailCnt = 0;
        pBFeeEntry->BeamformEntryState = BEAMFORMING_ENTRY_STATE_PROGRESSED;
		Beamforming_SetBeamFormStatus(priv, (pBeamformingInfo->BeamformeeCurIdx));
		ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s], Sounding Success\n", __FUNCTION__));
	}
    else        // Receive CSI failure
	{
        pBFeeEntry->BeamformEntryState = BEAMFORMING_ENTRY_STATE_INITIALIZED;
        pBFeeEntry->LogStatusFailCnt++;
		ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s], Sounding Fail\n", __FUNCTION__));
	}

    pBFeeEntry->bBeamformingInProgress = FALSE;
	ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, pEntry->LogStatusFailCnt : %d\n", __FUNCTION__, pBFeeEntry->LogStatusFailCnt));  

    // Receive CSI failure
    if(pBFeeEntry->LogStatusFailCnt > 50)
	{
		ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]@%s, LogStatusFailCnt > 50\n", __FUNCTION__));
        
        pBeamformingInfo->CurDelBFerBFeeEntrySel = BFeeEntry;
        if(Beamforming_DeInitEntry(priv, pBFeeEntry->MacAddr))
			Beamforming_Notify(priv);
	}

}	

int shortenSoundingPeriod(struct rtl8192cd_priv *priv)
{
	PRT_BEAMFORMING_INFO 		pBeamformingInfo = &(priv->pshare->BeamformingInfo);
	PRT_BEAMFORMING_ENTRY		pEntry = &(pBeamformingInfo->BeamformeeEntry[0]);
	struct stat_info 			*pstat;
	struct rtl8192cd_priv 		*vxd_priv;	
	u4Byte idx, j, ret=0;
	pBeamformingInfo->BeamformingPeriodState = 0;     //

	for(idx=0 ; idx<BEAMFORMEE_ENTRY_NUM; idx++)
	{
		pEntry = &(pBeamformingInfo->BeamformeeEntry[idx]);
		if( pEntry->bUsed)  
		{
			pstat = get_stainfo(priv, pEntry->MacAddr);
			if(pstat) 
			{
				if(pstat->tx_avarage > 625000)//5Mbps
				{
					++ret;
					if(idx == 0)                                                                        //
						pBeamformingInfo->BeamformingPeriodState+=1;        // entry 0 only = 1
					else                                                                                  // entry 1 only = 2
						pBeamformingInfo->BeamformingPeriodState+=2;        // entry 0 and 1 = 3
				}
			}

#ifdef MBSSID
		  if ((OPMODE & WIFI_AP_STATE) && priv->pmib->miscEntry.vap_enable)
		  {
			for (j=0; j<RTL8192CD_NUM_VWLAN; j++)
			{
				if ((priv->pvap_priv[j]->assoc_num > 0) && IS_DRV_OPEN(priv->pvap_priv[j]))
				{
					pstat = get_stainfo(priv->pvap_priv[j], pEntry->MacAddr);
					if(pstat)
					{
							if(pstat->tx_avarage > 625000)  // 5Mbps
						{
							++ret;
							if(idx == 0)                                                                        //
								pBeamformingInfo->BeamformingPeriodState|=1;        // entry 0 only = 1
							else                                                                                  // entry 1 only = 2
								pBeamformingInfo->BeamformingPeriodState|=2;        // entry 0 and 1 = 3
						}
					}
				}
			}
		}
#endif		
#ifdef UNIVERSAL_REPEATER
			vxd_priv = GET_VXD_PRIV(priv);
			if((OPMODE_VXD & WIFI_STATION_STATE) && (vxd_priv->assoc_num > 0) && IS_DRV_OPEN(vxd_priv) 	)
			{		
				pstat = get_stainfo(vxd_priv, pEntry->MacAddr);
				if(pstat)
				{
					if(pstat->tx_avarage >  625000)  // 5Mbps
					{
						++ret;
						if(idx == 0)                                                                        //
							pBeamformingInfo->BeamformingPeriodState|=1;        // entry 0 only = 1
						else                                                                                  // entry 1 only = 2
							pBeamformingInfo->BeamformingPeriodState|=2;        // entry 0 and 1 = 3
					}
				}
			}
#endif
			
		}
	}
	//panic_printk("BeamformPeriodState = %d\n", pBeamformingInfo->BeamformingPeriodState);
	return ret;
}

u1Byte
getBFeeStaNum(
	struct rtl8192cd_priv *priv
	)
{
	PRT_BEAMFORMING_INFO 		pBeamformingInfo = &(priv->pshare->BeamformingInfo);
	int idx;
	u1Byte BFee_STA_Num = 0;
	for(idx=0 ; idx<BEAMFORMEE_ENTRY_NUM; idx++)
	{
		if(pBeamformingInfo->BeamformeeEntry[idx].bUsed)
			BFee_STA_Num++;
	}
	return BFee_STA_Num;	
}


VOID
Beamforming_TimerCallback(
	struct rtl8192cd_priv *priv
	)
{
	BOOLEAN						ret = FALSE;//, timer_set=FALSE;
	PRT_BEAMFORMING_INFO 		pBeamformingInfo = &(priv->pshare->BeamformingInfo);
	int 							idx = pBeamformingInfo->BeamformeeCurIdx;		
	PRT_BEAMFORMING_ENTRY		pEntry = &(pBeamformingInfo->BeamformeeEntry[idx]);
	PRT_BEAMFORMING_TIMER_INFO	pBeamformingTimerInfo = &(pBeamformingInfo->BeamformingTimerInfo[idx]);	
	u1Byte						BFee_STA_Num = 0;
	u1Byte						index = 0;

	BFee_STA_Num = getBFeeStaNum(priv);
	
	if (!(priv->drv_state & DRV_STATE_OPEN))
		return;

#if 0
	for(idx=0 ; idx<BEAMFORMEE_ENTRY_NUM; idx++)  
	{
#else
	{		

		if(BFee_STA_Num == 2)
		{
			if(pBeamformingInfo->BeamformingPeriodState == 0 || pBeamformingInfo->BeamformingPeriodState == 3)
			{
				if(pBeamformingInfo->BeamformeeEntry[idx^1].bUsed)
					idx ^=1;
			}
			else if(pBeamformingInfo->BeamformingPeriodState == 2)
			{
				idx = 1;
			}
			else
				idx = 0;
		}	
		else
		{  
			if(pBeamformingInfo->BeamformeeEntry[0].bUsed)
				idx = 0;
			else if(pBeamformingInfo->BeamformeeEntry[1].bUsed)
				idx = 1;
		}
		
		pBeamformingInfo->BeamformeeCurIdx = idx;
#endif
		pEntry = &(pBeamformingInfo->BeamformeeEntry[idx]);
		pBeamformingTimerInfo = &(pBeamformingInfo->BeamformingTimerInfo[idx]);
		
		if(pEntry->bBeamformingInProgress)
		{
			Beamforming_End(priv, 0);
		}
		if( pEntry->bUsed) 
		{
			ret = BeamformingStart_V2( priv, idx, pBeamformingTimerInfo->Mode, pEntry->BW);

		}

//		if(ret && !timer_set)
//		if(ret)
		if(pBeamformingInfo->BeamformeeEntry[0].bUsed || pBeamformingInfo->BeamformeeEntry[1].bUsed)
		{		
			if(pBeamformingInfo->BeamformState >= BEAMFORMING_STATE_START_1BFee)
			{
				if(pBeamformingTimerInfo->Mode == SOUNDING_SW_VHT_TIMER || pBeamformingTimerInfo->Mode == SOUNDING_SW_HT_TIMER)  
				{
					if(shortenSoundingPeriod(priv)){
						if(pBeamformingInfo->BeamformingPeriodState == 1 || pBeamformingInfo->BeamformingPeriodState == 2)
						{
							ODM_SetTimer(ODMPTR, &pBeamformingInfo->BeamformingTimer, pBeamformingTimerInfo->BeamPeriod/100);
						}
						else   // pBeamformingInfo->BeamformingPeriodState == 3
						{
							ODM_SetTimer(ODMPTR, &pBeamformingInfo->BeamformingTimer, pBeamformingTimerInfo->BeamPeriod/200);
						}
					}
					else  
					{
						ODM_SetTimer(ODMPTR, &pBeamformingInfo->BeamformingTimer, pBeamformingTimerInfo->BeamPeriod);
					}
				}
				else
				{
					int BeamPeriod = priv->pshare->rf_ft_var.soundingPeriod;

					if(pBeamformingTimerInfo->Mode == SOUNDING_HW_VHT_TIMER || pBeamformingTimerInfo->Mode == SOUNDING_HW_HT_TIMER)
						BeamPeriod *=32;	//HW timer, clock = 32K
	
					if(shortenSoundingPeriod(priv))
					{
						if(pBeamformingInfo->BeamformingPeriodState == 1 || pBeamformingInfo->BeamformingPeriodState == 2) //only one entry is in used
							BeamPeriod /= 100;
						else //two entries are in used
							BeamPeriod /= 200;
					}

					if(pBeamformingTimerInfo->BeamPeriod != BeamPeriod)
					{
						pBeamformingTimerInfo->BeamPeriod = BeamPeriod;
					}
					Beamforming_SetHWTimer(priv, pBeamformingTimerInfo->BeamPeriod);
				}
//				timer_set = 1;
			}
		}	

	
	}

}

VOID Beamforming_SWTimerCallback(unsigned long task_priv)
{
	struct rtl8192cd_priv *priv = (struct rtl8192cd_priv *)task_priv;
	Beamforming_TimerCallback(priv);
//	mod_timer(&priv->txbf_swtimer, jiffies + priv->pshare->rf_ft_var.soundingPeriod);
}

VOID
beamform_InitSoundingVars(
	struct rtl8192cd_priv *priv
)
{
	PRT_BEAMFORMING_INFO 		pBeamInfo = &(priv->pshare->BeamformingInfo);
	PRT_SOUNDING_INFOV2			pSoundingInfo = &(pBeamInfo->SoundingInfoV2);
	
	pSoundingInfo->State = SOUNDING_STATE_NONE;
	pSoundingInfo->SUBFeeCurIdx = 0xFF;
	pSoundingInfo->CandidateMUBFeeCnt = 0;
	pSoundingInfo->MinSoundingPeriod = 0;
	pSoundingInfo->SoundRemainCntPerPeriod = 0;	// Get from sounding list. Ex: SU STA1, SU STA2, MU STA(1~n) => the value will be 2+1=3.
	pSoundingInfo->SUSoundNumPerPeriod = 0;
	pSoundingInfo->MUSoundNumPerPeriod = 0;
}

BOOLEAN
beamforming_rf_iqgen_setting (
	struct rtl8192cd_priv *priv
)
{
	BOOLEAN is_iqgen_setting_ok = FALSE;
	
#ifdef CONFIG_WLAN_HAL_8814AE
		if(GET_CHIP_VER(priv) == VERSION_8814A)
			is_iqgen_setting_ok = beamforming_setiqgen_8814a(priv);
#endif

	return is_iqgen_setting_ok;

}


VOID
Beamforming_Init(
	struct rtl8192cd_priv *priv
	)
{
	PRT_BEAMFORMING_INFO pBeamInfo = &(priv->pshare->BeamformingInfo);
	PRT_BEAMFORMING_PERIOD_INFO	pBeamPeriodInfo = &(pBeamInfo->BeamformingPeriodInfo);
	BOOLEAN is_iqgen_setting_ok = FALSE;

	pBeamInfo->BeamformingPeriodState = 0;
	pBeamPeriodInfo->Mode = SOUNDING_STOP_OID_TIMER;

	ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, Init Timer\n", __FUNCTION__));

	if(GET_CHIP_VER(priv)== VERSION_8822B) {

#if 1 //eric-txbf init pBeamInfo->BeamformCap
	if(priv->pmib->dot11RFEntry.txbfer)
	{
		pBeamInfo->BeamformCap |= (BEAMFORMER_CAP_HT_EXPLICIT);

		if(priv->pmib->dot11BssType.net_work_type |= WIRELESS_11AC)
			pBeamInfo->BeamformCap |= (BEAMFORMER_CAP_VHT_SU|BEAMFORM_CAP_VHT_MU_BFER);

	}
	if(priv->pmib->dot11RFEntry.txbfee)
	{
		pBeamInfo->BeamformCap |= (BEAMFORMEE_CAP_HT_EXPLICIT);

		if(priv->pmib->dot11BssType.net_work_type |= WIRELESS_11AC)
			pBeamInfo->BeamformCap |= (BEAMFORMEE_CAP_VHT_SU|BEAMFORM_CAP_VHT_MU_BFEE);
	}

	//panic_printk("Beamforming_Init: pBeamInfo->BeamformCap = 0x%x \n");
#endif


		init_timer(&pBeamInfo->BeamformingTimer);
		pBeamInfo->BeamformingTimer.function = beamform_SoundingTimerCallback;
		pBeamInfo->BeamformingTimer.data = (unsigned long)priv;
	
		init_timer(&pBeamInfo->BFSoundingTimeoutTimer);
		pBeamInfo->BFSoundingTimeoutTimer.function = beamform_SoundingTimeout;
		pBeamInfo->BFSoundingTimeoutTimer.data = (unsigned long)priv;
		beamform_InitSoundingVars(priv);
	}
	else
	{	
	init_timer(&pBeamInfo->BeamformingTimer);
	pBeamInfo->BeamformingTimer.function = Beamforming_SWTimerCallback;
	pBeamInfo->BeamformingTimer.data = (unsigned long)priv;
	}

	is_iqgen_setting_ok = beamforming_rf_iqgen_setting(priv);
	ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, is_iqgen_setting_ok = %d\n", __FUNCTION__, is_iqgen_setting_ok));
	
}


VOID
Beamforming_Release(
	struct rtl8192cd_priv *priv
	)
{
	PRT_BEAMFORMING_INFO pBeamformingInfo = &(priv->pshare->BeamformingInfo);
	BeamformingReset(priv);	
	ODM_CancelTimer(ODMPTR, &pBeamformingInfo->BeamformingTimer);
	if(GET_CHIP_VER(priv)== VERSION_8822B)
		ODM_CancelTimer(ODMPTR, &pBeamformingInfo->BFSoundingTimeoutTimer);

	ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, Release Timer\n", __FUNCTION__));  
}		



VOID
Beamforming_Enter(
	struct rtl8192cd_priv *priv,
	struct stat_info	*pstat
)
{
	u1Byte	BFerBFeeIdx = 0xff;
	PRT_BEAMFORMING_INFO 	pBeamInfo = &(priv->pshare->BeamformingInfo);
	PRT_SOUNDING_INFOV2		SoundingInfoV2 = &(priv->pshare->BeamformingInfo.SoundingInfoV2);

	if(priv->pmib->dot11RFEntry.txbf == 0) //eric-mu
		return;

	if(Beamforming_InitEntry(priv, pstat, &BFerBFeeIdx))
	{
		Beamforming_SetBeamFormEnter(priv, BFerBFeeIdx);

		if(GET_CHIP_VER(priv)== VERSION_8822B) {
			ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, BeamformCap = 0x%x State = 0x%x \n", __FUNCTION__, pBeamInfo->BeamformCap, pBeamInfo->SoundingInfoV2.State));  
			//if(pBeamInfo->BeamformCap & (BEAMFORMER_CAP_HT_EXPLICIT|BEAMFORMER_CAP_VHT_SU|BEAMFORM_CAP_VHT_MU_BFER))
			{
				if(pBeamInfo->SoundingInfoV2.State == SOUNDING_STATE_NONE)
				{
					pBeamInfo->SoundingInfoV2.State = SOUNDING_STATE_INIT;
					ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, Set BeamformingTimer = 2000\n", __FUNCTION__)); 
					ODM_SetTimer(ODMPTR, &pBeamInfo->BeamformingTimer, 2000); // Start sounding after 2 sec.
				}
			}
		}
	}	
}


VOID
Beamforming_Leave(
	struct rtl8192cd_priv *priv,
	pu1Byte			RA
	)
{
	u1Byte		Idx = 0;
	PRT_BEAMFORMING_INFO 	pBeamformingInfo = &(priv->pshare->BeamformingInfo);
	Beamforming_GetBFeeEntryByAddr(priv, RA, &Idx);

	if(RA == NULL)
	{
		BeamformingReset(priv);
		ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, Reset entry\n", __FUNCTION__));  
	}
	else
	{
        pBeamformingInfo->CurDelBFerBFeeEntrySel = BFerBFeeEntry;
		Beamforming_DeInitEntry(priv, RA);
		ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s, DeInit entry %d\n", __FUNCTION__, Idx));  
	}

	Beamforming_Notify(priv);

}


VOID
Beamforming_SetTxBFen(
	struct rtl8192cd_priv *priv,
	u1Byte			MacId,
	BOOLEAN			bTxBF
	)
{
	u1Byte					Idx = 0;
//	PRT_BEAMFORMING_INFO 	pBeamformingInfo = &(priv->pshare->BeamformingInfo);
	PRT_BEAMFORMING_ENTRY	pEntry;

	pEntry = Beamforming_GetEntryByMacId(priv, MacId, &Idx);

	if(pEntry == NULL)
		return;
	else
		pEntry->bTxBF = bTxBF;

	Beamforming_Notify(priv);
}

BEAMFORMING_CAP
Beamforming_GetBeamCap(
	struct rtl8192cd_priv *priv,
	IN PRT_BEAMFORMING_INFO 	pBeamInfo
	)
{
	u1Byte					i;
	BOOLEAN 				bSelfBeamformer = FALSE;
	BOOLEAN 				bSelfBeamformee = FALSE;
	RT_BEAMFORMING_ENTRY	BeamformeeEntry;
	RT_BEAMFORMER_ENTRY	BeamformerEntry;
	BEAMFORMING_CAP 		BeamformCap = BEAMFORMING_CAP_NONE;

	/*
	for(i = 0; i < BEAMFORMEE_ENTRY_NUM; i++)
	{
		BeamformEntry = pBeamInfo->BeamformeeEntry[i];

		if(BeamformEntry.bUsed)
		{
			if( (BeamformEntry.BeamformEntryCap & BEAMFORMEE_CAP_VHT_SU) ||
				(BeamformEntry.BeamformEntryCap & BEAMFORMEE_CAP_HT_EXPLICIT))
				bSelfBeamformee = TRUE;
			if( (BeamformEntry.BeamformEntryCap & BEAMFORMER_CAP_VHT_SU) ||
				(BeamformEntry.BeamformEntryCap & BEAMFORMER_CAP_HT_EXPLICIT))
				bSelfBeamformer = TRUE;
		}

		if(bSelfBeamformer && bSelfBeamformee)
			i = BEAMFORMEE_ENTRY_NUM;
	}
	*/

	for(i = 0; i < BEAMFORMEE_ENTRY_NUM; i++)
	{
		BeamformeeEntry = pBeamInfo->BeamformeeEntry[i];

		if(BeamformeeEntry.bUsed)
		{
			bSelfBeamformer = TRUE;
			//panic_printk("[Beamform]%s, BFee entry %d bUsed=TRUE\n", __FUNCTION__, i);  
		}
	}

	for(i = 0; i < BEAMFORMER_ENTRY_NUM; i++)
	{
		BeamformerEntry = pBeamInfo->BeamformerEntry[i];

		if(BeamformerEntry.bUsed)
		{
			bSelfBeamformee = TRUE;
			//panic_printk"[Beamform]%s, BFer entry %d bUsed=TRUE\n", __FUNCTION__, i);  
		}
	}

	if(bSelfBeamformer)
		BeamformCap |= BEAMFORMER_CAP;
	if(bSelfBeamformee)
		BeamformCap |= BEAMFORMEE_CAP;

	return BeamformCap;
}

VOID
Beamforming_GetNDPAFrame(
	struct rtl8192cd_priv *priv,
	pu1Byte 					pNDPAFrame
	)
{
	pu1Byte						TA ;
	u1Byte						Idx, Sequence;
	PRT_BEAMFORMER_ENTRY		pBeamformerEntry = NULL;
    PRT_BEAMFORMING_INFO    pBeamformingInfo = &(priv->pshare->BeamformingInfo);

	if (GET_CHIP_VER(priv) != VERSION_8812E)
		return;
	if(IsCtrlNDPA(pNDPAFrame) == FALSE)
		return;

	TA = GetAddr2Ptr(pNDPAFrame);
	// Remove signaling TA. 
	TA[0] = TA[0] & 0xFE; 

        pBeamformerEntry = Beamforming_GetBFerEntryByAddr(priv, TA, &Idx);

        if(pBeamformerEntry == NULL)
		return;
        else if(!(pBeamformerEntry->BeamformEntryCap & BEAMFORMEE_CAP_VHT_SU))
		return;
        
        // NDPALogSuccess: As long as 8812A receive NDPA and feedback CSI succeed once, clock reset is NO LONGER needed !2015-04-10, Jeffery
        // ClockResetTimes: While BFer entry always doesn't receive our CSI, clock will reset again and again.So ClockResetTimes is limited to 5 times.2015-04-13, Jeffery
        else if( pBeamformerEntry->NDPALogSuccess==1 )
        {
			ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]@%s ,ClockResetTimes=%d, NDPALogSuccess=%d, clock reset is no longer needed!!\n",
			__FUNCTION__, 
			pBeamformerEntry->ClockResetTimes,
			pBeamformerEntry->NDPALogSuccess));
            return;
        }
        else if( pBeamformerEntry->ClockResetTimes==3 )
        {
			 ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]@%s ,ClockResetTimes=%d, DeInit BFer Entry !!\n",
						__FUNCTION__,
						pBeamformerEntry->ClockResetTimes,
						pBeamformerEntry->NDPALogSuccess));

            pBeamformingInfo->CurDelBFerBFeeEntrySel = BFerEntry ;
            Beamforming_DeInitEntry(priv, pBeamformerEntry->MacAddr);
			return;
        }

		Sequence = (pNDPAFrame[16]) >> 2;

        ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]@%s Start, Sequence=%d, NDPALogSeq=%d, NDPAPreLogSeq=%d, NDPALogRetryCnt=%d, ClockResetTimes=%d, NDPALogSuccess=%d\n",
					__FUNCTION__,
					Sequence,
					pBeamformerEntry->NDPALogSeq,
					pBeamformerEntry->NDPAPreLogSeq,
					pBeamformerEntry->NDPALogRetryCnt,
					pBeamformerEntry->ClockResetTimes,
					pBeamformerEntry->NDPALogSuccess));

        if ((pBeamformerEntry->NDPALogSeq != 0) && (pBeamformerEntry->NDPAPreLogSeq != 0))
	{
            //2 Success Condition
            if( (pBeamformerEntry->NDPALogSeq!=Sequence)&&(pBeamformerEntry->NDPALogSeq!=pBeamformerEntry->NDPAPreLogSeq) )
	{
                /* break option for clcok reset, 2015-03-30, Jeffery */
                pBeamformerEntry->NDPALogRetryCnt = 0;

                /*As long as 8812A receive NDPA and feedback CSI succeed once, clock reset is no longer needed.*/
                /*That is, NDPALogSuccess is NOT needed to be reset to zero, 2015-04-13, Jeffery*/
                pBeamformerEntry->NDPALogSuccess=1;
	}

            //2 Fail Condition
	else
	{
                
                if (pBeamformerEntry->NDPALogRetryCnt == 5)
                {
                
                    pBeamformerEntry->ClockResetTimes++;
                    pBeamformerEntry->NDPALogRetryCnt = 0;

                    watchdog_kick();

                    ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[Beamforming]@%s ,Clock Reset!!! ClockResetTimes=%d\n",
	                    __FUNCTION__, 
	                    pBeamformerEntry->ClockResetTimes));  
#ifdef CONFIG_RTL_8812_SUPPORT	
					_Beamforming_CLK(priv);
#endif		
				}
                else
                    pBeamformerEntry->NDPALogRetryCnt++;
            }
        }

        pBeamformerEntry->NDPAPreLogSeq = pBeamformerEntry->NDPALogSeq;
        pBeamformerEntry->NDPALogSeq = Sequence;

}


/*********************************************************/
/*                                          MU-MIMO                                                     */
/*********************************************************/
#if 1//(SUPPORT_MU_BF == 1)

void input_value_16(pu2Byte p, unsigned char start, unsigned char end, u2Byte value)
{
	u2Byte bit_mask = 0;

	if(value > 0) //only none-zero value needs to be assigned 
	{
		if(start == end) //1-bit value
		{
			*p |= BIT(start);
		}
		else
		{
			unsigned char x = 0;
				
			for(x = 0; x<=(end-start); x ++)
				bit_mask |= BIT(x);

			*p |= ((value&bit_mask) << start);	
		}
	}

}

#if 1 //eric-8822 tx hang
unsigned char is_mgmt_q_empty(struct rtl8192cd_priv *priv)
{
	int loop = 0;
	unsigned int tmp32 = 0;
	unsigned int tmp0, tmp1 = 0;

	while(1) {
		
		delay_us(100);
		
		loop ++;		
		if(priv->pshare->rf_ft_var.mloop && (loop > priv->pshare->rf_ft_var.mloop))
			return 0;
		
#if 1
		tmp32 = RTL_R32(0x3b0);

		tmp0 = tmp32 & 0xfff; //[0:11]
		tmp1 = ((tmp32 & 0xfff0000) >> 16 ); //[16:27]
		
		if(tmp0 == tmp1){
			tmp32 = RTL_R32(0x410);

			if(tmp32 & BIT(22)) {
				panic_printk("[%s][%d] Cost %d(100ns) \n", __FUNCTION__, __LINE__, loop);
				return 1;
			}
		}
#else
		tmp32 = RTL_R32(0x410);

		if(tmp32 & BIT(22))
			return 1;
#endif

	}
}
#endif

VOID
ConstructVHTNDPAPacket_MU(
	struct rtl8192cd_priv *priv,
	pu1Byte			RA,
	u2Byte			AID,
	pu1Byte			Buffer,
	pu4Byte			pLength,
	u1Byte 			BW
	)
{
	u2Byte					Duration= 0;
	u1Byte					Sequence = 0;
	pu1Byte					pNDPAFrame = Buffer;
	u2Byte					tmp16 = 0;
	char					idx =0 ;
	
	RT_NDPA_STA_INFO		STAInfo;
	int aSifsTime = ((priv->pmib->dot11BssType.net_work_type & WIRELESS_11N) && (priv->pshare->ht_sta_num)) ? 0x10 : 10;

	PRT_BEAMFORMING_INFO		pBeamInfo = &(priv->pshare->BeamformingInfo);
	PRT_BEAMFORMING_ENTRY	pEntry;

	// Frame control.
	SET_80211_HDR_FRAME_CONTROL(pNDPAFrame, 0);
	SET_80211_HDR_TYPE_AND_SUBTYPE(pNDPAFrame, Type_NDPA);

#if 0 //eric-8822
	if(!memcmp(pBeamInfo->BeamformeeEntry[0].MacAddr, priv->pshare->rf_ft_var.ndpa_mac, 6)){
			panic_printk("[%s][%d] Assign MAC is already STA1 \n", __FUNCTION__, __LINE__); 	
	}
	else if(!memcmp(pBeamInfo->BeamformeeEntry[1].MacAddr, priv->pshare->rf_ft_var.ndpa_mac, 6)){	
			panic_printk("[%s][%d] Assign MAC is STA2, swap with STA1 \n", __FUNCTION__, __LINE__);
			memcpy(RA, pBeamInfo->BeamformeeEntry[1].MacAddr, 6);
	}
	//else
			//panic_printk("[%s][%d] Assign MAC is NOT connected \n", __FUNCTION__, __LINE__);
#endif

	//panic_printk("[%s][%d] %02x %02x %02x %02x %02x %02x\n", __FUNCTION__, __LINE__, 
		//RA[0], RA[1], RA[2], RA[3], RA[4], RA[5]);

	memcpy((void *)GetAddr1Ptr(pNDPAFrame), RA, MACADDRLEN);
	memcpy((void *)GetAddr2Ptr(pNDPAFrame), GET_MY_HWADDR, MACADDRLEN);

	Duration = 2*aSifsTime + 44;
	
	if(BW == HT_CHANNEL_WIDTH_80)
		Duration += 40;
	else if(BW == HT_CHANNEL_WIDTH_20_40)
		Duration+= 87;
	else	
		Duration+= 180;

	SetDuration(pNDPAFrame, Duration);
	Sequence = GET_HW(priv)->sounding_seq<<2;
	GET_HW(priv)->sounding_seq =  (GET_HW(priv)->sounding_seq+1) & 0xfff;
	 
	memcpy(pNDPAFrame+16, &Sequence,1);

	//eric-8822 ?? there will be multiple STA infos ??

	if (OPMODE & WIFI_ADHOC_STATE)
		AID = 0;


	*pLength = 17;


	/* Construct STA info. for multiple STAs */

#if 0 //eric-8822
	if(!memcmp(pBeamInfo->BeamformeeEntry[0].MacAddr, priv->pshare->rf_ft_var.ndpa_mac, 6)){
		panic_printk("[%s][%d] Assign MAC is already STA1 \n", __FUNCTION__, __LINE__);		
	}
	else if(!memcmp(pBeamInfo->BeamformeeEntry[1].MacAddr, priv->pshare->rf_ft_var.ndpa_mac, 6)){	
		panic_printk("[%s][%d] Assign MAC is STA2, swap with STA1 +++\n", __FUNCTION__, __LINE__);
		for (idx = (BEAMFORMEE_ENTRY_NUM-1); idx >= 0; idx--) {	

			panic_printk("[%s][%d] idx = %d\n", __FUNCTION__, __LINE__, idx);
			
			if((idx != 0) && (idx != 1))
				break;
			
			pEntry = &(pBeamInfo->BeamformeeEntry[idx]);

			if(!pEntry)
				break;
			
			if (pEntry->is_mu_sta) {
				
				STAInfo.AID = pEntry->AID;
				STAInfo.FeedbackType = 1; /* 1'b1: MU */
				STAInfo.NcIndex = 0;
				tmp16 = 0;
				memset(&tmp16, 0, sizeof(tmp16));
			
				input_value_16(&tmp16, 0, 11, pEntry->AID);
				input_value_16(&tmp16, 12, 12, 1);
				input_value_16(&tmp16, 13, 15, 0);

				tmp16 = cpu_to_le16(tmp16);
				memcpy(pNDPAFrame+(*pLength), &tmp16, 2);

				*pLength += 2;
			}
		}
		panic_printk("[%s][%d] Assign MAC is STA2, swap with STA1 ---\n", __FUNCTION__, __LINE__);
		return;
	}
	//else
		//panic_printk("[%s][%d] Assign MAC is NOT connected \n", __FUNCTION__, __LINE__);
#endif

	for (idx = 0; idx < BEAMFORMEE_ENTRY_NUM; idx++) {		
		pEntry = &(pBeamInfo->BeamformeeEntry[idx]);
		
		if (pEntry->is_mu_sta) {
			//panic_printk("[%d] is_mu_sta=%d AID=%d P_AID=0x%x\n", idx, pEntry->is_mu_sta, pEntry->AID, pEntry->P_AID);
			STAInfo.AID = pEntry->AID;
			STAInfo.FeedbackType = 1; /* 1'b1: MU */
			STAInfo.NcIndex = 0;
			tmp16 = 0;
			memset(&tmp16, 0, sizeof(tmp16));
			
			input_value_16(&tmp16, 0, 11, pEntry->AID);
			input_value_16(&tmp16, 12, 12, 1);
			input_value_16(&tmp16, 13, 15, 0);

			tmp16 = cpu_to_le16(tmp16);
			memcpy(pNDPAFrame+(*pLength), &tmp16, 2);

			*pLength += 2;
		}
	}
	
}


BOOLEAN
SendVHTNDPAPacket_MU(
	struct rtl8192cd_priv *priv,
	IN	pu1Byte			RA,
	IN	u2Byte			AID,
	u1Byte 				BW,
	u1Byte		NDPTxRate
	)
{
	BOOLEAN					ret = TRUE;
	u4Byte 					PacketLength;
	unsigned char *pbuf 	= get_wlanllchdr_from_poll(priv);
	PRT_BEAMFORMING_INFO 		pBeamInfo = &(priv->pshare->BeamformingInfo);
	DECLARE_TXINSN(txinsn);	

#if 0 //eric-8822 tx hang
	{
		unsigned int tmp8 = RTL_R8(0x522);
		if(tmp8 & 0x10) {
			panic_printk("[%s] 0x522=0x%x DROP +++ \n", __FUNCTION__, RTL_R8(0x522));
			return TRUE;
		}
	}
#if 0
		if(!is_mgmt_q_empty(priv)){
			panic_printk("[%s] DROP +++ \n", __FUNCTION__);
			return TRUE;
		}
#endif
#endif

	//ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]: => \n", __func__));
	if(pbuf)
	{
		memset(pbuf, 0, sizeof(struct wlan_hdr));

		ConstructVHTNDPAPacket_MU	(
			priv, 
			RA,
			AID,
			pbuf,
			&PacketLength,
			BW
			);

#if 1 //eric-mu
		if(priv->pshare->rf_ft_var.sndlen != 0xff ){
			printk(" !!! Force PacketLength = %d \n", priv->pshare->rf_ft_var.sndlen);
			PacketLength = priv->pshare->rf_ft_var.sndlen;

		}
#endif


		txinsn.q_num = MANAGE_QUE_NUM;
		txinsn.fr_type = _PRE_ALLOCLLCHDR_;		
		txinsn.phdr = pbuf;
		txinsn.hdr_len = PacketLength;
		txinsn.fr_len = 0;
		txinsn.fixed_rate = 1;	
		txinsn.tx_rate = NDPTxRate;	// According to Nr
		txinsn.ndpa = 1; //broadcast NDPA

		if(pBeamInfo->SoundingInfoV2.CandidateMUBFeeCnt  > 1)
			txinsn.SND_pkt_sel = 1; //broadcast NDPA
		else
			txinsn.SND_pkt_sel = 0; //unicast NDPA
#if 0
panic_printk("[%s] %02x:%02x:%02x:%02x:%02x:%02x TxRate = 0x%x MUCnt = %d Len=%d\n", __FUNCTION__, 
	RA[0],RA[1],RA[2],RA[3],RA[4],RA[5],
	NDPTxRate, pBeamInfo->SoundingInfoV2.CandidateMUBFeeCnt, PacketLength);
panic_printk("0x14c0 = 0x%x \n", RTL_R32(0x14c0));
{
		int i = 0;

		for(i = 0; i<PacketLength; i++){
			if((i%10) == 0)
				printk("\n");

			printk("0x%02x ", pbuf[i]);

		}
		printk("\n");
}
#endif


#if 1 //eric-mu
		if(priv->pshare->rf_ft_var.snd != 0xff ){
			printk(" !!! Force SND_pkt_sel = 0x%x \n", priv->pshare->rf_ft_var.snd);
			txinsn.SND_pkt_sel = priv->pshare->rf_ft_var.snd;

		}
#endif

		if (rtl8192cd_wlantx(priv, &txinsn) == CONGESTED) {		
			netif_stop_queue(priv->dev);		
			priv->ext_stats.tx_drops++; 	
			panic_printk("[%s]TX DROP: Congested!\n", __FUNCTION__);
			if (txinsn.phdr)
				release_wlanhdr_to_poll(priv, txinsn.phdr); 			
			if (txinsn.pframe)
				release_mgtbuf_to_poll(priv, txinsn.pframe);
			return 0;	
		}
	}
	else
		ret = FALSE;

	return ret;
}


VOID
ConstructReportPollPacket(
	struct rtl8192cd_priv *priv,
	pu1Byte			RA,
	pu1Byte			Buffer,
	pu4Byte			pLength,
	u1Byte 			BW
	)
{
	u2Byte					Duration= 0;
	u1Byte					FSR_Bitmap = 0xff;
	pu1Byte					pNDPAFrame = Buffer;
	u2Byte					tmp16;
	
	int aSifsTime = ((priv->pmib->dot11BssType.net_work_type & WIRELESS_11N) && (priv->pshare->ht_sta_num)) ? 0x10 : 10;

	// Frame control.
	SET_80211_HDR_FRAME_CONTROL(pNDPAFrame, 0);
	SET_80211_HDR_TYPE_AND_SUBTYPE(pNDPAFrame, Type_Beamforming_Report_Poll);

	memcpy((void *)GetAddr1Ptr(pNDPAFrame), RA, MACADDRLEN);
	memcpy((void *)GetAddr2Ptr(pNDPAFrame), GET_MY_HWADDR, MACADDRLEN);

	Duration = 2*aSifsTime + 44;
	
	if(BW == HT_CHANNEL_WIDTH_80)
		Duration += 40;
	else if(BW == HT_CHANNEL_WIDTH_20_40)
		Duration+= 87;
	else	
		Duration+= 180;

	SetDuration(pNDPAFrame, Duration);

	memcpy(pNDPAFrame+16, &FSR_Bitmap, 1);

	*pLength = 17;
}


BOOLEAN
SendReportPollPacket(
	struct rtl8192cd_priv *priv,
	IN	pu1Byte			RA,
	u1Byte 				BW,
	u1Byte				TxRate,
	BOOLEAN				bFinalPoll
	)
{
	BOOLEAN					ret = TRUE;
	u4Byte 					PacketLength = 0;
	unsigned char *pbuf 	= get_wlanllchdr_from_poll(priv);
	DECLARE_TXINSN(txinsn);	

	//panic_printk("[%s]%02x:%02x:%02x:%02x:%02x:%02x TxRate = 0x%x bFinalPoll = %d\n", __FUNCTION__, 
		//RA[0],RA[1],RA[2],RA[3],RA[4],RA[5],
		//TxRate, bFinalPoll);

#if 0 //eric-mu
	printk("No Send Report Poll \n");
	return 0;
#endif

#if 0 //eric-8822 tx hang
	{
		unsigned int tmp8 = RTL_R8(0x522);
		if(tmp8 & 0x10) {
			panic_printk("[%s] 0x522=0x%x DROP +++ \n", __FUNCTION__, RTL_R8(0x522));
			return TRUE;
		}
	}
#if 0
		if(!is_mgmt_q_empty(priv)){
			panic_printk("[%s] DROP +++ \n", __FUNCTION__);
			return TRUE;
		}
#endif
#endif

		

	if(pbuf)
	{
		memset(pbuf, 0, sizeof(struct wlan_hdr));

		ConstructReportPollPacket	(
			priv, 
			RA,
			pbuf,
			&PacketLength,
			BW
			);

		txinsn.q_num = MANAGE_QUE_NUM;
		txinsn.fr_type = _PRE_ALLOCLLCHDR_;		
		txinsn.phdr = pbuf;
		txinsn.hdr_len = PacketLength;
		txinsn.fr_len = 0;
		txinsn.fixed_rate = 1;	
		txinsn.tx_rate = TxRate;	// According to Nr	
		txinsn.ndpa = 1;
		if (bFinalPoll)
			txinsn.SND_pkt_sel = 3;
		else
			txinsn.SND_pkt_sel = 2;

#if 0 //eric-mu
		txinsn.ndpa = 0;
		txinsn.SND_pkt_sel = 0;
#endif

		if (rtl8192cd_wlantx(priv, &txinsn) == CONGESTED) {		
			netif_stop_queue(priv->dev);		
			priv->ext_stats.tx_drops++; 	
			panic_printk("[%s]TX DROP: Congested!\n", __FUNCTION__);
			if (txinsn.phdr)
				release_wlanhdr_to_poll(priv, txinsn.phdr); 			
			if (txinsn.pframe)
				release_mgtbuf_to_poll(priv, txinsn.pframe);
			return 0;	
		}
	}
	else
		ret = FALSE;

	return ret;
}

#if 1 //eric-8822
void input_value_64(unsigned long long *p, unsigned char start, unsigned char end, unsigned int value)
{
	unsigned long long bit_mask = 0;

	if(value > 0) //only none-zero value needs to be assigned 
	{
		if(start == end) //1-bit value
		{
			*p |= BIT(start);
		}
		else
		{
			unsigned char x = 0;
				
			for(x = 0; x<=(end-start); x ++)
				bit_mask |= BIT(x);

			*p |= ((value&bit_mask) << start);	
		}
	}

}


#if defined(RTK_AC_SUPPORT) 
void assign_mu_sta1_by_cmd(struct rtl8192cd_priv *priv)
{
	PRT_BEAMFORMING_INFO		pBeamInfo = &(priv->pshare->BeamformingInfo);
	PRT_BEAMFORMING_ENTRY	pEntry;
	unsigned int tmp32;

	if(priv->pshare->rf_ft_var.ndpa_swap == 0)		
		return;

#if 0
	RTL_W8(0x522, 0x0);

	tmp32 = RTL_R32(0x14c0);
	tmp32 &= (~BIT7); 	
	RTL_W32(0x14c0, tmp32);
#endif

#if 0
	panic_printk("[Target] %02x:%02x:%02x:%02x:%02x:%02x\n", 
		priv->pshare->rf_ft_var.ndpa_mac[0], priv->pshare->rf_ft_var.ndpa_mac[1], priv->pshare->rf_ft_var.ndpa_mac[2], 
		priv->pshare->rf_ft_var.ndpa_mac[3], priv->pshare->rf_ft_var.ndpa_mac[4], priv->pshare->rf_ft_var.ndpa_mac[5]);

	panic_printk("[0] %02x:%02x:%02x:%02x:%02x:%02x\n", 
		pBeamInfo->BeamformeeEntry[0].MacAddr[0], pBeamInfo->BeamformeeEntry[0].MacAddr[1], pBeamInfo->BeamformeeEntry[0].MacAddr[2], 
		pBeamInfo->BeamformeeEntry[0].MacAddr[3], pBeamInfo->BeamformeeEntry[0].MacAddr[4], pBeamInfo->BeamformeeEntry[0].MacAddr[5]);
		
	panic_printk("[1] %02x:%02x:%02x:%02x:%02x:%02x\n", 
		pBeamInfo->BeamformeeEntry[1].MacAddr[0], pBeamInfo->BeamformeeEntry[1].MacAddr[1], pBeamInfo->BeamformeeEntry[1].MacAddr[2], 
		pBeamInfo->BeamformeeEntry[1].MacAddr[3], pBeamInfo->BeamformeeEntry[1].MacAddr[4], pBeamInfo->BeamformeeEntry[1].MacAddr[5]);
#endif		
#if 1
	if(!memcmp(pBeamInfo->BeamformeeEntry[1].MacAddr, priv->pshare->rf_ft_var.ndpa_mac, 6)){	
		struct stat_info *pstat1 = get_stainfo(priv, pBeamInfo->BeamformeeEntry[0].MacAddr);
		struct stat_info *pstat2 = get_stainfo(priv, pBeamInfo->BeamformeeEntry[1].MacAddr);

		panic_printk("[%s][%d] Assign MAC is STA2, swap with STA1 \n", __FUNCTION__, __LINE__);

		Beamforming_Leave(priv, pstat1->hwaddr);
		Beamforming_Leave(priv, pstat2->hwaddr);

		delay_ms(100);
		delay_ms(100);
		delay_ms(100);
		
#if 1
		RTL_W8(0x522, 0x0);
				
		tmp32 = RTL_R32(0x14c0);
		tmp32 &= (~BIT7);	
		RTL_W32(0x14c0, tmp32);
#endif

		Beamforming_Enter(priv, pstat2);
		Beamforming_Enter(priv, pstat1);
	}
	//else
		//panic_printk("[%s][%d] Assign MAC is NOT connected \n", __FUNCTION__, __LINE__);
#endif
}
#endif
#endif

void issue_action_GROUP_ID(struct rtl8192cd_priv *priv, unsigned char idx)
{
	unsigned char	*pbuf;
	//unsigned char	membership_status[8] = {0,1,2,3,4,5,6,7};
	//unsigned char	user_position[16] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
	unsigned char	hwaddr_tmp[6] = {0x00, 0xe0, 0x4c, 0x88, 0x22, 0xee};

	PRT_BEAMFORMING_INFO	pBeamInfo = &(priv->pshare->BeamformingInfo);
	struct stat_info *pstat = get_stainfo(priv, pBeamInfo->BeamformeeEntry[idx].MacAddr);

	//panic_printk("\n eric-mu [%s][%d] \n\n", __FUNCTION__, __LINE__);
	
	DECLARE_TXINSN(txinsn);

	txinsn.q_num = MANAGE_QUE_NUM;
	txinsn.fr_type = _PRE_ALLOCMEM_;
#ifdef P2P_SUPPORT	/*cfg p2p cfg p2p*/
	if(rtk_p2p_is_enabled(priv)){
      	txinsn.tx_rate = _6M_RATE_;
	}else
#endif    
    	txinsn.tx_rate = find_rate(priv, NULL, 0, 1);
#ifndef TX_LOWESTRATE
	txinsn.lowest_tx_rate = txinsn.tx_rate;
#endif	
	txinsn.fixed_rate = 1;

	pbuf = txinsn.pframe = get_mgtbuf_from_poll(priv);
	if (pbuf == NULL)
		goto issue_opm_notification_fail;

	txinsn.phdr = get_wlanhdr_from_poll(priv);
	if (txinsn.phdr == NULL)
		goto issue_opm_notification_fail;

	memset((void *)(txinsn.phdr), 0, sizeof(struct wlan_hdr));

	pbuf[0] = _VHT_ACTION_CATEGORY_ID_;
	pbuf[1] = _VHT_ACTION_GROUPID_ID_;

#if defined(RTK_AC_SUPPORT) 
	if((priv->pshare->rf_ft_var.gidforce != 0xff) && (priv->pshare->rf_ft_var.gidforce <= 63)){
		unsigned long long tmp64 = 0;
		unsigned char gid = priv->pshare->rf_ft_var.gidforce;
		input_value_64(&tmp64, gid, gid, 1);
		tmp64 = cpu_to_le64(tmp64);
		memcpy((pbuf+2), &tmp64, 8);
	}
	else
#endif
	memcpy((pbuf+2), &pBeamInfo->BeamformeeEntry[idx].gid_valid, 8);

	memcpy((pbuf+2+8), &pBeamInfo->BeamformeeEntry[idx].user_position, 16);
	
	//construct_GID_membership_status(priv, membership_status);
	//construct_GID_user_position(priv, user_position);

	//memcpy((pbuf+2), membership_status, 8);
	//memcpy((pbuf+2+8), user_position, 16);
	
	txinsn.fr_len = _GROUPID_Frame_Length_;

	SetFrameSubType((txinsn.phdr), WIFI_WMM_ACTION);

	if(!pstat) {
		panic_printk("issue_action_GROUP_ID pstat = NULL !\n");
		memcpy((void *)GetAddr1Ptr((txinsn.phdr)), hwaddr_tmp, MACADDRLEN);	//eric-8822
	}
	else
	memcpy((void *)GetAddr1Ptr((txinsn.phdr)), pstat->hwaddr, MACADDRLEN);
	
	memcpy((void *)GetAddr2Ptr((txinsn.phdr)), GET_MY_HWADDR, MACADDRLEN);
	memcpy((void *)GetAddr3Ptr((txinsn.phdr)), BSSID, MACADDRLEN);


	if ((rtl8192cd_firetx(priv, &txinsn)) == SUCCESS) {
		return;
	}

issue_opm_notification_fail:

	if (txinsn.phdr)
		release_wlanhdr_to_poll(priv, txinsn.phdr);
	if (txinsn.pframe)
		release_mgtbuf_to_poll(priv, txinsn.pframe);
	return;
}

VOID
DBG_ConstructVHTNDPAPacket_MU(
	struct rtl8192cd_priv *priv,
	pu1Byte			RA,
	u2Byte			AID,
	pu1Byte			Buffer,
	pu4Byte			pLength,
	u1Byte 			BW
	)
{
	u2Byte					Duration= 0;
	u1Byte					Sequence = 0, idx;
	pu1Byte					pNDPAFrame = Buffer;
	u2Byte					tmp16 = 0;
	
	int aSifsTime = ((priv->pmib->dot11BssType.net_work_type & WIRELESS_11N) && (priv->pshare->ht_sta_num)) ? 0x10 : 10;

	// Frame control.
	SET_80211_HDR_FRAME_CONTROL(pNDPAFrame, 0);
	SET_80211_HDR_TYPE_AND_SUBTYPE(pNDPAFrame, Type_NDPA);

	memcpy((void *)GetAddr1Ptr(pNDPAFrame), RA, MACADDRLEN);
	memcpy((void *)GetAddr2Ptr(pNDPAFrame), GET_MY_HWADDR, MACADDRLEN);

	Duration = 2*aSifsTime + 44;
	
	if(BW == HT_CHANNEL_WIDTH_80)
		Duration += 40;
	else if(BW == HT_CHANNEL_WIDTH_20_40)
		Duration+= 87;
	else	
		Duration+= 180;

	SetDuration(pNDPAFrame, Duration);
	Sequence = GET_HW(priv)->sounding_seq<<2;
	GET_HW(priv)->sounding_seq =  (GET_HW(priv)->sounding_seq+1) & 0xfff;
	 
	memcpy(pNDPAFrame+16, &Sequence,1);

	//eric-8822 ?? there will be multiple STA infos ??

	if (OPMODE & WIFI_ADHOC_STATE)
		AID = 0;


	*pLength = 17;

	/* Construct STA info. for multiple STAs */
	tmp16 = 0;
	memset(&tmp16, 0, sizeof(tmp16));
	
	input_value_16(&tmp16, 0, 11, AID);
	input_value_16(&tmp16, 12, 12, 1); /*1: MU 0: SU*/
	input_value_16(&tmp16, 13, 15, 0); /*Nc index*/

	tmp16 = cpu_to_le16(tmp16);
	memcpy(pNDPAFrame+(*pLength), &tmp16, 2);

	*pLength += 2;
	
}


BOOLEAN
DBG_SendVHTNDPAPacket_MU(
	struct rtl8192cd_priv *priv,
	IN	pu1Byte			RA,
	IN	u2Byte			AID,
	u1Byte 				BW,
	u1Byte		NDPTxRate
	)
{
	BOOLEAN					ret = TRUE;
	u4Byte 					PacketLength;
	unsigned char *pbuf 	= get_wlanllchdr_from_poll(priv);
	DECLARE_TXINSN(txinsn);	

#if 1 //eric-8822 tx hang
{
	unsigned int tmp8 = RTL_R8(0x522);
	if(tmp8 & 0x10) {
		panic_printk("[%s] 0x522=0x%x DROP +++ \n", __FUNCTION__, RTL_R8(0x522));
		return TRUE;
	}
}
#if 0
	if(!is_mgmt_q_empty(priv)){
		panic_printk("[%s] DROP +++ \n", __FUNCTION__);
		return TRUE;
	}
#endif
#endif


	if(pbuf)
	{
		memset(pbuf, 0, sizeof(struct wlan_hdr));

		DBG_ConstructVHTNDPAPacket_MU	(
			priv, 
			RA,
			AID,
			pbuf,
			&PacketLength,
			BW
			);

		txinsn.q_num = MANAGE_QUE_NUM;
		txinsn.fr_type = _PRE_ALLOCLLCHDR_;		
		txinsn.phdr = pbuf;
		txinsn.hdr_len = PacketLength;
		txinsn.fr_len = 0;
		txinsn.fixed_rate = 1;	
		txinsn.tx_rate = NDPTxRate;	// According to Nr	
		txinsn.ndpa = 1;
		txinsn.SND_pkt_sel = 0; //unicast NDPA

		if (rtl8192cd_wlantx(priv, &txinsn) == CONGESTED) {		
			netif_stop_queue(priv->dev);		
			priv->ext_stats.tx_drops++; 	
			panic_printk("[%s]TX DROP: Congested!\n", __FUNCTION__);
			if (txinsn.phdr)
				release_wlanhdr_to_poll(priv, txinsn.phdr); 			
			if (txinsn.pframe)
				release_mgtbuf_to_poll(priv, txinsn.pframe);
			return 0;	
		}
	}
	else
		ret = FALSE;

	return ret;
}

/*******************************************/
/*New SU/MU Sounding Procesure                                 */
/*******************************************/

u1Byte
beamform_GetFirstMUBFeeEntryIdx(
	struct rtl8192cd_priv *priv
	)
{
	PRT_BEAMFORMING_INFO 		pBeamInfo = &(priv->pshare->BeamformingInfo);
	u1Byte					idx = 0xFF;
	BOOLEAN					bFound = FALSE;

	for (idx = 0; idx < BEAMFORMEE_ENTRY_NUM; idx++) {
		if (pBeamInfo->BeamformeeEntry[idx].bUsed && 
			(pBeamInfo->BeamformeeEntry[idx].BeamformEntryCap & BEAMFORM_CAP_VHT_MU_BFEE)) {			
			//RT_TRACE(COMP_BF, DBG_LOUD, ("[%s] idx=%d!\n", __func__, idx));
			bFound = TRUE;
			break;
		}	
	}

	if (!bFound)
		idx = 0xFF;

	return idx;
}

VOID
beamform_UpdateMinSoundingPeriod(
	struct rtl8192cd_priv *priv,
	IN u2Byte			CurBFeePeriod,
	IN BOOLEAN		bBFeeLeave
	)
{
	PRT_BEAMFORMING_INFO 		pBeamInfo = &(priv->pshare->BeamformingInfo);
	u1Byte		i = 0;
	u2Byte		tempMinVal = 0xFFFF;			

	ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s() ==> bBFeeLeave(%d)\n", __func__, bBFeeLeave));

	if(bBFeeLeave)
	{
		// When a BFee left, we need to find the latest min sounding period from the remaining BFees.
		for (i= 0; i < BEAMFORMEE_ENTRY_NUM; i++)
		{
			if (pBeamInfo->BeamformeeEntry[i].bUsed)
			{			
				ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("Idx(%d) SoundPeriod=%d!\n", i, pBeamInfo->BeamformeeEntry[i].SoundPeriod));
				if(pBeamInfo->BeamformeeEntry[i].SoundPeriod < tempMinVal)
					tempMinVal = pBeamInfo->BeamformeeEntry[i].SoundPeriod;
			}	
		}

		pBeamInfo->SoundingInfoV2.MinSoundingPeriod = (tempMinVal == 0xFFFF)? 0 : tempMinVal;
	}
	else
	{
		if(pBeamInfo->SoundingInfoV2.MinSoundingPeriod == 0)
		{
			// Use current BFee's sounding period as the minimun one.
			pBeamInfo->SoundingInfoV2.MinSoundingPeriod = CurBFeePeriod;	
		}
		else
		{
			// Update min sounding period
			if(CurBFeePeriod < pBeamInfo->SoundingInfoV2.MinSoundingPeriod)
				pBeamInfo->SoundingInfoV2.MinSoundingPeriod = CurBFeePeriod;
		}
	}
}


RT_STATUS
beamform_GetSoundingList(
	struct rtl8192cd_priv *priv
)
{
	PRT_BEAMFORMING_INFO 		pBeamInfo = &(priv->pshare->BeamformingInfo);
	u1Byte						i, MUIdx = 0, SUIdx = 0;
	BOOLEAN						bAddSUSoundingList = FALSE;
	RT_STATUS					rtStatus = RT_STATUS_SUCCESS;

	//
	// Add MU BFee list first because MU priority is higher than SU
	//
	for(i=0; i<BEAMFORMEE_ENTRY_NUM; i++)
	{
		if(pBeamInfo->BeamformeeEntry[i].bUsed)
		{
			if(pBeamInfo->BeamformeeEntry[i].HwState != BEAMFORM_ENTRY_HW_STATE_ADDED)
			{		
				ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]: Invalid BFee idx(%d) Hw state=%d\n", __func__, i, pBeamInfo->BeamformeeEntry[i].HwState));
				return RT_STATUS_FAILURE;
			}
			else
			{
				// Decrease BFee's SoundCnt per period.
				if(pBeamInfo->BeamformeeEntry[i].SoundCnt > 0)
					pBeamInfo->BeamformeeEntry[i].SoundCnt--;
			}

#if 1 //eric-mu
{
			PRT_SOUNDING_INFOV2			pSoundingInfo = &(pBeamInfo->SoundingInfoV2);
			
			if(pSoundingInfo->State == SOUNDING_STATE_INIT) 
				pBeamInfo->BeamformeeEntry[i].SoundCnt = 0;
}
#endif
		
			//
			// <tynli_Note> If the STA supports MU BFee capability then we add it to MUSoundingList directly
			//	because we can only sound one STA by unicast NDPA with MU cap enabled to get correct channel info.
			//	Suggested by BB team Luke Lee. 2015.11.25.
			//
			// MU BFee
			//ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]: idx = %d, BeamformEntryCap = 0x%x, SoundCnt = %d\n", __func__, i, pBeamInfo->BeamformeeEntry[i].BeamformEntryCap, pBeamInfo->BeamformeeEntry[i].SoundCnt));
			if(pBeamInfo->BeamformeeEntry[i].BeamformEntryCap & (BEAMFORM_CAP_VHT_MU_BFER)) //eric-mu
			{
				if(MUIdx < MAX_NUM_BEAMFORMEE_MU)
				{
					if(pBeamInfo->BeamformeeEntry[i].SoundCnt == 0)
					{
						pBeamInfo->BeamformeeEntry[i].bCandidateSoundingPeer = TRUE;
						//panic_printk("eric-mu [%s][%d] %d %d\n", __FUNCTION__, __LINE__, pBeamInfo->BeamformeeEntry[i].SoundPeriod, pBeamInfo->SoundingInfoV2.MinSoundingPeriod);
						pBeamInfo->BeamformeeEntry[i].SoundCnt = GetInitSoundCnt(pBeamInfo->BeamformeeEntry[i].SoundPeriod, pBeamInfo->SoundingInfoV2.MinSoundingPeriod);
						pBeamInfo->SoundingInfoV2.MUSoundingList[MUIdx] = i;
						MUIdx++;
					}
				}
			}
			else
			{
				// <1> Support SU BFee Cap
				if(pBeamInfo->BeamformeeEntry[i].BeamformEntryCap & (BEAMFORMER_CAP_VHT_SU|BEAMFORMER_CAP_HT_EXPLICIT)) //eric-mu
				{
					if(SUIdx < MAX_NUM_BEAMFORMEE_SU)
					{
						// <2> If the remain count is 0, then it can be sounded at this time.
						if(pBeamInfo->BeamformeeEntry[i].SoundCnt == 0)
						{
							pBeamInfo->BeamformeeEntry[i].bCandidateSoundingPeer = TRUE;							
							pBeamInfo->BeamformeeEntry[i].SoundCnt = GetInitSoundCnt(pBeamInfo->BeamformeeEntry[i].SoundPeriod, pBeamInfo->SoundingInfoV2.MinSoundingPeriod);
							pBeamInfo->SoundingInfoV2.SUSoundingList[SUIdx] = i;
							SUIdx++;
						}
					}
				}
			}
		}
	}

	pBeamInfo->SoundingInfoV2.CandidateMUBFeeCnt = MUIdx;

#if 0
	//
	// Add SU BFee list
	//
	for(i=0; i<MAX_BEAMFORMEE_ENTRY_NUM; i++)
	{
		// SU BFee
		if(pBeamInfo->BeamformeeEntry[i].BeamformEntryCap & (BEAMFORMEE_CAP_VHT_SU|BEAMFORMEE_CAP_HT_EXPLICIT))
		{
			// If there are at least 2 MU STAs, it means MU sounding will be performed, so we should skip the STA 
			// which supports MU BFee capability.
			if((pBeamInfo->SoundingInfoV2.MUBFeeCnt >= 2) && 
				(pBeamInfo->BeamformeeEntry[i].BeamformEntryCap & (BEAMFORMEE_CAP_VHT_MU)))
			{
				bAddSUSoundingList = FALSE;
			}
			else
			{
				bAddSUSoundingList = TRUE;
			}
		}

		if(bAddSUSoundingList)
		{
			if(SUIdx < MAX_NUM_BEAMFORMEE_SU)
			{
				pBeamInfo->SoundingInfoV2.SUSoundingList[SUIdx] = i;
				SUIdx++;
			}
		}
	}
#endif	
	//pBeamInfo->SoundingInfoV2.CurSUBFeeCnt = SUIdx;

	if(SUIdx + MUIdx == 0)
		rtStatus = RT_STATUS_FAILURE;

	//ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]: There are %d SU and %d MU BFees in this sounding period\n", __func__, SUIdx, MUIdx));
	//panic_printk("eric-mu [%s] %d SU and %d MU BFees status = %d \n", 
		//__FUNCTION__, SUIdx, MUIdx, rtStatus);

	return rtStatus;
}

u1Byte
beamform_GetSUSoundingIdx(
	struct rtl8192cd_priv *priv
)
{
	PRT_BEAMFORMING_INFO 		pBeamInfo = &(priv->pshare->BeamformingInfo);
	PRT_SOUNDING_INFOV2			pSoundingInfo = &(pBeamInfo->SoundingInfoV2);
	u1Byte						i, idx = BEAMFORMEE_ENTRY_NUM;

	//
	// Get non-sound SU BFee index
	//
#if 1
	for(idx=0; idx<BEAMFORMEE_ENTRY_NUM; idx++)
	{
		if(pBeamInfo->BeamformeeEntry[idx].bUsed && (!pBeamInfo->BeamformeeEntry[idx].bSound) &&
			pBeamInfo->BeamformeeEntry[idx].bCandidateSoundingPeer)
		{ //eric-txbf ??
			if((pBeamInfo->BeamformeeEntry[idx].BeamformEntryCap & (BEAMFORMEE_CAP_VHT_SU|BEAMFORMEE_CAP_HT_EXPLICIT|BEAMFORMER_CAP_VHT_SU|BEAMFORMER_CAP_VHT_SU)) &&
				!(pBeamInfo->BeamformeeEntry[idx].BeamformEntryCap & (BEAMFORM_CAP_VHT_MU_BFEE)))
			{
				pBeamInfo->BeamformeeEntry[idx].bSound = TRUE;
				break;
			}
		}
	}
#else
	for(i=0; i<pSoundingInfo->SUBFeeCnt; i++)
	{
		if(pBeamInfo->BeamformeeEntry[pSoundingInfo->SUSoundingList[i]].bUsed &&
			(!pBeamInfo->BeamformeeEntry[pSoundingInfo->SUSoundingList[i]].bSound) &&
			pBeamInfo->BeamformeeEntry[i].bCandidateSoundingPeer)
		{
			idx = pSoundingInfo->SUSoundingList[i];
			pBeamInfo->BeamformeeEntry[idx].bSound = TRUE;
			break;
		}
	}

#endif
	return idx;

}

BOOLEAN
beamform_IsLastSoundingPeer(
	struct rtl8192cd_priv *priv
)
{
	PRT_BEAMFORMING_INFO 		pBeamInfo = &(priv->pshare->BeamformingInfo);
	PRT_SOUNDING_INFOV2			pSoundingInfo = &(pBeamInfo->SoundingInfoV2);
	u1Byte						idx;
	BOOLEAN						bLastSoundPeer = TRUE;

	for(idx=0; idx<BEAMFORMEE_ENTRY_NUM; idx++)
	{
		//ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]: bUsed = %d, bSound = %d, bCandidateSoundingPeer = %d\n", 
			//__func__, pBeamInfo->BeamformeeEntry[idx].bUsed, pBeamInfo->BeamformeeEntry[idx].bSound, pBeamInfo->BeamformeeEntry[idx].bCandidateSoundingPeer));
		if(pBeamInfo->BeamformeeEntry[idx].bUsed && (!pBeamInfo->BeamformeeEntry[idx].bSound) &&
			pBeamInfo->BeamformeeEntry[idx].bCandidateSoundingPeer)
		{
			bLastSoundPeer = FALSE;
			break;
		}
	}

	//ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]: %d\n", __func__, bLastSoundPeer));

	return bLastSoundPeer;
}

VOID
beamform_ResetSoundingVars(
	struct rtl8192cd_priv *priv
)
{
	PRT_BEAMFORMING_INFO 		pBeamInfo = &(priv->pshare->BeamformingInfo);
	PRT_SOUNDING_INFOV2			pSoundingInfo = &(pBeamInfo->SoundingInfoV2);
	u1Byte						idx;
	
	pSoundingInfo->SUSoundNumPerPeriod = 0;
	pSoundingInfo->MUSoundNumPerPeriod = 0;

	// Clear bSound flag for the new period.
	for(idx=0; idx<BEAMFORMEE_ENTRY_NUM; idx++)
	{
		if(pBeamInfo->BeamformeeEntry[idx].bUsed && pBeamInfo->BeamformeeEntry[idx].bSound)
		{
			pBeamInfo->BeamformeeEntry[idx].bSound = FALSE;
			pBeamInfo->BeamformeeEntry[idx].bCandidateSoundingPeer = FALSE;
		}
	}
}

VOID
beamform_DecreaseBFeeSoundCnt(
	struct rtl8192cd_priv *priv
)
{
	PRT_BEAMFORMING_INFO 		pBeamInfo = &(priv->pshare->BeamformingInfo);
	u1Byte						idx;
	
	// Clear bSound flag for the new period.
	for(idx=0; idx<BEAMFORMEE_ENTRY_NUM; idx++)
	{
		if(pBeamInfo->BeamformeeEntry[idx].bUsed && (!pBeamInfo->BeamformeeEntry[idx].bSound))
		{
			if(pBeamInfo->BeamformeeEntry[idx].SoundCnt > 0)
				pBeamInfo->BeamformeeEntry[idx].SoundCnt--;
		}
	}
}

VOID 
beamform_SoundingTimerCallback(
	struct rtl8192cd_priv *priv
    )
{
	PRT_BEAMFORMING_INFO 		pBeamInfo = &(priv->pshare->BeamformingInfo);
	PRT_SOUNDING_INFOV2			pSoundingInfo = &(pBeamInfo->SoundingInfoV2);
	PRT_BEAMFORMING_ENTRY		pEntry = NULL;
	u1Byte			SUSoundingIdx = BEAMFORMEE_ENTRY_NUM;
	u4Byte			TimeoutPeriod = 0;
	BOOLEAN			bSetTimeoutTimer = FALSE;
	RT_STATUS					rtStatus = RT_STATUS_SUCCESS;
	static u2Byte		WaitCnt = 0;
	u1Byte					NDPTxRate;

	//panic_printk("\n eric-mu [%s][%d] +++ pSoundingInfo->State = %d\n", __FUNCTION__, __LINE__, pSoundingInfo->State); //eric-mu

#if 0 //eric-mu
	ODM_SetTimer(ODMPTR, &pBeamInfo->BFSoundingTimeoutTimer, 100);
	return;
#endif

	//ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s] ===> pSoundingInfo->State = %d\n", __func__, pSoundingInfo->State));

	if(pSoundingInfo->State != SOUNDING_STATE_INIT && pSoundingInfo->State != SOUNDING_STATE_SU_SOUNDDOWN
		&& pSoundingInfo->State != SOUNDING_STATE_MU_SOUNDDOWN && pSoundingInfo->State != SOUNDING_STATE_SOUNDING_TIMEOUT)
	{
		ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]: Invalid State(%d) and return!\n", __func__, pSoundingInfo->State));
	}

	do
	{
		if(1) //eric-mu (pSoundingInfo->State == SOUNDING_STATE_INIT)
		{
			// Init Var
			beamform_ResetSoundingVars(priv);
 			
			// Get the sounding list of this sounding period
			//panic_printk("[%s][%d] \n", __FUNCTION__, __LINE__); 
			rtStatus = beamform_GetSoundingList(priv);
			if(rtStatus == RT_STATUS_FAILURE)
			{
				WaitCnt=0;
				pSoundingInfo->State = SOUNDING_STATE_NONE;
				ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]: No BFees found. Set to SOUNDING_STATE_NONE \n", __func__));
				break;
			}
			else if(rtStatus == RT_STATUS_FAILURE)
			{
				// Set the next sounding timer
				if(WaitCnt < 500)
				{
					WaitCnt++;
					//PlatformSetTimer(Adapter, &(pBeamInfo->BFSoundingTimer), 2); // 2ms
					ODM_SetTimer(ODMPTR, &pBeamInfo->BeamformingTimer, 2); 
				} 
				else
				{
					WaitCnt=0;
					pSoundingInfo->State = SOUNDING_STATE_NONE;
					ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]: Wait changing Hw state timeout!!. Set to SOUNDING_STATE_NONE \n", __func__));
				}
				break;
			}
			else if(rtStatus == RT_STATUS_SUCCESS)
			{
				WaitCnt=0;
				// Decrease All BFees' SoundCnt per period.
				//beamform_DecreaseBFeeSoundCnt(Adapter);
			}
			else
			{
				WaitCnt=0;
				ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]: Unkown state! \n", __func__));
				break;
			}
		}
		
		SUSoundingIdx = beamform_GetSUSoundingIdx(priv);
		if((SUSoundingIdx < BEAMFORMEE_ENTRY_NUM) && (pSoundingInfo->CandidateMUBFeeCnt == 0))
		{
			//printk("[%s][%d] SU Sounding  +++\n", __FUNCTION__, __LINE__);
			pSoundingInfo->SUBFeeCurIdx = SUSoundingIdx;
			// Set to sounding start state
			pSoundingInfo->State = SOUNDING_STATE_SU_START;
			//ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]: Set to SOUNDING_STATE_SU_START \n", __func__));

			pEntry = &(pBeamInfo->BeamformeeEntry[SUSoundingIdx]);

			// Reset sounding timeout flag for the new sounding.
			pEntry->bSoundingTimeout = FALSE;

			// Start SU sounding
			if(pEntry->BeamformEntryCap & BEAMFORMER_CAP_HT_EXPLICIT) { //eric-txbf
				//beamform_SendSWVHTNDPAPacket(Adapter, pEntry->MacAddr, pEntry->AID, pEntry->SoundBW);
				NDPTxRate = Beamforming_GetHTNDPTxRate(priv, pEntry->CompSteeringNumofBFer);
				SendHTNDPAPacket(priv, pEntry->MacAddr, pEntry->BW, NDPTxRate);
			} else if(pEntry->BeamformEntryCap & BEAMFORMER_CAP_VHT_SU) { //eric-txbf
				//beamform_SendHTNDPAPacket(Adapter, pEntry->MacAddr, pEntry->SoundBW);
				NDPTxRate = Beamforming_GetVHTNDPTxRate(priv, pEntry->CompSteeringNumofBFer);
				SendVHTNDPAPacket(priv,pEntry->MacAddr, pEntry->AID, pEntry->BW, NDPTxRate);
			}
			pSoundingInfo->SUSoundNumPerPeriod++;
		
			// Count the next period
			TimeoutPeriod = SU_SOUNDING_TIMEOUT;
			bSetTimeoutTimer = TRUE;
		}
		else
		{
			// If there is no SU BFee then find MU BFee and perform MU sounding.
			//
			// <tynli_note> Need to check the MU starting condition. 2015.12.15.
			//
			if(pSoundingInfo->CandidateMUBFeeCnt > 0)
			{
				pSoundingInfo->State = SOUNDING_STATE_MU_START;
				ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]: Set to SOUNDING_STATE_MU_START \n", __func__));

				// Update MU BFee info
				{
					u1Byte	i;
					for(i=0; i<MAX_NUM_BEAMFORMEE_MU; i++)
					{
						pEntry = &(pBeamInfo->BeamformeeEntry[i]); //&(pBeamInfo->BeamformeeEntry[pSoundingInfo->MUSoundingList[i]]); //eric-mu ??
						//ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]: BeamformEntryCap = 0x%x, bCandidateSoundingPeer = %d\n", 
							//__func__, pEntry->BeamformEntryCap, pEntry->bCandidateSoundingPeer));
						if((pEntry->BeamformEntryCap & BEAMFORM_CAP_VHT_MU_BFER) &&
							pEntry->bCandidateSoundingPeer)
						{
							pEntry->bSound = TRUE;
						}
					}
				}

				// Send MU NDPA
				//beamform_SendSWVHTMUNDPAPacket(Adapter, pEntry->SoundBW);

				// Send BF report poll
				if(0){
					printk("[%s][%d] No Sounding  +++\n", __FUNCTION__, __LINE__);
					ODM_SetTimer(ODMPTR, &pBeamInfo->BeamformingTimer, 10*1000);
					break;
				}
				else
				{
					u1Byte				idx, PollSTACnt = 0;
					BOOLEAN 			bGetFirstBFee = FALSE;
					
					if(pSoundingInfo->CandidateMUBFeeCnt > 1)
					{ /* More than 1 MU STA*/

#if 0 //eric-mu
for(idx = 0; idx < BEAMFORMEE_ENTRY_NUM; idx++)
{		
	pEntry = &(pBeamInfo->BeamformeeEntry[idx]);
	if((pEntry->BeamformEntryCap & BEAMFORM_CAP_VHT_MU_BFER) &&  //eric-mu send report poll
		pEntry->bCandidateSoundingPeer)
		issue_action_GROUP_ID(priv, idx);
}
//delay_ms(20);
#endif


#if 0
{
	unsigned long x;

	SAVE_INT_AND_CLI(x);
	SendVHTNDPAPacket_MU(priv, pEntry->MacAddr, pEntry->AID, pEntry->BW, _NSS2_MCS0_RATE_); //eric-mu
	SendReportPollPacket(priv, pEntry->MacAddr, pEntry->BW, _24M_RATE_,FALSE);
	RESTORE_INT(x);
}
#else
if(0)
	printk("[%s][%d] No Sounding  +++\n", __FUNCTION__, __LINE__);
else
{
	unsigned long x;
	//printk("[%s][%d] Start Sounding  +++\n", __FUNCTION__, __LINE__);
	RTL_W8(0x719, 0x82);

	SAVE_INT_AND_CLI(x);

	//RTL_W8(0x522, 0x10);
	//printk("0x522 = 0x%x +++\n", RTL_R8(0x522));

						for(idx = 0; idx < BEAMFORMEE_ENTRY_NUM; idx++)
						{		
							pEntry = &(pBeamInfo->BeamformeeEntry[idx]);
							if((pEntry->BeamformEntryCap & BEAMFORM_CAP_VHT_MU_BFER) &&  //eric-mu send report poll
								pEntry->bCandidateSoundingPeer)
							{
								if(bGetFirstBFee)
								{
									PollSTACnt++;
									if(PollSTACnt == (pSoundingInfo->CandidateMUBFeeCnt - 1))/* The last STA*/
										SendReportPollPacket(priv, pEntry->MacAddr, pEntry->BW, _24M_RATE_,TRUE);	
									else
										SendReportPollPacket(priv, pEntry->MacAddr, pEntry->BW, _24M_RATE_,FALSE);	
								}
								else
								{
									SendVHTNDPAPacket_MU(priv, pEntry->MacAddr, pEntry->AID, pEntry->BW, _NSS2_MCS0_RATE_); //eric-mu
									//SendReportPollPacket(priv, pEntry->MacAddr, pEntry->BW, _NSS1_MCS0_RATE_,FALSE);
									bGetFirstBFee = TRUE;
								}
							}
						}

	//RTL_W8(0x522, 0x0);
	//printk("0x522 = 0x%x ---\n", RTL_R8(0x522));
	
	RESTORE_INT(x);
	
	//ODM_SetTimer(ODMPTR, &pBeamInfo->BeamformingTimer, 10*1000);
	//break;
}
#endif
					}
#if 0 //eric-mu send single mu ndpa
					else if(pSoundingInfo->CandidateMUBFeeCnt == 1)
					{
						for(idx = 0; idx < BEAMFORMEE_ENTRY_NUM; idx++)
						{		
							pEntry = &(pBeamInfo->BeamformeeEntry[idx]);
							if((pEntry->BeamformEntryCap & BEAMFORM_CAP_VHT_MU_BFER) &&  //eric-mu send report poll
								pEntry->bCandidateSoundingPeer)
							{
								issue_action_GROUP_ID(priv, idx);
								SendVHTNDPAPacket_MU(priv, pEntry->MacAddr, pEntry->AID, pEntry->BW, _NSS1_MCS0_RATE_); //eric-mu
							}
						}						
					}
#endif



				}
		
				pSoundingInfo->MUSoundNumPerPeriod++;
				TimeoutPeriod = MU_SOUNDING_TIMEOUT;
				bSetTimeoutTimer = TRUE;
			}
			
		}

		//ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("SUSoundNumPerPeriod=%d, MUSoundNumPerPeriod=%d\n", 
			//pSoundingInfo->SUSoundNumPerPeriod, pSoundingInfo->MUSoundNumPerPeriod));
		
		// Set the next timer
		if(bSetTimeoutTimer)
		{
			int timeout = 40;
			//PlatformSetTimer(Adapter, &(pBeamInfo->BFSoundingTimeoutTimer), TimeoutPeriod);

			if(priv->pshare->rf_ft_var.mutime)
				timeout = priv->pshare->rf_ft_var.mutime;
			
			ODM_SetTimer(ODMPTR, &pBeamInfo->BFSoundingTimeoutTimer, timeout); //eric-mu
			break;
			//ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s(): Set Timeout Timer %d\n", __func__, TimeoutPeriod));
			
		}
	}
	while(FALSE);

	//ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s(): <===\n", __func__));

}

VOID 
beamform_SoundingTimeout(
    struct rtl8192cd_priv *priv
    )
{
	PRT_BEAMFORMING_INFO 		pBeamInfo = &(priv->pshare->BeamformingInfo);
	PRT_SOUNDING_INFOV2			pSoundingInfo = &(pBeamInfo->SoundingInfoV2);
	PRT_BEAMFORMING_ENTRY	pBFeeEntry = &(pBeamInfo->BeamformeeEntry[pSoundingInfo->SUBFeeCurIdx]);
	u4Byte			NextSoundPeriod = 0;
	u1Byte			DelayTime;
	//ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("%s(): ==>\n", __func__));


#if 0 //eric-mu
	ODM_SetTimer(ODMPTR, &pBeamInfo->BeamformingTimer, 10*1000);
	return; 
#endif

	if((pSoundingInfo->State == SOUNDING_STATE_SU_START && pSoundingInfo->State != SOUNDING_STATE_MU_SOUNDDOWN) ||
		(pSoundingInfo->State == SOUNDING_STATE_MU_START && pSoundingInfo->State != SOUNDING_STATE_SU_SOUNDDOWN))
	{
		if(!(pBFeeEntry->BeamformEntryCap & BEAMFORM_CAP_VHT_MU_BFEE) && 
			(pBFeeEntry->BeamformEntryCap & (BEAMFORMEE_CAP_VHT_SU|BEAMFORMEE_CAP_HT_EXPLICIT)))
		{	// SU BFee
			pBFeeEntry->bSoundingTimeout = TRUE;
			//ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]: The BFee entry[%d] is set to bSoundingTimeout!\n", __func__, pSoundingInfo->SUBFeeCurIdx));
		}
		pSoundingInfo->State = SOUNDING_STATE_SOUNDING_TIMEOUT;
		//ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]: Set to SOUNDING_STATE_MU_SOUND_TIMEOUT \n", __func__));		
	}

	if(!beamform_IsLastSoundingPeer(priv))
	{
		NextSoundPeriod = 0; //eric-mu ??		
		//ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]: Set next sounding start time to %d ms \n", __func__, NextSoundPeriod));
		//PlatformSetTimer(Adapter, &(pBeamInfo->BFSoundingTimer), NextSoundPeriod);
		ODM_SetTimer(ODMPTR, &pBeamInfo->BeamformingTimer, NextSoundPeriod);
	}
	else
	{
		if(pBeamInfo->beamformee_su_cnt > 0 || pBeamInfo->beamformee_mu_cnt > 0)
		{
			DelayTime = SU_SOUNDING_TIMEOUT*pSoundingInfo->SUSoundNumPerPeriod + MU_SOUNDING_TIMEOUT*pSoundingInfo->MUSoundNumPerPeriod;
#if 1 //eric-mu
			NextSoundPeriod = (pSoundingInfo->MinSoundingPeriod > DelayTime)?(pSoundingInfo->MinSoundingPeriod):(DelayTime) ;
#else
			NextSoundPeriod = (pSoundingInfo->MinSoundingPeriod > DelayTime)?(pSoundingInfo->MinSoundingPeriod - DelayTime):0 ;
#endif

			//panic_printk("eric-mu [%s][%d] MinSoundingPeriod=%d DelayTime=%d\n", 
				//__FUNCTION__, __LINE__, 
				//pSoundingInfo->MinSoundingPeriod, DelayTime);

			if(priv->pshare->rf_ft_var.mutime)
				NextSoundPeriod = priv->pshare->rf_ft_var.mutime; //eric-mu
			else
				NextSoundPeriod = 40;
		
			pSoundingInfo->State = SOUNDING_STATE_INIT;
			//ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]: Set to SOUNDING_STATE_INIT \n", __func__));
			//ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]: Set next sounding start time to %d ms \n", __func__, NextSoundPeriod));

			//PlatformSetTimer(Adapter, &(pBeamInfo->BFSoundingTimer), NextSoundPeriod);
			ODM_SetTimer(ODMPTR, &pBeamInfo->BeamformingTimer, NextSoundPeriod);
		}
		else
		{
			pSoundingInfo->State = SOUNDING_STATE_NONE;
			ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_LOUD, ("[%s]: Set to SOUNDING_STATE_NONE \n", __func__));
		}
	}
}

VOID
Beamform_SoundingDown(
	struct rtl8192cd_priv *priv,
	IN BOOLEAN		Status	
	)
{	

	PRT_BEAMFORMING_INFO 		pBeamInfo = &(priv->pshare->BeamformingInfo);
	PRT_SOUNDING_INFOV2		pSoundingInfo = &(pBeamInfo->SoundingInfoV2);
	PRT_BEAMFORMING_ENTRY		pEntry = NULL;

#if 0 //eric-mu
	if(Status == 0)
		return;
	
	if(pSoundingInfo->State == SOUNDING_STATE_NONE)
		return;
#endif

	if(priv->pshare->rf_ft_var.debug8822 == 7)
	ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_TRACE, ("%s(): Status=%d, pSoundingInfo->State=%d\n", __func__, Status, pSoundingInfo->State));

	if (pSoundingInfo->State == SOUNDING_STATE_MU_START)
	{
		//ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_TRACE, ("%s: MU sounding done\n", __func__));
		//pBeamInfo->is_mu_sounding_in_progress = FALSE;
		pSoundingInfo->State = SOUNDING_STATE_MU_SOUNDDOWN;
		//ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_TRACE, ("[%s]: Set to SOUNDING_STATE_MU_SOUNDDOWN \n", __func__));
		pBeamInfo->SetHalSoundownOnDemandCnt++;
		//Beamform_SetHwConfig(pAdapter, BEAMFORM_SET_HW_TYPE_SOUND_DOWN);	
		Beamforming_SetBeamFormStatus(priv, 0);
		//HalComTxbf_Set(pDM_Odm, TXBF_SET_SOUNDING_STATUS, (pu1Byte)&(pBeamInfo->BeamformeeCurIdx));
	} 
	else if(pSoundingInfo->State == SOUNDING_STATE_SU_START)
	{
		ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_TRACE, ("%s(): SU entry[%d] sound down\n", __func__, pSoundingInfo->SUBFeeCurIdx));

		pEntry = &(pBeamInfo->BeamformeeEntry[pSoundingInfo->SUBFeeCurIdx]);
		pSoundingInfo->State = SOUNDING_STATE_SU_SOUNDDOWN;
		//
		// <tynli_note> pEntry->bSoundingTimeout this flag still cannot avoid old sound down event happens in the new sounding period. 2015.12.10
		//
		if (pEntry->bSoundingTimeout) {
			ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_TRACE, ("[%s] WARNING! The entry[%d] is bSoundingTimeout!\n", __func__, pSoundingInfo->SUBFeeCurIdx));
			pEntry->bSoundingTimeout = FALSE;
			return;
		}

		if (0) //<tynli_mark_review> ((pDM_Odm->TxBfDataRate >= ODM_RATEVHTSS3MCS7) && (pDM_Odm->TxBfDataRate <= ODM_RATEVHTSS3MCS9)) 
		{
			ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_TRACE, ("[%s] VHT3SS 7,8,9, do not apply V matrix.\n", __func__));
			//pEntry->BeamformEntryState = BEAMFORMING_ENTRY_STATE_INITIALIZED;
			//HalComTxbf_Set(pDM_Odm, TXBF_SET_SOUNDING_STATUS, (pu1Byte)&(pBeamInfo->BeamformeeCurIdx));
			pEntry->bDeleteSounding = TRUE;
			pBeamInfo->SetHalSoundownOnDemandCnt++;
			//Beamform_SetHwConfig(pAdapter, BEAMFORM_SET_HW_TYPE_SOUND_DOWN); 	
		}
		else if (Status == 1)	// success
		{
			pEntry->LogStatusFailCnt = 0;
			pEntry->BeamformEntryState = BEAMFORMING_ENTRY_STATE_PROGRESSED; //eric-txbf
			//HalComTxbf_Set(pDM_Odm, TXBF_SET_SOUNDING_STATUS, (pu1Byte)&(pBeamInfo->BeamformeeCurIdx));
			pEntry->bDeleteSounding = FALSE;
			pBeamInfo->SetHalSoundownOnDemandCnt++;
			//Beamform_SetHwConfig(pAdapter, BEAMFORM_SET_HW_TYPE_SOUND_DOWN); 
			Beamforming_SetBeamFormStatus(priv, pSoundingInfo->SUBFeeCurIdx);
		}
		else
		{
			/*pEntry->LogStatusFailCnt++;*/
			pEntry->BeamformEntryState = BEAMFORMING_ENTRY_STATE_INITIALIZED;
			Beamforming_SetBeamFormStatus(priv, pSoundingInfo->SUBFeeCurIdx);
			//HalComTxbf_Set(pDM_Odm, TXBF_SET_TX_PATH_RESET, (pu1Byte)&(pBeamInfo->BeamformeeCurIdx));
			ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_TRACE, ("[%s] LogStatusFailCnt %d\n", __func__, pEntry->LogStatusFailCnt));
		}
		
		/*if (pEntry->LogStatusFailCnt > 50) {*/
		if (0) {
			ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_TRACE, ("%s LogStatusFailCnt > 50, Stop SOUNDING\n", __func__));
			//beamform_DeleteBeamformEntry(pAdapter, pEntry->MacAddr);

			/*Modified by David - Every action of deleting entry should follow by Notify*/
			//phydm_Beamforming_Notify(pDM_Odm);

			pBeamInfo->CurDelBFerBFeeEntrySel = BFeeEntry;
		        if(Beamforming_DeInitEntry(priv, pEntry->MacAddr))
					Beamforming_Notify(priv);
		}
		//pEntry->bBeamformingInProgress = FALSE;		
		ODM_RT_TRACE(ODMPTR, PHYDM_COMP_TXBF, ODM_DBG_TRACE, ("[%s]: Set to SOUNDING_STATE_SU_SOUNDDOWN \n", __func__));
	}
}	

#endif //SUPPORT_MU_BF == 1

#endif

