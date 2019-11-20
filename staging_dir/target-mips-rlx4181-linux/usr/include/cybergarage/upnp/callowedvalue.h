/******************************************************************
*
*	CyberLink for C
*
*	Copyright (C) Satoshi Konno 2005
*
*	File: callowedvalue.h
*
*	Revision:
*
*	02/23/05
*		- first revision
*
******************************************************************/

#ifndef _CG_UPNP_CALLOWEDVALUE_H_
#define _CG_UPNP_CALLOWEDVALUE_H_

#include <cybergarage/typedef.h>
#include <cybergarage/xml/cxml.h>
#include <cybergarage/util/clist.h>

#ifdef  __cplusplus
extern "C" {
#endif

/****************************************
* Define
****************************************/

#define CG_UPNP_ALLOWEDVALUE_ELEM_NAME "allowedValue"
#define CG_UPNP_ALLOWEDVALUELIST_ELEM_NAME "allowedValueList"

#define CG_UPNP_ALLOWEDVALUERANGE_ELEM_NAME "allowedValueRange"
#define CG_UPNP_ALLOWEDVALUERANGE_MAXIMUM "maximum"
#define CG_UPNP_ALLOWEDVALUERANGE_MINIMUM "minimum"
#define CG_UPNP_ALLOWEDVALUERANGE_STEP "step"

/****************************************
* Data Type
****************************************/

typedef struct _CgUpnpAllowedValue {
	BOOL headFlag;
	struct _CgUpnpAllowedValue *prev;
	struct _CgUpnpAllowedValue *next;
	CgXmlNode *allowedValueNode;
} CgUpnpAllowedValue, CgUpnpAllowedValueList;

typedef struct _CgUpnpAllowedValueRange {
	CgXmlNode *allowedValueRangeNode;
} CgUpnpAllowedValueRange;

/****************************************
* Function (AllowedValue)
****************************************/

CgUpnpAllowedValue *cg_upnp_allowedvalue_new();
void cg_upnp_allowedvalue_delete(CgUpnpAllowedValue *allowedvalue);

#define cg_upnp_allowedvalue_next(allowedVal) (CgUpnpAllowedValue *)cg_list_next((CgList *)allowedVal)

#define cg_upnp_allowedvalue_isallowedvaluenode(node) cg_xml_node_isname(node, CG_UPNP_ALLOWEDVALUE_ELEM_NAME)

#define cg_upnp_allowedvalue_setallowedvaluenode(allowedVal,node) (allowedVal->allowedValueNode = node)
#define cg_upnp_allowedvalue_getallowedvaluenode(allowedVal) (allowedVal->allowedValueNode)

/**** value ****/
#define cg_upnp_allowedvalue_setvalue(allowedVal, value) cg_xml_node_setvalue(cg_upnp_allowedvalue_getallowedvaluenode(allowedVal), value)
#define cg_upnp_allowedvalue_getvalue(allowedVal) cg_xml_node_getvalue(cg_upnp_allowedvalue_getallowedvaluenode(allowedVal))

/****************************************
* Function (AllowedValueList)
****************************************/

CgUpnpAllowedValueList *cg_upnp_allowedvaluelist_new();
void cg_upnp_allowedvaluelist_delete(CgUpnpAllowedValueList *allowedvalueList);

#define cg_upnp_allowedvaluelist_clear(allowedvalueList) cg_list_clear((CgList *)allowedvalueList, (CG_LIST_DESTRUCTORFUNC)cg_upnp_allowedvalue_delete)
#define cg_upnp_allowedvaluelist_size(allowedvalueList) cg_list_size((CgList *)allowedvalueList)
#define cg_upnp_allowedvaluelist_gets(allowedvalueList) (CgUpnpAllowedValue *)cg_list_next((CgList *)allowedvalueList)
#define cg_upnp_allowedvaluelist_add(allowedvalueList, allowedvalue) cg_list_add((CgList *)allowedvalueList, (CgList *)allowedvalue)

/****************************************
* Function (AllowedValueRange)
****************************************/

CgUpnpAllowedValueRange *cg_upnp_allowedvaluerange_new();
void cg_upnp_allowedvaluerange_delete(CgUpnpAllowedValueRange *allowedvaluerange);

#define cg_upnp_allowedvaluerange_next(allowedvaluerange) (CgUpnpAllowedValueRange *)cg_list_next((CgList *)allowedvaluerange)

#define cg_upnp_allowedvaluerange_isallowedvaluerangenode(node) cg_xml_node_isname(node, CG_UPNP_ALLOWEDVALUERANGE_ELEM_NAME)

#define cg_upnp_allowedvaluerange_setallowedvaluerangenode(allowedvaluerange,node) (allowedvaluerange->allowedValueRangeNode = node)
#define cg_upnp_allowedvaluerange_getallowedvaluerangenode(allowedvaluerange) (allowedvaluerange->allowedValueRangeNode)

/****  maximum ****/
#define cg_upnp_allowedvaluerange_setmaximum(allowedVal, value) cg_xml_node_setchildnode(cg_upnp_allowedvaluerange_getallowedvaluerangenode(allowedVal), CG_UPNP_ALLOWEDVALUERANGE_MAXIMUM, value)
#define cg_upnp_allowedvaluerange_getmaximum(allowedVal) cg_xml_node_getchildnodevalue(cg_upnp_allowedvaluerange_getallowedvaluerangenode(allowedVal), CG_UPNP_ALLOWEDVALUERANGE_MAXIMUM)

/**** minimum ****/
#define cg_upnp_allowedvaluerange_setminimum(allowedVal, value) cg_xml_node_setchildnode(cg_upnp_allowedvaluerange_getallowedvaluerangenode(allowedVal), CG_UPNP_ALLOWEDVALUERANGE_MINIMUM, value)
#define cg_upnp_allowedvaluerange_getminimum(allowedVal) cg_xml_node_getchildnodevalue(cg_upnp_allowedvaluerange_getallowedvaluerangenode(allowedVal), CG_UPNP_ALLOWEDVALUERANGE_MINIMUM)

/**** step ****/
#define cg_upnp_allowedvaluerange_setstep(allowedVal, value) cg_xml_node_setchildnode(cg_upnp_allowedvaluerange_getallowedvaluerangenode(allowedVal), CG_UPNP_ALLOWEDVALUERANGE_STEP, value)
#define cg_upnp_allowedvaluerange_getstep(allowedVal) cg_xml_node_getchildnodevalue(cg_upnp_allowedvaluerange_getallowedvaluerangenode(allowedVal), CG_UPNP_ALLOWEDVALUERANGE_STEP)

#ifdef  __cplusplus
}
#endif

#endif
