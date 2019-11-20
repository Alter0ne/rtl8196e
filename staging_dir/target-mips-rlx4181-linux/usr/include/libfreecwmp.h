/*
 *	This program is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	Copyright (C) 2011-2012 Luka Perkov <freecwmp@lukaperkov.net>
 */

#ifndef _LIBFREECWMP_H__
#define _LIBFREECWMP_H__

#include <stdbool.h>
#include <stdlib.h>

/*
 * various defines used in projects
 */
#define ARRAY_SIZE(x) (sizeof(x) / sizeof(x[0]))
#define ARRAY_AND_SIZE(x) (x), ARRAY_SIZE(x)

#define NEWLINE "\r\n"

/*
 * CWMP messages
 */
#define XML_PROLOG \
	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>" NEWLINE

#define CWMP_VERSION_1_0 \
	"urn:dslforum-org:cwmp-1-0"

#define CWMP_VERSION_1_1 \
	"urn:dslforum-org:cwmp-1-1"

#define CWMP_VERSION_1_2 \
	"urn:dslforum-org:cwmp-1-2"

/* by default use CWMP version 1.0 */
#define XML_SOAP_ENVELOPE_HEAD \
	"<soap_env:Envelope"						\
	" xmlns:soap_env=\"http://schemas.xmlsoap.org/soap/envelope/\""	\
	" xmlns:soap_enc=\"http://schemas.xmlsoap.org/soap/encoding/\""	\
	" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\""		\
	" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""	\
	" xmlns:cwmp=\"" CWMP_VERSION_1_0 "\""				\
	">"

#define XML_SOAP_ENVELOPE_TAIL \
	"</soap_env:Envelope>"

#define XML_CWMP_HEAD \
	"<soap_env:Header />"

#define XML_CWMP_BODY_HEAD \
	"<soap_env:Body>"

#define XML_CWMP_BODY_TAIL \
	"</soap_env:Body>"

#define XML_CWMP_INFORM_RESPONSE \
	"<cwmp:InformResponse>"			\
	"<MaxEnvelopes>1</MaxEnvelopes>"	\
	"</cwmp:InformResponse>"

#define XML_CWMP_CMD_REBOOT \
	"<cwmp:Reboot>"			\
	"<CommandKey></CommandKey>"	\
	"</cwmp:Reboot>"

#define XML_CWMP_FAULT \
	"<cwmp:Fault>"				\
	"<FaultCode></FaultCode>"		\
	"<FaultString></FaultString>"		\
	"</cwmp:Fault>"

#define XML_CWMP_GENERIC_HEAD \
	XML_SOAP_ENVELOPE_HEAD	\
	XML_CWMP_HEAD		\
	XML_CWMP_BODY_HEAD

#define XML_CWMP_GENERIC_TAIL \
	XML_CWMP_BODY_TAIL	\
	XML_SOAP_ENVELOPE_TAIL	\
	NEWLINE

/*
 * various data types used in projects
 */
typedef unsigned char u_char;

typedef struct {
	size_t len;
	u_char *data;
} cwmp_str_t;

enum {
	L_CRIT,
	L_WARNING,
	L_NOTICE,
	L_INFO,
	L_DEBUG
};

enum {
	BOOTSTRAP = 0,
	BOOT,
	PERIODIC,
	SCHEDULED,
	VALUE_CHANGE,
	KICKED,
	CONNECTION_REQUEST,
	TRANSFER_COMPLETE,
	DIAGNOSTICS_COMPLETE,
	REQUEST_DOWNLOAD,
	AUTONOMOUS_TRANSFER_COMPLETE
};

enum {
	GET_PARAMETER_VALUE = 0,
	SET_PARAMETER_VALUE,
	GET_PARAMETER_NOTIFICATION,
	SET_PARAMETER_NOTIFICATION
};

/*
 * exposed functions
 */
void lfc_log_message(char *name, int priority, const char *format, ...);

char * lfc_str_action(int code);

char * lfc_str_event_code(int code, bool human);
int lfc_int_event_code(char *code);

char * lfc_get_current_time(char *format);
int lfc_get_remaining_time(char *format, char *time);

#endif
