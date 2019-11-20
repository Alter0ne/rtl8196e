/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * Tooltips for various fields across different pages in the gui
 *
 * Copyright (C) 2006-2010, Digium, Inc.
 *
 * Ryan Brindley <rbrindley@digium.com>
 * Erin Spiceland <espiceland@digium.com>
 *
 * See http://www.asterisk.org for more information about
 * the Asterisk project. Please do not directly contact
 * any of the maintainers of this project for assistance;
 * the project provides a web site, mailing lists and IRC
 * channels for your use.
 *
 * This program is free software, distributed under the terms of
 * the GNU General Public License Version 2. See the LICENSE file
 * at the top of the source tree.
 *
 */

var tooltips = new Object;
//	Tooltips for users.html in english
	tooltips['users']= {};
	tooltips['users'] .en = [];

	tooltips['users'] .en[0] = "<B>Extension:</B> The numbered extension, i.e. 1234, that will be associated with this particular User / Phone." ;  //Extension
	tooltips['users'] .en[1] = "<B>Name:</B> A character-based name for this user, i.e. \"Bob Jones\" " ;  //Name
	tooltips['users'] .en[2] = "<B>Password:</B> The password for the user's sip/iax account , Ex: \"12u3b6\" " ; //Password
	tooltips['users'] .en[3] = "<B>E-Mail:</B> The e-mail address for this user, i.e. \"bobjones@bobjones.null\"";  //E-mail
	tooltips['users'] .en[4] = "<B>Caller ID:</B> The Caller ID (CID) string used when this user calls another internal user."; //Caller ID
	tooltips['users'] .en[5] = "<B>Analog Phone:</B> If this user is attached to an analog port on the system, please choose the port number here.";  //Analog Phone

	tooltips['users'] .en[6] = "<B>Dial Plan:</B> Please choose the DialPlan for this user.  DialPlans are sets of calling rules and can be managed form the \"Dial Plans\" panel.";

	tooltips['users'] .en[7] = ""; 

	tooltips['users'] .en[8] = "<B>Voicemail:</B> Check this box if the user should have a voicemail account."; 

	tooltips['users'] .en[9] = "<B>In Directory:</B> Check this option if the user is to be listed in the system telephone directory."; 

	tooltips['users'] .en[10] = "<B>Session Initiation Protocol</B> Check this option if the User or Phone is using SIP or is a SIP device.";

	tooltips['users'] .en[11] = "<B>InterAsterisk eXchange Protocol:</B> Check this option if the User or Phone is using IAX or is an IAX device.";

	tooltips['users'] .en[12] = "<B>ADA/Manager:</B> Check this option if this user is to be permitted Asterisk Manager Interface (AMI) interface access for AMI client applications, or use the Asterisk Desktop Assistant.";

	tooltips['users'] .en[13] = "<B>Call Waiting:</B> Check this option if the User or Phone should have Call-Waiting capability.";

	tooltips['users'] .en[14] = "<B>3-Way Calling:</B>Check this option if the User or Phone should have 3-Way Calling capability.";

	tooltips['users'] .en[15] = "<B>Is Agent:</B> Check this option if this User or Phone is a Call Queue Member (Agent)";

	tooltips['users'] .en[16] = "<B>VM Password:</B> Voicemail Password for this user, Ex: \"1234\".";
	
tooltips['users'] .en[17] = "<B>Hints:</B> If checked the phone being called will be sent a \"hint\" to light up status lamps on SIP phones.";

	tooltips['users'] .en[21] = "<B>Can Reinvite:</B> By default, Asterisk will route the media steams from SIP endpoints through itself.  Enabling this option causes asterisk to attempt to negotiate the endpoints to route the media stream directly, bypassing asterisk.  It is not always possible for asterisk to negotiate endpoint-to-endpoint media routing.";

	tooltips['users'] .en[22] = "<B>NAT:</B> Try this setting when Asterisk is on a public IP, communicating with devices hidden behind a NAT device (broadband router). If you have one-way audio problems, you usually have problems with your NAT configuration or your firewall's support of SIP+RTP ports.";

	tooltips['users'] .en[23] = "<B>DTMFMode:</B> Set default dtmfmode for sending DTMF. Default: rfc2833 <BR><B>Other options:</B><BR>info : SIP INFO messages<BR>inband : Inband audio (requires 64 kbit codec -alaw, ulaw)<BR>auto : Use rfc2833 if offered, inband otherwise";

	tooltips['users'] .en[26] = "Insecure: <B>Port</B> allows matching of peers by IP address without matching port number. <B>Invite</B> removes the requirement for authentication of incoming INVITE messages. <B>Port,Invite</B> allows both the matching of peers by IP address without matching port number and removes the requirement for authentication of incoming INVITE messages. <B>No</B> requires normal IP-based matching and authenticated INVITES.";

	tooltips['users'] .en[27] = "<B>OutBound caller ID:</B> Caller ID that would be applied for out bound calls from this user. Note that your ability to manipulate your outbound Caller ID may be limited by your VoIP provider." ;

	tooltips['users'] .en[30] = "<B>Flash</B> sets the amount of time, in milliseconds, that must have passed since the last hook-flash event received by asterisk before it will recognize a second event.  If a second event occurs in less time than defined for Flash, then asterisk will ignore the event.  The default value of Flash is 750 ms, and it can be configured in 1ms increments." ;

	tooltips['users'] .en[31] = "<B>RXFlash</B> sets the amount of time, in milliseconds, that the hook-flash must remain depressed in order for asterisk to consider such an event a valid flash event.  The default value of RXFlash is 1250ms and it can be configured in 1ms increments." ;
	tooltips['users'] .en[32] = "<B>Require Call Token:</B> Enable or disable requiring call token.  Default is 'yes'. Using 'auto' may lock out users who depend on backward compatibility when peer authentication credentials are shared between physical endpoints.";
	tooltips['users'] .en[33] = "<B>MWI From:</B> When sending MWI NOTIFY requests, use this in the From: header as the \"name\" portion.  Also fill the \"user\" portion of the URI in the From: header with this value if no fromuser is set.";

	tooltips['users'] .en[94] = "Line Keys is the number of lines that are tied to this SIP registration.";

	tooltips['users'] .en[95] = "<B>Codecs</B> A codec is a compression or decompression algorithm run against voice as it is moved between analog (speaking) and digital (VoIP). <B>u-law</B> A PSTN standard codec, used in North America, that provides very good voice quality and consumes 64kbit/s for each direction (receiving and transmitting) of a VoIP call.  u-law should be supported by all VoIP phones. <B>a-law</B> A PSTN standard codec, used outside of North America, that provides very good voice quality and consumes 64kbit/s for each direction (receiving and transmitting) of a VoIP call.  a-law should be supported by all VoIP phones. <B>GSM</B> A wireless standard codec, used worldwide, that provides okay voice quality and consumes 13.3kbit/s for each direction (receiving and transmitting) of a VoIP call.  GSM is supported by many VoIP phones. <B>G.726</B> A PSTN codec, used worldwide, that provides good voice quality and consumes 32kbit/s for each direction (receiving and transmitting) of a VoIP call. G.726 is supported by some VoIP phones. <B>G.722</B> A high-fidelity codec for VoIP calls that provides excellent voice quality and consumes 64kbit/s for each direction (receiving and transmitting) of a VoIP call. At present, G.722 is only supported by a limited number of VoIP phones.";

	tooltips['users'] .en[96] = "Line Number - Polycom-brand VoIP phones are capable of servicing between 1 and 6 separate VoIP lines, depending on the model of the phone. If you have elected to use the Polycom Autoprovisioning feature of Asterisk 1.6, this option may be used to define which line of your phone is configured for the user / extension that is being configured.  One may, for example, define extension 6001 as the first line of a phone, and extension 6002 as the second line; thus, line one would ring when someone calls 6001, and line two would ring when 6002 is called.  More than one user/extension may not be assigned to the same line on the same telephone.";

	tooltips['users'] .en[97] = "Polycom Autoprovisioning ";

	tooltips['users'] .en[98] = "Need a phone, some software, or something else ? Click on the 'Where to Buy' button to get directly to products." ;

	tooltips['users'] .en[99] = "<B>MAC Address/Phone Serial:</B> Enter the serial number of a Polycom phone to enable phone provisioning. While entering the MAC address, ignore any ':' or '-' " ;



// Tooltips for Conferencing (meetme)
	tooltips['meetme']= {};
	tooltips['meetme'] .en = [];

	tooltips['meetme'] .en[0] = "<B>Extension:</B> This is the number dialed to reach this Conference Bridge.";  //Extension:
	tooltips['meetme'] .en[10] = "<B>Marked/Admin user Extension:</B> If the conference bridge is to have marked users or admin users, then those users should enter the conference bridge using a separate extension.  Admin conference users can lock and unlock the conference and can kick the most recent conference participant.  Marked users are special users whose entrance and exit, if the <i>Wait for Marked user</i> or <i>Close conference when last marked user exits</i> can either begin or end the conference altogether.";
	tooltips['meetme'] .en[1] = "<B>Pin Code:</B> set an optional pin code, Ex: \"1234\" that must be entered in order to access the Conference Bridge."; // Personal Identification Number
	tooltips['meetme'] .en[2] = "<B>Administrator PIN Code:</B> Defining this option sets a PIN for Conference Administrators."; // Administrator PIN Code
	tooltips['meetme'] .en[3] = "<B>Play Hold Music for First Caller:</B> Checking this option causes Asterisk to play Hold Music to the first user in a conference, until another user has joined the same conference.";
	tooltips['meetme'] .en[4] = "<B>Enable Caller Menu:</B> Checking this option allows a user to access the Conference Bridge menu by pressing the * \"Asterisk\" key on their dialpad.";
	tooltips['meetme'] .en[5] = "<B>Announce Callers:</B> Checking this option announces, to all Bridge participants, the joining of any other participants.";
	tooltips['meetme'] .en[6] = "<B>Advanced:</B> Show/Hide Advanced Conference Bridge configuration options.";
	tooltips['meetme'] .en[7] = "<B>Room Override:</B> This option allows the entry of a secondary extension that may be used to access this Conference Bridge. This is useful in the event that one wants to set a separate extension, having different options, to access the same Bridge.";
	tooltips['meetme'] .en[8] = "<B>Quiet Mode:</B> Do not play enter/leave sounds";
	tooltips['meetme'] .en[9] = "<B>Wait for Marked User:</B> Prevent conference participants from hearing each other until the marked user has joined.";

	tooltips['meetme'] .en[11] = "<B>Record Conference:</B> Record this conference in a WAV format. Default filename is meetme-conf-rec-${Conference Number}-${UNIQUEID}.";

	tooltips['meetme'] .en[12] = "Close the conference bridge when the last marked user logs out of the conference call";

// Tooltips for Voicemail
	tooltips['voicemail']= new Object;
	tooltips['voicemail'] .en = new Array;

	tooltips['voicemail'] .en[0] = "<B>Extension for checking Message:</B>This option, i.e. \"2345,\" defines the extension that Users call in order to access their voicemail accounts.";
	tooltips['voicemail'] .en[1] = "<B>Attach recording to e-mail:</B> This option defines whether or not voicemails are sent to the Users' e-mail addresses as attachments. Note: You need to have an smtp server configured for this functionality.";
	tooltips['voicemail'] .en[2] = "<B>Say Message Caller-ID:</B> If this option is enabled, the Caller ID of the party that left the message will be played back before the voicemail message begins playing.";
	tooltips['voicemail'] .en[3] = "<B>Say Message Duration (in minutes) :</B> If this option is set, the duration of the message in mintues will be played back before the voicemail message begins playing.";
	tooltips['voicemail'] .en[4] = "<B>Send messages by e-mail only:</B> If this option is set, then voicemails will not be checkable using a Phone.  Messages will be sent via e-mail, only.  Note: You need to have an smtp server configured for this functionality.";
	tooltips['voicemail'] .en[5] = "<B>Maximum messages per folder:</B> This select box sets the maximum number of messages that a user may have in any of their folders.";
	tooltips['voicemail'] .en[6] = "<B>Maximum Message Time:</B> This select box sets the maximum duration of a voicemail message in seconds. Message recording will not occur for times greater than this amount.";
	tooltips['voicemail'] .en[7] = "<B>Minimum message Time:</B> This select box sets the minimum duration of a voicemail message in seconds. Messages below this threshold will be automatically deleted.";
	tooltips['voicemail'] .en[8] = "<B>Advanced:</B> Show/Hide Advanced Voicemail configuration options.";
	tooltips['voicemail'] .en[9] = "<B>Dail 'O' for Operator:</B> Enable Callers to exit the voicemail application and connect to an operator extension. The operator extension must be defined from the 'Options' panel.";
	tooltips['voicemail'] .en[10] = "<B>Attach Format:</B> This selection box controls the format in which messages are delivered by e-mail.";
	tooltips['voicemail'] .en[11] = "<B>Allow callers to Review:</B> Checking this option allows the caller to review their message before it is submitted as a new voicemail message.";
	tooltips['voicemail'] .en[12] = "<B>Play Envelope:</B> Turn on/off playing introductions about each message when accessing them from the voicemail application.";
	tooltips['voicemail'] .en[13] = "<B> Max Greeting:</B> Set the maximum number of seconds for a User's voicemail greeting.";
	tooltips['voicemail'] .en[15] = "<B> Direct VoiceMail Dial:</B> Check this to enable direct voicemail dial. For instance, if John's extension is 6001, you would be able to directly dial into John's voicemailbox by dialing #6001 to leave him a message.";
	tooltips['voicemail'] .en[16] = "<b>SMTP Server</b> The IP address or hostname of an SMTP server that this may connect to, without authentication, in order to send e-mail notifications of your voicemails; i.e. mail.yourcompany.com";
	tooltips['voicemail'] .en[17] = "<b>Port</b> The port number on which the SMTP server is running; generally port 25.";


// Tooltips for CallQueues (queues)
	tooltips['queues']= new Object;
	tooltips['queues'].en = new Array;

	tooltips['queues'].en[0] = "<B>Queue Extension:</B> This option defines the numbered extension that may be dialed to reach this Queue.";
	tooltips['queues'].en[1] = "<B>Name:</B> This option defines a name for this Queue, i.e. \"Sales\". 'Name' is a label to help you see this queue in the queue list.";
	tooltips['queues'].en[2] = "<B>Strategy:</B> This option sets the Ringing Strategy for this Queue.  The options are<OL><LI>RingAll: Ring All available Agents simultaneously until one answers. <LI>RoundRobin: Take turns ringing each available Agent <LI>LeastRecent: Ring the Agent which was least recently called <LI>FewestCalls: Ring the Agent with the fewest completed calls <LI>Random: Ring a Random Agent <LI>RRmemory: RoundRobin with Memory, Remembers where it left off in the last ring pass</OL>";
	tooltips['queues'].en[3] = "<B>Agents:</B> This selection shows all Users defined as Agents in their User conf.  Checking a User here makes them a member of the current Queue.";
	tooltips['queues'].en[4] = "<B>Advanced:</B> Advanced Queue Configuration Options";
	tooltips['queues'].en[5] = "<B>Timeout:</B> How many seconds an Agent's phone will ring before the Queue tries to ring the next Agent.";
	tooltips['queues'].en[6] = "<B>Wrapup Time:</B> How many seconds after the completion of a call an Agent will have before the Queue can ring them with a new call. The default is 0, which is no delay.";
	tooltips['queues'].en[7] = "<B>AutoFill</B> Defining this option causes the Queue, when multiple calls are in it at the same time, to push them to Agents simultaneously.  Thus, instead of completing one call to an Agent at a time, the Queue will complete as many calls simultaneously to the available Agents.";
	tooltips['queues'].en[8] = "<B> AutoPause:</B>Enabling this option pauses an agent if they fail to answer a call.  This means that the agent is still logged into the queue, but they will not receive calls from the queue.  Once paused, an agent can unpause by logging into the queue using the regular agent login extension.";
	tooltips['queues'].en[9] = "<B> MaxLen:</B> How many calls can be queued at once. This count does not include calls that have been connected with Agents, it only includes calls that have not yet been connected. Default is 0, which is no limit. When the limit has been reached, a caller will hear a busy tone and advance to the next calling rule after attempting to enter the queue.";
	tooltips['queues'].en[10] = "<B>Join Empty:</B> This option controls whether callers can join a call queue that has no agents. There are three options, <LI>Yes: Callers can join a call queue with no agents or only unavailable agents <li>No: Callers cannot join a queue with no agents <li>Strict: Callers cannot join a queue with no agents or if all agents are unavailable. <BR> The default option is No. ";
	tooltips['queues'].en[11] = "<B>LeaveWhenEmpty:</B>This option controls whether callers already on hold are forced out of a queue that has no agents. There are three options.<LI>Yes: Callers are forced out of a queue when no agents are logged in.<LI>No: Callers will remain in a queue with no agents.<LI>Strict: Callers are forced out of a queue with no agents logged in, or if all logged in agents are unavailable.<BR>The default option is Strict.<BR/><BR/>After a caller has left the queue, a caller will hear a busy tone and advance to the next calling rule after attempting to enter the queue.";
	tooltips['queues'].en[12] = "<B> Report Hold Time:</B> Enabling this option causes Asterisk to report, to the Agent, the hold time of the caller before the caller is connected to the Agent.";
	tooltips['queues'].en[13] = "<B> Music On Hold:</B> Select the 'Music on Hold' Class for this Queue. 'Music on Hold' classes can be managed from the the 'Music On Hold' panel on the left.";
	tooltips['queues'] .en[14] = "<B>Agent Login Extension:</B> Extension to be dialed for the Agents to Login to the Specific Queue. <br> This is an extension that all the Agents can Call to Login to their specified Queues. ";
	tooltips['queues'] .en[15] = "<B>Agent Callback Login Extension:</B> Extension to be dialed for the Agents to Login to the Queues they are apart of.<br> Same as Agent Login Extension, except you do not have to remain on the line. ";
	tooltips['queues'] .en[16] = "<B>Agent Logout</B>";
	tooltips['queues'] .en[17] = "<B>KeyPress Events:</B> If a caller presses a key while waiting in the queue, this setting selects which voice menu should process the key press.";

// Tooltips for SIP_General (sip_general)
	tooltips['sip_general']= new Object;
	tooltips['sip_general'].en = new Array;

	tooltips['sip_general'].en[0] = "<B>Context:</B> Default context for incoming calls";
	tooltips['sip_general'].en[1] = "<B>Realm for digest authentication:</B> Realm for digest authentication.defaults to \'asterisk\'. If you set a system name in asterisk.conf, it defaults to that system name. Realms MUST be globally unique according to RFC 3261. Set this to your host name or domain name";
	tooltips['sip_general'].en[2] = "<B>UDP Port to bind to:</B> SIP standard port is 5060";
	tooltips['sip_general'].en[3] = "<B>IP address to bind to:</B> 0.0.0.0 binds to all";
	tooltips['sip_general'].en[4] = "<B>Domain:</B> Comma separated list of domains which Asterisk is responsible for";
	tooltips['sip_general'].en[5] = "<B>Allow guest calls:</B> Enable guest calls.";
	tooltips['sip_general'].en[6] = "<B>Overlap dialing support:</B> Enable dialing support";
	tooltips['sip_general'].en[7] = "<B>Allow Transfers:</B> Enable Transfers";
	tooltips['sip_general'].en[8] = "<B>Enable DNS SRV lookups (on outbound calls): </B> Enable DNS SRV lookups on calls";
	tooltips['sip_general'].en[9] = "<B>Pedantic:</B> Enable slow, pedantic checking of Call-ID:s, multiline SIP headers and URI-encoded headers";
	tooltips['sip_general'].en[10] = "<B> Type of Service:</B> ";
	tooltips['sip_general'].en[11] = "<B> TOS for Signalling packets:</B> Sets Type of Service for SIP packets";
	tooltips['sip_general'].en[12] = "<B>TOS for RTP audio packets:</B> Sets Type of Service for RTP audio packets";
	tooltips['sip_general'].en[13] = "<B>TOS for RTP video packets:</B> Sets Type of Service for RTP video packets";
	tooltips['sip_general'].en[14] = "<B> Max Registration/Subscription Time:</B> Maximum duration (in seconds) of incoming registration/subscriptions we allow. Default 3600 seconds.";
	tooltips['sip_general'].en[15] = "<B> Min Registration/Subscription Time:</B> Minimum duration (in seconds) of registrations/subscriptions.  Default 60 seconds";
	tooltips['sip_general'].en[16] = "<B> Default Incoming/Outgoing Registration Time:</B>  Default duration (in seconds)  of incoming/outoing registration";
	tooltips['sip_general'].en[17] = "<B> Min RoundtripTime (T1 Time):</B>  Minimum roundtrip time for messages to monitored hosts,  Defaults to 100 ms";
	tooltips['sip_general'].en[18] = "<B> Override Notify MIME Type:</B> Allow overriding of mime type in MWI NOTIFY";
	tooltips['sip_general'].en[19] = "<B> Time between MWI Checks: </B> Default Time between Mailbox checks for peers";
	tooltips['sip_general'].en[20] = "<B> Music On Hold Interpret:</B> This option specifies a preference for which music on hold class this channel should listen to when put on hold if the music class has not been set on the channel with Set(CHANNEL(musicclass)=whatever) in the dialplan, and the peer channel putting this one on hold did not suggest a music class";
	tooltips['sip_general'].en[21] = "<B> Music On Hold Suggest:</B> This option specifies which music on hold class to suggest to the peer channel when this channel places the peer on hold. It may be specified globally or on a per-user or per-peer basis.";
	tooltips['sip_general'].en[22] = "<B> Language:</B> Default language setting for all users/peers";
	tooltips['sip_general'].en[23] = "<B> Enable Relaxed DTMF:</B> Relax dtmf handling";
	tooltips['sip_general'].en[24] = "<B> RTP TimeOut:</B> Terminate call if 60 seconds of no RTP activity when we're not on hold ";
	tooltips['sip_general'].en[25] = "<B>RTP HoldTimeOut:</B> Terminate call if 300 seconds of no RTP activity when we're on hold (must be > rtptimeout)";
	tooltips['sip_general'].en[26] = "<B>Trust Remote Party ID:</B> If Remote-Party-ID should be trusted";
	tooltips['sip_general'].en[27] = "<B>Send Remote Party ID:</B>If Remote-Party-ID should be sent";
	tooltips['sip_general'].en[28] = "<B>Generate In-Band Ringing:</B> If we should generate in-band ringing always use \'never\' to never use in-band signalling, even in cases where some buggy devices might not render it. Default: never";
	tooltips['sip_general'].en[29] = "<B>Server UserAgent:</B> Allows you to change the user agent string";
	tooltips['sip_general'].en[30] = "<B>Allow Nonlocal Redirect:</B>If checked, allows 302 or REDIR to non-local SIP address Note that promiscredir when redirects are made to the local system will cause loops since Asterisk is incapable of performing a \'hairpin\' call";
	tooltips['sip_general'].en[31] = "<B>Add 'user=phone' to URI:</B> If checked, \'user=phone\' is added to uri that contains a valid phone number";
	tooltips['sip_general'].en[32] = "<B>DTMF Mode:</B> Set default dtmfmode for sending DTMF. Default: rfc2833H";
	tooltips['sip_general'].en[33] = "<B>Send Compact SIP Headers:</B>  send compact sip headers";
	tooltips['sip_general'].en[34] = "<B> SIP Video Related:</B>";
	tooltips['sip_general'].en[35] = "<B>Max Bitrate (kb/s):</B>Maximum bitrate for video calls (default 384 kb/s)";
	tooltips['sip_general'].en[36] = "<B>Support for SIP Video:</B>Turn on support for SIP video";
	tooltips['sip_general'].en[37] = "<B>Generate Manager Events:</B> Generate manager events when sip ua performs events (e.g. hold)";
	tooltips['sip_general'].en[38] = "<B>Reject NonMatching Invites:</B> When an incoming INVITE or REGISTER is to be rejected, for any reason, always reject with \'401 Unauthorized\' instead of letting the requester know whether there was a matching user or peer for their request";
	tooltips['sip_general'].en[39] = "<B>NonStandard G.726 Support:</B>  If the peer negotiates G726-32 audio, use AAL2 packing order instead of RFC3551 packing order (this is required for Sipura and Grandstream ATAs, among others). This is contrary to the RFC3551 specification, the peer _should_ be negotiating AAL2-G726-32 instead";
	tooltips['sip_general'].en[40] = "<B> T.38 FAX Passthrough Support:</B>";
	tooltips['sip_general'].en[41] = "<B>T.38 fax (UDPTL) Passthrough:</B>Enables T.38 fax (UDPTL) passthrough on SIP to SIP calls";
	tooltips['sip_general'].en[42] = "<B>Sip Debugging:</B>";
	tooltips['sip_general'].en[43] = "<B>Enable SIP debugging: </B>Turn on SIP debugging by default ";
	tooltips['sip_general'].en[44] = "<B>Record SIP History:</B> Record SIP history by default";
	tooltips['sip_general'].en[45] = "<B>Dump SIP History:</B> Dump SIP history at end of SIP dialogue";
	tooltips['sip_general'].en[46] = "<B>Status Notifications (Subscriptions):</B>";
	tooltips['sip_general'].en[47] = "<B>Subscribe Context:</B>  Set a specific context for SUBSCRIBE requests. Useful to limit subscriptions to local extensions";
	tooltips['sip_general'].en[48] = "<B>Allow Subscribe:</B> Support for subscriptions.";
	tooltips['sip_general'].en[49] = "<B>Notify on Ringing:</B> Notify subscriptions on RINGING state";
	tooltips['sip_general'].en[50] = "<B>Outbound SIP Registrations:</B>";
	tooltips['sip_general'].en[51] = "<B>Register:</B> Register as a SIP user agent to a SIP proxy (provider)";
	tooltips['sip_general'].en[52] = "<B>Register TimeOut:</B> Retry registration calls at every \'x\' seconds (default 20)";
	tooltips['sip_general'].en[53] = "<B>Register Attempts:</B> Number of registration attempts before we give up; 0 = continue foreverp";
	tooltips['sip_general'].en[54] = "<B>NAT Support:</B>";
	tooltips['sip_general'].en[55] = "<B>Extern ip:</B>Address that we're going to put in outbound SIP messages if we're behind a NAT";
	tooltips['sip_general'].en[56] = "<B>Extern Host:</B>Alternatively you can specify an external host, and Asterisk will perform DNS queries periodically.  Not recommended for production environments!  Use externip instead";
	tooltips['sip_general'].en[57] = "<B>Extern Refresh:</B> How often to refresh externhost if used. You may specify a local network in the field below";
	tooltips['sip_general'].en[58] = "<B>Local Network Address: </B>  \'192.168.0.0/255.255.0.0\'  : All RFC 1918 addresses are local networks, \'10.0.0.0/255.0.0.0\' : Also RFC1918,  \'172.16.0.0/12\' : Another RFC1918 with CIDR notation, \'169.254.0.0/255.255.0.0\' : Zero conf local network";
	tooltips['sip_general'].en[59] = "<B>NAT mode:</B>Global NAT settings  (Affects all peers and users); yes = Always ignore info and assume NAT; no = Use NAT mode only according to RFC3581; never = Never attempt NAT mode or RFC3581 support; route = Assume NAT, don't send rport";
	tooltips['sip_general'].en[60] = "<B>Allow RTP Reinvite:</B>Asterisk by default tries to redirect the RTP media stream (audio) to go directly from the caller to the callee.  Some devices do not support this (especially if one of them is behind a NAT).";
	tooltips['sip_general'].en[61] = "<B>Realtime Support:</B>";
	tooltips['sip_general'].en[62] = "<B>Auto-Expire Friends:</B> Auto-Expire friends created on the fly on the same schedule as if it had just registered? (yes|no|<seconds>) If set to yes, when the registration expires, the friend will vanish from the configuration until requested again. If set to an integer, friends expire within this number of seconds instead of the registration interval.";
	tooltips['sip_general'].en[63] = "<B>Cache Friends: </B> Cache realtime friends by adding them to the internal list ";
	tooltips['sip_general'].en[64] = "<B>Save SysName:</B> Save systemname in realtime database at registration";
	tooltips['sip_general'].en[65] = "<B>Send Registry Updates:</B> Send registry updates to database using realtime?";
	tooltips['sip_general'].en[66] = "<B>Ignore Expired Peers:</B>  Enabling this setting has two functions: <P> For non-realtime peers, when their registration expires, the information will _not_ be removed from memory or the Asterisk database if you attempt to place a call to the peer, the existing information will be used in spiteof it having expired</P> <P>For realtime peers, when the peer is retrieved from realtime storage, the registration information will be used regardless of whether it has expired or not; if it expires while the realtime peer is still in memory (due to caching or other reasons), the information will not be removed from realtime storage</P>";
	tooltips['sip_general'].en[67] = "<B>SIP Domain Support:</B>";
	tooltips['sip_general'].en[68] = "<B>Domain:</B> List of \'allowed\' domains";
	tooltips['sip_general'].en[69] = "<B>From Domain:</B> When making outbound SIP INVITEs to non-peers, use your primary domain \'identity\' for From: headers instead of just your IP address. This is to be polite and it may be a mandatory requirement for some destinations which do not have a prior account relationship with your server. ";
	tooltips['sip_general'].en[70] = "<B>Auto Domain:</B>Turn this on to have Asterisk add local host name and local IP to domain list.";
	tooltips['sip_general'].en[71] = "<B>Allow External Domains:</B>Allow requests for domains not serviced by this server ";
	tooltips['sip_general'].en[72] = "<B>Allow External Invites:</B> Enable INVITE and REFER to non-local domains ";
	tooltips['sip_general'].en[73] = "<B> Jitter Buffer Configuration:</B> ";
	tooltips['sip_general'].en[74] = "<B>Enable Jitter Buffer:</B>  Enables the use of a jitterbuffer on the receiving side of a SIP channel.";
	tooltips['sip_general'].en[75] = "<B>Force Jitter Buffer:</B>Forces the use of a jitterbuffer on the receive side of a SIP channel ";
	tooltips['sip_general'].en[76] = "<B>Log Frames:</B>Enables jitterbuffer frame logging.";
	tooltips['sip_general'].en[77] = "<B>Max Jitter Buffer:</B> Max length of the jitterbuffer in milliseconds";
	tooltips['sip_general'].en[78] = "<B>Resync Threshold:</B> Jump in the frame timestamps over which the jitterbuffer is resynchronized. Useful to improve the quality of the voice, with big jumps in/broken timestamps, usualy sent from exotic devices and programs. Defaults to 1000.";
	tooltips['sip_general'].en[79] = "<B>Implementation: </B>Jitterbuffer implementation, used on the receiving side of a SIP channel. Two implementations are currenlty available - \'fixed\' (with size always equals to jbmaxsize) and \'adaptive\' (with variable size, actually the new jb of IAX2)";


// Tooltips for IAX_General (iax_general)
	tooltips['iax_general']= new Object;
	tooltips['iax_general'].en = new Array;
	tooltips['iax_general'].en[0] = "<B>Bind Port:</B> Allows iax2 to listen to another port";
	tooltips['iax_general'].en[1] = "<B>Bind Address:</B> Force iax2 to bind to a specific address instead of all addresses ";
	tooltips['iax_general'].en[2] = "<B>IAX1 Compatibility:</B> allow/disallow iax1 style compatibility";
	tooltips['iax_general'].en[3] = "<B>No Checksums:</B> ";
	tooltips['iax_general'].en[4] = "<B>Delay Reject:</B> Cause or Remove iax2 to delay reject of calls to avoid dos";
	tooltips['iax_general'].en[5] = "<B>ADSI:</B> allow for adsi phone compatibility";
	tooltips['iax_general'].en[6] = "<B>AMA Flags:</B> remove, set flags for call detailed record";
	tooltips['iax_general'].en[7] = "<B>Accountcode:</B> set account code variable for call detailed record";
	tooltips['iax_general'].en[8] = "<B>Music On Hold Interpret:</B> remove allow hold music to be set by far end on box ";
	tooltips['iax_general'].en[9] = "<B>Music On Hold Suggest:</B> suggest music on hold for the channel";
	tooltips['iax_general'].en[10] = "<B>Language:</B> Set default language for channel, used by prompts etc";
	tooltips['iax_general'].en[11] = "<B>Bandwidth:</B> Sets allowed codecs for different bandwidth situations";
	tooltips['iax_general'].en[12] = "<B>Enable Jitter Buffer:</B> delay audio as to build a buffer to handle network jitter";
	tooltips['iax_general'].en[13] = "<B>Force Jitter Buffer:</B> force jitter buffering on all connections";
	tooltips['iax_general'].en[14] = "<B>Drop Count:</B> ";
	tooltips['iax_general'].en[15] = "<B>Max Jitter Buffer:</B> max time to buffer for jitter in milliseconds";
	tooltips['iax_general'].en[16] = "<B>Max Interpolation Frames:</B> number of interpolated  frames the jitter buffer should return consecutively.";
	tooltips['iax_general'].en[17] = "<B>Resync Threshold:</B> if amount of time is greater than resync threshold resync the jitter buffer.";
	tooltips['iax_general'].en[18] = "<B>Max Excess Buffer:</B> number of milliseconds to pad to the jitter buffer.";
	tooltips['iax_general'].en[19] = "<B>Min Excess Buffer:</B> ";
	tooltips['iax_general'].en[20] = "<B>Jitter Shrink Rate:</B> ";
	tooltips['iax_general'].en[21] = "<B>Trunk Freq:</B> frequency of trunk frames measured in millisecands";
	tooltips['iax_general'].en[22] = "<B>Trunk Time Stamps:</B> attach time stamps to trunk frames";
	tooltips['iax_general'].en[23] = "<B>Min Reg Expire:</B> minimum time for registration to exist";
	tooltips['iax_general'].en[24] = "<B>Max Reg Expire:</B> longest time for registration to exist";
	tooltips['iax_general'].en[25] = "<B>IAX ThreadCount:</B> number of iax helper threads to exist.";
	tooltips['iax_general'].en[26] = "<B>IAX Max ThreadCount:</B> maximum number of iax threads allowed.";
	tooltips['iax_general'].en[27] = "<B>Register:</B> ";
	tooltips['iax_general'].en[28] = "<B>Reg Context:</B> dynamically create extensions on registration in this context";
	tooltips['iax_general'].en[29] = "<B>Auto Kill:</B> If we have not recieved ACK for our NEW message in 2000ms terminate the connection.";
	tooltips['iax_general'].en[30] = "<B>Authentication Debugging:</B> dont show authincation traffic to ease debugging.";
	tooltips['iax_general'].en[31] = "<B>Codec Priority:</B> set codec negotiation priority to caller, host, disabled, reqonly.";
	tooltips['iax_general'].en[32] = "<B>Type of Service:</B> set tos bit for preferred ip routing";
	tooltips['iax_general'].en[33] = "<B>Cache Friends:</B> cache database queries on iax2 friends";
	tooltips['iax_general'].en[34] = "<B>Send Registry Updates:</B> update database for the new information from the registration";
	tooltips['iax_general'].en[35] = "<B>Auto-Expire Friends:</B> delete database parameters when the iax peer leaves.";
	tooltips['iax_general'].en[36] = "<B>Ignore Expired Peers:</B> if the peers ip registration is expired use the ip information  if available.";
	tooltips['iax_general'].en[37] = "<B>Disallowed Codecs:</B> set default disallow codecs to the following";
	tooltips['iax_general'].en[38] = "<B>Allowed Codecs:</B> set allowed codecs to the following.";
	tooltips['iax_general'].en[40] = "A single IP address or a range of IP addresses for which call token validation is not required in the form <b>11.11.11.11</b> or <b>11.11.11.11/22.22.22.22</b>.";
	tooltips['iax_general'].en[41] = "<B>Max Call Numbers:</B> Limits the amount of call numbers allowed for a single IP address.";
	tooltips['iax_general'].en[42] = "<B>Max Nonvalidated Call Numbers:</B> Limits the amount of nonvalidated call numbers for all IP addresses combined.";
	tooltips['iax_general'].en[43] = "<B>Call Number Limits:</B> Limits the call numbers for a given IP range.";


	// Tooltips for Options (options)
	tooltips['options']= new Object;
	tooltips['options'].en = new Array;
	tooltips['options'].en[0] = "<B>Current Password:</B> Please enter your existing password";
	tooltips['options'].en[1] = "<B>New Password:</B> Enter the New Password";
	tooltips['options'].en[2] = "<B>Retype New Password:</B> Retype New Password ";
	tooltips['options'].en[3] = "<B>Bind Address:</B> The IP address to which the GUI will be assigned to.";
	tooltips['options'].en[4] = "<B>Port:</B> GUI port. Must be specified in browser while accessing the GUI";
	tooltips['options'].en[5] = "<B>HTTP Timeout:</B> Session time out in seconds";
	tooltips['options'].en[6] = "";
	tooltips['options'].en[7] = "<B>Global Outbound CallerID:</B> This is default global CallerID that is used for all outgoing calls when no other CallerID is defined that has a higher priority.<BR /><BR />\n" + 
	    "When making outgoing calls the following rules are used to determine which CallerID will be used, if they exist:<BR /><BR />\n" +
		"<UL>\n" + 
		"  <LI>The <U>first</U> CallerID used is a CallerID set for the user making the call defined in the 'Users' tab.</LI>\n" + 
		"  <LI>The <U>second</U> CallerID is the one that is set in the 'VoIP Trunks' configuration, if applicable.</LI>\n" + 
		"  <LI><I>The <U>last</U> CallerID used for outgoing calls is the Global CID defined in the 'Options' tab.</I></LI>\n" + 
		"</UL>\n";
	tooltips['options'].en[8] = "The Operator Extension is the extension which will be dialed when a caller presses '0' to exit Voicemail. It is also available as a Voice Menu option.";
	tooltips['options'].en[9] = "The Language setting allows the user to specify the default prompts language for phone to phone, inbound, and outbound calls.  If a soundpack selection is made but not already installed, then the pack will be downloaded from Digium." ;
	tooltips['options'].en[10] = "Enables the display of a graphic on a phone's LCD display when the phone is idle." ;
	tooltips['options'].en[11] = "Number of seconds to ring a device before sending to the user's Voicemail Box" ;


	tooltips['options'].en[13] = "The timeout variable is the number of seconds the phone will wait for each segment of a digit map expressed as an integer.";
	tooltips['options'].en[14] = "<B>Global OutBound CID Name:</B> This is the global CallerID Name that is used for all outgoing calls. If this value is defined, all out going calls will have a 'CallerId Name' set to this value. This would be usually your company name. Leave this value blank if you want the users 'CallerID Name' to appear on outbound calls." ;

	// Tooltips for Directory
	tooltips['directory']= new Object;
	tooltips['directory'].en = new Array;
	tooltips['directory'].en[0] = "Extension to dial for accessing the Name Directory";
	tooltips['directory'].en[1] = "In addition to the name, also read the extension number to the caller before presenting dialing options";
	tooltips['directory'].en[2] = "Allow the caller to enter the first name of a user in the directory instead of using the last name";



	// Tooltips for status(status)
	tooltips['status']= new Object;
	tooltips['status'].en = new Array;
	tooltips['status'].en[0] = "<B>List of active channels:</B> Shows the list of active channels ";
	tooltips['status'].en[1] = "<B>Refresh:</B> Refresh the list of active channels";
	tooltips['status'].en[2] = "<B>Transfer:</B> Transfer selected channel";
	tooltips['status'].en[3] = "<B>Hangup:</B> Hangup selected Channel";

	// Tooltips for Voicemenus (menus)
	tooltips['menus']= {};
	tooltips['menus'].en = [];
	tooltips['menus'].en[0] = "<B>Name:</B> A name for the Voice Menu";
	tooltips['menus'].en[1] = "<B>Actions:</B> A sequence of actions performed when a call enters the menu.";
	tooltips['menus'].en[2] = "<B>Add a new step:</B>Add additional steps performed during the menu.";
	tooltips['menus'].en[3] = "<B>Dial other Extensions:</B>Is the caller allowed to dial extensions other than the ones explicitly defined";
	tooltips['menus'].en[4] = "<B>Keypress Events:</B> Allow key press events will cause the system to listen for DTMF input from the caller and define the actions that occur when a user presses the corresponding digit.";
	tooltips['menus'].en[5] = "<B>Extension(optional):</B> If you want this Voicemenu to be accessible by dialing an extension, then enter that extension number";
	tooltips['menus'].en[6] = "<B>t</B> Defines the timeout action. Timeout occurs when no DTMF entry is detected for 30 seconds after the voice menu has finished playing its prompts.";
	tooltips['menus'].en[7] = "<B>i</B> Defines the invalid action. The invalid action is triggered if the user enters a  DTMF that is not otherwise defined for the voicemenu.";
	
	// Tooltips for Calling Rules  (numberplan)
	tooltips['callingrules']= new Object;
	tooltips['callingrules'].en = new Array;
	tooltips['callingrules'].en[0] = "<B>Calling Rule Name:</B> Name of this Calling Rule. Ex: 'Local' or 'LongDistance' etc.";
	tooltips['callingrules'].en[1] = "<B>Pattern:</B> " +
		"All patterns are prefixed by the \"_\" character.  In patterns, some characters have special meanings:<br />\n" +
		"<ul>\n" +
		"  <li>X ... Any Digit from 0-9</li>\n" +
		"  <li>Z ... Any Digit from 1-9</li>\n" +
		"  <li>N ... Any Digit from 2-9</li>\n" +
		"  <li>[12345-9] ... Any Digit in the brackets (in this example, 1,2,3,4,5,6,7,8,9)</li>\n" +
		"  <li>. ... Wildcard, matches anything remaining; i.e. _9011. Matches anything starting with 9011 (excluding 9011 itself)</li>\n" +
		"  <li>! ... Wildcard, causes the matching process to complete as soon as it can unambiguously determine that no other matches are possible.\n" +
		"</ul>\n" +
		"For example, the extension _NXXXXXX would match normal 7 digit dialings, while _1NXXNXXXXX would represent a three digit area code plus phone number, proceeded by a one." +
		"&nbsp;&nbsp;<b>Tip: </b> Use '_X.' pattern to define a rule matching all incoming calls that provide a DID number.  Use 's' to define a rule matching  all incoming calls which do not have a DID number, for example, inbound analog calls.";

	tooltips['callingrules'].en[2] = "<B>Strip:</B> Allows the user to specify the number of digits that will be stripped from  the front of the dialing string before the call is placed via the trunk selected in \"Use Trunk.\"  One might; for example, want users to dial 9 before their long distance calls; however one does not dial 9 before those calls are placed onto analog lines and the PSTN, so one should strip 1 digit from the front before the call is placed.";
	tooltips['callingrules'].en[3] = "<B>Prepend these digits:</B> Allows the user to specify digits that are prepended before the call is placed via the trunk.  If a user's trunk required 10 digit dialing, but users were more comfortable performing 7 digit dialing, this field could be used to prepend a 3 digit area code to all 7 digit strings before they are placed to the trunk. User may also prepend a 'w' character for analog trunks to provide a slight delay before dialing";

	tooltips['callingrules'].en[4] = "<B>FailOver Trunk:</B> Failover trunks can be used to make sure that a call goes through an alternate route, when the primary trunk is busy or down If \"Use Failover Trunk\" is checked and \"Failover trunk\" is defined, then calls that cannot be placed via the regular trunk may have a secondary trunk defined.  If a user's primary trunk is a VoIP trunk, but one wants calls to use the PSTN when the VoIP trunk isn't available, this option is a good idea.";
	tooltips['callingrules'].en[5] = "<B>Send To Local Destination:</B> If this option is checked and Destination is defined, calls matching the specified pattern may be sent to a local extension.";
	tooltips['callingrules'].en[6] = "Defines the Trunk that calls, matching the specified pattern, will be placed through.";
	tooltips['callingrules'].en[7] = "<b>Filter:</b> This option is used to filter out certain characters.  The characters listed in this field will be permitted, while all others will be filtered out.  For example, a value of '012345' would allow 0, 1, 2, 3, 4, and 5, but filter out 6, 7, 8, and 9.";
	tooltips['callingrules'].en[8] = "<b>Caller ID:</b> This field takes precedence over any other Caller ID settings that may be configured for users or trunks. Specify a caller id string in the form <b>Name &lt;000-000-0000&gt;</b> or  <b>Name &lt;0000&gt;</b>.  Leaving this setting blank will allow the Caller ID settings configured in other areas to remain in place. Using only a number here will cause the name field to be blank, and the same is true for using a name only.  This field is ignored when the destination is a custom macro or application.";


	// Tooltips for Incoming Calls (incoming)
	tooltips['incoming']= new Object;
	tooltips['incoming'].en = new Array;
	tooltips['incoming'].en[0] = "<B>Add a Incoming Rule:</B> Define a new Rule for handling Incoming calls based on service provider and/or the number called.";
	tooltips['incoming'].en[1] = "<B>Pattern:</B> Pattern of the DID number, <i>not</I> the pattern for the CID (caller id number)";


	// Tooltips for Record a Menu (record)
	tooltips['record']= new Object;
	tooltips['record'].en = new Array;
	tooltips['record'].en[0] = "<B>Record a new Voice Menu:</B> Lets you record a new voice menu using any user extension device";
	tooltips['record'].en[1] = "<B>Filename:</B> File name under which the recorded file should be saved to. Ex: MainGreeting ";
	tooltips['record'].en[2] = "<B>Extension used for recording:</B> Select a device through which this voice menu will be recorded.";
	tooltips['record'].en[90] = "To help you create a customized IVR system for your business, voice prompts are available in English, French and Spanish, and are provided by Allison Smith and June Wallack. Click on the 'Where to Buy' button to get directly to the <i>IVR Voice Prompts</i> section on the Digium Store.";


	// Tooltips for System Information  (sysinfo)
	tooltips['sysinfo']= new Object;
	tooltips['sysinfo'].en = new Array;
	tooltips['sysinfo'].en[0] = "<B>General:</B> Information about OS, Uptime, Asterisk, Date, Timezone and Hostname";
	tooltips['sysinfo'].en[1] = "<B>ifconfig:</B> Network devices information (ifconfig)";
	tooltips['sysinfo'].en[2] = "<B>Resources:</B> Disk and Memory usage information";
	tooltips['sysinfo'].en[3] = "<B>Logs:</B> Asterisk Log files";

	tooltips['graph']= new Object;
	tooltips['graph'].en = new Array;
	tooltips['graph'].en[0] = "<B>CPU Usage:</B> Real-Time Updating Graph For CPU Usage";




//	Tooltips for networking.html in english
	tooltips['networking']= new Object;
	tooltips['networking'] .en = new Array;

	tooltips['networking'] .en[0] = "<B>Hostname:</B> Set the host name for your Asterisk Appliance" ;
	tooltips['networking'] .en[1] = "<B>NTP Server:</B> Sync time from a network time server" ;
	tooltips['networking'] .en[2] = "<B>SSH:</B> Enable/Disable SSH access to your Asterisk Appliance" ;
	tooltips['networking'] .en[3] = "<B>URL for Polycom Auto Provisioning:</B> Set http address for Auto Provisioning Polycom Phones.<BR>Example: http://192.168.69.1/phoneprov" ;



//	Tooltips for http_options.html in english
	tooltips['http_options']= new Object;
	tooltips['http_options'].en = new Array;

	tooltips['http_options'].en[0] = "" ;
	tooltips['http_options'].en[1] = "<B>Bind IP:</B> GUI will be available only on this IP address, if not sure please enter the LAN IP address. If you want the GUI to be available on both LAN & WAN interfaces - enter 0.0.0.0" ;
	tooltips['http_options'].en[2] = "<B>Port:</B> Please enter the port number on which you want to access the GUI. If not sure enter 80." ;


	// Tooltips for Service Providers (trunks)
	tooltips['trunks']= {};
	tooltips['trunks'].en = [];
	tooltips['trunks'].en[0] = "<B>Analog/Voip Trunks:</B> Analog lines are attached to analog interfaces of the PBX using FXO cards. Voice over IP (VoIP) connections are provided by an Internet Telephony Service Provider (ITSP).";
	tooltips['trunks'].en[1] = "<B>Provider:</B> Please select provider of your voice transport service.";
	tooltips['trunks'].en[2] = "<B>Lines:</B> Individual lines of the PBX Ex: Analog Port #3: The third analog port of the PBX.";
	tooltips['trunks'].en[3] = "<B>Username:</B> The username for your account with the provider - please contact your provider if you do not know it.";
	tooltips['trunks'].en[4] = "<B>Password:</B> The password for your account with the provider - please contact your provider if you do not know it.";
	tooltips['trunks'].en[5] = "<B>Ngt Host:</B> This is the IP of the SIP server provided by NGT for your specific user account.";
	// advanced options for analog trunks
	tooltips['trunks'].en[21] = "<B>Busy Detection:</B> Busy Detection is used to detect far end hangup or for detecting busy signal. Enable to turn this feature on";
	tooltips['trunks'].en[22] = "<B>Busy Count:</B> If Busy Detection is enabled, it is also possible to specify how many busy tones to wait for before hanging up. The default is 4, but better results can be achieved if set to 6 or even 8. Mind that the higher the number, the more time that will be needed to hangup a channel, but lowers the probability that you will get random hangups.";
	tooltips['trunks'].en[23] = "<B>Busy Pattern:</B> If Busy Detection is enabled, it is also possible to specify the cadence of your busy signal. In many countries, it is 500msec on, 500msec off. Without Busy Pattern specified, asterisk will accept any regular sound-silence pattern that repeats <Busy Count> times as a busy signal. If you specify Busy Pattern, then asterisk will further check the length of the sound (tone) and silence, which will further reduce the chance of a false positive.";
	tooltips['trunks'].en[24] = "<B>Ring Timeout:</B> Trunk (FXO) devices must have a timeout to determine if there was a hangup before the line was answered. This value can be tweaked to shorten how long it takes before asterisk considers a non-ringing line to have hungup.";
	tooltips['trunks'].en[25] = "<B>Answer on Polarity Switch:</B> If this option is enabled, the reception of a polarity reversal will mark when a outgoing call is answered by the remote party.";

	tooltips['trunks'].en[26] = "<B>Hangup on Polarity Switch:</B> In some countries, a polarity reversal is used to signal the disconnect of a phone line. If the Hangup on Polarity Switch option is enabled, the call will be considered \"hung up\" on a polarity reversal.";
	tooltips['trunks'].en[27] = "<B>Call Progress:</B> On trunk interfaces it can be useful to attempt to follow the progress of a call through RINGING, BUSY, and ANSWERING. If turned on, call progress attempts to determine answer, busy, and ringing on phone lines. This feature is HIGHLY EXPERIMENTAL and can easily detect false answers. Few zones are currently supported, but may be selected with the Progress Zone option. This feature can also easily detect false hangups. The symptoms of this is being disconnected in the middle of a call for no reason.";
	tooltips['trunks'].en[28] = "<B>Progress Zone :</B> If Call Progress is enabled, Progress Zone defines the call progress zone for the trunk interfaces.";
	tooltips['trunks'].en[29] = "<B>Use CallerID :</B> Enabling this option enabled Caller ID detection.";

	tooltips['trunks'].en[30] = "<B>Caller ID Start:</B> This options allows one to define the start of a Caller ID signal: Ring, to start when a ring is received, or Polarity, to start when a polarity reversal is started.";
	tooltips['trunks'].en[31] = "<B>CallerID:</B> This option allows the lines to report the Caller ID string as received from the telco, or as a fixed value by using the Custom option.";
	tooltips['trunks'].en[32] = "<B>Pulse Dial:</B> If this option is enabled, pulse mode dialing, instead of DTMF, will be enabled.";
	tooltips['trunks'].en[33] = "<B>CID Signalling :</B> This option defines the type of Caller ID signalling to use: bell (bell202 as used in the United States), v23 (as used in the UK), v23_jp (as used in Japan), or dtmf (as used in Denmark, Sweden, and Holland).";
	tooltips['trunks'].en[34] = "Flash Time defines the time, in milliseconds, that is generated for a flash operation.";
	tooltips['trunks'].en[35] = "Receiver Flash Time defines the time, in milliseconds, that is required for the receiver to recognize a flash operation.";
	tooltips['trunks'].en[43] = "<B>Groups:</B> This GUI uses groups in the backend and creates a group for every trunk to use by itself.  You can also create new groups and add multiple trunks to each one to use in calling rules so that Asterisk will be able to choose one of several available trunks when handling calls.  To create a new group, you must add a trunk to it by selecting \"New\" while adding or editing a trunk. To delete a group, simply remove all trunks from it.";
	// end of 'advanced options for analog trunks'
	
	// BEGIN trunks_voip.html
	tooltips['trunks'].en[36] = "<B>CallerID:</B> This is the number that the trunk will try to use when making outbound calls. For some providers it is not possible to set the CallerID with this option, and this option might be ignored.<BR /><BR />\n" + 
	    "When making outgoing calls the following rules are used to determine which CallerID will be used, if they exist:<BR /><BR />\n" +
		"<UL>\n" + 
		"  <LI>The <U>first</U> CallerID used is a CallerID set for the user making the call defined in the 'Users' tab.</LI>\n" + 
		"  <LI><I>The <U>second</U> CallerID is the one that is set in the 'VoIP Trunks' configuration, if applicable.</I></LI>\n" + 
		"  <LI>The <U>last</U> CallerID used for outgoing calls is the Global CID defined in the 'Options' tab.</LI>\n" + 
		"</UL>\n";
	tooltips['trunks'].en[37] = "<B>Provider Name:</B> A unique label to help you identify this trunk when listed in outbound rules, incoming rules etc.";
	tooltips['trunks'].en[38] = "<B>Trunk Name:</B> A unique label to help you identify this trunk when listed in outbound rules, incoming rules etc. Ex: 'Port 5' ";
	tooltips['trunks'].en[39] = "<B>Username:</B> Username that you authenticate with at VoIP provider. Must be unique if <B>Context Naming</B> is based on Username.";
	tooltips['trunks'].en[40] = "<B>Context Naming:</B> How should Asterisk GUI determine the context name in Asterisk's .conf files.  Asterisk can assign a unique name itself, or you can base it upon the <B>Provider Name</B> or <B>Username</B> that you enter below.  Let Asterisk assign a name unles your VoIP provider requires otherwise. <br /> If you plan on using a contact extension, this feature needs to be set to 'Based on Username'. ";
	tooltips['trunks'].en[41] = "<B>Hostname:</B> IP address or URL for your VoIP providers server.";
	tooltips['trunks'].en[42] = "<B>Contact Extension:</B> This requires that the context naming of this trunk be based on the username. <br /> If this is disabled, then you will need to recreate this trunk and select 'Context Naming' to be 'Based on Username'.";
	// END   trunks_voip.html
	



//	Tooltips for 'Operation Mode settings' in english
	tooltips['opermode_settings']= {};
	tooltips['opermode_settings'].en = [];
	tooltips['opermode_settings'].en[0] = "Opermode: Specifies On Hook Speed, Ringer Impedance, Ringer Threshold, Current Limiting, TIP/RING voltage adjustment, Minimum Operational Loop Current, and AC Impedance selection as predefined for your country's analog line characteristics.  Please choose your country, or your nearest neighboring country.  FCC is equivalent to United States.  TBR21 is equivalent to Austria, Belgium, Denmark, Finland, France, Germany, Greece, Iceland, Ireland, Italy, Luxembourg, Netherlands, Norway, Portugal, Spain, Sweden, Switzerland, and the United Kingdom.  If no user choice is specified, the default is FCC." ;
	tooltips['opermode_settings'].en[1] = "a-law Override:  Specifies the codec to be used for analog lines.  North American users should choose ulaw.  All other countries, unless otherwise known, should be assumed to be alaw. If no user choice is specified, the default is ulaw.";
	tooltips['opermode_settings'].en[2] = "FXS Honor Mode: This option allows the user to determine if they would like Opermode characteristics applied to trunk (FXO) modules only, or both trunk (FXO) and station (FXS) modules. If no user choice is specified, the default is 'apply opermode to fxo modules only'.";
	tooltips['opermode_settings'].en[3] = "Boost Ringer: This option allows the user to define whether they require normal ringing voltage (40V) or maximum ringing voltage (89V) for analog phones attached to station (FXS) modules.  If no user choice is specified, the default is 'normal'.";

	tooltips['opermode_settings'].en[4] = "Low Power: This option, generally used in conjunction with the Fast Ringer option, allows the user to set the peak voltage during Fast Ringer operation to 50V.  If no user choice is specified, the default is normal.";

	tooltips['opermode_settings'].en[5] = "Fast Ringer: This option, sometimes used in conjunction with the Low Power option, allows the user to increase the ringing speed to 25Hz.  If no user choice is specified, the default is 'normal'.";

	tooltips['opermode_settings'].en[6] = "Ring Detect:  This option allows the user to choose from normal ring detection, or a <i>full wave</i> detection to prevent false ring detection for lines where Caller ID is sent before the first ring and proceeded by a polarity reversal, as in the UK.  If no user choice is specified, the default is 'standard'.";

	tooltips['opermode_settings'].en[7] = "MWI Mode: This option allows the user to specify the type of Message Waiting Indicator detection to be done on trunk (FXO) interfaces. The options are 'none' which performs no detection; 'FSK' which performs Frequency Shift Key detection ; or 'NEON' which performs Neon MWI detection.  If no user choice is specified, the default is 'none'.";

	tooltips['opermode_settings'].en[8] = " Echo Cancellation NLP Type: This option allows the user to specify the type of Non Linear Processor they want applied to the post echo-cancelled audio reflections received from analog connections.  There are several options: <LI>None: disables NLP processing and is not a recommended setting.  Under most circumstances, users choosing 'None' will experience some residual echo. <LI>Mute: causes the NLP to mute inbound audio steams while a user connected to the appliance is speaking. For users in quiet environments, Mute may be acceptable. <LI>Random Noise: causes the NLP to inject random noise to mask the echo reflection. For users in normal environments, Random Noise may be acceptable. <LI>Hoth Noise: causes the NLP to inject a low-end Gaussian noise with a frequency spectrum similar to voice.  For users in normal environments, Hoth Noise may be acceptable. <LI>Suppression NLP: causes the NLP to suppress echo reflections by reducing the amplitude of their volume. Suppression may be used in combination with the 'Echo cancellation NLP Max Suppression' option. For users in loud environments, Suppression may be the best option. <BR>If no user choice is specified, the default is 'Suppression NLP.'";

	tooltips['opermode_settings'].en[9] = "Echo Cancellation NLP Threshold: This option specifies the threshold, in dB difference between the received audio post-echo cancellation and the transmitted audio, where the NLP will engage. If no user choice is specified, the default is 24 db.";

	tooltips['opermode_settings'].en[10] = "Echo Cancellation NLP Max Suppression: This option, only usable when the Echo Cancellation NLP Type is set to Suppression, specifies the maximum amount of dB that the NLP should attenuate the residual echo. Lower numbers mean that the NLP will provide less suppression, meaning the residual echo will sound louder. Higher numbers, especially those approaching or equaling the Echo Cancellation NLP Threshold option, will nearly mute the residual echo. If no user choice is specified, the default is 24 db.";

	tooltips['opermode_settings'].en[11] = "Echo Cancellation Type: DAHDI uses modular echo cancellers that are configured per channel. However, echo cancellation via the GUI is currently a global setting. The echo cancellers are compiled and installed as part of the dahdi-linux package. You can specify which echo canceller to be used. The default behavior is for there to be NO echo canceller on any channel. If restore this default, simply disable the echo canceller. Hpec is only a valid echo canceller if it is compiled and installed. Hardware echo cancellation will be performed instead of software echo cancellation for any channel that has a hardware echo cancellation module installed.";

// Tooltips for "Configuring Hardware" in english
	tooltips['confighw'] = { en: [] };
	tooltips['confighw'].en[0] = "<B>Tone Region:</B> Please choose your country or your nearest neighboring country for default Tones (Ex: dialtone, busy tone, ring tone etc.)";

// Tooltips for "Call Parking" in english
	tooltips['parking'] = { en: [] };
	tooltips['parking'].en[0] = "<B>Max Parking Time: </B> How many seconds that a call can be parked for before it is reconnected by ringing the phone that parked the call, or disconnecting the call if it is not answered and moving on with the next calling rule that is defined in this context.";

// Tooltips for "Active Channels" in english
	tooltips['channels'] = { en: [] };
	tooltips['channels'].en[0] = "<B>User: </B> A user on the PBX to transfer the call to.";



// Tooltips for "Follow Me" in english
	tooltips['followme'] = { en: [] };
	tooltips['followme'].en[0] = "<B>Status:</B> Enable/Disable FollowMe for this user.";
	tooltips['followme'].en[1] = "Music On Hold class that the caller would hear while tracking the user.";
	tooltips['followme'].en[2] = "<B>DialPlan:</B> DialPlan that would be used for dialing the FollowMe numbers. By default this would be the same dialplan as that of the user.";
	tooltips['followme'].en[3] = "<B>Destinations:</B> List of extensions/numbers that would be dialed to reach the user during FollowMe.";
	tooltips['followme'].en[4] = "<B>New FollowMe Number:</B> Add a new FollowMe number which could be a 'Local Extension' or an 'Outside Number'. The selected dialplan should have permissions to dial any outside numbers defined.";
	tooltips['followme'].en[5] = "<B>Dial Order:</B> This is the order in which the FollowMe destinations are dialed to reach the user.";


// Tooltips for "Paging" in english
	tooltips['paging'] = { en: [] };
	tooltips['paging'].en[0] = "<B>Alert-Info Header:</B> This is the value that is sent to the phone for an intercom call in the alert info header. It is not recommended that this valued be changed from the default of Intercom. ";
	tooltips['paging'].en[1] = "Dial sequence that is used to prefix an extension to dial it as a Page. For instance setting this value to ** would allow to page the extension 6000 by dialing **6000.";
	tooltips['paging'].en[2] = "Dial sequence that is used to prefix an extension to dial it as Intercom. For instance setting this value to *# would allow to initiate an intercom call with extension 6000 by dialing *#6000.";
	tooltips['paging'].en[3] = "If this option is checked, a beep sound will be played when the intercom call is connected to inform users they can begin talking.";

// Tooltips for "CDR Viewer" in english
	tooltips['CDR'] = { en: [] };
	tooltips['CDR'].en[0] = "<B>System calls</B> are internal calls triggered by the Asterisk GUI and flagged with a destination context of <I>asterisk_guitools</I>. Select this option to include them in the CDR list. This also includes calls with the same major unique ID as the actual <I>asterisk_guitools</I> call.";
	tooltips['CDR'].en[1] = "<B>Inbound calls</B> are calls originating from a non-internal source (like a VoIP trunk) and sent to an internal extension";
	tooltips['CDR'].en[2] = "<B>Outbound calls</B> are calls sent to a non-internal source (like a VoIP trunk) from an internal extension";
	tooltips['CDR'].en[3] = "<B>Internal calls</B> are calls from one user extension to another and are not sent over a trunk";
	tooltips['CDR'].en[4] = "<B>External calls</B> are calls from one trunk to another trunk and are not sent to any internal extension";
	tooltips['CDR'].en[5] = "<B>All fields.</B> Select this option to display all fields recorded in the <I>Master.csv</I> CDR file.";
