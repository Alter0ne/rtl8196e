/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * welcome2.html functions
 *
 * Copyright (C) 2006-2008, Digium, Inc.
 *
 * Ryan Brindley <rbrindley@digium.com
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

var REGISTRY_OUTPUT = {};
var manager_events = {};
var manager_timers = {};
var extension_loads = { all: true, analog: false, features: false, iax: false, sip: false };
var mgr = {};

var loadTrunks = function() {
	EX_CF = config2json({filename:'extensions.conf', usf:0 });

	$('#trunks_list tr:not(:first)').remove();
	var TBL = _$('trunks_list');
	var addCell = ASTGUI.domActions.tr_addCell; // temporarily store the function

/* IAX and SIP Trunks */
	var t = parent.ASTGUI.cliCommand('iax2 show registry') ;
	REGISTRY_OUTPUT.iax2 = ASTGUI.parseCLIResponse(t) ;
	t = parent.ASTGUI.cliCommand('sip show registry') ;
	REGISTRY_OUTPUT.sip = ASTGUI.parseCLIResponse(t) ;
	var d = parent.pbx.trunks.list({sip: true, iax: true});

	d.each( function(item){
		var ttype = parent.pbx.trunks.getType(item);
		var newRow = TBL.insertRow(-1);
		newRow.className = ((TBL.rows.length)%2==1)?'odd':'even';

		var reg = ASTGUI.getTrunkStatus(REGISTRY_OUTPUT, item, ttype) ;
		addCell( newRow , { html: reg } );

		addCell( newRow , { html: parent.sessionData.pbxinfo.trunks[ttype][item]['trunkname'] } );
		addCell( newRow , { html: ttype } );
		addCell( newRow , { html: parent.sessionData.pbxinfo.trunks[ttype][item]['username'] || '' } );
		addCell( newRow , { html: parent.sessionData.pbxinfo.trunks[ttype][item]['host'] } );
		//addCell( newRow , { html: default_IncomingRule_trunk(item) });
	} );
/* End IAX and SIP Trunks */

/* Analog Trunks */
	var c = parent.pbx.trunks.list({analog: true});
	c.each(function(item){
		var newRow = TBL.insertRow(-1);
		newRow.className = ((TBL.rows.length)%2==1)?'odd':'even';

		addCell( newRow , { html:''} );
		addCell( newRow , { html: parent.sessionData.pbxinfo['trunks']['analog'][item]['trunkname'] } );
		addCell( newRow , { html:'Analog'} );
		addCell( newRow , { html:''} );
		addCell( newRow , { html:'Ports ' + parent.sessionData.pbxinfo['trunks']['analog'][item][top.sessionData.DahdiChannelString] } );
	});
/* End Analog Trunks */

/* Providers */
	var d = parent.sessionData.pbxinfo.trunks.providers ;
	for(var e in d){ if(d.hasOwnProperty(e)){

			var ttype = parent.pbx.trunks.getType(e);
			var newRow = TBL.insertRow(-1) ;
			newRow.className = ((TBL.rows.length)%2==1)?'odd':'even';
			var reg = ASTGUI.getTrunkStatus(REGISTRY_OUTPUT, e, ttype) ;
			addCell( newRow , { html: reg } );
			addCell( newRow , { html: d[e]['trunkname'] } );
			addCell( newRow , { html: ttype } );
			addCell( newRow , { html: d[e]['username'] || '' } );
			addCell( newRow , { html: d[e]['host'] } );
	}}
/* End Providers */

/* Digital Trunks */
	var c = parent.sessionData.pbxinfo['trunks']['pri'] ;
	for(var d in c){if(c.hasOwnProperty(d)){
		var newRow = TBL.insertRow(-1);
		newRow.className = ((TBL.rows.length)%2==1)?'odd':'even';
		addCell( newRow , { html:'' });
		addCell( newRow , { html: c[d]['trunkname'] });
		addCell( newRow , { html: 'Digital (' + c[d]['signalling'] + ')' }); // 
		var tmp_channels = c[d]['dahdichan'] || c[d]['zapchan'] || ' ' ;
		addCell( newRow , { html: 'Ports: ' + tmp_channels });
		addCell( newRow , { html:''} );
	}}


	var c = parent.sessionData.pbxinfo['trunks']['bri'] ;
	for(var d in c){if(c.hasOwnProperty(d)){
		var newRow = TBL.insertRow(-1);
		newRow.className = ((TBL.rows.length)%2==1)?'odd':'even';
		addCell( newRow , { html:'' });
		addCell( newRow , { html: c[d]['trunkname'] });
		addCell( newRow , { html: 'BRI' });
		addCell( newRow , { html: 'Ports: ' + c[d]['ports'] });
		addCell( newRow , { html:''} );
	}}
/* End Digital Trunks */
};

/* This is ugly because doing it the pretty way is unacceptably slow. */
var loadExtensions = function() {

	/* List all User Extensions */
	var exten_tbody = $("#extensions_list > tbody");
	var ul = parent.pbx.users.list();
	ul = ul.sortNumbers( );
	ul.each(function(user){ // list each user in table
		var ud = parent.sessionData.pbxinfo.users[user];
		var tmp_usertype_a = [];

		/* check to see if the extension has iax and sip */
		if( ud.getProperty('hassip').isAstTrue() && ud.getProperty('hasiax').isAstTrue() ) {
			tmp_usertype_a.push( 'SIP/IAX User' );
			var new_class = 'iax sip';
		} else if ( ud.getProperty('hasiax').isAstTrue() ) {
			tmp_usertype_a.push( '&nbsp;IAX User' );
			var new_class = 'iax';
		} else if ( ud.getProperty('hassip').isAstTrue() ) {
			tmp_usertype_a.push( '&nbsp;SIP User' );
			var new_class = 'sip';
		}

		/* check seperately if it has analog */
		if (ud.getProperty(top.sessionData.DahdiChannelString) !== '') {
			tmp_usertype_a.push( 'Analog User (Port ' + ud[top.sessionData.DahdiChannelString] + ')' ) ;
			new_class += ' analog'
		}

		if( !ud.getProperty('context') || ! parent.sessionData.pbxinfo.callingPlans[ud.getProperty('context')] ){
			var tmp_userstring = user + '<font color=red>*No DialPlan assigned</font>' ;
		}else{
			var tmp_userstring = user;
		}
		var new_row = "<tr class=\"" + new_class.replace(/^ /,'') + "\">\n";

		new_row += "<td><img src=\"images/refresh.png\" border=\"0\" id=\"exten_status_" + user + "\"></td>\n";
		new_row += "<td>" + tmp_userstring + "</td>\n";
		new_row += "<td>" + ud.getProperty('fullname') + "</td>\n";
		new_row += "<td class=\"exten_status\" id=\"exten_mailbox_" + user + "\"></td>\n";
		new_row += "<td>" + tmp_usertype_a.join(',&nbsp;') + "</td>\n";

		exten_tbody.append(new_row + "</tr>");
	});
	
	var updateUserStatus = function(user){ // list each user in table
		if(document.getElementById("exten_status_" + user)){ 
			document.getElementById("exten_status_" + user).parentNode.innerHTML = ASTGUI.getUser_DeviceStatus_Image(user); 
			var tmp_mails = ASTGUI.mailboxCount(user); 
			if( tmp_mails.count_new > 0 ){ 
				document.getElementById("exten_mailbox_" + user).innerHTML = '<font color=#888B8D> Messages : <B><font color=red>'
					+ tmp_mails.count_new + '</font>/' + tmp_mails.count_old + '</B></font>' ; 
			}else{ 
				document.getElementById("exten_mailbox_" + user).innerHTML = '<font color=#888B8D> Messages : '
					+ tmp_mails.count_new + '/' + tmp_mails.count_old + '</font>' ; 
			} 
		}
	};
	var uidx = -1;
	var id = setInterval(function(){
		updateUserStatus(ul[uidx++]);
		if(uidx >= ul.length){
			clearInterval(id);
		}
	}, 300);

	(function(){ // List all RingGroup Extensions
		var c = parent.sessionData.pbxinfo.ringgroups ;
		for(var d in c){if(c.hasOwnProperty(d)){
			var new_row = $('<tr></tr>').addClass('feature');
				$("<td></td>").html('').appendTo(new_row)
				$("<td></td>").html(c[d]['extensions'] || '--').appendTo(new_row);
				$("<td></td>").html(c[d]['NAME'] || d).appendTo(new_row);
				$("<td></td>").html('').appendTo(new_row);
				$("<td></td>").html('Ring Group').appendTo(new_row);
			exten_tbody.append(new_row);
		}}
	})();

	(function(){ // List all VoiceMenu Extensions
		var c = parent.sessionData.pbxinfo.voicemenus ;
		for(var d in c){if(c.hasOwnProperty(d)){
			var new_row = $('<tr></tr>').addClass('feature');

			$("<td></td>").html('').appendTo(new_row);
			$("<td></td>").html(ASTGUI.parseContextLine.getExten(c[d].getProperty('alias_exten')) || '--').appendTo(new_row);
			$("<td></td>").html(c[d]['comment'] || d).appendTo(new_row);
			$("<td></td>").html('').appendTo(new_row);
			$("<td></td>").html('Voice Menu').appendTo(new_row);

			exten_tbody.append(new_row);
		}}
	})();

	(function(){ // List VoicemailMain
		var tmp = '';
		if( parent.sessionData.pbxinfo['localextensions'].hasOwnProperty('VoiceMailMain') ){
			tmp = ASTGUI.parseContextLine.getExten( parent.sessionData.pbxinfo['localextensions']['VoiceMailMain'] ) ;
		}

		if( tmp ){
			var tmp_voicemails = tmp;
		}else{
			var tmp_voicemails = '-- <font color=red>*No Extension assigned</font>' ;
		}

		var new_row = $('<tr></tr>').addClass('feature');

		$("<td></td>").html('').appendTo(new_row);
		$("<td></td>").html(tmp_voicemails).appendTo(new_row);
		$("<td></td>").html('<b>Check Voicemails</b>').appendTo(new_row);
		$("<td></td>").html('').appendTo(new_row);
		$("<td></td>").html('VoiceMailMain').appendTo(new_row);

		exten_tbody.append(new_row);
	//	addCell( newRow ,
	//		{	html: tmp_voicemails ,
	//			onclickFunction: function(){
	//				parent.miscFunctions.click_panel( 'voicemail.html');
	//			}
	//		}
	//	);
	})();

	(function(){ // VoiceMail Groups
		var vmgroups = parent.sessionData.pbxinfo.vmgroups.getOwnProperties();
		vmgroups.each(function( this_vmg_exten ){
			var new_row = $('<tr></tr>').addClass('feature');

			$("<td></td>").html('').appendTo(new_row);
			$("<td></td>").html(this_vmg_exten).appendTo(new_row);
			$("<td></td>").html(parent.sessionData.pbxinfo.vmgroups[this_vmg_exten].getProperty('label')).appendTo(new_row);
			$("<td></td>").html('').appendTo(new_row);
			$("<td></td>").html('VoiceMail Group').appendTo(new_row);

			exten_tbody.append(new_row);
		});
	})();

	(function(){
		var tmp = parent.sessionData.pbxinfo['localextensions'].getProperty('defaultDirectory') ;
		if( tmp ){
			var tmp_dirExten = tmp;
		}else{
			var tmp_dirExten = '-- <font color=red>*No Extension assigned</font>' ;
		}
		
		var new_row = $('<tr></tr>').addClass('feature');

		$("<td></td>").html('').appendTo(new_row);
		$("<td></td>").html(tmp_dirExten).appendTo(new_row);
		$("<td></td>").html('<b>Dial by Names</b>').appendTo(new_row);
		$("<td></td>").html('').appendTo(new_row);
		$("<td></td>").html('Directory').appendTo(new_row);

		exten_tbody.append(new_row);
	//	addCell( newRow ,
	//		{	html: tmp_dirExten ,
	//			onclickFunction: function(){ parent.miscFunctions.click_panel( 'directory.html'); }
	//		}
	//	);
	})();

	$("#extensions_list tbody tr:odd").removeClass("even").addClass("odd"); 	//removing classes to
	$("#extensions_list tbody tr:even").removeClass("odd").addClass("even");	//accuont for old js code
};

var loadQueues = function() {
	var queue_template = $('#queue_left_container > div.template');
	var queue_conts = {}; //queue containers array
	queue_conts[0] = $('#queue_left_container');
	queue_conts[1] = $('#queue_right_container');
	var cont_index = 0;

	$('.queue:not(.template)').remove();

	var queue_status = ASTGUI.cliCommand('queue show');
	queue_status = parent.ASTGUI.parseCLIResponse(queue_status);
	//ast-gui*CLI> show queues
	//6501         has 1 calls (max unlimited) in 'ringall' strategy (0s holdtime), W:0, C:0, A:4, SL:0.0% within 0s
	//   Members: 
	//      Agent/6000 (Busy) has taken no calls yet
	//   Callers: 
	//      1. IAX2/6000-453 (wait: 0:03, prio: 0)
	//
	//6500         has 0 calls (max unlimited) in 'ringall' strategy (0s holdtime), W:0, C:0, A:2, SL:0.0% within 0s
	//   No Members
	//   No Callers

	if (queue_status.contains('No queues.')) {
		return;
	}

	queue_status = queue_status.split('\n\n');
	queue_status.each( function(q) { 
		q = q.trim();
		if (q.length == 0) {
			return;
		}

		var queue = queue_template.clone();
		var agent_cnt = 0;
		var call_cnt = 0;
		lines = q.split('\n');
		var line_index = 0;

		//the following will always be the first line.
		//6501         has 0 calls (max unlimited) in 'ringall' strategy (0s holdtime), W:0, C:0, A:0, SL:0.0% within 0s
		var line = lines[line_index].split(' ');
		line_index++;
		var q_name = line[0]; //6501

		for (var i=0; i<line.length; i++) {
			if (line[i].contains('strategy')) {
				var strategy = line[i-1].replace(/'/g,'');
			} else if (line[i].contains('W:')) {
			} else if (line[i].contains('C:')) {
				var calls_completed = line[i].match('\\d\\d*').toString();
			} else if (line[i].contains('A:')) {
				var calls_abandoned = line[i].match('\\d\\d*').toString();
			} else if (line[i].contains('SL:')) {
				var service_level = line[i].concat(' ',line[i+1],' ',line[i+2]);
			}
		}

		if( !lines[line_index] ) lines[line_index] = '';
		//line #2 (index=1) will either be 'Members:' or 'No Members'
		if (lines[line_index].contains('Members:')) {
			line_index++;
			var agents = queue.contents().find('.agents');
			var agent_template = $('#agent_template');

			//traverse through all the members til the Callers line appears
			//Callers line will either be 'Callers:' or 'No Callers'
			while (!lines[line_index].contains('Callers')) {
				var agent = agent_template.clone();

				//      Agent/6000 (Busy) has taken no calls yet
				line = lines[line_index].trim();
				var a_exten = line.split(' ')[0].split('/')[1];
				var a_status = line.match('[\\s\\w][\\s\\w]*(?=\\))').toString();

				switch(a_status.toLowerCase()) {
					case 'ringing':
					case 'ring in use':
						var a_status_img = 'images/agent_ringing.png';
						agent_cnt++;
						break;
					case 'busy':
					case 'in use':
						var a_status_img = 'images/agent_busy.png';
						agent_cnt++;
						break;
					case 'available':
					case 'not in use':
						var a_status_img = 'images/agent_loggedin.png';
						agent_cnt++;
						break;
					default:
						var a_status_img = 'images/agent_loggedout.png';
						break;
				}

				agent.children('span.extension').html(a_exten);
				agent.children('img.status_icon').attr('src',a_status_img);

				agent.removeClass('template');
				agent.removeAttr('id');
				agent.addClass('agent_'+a_exten);
				agents.append(agent);

				line_index++;
			}
		} else if (lines[line_index].contains('No Members')) {
			queue.contents().find('.agents').html('No Agents');	
			line_index++;
		}

		//next line will either be 'Callers:' or 'No Callers'
		if (lines[line_index].contains('Callers:')) {
			line_index++;
			var calls = queue.contents().find('.calls > table > tbody');
			var call_template = $('<tr></tr>').addClass('call');

			//The remaining lines of the queue should be calls
			while ( lines[line_index] ) {
				call_cnt++;
				var call = call_template.clone();

				//      1. IAX2/6000-453 (wait: 0:03, prio: 0)
				line = lines[line_index].trim();
				var sections = line.split(' ');

				//'1.'
				var order = sections[0];
				
				//find '6000' in 'IAX2/6000-453'
				var cid = sections[1].match('\\d\\d*(?=-\\d*)').toString();

				for (var i=2; i<sections.length; i++) {
					if (sections[i].contains('wait')) {
						//'0:03'
						var waittime = sections[i+1].replace(/,/,'');

						var wt_tmp = waittime.split(':');
						var secs = (parseInt(wt_tmp[0], 10) * 60) + parseInt(wt_tmp[1], 10);
						if (secs > 30) {
							var call_class = 'old';
						} else if (secs > 120) {
							var call_class = 'average';
						} else {
							var call_class = 'new';
						}
						i++; //no need to check for the next section as we know its '0:03'
						continue;
					} else if (sections[i].contains('prio')) {
						//'0'
						var priority = sections[i+1].replace(/\)/,'');
						i++; //no need to check for the next section as we know its '0'
						continue;
					}
				}

				$('<td></td>').html(order).appendTo(call);
				$('<td></td>').html(cid).appendTo(call);
				$('<td></td>').html(waittime).addClass('count_html_up').appendTo(call);

				call.addClass(call_class);
				call.attr('id',''+q_name.toString()+'_call_'+cid.toString());
				calls.append(call);
				line_index++;
			}
		} else if (lines[line_index].contains('No Callers')) {
			queue.contents().find('.calls > table.list > tbody').html('<tr><td>No Calls</td></tr>');
			line_index++;
		}

		queue.contents().find('.name').html(q_name);
		queue.contents().find('.strategy').html(strategy);
		queue.contents().find('.users_agents').html(call_cnt + " calls, " + agent_cnt + " agents");
		queue.contents().find('.service_level').html(service_level);
		queue.contents().find('.calls_complete').html(calls_completed);
		queue.contents().find('.calls_abandoned').html(calls_abandoned);
		queue.removeClass('template');
		queue.attr('id','queue_'+q_name);
		queue_conts[cont_index].append(queue);
		cont_index = cont_index == 1 ? 0 : 1;
	});
};

var loadConferenceRooms = function() {
	var conf_template = $('#sys_status_meetme > .body > div.template');
	var conf_body = $('#sys_status_meetme > .body');
	var active_conferences = {};

	var show_chans = ASTGUI.cliCommand('core show channels concise');
	// DAHDI/pseudo-1529934219!default!s!1!Rsrvd!(None)!!!!3!1086!(None)
	// SIP/6001-081f6500!DLPN_users!6300!1!Up!MeetMe!6300|MIx!6001!!3!1086!(None)
	show_chans = parent.ASTGUI.parseCLIResponse(show_chans);
	var chans = show_chans.trim().split('\n');

	for (var i=0; i<chans.length; i++) {
		var chan = chans[i].trim();
		if (chan.length == 0) {
			continue;
		}

		chan = chan.split('!');
		/* 0. Channel		1. Context	2. Extension	3. Prio		4. State
		*  5. Application	6. Data		7. CallerID	8. <blank>	9. AccountCode
		*  10. Duration		10. BridgedTo 						*/
		if ( chan.length < 6 || chan[5].toLowerCase() != 'meetme') {
			continue;
		}

		var conf_room = chan[6].match('[\\w\\d][\\d\\w]*(?=[\\||,])');

		if (active_conferences[conf_room]) {
	/* Existing Conf */
			var conf = active_conferences[conf_room];
			var tmp_dur = parseInt(chan[10], 10);
			conf.duration = conf.duration < tmp_dur ? tmp_dur : conf.duration;
		} else {
	/* New Conf */
			var conf = {};
			conf.id = conf_room;
			conf.count = 0;
			conf.duration = parseInt(chan[10], 10);
			conf.users = new Array();
		}
		
		var user = {};
		user.cid = chan[7];
		user.duration = chan[10];

		conf.count++;
		conf.users.push(user);

		active_conferences[conf_room] = conf;
	}

	var active_count = 0;
	var m = parent.sessionData.pbxinfo.conferences;
	for( l in m ) { 
		if( !m.hasOwnProperty(l) || l =='admin') {
			continue;
		}

		var tmp = conf_template.clone();
		tmp.attr('id','conf_'+l.toString());
		tmp.appendTo(conf_body);
		var users_template = conf_template.contents().find('.members tr.template');

		tmp.contents().find(".name").html(l);
		if( active_conferences.hasOwnProperty(l) ) {
			active_count++;
			tmp.contents().find(".status").html(Number(active_conferences[l].count) + ' Users');
			tmp.contents().find(".time").addClass('count_html_up').html(secs2html(active_conferences[l].duration));
			
			var conf_list_tbody = tmp.contents().find('.members > .list > tbody').empty();
			while ( user = active_conferences[l].users.pop() ) {
				addMeetmeMember(l, user.cid, user.duration);
			}

		}else{
			tmp.contents().find(".status").html('Not In Use');
			tmp.contents().find(".time").html("00:00");
			tmp.contents().find(".members > .list > tbody").empty().html('<tr><td colspan="4">No Active Members</td></tr>');
			tmp.find('.minimaxi').html('[ + ]');
			tmp.find('.body').hide();

		}

		tmp.removeClass("template");
	}
};

var loadParkingLot = function() {
	var plot_tbody = $("#parking_lot_list > tbody");
	plot_tbody.empty();

	$.get( ASTGUI.paths.rawman, { action: 'ParkedCalls' }, function(op){
		var chunks = ASTGUI.miscFunctions.getChunksFromManagerOutput(op, 1);

		chunks.each( function( chunk ){
			if( !chunk.hasOwnProperty('Event') || !chunk.hasOwnProperty('Exten') ) {
				return;
			}
			var new_row = $('<tr></tr>').attr('id','plot_'+chunk.Exten.toString())
	
			$("<td></td>").html(chunk.CallerID).appendTo(new_row);
			$("<td></td>").html(chunk.Channel).appendTo(new_row);
			$("<td></td>").html(chunk.Exten).appendTo(new_row);
			$("<td></td>").html(chunk.Timeout).addClass('count_down').appendTo(new_row);

			new_row.appendTo(plot_tbody);
		});

		if( $("#parking_lot_list > tbody > tr").length < 1 ) {
			pushRow(plot_tbody, [
				$("<td></td>").attr('colspan','4').html("No Parked Calls")
			]);
		}
	});
};

manager_events.watch = function() {
	$.get( ASTGUI.paths.rawman, {action:'waitevent'}, function(op){ manager_events.parseOutput(op); });
};

manager_events.parseOutput = function(op) {
	var events = op.toLowerCase();
	events = events.split('\r\n\r\n');

	try {
	for (var i=0; i<events.length; i++) {
		var event = events[i];
		if (event.contains('response: success')) {
			continue;
		} else if (event.contains('event: waiteventcomplete')) {
			continue;
		} else if (event.length == 0) {
			continue;
		}
		/* this is used for manager_events.parseEvent() which was
		 * decided to only be called in the cases that needed to
		 * have the event parsed. This is for efficiency reasons.
		 */
		var event_lines = event.split('\r\n');

		switch (event_lines[0].trim()) {
		case 'event: agentcallbacklogin':
			var eventObj = this.parseEvent(event_lines);
			this.updateAgent("callbacklogin", eventObj.agent, eventObj.loginchan);
			break;
		case 'event: agentcallbacklogoff':
			var eventObj = this.parseEvent(event_lines);
			this.updateAgent("callbacklogoff", eventObj.agent, eventObj.loginchan);
			break;
		case 'event: extensionstatus':
			var eventObj = this.parseEvent(event_lines);
			this.updateExtension(eventObj.exten.trim(), eventObj.context, eventObj['_status']);
			break;
		case 'event: hangup':
			var eventObj = this.parseEvent(event_lines);
			/* exten = 'Channel: Zap/2-1' or 'Channel: SIP/101-3f3f' */
			var exten = eventObj.channel.split('/')[1].split('-')[0];
			this.updateExtension(exten.trim(),'','hangup');
			break;
		case 'event: join':
			var eventObj = this.parseEvent(event_lines);
			/* exten = 'Channel: Zap/2-1' or 'Channel: SIP/101-3f3f' */
			var exten = eventObj.channel.split('/')[1].split('-')[0];
			this.addQueueCall(eventObj.queue, exten, eventObj.callerid, eventObj.position);
			break;
		case 'event: leave':
			var eventObj = this.parseEvent(event_lines);
			/* event_lines[2]= 'Channel: Zap/2-1' or 'Channel: SIP/101-3f3f' */
			var exten = eventObj.channel.split('/')[1].split('-')[0];
			this.removeQueueCall(eventObj.queue, exten);
			break;
		case 'event: meetmejoin':
			var eventObj = this.parseEvent(event_lines);
			/* event_lines[2]= 'Channel: Zap/2-1' or 'Channel: SIP/101-3f3f' */
			var chan = eventObj.channel.split('/')[1].split('-')[0];
			var usernum = event_lines[5].split(' ')[1];
			this.meetmeJoin(eventObj.meetme, eventObj.usernum, chan, 0);
			break;
		case 'event: meetmeleave':
			var eventObj = this.parseEvent(event_lines);
			/* event_lines[2]= 'Channel: Zap/2-1' or 'Channel: SIP/101-3f3f' */
			var chan = eventObj.channel.split('/')[1].split('-')[0];
			this.meetmeLeave(eventObj.meetme, eventObj.usernum, chan);
			break;
		case 'event: meetmetalking':
			var eventObj = this.parseEvent(event_lines);
			/* event_lines[2]= 'Channel: Zap/2-1' or 'Channel: SIP/101-3f3f' */
			var chan = eventObj.channel.split('/')[1].split('-')[0];
			state = eventObj['_status'].toString() == 'on' ? true : false;
			this.meetmeTalking(eventObj.meetme, chan, state);
			break;
		case 'event: newchannel':
			var eventObj = this.parseEvent(event_lines);
			/* event_lines[2]= 'Channel: Zap/2-1' or 'Channel: SIP/101-3f3f' */
			var exten = eventObj.channel.split('/')[1].split('-')[0];
			this.updateExtension(exten.trim(), '', eventObj[mgr.newchannel.channelstate]);
			break;
		case 'event: newstate':
			var eventObj = this.parseEvent(event_lines);
			/* event_lines[2]= 'Channel: Zap/2-1' or 'Channel: SIP/101-3f3f' */
			var exten = eventObj.channel.split('/')[1].split('-')[0];
			this.updateExtension(exten.trim(), '', eventObj[mgr.newstate.channelstate]);
			break;
		case 'event: parkedcall':
			var eventObj = this.parseEvent(event_lines);
			this.parkedCall(eventObj.exten, eventObj.channel, eventObj.from, eventObj,timeout, eventObj.callerid);
			break;
		case 'event: parkedcallgiveup':
		case 'event: parkedcalltimeout':
		case 'event: unparkedcall':
			var eventObj = this.parseEvent(event_lines);
			this.unparkedCall(eventObj.exten);
			break;
		case 'event: queuememberstatus':
			var eventObj = this.parseEvent(event_lines);
			var agent = eventObj.membername.split('/')[1];	//MemberName: 6000
			this.updateAgent('status_'+eventObj['_status'].toString(), agent, eventObj.queue);
			break;
		case 'event: agentcalled':
		case 'event: agentcomplete':
		case 'event: agentconnect':
		case 'event: agentdump':
		case 'event: agentlogin':
		case 'event: agentlogoff':
		case 'event: queuememberpaused':
		case 'event: dial':
		case 'event: musiconhold':
		case 'event: link':
		case 'event: messagewaiting':
		case 'event: newcallerid':
		case 'event: newexten':
		case 'event: rename':
		case 'event: setcdruserfield':
		case 'event: unlink':
		case 'event: alarm':
		case 'event: alarmclear':
		case 'event: dndstate':
		case 'event: logchannel':
		case 'event: peerstatus':
		case 'event: registry':
		case 'event: reload':
		case 'event: shutdown':
		case 'event: userevent':
			break;
		default:
			//console.log(event[3]);
			break;
		}
	}
	} catch (err) {
		top.log.error('Error Parsing waitevent response : manager_events.parseOutput() ');
		throw(err);
	}

	this.watch();
};

manager_events.updateAgent = function(event, agent, loginchan) {
	/* The agent can belong to multiple queues, below will gather all of its locations*/
	var agent_locs = $('#sys_status_queues .agent_'+agent.toString());

	switch(event) {
	case 'callbacklogin':
		agent_locs.children('.status_icon').attr('src','images/agent_loggedin.png');
		break;
	case 'callbacklogoff':
		agent_locs.children('.status_icon').attr('src','images/agent_loggedout.png');
		break;
	case 'status_0': /* Unknown */
		agent_locs.children('.status_icon').attr('src','images/agent_loggedout.png');
		break;
	case 'status_1': /* Not In Use */
		agent_locs.children('.status_icon').attr('src','images/agent_loggedin.png');
		break;
	case 'status_2': /* In Use */
	case 'status_3': /* Busy */
		agent_locs.children('.status_icon').attr('src','images/agent_busy.png');
		break;
	case 'status_4': /* Invalid */
		agent_locs.children('.status_icon').attr('src','images/agent_loggedout.png');
		break;
	case 'status_5': /* Not Available (Disconnected) */
		agent_locs.children('.status_icon').attr('src','images/agent_loggedout.png');
		break;
	case 'status_6': /* Ringing */
		agent_locs.children('.status_icon').attr('src','images/agent_ringing.png');
		break;
	case 'status_7': /* Ring In Use */
		agent_locs.children('.status_icon').attr('src','images/agent_ringing.png');
		break;
	case 'status_8': /* On Hold */
		agent_locs.children('.status_icon').attr('src','images/agent_busy.png');
		break;
	default:
		alert('updateAgent: '+event.toString());
		break;
	}

	agent_locs.parents('.queue').find('.users_agents').each( function() {
		var agents = $(this).parents('.queue').find(".status_icon[src!='images/agent_loggedout.png']").length - 1 /* template */;
		this.innerHTML = this.innerHTML.replace(/[0-9][0-9]*(?= agents)/,agents);
	});
};

manager_events.addQueueCall = function (queue, exten, cid, position) {
	var calls_tbody = $('#queue_'+queue.toString()+' .calls > .list > tbody');
	var new_row = $('<tr></tr>').attr('id',''+queue.toString()+'_call_'+exten.toString()).addClass('call').addClass('new');

	if (position == 1) {
		calls_tbody.empty();
	}

	$("<td></td>").html(position).appendTo(new_row);
	$("<td></td>").html(cid).appendTo(new_row);
	$("<td></td>").html('0:00').addClass('count_html_up').appendTo(new_row);

	calls_tbody.append(new_row);

	var calls = calls_tbody.children('tr').length;
	var innerHTML = $('#queue_'+queue.toString()+' .users_agents').html();
	$('#queue_'+queue.toString()+' .users_agents').html(innerHTML.replace(/[0-9][0-9]*(?= calls,)/,calls));
};

manager_events.removeQueueCall = function (queue, exten) {
	var calls_tbody = $('#queue_'+queue.toString()+' .calls > .list > tbody');
	var calls;
	var call_cnt = 1;

	$('#'+queue.toString()+'_call_'+exten.toString()).remove();

	calls_tbody.children('tr').each( function() {
		$(this).children('td:first').html(call_cnt+'. ');
		call_cnt++;
	}); 

	if ( (calls = calls_tbody.children('tr').length) == 0) {
		calls_tbody.append(
			$('<tr></tr>').append(
				$('<td></td>').attr('colspan','3').html('No Calls')
			)
		);
	}

	var innerHTML = $('#queue_'+queue.toString()+' .users_agents').html();
	$('#queue_'+queue.toString()+' .users_agents').html(innerHTML.replace(/[0-9][0-9]*(?= calls,)/,calls));
};

manager_events.updateExtension = function(exten, context, state) {
	var exten_status_img = $('#exten_status_'+exten.toString()+' img');
	if (typeof state == 'undefined') {
		state = "Unknown";
	}
	switch(state.toString()) {
	case '-1':	/* Invalid */
		var state = 'Invalid';
		exten_status_img.attr('src','images/status_gray.png');
		break;
	case '0':	/* Not In Use */
		var state = 'Not In Use';
		exten_status_img.attr('src','images/status_green.png');
		break;
	case '1':	/* In Use */
		var state = 'In Use';
		exten_status_img.attr('src','images/status_red.png');
		break;
	case '2':	/* Busy */
	case 'busy':	/* Busy */
		var state = 'Busy';
		exten_status_img.attr('src','images/status_red.png');
		break;
	case '4':	/* Unavailable */
		var state = 'Unavailable';
		exten_status_img.attr('src','images/status_gray.png');
		break;
	case '8':	/* Ringing */
	case 'ringing':	/* Ringing */
		var state = 'Ringing';
		exten_status_img.attr('src','images/status_orange.png');
		break;
	case '9':	/* Ringing In Use */
		var state = 'Ringing In Use';
		exten_status_img.attr('src','images/status_orange.png');
		break;
	case '16':	/* On Hold */
		var state = 'On Hold';
		exten_status_img.attr('src','images/status_red.png');
		break;
	case 'dialing':	/* Dialing */
		//var state = 'Dialing';
		//exten_status_img.attr('src','images/status_red.png');
		break;
	case 'down':	/* Down */
		var state = 'Available';
		exten_status_img.attr('src','images/status_green.png');
		break;
	case 'hangup':
		var state = 'Hangup';
		exten_status_img.attr('src','images/status_green.png');
		break;
	case 'ring':	/* Line is ringing */
		var state = 'Ring';
		exten_status_img.attr('src','images/status_red.png');
		break;
	case 'rsrvd':	/* Reserved */
		var state = 'Reserved';
		exten_status_img.attr('src','images/status_red.png');
		break;
	case 'up':	/* Up */
		var state = 'Up';
		exten_status_img.attr('src','images/status_red.png');
		break;
	default:
		top.log.debug("updateExtension :: We have encountered an unknown extension state of " + state.toString());
		var state = 'Unknown';
		exten_status_img.attr('src','images/status_gray.png');
		break;
	}

	exten_status_img.parent().siblings('.exten_status').html(state);
	$('#extensions_list').trigger('update');
};

manager_events.parkedCall = function(exten, chan, from, timeout, cid) {
	var plot_tbody = $('#parking_lot_list > tbody');

	if (plot_tbody.children('tr').length == 1 && plot_tbody.contents('tr > td:first').html().contains('No Parked Calls')) {
		/* If plot_tbody only has 1 row and it contains 'No Parked Calls' then empty tbody */
		plot_tbody.empty();
	}

	var new_row = $('<tr></tr>').attr('id','plot_'+exten.toString());

	$('<td></td>').html(cid).appendTo(new_row);
	$('<td></td>').html(chan).appendTo(new_row);
	$('<td></td>').html(exten).appendTo(new_row);
	$('<td></td>').html(timeout).addClass('count_down').appendTo(new_row);

	plot_tbody.append(new_row);
};

manager_events.unparkedCall = function(exten) {
	var plot_tbody = $('#parking_lot_list > tbody');

	$('#plot_'+exten.toString()).remove();

	if (plot_tbody.children('tr').length == 0) {
		plot_tbody.append(
			$('<tr></tr>').append(
				$('<td></td>').attr('colspan','4').html('No Parked Calls')
			)
		);
	}
};

manager_events.meetmeJoin = function (meetme, user_num, chan) {
	var conf = $('#conf_'+meetme.toString()+' .members > .list > tbody');
	if (conf.find('tr:first > td:first').html() == 'No Active Members') {
		conf.empty();
		conf = $('#conf_'+meetme.toString());
		conf.find('.status').html('1 User');
		conf.find('.time').addClass('count_html_up').html('00:00');
		conf.find('.body').show();
		conf.find('.minimaxi').html('[ - ]');
	} else {
		conf = $('#conf_'+meetme.toString());
		var status = conf.contents().find('.status').html();
		status = status.split(' ')[0];
		status = parseInt(status, 10)+1;
		conf.find('.status').html(status+' Users');
	}

	addMeetmeMember(meetme, chan, 0);
};

manager_events.meetmeLeave = function (meetme, user_num, chan) {
	$('#'+meetme.toString()+'_'+chan.toString()).remove();
	
	var conf = $('#conf_'+meetme.toString()+' .members > .list > tbody');
	if (conf.children('tr').length == 0 ) {
		conf = $('#conf_'+meetme.toString());
		conf.find(".status").html('Not In Use');
		conf.find(".time").removeClass('count_html_up').html("00:00");
		conf.find(".members > .list > tbody").empty().html('<tr><td colspan="4">No Active Members</td></tr>');
		conf.find('.body').hide();
		conf.find('.minimaxi').html('[ + ]');
	} else {
		conf = $('#conf_'+meetme.toString());
		var status = conf.contents().find('.status').html();
		status = status.split(' ')[0];
		status = parseInt(status, 10)-1;
		conf.find('.status').html(status+' Users');
	}
};

manager_events.meetmeTalking = function(meetme, exten, is_talking) {
	var user_stat;
	if ( user_stat = $('#'+meetme.toString()+'_'+exten.toString()+' .mem_status') ) {
		user_stat.html( is_talking ? 'Talking' : 'Active' );
	}
};

manager_events.parseEvent = function(event_lines) {
	var eventObj = new Object();
	
	for (var i=0; i<event_lines.length; i++) {
		var line = event_lines[i];
		line = line.split(': ');
		/* 'status' is a reserved term in javascript....dangit */
		line[0] = (line[0] === 'status') ? '_status' : line[0];
		eventObj[line[0]] = line[1];
	}

	return eventObj;
};

//abstracted out because its called in manager_events.meetmeJoin & loadConferences
var addMeetmeMember = function(meetme, user_num, duration) {
	//set duration's default value
	duration = typeof(duration) != 'undefined' ? duration : 0;

	var conf = $('#conf_'+meetme.toString()+' .members > .list > tbody');

	var new_row = $('#sys_status_meetme > .body > div.template .members tr.template').clone();
	new_row.children('.extension').html(user_num);
	new_row.children('.duration').addClass('count_html_up').html(secs2html(duration));
	new_row.children('.mem_status').html('Active');

	new_row.removeClass('template');
	new_row.attr('id',meetme.toString()+'_'+user_num);
	conf.append(new_row);
};

var countHtml = function() {
	$('.count_html_up').each( function(i) {
		var old_time = this.innerHTML;
		var old_time_secs = html2secs(old_time.toString());

		var new_time = 1+old_time_secs;
		var new_time_html = secs2html(new_time);

		this.innerHTML = new_time_html;
	});

	$('.count_html_down').each( function(i) {
		var old_time = this.innerHTML;
		old_time = html2secs(old_time.toString());

		var new_time = parseInt(old_time, 10)-1;
		new_time = secs2html(new_time);

		this.innerHTML = new_time;
	});

	setTimeout(arguments.callee,1000);
};

var countSecs = function() {
	$('.count_up').each( function(i) {
		this.innerHTML = parseInt(this.innerHTML, 10)+1;
	});

	$('.count_down').each( function(i) {
		this.innerHTML = parseInt(this.innerHTML, 10)-1;
	});

	setTimeout(arguments.callee,1000);
};

var html2secs = function(html) {
	html = html.trim().split(':');

	if (html.length == 3) {
		return (parseInt(html[0], 10) * 60*60) + (parseInt(html[1], 10) * 60) + parseInt(html[2], 10);
	} else if (html[0].toString() != '0' && html[0].toString() != '00') {
		return ((parseInt(html[0], 10)*60)+parseInt(html[1], 10));
	} else {
		return parseInt(html[1], 10);
	}
};

var secs2html = function(secs) {
	var tmp = secs;
	var mins = Math.floor(secs/60);
	var hrs = Math.floor(mins/60);
	mins = mins % 60;
	secs = secs % 60;
	if (hrs != 0) {
		return ''+hrs.toString()+':'+mins.toString()+':'+secs.addZero();
	} else {
		return ''+mins.toString()+':'+secs.addZero();
	}
};

var pushRow = function(tbody, cells) {
	var new_row = $("<tr></tr>");
	for (var i=0; i < cells.length; i++) {
		cells[i].appendTo(new_row);
	}
	new_row.appendTo(tbody);
};
