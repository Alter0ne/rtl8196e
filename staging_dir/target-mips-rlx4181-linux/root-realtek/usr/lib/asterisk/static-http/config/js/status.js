/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * status.html functions
 *
 * Copyright (C) 2006-2008, Digium, Inc.
 *
 * Pari Nannapaneni <pari@digium.com>
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
var CUR_CHAN;
var COUNTDOWN = 10;

var show_Edit_ChannelDirection = function(chan) {
	CUR_CHAN = chan;
	_$('USERS').selectedIndex = -1;
	$('#div_activechannel_edit').showWithBg();
};

var doTransfer = function() {
	var to = _$('USERS').value;
	ASTGUI.doTransfer(CUR_CHAN, to);
	ASTGUI.feedback({ msg:'Transfered ' + CUR_CHAN + ' to ' + to, showfor: 3, color: 'blue' });	
	$('#div_activechannel_edit').hideWithBg();
};

var show_Edit_ChannelHangup = function(chan) {
	if(confirm("Are you sure you want to hangup " + chan + "?")) {
		ASTGUI.doHangup(chan);
		ASTGUI.feedback({ msg:'Hungup Channel ' + chan, showfor: 3, color: 'blue' });	
	}

	updateChannels_List(); /* Refresh */
};

var localajaxinit = function(){
	top.document.title = 'Monitor active calls' ;

	var ul = parent.pbx.users.list();
	ul.sort();
	ul.each( function(user) {
		ASTGUI.selectbox.append('USERS', user, user);
	});

	setInterval( function(){
		if( COUNTDOWN == 0){
			updateChannels_List();
			return;
		}

		_$('thisPage_COuntDown').innerHTML = 'Refreshing Active Channels in ' + COUNTDOWN + ' Seconds';
		COUNTDOWN--;
	}, 1100);

	updateChannels_List();
};


var updateChannels_List = function(){
	COUNTDOWN = 10;
	var TBL = _$('table_channel_list') ;
	ASTGUI.domActions.clear_table(TBL);

	var addCell = ASTGUI.domActions.tr_addCell;
	/* XXX Move this function to ASTGUI */
	var ul = parent.miscFunctions.listOfChannels();
	_$('thisPage_lite_Heading').innerHTML = "Active Channels - " + ul.length;

	if(!ul.length){
		ASTGUI.domActions.clear_table(TBL);
		var newRow = TBL.insertRow(-1);
		newRow.className = 'even';
		addCell( newRow , { html:'No Channels Open !!', align:'center'} );
		return ;
	}

	(function(){ // add first row
		var newRow = TBL.insertRow(-1);
		newRow.className = "frow";
		addCell( newRow , { html:'Channel'});
		addCell( newRow , { html:'State'});
		addCell( newRow , { html:'Seconds'});
		addCell( newRow , { html:'Application'});
		addCell( newRow , { html:''});
		addCell( newRow , { html:''});

	})();

	ul.each( function(chan){ // list each channel in table
		var newRow = TBL.insertRow(-1); 
		newRow.className = ((TBL.rows.length)%2==1)?'odd':'even';
		addCell( newRow , { html: chan.Channel });
		if ( chan.State == "Up" ){
			addCell( newRow , { html: '<font color=green><b>Up</b></font>' });
		}else{
			addCell( newRow , { html: '<font color=red><b>' + chan.State + '<b></font>' });
		}
		addCell( newRow , { html: chan.Seconds });
		var tmp_a = [];
		tmp_a[0] = "<span class='guiButton' onclick=\"show_Edit_ChannelDirection('" + chan.Channel +"')\">Transfer</span>&nbsp;" ;
		tmp_a[1] = "<span class='guiButton' onclick=\"show_Edit_ChannelHangup('" + chan.Channel +"')\">Hangup</span>&nbsp;" ;

		var tmp_app = '';

		var tmp_dialplan_context = context2json({ filename:'extensions.conf' , context : chan.Context , usf: 0 });
		if( ASTGUI.isArray( tmp_dialplan_context ) ){
			var tmp_dialplan_lineIndex = tmp_dialplan_context.indexOfLike('exten='+ chan.Extension + ',' + chan.Priority +',');
			if(tmp_dialplan_lineIndex != -1){
				var tmp_dialplan_line = tmp_dialplan_context[tmp_dialplan_lineIndex];
				tmp_app = ASTGUI.parseContextLine.getAppWithArgs(tmp_dialplan_line);
			}
		}
		addCell( newRow , { html: '<i>' + tmp_app + '</i>' });
		addCell( newRow , { html: tmp_a[0] , align:'center' });
		addCell( newRow , { html: tmp_a[1] , align:'center' });
	});
};
