/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * meetme.html functions
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
var isNewBridge ;
var EDIT_BRIDGE ;

var loadDOMElements = function(){
	DOM_table_mml = _$('table_meetmelist');
	DOM_edit_MeetMeDiv = _$('edit_MeetMeDiv');
	DOM_edit_MeetMe_title = _$('edit_MeetMe_title');
	DOM_edit_Ext = _$('edit_Ext');
	DOM_edit_adminExtension = _$('edit_adminExtension');
	DOM_edit_PinCode = _$('edit_PinCode');
	DOM_edit_AdminPinCode = _$('edit_AdminPinCode');
	DOM_edit_moh_firstcaller = _$('edit_moh_firstcaller');
	DOM_edit_callerMenu = _$('edit_callerMenu');
	DOM_edit_announceCallers = _$('edit_announceCallers');
	DOM_edit_quietMode = _$('edit_quietMode');
	DOM_edit_waitMarked = _$('edit_waitMarked');
	DOM_edit_closeLastMarkedUser = _$('edit_closeLastMarkedUser');
	DOM_edit_recording = _$('edit_recording');
};

var updateMeetmesTable = function(){
	var addCell = ASTGUI.domActions.tr_addCell; // temporarily store the function
	(function(){ // add first row
		var newRow = DOM_table_mml.insertRow(-1);
		newRow.className = "frow";
		addCell( newRow , { html:'', width:'15px'} );
		addCell( newRow , { html:'ConferenceRoom'} );
		addCell( newRow , { html:'Password'} );
		addCell( newRow , { html:'Admin Extension'} );
		addCell( newRow , { html:'Announce Callers'} );
		addCell( newRow , { html:'MusicOnHold'} );
		addCell( newRow , { html:''} );
	})();
	(function(){
		var meetme_rooms = [];
		var m = parent.sessionData.pbxinfo.conferences ;
		for( l in m ){ if( m.hasOwnProperty(l) && l !='admin'){
			meetme_rooms.push(l);
		}}
		meetme_rooms.sortNumbers();
		meetme_rooms.each( function(this_room){
			var this_MEETME = ASTGUI.cloneObject(parent.sessionData.pbxinfo.conferences[this_room]) ;
			var a = this_MEETME['configOptions'].split("${EXTEN}");
			if(a.length <= 1){ return; }
			var room_options = a[1].betweenXY(',',')') ;

			var newRow = DOM_table_mml.insertRow(-1);
			newRow.className = ((DOM_table_mml.rows.length)%2==1)?'odd':'even';
			var pwstr = parent.sessionData.pbxinfo.conferences[this_room]['pwdString'] ;
			var tmp = "<span class='guiButton' onclick=\"editMeetme_form('" + this_room +"')\">Edit</span>&nbsp;"
					+ "<span class='guiButtonDelete' onclick=\"delete_meetMe_confirm('" + this_room +"')\">Delete</span>" ;
			addCell( newRow , { html:''} );
			addCell( newRow , { html: this_room } );
			addCell( newRow , { html: pwstr.split(',')[1] || '<i><font color=#308b5a>No Password</font></i>' } );
			addCell( newRow , { html: ASTGUI.parseContextLine.getExten( this_MEETME.getProperty('adminOptions')) || '--' } );
			addCell( newRow , { html: (room_options.contains('I')) ? 'Yes' : 'No'} ); //Announce Callers
			addCell( newRow , { html: (room_options.contains('M')) ? 'Yes' : 'No'} ); //MusicOnHold
			addCell( newRow , { html: tmp } ); // Edit Options
		});
	})();
	(function(){
		if( DOM_table_mml.rows.length == 1 ){
			ASTGUI.domActions.clear_table(DOM_table_mml);
			var newRow = DOM_table_mml.insertRow(-1);
			newRow.className = 'even';
			addCell( newRow , { html:'No Conference rooms defined !!'} );
			return ;
		}
	})();
};

var delete_meetMe_confirm = function(room){
	if(!confirm("Delete Conference Bridge "+ room + " ?")) { return true; }
	var u = new listOfSynActions('extensions.conf') ;
		if( parent.sessionData.pbxinfo.conferences[room]['configOptions'] ){
			u.new_action('delete', ASTGUI.contexts.CONFERENCES, 'exten', '', parent.sessionData.pbxinfo.conferences[room]['configOptions'] );
		}
		if( parent.sessionData.pbxinfo.conferences[room]['adminOptions'] ){
			u.new_action('delete', ASTGUI.contexts.CONFERENCES, 'exten', '', parent.sessionData.pbxinfo.conferences[room]['adminOptions'] );
		}
		u.callActions();
	u.clearActions('meetme.conf'); // flush u and point to a different file
		if( parent.sessionData.pbxinfo.conferences[room]['pwdString'] ){
			u.new_action('delete', 'rooms', 'conf', '', parent.sessionData.pbxinfo.conferences[room]['pwdString'] );
		}
		u.callActions();
	delete parent.sessionData.pbxinfo.conferences[room] ;
	ASTGUI.feedback( { msg:"'Conference Bridge' deleted", showfor: 3, color:'red', bgcolor:'#FFFFFF' } );
	window.location.reload();
};

var newMeetme_form = function(){
	isNewBridge = true ;
	EDIT_BRIDGE = '' ;
	show_MeetMe_Form();
};

var editMeetme_form = function(k){
	isNewBridge = false ;
	EDIT_BRIDGE = k ;
	show_MeetMe_Form();
};

var show_MeetMe_Form = function(){
	if(isNewBridge == true){
		ASTGUI.resetTheseFields([DOM_edit_Ext , DOM_edit_PinCode , DOM_edit_AdminPinCode , DOM_edit_moh_firstcaller , DOM_edit_callerMenu , DOM_edit_announceCallers, DOM_edit_quietMode , DOM_edit_waitMarked]);
		var tmp_allextensions = ASTGUI.cloneObject( parent.miscFunctions.getAllExtensions() );
		DOM_edit_Ext.value  = tmp_allextensions.firstAvailable( parent.sessionData.GUI_PREFERENCES.getProperty('mm_start') );
		//mml.push(DOM_edit_Ext.value);
		_$('edit_adminExtension').value = ''; // mml.firstAvailable( parent.sessionData.GUI_PREFERENCES.getProperty('mm_start') );
		DOM_edit_Ext.disabled = false;
		DOM_edit_MeetMe_title.innerHTML = 'New Conference Bridge';
		ASTGUI.feedback( { msg:'New Conference Bridge', showfor: 3 } );
		$(DOM_edit_MeetMeDiv).showWithBg();
		return;
	}

	var TMP_EDIT_BRIDGE = ASTGUI.cloneObject(parent.sessionData.pbxinfo.conferences[EDIT_BRIDGE]);

	var room_options = TMP_EDIT_BRIDGE['configOptions'].split("${EXTEN}")[1].betweenXY(',',')') ;
	var pwstr = TMP_EDIT_BRIDGE['pwdString'] ;
	DOM_edit_Ext.value = EDIT_BRIDGE;
	DOM_edit_Ext.disabled = true ;
	DOM_edit_adminExtension.value = ASTGUI.parseContextLine.getExten( TMP_EDIT_BRIDGE['adminOptions'] ) ;
	DOM_edit_PinCode.value = pwstr.split(',')[1] || '' ;
	DOM_edit_AdminPinCode.value = pwstr.split(',')[2] || '' ;
	DOM_edit_moh_firstcaller.checked = ( room_options.contains('M') ) ? true : false ;
	DOM_edit_callerMenu.checked = ( room_options.contains('s') ) ? true : false ;
	DOM_edit_announceCallers.checked = ( room_options.contains('I') ) ? true : false ;
	DOM_edit_quietMode.checked = ( room_options.contains('q') ) ? true : false ;
	DOM_edit_waitMarked.checked = ( room_options.contains('w') ) ? true : false ;
	DOM_edit_closeLastMarkedUser.checked = ( room_options.contains('x') ) ? true : false ;
	DOM_edit_recording.checked = ( room_options.contains('r') ) ? true : false ;
	DOM_edit_MeetMe_title.innerHTML = 'Edit Conference Bridge ' + EDIT_BRIDGE ;
	ASTGUI.feedback( { msg:"Edit 'Conference Bridge'", showfor:2 } );
	$(DOM_edit_MeetMeDiv).showWithBg();
};

var edit_meetMe_apply = function(){
	// Validations

	if ( !ASTGUI.checkRequiredFields(['edit_Ext']) ){
		return ;
	}
	if ( !ASTGUI.validateFields( ['edit_AdminPinCode', 'edit_PinCode', 'edit_adminExtension', 'edit_Ext' ] ) ){
		return ;
	}
	if( DOM_edit_PinCode.value.trim() && DOM_edit_PinCode.value == DOM_edit_AdminPinCode.value ) {
		ASTGUI.highlightField(DOM_edit_PinCode , "'PinCode' and 'Admin PinCode' must be different");
		return;
	}
	if( DOM_edit_PinCode.value.length > 10 ) {
		ASTGUI.highlightField(DOM_edit_PinCode , "'PinCode' can not be more than 10 digits");
		return;
	}
	if( DOM_edit_AdminPinCode.value.length > 10 ) {
		ASTGUI.highlightField( DOM_edit_AdminPinCode , "'Admin PinCode' can not be more than 10 digits");
		return;
	}
	if( DOM_edit_adminExtension.value.trim() && !DOM_edit_AdminPinCode.value.trim() ){
		ASTGUI.highlightField(DOM_edit_AdminPinCode , " Admin extension must have an 'Admin PinCode' ");
		return;
	}
	if( ASTGUI.getFieldValue(DOM_edit_adminExtension) && ASTGUI.getFieldValue(DOM_edit_adminExtension) == ASTGUI.getFieldValue(DOM_edit_Ext) ){
		ASTGUI.highlightField(DOM_edit_adminExtension , " Admin extension must be different from Room Extension");
		return;
	}
	// always create new bridge 
	// "exten => 6100,1,MeetMe(${EXTEN}|MI)" under [ASTGUI.contexts.CONFERENCES]
	// "conf = 6100,1234,1234" under [rooms] in meetme.conf
	var new_MeetmeOptions = [] ;

	if( DOM_edit_moh_firstcaller.checked) { new_MeetmeOptions.push('M'); }
	if( DOM_edit_callerMenu.checked) { new_MeetmeOptions.push('s'); }
	if( DOM_edit_announceCallers.checked) { new_MeetmeOptions.push('I'); }
	if( DOM_edit_quietMode.checked) { new_MeetmeOptions.push('q'); }
	if( DOM_edit_waitMarked.checked) { new_MeetmeOptions.push('w'); }
	if( DOM_edit_closeLastMarkedUser.checked) { new_MeetmeOptions.push('x'); }
	if( DOM_edit_recording.checked) { new_MeetmeOptions.push('r'); }
	if( DOM_edit_waitMarked.checked || DOM_edit_closeLastMarkedUser.checked ){
		if( !DOM_edit_adminExtension.value.trim() ){
			ASTGUI.highlightField(DOM_edit_adminExtension, "Please enter an Extension for marked/Admin users");
			return;
		}
	}

	var u = new listOfSynActions('extensions.conf') ;
	var w = new listOfSynActions('meetme.conf') ;

	var new_exten = DOM_edit_Ext.value ;
	var new_MeetmeString = new_exten + ",1,MeetMe(${EXTEN}," + new_MeetmeOptions.join('')  +")" ;
	if( ASTGUI.getFieldValue(DOM_edit_adminExtension) ){
		var new_MeetmeString_adminOptions = DOM_edit_adminExtension.value + ",1,MeetMe(" + new_exten + "," + new_MeetmeOptions.join('') + 'aA' +")" ;
	}else{
		var new_MeetmeString_adminOptions = '';
	}

	var new_passString = DOM_edit_Ext.value + ',' + DOM_edit_PinCode.value + ',' + DOM_edit_AdminPinCode.value ;

	if( isNewBridge == false ){ // delete/update old bridge values
		if( parent.sessionData.pbxinfo.conferences[EDIT_BRIDGE]['configOptions'] ){
			u.new_action('delete', ASTGUI.contexts.CONFERENCES, 'exten', '', parent.sessionData.pbxinfo.conferences[EDIT_BRIDGE]['configOptions'] );
		}
		if( parent.sessionData.pbxinfo.conferences[EDIT_BRIDGE]['adminOptions'] ){
			u.new_action('delete', ASTGUI.contexts.CONFERENCES, 'exten', '', parent.sessionData.pbxinfo.conferences[EDIT_BRIDGE]['adminOptions'] );
		}
		u.callActions();
		u.clearActions();
		if( parent.sessionData.pbxinfo.conferences[EDIT_BRIDGE]['pwdString'] ){
			w.new_action('delete', 'rooms', 'conf', '', parent.sessionData.pbxinfo.conferences[EDIT_BRIDGE]['pwdString'] );
		}
		w.callActions();
		w.clearActions();
		parent.sessionData.pbxinfo.conferences[EDIT_BRIDGE]['configOptions'] = new_MeetmeString ;
		parent.sessionData.pbxinfo.conferences[EDIT_BRIDGE]['pwdString'] = new_passString ;
		parent.sessionData.pbxinfo.conferences[EDIT_BRIDGE]['adminOptions'] = new_MeetmeString_adminOptions ;
	}else{
		var NU_EXT = ASTGUI.getFieldValue(DOM_edit_Ext);

		if( parent.miscFunctions.ifExtensionAlreadyExists(NU_EXT) ){
			ASTGUI.highlightField(DOM_edit_Ext , 'Extension already exists');
			parent.ASTGUI.dialog.hide();
			return;
		}

		var ADMIN_EXT = ASTGUI.getFieldValue(DOM_edit_adminExtension);
		if( ADMIN_EXT && parent.miscFunctions.ifExtensionAlreadyExists(ADMIN_EXT) ){
			ASTGUI.highlightField(DOM_edit_adminExtension , 'Extension already exists');
			parent.ASTGUI.dialog.hide();
			return;
		}

		if( ! ASTGUI.miscFunctions.isExtensionInRange( NU_EXT ,'mm_start','mm_end') ){
			ASTGUI.highlightField(DOM_edit_Ext , 'Extension is not in preferred range');
			parent.ASTGUI.dialog.hide();
			return;
		}

		parent.sessionData.pbxinfo.conferences[NU_EXT] = new ASTGUI.customObject ;
		parent.sessionData.pbxinfo.conferences[NU_EXT]['configOptions'] = new_MeetmeString ;
		parent.sessionData.pbxinfo.conferences[NU_EXT]['pwdString'] = new_passString ;
		parent.sessionData.pbxinfo.conferences[NU_EXT]['adminOptions'] = new_MeetmeString_adminOptions ;
	}

	u.new_action('append', ASTGUI.contexts.CONFERENCES, 'exten', new_MeetmeString );
	if( new_MeetmeString_adminOptions.trim().length ){
		u.new_action('append', ASTGUI.contexts.CONFERENCES, 'exten', new_MeetmeString_adminOptions );
	}
	w.new_action('append', 'rooms', 'conf', new_passString);

	u.callActions();
	w.callActions();
	ASTGUI.feedback( { msg:'Updated !', showfor:2, color:'green', bgcolor:'#FFFFFF' } );
	window.location.reload();
};

var localajaxinit = function(){
	top.document.title = 'Manage Conference Bridges' ;
	if( !ASTGUI.miscFunctions.alertIfRangeisNotdefined('mm_start','mm_end', 'Conferences') ){
		$('.top_buttons').hide();
		return;
	}
	loadDOMElements();
	updateMeetmesTable();
};
