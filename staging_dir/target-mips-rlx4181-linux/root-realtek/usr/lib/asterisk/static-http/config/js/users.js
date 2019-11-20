/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * users.html functions
 *
 * Copyright (C) 2006-2010, Digium, Inc.
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
var isNewUSER;
var EXTENSION_EDIT;
var zapchan_Before = '';
var MULTI_FIELDS = ['edit_check_multiple_obcid','edit_multiple_obcid','edit_check_multiple_dialplan','edit_multiple_dialplan','edit_check_multiple_hasvoicemail','edit_multiple_hasvoicemail','edit_check_multiple_vmsecret', 'edit_multiple_vmCHoice1', 'edit_multiple_vmCHoice2', 'edit_multiple_vmsecret', 'edit_check_multiple_hassip', 'edit_multiple_hassip', 'edit_check_multiple_hasiax', 'edit_multiple_hasiax', 'edit_check_multiple_allow',  'edit_multiple_codec_one', 'edit_multiple_codec_two', 'edit_multiple_codec_three', 'edit_multiple_codec_fourth', 'edit_multiple_codec_fifth', 'edit_check_multiple_flashrxflash',  'edit_multiple_flash', 'edit_multiple_rxflash', 'edit_check_multiple_secret', 'edit_check_multiple_iax', 'edit_multiple_seCHoice1', 'edit_multiple_seCHoice2', 'edit_multiple_secret', 'edit_check_multiple_nat',  'edit_multiple_nat', 'edit_check_multiple_canreinvite', 'edit_multiple_canreinvite', 'edit_check_multiple_dtmfmode',  'edit_multiple_dtmfmode', 'edit_check_multiple_insecure',  'edit_multiple_insecure', 'edit_check_multiple_3waycalling',  'edit_multiple_3waycalling', 'edit_check_multiple_indirectory',  'edit_multiple_indirectory', 'edit_check_multiple_callwaiting',  'edit_multiple_callwaiting', 'edit_check_multiple_cti',  'edit_multiple_cti', 'edit_check_multiple_isagent',  'edit_multiple_isagent', 'edit_check_multiple_pickupgroup',  'edit_multiple_pickupgroup', 'edit_multiple_requirecalltoken', 'edit_multiple_maxcallnumbers'];

var show_UserEdit_normal = function(){ // show_UserEdit_normal();
	$('#edit_User_Advanced_DIV').hideWithBg();
	$('#edit_userExtension_div').showWithBg();
};

var save_User_Advanced = function(){ // save_User_Advanced();
	parent.ASTGUI.dialog.waitWhile('Saving ..') ;

	try{
		ASTGUI.miscFunctions.empty_context({ filename:'users.conf', context : EXTENSION_EDIT, cb : function(){
			var x = new listOfActions('users.conf');

			var user_lines = ASTGUI.getFieldValue('edit_USER_Advanced_details') ;
			user_lines = user_lines.split('\n');
			user_lines.each( function(this_line){
				x.new_action( 'append', EXTENSION_EDIT , this_line.beforeChar('=') , this_line.afterChar('=') );
			});
			x.callActions( function(){
				ASTGUI.dialog.waitWhile('Updated User information <BR> Reloading GUI ... ');
				setTimeout( function(){ top.window.location.reload(); } , 2000 );
			});
		}});
	}catch(err){
		alert("Error saving User information");
		top.window.location.reload();
	}
};

var show_UserEdit_Advanced = function(){ // show_UserEdit_Advanced();
	parent.ASTGUI.dialog.waitWhile('Loading ..') ;
	$('#edit_User_Advanced_DIV').showWithBg();
	$('#edit_userExtension_div').hideWithBg();
	$('#edit_User_Advanced_DIV .dialog_title > span').html( 'Edit User ' + EXTENSION_EDIT + " -- Advanced");

	var textarea = _$('edit_USER_Advanced_details');
	textarea.value = '';
	var t = config2json({filename:'users.conf', usf:0});
	if( t.hasOwnProperty(EXTENSION_EDIT) ){
		//textarea.rows = t[EXTENSION_EDIT].length + 1 ;
		textarea.value = t[EXTENSION_EDIT].join('\n');
	}

	parent.ASTGUI.dialog.hide() ;
};

var initialize_formFields = function(){ // initialize_formFields();
	//Load dialplans into 'edit_user_dialplan'
	var dps = parent.pbx.call_plans.list() ;
	dps.each(function(plan){
		var t = plan.withOut(ASTGUI.contexts.CallingPlanPrefix);
		ASTGUI.selectbox.append( 'edit_user_dialplan' , t, plan);
		ASTGUI.selectbox.append( 'edit_multiple_dialplan' , t, plan);
	});
	// Load Analog stations into 'edit_fxs'
	ASTGUI.selectbox.append( 'edit_fxs' ,'None', '');
	parent.sessionData.FXS_PORTS_DETECTED.each( function(item) { ASTGUI.selectbox.append( 'edit_fxs' , 'Port ' + item, item); } );

	(function(){
		var R = [];
		R.push( {optionText:'None', optionValue :'' });
		for ( var r in parent.sessionData.listOfCodecs ){
			R.push( {optionText: parent.sessionData.listOfCodecs[r] , optionValue : r });
		}
		ASTGUI.selectbox.populateArray('codec_one', R);
		ASTGUI.selectbox.populateArray('codec_two', R);
		ASTGUI.selectbox.populateArray('codec_three', R);
		ASTGUI.selectbox.populateArray('codec_fourth', R);
		ASTGUI.selectbox.populateArray('codec_fifth', R);
		ASTGUI.selectbox.populateArray('edit_multiple_codec_one', R);
		ASTGUI.selectbox.populateArray('edit_multiple_codec_two', R);
		ASTGUI.selectbox.populateArray('edit_multiple_codec_three', R);
		ASTGUI.selectbox.populateArray('edit_multiple_codec_fourth', R);
		ASTGUI.selectbox.populateArray('edit_multiple_codec_fifth', R);
	})();

	ASTGUI.selectbox.populateOptions( 'edit_pickupgroup', 63 );
	ASTGUI.selectbox.populateOptions( 'edit_multiple_pickupgroup', 63 );

	ASTGUI.domActions.enableDisableByCheckBox( 'edit_hasSip' , ['edit_nat','edit_canreinvite','edit_dtmfmode','edit_insecure'] );

	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_obcid' , [ 'edit_multiple_obcid' ] );
	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_dialplan' , [ 'edit_multiple_dialplan' ] );
	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_hasvoicemail' , [ 'edit_multiple_hasvoicemail' ] );
	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_vmsecret' , [ 'edit_multiple_vmCHoice1', 'edit_multiple_vmCHoice2', 'edit_multiple_vmsecret'] );
	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_hassip' , [ 'edit_multiple_hassip' ] );
	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_hasiax' , [ 'edit_multiple_hasiax' ] );
	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_allow' , [ 'edit_multiple_codec_one', 'edit_multiple_codec_two', 'edit_multiple_codec_three' , 'edit_multiple_codec_fourth' , 'edit_multiple_codec_fifth'  ] );
	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_flashrxflash' , [ 'edit_multiple_flash', 'edit_multiple_rxflash'  ] );
	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_secret' , [ 'edit_multiple_seCHoice1', 'edit_multiple_seCHoice2', 'edit_multiple_secret' ] );
	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_iax' , [ 'edit_multiple_requirecalltoken' ,'edit_multiple_maxcallnumbers' ] );
	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_nat' , [ 'edit_multiple_nat' ] );
	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_canreinvite' , [ 'edit_multiple_canreinvite' ] );
	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_dtmfmode' , [ 'edit_multiple_dtmfmode' ] );
	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_insecure' , [ 'edit_multiple_insecure' ] );
	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_3waycalling' , [ 'edit_multiple_3waycalling' ] );
	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_indirectory' , [ 'edit_multiple_indirectory' ] );
	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_callwaiting' , [ 'edit_multiple_callwaiting' ] );
	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_cti' , [ 'edit_multiple_cti' ] );
	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_isagent' , [ 'edit_multiple_isagent' ] );
	ASTGUI.domActions.enableDisableByCheckBox( 'edit_check_multiple_pickupgroup' , [ 'edit_multiple_pickupgroup' ] );

	
};

var load_users_table = function(){ // load_users_table
	var TBL = _$('table_userslist') ;
	var addCell = ASTGUI.domActions.tr_addCell; // temporarily store the function
	var ul = parent.pbx.users.list();  ul = ul.sortNumbers( );
	if(!ul.length){
		ASTGUI.domActions.clear_table( TBL );
		var newRow = TBL.insertRow(-1);
		newRow.className = 'even';
		addCell( newRow , { html:'No users created !!'} );
		return ;
	}
	(function(){ // add first row
		var CA = document.createElement('input'); CA.type = 'checkbox' ; CA.id = 'CHECKALL' ;
		ASTGUI.events.add( CA , 'click' , function(){
			if( _$('CHECKALL').checked ) {
				ASTGUI.domActions.CheckAll('selected_extensions');
			}else{
				ASTGUI.domActions.unCheckAll('selected_extensions');
			}
		});

		var newRow = TBL.insertRow(-1);
		newRow.className = "frow";

		var newcell = newRow.insertCell( newRow.cells.length );
		newcell.width = '15px' ;
		newcell.appendChild( CA );

		addCell( newRow , { html:'Extension'} );
		addCell( newRow , { html:'Full Name'} );
		addCell( newRow , { html:'Port'} );
		addCell( newRow , { html:'SIP'} );
		addCell( newRow , { html:'IAX'} );
		addCell( newRow , { html:'DialPlan', onclickFunction: function(){ parent.miscFunctions.click_panel('dialplans.html'); } } );
		addCell( newRow , { html:'OutBound CID'} );
		addCell( newRow , { html:''} );
	})();

	ul.each(function(user){ // list each user in table
		var tmp = [] ;
		tmp[0] = "<span class='guiButton' onclick=\"EDIT_USER_FORM('" + user +"')\">Edit</span>&nbsp;"
			+ "<span class='guiButtonDelete' onclick=\"DELETE_USER('" + user +"')\">Delete</span>" ;
		tmp[1] = parent.sessionData.pbxinfo.GLOBALS[ASTGUI.globals.obcidUsrPrefix + user] || '<span class=\'no_obcid\'>none</span>' ;

		var ud = parent.sessionData.pbxinfo.users[user]; // temporarily store all details of this user
		var newRow = TBL.insertRow(-1); //newRow.className = "frow";
		newRow.className = ((TBL.rows.length)%2==1)?'odd':'even';
		addCell( newRow , { html: "<input type=checkbox  class='selected_extensions' value='"+ user +"'>" } );
		addCell( newRow , { html: user } );
		addCell( newRow , { html: ud.getProperty('fullname') || '--' } );
		addCell( newRow , { html: ud.getProperty(top.sessionData.DahdiChannelString) || '--' } );
		addCell( newRow , { html: ( ud.getProperty('hassip').isAstTrue() ) ? 'Yes' : '--' , align:'center'} );
		addCell( newRow , { html: ( ud.getProperty('hasiax').isAstTrue() ) ? 'Yes' : '--' , align:'center'} );

		var tmp_this_dp = ud.getProperty('context').withOut(ASTGUI.contexts.CallingPlanPrefix) ;
		if( !tmp_this_dp || !parent.sessionData.pbxinfo.callingPlans[ud.getProperty('context')] ){
			addCell( newRow , { html: '<font color=red>No DialPlan assigned</font>'});
		}else{
			addCell( newRow , { html: tmp_this_dp } );
		}

		addCell( newRow , { html:tmp[1], align:'center'} );
		addCell( newRow , { html:tmp[0], align:'center'} );
	});
};

var RESET_USER_FORM_FIELDS = function(){ // RESET_USER_FORM_FIELDS();
	ASTGUI.resetTheseFields( ['new_ext','edit_fullname','edit_user_dialplan','edit_OutBoundCallerid','edit_hasvoicemail','edit_vmsecret','edit_email','edit_hasSip','edit_hasIax','edit_fxs','edit_flash','edit_rxflash','codec_one','codec_two','codec_three','codec_fourth','codec_fifth','macaddress','linenumber','edit_secret','edit_nat','edit_canreinvite','edit_dtmfmode','edit_insecure', 'edit_mwifrom', 'edit_3wayCalling','edit_inDirectory','edit_callWaiting','edit_cti','edit_isagent','edit_pickupgroup','edit_linekeys','edit_maxcallnumbers' ,'edit_requirecalltoken'] );
	_$('edit_fxs').selectedIndex = 0; _$('codec_one').selectedIndex = 0; _$('codec_two').selectedIndex = 0; _$('codec_three').selectedIndex = 0; _$('codec_fourth').selectedIndex = 0; 
	_$('codec_fifth').selectedIndex = 0; _$('linenumber').selectedIndex = 0; _$('edit_pickupgroup').selectedIndex = 0;

	if (parent.sessionData.PLATFORM.isABE || parent.sessionData.PLATFORM.isAA50) {
		$('#mwi_from').show();
	} else {
		$('#mwi_from').hide();
	}

	ASTGUI.updateFieldToValue( 'edit_user_dialplan', parent.sessionData.GUI_PREFERENCES.getProperty('default_dialplan') );

	if( isNewUSER == false){
		_$('new_ext').disabled = true;
		var uinfo = parent.sessionData.pbxinfo.users[EXTENSION_EDIT];

		if( uinfo.getProperty('mailbox').contains('MailboxStore') ){
			$('#tmp_hideIfRmwi').hide();
		}else{
			$('#tmp_hideIfRmwi').show();
		}

		ASTGUI.updateFieldToValue( 'new_ext', EXTENSION_EDIT );
		ASTGUI.updateFieldToValue( 'edit_fullname', uinfo.getProperty('fullname') );
		ASTGUI.updateFieldToValue( 'edit_user_dialplan', uinfo.getProperty('context') );
		$('#edit_callerid_span').html(EXTENSION_EDIT);
		ASTGUI.updateFieldToValue( 'edit_OutBoundCallerid', parent.astgui_manageusers.getOBCID_user(EXTENSION_EDIT) );
		ASTGUI.updateFieldToValue( 'edit_hasvoicemail', uinfo.getProperty('hasvoicemail') );
		ASTGUI.updateFieldToValue( 'edit_vmsecret', uinfo.getProperty('vmsecret') );
		ASTGUI.updateFieldToValue( 'edit_email', uinfo.getProperty('email') );
		ASTGUI.updateFieldToValue( 'edit_hasSip', uinfo.getProperty('hassip') );
		ASTGUI.updateFieldToValue( 'edit_hasIax', uinfo.getProperty('hasiax') );
		ASTGUI.updateFieldToValue( 'edit_fxs', uinfo.getProperty(top.sessionData.DahdiChannelString) );
		ASTGUI.updateFieldToValue( 'edit_flash', uinfo.getProperty('flash') );
		ASTGUI.updateFieldToValue( 'edit_rxflash', uinfo.getProperty('rxflash') );
		if( uinfo.getProperty('allow') == 'all'){
			ASTGUI.updateFieldToValue( 'codec_one', 'ulaw' );
			ASTGUI.updateFieldToValue( 'codec_two', 'alaw' );
			ASTGUI.updateFieldToValue( 'codec_three', 'gsm' );
			ASTGUI.updateFieldToValue( 'codec_fourth', 'g726' );
			ASTGUI.updateFieldToValue( 'codec_fifth', 'g722' );
		}else{
			var codecs_tmp = uinfo.getProperty('allow').split(',') ;
			ASTGUI.updateFieldToValue( 'codec_one', (codecs_tmp[0] && codecs_tmp[0].trim()) ||'' );
			ASTGUI.updateFieldToValue( 'codec_two', (codecs_tmp[1] && codecs_tmp[1].trim()) ||'' );
			ASTGUI.updateFieldToValue( 'codec_three', (codecs_tmp[2] && codecs_tmp[2].trim()) ||'' );
			ASTGUI.updateFieldToValue( 'codec_fourth', (codecs_tmp[3] && codecs_tmp[3].trim()) ||'' );
			ASTGUI.updateFieldToValue( 'codec_fifth', (codecs_tmp[4] && codecs_tmp[4].trim()) ||'' );
		}
		ASTGUI.updateFieldToValue( 'macaddress', uinfo.getProperty('macaddress') );
		ASTGUI.updateFieldToValue( 'linenumber', uinfo.getProperty('linenumber') );
		ASTGUI.updateFieldToValue( 'edit_linekeys', uinfo.getProperty('LINEKEYS') );
		ASTGUI.updateFieldToValue( 'edit_secret', uinfo.getProperty('secret') );
		ASTGUI.updateFieldToValue( 'edit_nat', uinfo.getProperty('nat') );
		ASTGUI.updateFieldToValue( 'edit_canreinvite', uinfo.getProperty('canreinvite') );
		ASTGUI.updateFieldToValue( 'edit_dtmfmode', uinfo.getProperty('dtmfmode') );
		ASTGUI.updateFieldToValue( 'edit_insecure', uinfo.getProperty('insecure') );
		ASTGUI.updateFieldToValue( 'edit_mwifrom', uinfo.getProperty('mwi_from') );
		ASTGUI.updateFieldToValue( 'edit_3wayCalling', uinfo.getProperty('threewaycalling') );
		ASTGUI.updateFieldToValue( 'edit_inDirectory', uinfo.getProperty('hasdirectory') );
		ASTGUI.updateFieldToValue( 'edit_callWaiting', uinfo.getProperty('callwaiting') );
		ASTGUI.updateFieldToValue( 'edit_cti', uinfo.getProperty('hasmanager') );
		ASTGUI.updateFieldToValue( 'edit_isagent', uinfo.getProperty('hasagent') );
		ASTGUI.updateFieldToValue( 'edit_pickupgroup', uinfo.getProperty('pickupgroup') );
		ASTGUI.updateFieldToValue( 'edit_maxcallnumbers', uinfo.getProperty('maxcallnumbers') );
		ASTGUI.updateFieldToValue( 'edit_requirecalltoken', uinfo.getProperty('requirecalltoken') );
		zapchan_Before = uinfo.getProperty(top.sessionData.DahdiChannelString);
	}else{
			_$('new_ext').disabled = false;
			ASTGUI.updateFieldToValue( 'codec_one', 'ulaw' );
			ASTGUI.updateFieldToValue( 'codec_two', 'gsm' );
			ASTGUI.updateFieldToValue( 'edit_hasSip', 'yes' );
			ASTGUI.updateFieldToValue( 'edit_hasIax', 'yes' );
			ASTGUI.updateFieldToValue( 'edit_flash', '750' );
			ASTGUI.updateFieldToValue( 'edit_rxflash', '1250' );
			var tmp_allextensions = ASTGUI.cloneObject( parent.miscFunctions.getAllExtensions() );
			var tmp_newEXT = tmp_allextensions.firstAvailable( parent.sessionData.GUI_PREFERENCES.getProperty('ue_start') );
			ASTGUI.updateFieldToValue( 'new_ext', tmp_newEXT );
			$('#edit_callerid_span').html(tmp_newEXT);
	}

	_$('edit_hasSip').updateStatus();
};

var NEW_USER_FORM = function(){ // NEW_USER_FORM();
	var dps = parent.pbx.call_plans.list() ;
	if(!dps.length){
		ASTGUI.yesOrNo (
			{	msg: "No DialPlans defined !! <BR><BR> A dialplan is required for creating new users. <BR> You will now be redirected to the 'Manage DialPlans' page.",
				ifyes: function(){ parent.miscFunctions.click_panel('dialplans.html'); } ,
				ifno: function(){ } ,
				btnYes_text :'&nbsp;&nbsp;Ok&nbsp;&nbsp;',
				title: 'No DialPlans found !!',
				hideNo: true
			}
		);
		return;
	}

	isNewUSER = true;
	EXTENSION_EDIT = '';
	zapchan_Before = '';
	_$('edit_userExtension_div_title').innerHTML = 'Create New User';
	ASTGUI.feedback( { msg: 'Create New User !', showfor: 2 , color: 'green', bgcolor: '#FFFFFF' } );
	RESET_USER_FORM_FIELDS();
	$('#edit_fxs').change(); /* disable/enabled 3-way/callwaiting based on analog */
	$('#edit_userExtension_div').showWithBg();
	$('#User_AdvancedEditButton').hide();
};

var EDIT_USER_FORM = function(a){ // EDIT_USER_FORM();
	$('#User_AdvancedEditButton').show();
	isNewUSER = false ;
	EXTENSION_EDIT = a ;
	ASTGUI.feedback( { msg: 'Edit User Extension !', showfor: 2 , color: 'green', bgcolor: '#FFFFFF' } );
	_$('edit_userExtension_div_title').innerHTML = 'Edit User Extension - ' + a ;
	RESET_USER_FORM_FIELDS();
	$('#edit_fxs').change(); /* disable/enabled 3-way/callwaiting based on analog */
	$('#edit_userExtension_div').showWithBg();
	try{_$('edit_userExtension_div').scrollIntoView(true);}catch(err){}
};
	
var getSelectedUsers = function(){ // getSelectedUsers();
	return ASTGUI.domActions.get_checked('selected_extensions') ; 
};

var EDIT_SELECTEDUSERs_FORM = function(){ // EDIT_SELECTEDUSERs_FORM();
	var sel_users = getSelectedUsers();
	if(!sel_users.length){
		ASTGUI.feedback( { msg: ' Please select one or more users to modify !', showfor: 4 });
		return;
	}
	ASTGUI.resetTheseFields( MULTI_FIELDS );
	MULTI_FIELDS.each( function(this_field_str){
		try{
			_$(this_field_str).updateStatus();
		}catch(err){

		}
	});

	_$('edit_multiple_codec_one').selectedIndex = 0
	_$('edit_multiple_codec_two').selectedIndex = 0
	_$('edit_multiple_codec_three').selectedIndex = 0
	_$('edit_multiple_codec_fourth').selectedIndex = 0
	_$('edit_multiple_codec_fifth').selectedIndex = 0
	$('#edit_selectedExtensions_div').showWithBg();
};

var DELETE_USER = function(a){ // DELETE_USER();
	var reload_page = function(){
		ASTGUI.feedback( { msg:'Deleted User - ' + a , showfor:2,  color:'#a02920' } );
		parent.ASTGUI.dialog.hide();
		window.location.reload();
	};
	if (!confirm('Are you sure you want to delete the selected user - ' + a + ' ?')) { return; }
	var yn = confirm("Delete user's voicemail box too ?" );
	parent.ASTGUI.dialog.waitWhile(' deleting user ' + a );
	parent.pbx.users.remove({user: a , vmdel: yn , callback: reload_page});
};

var DELETE_SELECTED_USERS = function(){ // DELETE_SELECTED_USERS();
	var sel_users = getSelectedUsers();
	if(!sel_users.length){
		ASTGUI.feedback( { msg: ' Please select one or more users to delete !', showfor: 4 });
		return;
	}
	if( !confirm('Are you sure you want to delete the selected users - ' + sel_users.join(',') + ' ? \n\n Note: This would delete any voicemails stored in the users mailboxes.') ) { return; }

	var after_deletingFirstUser = function(){
		sel_users.removeFirst();
		if(!sel_users.length ){
			ASTGUI.feedback( { msg:'Deleted selected Users !!', showfor:2,  color:'#a02920' } );
			parent.ASTGUI.dialog.hide();
			window.location.reload();
		}else{
			delete_firstUser();
		}
	};

	var delete_firstUser = function(){
		//var yn = confirm("Delete voicemail box for user " + sel_users[0] + " ?" );
		parent.ASTGUI.dialog.waitWhile(' deleting user ' + sel_users[0] );
		parent.pbx.users.remove({user: sel_users[0] , vmdel: true , callback: after_deletingFirstUser});
	};

	parent.ASTGUI.dialog.waitWhile(' Deleting selected users .... ');
	delete_firstUser();
};

var check_duplicate_lineNumber = function(){ // check_duplicate_lineNumber()  check if another user has the same mac address and line number
	var ul = parent.pbx.users.list();
	for( var f=0 ; f < ul.length ; f++ ){
		var uinfo = parent.sessionData.pbxinfo.users[ ul[f] ];
		if( !isNewUSER && EXTENSION_EDIT == ul[f] ){
			continue;
		}
		var ma = ASTGUI.getFieldValue('macaddress');
		var ln = ASTGUI.getFieldValue('linenumber');
		if( ma && ma == uinfo.getProperty('macaddress') && ln == uinfo.getProperty('linenumber') ){
			return false;
		}
	}
	return true;
};

var SAVE_USER_FORM = function(){ // SAVE_USER_FORM();
	if ( !ASTGUI.checkRequiredFields(['new_ext']) ) return ;
	if ( !ASTGUI.validateFields(['new_ext'] ) ) return ;
	if ( $('edit_maxcallnumbers').attr('value') && !ASTGUI.validateFields(['edit_maxcallnumbers'] ) ){
		return;
	}

	if ( ASTGUI.getFieldValue('edit_hasSip').isAstTrue() && !ASTGUI.getFieldValue('edit_fxs') && !ASTGUI.getFieldValue('macaddress') ){
		ASTGUI.updateFieldToValue( 'macaddress',  ASTGUI.getFieldValue('new_ext') );
	}else if( ASTGUI.getFieldValue('edit_hasSip').isAstTrue() ){
		if ( !ASTGUI.validateFields(['macaddress'])) return ;
	}

	if( ! check_duplicate_lineNumber() ){
		ASTGUI.highlightField('linenumber', "duplicate 'Line Number'");
		return;
	}
	var tmp = ASTGUI.getFieldValue('edit_OutBoundCallerid') ;
	if( tmp && tmp.length < 2 ){
		ASTGUI.highlightField('edit_OutBoundCallerid', "OutBound CallerId should be atleast 2 digits");
		return;
	}
	if( tmp && tmp.length > 19  ){
		ASTGUI.highlightField('edit_OutBoundCallerid', "Too many digits in OutBound CallerId");
		return;
	}
	if(/[^\d-\(\) \+]/.test(tmp)){
		ASTGUI.highlightField('edit_OutBoundCallerid', "Please use numeric characters only.");
		return;
	}
	tmp = '';
	if( !_$('edit_hasSip').checked && !_$('edit_hasIax').checked && !_$('edit_fxs').value ){
		ASTGUI.feedback( { msg:'You need to choose a technology or assign an analog station for this user extension.' , showfor: 3, color: 'red' } );
		return;
	}

	if( (ASTGUI.getFieldValue('edit_hasSip').isAstTrue() ||  ASTGUI.getFieldValue('edit_hasIax').isAstTrue() ) && !ASTGUI.getFieldValue('codec_one') && !ASTGUI.getFieldValue('codec_two') && !ASTGUI.getFieldValue('codec_three') && !ASTGUI.getFieldValue('codec_fourth') && !ASTGUI.getFieldValue('codec_fifth') ){
		ASTGUI.feedback( { msg:'You need to choose at least one codec !' , showfor: 5, color: 'red' } );
		return;
	}

	if( isNewUSER  ){
			var NU_EXT = ASTGUI.getFieldValue('new_ext');
			if( parent.miscFunctions.ifExtensionAlreadyExists(NU_EXT) ){
				ASTGUI.highlightField('new_ext', 'Extension already exists');
				parent.ASTGUI.dialog.hide();
				return;
			}
			if( ! ASTGUI.miscFunctions.isExtensionInRange( NU_EXT ,'ue_start','ue_end') ){
				ASTGUI.highlightField('new_ext' , 'Extension is not in preferred range');
				parent.ASTGUI.dialog.hide();
				return;
			}
			var cb = function(){
				// Now we will call this function again , to update the rest of the User Properties
				EXTENSION_EDIT = NU_EXT ;
				isNewUSER = false ;
				setTimeout( function(){  SAVE_USER_FORM();  } , 500 );
			};
			var newuser_settings = ASTGUI.toCustomObject( { fullname : ASTGUI.getFieldValue('edit_fullname'), registersip: 'no', host: 'dynamic', callgroup : '1' } );
			parent.ASTGUI.dialog.waitWhile(' Saving... ');
			top.pbx.users.add( NU_EXT, newuser_settings, cb);
			return;
	}else{
		///////////////////
		parent.ASTGUI.dialog.waitWhile(' Saving... ');
		var u = EXTENSION_EDIT;
		var tmp_obj = {};
		var x = new listOfActions();
		x.filename('users.conf');
		x.new_action('update', u, 'fullname', 		ASTGUI.getFieldValue('edit_fullname') );	tmp_obj['fullname'] = ASTGUI.getFieldValue('edit_fullname') ;
		x.new_action('update', u, 'context', 		ASTGUI.getFieldValue('edit_user_dialplan') );	tmp_obj['context'] = ASTGUI.getFieldValue('edit_user_dialplan') ;
		x.new_action('update', u, 'cid_number', 	EXTENSION_EDIT );	tmp_obj['cid_number'] = EXTENSION_EDIT ;
		x.new_action('update', u, 'hasvoicemail',	ASTGUI.getFieldValue('edit_hasvoicemail') );	tmp_obj['hasvoicemail'] = ASTGUI.getFieldValue('edit_hasvoicemail') ;
		x.new_action('update', u, 'vmsecret', 		ASTGUI.getFieldValue('edit_vmsecret') );	tmp_obj['vmsecret'] = ASTGUI.getFieldValue('edit_vmsecret') ;
		x.new_action('update', u, 'email', 		ASTGUI.getFieldValue('edit_email') );		tmp_obj['email'] = ASTGUI.getFieldValue('edit_email') ;
		x.new_action('update', u, 'threewaycalling',	ASTGUI.getFieldValue('edit_3wayCalling') );	tmp_obj['threewaycalling'] = ASTGUI.getFieldValue('edit_3wayCalling') ;
		x.new_action('update', u, 'hasdirectory', 	ASTGUI.getFieldValue('edit_inDirectory') );	tmp_obj['hasdirectory'] = ASTGUI.getFieldValue('edit_inDirectory') ;
		x.new_action('update', u, 'callwaiting', 	ASTGUI.getFieldValue('edit_callWaiting') );	tmp_obj['callwaiting'] = ASTGUI.getFieldValue('edit_callWaiting') ;
		x.new_action('update', u, 'hasmanager', 	ASTGUI.getFieldValue('edit_cti') );		tmp_obj['hasmanager'] = ASTGUI.getFieldValue('edit_cti') ;
		if( ASTGUI.getFieldValue('edit_cti') == 'yes' ){
			var tmp_permisions = 'system,call,log,verbose,command,agent,user,config,originate' ;
			x.new_action('update', u, 'managerread', tmp_permisions); 				tmp_obj['managerread'] = tmp_permisions ;
			x.new_action('update', u, 'managerwrite', tmp_permisions);				tmp_obj['managerwrite'] = tmp_permisions ;
		}else{
			x.new_action('delete', u, 'managerread', '' , '' );					tmp_obj['managerread'] = '';
			x.new_action('delete', u, 'managerwrite', '' , '' );					tmp_obj['managerwrite'] = '';
		}
		x.new_action('update', u, 'hasagent', 		ASTGUI.getFieldValue('edit_isagent') );		tmp_obj['hasagent'] = ASTGUI.getFieldValue('edit_isagent') ;
		x.new_action('update', u, 'hassip', 		ASTGUI.getFieldValue('edit_hasSip') );		tmp_obj['hassip'] = ASTGUI.getFieldValue('edit_hasSip') ;
		x.new_action('update', u, 'hasiax', 		ASTGUI.getFieldValue('edit_hasIax') );		tmp_obj['hasiax'] = ASTGUI.getFieldValue('edit_hasIax') ;
		x.new_action('update', u, 'secret', 		ASTGUI.getFieldValue('edit_secret') );		tmp_obj['secret'] = ASTGUI.getFieldValue('edit_secret') ;
		x.new_action('update', u, 'nat', 		ASTGUI.getFieldValue('edit_nat') );		tmp_obj['nat'] = ASTGUI.getFieldValue('edit_nat') ;
		x.new_action('update', u, 'canreinvite', 	ASTGUI.getFieldValue('edit_canreinvite') );	tmp_obj['canreinvite'] = ASTGUI.getFieldValue('edit_canreinvite') ;
		x.new_action('update', u, 'dtmfmode', 		ASTGUI.getFieldValue('edit_dtmfmode') );	tmp_obj['dtmfmode'] = ASTGUI.getFieldValue('edit_dtmfmode') ;
		x.new_action('update', u, 'insecure', 		ASTGUI.getFieldValue('edit_insecure') );	tmp_obj['insecure'] = ASTGUI.getFieldValue('edit_insecure') ;
	if (parent.sessionData.PLATFORM.isABE || parent.sessionData.PLATFORM.isAA50) {
		x.new_action('update', u, 'mwi_from', 		ASTGUI.getFieldValue('edit_mwifrom') );	tmp_obj['mwi_from'] = ASTGUI.getFieldValue('edit_mwifrom') ;
	}
		x.new_action('update', u, 'pickupgroup', 	ASTGUI.getFieldValue('edit_pickupgroup') );	tmp_obj['pickupgroup'] = ASTGUI.getFieldValue('edit_pickupgroup') ;
		if (ASTGUI.getFieldValue('edit_requirecalltoken') == ''){
			x.new_action('delete', u, 'requirecalltoken','');	tmp_obj['requirecalltoken'] = '';
		} else {
			x.new_action('update', u, 'requirecalltoken',	ASTGUI.getFieldValue('edit_requirecalltoken') );	tmp_obj['requirecalltoken'] = ASTGUI.getFieldValue('edit_requirecalltoken') ;
		}
		if(ASTGUI.getFieldValue('edit_maxcallnumbers') ){
			x.new_action('update', u, 'maxcallnumbers', 	ASTGUI.getFieldValue('edit_maxcallnumbers') );	tmp_obj['maxcallnumbers'] = ASTGUI.getFieldValue('edit_maxcallnumbers') ;
		}else{
			x.new_action('delete', u, 'maxcallnumbers', '');	tmp_obj['maxcallnumbers'] = '';
		}

		var tmp_cl = parent.sessionData.pbxinfo.users[u].getProperty('call-limit') ;
		if( !tmp_cl ){
			x.new_action('update', u, 'call-limit', '100' );
			tmp_obj['call-limit'] = '100' ;
		}

		if( zapchan_Before != ASTGUI.getFieldValue('edit_fxs') ){ 	top.cookies.set( 'require_restart' , 'yes' );	}
		(function(){
			var fs = ASTGUI.getFieldValue('edit_fxs');
			if(fs){
				var tmp_flash = ASTGUI.getFieldValue('edit_flash') || '750';
				var tmp_rxflash = ASTGUI.getFieldValue('edit_rxflash') || '1250';
				var sg = (parent.sessionData.PORTS_SIGNALLING.ls.contains(fs)) ? 'fxo_ls':'fxo_ks' ;

				x.new_action('delete', u, top.sessionData.DahdiChannelString , '' ) ;
				x.new_action('append', u, top.sessionData.DahdiChannelString, fs );					tmp_obj[top.sessionData.DahdiChannelString] = fs ;
				x.new_action('update', u, 'signalling', sg) ;					tmp_obj['signalling'] = sg ;
				x.new_action('update', u, 'flash',  tmp_flash) ;				tmp_obj['flash'] = tmp_flash ;
				x.new_action('update', u, 'rxflash', tmp_rxflash ) ;				tmp_obj['rxflash'] = tmp_rxflash ;
			}else{
				x.new_action('delete', u, top.sessionData.DahdiChannelString, '') ;					tmp_obj[top.sessionData.DahdiChannelString] = '' ;
			}

			var codecs = '';
			if( ASTGUI.getFieldValue('codec_one') ){ codecs = codecs + ASTGUI.getFieldValue('codec_one') }
			if( ASTGUI.getFieldValue('codec_two') ){ codecs = codecs + ',' + ASTGUI.getFieldValue('codec_two') }
			if( ASTGUI.getFieldValue('codec_three') ){ codecs = codecs + ',' + ASTGUI.getFieldValue('codec_three') }
			if( ASTGUI.getFieldValue('codec_fourth') ){ codecs = codecs + ',' + ASTGUI.getFieldValue('codec_fourth') }
			if( ASTGUI.getFieldValue('codec_fifth') ){ codecs = codecs + ',' + ASTGUI.getFieldValue('codec_fifth') }
			x.new_action('delete', u, 'disallow', '') ;
			x.new_action('delete', u, 'allow', '') ;
			x.new_action('append', u, 'disallow', 'all') ;					tmp_obj['disallow'] = 'all' ;
			if(!codecs){
				x.new_action('append', u, 'allow', 'all' ) ;					tmp_obj['allow'] = 'all' ;
			}else{
				x.new_action('append', u, 'allow', codecs ) ;					tmp_obj['allow'] = codecs ;
			}
		})();
		if( _$('macaddress').value ){
			x.new_action('update', u, 'macaddress', ASTGUI.getFieldValue('macaddress') );		tmp_obj['macaddress'] = ASTGUI.getFieldValue('macaddress');
			x.new_action('update', u, 'autoprov', 'yes');						tmp_obj['autoprov'] = 'yes';
			x.new_action('update', u, 'label', u );							tmp_obj['label'] = u;
			x.new_action('update', u, 'linenumber', ASTGUI.getFieldValue('linenumber') );		tmp_obj['linenumber'] = ASTGUI.getFieldValue('linenumber');
			x.new_action('update', u, 'LINEKEYS', ASTGUI.getFieldValue('edit_linekeys') );		tmp_obj['LINEKEYS'] = ASTGUI.getFieldValue('edit_linekeys');
		}else{
			x.new_action('update', u, 'autoprov', 'no');						tmp_obj['autoprov'] = 'no';
			x.new_action('update', u, 'label', '' );						tmp_obj['label'] = '';
			x.new_action('update', u, 'macaddress', ASTGUI.getFieldValue('macaddress') );		tmp_obj['macaddress'] = ASTGUI.getFieldValue('macaddress');
			x.new_action('update', u, 'linenumber', ASTGUI.getFieldValue('linenumber') );		tmp_obj['linenumber'] = ASTGUI.getFieldValue('linenumber');
			x.new_action('update', u, 'LINEKEYS', ASTGUI.getFieldValue('edit_linekeys') );		tmp_obj['LINEKEYS'] = ASTGUI.getFieldValue('edit_linekeys');
		}
		var after = function(){
			parent.sessionData.pbxinfo.users[u].updateProperties(tmp_obj); // update all the edited values
			var v = ASTGUI.globals.obcidUsrPrefix + u;
			var w = ASTGUI.getFieldValue('edit_OutBoundCallerid') ;
			if( w ){
				if( ASTGUI.updateaValue({ file: 'extensions.conf', context: 'globals', variable: v , value:w }) ){
					try{ parent.sessionData.pbxinfo['GLOBALS'][v] = w; } catch(err){ }
				}
			}else{
				var mm = new listOfSynActions('extensions.conf') ;
				mm.new_action('delete', 'globals', v , '');
				mm.callActions();
				try{ delete parent.sessionData.pbxinfo['GLOBALS'][v] } catch(err){ }
			}
			parent.ASTGUI.dialog.hide();
			ASTGUI.feedback( { msg:'Updated !', showfor:2, color:'green' } );
			window.location.reload();
		};
	
		x.callActions(after);
		///////////////////
	}

// END OF SAVE_USER_FORM();
};
	
var save_multiple_users = function(){ // save_multiple_users()
	var users = ASTGUI.domActions.get_checked('selected_extensions') ;
	var x = new listOfActions('users.conf');
	var merge_list = {};
	var OBCID_LIST = {};

	users.each( function( u ) {
		///// ***************** /////
		var tmp_obj = {};
		//'edit_check_multiple_obcid' , 'edit_multiple_obcid' ,  
		if ( _$('edit_check_multiple_obcid').checked  ){
			OBCID_LIST[ASTGUI.globals.obcidUsrPrefix + u] = ASTGUI.getFieldValue('edit_multiple_obcid') ;
		}

		if( _$('edit_check_multiple_dialplan').checked ){
			x.new_action('update', u , 'context',  ASTGUI.getFieldValue('edit_multiple_dialplan') );
			tmp_obj['context'] = ASTGUI.getFieldValue('edit_multiple_dialplan') ;
		}
		if( _$('edit_check_multiple_hasvoicemail').checked ){
			x.new_action('update', u, 'hasvoicemail',  ASTGUI.getFieldValue('edit_multiple_hasvoicemail') );
			tmp_obj['hasvoicemail'] = ASTGUI.getFieldValue('edit_multiple_hasvoicemail') ;
		}
		if( _$('edit_check_multiple_vmsecret').checked ){
			if( _$('edit_multiple_vmCHoice1').checked ){
				x.new_action('update', u, 'vmsecret',  ASTGUI.getFieldValue('edit_multiple_vmsecret') );
				tmp_obj['vmsecret'] = ASTGUI.getFieldValue('edit_multiple_vmsecret') ;
			}
			if( _$('edit_multiple_vmCHoice2').checked ){
				x.new_action('update', u, 'vmsecret',  u );
				tmp_obj['vmsecret'] = u ;
			}
		}
		if( _$('edit_check_multiple_hassip').checked ){
			x.new_action('update', u, 'hassip',  ASTGUI.getFieldValue('edit_multiple_hassip') );
			tmp_obj['hassip'] = ASTGUI.getFieldValue('edit_multiple_hassip') ;
		}
		if( _$('edit_check_multiple_hasiax').checked ){
			x.new_action('update', u, 'hasiax',  ASTGUI.getFieldValue('edit_multiple_hasiax') );
			tmp_obj['hasiax'] = ASTGUI.getFieldValue('edit_multiple_hasiax') ;
		}
		if( _$('edit_check_multiple_allow').checked ){
			var codecs = '';
			if( ASTGUI.getFieldValue('edit_multiple_codec_one') ){ codecs = codecs + ASTGUI.getFieldValue('edit_multiple_codec_one') }
			if( ASTGUI.getFieldValue('edit_multiple_codec_two') ){ codecs = codecs + ',' + ASTGUI.getFieldValue('edit_multiple_codec_two') }
			if( ASTGUI.getFieldValue('edit_multiple_codec_three') ){ codecs = codecs + ',' + ASTGUI.getFieldValue('edit_multiple_codec_three') }
			if( ASTGUI.getFieldValue('edit_multiple_codec_fourth') ){ codecs = codecs + ',' + ASTGUI.getFieldValue('edit_multiple_codec_fourth') }
			if( ASTGUI.getFieldValue('edit_multiple_codec_fifth') ){ codecs = codecs + ',' + ASTGUI.getFieldValue('edit_multiple_codec_fifth') }
			x.new_action('update', u, 'allow',  codecs);
			tmp_obj['allow'] = codecs ;
		}
		if( _$('edit_check_multiple_flashrxflash').checked ){
			x.new_action('update', u, 'flash',  ASTGUI.getFieldValue('edit_multiple_flash') );
			tmp_obj['flash'] = ASTGUI.getFieldValue('edit_multiple_flash') ;
			x.new_action('update', u, 'rxflash',  ASTGUI.getFieldValue('edit_multiple_rxflash') );
			tmp_obj['rxflash'] = ASTGUI.getFieldValue('edit_multiple_rxflash') ;
		}
		if( _$('edit_check_multiple_secret').checked ){
			if(ASTGUI.getFieldValue('edit_multiple_seCHoice1') == 'on'){
				x.new_action('update', u, 'secret',  ASTGUI.getFieldValue('edit_multiple_secret') );
				tmp_obj['secret'] = ASTGUI.getFieldValue('edit_multiple_secret') ;
			}else if(ASTGUI.getFieldValue('edit_multiple_seCHoice2') == 'on'){
				x.new_action('update', u, 'secret',  '' );
				tmp_obj['secret'] = '' ;
			}
		}
		if( _$('edit_check_multiple_iax').checked ){
			if ( ASTGUI.getFieldValue('edit_multiple_maxcallnumbers') ){
				if ( !ASTGUI.validateFields(['edit_multiple_maxcallnumbers'] ) ){
					return;
				}else{
					x.new_action('update', u, 'maxcallnumbers',  ASTGUI.getFieldValue('edit_multiple_maxcallnumbers') );
					tmp_obj['maxcallnumbers'] = ASTGUI.getFieldValue('edit_multiple_maxcallnumbers') ;
				}
			}else{
					x.new_action('delete', u, 'maxcallnumbers', '' );
					tmp_obj['maxcallnumbers'] = '';
			}
			if( ASTGUI.getFieldValue('edit_multiple_requirecalltoken') != ''){
				x.new_action('update', u, 'requirecalltoken',  ASTGUI.getFieldValue('edit_multiple_requirecalltoken') );
				tmp_obj['requirecalltoken'] = ASTGUI.getFieldValue('edit_multiple_requirecalltoken') ;
			}
		}
		if( _$('edit_check_multiple_nat').checked ){
			x.new_action('update', u, 'nat',  ASTGUI.getFieldValue('edit_multiple_nat') );
			tmp_obj['nat'] = ASTGUI.getFieldValue('edit_multiple_nat') ;
		}
		if( _$('edit_check_multiple_canreinvite').checked ){
			x.new_action('update', u, 'canreinvite',  ASTGUI.getFieldValue('edit_multiple_canreinvite') );
			tmp_obj['canreinvite'] = ASTGUI.getFieldValue('edit_multiple_canreinvite') ;
		}
		if( _$('edit_check_multiple_dtmfmode').checked ){
			x.new_action('update', u, 'dtmfmode',  ASTGUI.getFieldValue('edit_multiple_dtmfmode') );
			tmp_obj['dtmfmode'] = ASTGUI.getFieldValue('edit_multiple_dtmfmode') ;
		}
		if( _$('edit_check_multiple_insecure').checked ){
			x.new_action('update', u, 'insecure',  ASTGUI.getFieldValue('edit_multiple_insecure') );
			tmp_obj['insecure'] = ASTGUI.getFieldValue('edit_multiple_insecure') ;
		}
		if( _$('edit_check_multiple_3waycalling').checked ){
			x.new_action('update', u, 'threewaycalling',  ASTGUI.getFieldValue('edit_multiple_3waycalling') );
			tmp_obj['threewaycalling'] = ASTGUI.getFieldValue('edit_multiple_3waycalling') ;
		}
		if( _$('edit_check_multiple_indirectory').checked ){
			x.new_action('update', u, 'hasdirectory',  ASTGUI.getFieldValue('edit_multiple_indirectory') );
			tmp_obj['hasdirectory'] = ASTGUI.getFieldValue('edit_multiple_indirectory') ;
		}
		if( _$('edit_check_multiple_callwaiting').checked ){
			x.new_action('update', u, 'callwaiting',  ASTGUI.getFieldValue('edit_multiple_callwaiting') );
			tmp_obj['callwaiting'] = ASTGUI.getFieldValue('edit_multiple_callwaiting') ;
		}
		if( _$('edit_check_multiple_cti').checked ){
			x.new_action('update', u, 'hasmanager',  ASTGUI.getFieldValue('edit_multiple_cti') );
			tmp_obj['hasmanager'] = ASTGUI.getFieldValue('edit_multiple_cti') ;
		}
		if( _$('edit_check_multiple_isagent').checked ){
			x.new_action('update', u, 'hasagent',  ASTGUI.getFieldValue('edit_multiple_isagent') );
			tmp_obj['hasagent'] = ASTGUI.getFieldValue('edit_multiple_isagent') ;
		}
		if( _$('edit_check_multiple_pickupgroup').checked ){
			x.new_action('update', u, 'pickupgroup',  ASTGUI.getFieldValue('edit_multiple_pickupgroup') );
			tmp_obj['pickupgroup'] = ASTGUI.getFieldValue('edit_multiple_pickupgroup') ;
		}
		merge_list[u] = tmp_obj ;
		///// ***************** /////
	});
	
	var after = function(){
		for (var f in merge_list){ if( merge_list.hasOwnProperty(f) ){
			parent.sessionData.pbxinfo.users[f].updateProperties( merge_list[f] );
		}}

		for ( var v in OBCID_LIST ){ if( OBCID_LIST.hasOwnProperty(v) ) {
			if( ASTGUI.updateaValue( { file: 'extensions.conf', context: 'globals', variable: v , value: OBCID_LIST[v] } ) ){
				try{ parent.sessionData.pbxinfo['GLOBALS'][v] = OBCID_LIST[v] ;} catch(err){ }
			}
		} }

		parent.ASTGUI.dialog.hide();
		ASTGUI.feedback( { msg:'Updated !' , showfor: 2 , color: 'green' } );
		window.location.reload();
	};
	x.callActions( after );
};


var buynow_action = function(){
	win=window.open('','myWin');
	document.buynow_form.target='myWin';
	document.buynow_form.submit();
};
