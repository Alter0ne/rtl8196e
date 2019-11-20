/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * menus.html functions
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
/*
parent.sessionData.pbxinfo.voicemenus[voicemenu-custom-1] = {  // ASTGUI.contexts.VoiceMenuPrefix
	comment : 'Welcome VoiceMenu', 	// label given to this voicemenu (gui-only field )
	alias_exten : '6070,1,Goto(voicemenu-custom-1|s|1)' ,   // line as read from [ASTGUI.contexts.VoiceMenuExtensions] context
	includes : ['default'] , // contexts included in this voicemenu
	steps : [ 's,1,Answer',  's,2,Wait(1)',  's,3,Background(thank-you-for-calling)' ]  , // sequence of steps - the 's' extension
	keypress_events : {
		// key press events - assumption is that each key press action is defined in a single line 
		// 	all priorities greater than 1 are ignored
		// if you want a sequence of steps to be executed on a keypress 
		// then build another menu with all the steps and point the key press action to that voicemenu
		0 : 'Goto(voicemenu-custom-1|s|1)' , // exten = 0,1,Goto(voicemenu-custom-1|s|1)
		1 : 'Hangup()' ,  // exten = 1,1,Hangup()
		.....
	}
} ;

*/

var isNewVmenu ;
var EDIT_VMENU ;
var CURRENT_ACTIONS = [] ;
var LISTOFSOUNDFILES = [];
var AGI_FILES = [];

var chop_Extension = function(a){
	if( !a ){ return ''; }
	if( a.endsWith('.gsm') ){ return a.rChop('.gsm'); }
	else if( a.endsWith('.wav') ){ return a.rChop('.wav'); }
	else if( a.endsWith('.alaw') ){ return a.rChop('.alaw'); }
	else if( a.endsWith('.g722') ){ return a.rChop('.g722'); }
	else if( a.endsWith('.g729') ){ return a.rChop('.g729'); }
	else if( a.endsWith('.ulaw') ){ return a.rChop('.ulaw'); }
	else if( a.endsWith('.wav') ){ return a.rChop('.wav'); }
	else if( a.endsWith('.mp3') ){ return a.rChop('.mp3'); }
	return a;
}

var VoiceMenus_miscFunctions = {

	show_VMenuEdit_normal : function(){ // VoiceMenus_miscFunctions.show_VMenuEdit_normal();
		$('#edit_VM_Advanced_DIV').hideWithBg();
		$('#div_vmenu_edit').showWithBg();
	},

	show_VMenuEdit_Advanced : function(){
		parent.ASTGUI.dialog.waitWhile('Loading ..') ;
		if (EDIT_VMENU === '') {
			EDIT_VMENU = top.pbx.voice_menus.next();
		}
		$('#edit_VM_Advanced_DIV').showWithBg();
		$('#div_vmenu_edit').hideWithBg();
		_$('edit_VM_Advanced_DIV_Title').innerHTML = '[' + EDIT_VMENU + ']' ;

		_$('edit_VM_Advanced_details').value = '';
		var t = config2json({filename:'extensions.conf', usf:0});
		if( t.hasOwnProperty(EDIT_VMENU) ){
			_$('edit_VM_Advanced_details').rows = t[EDIT_VMENU].length + 1 ;
			_$('edit_VM_Advanced_details').value = t[EDIT_VMENU].join('\n');
		}

		parent.ASTGUI.dialog.hide() ;
	},

	save_VMenu_Advanced : function(){ // VoiceMenus_miscFunctions.save_VMenu_Advanced();
		parent.ASTGUI.dialog.waitWhile('Saving ..') ;

		try{
			ASTGUI.miscFunctions.empty_context({ filename:'extensions.conf', context : EDIT_VMENU, cb : function(){}});
			var x = new listOfActions('extensions.conf');
	
			var vmenu_lines = ASTGUI.getFieldValue('edit_VM_Advanced_details') ;
			vmenu_lines = vmenu_lines.split('\n');
			vmenu_lines.each( function(this_line){
				if (!this_line.contains('=') && !this_line[0] !== ';') {
					parent.ASTGUI.dialog.hide();
					parent.ASTGUI.feedback({ msg:'All lines must be "exten=XXX,N,App()" format', showfor: 5});
					$('#edit_VM_Advanced_details').addClass('inputValidationFailed').focus();
					throw('error');
				}
				x.new_action( 'append', EDIT_VMENU , this_line.beforeChar('=') , this_line.afterChar('=') );
			});
			x.callActions( function(){
				ASTGUI.dialog.waitWhile('Updated VoiceMenu information <BR> Reloading GUI ... ');
				setTimeout( function(){ window.location.reload(); } , 2000 );
			});
		}catch(err){
			if (err === 'error') {
				return;
			}

			alert("Error saving VoiceMenu");
			window.location.reload();
		}
	},

	anotherMenuExistbyThisName: function( thisName ){		//	VoiceMenus_miscFunctions.anotherMenuExistbyThisName(a)
		var m = parent.sessionData.pbxinfo.voicemenus ;
		for( l in m ){ if( m.hasOwnProperty(l) ){
			if ( isNewVmenu == true && parent.sessionData.pbxinfo.voicemenus[l].getProperty('comment') == thisName ){
				return true;
			}
			if ( isNewVmenu == false && l != EDIT_VMENU && parent.sessionData.pbxinfo.voicemenus[l].getProperty('comment') == thisName ){
				return true;
			}
		}}
		return false;
	},

	load_Sounds: function(){ // VoiceMenus_miscFunctions.load_Sounds();
		parent.ASTGUI.dialog.waitWhile('loading list of sounds ..');

		var LANG_PATH = top.sessionData.directories.Sounds ;

		var c = config2json({filename: 'users.conf', usf:1});
		if(c.hasOwnProperty('general')){
			if ( c['general'].getProperty('language') != 'en' ){
				var LANG_PATH = top.sessionData.directories.Sounds + c['general'].getProperty('language') + '/' ;
			}
		}

		ASTGUI.listSystemFiles( LANG_PATH , function(listOfFiles) {
			var tmp_obj = {};
			listOfFiles.each(function( CURRENT_FILE ){
				CURRENT_FILE = chop_Extension(CURRENT_FILE);
				if( !tmp_obj.hasOwnProperty(CURRENT_FILE) ){
					tmp_obj[CURRENT_FILE] = 1 ;
					LISTOFSOUNDFILES.push(CURRENT_FILE);
				}
			});
			VoiceMenus_miscFunctions.load_recordedSounds();
		});
	},

	load_recordedSounds: function(){
		ASTGUI.listSystemFiles( top.sessionData.directories.menusRecord , function(recfiles) {
			var tmp_obj = {};
			recfiles.each(function( CURRENT_FILE ){
				CURRENT_FILE = 'record/' + chop_Extension(CURRENT_FILE);
				if( !tmp_obj.hasOwnProperty(CURRENT_FILE) ){
					tmp_obj[CURRENT_FILE] = 1 ;
					LISTOFSOUNDFILES.push(CURRENT_FILE);
				}
			});
			ASTGUI.COMBOBOX.call( _$('newstep_sound') , LISTOFSOUNDFILES, 450 );
			VoiceMenus_miscFunctions.load_aig_files();
		});
	},

	load_aig_files : function(){
		parent.ASTGUI.dialog.waitWhile('loading list of AIG scripts ..');
		ASTGUI.listSystemFiles( top.sessionData.directories.AGIBIN , function(recfiles) {
			AGI_FILES = recfiles;
			parent.ASTGUI.dialog.hide();
			ASTGUI.COMBOBOX.call( _$('newstep_agi') , AGI_FILES, 450 );
		});
	},

	show_newstep_tds: function(a){ // VoiceMenus_miscFunctions.show_newstep_tds()
		var tip_chosenStep = _$('tip_chosenStep'); tip_chosenStep.innerHTML = '';
		if(a){
			_$('newStep_select_tr').style.display = 'none';
			_$('newStep_details_tr').style.display = '';
			ASTGUI.updateFieldToValue('newstep_pwd','');
			var step_action = _$('newStep_select_action').value ;
			$('.class_newStep_details_td').hide();
			var lbl = function(a){ _$('newstep_comment_span').innerHTML = a ; };
			switch(step_action){
				case 'Answer':
					lbl('Answer');
					tip_chosenStep.innerHTML = 'Answer a channel if ringing';
					break;
				case 'AGI':
					lbl('AGI');
					$('#newstep_agi').show();
					ASTGUI.updateFieldToValue('newstep_agi','');
					tip_chosenStep.innerHTML = 'Executes an AGI compliant application';
					break;

				case 'Ringing':
					lbl('Ringing');
					tip_chosenStep.innerHTML = 'Indicate ringing tone';
					break;
				case 'Authenticate':
					lbl('Authenticate');
					$('#newstep_pwd').show();
					tip_chosenStep.innerHTML = ' This application asks the caller to enter a given password in order to continue dialplan execution. ';
					break;

				case 'SayAlpha':
					lbl('SayAlpha');
					$('#newstep_pwd').show();
					tip_chosenStep.innerHTML = ' Say each character in the string including letters, numbers and other characters, one by one ';
					break;

				case 'SayDigits':
					lbl('SayDigits');
					$('#newstep_pwd').show();
					tip_chosenStep.innerHTML = 'Say the digits, one by one';
					break;

				case 'SayNumber':
					lbl('SayNumber');
					$('#newstep_pwd').show();
					tip_chosenStep.innerHTML = 'Say a number (e.g. \'six thousand, five hundred and seventy two\')';
					break;

				case 'Background':
					lbl('Background');
					$('#newstep_sound').show();
					tip_chosenStep.innerHTML = 'Play an audio file while waiting for digits of an extension to go to. (For custom voice prompts, type "record/" before your prompt file name)';
					break;
				case 'Busy':
					lbl('Busy Tone');
					$('#newstep_seconds').show();
					tip_chosenStep.innerHTML = 'Indicate the Busy condition';
					break;
				case 'Congestion':
					lbl('Congestion');
					$('#newstep_seconds').show();
					tip_chosenStep.innerHTML = 'Indicate the congestion condition to the calling channel.';
					break;
				case 'DigitTimeout':
					lbl('Digit Timeout');
					$('#newstep_seconds').show();
					tip_chosenStep.innerHTML = '';
					break;
				case 'DISA':
					lbl('DISA Password : ');
					$('#newstep_pwd, #newstep_disaContext').show();
					tip_chosenStep.innerHTML = 'Allow someone from outside the telephone switch (PBX) to obtain an <i>internal</i> system dialtone and to place calls from it as if they were placing a call from  within the switch.' ;
					break;
				case 'ResponseTimeout':
					lbl('Response Timeout');
					$('#newstep_seconds').show();
					tip_chosenStep.innerHTML = '';
					break;
				case 'Playback':
					lbl('Play Sound');
					$('#newstep_sound').show();
					tip_chosenStep.innerHTML = 'Plays back given file. (For custom voice prompts, type "record/" before your prompt file name)';
					break;
				case 'Wait':
					lbl('Wait');
					$('#newstep_seconds').show();
					tip_chosenStep.innerHTML = 'Pause dialplan execution for a specified number of seconds';
					break;
				case 'WaitExten':
					lbl('WaitExten');
					$('#newstep_seconds').show();
					tip_chosenStep.innerHTML = 'Wait for the user to enter a new extension for a specified number of seconds.';
					break;
				case 'WaitForRing':
					lbl('WaitForRing');
					$('#newstep_seconds').show();
					tip_chosenStep.innerHTML = 'Pause dialplan execution until next ring for a specified number of seconds.  Cancels answer if no ring occurs.';
					break;
				case 'toDestination':
					lbl('To Destiantion');
					$('#newstep_gotoDestination').show();
					tip_chosenStep.innerHTML = '';
					break;
				case 'toDestinationByCallerId':
					lbl('Goto Destiantion if CallerId is');
					$('#newstep_pwd').val('').show();
					$('#newstep_gotoDestinationByCallerId').show();
					tip_chosenStep.innerHTML = 'Goto this destination, if callerId number matches the specified number';
					break;
				case 'setLanguage':
					lbl('Set Language');
					$('#newstep_setlanguage').show();
					tip_chosenStep.innerHTML = '';
					break;
				case 'SetMusicOnHold':
					lbl('Set MusicOhHold Class');
					_$('newstep_mohClass').selectedIndex = -1;
					$('#newstep_mohClass').show();
					tip_chosenStep.innerHTML = '';
					break;
				case 'GotoDirecotry':
					lbl('To Directory');
					tip_chosenStep.innerHTML = '';
					break;
				case 'Hangup':
					lbl('Hangup');
					tip_chosenStep.innerHTML = 'Hang up the calling channel';
					break;
				case 'DialViaTrunk':
					lbl('Dial an external Number');
					tip_chosenStep.innerHTML = 'Place a call outside the pbx using the selected trunk.';
					$('#newstep_dial_ThisNumber').show();
					$('#newstep_dial_ViaTrunk').show();
					_$('newstep_dial_ThisNumber').value = '';
					_$('newstep_dial_ViaTrunk').selectedIndex = -1 ;
					break;
				case 'UserEvent':
					lbl('User Event');
					tip_chosenStep.innerHTML = 'Send an arbitrary event to the manager interface';
					$('#newstep_UserEvent_eventname').show();
					$('#newstep_UserEvent_body').show();
					$('#newstep_UserEvent_body_lbl').show();
					_$('newstep_UserEvent_eventname').value = '';
					_$('newstep_UserEvent_body').value = '';
					break;
				case 'CustomApp':
					lbl('Custom App');
					$('#newstep_custom').show();
					tip_chosenStep.innerHTML = 'Add a <i>Custom Step</i> to the VoiceMenu';
					break;
				case 'DialViaSkype':
					lbl('Dial via Skype');
					$('#newstep_dial_ThisNumber').show();
					_$('newstep_dial_ThisNumber').value = '';
					tip_chosenStep.innerHTML = 'Dial a number/buddy using the default skype account';
					break;
				case 'DialViaGtalk':
					lbl('Dial via gtalk');
					$('#newstep_gtalkpeers').show();
					_$('newstep_gtalkpeers').selectedIndex = -1 ;
					tip_chosenStep.innerHTML = 'Dial a user on the google talk network.';
					break;
				case 'Macro':
					lbl('Macro');
					$('#newstep_custom').show();
					tip_chosenStep.innerHTML = "macroname,arg1,arg2 .... Executes a macro using the context 'macro-&lt;macroname&gt;'";
					break;
				default:
					break;
			}

		}else{
			_$('newStep_select_action').selectedIndex = 0;
			_$('newStep_select_tr').style.display = '';
			_$('newStep_details_tr').style.display = 'none';
		}
	},

	reset_VoicemenuFields: function(){ // VoiceMenus_miscFunctions.reset_VoicemenuFields();
		ASTGUI.resetTheseFields (['vmenu_name', 'vmenu_ext', 'vmenu_dialOther']) ;
		ASTGUI.domActions.removeAllChilds('sqSteps');
		CURRENT_ACTIONS = [] ;
		_$('newStep_select_action').selectedIndex = 0;
		//var r = ['sqSteps', 'keyPress_1', 'keyPress_2', 'keyPress_3', 'keyPress_4', 'keyPress_5', 'keyPress_6', 'keyPress_7', 'keyPress_8', 'keyPress_9', 'keyPress_pound' , 'keyPress_star' ] ;
		//r.each( function(el){
		//	ASTGUI.domActions.removeAllChilds( el );
		//});
		//_$('vmenu_allowKeyPressEvents').updateStatus();
	},

	push_newstep: function(){ // VoiceMenus_miscFunctions.push_newstep()
		_$('tip_chosenStep').innerHTML = '';
		var step_action = _$('newStep_select_action').value ;
		var newstep = '';
		switch(step_action){
			case 'Answer':
				newstep = 'Answer()';
				break;
			case 'Ringing':
				newstep = 'Ringing()';
				break;
			case 'Authenticate':
				newstep = 'Authenticate(' + ASTGUI.getFieldValue('newstep_pwd') + ')';
				break;
			case 'SayAlpha':
				newstep = 'SayAlpha(' + ASTGUI.getFieldValue('newstep_pwd') + ')';
				break;
			case 'SayDigits':
				newstep = 'SayDigits(' + ASTGUI.getFieldValue('newstep_pwd') + ')';
				break;
			case 'SayNumber':
				newstep = 'SayNumber(' + ASTGUI.getFieldValue('newstep_pwd') + ')';
				break;
			case 'Background':
				newstep = 'Background(' + ASTGUI.getFieldValue('newstep_sound') + ')';
				break;
			case 'Busy':
				newstep = 'Busy(' + ASTGUI.getFieldValue('newstep_seconds') + ')';
				break;
			case 'Congestion':
				newstep = 'Congestion(' + ASTGUI.getFieldValue('newstep_seconds') + ')';
				break;
			case 'DigitTimeout':
				newstep = 'Set(TIMEOUT(digit)=' + ASTGUI.getFieldValue('newstep_seconds') + ')';
				break;
			case 'DISA':
				newstep = 'DISA(' + ASTGUI.getFieldValue('newstep_pwd') + top.session.delimiter + ASTGUI.getFieldValue('newstep_disaContext') +')';
				break;
			case 'ResponseTimeout':
				newstep = 'Set(TIMEOUT(response)=' + ASTGUI.getFieldValue('newstep_seconds') + ')';
				break;
			case 'Playback':
				newstep = 'Playback(' + ASTGUI.getFieldValue('newstep_sound') + ')';
				break;
			case 'Wait':
				newstep = 'Wait(' + ASTGUI.getFieldValue('newstep_seconds') + ')';
				break;
			case 'AGI':
				newstep = 'AGI(' + ASTGUI.getFieldValue('newstep_agi') + ')';
				break;
			case 'WaitExten':
				newstep = 'WaitExten(' + ASTGUI.getFieldValue('newstep_seconds') + ')';
				break;
			case 'WaitForRing':
				newstep = 'WaitForRing(' + ASTGUI.getFieldValue('newstep_seconds') + ')';
				break;
			case 'toDestination':
				newstep = ASTGUI.getFieldValue('newstep_gotoDestination');
				break;
			case 'toDestinationByCallerId':
				// newstep = 'GotoIf($["${CALLERID(num)}" == "newstep_pwd"]? newstep_gotoDestinationByCallerId )'
				newstep = "GotoIf($[\"${CALLERID(num)}\" == \""
 					+ ASTGUI.getFieldValue('newstep_pwd')
					+ "\"]?"+ ASTGUI.getFieldValue('newstep_gotoDestinationByCallerId') +")" ;
				break;
			case 'setLanguage':
				newstep = 'Set(CHANNEL(language)=' + ASTGUI.getFieldValue('newstep_setlanguage') + ')';
				break;
			case 'GotoDirecotry':
				newstep = 'Directory(' + 'default' + ')';
				break;
			case 'SetMusicOnHold':
				newstep = 'SetMusicOnHold(' + ASTGUI.getFieldValue('newstep_mohClass') + ')';
				break;
			case 'Hangup':
				newstep = 'Hangup()';
				break;
			case 'DialViaTrunk':
				var tmp_trunkName = ASTGUI.getFieldValue('newstep_dial_ViaTrunk');
				var tmp_extern_num = ASTGUI.getFieldValue('newstep_dial_ThisNumber');
				if (!parent.ASTGUI.validateFields([_$('newstep_dial_ThisNumber')])) {
					return;
				}
				var tname = parent.pbx.trunks.getTrunkIdByName(tmp_trunkName);
				newstep =  'Macro('+ ASTGUI.contexts.dialtrunks + ',${' + tmp_trunkName + '}/'+ tmp_extern_num +',,'+ (tname ? tname : tmp_trunkName) + ',)' ;
				break;
			case 'UserEvent':
				var tmp_EventName = ASTGUI.getFieldValue('newstep_UserEvent_eventname');
				var tmp_EventBody = ASTGUI.getFieldValue('newstep_UserEvent_body');
				if( tmp_EventBody.trim() ){
					tmp_EventName = tmp_EventName + top.session.delimiter + tmp_EventBody ;
				}
				newstep =  'UserEvent('+ tmp_EventName + ')';
				break;
			case 'CustomApp':
				newstep = ASTGUI.getFieldValue('newstep_custom') ;
				break;
			case 'DialViaSkype':
				newstep = 'Dial(Skype/' + ASTGUI.getFieldValue('newstep_dial_ThisNumber') + ')' ;
				break;
			case 'DialViaGtalk':
				newstep = 'Dial(gtalk/' + ASTGUI.getFieldValue('newstep_gtalkpeers') + ')' ;
				break;
			case 'Macro':
				newstep = 'Macro(' + ASTGUI.getFieldValue('newstep_custom') + ')' ;
				break;
			default:
				break;
		}
		CURRENT_ACTIONS.push(newstep);
		this.refresh_allSteps();
		this.show_newstep_tds(false);
	},

	refresh_allSteps: function(){ // VoiceMenus_miscFunctions.refresh_allSteps()
		ASTGUI.domActions.removeAllChilds( 'sqSteps' );
		var add_sqStep = function(a){
			var txt = CURRENT_ACTIONS[a];
			var tmp = document.createElement('div');
			tmp.STEPNO = a ;
			var sp_desc = document.createElement('span');
				sp_desc.className = 'step_desc';
				sp_desc.innerHTML = ASTGUI.parseContextLine.showAs(txt) ;
			var sp_up = document.createElement('span');
				sp_up.className = 'step_up';
				sp_up.innerHTML = '&nbsp;';
			var sp_down = document.createElement('span');
				sp_down.className = 'step_down';
				sp_down.innerHTML = '&nbsp;';
			var sp_delete = document.createElement('span');
				sp_delete.className = 'step_delete';
				sp_delete.innerHTML = '&nbsp;';
	
			tmp.appendChild(sp_desc) ;
			tmp.appendChild(sp_delete) ;
			tmp.appendChild(sp_up) ;
			tmp.appendChild(sp_down) ;
			_$('sqSteps').appendChild(tmp) ;
		};
		if( CURRENT_ACTIONS[0] && CURRENT_ACTIONS[0].toLowerCase().beginsWith('noop(') ){
			CURRENT_ACTIONS.splice(0,1);
		}
		for( var t=0; t < CURRENT_ACTIONS.length ; t++ ){
			add_sqStep(t);
		}
	}
};


var delete_voiceMenu_confirm = function(a){
	if (!confirm('delete selected voicemenu ?')) { return; }
	EDIT_VMENU = a ;
	parent.pbx.voice_menus.remove(EDIT_VMENU);
	window.location.reload();
};

var voiceMenu_saveChanges = function(){

	if ( !ASTGUI.checkRequiredFields(['vmenu_name']) ){
		return ;
	}
	if ( !ASTGUI.validateFields( ['vmenu_ext'] ) ){
		return ;
	}
	if ( VoiceMenus_miscFunctions.anotherMenuExistbyThisName( ASTGUI.getFieldValue('vmenu_name') ) ){
		ASTGUI.highlightField('vmenu_name' , 'Another Voicemenu exists by this name !');
		return;
	}

	if( isNewVmenu == true && ASTGUI.getFieldValue('vmenu_ext') ){
		if( parent.miscFunctions.ifExtensionAlreadyExists( ASTGUI.getFieldValue('vmenu_ext') ) ){
			ASTGUI.highlightField('vmenu_ext' , 'Extension already exists');
			parent.ASTGUI.dialog.hide();
			return;
		}
		if( !ASTGUI.miscFunctions.isExtensionInRange( ASTGUI.getFieldValue('vmenu_ext') , 'vme_start', 'vme_end' ) ){
			ASTGUI.highlightField( _$('vmenu_ext'), 'Extension is not in preferred range' );
			parent.ASTGUI.dialog.hide();
			return;
		}
	}

	if(!CURRENT_ACTIONS.length){
		ASTGUI.feedback( { msg:'List of Actions should have at least 1 step !', showfor:2 });
		return;
	}

	var vm = {
		comment: ASTGUI.getFieldValue('vmenu_name'),
		alias_exten: '',
		includes: [],
		steps: [],
		keypress_events: {}
	};
	vm = ASTGUI.toCustomObject(vm) ;
	if( _$('vmenu_dialOther').checked ){
		vm.includes.push('default');
	}

	if(_$('vmenu_allowKeyPressEvents').checked){
		['0','1','2','3','4','5','6','7','8','9','*','#','t','i'].each( function(this_key){
			if ( _$( 'keyPress_' + this_key ).KPE ){
				vm.keypress_events[this_key] = _$( 'keyPress_' + this_key ).KPE ;
			}
		} );
	}

	if( isNewVmenu == true ){
		var vmenu_name = parent.pbx.voice_menus.next();
	}else{
		var vmenu_name = EDIT_VMENU ;
		parent.pbx.voice_menus.remove( vmenu_name );
	}

	var after = function(){
		window.location.reload();
	};

	vm.steps.push( 'NoOp(' + vm.comment + ')' );
	vm.steps = vm.steps.concat( CURRENT_ACTIONS );
	vm.alias_exten = ASTGUI.getFieldValue('vmenu_ext');
	parent.pbx.voice_menus.add( vmenu_name, vm , after );
};

var show_createNewMenu_Form = function(){
	isNewVmenu = true ;
	_$('div_vmenu_edit_title').innerHTML = 'Create New VoiceMenu';
	CURRENT_ACTIONS = ['s,1,Answer'] ;
	EDIT_VMENU = '' ;
	VoiceMenus_miscFunctions.reset_VoicemenuFields();
	VoiceMenus_miscFunctions.refresh_allSteps();

	var tmp_allextensions = ASTGUI.cloneObject( parent.miscFunctions.getAllExtensions() );
	_$('vmenu_ext').value  = tmp_allextensions.firstAvailable( parent.sessionData.GUI_PREFERENCES.getProperty('vme_start') );
	_$('vmenu_allowKeyPressEvents').checked = false ;
	_$('vmenu_allowKeyPressEvents').updateStatus();

	$('#div_vmenu_edit').showWithBg();
};


var edit_voiceMenu_form = function(this_menu){
	EDIT_VMENU = this_menu;
	isNewVmenu = false ;
	_$('div_vmenu_edit_title').innerHTML = 'Edit VoiceMenu ' + EDIT_VMENU ;
	VoiceMenus_miscFunctions.reset_VoicemenuFields();
	var THIS_VM = parent.sessionData.pbxinfo.voicemenus[EDIT_VMENU];
	ASTGUI.updateFieldToValue('vmenu_name',  THIS_VM.getProperty('comment'));
	ASTGUI.updateFieldToValue('vmenu_ext',  ASTGUI.parseContextLine.getExten(THIS_VM.getProperty('alias_exten')));
	_$('vmenu_dialOther').checked =  ( ASTGUI.miscFunctions.ArrayContains(THIS_VM.includes, 'default' ) ) ? true: false;
	CURRENT_ACTIONS = ASTGUI.cloneObject(THIS_VM.steps) ;
	VoiceMenus_miscFunctions.refresh_allSteps();

	(function(){
		var kpe = false;
		['0','1','2','3','4','5','6','7','8','9','*','#','t','i'].each( function(this_key){
			if ( THIS_VM.keypress_events[this_key] ){
				_$( 'keyPress_' + this_key ).innerHTML = ASTGUI.parseContextLine.showAs(THIS_VM.keypress_events[this_key]) ;
				_$( 'keyPress_' + this_key ).KPE = THIS_VM.keypress_events[this_key] ;
				kpe = true;
			}else{
				_$( 'keyPress_' + this_key ).innerHTML = '--';
				_$( 'keyPress_' + this_key ).KPE = '';
			}
		} );
		_$('vmenu_allowKeyPressEvents').checked = kpe ;
		_$('vmenu_allowKeyPressEvents').updateStatus();
	})();

	$('#div_vmenu_edit').showWithBg();
};

var edit_voiceMenu_KeyPressActions_form = function(){

	$('#div_vmenu_keyPressActions').showWithBg();
};


var updateVoiceMenus_Table = function(){
	(function(){
		var someArray = parent.miscFunctions.getAllDestinations() ;
		ASTGUI.selectbox.populateArray('newstep_gotoDestination', someArray);

		var someOtherArray = parent.miscFunctions.getAllDestinations(1) ;
		ASTGUI.selectbox.populateArray('newstep_gotoDestinationByCallerId', someOtherArray);

		ASTGUI.selectbox.populateArray('kpe_destinations', someArray);
	})();
	ASTGUI.selectbox.insert_before('kpe_destinations','None', '', 0);

	$('#sqSteps').click(function(event){
		var s = ASTGUI.events.getTarget(event);
		var cl =  $(s).attr("class") ;
		if(!cl || !cl.beginsWith('step_') ){return;}
		var stepNo = Number( s.parentNode.STEPNO );
		switch(cl){
			case 'step_delete':
				CURRENT_ACTIONS.splice(stepNo,1);
				break;
			case 'step_up':
				if(stepNo == 0) return;
				var tmp = CURRENT_ACTIONS[stepNo] ;
				CURRENT_ACTIONS.splice(stepNo, 1);
				CURRENT_ACTIONS.splice(stepNo-1, 0, tmp);
				break;
			case 'step_down':
				if(stepNo == (CURRENT_ACTIONS.length-1) ) return;
				var tmp = CURRENT_ACTIONS[stepNo] ;
				CURRENT_ACTIONS.splice(stepNo+2, 0, tmp);
				CURRENT_ACTIONS.splice(stepNo, 1);
				break;
			default:
				break;
		}
		VoiceMenus_miscFunctions.refresh_allSteps();
	});

	try{
		VoiceMenus_miscFunctions.load_Sounds();
	}catch(err){

	}

	var addCell = ASTGUI.domActions.tr_addCell; // temporarily store the function
	var lvms = parent.sessionData.pbxinfo.voicemenus.getOwnProperties();
	var TBL = _$('table_VoiceMenuslist') ;
	if( lvms.length == 0 ){
		ASTGUI.domActions.clear_table( TBL );
		var newRow = TBL.insertRow(-1);
		newRow.className = 'even';
		addCell( newRow , { html:'No VoiceMenus defined !!'} );
		return;
	}else{

		(function(){ // add first row
			var newRow = TBL.insertRow(-1);
			newRow.className = "frow";
			addCell( newRow , { html:'', width:'15px'} );
			addCell( newRow , { html:'&nbsp;&nbsp;&nbsp;&nbsp;Label', align:'left'} );
			addCell( newRow , { html:'Extension'} );
			addCell( newRow , { html:'Dial Other Extensions'} );
			addCell( newRow , { html:'Key Press Actions'} );
			addCell( newRow , { html:''} );
		})();

		lvms.each( function(vm_name){
			var THIS_VM  = parent.sessionData.pbxinfo.voicemenus[vm_name] ;
			var lbl = THIS_VM.comment || vm_name;
			var ae_line = THIS_VM.getProperty('alias_exten') ;
			var ae = ( ae_line ) ? ASTGUI.parseContextLine.getExten(ae_line) : '';
			var newRow = TBL.insertRow(-1);
			newRow.className = ((TBL.rows.length)%2==1)?'odd':'even';
			addCell( newRow , { html:'', width:'15px'});

			addCell( newRow , { html: lbl });
			addCell( newRow , { html: ae });
			addCell( newRow , { html: ( ASTGUI.miscFunctions.ArrayContains(THIS_VM.includes, 'default' ) ) ? 'Yes': 'No' }); // dial other extensions

			var hasKeyPresssEvents = function(){
				var u = ['0','1','2','3','4','5','6','7','8','9','*','#','t','i'] ;
				for( var i = 0 ; i < u.length ; i++ ){
					if( THIS_VM.hasOwnProperty('keypress_events') && THIS_VM.keypress_events.hasOwnProperty(u[i]) && THIS_VM.keypress_events[u[i]].trim() ){
						return 'Yes';
					}
				}
				return 'No';
			};
			var ggg = hasKeyPresssEvents();

			addCell( newRow , { html: ggg }); // key press actions


			var tmp = "<span class='guiButton' onclick=\"edit_voiceMenu_form('" + vm_name +"')\">Edit</span>" + 
				"<span class='guiButtonDelete' onclick=\"delete_voiceMenu_confirm('" + vm_name +"')\">Delete</span>" ;
			addCell( newRow , { html: tmp } );

		});
	}
};

var localajaxinit = function(){
	top.document.title = 'Voice Menus Configuration' ;

	var am = top.cookies.get('advancedmode');
	if( am && am == 'yes' ){
		ASTGUI.selectbox.append('newStep_select_action', 'Custom App', 'CustomApp');
	}

	setTimeout( function(){
		var modules_show = ASTGUI.cliCommand('module show');
		if( modules_show.contains('res_skypeforasterisk') && modules_show.contains('chan_skype') ){
			ASTGUI.selectbox.append('newStep_select_action', 'Dial via Skype', 'DialViaSkype');
		}
		if( modules_show.contains('res_jabber') && modules_show.contains('chan_gtalk') ){
			ASTGUI.selectbox.append('newStep_select_action', 'Dial via gtalk', 'DialViaGtalk');
			var GTALK_CNF = config2json({ filename:'gtalk.conf', usf:1 }); // buddies
			for( buddy in GTALK_CNF ){
				if( !GTALK_CNF.hasOwnProperty(buddy) || buddy == 'general' ) continue;
				ASTGUI.selectbox.append( 'newstep_gtalkpeers' , GTALK_CNF[buddy].username , GTALK_CNF[buddy].connection + '/' + GTALK_CNF[buddy].username );
			}
		}



	}, 1000);

	if( !ASTGUI.miscFunctions.alertIfRangeisNotdefined('vme_start','vme_end', 'VoiceMenus') ){
		$('.top_buttons').hide();
		return;
	}

	(function(){
		if ( jQuery.browser.msie ){ 
			$('#tr_keypressactions TR').hover (
				function(){ this.style.backgroundColor='#e5ac79';  },
				function(){ this.style.backgroundColor='#F4EFE5'; }
			) ;
		}
	})();

	(function(){
		// list non-analog trunks by trunk.
		var t = ASTGUI.cloneObject(parent.pbx.trunks.list({iax: true, providers: true, sip: true}));
		var TMP_FORSORT = [];
		t.each(function(item){
			TMP_FORSORT.push( parent.pbx.trunks.getName(item) + ':::::::' +  item);
		});
		TMP_FORSORT.sort();
		var trunks_selectbox = _$('newstep_dial_ViaTrunk');
		TMP_FORSORT.each( function(this_str){
			var a = this_str.split(':::::::');
			ASTGUI.selectbox.append( trunks_selectbox, 'via Trunk ' + a[0], a[1] );
		});

		// list analog trunks by group.
		var g = parent.pbx.trunks.listAllGroups();
		g.each( function(group){
			var tr = parent.pbx.trunks.getTrunkNamesByGroup(group);
			var trstr = tr.join(", ");
			if (trstr.length > 30){
				trstr = trstr.substr(30) + '...';
			}
			ASTGUI.selectbox.append( trunks_selectbox, 'via ' + parent.pbx.trunks.getGroupDescription(group), "group_" + group);
		});


	})();

	(function(){
		var mcls = config2json({filename: 'musiconhold.conf', catlist:'yes'});
		mcls.each( function(this_class){
			ASTGUI.selectbox.append('newstep_mohClass', this_class, this_class );
		});

		var c = parent.pbx.call_plans.list() ;
		c.each(function(plan){
			ASTGUI.selectbox.append('newstep_disaContext', plan.withOut( ASTGUI.contexts.CallingPlanPrefix ) , plan );
		});
	})();

	ASTGUI.domActions.showHideByCheckBox( 'vmenu_allowKeyPressEvents' , 'tr_keypressactions' );
	updateVoiceMenus_Table();
	$(".voicemenuClass_Action").click( function(event){
		try{
			var s = ASTGUI.events.getTarget(event);
			if( !s.id || !s.id.beginsWith( 'keyPress_' ) ){
				return;
			}
			_$('kpe_destinations_updatebutton').KPE_FOR = s.id ;
			s.appendChild( _$('kpe_destinations') );
			s.appendChild( _$('kpe_destinations_updatebutton') );
			var this_kpe = s.KPE || '' ;
			ASTGUI.selectbox.selectDestinationOption( 'kpe_destinations' , this_kpe );
		}catch(err){ }
	});
};


var update_a_keyPressEvent = function(){
	var d = _$('div_vmenu_keyPressActions_edit');
	d.appendChild( _$('kpe_destinations') );
	d.appendChild( _$('kpe_destinations_updatebutton') );
	try{
		var t = _$('kpe_destinations_updatebutton').KPE_FOR ;
	}catch(err){
		var t = '';
	}
	if(!t.beginsWith( 'keyPress_' ) ){
		return;
	}
	_$(t).KPE = _$('kpe_destinations').value ;

	var ti = ASTGUI.parseContextLine.showAs( _$(t).KPE );
	_$(t).innerHTML = ti || '--';
	_$('kpe_destinations_updatebutton').KPE_FOR = '';
};
