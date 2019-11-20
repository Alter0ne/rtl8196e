/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * preferences.html functions
 *
 * Copyright (C) 2006-2011, Digium, Inc.
 *
 * Pari Nannapaneni <pari@digium.com>
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
var OPEXTENSION = '';

var polycom_digitmap_string = '';
var polycom_digitmap_timeout_string = '';


var loadDOMelements = function(){
	DOM_obCid_input = _$('obCid_input') ;
	DOM_op_ext = _$('op_ext') ;
};


function localajaxinit(){
	top.document.title = "General Preferences" ;
	if ((ASTGUI.version.gteq("1.6.0") || parent.sessionData.PLATFORM.isAA50 || parent.sessionData.PLATFORM.isABE) && !parent.sessionData.PLATFORM.isAA50_OEM) {
		$('.16_Only').show();
		(function(){
			var c = config2json( { filename: 'phoneprov.conf' , usf: 0 } );
			if( c.hasOwnProperty('polycom') &&  c['polycom'].contains('setvar=IDLEIMAGE_ACTIVE=1') ){
				_$('op_polycom_idleimage').checked = true ;
			}else{
				_$('op_polycom_idleimage').checked = false ;
			}

			if( c.hasOwnProperty('polycom') ){
				var tmp_dm_index = c['polycom'].indexOfLike('setvar=DIGITMAP=') ;
				if( tmp_dm_index >= 0 ){
					polycom_digitmap_string = c['polycom'][tmp_dm_index].afterChar('=');
					_$('op_voip_digitmap').value = polycom_digitmap_string.lChop('DIGITMAP=');
				}

				var tmp_dmto_index = c['polycom'].indexOfLike('setvar=DIGITMAPTIMEOUT=') ;
				if( tmp_dmto_index >= 0 ){
					polycom_digitmap_timeout_string = c['polycom'][tmp_dmto_index].afterChar('=');
					_$('op_voip_digit_timeout').value = polycom_digitmap_timeout_string.lChop('DIGITMAPTIMEOUT=');
				}
			}

		})();
	}

	var fd_text = (parent.sessionData.PLATFORM.isAA50 || parent.sessionData.PLATFORM.isABE ) ? 'Factory Reset' : 'Reset Configuration' ;
	var t = [ 	{url:'#', desc:'General Preferences', selected:true } ,
			{url:'language.html', desc:'Language' } ,
			{url:'password.html', desc:'Change Password'}
		];
	if( parent.sessionData.PLATFORM.isAA50 ) { 
		t.push({url:'reset_defaults.html', desc: fd_text });
	};
	t.push( {url:'reboot.html', desc:'Reboot' } ); 
	t.push( {url: 'flipadvanced.html', desc:'Advanced Options' } );

	ASTGUI.tabbedOptions( _$('tabbedMenu') , t );
	loadDOMelements();

	var cusers = parent.pbx.users.list();
	var op_tmp ;
	ASTGUI.selectbox.append( DOM_op_ext, '<none>', '');
	cusers.each(function(user){
		op_tmp = 'Goto(default,' + user + ',1)';
		ASTGUI.selectbox.append( DOM_op_ext , 'User ' + user , op_tmp);
	});
	(function(){
		var c = context2json ({ filename:'extensions.conf' , context: 'globals', usf: 1 });
		var ringtime = ( c.hasOwnProperty('RINGTIME') ) ? c['RINGTIME'] : '20' ;
		ASTGUI.updateFieldToValue( 'op_ringTimeOut_Voicemail', ringtime );
		try{
			ASTGUI.updateFieldToValue( DOM_obCid_input, c[ASTGUI.globals.obcidstr] || ''  );
			ASTGUI.updateFieldToValue( 'obCidName_input', c[ASTGUI.globals.obcidNamestr] || ''  );
		}catch(err){}

	})();

	try{
		OPEXTENSION  = ( parent.sessionData.pbxinfo.localextensions && parent.sessionData.pbxinfo.localextensions.Operator ) ? parent.sessionData.pbxinfo.localextensions.Operator : '' ;
	}catch(err){}
	ASTGUI.selectbox.selectOption( DOM_op_ext , OPEXTENSION );
	loadExtensions_ranges();


	$("#tooltip_img_voipPhoneDigitMap").mouseover(function(){
		ASTGUI.domActions.alignBbelowA( _$("tooltip_img_voipPhoneDigitMap"),_$('tooltip_voipPhoneDigitMap'), -120, 0);
		$('#tooltip_voipPhoneDigitMap').show();
	}).mouseout(function(){ $('#tooltip_voipPhoneDigitMap').hide(); });

};

var loadExtensions_ranges = function(){
	var c = context2json ({ filename: ASTGUI.globals.configfile , context: 'general', usf: 1 });
	ASTGUI.updateFieldToValue( 'ue_start' , c.getProperty('ue_start') );
	ASTGUI.updateFieldToValue( 'ue_end' , c.getProperty('ue_end') );
	ASTGUI.updateFieldToValue( 'mm_start' , c.getProperty('mm_start') );
	ASTGUI.updateFieldToValue( 'mm_end' , c.getProperty('mm_end') );
	ASTGUI.updateFieldToValue( 'qe_start' , c.getProperty('qe_start') );
	ASTGUI.updateFieldToValue( 'qe_end' , c.getProperty('qe_end') );
	ASTGUI.updateFieldToValue( 'vme_start' , c.getProperty('vme_start') );
	ASTGUI.updateFieldToValue( 'vme_end' , c.getProperty('vme_end') );
	ASTGUI.updateFieldToValue( 'rge_start' , c.getProperty('rge_start') );
	ASTGUI.updateFieldToValue( 'rge_end' , c.getProperty('rge_end') );
	ASTGUI.updateFieldToValue( 'vmg_start' , c.getProperty('vmg_start') );
	ASTGUI.updateFieldToValue( 'vmg_end' , c.getProperty('vmg_end') );

	var disabled = c.getProperty('disable_extension_ranges');
	ASTGUI.updateFieldToValue( 'disable_extension_ranges' , disabled == 'yes' ? 'on' : 'off' );
	refreshExtensionRangesForm(disabled);
};



var verify_Ranges = function(){
	var arrF = ['ue_start', 'ue_end', 'mm_start', 'mm_end', 'qe_start', 'qe_end', 'vme_start', 'vme_end', 'rge_start', 'rge_end', 'vmg_start', 'vmg_end' ] ;
	var arr = [];
	for(var i=0; i < arrF.length ; i++){
		arr.push( _$(arrF[i]) );
	}
	if ( !ASTGUI.checkRequiredFields(arr) ){
		return  false;
	}
	if ( !ASTGUI.validateFields(arr) ) {
		return false;
	}
	for( var i=0; i < arrF.length ; i++ ){
		var tmp = ASTGUI.getFieldValue( arrF[i] );
		if( !tmp.length.isValueInBetween(2,6)  ){
			_$(arrF[i]).focus();
			_$(arrF[i]).select();
			ASTGUI.feedback( { msg:'Extension should be 2 to 6 digits ', showfor:3 } );
			return false;
		}
	}
	for( var i=0; i < arrF.length ; i++ ){
		var tmp = ASTGUI.getFieldValue( arrF[i] );
		if(!arrF[i].endsWith('_start') ){ continue; }
		var a = ASTGUI.getFieldValue(arrF[i]) ,  b = ASTGUI.getFieldValue(arrF[i+1]) ;
		var tmp_arr = arrF.withOut( arrF[i] ).withOut( arrF[i+1] ) ;
		for( var j =0 ; j < tmp_arr.length ; j=j+2 ){
			var c = Number( ASTGUI.getFieldValue(tmp_arr[j]) ) ;
			var d = Number( ASTGUI.getFieldValue(tmp_arr[j+1]) ) ;
			if( a.isValueInBetween(c,d) ) {
				_$(arrF[i]).focus();
				_$(arrF[i]).select();
				ASTGUI.feedback( { msg:'Invalid Range', showfor:2 });
				return false;
			}
			if( b.isValueInBetween(c,d) ){
				_$(arrF[i+1]).focus();
				_$(arrF[i+1]).select();
				ASTGUI.feedback( { msg:'Invalid Range', showfor:2 });
				return false;
			}
		}
	}
	return true;
};

var saveDisableExtensionRanges = function(){
	var disabled = ASTGUI.getFieldValue("disable_extension_ranges");
	ASTGUI.updateFieldToValue('disable_extension_ranges', 'no');
	if(disabled == 'yes'){
		if(!confirm("The Asterisk GUI uses these settings to help ensure that you cannot create extensions "
			+ "that may cause problems in Asterisk.  We really do not recommend disabling this feature.   Are "
			+ "you sure that you want to disable this?  Click OK to disable.")){return;}
		ASTGUI.updateFieldToValue('disable_extension_ranges', 'yes');
	}
	refreshExtensionRangesForm(disabled);
};

var refreshExtensionRangesForm = function(disabled){
	if(disabled == 'yes'){
		_$('ue_start').disabled = true;
		_$('ue_end').disabled = true;
		_$('mm_start').disabled = true;
		_$('mm_end').disabled = true;
		_$('vme_start').disabled = true;
		_$('vme_end').disabled = true;
		_$('rge_start').disabled = true;
		_$('rge_end').disabled = true;
		_$('qe_start').disabled = true;
		_$('qe_end').disabled = true;
		_$('vmg_start').disabled = true;
		_$('vmg_end').disabled = true;
		$('#reset_ranges_button').hide();
	}else{
		_$('ue_start').disabled = false;
		_$('ue_end').disabled = false;
		_$('mm_start').disabled = false;
		_$('mm_end').disabled = false;
		_$('vme_start').disabled = false;
		_$('vme_end').disabled = false;
		_$('rge_start').disabled = false;
		_$('rge_end').disabled = false;
		_$('qe_start').disabled = false;
		_$('qe_end').disabled = false;
		_$('vmg_start').disabled = false;
		_$('vmg_end').disabled = false;
		$('#reset_ranges_button').show();
	}
};

var save_changes = function(){
	var disabled = ASTGUI.getFieldValue("disable_extension_ranges");
	var x = new listOfSynActions(ASTGUI.globals.configfile);
	x.new_action('update', 'general', 'disable_extension_ranges', disabled );
	x.callActions();
	top.sessionData.GUI_PREFERENCES.disable_extension_ranges = disabled;

	if( !verify_Ranges() ){ return; }
	parent.sessionData.pbxinfo.GLOBALS[ASTGUI.globals.obcidstr] = DOM_obCid_input.value;

	var u = new listOfSynActions('extensions.conf');
		u.new_action('update', 'globals', ASTGUI.globals.obcidstr, ASTGUI.getFieldValue(DOM_obCid_input) );
		u.new_action('update', 'globals', ASTGUI.globals.obcidNamestr, ASTGUI.getFieldValue('obCidName_input') );
		u.new_action('update', 'globals', 'RINGTIME', ASTGUI.getFieldValue('op_ringTimeOut_Voicemail') );

		u.new_action('delete', 'default', 'exten','' ,'o,1,' + OPEXTENSION );
		if (ASTGUI.getFieldValue(DOM_op_ext) != '') {
			u.new_action('append', 'default', 'exten', 'o,1,' + ASTGUI.getFieldValue(DOM_op_ext) );
		}
	u.callActions();

	if ((ASTGUI.version.gteq("1.6.0") || parent.sessionData.PLATFORM.isAA50 || parent.sessionData.PLATFORM.isABE) && !parent.sessionData.PLATFORM.isAA50_OEM) {
		u.clearActions('phoneprov.conf');
		u.new_action('delete', 'polycom', 'setvar','', 'IDLEIMAGE_ACTIVE=1' );
		u.new_action('delete', 'polycom', 'setvar','', 'IDLEIMAGE_ACTIVE=0' );
		if( _$('op_polycom_idleimage').checked ){
			u.new_action( 'append', 'polycom', 'setvar', 'IDLEIMAGE_ACTIVE=1' );
		}else{
			u.new_action( 'append', 'polycom', 'setvar', 'IDLEIMAGE_ACTIVE=0' );
		}

		if( polycom_digitmap_string ){
			u.new_action('delete', 'polycom', 'setvar', '' , polycom_digitmap_string );
		}
		if( _$('op_voip_digitmap').value ){
			u.new_action( 'append', 'polycom', 'setvar', 'DIGITMAP=' + _$('op_voip_digitmap').value );
		}

		if( polycom_digitmap_timeout_string ){
			u.new_action('delete', 'polycom', 'setvar', '' , polycom_digitmap_timeout_string );
		}
		if( _$('op_voip_digit_timeout').value ){
			u.new_action( 'append', 'polycom', 'setvar', 'DIGITMAPTIMEOUT=' + _$('op_voip_digit_timeout').value );
		}

		u.callActions();
	}

	u.clearActions(ASTGUI.globals.configfile);
	u.new_action('update', 'general', 'ue_start', ASTGUI.getFieldValue('ue_start') );
	u.new_action('update', 'general', 'ue_end', ASTGUI.getFieldValue('ue_end') );
	u.new_action('update', 'general', 'mm_start', ASTGUI.getFieldValue('mm_start') );
	u.new_action('update', 'general', 'mm_end', ASTGUI.getFieldValue('mm_end') );
	u.new_action('update', 'general', 'qe_start', ASTGUI.getFieldValue('qe_start') );
	u.new_action('update', 'general', 'qe_end', ASTGUI.getFieldValue('qe_end') );
	u.callActions();
	u.clearActions();
	u.new_action('update', 'general', 'vme_start', ASTGUI.getFieldValue('vme_start') );
	u.new_action('update', 'general', 'vme_end', ASTGUI.getFieldValue('vme_end') );
	u.new_action('update', 'general', 'rge_start', ASTGUI.getFieldValue('rge_start') );
	u.new_action('update', 'general', 'rge_end', ASTGUI.getFieldValue('rge_end') );
	u.new_action('update', 'general', 'vmg_start', ASTGUI.getFieldValue('vmg_start') );
	u.new_action('update', 'general', 'vmg_end', ASTGUI.getFieldValue('vmg_end') );
	u.callActions();

	parent.sessionData.GUI_PREFERENCES.updateProperties({
		ue_start: ASTGUI.getFieldValue('ue_start'),
		ue_end: ASTGUI.getFieldValue('ue_end'),
		mm_start: ASTGUI.getFieldValue('mm_start'),
		mm_end: ASTGUI.getFieldValue('mm_end'),
		qe_start: ASTGUI.getFieldValue('qe_start'),
		qe_end: ASTGUI.getFieldValue('qe_end'),
		vme_start: ASTGUI.getFieldValue('vme_start'),
		vme_end: ASTGUI.getFieldValue('vme_end'),
		rge_start: ASTGUI.getFieldValue('rge_start'),
		rge_end: ASTGUI.getFieldValue('rge_end'),
		vmg_start: ASTGUI.getFieldValue('vmg_start'),
		vmg_end: ASTGUI.getFieldValue('vmg_end')
	});

	parent.sessionData.pbxinfo.localextensions.Operator = DOM_op_ext.value ;
	ASTGUI.feedback({msg:' Saved !!', showfor: 3 , color: '#5D7CBA', bgcolor: '#FFFFFF'}) ;
	window.location.reload();
}


var reset_ranges_default = function(){
	ASTGUI.updateFieldToValue('ue_start', '6000');
	ASTGUI.updateFieldToValue('ue_end', '6299');
	ASTGUI.updateFieldToValue('mm_start', '6300');
	ASTGUI.updateFieldToValue('mm_end', '6399');
	ASTGUI.updateFieldToValue('vme_start', '7000');
	ASTGUI.updateFieldToValue('vme_end', '7100');
	ASTGUI.updateFieldToValue('rge_start', '6400');
	ASTGUI.updateFieldToValue('rge_end', '6499');
	ASTGUI.updateFieldToValue('qe_start', '6500');
	ASTGUI.updateFieldToValue('qe_end', '6599');
	ASTGUI.updateFieldToValue('vmg_start', '6600');
	ASTGUI.updateFieldToValue('vmg_end', '6699');
};
