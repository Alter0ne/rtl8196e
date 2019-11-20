/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * voicemail.html functions
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

//
/*
; direct voicemail dial - version 0.1
[default]
exten = _*6XXX,1,VoiceMail(${EXTEN:1}@default) 
exten = 5700,1,VoiceMailMain(${CALLERID(num)}@default)



; direct voicemail dial - version 0.2
[default]
exten = _#6XXX,1,VoiceMail(${EXTEN:1}@default,s) 
exten = 5700,1,VoiceMailMain(${CALLERID(num)}@default)



; direct voicemail dial - version 0.3
[default]
exten = _#6XXX,1,Set(MBOX=${EXTEN:1}@default)
exten = _#6XXX,n,VoiceMail(${MBOX})
exten = a,1,VoicemailMain(${MBOX})
exten = 5700,1,VoiceMailMain(${CALLERID(num)}@default)


*/
//


var TMP_VMEXTEN_TODELETE = '';
var TODELETE = '';
var OLD_DVD = '1,VoiceMail(${EXTEN:1}@default' ;
//var DVD = ',1,VoiceMail(${EXTEN:1}@default,s)' ; // direct voicemail dial

var localajaxinit = function( ){
	(function (){
		var t = [
			{url:'#', desc:'General Settings', selected:true } ,
			{url:'emailsettings.html', desc:'Email Settings for VoiceMails'}
		];
		if(parent.sessionData.PLATFORM.isAA50 ){
			t.push( {url:'smtp_settings.html', desc:'SMTP Settings'} );
		}
		ASTGUI.tabbedOptions( _$('tabbedMenu') , t);
	})();
	top.document.title = 'General Voice Mail Settings' ;

	(function (){
		if(parent.sessionData.pbxinfo['localextensions'].hasOwnProperty('VoiceMailMain') ){
			TODELETE = parent.sessionData.pbxinfo['localextensions']['VoiceMailMain'] ;
			_$('VoiceMailExtension').value = ASTGUI.parseContextLine.getExten(TODELETE);
		}
	})();

	(function (){
		_$('directVoiceMailDial').checked = false;
		var r = context2json({ filename:'extensions.conf' , context : 'default' , usf:0 });
		for( var ri = 0; ri < r.length ; ri ++ ){
			if( ((r[ri].beginsWith('exten=_*') || r[ri].beginsWith('exten=_#') ) && r[ri].contains(OLD_DVD) ) || r[ri].contains( 'Set(MBOX=${EXTEN:1}@default)' ) ){
				TMP_VMEXTEN_TODELETE = ASTGUI.parseContextLine.getExten(r[ri]);
				_$('directVoiceMailDial').checked = true;
				break;
			}
		}
	})();

	var c = config2json({filename:'voicemail.conf', usf:1});
	if(! c.hasOwnProperty('general') ){return;}

	_$('maxgreet').value = ( c.general.hasOwnProperty('maxgreet') ) ? c.general.maxgreet : '' ;
	_$('operator').checked = ( c.general.hasOwnProperty('operator') && c.general.operator =='yes' ) ? true : false ;

	var maxmsg = ( c.general.hasOwnProperty('maxmsg') ) ? c.general.maxmsg : '' ;
	ASTGUI.selectbox.selectOption_Add( _$('maxmsg') , maxmsg );
	var maxmessage = ( c.general.hasOwnProperty('maxmessage') ) ? c.general.maxmessage : '' ;
	ASTGUI.selectbox.selectOption_Add( _$('maxmessage') , maxmessage );
	var minmessage = ( c.general.hasOwnProperty('minmessage') ) ? c.general.minmessage : '' ;
	ASTGUI.selectbox.selectOption_Add( _$('minmessage') , minmessage );

	_$('saycid').checked = ( c.general.hasOwnProperty('saycid') && c.general.saycid =='yes' ) ? true : false ;
	_$('sayduration').checked = ( c.general.hasOwnProperty('sayduration') && c.general.sayduration =='yes' ) ? true : false ;
	_$('envelope').checked = ( c.general.hasOwnProperty('envelope') && c.general.envelope =='yes' ) ? true : false ;
	_$('review').checked = ( c.general.hasOwnProperty('review') && c.general.review=='yes' ) ? true : false ;
}


var update = function(){
	var after = function(){
		var u = new listOfSynActions('extensions.conf') ;
		if(TODELETE){
			u.new_action('delete', 'default', 'exten', '', TODELETE);
			delete parent.sessionData.pbxinfo['localextensions']['VoiceMailMain'];
		}

		var y = ASTGUI.getFieldValue('VoiceMailExtension');
		if(y){
			ASTGUI.updateaValue({file:'users.conf', context :'general', variable :'vmexten', value : y});
			var t = y + ',1,VoiceMailMain(${CALLERID(num)}@default)' ;
			u.new_action('append', 'default', 'exten', t );
			parent.sessionData.pbxinfo['localextensions']['VoiceMailMain'] = t;
		}
		u.callActions();

		ASTGUI.feedback( { msg: 'Changes Saved !', showfor: 2 , color: 'green', bgcolor: '#FFFFFF' } );
		window.location.reload();
	};


	(function(){
		var tmp_arr = [ 'exten=a,' ] ;
		if( TMP_VMEXTEN_TODELETE.trim().length ){
			tmp_arr.push( 'exten=' + TMP_VMEXTEN_TODELETE );
		}

		ASTGUI.miscFunctions.delete_LinesLike(
			{
				context_name : 'default' ,
				beginsWithArr: tmp_arr ,
				filename: 'extensions.conf',
				//hasThisString: TMP_VMEXTEN_TODELETE,
				cb:function(){
					var ues = parent.sessionData.GUI_PREFERENCES.ue_start ;
					if( _$('directVoiceMailDial').checked && ues ){
						var tmp_dvdExten = '_#' + ues.charAt(0) + 'X'.times( String(ues).length -1 ) ;
						var u = new listOfSynActions('extensions.conf');
						u.new_action('append', 'default', 'exten', tmp_dvdExten + ',1,Set(MBOX=${EXTEN:1}@default)' );
						u.new_action('append', 'default', 'exten', tmp_dvdExten + ',n,VoiceMail(${MBOX})' );
						u.new_action('append', 'default', 'exten', 'a,1,VoicemailMain(${MBOX})');
						u.callActions();
					}
				}
			}
		);
	})();


	var x = new listOfActions();
	x.filename('voicemail.conf');
	x.new_action( 'update', 'general' , 'maxgreet', _$('maxgreet').value );
	var tmp_operator = (_$('operator').checked) ?'yes' : 'no' ;
	x.new_action( 'update', 'general' , 'operator', tmp_operator );
	x.new_action( 'update', 'general' , 'maxmsg', _$('maxmsg').value );
	x.new_action( 'update', 'general' , 'maxmessage', _$('maxmessage').value );
	x.new_action( 'update', 'general' , 'minmessage', _$('minmessage').value );

	var tmp_saycid = (_$('saycid').checked) ?'yes' : 'no' ;
	x.new_action( 'update', 'general' , 'saycid', tmp_saycid );
	var tmp_sayduration = (_$('sayduration').checked) ?'yes' : 'no' ;
	x.new_action( 'update', 'general' , 'sayduration', tmp_sayduration );
	if( tmp_sayduration.isAstTrue() ){
		x.new_action( 'update', 'general' , 'saydurationm', '0' );
	}
	var tmp_envelope = (_$('envelope').checked) ?'yes' : 'no' ;
	x.new_action( 'update', 'general' , 'envelope', tmp_envelope );
	var tmp_review = (_$('review').checked) ?'yes' : 'no' ;
	x.new_action( 'update', 'general' , 'review', tmp_review );

	x.callActions(after) ;

}
