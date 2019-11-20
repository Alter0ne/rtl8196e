/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * sip.html functions
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
// Realtime options - 'rtautoclear','rtcachefriends','rtsavesysname','rtupdate','ignoreregexpire'

var fieldnames = ['allowexternaldomains' ,'allowguest' ,'allowoverlap' ,'allowsubscribe' ,'allowtransfer' ,'alwaysauthreject' ,'autodomain' ,'bindaddr' ,'bindport' ,'callevents' ,'canreinvite' ,'checkmwi' ,'compactheaders' ,'context' ,'defaultexpiry', 'domain' ,'dtmfmode' ,'dumphistory' ,'externhost' ,'externip' ,'externrefresh' ,'fromdomain' ,'g726nonstandard','jbenable' ,'jbforce' ,'jbimpl' ,'jblog' ,'jbmaxsize' ,'jbresyncthreshold' ,'language' ,'localnet' ,'maxcallbitrate' ,'maxexpiry' ,'minexpiry' ,'mohinterpret' ,'mohsuggest', 'mwi_from', 'nat' ,'notifyringing' ,'pedantic' ,'progressinband' ,'promiscredir' ,'realm' ,'recordhistory' ,'register' ,'registerattempts' ,'registertimeout' ,'relaxdtmf' , 'rtpholdtimeout' ,'rtptimeout' ,'sendrpid' ,'sipdebug' ,'srvlookup' ,'subscribecontext' ,'t1min' ,'t38pt_udptl' ,'tos_audio' ,'tos_sip' ,'tos_video' ,'trustrpid' ,'useragent' ,'usereqphone' ,'videosupport'] ;


var localajaxinit = function(){
	top.document.title = 'global SIP settings' ;
	(function(){
		var hideall = function(){
			$('#sipoptions_general').hide();
			$('#sipoptions_tos').hide();
			$('#sipoptions_debugNotify').hide();
			$('#sipoptions_nat').hide();
			//$('#sipoptions_realtime').hide();
			$('#sipoptions_misc').hide();
			$('#sipoptions_JitterBuffer').hide();
			$('#sipoptions_codecs').hide();
		};

		var t = [
			{url:'#', desc:'General', click_function: function(){ hideall(); $('#sipoptions_general').show(); if (parent.sessionData.PLATFORM.isABE || parent.sessionData.PLATFORM.isAA50) { $('#mwi_from_container').show(); } else { $('#mwi_from_container').hide(); }}  } ,
			{url:'#', desc:'TOS', click_function: function(){ hideall(); $('#sipoptions_tos').show(); }  } ,
			{url:'#', desc:'DebugNotify', click_function: function(){ hideall(); $('#sipoptions_debugNotify').show(); }  } ,
			{url:'#', desc:'NAT', click_function: function(){ hideall(); $('#sipoptions_nat').show(); }  } ,
			//{url:'#', desc:'RealTime', click_function: function(){ hideall(); $('#sipoptions_realtime').show(); }  } ,
			{url:'#', desc:'Misc', click_function: function(){ hideall(); $('#sipoptions_misc').show(); }  } ,
			{url:'#', desc:'Jitter Buffer', click_function: function(){ hideall(); $('#sipoptions_JitterBuffer').show(); }  } ,
			{url:'#', desc:'Codecs', click_function: function(){ hideall(); $('#sipoptions_codecs').show(); }  }
		];
		ASTGUI.tabbedOptions( _$('tabbedMenu') , t );
		$('#tabbedMenu').find('A:eq(0)').click();
	})();


	var c = context2json({ filename:'sip.conf' , context : 'general' , usf:1 });
	var AU = ASTGUI.updateFieldToValue ; // temporarily cache function
	fieldnames.each( function(fld){
		var val = ( c[fld] ) ? c[fld] : '';
		AU(fld,val) ;
	});
	var disallowed = false;
	var real_codecs;
	ASTGUI.CODECSLIST.populateCodecsList(_$('allow'));
	if( c.hasOwnProperty('allow') ){ real_codecs = c['allow']; }
	if( c.hasOwnProperty('disallow') ) { disallowed = c['disallow'].split(','); } 
	var default_selected = ['ulaw','alaw','gsm'];
	default_selected.each( function(val) {
		if (!disallowed.contains(val) && !c.hasOwnProperty('allow')) {
			real_codecs = real_codecs + "," + val;
		}
	});
	ASTGUI.CODECSLIST.selectCodecs(_$('allow'), real_codecs);
}

var saveChanges = function(){
	var cat = 'general';
	var after = function(){
		parent.ASTGUI.dialog.hide();
		ASTGUI.feedback({ msg:'Changes Saved !', showfor:2 });
	};
	var skip_ifempty = ['register', 'localnet', 'externhost', 'externip'];
	var x = new listOfActions('sip.conf');
	var AG = ASTGUI.getFieldValue;
	fieldnames.each( function(fld){
		var val = AG(fld).trim();
		if (val == "") {
			if (skip_ifempty.contains(fld)) {
				return;
			}
			x.new_action('delete', cat , fld , '') ;
		}else{
			if(fld == "localnet"){
				x.new_action('delete', cat , fld , '') ;
				val.split(',').each(function(item){
					x.new_action('append', cat , fld , item) ;
				});
			}else{
				x.new_action('update', cat , fld , val) ;
			}
		}
	});
	x.new_action('delete', cat , 'disallow', '' ) ;
	x.new_action('delete', cat , 'allow', '' ) ;
	x.new_action('append', cat , 'disallow', 'all' ) ;
	x.new_action('append', cat , 'allow', ASTGUI.CODECSLIST.getSelectedCodecs(_$('allow')) ) ;
	parent.ASTGUI.dialog.waitWhile(' Saving ...');
	setTimeout( function(){ x.callActions(after) ; } , 300 );
}
