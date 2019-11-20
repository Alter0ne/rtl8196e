/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * iax.html functions
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
//Realtime :  'rtcachefriends', 'rtignoreexpire', 'rtupdate', 'rtautoclear' 
// CDR: amaflags, accountcode

var fieldnames = [ 'adsi', 'authdebug', 'autokill', 'bandwidth', 'bindaddr', 'bindport', 'codecpriority', 'delayreject', 'dropcount', 'forcejitterbuffer', 'iaxcompat', 'iaxmaxthreadcount', 'iaxthreadcount', 'jitterbuffer', 'jittershrinkrate', 'language', 'maxexcessbuffer', 'maxjitterbuffer', 'maxjitterinterps', 'maxregexpire', 'minexcessbuffer', 'minregexpire', 'mohinterpret', 'mohsuggest', 'nochecksums','resyncthreshold', 'tos', 'trunkfreq', 'trunktimestamps', 'calltokenoptional', 'maxcallnumbers', 'maxcallnumbers_nonvalidated'];
var callNumberLimitsList = [];

var localajaxinit = function(){
	top.document.title = 'global IAX settings' ;
	(function(){
		var hideall = function(){
			$('#iaxoptions_general').hide() ;
			//$('#iaxoptions_cdr').hide() ;
			$('#iaxoptions_jitterBuffer').hide() ;
			$('#iaxoptions_trunkregistration').hide() ;
			//$('#iaxoptions_realtime').hide() ;
			$('#iaxoptions_Codecs').hide() ;
			$('#iaxoptions_security').hide() ;
		};

		var t = [
			{url:'#', desc:'General', click_function: function(){ hideall(); $('#iaxoptions_general').show(); }  } ,
			//{url:'#', desc:'CDR', click_function: function(){ hideall(); $('#iaxoptions_cdr').show(); } },
			{url:'#', desc:'Jitter Buffer', click_function: function(){ hideall(); $('#iaxoptions_jitterBuffer').show();} },
			{url:'#', desc:'Registration', click_function: function(){ hideall(); $('#iaxoptions_trunkregistration').show(); } },
			//{url:'#', desc:'Realtime', click_function: function(){ hideall(); $('#iaxoptions_realtime').show(); } },
			{url:'#', desc:'Codecs', click_function: function(){ hideall(); $('#iaxoptions_Codecs').show(); } },
			{url:'#', desc:'Security', click_function: function(){ hideall(); $('#iaxoptions_security').show(); } }
		];
		ASTGUI.tabbedOptions( _$('tabbedMenu') , t );
		$('#tabbedMenu').find('A:eq(0)').click();
	})();

	var c = context2json({ filename:'iax.conf' , context : 'general' , usf:1 });
	var AU = ASTGUI.updateFieldToValue ; // temporarily cache function
	fieldnames.each( function(fld){
		var val = ( c[fld] ) ? c[fld] : '';
		AU(fld,val) ;
	});

	var d = context2json({ filename:'iax.conf' , context : 'callnumberlimits' , usf:0 });
	if(d){
		d.each( function(fld){
			pushNewCallNumberLimit(fld);
		});
	}

	var disallowed = false;
	var real_codecs;
	ASTGUI.CODECSLIST.populateCodecsList(_$('allow'));
	if( c.hasOwnProperty('allow') ){ real_codecs = c['array']; }
	if( c.hasOwnProperty('disallow') ) { disallowed = c['disallow'].split(','); } 
	var default_selected = ['ulaw','alaw','gsm'];
	default_selected.each( function(val) {
		if (!disallowed.contains(val)) {
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
	var x = new listOfActions('iax.conf');
 	/* Can't use fieldnames.each here because the return statement
 	would return from the anonymous function passed to the iterator, 
 	not saveChanges(). */
 	for(var i = 0; i < fieldnames.length ; i++){
 		var fld = fieldnames[i];
 		var val = ASTGUI.getFieldValue(fld).trim();
		if (val == "") {
			if (skip_ifempty.contains(fld)) {
				return;
			}
			x.new_action('delete', cat , fld , '') ;
		} else {
 			if (!ASTGUI.validateFields([fld])) { return; }
			x.new_action('update', cat , fld , val) ;
		}
 	}
 
 	x.new_action('delcat', 'callnumberlimits', '', '');
 	if (callNumberLimitsList.length >= 0){
 		x.new_action('newcat', 'callnumberlimits', '', '')
 	}
 
 	callNumberLimitsList.each( function(eachOne){
 		var pieces = eachOne.split('=');
 		x.new_action('append', 'callnumberlimits' , pieces[0], pieces[1]) ;
 	});

	x.new_action('delete', cat , 'disallow', '' ) ;
	x.new_action('delete', cat , 'allow', '' ) ;
	x.new_action('append', cat , 'disallow', 'all' ) ;
	x.new_action('append', cat , 'allow', ASTGUI.CODECSLIST.getSelectedCodecs(_$('allow')) ) ;

	parent.ASTGUI.dialog.waitWhile(' Saving ...');
	setTimeout( function(){ x.callActions(after) ; } , 300 );
	hideCallNumberLimitsForm();
}

var showCallNumberLimitsForm = function(){ 
	ASTGUI.updateFieldToValue('form_newCallNumberLimit_ip', '');
	ASTGUI.updateFieldToValue('form_newCallNumberLimit_maxcallnumbers', '');
	$('.form_newCallNumberLimit').show();
	$($('.form_newCallNumberLimit')[0]).hide();
}

var hideCallNumberLimitsForm = function(){
	$('.form_newCallNumberLimit').hide();
	$($('.form_newCallNumberLimit')[0]).show();
}

var pushNewCallNumberLimit = function(limit_str){
	if(limit_str){
		var p = limit_str.split("=");
		var new_ip = p[0];
		var new_maxcallnumbers = p[1];
	}else{
		var new_ip = ASTGUI.getFieldValue( 'form_newCallNumberLimit_ip' );
		var new_maxcallnumbers = ASTGUI.getFieldValue( 'form_newCallNumberLimit_maxcallnumbers' );
	}
	var tmp_pieces = new_ip.split("/");
	if (!limit_str && !ASTGUI.validateFields(['form_newCallNumberLimit_ip'])){
		return;
	} else if (!callNumberLimitsList.contains(new_ip + "=" + new_maxcallnumbers)){
		callNumberLimitsList.push(new_ip + "=" + new_maxcallnumbers);
		clearCallNumberLimitsForm();
		refreshCallNumberLimitsList();
	}
}

var deleteCallNumber = function(indexno){
	callNumberLimitsList.splice(indexno, 1);
	refreshCallNumberLimitsList();
}

var clearCallNumberLimitsForm = function(){
		ASTGUI.updateFieldToValue('form_newCallNumberLimit_ip', '');
		ASTGUI.updateFieldToValue('form_newCallNumberLimit_maxcallnumbers', '');
}

var refreshCallNumberLimitsList = function(){
	ASTGUI.domActions.removeAllChilds( 'callNumberLimits');

	for (var i = 0; i < callNumberLimitsList.length; i++){
		var pieces = callNumberLimitsList[i].split("=");

		var row_div = document.createElement('div');
		row_div.id = callNumberLimitsList[i];
		row_div.innerHTML = pieces[0] + " = " + pieces[1];

		var sp_delete = document.createElement('span');
		sp_delete.className = 'callnumber_delete';
		sp_delete.innerHTML = '&nbsp;';
		sp_delete.id = "callNumberLimit" + i;
		row_div.appendChild(sp_delete);

		_$('callNumberLimits').appendChild(row_div);
		$('#callNumberLimit' + i).click(function(e){
			deleteCallNumber($(this).attr('id').substr(15));
		});
	}

}

