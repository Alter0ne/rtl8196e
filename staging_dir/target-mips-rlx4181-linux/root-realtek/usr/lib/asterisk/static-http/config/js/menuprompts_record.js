/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * menuprompts_record.html functions
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

var CURRENT_FILE ; // whatever file that needs to be deleted/playback/record over
var ACTION ; // 'PLAY'/'RECORD';

var show_record_New = function(){
	ASTGUI.showbg(true);
	_$('recordnew_content').style.display = '';
	_$('newvmenu_name').focus();
};

var show_playFile = function(x){
	ACTION = 'PLAY';
	CURRENT_FILE = x ;
	_$('playVmenu_ext_0').innerHTML = "Extension used for PlayBack :";
	_$('play_record_button').innerHTML = 'Play';
	_$('playVmenu_name').innerHTML = CURRENT_FILE;
	$('#playFile_content').showWithBg();
};

var show_recordFile = function(x){
	if(!confirm("Are you sure you want to Record over an existing Voice Menu prompt?")){ return true; }
	ACTION = 'RECORD';
	CURRENT_FILE = x ;
	_$('playVmenu_ext_0').innerHTML = "Extension used for Recording :";
	_$('play_record_button').innerHTML = 'Record';
	_$('playVmenu_name').innerHTML = CURRENT_FILE;
	$('#playFile_content').showWithBg();
};

var play_record_file = function(){
	var y = _$('playVmenu_ext').value;
	if( ACTION == 'PLAY' ) {
		if( CURRENT_FILE.endsWith('.gsm') ){
			CURRENT_FILE = CURRENT_FILE.withOut('.gsm');
		}else if( CURRENT_FILE.endsWith('.alaw') ){
			CURRENT_FILE = CURRENT_FILE.withOut('.alaw');
		}else if( CURRENT_FILE.endsWith('.g722') ){
			CURRENT_FILE = CURRENT_FILE.withOut('.g722');
		}else if( CURRENT_FILE.endsWith('.g729') ){
			CURRENT_FILE = CURRENT_FILE.withOut('.g729');
		}else if( CURRENT_FILE.endsWith('.ulaw') ){
			CURRENT_FILE = CURRENT_FILE.withOut('.ulaw');
		}else if( CURRENT_FILE.endsWith('.wav') ){
			CURRENT_FILE = CURRENT_FILE.withOut('.wav');
		}else if( CURRENT_FILE.endsWith('.mp3') ){
			CURRENT_FILE = CURRENT_FILE.withOut('.mp3');
		}
		ASTGUI.feedback( { msg:'Play Request Successfull !!', showfor:2 });
		makeRequest ({
			action  : 'originate' ,
			channel : 'Local/' + y ,
			context : ASTGUI.contexts.guitools ,
			exten   : 'play_file',
			priority : '1',
			Variable : 'var1=' + top.sessionData.directories.menusRecord + CURRENT_FILE
		});
	}
	if(ACTION == 'RECORD' ){
		RecordFile(y);
	}
	$('#playFile_content').hideWithBg();
	CURRENT_FILE = '';
};

var delete_file = function(fn){
	if(!confirm("Delete selected Voice Menu prompt ?")){ return ; }
	ASTGUI.dialog.waitWhile(' Deleting file ...');
	parent.ASTGUI.systemCmd( "/bin/rm -f '" + top.sessionData.directories.menusRecord + fn + "'", function(){ 
		ASTGUI.feedback( { msg:'Delete Request Successfull !', showfor:2 });
		setTimeout ( function(){ ASTGUI.dialog.hide(); window.location.reload(); } , 1500 );
	});
};

var record_new_Verify = function (){
	if( _$('newvmenu_name').value == "" ){ 
		alert("Please Enter a name for the new Voice Menu prompt");
		_$('newvmenu_name').focus();
		return true;
	}
	if ( _$('newvmenu_name').value.search('^[a-zA-Z_0-9]*$') == -1){
		alert("spaces and special characters are not allowed in the filename.");
		_$('newvmenu_name').focus();
		return true;
	}
	if ( _$('newvmenu_ext').value == ""){
		alert("Please Select an extension to record this Voice Menu prompt");
		_$('newvmenu_ext').focus();
		return true;
	}
	CURRENT_FILE = _$('newvmenu_name').value + _$('newvmenu_format').value ;
	$('#recordnew_content').hideWithBg();
	RecordFile( _$('newvmenu_ext').value );
};

var RecordFile = function( extension ){ // uses/dials  extension to record into CURRENT_FILE
	ASTGUI.feedback( { msg:'Record Request Successfull !!', showfor:2 });
	makeRequest ( { 
		action  : 'originate' ,
		channel : 'Local/' + extension ,
		context : ASTGUI.contexts.guitools ,
		exten   : 'record_vmenu',
		priority : '1',
		Variable : 'var1=' + top.sessionData.directories.menusRecord + CURRENT_FILE
	});

	parent.ASTGUI.dialog.waitWhile( 'Please wait while the system <BR> Calls the specified Extension ... ' );
	setTimeout(
		function(){
			ASTGUI.dialog.hide();
			ASTGUI.feedback( { msg:" When you are done recording, \n Refresh Page to update files list !!", showfor:2 } );
		} , 3000 
	);
};


var localajaxinit = function(){
	if( parent.sessionData.PLATFORM.isABE ){ // ABE-1600
		$('#uploadVoiceMenuPrompt_Button').hide();
	}

	$('#recfilesTable').hide();
	top.document.title = 'Record a Voice Menu Prompt';
	if( parent.sessionData.PLATFORM.isAA50 && !parent.sessionData.hasCompactFlash ){
		$('#nocf').show();
		$('#thispageContent').hide();
		return true;
	};
	var newvmenu_ext = _$('newvmenu_ext');
	var playVmenu_ext = _$('playVmenu_ext');
	var _rft = _$('recfilesTable');
	var ul = parent.pbx.users.list();  ul = ul.sortNumbers();
	ul.each( function(user){ // list each user in table
		ASTGUI.selectbox.append(newvmenu_ext , user , user );
		ASTGUI.selectbox.append(playVmenu_ext , user , user );
	});

	(function(){
		var newRow = _rft.insertRow(-1);
		newRow.className = "frow";
		ASTGUI.domActions.tr_addCell( newRow , { html: '#', width:'35', align: 'center' });
		ASTGUI.domActions.tr_addCell( newRow , { html: 'Name', width:'180' });
		ASTGUI.domActions.tr_addCell( newRow , { html: 'Options' } );
	})();

	$('#whereToBuy_button').tooltip({delay:0.9,showURL:false,top:15,left:-300});

	ASTGUI.listSystemFiles( top.sessionData.directories.menusRecord , function(recfiles) {
		for( var i=0 ; i < recfiles.length ; i++ ){
			var filename = recfiles[i].stripTags();
			var newRow = _rft.insertRow(-1);
			newRow.className = ((_rft.rows.length)%2==1)?'odd':'even';
			var tmp = "<input type='button' value='Record Again' onclick='show_recordFile(\"" + filename + "\")' class='splbutton'> &nbsp; <input type=button value='Play' onclick='show_playFile(\"" + filename + "\")' class='splbutton'> &nbsp; <input type='button' onclick='delete_file(\""+ filename + "\")'  value='Delete' class='splbutton'>" ;
			ASTGUI.domActions.tr_addCell( newRow , { html: _rft.rows.length-1 , align:'center', width:'35' }) ;
			ASTGUI.domActions.tr_addCell( newRow , { html: filename , width:'180' });
			ASTGUI.domActions.tr_addCell( newRow , { html: tmp , align:'center' });
		}
		if( _rft.rows.length == 1 ){
			ASTGUI.domActions.clear_table(_rft);
			var newRow = _rft.insertRow(-1);
			ASTGUI.domActions.tr_addCell( newRow , { html: '<BR><I> No custom Voice Menu prompts found !!</I> <BR><BR>' +
				"You can record a new VoiceMenu Prompt by clicking on the 'Record a new Voice Menu prompt '<BR>" +
				" or click on the 'Upload a Voice Menu prompt' button to upload a custom voice menu.<BR><BR>"
			 , align:'center' }) ;
		}
		$('#recfilesTable').show();
	});
};
