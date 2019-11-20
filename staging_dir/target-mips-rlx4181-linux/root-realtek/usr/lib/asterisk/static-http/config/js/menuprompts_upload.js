/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * menuprompts_upload functions
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

var upload_Filename = ""; // will be updated by upload_form.html
var starteduploading = 0;
var upload_Path; // path for 'uploads' as defined in http.conf - this variable will be automatically updated from http.conf

onUploadForm_load = function(){
	if(!top.sessionData.httpConf.postmappings_defined || !top.sessionData.httpConf.uploadPaths['voicemenuprompts'] ){
		$('#uploadForm_container').hide();
		parent.ASTGUI.dialog.waitWhile( 'Creating configuration needed for uploading voice prompts ...' );

		var tmp_a = top.sessionData.directories.menusRecord.rChop('/');
		var u = new listOfSynActions('http.conf') ;
			if( !top.sessionData.httpConf.postmappings_defined ){
				u.new_action( 'newcat', 'post_mappings', '', '');
			}
			u.new_action('append', 'post_mappings', 'voicemenuprompts', tmp_a ) ;
			u.callActions();
			top.sessionData.httpConf.postmappings_defined = true ;
			top.sessionData.httpConf.uploadPaths['voicemenuprompts'] = tmp_a ;

		ASTGUI.cliCommand('reload');
		setTimeout(
			function(){
				parent.ASTGUI.dialog.hide();
				window.location.reload();
			}
		, 1000 ); // reload page
		return ;
	}

	var upload_action_path = (top.sessionData.httpConf.prefix) ? '/' + top.sessionData.httpConf.prefix + '/voicemenuprompts' : '/voicemenuprompts' ;
	_$('uploadiframe').contentWindow.document.getElementById('form22').action =  upload_action_path ;
}; 


onUploadForm_beforeUploading = function(){
	if(!parent.sessionData.PLATFORM.isAA50 && !parent.sessionData.PLATFORM.isABE && ASTGUI.version.lt("1.6.0")) {
		alert("File Uploads are supported in Asterisk 1.6.0 and higher.");
		return;
	}
	var tmp_fname = upload_Filename.toLowerCase();
	if( tmp_fname.endsWith('.wav') || tmp_fname.endsWith('.gsm') || tmp_fname.endsWith('.ulaw') || tmp_fname.endsWith('.alaw') ){
		starteduploading = 1;
		ASTGUI.dialog.waitWhile('File Upload in progress, please wait ..');
		return true;
	}else{
		starteduploading = 0;
		ASTGUI.feedback({ msg:'Sound file must be wav, gsm, ulaw or alaw !!', showfor: 3, color: 'red' });
		alert('Sound file must be wav, gsm, ulaw or alaw !');
		return false;
	}
};

onUploadForm_unload = function(){
	if(!starteduploading){ return; }
	ASTGUI.dialog.waitWhile('VoiceMenu prompt Uploaded');
	ASTGUI.feedback({ msg:'VoiceMenu prompt Uploaded !!', showfor: 3 });
	$('#uploadForm_container').hide();
	setTimeout( function(){
		ASTGUI.dialog.hide();
		window.location.href = 'menuprompts_record.html'; 
	} , 1000 );
	return;
};

function localajaxinit() {
	top.document.title = 'Upload a custom Voice Menu Prompt';
	if( parent.sessionData.PLATFORM.isAA50 && !parent.sessionData.hasCompactFlash ){
		$('#nocf').show();
		$('#uploadForm_container').hide();
		return true;
	}

	parent.ASTGUI.dialog.waitWhile('Loading ...');
	parent.ASTGUI.systemCmd( "mkdir -p " + top.sessionData.directories.menusRecord , function(){
		parent.ASTGUI.dialog.hide();
	});

	ASTGUI.events.add( _$('BACKBUTTON') , 'click' , function(){ window.location.href = 'menuprompts_record.html' ;} );

};
