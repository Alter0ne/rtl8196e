/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * flashupdate.html functions
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
var uImage_filename;
var uImage_uploadpath = "/var/lib/asterisk/sounds/imageupdate";
loaded_external = false;
current_version = '';
starteduploading = 0;
var overlay_upload_Path ;
var overlay_disk_Path = '/var/lib/asterisk/sounds/asteriskoverlay/';

var check_forNewFirmwareVersions = function(){
	parent.ASTGUI.dialog.waitWhile(' Checking for firmware updates ...');
	$.getScript( ASTGUI.globals.firmwareVersionUrl , function(){
		parent.ASTGUI.dialog.hide();
		try{
			if(loaded_external){
				// latest_firmware_version
				var cv = {};
				var lv = {};

				var tmp_current_version_array = current_version.split('.') ;
				var tmp_latest_firmware_version_array = latest_firmware_version.split('.');

				if( tmp_current_version_array.length >= 3 ){
					cv.major = Number( tmp_current_version_array[0] || 0 ) ;
					cv.minor = Number( tmp_current_version_array[1] || 0  ) ;
					cv.point = Number( tmp_current_version_array[2] || 0  ) ;
					cv.subpoint = Number( tmp_current_version_array[3] || 0 ) ;
	
					lv.major = Number( tmp_latest_firmware_version_array[0] || 0 ) ;
					lv.minor = Number( tmp_latest_firmware_version_array[1] || 0 ) ;
					lv.point = Number( tmp_latest_firmware_version_array[2] || 0 ) ;
					lv.subpoint = Number( tmp_latest_firmware_version_array[3] || 0 ) ;

					if(lv.major > cv.major || (lv.major == cv.major && lv.minor > cv.minor) || ( cv.major == lv.major && cv.minor == lv.minor && lv.point > cv.point ) || ( cv.major == lv.major && cv.minor == lv.minor && lv.point == cv.point && lv.subpoint > cv.subpoint ) ){
						alert('A newer version of firmware ('+ latest_firmware_version +') is available !!');
						return;
					}
				}

				alert( 'You have ' + current_version + ' installed !!' + '\n' 
					+ 'The latest firmware version is ' + latest_firmware_version );
				return;
			}
		}catch(err){}
	});

	(function (){
		var oops = function(){ 
			if(loaded_external){return;}
			parent.ASTGUI.dialog.hide();
			alert("could not connect to the server");
		};
		setTimeout( oops , 70000 ); // if not in 7 seconds - then consider as failed
	})();
};


var execute_applyuimage = function(a){
	var b;
	ASTGUI.feedback( { msg:'Upgrading firmware', showfor:30 });
	parent.ASTGUI.dialog.waitWhile('Upgrading Firmware ... ');

	if(a=="http"){
		b = "imageupload wget -O "+ uImage_uploadpath + "/uImage " + _$('httpurl').value;    
	}else if(a == "file"){
		b = "imageupload mv " + uImage_uploadpath + "/" + uImage_filename + " " + uImage_uploadpath + "/uImage";
	}else if(a == "tftp"){
		if( _$('tftp_filename').value.length ){
			var fname = " " + _$('tftp_filename').value ;
		}else{
			var fname = "" ;
		}
		b = "imageupload tftp -g -r " + fname + " -l " + uImage_uploadpath +  "/uImage " + _$('tftpurl').value;
	}
	parent.ASTGUI.systemCmd( b, function(){ check_flashupdateresult();} );
};


var check_flashupdateresult = function(){
	var tmp = ASTGUI.loadHTML('/static/flashresult');
	if ( tmp.match("FAIL: ") ) {
		parent.ASTGUI.dialog.hide();
		ASTGUI.feedback( { msg:'Firmware upgrade FAILED', showfor:5 });
		alert("Firmware Update FAILED" + "\n"+ tmp);
		return ;
	}
	if ( tmp.match("PASS: ") ) {
		flashupdate_success();
	}
};


var flashupdate_success = function(){
	parent.ASTGUI.dialog.hide();
	ASTGUI.feedback( { msg:'Firmware image copied', showfor:5 });
	top.cookies.set('firmware_reboot','yes');
	_$('fstatus').innerHTML = 'Finished copying firmware';
	parent.ASTGUI.yesOrNo({
		hideNo: true,
		msg: "Finished copying firmware <BR> Click 'Ok' to reboot your appliance. <BR><BR> Note: reboot might take 5 to 8 minutes while upgrading firmware" ,
		ifyes: function(){
			if( parent.sessionData.PLATFORM.isAA50 && top.cookies.get('configFilesChanged') == 'yes' ){
				parent.ASTGUI.dialog.waitWhile('Rebooting !');
				setTimeout( function(){
						parent.ASTGUI.dialog.hide();
						parent.ASTGUI.yesOrNo({
							msg: 'You have unsaved changes !! <BR>Do you want to save these changes before rebooting ?' ,
							ifyes: function(){
								ASTGUI.systemCmd ('save_config', function(){
									top.cookies.set( 'configFilesChanged' , 'no' );
									ASTGUI.systemCmd ('reboot', parent.miscFunctions.AFTER_REBOOT_CMD );
								});
							},
							ifno:function(){
								top.cookies.set( 'configFilesChanged' , 'no' );
								ASTGUI.systemCmd ('reboot', parent.miscFunctions.AFTER_REBOOT_CMD );
							},
							title : 'Save changes before reboot ?',
							btnYes_text :'Yes',
							btnNo_text : 'No'
						});
					}, 2000 );
			}else{
				ASTGUI.systemCmd ('reboot', parent.miscFunctions.AFTER_REBOOT_CMD );
			}
		},
		ifno:function(){
			
		},
		title : 'Click Ok to reboot',
		btnYes_text :'Ok',
		btnNo_text : 'Cancel'
	});


}

var localajaxinit = function (){
	top.document.title = 'Update appliance firmware' ;
	if( !parent.sessionData.hasCompactFlash ){
		//ASTGUI.dialog.alertmsg('You need a CompactFlash&reg; to use this feature');
		_$('nocf').style.display = '';
		return false;
	}

	_$('mainDiv').style.display = '';
	_$('updatetype_http').checked = true;
	switch_httptftp('h');

	var c = config2json({filename:'http.conf', usf:1});
	_$('tdupload').style.display = ( c.hasOwnProperty('post_mappings') && c['post_mappings'].hasOwnProperty('uploads') && c['post_mappings']['uploads'] == uImage_uploadpath) ? '' : 'none';

	ASTGUI.systemCmdWithOutput( "ls /var/lib/asterisk/sounds/a*" , function(a){
		if( a.contains('/var/lib/asterisk/sounds/asteriskoverlay') ){
			_$('uploadOVERLAY_iframe').src = "upload_form.html" ;
			$("#overlayUpload_TR").show();
		}
		if(!parent.sessionData.PLATFORM.isAA50_OEM ){
			$('#UpdatePolycomFirmware').show();
			ASTGUI.systemCmdWithOutput( 'firmware_version' , function(a){
				current_version = a.trim();
				_$('span_current_fwversion').innerHTML = '<B> Current Firmware version : ' + current_version + '&nbsp;&nbsp;</B>';
				_$('check_forNewFirmwareVersions_button').style.display = '';
			});
		}
	});
	
};

var switch_httptftp = function(a){
	if(a=='h'){
		_$('tr_1').style.display = "";
		_$('tr_2').style.display = "none";
		_$('tr_3').style.display = "none";
	}
	if(a=='t'){
		_$('tr_1').style.display = "none";
		_$('tr_2').style.display = "";
		_$('tr_3').style.display = "";
	}
};

var call_flashupdate = function(){
	var h = _$('updatetype_http');
	var hu = _$('httpurl');
	var t = _$('updatetype_tftp');
	var tu = _$('tftpurl');

	if( h.checked && !hu.value.length ){
		ASTGUI.dialog.alertmsg('Please enter the url of the flash image');
		return false;
	}

	if( t.checked && !tu.value.length ){
		ASTGUI.dialog.alertmsg('Please enter a TFTP server');
		return false;
	}

	if( h.checked ){
		execute_applyuimage("http");
		return;
	}

	if( t.checked ){
		execute_applyuimage("tftp");
		return;
	}
};


var onUploadForm_load = function(){
	if(!top.sessionData.httpConf.postmappings_defined || !top.sessionData.httpConf.uploadPaths['backups'] ){
		top.log.error('AG102');
		$('#uploadForm_container').hide();
		return ;
	}

	overlay_upload_Path = top.sessionData.httpConf.uploadPaths['backups'] ;
	var upload_action_path = (top.sessionData.httpConf.prefix) ? '/' + top.sessionData.httpConf.prefix + '/backups' : '/backups' ;
	_$('uploadOVERLAY_iframe').contentWindow.document.getElementById('form22').action =  upload_action_path ;
	_$('uploadOVERLAY_iframe').contentWindow.document.getElementById('UploadFORM_UPLOAD_BUTTON').value =  'Upload OverLay file' ;
};


var onUploadForm_beforeUploading = function(){
	if( !upload_Filename || !upload_Filename.toLowerCase().endsWith('.tar') ){
		alert('overlay file needs to be a tar file !');
		return false;
	}

	starteduploading = 1;
	parent.ASTGUI.dialog.waitWhile('File Upload in progress, please wait ..');
	return true;
};


var onUploadForm_unload = function(){
	if(!starteduploading){ return; }
	if ( overlay_upload_Path.endsWith('/') ){ overlay_upload_Path = overlay_upload_Path.rChop('/'); }

	ASTGUI.feedback({ msg:'Overlay File Uploaded !!', showfor: 3 });
	$('#overlayUpload_TR').hide();
	parent.ASTGUI.dialog.waitWhile('unpacking overlay file');


	ASTGUI.systemCmd( 'rm '+ overlay_disk_Path + '* -rf' , function(){
		ASTGUI.systemCmd( 'tar -xf '+ overlay_upload_Path + '/' + upload_Filename + ' -C ' + overlay_disk_Path, function(){
			ASTGUI.systemCmd( 'rm -f '+ overlay_upload_Path + '/' + upload_Filename , function(){
				ASTGUI.feedback({ msg:'Done !!', showfor: 3 });
				var t = top.window.location.href;
				top.window.location.replace(t,true);
			});
		});
	});
	return;
};
