/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * Backup functions
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

var bkpPath = top.sessionData.directories.ConfigBkp ;
var upload_Filename = ""; // will be updated by upload_form.html
var starteduploading = 0;
var upload_Path; // path for 'uploads' as defined in http.conf - this variable will be automatically updated from http.conf

onUploadForm_beforeUploading = function(){
	if (!parent.sessionData.PLATFORM.isAA50 && !parent.sessionData.PLATFORM.isABE && ASTGUI.version.lt("1.6.0")) {
		alert("File Uploads are supported in Asterisk 1.6.0 and higher.");
		return;
	}
	starteduploading = 1;
	parent.ASTGUI.dialog.waitWhile('File Upload in progress, please wait ..');
	return true;
};

onUploadForm_load = function(){
	if( parent.sessionData.PLATFORM.isABE ){ // ABE-1600
		$('#uploadForm_container').hide();
		return;
	}
	if(!top.sessionData.httpConf.postmappings_defined || !top.sessionData.httpConf.uploadPaths['backups'] ){
		top.log.error('AG102');
		$('#uploadForm_container').hide();
		return ;
	}

	upload_Path = top.sessionData.httpConf.uploadPaths['backups'] ;
	var upload_action_path = (top.sessionData.httpConf.prefix) ? '/' + top.sessionData.httpConf.prefix + '/backups' : '/backups' ;
	_$('uploadiframe').contentWindow.document.getElementById('form22').action =  upload_action_path ;
	_$('uploadiframe').contentWindow.document.getElementById('UploadFORM_UPLOAD_BUTTON').value =  'Upload Backup to Unit' ;
};


onUploadForm_unload = function(){
	if(!starteduploading){ return; }
	parent.ASTGUI.dialog.waitWhile('Backup File Uploaded');
	ASTGUI.feedback({ msg:'Backup File Uploaded !!', showfor: 3 });
	$('#uploadForm_container').hide();
	setTimeout( function(){ window.location.href = 'backup.html'; } , 1000 );
	return;
};

function localajaxinit() {
	top.document.title = 'Configuration Backups';
	if( parent.sessionData.PLATFORM.isAA50 && !parent.sessionData.hasCompactFlash ){
		$('#nocf').show();
		$('#thispageContent').hide();
		return true;
	}

	var rand_2 = Math.round(100000*Math.random());
	var tmp_check_perms_guibkps = function(){
		ASTGUI.dialog.waitWhile('Checking write privileges on backups folder');
		ASTGUI.systemCmd( "touch "+ top.sessionData.directories.ConfigBkp + rand_2 , function(){

			ASTGUI.listSystemFiles( top.sessionData.directories.ConfigBkp , function(a){
				a = a.join('');
				if( a.contains(rand_2) ){
					ASTGUI.systemCmd( "rm '"+ top.sessionData.directories.ConfigBkp + rand_2 + "'" , function(){});
				}else{
					ASTGUI.dialog.alertmsg( 'missing ' + top.sessionData.directories.ConfigBkp + '<BR> OR Asterisk does not have write privileges on ' + top.sessionData.directories.ConfigBkp );
					return;
				}

				localajaxinit_2();
			});
		});
	};

	tmp_check_perms_guibkps();

}

var localajaxinit_2 = function(){
	parent.ASTGUI.dialog.waitWhile(' Loading list of Previous Backup files !');
	parent.ASTGUI.systemCmd( "mkdir -p " + bkpPath , function(){
		parent.ASTGUI.listSystemFiles( bkpPath , function(bkpfiles) {
			try{
				ASTGUI.domActions.clear_table(_$('bkpfilesTable'))
				for( var i =0 ; i < bkpfiles.length ; i++){
					addrow_totable( bkpfiles[i].stripTags(), i );
				}
				var _bft = _$('bkpfilesTable') ;
				if( _bft.rows.length == 0 ){
					_$('table_one').style.display="none";
					var newRow = _bft.insertRow(-1);
					var newCell0 = newRow.insertCell(0);
					newCell0 .align = "center";
					newCell0 .innerHTML = "<BR><I> No Previous Backup configurations found !!</I> <BR><BR>" +
						"Please click on the 'Create New Backup' button<BR> to  take a backup of the current system configuration<BR><BR>" ;
				}
			}finally{
				parent.ASTGUI.dialog.hide();
			}
		});
	});
}

function restore_uploadedbkpfile(){

}

function addrow_totable(filename, i ){
	// filename is of format "backup_2008nov19_164527__2008nov19.tar"
	if(!filename.contains('__')){ return true;}
	var fname = filename.split("__") ; // var fname[1] = 2007mar12.tar

	var tmp_a = fname[1] ; // tmp_a = 2008nov19.tar
	if(tmp_a.contains('_sounds.')){
		tmp_a = tmp_a.withOut('_sounds');
		fname[0] = fname[0] + '<BR><b>Voicemails & Prompts</b>';
	}

	var filedate = tmp_a.rChop('.tar');
	var day = filedate.substr(7);
	var month = filedate.substr(4,3);
	var year = filedate.substr(0,4);
	
	var newRow = _$('bkpfilesTable').insertRow(-1);
	newRow.style.backgroundColor='#FFFFFF';
	newRow.onmouseover= function(){ this.style.backgroundColor='#F9F0D1'; };
	newRow.onmouseout=function(){ this.style.backgroundColor='#FFFFFF'; };

	ASTGUI.domActions.tr_addCell( newRow , { html: _$('bkpfilesTable').rows.length , width:35, align:'center' } );
	ASTGUI.domActions.tr_addCell( newRow , { html: fname[0] , width:180 } );
	ASTGUI.domActions.tr_addCell( newRow , { html: month.capitalizeFirstChar()  + " " + day + ", " + year , width : 125 } );

	var tmp = "<span onclick='dld_bkp(\""+ filename + "\")'class=\"guiButton\">Download from Unit</span>&nbsp;"  +
			"<span onclick='restore_bkp(\""+ filename + "\")' class=\"guiButton\">Restore Previous Config</span>&nbsp;"  +
			"<span onclick='delete_bkp(\""+ filename + "\")'  class=\"guiButtonDelete\">Delete</span>" ;
	ASTGUI.domActions.tr_addCell( newRow , { html: tmp , align:'center'} );
}


function dld_bkp( filename ){
	parent.ASTGUI.systemCmd( "mkdir -p "+ top.sessionData.directories.ConfigBkp_dldPath + " ; /bin/rm " + top.sessionData.directories.ConfigBkp_dldPath + "* ", function(){
		parent.ASTGUI.systemCmd( "/bin/ln -s "+ bkpPath + filename + " " + top.sessionData.directories.ConfigBkp_dldPath + filename , function(){
			var location_dir = window.location.href ;
			location_dir = location_dir.rChop('backup.html') ;
			var download_link =  location_dir + "private/bkps/" + filename ;
			var save_text = ( jQuery.browser.msie ) ? "'Save Target As...'" : "'Save Link As..'" ;

			ASTGUI.feedback( { msg:'Download using the <i>Download File</i> link', showfor:2 });
			_$('backup_download_Link_url').innerHTML = '<center><A href="' + download_link + '"><b>Download File</b></A>'
								+ "<BR>Right Click on the above link and download using the " + save_text + " option</center>" ;
			$('#backup_download_Link').show();
			//var dld_window = window.open ("", "mywindow","toolbar=0,scrollbars=0,location=0,statusbar=0,menubar=0,resizable=0,width=500,height=200");
			//dld_window.document.write('<BR><center><A href="' + download_link + '"><b>Download From Unit</b></A></center>');

		});
	});
}


function restore_bkp(filename){
	if( parent.sessionData.PLATFORM.isAA50 ){
		if(confirm('This will restart the appliance after restoring the backup configuration. \n\n Are you sure you want to proceed ?')){
			restore_bkp_step3(bkpPath + filename);
		}else{
			return;
		}
	}else{
		if(confirm('All your old configuration will be replaced by this backup configuration. \n\n Are you sure you want to proceed ?')){
			//parent.astmanEngine.run_tool("rm /etc/asterisk/* -rf ", callback=function(){  } );
			restore_bkp_step3(bkpPath + filename);
		}
	}
}

function restore_bkp_step3(file_fullpath){
	if( parent.sessionData.PLATFORM.isAA50 ){
		parent.ASTGUI.dialog.waitWhile(' The System will reboot shortly ');
		parent.ASTGUI.systemCmd( top.sessionData.directories.script_restoreBackup + " " + file_fullpath, function(){
			ASTGUI.feedback( { msg:'Configuration restored !!', showfor:2 });
			parent.miscFunctions.AFTER_REBOOT_CMD();
			/* ***************** Todo Restart ******************* */
			//if(starteduploading){ delete_bkp2(file_fullpath); }
		});
	}else{
		parent.ASTGUI.dialog.waitWhile(' Restoring Configuration ');
		parent.ASTGUI.systemCmd( " tar -xvf " + file_fullpath + ' -C / ', function(){
			ASTGUI.feedback( { msg:'Configuration restored !!', showfor:2 });

			if(starteduploading){
				delete_bkp2(file_fullpath);
			}else{
				var t = ASTGUI.cliCommand('reload') ;
				setTimeout( function(){ top.window.location.reload(); } , 1000 );
			}
		});
	}
}

function delete_bkp( filename ){
	if(!confirm("Delete selected Backup Configuration ?")){ return ; }
	delete_bkp2( bkpPath + filename );
}

function delete_bkp2( file_fullpath ){
	parent.ASTGUI.systemCmd( "/bin/rm -f " + file_fullpath , function(){
		ASTGUI.feedback( { msg:'Delete Request Successfull !', showfor:2 });
		setTimeout( function(){ window.location.reload(); } , 1000 );
	});
}


function take_bkp(){
	ASTGUI.showbg(true);

	var months = ["jan", "feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"];
	var today=new Date()
	var year = today.getFullYear();
	var month = months[ today.getMonth() ];
	var day = today.getDate().addZero();
	var hour = today.getHours().addZero() ;
	var minute = today.getMinutes().addZero() ;
	var seconds = today.getSeconds().addZero() ;
	var bkpfile =  "backup_" + year + month + day + "_" + hour + minute + seconds ;

	ASTGUI.updateFieldToValue( 'newbkp_name', bkpfile );
	
	if( parent.sessionData.PLATFORM.isAA50 ){
	 	$(".AA50only").show();
	}else{
		$(".AA50only").hide();
	}
	
	$('#newbkp_content').show() ;
	_$('newbkp_name').focus();
}

function cancel_newbackup(){
	ASTGUI.showbg(false);
	_$('newbkp_content').style.display="none" ;
}

function backup_new(){
	var _nn = _$('newbkp_name');
	if ( !ASTGUI.checkRequiredFields( [_nn] ) ) return ;
	if ( !ASTGUI.validateFields( [_nn] ) ) return ;

	var months = ["jan", "feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"];
	var today=new Date()
	var year = today.getFullYear();
	var month = months[ today.getMonth() ];
	var day = today.getDate().addZero();
	//var hour =addzero(today.getHours());
	//var minute =addzero(today.getMinutes());
	//var seconds =addzero(today.getSeconds());
	var bkpfile =  _nn.value +"__" + year + month + day +".tar";

	if( parent.sessionData.PLATFORM.isAA50  ){
		var fullback = _$('newbkp_completeBackup').checked ;
		if(fullback){
			var tmp_script = top.sessionData.directories.script_takeBackup + ' ' +  bkpPath + bkpfile + ' ' + 'YES';
		}else{
			var tmp_script = top.sessionData.directories.script_takeBackup + ' ' +  bkpPath + bkpfile ;
		}
		parent.ASTGUI.systemCmd( tmp_script , function(){
			ASTGUI.feedback( { msg:'Backup Successful', showfor:2 });
			window.location.reload();
		}) ;
		return ;
	}else{
		parent.ASTGUI.systemCmd( "tar -chf " + bkpPath + bkpfile + ' ' +  ' /etc/asterisk', function(){
			ASTGUI.feedback( { msg:'Backup Successful', showfor:2 });
			parent.ASTGUI.dialog.waitWhile('Reloading List of backup Files');
			setTimeout( function(){ parent.ASTGUI.dialog.hide(); window.location.reload(); } , 2000 );
		});
		return ;
	}
}
