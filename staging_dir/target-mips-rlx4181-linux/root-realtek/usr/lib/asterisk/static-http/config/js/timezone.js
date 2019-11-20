/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * timezone.html functions
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
var upload_Path; // path for 'timezone' as defined in http.conf - (usually /usr/share/zoneinfo) but the value will be automatically loaded from http.conf
var default_tz = "";
var DOM_bft ;

onUploadForm_beforeUploading = function(){
	starteduploading = 1;
	ASTGUI.dialog.waitWhile('File Upload in progress, please wait ..');
	return true;
};

onUploadForm_load = function(){
	if(!top.sessionData.httpConf.postmappings_defined || !top.sessionData.httpConf.uploadPaths['timezone'] ){
		top.log.error('AG102');
		$('#tdupload').hide(); //
		return ;
	}
	upload_Path = top.sessionData.httpConf.uploadPaths['timezone'] + '/' ;
	var upload_action_path = (top.sessionData.httpConf.prefix) ? '/' + top.sessionData.httpConf.prefix + '/timezone' : '/timezone' ;
	_$('uploadiframe').contentWindow.document.getElementById('form22').action =  upload_action_path ;
}; 

onUploadForm_unload = function(){
	if(!starteduploading){ return; }
	ASTGUI.dialog.hide();
	window.location.reload();
	return;
};

function aaaa(){
	addFROW_totable()
	ASTGUI.dialog.waitWhile('Loading ..');
	ASTGUI.listSystemFiles( upload_Path , function(tzfiles) {
		for( var i =0 ; i < tzfiles.length ; i++){
			addrow_totable( tzfiles[i].stripTags());
		}
		if( DOM_bft.rows.length == 1 ){
			var newRow = DOM_bft.insertRow(-1);
			ASTGUI.domActions.tr_addCell( newRow , { html: "<BR><I> No Previous TimeZone files found !!</I> <BR>", align: "center" }) ;
		}
		ASTGUI.dialog.hide();
	});

};

function addFROW_totable(){
	var newRow = DOM_bft.insertRow(-1);
	newRow.className = 'frow';
	ASTGUI.domActions.tr_addCell( newRow , { html: '' , align: "center", width: '35' });
	ASTGUI.domActions.tr_addCell( newRow , { html: 'Name' });
	ASTGUI.domActions.tr_addCell( newRow , { html: 'Options' , align: "center", width: "200px" });
};


function addrow_totable(filename){
	var newRow = DOM_bft.insertRow(-1);
	newRow.className = ((DOM_bft.rows.length)%2==1)?'odd':'even';
// 	if(default_tz!=filename){
// 		newRow.onmouseover= function(){ this.style.backgroundColor='#F9F0D1'; };
// 		newRow.onmouseout=function(){ this.style.backgroundColor='#FFFFFF'; };
// 	}
	ASTGUI.domActions.tr_addCell( newRow , { html: DOM_bft.rows.length , align: "center", width: '35' }) ;
	ASTGUI.domActions.tr_addCell( newRow , { html: filename, align: "left" }) ;
	/*
	var k_tmp = (default_tz==filename)?"Default TimeZone":"<input type=\"button\" onclick='setaslocal(\""+ filename + "\")'  value=\"Set as Default\" class=\"splbutton\">&nbsp;"  +
	"<input type=\"button\" onclick='delete_tz(\""+ filename + "\")'  value=\"Delete\"  class=\"splbutton\">" ;
	*/
	var k_tmp = (default_tz==filename)?"Default TimeZone":"<input type=\"button\" onclick='setaslocal(\""+ filename + "\")'  value=\"Set as Default\" class=\"splbutton\">" ;
	ASTGUI.domActions.tr_addCell( newRow , { html: k_tmp , align: "center", width: "200px" }) ;
};


function delete_tz( filename ){
	if(!confirm("Delete selected TimeZone ?")){ return ; }
	ASTGUI.systemCmd( "/bin/rm -f "+ upload_Path + filename  , function(){
		ASTGUI.feedback( { msg:'Delete Request Successfull !', showfor:2 });
		window.location.reload();
	});
};

function setaslocal( filename ){
	if(!confirm("Set '"+ filename +"' as the Default TimeZone ?")){ return ; }
	ASTGUI.systemCmd( "rm /etc/localtime ; /bin/ln -s " + upload_Path + filename + " /etc/localtime" , function(){
		ASTGUI.feedback( { msg:'Default Timezone set to '+ filename + ' !', showfor:2 });
		top.cookies.set( 'configFilesChanged' , 'yes' );
		parent.$('#applyChanges_Button').show();
		alert("you have to click 'Apply Changes' and restart the appliance for this change to take effect");
		window.location.reload();
	});
};


var localajaxinit = function(){
	top.document.title = 'Choose local TimeZone' ;
	DOM_bft = _$('tzfilesTable') ;
		var t = [
			{url:'networking.html', desc:'General' },
			{url:'networking.html?tab=wan', desc:'WAN' },
			{url:'networking.html?tab=lan', desc:'LAN' },
			{url:'#', desc:'TimeZone', selected:true }
		];
		ASTGUI.tabbedOptions( _$('tabbedMenu') , t );

	ASTGUI.systemCmdWithOutput( 'ls -l /etc/localtime ' , function(op){
		if(op.length == 0){
			ASTGUI.feedback( { msg:'You do not have a default TimeZone', showfor:2 });
		}else{
			var p = op.split("->");
			if(p.length > 1){
				p = p[1].trim().split(upload_Path);
				default_tz = p[1];
			}
		}
		_$('default_tz').innerHTML = (default_tz.length > 1) ? default_tz : "None";
		aaaa();
	});
}




function call_update_tz( ){
	ASTGUI.dialog.waitWhile('Updating Timezones..');
	
	ASTGUI.systemCmd( "update_tz" , function(){
		setTimeout( function(){
			ASTGUI.dialog.hide();
			ASTGUI.feedback({ msg:'Please reboot your appliance !', showfor:2 });
			window.location.reload();
		}, 20000 );
	});
};
