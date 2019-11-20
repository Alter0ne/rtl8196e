/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * mohfiles.html functions
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
var CURRENTMOH_CLASSNAME = '';
var CURRENTMOH_PATH = '';
var CURRENTMOH_PATH_PM_NAME = ''; // postmappings name given to current path in http.conf

var upload_Filename = ""; // will be updated by upload_form.html
var starteduploading = 0;
var IFRAME_CREATED = false;


var onUploadForm_load = function(){
	var tmp_a = (parent.sessionData.httpConf.prefix) ? parent.sessionData.httpConf.prefix : '' ;
	var tmp = (tmp_a) ? '/' + tmp_a + '/' + CURRENTMOH_PATH_PM_NAME : '/' + CURRENTMOH_PATH_PM_NAME ;
	_$('uploadiframe').contentWindow.document.getElementById('form22').action =  tmp ;
};

var onUploadForm_beforeUploading = function(){
	if (!parent.sessionData.PLATFORM.isAA50 && !parent.sessionData.PLATFORM.isABE  && ASTGUI.version.lt("1.6.0")) {
		alert("File Uploads are supported in Asterisk 1.6.0/trunk");
		return;
	}
	var tmp_fname = upload_Filename.toLowerCase();
	if( tmp_fname.endsWith('.wav') || tmp_fname.endsWith('.gsm') || tmp_fname.endsWith('.ulaw') || tmp_fname.endsWith('.alaw') ){
		starteduploading = 1;
		parent.ASTGUI.dialog.waitWhile('File Upload in progress, please wait ..');
		return true;
	}else{
		starteduploading = 0;
		ASTGUI.feedback({ msg:'Sound file must be wav, gsm, ulaw or alaw !!', showfor: 3, color: 'red' });
		alert('Sound file must be wav, gsm, ulaw or alaw !');
		return false;
	}
};

var onUploadForm_unload = function(){
	if(!starteduploading){ return; }
	parent.ASTGUI.dialog.waitWhile('Sound File Uploaded');
	ASTGUI.feedback({ msg:'Sound File Uploaded !!', showfor: 3 });
	$('#uploadForm_container').hide();
	setTimeout( function(){ 
		parent.ASTGUI.dialog.hide();
		window.location.reload();
	} , 1000 );
	return;
};


var localajaxinit = function() {
	top.document.title = 'Manage Music-On-Hold Files';
	manage_moh.load_mohClasses();
	if( parent.sessionData.PLATFORM.isAA50 && !parent.sessionData.hasCompactFlash ){
		$('#nocf').show();
		$('#thispageContent').hide();
		_$('label_selectedClass').style.display = 'none';
		_$('moh_classes').style.display = 'none';
		_$('mohclass_addButton').style.display = 'none';
		_$('mohclass_deleteButton').style.display = 'none';
		return true;
	}
	if( parent.sessionData.PLATFORM.isAA50 ){
		$('#uploadForm_container legend')[0].innerHTML = '<B>&nbsp;Upload an 8 KHz Mono Music file (less than 10 Mb):&nbsp;</B>';
	}
	ASTGUI.events.add( 'moh_classes' , 'change' , manage_moh.select_MohClass );
};


var manage_moh = {
	load_mohClasses : function(){ // manage_moh.load_mohClasses();
		var mcls = config2json({filename: 'musiconhold.conf', usf: 1});
		for (var this_class in mcls ){
			if(mcls.hasOwnProperty(this_class)){
				ASTGUI.selectbox.append('moh_classes', this_class, mcls[this_class].directory );
			}
		}
		_$('moh_classes').selectedIndex = -1;
	},

	delete_file: function( filename ){
		if(!confirm("Delete selected music file ?")){ return ; }
		parent.ASTGUI.systemCmd( '/bin/rm -f ' + CURRENTMOH_PATH + '/' + filename , function(){
			ASTGUI.feedback({ msg:'Delete Request Successfull !', showfor:2 });
			setTimeout( function(){ 
				window.location.reload();
			} , 1000 );
		});
	},

	update_listofFilesTable : function(){ // manage_moh.update_listofFilesTable();
		var addrow_totable = function (filename){
			var newRow = _$('soundFilesTable').insertRow(-1);
			newRow.className = ((_$('soundFilesTable').rows.length)%2==1)?'odd':'even';
			ASTGUI.domActions.tr_addCell( newRow , { html: filename , align:'left' } );
//			ASTGUI.domActions.tr_addCell( newRow , { html: 'x Kb' , align:'center'} );
			var tmp = "<span onclick='manage_moh.delete_file(\""+ filename + "\")'class=\"guiButtonDelete\">Delete</span>&nbsp;" ;
			ASTGUI.domActions.tr_addCell( newRow , { html: tmp , align:'center'} );
		};
		$('#thispageContent').hide();
		parent.ASTGUI.dialog.waitWhile(' Loading list of files !');
		parent.ASTGUI.listSystemFiles( CURRENTMOH_PATH + '/' , function(sndfiles) {
			try{
				ASTGUI.domActions.clear_table(_$('soundFilesTable'));
				var newRow = _$('soundFilesTable').insertRow(-1);
				newRow.className = 'frow';
				ASTGUI.domActions.tr_addCell( newRow , { html: 'Sound File', align:'left' } );
//				ASTGUI.domActions.tr_addCell( newRow , { html: 'Size' , align:'center'});
				ASTGUI.domActions.tr_addCell( newRow , { html: 'Options' , align:'center'});
	
				for( var i = 0 ; i < sndfiles.length ; i++){
					if( !sndfiles[i].beginsWith( ASTGUI.contexts.mohdirPrefix ) && (sndfiles[i].endsWith('.mp3') || sndfiles[i].endsWith('.wav') || sndfiles[i].endsWith('.gsm')) ){
						addrow_totable( sndfiles[i].stripTags(), i );
					}
				}
				var _sft = _$('soundFilesTable') ;
				if( _sft.rows.length == 1 ){
					ASTGUI.domActions.clear_table(_$('soundFilesTable'));
					var newRow = _sft.insertRow(-1);
					var newCell0 = newRow.insertCell(0);
					newCell0 .align = "center";
					newCell0 .innerHTML = "<BR><I> No files found in this class !!</I> <BR>Use the above upload form to add music files to this class.<BR>" ;
				}
			}finally{
				if(!IFRAME_CREATED){
					var b = document.createElement('IFRAME');
					b.id = 'uploadiframe'; b.width = '520' ; b.height = '100'; b.frameborder='0'; b.border='0'; b.marginheight='0'; b.marginwidth='0' ;
					_$('uploadForm_container').appendChild(b);
					_$('uploadiframe').src = 'upload_form.html';
					IFRAME_CREATED = true;
				}
				$('#uploadForm_container').show();
				if (top.sessionData.PLATFORM.isABE) {
					$('#uploadForm_container').hide();
				}
				$('#thispageContent').show();
				parent.ASTGUI.dialog.hide();
			}
		});
	},

	delete_MohClass : function(){ // manage_moh.delete_MohClass();
		var tmp_moh_classname = _$('moh_classes').options[_$('moh_classes').selectedIndex].text;
		var tmp_MOH_PATH = _$('moh_classes').value;
			if(!tmp_moh_classname){
				ASTGUI.feedback( { msg:'Please select a moh class to delete', showfor:2 });
				return;
			}
			if(tmp_moh_classname == 'default'){
				ASTGUI.feedback( { msg:'default class can not be deleted !! ', showfor:2 });
				return;
			}
			if(!confirm("Delete MOH class " + tmp_moh_classname + " ?")){ return ; }
		var tmp_MOH_PATH_PM_NAME = manage_moh.get_PostMappingsName( tmp_MOH_PATH );

		parent.ASTGUI.dialog.waitWhile(' deleting moh class ...');
		// delete entry from musiconhold.conf
		var u = new listOfSynActions('musiconhold.conf') ;
		u.new_action('delcat', tmp_moh_classname, '', '');
		u.callActions();

		// delete entry from http.conf 
		u.clearActions('http.conf') ;
		u.new_action('delete', 'post_mappings', tmp_MOH_PATH_PM_NAME, '');
		u.callActions();

		// delete dir on disk
		ASTGUI.systemCmd( 'rm -rf ' + tmp_MOH_PATH  , function(){ 
			ASTGUI.cliCommand('reload'); // reload asterisk
			parent.ASTGUI.dialog.hide(); 
			setTimeout( function(){ window.location.reload();} , 1000 );// reload page
		});
	},

	add_MohClass: function(){ // manage_moh.add_MohClass()
		var tmp_moh_classname = _$('New_mohClass_name').value.trim() ;
			if ( !ASTGUI.checkRequiredFields(['New_mohClass_name']) ){
				return ;
			}
			if (!ASTGUI.validateFields(['New_mohClass_name'])){
				return;
			};

		var tmp_MOH_PATH_PM_NAME = ASTGUI.contexts.mohdirPrefix + tmp_moh_classname ;
		var tmp_MOH_PATH = top.sessionData.directories.MOH + tmp_MOH_PATH_PM_NAME ;

		parent.ASTGUI.dialog.waitWhile(' creating moh class ...');
		// create entry from musiconhold.conf
		var u = new listOfSynActions('musiconhold.conf') ;
		u.new_action('newcat', tmp_moh_classname, '', '');
		u.new_action('append', tmp_moh_classname , 'directory', tmp_MOH_PATH );
		u.new_action('append', tmp_moh_classname , 'mode', 'files');
		u.new_action('append', tmp_moh_classname , 'random', 'yes');
		u.callActions();

		// create entry from http.conf 
		u.clearActions('http.conf') ;
		u.new_action('update', 'post_mappings', tmp_MOH_PATH_PM_NAME, tmp_MOH_PATH );
		u.callActions();

		// create dir on disk
		ASTGUI.systemCmd( 'mkdir -p ' + tmp_MOH_PATH  , function(){ 
			ASTGUI.cliCommand('reload'); // reload asterisk
			parent.ASTGUI.dialog.hide();
			setTimeout( function(){ window.location.reload();} , 1000 );// reload page
		});

	},

	show_AddMohClass_form: function(){
		ASTGUI.domActions.alignBontopofA('mohclass_addButton' , 'div_newMOH_Class');
		$('#div_newMOH_Class').show();
		_$('New_mohClass_name').focus();
	},

	select_MohClass : function(){ // manage_moh.select_MohClass();
		$('#thispageContent').hide();
		$('#mohclass_deleteButton').show();
		CURRENTMOH_CLASSNAME = _$('moh_classes').options[_$('moh_classes').selectedIndex].text;
		CURRENTMOH_PATH = _$('moh_classes').value;
		CURRENTMOH_PATH_PM_NAME = manage_moh.get_PostMappingsName( CURRENTMOH_PATH );
		_$('label_selectedClass').innerHTML = "manage MOH class - <B>'" + CURRENTMOH_CLASSNAME + "'</B>";
		manage_moh.update_listofFilesTable();
	},

	get_PostMappingsName : function( some_path ){
		// lookup this path in http.conf and return the name
		var t = context2json({ filename:'http.conf' , context : 'post_mappings' , usf:1 });
		for ( var this_path_label in t ){
			if ( t.hasOwnProperty(this_path_label) ){
				if ( t[this_path_label] == some_path )
					return this_path_label;
			}
		}
		// TODO : if path is not found add new one and reload asterisk
		return '';
	}
};
