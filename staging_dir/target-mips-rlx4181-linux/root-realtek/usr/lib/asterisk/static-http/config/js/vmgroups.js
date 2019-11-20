/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * vmgroups.html functions
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
var isNEW_VMG ;
var EDIT_VMG ;
var vmg_chkbxClass = "VMG_Chkboxes";

var localajaxinit = function(){
	top.document.title = 'VoiceMail Groups';
	var addCell = ASTGUI.domActions.tr_addCell;

	(function(){ // load list of all voicemail boxes available on the system as checkboxes in 'edit_includeCheckboxes_div'
		var g = parent.pbx.users.list();
		var f = [];
		g.each( function(this_user){
			if( parent.sessionData.pbxinfo['users'][this_user].getProperty('hasvoicemail').isAstTrue() ){
				f.push(this_user);
			}
		} );

		if( f.length ){
			ASTGUI.domActions.populateCheckBoxes( 'edit_includeCheckboxes_div' , f, vmg_chkbxClass );
		}else{
			var tmp_span = document.createElement('span');
			tmp_span.innerHTML = 'Can not find any voicemail boxes !<BR> <A href=#>click here</A> to manage Voicemail accounts of users.';
			ASTGUI.events.add( tmp_span , 'click' , function(){
				$('#Edit_VMGROUP_DIV').hideWithBg();
				parent.miscFunctions.click_panel('users.html');
			} );

			$('#edit_includeCheckboxes_div').css( 'border', '1px solid #CDCDCD' );
			$('#edit_includeCheckboxes_div').css( 'text-align', 'center' );	
			_$('edit_includeCheckboxes_div').appendChild( tmp_span );
		}
	})();

	(function(){ // add first row
		var t = _$('table_VMGroups_list');
		var newRow = t.insertRow(-1);
		newRow.className = "frow";
		addCell( newRow , { html:'', width:'15px'} );
		addCell( newRow , { html:'Extension for VoiceMail Group'} );
		addCell( newRow , { html:'Label'} );
		addCell( newRow , { html:'Member MailBoxes'} );
		addCell( newRow , { html:''} );
	})();

	(function(){

		var vmgroups = parent.sessionData.pbxinfo.vmgroups.getOwnProperties();
		var tbl = _$('table_VMGroups_list');
		vmgroups.each(function( this_vmg_exten ){
			var newRow = tbl.insertRow(-1);
			newRow.className = ((tbl.rows.length)%2==1)?'odd':'even';
			addCell( newRow , { html:'', width:'15px'} );
			addCell( newRow , { html: this_vmg_exten} );
			addCell( newRow , { html: parent.sessionData.pbxinfo.vmgroups[this_vmg_exten].label } );
			addCell( newRow , { html: parent.sessionData.pbxinfo.vmgroups[this_vmg_exten].mailboxes.join(', ') } );
			addCell( newRow , { html: "<span class='guiButton' onclick='vmgroups_miscFunctions.edit_VMG_form("
						+ this_vmg_exten + ")'>Edit</span>" 
						+ "<span class='guiButtonDelete' onclick='vmgroups_miscFunctions.delete_VMG("
						+ this_vmg_exten + ")'>Delete</span>" 
					}
				);

		});


		if(tbl.rows.length == 1){
			ASTGUI.domActions.clear_table(tbl);
			var newRow = tbl.insertRow(-1);
			newRow.className = 'even';
			addCell( newRow , { html:'No VoiceMail Groups defined !!'} );
			return ;
		}
	})();
	
};

var vmgroups_miscFunctions = {

	reset_allFields : function(){ // vmgroups_miscFunctions.reset_allFields();
		if(isNEW_VMG){
			var tmp_allextensions = ASTGUI.cloneObject( parent.miscFunctions.getAllExtensions() );
			var NEW_EXT = tmp_allextensions.firstAvailable( parent.sessionData.GUI_PREFERENCES.getProperty('vmg_start') );
			_$('edit_vmgroup_exten').value = NEW_EXT ;
			_$('edit_vmgroup_label').value = '' ;
			_$('edit_vmgroup_exten').disabled = false;
			ASTGUI.domActions.unCheckAll( vmg_chkbxClass );
			_$('Edit_dialog_title').innerHTML = ' New Voice Mail Group ';
			return;
		}

		var tmp_this_VMG = ASTGUI.cloneObject(parent.sessionData.pbxinfo.vmgroups[EDIT_VMG]);
		_$('edit_vmgroup_exten').value = EDIT_VMG;
		_$('edit_vmgroup_label').value = tmp_this_VMG.label ;
		_$('edit_vmgroup_exten').disabled = true;
		ASTGUI.domActions.unCheckAll( vmg_chkbxClass );
		ASTGUI.domActions.checkSelected( vmg_chkbxClass , tmp_this_VMG.mailboxes );

		_$('Edit_dialog_title').innerHTML = ' Edit Voice Mail Group - ' + EDIT_VMG ;
	},

	show_NewVMG_form : function(){ // vmgroups_miscFunctions.show_NewVMG_form()
		isNEW_VMG = true;
		EDIT_VMG  = '';
		vmgroups_miscFunctions.reset_allFields();
		ASTGUI.feedback( { msg: 'Create new VoiceMail Group !', showfor: 2 , color: 'green', bgcolor: '#FFFFFF' } );
		$('#Edit_VMGROUP_DIV').showWithBg();
	},

	edit_VMG_form : function(vmgexten){ //vmgroups_miscFunctions.edit_VMG_form(exten)
		isNEW_VMG = false;
		EDIT_VMG  = vmgexten;
		vmgroups_miscFunctions.reset_allFields();
		ASTGUI.feedback( { msg: 'Edit VoiceMail Group !', showfor: 2 , color: 'green', bgcolor: '#FFFFFF' } );
		$('#Edit_VMGROUP_DIV').showWithBg();
	},

	save_VMGroup : function(){ // vmgroups_miscFunctions.save_VMGroup(exten)
		if( isNEW_VMG == false ){ // delete the previous vmg
			parent.pbx.vm_groups.remove(EDIT_VMG);
		}

		if ( !ASTGUI.checkRequiredFields([ 'edit_vmgroup_exten' , 'edit_vmgroup_label' ]) ) return ;
		if ( !ASTGUI.validateFields( ['edit_vmgroup_exten', 'edit_vmgroup_label' ] ) ) return ;

		if( isNEW_VMG ){
			var NU_EXT = ASTGUI.getFieldValue('edit_vmgroup_exten');
			if( ! ASTGUI.miscFunctions.isExtensionInRange( NU_EXT ,'vmg_start','vmg_end') ){
				ASTGUI.highlightField('edit_vmgroup_exten', 'Extension is not in preferred range');
				parent.ASTGUI.dialog.hide();
				return;
			}
			if( parent.miscFunctions.ifExtensionAlreadyExists(NU_EXT) ){
				ASTGUI.highlightField('edit_vmgroup_exten', 'Extension already exists');
				parent.ASTGUI.dialog.hide();
				return;
			}
		}

		var vm_exten = ASTGUI.getFieldValue('edit_vmgroup_exten');
		var a = new ASTGUI.customObject ;
			a.label = ASTGUI.getFieldValue('edit_vmgroup_label');
			a.mailboxes = ASTGUI.domActions.get_checked( vmg_chkbxClass );

		parent.pbx.vm_groups.add(vm_exten, a);

		ASTGUI.feedback( { msg: 'Changes Saved !', showfor: 2 , color: 'green', bgcolor: '#FFFFFF' } );
		window.location.reload();
	},

	delete_VMG : function(vmgexten){ // vmgroups_miscFunctions.delete_VMG(exten)
		if( !confirm('Delete VoiceMail Group ' + vmgexten + ' ?') ) return;
		parent.pbx.vm_groups.remove(vmgexten);
		setTimeout( function(){
				ASTGUI.feedback({ msg: 'VoiceMail Group Deleted !', showfor: 3 , color: 'red', bgcolor: '#FFFFFF' });
				window.location.reload();
			},500);
	}
};
