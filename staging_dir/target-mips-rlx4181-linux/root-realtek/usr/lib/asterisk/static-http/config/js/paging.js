/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * paging.html functions
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
/*

	[pagegroups]									// ASTGUI.contexts.PageGroups
	exten => 6051,1,Macro(pagingintercom,SIP/6000&SIP/6005&SIP/6007,qd)


*/


var isNewPgGrp;
var PgGrp_EDITING; // page group being edited
var listOfDialDevices = [];
var loadDOMelements = function(){
	DOM_pagegroup_editdiv = _$('pagegroup_editdiv');
	DOM_table_pageGroups_list = _$('table_pageGroups_list');



	DOM_text_pageGroup_Exten = _$('text_pgExten');
	DOM_select_pageGroup_Type = _$('select_pg_type');
	DOM_chk_pageGroup_beep = _$('chk_pgrp_beep');

	DOM_select_ringthesechannels = _$('select_ringthesechannels');
	DOM_select_fromlistofchannels = _$('select_fromlistofchannels');
		DOM_button_addall_toringlist = _$('button_addall_toringlist');
		DOM_button_add_toringlist = _$('button_add_toringlist');
		DOM_button_remove_fromringlist = _$('button_remove_fromringlist');
		DOM_button_removeall_fromringlist = _$('button_removeall_fromringlist');


};

var save_pageGroup = function(){
	if ( !ASTGUI.checkRequiredFields([DOM_text_pageGroup_Exten, DOM_select_pageGroup_Type]) ){
		return ;
	}
	if ( !ASTGUI.validateFields([DOM_text_pageGroup_Exten]) ){
		return ;
	}

	var TEMP_members = ASTGUI.selectbox.readAllValues(DOM_select_ringthesechannels) ;
	if( !TEMP_members.length ){
		ASTGUI.highlightField(DOM_select_ringthesechannels, 'You need to have at least one member in the Page/Intercom group !!');
		parent.ASTGUI.dialog.hide();
		return;
	}

	var PAGE_EXTEN = ASTGUI.getFieldValue( DOM_text_pageGroup_Exten );

	if( isNewPgGrp && parent.miscFunctions.ifExtensionAlreadyExists(PAGE_EXTEN) ){
		ASTGUI.highlightField(DOM_text_pageGroup_Exten , 'Extension already exists');
		parent.ASTGUI.dialog.hide();
		return;
	}
	// if( !ASTGUI.miscFunctions.isExtensionInRange( PAGE_EXTEN , 'rge_start' , 'rge_end' ) ){
	//	ASTGUI.highlightField(DOM_text_pageGroup_Exten, 'Extension is not in preferred range');
	//	parent.ASTGUI.dialog.hide();
	//	return;
	// }

	var tmp_after = function(){
		var tmp_options = [];
		if ( ASTGUI.getFieldValue(DOM_select_pageGroup_Type) == '2way' ){ tmp_options.push('d'); }
		if ( !DOM_chk_pageGroup_beep.checked ){ tmp_options.push('q'); }
	
		var tmp_new_line = PAGE_EXTEN + ',1,Macro(pagingintercom,' + TEMP_members.join('&') + ','+ tmp_options.join('') + ')' ;
	
		var later = function(){
			var msg = ( isNewPgGrp ) ? 'New Page/Intercom Group Created' : 'Page/Intercom Group Updated';
			ASTGUI.feedback({msg: msg, showfor:2});
			parent.ASTGUI.dialog.hide();
			window.location.reload();
		};
		parent.pbx.paging.add( tmp_new_line, later );
	};

	if( !isNewPgGrp ){ // if editing existing ring group
		parent.ASTGUI.dialog.waitWhile(' Saving... ');
		parent.pbx.paging.remove( PgGrp_EDITING, tmp_after );
	}else{
		parent.ASTGUI.dialog.waitWhile(' Saving... ');
		tmp_after();
	}
};


var GetDevice_UserName = function( device ){
	// device = SIP/6002 or Zap/1, or DAHDI/1 or IAX2/6001
	var tmp_ext = device.afterChar('/');
	var tmp_techn = device.beforeChar('/');

	if( tmp_techn.toLowerCase() == 'zap' || tmp_techn.toLowerCase() == 'dahdi' ){
		for ( var q in parent.sessionData.pbxinfo.users ){ if( parent.sessionData.pbxinfo.users.hasOwnProperty(q) ){
			if( parent.sessionData.pbxinfo.users[q].hasOwnProperty('zapchan') && parent.sessionData.pbxinfo.users[q]['zapchan'] == tmp_ext ){
				return q + '(AnalogPort ' + tmp_ext + ') ' + parent.sessionData.pbxinfo.users[q].getProperty('fullname');
			}
			if( parent.sessionData.pbxinfo.users[q].hasOwnProperty('dahdichan') && parent.sessionData.pbxinfo.users[q]['dahdichan'] == tmp_ext ){
				return q + '(AnalogPort ' + tmp_ext + ') ' + parent.sessionData.pbxinfo.users[q].getProperty('fullname');
			}
		}}
		return 'AnalogPort ' + tmp_ext ;
	}else{
		if( parent.sessionData.pbxinfo.users.hasOwnProperty(tmp_ext) ){
			var tmp_name = parent.sessionData.pbxinfo.users[tmp_ext].getProperty('fullname');
			return tmp_ext + '('+ tmp_techn +')' + ' ' + tmp_name
		}
		return tmp_ext + '(' + tmp_techn + ')' ;
	}
};


var resetFields = function(){
	if(isNewPgGrp){
		_$('pagegroup_editdiv_title').innerHTML = 'New Page/Intercom Group';

		ASTGUI.resetTheseFields([ DOM_text_pageGroup_Exten , DOM_select_pageGroup_Type , DOM_chk_pageGroup_beep ]);
		ASTGUI.selectbox.clear( DOM_select_ringthesechannels );
		ASTGUI.selectbox.clear( DOM_select_fromlistofchannels );
		listOfDialDevices.each( function(device){
			ASTGUI.selectbox.append( DOM_select_fromlistofchannels, GetDevice_UserName(device) , device);
		});

		var tmp_allextensions = ASTGUI.cloneObject( parent.miscFunctions.getAllExtensions() );
		DOM_text_pageGroup_Exten.value  = tmp_allextensions.firstAvailable( parent.sessionData.GUI_PREFERENCES.getProperty('rge_start') );
		return ;
	}

	var c = parent.sessionData.pbxinfo['pagegroups'] ;
	_$('pagegroup_editdiv_title').innerHTML = 'Edit Page/Intercom Group - ' + PgGrp_EDITING ;

	var line = '';
	try{
		line = c[  c.indexOfLike('exten='+PgGrp_EDITING+',1,Macro(pagingintercom')  ] ;
	}catch(err){}

	if(!line) return;
	var tmp_args = ASTGUI.parseContextLine.getArgs( line );
	var tmp_members = tmp_args[1].split('&');

	ASTGUI.updateFieldToValue( DOM_text_pageGroup_Exten, PgGrp_EDITING );
	ASTGUI.updateFieldToValue( DOM_select_pageGroup_Type, tmp_args[2].contains('d') ? '2way' : '1way' );
	DOM_chk_pageGroup_beep.checked = !tmp_args[2].contains('q');

	ASTGUI.selectbox.clear( DOM_select_ringthesechannels );
	var mbrs = ASTGUI.cloneObject(tmp_members) ; 
	mbrs.each(function(device){
		ASTGUI.selectbox.append( DOM_select_ringthesechannels, GetDevice_UserName(device) , device );
	});


	ASTGUI.selectbox.clear( DOM_select_fromlistofchannels );
	listOfDialDevices.each(function(device){ if( ! mbrs.contains(device) ){
		ASTGUI.selectbox.append( DOM_select_fromlistofchannels, GetDevice_UserName(device) , device );
	}});
};


var delete_pageGroup_confirm = function(d){
	if (!confirm('Delete Page/Intercom Group ?')) { return; }
	parent.ASTGUI.dialog.waitWhile(' Saving... ');
	parent.pbx.paging.remove(d, function(){
		ASTGUI.feedback({ msg:'Page/Intercom Group deleted', showfor:2 });
		parent.ASTGUI.dialog.hide();
		window.location.reload();
	});
};

var show_NewPageGroup_form = function(){
	PgGrp_EDITING = '';
	isNewPgGrp = true;
	resetFields();
	ASTGUI.feedback({ msg: 'Create New Page/Intercom Extension !', showfor: 2 , color: 'green', bgcolor: '#FFFFFF' });
	$(DOM_pagegroup_editdiv).showWithBg();
};


var edit_pageGroup_form = function(d){
	PgGrp_EDITING = d;
	isNewPgGrp = false;
	resetFields();
	ASTGUI.feedback( { msg: 'Edit Page/Intercom Extension !', showfor: 2 , color: 'green', bgcolor: '#FFFFFF' } );
	$(DOM_pagegroup_editdiv).showWithBg();
};


var update_PageGroupsTable = function(){
	var addCell = ASTGUI.domActions.tr_addCell; // temporarily store the function
	(function(){ // add first row
		var newRow = DOM_table_pageGroups_list.insertRow(-1);
		newRow.className = "frow";
		addCell( newRow , { html:'', width:'10px'} );
		addCell( newRow , { html: 'Extension' });
		addCell( newRow , { html: 'Type' });
		addCell( newRow , { html:'Members'});
		addCell( newRow , { html:''} );
	})();

	var c = parent.sessionData.pbxinfo['pagegroups'] ;
	c.each( function( line ){
		var this_exten = ASTGUI.parseContextLine.getExten(line);
		var tmp_args = ASTGUI.parseContextLine.getArgs( line );
		var tmp_members = tmp_args[1].split('&').join(', ');


		var newRow = DOM_table_pageGroups_list.insertRow(-1);
		var rn = DOM_table_pageGroups_list.rows.length;
		newRow.className = ((rn)%2==1)?'odd':'even';

		addCell( newRow , { html:'' } );
		addCell( newRow , { html: this_exten });
		addCell( newRow , { html: tmp_args[2].contains('d') ? '2-Way Intercom' : '1-Way Paging' });
		addCell( newRow , { html: tmp_members });
		var tmp = "<span class='guiButton' onclick=\"edit_pageGroup_form('" + this_exten +"')\">Edit</span>" +
				"<span class='guiButtonDelete' onclick=\"delete_pageGroup_confirm('" + this_exten +"')\">Delete</span>" ;
		addCell( newRow , { html: tmp } );
	});

	if(DOM_table_pageGroups_list.rows.length == 1){
		ASTGUI.domActions.clear_table(DOM_table_pageGroups_list);
		var newRow = DOM_table_pageGroups_list.insertRow(-1);
		newRow.className = 'even';
		addCell( newRow , { html:'No Page/Intercom Extensions defined !!'} );
		return ;
	}
};


var localajaxinit = function(){
	top.document.title = 'Paging & Intercom' ;
	loadDOMelements();

	if( !ASTGUI.miscFunctions.alertIfRangeisNotdefined('rge_start','rge_end', 'RingGroups') ){
		$('.top_buttons').hide();
		return;
	}

	(function(){
		var t = [{	url:'#',
				desc:'Group Paging/Intercom',
				click_function: function(){
					$('.hideall').hide();
					$('#TAB_group_Paging').show();

				}
			},{	url: '#',
				desc: 'Page an Extension',
				click_function: function(){
					$('.hideall').hide();
					$('#TAB_page_anExtension').show();

				}
			},{	url: '#',
				desc: 'Settings',
				click_function: function(){
					$('.hideall').hide();
					$('#TAB_paging_settings').show();

				}
			}];

		ASTGUI.tabbedOptions( _$('tabbedMenu') , t );
	})();
	try{
		(function (){
			var t = parent.pbx.users.list();
			t.each(function(usr){	
				if( parent.sessionData.pbxinfo['users'][usr]['hassip']  && parent.sessionData.pbxinfo['users'][usr]['hassip'] == 'yes' ){
					listOfDialDevices.push( 'SIP/' + usr );
				}
				if( parent.sessionData.pbxinfo['users'][usr]['hasiax']  && parent.sessionData.pbxinfo['users'][usr]['hasiax'] == 'yes' ){
					listOfDialDevices.push( 'IAX2/' + usr );
				}
			});
			t = parent.sessionData.FXS_PORTS_DETECTED ;
			t.each( function(fxs){
				listOfDialDevices.push( parent.sessionData.DahdiDeviceString + '/' + fxs );
			} );
	
			ASTGUI.events.add( DOM_button_add_toringlist , 'click' , function(){
				var t = DOM_select_fromlistofchannels.value ; if(!t){return;}
				var s = DOM_select_fromlistofchannels.options[DOM_select_fromlistofchannels.selectedIndex].text ; if(!s){ s = t; }
				DOM_select_fromlistofchannels.remove( DOM_select_fromlistofchannels.selectedIndex );
				ASTGUI.selectbox.append( DOM_select_ringthesechannels, s , t );
			});
			ASTGUI.events.add( DOM_button_remove_fromringlist , 'click' , function(){
				var t = DOM_select_ringthesechannels.value ; if(!t){return;}
				var s = DOM_select_ringthesechannels.options[DOM_select_ringthesechannels.selectedIndex].text ; if(!s){ s = t; }
				DOM_select_ringthesechannels.remove( DOM_select_ringthesechannels.selectedIndex );
				if( listOfDialDevices.contains(t) ){ ASTGUI.selectbox.append( DOM_select_fromlistofchannels, s, t ); }
			});
			ASTGUI.events.add( DOM_button_removeall_fromringlist , 'click' , function(){
				ASTGUI.selectbox.clear( DOM_select_ringthesechannels );
				ASTGUI.selectbox.clear( DOM_select_fromlistofchannels );
				listOfDialDevices.each(function(device){
					ASTGUI.selectbox.append( DOM_select_fromlistofchannels, GetDevice_UserName(device) , device );
				});
			});
	
			ASTGUI.events.add( 'button_addall_toringlist' , 'click' , function(){
				ASTGUI.selectbox.clear( DOM_select_ringthesechannels );
				ASTGUI.selectbox.clear( DOM_select_fromlistofchannels );
				listOfDialDevices.each( function(device){
					ASTGUI.selectbox.append( DOM_select_ringthesechannels, GetDevice_UserName(device) , device );
				});
			});
		})();
		update_PageGroupsTable();
	}catch(err){

	}finally{
		$('#tabbedMenu').find('A:eq(0)').click();

		var c = context2json ({ filename: 'extensions.conf' , context: 'globals', usf: 1 });
		ASTGUI.updateFieldToValue( 'text_Alert_Info_Header' , c.getProperty('PAGING_HEADER') );

		var d = context2json ({ filename: 'extensions.conf' , context: ASTGUI.contexts.PageAnExtension , usf: 0 });
		var i = d.length;
		while (i--) {
			if( d[i].endsWith(',q)') ){
				ASTGUI.updateFieldToValue( 'text_prefix_paging' , ASTGUI.parseContextLine.getExten(d[i]).lChop('_').withOut('X') );
			}
			if( d[i].endsWith(',qd)') ){
				ASTGUI.updateFieldToValue( 'text_prefix_intercom' , ASTGUI.parseContextLine.getExten(d[i]).lChop('_').withOut('X') );
			}
		}
	}
};

var load_Defaults_TAB_page_anExtension = function(){
	ASTGUI.resetTheseFields( ['text_prefix_paging','text_prefix_intercom']);
	$('#btn_ld_tpe').hide();
};

var save_TAB_paging_settings = function(){
	ASTGUI.updateaValue({ file:'extensions.conf', context :'globals', variable :'PAGING_HEADER', value : ASTGUI.getFieldValue('text_Alert_Info_Header') });
	ASTGUI.feedback({msg:' Saved !!', showfor: 3 , color: '#5D7CBA', bgcolor: '#FFFFFF'}) ;
};


var save_TAB_page_anExtension = function(){
	// exten => _**XXXX,1,Macro(pagingintercom,Local/${EXTEN:2},q)		// text_prefix_paging
	// exten => _*#XXXX,1,Macro(pagingintercom,Local/${EXTEN:2},qd)		// text_prefix_intercom
	// parent.sessionData.GUI_PREFERENCES.ue_start

	var TMP_CONTEXT = ASTGUI.contexts.PageAnExtension ;

	var tmp_X = 'X'.times( parent.sessionData.GUI_PREFERENCES.ue_start.length );
	var tmp_text_prefix_paging = ASTGUI.getFieldValue('text_prefix_paging');
	var tmp_text_prefix_intercom = ASTGUI.getFieldValue('text_prefix_intercom');

	var u = new listOfSynActions('extensions.conf');

	if( tmp_text_prefix_paging ){
		var pg_exten = '_' + tmp_text_prefix_paging + tmp_X + ',1,Macro(pagingintercom,Local/${EXTEN:' + tmp_text_prefix_paging.length + '},q)' ;
		u.new_action('append', TMP_CONTEXT , 'exten', pg_exten );
	}

	if( tmp_text_prefix_intercom ){
		var itcom_exten = '_' + tmp_text_prefix_intercom + tmp_X + ',1,Macro(pagingintercom,Local/${EXTEN:' + tmp_text_prefix_intercom.length + '},qd)' ;
		u.new_action('append', TMP_CONTEXT , 'exten', itcom_exten );
	}


	ASTGUI.miscFunctions.empty_context({ filename:'extensions.conf', context : TMP_CONTEXT , cb : function(){
		u.callActions();
		ASTGUI.feedback({msg:' Saved !!', showfor: 3 , color: '#5D7CBA', bgcolor: '#FFFFFF'}) ;
	}});
};
