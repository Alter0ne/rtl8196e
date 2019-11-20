/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * trunks_analog.html functions
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
var zapchan_Before = '';
var FXOS = [];
var isNew ;
var EDIT_TRUNK, DAHDICHANNELSTRING ;
var VOLSETTINGS = {};
var Electrical_Fields = ['busydetect', 'busycount', 'busypattern', 'ringtimeout', 'answeronpolarityswitch', 'hanguponpolarityswitch' , 'callprogress', 'progzone', 'usecallerid', 'cidstart', 'pulsedial', 'cidsignalling', 'flash', 'rxflash', 'mailbox'];

var ch_chkbxClass = "FXO_ChkBoxes";

var getSelectedChannels = function(){
	return ASTGUI.domActions.get_checked(ch_chkbxClass) ;
};

var resetChannels = function(){
	ASTGUI.domActions.unCheckAll( ch_chkbxClass );
};

var checkChannels = function(channels){
	ASTGUI.domActions.checkSelected(ch_chkbxClass, channels) ;
};

var populateGroupsSelect = function(){
	parent.pbx.trunks.listAllGroups().each(function(h){
		var tr = parent.pbx.trunks.getTrunkNamesByGroup(h);
		var trstr = tr.join(", ");
		if (trstr.length > 30){
			trstr = trstr.substr(30) + '...';
		}
		ASTGUI.selectbox.append( _$('edit_groups'), parent.pbx.trunks.getGroupDescription(h), h);
	});
};

var disable_usedChannels = function(trunk){ // trunk is (optional, used while editing a trunk, for new trunk leave blank)
	// FXOS -- list of all Analog channels
	var used = [];
	var c = parent.pbx.trunks.list({analog: true});
	c.each(function(item){
		if(trunk && trunk == item ) return;
		used = used.concat( ASTGUI.miscFunctions.chanStringToArray(parent.sessionData.pbxinfo['trunks']['analog'][item][DAHDICHANNELSTRING]) ) ;
	});
	ASTGUI.domActions.unCheckAll( ch_chkbxClass );
	ASTGUI.domActions.disableSelected(ch_chkbxClass, used);
};


var new_analogTrunk_form = function(){
	isNew = true;
	EDIT_TRUNK = '';
	zapchan_Before = '';
	disable_usedChannels();
	_$('new_ATRNK_DIV_title').innerHTML = 'New Analog Trunk';
	_$('new_ATRNK_addUpdateButton').innerHTML = 'Add';
	$('#HIDE_OnNEW_0').hide();
	$('#HIDE_OnNEW_1').hide();
	$('#HIDE_OnNEW_2').hide();
	$(DOM_new_ATRNK_DIV).showWithBg();
	ASTGUI.updateFieldToValue( 'edit_trunkName', '' );
	ASTGUI.resetTheseFields( Electrical_Fields );
	DOM_edit_groups_select.selectedIndex = -1;
};


var selectedTrunk_editOptions_form = function(w){
	EDIT_TRUNK = w;
	if(!FXOS.length ){
		ASTGUI.feedback({ msg:'No FXO ports detected !', showfor:4 });
		return;
	}
	isNew = false;
	_$('new_ATRNK_DIV_title').innerHTML = 'Edit Analog Trunk';
	_$('new_ATRNK_addUpdateButton').innerHTML = 'Update';
	disable_usedChannels(EDIT_TRUNK);
	checkChannels( parent.sessionData.pbxinfo['trunks']['analog'][EDIT_TRUNK][DAHDICHANNELSTRING] );
	zapchan_Before = parent.sessionData.pbxinfo['trunks']['analog'][EDIT_TRUNK][DAHDICHANNELSTRING];
	var ct = ASTGUI.contexts.TrunkDIDPrefix + EDIT_TRUNK ;
	ASTGUI.updateFieldToValue('edit_trunkName', getProperty(parent.sessionData.pbxinfo['trunks']['analog'][EDIT_TRUNK], 'trunkname'));
	getPreviousVolumeForChannelsofThisTrunk( EDIT_TRUNK );
	Electrical_Fields.each( function(fld){
		var fld_value = parent.sessionData.pbxinfo['trunks']['analog'][EDIT_TRUNK][fld] || '' ;
		ASTGUI.updateFieldToValue( _$(fld) ,  fld_value );
	} );

	DOM_edit_groups_select.selectedIndex = -1;
	var g = parent.sessionData.pbxinfo['trunks']['analog'][EDIT_TRUNK]['group'].toString().split(',');
	g.each(function(group){ ASTGUI.updateFieldToValue(DOM_edit_groups_select, group); });

	ASTGUI.updateFieldToValue(_$('dummy_customCid'), '');
	ASTGUI.updateFieldToValue( _$('dummy_callerid'), 'asreceived');
	if (parent.sessionData.pbxinfo['trunks']['analog'][EDIT_TRUNK]['callerid'] && parent.sessionData.pbxinfo['trunks']['analog'][EDIT_TRUNK]['callerid'] != 'asreceived') {
		ASTGUI.updateFieldToValue(_$('dummy_customCid'), parent.sessionData.pbxinfo.trunks.analog[EDIT_TRUNK].callerid);
		ASTGUI.updateFieldToValue( _$('dummy_callerid'), 'custom');
	}

	var variablename =  ASTGUI.globals.obcidUsrPrefix + EDIT_TRUNK ;
	var c = context2json({ filename:'extensions.conf', context: 'globals' , usf: 1 });
	ASTGUI.updateFieldToValue( 'trunk_obcid', getProperty(c, variablename) );

	$('#HIDE_OnNEW_0').show();
	if( parent.sessionData.PLATFORM.isAA50 ){
		$('#HIDE_OnNEW_1').show();
	}else{
		$('#HIDE_OnNEW_1').hide();
	}
	$('#HIDE_OnNEW_2').show();

	$(DOM_new_ATRNK_DIV).showWithBg();
};


var loadDOMelements = function(){
	DOM_table_ATRUNKS_list = _$('table_ATRUNKS_list');
	DOM_new_agtrnk_button = _$('new_agtrnk_button');
	DOM_new_ATRNK_DIV = _$('new_ATRNK_DIV');
	DOM_new_ATRNK_cls_container = _$('new_ATRNK_cls_container');
	DOM_trunk_OptionsList = _$('trunk_OptionsList');
	DOM_div_electrical = _$('div_electrical');
	DOM_div_audioLevels = _$('div_audioLevels');
	VOLUMES_TBL = _$('TABLE_PORTS_VOLUME');
	DOM_edit_groups_select = _$('edit_groups');

	FXOS.each(function(item){
		var lbl = document.createElement( 'label' ) ;
		var lbltext = document.createTextNode( item ) ;
		var ncbx = document.createElement('input') ;
			ncbx.type = 'checkbox' ;
			ncbx.value = item ;
			ncbx.className = ch_chkbxClass ;
		lbl.appendChild( ncbx ) ;
		lbl.appendChild( lbltext ) ;
		DOM_new_ATRNK_cls_container.appendChild( lbl ) ;
	});
};


var new_ATRNK_save_go = function(){
	var scs = getSelectedChannels().join(',');
	if( !scs ){
		ASTGUI.feedback({msg:'At least one Analog channel must be selected', showfor: 3 , color: '#8b442e'}) ;
		return;
	}

	if( zapchan_Before != scs ){
		top.cookies.set( 'require_restart' , 'yes' );
	}

	var groupstr = ASTGUI.getFieldValue(DOM_edit_groups_select) ? ASTGUI.getFieldValue(DOM_edit_groups_select).join(',') : 'New';
	groupstr = parent.pbx.trunks.formatGroupString(groupstr);
	
	if( ASTGUI.getFieldValue('edit_trunkName') ){
		var trunk_name = ASTGUI.getFieldValue('edit_trunkName');
	}else{
		var trunk_name = ( getSelectedChannels().length ) ? 'Ports ' + scs : 'Port ' + scs ;
	}

	if(isNew ){
		var cbf = function(){
			ASTGUI.feedback({msg:'Created New Analog trunk ', showfor: 3 , color: '#5D7CBA', bgcolor: '#FFFFFF'}) ;
			window.location.reload();
		};

		var tmp_object = {'zapchan':scs , 'trunkname': trunk_name , 'group': groupstr} ;
		Electrical_Fields.each(function(fld){
			tmp_object[fld] = ASTGUI.getFieldValue( _$(fld) );
		});

		if ($('#dummy_callerid').val() == 'asreceived') {
			tmp_object['callerid'] = 'asreceived';
		} else {
			tmp_object['callerid'] = $('#dummy_customCid').val();
		}

		parent.pbx.trunks.add('analog', tmp_object , cbf ) ;
		return;
	}else{
		var new_groups = groupstr.split(",");

		var ded_group = parent.pbx.trunks.getDedicatedGroup(parent.pbx.trunks.getName(EDIT_TRUNK));
		if(!new_groups.contains(ded_group)){
			/* See if there is another dedicated group */
			var save_old_groups = parent.sessionData.pbxinfo.trunks['analog'][EDIT_TRUNK]['group'].toString().split(',');
			parent.sessionData.pbxinfo.trunks['analog'][EDIT_TRUNK]['group'] = new_groups.join(',');
			if(!parent.pbx.trunks.getDedicatedGroup(parent.pbx.trunks.getName(EDIT_TRUNK))){
				top.log.debug("Can't remove a trunk from its dedicated group.");
				new_groups.push(ded_group);
				parent.sessionData.pbxinfo.trunks['analog'][EDIT_TRUNK]['group'] = new_groups.join(',');
			}
		}
	}

	/* If the user just put a trunk into another trunk's dedicated group, we will allow it
	   but create a new dedicated group for the other trunk. */
	var ana_trunks = parent.pbx.trunks.list({analog: true});
	ana_trunks.each(function(item){
		var ded_group = parent.pbx.trunks.getDedicatedGroup(parent.pbx.trunks.getName(item));
		/* make sure the user isn't trying to remove the trunk from its dedicated group */
		if(!ded_group || new_groups.contains(ded_group)){ /* We've just used another trunk's ded_group */
			/* should never happen. */
			var new_ded_group = parent.pbx.trunks.makeDedicatedGroup();
			var g = parent.sessionData.pbxinfo.trunks['analog'][item]['group'].toString().split(',');
			g.push(new_ded_group);
			parent.sessionData.pbxinfo.trunks['analog'][item]['group'] = g.join(',');
			var x = new listOfSynActions('users.conf');
			x.new_action('update', item, 'group', parent.sessionData.pbxinfo.trunks['analog'][item]['group']);
			x.callActions();
		}
	});
	var final_groups = [];
	for( var i = 0 ; i < new_groups.length; i++){
		if(new_groups[i]){
			final_groups.push(new_groups[i]);
		}
	}
	delete new_groups;
	groupstr = final_groups.join(",");
	parent.sessionData.pbxinfo.trunks['analog'][EDIT_TRUNK]['group'] = groupstr;
	var x = new listOfSynActions('extensions.conf');
	x.new_action('update', 'globals', EDIT_TRUNK, "DAHDI/g" + parent.pbx.trunks.getDedicatedGroup(parent.pbx.trunks.getName(EDIT_TRUNK)));
	x.callActions();

	// just update the selected channels
	(function(){
		var x = new listOfSynActions('users.conf');
			x.new_action('update', EDIT_TRUNK , DAHDICHANNELSTRING, scs );
			x.new_action('update', EDIT_TRUNK , 'group', groupstr);
			x.new_action('delete', EDIT_TRUNK , 'gui_volume', '' );
			x.new_action('delete', EDIT_TRUNK , 'gui_fxooffset', '' );
			x.new_action('delete', EDIT_TRUNK , 'rxgain', '' );
			x.callActions();
			x.clearActions();
			x.new_action('delete', EDIT_TRUNK , 'txgain', '' );
			x.new_action('delete', EDIT_TRUNK , 'signalling', '' );
			x.new_action('delete', EDIT_TRUNK , 'channel', '' );
			x.callActions();
			x.clearActions();

		x.new_action('update', EDIT_TRUNK , 'trunkname', trunk_name.guiMetaData() );
		parent.sessionData.pbxinfo['trunks']['analog'][EDIT_TRUNK]['trunkname'] = trunk_name ;
		parent.sessionData.pbxinfo['trunks']['analog'][EDIT_TRUNK]['group'] = groupstr;

		var zap_channels = ASTGUI.miscFunctions.chanStringToArray(scs);

		for(var p in VOLSETTINGS){ if (  VOLSETTINGS.hasOwnProperty(p) && !zap_channels.contains(p) ) {
			delete VOLSETTINGS[p];
		}}
		zap_channels.each( function(channel){
			var sg = (parent.sessionData.PORTS_SIGNALLING.ls.contains(channel)) ? 'fxs_ls':'fxs_ks' ;
			x.new_action('append', EDIT_TRUNK, 'signalling', sg);
			x.new_action( 'append', EDIT_TRUNK , 'channel', channel );
		} );
			x.callActions();
	})();

	var variablename =  ASTGUI.globals.obcidUsrPrefix + EDIT_TRUNK ;
	ASTGUI.updateaValue({ file: 'extensions.conf', context: 'globals', variable: variablename , value: ASTGUI.getFieldValue('trunk_obcid') }) ;

	parent.sessionData.pbxinfo['trunks']['analog'][EDIT_TRUNK][DAHDICHANNELSTRING] = scs;
	ASTGUI.feedback({msg:'Updated Analog trunk ', showfor: 3 , color: '#5D7CBA', bgcolor: '#FFFFFF'}) ;
	save_audioLevels( save_electrical );
};

var update_AnalogTrunksTable = function(){
	var addCell = ASTGUI.domActions.tr_addCell; // temporarily store the function

	if(!FXOS.length){
		var newRow = DOM_table_ATRUNKS_list.insertRow(-1);
			var ntd = document.createElement('TD');
			ntd.setAttribute('colspan',4); ntd.colSpan = 4; // for IE
			ntd.align = 'center';
			ntd.innerHTML = '<BR><B>No FXO ports detected !!</B><BR><BR>';
		newRow.appendChild(ntd);
		DOM_new_agtrnk_button.style.display = 'none';

		var c = parent.pbx.trunks.list({analog: true});
		if(!c.length){return;}
	}

	(function(){ // add first row
		var newRow = DOM_table_ATRUNKS_list.insertRow(-1);
		newRow.className = "frow";
		addCell( newRow , { html:'Trunk'} );
		addCell( newRow , { html:'Analog Ports'} );
		addCell( newRow , { html:''} );
	})();

	(function (){
		var c = parent.pbx.trunks.list({analog: true});
		c.each( function(item){
			 tmp = "<span class='guiButton' onclick=\"selectedTrunk_editOptions_form('" + item +"')\">Edit</span>" + 
				"<span class='guiButtonDelete' onclick=\"delete_trunk_confirm('" + item +"')\">Delete</span>" ;

			var newRow = DOM_table_ATRUNKS_list.insertRow(-1);
			newRow.className = ((DOM_table_ATRUNKS_list.rows.length)%2==1)?'odd':'even';
			addCell( newRow , { html: parent.sessionData.pbxinfo['trunks']['analog'][item]['trunkname'] });
			addCell( newRow , { html: parent.sessionData.pbxinfo['trunks']['analog'][item][DAHDICHANNELSTRING] } );
			addCell( newRow , { html: tmp} );
		});

		if(!c.length){
			ASTGUI.domActions.clear_table(DOM_table_ATRUNKS_list);
			var newRow = DOM_table_ATRUNKS_list.insertRow(-1);
			addCell( newRow , { html: '<BR> No Analog Trunks Defined.<BR><BR>'} );
		}
	})();
};

var delete_trunk_confirm = function(a){
	EDIT_TRUNK = a;

	var trunk_name = getProperty(parent.sessionData.pbxinfo['trunks']['analog'][EDIT_TRUNK], 'trunkname') || EDIT_TRUNK ;
	if(!confirm("Delete trunk '"+ trunk_name + "' ?  If you intend to reconfigure this trunk later, you will have to update any calling rules or voice menu Dial actions which use this trunk.")) { return true; }
	if( parent.pbx.trunks.remove(EDIT_TRUNK) ){ 
		ASTGUI.feedback({msg:'Deleted Analog trunk ' + "'" + trunk_name + "'" , showfor: 3 , color: '#5D7CBA', bgcolor: '#FFFFFF'}) ;
		window.location.reload();
	};
};

var localajaxinit = function(){
	DAHDICHANNELSTRING = parent.sessionData.DahdiChannelString;
	if( parent.sessionData.PLATFORM.AA50_SKU.contains('800') ){
		window.location.href= 'trunks_sps.html';
		return;
	};

	FXOS = parent.sessionData.FXO_PORTS_DETECTED ;
	top.document.title = 'Manage Analog Trunks' ;
	(function (){
		var t = [];
			t.push({url:'trunks_analog.html', desc:'Analog Trunks', selected:true });
		if( parent.sessionData.PLATFORM.isAA50 || parent.sessionData.PLATFORM.isABE ){
			t.push({url:'trunks_sps.html', desc:'Service Providers'});
		}
			t.push({url:'trunks_voip.html', desc:'VOIP Trunks'});
		if( !parent.sessionData.PLATFORM.isAA50 ){
			t.push({url:'trunks_digital.html', desc:'T1/E1/BRI Trunks'});
		}
		ASTGUI.tabbedOptions( _$('tabbedMenu') , t);

		var y = parent.pbx.users.list();
		y.each( function(user){
			if( getProperty(parent.sessionData.pbxinfo.users[user], 'hasvoicemail').isAstTrue() ){
				ASTGUI.selectbox.append('mailbox', user, user);
			}
		});
	})();
	populateGroupsSelect();
	loadDOMelements();
	update_AnalogTrunksTable();
};


var save_electrical = function(){
	var x = new listOfActions('users.conf');
	Electrical_Fields.each(function(fld){
		x.new_action('update', EDIT_TRUNK , fld , ASTGUI.getFieldValue( _$(fld) ) ) ;
	});

	if ($('#dummy_callerid').val() == 'asreceived') {
		parent.sessionData.pbxinfo['trunks']['analog'][EDIT_TRUNK]['callerid'] = 'asreceived';
		x.new_action('update', EDIT_TRUNK, 'callerid', 'asreceived');
	} else {
		parent.sessionData.pbxinfo['trunks']['analog'][EDIT_TRUNK]['callerid'] = $('#dummy_customCid').val();
		x.new_action('update', EDIT_TRUNK, 'callerid', $('#dummy_customCid').val());
	}

	var after = function(){
		parent.ASTGUI.dialog.hide();
		Electrical_Fields.each(function(fld){
			parent.sessionData.pbxinfo['trunks']['analog'][EDIT_TRUNK][fld] = ASTGUI.getFieldValue( _$(fld) ) ;
		});

		ASTGUI.feedback({ msg:'Changes Saved !', showfor:2 });
		window.location.reload();
	};

	parent.ASTGUI.dialog.waitWhile(' Saving Advanced Options ...');
	setTimeout( function(){ x.callActions(after) ; } , 200 );
};


var getPreviousVolumeForChannelsofThisTrunk = function( trunk ){
	var c = context2json({ filename:'users.conf' , context : trunk, usf:0 });
	var channels = ASTGUI.miscFunctions.chanStringToArray( parent.sessionData.pbxinfo.trunks.analog[trunk][DAHDICHANNELSTRING] ) ;
	if ( !channels.length ) return ;

	VOLSETTINGS = {} ;
	ASTGUI.domActions.clear_table (VOLUMES_TBL);
		var tmp_sel= document.createElement('select');
		ASTGUI.selectbox.append(tmp_sel, 'Low'     , '1') ;
		ASTGUI.selectbox.append(tmp_sel, 'Soft'    , '2') ;
		ASTGUI.selectbox.append(tmp_sel, 'Normal'  , '3') ;
		ASTGUI.selectbox.append(tmp_sel, 'Loud'    , '4') ;
		ASTGUI.selectbox.append(tmp_sel, 'Louder'  , '5') ;
		ASTGUI.selectbox.append(tmp_sel, 'Loudest' , '6') ;

	channels.each(function(channel){
		channel = channel.trim();
		if( !channel ){return;}
		var newRow = VOLUMES_TBL.insertRow(-1);
		ASTGUI.domActions.tr_addCell( newRow , { html:'Port ' + channel } );
		var newcell = newRow.insertCell( newRow.cells.length );
			var sel = tmp_sel.cloneNode(true);
			sel.id='port_' + channel+ '_guivolume' ;
		newcell.appendChild(sel);
		VOLSETTINGS[channel] = {};
		var t = c.indexOf( 'channel=' + channel );
		if( t == -1 ){
			ASTGUI.selectbox.selectOption (sel, '2') ;
			VOLSETTINGS[channel].gui_volume = 2 ;
			VOLSETTINGS[channel].linestodelete = [];
			return;
		}
		var s = c.slice(0, t).lastIndexOfLike('channel=') ;
		s = (s == -1) ? 0 : s ;
		var thisChannelSettings_ConfigArray = c.slice(s,t);
		var gvi = thisChannelSettings_ConfigArray.indexOfLike('gui_volume') ;
		VOLSETTINGS[channel].gui_volume = (gvi == -1) ? 2 : thisChannelSettings_ConfigArray[gvi].afterChar('=') ;
		ASTGUI.selectbox.selectOption ( sel, VOLSETTINGS[channel].gui_volume ) ;
	});
};


var save_audioLevels = function( CB_FN ){
	var get_guiVolValue = { '1':'-2', '2':'0', '3':'2', '4':'5', '5':'9', '6':'12' };
	var fxotune = config2json({filename:'fxotune.conf', usf:1}) ;

	var x = new listOfActions('users.conf');
		x.new_action('delete', EDIT_TRUNK , 'gui_volume', '' );
		x.new_action('delete', EDIT_TRUNK , 'gui_fxooffset', '' );
		x.new_action('delete', EDIT_TRUNK , 'rxgain', '' );
		x.new_action('delete', EDIT_TRUNK , 'txgain', '' );
		x.new_action('delete', EDIT_TRUNK , 'signalling', '' );
		x.new_action('delete', EDIT_TRUNK , 'channel', '' );
	for(var p in VOLSETTINGS){ if ( VOLSETTINGS.hasOwnProperty(p) ) {
		var gv = ASTGUI.getFieldValue( _$( 'port_' + p + '_guivolume' ) );
		x.new_action('append', EDIT_TRUNK , 'gui_volume', gv.guiMetaData() );
		if( fxotune[p] && fxotune[p]['fxorxgain'] ){
			var fx = -1 * Number(fxotune[p]['fxorxgain']) ;
		}else{
			var fx = 0;
		}
		var sg = (parent.sessionData.PORTS_SIGNALLING.ls.contains(p)) ? 'fxs_ls':'fxs_ks' ;
		x.new_action('append', EDIT_TRUNK , 'signalling', sg);
		x.new_action('append', EDIT_TRUNK , 'gui_fxooffset', fx.guiMetaData() );
		x.new_action('append', EDIT_TRUNK , 'rxgain', fx + Number(get_guiVolValue[gv]) );
		x.new_action('append', EDIT_TRUNK , 'txgain', '0.0' );
		x.new_action('append', EDIT_TRUNK , 'channel', p );
	}}

	var after = function(){
		parent.ASTGUI.dialog.hide();
		ASTGUI.feedback({ msg:'Changes Saved !', showfor:2 });
		if( CB_FN ){ CB_FN(); }
	};
	parent.ASTGUI.dialog.waitWhile(' Saving Audio Levels ...');
	setTimeout( function(){ x.callActions(after) ; } , 200 );
};


var reset_calibration = function(){
	var zc = parent.sessionData.pbxinfo['trunks']['analog'][EDIT_TRUNK][DAHDICHANNELSTRING].split(',') ;
	var x = new listOfActions('users.conf');
		x.new_action('delete', EDIT_TRUNK , 'gui_volume', '' );
		x.new_action('delete', EDIT_TRUNK , 'gui_fxooffset', '' );
		x.new_action('delete', EDIT_TRUNK , 'rxgain', '' );
		x.new_action('delete', EDIT_TRUNK , 'txgain', '' );
		x.new_action('delete', EDIT_TRUNK , 'channel', '' );

	var after = function(){
		var f = new listOfSynActions('fxotune.conf');
		zc.each(function(channel){
			f.new_action('delcat', channel , '', '' );
		});
		f.callActions();
		parent.ASTGUI.dialog.hide();
		//ASTGUI.feedback({ msg:'done !', showfor:2 });
		$("#div_calibrate").hideWithBg();
	};
	parent.ASTGUI.dialog.waitWhile(' Resetting calibration ...');
	x.callActions(after) ;
};


var calibrate_ports = function(){
	var reqFailed = true;
	var checkOutPut = function(){
		var op = ASTGUI.loadHTML('./fxotune_out').toLowerCase() ;
		var msg1 = 'starting fxo tuning' ;
		var msg2 =  'fxo tuning is complete';
		if( op.contains(msg1) ){
			parent.ASTGUI.dialog.waitWhile('Performing Calibration .... ');
			setTimeout( checkOutPut , 5000 );
			return;
		}
		if( op.contains(msg2) ){
			$("#div_calibrate").hideWithBg();
			// TODO updateUsers_fromFxotune();
			ASTGUI.dialog.alertmsg('Finished Calibrating !! <BR> Click "Apply Changes" and restart your appliance ');
		}
	};
	var zc = parent.sessionData.pbxinfo['trunks']['analog'][EDIT_TRUNK][DAHDICHANNELSTRING] ;
	var k = zc.split(',').join(' ') ;
	parent.ASTGUI.dialog.waitWhile('Starting Calibration script ..');
	var st = setTimeout(
			function(){
				if(reqFailed){
					parent.ASTGUI.dialog.hide();
				}
			}, 480000 // if there was no response in 8 minutes - hide the dialog so that the user can tryagain or navigate to a different page
		);

	parent.ASTGUI.systemCmd( '/bin/fxotune_from_gui ' + k , function(){
		clearTimeout( st );
		reqFailed = false;
		setTimeout( checkOutPut , 4000 );
	});
};
