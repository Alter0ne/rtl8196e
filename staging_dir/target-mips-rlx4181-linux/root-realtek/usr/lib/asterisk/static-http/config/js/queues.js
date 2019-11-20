/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * queues.html functions
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
var isNewQueue ;
var EDIT_Queue ;
var ag_chkbxClass = "agents_Chkboxes";
var QUEUES_CONF ;
var loginsettings = { agentLogin_line: '' , agentCallbackLogin_line: ''};

var QueueExistsbyThisName = function( thisName ){
	var m = parent.sessionData.pbxinfo.queues ;
	for( l in m ){ if( m.hasOwnProperty(l) && QUEUES_CONF.hasOwnProperty(l) ){
		if ( isNewQueue == true && QUEUES_CONF[l].getProperty('fullname') == thisName ){
			return true;
		}
		if ( isNewQueue == false && l != EDIT_Queue && QUEUES_CONF[l].getProperty('fullname') == thisName ){
			return true;
		}
	}}
	return false;
};


var loadDOMElements = function(){
	DOM_table_ql = _$('table_queueslist');
	DOM_edit_agents_chboxes = _$('edit_agents_chboxes');

	DOM_edit_QueueDiv = _$('edit_QueueDiv');
	DOM_edit_QueueDiv_title = _$('edit_QueueDiv_title');
	DOM_Queue_Ext = _$('edit_Ext'); // text box
	DOM_edit_label = _$('edit_label'); // text box
	DOM_edit_strategy = _$('edit_strategy'); // select
	DOM_edit_musicclass = _$('edit_musicclass'); // select
	DOM_edit_timeout = _$('edit_timeout'); // text
	DOM_edit_wrapuptime = _$('edit_wrapuptime'); // text
	DOM_edit_maxlen = _$('edit_maxlen'); // text
	DOM_edit_autofill = _$('edit_autofill'); // chkbox
	DOM_edit_autopause = _$('edit_autopause'); // chkbox
	DOM_edit_joinempty = _$('edit_joinempty'); // chkbox
	DOM_edit_leavewhenempty = _$('edit_leavewhenempty'); // chkbox
	DOM_edit_reportholdtime = _$('edit_reportholdtime'); // chkbox
	DOM_edit_voicemenuclass = _$('edit_voicemenuclass'); // select

};

var updateQueuesTable = function(){
	var addCell = ASTGUI.domActions.tr_addCell; // temporarily store the function
	(function(){ // add first row
		var newRow = DOM_table_ql.insertRow(-1);
		newRow.className = "frow";
		addCell( newRow , { html:'', width:'15px'} );
		addCell( newRow , { html:'Queue'} );
		addCell( newRow , { html:'Label'} );
		addCell( newRow , { html:'Strategy'} );
		addCell( newRow , { html:''} );
	})();
	(function(){
		var m = parent.sessionData.pbxinfo.queues;
		for( l in m ){ if( m.hasOwnProperty(l) ){
			if( m[l]['configLine'].contains(',1,agentlogin()') ){
				loginsettings.agentLogin_line = m[l]['configLine'];
				_$('login_exten').value = ASTGUI.parseContextLine.getExten(m[l]['configLine']) ;
				continue;
			}
			if( m[l]['configLine'].contains(',1,Goto(queue-member-manager,handle_member,1)') ){
				loginsettings.agentCallbackLogin_line = m[l]['configLine'];
				_$('login_callback_exten').value = ASTGUI.parseContextLine.getExten(m[l]['configLine']);
				continue;
			}

			var newRow = DOM_table_ql.insertRow(-1);
			newRow.className = ((DOM_table_ql.rows.length)%2==1)?'odd':'even';
			var tmp = "<span class='guiButton' onclick=\"editQueue_form('" + l +"')\">Edit</span>&nbsp;"
					+ "<span class='guiButtonDelete' onclick=\"delete_Queue_confirm('" + l +"')\">Delete</span>" ;
			addCell( newRow , { html:''} );
			addCell( newRow , { html: l } );
			addCell( newRow , { html: QUEUES_CONF[l].getProperty('fullname') }); // Label
			addCell( newRow , { html: QUEUES_CONF[l].getProperty('strategy') }); // Strategy
			addCell( newRow , { html: tmp } ); // Edit Options
		}}
	})();
	(function(){
		if( DOM_table_ql.rows.length == 1 ){
			ASTGUI.domActions.clear_table(DOM_table_ql);
			var newRow = DOM_table_ql.insertRow(-1);
			newRow.className = 'even';
			addCell( newRow , { html:'No Call Queues defined !!'} );
			return ;
		}
	})();

};

var delete_Queue_confirm = function(q){
	if(!confirm("Delete Queue "+ q + " ?")) { return true; }
	var u = new listOfSynActions('extensions.conf');
	if( parent.sessionData.pbxinfo.queues[q].hasOwnProperty('isOLDGUI') && parent.sessionData.pbxinfo.queues[q].isOLDGUI == true ){
		u.new_action( 'delete', 'default', 'exten', '' , parent.sessionData.pbxinfo.queues[q]['configLine'] );
	}
	u.new_action( 'delete', ASTGUI.contexts.QUEUES, 'exten', '' , parent.sessionData.pbxinfo.queues[q]['configLine'] );
	u.new_action( 'update', 'globals', 'QUEUES', parent.sessionData.pbxinfo.queues_list.split(',').withOut(q).join(','))
	parent.sessionData.pbxinfo.queues_list = parent.sessionData.pbxinfo.queues_list.split(',').withOut(q).join(',');
	u.callActions();
	var w = new listOfSynActions('queues.conf') ;
	w.new_action('delcat', q, '', '');
	w.callActions();
	delete parent.sessionData.pbxinfo.queues[q] ;
	ASTGUI.feedback({ msg:'Queue deleted!', showfor:2 , color:'red', bgcolor:'#FFFFFF' });
	window.location.reload();
};


var newQueue_form = function(){
	isNewQueue = true ;
	EDIT_Queue = '' ;
	show_Queue_Form();
};


var editQueue_form = function(k){
	isNewQueue = false ;
	EDIT_Queue = k ;
	show_Queue_Form();
};


var show_Queue_Form = function(){
	if(isNewQueue == true){

		ASTGUI.resetTheseFields([ DOM_Queue_Ext, DOM_edit_label ,DOM_edit_strategy , DOM_edit_musicclass , DOM_edit_timeout , DOM_edit_wrapuptime , DOM_edit_maxlen , DOM_edit_autofill , DOM_edit_autopause , DOM_edit_joinempty , DOM_edit_leavewhenempty , DOM_edit_reportholdtime, DOM_edit_voicemenuclass  ]); /* reset all fields */
		ASTGUI.domActions.unCheckAll( ag_chkbxClass );
		DOM_Queue_Ext.disabled = false;
		var tmp_allextensions = ASTGUI.cloneObject( parent.miscFunctions.getAllExtensions() );
		DOM_Queue_Ext.value  = tmp_allextensions.firstAvailable( parent.sessionData.GUI_PREFERENCES.getProperty('qe_start') );
		DOM_edit_QueueDiv_title.innerHTML = 'New Queue';
		$(DOM_edit_QueueDiv).showWithBg();
		ASTGUI.feedback({ msg:'Create New Queue!', showfor:2 });
		ASTGUI.updateFieldToValue(DOM_edit_musicclass, 'default' );
		ASTGUI.updateFieldToValue(DOM_edit_voicemenuclass, '' );
		return;
	}

	DOM_Queue_Ext.value = EDIT_Queue ; DOM_Queue_Ext.disabled = true ;
	DOM_edit_QueueDiv_title.innerHTML = 'Edit Queue ' + EDIT_Queue ;
	// load values for all other fields

	ASTGUI.updateFieldToValue(DOM_edit_label, QUEUES_CONF[EDIT_Queue].getProperty('fullname') );
	ASTGUI.updateFieldToValue(DOM_edit_strategy, QUEUES_CONF[EDIT_Queue].getProperty('strategy') );
	ASTGUI.updateFieldToValue(DOM_edit_musicclass, QUEUES_CONF[EDIT_Queue].getProperty('musicclass') );
	ASTGUI.updateFieldToValue(DOM_edit_timeout, QUEUES_CONF[EDIT_Queue].getProperty('timeout') );
	ASTGUI.updateFieldToValue(DOM_edit_wrapuptime, QUEUES_CONF[EDIT_Queue].getProperty('wrapuptime') );
	ASTGUI.updateFieldToValue(DOM_edit_maxlen, QUEUES_CONF[EDIT_Queue].getProperty('maxlen') );
	ASTGUI.updateFieldToValue(DOM_edit_autofill, QUEUES_CONF[EDIT_Queue].getProperty('autofill') );
	ASTGUI.updateFieldToValue(DOM_edit_autopause, QUEUES_CONF[EDIT_Queue].getProperty('autopause') );
	ASTGUI.updateFieldToValue(DOM_edit_joinempty, QUEUES_CONF[EDIT_Queue].getProperty('joinempty') );
	ASTGUI.updateFieldToValue(DOM_edit_leavewhenempty, QUEUES_CONF[EDIT_Queue].getProperty('leavewhenempty') );
	ASTGUI.updateFieldToValue(DOM_edit_reportholdtime, QUEUES_CONF[EDIT_Queue].getProperty('reportholdtime') );
	ASTGUI.updateFieldToValue(DOM_edit_voicemenuclass, QUEUES_CONF[EDIT_Queue].getProperty('context') );
	ASTGUI.domActions.checkSelected( ag_chkbxClass, (QUEUES_CONF[EDIT_Queue].getProperty('member')) ? QUEUES_CONF[EDIT_Queue]['member'].split(',') :[] ) ;
	ASTGUI.feedback({ msg:'Edit Queue !', showfor:2 });

	ASTGUI.showbg(true);
	DOM_edit_QueueDiv.style.display = '';
};


var edit_queue_apply = function(){
	var cat = ASTGUI.getFieldValue(DOM_Queue_Ext);
	var configLine = cat + ',1,Queue(${EXTEN})';
	if ( !ASTGUI.checkRequiredFields([ DOM_Queue_Ext , DOM_edit_label, DOM_edit_strategy]) ){
		return ;
	}
	if( !ASTGUI.validateFields(['edit_Ext', DOM_edit_label , 'edit_timeout','edit_wrapuptime','edit_maxlen']) ){
		return ;
	}

	if( QueueExistsbyThisName( ASTGUI.getFieldValue(DOM_edit_label) ) ){
		ASTGUI.highlightField( DOM_edit_label , 'There is another queue by this name !');
		return;
	}

	if( isNewQueue == true ){ // new QUEUE
		if(!ASTGUI.miscFunctions.isExtensionInRange( cat ,'qe_start','qe_end')){
			ASTGUI.highlightField(DOM_Queue_Ext, 'Extension is not in preferred range');
			parent.ASTGUI.dialog.hide();
			return;
		}

		if( parent.miscFunctions.ifExtensionAlreadyExists(cat) ){
			ASTGUI.highlightField(DOM_Queue_Ext, 'Extension already exists');
			parent.ASTGUI.dialog.hide();
			return;
		}

		var u = new listOfSynActions('extensions.conf') ;
		u.new_action('append', ASTGUI.contexts.QUEUES, 'exten', configLine );
		u.new_action('update', 'globals', 'QUEUES', parent.sessionData.pbxinfo.queues_list + "," + cat);
		parent.sessionData.pbxinfo.queues_list += "," + cat;
		u.callActions();

		var x = new listOfActions('queues.conf');
		x.new_action('delcat', cat, '', '');
		x.new_action('newcat', cat, '', ''); // create new context
		x.new_action('append', cat, 'fullname', ASTGUI.getFieldValue(DOM_edit_label));
		x.new_action('append', cat, 'strategy', ASTGUI.getFieldValue(DOM_edit_strategy));
		x.new_action('append', cat, 'timeout', ASTGUI.getFieldValue(DOM_edit_timeout));
		x.new_action('append', cat, 'wrapuptime', ASTGUI.getFieldValue(DOM_edit_wrapuptime));
		x.new_action('append', cat, 'autofill', ASTGUI.getFieldValue(DOM_edit_autofill));
		x.new_action('append', cat, 'autopause', ASTGUI.getFieldValue(DOM_edit_autopause));
		x.new_action('append', cat, 'joinempty', ASTGUI.getFieldValue(DOM_edit_joinempty));
		x.new_action('append', cat, 'leavewhenempty', ASTGUI.getFieldValue(DOM_edit_leavewhenempty));
		x.new_action('append', cat, 'reportholdtime', ASTGUI.getFieldValue(DOM_edit_reportholdtime));
		x.new_action('append', cat, 'maxlen', ASTGUI.getFieldValue(DOM_edit_maxlen));
		x.new_action('append', cat, 'musicclass', ASTGUI.getFieldValue(DOM_edit_musicclass));
		var s = ASTGUI.getFieldValue(DOM_edit_voicemenuclass); 
		if (s != '') x.new_action('append', cat, 'context', s);
	}else{ // Edit existing QUEUE
		if( parent.sessionData.pbxinfo.queues[cat].hasOwnProperty('isOLDGUI') && parent.sessionData.pbxinfo.queues[cat].isOLDGUI == true ){
			var u = new listOfSynActions('extensions.conf');
			u.new_action( 'delete', 'default', 'exten', '' , parent.sessionData.pbxinfo.queues[cat].configLine );
			u.new_action( 'append', ASTGUI.contexts.QUEUES, 'exten', configLine );
			u.callActions();
			delete parent.sessionData.pbxinfo.queues[cat].isOLDGUI ;
		}

		var x = new listOfActions('queues.conf');
		x.new_action('update', cat, 'fullname', ASTGUI.getFieldValue(DOM_edit_label));
		x.new_action('update', cat, 'strategy', ASTGUI.getFieldValue(DOM_edit_strategy));
		x.new_action('update', cat, 'timeout', ASTGUI.getFieldValue(DOM_edit_timeout));
		x.new_action('update', cat, 'wrapuptime', ASTGUI.getFieldValue(DOM_edit_wrapuptime));
		x.new_action('update', cat, 'autofill', ASTGUI.getFieldValue(DOM_edit_autofill));
		x.new_action('update', cat, 'autopause', ASTGUI.getFieldValue(DOM_edit_autopause));
		x.new_action('update', cat, 'joinempty', ASTGUI.getFieldValue(DOM_edit_joinempty));
		x.new_action('update', cat, 'leavewhenempty', ASTGUI.getFieldValue(DOM_edit_leavewhenempty));
		x.new_action('update', cat, 'reportholdtime', ASTGUI.getFieldValue(DOM_edit_reportholdtime));
		x.new_action('update', cat, 'maxlen', ASTGUI.getFieldValue(DOM_edit_maxlen));
		x.new_action('update', cat, 'musicclass', ASTGUI.getFieldValue(DOM_edit_musicclass));
		var s = ASTGUI.getFieldValue(DOM_edit_voicemenuclass);
		if (s != '') x.new_action('update', cat, 'context', s);
		else x.new_action('delete', cat, 'context');
	}

	$('.'+ag_chkbxClass).each(function(a) {
		x.new_action('delete', cat, 'member', $(this).val() );
	});
	var ags = ASTGUI.domActions.get_checked(ag_chkbxClass) ;
	ags.each( function(ag){
		x.new_action('append', cat, 'member', ag );
	});

	var after = function(){
		if( isNewQueue == true ){
			parent.sessionData.pbxinfo.queues[cat] = new ASTGUI.customObject ;
			parent.sessionData.pbxinfo.queues[cat]['configLine'] = configLine;
		}

		ASTGUI.feedback({ msg:'Changes Saved !', showfor:2 , color:'green', bgcolor:'#FFFFFF' });
		window.location.reload();
	};

	if( isNewQueue == true ){
		x.callActions(after);
	}else{
		ASTGUI.miscFunctions.delete_LinesLike({ context_name : cat , beginsWithArr: ['member=Agent'] , filename: 'queues.conf', cb:function(){
			x.callActions(after);
		} });
	}

};



var load_agents_checkboxes = function(){
	var ul = parent.pbx.users.list();
	var ul_agents = {};
	var agent_count = 0 ;
	ul.each(function(user){
		if( parent.sessionData.pbxinfo.users[user]['hasagent'] && parent.sessionData.pbxinfo.users[user]['hasagent'].isAstTrue() ){
			var added = 0;
			if (parent.sessionData.pbxinfo.users[user]['hassip'] && parent.sessionData.pbxinfo.users[user]['hassip'].isAstTrue()) {
				agent_count++; added++;
				ul_agents['SIP/' + user] = parent.sessionData.pbxinfo.users[user]['fullname'] + ' - SIP (' + user  + ')';
			}
			if (parent.sessionData.pbxinfo.users[user]['hasiax'] && parent.sessionData.pbxinfo.users[user]['hasiax'].isAstTrue()) {
				agent_count++; added++;
				ul_agents['IAX2/' + user] = parent.sessionData.pbxinfo.users[user]['fullname'] + ' - IAX (' + user  + ')';
			}
			if (added == 0){
				agent_count++;
				ul_agents['DAHDI/' + user] = parent.sessionData.pbxinfo.users[user]['fullname'] + ' - Analog (' + user  + ')';
			}
		}
	});

	if(!agent_count){
		var tmp_span = document.createElement('span');
		tmp_span.innerHTML = 'You do not have any users defined as agents !<BR> <A href=#>click here</A> to manage users.';
		$(DOM_edit_agents_chboxes).css( 'border', '1px solid #CDCDCD' );
		$(DOM_edit_agents_chboxes).css( 'text-align', 'center' );
		ASTGUI.events.add( tmp_span , 'click' , function(){
			$(DOM_edit_QueueDiv).hideWithBg();
			parent.miscFunctions.click_panel('users.html');
		});
		DOM_edit_agents_chboxes.appendChild(tmp_span);
	}else{
		ASTGUI.domActions.populateCheckBoxes( DOM_edit_agents_chboxes , ul_agents, ag_chkbxClass, true);
	}
};


var localajaxinit = function(){
	top.document.title = 'Manage call Queues' ;
	if( !ASTGUI.miscFunctions.alertIfRangeisNotdefined('qe_start','qe_end', 'Queues') ){
		$('#div_AgentLoginSettings').hide();
		$('.top_buttons').hide();
		return;
	}
	(function(){
		var t = [
			{url:'#', desc:'Queues', click_function: function(){ $('#div_ListOfQueues').show(); $('#div_AgentLoginSettings').hide(); }  } ,
			{url:'#', desc:'Agent Login Settings', click_function: function(){ $('#div_ListOfQueues').hide(); $('#div_AgentLoginSettings').show();} }
		];
		ASTGUI.tabbedOptions( _$('tabbedMenu') , t );
		$('#tabbedMenu').find('A:eq(0)').click();

		var mcls = config2json({filename: 'musiconhold.conf', catlist:'yes'});
		mcls.each( function(this_class){
			ASTGUI.selectbox.append('edit_musicclass', this_class, this_class );
		});
		_$('edit_musicclass').selectedIndex = -1 ;
/* Need to add list of voicemenus */
		ASTGUI.selectbox.append('edit_voicemenuclass', 'None', '' );
		var vmcls = parent.sessionData.pbxinfo.voicemenus.getOwnProperties();
		vmcls.each(function(vmenu){
			var vm_name = parent.sessionData.pbxinfo.voicemenus[vmenu].comment || vmenu ;
			ASTGUI.selectbox.append('edit_voicemenuclass', 'VoiceMenu -- ' + vm_name, vmenu );
		});
/* end */
	} )();

	loadDOMElements();
	load_agents_checkboxes();
	QUEUES_CONF = config2json({filename:'queues.conf', usf:1});
	updateQueuesTable();
}

var save_QueueSettings = function(){
	var u = new listOfSynActions('extensions.conf') ;
	var le = ASTGUI.getFieldValue('login_exten') ;
	var lce = ASTGUI.getFieldValue('login_callback_exten') ;
	if( !ASTGUI.validateFields(['login_exten', 'login_callback_exten']) ){
		return ;
	}
	if( le && le == lce ){
		ASTGUI.highlightField('login_exten' , 'Choose a different callback extension.');
		return;
	}

	if(loginsettings.agentLogin_line){
		var OLD_EXT = ASTGUI.parseContextLine.getExten( loginsettings.agentLogin_line ) ;
		if( le != OLD_EXT && parent.miscFunctions.ifExtensionAlreadyExists(le) ){
			ASTGUI.highlightField('login_exten' , 'Extension already exists');
			parent.ASTGUI.dialog.hide();
			return;
		}
		u.new_action('delete', ASTGUI.contexts.QUEUES , 'exten', '', loginsettings.agentLogin_line );
		if( parent.sessionData.pbxinfo.queues[OLD_EXT].hasOwnProperty('isOLDGUI') &&  parent.sessionData.pbxinfo.queues[OLD_EXT].isOLDGUI == true ){
			u.new_action('delete', 'default' , 'exten', '', loginsettings.agentLogin_line );
			delete parent.sessionData.pbxinfo.queues[OLD_EXT].isOLDGUI;
		}
 		delete parent.sessionData.pbxinfo.queues[OLD_EXT] ;
	}
	if(le){
		u.new_action('append', ASTGUI.contexts.QUEUES , 'exten',  le + ',1,agentlogin()');
		parent.sessionData.pbxinfo.queues[le] = new ASTGUI.customObject;
		parent.sessionData.pbxinfo.queues[le]['configLine'] = le + ',1,agentlogin()' ;
	}
	u.callActions();
	u.clearActions();

	if(loginsettings.agentCallbackLogin_line){
		var OLD_EXT = ASTGUI.parseContextLine.getExten( loginsettings.agentCallbackLogin_line ) ;
		if( lce != OLD_EXT && parent.miscFunctions.ifExtensionAlreadyExists(lce) ){
			ASTGUI.highlightField('login_callback_exten' , 'Extension already exists');
			parent.ASTGUI.dialog.hide();
			return;
		}
		u.new_action('delete', ASTGUI.contexts.QUEUES , 'exten', '', loginsettings.agentCallbackLogin_line );
		if( parent.sessionData.pbxinfo.queues[OLD_EXT].hasOwnProperty('isOLDGUI') &&  parent.sessionData.pbxinfo.queues[OLD_EXT].isOLDGUI == true ){
			u.new_action('delete', 'default' , 'exten', '', loginsettings.agentCallbackLogin_line );
			delete parent.sessionData.pbxinfo.queues[OLD_EXT].isOLDGUI;
		}
 		delete parent.sessionData.pbxinfo.queues[OLD_EXT] ;
	}
	if(lce){
		u.new_action('append', ASTGUI.contexts.QUEUES , 'exten', lce + ',1,Goto(queue-member-manager,handle_member,1)');
		parent.sessionData.pbxinfo.queues[lce] = new ASTGUI.customObject;
		parent.sessionData.pbxinfo.queues[lce]['configLine'] = lce + ',1,Goto(queue-member-manager,handle_member,1)' ;
	}

	u.callActions();
	ASTGUI.feedback({msg:'Changes Saved !', showfor: 3 , color: '#5D7CBA', bgcolor: '#FFFFFF'}) ;
	//window.location.reload();
};
