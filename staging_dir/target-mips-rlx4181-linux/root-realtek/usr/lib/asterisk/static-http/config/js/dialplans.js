/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * dialplans.html functions
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
var isNewDP ;
var EDIT_DP ;
var cr_chkbxClass = "callingRules_Chkboxes";
var CONTEXTS_CR = [] ; // store all callingrule contexts and other standard contexts in this array

var show_NewDialPlan_form = function(){
	isNewDP = true;
	EDIT_DP = '';
	resetEditForm();
	$(DOM_Edit_DLPN_DIV).showWithBg();
};

var show_EditDialPlan_form = function(k){
	isNewDP = false;
	EDIT_DP = k;
	resetEditForm();
	$(DOM_Edit_DLPN_DIV).showWithBg();
};

var delete_DP_confirm = function(k){
	var ul = parent.pbx.users.list();
	for( var f=0 ; f < ul.length ; f++ ){
		if( parent.sessionData.pbxinfo.users[ ul[f] ].getProperty('context') == k ){
			ASTGUI.feedback( { msg:'Can not delete dialplan !<BR> The selected Dial Plan is in use by one or more users.', showfor:3, color:'red' } );
			return;
		}
	}

	if( !confirm(' Delete DialPlan ' + k.withOut(ASTGUI.contexts.CallingPlanPrefix) + '?' ) ){ return; }
	parent.ASTGUI.dialog.waitWhile(' Applying Changes ...');
	parent.pbx.call_plans.remove(k);
	var after = function(){
		ASTGUI.feedback( { msg: 'deleted DialPlan !', showfor: 2 , color: 'blue', bgcolor: '#FFFFFF' } );
		parent.ASTGUI.dialog.hide();
		window.location.reload();
	};
	setTimeout(after, 700);
};

var edit_DP_save_go = function(){
	if( !ASTGUI.validateFields([ DOM_edit_dlpn_name ]) ){
		return ;
	}
	if( !ASTGUI.checkRequiredFields([ DOM_edit_dlpn_name ]) ){
		return ;
	}

	if (DOM_edit_dlpn_name.value.length > 70) {
		ASTGUI.feedback({ msg: 'Dialplan name is too long. Please keep it under 70 chars.', showfor: 3, color: 'red'});
		DOM_edit_dlpn_name.focus();	
		return;
	}

	var dp_name = DOM_edit_dlpn_name.value;

	if( isNewDP ){ // check if there is already a DialPlan by this name
		var tmp_dps = parent.pbx.call_plans.list() ;
		if( tmp_dps.contains( ASTGUI.contexts.CallingPlanPrefix + dp_name ) ){
			ASTGUI.feedback( { msg:' DialPlan name already in use ! <BR> Please choose another name.', showfor: 3, color:'red' } );
			DOM_edit_dlpn_name.focus();
			return;
		}
	}

	var dp = { includes: ASTGUI.domActions.get_checked(cr_chkbxClass) };
	dp = ASTGUI.toCustomObject(dp) ;
	parent.ASTGUI.dialog.waitWhile(' Applying Changes ...');

	var after = function(){
		parent.ASTGUI.dialog.hide();
		ASTGUI.feedback( { msg: 'DialPlan updated !', showfor: 2 , color: 'blue', bgcolor: '#FFFFFF' } );
		window.location.reload();
	};

	var add = function(){
		if( isNewDP == false && dp_name != EDIT_DP){ // if dialplan name is changed, update any users context who have this dialplan
			(function(){
				var ul = parent.pbx.users.list();
				ul.each(
					function(user){
						if( parent.sessionData.pbxinfo.users[user].context == EDIT_DP ){
							var t = ASTGUI.contexts.CallingPlanPrefix + dp_name ;
							parent.pbx.users.edit(user, {context: t });
						}
					}
				);
			})();
		}
		parent.pbx.call_plans.add(dp_name, dp , after );
	};
	if(isNewDP){
		add();
	}else{
		parent.pbx.call_plans.remove(EDIT_DP);
		setTimeout(add, 700);
	}

}

var resetEditForm = function(){
	if(isNewDP){
		_$('Edit_dialog_title').innerHTML = 'Create New DialPlan';
		ASTGUI.domActions.unCheckAll( cr_chkbxClass );
		ASTGUI.domActions.checkSelected( cr_chkbxClass, ASTGUI.includeContexts ) ;
		DOM_edit_dlpn_name.value = parent.pbx.call_plans.nextAvailable() ;
		return ;
	}

	_$('Edit_dialog_title').innerHTML = 'Edit DialPlan ' ;
	DOM_edit_dlpn_name.value = EDIT_DP.withOut(ASTGUI.contexts.CallingPlanPrefix);
	var theseRules = ASTGUI.cloneObject( parent.sessionData.pbxinfo.callingPlans[EDIT_DP].includes ) ;
		// We really do not need to cloneObject() here
		// but IE is attaching prototype methods as if they are real methods to any /string / array / Objects in a parent iframe
		// and when we access them again , we get 'trying to access method from a freed script' error

	ASTGUI.domActions.checkSelected( cr_chkbxClass, theseRules ) ;
}

var set_thisDP_as_default = function(a){
	ASTGUI.updateaValue({ file: ASTGUI.globals.configfile, context :'general', variable :'default_dialplan', value : a });
	parent.sessionData.GUI_PREFERENCES.default_dialplan = a;
	ASTGUI.feedback( { msg: 'Updated default DialPlan !', showfor: 2 , color: 'blue', bgcolor: '#FFFFFF' } );
	window.location.reload();
};

var load_CallingPlansTable = function(){
	var addCell = ASTGUI.domActions.tr_addCell; // temporarily store the function
	(function(){ // add first row
		var newRow = DOM_table_DialPlans_list.insertRow(-1);
		newRow.className = "frow";
		addCell( newRow , { html:'Default' } );
		addCell( newRow , { html:'Dial Plan'} );
		addCell( newRow , { html:'Calling Rules', width: '610px'} );
		addCell( newRow , { html:'Options', width: '120px'} );
	})();	

	(function (){
		var c = parent.pbx.call_plans.list() ;
		c.each(function(plan){
			var newRow = DOM_table_DialPlans_list.insertRow(-1);
			newRow.className = ((DOM_table_DialPlans_list.rows.length)%2==1)?'odd':'even';
			var img_name = ( parent.sessionData.GUI_PREFERENCES.default_dialplan == plan ) ? 'images/edit.gif' : 'images/checkbox_blank.gif';
			addCell( newRow , { html: "<A href='#' TITLE='Set this as the default dial plan when creating new users'><img src=" + img_name + " border=0 onclick=\"set_thisDP_as_default('" + plan +"')\"></A>" } );
			addCell( newRow , { html: plan.withOut( ASTGUI.contexts.CallingPlanPrefix )});

			var dr_woPfx = [] ;
			var this_includes = parent.sessionData.pbxinfo.callingPlans[plan].includes;
			try{
				for (var k = 0 ; k < this_includes.length ; k++){
					var k_each = this_includes[k] ;
					if( CONTEXTS_CR.contains(k_each) ){
						dr_woPfx.push( k_each.lChop(ASTGUI.contexts.CallingRulePrefix) );
					}else{
						dr_woPfx.push( '<font color=red>' + k_each.lChop(ASTGUI.contexts.CallingRulePrefix) + '?</font>' );
					}
				}
			}catch(err){
				top.log.error(err.description);
			}

			addCell( newRow , { html: dr_woPfx.join(', ') } );

			var tmp = "<span class='guiButton' onclick=\"show_EditDialPlan_form('" + plan +"')\">Edit</span>" + 
				"<span class='guiButtonDelete' onclick=\"delete_DP_confirm('" + plan +"')\">Delete</span>" ;
			addCell( newRow , { html: tmp } );
		});

		if(DOM_table_DialPlans_list.rows.length == 1){
			ASTGUI.domActions.clear_table(DOM_table_DialPlans_list);
			var newRow = DOM_table_DialPlans_list.insertRow(-1);
			newRow.className = 'even';
			addCell( newRow , { html:'No DialPlans defined !!'} );
			return ;
		}
	})();



};


var load_callingRules_checkboxes = function(){

	var g = parent.sessionData.pbxinfo.callingRules.getOwnProperties() ;
	var f = {};
	g.each(function(rule){
		f[rule] = rule.withOut(ASTGUI.contexts.CallingRulePrefix);
		CONTEXTS_CR.push(rule);
	});
	if( CONTEXTS_CR.length ){
		ASTGUI.domActions.populateCheckBoxes( DOM_edit_includeCheckboxes_div , f, cr_chkbxClass);
	}else{

		var tmp_span = document.createElement('span');
		tmp_span.innerHTML = 'You do not have any calling Rules defined !<BR> <A href=#>click here</A> to manage calling rules.';
		$(DOM_edit_includeCheckboxes_div).css( 'border', '1px solid #CDCDCD' );
		$(DOM_edit_includeCheckboxes_div).css( 'text-align', 'center' );

		ASTGUI.events.add( tmp_span , 'click' , function(){
			$(DOM_Edit_DLPN_DIV).hideWithBg();
			parent.miscFunctions.click_panel('callingrules.html');
		} );

		DOM_edit_includeCheckboxes_div.appendChild(tmp_span);
	}
	var f = {};
	ASTGUI.includeContexts.each( function(ct){ f[ct] = ct; CONTEXTS_CR.push(ct); });
	ASTGUI.domActions.populateCheckBoxes( DOM_edit_LC_includeCheckboxes_div , f, cr_chkbxClass);
};


function localajaxinit(){
	top.document.title = 'Manage Dialplans' ;
	// show list of dialplans
	// - each dialplan is a set of calling rules
	// OnEdit show Checkboxes - which are ticked
	DOM_table_DialPlans_list = _$('table_DialPlans_list');
	DOM_Edit_DLPN_DIV = _$('Edit_DLPN_DIV');
	DOM_edit_dlpn_name = _$('edit_dlpn_name'); 
	DOM_edit_includeCheckboxes_div = _$('edit_includeCheckboxes_div');
	DOM_edit_LC_includeCheckboxes_div = _$('edit_LC_includeCheckboxes_div');

	load_callingRules_checkboxes();
	load_CallingPlansTable();
	
	(function(){
		var t = context2json({ filename: ASTGUI.globals.configfile , context : 'general' , usf:1 });
		parent.sessionData.GUI_PREFERENCES.default_dialplan = t.getProperty('default_dialplan');
	})();
};
