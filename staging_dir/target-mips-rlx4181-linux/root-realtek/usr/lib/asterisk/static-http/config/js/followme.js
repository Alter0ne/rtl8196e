/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * follow.html functions
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
var EDIT_USER ;
var CURRENT_DESTINATIONS = [] ;
var FOLLOWME_OPTIONS = '';

var load_users_table = function(){
	var TBL = _$('table_userslist') ;
	var addCell = ASTGUI.domActions.tr_addCell;
	var ul = parent.pbx.users.list();  ul = ul.sortNumbers( );

	if(!ul.length){
		ASTGUI.domActions.clear_table(TBL);
		var newRow = TBL.insertRow(-1);
		newRow.className = 'even';
		addCell( newRow , { html:'No users created !!'} );
		return ;
	}

	(function(){ // add first row
		var newRow = TBL.insertRow(-1);
		newRow.className = "frow";
		addCell( newRow , { html:'Extension'});
		addCell( newRow , { html:'Follow Me'});
		addCell( newRow , { html:'Follow Order'});

		addCell( newRow , { html:''});
	})();

	var ext_globals = context2json({filename: 'extensions.conf', context: 'globals' , usf: 1});
	var followme_cnf = config2json({filename: 'followme.conf', usf: 0});


	if( ext_globals.hasOwnProperty('FOLLOWMEOPTIONS') ){
		FOLLOWME_OPTIONS = ext_globals.FOLLOWMEOPTIONS ;
		_$('chk_fmoptions_s').checked = ( FOLLOWME_OPTIONS.contains('s') ) ? true : false;
		_$('chk_fmoptions_a').checked = ( FOLLOWME_OPTIONS.contains('a') ) ? true : false;
		_$('chk_fmoptions_n').checked = ( FOLLOWME_OPTIONS.contains('n') ) ? true : false;
	}

	ul.each( function(user){ // list each user in table
		var fmvar = 'FOLLOWME_' + user ;

		var newRow = TBL.insertRow(-1); 
		newRow.className = ((TBL.rows.length)%2==1)?'odd':'even';
		addCell( newRow , { html: user });
		if ( ext_globals.hasOwnProperty(fmvar) &&  ext_globals[fmvar] == '1' ){
			addCell( newRow , { html: '<font color=green><b>Enabled</b></font>' });
			var tmp_a = "<span class='guiButton' onclick=\"show_Edit_FollowMeUser('" + user +"', 1)\">Edit</span>&nbsp;" ;
		}else{
			addCell( newRow , { html: '<font color=red><b>Disabled<b></font>' });
			var tmp_a = "<span class='guiButton' onclick=\"show_Edit_FollowMeUser('" + user +"', 0)\">Edit</span>&nbsp;" ;
		}
		if( followme_cnf.hasOwnProperty(user) &&  followme_cnf[user].containsLike('number=') ){
			var tmp_followorder = [];
			followme_cnf[user].each( function(line){
				if (line.beginsWith('number=') ){
					line = line.lChop('number=') ;
					// var fl_number = line.split(',')[0] ;
					// var fl_seconds = line.split(',')[1];
					tmp_followorder.push( line.split(',')[0].split('&').join(' & ') );
				}
			});
			tmp_followorder = tmp_followorder.join(', ');
			addCell( newRow , { html: (tmp_followorder.length > 50) ? tmp_followorder.substr(0,50) + '....' : tmp_followorder , align :'left' });
		}else{
			addCell( newRow , { html: '<i>Not Configured</i>' });
		}
		addCell( newRow , { html: tmp_a , align:'center' });
	} );
};


var localajaxinit = function(){
	ASTGUI.tabbedOptions( _$('tabbedMenu') , [
		{	url: '#',
			desc: 'FollowMe Preferences for Users',
			click_function: function(){ $('#div_ONE_FOLLOWMEUSERS').show(); $('#div_TWO_FOLLOWME_PREFS').hide(); }
		},{
			url: '#',
			desc: 'FollowMe Options',
			click_function: function(){ $('#div_ONE_FOLLOWMEUSERS').hide(); $('#div_TWO_FOLLOWME_PREFS').show(); }
		}
	]);

	ASTGUI.events.add( 'newFM_Number_radio_local', 'click' , function(){
		$('#FMU_newNumber_local').hide();
		$('#FMU_newNumber_External').hide();

		if( _$('newFM_Number_radio_local').checked ){
			$('#FMU_newNumber_local').show();
		}
	});

	ASTGUI.events.add( 'newFM_Number_radio_Externals', 'click' , function(){ 
		$('#FMU_newNumber_local').hide();
		$('#FMU_newNumber_External').hide();

		if( _$('newFM_Number_radio_Externals').checked ){
			$('#FMU_newNumber_External').show();
		}
	});

	followMe_MiscFunctions.load_LocalExtensionsList();
	top.document.title = 'Follow Me' ;

	load_users_table();

	$('#tabbedMenu').find('A:eq(0)').click();

	(function(){
		var mcls = config2json({filename: 'musiconhold.conf', usf: 1});
		for (var this_class in mcls ){
			if(mcls.hasOwnProperty(this_class)){
				ASTGUI.selectbox.append('FMU_moh', this_class, this_class );
			}
		}
		_$('FMU_moh').selectedIndex = -1;

		var c = parent.pbx.call_plans.list() ;
		var tmp_pfx = ASTGUI.contexts.CallingPlanPrefix ;
		for( var c_t = 0 ; c_t < c.length ; c_t++ ){
			ASTGUI.selectbox.append( 'FMU_context' , c[c_t].withOut(tmp_pfx) , c[c_t] );
		}
	})();

	$('#sqDestinations').click(function(event){
		var s = ASTGUI.events.getTarget(event);
		var cl =  $(s).attr("class") ;
		if(!cl || !cl.beginsWith('step_') ){return;}
		var stepNo = Number( s.parentNode.STEPNO );
		switch(cl){
			case 'step_delete':
				CURRENT_DESTINATIONS.splice(stepNo,1);
				break;
			case 'step_up':
				if(stepNo == 0) return;
				var tmp = CURRENT_DESTINATIONS[stepNo] ;
				CURRENT_DESTINATIONS.splice(stepNo, 1);
				CURRENT_DESTINATIONS.splice(stepNo-1, 0, tmp);
				break;
			case 'step_down':
				if(stepNo == (CURRENT_DESTINATIONS.length-1) ) return;
				var tmp = CURRENT_DESTINATIONS[stepNo] ;
				CURRENT_DESTINATIONS.splice(stepNo+2, 0, tmp);
				CURRENT_DESTINATIONS.splice(stepNo, 1);
				break;
			default:
				break;
		}
		followMe_MiscFunctions.refresh_allDestinations();
	});

};



var show_Edit_FollowMeUser = function(u, fm_ed){

	EDIT_USER = u ;
	followMe_MiscFunctions.reset_Fields();

	// load fields
	if( !fm_ed ){
		fm_ed = 0;
	}else{
		fm_ed = Number(fm_ed);
	}

	if( fm_ed ){
		_$('FMU_Enable').checked = true ;
		_$('FMU_Disable').checked = false ;
	}else{
		_$('FMU_Enable').checked = false ;
		_$('FMU_Disable').checked = true ;
	}

	CURRENT_DESTINATIONS = [];

	ASTGUI.updateFieldToValue('FMU_context', parent.sessionData.pbxinfo.users[EDIT_USER].getProperty('context'));

	var cxt = context2json({ filename:'followme.conf' , context : EDIT_USER , usf: 0 });
	if(!cxt){ cxt = []; }
	for( var t =0; t < cxt.length ; t++ ){

		if( cxt[t].beginsWith('musicclass=') ){
			ASTGUI.updateFieldToValue('FMU_moh', cxt[t].afterChar('=') );
		}

		if( cxt[t].beginsWith('context=') ){
			ASTGUI.updateFieldToValue('FMU_context', cxt[t].afterChar('=') );
		}

		if( cxt[t].beginsWith('number=') ){
			CURRENT_DESTINATIONS.push( cxt[t].afterChar('=') );
		}
	}

	followMe_MiscFunctions.hide_FORM_newFM_Number();
	followMe_MiscFunctions.refresh_allDestinations();

	$('#div_followUser_edit').showWithBg();
};





var save_FollowMeUser = function(){
	// update follow me status of the user in extensions.conf
	// update followme.conf with all other chosen preferences
	// reload the page

	var fm_enabled = ( _$('FMU_Enable').checked ) ? '1' : '0' ;
	ASTGUI.updateaValue({ file:'extensions.conf', context :'globals', variable : 'FOLLOWME_' + EDIT_USER , value : fm_enabled });

	var FOLLOWME_CONF = config2json({ filename:'followme.conf', usf:0 });

	var u = new listOfActions('followme.conf');
	if( FOLLOWME_CONF.hasOwnProperty(EDIT_USER) ){ u.new_action('delcat', EDIT_USER , '', ''); }
	u.new_action( 'newcat', EDIT_USER , '', '');
	u.new_action( 'append', EDIT_USER , 'musicclass', ASTGUI.getFieldValue('FMU_moh') );
	u.new_action( 'append', EDIT_USER , 'context', ASTGUI.getFieldValue('FMU_context') );
	for( var t=0; t < CURRENT_DESTINATIONS.length ; t++ ){
		u.new_action( 'append', EDIT_USER , 'number', CURRENT_DESTINATIONS[t] );
	}

	var uinfo = parent.sessionData.pbxinfo.users[EDIT_USER];
	if( !uinfo.getProperty('hasvoicemail').isAstTrue() ){
		ASTGUI.updateaValue({file:'users.conf', context : EDIT_USER, variable :'hasvoicemail', value :'yes'});
		parent.sessionData.pbxinfo.users[EDIT_USER].hasvoicemail = 'yes';
	}

	u.callActions( function(){
		window.location.reload();
	});

};


var followMe_MiscFunctions = {

	load_LocalExtensionsList : function(){ //  followMe_MiscFunctions.load_LocalExtensionsList()
		ASTGUI.selectbox.clear('FMU_newNumber_local');
		var ul = parent.pbx.users.list();  ul = ul.sortNumbers( );
		ul.each( function(user){
			ASTGUI.selectbox.append('FMU_newNumber_local', user + ' ' + parent.sessionData.pbxinfo.users[user].getProperty('fullname') , user);
		});
	},

	show_FORM_newFM_Number : function(){ // followMe_MiscFunctions.show_FORM_newFM_Number()
		ASTGUI.updateFieldToValue( 'FMU_newNumber_seconds', '30' );
		_$('newFM_Number_radio_local').checked = true;
		_$('newFM_Number_radio_Externals').checked = false;
		_$('newFM_Order_radio_after').checked = true;
		_$('newFM_Order_radio_alongWith').checked = false;
		_$('FMU_newNumber_local').selectedIndex = -1 ;
		ASTGUI.updateFieldToValue('FMU_newNumber_External', '');
		$('#FMU_newNumber_local').show();
		$('#FMU_newNumber_External').hide();
		$('.FORM_newFM_Number').show();
		$($('.FORM_newFM_Number')[0]).hide();
		$('#lastRow_Edit').hide();
	},

	hide_FORM_newFM_Number : function(){ // followMe_MiscFunctions.hide_FORM_newFM_Number()
		$('.FORM_newFM_Number').hide();
		$($('.FORM_newFM_Number')[0]).show();
		$('#lastRow_Edit').show();
	},

	reset_Fields : function(){ // followMe_MiscFunctions.reset_Fields()
		ASTGUI.resetTheseFields ([ 'FMU_Enable', 'FMU_Disable', 'FMU_moh', 'FMU_context','FMU_newNumber_local','FMU_newNumber_seconds' ]);
		ASTGUI.domActions.removeAllChilds( 'sqDestinations' ); CURRENT_DESTINATIONS = [] ;

	},

	refresh_allDestinations: function(){ // followMe_MiscFunctions.refresh_allDestinations()
		ASTGUI.domActions.removeAllChilds( 'sqDestinations' );
		var add_sqStep = function(a){
			var txt = CURRENT_DESTINATIONS[a];
			var tmp = document.createElement('div');
			tmp.STEPNO = a ;
			var sp_desc = document.createElement('span');
				sp_desc.className = 'step_desc';
				sp_desc.innerHTML = txt.split(',')[0].split('&').join(' <B>&</B> ') + ' (' +  txt.split(',')[1] + ' seconds)' ;
			var sp_up = document.createElement('span');
				sp_up.className = 'step_up';
				sp_up.innerHTML = '&nbsp;';
			var sp_down = document.createElement('span');
				sp_down.className = 'step_down';
				sp_down.innerHTML = '&nbsp;';
			var sp_delete = document.createElement('span');
				sp_delete.className = 'step_delete';
				sp_delete.innerHTML = '&nbsp;';
	
			tmp.appendChild(sp_desc) ;
			tmp.appendChild(sp_delete) ;
			tmp.appendChild(sp_up) ;
			tmp.appendChild(sp_down) ;
			_$('sqDestinations').appendChild(tmp) ;
		};
		for( var t=0; t < CURRENT_DESTINATIONS.length ; t++ ){
			add_sqStep(t);
		}
	},

	push_newdest: function(){ // followMe_MiscFunctions.push_newdest() ;
		var tmp_seconds = ASTGUI.getFieldValue( 'FMU_newNumber_seconds' ) || '30' ;
		if( _$('newFM_Number_radio_local').checked ){
			if( _$('FMU_newNumber_local').selectedIndex == -1 ){ 
				ASTGUI.highlightField( 'FMU_newNumber_local', 'Please select a Local Extension to Dial !');
				return; 
			}
			var tmp_number = ASTGUI.getFieldValue('FMU_newNumber_local');
		}
		if( _$('newFM_Number_radio_Externals').checked ){
			if( !ASTGUI.getFieldValue('FMU_newNumber_External') ){
				ASTGUI.highlightField( 'FMU_newNumber_External', 'Please Enter a Number to Dial !');
				return; 
			}
			var tmp_number = ASTGUI.getFieldValue('FMU_newNumber_External');
		}
		if( tmp_number.contains('-') ) tmp_number = tmp_number.withOut('-');
		var tmp_dest = tmp_number + ',' + tmp_seconds;

		var tmp_last = CURRENT_DESTINATIONS.lastValue();
		if( _$('newFM_Order_radio_after').checked || !tmp_last){
			CURRENT_DESTINATIONS.push(tmp_dest);
		}

		if ( _$('newFM_Order_radio_alongWith').checked && tmp_last){
			CURRENT_DESTINATIONS.replaceLastWith( tmp_last.split(',')[0] + '&' + tmp_dest );
		}

		this.refresh_allDestinations();
		this.hide_FORM_newFM_Number();
		//ASTGUI.resetTheseFields (['FMU_newNumber','FMU_newNumber_seconds' ]);
	}
};


var update_FollowMe_Options = function(){

	FOLLOWME_OPTIONS = '';

	if( _$('chk_fmoptions_n').checked ) FOLLOWME_OPTIONS = FOLLOWME_OPTIONS + 'n' ;
	if( _$('chk_fmoptions_a').checked ) FOLLOWME_OPTIONS = FOLLOWME_OPTIONS + 'a' ;
	if( _$('chk_fmoptions_s').checked ) FOLLOWME_OPTIONS = FOLLOWME_OPTIONS + 's' ;

	ASTGUI.updateaValue({ file:'extensions.conf', context :'globals', variable :'FOLLOWMEOPTIONS', value : FOLLOWME_OPTIONS });
	ASTGUI.feedback({msg:' Saved !!', showfor: 3 , color: '#5D7CBA', bgcolor: '#FFFFFF'}) ;

};
