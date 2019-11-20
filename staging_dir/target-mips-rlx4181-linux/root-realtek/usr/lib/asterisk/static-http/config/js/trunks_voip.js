/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * trunks_voip.html functions
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
var isNewTrunk ;
var EDIT_TRUNK ;
var EDIT_DID = '';
var EDIT_DID_rules = [];
var EDIT_TRUNK_luser = '' ; // old mwi luser


var loadDOMelements = function (){
	DOM_table_VOIPTrunks_list = _$('table_VOIPTrunks_list');
	DOM_edit_VOIPTrunk_DIV = _$('edit_VOIPTrunk_DIV');
	DOM_edit_VOIPTrunk_DIV_Title = _$('edit_VOIPTrunk_DIV_Title');
	DOM_edit_VOIPTrunk_Type = _$('edit_VOIPTrunk_Type');
	DOM_edit_VOIPTrunk_Context_Basis = _$('edit_VOIPTrunk_Context_Basis');
	DOM_edit_VOIPTrunk_Hostname = _$('edit_VOIPTrunk_Hostname');
	DOM_edit_VOIPTrunk_Username = _$('edit_VOIPTrunk_Username');
	DOM_edit_VOIPTrunk_Password = _$('edit_VOIPTrunk_Password');
};



var new_trunk_form = function(){
	$('.editTrunk_Field').hide();
	$('.editTrunk_Field_ermwi').hide();
	isNewTrunk = true;
	EDIT_TRUNK = '';
	show_Edit_Trunk();
};



var edit_trunk_form = function(ET){
	$('.editTrunk_Field').show();
	//$('.editTrunk_Field_ermwi').show();
	EDIT_TRUNK = ET ;
	isNewTrunk = false ;
	show_Edit_Trunk();
};



var show_Edit_Trunk = function(){
	ASTGUI.updateFieldToValue( 'codec_one', '' );
	ASTGUI.updateFieldToValue( 'codec_two', '' );
	ASTGUI.updateFieldToValue( 'codec_three', '' );
	ASTGUI.updateFieldToValue( 'codec_fourth', '' );
	ASTGUI.updateFieldToValue( 'codec_fifth', '' );

	if( isNewTrunk == true ) {
		DOM_edit_VOIPTrunk_DIV_Title.innerHTML = 'Create New SIP/IAX trunk';
		ASTGUI.resetTheseFields([ DOM_edit_VOIPTrunk_Type, DOM_edit_VOIPTrunk_Hostname , DOM_edit_VOIPTrunk_Username ,  DOM_edit_VOIPTrunk_Password , 'trunk_obcid' , 'edit_VOIPTrunk_Providername','trunk_fromdomain', 'trunk_fromuser','trunk_authuser', 'trunk_insecure' ]);
		DOM_edit_VOIPTrunk_Type.disabled = false;
		DOM_edit_VOIPTrunk_Context_Basis.disabled = false;
		ASTGUI.feedback( { msg: 'Create New Trunk', showfor:2 });
		$('#TR_trunktype').show();
		$('#TR_contextbasis').show();
	} else {
		$('#TR_trunktype').hide();
		$('#TR_contextbasis').hide();
		var ttype = parent.pbx.trunks.getType(EDIT_TRUNK);
		var tinfo = parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK];

		DOM_edit_VOIPTrunk_Type.disabled = true;
		DOM_edit_VOIPTrunk_Context_Basis.disabled = true;
		DOM_edit_VOIPTrunk_DIV_Title.innerHTML = 'Edit ' + ttype.toUpperCase() + ' trunk ' + EDIT_TRUNK;
		ASTGUI.updateFieldToValue( DOM_edit_VOIPTrunk_Type, ttype.toUpperCase() );
		ASTGUI.updateFieldToValue( DOM_edit_VOIPTrunk_Hostname , getProperty(tinfo, 'host') );
		ASTGUI.updateFieldToValue( DOM_edit_VOIPTrunk_Username , getProperty(tinfo, 'username') );
		ASTGUI.updateFieldToValue( DOM_edit_VOIPTrunk_Password , getProperty(tinfo, 'secret') );
		ASTGUI.updateFieldToValue( 'edit_VOIPTrunk_Providername' , getProperty(tinfo, 'trunkname') );
		ASTGUI.updateFieldToValue( 'trunk_fromdomain' , getProperty(tinfo, 'fromdomain') );
		ASTGUI.updateFieldToValue( 'trunk_fromuser' , getProperty(tinfo, 'fromuser') );
		ASTGUI.updateFieldToValue( 'trunk_authuser' , getProperty(tinfo, 'authuser') );
		ASTGUI.updateFieldToValue( 'trunk_insecure' , getProperty(tinfo, 'insecure') );
		ASTGUI.updateFieldToValue( 'trunk_outboundproxy' , getProperty(tinfo, 'outboundproxy') );

		if( getProperty(tinfo, 'allow') == 'all'){
			ASTGUI.updateFieldToValue( 'codec_one', 'ulaw' );
			ASTGUI.updateFieldToValue( 'codec_two', 'alaw' );
			ASTGUI.updateFieldToValue( 'codec_three', 'gsm' );
			ASTGUI.updateFieldToValue( 'codec_fourth', 'g726' );
			ASTGUI.updateFieldToValue( 'codec_fifth', 'g722' );
		}else{
			var codecs_tmp = getProperty(tinfo, 'allow').split(',') ;
			ASTGUI.updateFieldToValue( 'codec_one', (codecs_tmp[0] && codecs_tmp[0].trim()) || '' );
			ASTGUI.updateFieldToValue( 'codec_two', (codecs_tmp[1] && codecs_tmp[1].trim()) || '' );
			ASTGUI.updateFieldToValue( 'codec_three', (codecs_tmp[2] && codecs_tmp[2].trim()) || '' );
			ASTGUI.updateFieldToValue( 'codec_fourth', (codecs_tmp[3] && codecs_tmp[3].trim()) || '' );
			ASTGUI.updateFieldToValue( 'codec_fifth', (codecs_tmp[4] && codecs_tmp[4].trim()) || '' );
		}

		/* Don't allow editing field if the asterisk [context] is based on it. */
		DOM_edit_VOIPTrunk_Username.disabled = false;
		_$('edit_VOIPTrunk_Providername').disabled = false;
		if (EDIT_TRUNK == getProperty(tinfo, 'username')) DOM_edit_VOIPTrunk_Username.disabled = true;
		if (EDIT_TRUNK == getProperty(tinfo, 'trunkname')) _$('edit_VOIPTrunk_Providername').disabled = true;

		var c = context2json({ filename:'extensions.conf', context: 'globals' , usf: 1 });
		ASTGUI.updateFieldToValue( 'trunk_obcid', getProperty(c, ASTGUI.globals.obcidUsrPrefix + EDIT_TRUNK) );


		// remote MWI stuff (only for SIP trunks)
			if ( ttype == 'sip' ){
				$('#outboundproxy_field').show();
				$('.editTrunk_Field_ermwi').show();
			}else{
				$('#outboundproxy_field').hide();
				$('.editTrunk_Field_ermwi').hide();
			}
			(function(){
				_$('edit_ERMWI_luser').selectedIndex = -1 ;
				var mwi_user = '';

				var d = config2json({ filename:'users.conf', usf: 1 });
				c = d[EDIT_TRUNK];
				if ( c.hasOwnProperty('mwi') ){
					ASTGUI.updateFieldToValue( 'edit_ERMWI' , 'yes' );
					$('#editTrunk_Field_ermwi_row2').show();
					var ruser = c['mwi'].afterChar('/');
					ASTGUI.updateFieldToValue( 'edit_ERMWI_rmbx' ,  ruser );
				}else{
					$('#editTrunk_Field_ermwi_row2').hide();
					_$('edit_ERMWI').checked = false ;
					ASTGUI.updateFieldToValue('edit_ERMWI_rmbx' , '');
				}

				for( var this_user in d ){ if( d.hasOwnProperty(this_user) ){
					if (getProperty(d[this_user], 'mailbox').contains('MailboxStore') &&  getProperty(d[this_user], 'mailbox').contains(ruser) ){
						ASTGUI.updateFieldToValue( 'edit_ERMWI_luser' , this_user );
						EDIT_TRUNK_luser = this_user;
					}
				}}
			})();

		// End of 'remote MWI stuff'
		ASTGUI.feedback( { msg:'Edit Trunk', showfor: 2 });
	}
	$(DOM_edit_VOIPTrunk_DIV).showWithBg();
};


var edit_VOIPTrunk_save_go = function(){
	var topreload = false;
	var tmp_username = ASTGUI.getFieldValue('edit_VOIPTrunk_Username');
	if( /[^a-zA-Z_0-9\.]/.test(tmp_username) ){
		var tmp_confirm = confirm('The Voip Username appears to contain special characters. \n Click \'OK\' to continue, \'Cancel\' to abort.');
		if( !tmp_confirm ) return;
	}

	if ( !ASTGUI.checkRequiredFields(['edit_VOIPTrunk_Providername', 'edit_VOIPTrunk_Type', 'edit_VOIPTrunk_Context_Basis', 'edit_VOIPTrunk_Hostname']) ) {
		return ;
	}

	if( isNewTrunk == true ) {
	// New Trunk
		var tcv = $('#edit_VOIPTrunk_Context_Basis').val();  /* How do we assign context name ? */
		if (tcv == 'FromUser') {
			var tmp_ttype = parent.pbx.trunks.getType(DOM_edit_VOIPTrunk_Username.value);
			if( tmp_ttype ){
				ASTGUI.highlightField( DOM_edit_VOIPTrunk_Username , "Another trunk exists with this name !!" );
				return;
			}
		}
		else if (tcv == 'FromProvider') {
			var tmp_ttype = parent.pbx.trunks.getType(ASTGUI.getFieldValue('edit_VOIPTrunk_Providername'));
			if( tmp_ttype ){
				ASTGUI.highlightField( 'edit_VOIPTrunk_Providername' , "Another trunk exists with this name !!" );
				return;
			}
		}

		var ttv = DOM_edit_VOIPTrunk_Type.value;

		var trp = {
			host: 	  DOM_edit_VOIPTrunk_Hostname.value ,
			username: DOM_edit_VOIPTrunk_Username.value ,
			secret:   DOM_edit_VOIPTrunk_Password.value ,
			trunkname: ASTGUI.getFieldValue('edit_VOIPTrunk_Providername')
		};

		var cbf = function(){
			ASTGUI.feedback({msg:'Created New ' + ttv+ ' trunk !', showfor: 3 , color: 'green', bgcolor: '#FFFFFF'}) ;
			window.location.reload();
		};

		if (ttv =='SIP') { 
			var retval = parent.pbx.trunks.add('sip', trp, cbf, tcv);
		} else if (ttv =='IAX') { 
			var retval = parent.pbx.trunks.add('iax', trp, cbf, DOM_edit_VOIPTrunk_Context_Basis.value);
		}

		if (retval) {
			ASTGUI.feedback( { msg:'added voip trunk ' + tmp_username, showfor: 5, color:'red', bgcolor:'#ffffff' } );
			window.location.reload();
		}


	}else if(isNewTrunk == false){
	// Edit Existing Trunk
		parent.ASTGUI.dialog.waitWhile(' Saving ...') ;
			var codecs = '';
			if( ASTGUI.getFieldValue('codec_one') ){ codecs = codecs + ASTGUI.getFieldValue('codec_one') }
			if( ASTGUI.getFieldValue('codec_two') ){ codecs = codecs + ',' + ASTGUI.getFieldValue('codec_two') }
			if( ASTGUI.getFieldValue('codec_three') ){ codecs = codecs + ',' + ASTGUI.getFieldValue('codec_three') }
			if( ASTGUI.getFieldValue('codec_fourth') ){ codecs = codecs + ',' + ASTGUI.getFieldValue('codec_fourth') }
			if( ASTGUI.getFieldValue('codec_fifth') ){ codecs = codecs + ',' + ASTGUI.getFieldValue('codec_fifth') }

		var ttype = parent.pbx.trunks.getType(EDIT_TRUNK);
		var x = new listOfActions('users.conf');
		var v = new listOfActions('extensions.conf') ;

		if( ASTGUI.getFieldValue('trunk_obcid') ){
			v.new_action('update', 'globals', ASTGUI.globals.obcidUsrPrefix + EDIT_TRUNK, ASTGUI.getFieldValue('trunk_obcid') );
		}else{
			v.new_action('delete', 'globals', ASTGUI.globals.obcidUsrPrefix + EDIT_TRUNK, '','' );
		}

		var old_trunkUsername = parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK]['username'] ;

		if ($('#trunk_outboundproxy').val() !== '') {
			x.new_action('update', EDIT_TRUNK, 'outboundproxy', $('#trunk_outboundproxy').val());
		} else {
			x.new_action('delete', EDIT_TRUNK, 'outboundproxy', '');
		}
		parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK]['outboundproxy'] = $('#trunk_outboundproxy').val();
		x.new_action('update', EDIT_TRUNK , 'host', ASTGUI.getFieldValue(DOM_edit_VOIPTrunk_Hostname) );
			parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK]['host'] = ASTGUI.getFieldValue(DOM_edit_VOIPTrunk_Hostname) ;


		(function(){
			var tmp_fromdomain = ASTGUI.getFieldValue('trunk_fromdomain') ;
			if( tmp_fromdomain ){
				x.new_action('update', EDIT_TRUNK , 'fromdomain', tmp_fromdomain );
				parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK]['fromdomain'] = tmp_fromdomain ;
			}else{
				if( parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK].hasOwnProperty('fromdomain') ){
					delete parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK]['fromdomain'] ;
					x.new_action('delete', EDIT_TRUNK , 'fromdomain' );
				}
			}
	
			var tmp_fromuser = ASTGUI.getFieldValue('trunk_fromuser') ;
			if( tmp_fromuser ){
				x.new_action('update', EDIT_TRUNK , 'fromuser', tmp_fromuser );
				parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK]['fromuser'] = tmp_fromuser ;
			}else{
				if( parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK].hasOwnProperty('fromuser') ){
					delete parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK]['fromuser'] ;
					x.new_action('delete', EDIT_TRUNK , 'fromuser' );
				}
			}

			var tmp_authuser = ASTGUI.getFieldValue('trunk_authuser');
			if( tmp_authuser ){
				x.new_action('update', EDIT_TRUNK , 'authuser', tmp_authuser );
				parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK]['authuser'] = tmp_authuser ;
			}else{
				if( parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK].hasOwnProperty('authuser') ){
					delete parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK]['authuser'] ;
					x.new_action('delete', EDIT_TRUNK , 'authuser' );
				}
			}


			var tmp_insecure = ASTGUI.getFieldValue('trunk_insecure') ;
			x.new_action('update', EDIT_TRUNK , 'insecure', tmp_insecure );
			parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK]['insecure'] = tmp_insecure ;
		})();


		x.new_action('update', EDIT_TRUNK , 'secret', ASTGUI.getFieldValue(DOM_edit_VOIPTrunk_Password) );
			parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK]['secret'] = ASTGUI.getFieldValue(DOM_edit_VOIPTrunk_Password) ;
		x.new_action('update', EDIT_TRUNK , 'username', ASTGUI.getFieldValue(DOM_edit_VOIPTrunk_Username) );
			parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK]['username'] = ASTGUI.getFieldValue(DOM_edit_VOIPTrunk_Username) ;
		x.new_action('update', EDIT_TRUNK , 'trunkname', ASTGUI.getFieldValue('edit_VOIPTrunk_Providername').guiMetaData() );
			parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK]['trunkname'] = ASTGUI.getFieldValue('edit_VOIPTrunk_Providername') ;
		x.new_action('delete', EDIT_TRUNK, 'disallow', '') ;
		x.new_action('delete', EDIT_TRUNK, 'allow', '') ;
		x.new_action('append', EDIT_TRUNK, 'disallow', 'all') ;		parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK]['disallow'] = 'all' ;
		if( !codecs ){
			x.new_action('append', EDIT_TRUNK, 'allow', 'all' ) ;	parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK]['allow'] = 'all' ;
		}else{
			x.new_action('append', EDIT_TRUNK, 'allow', codecs ) ;	parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK]['allow'] = codecs ;
		}

		// remote MWI - only for SIP trunk
			if ( ttype == 'sip' ){
				var ruser = ASTGUI.getFieldValue('edit_ERMWI_rmbx') ;
				var luser = ASTGUI.getFieldValue('edit_ERMWI_luser') ;

				if ( _$('edit_ERMWI').checked && ruser){
					var tmp_mwistr= ASTGUI.getFieldValue(DOM_edit_VOIPTrunk_Username) + ':'+ ASTGUI.getFieldValue(DOM_edit_VOIPTrunk_Password) + '@' + ASTGUI.getFieldValue(DOM_edit_VOIPTrunk_Hostname) + '/' + ruser ;
					tmp_luser_mailbox = 'MailboxStore:'+ ruser ;

					x.new_action('update', EDIT_TRUNK , 'mwi', tmp_mwistr );
						parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK]['mwi'] = tmp_mwistr ;
					x.new_action('update', luser , 'mailbox', tmp_luser_mailbox );
						parent.sessionData.pbxinfo.users[luser].mailbox = tmp_luser_mailbox;
					x.new_action('update', luser , 'hasvoicemail', 'yes' );
						parent.sessionData.pbxinfo.users[luser].hasvoicemail = 'yes';

					if ( EDIT_TRUNK_luser && EDIT_TRUNK_luser != luser && parent.sessionData.pbxinfo.users.hasOwnProperty(EDIT_TRUNK_luser) ){
						x.new_action('update', EDIT_TRUNK_luser , 'mailbox', EDIT_TRUNK_luser );
						parent.sessionData.pbxinfo.users[EDIT_TRUNK_luser].mailbox = EDIT_TRUNK_luser;
					}
				}else{
					x.new_action('delete', EDIT_TRUNK , 'mwi', '','' );
						delete parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK].mwi ;
					if ( EDIT_TRUNK_luser && parent.sessionData.pbxinfo.users.hasOwnProperty(EDIT_TRUNK_luser) ){
						x.new_action('update', EDIT_TRUNK_luser , 'mailbox', EDIT_TRUNK_luser );
						parent.sessionData.pbxinfo.users[EDIT_TRUNK_luser].mailbox = EDIT_TRUNK_luser;
					}
				}

			}
		// End of remote MWI

		var after = function(){
			v.callActions( function(){
				parent.ASTGUI.dialog.hide() ;
				ASTGUI.feedback({ msg:'Saved Changes !!', showfor: 3 , color: 'green', bgcolor: '#FFFFFF' }) ;
				if( topreload ){
					top.window.location.reload();
				}else{
					window.location.reload();
				}
			});
		};

		x.callActions(after);
	}
};



var delete_trunk_confirm = function(ET){
	EDIT_TRUNK = ET ;
	if(!EDIT_TRUNK){return;}
	var ttype = parent.pbx.trunks.getType(EDIT_TRUNK);
	var tmp_name = parent.sessionData.pbxinfo.trunks[ttype][EDIT_TRUNK]['trunkname'] + ' -- ' + EDIT_TRUNK ;
	if(! confirm(' Delete VOIP trunk "' + tmp_name + '" ?' ) ){return;}
	parent.pbx.trunks.remove(EDIT_TRUNK);
	ASTGUI.feedback( { msg:'deleted VOIP Trunk ' + tmp_name, showfor: 5, color:'red', bgcolor:'#FFFFFF' } );
	window.location.reload();
};


var load_VOIPTrunksTable = function (){
	var addCell = ASTGUI.domActions.tr_addCell; // temporarily store the function
	(function(){ // add first row
		var newRow = DOM_table_VOIPTrunks_list.insertRow(-1);
		newRow.className = "frow";
		addCell( newRow , { html:'Provider Name'} );
		addCell( newRow , { html:'Type'} ); // sip/iax
		addCell( newRow , { html:'Hostname/IP'} ); // Hostname
		addCell( newRow , { html:'Username'} ); // Hostname
		addCell( newRow , { html: '' } );
	})();
	(function (){
		var c = [] ;
		var d = parent.pbx.trunks.list({sip: true, iax: true});
		d.each(function(item){
			var ttype = parent.pbx.trunks.getType(item);
			var newRow = DOM_table_VOIPTrunks_list.insertRow(-1);
			newRow.className = ((DOM_table_VOIPTrunks_list.rows.length)%2==1)?'odd':'even';

			addCell( newRow , { html: parent.sessionData.pbxinfo.trunks[ttype][item]['trunkname'] } );
			addCell( newRow , { html: ttype.toUpperCase() }); // sip/iax
			addCell( newRow , { html: parent.sessionData.pbxinfo.trunks[ttype][item]['host'] });
			addCell( newRow , { html: parent.sessionData.pbxinfo.trunks[ttype][item]['username'] || '' });

			var tmp_editString = "<span class='guiButton' onclick=\"edit_trunk_form('" + item +"')\">Edit</span>&nbsp;"
						+ "<span class='guiButtonDelete' onclick=\"delete_trunk_confirm('" + item +"')\">Delete</span>" ;
			addCell( newRow , { html: tmp_editString });
		});
	})();

	if( DOM_table_VOIPTrunks_list.rows.length == 1  ){
		ASTGUI.domActions.clear_table(DOM_table_VOIPTrunks_list);
		var newRow = DOM_table_VOIPTrunks_list.insertRow(-1);
		addCell( newRow , { html: "<BR><B>No SIP/IAX Trunks defined</B><BR><BR>" } );
	}
};



var localajaxinit = function(){
	top.document.title = 'Manage custom SIP/IAX Trunks ' ;
	(function (){ // populate TABS
		var t = [];
			t.push({url:'trunks_analog.html', desc:'Analog Trunks'});
		if( parent.sessionData.PLATFORM.isAA50 || parent.sessionData.PLATFORM.isABE ){
			t.push({url:'trunks_sps.html', desc:'Service Providers'});
		}
			t.push({url:'trunks_voip.html', desc:'VOIP Trunks', selected:true});
		if( !parent.sessionData.PLATFORM.isAA50 ){
			t.push({url:'trunks_digital.html', desc:'T1/E1/BRI Trunks'});
		}
		ASTGUI.tabbedOptions( _$('tabbedMenu') , t);

		var R = [];
		R.push( {optionText:'None', optionValue :'' } );
		for ( var r in parent.sessionData.listOfCodecs ){
			R.push( {optionText: parent.sessionData.listOfCodecs[r] , optionValue : r });
		}
		ASTGUI.selectbox.populateArray('codec_one', R);
		ASTGUI.selectbox.populateArray('codec_two', R);
		ASTGUI.selectbox.populateArray('codec_three', R);
		ASTGUI.selectbox.populateArray('codec_fourth', R);
		ASTGUI.selectbox.populateArray('codec_fifth', R);
	})(); // end of populate TABS
	(function(){
		var tmp = [] ;
		var destination = function(){
			this.optionText = '' ;
			this.optionValue = '' ;
		};

		var y = parent.pbx.users.list();
		y.each(function(user){
			var f = new destination;
			f.optionText = f.optionValue = user ;
			tmp.push(f);
		});
		
		ASTGUI.selectbox.populateArray( 'edit_ERMWI_luser', tmp );
		ASTGUI.domActions.showHideByCheckBox ( 'edit_ERMWI', 'editTrunk_Field_ermwi_row2' ) ;
	})();
	loadDOMelements();
	load_VOIPTrunksTable();
};
