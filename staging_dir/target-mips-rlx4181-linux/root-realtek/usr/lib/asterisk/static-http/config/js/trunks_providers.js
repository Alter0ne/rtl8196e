/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * trunks_providers.html functions
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
var PROVIDERS = {};
var EDIT_PROVIDER = '';

var localajaxinit = function(){
	DOM_table_SPS_list = _$('table_SPS_list');
	top.document.title = 'Service Providers' ;
	(function(){
		var t = [
			{url:'trunks_analog.html', desc:'Analog Trunks' } ,
			{url: '#', desc:'Service Providers', selected:true } ,
			{url:'trunks_voip.html', desc:'VOIP Trunks'}
		];
		if( !parent.sessionData.PLATFORM.isAA50 ){
			t.push({url:'trunks_digital.html', desc:'T1/E1/BRI Trunks'});
		}
		ASTGUI.tabbedOptions( _$('tabbedMenu') , t);
	})();

	(function(){
		var tmp_providers_conf = $.ajax({ url: ASTGUI.paths.rawman+'?action=getconfig&filename=providers.conf', async: false }).responseText;
		var tmp_providers_conf_lower = tmp_providers_conf.toLowerCase();
		if( tmp_providers_conf_lower.contains('response: error') && tmp_providers_conf_lower.contains('message: config file not found') ){
			ASTGUI.miscFunctions.createConfig( 'providers.conf', function(){ window.location.reload(); } ) ;
			return;
		}
		PROVIDERS = config2json({ configFile_output: tmp_providers_conf, usf : 1 });
	})();

	for (var this_provider in PROVIDERS ){ if(PROVIDERS.hasOwnProperty(this_provider) ){
		ASTGUI.selectbox.append('new_SP_Provider', PROVIDERS[this_provider].providername ||  this_provider , this_provider );
	}}

	$('#new_SP_Provider').click( function(){
		var selectedProvider = this.options[this.selectedIndex].value ;
		_$('TD_SP_desc').innerHTML = PROVIDERS[selectedProvider].providerdesc ;

		ASTGUI.domActions.removeAllChilds ('TD_SP_Logo_container');
		var logo_img  = document.createElement('IMG');
		$(logo_img).attr( 'src', PROVIDERS[selectedProvider].providerlogo );
		$(logo_img).attr('border', '0');
		_$('TD_SP_Logo_container').appendChild(logo_img) ;
	});

	providers_MiscFunctions.load_SPTrunksTable();
};




providers_MiscFunctions = {
	load_SPTrunksTable: function(){ // providers_MiscFunctions.load_SPTrunksTable();
		var addCell = ASTGUI.domActions.tr_addCell; // temporarily store the function

		(function(){ // add first row
			var newRow = DOM_table_SPS_list.insertRow(-1);
			newRow.className = "frow";
			addCell( newRow , { html:'', width:'15px'} );
			addCell( newRow , { html:'Trunk'} );
			addCell( newRow , { html:'Username'} );
			addCell( newRow , { html:'Hostname/IP'} ); // Hostname
			addCell( newRow , { html:''} );
		})();
	
		(function (){
			var d = parent.sessionData.pbxinfo.trunks.providers ;
			for(var e in d){
				if(d.hasOwnProperty(e)){
					var newRow = DOM_table_SPS_list.insertRow(-1);
					newRow.className = ((DOM_table_SPS_list.rows.length)%2==1)?'odd':'even';
					addCell( newRow , { html: ''} );
					addCell( newRow , { html: d[e]['trunkname'] });
					addCell( newRow , { html: getProperty(d[e], 'username') || '--' });
					addCell( newRow , { html: d[e]['host'] });
					tmp = "<span class='guiButton' onclick=\"providers_MiscFunctions.show_EditProviderForm('" + e +"')\">Edit</span>" + 
						"<span class='guiButtonDelete' onclick=\"providers_MiscFunctions.delete_ProviderForm('" + e +"')\">Delete</span>" ;
					addCell( newRow , { html: tmp} );
				}
			}
		})();

		if( DOM_table_SPS_list.rows.length == 1  ){
			ASTGUI.domActions.clear_table(DOM_table_SPS_list);
			var newRow = DOM_table_SPS_list.insertRow(-1);
			addCell( newRow , { html: "<BR><B>No Service Providers defined</B><BR><BR>" } );
		}
	},

	delete_ProviderForm : function(e){ // providers_MiscFunctions.delete_ProviderForm(e);
		if( !confirm('Are you sure you want to delete this Service Provider trunk ?') ){ return; }
		if( parent.pbx.trunks.remove(e) ){ window.location.reload(); };
	},
	
	updateProvider : function(){
		var provider_lines = ASTGUI.getFieldValue('edit_provider_details') ;
		provider_lines = provider_lines.split('\n');
		
		var x = new listOfActions('users.conf');

		x.new_action('delcat', EDIT_PROVIDER , '', '');
		x.new_action('newcat', EDIT_PROVIDER , '', '');
		
		provider_lines.each( function(this_line){
			x.new_action( 'append', EDIT_PROVIDER , this_line.beforeChar('=') , this_line.afterChar('=') );
		} );

		x.callActions( function(){
			ASTGUI.dialog.waitWhile('Updated Service Provider information <BR> Reloading GUI ... ');
			setTimeout( function(){ top.window.location.reload(); } , 2000 );
		});
	},

	show_EditProviderForm : function(e){ // providers_MiscFunctions.show_EditProviderForm();
		EDIT_PROVIDER = e ;
		_$('edit_provider_details').value = '';
		var t = config2json({filename:'users.conf', usf:0});
		if( t.hasOwnProperty(EDIT_PROVIDER) ){
			_$('edit_provider_details').rows = t[EDIT_PROVIDER].length + 1 ;
			_$('edit_provider_details').value = t[EDIT_PROVIDER].join('\n');
		}
		_$('edit_SP_DIV_Title').innerHTML = '[' + EDIT_PROVIDER  + ']' ;
		$('#edit_SPTrunk_DIV').showWithBg();
	},

	show_NewProviderForm : function(){ // providers_MiscFunctions.show_NewProviderForm();
		$('#new_SPTrunk_DIV').showWithBg();
	},

	createNewProvider : function(){ // providers_MiscFunctions.createNewProvider();
		if( _$('new_SP_Provider').selectedIndex == -1 ){
			alert('Please select a Provider');
			return;
		}

		var selectedProvider = _$('new_SP_Provider').value ;
		var trunk_template_obj = PROVIDERS[selectedProvider] ;

		if( trunk_template_obj.hasOwnProperty('trunk_username') ){
			var trunkname =  trunk_template_obj.trunk_username ;
			delete trunk_template_obj.trunk_username ;
		}else{
			var trunkname =  ASTGUI.getFieldValue('input_sp_uname') ;
		}

		if( !trunkname ){
			trunkname = parent.pbx.trunks.nextAvailTrunk() ;
		}
		var ct = ASTGUI.contexts.TrunkDIDPrefix + trunkname ;

		if(ASTGUI.getFieldValue('input_sp_uname')){
			trunk_template_obj.username = ASTGUI.getFieldValue('input_sp_uname') ;
		}
		if(ASTGUI.getFieldValue('input_sp_password')){
			trunk_template_obj.secret = ASTGUI.getFieldValue('input_sp_password') ;
		}

		if( trunk_template_obj.hasOwnProperty('providername') ){
			trunk_template_obj.provider = trunk_template_obj.providername.guiMetaData();
			delete trunk_template_obj.providername ;
		}else{
			trunk_template_obj.provider = selectedProvider ;
		}

		trunk_template_obj.trunkname = trunk_template_obj.providername + trunkname.guiMetaData();

		if( trunk_template_obj.hasOwnProperty('providerlogo') ) delete trunk_template_obj.providerlogo ;
		if( trunk_template_obj.hasOwnProperty('providerdesc') ) delete trunk_template_obj.providerdesc ;
		if( trunk_template_obj.hasOwnProperty('regurl') ) delete trunk_template_obj.regurl ;

		var x = new listOfActions('users.conf');
		x.new_action('delcat', trunkname , '', '');
		x.new_action('newcat', trunkname , '', '');
		for( var d in trunk_template_obj ){
			if( !trunk_template_obj.hasOwnProperty(d) ){ continue; }
			x.new_action( 'append', trunkname , d, trunk_template_obj[d] );
		}

		var cb = function(){
			var v = new listOfSynActions('extensions.conf') ;
			v.new_action('delcat', ct, '', '');
			v.new_action('newcat', ct, '', ''); // add context
			v.new_action('delcat', ct + ASTGUI.contexts.TrunkDefaultSuffix , '', '');
			v.new_action('newcat', ct + ASTGUI.contexts.TrunkDefaultSuffix , '', ''); // add context
			v.new_action('append', ct , 'include', ct + ASTGUI.contexts.TrunkDefaultSuffix );

			if( trunk_template_obj.hasOwnProperty('hassip') && trunk_template_obj.hassip == 'yes'){
				 v.new_action('update', 'globals', trunkname , 'SIP/' + trunkname );
			}

			if( trunk_template_obj.hasOwnProperty('hasiax') && trunk_template_obj.hasiax == 'yes'){
				 v.new_action('update', 'globals', trunkname , 'IAX2/' + trunkname);
			}

			var h = v.callActions();
			if( h.contains('Response: Success') ){
				ASTGUI.dialog.waitWhile('Added New VOIP trunk<BR> Reloading GUI ... ');
				setTimeout( function(){ top.window.location.reload(); } , 2000 );
			}else{
				// something failed ??
			}
		};

		x.callActions(cb);
	}
};
