/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * hardware_aa50.html functions
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
var oldLoadZone;
var hwcfgfile = ASTGUI.globals.hwcfgFile ;
var HAS_ANALOGHARDWARE = true; // if the user does not have any hardware - always set parent.sessionData.REQUIRE_RESTART to false
var isREVC = false;


var applySettings = {
	save_opermode_settings: function(){ // applySettings.save_opermode_settings();
		ASTGUI.dialog.waitWhile('saving...');
		var u = new listOfSynActions(ASTGUI.globals.configfile);
			u.new_action('update', 'general', 'opermode', ASTGUI.getFieldValue('opermode') );
			u.new_action('update', 'general', 'alawoverride', ASTGUI.getFieldValue('alawoverride') );
			u.new_action('update', 'general', 'fxshonormode', ASTGUI.getFieldValue('fxshonormode') );
			u.new_action('update', 'general', 'boostringer', ASTGUI.getFieldValue('boostringer') );
			u.callActions();
			u.clearActions();
	
		u.new_action('update', 'general', 'mwimode', ASTGUI.getFieldValue('mwimode') );
		if( ASTGUI.getFieldValue('mwimode') == 'NEON' ){
			u.new_action('update', 'general', 'neonmwi_level', ASTGUI.getFieldValue('neonmwi_level') );
			u.new_action('update', 'general', 'neonmwi_offlimit', ASTGUI.getFieldValue('neonmwi_offlimit') );
		}
		u.new_action('update', 'general', 'lowpower', ASTGUI.getFieldValue('lowpower') );
		u.new_action('update', 'general', 'fastringer', ASTGUI.getFieldValue('fastringer') );
		u.new_action('update', 'general', 'fwringdetect', ASTGUI.getFieldValue('fwringdetect') );
		u.callActions();
		u.clearActions();
	
		if( isREVC ){
			u.new_action('update', 'general', 'vpmnlptype', ASTGUI.getFieldValue('vpmnlptype') );
			u.new_action('update', 'general', 'vpmnlpthresh', ASTGUI.getFieldValue('vpmnlpthresh') );
			u.new_action('update', 'general', 'vpmnlpmaxsupp', ASTGUI.getFieldValue('vpmnlpmaxsupp') );
			u.callActions();
		}
	
		ASTGUI.dialog.waitWhile('updating modprobe.conf ...');
		var cmd1 = "cp /etc/asterisk/modprobe_default /etc/modprobe.conf";
		var params = "options sx00i ";
		var h = ASTGUI.getFieldValue('opermode') ;
			if(h){ params += " opermode=" + h; }
		h = ASTGUI.getFieldValue('alawoverride') ;
			if(h){ params += " alawoverride=" + h; }
		h = ASTGUI.getFieldValue('fxshonormode') ;
			if(h){ params += " fxshonormode=" + h; }
		h = ASTGUI.getFieldValue('boostringer') ;
			if(h){ params += " boostringer=" + h; }
		h = ASTGUI.getFieldValue('lowpower') ;
			if(h){ params += " lowpower=" + h; }
		h = ASTGUI.getFieldValue('fastringer') ;
			if(h){ params += " fastringer=" + h; }
		h = ASTGUI.getFieldValue('fwringdetect') ;
			if(h == '1'){ params += " fwringdetect=" + h; }
	
		if( ASTGUI.getFieldValue('mwimode') == 'NEON'){
			params += " neonmwi_monitor=1";
			var h = ASTGUI.getFieldValue('neonmwi_level');
			if(h){ params += ' neonmwi_level=' + h ; }
			var h = ASTGUI.getFieldValue('neonmwi_offlimit');
			if(h){ params += ' neonmwi_offlimit=' + h ; }
		}else{
			params += " neonmwi_monitor=0";
		}

		if( isREVC ){
			h = ASTGUI.getFieldValue('vpmnlptype') ;
				if(h){ params += " vpmnlptype=" + h; }
			h = ASTGUI.getFieldValue('vpmnlpthresh') ;
				if(h){ params += " vpmnlpthresh=" + h; }
			h = ASTGUI.getFieldValue('vpmnlpmaxsupp');
				if(h){ params += " vpmnlpmaxsupp=" + h; }
		}
	
		var cmd2 = "echo \"" + params + "\" >> /etc/modprobe.conf ";
	
		var update_usersConf = function(){
			// update MWI settings in users.conf
			var u = new listOfSynActions('users.conf');
			if( ASTGUI.getFieldValue('mwimode') == 'FSK'){
				u.new_action('update', 'general' , 'mwimonitor', 'fsk');
				u.new_action('update', 'general' , 'mwilevel', '512');
				u.new_action('update', 'general' , 'mwimonitornotify', '__builtin__');
			}
			if( ASTGUI.getFieldValue('mwimode') == 'NEON'){
				u.new_action('delete', 'general' , 'mwilevel', '','');
				u.new_action('update', 'general' , 'mwimonitor', 'neon');
				u.new_action('update', 'general' , 'mwimonitornotify', '__builtin__');
			}
			u.callActions();
			ASTGUI.dialog.hide();
			ASTGUI.feedback( { msg:"updated settings !!", showfor: 3 });
			alert('New settings will be applied on reboot!'); 
			window.location.reload();

		};
	
		ASTGUI.systemCmd( cmd1, function(){ 
			ASTGUI.systemCmd( cmd2, function(){ 
				ASTGUI.dialog.waitWhile('updating Analog Trunks with MWI settings ...');
				update_usersConf();
			});
		});
	},


	generate_zaptel: function(){
		parent.ASTGUI.systemCmd( top.sessionData.directories.script_generateZaptel + " applysettings" , function(){
			parent.sessionData.REQUIRE_RESTART = true ;
			parent.ASTGUI.systemCmd( "ztcfg -vv" , function(){
				applySettings.save_opermode_settings();
			});
		});
	},

	updateZaptel: function(){ // applySettings.updateZaptel();
		parent.ASTGUI.dialog.waitWhile('Saving Changes ...');
		var context = 'general';
		var x = new listOfActions('applyzap.conf');
		x.new_action('delcat', context,"", "");
		x.new_action('newcat', context, "", "");
		// write back any actual analog ports
		parent.sessionData.PORTS_SIGNALLING.ls = []; // reset previous signalling data
		parent.sessionData.PORTS_SIGNALLING.ks = [];

		if( parent.sessionData.FXO_PORTS_DETECTED.length){

			//x.new_action('append', context, 'fxsks', parent.sessionData.FXO_PORTS_DETECTED.join(',')); // FXO ports will be fxs signalled
			(function(){
				var ks_fxoPorts = [];
				var ls_fxoPorts = [];
				var t = parent.sessionData.FXO_PORTS_DETECTED ;
				for( var i = 0 ; i < t.length ; i++){
					if( _$('sig_analog_port_' + t[i] ).value == 'ls' ){
						ls_fxoPorts.push( t[i] );
					}else{
						ks_fxoPorts.push( t[i] );
					}
				}
				if( ls_fxoPorts.length ){
					x.new_action('append', context, 'fxsls', ls_fxoPorts.join(',')); // FXO ports will be fxs signalled
				}
				if( ks_fxoPorts.length ){
					x.new_action('append', context, 'fxsks', ks_fxoPorts.join(',')); // FXO ports will be fxs signalled
				}
				parent.sessionData.PORTS_SIGNALLING.ls = parent.sessionData.PORTS_SIGNALLING.ls.concat(ls_fxoPorts);
				parent.sessionData.PORTS_SIGNALLING.ks = parent.sessionData.PORTS_SIGNALLING.ks.concat(ks_fxoPorts);
			})();
		}

		if( parent.sessionData.FXS_PORTS_DETECTED.length){
			//x.new_action('append', context, 'fxoks', parent.sessionData.FXS_PORTS_DETECTED.join(',')); // FXS ports will be fxo signalled
			(function(){
				var ks_fxsPorts = [];
				var ls_fxsPorts = [];
				var t = parent.sessionData.FXS_PORTS_DETECTED ;
				for( var i = 0 ; i < t.length ; i++){
					if( _$('sig_analog_port_' + t[i] ).value == 'ls' ){
						ls_fxsPorts.push( t[i] );
					}else{
						ks_fxsPorts.push( t[i] );
					}
				}
				if( ls_fxsPorts.length ){
					x.new_action('append', context, 'fxols', ls_fxsPorts.join(',')); // FXS ports will be fxo signalled
				}
				if( ks_fxsPorts.length ){
					x.new_action('append', context, 'fxoks', ks_fxsPorts.join(',')); // FXS ports will be fxo signalled
				}
				parent.sessionData.PORTS_SIGNALLING.ls = parent.sessionData.PORTS_SIGNALLING.ls.concat(ls_fxsPorts);
				parent.sessionData.PORTS_SIGNALLING.ks = parent.sessionData.PORTS_SIGNALLING.ks.concat(ks_fxsPorts);
			})();
		}
		x.new_action('append', context, 'loadzone', _$('loadZone').value);
		x.new_action('append', context, 'defaultzone', _$('loadZone').value);
		x.callActions( applySettings.generate_zaptel );
	}
};



var localajaxinit = function(){
	ASTGUI.Log.Debug("Starting Loading Page hardware_aa50.html .. start function: window.onload()");
	ASTGUI.dialog.waitWhile('Detecting Hardware ...');
	top.document.title = "Analog Hardware Setup & Configuration";
	load_currentAnalogSettings();
	ASTGUI.Log.Debug("end of function: window.onload()");
};





var load_currentAnalogSettings = function(){
	if(!parent.sessionData.FXS_PORTS_DETECTED.length && !parent.sessionData.FXO_PORTS_DETECTED.length ){
		var newRow = _$('FXSFXO_ports_td').insertRow(-1) ;
		newRow.className = 'even' ;
		ASTGUI.domActions.tr_addCell( newRow , { html: 'No Analog Hardware detected !!' , align: 'center' } );
		HAS_ANALOGHARDWARE = false;
	}else{
		ASTGUI.dialog.waitWhile('loading current hardware settings ...');
		(function(){
			var tbl = _$('FXSFXO_ports_td');
			var addCell = ASTGUI.domActions.tr_addCell;
			var newRow = tbl.insertRow(-1);
			newRow.className = "frow";
			addCell( newRow , { html:'<B>Type</B>', align: 'center'});
			addCell( newRow , { html:'<B>Signalling</B>', align: 'center' });
			addCell( newRow , { html:'<B>User/Trunk</B>', align: 'center' });

			var h_2_orig = document.createElement('select');
			ASTGUI.selectbox.append( h_2_orig, 'Kewl Start', 'ks');
			ASTGUI.selectbox.append( h_2_orig, 'Loop Start', 'ls');
			h_2_orig.className = 'input8';

			parent.sessionData.FXS_PORTS_DETECTED.each( function( this_fxs_port ){
				var newRow = tbl.insertRow(-1);
				newRow.className = "even";
				var tmp_user_str = '<font color=red>unassigned</font>';
				var users = parent.sessionData.pbxinfo.users ;
				for( var this_user in users ){ 
					if( users.hasOwnProperty(this_user) ){
						if( users[this_user].getProperty('zapchan') == this_fxs_port ){
							tmp_user_str = this_user + ' ('+ users[this_user].getProperty('fullname') + ')';
							break;
						}
					}
				}
				addCell( newRow , { html: '<B>FXS Port </B>', align:'center' });
					var newcell = newRow.insertCell( newRow.cells.length );
					var h_3 = document.createElement('span'); h_3.innerHTML = 'Port ' + this_fxs_port + ' :&nbsp;' ;
					var h_2 = h_2_orig.cloneNode(true);
						h_2.id = 'sig_analog_port_' + this_fxs_port ;
						h_2.selectedIndex = ( ASTGUI.miscFunctions.ArrayContains(parent.sessionData.PORTS_SIGNALLING.ls, this_fxs_port ) ) ?  1 : 0 ;
				newcell.appendChild( h_3 );
				newcell.appendChild( h_2 );
				newcell.align = 'center';
				addCell( newRow , { html: tmp_user_str });

			} );


			parent.sessionData.FXO_PORTS_DETECTED.each( function( this_fxo_port ){
				var newRow = tbl.insertRow(-1);
				newRow.className = "odd";
				var tmp_user_str = '<font color=red>unassigned</font>';
				var atrunks = parent.sessionData.pbxinfo['trunks']['analog'] ;
				for( var this_trunk in atrunks ){
					if( atrunks.hasOwnProperty(this_trunk) ){
						var tmp = atrunks[this_trunk].getProperty('zapchan').split(',') ;
						if(  tmp.contains(this_fxo_port) ){
							tmp_user_str = atrunks[this_trunk].getProperty('trunkname') ;
							break;
						}
					}
				}

				addCell( newRow , { html: '<B>FXO Port </B>', align:'center' });

					var newcell = newRow.insertCell( newRow.cells.length );
					var h_3 = document.createElement('span'); h_3.innerHTML = 'Port ' + this_fxo_port + ' :&nbsp;' ;
					var h_2 = h_2_orig.cloneNode(true);
						h_2.id = 'sig_analog_port_' + this_fxo_port ;
						h_2.selectedIndex = ( ASTGUI.miscFunctions.ArrayContains(parent.sessionData.PORTS_SIGNALLING.ls, this_fxo_port ) ) ?  1 : 0 ;
				newcell.appendChild( h_3 );
				newcell.appendChild( h_2 );
				newcell.align = 'center';

				addCell( newRow , { html: tmp_user_str });
			});
		})();
	}

	(function(){ // load tonezone/loadzone
		// we parse zaptel.conf to get the loadzone
		var tmp_file = ASTGUI.globals.zaptelIncludeFile;
		var parseZaptelconf = function(zp){
			var t = zp['general'] ; // t is an array
			var line = '';
			_$('loadZone').selectedIndex = 0;
			for( var g=0; g < t.length; g++ ){
				line = t[g];
				try{
					if( line.beginsWith('loadzone=')) {
						ASTGUI.selectbox.selectOption( _$('loadZone'), line.lChop('loadzone=') );
						return;
					}
				}catch(err){
					_$('loadZone').selectedIndex = 0;
				}
			}
		};
		var s = $.ajax({ url: ASTGUI.paths.rawman+'?action=getconfig&filename=' + tmp_file , async: false }).responseText;
		if( s.contains('Response: Error') && s.contains('Message: Config file not found') ){
			ASTGUI.systemCmd( "touch /etc/asterisk/" + tmp_file, function(){
				var u = new listOfSynActions(tmp_file) ;
				u.new_action('delcat', 'general', "", ""); 
				u.new_action('newcat', 'general', "", ""); 
				u.new_action('update', 'general', '#include "../zaptel.conf" ;', ' ;');
				u.callActions();
				var q = config2json({filename:tmp_file, usf:0});
				parseZaptelconf(q);
			});
			return;
		}else{
			var q = config2json({ configFile_output:s , usf:0 });
			if( q.hasOwnProperty('general') ){
				parseZaptelconf(q);
			}
		}
		top.log.debug("end of function: loadConfigFiles.load_zaptel_conf()");
	})();

	(function(){ // load modprobe settings
		ASTGUI.selectbox.populateOptions ( 'vpmnlpthresh', 50) ;
		ASTGUI.selectbox.populateOptions ( 'vpmnlpmaxsupp', 50) ;
		ASTGUI.selectbox.insert_before('vpmnlpmaxsupp', '0', '0', 0) ;
	
		var c = context2json ({ filename: ASTGUI.globals.configfile , context: 'general', usf: 1 });
		ASTGUI.updateFieldToValue( 'opermode' , c.getProperty('opermode') );
		ASTGUI.updateFieldToValue( 'alawoverride' , c.getProperty('alawoverride') );
		ASTGUI.updateFieldToValue( 'fxshonormode' , c.getProperty('fxshonormode') );
		ASTGUI.updateFieldToValue( 'boostringer' , c.getProperty('boostringer') );
		ASTGUI.updateFieldToValue( 'lowpower' , c.getProperty('lowpower') );
		ASTGUI.updateFieldToValue( 'fastringer' , c.getProperty('fastringer') );
		ASTGUI.updateFieldToValue( 'fwringdetect' , c.getProperty('fwringdetect') );
		ASTGUI.updateFieldToValue( 'neonmwi_level' , c.getProperty('neonmwi_level') || '75' );
		ASTGUI.updateFieldToValue( 'neonmwi_offlimit' , c.getProperty('neonmwi_offlimit') || '16000' );
		ASTGUI.updateFieldToValue( 'mwimode' , c.getProperty('mwimode') );

		if(c.getProperty('mwimode') == 'NEON')$(".neon_settings").show();

		ASTGUI.events.add( _$('mwimode'), 'change' , function(){
			if( _$('mwimode').value == 'NEON'){
				$(".neon_settings").show();
			}else{
				$(".neon_settings").hide();
			}
		}) ;

		parent.ASTGUI.systemCmd( ' touch /etc/asterisk/applyzap.conf', function(){ // run ztscan and then try loading ztscan.conf
			setTimeout( function(){ 
				var DAHDI_version = "0";
				ASTGUI.systemCmdWithOutput( 'cat /sys/module/dahdi/version' , function(output){
					output = output.trim();
					if(output){
						DAHDI_version = output;
					}else{
						ASTGUI.systemCmdWithOutput( 'modinfo -F version dahdi' , function(output){
							output = output.trim();
							if(output){
								DAHDI_version = output;
							}
						});
					}
				});
				ASTGUI.systemCmdWithOutput( 'cat /proc/cmdline' , function(output){
					ASTGUI.dialog.hide();
					if( output.contains('boardrev=C') ){
						$('.hidevpm').show();
						isREVC = true;
						if(DAHDI_version.versionGreaterOrEqualTo("2.4.0") || parent.sessionData.PLATFORM.isAA50){
							ASTGUI.selectbox.append('vpmnlptype', "NLP Suppression", 4);
							ASTGUI.selectbox.append('vpmnlptype', "NLP AutoSuppression (default)", 6);
							ASTGUI.updateFieldToValue( 'vpmnlptype' , c.getProperty('vpmnlptype') || '6' );
							ASTGUI.updateFieldToValue( 'vpmnlpthresh' , c.getProperty('vpmnlpthresh') || '22' );
							ASTGUI.updateFieldToValue( 'vpmnlpmaxsupp' , c.getProperty('vpmnlpmaxsupp') || '10' );
						}else{
							ASTGUI.selectbox.append('vpmnlptype', "NLP Suppression (default)", 4);
							ASTGUI.updateFieldToValue( 'vpmnlptype' , c.getProperty('vpmnlptype') || '4' );
							ASTGUI.updateFieldToValue( 'vpmnlpthresh' , c.getProperty('vpmnlpthresh') || '24' );
							ASTGUI.updateFieldToValue( 'vpmnlpmaxsupp' , c.getProperty('vpmnlpmaxsupp') || '24' );
						}
					}
				});
			} , 700); 
		});
		
	})();

}; // End of load_currentAnalogSettings()
