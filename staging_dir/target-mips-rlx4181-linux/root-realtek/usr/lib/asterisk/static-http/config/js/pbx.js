/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * core parsing functions used in index.html
 *
 * Copyright (C) 2006-2011, Digium, Inc.
 *
 * Pari Nannapaneni <pari@digium.com>
 * Ryan Brindley <rbrindley@digium.com>
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

// this file is intended to be included in index.html only
// all the functions listed here usually updates the sessionData object in index.html
// if you are including this file in an iframe page - you are probably doing something wrong

readcfg = {	// place where we tell the framework how and what to parse/read from each config file
		// each function would read a config file and updates the sessionData.pbxinfo
	checkEssentials: function(){ //readcfg.checkEssentials();
		// check whether the config files are in proper format and have every thing needed for the gui to function properly
				var check_For_Contexts = {
					general : { static : 'yes', writeprotect : 'no', clearglobalvars : 'yes' },
					globals : { FEATURES : '' , DIALOPTIONS : '' , RINGTIME: '20', FOLLOWMEOPTIONS : '', PAGING_HEADER : 'Intercom' },
					'default' : {},
					'macro-stdexten' : [
						'exten=s,1,Set(__DYNAMIC_FEATURES=${FEATURES})',
						'exten=s,2,Set(ORIG_ARG1=${ARG1})',
						'exten=s,3,GotoIf($["${FOLLOWME_${ARG1}}" = "1"]?6:4)',
						'exten=s,4,Dial(${ARG2},${RINGTIME},${DIALOPTIONS})',
						'exten=s,5,Goto(s-${DIALSTATUS},1)',
						'exten=s,6,Macro(stdexten-followme,${ARG1},${ARG2})',
						'exten=s-NOANSWER,1,Voicemail(${ORIG_ARG1},u)',
						'exten=s-NOANSWER,2,Goto(default,s,1)',
						'exten=s-BUSY,1,Voicemail(${ORIG_ARG1},b)',
						'exten=s-BUSY,2,Goto(default,s,1)',
						'exten=_s-.,1,Goto(s-NOANSWER,1)',
						'exten=a,1,VoicemailMain(${ORIG_ARG1})'
					],
					'macro-stdexten-followme' : [
						'exten=s,1,Answer',
						'exten=s,2,Set(ORIG_ARG1=${ARG1})',
						'exten=s,3,Dial(${ARG2},${RINGTIME},${DIALOPTIONS})',
						'exten=s,4,Set(__FMCIDNUM=${CALLERID(num)})',
						'exten=s,5,Set(__FMCIDNAME=${CALLERID(name)})',
						'exten=s,6,Followme(${ORIG_ARG1},${FOLLOWMEOPTIONS})',
						'exten=s,7,Voicemail(${ORIG_ARG1},u)',
						'exten=s-NOANSWER,1,Voicemail(${ORIG_ARG1},u)',
						'exten=s-BUSY,1,Voicemail(${ORIG_ARG1},b)',
						'exten=s-BUSY,2,Goto(default,s,1)',
						'exten=_s-.,1,Goto(s-NOANSWER,1)',
						'exten=a,1,VoicemailMain(${ORIG_ARG1})'
					],
					'macro-pagingintercom' : [
						'exten=s,1,SIPAddHeader(Alert-Info: ${PAGING_HEADER})',
						'exten=s,2,Page(${ARG1},${ARG2})',
						'exten=s,3,Hangup'
					]
				};
		
				check_For_Contexts[ASTGUI.contexts.CONFERENCES] = {} ;
				check_For_Contexts[ASTGUI.contexts.RingGroupExtensions] = {} ;
				check_For_Contexts[ASTGUI.contexts.QUEUES] = {} ;
				check_For_Contexts[ASTGUI.contexts.VoiceMenuExtensions] = {} ;
				check_For_Contexts[ASTGUI.contexts.VoiceMailGroups] = {} ;
				check_For_Contexts[ASTGUI.contexts.Directory] = {} ;
				check_For_Contexts[ASTGUI.contexts.PageAnExtension] = {},
				check_For_Contexts[ASTGUI.contexts.PageGroups] = {} ;
				check_For_Contexts[ASTGUI.contexts.guitools] = [
					'exten=executecommand,1,System(${command})',
					'exten=executecommand,n,Hangup()',
					'exten=record_vmenu,1,Answer',
					'exten=record_vmenu,n,Playback(vm-intro)',
					'exten=record_vmenu,n,Record(${var1})',
					'exten=record_vmenu,n,Playback(vm-saved)',
					'exten=record_vmenu,n,Playback(vm-goodbye)',
					'exten=record_vmenu,n,Hangup',
					'exten=play_file,1,Answer',
					'exten=play_file,n,Playback(${var1})',
					'exten=play_file,n,Hangup'
				];
				if (ASTGUI.version.gteq("1.6.0")) {
					check_For_Contexts[ASTGUI.contexts.guitools][4] = 'exten=record_vmenu,n,Record(${var1},0,500,k)';
				}
		
				check_For_Contexts[ 'macro-' + ASTGUI.contexts.localcrcid ] = [
					'exten=s,1,Set(CALLERID(all)=${IF($[${LEN(${ARG4})} > 2]?${ARG4}:)})',
					'exten=s,n,Goto(${ARG1},${ARG2},${ARG3})'
				];
				check_For_Contexts[ 'macro-' + ASTGUI.contexts.dialtrunks ] = [
					// "; Macro by =  Brandon Kruse, Matthew O'Gorman, & Erin Spiceland <espiceland@digium.com>",
					'exten=s,1,GotoIf($[${LEN(${FMCIDNUM})} > 6]?1-fmsetcid,1)',
					'exten=s,n,GotoIf($[${LEN(${GLOBAL_OUTBOUNDCIDNAME})} > 1]?1-setgbobname,1)',
					'exten=s,n,Set(CALLERID(num)=${IF($[${LEN(${CID_${CALLERID(num)}})} > 2]?${CID_${CALLERID(num)}}:)})',
					'exten=s,n,Set(CALLERID(all)=${IF($[${LEN(${ARG5})} > 2]?${ARG5}:)})',
					'exten=s,n,GotoIf($[${LEN(${CALLERID(num)})} > 6]?1-dial,1)',
					'exten=s,n,Set(CALLERID(all)=${IF($[${LEN(${CID_${ARG3}})} > 6]?${CID_${ARG3}}:${GLOBAL_OUTBOUNDCID})})',
					'exten=s,n,Set(CALLERID(all)=${IF($[${LEN(${ARG5})} > 2]?${ARG5}:)})',
					'exten=s,n,Goto(1-dial,1)',
					'exten=1-setgbobname,1,Set(CALLERID(name)=${GLOBAL_OUTBOUNDCIDNAME})',
					'exten=1-setgbobname,n,Goto(s,3)',
					'exten=1-fmsetcid,1,Set(CALLERID(num)=${FMCIDNUM})',
					'exten=1-fmsetcid,n,Set(CALLERID(name)=${FMCIDNAME})',
					'exten=1-fmsetcid,n,Goto(s,4)',
					'exten=1-dial,1,Dial(${ARG1})',
					'exten=1-dial,n,Gotoif(${LEN(${ARG2})} > 0 ?1-${DIALSTATUS},1:1-out,1)',
					'exten=1-CHANUNAVAIL,1,Dial(${ARG2})',
					'exten=1-CHANUNAVAIL,n,Hangup()',
					'exten=1-CONGESTION,1,Dial(${ARG2})',
					'exten=1-CONGESTION,n,Hangup()',
					'exten=1-out,1,Hangup()'
				];

		ASTGUI.dialog.waitWhile('Verifying Dialplan Contexts needed for GUI');
		var EXN_CNF = config2json({ filename:'extensions.conf', usf:0 });

		var UPDATES = new listOfActions('extensions.conf');
		var tmp_createdNewCat ;
		for( var contextName in check_For_Contexts ){
			if( !check_For_Contexts.hasOwnProperty(contextName) ){ continue; }
			//contextName
			tmp_createdNewCat = false;
			if( !EXN_CNF.hasOwnProperty(contextName) ){
				UPDATES.new_action( 'newcat', contextName , '', '' );
				EXN_CNF[contextName] = [];
				tmp_createdNewCat = true;
			}

			if( ASTGUI.isArray( check_For_Contexts[contextName] )){ // compare whether the context matches the exact context
				if( check_For_Contexts[contextName].join('::') != EXN_CNF[contextName].join('::') ){
					if( tmp_createdNewCat == false ){
						UPDATES.new_action( 'delcat', contextName , '', '');
						UPDATES.new_action( 'newcat', contextName , '', '');
					}

					var tmp_context_array = ASTGUI.cloneObject(check_For_Contexts[contextName]);
					while( tmp_context_array.length ){
						UPDATES.new_action( 'append', contextName , tmp_context_array[0].beforeChar('=') , tmp_context_array[0].afterChar('=') );
						tmp_context_array.splice(0,1);
					}
				}
			}else{ // see if the context has all the properties 
				var tmp_context_obj = check_For_Contexts[contextName];
				for( var this_prop in tmp_context_obj ){
					if( !tmp_context_obj.hasOwnProperty(this_prop) ){ continue; }

					var tmp_string_index = EXN_CNF[contextName].indexOfLike(this_prop) ;
					if( tmp_string_index != -1 ){
						// if( EXN_CNF[contextName][tmp_string_index] == this_prop + '=' + tmp_context_obj[this_prop] ){ // do nothing
						 	continue;
						// }
						// UPDATES.new_action( 'delete', contextName , this_prop, '' ,tmp_context_obj[this_prop] );
					}
					UPDATES.new_action( 'append', contextName , this_prop, tmp_context_obj[this_prop] );
				}
			}
		}

		if( EXN_CNF.hasOwnProperty('default') && EXN_CNF['default'].contains('include=demo') ){
			UPDATES.new_action('delete', 'default', 'include', '', 'demo');
			UPDATES.new_action('append', 'default', ';include', 'demo ; This line was commented by ASTERISK GUI');
		}

		if( UPDATES.current_batch == 1 && UPDATES.current_batch_actionnumber == 0 ){ // no updates to extensions.conf

		}else{
			ASTGUI.dialog.waitWhile('Updating Extensions.conf ');
			sessionData.continueParsing = false ;
			UPDATES.callActions( function(){
				var t = ASTGUI.cliCommand('dialplan reload') ;
				setTimeout( function(){
					if( sessionData.DEBUG_MODE ){
						alert('updated extensions.conf - from readcfg.checkEssentials()::check_For_Contexts \n Click OK to Reload');
					}
					window.location.reload(); 
				} , 500 );
			} );
			return ;
		}

		(function(){
			var runScript_detectdahdi = function(){
				ASTGUI.systemCmd( sessionData.directories.script_detectdahdi , function(){
					setTimeout( function(){
						if( sessionData.DEBUG_MODE ){
							alert(ASTGUI.globals.dahdiIncludeFile + ' not found, ran script ' + sessionData.directories.script_detectdahdi + ' \n Click OK to Reload');
						}
						window.location.reload();
					} , 500 );
				});
			};

			var s = $.ajax({ url: ASTGUI.paths.rawman+'?action=getconfig&filename=' + ASTGUI.globals.dahdiIncludeFile , async: false }).responseText;

			var q = context2json({ configFile_output:s, context: 'general', usf:0 });
			if( q === null ){ // if context 'general' is not found
				ASTGUI.systemCmd( sessionData.directories.script_detectdahdi , function(){
					sessionData.continueParsing = false ;
					runScript_detectdahdi();
					return;
				});
			}

			if( s.contains('Response: Error') && s.contains('Message: Config file not found') ){
				sessionData.continueParsing = false ;
				ASTGUI.systemCmd( sessionData.directories.app_dahdi_genconf , function(){
					if( sessionData.DEBUG_MODE ){
						alert( 'Error reading ' + ASTGUI.globals.dahdiIncludeFile + ': ' + s + 'Ran ' + sessionData.directories.app_dahdi_genconf + '. Click OK to Reload' );
					}
					window.location.reload();
				});
				return;
			}else{
				var has_echo_can = false;
				q.each(function(line){
					if ( !line.beginsWith('fx') ){ return ;}
					if( line.beginsWith('fxoks=') || line.beginsWith('fxsks=') ){
						var ksports = ASTGUI.miscFunctions.chanStringToArray( line.afterChar('=') );
						sessionData.PORTS_SIGNALLING.ks = sessionData.PORTS_SIGNALLING.ks.concat(ksports);
					}
					if( line.beginsWith('fxols=') || line.beginsWith('fxsls=') ){
						var lsports = ASTGUI.miscFunctions.chanStringToArray( line.afterChar('=') );
						sessionData.PORTS_SIGNALLING.ls = sessionData.PORTS_SIGNALLING.ls.concat(lsports);
					}

					if (line.beginsWith('echocanceller=')) {
						has_echo_can = true;
					}
				});

				/* write echocanceller if it doesn't exist */
				if (!has_echo_can && sessionData.PLATFORM.isABE) {
					/* applyzap.conf might not exist yet */
					ASTGUI.miscFunctions.createConfig('applyzap.conf', function(){});

					/* setup applyzap's basics */
					var apply_zap = listOfSynActions('applyzap.conf');
					apply_zap.new_action('delcat', 'general', '', '');
					apply_zap.new_action('newcat', 'general', '', '');

					/* keep all info from the current config */
					q.each(function(line) {
						apply_zap.new_action('append', 'general', line.split('=')[0], line.split('=')[1]);
					});

					/* add the echocanceller line */
					apply_zap.new_action('append', 'general', 'echocanceller', 'mg2,1-240');

					/* send changes to asterisk */
					var result = apply_zap.callActions();
					if (result.contains('Response: Error')) {
						top.log.error('Error trying to updating applyzap.conf with echocanceller info.');
						top.log.error(result);
					} else {
						/* copy applyzap.conf to /etc/dahdi/system.conf */
						ASTGUI.systemCmd(sessionData.directories.script_generateZaptel + ' applysettings', function() {
								ASTGUI.dialog.alertmsg('Changes to your hardware configs has been made. <BR> Your hardware might not work properly until you reboot!');
						});
					}
				}
			}
		})();

		sessionData.continueParsing = true ;
	}, // end of readcfg.checkEssentials();

	guiPreferencesConf: function(){ //readcfg.guiPreferencesConf();
		top.log.ajax("AJAX Request : reading '" +  ASTGUI.globals.configfile + "'");
		var s = $.ajax({ url: ASTGUI.paths.rawman+'?action=getconfig&filename=' + ASTGUI.globals.configfile , async: false }).responseText;
		if( s.contains('Response: Error') && s.contains('Message: Config file not found') ){
			sessionData.continueParsing = false ;
			ASTGUI.dialog.waitWhile(" Creating a config file to store GUI Preferences");
			ASTGUI.miscFunctions.createConfig( ASTGUI.globals.configfile, function(){
				if( sessionData.DEBUG_MODE ){
					alert('created config file ' + ASTGUI.globals.configfile + '\n' + 'Click OK to reload');
				}
				window.location.reload();
			});
			return;
		}

		var gp = config2json({ configFile_output: s , usf:1 });
		if( !gp.hasOwnProperty('general') ){
			var u = new listOfSynActions(ASTGUI.globals.configfile) ;
			u.new_action ('newcat', 'general', '', '');
			u.callActions ();
			u.clearActions ();
			u.new_action('append', 'general', 'ue_start', '6000');
			u.new_action('append', 'general', 'ue_end', '6299');
			u.new_action('append', 'general', 'mm_start', '6300');
			u.new_action('append', 'general', 'mm_end', '6399');
			u.new_action('append', 'general', 'vme_start', '7000');
			u.new_action('append', 'general', 'vme_end', '7100');
			u.callActions ();
			u.clearActions ();
			u.new_action('append', 'general', 'rge_start', '6400');
			u.new_action('append', 'general', 'rge_end', '6499');
			u.new_action('append', 'general', 'qe_start', '6500');
			u.new_action('append', 'general', 'qe_end', '6599');
			u.new_action('append', 'general', 'vmg_start', '6600');
			u.new_action('append', 'general', 'vmg_end', '6699');
			u.callActions ();
			sessionData.GUI_PREFERENCES = new ASTGUI.customObject ;
			sessionData.GUI_PREFERENCES.updateProperties({ ue_start : '6000', ue_end : '6299', mm_start : '6300', mm_end : '6399', vme_start : '7000', vme_end : '7100', rge_start : '6400' , rge_end : '6499' , qe_start : '6500' , qe_end : '6599', vmg_start : '6600' , vmg_end : '6699' });
		}else{
			sessionData.GUI_PREFERENCES = gp['general'] ;
		}
		if(!sessionData.GUI_PREFERENCES.getProperty('disable_extension_ranges')){
			var u = new listOfSynActions(ASTGUI.globals.configfile) ;
			u.new_action('append', 'general', 'disable_extension_ranges', 'no');
			u.callActions();
		}
		sessionData.continueParsing = true ;
	}, // end of readcfg.guiPreferencesConf();

	ExtensionsConf: function(){ //readcfg.ExtensionsConf();
		pbx.queues.agents.setup();
		var c = config2json({filename:'extensions.conf', usf:0});
		//	look for numberplans, voicemenus, DID_trunk_x (incoming rules), timebasedrules, ringgroups, 
		//	and store the information in the following objects
		//		==> sessionData.pbxinfo.dialplans
		//		==> sessionData.pbxinfo.voicemenus
		//		==> sessionData.pbxinfo.incomingrules
		//		==> sessionData.pbxinfo.timebasedrules
		//		==> sessionData.pbxinfo.ringgroups
		//
		//	look for localextensions, Conferences, Queues under context [default] and store information in 
		//		==> sessionData.pbxinfo.localextensions
		//		==> sessionData.pbxinfo.conferences
		//		==> sessionData.pbxinfo.queues
		sessionData.pbxinfo['voicemenus'] = new ASTGUI.customObject; // reset all voicemenus data
		sessionData.pbxinfo['callingPlans'] = new ASTGUI.customObject; // reset all dialplan data
		sessionData.pbxinfo['callingRules'] = new ASTGUI.customObject; // reset all dialplan data
//		sessionData.pbxinfo['incomingrules'] = new ASTGUI.customObject; // reset all incoming rules data
		sessionData.pbxinfo['localextensions'] = new ASTGUI.customObject;
		sessionData.pbxinfo['conferences'] = new ASTGUI.customObject;
		sessionData.pbxinfo['vmgroups'] = new ASTGUI.customObject;
		sessionData.pbxinfo['queues'] = new ASTGUI.customObject;
		sessionData.pbxinfo['GLOBALS'] = new ASTGUI.customObject; // store extensions.conf --> [globals]
		sessionData.pbxinfo['ringgroups'] = new ASTGUI.customObject;
		sessionData.pbxinfo['timebasedRules'] = new ASTGUI.customObject;
		sessionData.pbxinfo['pagegroups'] = [];

		for(var d in c){ if(c.hasOwnProperty(d)) {
			// note: remember c[d] is an Array and is not an Object (we used usf:0)
			if( d == 'default' ){ 	// look for conferences, queues, and localextensions (voicemail, operator etc)
				astgui_parseDefaultContext( c[d] );
				continue;
			}
			if( d == ASTGUI.contexts.CONFERENCES ){
				continue;
			}
			if( d == ASTGUI.contexts.QUEUES ){
				continue;
			}
			if( d == ASTGUI.contexts.VoiceMenuExtensions ){
				continue;
			}
			if ( d == ASTGUI.contexts.VoiceMailGroups ){
				astgui_manageVMgroups.parseContext(c[d]);
			}
			if ( d == ASTGUI.contexts.PageGroups ){
				sessionData.pbxinfo['pagegroups'] = c[d] ;
			}
			if( d == 'globals' ){ // look for outboundcid of all users and globaloutboungcid
				astgui_parseGlobalContext( c[d] );
				continue;
			}
			if( d == ASTGUI.contexts.Directory){
				(function(){
					for ( var ci = 0 ; ci < c[d].length ; ci++ ){
						if( c[d][ci].toLowerCase().contains('directory(') &&   c[d][ci].toLowerCase().contains('default') ){
							sessionData.pbxinfo['localextensions']['defaultDirectory'] = ASTGUI.parseContextLine.getExten( c[d][ci] );
							break;
						}
					}
				})();
				continue;
			}
			if( d.beginsWith(ASTGUI.contexts.VoiceMenuPrefix) ){ // if context is a voicemenu 
				sessionData.pbxinfo['voicemenus'][d] = new ASTGUI.customObject; // create new object for this voicemenu
				sessionData.pbxinfo['voicemenus'][d] = pbx.voice_menus.parse(c[d]);
				continue;
			}

			if( d.beginsWith( ASTGUI.contexts.RingGroupPrefix ) ){ // if context is a ringGroup
				sessionData.pbxinfo['ringgroups'][d] = new ASTGUI.customObject;
				sessionData.pbxinfo['ringgroups'][d] = astgui_manageRingGroups.parseContext(d, c[d], c[ASTGUI.contexts.RingGroupExtensions]);  // create new object for this ringgroup
				continue;
			}

			if( d.beginsWith( ASTGUI.contexts.CallingPlanPrefix ) ){ // if context is a callplan/dialplan == set of calling rules
				sessionData.pbxinfo['callingPlans'][d] = new ASTGUI.customObject;
				sessionData.pbxinfo['callingPlans'][d] = astgui_manageCallPlans.parseContext( c[d] );
				continue;
			}

			if( d.beginsWith( ASTGUI.contexts.CallingRulePrefix ) ){ // if context is a calling Rule
				sessionData.pbxinfo['callingRules'][d] = new ASTGUI.customObject; // create new object for this dialplan
				// sessionData.pbxinfo['callingRules'][d] = astgui_manageCallingRules.parseContext( c[d] );
				sessionData.pbxinfo['callingRules'][d] = c[d] ;
				continue;
			}

// 			if( d.beginsWith(ASTGUI.contexts.TrunkDIDPrefix) && !d.contains( '_' + ASTGUI.contexts.TimeIntervalPrefix ) ){ // if is a incoming rules context 
// 				sessionData.pbxinfo['incomingrules'][d] = new ASTGUI.customObject; // create new object for this dialplan
// 				sessionData.pbxinfo['incomingrules'][d] = astgui_manageIncomingRules.parseContext(d, c[d] );
// 				continue;
// 			}

			if(d.beginsWith(ASTGUI.contexts.TimeBasedRulePrefix) ){
				continue ; // depricated in favor of time frames
				// sessionData.pbxinfo['timebasedRules'][d] = new ASTGUI.customObject; // create new object for this time based rule
				// sessionData.pbxinfo['timebasedRules'][d] = astgui_manageTimeBasedRules.parseContext( d , c[d] );
			}
		}}
		top.log.debug('parsed all contexts in extensions.conf');
		(function(){ // parse voicemenu extensions
			var vm = c[ASTGUI.contexts.VoiceMenuExtensions] ;
			var vm_default = c['default'] ; // Backward compatibility for old GUI
			vm = vm.concat(vm_default); // old gui used to create voiceMenu Extensions in [default] as opposed to in [voicemenus]
			vm.each( function(line){
				if( line.contains('Goto') && line.contains(ASTGUI.contexts.VoiceMenuPrefix) ){
					//var this_exten = ASTGUI.parseContextLine.getExten(line);
					var this_menuName = ASTGUI.parseContextLine.getArgs(line)[0];
					if( sessionData.pbxinfo['voicemenus'].hasOwnProperty(this_menuName) ){
						sessionData.pbxinfo['voicemenus'][this_menuName].alias_exten = line ;
					}
				}
			});
		})();
		(function(){// backward compatibility for old Meetmes
			// [default]  ---   exten => 6000,1,MeetMe(${EXTEN}|MI) 
			var df = c['default'] ;
			df.each( function(line){
				if( line.contains( ',1,MeetMe(${EXTEN}' ) ){
					var mm_ext = ASTGUI.parseContextLine.getExten(line) ;
					var tmp = new ASTGUI.customObject; 
						tmp.configOptions = line.afterChar('=');
						tmp.pwdString = '' ;
						tmp.adminOptions = '' ;
					if( !sessionData.pbxinfo.conferences.hasOwnProperty(mm_ext) ){
						sessionData.pbxinfo.conferences[mm_ext] = tmp ;
					}
				}
			} );
		})();
		(function(){// backward compatibility for old Queues
			// [default]  ---   exten = 6001,1,Queue(${EXTEN})
			var df = c['default'] ;
			df.each( function(line){
				if( line.contains( ',1,Queue(${EXTEN})') || line.contains(',1,agentlogin(') || line.contains(',1,agentcallbacklogin(') ){
					var q_ext = ASTGUI.parseContextLine.getExten(line) ;
					var tmp = new ASTGUI.customObject; 
						tmp.configLine = line.afterChar('=');
						tmp.isOLDGUI = true ;
					if( !sessionData.pbxinfo.queues.hasOwnProperty(q_ext) ){
						sessionData.pbxinfo.queues[q_ext] = tmp ;
					}
				}
			} );
		})();

		(function(){// backward compatibility for old RingGroups
			// [default]  ---   exten = 6010,1,Goto(ringroups-custom-1|s|1)
			var df = c['default'] ;
			df.each( function(line){
				if( line.contains( ',1,Goto(ringroups-custom-' ) ){
					var rg_ext = ASTGUI.parseContextLine.getExten(line) ;
					var rg_name = ASTGUI.parseContextLine.getArgs(line)[0] ; //ringroups-custom-1
					if( !sessionData.pbxinfo.ringgroups.hasOwnProperty(rg_name) ){
						var tmp = {
							NAME : rg_name,
							strategy : '',
							members : [],
							extension : rg_ext ,
							ringtime : '',
							fallback : '',
							isOLDRG : true
						};
						sessionData.pbxinfo['ringgroups'][rg_name] = ASTGUI.toCustomObject(tmp);
					}else{
						sessionData.pbxinfo['ringgroups'][rg_name].extension = rg_ext;
						sessionData.pbxinfo['ringgroups'][rg_name].isOLDRG = true;
					}
				}
			} );
		})();
	}, // end of readcfg.ExtensionsConf();

	httpConf: function(){ // readcfg.httpConf();
		var c = config2json({filename:'http.conf', usf:1});
		if( c.hasOwnProperty('general') && c['general'].hasOwnProperty('prefix') ) {	
			sessionData.httpConf.prefix = c['general']['prefix'] ;
		}
		if( c.hasOwnProperty('post_mappings') ) {
			sessionData.httpConf.postmappings_defined = true ;
			var e = c['post_mappings'];
			for(var d in e){ if(e.hasOwnProperty(d)) {
				sessionData.httpConf.uploadPaths[d] = e[d] ;
			}}
		}
	},

	UsersConf: function(){ // readcfg.UsersConf();
		var c = config2json({filename:'users.conf', usf:1});
		// we should actually be using usf:0 to handle cases like 'allow', 'disallow' defined in multiple lines
		// but for the time being we will continue with the assumption that they are all defined in one line
		
		var users_conf = listOfSynActions('users.conf');

		sessionData.pbxinfo['users'] = new ASTGUI.customObject ; // reset all users info
		if(!sessionData.pbxinfo['trunks']){ sessionData.pbxinfo['trunks'] = new ASTGUI.customObject ; }
		sessionData.pbxinfo['trunks']['analog'] = new ASTGUI.customObject ;
		sessionData.pbxinfo['trunks']['sip'] = new ASTGUI.customObject;
		sessionData.pbxinfo['trunks']['iax'] = new ASTGUI.customObject;
		sessionData.pbxinfo['trunks']['pri'] = new ASTGUI.customObject;
		sessionData.pbxinfo['trunks']['bri'] = new ASTGUI.customObject;
		sessionData.pbxinfo['trunks']['providers'] = new ASTGUI.customObject;

		for(var d in c){ if(c.hasOwnProperty(d)) {
			if(d=='general'){ continue; }

			if( c[d].hasOwnProperty('provider') ){
				// if is a service provider (will be handled by providers.js from trunks_sps.html )
				sessionData.pbxinfo.trunks.providers[d] = new ASTGUI.customObject ;
				sessionData.pbxinfo['trunks']['providers'][d] = c[d];
				continue ;
			}

			if( c[d].hasOwnProperty('hasexten') && c[d]['hasexten'] == 'no' && !d.beginsWith('span_') ){ // if context is a trunk - could be a analog, iax, sip
				if( c[d].hasOwnProperty('hasiax') && c[d]['hasiax'].isAstTrue() ){ // if the context is for an iax trunk
					sessionData.pbxinfo['trunks']['iax'][d] = c[d] ;
					continue;
				}
				if( c[d].hasOwnProperty('hassip') && c[d]['hassip'].isAstTrue() ){ // if the context is for an sip trunk
					sessionData.pbxinfo['trunks']['sip'][d] = c[d];
					continue;
				}
//				if( c[d]['zapchan'] && (!c[d]['hasiax'] || c[d]['hasiax'] =='no') && (!c[d]['hassip'] || c[d]['hassip'] =='no')){
				if( c[d].hasOwnProperty('zapchan') || c[d].hasOwnProperty('dahdichan') ){
					if (c[d].hasOwnProperty('zapchan') && sessionData.PLATFORM.isABE) {
						users_conf.new_action('delete', d, 'zapchan', '');
						users_conf.new_action('append', d, 'dahdichan', c[d]['zapchan']);
					}
					// if is an analog trunk - note that here we are NOT expecting a 'FXO channel(FXS signalled) on a T1/E1'
					// we assume that all the ports in zapchan are actual analog FXO ports
					// a trunk for T1/E1 analog channels would begin with 'span_'
					sessionData.pbxinfo['trunks']['analog'][d] = c[d];
					continue;
				}
			}

			if( d.beginsWith('span_') && ( c[d].hasOwnProperty('zapchan') || c[d].hasOwnProperty('dahdichan') ) ){
				sessionData.pbxinfo['trunks']['pri'][d] = c[d];
				continue;
			}

			if( c[d].hasOwnProperty('context') && c[d]['context'].beginsWith(ASTGUI.contexts.TrunkDIDPrefix)  ){
				// some providers like voicepulse requires the trunk context to be named as 'voicepulse'
				// which might not be in the trunk_x format
				// so check whether context beings with 'DID_' and check for whether it is an iax/sip trunk
				if(c[d].hasOwnProperty('hassip') && c[d]['hassip'] == 'yes' ){
					sessionData.pbxinfo['trunks']['sip'][d] = c[d];
					continue;
				}
				if(c[d].hasOwnProperty('hasiax') && c[d]['hasiax'] == 'yes' ){
					sessionData.pbxinfo['trunks']['iax'][d] = c[d];
					continue;
				}
			}


			//if( !d.beginsWith('span_') && !d.beginsWith('trunk_') && c[d].hasOwnProperty('context') && c[d]['context'].beginsWith(ASTGUI.contexts.CallingPlanPrefix) ){ // if is none of above we can assume it is a user context
			// TODO : we can also base the logic on 'hasexten' , cause all trunks have hasexten='yes' and users don't
			if( !d.beginsWith('span_') && !d.beginsWith('trunk_') ){ // if is none of above we can assume it is a user context
				if (c[d].hasOwnProperty('zapchan') && sessionData.PLATFORM.isABE) {
					users_conf.new_action('delete', d, 'zapchan', '');
					users_conf.new_action('append', d, 'dahdichan', c[d]['zapchan']);
					c[d]['dahdichan'] = c[d]['zapchan'];
				}
				if (!c[d].hasOwnProperty('type')) {
					users_conf.new_action('append', d, 'type', 'peer');
					c[d]['type'] = 'peer';
				}
				if (!c[d].hasOwnProperty('callcounter')) {
					users_conf.new_action('append', d, 'callcounter', 'yes');
					c[d]['callcounter'] = 'yes';
				}
				sessionData.pbxinfo['users'][d] = c[d];
				continue;
			}
		}}
		users_conf.callActions();
	}, // end of readcfg.UsersConf();

	MisdnConf: function(){ // readcfg.MisdnConf();
		var c = config2json({filename:'misdn.conf', usf:1});
		for(var d in c){ if( c.hasOwnProperty(d) && d.beginsWith('trunk_m') ) {
			sessionData.pbxinfo['trunks']['bri'][d] = c[d] ;
		}}
	},

	dahdiScanConf : function(){ // readcfg.dahdiScanConf();
		// reads dahdi_scan.conf and updates sessionData.FXO_PORTS_DETECTED and sessionData.FXS_PORTS_DETECTED
		// reads ASTGUI.globals.hwcfgFile and looks for analog hardware changes
		sessionData.FXO_PORTS_DETECTED = [];
		sessionData.FXS_PORTS_DETECTED = [];
		var y;
		var c = config2json({filename: ASTGUI.globals.dahdiScanOutput , usf:0});
		var tmp_dahdi_contexts = c.getOwnProperties();
		if( !tmp_dahdi_contexts.length ){ // no analog or digital hardware found, hide the hardware configuration & misdn panels
			miscFunctions.hide_panel('digital.html', 0);
			miscFunctions.hide_panel('misdn.html', 0);
		}

		for( var d in c ){ if (c.hasOwnProperty(d) ) {
			c[d].each( function( item ) {
				if( item.beginsWith('port=') && item.contains('FXO') && !item.contains('FAILED') ){ // we are looking for item if it is in the format 'port=4,FXO'
					y = item.betweenXY('=',',');
					sessionData.FXO_PORTS_DETECTED.push(String(y));
				}
				if( item.beginsWith('port') && item.contains('FXS') && !item.contains('FAILED') ){ // we are looking for item if it is in the format 'port=4,FXS'
					y = item.betweenXY('=',',');
					sessionData.FXS_PORTS_DETECTED.push(String(y));
				}
			});
		}}

		// Look for Changes in Analog Hardware ports
		var CONFIGURED_FXSPORTS = '';
		var CONFIGURED_FXOPORTS = '';
		var n = config2json({filename: ASTGUI.globals.hwcfgFile, usf:1});
		for( var l in n ){ if(n.hasOwnProperty(l)){ // l is location
			if(l=='ANALOGPORTS'){
				CONFIGURED_FXSPORTS = n[l].getProperty('FXS');
				CONFIGURED_FXOPORTS = n[l].getProperty('FXO');
			}
		}}

// 		if( CONFIGURED_FXOPORTS != sessionData.FXO_PORTS_DETECTED.join(',') || CONFIGURED_FXSPORTS != sessionData.FXS_PORTS_DETECTED.join(',') ){
// 			var message = 'Changes detected in your Analog Hardware !! <BR><BR>'
// 				+ 'You will now be redirected to the hardware configuration page';
// 			ASTGUI.yesOrNo({
// 				btnYes_text : 'Ok' ,
// 				title: 'Hardware changes detected ! ' ,
// 				msg: message ,
// 				ifyes: function(){
// 					miscFunctions.click_panel('digital.html');
// 				},
// 				ifno: function(){ } ,
// 				hideNo: true
// 			});
// 		}

	} // end of readcfg.dahdiScanConf();
}; // end of readcfg


astgui_parseDefaultContext = function( cxt ){ 
	// takes the default context cxt array and looks for any queues, Operator Extensions, VoiceMail extension
	// and updates the sessionData.pbxinfo accordingly
	cxt.each( function(line){
		// sessionData.pbxinfo['localextensions'] = {}; 
		// sessionData.pbxinfo['queues'] = {};
		if( line.beginsWith('exten=') && line.contains(',1,VoiceMailMain') ){ // if line is in the format 'exten=XXXXXX,1,VoiceMailMain'
			sessionData.pbxinfo['localextensions']['VoiceMailMain'] = line.afterChar('=') ;
			return true;
		}

		if( line.beginsWith('exten=o,1') ){ // if line is in the format 'exten=o,1,Goto(default,6000,1)'
			sessionData.pbxinfo['localextensions']['Operator'] = ASTGUI.parseContextLine.getAppWithArgs( line ); // Goto(default,6000,1)
			return true;
		}

	} );
};



astgui_parseGlobalContext = function(cxt){
	cxt.each( function(line){
		if( line.beginsWith(ASTGUI.globals.obcidstr) || line.beginsWith(ASTGUI.globals.obcidUsrPrefix) ){
			var x = line.beforeChar('=').trim();
			var y = line.afterChar('=').trim();
			sessionData.pbxinfo.GLOBALS[x] = y;
		}
	});
};




astgui_manageusers  = { // all the functions related to user management would reside in this object

	getOBCID_user: function(user){ // returns outbound callerid of the specified user
		return sessionData.pbxinfo['GLOBALS'].getProperty( ASTGUI.globals.obcidUsrPrefix + user ) ;
	},

	deleteuser: function(user, deletevm, cb){ // delete the specified user - usage :: if( astgui_manageusers.deletuser('6001') ){ success };
		var u = new listOfSynActions('users.conf') ;
			u.new_action('delcat', user, '', '');
		u.callActions();
		u.clearActions('extensions.conf');
			u.new_action('delete', 'globals', ASTGUI.globals.obcidUsrPrefix + user, '');
		u.callActions();
		delete sessionData.pbxinfo['users'][user] ;
		

		var qs_x = new listOfActions('queues.conf');
		var qs = config2json({filename:'queues.conf', usf:0 }) ;
		for( var Q in qs ){ if( qs.hasOwnProperty(Q) ){
			if( qs[Q].contains('member=Agent/' + user ) ){
				qs_x.new_action('delete', Q , 'member', '','Agent/' + user );
			}
		}}
		
		qs_x.callActions(function(){
			if( deletevm ){
				ASTGUI.systemCmd('rm ' + top.sessionData.directories.voicemails_dir + user + ' -rf', cb);
			}else{
				cb();
			}
		});
	},

	listOfUsers: function(){ // returns an array with list of users - useful for listing available users in a select box etc.
		//sessionData.pbxinfo.users = ASTGUI.toCustomObject(sessionData.pbxinfo.users);
		// astgui_manageusers.listOfUsers();
		return ( sessionData.pbxinfo.users && sessionData.pbxinfo.users.getOwnProperties && sessionData.pbxinfo.users.getOwnProperties() ) || [];
	},

	/* returns an array based on a manager packet/response */
// 	responseToArray: function(resp) {
// 		var msgs = new Array();
// 		var inmsg = 0;
// 		var msgnum = 0;
// 		var x,y;
// 		var s = resp;
// 		var allheaders = s.split('\r\n');
// 		for (x=0;x<allheaders.length;x++) {
// 			if (allheaders[x].length) {
// 				var fields = allheaders[x].split(': ');
// 				if (!inmsg) {
// 					msgs[msgnum] = new Object();
// 					msgs[msgnum].headers = {};
// 					msgs[msgnum].names = new Array();
// 					y=0;
// 				}
// 				msgs[msgnum].headers[fields[0].toLowerCase()] = allheaders[x].substr(fields[0].length +2);
// 				msgs[msgnum].names[y++] = fields[0].toLowerCase();
// 				inmsg=1;
// 			} else {
// 				if (inmsg) {
// 					inmsg = 0;
// 					msgnum++;
// 				}
// 			}
// 		}
// 	},

	getUserDetails: function(user){ // returns an object with the specified user details, if the specified user is not found returns null ;
		return sessionData.pbxinfo.users[user] || null ;
	},

	addUser: function(exten, userinfo, cb){ // adds new user 'exten' - userinfo object has all the properties, cb is the callback function
		// usage:: astgui_manageusers.addUser('6666', {'callwaiting': 'no', 'signalling' : 'fxo_ls'}, cb ); // cb is callback function
		//
		// Warning: this functions deletes any existing user 'exten'
		// note: 	this function updates sessionData.pbxinfo['users'] before actually making the ajax requests
		//		we need to address this issue at some point in future
		userinfo = ASTGUI.toCustomObject(userinfo) ;
		var disallow = userinfo.getProperty('disallow') || 'all' ;
		var allow = userinfo.getProperty('allow') || 'all' ;
		sessionData.pbxinfo['users'][exten] = userinfo ;
		sessionData.pbxinfo['users'][exten]['disallow'] = disallow ;
		sessionData.pbxinfo['users'][exten]['allow'] = allow ;
		delete userinfo.disallow ;
		delete userinfo.allow ;
		var x = new listOfActions();
		x.filename('users.conf');
		x.new_action('delcat', exten, '', '');
		x.new_action('newcat', exten, '', '');
		x.new_action('append', exten, 'username', exten );
		x.new_action('append', exten, 'transfer', 'yes' );
		x.new_action('append', exten, 'disallow', disallow ); // makesure 'disallow' is added before 'allow'
		x.new_action('append', exten, 'allow', allow );
		x.new_action('append', exten, 'mailbox', userinfo.mailbox || exten );
		sessionData.pbxinfo['users'][exten]['mailbox'] = userinfo.mailbox || exten ;

		x.new_action('append', exten, 'call-limit', '100' );
		sessionData.pbxinfo['users'][exten]['mailbox'] = '100' ;


		if(userinfo.mailbox) delete userinfo.mailbox;
		if(userinfo.hasOwnProperty('hassip') && userinfo['hassip'].isAstTrue() ){
			x.new_action( 'append', exten, 'host', 'dynamic' );
		}
		for( var d in userinfo ){ if( userinfo.hasOwnProperty(d) ) {
			x.new_action( 'append', exten, d, userinfo[d] );
		}}
		x.callActions(cb); // where after is the callback function
	},

	editUserProperty: function(p){ // update the value of an existing property, adds that property if that property does not currently exist.
		// usage:: astgui_manageusers.editUserProperty({ user:'6001', property: 'allow', value: 'all' });

		if(!sessionData.pbxinfo['users'][p.user]) { // if specified user does not exist
			return false;
		}
		var u = ASTGUI.updateaValue({ file: 'users.conf', context: p.user, variable: p.property, value: p.value }) ;
		if(u){
			try{ sessionData.pbxinfo['users'][p.user][p.property] = p.value ; } catch(err){ }
			return true;
		}else{
			return false;
		}
	}
};


astgui_managetrunks  = { // all the functions related to managing trunks would reside in this object

	deletetrunk: function(trunk){ // delete the specified trunk - usage :: if( astgui_managetrunks.deletetrunk('trunk_1') ){ success };
		var u = new listOfSynActions('users.conf') ;
		u.new_action('delcat', trunk, '', ''); 
		u.callActions();

		var EXT_CNF = config2json({filename:'extensions.conf', usf:0 }) ;
		var v = new listOfSynActions('extensions.conf') ;
		v.new_action('delete', 'globals', trunk , '' );
		for( var ct in EXT_CNF){ if( EXT_CNF.hasOwnProperty(ct) ){
			if( ct == ASTGUI.contexts.TrunkDIDPrefix + trunk || ct.beginsWith( ASTGUI.contexts.TrunkDIDPrefix + trunk + '_' ) ){
				v.new_action('delcat', ct , '' , '' );
			}
		}}
		v.callActions();

		try{
			if(sessionData.pbxinfo['trunks']['analog'][trunk]){ delete sessionData.pbxinfo['trunks']['analog'][trunk]; return true; }
			if(sessionData.pbxinfo['trunks']['sip'][trunk]){ delete sessionData.pbxinfo['trunks']['sip'][trunk]; return true; }
			if(sessionData.pbxinfo['trunks']['iax'][trunk]){ delete sessionData.pbxinfo['trunks']['iax'][trunk]; return true; }
			if(sessionData.pbxinfo['trunks']['pri'][trunk]){ delete sessionData.pbxinfo['trunks']['pri'][trunk]; return true; }
			if(sessionData.pbxinfo['trunks']['providers'][trunk]){ delete sessionData.pbxinfo['trunks']['providers'][trunk]; return true; }
		}catch(err){}
		return true;
	},

	listofAllTrunks : function(){
		var x = [];
		return x.concat( this.listOfAnalogTrunks(), this.listOfSIPTrunks(), this.listOfIAXTrunks(), this.listOfPRITrunks(), this.listOfProviderTrunks() , this.listOfBRITrunks() );
	},

	listOfProviderTrunks : function(){
		var list =[];
		try{
			for(var item in sessionData.pbxinfo.trunks.providers){ if(sessionData.pbxinfo.trunks.providers.hasOwnProperty(item)){
				list.push(item);
			}}
		} catch (err) { return []; }
		return list;
	},

	listOfAnalogTrunks: function(){ // astgui_managetrunks.listOfAnalogTrunks() ---> returns 'list of AnalogTrunks array'
		var list =[];
		try{
			for(var item in sessionData.pbxinfo.trunks.analog){ if(sessionData.pbxinfo.trunks.analog.hasOwnProperty(item)){
				list.push(item);
			}}
		} catch (err) { return []; }
		return list;
	},

	listOfBRITrunks: function(){
		var list =[];
		try{
			for(var item in sessionData.pbxinfo.trunks.bri){ if( sessionData.pbxinfo.trunks.bri.hasOwnProperty(item) ){
				list.push(item);
			}}
		} catch (err) { return []; }
		return list;
	},

	listOfSIPTrunks: function(){ // returns an array with list of SIPTrunks 
		var list =[];
		try{
			for(var item in sessionData.pbxinfo.trunks.sip){ if(sessionData.pbxinfo.trunks.sip.hasOwnProperty(item)){
				list.push(item);
			}}
		} catch (err) { return []; }
		return list;
	},

	listOfIAXTrunks: function(){ // returns an array with list of IAXTrunks 
		var list =[];
		try{
			for(var item in sessionData.pbxinfo.trunks.iax){ if(sessionData.pbxinfo.trunks.iax.hasOwnProperty(item)){
				list.push(item);
			}}
		} catch (err) { return []; }
		return list;
	},

	listOfPRITrunks: function(){ // returns an array with list of PRITrunks 
		var list =[];
		try{
			for(var item in sessionData.pbxinfo.trunks.pri){ if(sessionData.pbxinfo.trunks.pri.hasOwnProperty(item)){
				list.push(item);
			}}
		} catch (err) { return []; }
		return list;
	},

	getTrunkDetails: function(trunk){ // returns an object with the trunk details, if the specified trunk is not found returns null
		try{
			var x = null , tr = new ASTGUI.customObject;
			if( sessionData.pbxinfo.trunks.analog.hasOwnProperty(trunk) ){ x = sessionData.pbxinfo.trunks.analog; } // if is an analog trunk
			else if( sessionData.pbxinfo.trunks.sip.hasOwnProperty(trunk) ){ x = sessionData.pbxinfo.trunks.sip; } // if is a sip trunk
			else if( sessionData.pbxinfo.trunks.iax.hasOwnProperty(trunk) ){ x = sessionData.pbxinfo.trunks.iax; } // if is a iax trunk
			else if( sessionData.pbxinfo.trunks.pri.hasOwnProperty(trunk) ){ x = sessionData.pbxinfo.trunks.pri; } // if is a iax trunk
			else if( sessionData.pbxinfo.trunks.providers.hasOwnProperty(trunk) ){ x = sessionData.pbxinfo.trunks.providers; } // if is a service Provider trunk
			if( x === null ){ return x; }
			for( var d in x ){ if( x.hasOwnProperty(d) ){ tr[d] = x[d] ; }}
			return tr;

		}catch(err){
			return null;
		}
	},

	addAnalogTrunk: function(tr, cbf){ // creates a new analog trunk with the details metioned in tr object, cbf is callback function
		// usage:: astgui_managetrunks.addAnalogTrunk({ 'zapchan':'2,3,4' , (optional) trunkname:'Ports 2,3,4', (optional) group: '1,2,3'} , cbf) ;

		if( !tr.hasOwnProperty('zapchan') &&  !tr.hasOwnProperty('dahdichan') ){return false;} // zapchan is a required parameter. 

		if( tr.hasOwnProperty('zapchan') ){
			var TMP_CHANNELS = tr.zapchan ;
			delete tr.zapchan ;
		}
		if( tr.hasOwnProperty('dahdichan') ){
			var TMP_CHANNELS = tr.dahdichan ;
			delete tr.dahdichan ;
		}
		
		var trunk = astgui_managetrunks.misc.nextAvailableTrunk_x();
		var newgroup = astgui_managetrunks.misc.nextAvailableGroup();
		var group = '';
		if( tr.hasOwnProperty('group') ){
			group = tr.group;
			if(group.indexOf("New") > -1){
				group.replace(/New/, newgroup);
			}
		}else{
			group = newgroup;
		}
		var ct = ASTGUI.contexts.TrunkDIDPrefix + trunk;
		// there are no incoming rules for analog trunks - there is only one catch all rule
		// we only add the fallback extension "exten=s,3,..."  in incoming rules  Ex: 'exten = s,3,Goto(default|6000|1)' 
		// any other patterns are not defined for Analag trunks
		var x = new listOfActions();
		x.filename('users.conf');
		x.new_action('delcat', trunk , '', ''); // not really needed - but just in case
		x.new_action('newcat', trunk, '', ''); // create new trunk
		// add some default values for any AnalogTrunk
			sessionData.pbxinfo.trunks.analog[trunk] = new ASTGUI.customObject; // add new/reset analog trunk info in sessionData
		x.new_action('append', trunk, 'group', group);
			sessionData.pbxinfo.trunks.analog[trunk]['group'] = group;
		x.new_action('append', trunk, parent.sessionData.DahdiChannelString , TMP_CHANNELS);
			sessionData.pbxinfo.trunks.analog[trunk][parent.sessionData.DahdiChannelString] = TMP_CHANNELS;
		var zap_channels = ASTGUI.miscFunctions.chanStringToArray( TMP_CHANNELS );

		x.new_action('append', trunk, 'hasexten', 'no');
			sessionData.pbxinfo.trunks.analog[trunk]['hasexten'] = 'no';
		x.new_action('append', trunk, 'hasiax', 'no');
			sessionData.pbxinfo.trunks.analog[trunk]['hasiax'] = 'no';
		x.new_action('append', trunk, 'hassip', 'no');
			sessionData.pbxinfo.trunks.analog[trunk]['hassip'] = 'no';
		x.new_action('append', trunk, 'trunkstyle', 'analog'.guiMetaData() );
			sessionData.pbxinfo.trunks.analog[trunk]['trunkstyle'] = 'analog';
		x.new_action('append', trunk, 'context', ct);
			sessionData.pbxinfo.trunks.analog[trunk]['context'] = ct;

// 		var trunk_name = ( zap_channels.length > 1 ) ? 'Ports ' + tr.zapchan : 'Port ' + tr.zapchan ;
		x.new_action('append', trunk, 'trunkname', tr.trunkname.guiMetaData() );
			sessionData.pbxinfo.trunks.analog[trunk]['trunkname'] = tr.trunkname;
			delete tr.trunkname;


		// these fields are already added, delete before iterating through userinfo
		if( tr.hasOwnProperty('group') ){ delete tr.group ; }
		if( tr.hasOwnProperty('signalling') ){ delete tr.signalling ; } // we will set the signalling based on zaptel.conf, so ignore what ever signalling is passed
		if( tr.hasOwnProperty('hasiax') ){ delete tr.hasiax ; }
		if( tr.hasOwnProperty('hassip') ){ delete tr.hassip ; }
		if( tr.hasOwnProperty('trunkstyle') ){ delete tr.trunkstyle ; }

		for( var d in tr ){ if( tr.hasOwnProperty(d) ) {
			x.new_action( 'append', trunk , d, tr[d] );
				sessionData.pbxinfo.trunks.analog[trunk][d] = tr[d];
		}}

		// zap_channels
		zap_channels.each( function(channel){
			var temp_ls_List = ASTGUI.cloneObject( sessionData.PORTS_SIGNALLING.ls ) ;
			var sg = ( temp_ls_List.contains(channel) ) ? 'fxs_ls':'fxs_ks' ;
			x.new_action('append', trunk, 'signalling', sg);
			x.new_action( 'append', trunk , 'channel', channel );
		} );

		var cb = function(){
			var v = new listOfSynActions('extensions.conf') ;
			v.new_action('delcat', ct, '', '');
			v.new_action('newcat', ct, '', ''); // add context
			v.new_action('delcat', ct + ASTGUI.contexts.TrunkDefaultSuffix , '', '');
			v.new_action('newcat', ct + ASTGUI.contexts.TrunkDefaultSuffix , '', ''); // add context
			v.new_action('append', ct , 'include', ct + ASTGUI.contexts.TrunkDefaultSuffix );

			v.new_action('append', 'globals', trunk , parent.sessionData.DahdiDeviceString + '/g' + group);

			var h = v.callActions();
			if( h.contains('Response: Success') ){
				cbf();
			}else{
				// looks like something failed
				// may be we should re-check users.conf and extensions.conf again and delete sessionData.pbxinfo.trunks.analog[trunk] if needed??
			}
		};

		x.callActions(cb); 
	},

	addIAXTrunk: function( tr , cbf, cxb ){  //
		// usage:: astgui_managetrunks.addIAXTrunk( {'host':'iaxtel.com' , username:'my_username', secret:'my_secret', ....}, cbf, "context-basis" ) ;
		if( !tr.hasOwnProperty('host') ){ return false; } //check for required parameters

		// add some default values for any IAXTrunk

		var trunk = tr.username ;
		if (cxb == 'GUIAssigned') trunk = astgui_managetrunks.misc.nextAvailableTrunk_x();
		else if (cxb == 'FromProvider') trunk = tr.trunkname;
		else if (cxb == 'FromUser') trunk = tr.username;


		sessionData.pbxinfo.trunks.iax[trunk] = new ASTGUI.customObject; // add new/reset iax trunk info in sessionData

		tr.hasiax = 'yes' ;
		tr.registeriax = 'yes';
		tr.hassip = 'no' ;
		tr.registersip = 'no';
		tr.trunkstyle ='voip';
		tr.hasexten = 'no';
		tr.disallow ='all';
		tr.allow = 'all';

		var ct = ASTGUI.contexts.TrunkDIDPrefix + trunk;
		var x = new listOfActions();

		x.filename('users.conf');
		x.new_action('delcat', trunk , '', ''); // not really needed - but just in case
		x.new_action('newcat', trunk, '', ''); // create new trunk

		x.new_action('append', trunk, 'context', ct);
			sessionData.pbxinfo.trunks.iax[trunk]['context'] = ct;

		for( var d in tr ){ if( tr.hasOwnProperty(d) ) {
			sessionData.pbxinfo.trunks.iax[trunk][d] = tr[d];
			if(d=='trunkname'){
				tr[d] = tr[d].guiMetaData() ;
			}
			x.new_action( 'append', trunk , d, tr[d] );
		}}

		var cb = function(){
			var v = new listOfSynActions('extensions.conf') ;
			v.new_action('delcat', ct, '', '');
			v.new_action('newcat', ct, '', ''); // add context
			v.new_action('delcat', ct + ASTGUI.contexts.TrunkDefaultSuffix , '', '');
			v.new_action('newcat', ct + ASTGUI.contexts.TrunkDefaultSuffix , '', ''); // add context
			v.new_action('append', ct , 'include', ct + ASTGUI.contexts.TrunkDefaultSuffix );

			v.new_action('update', 'globals', trunk, 'IAX2/' + trunk);
			var h = v.callActions();
			if( h.contains('Response: Success') ){
				cbf();
			}else{
				// looks like something failed
				// may be we should re-check users.conf and extensions.conf again and delete sessionData.pbxinfo.trunks.iax[trunk] if needed??
			}
		}
		x.callActions(cb); 
	},

	addSIPTrunk: function(tr,cbf, cxb){ // 
		// usage:: astgui_managetrunks.addSIPTrunk( {'host':'sip_test.digium.com' , username:'my_username', secret:'my_secret',(required)fallback: '6001' ....}, cbf, "context-basis") ;
		if( !tr.hasOwnProperty('host') ){ return false; } //check for required parameters

		// add some default values for any SIPTrunk
		tr.hasiax = 'no' ; tr.registeriax = 'no';
		tr.hassip = 'yes' ;
		if ( tr.host == "dynamic" ) {
			tr.registersip = 'no';
		} else {
			tr.registersip = 'yes';
		}
		tr.trunkstyle ='voip';
		tr.hasexten = 'no';
		tr.disallow ='all';
		tr.allow = 'all';

		var trunk = tr.username ; /* default */
		if (cxb == 'GUIAssigned') trunk = astgui_managetrunks.misc.nextAvailableTrunk_x();
		else if (cxb == 'FromProvider') trunk = tr.trunkname;
		else if (cxb == 'FromUser') trunk = tr.username;

		var ct = ASTGUI.contexts.TrunkDIDPrefix + trunk;
		var x = new listOfActions();

		x.filename('users.conf');
		x.new_action('delcat', trunk , '', ''); // not really needed - but just in case
		x.new_action('newcat', trunk, '', ''); // create new trunk
			sessionData.pbxinfo.trunks.sip[trunk] = new ASTGUI.customObject; // add new/reset sip trunk info in sessionData
		x.new_action('append', trunk, 'context', ct);
			sessionData.pbxinfo.trunks.sip[trunk]['context'] = ct;

		for( var d in tr ){ if( tr.hasOwnProperty(d) ) {
			sessionData.pbxinfo.trunks.sip[trunk][d] = tr[d];
			if( d == 'trunkname' ){
				tr[d] = tr[d].guiMetaData() ;
			}
			x.new_action( 'append', trunk , d, tr[d] );
		}}

		var cb = function(){
			var v = new listOfSynActions('extensions.conf') ;
			v.new_action('delcat', ct, '', '');
			v.new_action('newcat', ct, '', ''); // add context

			v.new_action('delcat', ct + ASTGUI.contexts.TrunkDefaultSuffix , '', '');
			v.new_action('newcat', ct + ASTGUI.contexts.TrunkDefaultSuffix , '', ''); // add context
			v.new_action('append', ct , 'include', ct + ASTGUI.contexts.TrunkDefaultSuffix );

			v.new_action('update', 'globals', trunk, 'SIP/' + trunk);
			var h = v.callActions();

			if( h.contains('Response: Success') ){ 
				cbf();
			}else{ 
				// looks like something failed
				// may be we should re-check users.conf and extensions.conf again and delete sessionData.pbxinfo.trunks.sip[trunk] if needed??
			}
		}
		x.callActions(cb); 
	},

	misc:{ // object to store misc function used by other main functions in astgui_managetrunks
		nextAvailableTrunk_x: function(){ // Ex: return 'trunk_2' - if [trunk_1, trunk_3, trunk_5] currently exists
			//astgui_managetrunks.misc.nextAvailableTrunk_x()
			var x = [], y = astgui_managetrunks.listofAllTrunks(); 
			y.each( function( item ){
				if( item.beginsWith('trunk_') ){ x.push( item.split('trunk_')[1] ); }
			} );
			if( !x.length ){ return 'trunk_1' ; }
			return 'trunk_' + x.firstAvailable() ;
		},

		nextAvailableGroup: function() { // Ex: return '3' - if groups 1,2,4 are already defined in analog & pri trunks;
			//astgui_managetrunks.misc.nextAvailableGroup()
			var x = [], y = [];
			y = astgui_managetrunks.listOfAnalogTrunks();
			y.each( function( item ){
				x.push( sessionData.pbxinfo.trunks.analog[item]['group'] );
			} );

			y = astgui_managetrunks.listOfPRITrunks(); 
			y.each( function( item ){
				x.push( sessionData.pbxinfo.trunks.pri[item]['group'] );
			} );

			// we donot have to look in sip and iax trunks as they donot have a 'group' field
			return x.firstAvailable() ;
		},

		getTrunkType: function(TRUNK){ // astgui_managetrunks.misc.getTrunkType(TRUNK)
			if ( sessionData.pbxinfo.trunks.sip[TRUNK] ) { return 'sip' ; }
			if ( sessionData.pbxinfo.trunks.iax[TRUNK] ) { return 'iax' ; }
			if ( sessionData.pbxinfo.trunks.analog[TRUNK] ) { return 'analog' ; }
			if ( sessionData.pbxinfo.trunks.pri[TRUNK] ) { return 'pri' ; }
			if ( sessionData.pbxinfo.trunks.bri[TRUNK] ) { return 'bri' ; }
			if ( sessionData.pbxinfo.trunks.providers[TRUNK] ) { return 'providers' ; }
			return null;
		},

		getProviderTrunkType: function(TRUNK){ // astgui_managetrunks.misc.getProviderTrunkType(TRUNK)
			if (!sessionData.pbxinfo.trunks.providers.hasOwnProperty(TRUNK) ) { return '' ; }
			var T = sessionData.pbxinfo.trunks.providers[TRUNK] ;
			if( T.hasOwnProperty('hassip') && T['hassip'].isAstTrue() ){ return 'sip'; }
			if( T.hasOwnProperty('hasiax') && T['hasiax'].isAstTrue() ){ return 'iax'; }
			return '';
		},

		getTrunkName: function(TRUNK){ // astgui_managetrunks.misc.getTrunkName(TRUNK)
			if( TRUNK == 'Skype' ) return TRUNK ;
			var r = sessionData.pbxinfo.trunks;
			if ( r.sip[TRUNK] ) { return r.sip[TRUNK]['trunkname'] || TRUNK ; }
			if ( r.iax[TRUNK] ) { return r.iax[TRUNK]['trunkname'] || TRUNK ; }
			if ( r.analog[TRUNK] ) { return r.analog[TRUNK]['trunkname'] || TRUNK ; }
			if ( r.pri[TRUNK] ) { return r.pri[TRUNK]['trunkname'] || TRUNK ; }
			if ( r.bri[TRUNK] ) {
				if( r.bri[TRUNK]['trunkname'] ){
					return 'BRI - ' + r.bri[TRUNK]['trunkname'] ;
				}else{
					return TRUNK ;
				}
			}
			if ( r.providers[TRUNK] ) { return r.providers[TRUNK]['trunkname'] || TRUNK ; }
			return '';
		}
	}

};















astgui_manageCallingRules = {

	// ; Note that we expect only one calling rule in each 'CallingRule_XYZ' context
	// below are two contexts as expected in extensions.conf
	//
	// [CallingRule_911]
	//   exten = 911!,1,Macro( trunkdial-failover-0.3, ${trunk_1}/911, ${trunk_2}/911, trunk_1,trunk_2 )
	//
	// [CallingRule_1900]
	//   exten = 1900!,1,Hangup ; BLOCK 1900 calls
	//
	// [CallingRule_US]
	//    exten = _91NXXXXXXXXX,1,Macro( trunkdial-failover-0.3, ${trunk_1}/${EXTEN:1}, ${trunk_2}/${EXTEN:1},trunk_1,trunk_2 )
	//	OR
	//    exten = _91NXXXXXXXXX,1,Macro( trunkdial-failover-0.3, ${trunk_1}/PREFIX_1${EXTEN:1}, ${trunk_2}/PREFIX_2${EXTEN:0}, trunk_1, trunk_2 )
	//                                                                      ^^^^^^^^       ^^             ^^^^^^^^^       ^^
	// [CallingRule_Local]
	//    exten = _256XXXXXXX,1,Macro( trunkdial-failover-0.3, ${trunk_1}/${EXTEN:0}, ${trunk_2}/${EXTEN:0},trunk_1,trunk_2 )
	//    exten = _1256XXXXXXX,1,Macro( trunkdial-failover-0.3, ${trunk_1}/${EXTEN:1}, ${trunk_2}/${EXTEN:1},trunk_1,trunk_2 )
	//    exten = _91256XXXXXXX,1,Macro( trunkdial-failover-0.3, ${trunk_1}/${EXTEN:2}, ${trunk_2}/${EXTEN:2},trunk_1,trunk_2 )
	//
	/* sample structure for a callingRules object in javascript
		sessionData.pbxinfo.callingRules : {
			CallingRule_911 : [ 'exten = 911!,1,Macro( trunkdial-failover-0.3, ${trunk_1}/911, ${trunk_2}/911, trunk_1,trunk_2 )' ],
			CallingRule_1900 : [ 'exten = 1900!,1,Hangup ; BLOCK 1900 calls' ],
			CallingRule_Local : [ 
				'exten = _256XXXXXXX,1,Macro( trunkdial-failover-0.3, ${trunk_1}/${EXTEN:0}, ${trunk_2}/${EXTEN:0},trunk_1,trunk_2 )',
				'exten = _1256XXXXXXX,1,Macro( trunkdial-failover-0.3, ${trunk_1}/${EXTEN:1}, ${trunk_2}/${EXTEN:1},trunk_1,trunk_2 )',
				'exten = _91256XXXXXXX,1,Macro( trunkdial-failover-0.3, ${trunk_1}/${EXTEN:2}, ${trunk_2}/${EXTEN:2},trunk_1,trunk_2 )'
			]
		}
	*/

	createCallingRule: function(crname, crstring){ // usage :: astgui_manageCallingRules.createCallingRule(name, crstring)
		if(! crname.beginsWith(ASTGUI.contexts.CallingRulePrefix) ){
			crname = ASTGUI.contexts.CallingRulePrefix + crname ;
		}
		if( crstring.beginsWith('exten=') ){
			crstring = crstring.lChop('exten=');
		}
		var u = new listOfSynActions('extensions.conf') ;
		if( !sessionData.pbxinfo.callingRules.hasOwnProperty(crname) ){
			u.new_action('newcat', crname , '', '');
			sessionData.pbxinfo.callingRules[crname] = [] ;
		}
		u.new_action( 'append', crname , 'exten', crstring );
		u.callActions();
		sessionData.pbxinfo.callingRules[crname].push( 'exten=' + crstring );
		return true;
	},

	updateCallingRule: function( crname, oldString, newString ){ // astgui_manageCallingRules.createCallingRule( crname, oldString, newString  )
		var u = new listOfSynActions('extensions.conf') ;
		u.new_action('update', crname, 'exten', newString.lChop('exten=') , oldString.lChop('exten=') );
		u.callActions();
		sessionData.pbxinfo.callingRules[crname] = ASTGUI.cloneObject(sessionData.pbxinfo.callingRules[crname]).replaceAB(oldString, newString);
	},

	deleteCallingRule: function( crname, crstring ){
		if( crstring.beginsWith('exten=') ){
			crstring = crstring.lChop('exten=');
		}

		var u = new listOfSynActions('extensions.conf') ;

		if( ( sessionData.pbxinfo.callingRules[crname].length == 1 ) && (sessionData.pbxinfo.callingRules[crname][0] == 'exten=' + crstring ) ){
			u.new_action('delcat', crname , '', '');
			delete sessionData.pbxinfo.callingRules[crname] ;
		}else{
			u.new_action('delete', crname, 'exten', '', crstring );
			sessionData.pbxinfo.callingRules[crname] = ASTGUI.cloneObject(sessionData.pbxinfo.callingRules[crname]).withOut( 'exten=' + crstring ) ;
		}

		u.callActions();
		return true;
	}
};















astgui_manageCallPlans = { // manage calling plans/Dialplans
	// [CLPN_USA]
	//   include = CallingRule_US
	//   include = CallingRule_911

	/* sample structure for a callingplan object
	sessionData.pbxinfo.callingPlans : { 
		CLPN_USA : {
			includes : ['default', 'CallingRule_local', 'CallingRule_911', 'parkedcalls' ]  //contexts included in this dialplan
		}
	}
	*/
	parseContext: function(cxt){ // parse a callPlan context
		// takes an array as input ( an extensions.conf context read with usf:0 ) and returns a object (callPlan data structure)
		var dp = { includes: [] };
		cxt.each( function(line) {
			if( line.beginsWith('include=') ){ dp.includes.push( line.afterChar('=') ); return true; }
		});
		return dp;
	},

	listPlans: function(){ // astgui_manageCallPlans.listPlans();
		var list = [];
		try{
			for(var x in sessionData.pbxinfo.callingPlans ){ if( sessionData.pbxinfo.callingPlans.hasOwnProperty(x)){	
				list.push(x);
			}}
		}catch(err){
			return [];
		}
		return list;
	},

	addPlan: function( dp_name, dp , cbf ) {
	//	astgui_manageCallPlans.addPlan('CLPN_USA', { includes : ['default','parkedcalls'] }, callback_function );
		if( !dp_name.beginsWith(ASTGUI.contexts.CallingPlanPrefix) ){
			dp_name = ASTGUI.contexts.CallingPlanPrefix + dp_name;
		}
		var x = new listOfActions();
		x.filename('extensions.conf');
		x.new_action('delcat', dp_name, '', ''); 
		x.new_action('newcat', dp_name , '', ''); // create new callplan
		dp.includes.each( function(include_context) {
			x.new_action( 'append', dp_name , 'include' , include_context );
		});
		var on_success = function(){
			sessionData.pbxinfo.callingPlans[ dp_name ] = dp ; // that was simple :)
			cbf(); // execute callback
		};
		x.callActions(on_success);
	},

	deletePlan: function(dp){ // ex: astgui_manageCallPlans.deletePlan('CLPN_USA');
		var u = new listOfSynActions('extensions.conf') ;
		u.new_action('delcat', dp, '', '') ; 
		var g = u.callActions();
		if ( g.contains('Response: Success') ){
			try{
				if( sessionData.pbxinfo.callingPlans[dp] ) { // remove the dialplan from the pbxinfo
					delete sessionData.pbxinfo.callingPlans[dp] ;
				}
			}catch(err){}
			return true;
		}else{
			return false;
		}
	},

	dropCallingRule: function( callPlan, callingRule) { //astgui_manageCallPlans.dropCallingRule ('CLPN_USA', 'CallingRule_local')
		var u = new listOfSynActions('extensions.conf') ;
		u.new_action('delete', callPlan, 'include', '', callingRule ) ;
		var g = u.callActions();
		if ( g.contains('Response: Success') ){
			try{
				if( sessionData.pbxinfo.callingPlans[callPlan].includes ) {
					var t = sessionData.pbxinfo.callingPlans[callPlan].includes.withOut( callingRule ) ;
					sessionData.pbxinfo.callingPlans[callPlan].includes = t ;
				}
			}catch(err){}
			return true;
		}else{
			return false;
		}
	},

	includeCallingRule: function( callPlan, callingRule) { //astgui_manageCallPlans.includeCallingRule('CLPN_USA', 'CallingRule_local')
		var u = new listOfSynActions('extensions.conf') ;
		u.new_action('append', callPlan, 'include', callingRule ) ;
		var g = u.callActions();
		if ( g.contains('Response: Success') ){
			try{
				if( sessionData.pbxinfo.callingPlans[callPlan].includes ) {
					sessionData.pbxinfo.callingPlans[callPlan].includes.push( callingRule ) ;
				}
			}catch(err){}
			return true;
		}else{
			return false;
		}
	},

	nextAvailableDP: function( ){ // astgui_manageCallPlans.nextAvailableDP();
		var t = [];
		var s = ASTGUI.contexts.CallingPlanPrefix + 'DialPlan' ;
		var c = parent.astgui_manageCallPlans.listPlans() ;
		c.each( function(plan){
			if( plan.beginsWith(s) ){
				var d = Number( plan.lChop(s) ) ;
				if( isNaN(d) ){
				
				}else{
					t.push(d);
				}
			}
		});
		return 'DialPlan' + t.firstAvailable(1) ;
	}
};

















astgui_manageVoiceMenus = {
	/* sample structure for a voicemenu object 

	[voicemenus]
		exten = 6070,1,Goto(voicemenu-custom-1|s|1) // extension for voicemenus are defined here
	[voicemenu-custom-1]
		exten = s,1,NoOp(Welcome VoiceMenu)
		exten = s,2,Answer
		exten = s,3,Wait(1)
		exten = s,4,Background(thank-you-for-calling)
		exten = 2,1,Goto(default|6000|1) // keypress Action
		include = default

	is parsed as 
	vm = {
		comment : 'Welcome VoiceMenu', 	// label given to this voicemenu (gui-only field )
		alias_exten : '6070,1,Goto(voicemenu-custom-1|s|1)' ,   // if this voicemenu is given a local extension
		includes : ['default'] , // contexts included in this voicemenu
		steps : [ 'NoOp(Welcome VoiceMenu)', 'Answer',  'Wait(1)',  'Background(thank-you-for-calling)' ]  , // sequence of steps - the 's' extension
		keypressEvents : {
			// key press events - assumption is that each key press action is defined in a single line, all priorities greater than 1 are ignored
			// if you want a sequence of steps to be executed on a keypress, build another menu with all the steps & point the key to that menu
			2 : 'Goto(default|6000|1)'
		}
	};
	*/

	parseContext: function(cxt){ // takes array 'cxt' as input, parses cxt array and returns a Object (standard VoiceMenu structure)
		var vm = {
			comment:'',
			alias_exten:'',
			includes:[],
			steps:[],
			keypressEvents: { 0:'', 1:'', 2:'', 3:'', 4:'', 5:'', 6:'', 7:'', 8:'', 9:'', '#':'' , '*':'', t:'', i: ''}
		};
		try{
			var TMP_STEPS = ASTGUI.sortContextByExten(cxt, true);
			TMP_STEPS['s'].forEach( function(step){
				return ASTGUI.parseContextLine.getAppWithArgs(step);
			});
			vm.steps = TMP_STEPS['s'];
			vm.comment = vm.steps[0].getNoOp();
			['0','1','2','3','4','5','6','7','8','9','*','#','t','i'].each( function(this_key){
				if( TMP_STEPS.hasOwnProperty(this_key) && TMP_STEPS[this_key].length==1 ){
					vm.keypressEvents[this_key] = ASTGUI.parseContextLine.getAppWithArgs(TMP_STEPS[this_key][0]) ;
				}
			});
	
			cxt.each( function(line , cxt_index) {
				if( line.beginsWith('include=') ){
					vm.includes.push( line.afterChar('=') );
					return true;
				}
			});
		}catch(err){
			top.log.debug('Error Parsing VoiceMenu');
		}finally{
			return ASTGUI.toCustomObject(vm);
		}
	},

	addMenu: function(new_name, new_menu, cbf){ // Creates a New Voicemenu 'new_name' with a standard VoiceMenu structure of 'new_menu'
		var x = new listOfActions();
		x.filename('extensions.conf');
		x.new_action('delcat', new_name , '', ''); // not really needed - but just in case
		x.new_action('newcat', new_name , '', ''); // create new VoiceMenu
		new_menu.includes.each( function(item) { // usually ['default'] or just [] 
			x.new_action( 'append', new_name , 'include', item );
		});
		if( new_menu.alias_exten ){ // add 'exten = 7000,1,Goto(voicemenu-custom-1|s|1)' in  context 'voicemenus'
			if( !new_menu.alias_exten.contains(',') || !new_menu.alias_exten.toLowerCase().contains('goto(') ){// if new_menu.alias_exten is '4444'
				new_menu.alias_exten = new_menu.alias_exten.lChop('exten=') + ',1,Goto(' + new_name + ',s,1)' ;
			}
			x.new_action( 'append', ASTGUI.contexts.VoiceMenuExtensions , 'exten', new_menu.alias_exten );
		}
		new_menu.steps.each( function(step, i) { // note that first step should be NoOp(Comment)
			if( !step.beginsWith('s,') ){
				step = 's,' + (i+1) + ',' + step ;
			}
			x.new_action( 'append', new_name , 'exten', step );
		});
		for( var y in new_menu.keypressEvents){
			if(!new_menu.keypressEvents.hasOwnProperty(y) || new_menu.keypressEvents[y] == '' ){
				continue;
			}
			var kext = y + ',1,' + new_menu.keypressEvents[y] ;
			x.new_action( 'append', new_name , 'exten', kext );
		}
		var cb = function(){
			sessionData.pbxinfo.voicemenus[new_name] = ASTGUI.toCustomObject(new_menu) ; // update VoiceMenu info in sessionData :)
			cbf();
		};
		x.callActions(cb) ;
	},

	deleteMenu: function(menu_name){ // deletes voicemenu 'menu_name'
		var v = new listOfSynActions('extensions.conf') ;
		v.new_action('delcat', menu_name, '', ''); 
		if( sessionData.pbxinfo.voicemenus[menu_name]['alias_exten'] != '' ){
			var aext = sessionData.pbxinfo.voicemenus[menu_name]['alias_exten'].lChop('exten=') ;
			v.new_action('delete', ASTGUI.contexts.VoiceMenuExtensions , 'exten', '', aext);
			v.new_action('delete', 'default' , 'exten', '', aext); // backward compatibility with gui 1.x
		}
		var g = v.callActions();
		if( sessionData.pbxinfo.voicemenus.hasOwnProperty(menu_name) ) delete sessionData.pbxinfo.voicemenus[menu_name] ;
		return true;
	},

	nextAvailableVM_x : function(){ // astgui_manageVoiceMenus.nextAvailableVM_x();
		var x = [], y = sessionData.pbxinfo.voicemenus.getOwnProperties();
		y.each( function( item ){
			if( item.beginsWith(ASTGUI.contexts.VoiceMenuPrefix) ){ x.push( item.split(ASTGUI.contexts.VoiceMenuPrefix)[1] ); }
		} );
		if( !x.length ){ return ASTGUI.contexts.VoiceMenuPrefix + '1' ; }
		return ASTGUI.contexts.VoiceMenuPrefix + x.firstAvailable() ;
	}
};






astgui_manageConferences = {
	/* 
	The GUI creates/expects conference rooms in the following format	
		extensions.conf
			[ASTGUI.contexts.CONFERENCES]
			exten => 6001,1,MeetMe(${EXTEN}|MsIwq)
			exten => 6002,1,MeetMe(6001|MsIwqaA) // 6001's extension for admin/marked users
		meetme.conf 
			[rooms]
			conf = 6001,4567,7654 
		//6001 is the conference number, 4567 is the password to join, 7654 is the adminpwd

	the above conference room is stored in the data structure as
		sessionData.pbxinfo.conferences['6001'] = { configOptions : '6001,1,MeetMe(${EXTEN}|MsIwq)' ,adminOptions:'6002,1,MeetMe(${EXTEN}|MsIwqaA)',  pwdString : '6001,4567,7654' }
	*/

	loadMeetMeRooms: function(){
		// parses the context rooms [ASTGUI.contexts.CONFERENCES] in extensions.conf 
		var cxt = context2json({ filename:'extensions.conf' , context : ASTGUI.contexts.CONFERENCES , usf:0 }) ;
		if( cxt === null ){ // context not found
				var w = new listOfSynActions('extensions.conf') ;
				w.new_action('newcat', ASTGUI.contexts.CONFERENCES , '', '') ;
				w.callActions();
				cxt = [];
		}
		cxt.each(function(line){ // line is in the format 'conf=6001,4567,7654'
			if(!line.beginsWith('exten=') ){ return;}
			var b = ASTGUI.parseContextLine.getExten(line) ;
			var configOptions = line.afterChar('=');
			var params = configOptions.split("${EXTEN}")[1].betweenXY('|',')');
			if( params.contains('a') &&  params.contains('A') ) { // if is a meetMe Admin Login
				b = ASTGUI.parseContextLine.getArgs(line)[0] ;
			}
			if( !sessionData.pbxinfo.conferences.hasOwnProperty(b) ){
				sessionData.pbxinfo.conferences[b] = new ASTGUI.customObject ;
				sessionData.pbxinfo.conferences[b]['configOptions'] = '' ;
				sessionData.pbxinfo.conferences[b]['adminOptions'] = '' ;
				sessionData.pbxinfo.conferences[b]['pwdString'] = '' ;
			}
			if( params.contains('a') &&  params.contains('A') ) { // if is a meetMe Admin Login
				sessionData.pbxinfo.conferences[b]['adminOptions'] = configOptions;
			}else{
				sessionData.pbxinfo.conferences[b]['configOptions'] = configOptions;
			}
		});

		var pwds = context2json({ filename:'meetme.conf' , context : 'rooms' , usf:0 }) ;
		if(pwds === null ){ // context not found
				var w = new listOfSynActions('meetme.conf') ;
				w.new_action('newcat', 'rooms', '', '') ;
				w.callActions();
				pwds = [];
		}
		pwds.each(function(line){
			if(!line.beginsWith('conf=') ){ return;}
			var b = line.betweenXY('=',',') ;
				b = b.trim();
			if( !sessionData.pbxinfo.conferences.hasOwnProperty(b) ){
				sessionData.pbxinfo.conferences[b] = new ASTGUI.customObject ;
				sessionData.pbxinfo.conferences[b]['configOptions'] = '';
			}
			sessionData.pbxinfo.conferences[b]['pwdString'] = line.afterChar('=');
		});
	},

	getList: function(){ // astgui_manageConferences.getList(); returns an array with the list of conference room extensions
		if( !sessionData.pbxinfo.hasOwnProperty('conferences') ){return [];}
		var y = [];
		for( var x in sessionData.pbxinfo.conferences ){ if( sessionData.pbxinfo.conferences.hasOwnProperty(x) ){ y.push(x); } }
		return y;
	}

};





astgui_manageQueues = {
/* QueueDefinition
	extensions.conf
		[ASTGUI.contexts.QUEUES]
		exten = 6000,1,Queue(${EXTEN}) 
	queues.conf
		[6000]
		fullname = QueueName 
		strategy = roundrobin
		timeout = 100
		wrapuptime = 100
		autofill = yes
		autopause = yes
		maxlen = 100
		joinempty = yes
		leavewhenempty = yes
		reportholdtime = yes
		musicclass = default
		member = Agent/6001
		member = Agent/6002

and the data structure is 
	sessionData.pbxinfo.queues[6000] = {
		configLine: '6000,1,Queue(${EXTEN})'
	}
*/
	loadQueues: function(){ //sessionData.pbxinfo.queues[queue_exten]
		var cxt = context2json({ filename:'extensions.conf' , context : ASTGUI.contexts.QUEUES , usf:0 }) ;
		if( cxt === null ){ // context not found
				var w = new listOfSynActions('extensions.conf') ;
				w.new_action('newcat', ASTGUI.contexts.QUEUES , '', '') ;
				w.callActions();
				cxt = [];
		}
		cxt.each(function(line){ // line is in the format 'exten=6000,1,Queue(${EXTEN})'
			if(!line.beginsWith('exten=') ){ return;}
			var b = ASTGUI.parseContextLine.getExten(line) ;
			var configLine = line.afterChar('=');
			if( !sessionData.pbxinfo.queues.hasOwnProperty(b) ){
				sessionData.pbxinfo.queues[b] = new ASTGUI.customObject;
			}
			sessionData.pbxinfo.queues[b]['configLine'] = configLine;
		});
	}
};





astgui_manageRingGroups = {
/*
	[ringgroups] ; <-- ASTGUI.contexts.RingGroupExtensions in astman.js
	exten = 2345,1,Goto(ringroups-custom-X|s|1)

	[ringroups-custom-X]
	exten = s,1,NoOp(RINGGROUPNAME) ; <-- gui expects the ringgroup name , if not found name will be 'ringgroup X'
	exten = s,n,Dial(Zap/1,30,i)
	exten = s,n,Dial(Zap/4,30,i)
	exten = s,n,Dial(IAX2/6000,30,i)
	exten = s,n,Hangup

	sessionData.pbxinfo['ringgroups']['ringroups-custom-X'] = {
		NAME : RINGGROUPNAME,
		strategy : 'ringinorder',
		members : ['Zap/1','Zap/4', 'IAX2/6000']
		extension : '2345' ,
		ringtime : '30',
		fallback : 'Hangup'
	}

*/
	parseContext: function(cxtname, cxt, rgextns){ // parses array cxt and updates "sessionData.pbxinfo.ringgroups[cxtname]"
		var rg = new ASTGUI.customObject;
			rg.NAME = '';
			rg.members = [];
			rg.strategy = '';
			rg.ignore = true ;

		if( cxt[0].contains('exten=s,1') &&  cxt[0].toLowerCase().contains('noop(')  ){
			rg.NAME = cxt[0].betweenXY( '(' , ')' );
			cxt.splice(0,1);
		}else{
			rg.NAME = 'RingGroup ' + cxtname.withOut(ASTGUI.contexts.RingGroupPrefix);
		}

		var dialcount = 0;
		cxt.each( function(line) {
			if( line.beginsWith('gui_ring_groupname=') ){ // old gui ring group name
				rg.NAME = line.afterChar('=');
				return;
			}
			if( line.toLowerCase().contains('dial(') ){
				dialcount++;
				var args = ASTGUI.parseContextLine.getArgs( line );
				if ( args[0].contains('&') ){
					rg.members = rg.members.concat( args[0].split('&') );
				}else{
					rg.members.push( args[0] );
				}
				rg.ringtime = ( args[1] );
				rg.ignore =  ( args[2] &&  args[2].contains('i') ) ? true : false ;
			}
		});

		rg.strategy = (dialcount > 1) ? 'ringinorder':'ringall' ;
		var lastline = cxt[cxt.length -1].toLowerCase();
		if( (!lastline.contains('dial(')) && lastline.beginsWith('exten=s,n') ){
			rg.fallback = cxt[cxt.length -1].split('=s,n,')[1] ;
		}

		for(var u=0, v = rgextns.length; u < v ; u++ ){
			if( rgextns[u].contains(cxtname + '|') || rgextns[u].contains(cxtname + ',') ){
				rg.extension = ASTGUI.parseContextLine.getExten(rgextns[u]);
				break;
			}
		}

		return rg;
	},

	getRGsList: function(){ // astgui_manageRingGroups.getRGsList();
		var rgl = [] ;
		var c = sessionData.pbxinfo.ringgroups;
		for(var d in c){
			if(c.hasOwnProperty(d)){
				rgl.push(d);
			}
		}
		return rgl ;
	},

	nextAvailableRG_x : function(){ // astgui_manageRingGroups.nextAvailableRG_x();
		var x = [], y = this.getRGsList() ;
		y.each( function( item ){
			if( item.beginsWith(ASTGUI.contexts.RingGroupPrefix) ){ x.push( item.split(ASTGUI.contexts.RingGroupPrefix)[1] ); }
		} );
		if( !x.length ){ return ASTGUI.contexts.RingGroupPrefix + '1' ; }
		return ASTGUI.contexts.RingGroupPrefix + x.firstAvailable() ;
	},

	createNewRg: function( rg , callback , newName ){
		// astgui_manageRingGroups.createNewRg(rg , callback , newName );
		// rg is a standard ring group object , callback is callback function, newName(optional) to create with a specific contextname
		var newrg = newName || this.nextAvailableRG_x() ;
		if( !rg.fallback ){
			rg.fallback = 'Hangup'
		}
		var tmp_ignore = (rg.ignore) ? '${DIALOPTIONS}i' : '${DIALOPTIONS}' ;
		var x = new listOfActions();
		x.filename('extensions.conf');
		x.new_action('newcat', newrg , '', '');
		x.new_action('append', newrg, 'exten', 's,1,NoOp(' + rg.NAME  + ')' );
		if( rg.strategy == 'ringinorder' ){
			rg.members.each(
				function(member){
					x.new_action('append', newrg, 'exten', 's,n,Dial(' + member +',' + rg.ringtime + ','+ tmp_ignore + ')' );
				}
			);
		}else{
			if(rg.members.length){
				x.new_action('append', newrg, 'exten', 's,n,Dial(' + rg.members.join('&') +',' + rg.ringtime + ',' + tmp_ignore + ')');
			}
		}
		x.new_action( 'append', newrg, 'exten', 's,n,' + rg.fallback );
		var after = function() {
			if( rg.extension ){
				var u = new listOfSynActions('extensions.conf') ;
				u.new_action( 'append', ASTGUI.contexts.RingGroupExtensions , 'exten',  rg.extension + ',1,Goto(' + newrg + ',s,1)' );
				u.callActions();
			}
			sessionData.pbxinfo.ringgroups[newrg] = rg ;
			callback();
		};
		x.callActions( after );
	},

	deleteRg : function(rgname){
		// astgui_manageRingGroups.deleteRg(rgname)
		// rgname is the actual ringgroup context name - like 'ASTGUI.contexts.RingGroupPrefix + 1'
		var u = new listOfSynActions('extensions.conf') ;
		u.new_action('delcat', rgname , '', '');
		if( sessionData.pbxinfo.ringgroups[rgname].extension ){
			var f = sessionData.pbxinfo.ringgroups[rgname].extension ;
			u.new_action( 'delete', ASTGUI.contexts.RingGroupExtensions , 'exten', '', f + ',1,Goto(' + rgname + ',s,1)' ) ;
			if( sessionData.pbxinfo.ringgroups[rgname].hasOwnProperty('isOLDRG') && sessionData.pbxinfo.ringgroups[rgname].isOLDRG == true ){
				u.new_action( 'delete', 'default' , 'exten', '', f + ',1,Goto(' + rgname + '|s|1)' ) ;
			}
		}

		u.callActions();
		delete sessionData.pbxinfo.ringgroups[rgname] ;
	}
};

astgui_managePageGroups = {
	getPGsList : function(){ // astgui_managePageGroups.getPGsList();
 		var tmp_pgContext = sessionData.pbxinfo['pagegroups'] ;
		var tmp_pgExtens = [];
		tmp_pgContext.each(function(this_line){
			var t = ASTGUI.parseContextLine.getExten(this_line);
			if(t){ tmp_pgExtens.push(t); }
		});
		return tmp_pgExtens;
	},
	updatePGsCache : function(cbf){
		setTimeout(function(){
			sessionData.pbxinfo['pagegroups'] = context2json({filename:'extensions.conf', context: ASTGUI.contexts.PageGroups , usf:0});
			cbf();
		}, 1000);
	},
	addPageGroup : function(new_pg_line, cb){ // astgui_managePageGroups.addPageGroup( new_pg_line, cbf );
		var x = new listOfSynActions('extensions.conf');
		x.new_action('append', ASTGUI.contexts.PageGroups , 'exten', new_pg_line);
		x.callActions();
		this.updatePGsCache(cb);
	},
	deletePageGroup : function( pgexten , cb ){ // astgui_managePageGroups.deletePageGroup( pgexten, cbf );
		var AF = this.updatePGsCache ;
		ASTGUI.miscFunctions.delete_LinesLike({ context_name : ASTGUI.contexts.PageGroups , beginsWithArr: [ 'exten=' + pgexten + ',' ] , filename: 'extensions.conf', hasThisString:'Macro(', cb:function(){AF(cb);}});
	}
};

astgui_manageVMgroups = {
	/*
		[ASTGUI.contexts.VoiceMailGroups]
		exten = 6600,1, NoOp(Sales_VoiceMailGroup)
		exten = 6600,2, VoiceMail(6001@default&6003@default&6010@default)
		
			is stored as 

		sessionData.pbxinfo['vmgroups'][6600] = {
			label : 'Sales_VoiceMailGroup',
			mailboxes : ['6001','6003','6010']
		}
	*/
	addVMGroup: function( vmg_exten, vmg ){ 
		// add voicemail group - astgui_manageVMgroups.addVMGroup('6600', {label:'Sales_VoiceMailGroup', mailboxes:['6001','6003','6010']});
		var line_1 = vmg_exten + ',1,NoOp(' + vmg.label + ')' ;
		var line_2 = vmg_exten + ',2,VoiceMail(' + vmg.mailboxes.join('@default&') + '@default' + ')' ;

		var x = new listOfSynActions('extensions.conf');
		x.new_action( 'append', ASTGUI.contexts.VoiceMailGroups , 'exten', line_1);
		x.new_action( 'append', ASTGUI.contexts.VoiceMailGroups , 'exten', line_2);
		x.callActions();

		sessionData.pbxinfo['vmgroups'][vmg_exten] = vmg ;
	},

	parseContext: function(vmg_context){ // parse voicemail groups -- astgui_manageVMgroups.parseContext(cxt);
		try{
			var new_vm_group = function(){
				var a = new ASTGUI.customObject ; a.label = '' ; a.mailboxes = [] ;
				return a ;
			};
			vmg_context.each( function(line){
				if( line.toLowerCase().contains('noop(')  ){
					var tmp_vmgroupname = line.getNoOp();
					var tmp_vmgroup_exten = ASTGUI.parseContextLine.getExten( line );
					if ( !sessionData.pbxinfo.vmgroups.hasOwnProperty(tmp_vmgroup_exten) ){
						sessionData.pbxinfo.vmgroups[tmp_vmgroup_exten] = new_vm_group();
					}
					sessionData.pbxinfo.vmgroups[tmp_vmgroup_exten]['label'] = tmp_vmgroupname ;
				}
				if( line.toLowerCase().contains('voicemail(')  ){
					var tmp_vmgroup_exten = ASTGUI.parseContextLine.getExten( line );
					if ( !sessionData.pbxinfo.vmgroups.hasOwnProperty(tmp_vmgroup_exten) ){
						sessionData.pbxinfo.vmgroups[tmp_vmgroup_exten] = new_vm_group();
					}
					var tmp_vmmembers_String = ASTGUI.parseContextLine.getArgs(line)[0];
					tmp_vmmembers_String.split('&').each(
						function( this_memberstring ){
							this_memberstring = this_memberstring.trim();
							if( this_memberstring ){
								sessionData.pbxinfo.vmgroups[tmp_vmgroup_exten]['mailboxes'].push( this_memberstring.beforeChar('@').trim() );
							}
						}
					);
				}
			});
		}catch(err){ }
	},

	deleteVMGroup: function(vmg_exten){ // delete voicemail group -- astgui_manageVMgroups.deleteVMGroup('6050');
		ASTGUI.miscFunctions.delete_LinesLike(
			{
				context_name : ASTGUI.contexts.VoiceMailGroups,
				beginsWithArr:  [ 'exten=' + vmg_exten + ','   ,  'exten=' + vmg_exten + ' ,' ],
				filename: 'extensions.conf',
				cb: function(){}
			}
		);

		delete sessionData.pbxinfo.vmgroups[vmg_exten] ;
	}
	// get_ListOfVMGroup_Extensions: function(){
	// 	return sessionData.pbxinfo.vmgroups.getOwnProperties();
	// }
};


astgui_updateConfigFromOldGui = function(){
	// upgrades old GUI Configuration to New Gui

	var n = $.ajax({ url: ASTGUI.paths.rawman+'?action=getconfig&filename=' + ASTGUI.globals.configfile , async: false }).responseText;

	var n_Lower = n.toLowerCase();
	if( n_Lower.contains('response: error') && n_Lower.contains('message: config file not found') ){
		parent.ASTGUI.systemCmd( 'touch ' + top.sessionData.directories.asteriskConfig + ASTGUI.globals.configfile , function(){
			if( top.sessionData.DEBUG_MODE ){
				alert('Creating' + ASTGUI.globals.configfile + ' :: astgui_updateConfigFromOldGui() \n Click OK to Reload');
			}
			top.window.location.reload();
		});
		return false;
	}

	var guiprefs = config2json({ configFile_output: n, usf : 1 });
	if( !guiprefs.hasOwnProperty('general') ){
		var u = new listOfSynActions(ASTGUI.globals.configfile) ;
		u.new_action('newcat', 'general' , '', '') ;
		u.callActions() ;
		guiprefs_general = {} ;
	}else{
		guiprefs_general = guiprefs['general'] ;
	}
	
	if ( guiprefs_general.hasOwnProperty('config_upgraded') && guiprefs_general['config_upgraded'] == 'yes' ){
		// already upgraded, DO NOTHING
		return true;
	}

	var do_Upgrade = function(){
		var upgrade_alert = '';

		if ( sessionData.PLATFORM.isAA50 ){
			var upgrade_alert = "Your configuration will now be upgraded to work with this version of the AA50 firmware.\n"
			+ "An automatic backup of your old configuration is created and is available from the 'Backup' panel.\n"
			+ "If you downgrade your firmware to a previous version, you will need to restore this configuration.";
		}else{
			var upgrade_alert = "Your configuration will now be upgraded to work with the latest version of GUI. \n"
			+ "An automatic backup of your old configuration is available from the backups panel.";
		}

		if(upgrade_alert){
			alert( upgrade_alert );
		}

		parent.ASTGUI.dialog.waitWhile('Upgrading Configuration files .. ');

		var sa = new listOfActions('extensions.conf') ;
		var SU = new listOfActions('users.conf') ;

		var ext_conf = config2json({ filename:'extensions.conf', usf:0 });
	
		if( !ext_conf.hasOwnProperty(ASTGUI.contexts.CONFERENCES) ){
			sa.new_action('newcat', ASTGUI.contexts.CONFERENCES, '', '');
		};
		if( !ext_conf.hasOwnProperty(ASTGUI.contexts.RingGroupExtensions) ){
			sa.new_action('newcat', ASTGUI.contexts.RingGroupExtensions, '', '');
		};
		if( !ext_conf.hasOwnProperty(ASTGUI.contexts.QUEUES) ){
			sa.new_action('newcat', ASTGUI.contexts.QUEUES, '', '');
		};
		if( !ext_conf.hasOwnProperty(ASTGUI.contexts.VoiceMenuExtensions) ){
			sa.new_action('newcat', ASTGUI.contexts.VoiceMenuExtensions, '', '');
		};
	
		for ( var catname in ext_conf ){
			if( !ext_conf.hasOwnProperty(catname) || !catname.beginsWith('ringroups-custom-') ) continue;
			// OLD
			//	[ringroups-custom-1]
			//	gui_ring_groupname = SomeRG
			//	exten = s,1,NoOp(RINGGROUP)
			//	exten = s,n,Dial(SIP/6000,20,i)
			//	exten = s,n,Hangup
			// 
			// New
			//	[ringroups-custom-1]
			//	exten = s,1,NoOp(SomeRG)
			//	exten = s,n,Dial(SIP/6000,20,i)
			//	exten = s,n,Hangup
			var this_RG = ext_conf[catname];
			var tmp_oldRGName = '';
			this_RG.each( function( this_RG_line ){
				if( this_RG_line.beginsWith('gui_ring_groupname =') ){
					tmp_oldRGName = this_RG_line.afterChar('=');
					sa.new_action( 'delete', catname  , 'gui_ring_groupname', '', tmp_oldRGName );
				}
	
				if( tmp_oldRGName && this_RG_line.endsWith('s,1,NoOp(RINGGROUP)') ){
					var tmp_toReplace = this_RG_line.afterChar('=');
					sa.new_action( 'update', catname  , 'exten', tmp_toReplace.replaceXY('RINGGROUP', tmp_oldRGName) , tmp_toReplace );
				}
			});
		}
	
		var default_Context = ext_conf['default'] ;
		default_Context.each( function( this_line ){
			if(!this_line.beginsWith('exten=') ){ return ; }
			var match_str = this_line.afterChar('=');
	
			if ( this_line.contains('MeetMe(${EXTEN}') ){
			// Move any conferences into [conferences]
				// old
				// 	[default]
				// 	exten => 6000,1,MeetMe(${EXTEN}|MI) // delete this line
						sa.new_action('delete', 'default' , 'exten', '', match_str );
				// new
				//	[conferences]
				// 	exten => 6000,1,MeetMe(${EXTEN}|MI)
					sa.new_action('append', ASTGUI.contexts.CONFERENCES , 'exten', match_str );
	
				// old
				//	[voicemenu-custom-?] 
				//	exten = ????,?,Goto(default|6000|1)
				// 
				// new
				//	[voicemenu-custom-?] 
				//	exten = ????,?,Goto(conferences|6000|1)
				var tmp_exten = ASTGUI.parseContextLine.getExten(match_str);
				var tmp_oldMM_gotoStr = 'Goto(default,' + tmp_exten + ',1)' ;
	
				for ( var catname in ext_conf ){
					if( !ext_conf.hasOwnProperty(catname) ) continue;
					if( catname.beginsWith('DID_') ){
						if( catname.contains( '_' + ASTGUI.contexts.TimeIntervalPrefix ) || catname.endsWith(ASTGUI.contexts.TrunkDefaultSuffix) ){ continue; }
					}else if( catname.beginsWith('voicemenu-custom-') ){
	
					}else{
						continue;
					}
	
					var this_menu = ext_conf[catname] ;
					this_menu.each( function( this_menu_line ){
						if( this_menu_line.contains( tmp_oldMM_gotoStr ) ){
							var tmp_toReplace = this_menu_line.afterChar('=');
							var tmp_ReplaceWith = tmp_toReplace.replaceXY(tmp_oldMM_gotoStr, 'Goto(' + ASTGUI.contexts.CONFERENCES +',' + tmp_exten + ',1)' );
							sa.new_action( 'update', catname , 'exten', tmp_ReplaceWith , tmp_toReplace );
						}
					});
				}
	
				return;
			}
	
			if ( this_line.contains('Goto(ringroups-custom-') ){
				var THIS_RGNAME = this_line.betweenXY( '(' , ')' ) ;
				// Move Any Ring groups
					// old
					// 	[default]
					// 	exten = 2345,1,Goto(ringroups-custom-1|s|1)
					sa.new_action('delete', 'default' , 'exten', '', match_str );
	
				// new
				//	[ringgroups]
				// 	exten = 2345,1,Goto(ringroups-custom-1|s|1)
					sa.new_action('append', ASTGUI.contexts.RingGroupExtensions , 'exten', match_str );
	
	
				// old
				//	[voicemenu-custom-?] 
				//	exten = ????,?,Goto(default|2345|1)
				// 
				// new
				//	[voicemenu-custom-?] 
				//	exten = ????,?,Goto(ringroups-custom-1|s|1)
				var tmp_exten = ASTGUI.parseContextLine.getExten(match_str);
				var tmp_oldRG_gotoStr = 'Goto(default|' + tmp_exten + '|1)' ;
				for ( var catname in ext_conf ){
					if( !ext_conf.hasOwnProperty(catname) ) continue;
					if( catname.beginsWith('DID_') ){
						if( catname.contains( '_' + ASTGUI.contexts.TimeIntervalPrefix ) || catname.endsWith(ASTGUI.contexts.TrunkDefaultSuffix) ){ continue; }
					}else if( catname.beginsWith('voicemenu-custom-') ){
	
					}else{
						continue;
					}
	
					var this_menu = ext_conf[catname] ;
					this_menu.each( function( this_menu_line ){
						if( this_menu_line.contains( tmp_oldRG_gotoStr ) ){
							var tmp_toReplace = this_menu_line.afterChar('=');
							var tmp_ReplaceWith = tmp_toReplace.replaceXY( tmp_oldRG_gotoStr, 'Goto(' + THIS_RGNAME +',s,1)' );
							sa.new_action( 'update', catname , 'exten', tmp_ReplaceWith , tmp_toReplace );
						}
					});
				}
				return;
			}

			if ( this_line.contains('agentlogin') || this_line.contains('agentcallbacklogin') ){
				sa.new_action('delete', 'default' , 'exten', '', match_str );
				sa.new_action('append', ASTGUI.contexts.QUEUES , 'exten', match_str );
				return;
			}
	
			if ( this_line.contains('Queue(${EXTEN})') ){
				// Move any Queues
					// old
					//	[default]
					//	exten = 6003,1,Queue(${EXTEN})
					sa.new_action('delete', 'default' , 'exten', '', match_str );
			
					// new
					//	[queues]
					//	exten = 6003,1,Queue(${EXTEN})
					sa.new_action('append', ASTGUI.contexts.QUEUES , 'exten', match_str );
	
					// old
					//	[voicemenu-custom-?] 
					//	exten = ????,?,Goto(default|6003|1)
					// 
					// new
					//	[voicemenu-custom-?] 
					//	exten = ????,?,Goto(queues|6003|1)
					var tmp_exten = ASTGUI.parseContextLine.getExten(match_str);
					var tmp_oldQ_gotoStr = 'Goto(default|' + tmp_exten + '|1)' ;
					for ( var catname in ext_conf ){
					if( !ext_conf.hasOwnProperty(catname) ) continue;
						if( catname.beginsWith('DID_') ){
							if( catname.contains( '_' + ASTGUI.contexts.TimeIntervalPrefix ) || catname.endsWith(ASTGUI.contexts.TrunkDefaultSuffix) ){ continue; }
						}else if( catname.beginsWith('voicemenu-custom-') ){
		
						}else{
							continue;
						}
	
						var this_menu = ext_conf[catname] ;
						this_menu.each( function( this_menu_line ){
							if( this_menu_line.contains( tmp_oldQ_gotoStr ) ){
								var tmp_toReplace = this_menu_line.afterChar('=');
								var tmp_ReplaceWith = tmp_toReplace.replaceXY(tmp_oldQ_gotoStr, 'Goto(' + ASTGUI.contexts.QUEUES +',' + tmp_exten + ',1)' );
								sa.new_action( 'update', catname , 'exten', tmp_ReplaceWith , tmp_toReplace );
							}
						});
					}
				return;
			}
	
			if ( this_line.contains('Goto(voicemenu-custom-') ){
				// Move any Voice Menus
					// old
					//	[default]
					//	exten = 7000,1,Goto(voicemenu-custom-1|s|1)
					sa.new_action('delete', 'default' , 'exten', '', match_str );
	
				// new
				//	[voicemenus]
				//	exten = 7000,1,Goto(voicemenu-custom-1|s|1)
					sa.new_action('append', ASTGUI.contexts.VoiceMenuExtensions , 'exten', match_str );

					var tmp_exten = ASTGUI.parseContextLine.getExten(match_str);
					var tmp_oldVM_gotoStr = ASTGUI.parseContextLine.getAppWithArgs(match_str);
					for ( var catname in ext_conf ){
						if( !ext_conf.hasOwnProperty(catname) ) continue;
						if( catname.beginsWith('DID_') ){
							if( catname.contains( '_' + ASTGUI.contexts.TimeIntervalPrefix ) || catname.endsWith(ASTGUI.contexts.TrunkDefaultSuffix) ){ continue; }
						}else if( catname.beginsWith('voicemenu-custom-') ){
		
						}else{
							continue;
						}
		
						var this_menu = ext_conf[catname] ;
						this_menu.each( function( this_menu_line ){
							if( this_menu_line.contains( tmp_oldVM_gotoStr ) ){
								var tmp_toReplace = this_menu_line.afterChar('=');
								var tmp_ReplaceWith = tmp_toReplace.replaceXY( tmp_oldVM_gotoStr, 'Goto(' + ASTGUI.contexts.VoiceMenuExtensions + ',' + tmp_exten + ',1)' );
								sa.new_action( 'update', catname , 'exten', tmp_ReplaceWith , tmp_toReplace );
							}
						});
					}

				return;
			}
	
			if( this_line.endsWith(',VoiceMailMain') ){
				// VoiceMail Main
					// old
					// 	[default]
					//	exten => 6050,1,VoiceMailMain //delete
					sa.new_action('delete', 'default' , 'exten', '', match_str );
	
					// new
					// 	[default]
					//	exten = 6050,1,VoiceMailMain(${CALLERID(num)}@default)
					sa.new_action('append', 'default' , 'exten', match_str.replaceXY(',VoiceMailMain', ',VoiceMailMain(${CALLERID(num)}@default)' )  );
			}
		});
	
		//	for (var catname in ext_conf){
		//		if( !ext_conf.hasOwnProperty(catname) || !catname.beginsWith('voicemenu-custom-') ) continue;
		//	
		//	}
	
		for (var catname in ext_conf){
			// look for DID_trunk_x (ignore DID_TRUNK_timeinterval_some, ignore DID_trunk_x_default)
			// 	if there is no DID_trunk_x_default rename DID_trunk_x to DID_trunk_x_default
			//	create DID_TRUNK_x & include DID_trunk_x_default
	
			// old
			//	[DID_trunk_x]
			//	exten = _XXX,1,...
			//	exten = _YYY,1,...
	
			// new
			//	[DID_trunk_x_default]
			//	exten = _XXX,1,...
			//	exten = _YYY,1,...
			//
			//	[DID_trunk_x]
			//	include = DID_trunk_x_default
			//
	
			if( !ext_conf.hasOwnProperty(catname) || !catname.beginsWith('DID_') ) continue;
			if( catname.contains( '_' + ASTGUI.contexts.TimeIntervalPrefix ) || catname.endsWith(ASTGUI.contexts.TrunkDefaultSuffix) ) continue;
			if( !ext_conf.hasOwnProperty( catname + ASTGUI.contexts.TrunkDefaultSuffix ) ){
				sa.new_action('renamecat', catname, '', catname + ASTGUI.contexts.TrunkDefaultSuffix );
				sa.new_action('newcat', catname, '', '');
				sa.new_action('append', catname , 'include', catname + ASTGUI.contexts.TrunkDefaultSuffix );
			}
		}
	
		var USERS_CONF = config2json({ filename:'users.conf', usf:1 }) ;
		for (var catname in ext_conf){
			// Upgrade dialing rules and create dialplans in the new format
			if( !ext_conf.hasOwnProperty(catname) || !catname.beginsWith('numberplan-custom-') ) continue;
			var this_context = ext_conf[catname] ;
	
			(function(){
				sa.new_action('delcat', catname , '', '');
	
				var callingRULES_list = [];
				var tmp_index = this_context.indexOfLike('plancomment=') ;
				var DP_LABEL = ( tmp_index != -1 ) ? this_context[tmp_index].afterChar('=') : catname ;
				var DP_CONTEXT_NAME = ASTGUI.contexts.CallingPlanPrefix + DP_LABEL ;
		
				this_context.each(
					function(line){
						if( !line.beginsWith('exten=') ) return;
						var pattern = ASTGUI.parseContextLine.getExten( line );
						var macroargs = ASTGUI.parseContextLine.getArgs( line ); // macroname = macroargs[0] ;
						var t1 = ASTGUI.parseContextLine.parseTrunkDialArgument( macroargs[1] ) ; // t1.name (trunkname), t1.prepend, t1.stripx
						var tmp_index = this_context.indexOfLike( 'comment=' + pattern + ',' );
						var clr_name = ( tmp_index == -1 ) ? pattern : this_context[tmp_index].split(',')[2] ;
		
						// create a new context for this callingrule
						var newcr_Context_name = ASTGUI.contexts.CallingRulePrefix + clr_name ;
						callingRULES_list.push( newcr_Context_name );
						var newcr_string = pattern + ',1,Macro(' + ASTGUI.contexts.dialtrunks + ',' + macroargs[1] + ',,' + t1.name + ',)' ;
		
						sa.new_action('newcat', newcr_Context_name , '', '');
						sa.new_action('append', newcr_Context_name , 'exten', newcr_string );
					}
				) ;
		
				sa.new_action('newcat', DP_CONTEXT_NAME, '', '');
				callingRULES_list.each( function(clr){
					sa.new_action('append', DP_CONTEXT_NAME , 'include', clr);
				});
				// include all local contexts for any upgraded DP_Context
				ASTGUI.includeContexts.each( function( this_localContext ){
					sa.new_action('append', DP_CONTEXT_NAME , 'include', this_localContext);
				});

				(function(){
					// update any users with $catname as Context to $DP_CONTEXT_NAME
					for ( usr in USERS_CONF ){
						if( !USERS_CONF.hasOwnProperty(usr) ) continue;
						if( USERS_CONF[usr].hasOwnProperty('context') && USERS_CONF[usr].context == catname ){
							SU.new_action( 'update', usr , 'context', DP_CONTEXT_NAME , catname );
						}
					}
				})();
			})();
		}

		(function(){ // add linenumber=1 for any users that does not have it set (to support new resphoneprov )
			for ( usr in USERS_CONF ){
				if( !USERS_CONF.hasOwnProperty(usr) || usr == 'general' ) continue;
				if( USERS_CONF[usr].hasOwnProperty('hasexten') && USERS_CONF[usr].hasexten == 'no' ) continue; // skip any trunks
				if( !USERS_CONF[usr].hasOwnProperty('linenumber')){
					SU.new_action('append', usr , 'linenumber', '1');
				}
			}
		})();

		sa.callActions( function(){
			SU.callActions( function(){
				var u = ASTGUI.updateaValue({ file: ASTGUI.globals.configfile, context: 'general', variable: 'config_upgraded', value: 'yes' }) ;
				if( top.sessionData.DEBUG_MODE ){
					alert('Upgraded Configuration :: astgui_updateConfigFromOldGui() \n Click OK to Reload');
				}
				top.window.location.reload();
			});
		});
		// create new 'OutBound Calling Rules' out of numer plans
	};

	try{
		return false;
	}finally {
		if ( !sessionData.PLATFORM.isAA50 ){
			//Upgrade_backup_of_V1.1.1__2008Jul08.tar
			var months = ["jan", "feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"];
			var today=new Date();
			var year = today.getFullYear();
			var month = months[ today.getMonth() ];
			var day = today.getDate().addZero();
			var tmp_bkpFileName = ( sessionData.PLATFORM.isABE ) ? 'Upgrade_backup_of_C1.x__' + year + month + day + '.tar' :  'Upgrade_backup_before_GUI__' + year + month + day + '.tar' ;
	
			parent.ASTGUI.dialog.waitWhile('Taking Backup of current configuration ...');
			ASTGUI.systemCmd( "tar -cf " + top.sessionData.directories.ConfigBkp + tmp_bkpFileName + ' ' +  ' /etc/asterisk', function(){
				ASTGUI.feedback({ msg:'Backup Successful', showfor:2 });
				do_Upgrade();
			});
		}else{
			do_Upgrade();
		}
	}
};


var localajaxinit = function(){
	top.document.title = 'Asterisk Configuration' ;
	$.getScript( 'js/guiversion.js', function(){
		try{
			sessionData.gui_version = gui_version ;
			_$('parent_div_guiVersion').innerHTML = "<font color='#8d8d8d'>GUI-version : " + sessionData.gui_version + '</font>';
		}catch(err){}
		after_localajaxinit();
	});
};
