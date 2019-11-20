/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * hardware.html functions
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
var MODULE_NAME = 'wctdm24xxp';
var DRIVERS_LIST = [ 'tor2', 'wcfxo', 'wct1xxp', 'wct4xxp', 'wctc4xxp', 'wctdm24xxp', 'wctdm', 'wcte11xp', 'wcte12xp', 'wcusb', 'xpp_usb' ];

var SPANS = {};

var oldSpanCount = 0; 	// we get this from previuos applyzap.conf
var oldLoadZone;
var GROUPS = [];
var NEWGROUPNUMBER;
var CURRENT_SPAN; 	// span being edited
var CONFIGUREDHARDWARE = {}; // configuration read from an existing hwcfgfile (if exists)
var DETECTEDHARDWARE = {};
var hwcfgfile = ASTGUI.globals.hwcfgFile ;
var spans_todelete = [];  // we delete all the span_x entries in users.conf (and their extensions.conf counter parts) before writing back any new information.
var hwchanged = true ; // -1 for no previous configuration found (first time), true for detected hardware changes, false for no hardware changes
var HAS_ANALOGHARDWARE = true; var HAS_DIGITALHARDWARE = true; 
		// if the user does not have any hardware - always set parent.sessionData.REQUIRE_RESTART to false
var SPANCOUNT_LOCATION = {}; // this object is used to store the number of spans found in each location Ex: SPANCOUNT_LOCATION['PCI Bus 02 Slot 04'] = 4;
var portsSignalling = {};

var digital_miscFunctions = {
	show_analog_signalling_form: function(a){ //digital_miscFunctions.show_analog_signalling_form()
		var b = [], ct = _$('edit_analog_signalling_options_container');
		ct.FXSFXO = a ;
		ASTGUI.domActions.removeAllChilds(ct);

		if(a == 'FXO') b = parent.sessionData.FXO_PORTS_DETECTED;
		if(a == 'FXS') b = parent.sessionData.FXS_PORTS_DETECTED;

		var h_2_orig = document.createElement('select');
		ASTGUI.selectbox.append( h_2_orig, 'Kewl Start', 'ks');
		ASTGUI.selectbox.append( h_2_orig, 'Loop Start', 'ls');

		b.each( function(this_port){
			var h = document.createElement('div');
			var h_1 = document.createElement('span');
			var h_2 = h_2_orig.cloneNode(true);

			h_1.innerHTML = 'Port ' + this_port + '&nbsp;:&nbsp;' ;
			h_2.PORT = this_port;
			h_2.className = 'EASOC_PORT';
			h.appendChild(h_1);
			h.appendChild(h_2);
			if(  portsSignalling.ls.contains(this_port)  ){
				ASTGUI.selectbox.selectOption(h_2, 'ls');
			}else{
				ASTGUI.selectbox.selectOption(h_2, 'ks');
			}
			ct.appendChild(h);
		} );

		//console.log( portsSignalling.ls );
		//console.log( parent.sessionData.PORTS_SIGNALLING.ks );

		$('#edit_analog_signalling').showWithBg();
	},

	save_analog_signalling_prefs: function(){ // digital_miscFunctions.save_analog_signalling_prefs()
		var t = $('.EASOC_PORT') ;
		for(var i=0; i < t.length; i ++){
			//alert( t[i].PORT + ' : ' + t[i].value ) ;
			if(t[i].value == 'ls'){
				portsSignalling.ls.push_IfNotPresent( t[i].PORT );
				portsSignalling.ks = portsSignalling.ks.withOut( t[i].PORT ) ;
			}
			if(t[i].value == 'ks'){
				portsSignalling.ks.push_IfNotPresent( t[i].PORT );
				portsSignalling.ls = portsSignalling.ls.withOut( t[i].PORT ) ;
			}
		}
		var ct = _$('edit_analog_signalling_options_container');
		var ct_r = _$( 'FXSFXO_ports_td__' + ct.FXSFXO );
		ct_r.style.background = "#C9AAAA";
		$('#edit_analog_signalling').hideWithBg();
	}
};


function detectHwChanges(){ // compare DETECTEDHARDWARE vs CONFIGUREDHARDWARE 
// returns true if a hardware change is detected and false if there are no hardware changes
// we can actually check a lot of things here like
	// [A] check if - any cards are missing 
	//	if yes - delete all existing configuration and ask the user to reconfigure from scratch

	// [B] check if - any cards are added  - if something is added 
	//	- check if the basechan of configured hardware matches with basechan of detected hardware
	//	- if (matches) { 
	//		no need to delete existing configuration - just present the current card information as detected/configured and new card as unconfigured 
	//	}else{ 
	//		delete all existing configuration and ask the user to reconfigure from scratch 
	//	}

	// [C] if all the cards match - check if basechan of detected hardware matches with basechan of configured hardware
	//	- if does not match - delete all existing configuration and ask the user to reconfigure from scratch
// BUT to avoid all the complexity and to keep things simple (atleast for now) - we will just do [c]
	// check if the devices match 
	// check if the basechan match for all the devices

	var configured_devices = [];
	var detected_devices = [];
	for( var l in CONFIGUREDHARDWARE ){ if(CONFIGUREDHARDWARE.hasOwnProperty(l)){ 
		configured_devices.push( CONFIGUREDHARDWARE[l]['device'] + '::' + CONFIGUREDHARDWARE[l]['basechan'] + '::' + CONFIGUREDHARDWARE[l]['type'] ); 
		// this way we can check for whether 'device' and 'basechan' and 'type' all matched in one go
	}}
	for( var l in DETECTEDHARDWARE ){ if(DETECTEDHARDWARE.hasOwnProperty(l)){ 
		detected_devices.push( DETECTEDHARDWARE[l]['device'] + '::' + DETECTEDHARDWARE[l]['basechan'] + '::' + DETECTEDHARDWARE[l]['type']  ); 
	}}
	configured_devices.sort(); detected_devices.sort();
	if( !configured_devices.length && !detected_devices.length){ return false; }
	if(configured_devices.length == detected_devices.length){
		for(var l=0; l < configured_devices.length; l++){
			if(configured_devices[l] != detected_devices[l]){ // devices does not match - but the number of devices match
				top.log.debug("DEVICES or basechans does not MATCHED");
				return true;
			}
		}
		top.log.debug("DEVICES and basechans MATCH");
		return false;
	}else{	
		top.log.debug("DEVICES or basechans does not MATCHED");
		return true;
	}
}





function verify_priChLimit(){
	var l = String(CURRENT_SPAN);
	if( !_$('editspan_signalling').value.beginsWith('pri')){ return true;}
	var y = Number(_$('edit_DefinedChans').value);
	if( Number(SPANS[l]['totchans'] ) <= 24 && y ==24){return false;}//alert("You should leave atleast 1 channel for PRI signalling");
	if( Number(SPANS[l]['totchans'] ) > 24 && y ==31){return false;}//alert("You should leave atleast 1 channel for PRI signalling");
	return true;
};

function calc_bchan_fxx(l){ // calculates the bchan,and fxx strings for a given span
	var y = Number(SPANS[l]['min_ch']);
	var z = Number(SPANS[l]['definedchans']);
	if(z==1){return String(y);}

	if( SPANS[l]['signalling'] && !SPANS[l]['signalling'].beginsWith('pri') ){ // if is an fxo/fxs or e&m
		return y + "-" + String(y+z);
	}else{
		if(SPANS[l]['totchans'] <= 24){ // if T1
			return y + "-" + String(y+z-1);
		}else{ // if E1 - take first 15 as b-channels, then a d channel and then take the next 15 as bchannels
			if(z==16){ return y+"-"+String(y+14)+","+String(y+16); }
			if(z<= 15){
				return y+"-"+String(y+z-1);
			}else{
				return y+"-"+String(y+14)+","+String(y+16) +"-"+ String(y+z);
			}
		}
	}
}


function canelSpanInfo(){
	_$('edit_span').style.display = "none";
	ASTGUI.showbg(false);
	CURRENT_SPAN = null;
}

var editSPAN = function(l){ // show values for SPAN l in the edit_span dialog box
	CURRENT_SPAN = l;
	_$('editspan_SPAN').innerHTML = SPANS[l]['description'];
	_$('editspan_ALARMS').innerHTML = SPANS[l]['alarms'];

	// console.log( SPANS[l]['min_ch'] );
	// console.log( SPANS[l]['max_ch'] );
	// console.log( SPANS[l]['definedchans']  );

	ASTGUI.selectbox.clear( _$('editspan_fac') );
	var w = _$('edit_DefinedChans') ;

	var ijkl = function(){
		ASTGUI.selectbox.clear(w);
		var f = Number( SPANS[l]['totchans'] ), g=0;
		if(f == 31){ f = 30;} // always make sure that only a max of 30 ports are available on E1 
		for(g=1; g <=f; g++){ ASTGUI.selectbox.append(w,g,g); }
		ASTGUI.selectbox.selectOption(w,SPANS[l]['definedchans']);
		_$('edit_labelReserved').innerHTML = SPANS[l]['reserved_ch'];
		_$('edit_labelZapchan').innerHTML = calc_bchan_fxx(l);
	}();

	if ( Number(SPANS[l]['totchans']) <= 24 ){
		ASTGUI.selectbox.append(_$('editspan_fac'),'ESF/B8ZS', 'ESF/B8ZS');
		ASTGUI.selectbox.append(_$('editspan_fac'),'D4/AMI', 'D4/AMI');
	}else{
		ASTGUI.selectbox.append(_$('editspan_fac'),'CCS/HDB3', 'CCS/HDB3');
		ASTGUI.selectbox.append(_$('editspan_fac'),'CCS/CRC4/HDB3', 'CCS/CRC4/HDB3');
	}

	if(SPANS[l]['framing'] && SPANS[l]['coding']) {
		if( SPANS[l]['framing'] == 'CCS/CRC4' ){
			ASTGUI.selectbox.selectOption( _$('editspan_fac') , 'CCS/CRC4/HDB3' );
		}else{
			ASTGUI.selectbox.selectOption( _$('editspan_fac') , SPANS[l]['framing'] + '/' + SPANS[l]['coding'] );
		}
	}

	_$('editspan_channels').innerHTML = String(SPANS[l]['definedchans']) + "/" + String(SPANS[l]['totchans']) + " ("+SPANS[l]['spantype']+")";

	if( SPANS[l]['signalling'] ){
		ASTGUI.selectbox.selectOption( _$('editspan_signalling') , SPANS[l]['signalling'] );
	}else{
		_$('editspan_signalling').selectedIndex = -1 ; 
	}
	disablEnable_sc();
	if(SPANS[l]['switchtype']){
		ASTGUI.selectbox.selectOption( _$('editspan_switchtype') , SPANS[l]['switchtype'] );
	}else{
		_$('editspan_switchtype').selectedIndex = -1 ;
	}

	(function (){
		ASTGUI.selectbox.clear( _$('editspan_syncsrc'));
		var y = SPANCOUNT_LOCATION[ SPANS[l]['location'] ];
		var u =0; 
		while(u<=y){ ASTGUI.selectbox.append( _$('editspan_syncsrc'),u,u ); u++ }
		if( !SPANS[l].hasOwnProperty('syncsrc') ){ SPANS[l]['syncsrc'] = '1' } // default
		ASTGUI.selectbox.selectOption( _$('editspan_syncsrc') , SPANS[l]['syncsrc'] );
	})();

	ASTGUI.selectbox.selectOption( _$('editspan_lbo') , SPANS[l]['lbo'] );
	_$('edit_span').style.display = "";
	ASTGUI.showbg(true);
};


function showtable(){ // navigates through the SPANS object and presents as a table to the user
	var keepalert = false ;
	try{
		var tbl = _$('digitalcardstable') ;
		var add_fRow = function(){
			var newRow = tbl.insertRow(-1);
			newRow.className = "frow";
			ASTGUI.domActions.tr_addCell( newRow, {html: 'SPAN' } );
			ASTGUI.domActions.tr_addCell( newRow, {html: 'ALARMS' } );
			ASTGUI.domActions.tr_addCell( newRow, {html: 'Framing/Coding' } );
			ASTGUI.domActions.tr_addCell( newRow, {html: 'channels Used/Total', align : 'center' } );
			ASTGUI.domActions.tr_addCell( newRow, {html: 'Signalling', align : 'center'} );
			ASTGUI.domActions.tr_addCell( newRow, {html: '' } );
		};
		var addrow_totable = function(span){
			if( LAST_LOCATION != SPANS[span]['location'] ){
				var newRow = tbl.insertRow(-1);
				var tmp = '<B>' + SPANS[span]['location'] + ' --> ' + SPANS[span]['devicetype'] + '</B>' ;
				ASTGUI.domActions.tr_addCell( newRow, {html: tmp , colspan :6, bgcolor : '#B0B9D0' , align : 'center' } );
			}

			var singalling_defs = {
				pri_net: 'PRI - Net',
				pri_cpe: 'PRI - CPE',
				bri_net_ptmp: 'BRI PtP - Net',
				bri_cpe_ptmp: 'BRI PtP - CPE',
				bri_net: 'BRI PtP - Net',
				bri_cpe: 'BRI PtP - CPE',
				em: 'E & M',
				em_w: 'E & M -- Wink',
				featd:'E & M -- featd(DTMF)',
				fxo_ks:'FXOKS',
				fxo_ls:'FXOLS'
			};

			var sno = tbl.rows.length + 1;
			var newRow = tbl.insertRow(-1);
			newRow.className = ((tbl.rows.length)%2==1)?"odd":"even";
			newRow.id ="row" + span ;
			newRow["span_value"] = span;
			(function(){
				var u = SPANS[span]['name'].split('/'); // Ex: name=TE4/2/1 where 2 is card number and 1 is the span number on that card
				var w = SPANS[span]['devicetype'] ; // Ex: 'Wildcard TE410P/TE405P (1st Gen)'
				var v = w + ', Card ' + String(Number(u[1]) + 1) + ' - Port ' + u[2] + "&nbsp;(span_" + span + ")&nbsp;&nbsp;" ;
				ASTGUI.domActions.tr_addCell( newRow, { html: v } );// Ex: 'Wildcard TE410P/TE405P (1st Gen), Card 3 - Port 1, (span_3)'
			})();
			ASTGUI.domActions.tr_addCell( newRow, { html: SPANS[span]['alarms'] , align:'center' } );
			ASTGUI.domActions.tr_addCell( newRow, { html: (SPANS[span]['framing'] && SPANS[span]['coding']) ? SPANS[span]['framing'] + '/' + SPANS[span]['coding'] : '' } );
			ASTGUI.domActions.tr_addCell( newRow, { html: String(SPANS[span]['definedchans']) + '/' + String(SPANS[span]['totchans']) , align:'center' } );
			ASTGUI.domActions.tr_addCell( newRow, { html: (SPANS[span]['signalling'])? singalling_defs[ SPANS[span]['signalling'] ] : "<font color=red>NOT DEFINED</font>" , align:'center' } );
			ASTGUI.domActions.tr_addCell( newRow, { html: "<span class=\"guiButton\" id='" + "span_" + span + "'  onclick=\"editSPAN( '"+ span + "');\">Edit</span>" , align:'center', width: 90} );
			LAST_LOCATION = SPANS[span]['location'] ;
		};

		$('#digital_settings').show();
		ASTGUI.domActions.clear_table(tbl);
		add_fRow();
		var foo_spans =0;
		var LAST_LOCATION = '';
		for( var k in SPANS ){ if( SPANS.hasOwnProperty(k) ){ foo_spans++; addrow_totable(k); }}
		if(!foo_spans){
			ASTGUI.domActions.clear_table(tbl);
			var newRow = tbl.insertRow(-1);
			newRow.className = "even";
	
			var newCell0 = newRow.insertCell(0);
			newCell0.innerHTML = "No Digital Hardware detected !!";
			HAS_DIGITALHARDWARE = false;
		}

		if(foo_spans && hwchanged == -1){ // no previous hardware information found - configuring for the first time
			ASTGUI.dialog.alertmsg('Please configure your hardware using the Edit button(s)' + '<BR>'+
				"When done click on the 'Update Settings'" );
			keepalert = true;
		}else{ // if previous config file found 
			if(foo_spans && hwchanged){ //
				ASTGUI.dialog.alertmsg('Hardware Changes detected !! <BR><BR>' +
					'When you "Update Settings" all your previous settings will be over written' );
				keepalert = true;
			}else{ // no hardware changes detected
				//gui_alert('No Hardware Changes detected !! ');
				keepalert = false;
			}
		}
	}catch(err){

	}finally{
		loadConfigFiles.load_zaptel_conf(); // used only for retreiving loadzone
		if( !keepalert ){
			parent.ASTGUI.dialog.hide();
		}
	}
}


var loadConfigFiles = {
	// read hwcfgfile (if exists) into CONFIGUREDHARDWARE - so that the GUI knows the last configured hardware
	// run ztscan - to detect digital cards
	// read ztscan.conf - store hardware information in DETECTEDHARDWARE, read spans information - also set the max , min values for each span
	//   see if the hardware matches with that from CONFIGUREDHARDWARE 
	//   (this way we know if any changes in digital hardware since the gui was last used)
	// read users.conf - and read spans information 
	//   check if the channels in zapchan are within max and min
	//   if yes then set the current range values
	load_hwcfgfile: function(){ // read hwcfgfile (if exists) into CONFIGUREDHARDWARE 
		top.log.debug("load last configured hardware information, start function: loadConfigFiles.load_hwcfgfile()");
		var n = config2json({filename:hwcfgfile, usf:1});
		if( n.getOwnProperties().length == 0 ){ // if file not found or no previous hardware detected
				hwchanged = -1;
				loadConfigFiles.runZtscan();
				return;
		}else{
			CONFIGUREDHARDWARE = {};
			for( var l in n ){ if(n.hasOwnProperty(l) && l!='ANALOGPORTS' ){ // l is location
				CONFIGUREDHARDWARE[l] = {};
				CONFIGUREDHARDWARE[l]['device'] = n[l]['device'];
				CONFIGUREDHARDWARE[l]['basechan'] = n[l]['basechan'];
				CONFIGUREDHARDWARE[l]['type'] = n[l]['type'];
			}}
			loadConfigFiles.runZtscan();
		}
		top.log.debug("end of function: loadConfigFiles.load_hwcfgfile()");
	},

	runZtscan: function(){
		top.log.debug("start function: loadConfigFiles.runZtscan()");

		ASTGUI.miscFunctions.createConfig( 'applyzap.conf', function(){
			parent.ASTGUI.systemCmd( top.sessionData.directories.app_Ztscan , function(){ // run ztscan and then try loading ztscan.conf
				window.setTimeout( loadConfigFiles.readZtscanConf , 700 ); // leave some time for ztscan to generate ztscan.conf
			});
			top.log.debug("end of function: loadConfigFiles.runZtscan()");
		});
	},

	readZtscanConf: function(){
		top.log.debug("start function: loadConfigFiles.readZtscanConf()");
		var ztsc = $.ajax({ url: ASTGUI.paths.rawman+'?action=getconfig&filename=ztscan.conf', async: false }).responseText;
		var ztsc_Lower = ztsc.toLowerCase();
		if( ztsc_Lower.contains('response: error') && ztsc_Lower.contains('message: config file not found') ){
			parent.ASTGUI.dialog.waitWhile("Please check if <B>ztscan</B> is installed ? <BR> /etc/asterisk/ztscan.conf not found");
			ASTGUI.feedback( { msg:"No Cards/Spans found,  No Config File found !!", showfor:2 });
			return;
		}
		var n = config2json({ configFile_output: ztsc, usf : 1 });

		for( var l in n ){ if(n.hasOwnProperty(l)){
			if(n[l]['type'] == 'analog'){
				DETECTEDHARDWARE[ n[l]['location'] ] = {};
				DETECTEDHARDWARE[ n[l]['location'] ]['device'] = n[l]['devicetype'];
				DETECTEDHARDWARE[ n[l]['location'] ]['basechan'] = n[l]['basechan'];
				DETECTEDHARDWARE[ n[l]['location'] ]['type'] = n[l]['type'] ;
				continue;
			} // in this page, we care only about digital spans 
			//  note: function detectHwChanges checks if there are any changes in analog ports detected
			if(  n[l]['description'].toLowerCase().contains('ztdummy') ){ continue;} // ignore ztdummy :-)
			SPANS[l] = {};
			for( var k in n[l] ){ if(n[l].hasOwnProperty(k)){ 
				SPANS[l][k] = n[l][k]; // store all the other fields in spans[l]
				if( k == 'location'){
					if(!(SPANCOUNT_LOCATION[n[l]['location']]) ){ SPANCOUNT_LOCATION[n[l]['location']] = 0; }
					SPANCOUNT_LOCATION[n[l]['location']] = SPANCOUNT_LOCATION[n[l]['location']] + 1;
				}
				if( k=='location' && !(DETECTEDHARDWARE[n[l]['location']]) ){
					DETECTEDHARDWARE[ n[l]['location'] ] = {};
					DETECTEDHARDWARE[ n[l]['location'] ]['device'] = n[l]['devicetype'];
					DETECTEDHARDWARE[ n[l]['location'] ]['basechan'] = n[l]['basechan'];
					DETECTEDHARDWARE[ n[l]['location'] ]['type'] = n[l]['type'];
				}
				if( k == 'totchans' ){
					SPANS[l]['spantype'] = n[l]['type'].split('-')[1]; // part after '-' in 'digital-T1' or 'digital-E1'
					SPANS[l]['min_ch'] = Number(n[l]['basechan']); 
					SPANS[l]['max_ch'] =  Number(n[l]['basechan']) + Number(n[l]['totchans']) - 1 ; 
					SPANS[l]['reserved_ch'] = Number(n[l]['basechan']) + ( (Number(n[l]['totchans'])==24)? 23 : 15 );
					SPANS[l]['definedchans'] = (Number(n[l]['totchans'])==24)? 23 : 30 ; // default values
				}
				if( k == 'lbo' ) {
					switch(n[l][k]){
						case '0 db (CSU)/0-133 feet (DSX-1)':
							SPANS[l]['lbo'] = 0; break; 
						case '133-266 feet (DSX-1)':
							SPANS[l]['lbo'] = 1; break; 
						case '266-399 feet (DSX-1)':
							SPANS[l]['lbo'] = 2; break;
						case '399-533 feet (DSX-1)':
							SPANS[l]['lbo'] = 3; break;
						case '533-655 feet (DSX-1)':
							SPANS[l]['lbo'] = 4; break;
						case '-7.5db (CSU)':
							SPANS[l]['lbo'] = 5; break;
						case '-15db (CSU)':
							SPANS[l]['lbo'] = 6; break;
						case '-22.5db (CSU)':
							SPANS[l]['lbo'] = 7; break;
						default:
							SPANS[l]['lbo'] = 0; break; 
					}
				}
			}}
		}}
		if(hwchanged != -1){ hwchanged = detectHwChanges(); }
		top.log.debug("end of function: loadConfigFiles.readZtscanConf()");
		loadConfigFiles.readUsersConf(); // read span_x (where T1/E1 trunks are defined)
	},

	readUsersConf: function(){
		top.log.debug("start function: loadConfigFiles.readUsersConf()");

		var usrs = $.ajax({ url: ASTGUI.paths.rawman+'?action=getconfig&filename=users.conf', async: false }).responseText;
		var usrs_Lower = usrs.toLowerCase();
		if( usrs_Lower.contains('response: error') && usrs_Lower.contains('message: config file not found') ){
			ASTGUI.feedback( { msg:"No Users File found !!", showfor:2 });
			parent.ASTGUI.dialog.waitWhile("/etc/asterisk/users.conf not found");
			return;
		}
		var n = config2json({ configFile_output: usrs, usf : 1 });// read users.conf and load switchtype, signalling, zapchan into the SPANS object

		(function(){
			var efgh = function(zc){ 
					// calculate the number of channels defined for use in zapchan string
					// zc can be in the format 'u', 'u-v', 'u-v,w-x' or 'u-v,w'
					// the function returns 1 if u, (v-u +1) for the second case 
					// and x-u for the third and // v-u+2 for the fourth case
				try{
					if( zc.contains('-') ){
						if(zc.contains(',')){ // case 3 or case 4
							if( zc.split('-').length > 2 ) { // case 3
								return (Number(zc.split('-')[2]) - Number(zc.split('-')[0]));
							}else{ // case 4
								return (Number((zc.split('-')[1]).split(',')[0])-Number(zc.split('-')[0]) + 2);
							}
						} else { // case 'u-v'
							return (Number(zc.split('-')[1])-Number(zc.split('-')[0])+1);
						}
					}else{ // case 1
						return 1;
					}
				}catch(err){
					return null;
				}
			};

			var tmp;
			var tmp_spantodelete = {};
			for( var l in n ){ if(n.hasOwnProperty(l)){

				if( n[l]['group'] && l !='general' ){
					if( GROUPS.contains( Number(n[l]['group'])) ){
						// duplicate group ??? 
						// we wish to address this situation in future
					}else{
						GROUPS.push( Number(n[l]['group']) );
					}
				}

				if( l.beginsWith('span_')) {
					tmp_spantodelete = {};
					tmp_spantodelete['spanName'] = l;
					if( n[l]['context'] ){ tmp_spantodelete['spanContext'] = n[l]['context']; }

					spans_todelete.push(tmp_spantodelete);

					tmp = l.split('span_')[1];
					//if (!SPANS[tmp]){ SPANS[tmp] = {}; }
					if (!SPANS[tmp]){  //ztscan did not detect any such span as tmp
						continue;
					}
					SPANS[tmp]['definedchans'] = 0;
					for( var k in n[l] ){ if(n[l].hasOwnProperty(k)){
						if( k == 'signalling' || k == 'zapchan' || k == 'switchtype' ){ SPANS[tmp][k] = n[l][k]; }
						if( k=='zapchan'){ SPANS[tmp]['definedchans'] = efgh(n[l][k]); }
					}}
				}
			}}
		})();
		top.log.debug("end of function: loadConfigFiles.readUsersConf()");
		showtable();
	},

	load_zaptel_conf: function(){
		top.log.debug("start function: loadConfigFiles.load_zaptel_conf()");
		// we parse zaptel.conf to get the loadzone and syncsrc for each span
		var tmp_file = ASTGUI.globals.zaptelIncludeFile;
		var parseZaptelconf = function(zp){
			(function (){
				var t = (zp.hasOwnProperty('general')) ? zp['general'] : [] ; // t is an array
				var line = '';
				_$('loadZone').selectedIndex = 0;
				for(var g=0; g < t.length; g++){
					line = t[g];
					try{
						if( line.beginsWith('loadzone=')) {
							ASTGUI.selectbox.selectOption( _$('loadZone'), line.withOut('loadzone=') );
							return;
						}
					}catch(err){
						_$('loadZone').selectedIndex = 0;
					}
				}
			})();

			(function (){
				var t = (zp.hasOwnProperty('general')) ? zp['general'] : [] ; // t is an array
				t.each(function(line){
					try{
					if(line.beginsWith('span=')){
						var y = ASTGUI.parseContextLine.read(line)[1] ;
						var span_no = y.split(',')[0];
						var src_span = y.split(',')[1];
						if(SPANS[span_no]){ SPANS[span_no]['syncsrc'] = src_span; }
					}
					}catch(err){}
				});
			})();
		};

		var s = $.ajax({ url: ASTGUI.paths.rawman+'?action=getconfig&filename=' + tmp_file , async: false }).responseText;
		if( s.contains('Response: Error') && s.contains('Message: Config file not found') ){
			ASTGUI.miscFunctions.createConfig( tmp_file, function(){
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
	}
};

function disablEnable_sc() {
	if( !_$('editspan_signalling').value.beginsWith('pri') ){
		_$('editspan_switchtype').disabled =  true;
		_$('editspan_switchtype').selectedIndex = -1;

		if( _$('edit_DefinedChans').options.length == 24) {
			_$('edit_DefinedChans').options[23].disabled = false;
		}
		_$('edit_DefinedChans').selectedIndex = 23 ;
	}else{
		_$('editspan_switchtype').disabled =  false;
		if( _$('edit_DefinedChans').options.length == 24) { // if is a PRI singalled T1
			_$('edit_DefinedChans').options[23].disabled = true;
			if(_$('edit_DefinedChans').selectedIndex == 23 ){ _$('edit_DefinedChans').selectedIndex = 22 ; }
		}
	}

	return true;
}

function updateSpanInfo(){
	if( !verify_priChLimit() ){
		alert("You should leave atleast one channel for signalling.");
		return true;
	}
	if(!_$('editspan_fac').value){
		alert("Please select a Framing/Coding !");
		return true;
	}
	if(!_$('editspan_signalling').value){
		alert("Please select a signalling !");
		return true;
	}
	if(_$('editspan_signalling').value.beginsWith('pri') &&  !_$('editspan_switchtype').value ){
		alert("Please select a Switch Type !");
		return true;
	}

	var b = String(CURRENT_SPAN);

	if( _$('editspan_fac').value == 'CCS/CRC4/HDB3' ){
		SPANS[b]['framing'] = 'CCS/CRC4';
		SPANS[b]['coding'] = 'HDB3';
	}else{
		SPANS[b]['framing'] = _$('editspan_fac').value.split('/')[0];
		SPANS[b]['coding'] = _$('editspan_fac').value.split('/')[1];;
	}

	SPANS[b]['signalling'] = _$('editspan_signalling').value;
	SPANS[b]['switchtype'] = _$('editspan_switchtype').value;
	SPANS[b]['syncsrc'] = _$('editspan_syncsrc').value;
	SPANS[b]['lbo'] = _$('editspan_lbo').value;

	_$('row'+ b).style.background = "#C9AAAA";

	canelSpanInfo();
}




var storeDetectedHardware = function(){
	var cmd = 'rm /etc/asterisk/' + hwcfgfile + '; touch /etc/asterisk/' + hwcfgfile;
	parent.ASTGUI.systemCmd( cmd, function(){ 
		var storeNewinfo = function(){
			var after = function(){
				parent.sessionData.REQUIRE_RESTART = (HAS_ANALOGHARDWARE || HAS_DIGITALHARDWARE)? true : false;

				if(parent.sessionData.REQUIRE_RESTART){
					alert("You need to restart your machine for these settings to take effect");
				}
				top.window.location.reload(); // we do top.reload() so that users.conf is reparsed for any changes in trunks 
			};
			var x = new listOfActions();
			x.filename(hwcfgfile);
			for(var g in DETECTEDHARDWARE){ if( DETECTEDHARDWARE.hasOwnProperty(g) ) { // g is location
				x.new_action('newcat', g , "", "");
				x.new_action('update', g , "device", DETECTEDHARDWARE[g]['device']);
				x.new_action('update', g , "basechan", DETECTEDHARDWARE[g]['basechan'] );
				x.new_action('update', g , "type", DETECTEDHARDWARE[g]['type'] );
			}}
			x.new_action('newcat', 'ANALOGPORTS' , '', '');
			x.new_action('update', 'ANALOGPORTS' , 'FXS', parent.sessionData.FXS_PORTS_DETECTED.join(','));
			x.new_action('update', 'ANALOGPORTS' , 'FXO', parent.sessionData.FXO_PORTS_DETECTED.join(','));
			x.callActions(after);
		};
		setTimeout( storeNewinfo , 700); // leave some time for rm, touch
	});

};



var applySettings = {
	save_opermode_settings: function(){ // applySettings.save_opermode_settings();
		ASTGUI.dialog.waitWhile('saving...');
		var u = new listOfSynActions(ASTGUI.globals.configfile);

			if( _$('enable_disable_checkbox_opermode').checked ){
				u.new_action('update', 'general', 'opermode', ASTGUI.getFieldValue('opermode') );
			}

			if( _$('enable_disable_checkbox_alawoverride').checked ){
				u.new_action('update', 'general', 'alawoverride', ASTGUI.getFieldValue('alawoverride') );
			}

			if( _$('enable_disable_checkbox_fxshonormode').checked ){
				u.new_action('update', 'general', 'fxshonormode', ASTGUI.getFieldValue('fxshonormode') );
			}

			if( _$('enable_disable_checkbox_boostringer').checked ){
				u.new_action('update', 'general', 'boostringer', ASTGUI.getFieldValue('boostringer') );
			}

			u.new_action('update', 'general', 'ZAPMODULE_NAME', ASTGUI.getFieldValue('zap_moduleName') );
			u.callActions();
			u.clearActions();

		if( _$('enable_disable_checkbox_mwimode').checked ){
			u.new_action('update', 'general', 'mwimode', ASTGUI.getFieldValue('mwimode') );
			if( ASTGUI.getFieldValue('mwimode') == 'NEON' ){
				u.new_action('update', 'general', 'neonmwi_level', ASTGUI.getFieldValue('neonmwi_level') );
				u.new_action('update', 'general', 'neonmwi_offlimit', ASTGUI.getFieldValue('neonmwi_offlimit') );
			}
		}

		if( _$('enable_disable_checkbox_lowpower').checked ){
			u.new_action('update', 'general', 'lowpower', ASTGUI.getFieldValue('lowpower') );
		}

		if( _$('enable_disable_checkbox_fastringer').checked ){
			u.new_action('update', 'general', 'fastringer', ASTGUI.getFieldValue('fastringer') );
		}

		if( _$('enable_disable_checkbox_fwringdetect').checked ){
			u.new_action('update', 'general', 'fwringdetect', ASTGUI.getFieldValue('fwringdetect') );
		}

		u.callActions();
		u.clearActions();
	
		ASTGUI.dialog.waitWhile('updating modprobe configuration ...');
		var cmd1 = "cp /etc/asterisk/modprobe_default /etc/modprobe.d/dahdi.conf";
		var params = "options " + ASTGUI.getFieldValue('zap_moduleName') ;

		if( _$('enable_disable_checkbox_opermode').checked ){
			var h = ASTGUI.getFieldValue('opermode') ;
				if(h){ params += " opermode=" + h; }
		}

		if( _$('enable_disable_checkbox_alawoverride').checked ){
			h = ASTGUI.getFieldValue('alawoverride') ;
			if(h){ params += " alawoverride=" + h; }
		}

		if( _$('enable_disable_checkbox_fxshonormode').checked ){
		h = ASTGUI.getFieldValue('fxshonormode') ;
			if(h){ params += " fxshonormode=" + h; }
		}

		if( _$('enable_disable_checkbox_boostringer').checked ){
			h = ASTGUI.getFieldValue('boostringer') ;
			if(h){ params += " boostringer=" + h; }
		}

		if( _$('enable_disable_checkbox_lowpower').checked ){
			h = ASTGUI.getFieldValue('lowpower') ;
			if(h){ params += " lowpower=" + h; }
		}

		if( _$('enable_disable_checkbox_fastringer').checked ){
			h = ASTGUI.getFieldValue('fastringer') ;
			if(h){ params += " fastringer=" + h; }
		}

		if( _$('enable_disable_checkbox_fwringdetect').checked ){
			h = ASTGUI.getFieldValue('fwringdetect') ;
			if(h == '1'){ params += " fwringdetect=" + h; }
		}

		if( _$('enable_disable_checkbox_mwimode').checked ){
			if( ASTGUI.getFieldValue('mwimode') == 'NEON'){
				params += " neonmwi_monitor=1";
				var h = ASTGUI.getFieldValue('neonmwi_level');
				if(h){ params += ' neonmwi_level=' + h ; }
				var h = ASTGUI.getFieldValue('neonmwi_offlimit');
				if(h){ params += ' neonmwi_offlimit=' + h ; }
			}else{
				params += " neonmwi_monitor=0";
			}
		}
	
		var cmd2 = "echo \"" + params + "\" >> /etc/modprobe.d/dahdi.conf ";
	
		var update_usersConf = function(){
			// update MWI settings in users.conf
			var u = new listOfSynActions('users.conf');
			if( _$('enable_disable_checkbox_mwimode').checked ){
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
			}
			u.callActions();
			ASTGUI.dialog.hide();
			ASTGUI.feedback( { msg:"updated settings !!", showfor: 3 });

			storeDetectedHardware();
		};
	
		if( !ASTGUI.getFieldValue('zap_moduleName') ){
			ASTGUI.dialog.hide();
			ASTGUI.feedback( { msg:"updated settings !!", showfor: 3 });
			storeDetectedHardware();
			return;
		}

		ASTGUI.systemCmd( cmd1, function(){ 
			ASTGUI.systemCmd( cmd2, function(){ 
				ASTGUI.dialog.waitWhile('updating Analog Trunks with MWI settings ...');
				update_usersConf();
			});
		});
	},

	generate_zaptel: function(){
		parent.ASTGUI.systemCmd( top.sessionData.directories.script_generateZaptel + " applysettings" , function(){
			parent.sessionData.REQUIRE_RESTART = (HAS_ANALOGHARDWARE || HAS_DIGITALHARDWARE)? true : false;
			parent.ASTGUI.systemCmd( "ztcfg -vv" , function(){
				applySettings.save_opermode_settings();
				return true ;
			});
		});
	},

	updateUsersConf: function(){
		/* update the users.conf to make sure there are corresponding [SPAN_x] contexts are updated */
		// for each span update span with new values of 'switchtype', 'singalling'

		var USR_CF = config2json({filename:'users.conf', usf:1 });
		var EX_CF = config2json({filename:'extensions.conf', usf:1 });

		var d ='', e ='' ;
		var x = new listOfActions(); x.filename('users.conf');
		var y = new listOfActions(); y.filename('extensions.conf');
		var pri_trunk = {};
		for( var k in SPANS ){ if( SPANS.hasOwnProperty(k) ){ if(SPANS[k]['signalling']){


			d = 'span_'+ String(k);
			e = ASTGUI.contexts.TrunkDIDPrefix + d ;

			if( !USR_CF.hasOwnProperty(d) ){
				NEWGROUPNUMBER = GROUPS.firstAvailable() ;
				GROUPS.push(NEWGROUPNUMBER);
				x.new_action('newcat', d , "", "");
				x.new_action('update', d , "group", NEWGROUPNUMBER );
			}else{
				NEWGROUPNUMBER = USR_CF[d]['group'] || '' ;
			}

			pri_trunk['group'] = NEWGROUPNUMBER ;

			if( !EX_CF['globals'].hasOwnProperty(d) ){
				y.new_action('update', 'globals', d,'Zap/g'+String(NEWGROUPNUMBER));
			}

			if( !EX_CF.hasOwnProperty(e) ){
				//alert("'" + e + "'");
				y.new_action('newcat', e , '', '');
			}

			x.new_action('update', d , "hasexten", 'no');
			if ( SPANS[k]['signalling'].beginsWith('pri') ){
				// we do not want a switchtype to be set for channelized T1/E1 interfaces
				x.new_action('update', d , 'switchtype', SPANS[k]['switchtype']);
				pri_trunk['switchtype'] = SPANS[k]['switchtype'];
			}

			x.new_action('update', d , "signalling", SPANS[k]['signalling']);
				pri_trunk['signalling'] = SPANS[k]['signalling'];
			x.new_action('update', d , "trunkname", 'Span '+String(k));
				pri_trunk['trunkname'] = 'Span '+String(k) ;
			x.new_action('update', d , "trunkstyle", 'digital'.guiMetaData() );
				pri_trunk['trunkstyle'] = 'digital';
			x.new_action('update', d , "hassip", 'no');
				pri_trunk['hassip'] = 'no';
			x.new_action('update', d , "hasiax", 'no');
				pri_trunk['hasiax'] = 'no';

			if ( !SPANS[k]['signalling'].beginsWith('fxo') ){ 
				// we do not want a context to be set for user stations
				// instead, this context would be set when the station is assigned to a user
				x.new_action('update', d , "context", e);
				pri_trunk['context'] = e ;
			}
			x.new_action('update', d , "zapchan", SPANS[k]['zapchanstring']);
				pri_trunk['zapchan'] = SPANS[k]['zapchanstring'] ;

			parent.sessionData.pbxinfo['trunks']['pri'][d] = pri_trunk;
		}}}
		x.callActions( function(){ y.callActions( applySettings.generate_zaptel ); } );
	},


	cleanUsersConf: function(){

		if( _$('RESET_ANY_DIGITAL_TRUNKS').checked ){
			var x = new listOfActions(); x.filename('users.conf');
			var y = new listOfActions(); y.filename('extensions.conf');
			
			spans_todelete.each( function(item) {
				x.new_action('delcat', item['spanName'], '', '');// for deleting old span info from users.conf
				if(item['spanContext']){ // for deleting old span info from extensions.conf
					y.new_action('delete', 'globals', item['spanName'], "", "");
					y.new_action('delcat', item['spanContext'] , "", "");
				}
			});
			parent.sessionData.pbxinfo['trunks']['pri'] = {};
			x.callActions( function(){ y.callActions(applySettings.updateUsersConf); } );

		}else{
			applySettings.updateUsersConf();
		}

	},

	updateZaptel: function(){ 
	// navigate through the SPANS object and save it to the applyzap.conf, 
	// then call a script which will generate zaptel.conf from it and asks the user to restart his machine
		parent.ASTGUI.dialog.waitWhile('Saving Changes ...');
		var fxx={}, bchanstring = '', dchanstring = '', context = 'general';
		var totalchans = 0, firstpart , secondpart, tmp2 , tmp3;

		var x = new listOfActions(); x.filename('applyzap.conf');
		x.new_action('delcat', context,"", "");
		x.new_action('newcat', context, "", "");
		for( var k in SPANS ){ if( SPANS.hasOwnProperty(k) ){ if(SPANS[k]['signalling'] ){
			firstpart = "span";
			/* XXX Timing source for card is being set to zero? */
			/* LBO is being set to 0 */
			SPANS[k]['fac'] = SPANS[k]['framing'] + '/' + SPANS[k]['coding'];
			if(SPANS[k]['lbo'] == "") { SPANS[k]['lbo'] = 0; }
			if(SPANS[k]['syncsrc'] == "") { SPANS[k]['syncsrc'] = 1; }
			secondpart = k + "," + SPANS[k]['syncsrc']  + "," + SPANS[k]['lbo'] + "," + SPANS[k]['fac'].toLowerCase().replace("/", ",");
			x.new_action('append', context, firstpart, secondpart );
			tmp2 = (bchanstring)? ",":"";
			tmp3 = (dchanstring)? ",":"";
			var ppp = calc_bchan_fxx(k);
			if ( !SPANS[k]['signalling'].beginsWith('pri') ){
				if(SPANS[k]['signalling'].beginsWith('fx')){ 
					var qqq = (SPANS[k]['signalling']).replace('_',''); // zaptel.conf uses fxoks instead of fxo_ks
				}else{
					var qqq = 'e&m'; // zaptel.conf uses fxoks instead of fxo_ks
				}

				if(fxx[ qqq ]){
					fxx[ qqq ] = fxx[ qqq ] + "," + ppp;
				}else{
					fxx[ qqq ] = ppp;
				}
			}else{
				bchanstring += tmp2 + ppp;
				dchanstring += tmp3 + SPANS[k]['reserved_ch'];
			}
			SPANS[k]['zapchanstring'] = ppp;
		}}}

		for( var e in fxx ){ if(fxx.hasOwnProperty(e)){	x.new_action('append', context, e, fxx[e]);	}}

		if(bchanstring.trim()){ x.new_action('append', context, 'bchan', bchanstring); }
		if(dchanstring.trim()){ x.new_action('append', context, 'dchan', dchanstring); }

		// write back any actual analog ports
		parent.sessionData.PORTS_SIGNALLING.ls = []; // reset previous signalling data
		parent.sessionData.PORTS_SIGNALLING.ks = [];

		if( parent.sessionData.FXO_PORTS_DETECTED.length){
			//portsSignalling.ks
			//x.new_action('append', context, 'fxsks', parent.sessionData.FXO_PORTS_DETECTED.join(',')); // FXO ports will be fxs signalled
			(function(){
				var ks_fxoPorts = [];
				var ls_fxoPorts = [];
				var t = parent.sessionData.FXO_PORTS_DETECTED ;

				for( var i = 0 ; i < t.length ; i++){
					if( portsSignalling.ls.contains(t[i]) ){
						ls_fxoPorts.push(t[i]);
					}else{
						ks_fxoPorts.push(t[i]);
					}
				}

				if( ls_fxoPorts.length ){
					x.new_action('append', context, 'fxsls', ls_fxoPorts.join(',')); // FXO ports will be fxs signalled
				}
				parent.sessionData.PORTS_SIGNALLING.ls = parent.sessionData.PORTS_SIGNALLING.ls.concat(ls_fxoPorts);

				if( ks_fxoPorts.length ){
					x.new_action('append', context, 'fxsks', ks_fxoPorts.join(',')); // FXO ports will be fxs signalled
				}
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
					if( portsSignalling.ls.contains(t[i]) ){
						ls_fxsPorts.push(t[i]);
					}else{
						ks_fxsPorts.push(t[i]);
					}
				}

				if( ls_fxsPorts.length ){
					x.new_action('append', context, 'fxols', ls_fxsPorts.join(',')); // FXS ports will be fxo signalled
				}
				parent.sessionData.PORTS_SIGNALLING.ls = parent.sessionData.PORTS_SIGNALLING.ls.concat(ls_fxsPorts);

				if( ks_fxsPorts.length ){
					x.new_action('append', context, 'fxoks', ks_fxsPorts.join(',')); // FXS ports will be fxo signalled
				}
				parent.sessionData.PORTS_SIGNALLING.ks = parent.sessionData.PORTS_SIGNALLING.ks.concat(ks_fxsPorts);
			})();
		}

		x.new_action('append', context, 'loadzone', _$('loadZone').value);
		x.new_action('append', context, 'defaultzone', _$('loadZone').value);
		x.callActions( applySettings.cleanUsersConf );
	}
};


function applyDigitalSettings(){
 	// write to applyzap.conf - generate new applyzap.conf
 	// write to users.conf - update [spans_x] contexts
 	// call the asterisk_guiEditZap - which will generate zaptel.conf from applyzap.conf
	ASTGUI.showbg(true);
	applySettings.updateZaptel();
}


var localajaxinit = function(){
	top.log.debug("Starting Loading Page digital.html .. start function: window.onload()");
	portsSignalling = ASTGUI.cloneObject(parent.sessionData.PORTS_SIGNALLING);

	ASTGUI.domActions.enableDisableByCheckBox ('enable_disable_checkbox_opermode', 'opermode') ;
	ASTGUI.domActions.enableDisableByCheckBox ('enable_disable_checkbox_mwimode', 'mwimode') ;
	ASTGUI.domActions.enableDisableByCheckBox ('enable_disable_checkbox_fwringdetect', 'fwringdetect') ;
	ASTGUI.domActions.enableDisableByCheckBox ('enable_disable_checkbox_lowpower', 'lowpower') ;
	ASTGUI.domActions.enableDisableByCheckBox ('enable_disable_checkbox_fastringer', 'fastringer') ;
	ASTGUI.domActions.enableDisableByCheckBox ('enable_disable_checkbox_boostringer', 'boostringer') ;
	ASTGUI.domActions.enableDisableByCheckBox ('enable_disable_checkbox_fxshonormode', 'fxshonormode') ;
	ASTGUI.domActions.enableDisableByCheckBox ('enable_disable_checkbox_alawoverride', 'alawoverride') ;

	ASTGUI.COMBOBOX.call( _$('zap_moduleName') , DRIVERS_LIST , 195 );

	if( parent.sessionData.PLATFORM.isAA50 ){
		(function(){
			var t = [
				{url:'#', desc:'Hardware Configuration', selected:true } ,
				{url:'modprobe_settings.html', desc:'Operation Mode'}
			];
			ASTGUI.tabbedOptions( _$('tabbedMenu') , t);
			$('#tabbedMenu_container').show();
		})();
		ASTGUI.dialog.waitWhile('Detecting Hardware ...');
		top.document.title = "Analog Hardware Setup & Configuration";
		_$('iframeTitleBar_title').innerHTML = 'Analog Hardware Setup & Configuration';
	}else{
		ASTGUI.dialog.waitWhile('Detecting Analog/Digital Hardware ...');
		top.document.title = "Analog & Digital Cards(T1/E1) Setup & Configuration";
	}

	if(!parent.sessionData.FXS_PORTS_DETECTED.length && !parent.sessionData.FXO_PORTS_DETECTED.length ){
		var newRow = _$('FXSFXO_ports_td').insertRow(-1) ;
		newRow.className = 'even' ;
		ASTGUI.domActions.tr_addCell( newRow , { html: 'No Analog Hardware detected !!' , align: 'center' } );
		HAS_ANALOGHARDWARE = false;
	}else{
		if(parent.sessionData.FXS_PORTS_DETECTED.length){
			var s1 = parent.sessionData.FXS_PORTS_DETECTED.join(' , ') ;
		}else{
			var s1 = '--' ;
		}

		if(parent.sessionData.FXO_PORTS_DETECTED.length){
			var s2 = parent.sessionData.FXO_PORTS_DETECTED.join(' , ') ;
		}else{
			var s2 = '--' ;
		}

		var tbl = _$('FXSFXO_ports_td');
		var addCell = ASTGUI.domActions.tr_addCell;
		var newRow = tbl.insertRow(-1);
		newRow.className = "frow";
		addCell( newRow , { html:'Type', align: 'center' , width:'150px' });
		addCell( newRow , { html:'Ports' });
		addCell( newRow , { html:'', width:'90px' });

		var newRow = tbl.insertRow(-1);
		newRow.className = "even";
		newRow.id = "FXSFXO_ports_td__FXS";
		addCell( newRow , { html: '<B>FXS Ports</B>', align:'center'});
		addCell( newRow , { html: s1 });
		addCell( newRow , { html: (parent.sessionData.FXS_PORTS_DETECTED.length) ? "<span class=guiButton onclick=\"digital_miscFunctions.show_analog_signalling_form(\'FXS\')\">Edit</span>" : '' , align:'center' });

		var newRow = tbl.insertRow(-1);
		newRow.className = "odd";
		newRow.id = "FXSFXO_ports_td__FXO";
		addCell( newRow , { html:'<B>FXO Ports</B>', align:'center' });
		addCell( newRow , { html: s2});
		addCell( newRow , { html: (parent.sessionData.FXO_PORTS_DETECTED.length) ? "<span class=guiButton onclick=\"digital_miscFunctions.show_analog_signalling_form(\'FXO\')\">Edit</span>" : '', align:'center' });

	}

	var edit_DefinedChans_changed = function(){
		var b = CURRENT_SPAN ;
		var y = Number(_$('edit_DefinedChans').value);
		if( !verify_priChLimit() ){
			alert("You should leave atleast one channel for signalling");
			return true;
		}

		SPANS[b]['definedchans'] = y;
		//_$('edit_labelReserved').innerHTML = SPANS[b]['reserved_ch'];
		_$('edit_labelZapchan').innerHTML = calc_bchan_fxx(b);
	};

	ASTGUI.events.add( _$('edit_DefinedChans'), "change", edit_DefinedChans_changed );

	top.log.debug("end of function: window.onload()");

	(function(){ // load modprobe settings
		var c = context2json ({ filename: ASTGUI.globals.configfile , context: 'general', usf: 1 });

		ASTGUI.updateFieldToValue( 'zap_moduleName' , c.getProperty('ZAPMODULE_NAME') );

		ASTGUI.updateFieldToValue( 'opermode' , c.getProperty('opermode') );
			_$('enable_disable_checkbox_opermode').checked = (c.getProperty('opermode')) ? true : false ;
			_$('enable_disable_checkbox_opermode').updateStatus();

		ASTGUI.updateFieldToValue( 'alawoverride' , c.getProperty('alawoverride') );
			_$('enable_disable_checkbox_alawoverride').checked = (c.getProperty('alawoverride')) ? true : false ;
			_$('enable_disable_checkbox_alawoverride').updateStatus();

		ASTGUI.updateFieldToValue( 'fxshonormode' , c.getProperty('fxshonormode') );
			_$('enable_disable_checkbox_fxshonormode').checked = (c.getProperty('fxshonormode')) ? true : false ;
			_$('enable_disable_checkbox_fxshonormode').updateStatus();


		ASTGUI.updateFieldToValue( 'boostringer' , c.getProperty('boostringer') );
			_$('enable_disable_checkbox_boostringer').checked = (c.getProperty('boostringer')) ? true : false ;
			_$('enable_disable_checkbox_boostringer').updateStatus();


		ASTGUI.updateFieldToValue( 'lowpower' , c.getProperty('lowpower') );
			_$('enable_disable_checkbox_lowpower').checked = (c.getProperty('lowpower')) ? true : false ;
			_$('enable_disable_checkbox_lowpower').updateStatus();


		ASTGUI.updateFieldToValue( 'fastringer' , c.getProperty('fastringer') );
			_$('enable_disable_checkbox_fastringer').checked = (c.getProperty('fastringer')) ? true : false ;
			_$('enable_disable_checkbox_fastringer').updateStatus();


		ASTGUI.updateFieldToValue( 'fwringdetect' , c.getProperty('fwringdetect') );
			_$('enable_disable_checkbox_fwringdetect').checked = (c.getProperty('fwringdetect')) ? true : false ;
			_$('enable_disable_checkbox_fwringdetect').updateStatus();

		ASTGUI.updateFieldToValue( 'neonmwi_level' , c.getProperty('neonmwi_level') || '75' );
		ASTGUI.updateFieldToValue( 'neonmwi_offlimit' , c.getProperty('neonmwi_offlimit') || '16000' );
		ASTGUI.updateFieldToValue( 'mwimode' , c.getProperty('mwimode') );
			_$('enable_disable_checkbox_mwimode').checked = (c.getProperty('mwimode')) ? true : false ;
			_$('enable_disable_checkbox_mwimode').updateStatus();

		if(c.getProperty('mwimode') == 'NEON')$(".neon_settings").show();

		ASTGUI.events.add( _$('mwimode'), 'change' , function(){
			if( _$('mwimode').value == 'NEON'){
				$(".neon_settings").show();
			}else{
				$(".neon_settings").hide();
			}
		}) ;
	})();

	ASTGUI.systemCmd( 'touch /etc/asterisk/modprobe_default', function(){
		loadConfigFiles.load_hwcfgfile(); // try to load last detected/configured hardware information
	});

}
