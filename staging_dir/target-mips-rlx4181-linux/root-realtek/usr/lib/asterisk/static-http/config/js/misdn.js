/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * misdn.html functions
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
var CARDS = 0;
var PORTS = {}; // each port corresponds to a physical port on the bri card
var pmode_defs = {
	'te_ptp':'TE-Mode, PTP',
	'te_ptmp':'TE-Mode, PTMP',
	'te_capi_ptp':'TE-Mode(Capi), PTP',
	'te_capi_ptmp':'TE-Mode(Capi), PTMP',
	'nt_ptp':'NT-Mode, PTP',
	'nt_ptmp':'NT-Mode, PTMP'
};
var mISDNTRUNKS = {};
var isnewTrunk ;

var misdnConfig = {

	detectCards: function(){
		parent.ASTGUI.systemCmdWithOutput( top.sessionData.directories.app_mISDNscan , function(s){ // run 'misdn-init scan'
			try{
				var records = s.split("\n");
				var span=1, ns;
				for(var mn = 0; mn < records.length; mn++) {
					if(records[mn].contains('card')) { /* assume b410p */
						CARDS++; ns = (span + 3);
						var csp = 1;
						while(span <= ns) {
							if(!PORTS[span]) {
								PORTS[span] = {}; 
								PORTS[span]['card'] = records[mn];
								PORTS[span]['cardno'] = CARDS;
								PORTS[span]['card_portno'] = csp;
							} span++; csp++;
						}
					}
				}
				if(CARDS == 0) {
					ASTGUI.dialog.alertmsg('No mISDN Cards detected (found 0 spans)!');
					_$('div_misdncardstable').style.display = 'none';
					_$('div_misdnTrunkstable').style.display = 'none';
					ASTGUI.feedback( { msg:'No mISDN Cards found !!', showfor:2 });
					return false;
				}
				_$('b410p_cards').innerHTML = "<B>"+CARDS+"</B> B410p Card(s) detected !";
			}catch(err){
				ASTGUI.dialog.alertmsg('No mISDN Cards detected (found 0 spans)!');
				_$('div_misdncardstable').style.display = 'none';
				ASTGUI.feedback( { msg:"No mISDN Cards found !!", showfor:2 });
				return false;
			}
			parent.ASTGUI.dialog.hide();
			misdnConfig.readCardsConfiguration(); // reads and parses misdn-init.conf

			return true;
		});
	},
	readCardsConfiguration: function(){
		(function(){
			var u = new listOfSynActions("misdninit_guiRead.conf") ;
			u.new_action('delcat', 'general', '', ''); 
			u.new_action('newcat', 'general', '', ''); 
			u.new_action('update', 'general', '#include "../misdn-init.conf" ;', ' ;');
			u.callActions();
		})();
		var parseMisdnInit = function(k){
			/*
			// gui expects misdn-init.conf in the following format
			card = 1,0x4
			te_ptp = 1,2,3,4
			poll = 128
			dsp_options = 0
			debug = 0 
			*/
			var records = k['general'] ; // TODO , handle case where k['general'] is not found
			var a,b,c,d;

			for(var t=0; t < records.length; t++ ){
				if( records[t].beginsWith('nt_ptp') || records[t].beginsWith('nt_ptmp') || records[t].beginsWith('te_ptp') || records[t].beginsWith('te_ptmp')  || records[t].beginsWith('te_capi_ptp') || records[t].beginsWith('te_capi_ptmp') ){
					a = records[t].split("=")[0];
					b = records[t].split("=")[1];
					if(!b.length){ continue; }
					if( b.contains(",") ){
						c = b.split(",");
						c.each( function(d) {
							if(PORTS[d]){
								PORTS[d]['portType'] = a.trim(); // set port type of port d to a
							}
						});
					}else{
						if(PORTS[b]){
							PORTS[b]['portType'] = a.trim(); // set port type of port b to a
						}
					}
				}

				if(records[t].beginsWith('option=')){ // 'option=1,master_clock' or 'option=2,ais,nocrc4'
					var a = ASTGUI.parseContextLine.read(records[t]); // a[1] is '1,master_clock' or '2,ais,nocrc4'
					var b = a[1].split(',')[0]; // b is is the misdn port numer
					var c = records[t].split(',').slice(1).join(','); // 'master_clock' or 'ais,nocrc4'
					PORTS[b]['option'] = c ;
				}
			}
			// done parsing misdn-init.conf
			// show table
			ASTGUI.miscFunctions.createConfig( 'applymisdn.conf', function(){ } ) ;// just to make sure it is there later when we write to it
			try{
				misdnConfig.showMisdnConfiginTable();
			}catch(err){

			}finally{
				load_mISDNtrunks();
			}
		};
		var q = config2json({filename:"misdninit_guiRead.conf", usf:0});
		parseMisdnInit(q) ;
	},

	showMisdnConfiginTable: function(){
		var tbl = _$('misdntable') ;
		var add_fRow = function(){
			var newRow = tbl.insertRow(-1);
			newRow.className = "frow";
			ASTGUI.domActions.tr_addCell( newRow , { html: 'Card/Port' }) ;
			ASTGUI.domActions.tr_addCell( newRow , { html: 'Mode' }) ;
			ASTGUI.domActions.tr_addCell( newRow , { html: '' }) ;
		};
		var addrow_totable = function(port_no){
			var sno = tbl.rows.length + 1;
			var newRow = tbl.insertRow(-1);
			newRow.id = 'misdntable_r'+port_no;
			newRow["port_no"] = port_no;
			if( PORTS[port_no]['edited'] ){ newRow.style.background = "#C9AAAA"; }

			ASTGUI.domActions.tr_addCell( newRow , { html: PORTS[port_no]['cardno'] + "/" + PORTS[port_no]['card_portno'], align: 'center' }) ;
			ASTGUI.domActions.tr_addCell( newRow , { html: pmode_defs[PORTS[port_no]['portType']], align: 'center' }) ;
			ASTGUI.domActions.tr_addCell( newRow , { html: '<input type=button value="Edit" onclick="edit_port(' + port_no  + ')">', align: 'center' }) ;
		};
		ASTGUI.domActions.clear_table(tbl);
		add_fRow();
		for( var k in PORTS ){ if( PORTS.hasOwnProperty(k) ){ addrow_totable(k); }}
	}
};


function edit_port(p){
	if( !PORTS[p].hasOwnProperty('option') ) { PORTS[p]['option'] = ''; }
	ASTGUI.selectbox.selectOption( _$('editport_option'), PORTS[p]['option'] );
	ASTGUI.selectbox.selectOption( _$('editport_type'), PORTS[p]['portType'] );
	_$('editport_label').innerHTML = PORTS[p]['cardno'] + "/" + PORTS[p]['card_portno'] ;
	$('#edit_port').showWithBg();
	_$('edit_port')['port_editing'] = p;
}


function canelPortInfo(){
	$('#edit_port').hideWithBg();
}

function updatePortInfo(){
	var p = _$('edit_port')['port_editing'];
	PORTS[p]['portType'] = _$('editport_type').value;
	if(  _$('editport_option').value == 'master_clock' ){ // condition to make sure none of the other spans on this card is a master_clock
		(function (){
			var this_card = PORTS[p]['cardno'] ;
			var other_ports_onThisCard = [];
			for(var d in PORTS){if( PORTS.hasOwnProperty(d) && PORTS[d]['cardno'] == this_card && d != p ){	
				other_ports_onThisCard.push(d);
			}}

			for(var y=0; y < other_ports_onThisCard.length ; y ++){
				var k = other_ports_onThisCard[y];
				if( PORTS[k].hasOwnProperty('option') && PORTS[k]['option'] == 'master_clock'){PORTS[k]['option'] = ''; }
			}
		})(); 
	}

	PORTS[p]['option'] = _$('editport_option').value ;
	PORTS[p]['edited'] = true;
	misdnConfig.showMisdnConfiginTable();
	$('#edit_port').hideWithBg(); 
	// _$('misdntable_r'+p).style.background = "#C9AAAA";

}

function reloadpage(){
 	window.location.reload(); 
}


function generate_applyMisdn(){
 //save the PORTS object with some default options into /etc/misdn-init.conf via /etc/asterisk/applymisdn.conf
	 // first generate the output into [general] section of applymisdn.conf

	var d= 'general', e, addedcards = {};
	var pmode_ports = {'te_ptp':[] , 'te_ptmp':[] ,'te_capi_ptp':[] , 'te_capi_ptmp':[] ,'nt_ptp':[] , 'nt_ptmp': [] };

	var x = new listOfActions();
	x.filename('applymisdn.conf');
	x.new_action('delcat', d , "", "");
	x.new_action('newcat', d , "", "");

	for( var k in PORTS ){ if( PORTS.hasOwnProperty(k) ){ 
		e = PORTS[k]['card'].split('=')[1] ;
		if(!addedcards[e]){
			addedcards[e] = true;
			x.new_action('append', d , 'card', e );
		}
		if(PORTS[k]['portType']){ pmode_ports[PORTS[k]['portType']].push(k); }
	}}

	var tmp_optionMaster_clock ;
	for( var k in PORTS ){ if( PORTS.hasOwnProperty(k) ){
		if( PORTS[k]['option'] ){
			x.new_action('append', d , 'option',  k + ',' + PORTS[k]['option']);
		if( PORTS[k]['option'] == 'master_clock' ){
			tmp_optionMaster_clock = k;
		}
		}
	}}
	if( !tmp_optionMaster_clock ){
		x.new_action( 'append', d , 'option', '1,master_clock' );
	}

	for( var k in pmode_ports ){ if( pmode_ports.hasOwnProperty(k) && pmode_ports[k].length ){
		x.new_action('append', d , k , pmode_ports[k].join(','));
	}}

	x.new_action('append', d, "poll", "128" );
	x.new_action('append', d, "dsp_options", "0");
	x.new_action('append', d, "debug" , "0");
	x.callActions(function(){
		// call the script that would generate /etc/misdn-init.conf from /etc/asterisk/applymisdn.conf
		parent.ASTGUI.systemCmd( top.sessionData.directories.script_generatemISDN_init + " applysettings", function(){ 
			alert("You need to restart your machine for these settings to take effect");
			window.location.reload();
			return true; 
		});
	});
}


var showMisdnTrunksinTable = function(){
	var tbl = _$('misdntrunkstable') ;
	var add_fRow = function(){
		var nr = tbl.insertRow(-1); nr.className = "frow";
		ASTGUI.domActions.tr_addCell( nr , { html: 'Trunk Name' }) ;
		ASTGUI.domActions.tr_addCell( nr , { html: 'Ports' }) ;
		ASTGUI.domActions.tr_addCell( nr , { html: '' }) ;
	};

	var addrow_totable = function(trunk){
		var sno = tbl.rows.length + 1;
		var nr = tbl.insertRow(-1);
		//newRow.id = 'misdntable_r'+port_no;
		//newRow["port_no"] = port_no;
		//if( PORTS[port_no]['edited'] ){ newRow.style.background = "#C9AAAA"; }

		ASTGUI.domActions.tr_addCell( nr , { html: mISDNTRUNKS[trunk]['trunkname'] , align: 'center' }) ;
		ASTGUI.domActions.tr_addCell( nr , { html: mISDNTRUNKS[trunk]['ports'] , align: 'center' }) ;
		ASTGUI.domActions.tr_addCell( nr , { html: "<input type=button value='Delete' onclick=\"delete_trunk('" + trunk  + "')\">" , align:'center'});
	};

	ASTGUI.domActions.clear_table(tbl);
	add_fRow();
	for( var k in mISDNTRUNKS ){ if( mISDNTRUNKS.hasOwnProperty(k) ){ addrow_totable(k); }}
	if(tbl.rows.length == 1){
		tbl.style.display = 'none';
		_$('div_noTrunks').innerHTML = '<BR><BR>You do not have any mISDN trunks defined';
	}else{
		tbl.style.display = '';
		_$('div_noTrunks').innerHTML = '<BR>List of mISDN Service providers (trunks)';
	}
}

function load_mISDNtrunks(){
	var q = config2json({filename:"misdn.conf", usf:1});
	var parseUsersConf = function(n){
		for( var l in n ){ 
			if( n.hasOwnProperty(l) && l.beginsWith('trunk_m') && n[l]['hasmisdn'] =='yes' ){
				if(!mISDNTRUNKS[l]) { mISDNTRUNKS[l] = {};}
				mISDNTRUNKS[l]['trunkname'] = n[l]['trunkname'];
				mISDNTRUNKS[l]['context'] = n[l]['context'];
				mISDNTRUNKS[l]['ports'] = n[l]['ports'];
			}
		};
		showMisdnTrunksinTable();
	};
	parseUsersConf(q); // TODO  - handle File not found 
}


var new_misdntrunk = function(){
	isnewTrunk = true;
	_$('edit_MTrunk_ports').value = '';
	_$('edit_MTrunk_trunkName').value = '';
	$('#edit_trunk').showWithBg();
};

var canelTrunkInfo = function(){
	$('#edit_trunk').hideWithBg();
}

var edit_trunk = function(k){
	isnewTrunk = false;
	$('#edit_trunk').showWithBg();
	_$('edit_trunk')['trunk_being_Edited'] = k;
};


var delete_trunk = function(k){
	if(!confirm("Are you sure you want to delete this BRI Trunk ?")){ return true; }
	//delete in misdn.conf
	// delete in globals in extensions.conf
	// delete the DID context in extensions.conf
	(function(){
		var u = new listOfSynActions('misdn.conf') ;
		u.new_action('delcat', k, "", "");
		u.callActions();
	})();
	(function(){
		var u = new listOfSynActions('extensions.conf') ;
		u.new_action('delcat', 'DID_' + k , "", "");
		u.new_action('delete', 'globals', k, '', 'mISDN/g:' + k );
		u.callActions();
		top.window.location.reload();
	})();
};

var updateTrunkInfo = function(){

	if(isnewTrunk){
		// create new trunk
		// get the available trunk number
		var getnextavailabletrunk_number = function(){
			var ts = [];
			for(var t in mISDNTRUNKS){ if( mISDNTRUNKS.hasOwnProperty(t) ){
					var i = t.split('trunk_m');
					ts.push( Number(t.split('trunk_m')[1] ) );
			}}

			if(!ts.length){ return "1"; }

			var c = 1 ;
			var sortNumbers =function(a,b){return a - b};
			ts.sort(sortNumbers);
			for(var u =0 ; u < ts.length ; u++ ){
				if( c < ts[u] ){ return c;}
				c++ ;
			}
			return c;
		};
		var y = getnextavailabletrunk_number();
		var newtrunkname =  "trunk_m" + y;
		
	/////////////////uri += build_action('append' , c , d , "poll" , "128") ; c++ ;
	/////////////////	uri += build_action('newcat', c, d , "", ""); c++;
	////////////////uri += build_action('update', c, 'general', '#include "../misdn-init.conf" ; = ', ''); c++;	

		var x = new listOfActions();
		x.filename('misdn.conf');
		x.new_action('newcat', newtrunkname , "", "");
		x.new_action('append', newtrunkname , "trunkname", _$('edit_MTrunk_trunkName').value  );
		x.new_action('append', newtrunkname , "context", "DID_" + newtrunkname);
		x.new_action('append', newtrunkname , "ports", _$('edit_MTrunk_ports').value);
		x.new_action('append', newtrunkname , "hasmisdn", 'yes');
		x.new_action('append', newtrunkname , "msns", '*');

		x.callActions(function(){
			var u = new listOfSynActions('extensions.conf') ;
			u.new_action('newcat', "DID_"+newtrunkname, "", "");
			u.new_action('update', 'globals', newtrunkname, 'mISDN/g:' + newtrunkname );
			u.callActions();
			try{
				if(mISDNTRUNKS[newtrunkname]) { delete mISDNTRUNKS[newtrunkname];}
				mISDNTRUNKS[newtrunkname] = {};
				mISDNTRUNKS[newtrunkname]['trunkname'] = _$('edit_MTrunk_trunkName').value ;
				mISDNTRUNKS[newtrunkname]['context'] = "DID_" + newtrunkname;
				mISDNTRUNKS[newtrunkname]['ports'] = _$('edit_MTrunk_ports').value;
				showMisdnTrunksinTable();
			}catch(err){

			}finally{
				_$('edit_trunk').style.display = "none";
				alert('BRI trunk created. \n Click OK to reload');
				top.window.location.reload();
			}
		});
	}else{
		// update existing trunk
		var k = _$('edit_trunk')['trunk_being_Edited'];
		var u = new listOfSynActions('misdn.conf') ;
		u.new_action('update', k, 'ports', _$('edit_MTrunk_ports').value );
		u.new_action('update', k, 'trunkname', _$('edit_MTrunk_trunkName').value );
		u.callActions();
		try{
			mISDNTRUNKS[k]['trunkname'] = _$('edit_MTrunk_trunkName').value ;
			mISDNTRUNKS[k]['ports'] = _$('edit_MTrunk_ports').value ;
			showMisdnTrunksinTable();
		}catch(err){

		}finally{
			_$('edit_trunk').style.display = "none";
		}
	}
}


function localajaxinit(){
	top.document.title = 'misdn Cards Configuration' ;
	parent.ASTGUI.dialog.waitWhile('Detecting BRI Cards ...');
	//setTimeout( function(){ parent.ASTGUI.dialog.hide(); } , 4000);
	ASTGUI.miscFunctions.createConfig( 'misdninit_guiRead.conf', function(){
		ASTGUI.systemCmdWithOutput( " echo '' ", function(){ // clean up output file
			misdnConfig.detectCards();
		});
	});
}
