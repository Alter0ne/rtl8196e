/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * Javascript functions for accessing manager over HTTP and Some UI components/functions used in AsteriskGUI.
 *
 * Copyright (C) 2006-2011, Digium, Inc.
 *
 * Mark Spencer <markster@digium.com>
 * Pari Nannapaneni <pari@digium.com>
 * Ryan Brindley <rbrindley@digium.com>
 * Erin Spiceland <espiceland@digium.com>
 *
 * Thanks to Steven Levithan (http://stevenlevithan.com) for his
 * implementation of trim()
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

_$ = function(x){
	if ( typeof x != 'string' ){ return null ;}
	try{
		return document.getElementById(x); 
	}catch(err){ return null; }
};

// Some custom methods to Array Objects
	Array.prototype.replaceAB = function(a, b) { // return new array with all instances of a replaced with b
		var x =[];
		for(var i=0, j = this.length ; i < j; i++ ){
			if( this[i] === a ){
				x.push(b);
			}else{
				x.push(this[i]);
			}
		}
		return x;
	};

	Array.prototype.lastValue = function(){
		// [0,1,2]
		return (this.length)? this[this.length - 1] : null;
	};

	Array.prototype.replaceLastWith = function(a){
		if( this.length )
			this[this.length - 1] = a ;
	}

	Array.prototype.contains = function(str) {
		return this.indexOf(str) != -1 ;
	};

	Array.prototype.containsLike = function(str) {
		return this.indexOfLike(str) != -1;
	};
	
	/* This cannot be used if you need to return from the calling
	function early. The return statement will return from this 
	anonymous function instead. 
	
	This method can also introduce race conditions if you use the array
	right after calling it. */
	Array.prototype.each = function(iterator) {
		for(var i=0 , j = this.length; i < j ; i++ ){
			iterator(this[i] , i);
		}
	};

	Array.prototype.forEach = function(iterator) { // call a function on each element and update the element with the returned value
		var i = this.length;
		while (i--) {
			this[i] = iterator(this[i] , i);
		}
	};
	
	Array.prototype.firstAvailable = function(start) {
		start = (!start)? 1 : Number( start );
		if(!this.length){
			return start;
		}
		var x = [];
		for( var y=0 ; y < this.length ; y++ ){
			var NT = Number(this[y]) ;
			if( NT < start )
				continue;
			x.push(NT);
		}
		if( !x.length ){
			return start;
		}
		while(true){
			if( x.contains(start) ){
				start++;
			}else{
				return start;
			}
		}
	};

	Array.prototype.removeFirst = function(){ // opposite of push - removes the first element of the array
		this.splice(0,1);
	};

	Array.prototype.removeLast = function(){ // removes the last element of the array
		this.pop();
	};
	
	if(!Array.indexOf){
		Array.prototype.indexOf = function(a){
			var i = this.length;
			while (i--) {
				if( this[i] === a ){
					return i;
				}
			}
			return -1;
		}
	}

	Array.prototype.indexOfLike = function( searchString ){
		if(!searchString.length){ return -1; }
		for(var i=0; i < this.length; i++ ){ if( this[i].beginsWith(searchString) ){ return i ; } }
		return -1 ;
	};

	Array.prototype.lastIndexOfLike = function( searchString ){
		if(!searchString.length){ return -1;}
		var i = this.length;
		while (i--) {
			if( typeof this[i] == 'string' && this[i].beginsWith(searchString) ){ return i; }
		}
		return -1 ;
	};

	Array.prototype.push_IfNotPresent = function( a ){
		if(!this.contains(a)) this.push(a);
	};
	
	Array.prototype.sortNumbers = function() {
		return this.sort(function(a,b){return a - b});
	};
	
	Array.prototype.withOut = function(e) {
		var x =[];
		if( typeof e == 'string' || typeof e == 'number' ){
			var y = [e];
		}else if( e instanceof Array ){
			var y = e;
		}else{
			return this;
		}

		for( var a =0 ; a < y.length ; a++ ){
			var b = y[a];
			for( var i=0, j=this.length ; i < j ; i++ ){
				if( !(this[i] === b) && !y.contains(this[i]) && !x.contains(this[i]) ){
					x.push(this[i]);
				}
			}
		}

		return x ;
	};

// String Manipulation, and other custom methods for String Objects
	String.prototype.addZero = function(){
		return ( Number(this) < 10)? "0" + this : this;
	};

	String.prototype.afterChar = function(k){
		if(k.length > 1){ alert('String.afterChar() should be used with a single character'); return null;}
		var v = this.indexOf(k);
		if( v == -1){ return ''; }
		return this.substring(v+1);
	};

	String.prototype.afterStr = function(x){
		if( !this.contains(x) ){ return ''; }
		if(x.length == 1){ return this.afterChar(x); }
		var pos = this.indexOf(x) + x.length ;
		return this.substr(pos);
	};

	String.prototype.beforeChar = function(k){
		if(k.length > 1){ 
			alert('String.beforeChar() should be used with a single character');
			return null;
		}
		var v = this.indexOf(k);
		if( v == -1){ return ''; }
		return this.substring(0,v);
	};

	String.prototype.beforeStr = function(x){
		var r = this.afterStr(x);
		return this.withOut(x+r);
	};

	String.prototype.beginsWith = function(a){
		return this.length>=a.length && this.substring(0,a.length)==a
	};

	String.prototype.betweenXY = function(X,Y){
		if(X.length > 1 || Y.length > 1){ alert('String.betweenXY() accepts single character arguments'); return null;}
		var t = this.afterChar(X);
		return t.beforeChar(Y);
	};

	String.prototype.bold_X = function(x){
		if(x==''){return this ;}
		var position = this.toLowerCase().indexOf( x.toLowerCase() ) ;
		if (position == -1){ return this; }
		var c = this.substr( position , x.length );
		return  this.replace( c, "<B>" + c + "</B>" , "" );
	};
	
	String.prototype.camelize = function(){
	    var parts = this.split(' '), len = parts.length;
		var camelized = '';
	    for (var i = 0; i < len; i++)
	      camelized += parts[i].charAt(0).toUpperCase() + parts[i].substring(1) + ' ';
	    return camelized;
	};

	String.prototype.capitalizeFirstChar = function() {
		return this.charAt(0).toUpperCase() + this.substring(1).toLowerCase();
	};

	String.prototype.contains=function(a){
		return this.indexOf(a)!=-1;
	};

	String.prototype.endsWith=function(a){
		return this.length >= a.length && this.substring(this.length-a.length)==a
	};

	String.prototype.escapeHTML = function() {
		var a = document.createTextNode(this);
		var b = document.createElement('div');
		b.appendChild(a);
		return b.innerHTML;
	};

	String.prototype.isAstTrue = function () {
		return ["yes", "true", "y", "t", "1", "on"].contains(this.toLowerCase().trim());
	};

	String.prototype.getNoOp = function(){
		return ( this.toLowerCase().indexOf('noop(') == -1 ) ? '' : this.betweenXY('(',')') ; // todo: handle multiple ')'s
	};

	String.prototype.guiMetaData = function(){
		return this + ' ; GUI metadata';
	};

	String.prototype.isValueInBetween = function (a,b) {
		a = Number(a);
		b = Number(b);
		var c = Number(this) , a1 = Math.min(a,b) , b1 = Math.max(a,b);
		return ( c >= a1 && c <= b1 ) ? true : false ;
	};

	String.prototype.lChop = function(c){ // chop a string from the beginning of the string
		if(this.beginsWith(c)){
			return this.substr(c.length);
		}
		return this;
	};

	String.prototype.rChop = function(c){ // chop a string from the end of the string
		if( this.indexOf(c) == -1 || !this.endsWith(c) ){
			return String(this); //actually we should be doing 'return this;' but for some reason firebug is reporting the returned string as an object
		}
		return this.substr( 0, this.length - c.length);
	};

	String.prototype.replaceXY = function(X,Y){
		return this.split(X).join(Y);
	};

	String.prototype.nl2br = function(){ // replace new lines with <BR>
		return this.split('\n').join('<BR>');
	};

	String.prototype.times = function(a){
		return ( a < 1 ) ? '' : new Array(a+1).join(this);
	};

	String.prototype.stripTags = function() {
		return this.replace(/<\/?[^>]+>/gi, '');
	}

	String.prototype.trim = function(){
		/* Thanks to Steve Levithan (http://stevenlevithan.com) for this code */
		var str = this.replace(/^\s\s*/, ''),
				ws = /\s/,
				i = str.length;
		while (ws.test(str.charAt(--i)));
		return str.slice(0, i+1);
	};

	String.prototype.withOut = function(k){
		return this.split(k).join('');
	};

	String.prototype.versionGreaterOrEqualTo = function(c){
		var v = this.split(".");
		var c = c.split(".");
		for(var i = 0; i < 6; i++){
			c[i] = parseInt(c[i] || 0);
			v[i] = parseInt(v[i] || 0);
			if(v[i] > c[i]){
				return true;
			}else if(v[i] < c[i]){
				return false;
			}
		}
		return true; /* They are equal */
	};


Number.prototype.addZero = function(){
	return ( this < 10)? "0" + String(this) : String(this);
};

Number.prototype.isValueInBetween = function (a,b) {
	a = Number(a);
	b = Number(b);
	var a1 = Math.min(a,b) , b1 = Math.max(a,b);
	return ( this >= a1 && this <= b1 ) ? true : false ;
};

Number.prototype.guiMetaData = function(){
	return String(this) + ' ; GUI metadata';
};

// The ASTGUI Object - global varibles and various core GUI component functions
var ASTGUI = {
	includeContexts: [], // updated below

	globals: {
		providerUrl: './js/providers.js', // ASTGUI.globals.providerUrl
		firmwareVersionUrl: 'https://gui-dl.digium.com/aa50/fw_version.js', // ASTGUI.globals.firmwareVersionUrl
		appname : 'Asterisk GUI',
		lang : 'en',
		GUI_DB : 'astgui', // name of the ASTDB database used by GUI -- ASTGUI.globals.GUI_DB
		msg_notLoggedIn: 'Message: Authentication Required',
		configfile : 'guipreferences.conf', // will be created if the file does not exist , ASTGUI.globals.configfile
		g729RegInfo: 'g729reginfo.conf', // ASTGUI.globals.g729RegInfo, the sessionData.directories.script_Registerg729 script will read this file to generate tab delimited file
		hwcfgFile: 'gui_confighw.conf', // file to store configured hardware information, to detect hardware changes
		dahdiIncludeFile: 'dahdi_guiread.conf', // file that will be used to read zaptel.conf or dahdi/system.conf , ASTGUI.globals.dahdiIncludeFile
		dahdiScanOutput: 'dahdi_scan.conf', // file that will be used to read output from ztscan or dahdi_scan, ASTGUI.globals.dahdiScanOutput
		pingInterval : 5000,
		app_factoryReset : '/bin/reset_config', // ASTGUI.globals.app_factoryReset
		fnf : 'ERROR:FNF',
		obcidstr : 'GLOBAL_OUTBOUNDCID', // ASTGUI.globals.obcidstr
		obcidNamestr : 'GLOBAL_OUTBOUNDCIDNAME', // ASTGUI.globals.obcidNamestr
		obcidUsrPrefix : 'CID_', // ASTGUI.globals.obcidUsrPrefix
		sbcid_1 : 's,1,ExecIf($[ "${CALLERID(num)}"="" ],SetCallerPres,unavailable)', // ASTGUI.globals.sbcid_1
		sbcid_2 : 's,2,ExecIf($[ "${CALLERID(num)}"="" ],Set,CALLERID(all)=unknown <0000000>)',
		timeservers: [ 'north-america.pool.ntp.org', 'asia.pool.ntp.org', 'europe.pool.ntp.org', 'oceania.pool.ntp.org', 'south-america.pool.ntp.org' ],
		version : '2.0' // gui version
	},

	contexts: {
		guitools : 'asterisk_guitools', // gui tools context
		dialtrunks : 'trunkdial-failover-0.3', // trunkdial macro with failback trunk and setcid, ASTGUI.contexts.dialtrunks
		localcrcid: 'local-callingrule-cid-0.1', // setcid for local calling rules, ASTGUI.contexts.localcrcid
		subscribe : 'default',	//subscribecontext for sip.conf (aka devicestate fun)
		CONFERENCES : 'conferences', // ASTGUI.contexts.CONFERENCES
		QUEUES : 'queues', //ASTGUI.contexts.QUEUES
		TrunkDIDPrefix : 'DID_', // context for trunks -  - ASTGUI.contexts.TrunkDIDPrefix 
		TrunkDefaultSuffix : '_default', // ASTGUI.contexts.TrunkDefaultSuffix - to create 'DID_trunk_default' that will be included in [DID_trunk]
		RingGroupPrefix: 'ringroups-custom-', // ASTGUI.contexts.RingGroupPrefix 
		RingGroupExtensions: 'ringgroups', // ASTGUI.contexts.RingGroupExtensions
		PageAnExtension :'page_an_extension', // ASTGUI.contexts.PageAnExtension
		PageGroups : 'pagegroups', // ASTGUI.contexts.PageGroups
		TimeBasedRulePrefix: 'timebasedrule-custom-', // ASTGUI.contexts.TimeBasedRulePrefix 
		TimeIntervalPrefix: 'timeinterval_', // ASTGUI.contexts.TimeIntervalPrefix
		VoiceMenuPrefix: 'voicemenu-custom-', // ASTGUI.contexts.VoiceMenuPrefix
		VoiceMenuExtensions: 'voicemenus', // ASTGUI.contexts.VoiceMenuExtensions
		VoiceMailGroups: 'voicemailgroups', // ASTGUI.contexts.VoiceMailGroups
		Directory: 'directory', // ASTGUI.contexts.Directory
		CallingRulePrefix : 'CallingRule_', // context for calling rules being with - ASTGUI.contexts.CallingRulePrefix 
		CallingPlanPrefix: 'DLPN_', // context for DialPlans -- ASTGUI.contexts.CallingPlanPrefix
		gtalkIncomingContext: 'gtalk_incoming_', // ASTGUI.contexts.gtalkIncomingContext
		skypeIncomingContext: 'skype_incoming_', // ASTGUI.contexts.skypeIncomingContext
		mohdirPrefix : 'guimohdir_' // ASTGUI.contexts.mohdirPrefix
		// music on hold directories created by gui will have this prefix
		// also post_mappings definitions in http.conf will have this name
	},

	errorCodes:{
		'AG101':'Aborting Upload : Action not defined for upload Form <BR>' + 
			'Please set the Form action in the parent page via onUploadForm_beforeUploading()',
		'AG102':'Disabling all Upload forms in the gui: <BR>' +
			'Either post_mappings or post_mappings->uploads is not defined in http.conf',
		'AG150':' SyncActions being used for more than 5 actions'
	},

	ASTDB:{
		updateKey : function( k ){ 
			// ASTGUI.ASTDB.updateKey( { dbname: 'astgui', key : 'keyname', keyvalue : 'keyvalue' } );
			// dbname is optional, defaults to ASTGUI.globals.GUI_DB
			// 	returns true if success, false otherwise
			if( !k.hasOwnProperty('dbname') ){
				k.dbname = ASTGUI.globals.GUI_DB ;
			}

			var s = ASTGUI.cliCommand('database put '+ k.dbname + ' ' + k.key + ' ' + k.keyvalue );
			if(s.contains('successfully')) return true;
			return false;
		},

		deleteKey : function( k ){
			// ASTGUI.ASTDB.deleteKey( { dbname: 'astgui', key : 'keyname' } );
			// dbname is optional, defaults to ASTGUI.globals.GUI_DB
			// 	returns true if success, false otherwise
			if( !k.hasOwnProperty('dbname') ){
				k.dbname = ASTGUI.globals.GUI_DB ;
			}

			var s = ASTGUI.cliCommand('database del '+ k.dbname + ' ' + k.key);
			if(s.contains('entry removed')) return true;
			return false;
		},


		getKey : function(k){
			// ASTGUI.ASTDB.getKey( { dbname: 'astgui', key : 'keyname' } );
			// dbname is optional, defaults to ASTGUI.globals.GUI_DB
			// returns null if key is not found, otherwise returns the key-value 
			if( !k.hasOwnProperty('dbname') ){
				k.dbname = ASTGUI.globals.GUI_DB ;
			}

			var s = ASTGUI.cliCommand('database get '+ k.dbname + ' ' + k.key);
			if( s.contains('entry not found')) return null;
			var op = ASTGUI.parseCLIResponse( s );
			op = op.trim();
			op = op.withOut('Value:')
			return op.trim();
		},

		getAllKeys : function(k){
			// ASTGUI.ASTDB.getAllKeys( { dbname: 'astgui' } );
			// dbname is optional, defaults to ASTGUI.globals.GUI_DB
			// returns an object with all the keys in the database as properties and key-values as propertyvalues
			// returns a null if database not found

			if( !k.hasOwnProperty('dbname') ){
				k.dbname = ASTGUI.globals.GUI_DB ;
			}

			var db_keys = {};
			var tmp_dbpath = '/' + k.dbname + '/' ;
			var s = ASTGUI.cliCommand('database show '+ k.dbname);

			if(s.contains('entry not found')) return null;
			var op = ASTGUI.parseCLIResponse( s );
			if( op.trim() == ''){ return {}; }

			var tmp_lines = op.trim().split('\n');
			tmp_lines.each( function(this_line){
				var this_line = this_line.withOut(tmp_dbpath);
				var this_key = this_line.beforeChar(':').trim();
				var this_keyVal = this_line.afterChar(':').trim();
				db_keys[ this_key ] =  this_keyVal ;
			});

			return db_keys;
		}
	},

	checkRequiredFields: function( fields ){
		// fields is an array of fieldnames or elements
		if(!ASTGUI.isArray(fields)){
			return true;
		}
		for(var g=0; g < fields.length ; g++ ){
			var field = fields[g];
			if(typeof field =='string'){
				field = _$(field); 
			}
			var required = $(field).attr('required');
			if( required && required.toString().isAstTrue() ){
				var x = field.value.trim() ;
				var pcn = ( field.className ) ? field.className : '' ;
				if( !x ){
					ASTGUI.feedback( { msg:'Required Field', showfor:2 } );
					field.className = 'inputValidationFailed';
					setTimeout( function(){ field.className = pcn ; } , 4000 );
					try{ field.focus(); }catch(e){}
					return false;
				}
			}
		}
		return true;
	},

	cliCommand : function(cmd) { 
		// ASTGUI.cliCommand(cmd);
		//	Execute an asterisk CLI command and return the output
		if( typeof cmd != 'string' ) {
			top.log.warn( 'cliCommand: Expecting cmd as String' );
			return '';
		}
		top.log.debug("Executing manager command : '" + cmd + "'");
		return makeSyncRequest ( { action :'command', command: cmd } );
	},

	getUser_DeviceStatus : function( usr ){ 
		// ASTGUI.getUser_DeviceStatus(usr) 
		//	Get the DeviceStatus for a UserExtension
		if( typeof usr == 'number' ) usr = String(usr);
		if( typeof usr != 'string' ){
			top.log.warn( 'getUser_DeviceStatus: Expecting usr as String' );
			return 'U';
		}
		var t = makeSyncRequest({ action :'ExtensionState', Exten: usr }) ;
		if( t.contains('Status: 0') ) return 'F' ; // No Device is Busy/InUse
		if( t.contains('Status: 1') ) return 'B' ; // 1 or more devices InUse
		if( t.contains('Status: 2') ) return 'B' ; // All Devices Busy
		if( t.contains('Status: 4') ) return 'U' ; // All Devices Unavailable/Unregistered
		if( t.contains('Status: 8') ) return 'R' ; // All Devices Ringing
		return null;
	},

	getUser_DeviceStatus_Image : function( usr ){
		// ASTGUI.getUser_DeviceStatus_Image(usr) 
		//	Get the DeviceStatus Image for a UserExtension
		var s =  this.getUser_DeviceStatus(usr) ;
		switch(s){
			case 'F': // No Device is Busy/InUse
				return "<img src='images/status_green.png' border=0>";
				break ;
			case 'B': // Busy
				return "<img src='images/status_red.png' border=0>";
				break ;
			case 'R': // Ringing
				return "<img src='images/status_orange.png' border=0>";
				break ;
			case 'U': // UnAvailable
			default :
				return "<img src='images/status_gray.png' border=0>";
				break ;
		}
	},

	mailboxCount : function(mbox){
		// ASTGUI.mailboxCount(mbox)
		//	returns the number of New/Old Voicemail Messages for a user
		//	returns an object "{count_new: 1, count_old: 2}"

		var tr = { count_new:0 , count_old : 0 };
		if( typeof mbox == 'number' ) mbox = String(mbox);
		if( typeof mbox != 'string' ){
			top.log.warn( 'mailboxCount: Expecting mbox as String' );
			return tr;
		}
		if(!mbox.contains('@')){ mbox = mbox + '@default' ; }
		var t = makeSyncRequest ( { action :'MailboxCount', Mailbox: mbox } );
		try{
			var lines = t.split('\n');
			lines.each(function( this_line){
				if(!this_line.contains('Messages:') ) return;
				this_line = this_line.trim();
				if( this_line.contains('NewMessages:') ){
					tr.count_new = Number(this_line.afterChar(':').trim());
				}
				if( this_line.contains('OldMessages:') ){
					tr.count_old = Number(this_line.afterChar(':').trim());
				}
			});
		}finally{
			return tr;
		}
	},

	doTransfer : function(from, to) {
		// ASTGUI.doTransfer(from, to)
		//	issue channel redirect
		top.log.debug("About to transfer " + from + " to " + to);
		return makeSyncRequest ( { action :'redirect', channel :from, exten :to, context :'default', priority :'1' } );
	},

	doHangup : function(chan) {
		// ASTGUI.doHangup(chan)
		//	Hangsup a given channel
		top.log.debug("Executing hangup on channel : '" + chan + "'");
		return makeSyncRequest ( { action :'hangup', channel: chan } );
	},

	cookies: {
		getCookie: function(x){ // ASTGUI.cookies.getCookie('username')
			var ck = top.document.cookie; // mansession_id="6f3fadcb"; username=admin
			if( ck.indexOf( x + '=' ) == -1 ){
				return '';
			}
			var cookies = ck.split(';');
			for(var y=0; y < cookies.length; y++){
				cookies[y] = cookies[y].trim();
				if( cookies[y].beginsWith(x +'=') ){
					return cookies[y].split( x + '=' )[1] ;
				}
			}
			return '';
		},

		setCookie: function(x , y){ // ASTGUI.cookies.setCookie( 'something' , 'valueofSomething' );
			var tmp = x + '=' + y + '; path = /' ;
			top.document.cookie = tmp;
		},

		removeCookie: function(x){
			top.document.cookie = x + '=somevalue; expires=Fri, 22 Oct 1999 00:00:00 UTC; path=/' ;
		},

		clearCookies: function(){  // ASTGUI.cookies.clearCookies()
			top.document.cookie = '';
		}
	},

	cloneObject: function(a){ // ASTGUI.cloneObject(obj)
		if(ASTGUI.isArray(a)){
			return [].concat(a);
		}
		if( typeof a != 'object' ){
			return a;
		}
		var b = new ASTGUI.customObject ;
		for( var i in a ){
			if( a.hasOwnProperty(i) ){
				b[i] = ASTGUI.toCustomObject( a[i] );
			}
		}
		return b;
	},

	CODECSLIST: {
		getSelectedCodecs : function(el){ // ASTGUI.CODECSLIST.getSelectedCodecs(el);
			if ( typeof el == 'string'){ el = _$(el) ; }
			var s = [];
			el.CODECS_SELECTEDORDER.each(function(codec){
				var t = codec.trim();
				if(t != ''){ s.push(codec); }
			});
			return s.join(',') ;
		},

		populateCodecsList : function(el){ // ASTGUI.CODECSLIST.populateCodecsList(el);
		// create codecs check boxes inside el and bind events to each checkbox such that the selected codecs 
			var r =  'codecs_chkbx_class' + Math.round( 10000 * Math.random() );
			if ( typeof el == 'string'){ el = _$(el) ; }
			el.checkboxClass = r;
			el.CODECS_SELECTEDORDER = [];
			ASTGUI.domActions.populateCheckBoxes(el , parent.sessionData.listOfCodecs , r);
			$('.' + r).click(
				function() {
					if(this.checked){
						el.CODECS_SELECTEDORDER.push(this.value);
						return;
					}
					var t = el.CODECS_SELECTEDORDER.withOut(this.value);
					el.CODECS_SELECTEDORDER = t ;
					return ;
				}
			);
		},

		selectCodecs : function(el, codecs){ // ASTGUI.CODECSLIST.selectCodecs(el, codecs);
			// el is the element in which codec checkboxes are populated, codecs is the codecs string
			if ( typeof el == 'string'){ el = _$(el) ; }
			if( codecs == 'all' ){
				ASTGUI.domActions.CheckAll(el.checkboxClass);
				el.CODECS_SELECTEDORDER = ASTGUI.domActions.get_checked(el.checkboxClass);
			}else{
				if( codecs.trim() == '' ){
					el.CODECS_SELECTEDORDER = [] ;
				}else{
					el.CODECS_SELECTEDORDER = ( codecs.contains(',') ) ? codecs.split(',') : codecs;
				}
				ASTGUI.domActions.checkSelected( el.checkboxClass , el.CODECS_SELECTEDORDER ) ;
			}
		}
	},

	COMBOBOX: function (a,w){		// Usage - ASTGUI.COMBOBOX.call( element , OptionsArray, width(Optional)  );
		// this.comboDiv - the div element created
		// this.comboOptions - the array of options
		var e = this;
		$(e).autocomplete(a, { width: w } );
	},

	customObject : function(){
		//	eliminates the need of 'hasOwnProperty' to read this objects propeties, look for this objects prototype below.
		if ( !(this instanceof ASTGUI.customObject) ) { return new ASTGUI.customObject(); }
	},

	TABLE : function(a){
		if(!a) return;
		if ( !(this instanceof ASTGUI.TABLE) ) { return new ASTGUI.TABLE(a); }
		this.TableElement = ( typeof a == 'string' ) ? _$(a) : a ;
		this.TableRow = null ;
	},

	toCustomObject : function(a){// if a is a native object returns an ASTGUI.customObject version of a
		if( ASTGUI.isArray(a) || a === null || typeof a =='number' || typeof a =='string' || typeof a =='boolean' || typeof a != 'object' ) return a;

		var b = new ASTGUI.customObject ;
		for( var i in a ){ if( a.hasOwnProperty(i) ){
			b[i] = ASTGUI.toCustomObject( a[i] );
		}}
		return b;
	},

	Log: {
		doLog: function(msg, color){
			if(!top.sessionData.DEBUG_MODE ){ return true; }
			if( typeof msg == 'object' ){
				msg = 'OBJECT : ' + ASTGUI.getObjectPropertiesAsString(msg) ;
			}

			var now = new Date();
			var h = now.getHours().addZero() + ':' + now.getMinutes().addZero() + ':' + now.getSeconds().addZero() ;
			if( top.sessionData.DEBUG_LOG.length > 250 ){
				top.sessionData.DEBUG_LOG = top.sessionData.DEBUG_LOG.slice(0,50);
			}
			top.sessionData.DEBUG_LOG.unshift( h + ' <font color='+ color +'>' + msg + '</font>' );
		},

		Ajax: function(msg){ // ASTGUI.Log.Ajax(msg);
			if ( top.sessionData.DEBUG_WHICH.Ajax == true ) this.doLog( msg , '#96997C' );
		},

		Debug: function( msg ){ // ASTGUI.Log.Debug();
			if ( top.sessionData.DEBUG_WHICH.Debug == true ) this.doLog( msg , '#4C9996' );
		},

		Error: function( msg ){ // ASTGUI.Log.Error();
			if( !msg || !top.sessionData || !top.sessionData.DEBUG_WHICH.Error ) return;
			if( msg.length <=5 && ASTGUI.errorCodes[msg] ){
				this.doLog( '<B>' + ASTGUI.errorCodes[msg] + '</B>' , '#992b23' );
			}else{
				this.doLog( '<B>' + msg + '</B>' , '#992b23' );
			}
		},

		Console: function( msg ){ // ASTGUI.Log.Console();
			if( top.sessionData.DEBUG_WHICH.Console == true && window.console && window.console.firebug ) console.log ( msg );
		},

		Info: function( msg ){ // ASTGUI.Log.Info(msg);
			if( top.sessionData.DEBUG_WHICH.Info == true ) this.doLog( msg , '#9A9A9A' );
		},

		Warn: function( msg ){ // ASTGUI.Log.Warn( msg );
			if( top.sessionData.DEBUG_WHICH.Warn == true ) this.doLog( msg , '#F47A00' );
		}
	},

	dialog : {
		defaultMessage : 'Loading ...',
		load_iframe : function(msg){
			top.alertframename = "alertiframe" ;
			top.alertmsg = msg ;
			var h,_hs;
			if( !top.document.getElementById(top.alertframename)){
				h= top.document.createElement("IFRAME");
				h.setAttribute("id", top.alertframename );
				h.setAttribute("ALLOWTRANSPARENCY", "true");
				_hs = h.style ;
				_hs.position="absolute";
				_hs.left= 0;
				_hs.top= 0;
				_hs.width= '100%';
				_hs.height= '100%';
				_hs.zIndex = 9999 ;
				h.src = "guialert.html" ;
				h.frameBorder="0";
				h.scrolling="no";
				_hs.filter='progid:DXImageTransform.Microsoft.Alpha(style=0,opacity=90)';
				//h.style.MozOpacity = .90;
				top.document.body.appendChild(h);
			}else{
				top.document.getElementById( top.alertframename ).contentWindow.update( );
				top.document.getElementById( top.alertframename ).style.display = "";
			}
		},

		waitWhile : function(msg){
			// ASTGUI.dialog.waitWhile('Saving...');
			//	use this dialog when you want to inform the user about an action in progress - Ex: 'Saving...' or 'Applying CHanges..' or 'Reloading Asteisk...' etc
			if ( typeof msg != 'string') return;
			top.alertmsgtype = 2 ;
			this.load_iframe(msg);
		},

		alertmsg : function(msg){
			// ASTGUI.dialog.alertmsg('Some Alert Message');
			//	Display custom alert message with an 'Ok' button	
			if ( typeof msg != 'string') return;
			top.alertmsgtype = 1 ;
			this.load_iframe(msg);
		},

		hide : function(){
			// ASTGUI.dialog.hide();
			// Hide the dialog message, use this when you want to hide the 'waitWhile' message
			try{
				top.document.getElementById( top.alertframename ).style.display = "none";
			} catch (err){ }
		},

		show : function(){
			try{
				top.document.getElementById( top.alertframename ).style.display = '';
			} catch (err){ }
		}
	},

	domActions: {
		alignBbelowA: function(a,b, offsetLeft, offsetTop ){
			// ASTGUI.domActions.alignBbelowA( element1, element2 )
			//	Moves/Aligns Element-B below Element-A
			//	You can further control the position by sending additional offset parameters (optional)
			try{
			if ( typeof a == 'string'){ a = _$(a) ; }
			if ( typeof b == 'string'){ b = _$(a) ; }
			b.style.position = 'absolute';
			var tmp_left = a.offsetLeft;
			var tmp_top = a.offsetTop + a.offsetHeight;
			var tmp_parent = a;
	
			while(tmp_parent.offsetParent != document.body){
				tmp_parent = tmp_parent.offsetParent;
				tmp_left += tmp_parent.offsetLeft;
				tmp_top += tmp_parent.offsetTop;
			}
			b.style.left = tmp_left + ( offsetLeft || 0 );
			b.style.top = tmp_top + (offsetTop || 1);
			}catch(err){
				top.log.error(err.description);
			}
		},

		alignBontopofA: function(a,b){
			// ASTGUI.domActions.alignBontopofA();
			//	set Element-B's co-ordinates to those of Element-A
			try{
				if ( typeof a == 'string'){ a = _$(a) ; }
				if ( typeof b == 'string'){ b = _$(b) ; }
				ASTGUI.domActions.alignBbelowA(a,b);
				b.style.top = b.style.top - a.offsetHeight ;
			}catch(err){
				top.log.error(err.description);
			}
		},

		CheckAll: function(x){ // check all checkboxes of class x - ASTGUI.domActions.CheckAll();
			var y = $("." + x) ;
			for(var g=0, h = y.length; g < h; g++){
				y[g].checked = true;
			}
		},

		disableAllofClass: function(x){ // disable all fields of class x
			var y = $("." + x) ;
			for(var g=0, h = y.length; g < h; g++){
				y[g].disabled = true;
			}
		},

		checkIf_isAstTrue: function(el, str){ // ASTGUI.domActions.checkIf_isAstTrue(el,str);
			if ( typeof str != 'string' ){ return; }
			if ( typeof el == 'string' ){ el = _$(el); }
			el.checked = ( str.isAstTrue() ) ? true:false ;
		},

		checkSelected: function(x,y){ // ASTGUI.domActions.checkSelected( 'class', [val1,val2,...]);
			// x is class of checkboxes, y is an array of values, this functions checks a checkbox if it its value is present in array 'y'
			//try{
				var y_copy = ASTGUI.cloneObject(y);
				var chbs = $( "." + x ) ; //jquery selector
				for( var g=0, h = chbs.length; g < h  ; g++ ) {
					chbs[g].checked = ( y_copy.contains(chbs[g].value) ) ? true : false;
				}
			//}catch(err){ }
		},

		disableSelected: function(x,y){	// disable some fields (whose "values" (not names or ids) are in array y), of class x 
			// ASTGUI.domActions.disableSelected( 'class', [1,2] );
			var chbs = $( "." + x ) ; //jquery selector
			for( var g=0, h = chbs.length; g < h  ; g++ ) {
				chbs[g].disabled = ( y.contains(chbs[g].value) ) ? true : false ;
			}
		},

		clear_table: function(h){ // ASTGUI.domActions.clear_table($el)
			ASTGUI.domActions.removeAllChilds(h);
		},

		findPos: function (el){ // returns the 'el.left, el.top' in pixels of a given element
			if ( typeof el == 'string'){ el = _$(el) ; }
			var curleft = curtop = 0;
			if (el.offsetParent) {
				do {
					curleft += el.offsetLeft;
					curtop += el.offsetTop;
				} while (el = el.offsetParent);
			}
			return { cleft: curleft, ctop:curtop } ;
		},

		get_checked: function (x){ // returns an array of selected checkbox values  from a set of checkboxes of class x
			var chk = [] ;
			var y = $( "." + x ) ; //jquery selector
			for( var g=0, h = y.length; g < h  ; g++){
				if(y[g].checked){
					chk.push( y[g].value );
				}
			}
			return chk;
		},

		removeAllChilds: function(x){ // ASTGUI.domActions.removeAllChilds(el);
			if ( typeof x == 'string'){ x = _$(x) ; }
			while(x.firstChild){
				x.removeChild(x.firstChild);
			}
		},

		setTextbox_DefaultValue: function(el, defval){ // usage :: ASTGUI.domActions.setTextbox_DefaultValue(el, "Default Value") ;
			if ( typeof el == 'string'){ el = _$(el) ; }
			el.defaultValue = defval;
			el.value = defval;
			$(el).focus(
				function(){
					if( this.value == this.defaultValue ){
						this.value = '' ;
					};
				}
			).blur(
				function () {
					if( this.value == '' ){
						this.value = this.defaultValue ;
					}
				}
			);
		},

		tr_addCell: function(tr, nc){ // usage :: ASTGUI.domActions.tr_addCell( el, { html:'newCell Text' , align:'center', width:'20px' }  )
			try{
			var ih = nc.html; delete nc.html;
			var newcell = tr.insertCell( tr.cells.length );
			if( nc.id ){ newcell.id = nc.id ; delete nc.id; }
			newcell.innerHTML = ih;
			if( nc.onclickFunction && typeof nc.onclickFunction == "function" ){
				ASTGUI.events.add( newcell , 'click' , nc.onclickFunction ) ;
				$(newcell).css('cursor', 'pointer');
				delete nc.onclickFunction;
			}
			for( var k in nc){
				if( nc.hasOwnProperty(k) ){
					if(k.toLowerCase() == 'colspan'){
						newcell.colSpan = nc[k];
					}else{
						newcell[k] = nc[k];
					}
				}
			}
			}catch(err){
				top.log.error(err.description);
			}
		},

		unCheckAll: function(x){ // uncheck all checkboxes of class x
			var y = $("." + x) ; //jquery selector
			for(var g=0, h = y.length; g < h; g++){
				y[g].checked = false;
			}
		},

		populateCheckBoxes: function( div, values, ofclass, withBR){ // ASTGUI.domActions.populateCheckBoxes(div, values, ofclass);
			// represent 'array values' OR 'Object values' as a list of checkboxes of class 'ofclass' as childnodes of element 'div'
			// Ex: values = { '1':'One', '1':'Two' } or values = [1,2]
			try{
			var c = {};
			if(ASTGUI.isArray(values)){
				values.each( function(tv){ c[tv] = tv; } );
			}else{
				c = values;
			}
			if ( typeof div == 'string'){ div = _$(div) ; }
			for(var d in c){
				if(c.hasOwnProperty(d)){
					var nbr = document.createElement( 'SPAN' ) ;
					var lbl = document.createElement( 'label' ) ;
						var ncbx = document.createElement('input') ;
							ncbx.type = 'checkbox' ;
							ncbx.value = d ;
							ncbx.id = Math.round(10000*Math.random()) + '_' + d ;
							ncbx.className = ofclass ;
						var span = document.createElement('SPAN') ;
						
						span.innerHTML = (withBR) ? c[d] + '&nbsp;<BR>' : c[d] + '&nbsp;' ;
						
					lbl.appendChild( ncbx ) ;
					lbl.appendChild( span ) ;
					//var tmp_span = document.createElement('SPAN') ; tmp_span.innerHTML = '&#173;'; //tmp_span.innerHTML = '&#173;';
					nbr.appendChild(lbl) ;
					div.appendChild( nbr ) ;
				}
			}
			}catch(err){
				top.log.error(err.description);
			}
		},

		showHideByCheckBox: function(chk , el){ // ASTGUI.domActions.showHideByCheckBox (chk, el) ;
			if ( typeof chk == 'string'){ chk = _$(chk) ; }
			if ( typeof el == 'string'){ el = _$(el) ; }
			chk.updateStatus = function(){ el.style.display = (chk.checked)?'':'none'; } ;
			ASTGUI.events.add( chk, 'click' , chk.updateStatus );
		},

		showHideClassByCheckBox: function(chk , cLass , reverse_behaviour){ // ASTGUI.domActions.showHideClassByCheckBox(chk, cLass) ;
			if ( typeof chk == 'string'){ chk = _$(chk) ; }
			chk.updateStatus = function(){
				if(reverse_behaviour ){
					if(chk.checked){
						$('.'+cLass).hide();
					}else{
						$('.'+cLass).show();
					}
				}else{
					if(chk.checked){
						$('.'+cLass).show();
					}else{
						$('.'+cLass).hide();
					}
				}
			};
			ASTGUI.events.add( chk, 'click' , chk.updateStatus );
		},

		enableDisableByCheckBox: function(chk , el, reverse_behaviour) { // ASTGUI.domActions.enableDisableByCheckBox (chk, el) ;
			// this function can also use for radio boxes
			if ( typeof chk == 'string'){ chk = _$(chk) ; }
			if ( typeof el == 'string'){ el = _$(el) ; }
			if( ASTGUI.isArray(el) ){
				chk.updateStatus = function(){
					el.each( function(el_this){
						if ( typeof el_this == 'string'){ el_this = _$(el_this) ; }
						if(!reverse_behaviour){
							el_this.disabled = !(chk.checked);
						}else{
							el_this.disabled = chk.checked;
						}
					});
					
				};
			}else{
				if(!reverse_behaviour){
					chk.updateStatus = function(){ el.disabled = !(chk.checked) } ;
				}else{
					chk.updateStatus = function(){ el.disabled = chk.checked; } ;
				}
			}

			ASTGUI.events.add( chk, 'click' , chk.updateStatus );
		}
	},

	events: {
		getTarget: function(x){
			x = x || window.event;
			return x.target || x.srcElement;
		},
		add: function(a,b,c){ // a is element , b is event (string) , c is the function 
			if ( typeof a == 'string'){ a = _$(a) ; }
			if($.browser.msie){
				a.attachEvent('on'+b, c);
				return;
			}
			if(a.addEventListener){
				a.addEventListener(b, c, false);
				return;
			}
			a['on'+b] = c ;
		},
		remove: function(a,b,c){
			if ( typeof a == 'string'){ a = _$(a) ; }
			if($.browser.msie){
				a.detachEvent('on'+b, c);
				return;
			}
			if(a.removeEventListener){
				a.removeEventListener(b, c, false);
				return;
			}
			a['on'+b] = null ;
		}
	},

	feedback : function( fb ){
		// usage  ::  ASTGUI.feedback( { msg:'your message here', showfor:2, color:'#000000', bgcolor:'#FFFFFF' } );
		top.miscFunctions.setFeedback(fb);
	},

	getFieldValue : function(el){ // ASTGUI.getFieldValue(el)
		if( !el ){ return ''; }
		if ( typeof el == 'string'){ el = _$(el) ; }
		switch(el.type){
			case 'checkbox':
				return (el.checked) ? 'yes':'no' ;
				break;
			case 'radio':
				return (el.checked) ? el.value : '' ;
				break;
			case 'select-one':
				return el.value ; //.trim()
				break;
			case 'select-multiple':
				var res = [];
				for (var i = 0 ; i < el.options.length; i++){
					if (el.options[i].selected){
						res.push(el.options[i].value)
					}
				}
				return res;
				break;
			case 'text':
			case 'textarea':
			case 'password':
			default:
				return el.value.trim() ;
				break;
		}
		return '';
	},

	getObjectPropertiesAsString : function(a){ // takes an object and returns all its properties, values as a string
		var ar = [];
		for(var d in a){
			if(!a.hasOwnProperty(d)){ continue; }

			if( typeof a[d] == 'object' ){
				if( ASTGUI.isArray(a[d]) ){
					ar.push(  d + ' : [' + a[d].join(',') + ']' );
				}else{
					ar.push(  d + ' : ' + ASTGUI.getObjectPropertiesAsString(a[d]) );
				}
			}else{
				ar.push(d + ' : '+ a[d]);
			}
		}
		return '{' + ar.join(' ,') + '}' ;
	},

	getTrunkStatus : function (registry , trunkname, ttype){ // ASTGUI.getTrunkStatus (registry, trunkname);
		/* registry should be == {
			iax2 : ASTGUI.parseCLIResponse( ASTGUI.cliCommand('iax2 show registry') ) ,
			sip : ASTGUI.parseCLIResponse( ASTGUI.cliCommand('sip show registry') )
		} ;
		*/
		// trunkname == trunkname as trunk_x 
		try{
		var this_IP = '';
		if(!ttype || ( ttype != 'sip' && ttype != 'iax' && ttype != 'providers' ) ){ return '--'; }
		if( ttype == 'providers' ){
			var uname = parent.sessionData.pbxinfo.trunks[ttype][trunkname]['username'];
			var host = parent.sessionData.pbxinfo.trunks[ttype][trunkname]['host'];
			ttype = parent.pbx.trunks.getProviderType(trunkname); // find 'sip' or 'iax'
			if ( ttype != 'sip' && ttype != 'iax'){
				return '--';
			}
		}else{
			var uname = parent.sessionData.pbxinfo.trunks[ttype][trunkname]['username'];
			var host = parent.sessionData.pbxinfo.trunks[ttype][trunkname]['host'];
		}
		if( !uname) {return '';}

		(function(){
			if(ttype=='iax'){var a ='iax2';}
			if(ttype=='sip'){var a ='sip';}
			var t = ASTGUI.cliCommand(a + ' show peer ' + trunkname) ;
			t = ASTGUI.parseCLIResponse( t );
			var IP = '';
			var s = t.split('\n');
			for(var i=0; i < s.length; i++ ){
				var line = s[i];
				if(line.contains('Addr->IP') && !line.contains('(Unspecified)')){ // if line is  'Addr->IP     : 68.62.219.197 Port 5060'
					var tmp = line.afterChar(':'); // tmp = '68.62.219.197 Port 5060' ;
					var tmp2 = tmp.split(' Port ');
					IP = tmp2.join(':');
					IP = IP.trim();
					this_IP = IP;
					return;
				} else if (line.contains('Defaddr->IP')) {
					var loc = line.afterChar(':').split(' Port ');
					IP = loc.join(':');
					IP = IP.trim();
					this_IP = IP;
				}
			}
		})();

		if(ttype=='iax'){
			var lines = registry.iax2.split('\n');
			uname = uname.substring(0,10);
		}else if(ttype=='sip'){
			var lines = registry.sip.split('\n');
			uname = uname.substring(0,12);
		}

		//var uname_lc = uname.toLowerCase();
		// TODO: come up with a better alternative than this
		// cli output of 'sip show registry' shows only a part of long usernames
		// We should use action: status like we do for active channel monitoring.
		
		var uname_lc = uname.toLowerCase();

		for(var i = 0; i < lines.length; i++) {
			var line_orig = lines[i];
			var line = lines[i].trim().toLowerCase();
			if (!line || line.beginsWith('host') ) { 
				continue; 
			}
			if((line.beginsWith(host) || (this_IP && line.beginsWith(this_IP + ' ')))  && line.contains(' ' + uname_lc + ' ')) {
				var vals = line_orig.split(/[ \t][ \t]*/); /* Host, Username, Refresh, State, Reg.Time */
				var sip_index = ASTGUI.version.gteq("1.6.1") ? 4 : 3;
				var state = (ttype === 'sip') ? vals[sip_index] : vals[5];
				switch(state) {
				case 'registered':
				case 'Registered':
					return '<font color="green">'+state+'</font>';
				default:
					return '<font color="red">'+state+'</font>';
				}
			}
		}
		return '<font color=red>Unrecognized Trunk</font>';
		}catch(err){
			top.log.error(err);
		}
	},

	parseGETparam : function(url_string, getparam ){ // ASTGUI.parseGETparam( url , 'EXTENSION_EDIT');
		// parse a URL string and return the value of a GET parameter
		// url can be in '....html?EXTENSION_EDIT=XXXX&YYYYYYY...' or '....html?EXTENSION_EDIT=XXXX'
		var t = getparam + '=' ;
		if( url_string.contains(t) ){
			var g = url_string.afterStr(t);
			return g.contains('&') ? g.beforeChar('&') : g ;
		}
		return '';
	},

	hideDrag : function(event){
		// Use this as a Cancel Button event, DONOT use this for hiding the div after save()/update(),  use the '$().hideWithBg()' instead
		var et = ASTGUI.events.getTarget(event) ;
		while( et.nodeName.toUpperCase() != 'DIV' ){ et = et.parentNode ; }
		et.style.display = 'none';
		ASTGUI.feedback( { msg:'Changes cancelled !', showfor:2,  color:'#32633d' } );
		try{
			et.ownerDocument.getElementById('bg_transparent').style.display ='none';
		}catch(err){}
	},

	isNull : function(a){
		// ASTGUI.isNull(a) ;
		return a===null
	},

	isArray: function(a){
		// ASTGUI.isArray(a) ;
		return a instanceof Array || ( a!= null && typeof a=="object" && typeof a.push == "function" && typeof a.concat == "function" )
	},

	loadHTML: function(u , cb){
		// ASTGUI.loadHTML(url)
		// loads URL 'url' in synchronus mode. note that 'url' is restricted by same origin policy
		var r =  Math.round(10000*Math.random());

		if( cb && typeof cb == 'function' ) {
			$.ajax({ url: u + '?r=' + r , async: true, success: function(msg){
				cb(msg);
			}});
		}else{
			var s = $.ajax({ url: u + '?r=' + r , async: false });
			return s.responseText;
		}
	},

	listSystemFiles : function( dir , cb ){
		// ASTGUI.listSystemFiles( dir , callBackFunction )
		// list of files in 'dir' will be sent to callBackFunction as an array
		if ( typeof dir != 'string') cb([]) ;
		try{
		this.systemCmd( top.sessionData.directories.script_ListFiles + ' ' +  dir , function(){
			ASTGUI.loadHTML( top.sessionData.directories.output_SysInfo , function(op){
				var tmp_files = op.split('\n');
				var files = [];
				for( var i =0 ; i < tmp_files.length ; i++){
					if( typeof tmp_files[i] == "undefined" ){ continue; }
					tmp_files[i] = tmp_files[i].trim();
					if( tmp_files[i] == "" ){ continue; }
					files.push(tmp_files[i]);
				}
				cb(files);
			});
		});
		}catch(err){
			top.log.error(err.description);
			cb([]);
		}
	},

	version: {
		/* other is greater than sessionData.AsteriskVersion */
		gt: function (other) {
			var res = ASTGUI.version.greater_than_equal_to(other);
			return res != 'equal' && res;
		},

		/* other is less than sessionData.AsteriskVersion */
		lt: function (other) {
			return !ASTGUI.version.greater_than_equal_to(other);
		},

		/* other is less than or equal to sessionData.AsteriskVersion */
		lteq: function (other) {
			var res = ASTGUI.version.greater_than_equal_to(other);
			return res == 'equal' || !res;
		},

		/* other is greater than or equal to sessionData.AsteriskVersion */
		gteq: function (other) {
			var res = ASTGUI.version.greater_than_equal_to(other);
			return res == 'equal' || res;
		},

		/* other is equal to sessionData.AsteriskVersion */
		eq: function (other) {
			var res = ASTGUI.version.greater_than_equal_to(other);
			return res == 'equal';
		},

		/* This is the meat of the version comparision.  Return false if other is
		 * greater than sessionData.AsteriskVersion, true if other is less than 
		 * sessionData.AsteriskVersion, and equal if they are equal. */
		greater_than_equal_to: function (other) {
			if (!top.sessionData.AsteriskVersion) { return false; }
			if (top.sessionData.AsteriskBranch == 'trunk') { return true; }
			if (top.sessionData.VersionCache[other] != undefined
					&& (top.sessionData.VersionCache[other] == true
					|| top.sessionData.VersionCache[other] == false
					|| top.sessionData.VersionCache[other] == "equal")) {
				return top.sessionData.VersionCache[other];
			}
			var thisversion = top.sessionData.AsteriskVersion || top.sessionData.AsteriskBranch;
			var thispieces = thisversion.split('.');
			var otherpieces = other.split('.');
			for (var i = 0; i < ((thispieces.length < otherpieces.length) ? thispieces.length : otherpieces.length); i++) {
				if (thispieces[i] > otherpieces[i]) {
					top.sessionData.VersionCache[other] = true;
					return true;
				} else if (thispieces[i] < otherpieces[i]) {
					top.sessionData.VersionCache[other] = false;
					return false;
				}
			}
			while (thispieces[i] || otherpieces[i] || i < 10) {
					if (thispieces[i] == otherpieces[i]) {
						top.sessionData.VersionCache[other] = "equal";
						return "equal";
					} else if (thispieces[i] == undefined) {
						top.sessionData.VersionCache[other] = false;
						return false;
					} else if (otherpieces[i] == undefined) {
						top.sessionData.VersionCache[other] = true;
						return true;
					} else if (thispieces[i] > otherpieces[i]) {
						top.sessionData.VersionCache[other] = true;
						return true;
					}
				i++;
			}
			top.sessionData.VersionCache[other] = "equal";
			return "equal";
		}
	},

	miscFunctions: {
		getChunksFromManagerOutput : function( op , usf){
			// ASTGUI.miscFunctions.getChunksFromManagerOutput( output_str ) ;
			if ( typeof op != 'string') return [] ;
			try{
			var tr_Array = [];
			var tmp_chunk = (usf) ? {} : [] ;
			var count = 0;
			var tmp_lines = op.split('\n');
			if( tmp_lines[0].contains('Response: ') ) tmp_lines.removeFirst();
			if( tmp_lines[0].contains('Message: ') ) tmp_lines.removeFirst();

			for( var r = 0; r < tmp_lines.length ; r++ ){
				var this_line = tmp_lines[r];
				if( this_line.trim() == '' ){
					if( !count ){ continue; }
					tr_Array.push(tmp_chunk);
					tmp_chunk = (usf) ? {} : [] ;
					count = 0;
					continue;
				}

				if( !this_line.contains(': ') ){ continue;  }

				if( usf ){
					tmp_chunk[ this_line.beforeStr(': ').trim() ] = this_line.afterStr(': ').trim() ;
				}else{
					tmp_chunk.push(this_line) ;
				}
				count++ ;
			}

			return tr_Array ;
			}catch(err){
				top.log.error(err.description);
				return [] ;
			}
		},

		createConfig : function( fileName, callback){ // ASTGUI.miscFunctions.createConfig( 'filaName', function(){ } ) ;
			if (typeof fileName != 'string') callback();
			if (ASTGUI.version.gteq("1.6.0")) {
				var s = $.ajax({ url: ASTGUI.paths.rawman+'?action=createconfig&filename='+ fileName , async: false }).responseText;
				callback();
			} else {
				ASTGUI.systemCmd('touch ' + top.sessionData.directories.asteriskConfig + fileName, callback);
			}
		},

		ArrayContains : function(arr, str){ // ASTGUI.miscFunctions.ArrayContains(arr, str )
			// There is already an Array.prototype function for this
			// but IE is going crazy when using the prototype method on a parent window array
			for(var i=0, j = arr.length ; i < j; i++ ){
				if( arr[i] === str ) return true;
			}
			return false;
		},
			
		moveUpDown_In_context: function(context, line , updown , cbf ){
			// ASTGUI.miscFunctions.moveUpDown_In_context( ct , line , bool , cbf ) ;  bool = true for Up, false for Down (default)
			// Use this function when you want to move a line Up/Down with in a context
			// Example:
			//
			// [somcontext]
			// include=context2
			// include=context1
			//
			// use:
			// 	ASTGUI.miscFunctions.moveUpDown_In_context( 'somcontext' , 'include=context2' , false , someCallBackFunction ) ;
			// to move the 'include=context2' below the 'include=context1'

			try{
			updown = Number(updown);
			var t = context2json({ filename:'extensions.conf' , context : context , usf: 0 });
	
			if( !t.contains(line) ){ // Could not find the rule in this context.
				cbf(t);
			}
	
			for( var ti = 0 ; ti < t.length ; ti++ ){
				if( t[ti] == line ){
					var tmp_a = (updown) ? t[ ti - 1 ] : t[ ti + 1 ] ;
					t[ti] = tmp_a ;
					if( updown ){ // move up
						t[ti-1] = line ;
					}else{ // move down
						t[ti+1] = line ;
					}
					break;
				}
			}

			ASTGUI.miscFunctions.empty_context({ filename:'extensions.conf', context : context, cb : function(){
				var x = new listOfActions('extensions.conf');
				t.each(function( this_line ){
					x.new_action('append', context , this_line.beforeChar('=') ,  this_line.afterChar('=') );
				});
				x.callActions( function(){
					cbf(t);
				});
			}});
			}catch(err){
				top.log.error(err.description);
			}
		},

		empty_context: function( ct ){ // ASTGUI.miscFunctions.empty_context({ filename:'somefile.conf', context : 'context_x', cb : fn })
			try{
			//if (ASTGUI.version.gteq("1.6.0")) {
			//	var u = new listOfSynActions(ct.filename);
			//	u.new_action('emptycat', ct.context , '', '' ) ;
			//	u.callActions();
			//	ct.cb();
			//	return;
			//}else{
				var sel_cxt = context2json({ filename: ct.filename , context : ct.context , usf:0 });
				var x = new listOfActions(ct.filename);
				sel_cxt.each(function(line){
					var var_name = line.beforeChar('=');
					var var_value = line.afterChar('=');
					x.new_action('delete', ct.context , var_name, '', var_value);
				});
				x.callActions(ct.cb);
			//}
			} catch(err) {
				if (err.contains("is null")) { /* Context was already empty */
					ct.cb();
				} else {
					top.log.error(err.message);
				}
			}
		},

		delete_LinesLike: function( ct ){
			// ASTGUI.miscFunctions.delete_LinesLike({ context_name : 'voicemailgroups' , beginsWithArr: ['exten=6050,'] , filename: 'extensions.conf', hasThisString:'somestring', cb:function(){} });
			// deletes all line in the context 'voicemailgroups' that beings with 'exten=6050,' and contains the string hasThisString (optional)

			try{
			var sel_cxt = context2json({ filename: ct.filename, context : ct.context_name , usf:0 });
			if ( typeof ct.beginsWithArr == 'string' ){
				ct.beginsWithArr = [ct.beginsWithArr];
			}

			var lines_to_delete = [];
				sel_cxt.each( function( line ){
					ct.beginsWithArr.each( function( this_beginsWithStr ){
						if( ct.hasThisString ){
							if( line.beginsWith( this_beginsWithStr ) && line.contains( ct.hasThisString) ){
								lines_to_delete.push( line );
							}
						}else{
							if( line.beginsWith( this_beginsWithStr ) ){
								lines_to_delete.push( line );
							}
						}
					});
				});

			var x = new listOfActions(ct.filename);
			lines_to_delete.each( function(line){
				var var_name = line.beforeChar('=');
				var var_value = line.afterChar('=');
				x.new_action('delete', ct.context_name , var_name, '', var_value);
			});

			x.callActions(ct.cb);
			}catch(err){
				top.log.error(err.description);
				ct.cb();
			}
		},

		chanStringToArray: function(chs){ // ASTGUI.miscFunctions.chanStringToArray();
			// expects chs as '5' or '5-8' or '5,6,7,8' and returns [5] or [5,6,7,8]
			try{
			if ( !chs ) return [];
			if ( typeof chs != 'string') chs = String(chs) ;
			chs = chs.trim();
			var tr = [];

			if( chs.contains(',') ){
				var s = chs.split(',');
				tr = tr.concat(s);
				tr.forEach( function(u){ return String(u); } );
				return tr;
			}

			if( chs.contains('-') ){
				var a = Number( chs.beforeChar('-') );
				var b = Number( chs.afterChar('-') );
				for( var i=a; i <= b ; i++ ){
					tr.push( String(i) ) ;
				}
				return tr;
			}

			return [chs];
			}catch(err){
				top.log.warn( err.description );
				return [chs];
			}
		},

		alertIfRangeisNotdefined: function(a , b, for_what){ // ASTGUI.miscFunctions.alertIfRangeisNotdefined();
			if(parent.sessionData.GUI_PREFERENCES.getProperty('disable_extension_ranges') == 'yes'){ return true; }
			if(!a || !b){return true;}
			if( !parent.sessionData.GUI_PREFERENCES.getProperty(a) || !parent.sessionData.GUI_PREFERENCES.getProperty(b) ){
				ASTGUI.dialog.alertmsg("You do not have an extension range defined for '"+ for_what +"'. Please define your <i>Extension Preferences</I> from the 'Options' panel");
				return false;
			}
			return true;
		},

		isExtensionInRange: function(ext,a,b){ // ASTGUI.miscFunctions.isExtensionInRange('6000','ue_start','ue_end') ;
			if(parent.sessionData.GUI_PREFERENCES.getProperty('disable_extension_ranges') == 'yes'){ return true; }
			var v = parent.sessionData.GUI_PREFERENCES.getProperty(a) ;
			var w = parent.sessionData.GUI_PREFERENCES.getProperty(b) ;
			if( !v || !w ){
				return true;
			}
			return ext.isValueInBetween(v, w);
		},

		/* ampmTime should be in "HH:MM [AM|PM]" */
		AMPM_to_asteriskTime: function(ampmTime){
			var pattern = /^\s*(\d{1,2})(:(\d{2}))?\s*(\s?(AM|am|PM|pm))?\s*$/;

			var match = ampmTime.match(pattern);
			if (match == null) {
				top.log.debug('ampmTime is not in a valid format.');
				return '';
			}

			hour = parseInt(match[1],10);
			minute = parseInt(match[3], 10) || 0;
			//match[3] is the whitespace plus match[4]
			ampm = match[5] || null;

			if (!ampm && (hour < 0 || hour > 23)) {
				top.log.debug('ampmTime must have its hour inbetween 0 and 23.');
				return '';
			} else if (ampm && (hour < 0 || hour > 12)) {
				top.log.debug('ampmTime must have its hour inbetween 0 and 12 if "AM|PM" exists');
				return '';
			} else if (ampm && ampm.match('[pP][mM]')) {
				hour+=12;
			} else if (ampm && ampm.match('[aA][mM]') && hour === 12) {
				hour = 0;
			}
			hour = (hour < 10) ? hour.addZero() : hour;

			if (minute < 0 || minute > 59) {
				top.log.debug('ampmTime must have its minute inbetween 0 and 59');
				return '';
			}
			minute = minute < 10 ? minute.addZero() : minute;

			return hour + ':' + minute;
		},
		
		asteriskTime_to_AMPM: function(atime){ // takes atime as '00:15' or '23:45' and returns '12:15 AM' or '11:45PM'
			//ASTGUI.miscFunctions.asteriskTime_to_AMPM('23:15');
			var hour = atime.split(':')[0];
			var mins = atime.split(':')[1];
			if( hour == '00' ){
				return '12:' + mins + ' AM';
			}
			if( Number(hour) < '12' ){
				return hour + ':' + mins + ' AM';
			}
			if( hour == '12' ){
				return hour + ':' + mins + ' PM';
			}
			if( Number(hour) > 12 ){
				return (Number(hour)-12).addZero() + ':' + mins + ' PM';
			}
			return '';
		},

		GotoIftime_in_humanReadable: function( gotoiftime_str ){ 
			//ASTGUI.miscFunctions.GotoIftime_in_humanReadable( '08:00-17:00,mon-fri,*,*' ) ; // returns a human readable form as ' 8 AM to 5 PM on Monday through Friday'
			var WEEKDAYS = {mon: 'Monday', tue: 'Tuesdays', wed: 'Wednesday', thu: 'Thursday', fri: 'Friday', sat:'Saturday', sun:'Sunday'};

			var PIECES = gotoiftime_str.contains(',') ? gotoiftime_str.split(',') : gotoiftime_str.split('|') ;
			var toreturn = [];
			if( PIECES[0] != '*' ){
				toreturn.push( ASTGUI.miscFunctions.asteriskTime_to_AMPM(PIECES[0].split('-')[0]) + ' to ' +  ASTGUI.miscFunctions.asteriskTime_to_AMPM(PIECES[0].split('-')[1]) );
			}

			if( PIECES[2] == '*' && PIECES[3] == '*' ){ // by week days
				if( PIECES[1].contains('-') ){ // range of week days
					toreturn.push( 'on ' + WEEKDAYS[PIECES[1].split('-')[0]] + ' through ' + WEEKDAYS[PIECES[1].split('-')[1]]  );
				}else{
					toreturn.push( 'on ' + WEEKDAYS[PIECES[1]] + 's' );
				}

				return toreturn.join(' ');
			}else{ // by days of month
				if( PIECES[2].contains('-') ){ // range of dates
					if( PIECES[3] != '*' ){
						toreturn.push( 'on ' + PIECES[3] + ' ' + PIECES[2].split('-')[0] + ' through ' + PIECES[3] + ' ' + PIECES[2].split('-')[1] );
					}else{
						toreturn.push( 'on ' + PIECES[2].split('-')[0] + ' through ' + PIECES[2].split('-')[1] + ' of any Month'  );
					}
				}else{
					toreturn.push( 'on ' + PIECES[3] + ' ' + PIECES[2].split('-')[0] );
				}
				return toreturn.join(' ');
			}
		}
	},

	parseCLIResponse : function (op){ // op is CLI command output via http 
		if (typeof op != 'string') return op;
		op = op.replace(/Response: Follows/, "");
		op = op.replace(/Privilege: Command/, "");
		op = op.replace(/--END COMMAND--/, "");
		return op;
	},

	parseContextLine: {
		read: function(q){ // ASTGUI.parseContextLine.read();
			// expects q as 'blah=foo' and returns ['blah','foo']
			if (typeof q != 'string') return [];
			var v = q.indexOf("=");
			if( v == -1){ return []; }
			return  [q.substring(0,v), q.substr(v+1)];
		},

		getExten: function(q){ // ASTGUI.parseContextLine.getExten();
			// use this when you want to get the pattern from a calling rule
			// expects q as 'exten=_X.,1,foo' or '_X.,1,foo'
			//	and returns _X.
			if (typeof q != 'string') {
				return '';
			}

			if( q.match('exten=') ){
				return q.split('exten=')[1].split(',')[0];
			} else {
				return q.split(',')[0];
			}
		},

		getPriority: function(q){ // ASTGUI.parseContextLine.getPriority();
			// expects  q  as 'exten=s,2,foo' OR 's,2,foo'   and   returns 2
			if (typeof q != 'string') return '';
			if( q.match('exten=') ){
				return q.split('exten=')[1].split(',')[1].trim();
			}else{
				return q.split(',')[1].trim();
			}
		},

		getAppWithArgs: function(q){ // ASTGUI.parseContextLine.getAppWithArgs();
			// expects  q  as 'exten=s,2,foo(ssssssss,ssdsd,assd)' OR 's,2,foo(ssssssss,ssdsd,assd)' OR even 'foo(ssssssss,ssdsd,assd)'
			//	and   returns 'foo(ssssssss,ssdsd,assd)' ;  app with arguments(if any)
			if (typeof q != 'string') return '';
			if( !q.contains('(') ){ // if q == something like 's,n,Hangup' return 'Hangup'
				var l = q.lastIndexOf(',');
				return q.substring(l+1);
			}else{
				var tmp = q.beforeChar('(') ;
				if( !tmp.contains(',') ){
					return q;
				}else{
					var l = tmp.lastIndexOf(',');
					return q.substring(l+1);
				}
			}
			top.log.error( "ASTGUI.parseContextLine.getAppWithArgs() could not parse \"" + q + "\" " );
			return '';
		},

		getApp: function(q){ // ASTGUI.parseContextLine.getApp();
			// expects  q  as 'exten=s,2,foo(ssssssss,ssdsd,assd)' OR 's,2,foo(ssssssss,ssdsd,assd)'   
			//	and   returns 'foo' ;// appname -- without arguments
			if (typeof q != 'string') return '';
			var y = ASTGUI.parseContextLine.getAppWithArgs(q);
			return ( y.contains('(') ) ? y.split('(')[0].trim() : y.trim() ;
		},

		getArgs: function(q){ // ASTGUI.parseContextLine.getArgs();
			// expects  q  as 'exten=s,2,foo(ssssssss,ssdsd,assd)' OR 's,2,foo(ssssssss,ssdsd,assd)' OR 's,2,foo(ssssssss,ssdsd,assd)'
			// OR 's,1,Answer' OR 's,n,Hangup'
			// and   returns [ssssssss,ssdsd,assd] or [] // arguments as an array 
			if (typeof q != 'string' || !q ) return [];
			q = q.trim();
			if ( !q.endsWith(')') || !q.contains('(') ){
				top.log.error( "ASTGUI.parseContextLine.getArgs() - No Argument found for \"" + q + "\" " );
				return [];
			}
			var y = ASTGUI.parseContextLine.getAppWithArgs(q);
			y = y.substr(0, (y.length-1));
			var x = y.afterChar('(');
			return this.getArgsArrayFromArgsString(x);
		},

		getArgsArrayFromArgsString: function(x){ // expects x as 'context,exten,pri' or 'context,exten,pri'
			if (typeof x != 'string') return [];

			var nested_parse = function (str, sep) {
				var buffer = '';
				var stack = new Array();
				var depth = 0;
				var len = str.length;
				for(var i=0; i<len; i++) {
					var next = str.charAt(i);
					switch(next) {
					case '(':
						depth++;
						break;
					case sep.toString():
						if (!depth) {
							if (buffer != '') {
								stack.push(buffer);
								buffer = '';
							}

							continue;
						}

						break;
					/*case ' ':
						if (!depth) {
							continue;
						}
						break;*/
					case ')':
						if (depth) {
							depth--;
						} else {
							stack.push("" + buffer + next);
							buffer = '';
							continue;
						}
						break;
					}
					buffer += next;
				}

				if (buffer != '') {
					stack.push(buffer);
				}

				return stack;
			};

			if(x.contains(',') ){
				return nested_parse(x,',');
			}
			if(x.contains('|') ){
				return nested_parse(x,'|');
			}
			return [x] ;
		},

		getArgsArrayFrom_AppWithArgs_String: function(x){ // expects x as 'goto(context,exten,pri)' or 'goto(context,exten,pri)'
			// usage : ASTGUI.parseContextLine.getArgsArrayFrom_AppWithArgs_String(x)
			if (typeof x != 'string') return [];
			if( !x.contains('(') ) {
				return this.getArgsArrayFromArgsString(y);
			}
			var y = x.substr(0, (x.length-1));
			    y = y.afterChar('(');
			return this.getArgsArrayFromArgsString(y);
		},

		toKnownContext: function(args){  // usage ASTGUI.parseContextLine.toKnownContext(y)
			// converts args to a readable format - ex: default,6000,1 to 'user 6000'
			if(!args.length){ return ''; }
			try{
			if(typeof args == 'string'){
				args = this.getArgsArrayFromArgsString(args);
			}
			if( args[0] == 'default' ){
				var u = args[1] ;
				if( u == 'o'){
					return 'Goto Operator';
				}
				if( parent.sessionData.pbxinfo.users.hasOwnProperty(u) ){
					return 'Goto User ' + u ;
				}
			};
			if( args[0] == 'pagegroups' ){
				return 'Goto Page Group ' + args[1] ;
			};
			if( args[0] == ASTGUI.contexts.QUEUES ){
				return 'Goto Queue ' + args[1] ;
			};
			if( args[0] == ASTGUI.contexts.CONFERENCES ){
				return 'Goto Conference ' + args[1] ;
			};
			if( args[0].contains(ASTGUI.contexts.RingGroupPrefix) ) {
				if( parent.sessionData.pbxinfo['ringgroups'].hasOwnProperty(args[0]) ){
					var rgname = parent.sessionData.pbxinfo['ringgroups'][args[0]].NAME ;
				}else{
					var rgname = ' ' + args[0] +  ' <font color=red><B>?</B></font>' ;
				}
				return 'Goto RingGroup ' + rgname ;
			};
			if( args[0].contains(ASTGUI.contexts.TimeBasedRulePrefix) ){
				var tbr_label = parent.sessionData.pbxinfo.timebasedRules[args[0]].label || args[0] ;
				return 'Goto TimeBased Rule -- ' + tbr_label  ;
			};
			if( args[0].contains(ASTGUI.contexts.VoiceMenuPrefix) ){
				var vm_name = parent.sessionData.pbxinfo.voicemenus[args[0]].comment || args[0] ;
				return 'Goto VoiceMenu -- ' + vm_name ;
			};
			if( args[0] == ASTGUI.contexts.VoiceMailGroups ){
				var t = parent.sessionData.pbxinfo.vmgroups;
				return 'Goto VoiceMail Group -- ' + (( t[args[1]] && t[args[1]]['label']) || args[1] ) ;
			}
			return 'Goto ' + args.join() ;
			}catch(err){
				top.log.error(err.description);
				return 'Goto ' + args.join() ;
			}
		},

		showAs : function(q){ // ASTGUI.parseContextLine.showAs(line)
			// expects q as 'extension,priority,Goto(context,extension,1)' or 'Goto(context,extension,1)' or 'Goto(context,extension,1)'
			// or 'extensions,priority,Hangup'
			// returns - "User extension" or 'VoiceMenu extension' or 'Hangup' or 'Busy' or some string - depends on q.
			// use this when you want to represent the action to the user
			if (typeof q != 'string') {
				top.log.error('ASTGUI.parseContextLine.showAs: expecting q as string');
				return '??';
			}
			var app = ASTGUI.parseContextLine.getApp(q) ;
			var args = ASTGUI.parseContextLine.getArgs(q) ;
			var q_LC = q.toLowerCase();
			var all_LC = app.toLowerCase() ;

			if( all_LC == "macro" ){
				if( args[0] && args[0].contains('trunkdial-failover') && args[1] ){
					var tmp_trunkDetails = ASTGUI.parseContextLine.parseTrunkDialArgument(args[1]);
					var tmp_trunkString = (tmp_trunkDetails.name) ? ' using trunk ' + tmp_trunkDetails.name : ' via ' + tmp_trunkDetails.channel ;
					return 'Dial ' + tmp_trunkDetails.prepend + tmp_trunkString ;
				}
				return 'Run Macro ' + (args[0] || '???');
			}

			if( all_LC == "answer" ){
				return 'Answer the call'
			}

			if( all_LC == "authenticate" ){
				return 'Authenticate for password ' + ( args[0] || ' ??? ' );
			}

			if( all_LC == "disa" ){
				if ( args[0] && args[1] && args[0] == 'no-password' ){
					return 'DISA using context ' + args[1] + ' (no password)';
				}
				return 'DISA using password ' + args[0] + ' against context ' + args[1];
			}

			if( all_LC == "background" ){
				return 'Play ' + (args[0] || '???') + ' & Listen for KeyPress events' ;
			}

			if( all_LC == "playback" ){
				return 'Play ' + (args[0] || '???') + ' & Donot Listen for KeyPress events' ;
			}

			if( all_LC == "agi" ){
				return (args[0])? 'Execute AGI script <b>' + args[0] + '</b>' :  'Execute AGI script <font color=red>!</font>' ;
			}

			if( all_LC == "waitexten" ){
				return "Wait '"+ args[0] + "' sec for the user to enter an extension";
			}

			if( all_LC == "wait" ){
				return "Pause dialplan execution for '"+ args[0] + "' seconds";
			}

			if( all_LC == "hangup" ){
				return "Hangup call";
			}

			if( all_LC == "echo" ){
				return "do the ECHO test";
			}

			if( all_LC == "busy" ){
				return (args[0]) ? "Play BusyTone for '" + args[0] + "' seconds and Hangup" : 'Play Busy Tone indefinetely' ;
			}

			if( all_LC == "congestion" ){
				return (args[0]) ? "indicate Congestion for '" + args[0] + "' seconds and Hangup" : 'indicate congestion indefinetely' ;
			}

			if( all_LC == "directory" ){
				return 'Directory for ' + ( (args[0] && 'context ' +  args[0] ) || '???');
			}

			if( all_LC == "voicemailmain" ){
				return 'Check VoiceMail';
			}

			if( all_LC == "voicemail" ){
				return 'leave Voicemail for user ' + (args[0] || '???');
			}

			if( all_LC == "setmusiconhold" ){
				return "Set Music-On-Hold class '" + (args[0] || 'default') + "'" ;
			}

			if( all_LC == "meetme" ){
				return 'Goto Conference room ' + (args[0] || '???');
			}


			if( all_LC == "dial" ){
				return 'Dial ' + ( args[0] || '???' );
			}

			if( all_LC == "set" && args[0] && args[0].contains('timeout(digit)') ){
				return "Digit Timeout to " + args[0].afterStr('TIMEOUT(digit)=') + " seconds" ; //  exten => s,n,Set(TIMEOUT(digit)=5);
			}

			if( all_LC == "set" && args[0] && ( args[0].toLowerCase().contains('language()=') || args[0].toLowerCase().contains('channel(language)=') )  ){
				return "Set Language to " + args[0].afterStr(')=').trim() ; //
			}

			if( all_LC == "set" && args[0] && args[0].contains('timeout(response)') ){
				return "Response Timeout to " + args[0].afterStr('TIMEOUT(response)=') + " seconds" ; //  exten => s,n,Set(TIMEOUT(response)=10);
			}

			if( all_LC == "goto" ){
				return this.toKnownContext(args);
			}

			return q;
		},

		parseTrunkDialArgument: function(y){ // usage ASTGUI.parseContextLine.parseTrunkDialArgument(y)
			// expects y as  '${trunk_1}/XXX${EXTEN:X}' OR SIP/user/XXX${EXTEN:X}
			if( !y || typeof y != 'string') {
				top.log.error('ASTGUI.parseContextLine.parseTrunkDialArgument: expecting y as string');
				return { name : '', channel : '', prepend : '', stripx : '' };
			}
			var WhatToDial = '';
			y = y.trim();
			if( y.beginsWith('${') && y.afterChar('}').beginsWith('/') ) {
				var trunkname = y.betweenXY('{' , '}');
				var channel = '' // TODO - lookup in globals context on extensions.conf
				WhatToDial = y.afterChar('}').substr(1);
			} else {
				var u = y.split('/');
				if( u.length > 2 && !y.beginsWith('${') ){ // y is in 'SIP/user/PREFIX_1${EXTEN:1}'
					var trunkname = null ;
					var channel = u[0] + '/' + u[1] ;
					u.splice(0,2);
					WhatToDial = u.join('/'); // take the part after second '/'
				}
				if( u.length == 2){
					var trunkname = u[0] ;
					var channel = u[0] ;
					var WhatToDial = u[1] ;
				}
			}
			// we expect WhatToDial to be in '1${EXTEN:1}' or in '${EXTEN}' or in '${EXTEN:1}' or in '9${EXTEN}' format or a plain extensin string
			if( WhatToDial.contains('${') ){
				if(!WhatToDial.contains('${EXTEN')){ // if WhatToDial is in some other format that the gui does not understand
					// TODO : replace the above if condition with a regular expression to check for the acceptable formats
					// TODO : THROW ERROR
					return {name : trunkname, channel : channel, prepend : WhatToDial, stripx : '', filter: ''};
				}
				var prepend = WhatToDial.beforeChar('$') ;
				if (WhatToDial.contains('FILTER')) {
					var filterstring = WhatToDial.betweenXY('(', ')');
					filterstring = filterstring.split(',');

					var extenString = filterstring[1].betweenXY('{', '}');
					var filter = filterstring[0];
				} else {
					var extenString = WhatToDial.betweenXY('{','}') ;
					var filter = '';
				}
				var stripXdigitsfromfront = ( extenString.contains(':') ) ? extenString.afterChar(':') || '0' : '0' ;
			} else { // WhatToDial is a plain extension string such as '911' or 'pari'
				var prepend = WhatToDial ;
				var stripXdigitsfromfront = 'ALL' ;
				var filter = '';
			}
			return { name : trunkname, channel : channel, prepend : prepend, stripx : stripXdigitsfromfront, filter: filter};
		},

		obCallingRule: function(str){ // usage ASTGUI.parseContextLine.obCallingRule(str)
			/* expects str as 
				//    'exten = _91256XXXXXXX,1,Macro( trunkdial-failover-0.3, ${trunk_1}/${EXTEN:2}, ${trunk_2}/${EXTEN:2},trunk_1,trunk_2 )'
				// OR
				//  exten = 1900!,1,Hangup ; BLOCK 1900 calls
				and returns
					{ 
						actualString : 'exten = _91256XXXXXXX,1,Macro( trunkdial-failover-0.3, ${trunk_1}/${EXTEN:2}, ${trunk_2}/${EXTEN:2},trunk_1,trunk_2 )'
						pattern : '_91256XXXXXXX',
						macroname : trunkdial-failover-0.3,
		
						firstTrunk : 'trunk_1' ,
						firstPrepend : '' , // prependtext for trunk1
						stripdigits_firstTrunk : 2,
		
						secondTrunk : 'trunk_2',
						secondPrepend : '' , // prependtext for trunk2
						stripdigits_secondTrunk : 2
					}
			*/
			if (typeof str != 'string') {
				top.log.error('ASTGUI.parseContextLine.obCallingRule: expecting str as string');
				return null;
			}

			var cr = { };
			cr.actualString = str ;
			cr.pattern = ASTGUI.parseContextLine.getExten(str);
			cr.callerID = '';
			if( str.contains('Macro(') ){ 
				var macroargs = ASTGUI.parseContextLine.getArgs(str);
				if( str.contains('trunkdial') ){ // if is some version of trunk dial marco
					cr.macroname = macroargs[0] ;
					var t1 = ASTGUI.parseContextLine.parseTrunkDialArgument( macroargs[1] ) ;
						cr.firstTrunk = t1.name ;
						cr.firstPrepend = t1.prepend ;
						cr.stripdigits_firstTrunk = t1.stripx ;
						cr.firstFilter = t1.filter;
		
					// Why is the second part of this condition necessary?
					// if( macroargs.length <= 2  || ( macroargs.length > 2 && macroargs[2].trim() == '') ){ // if a failback trunk is not defined
					if( macroargs.length <= 3 ){ // if a failover trunk is not defined
						cr.secondTrunk = '' ;
						cr.secondPrepend = '' ;
						cr.stripdigits_secondTrunk = '' ;
						cr.secondFilter = '';
					}else{
						var t2 = ASTGUI.parseContextLine.parseTrunkDialArgument( macroargs[2] ) ;
						cr.secondTrunk = t2.name ;
						cr.secondPrepend = t2.prepend ;
						cr.stripdigits_secondTrunk = t2.stripx ;
						cr.secondFilter = t2.filter;
					}	

					if(macroargs.length == 4 || macroargs.length == 6){
							cr.callerID = macroargs[macroargs.length - 1];
					}
				}else if( str.contains('local-callingrule') ){ // if is some version of localcrcid macro
					cr.destination = "Goto(" + macroargs[1] + ',' + macroargs[2] + ',' + macroargs[3] + ')';
					cr.callerID = macroargs[4] ? macroargs[4] : '';
				}
			}else{
				cr.destination = ASTGUI.parseContextLine.getAppWithArgs( str ) ;
			}
			return cr;
		}
	},

	resetTheseFields : function( flds ){ // ASTGUI.resetTheseFields(arr) , flds = [ el1, el2 , el_3 ] ; - sets each element to blank value
		if( typeof flds == 'string' ){ flds = [flds]; }
		if(!ASTGUI.isArray(flds)){ return; }
		var reset_el = function(el){
			var tmp_dfalt = $(el).attr('dfalt');
			switch(el.type){
				case 'text':
					el.value = '';
					if( tmp_dfalt )
						el.value = tmp_dfalt;
					break ;
				case 'checkbox':
					el.checked = false;
					if( tmp_dfalt)
						ASTGUI.domActions.checkIf_isAstTrue( el , tmp_dfalt);
					break ;
				case 'radio':
					el.checked = false;
					break ;
				case 'select-one':
					el.selectedIndex = -1;
					if(tmp_dfalt) ASTGUI.selectbox.selectOption(el, tmp_dfalt);
					break ;
				case 'textarea':
					el.value = '';
					if( tmp_dfalt )
						el.value = tmp_dfalt;
					break ;
				default : break ;
			}
		};
		var el;
		top.log.debug("flds is " + flds);
		for (var i=0; i < flds.length ; i++ ) {
			el = flds[i] ;
			if ( typeof el == 'string'){ el = _$(el) ; }
			el.disabled = false;
			reset_el( el );
		}
	},

	selectbox: {
		selectDestinationOption: function(el, dest){ // ASTGUI.selectbox.selectDestinationOption(el,dest)
			// selects a given destination 'dest' in select box el
			// dest could be 'context,extention,priority' or 'context,etension,priority' or 'goto(context,etension,priority)' or 's,n,goto(context,etension,priority)'
			//console.log('We are looking for "' + dest + ' " in the selectbox');
			var args = [];
			if ( typeof el == 'string'){
				el = _$(el) ;
				if( !el ){
					top.log.error( 'No Element by that id ' );
					return;
				}
			}
			el.selectedIndex = -1;
			if(!dest){return;}

			if( dest.contains('(') && ( dest.contains(',') || dest.contains('|') ) ){
				ASTGUI.selectbox.selectOption(el , dest);
				return;
			}

			if( !dest.contains('(') && !dest.contains(',') && !dest.contains('|') ) {
				ASTGUI.selectbox.selectOption(el , dest);
				return;
			}

			if(dest.contains('(')){
				var tmp = dest.beforeChar('(');
				if( tmp.contains(',') ){
					args = ASTGUI.parseContextLine.getArgs(dest);
				}else{
					args = ASTGUI.parseContextLine.getArgsArrayFrom_AppWithArgs_String(dest);
				}
			}else{
				if(dest.contains(',') || dest.contains('|') ){
					args = ASTGUI.parseContextLine.getArgsArrayFromArgsString(dest);
				}else{
					args = [dest];
				}
			}

			var args_v = args.join(',') ;
			var args_c = args.join(',') ;
			var dest_args_v = 'Goto('+ args.join(',') + ')';
			var dest_args_c = 'Goto('+ args.join(',') + ')' ;

			for( var i=0, j = el.options.length ; i < j ; i++ ){
				if( (el.options[i].value == dest_args_v) || (el.options[i].value == dest_args_c) || (el.options[i].value == args_v) || (el.options[i].value == args_c )){
					// the select box option vale could be 'goto(context,exten,priority)' or 'Hangup' or just 'context,exten,priority'
					el.selectedIndex = i;
					return;
				}
			}
		},

		populateArray: function(el, arr){ // arr = [{optionText:'TEXT', optionValue:'VALUE'}, {..}] // ASTGUI.selectbox.populateArray(el,arr);
			if ( typeof el == 'string'){ el = _$(el) ; }
			ASTGUI.selectbox.clear(el);
			try{
				for(var f=0, arr_l = arr.length ; f < arr_l ; f++){
					if ( typeof arr[f] == 'string' || !arr[f].optionText ){
						ASTGUI.selectbox.append( el, arr[f], arr[f] );
					}else{
						ASTGUI.selectbox.append( el, arr[f].optionText, arr[f].optionValue );
					}
				}
			}catch(err){}
		},

		readAllValues: function(el){
			// get an array of all option values in a select box
			if ( typeof el == 'string'){ el = _$(el) ; }
			var y = [] ;
			for (var x=0; x < el.options.length; x++) {
				if(el.options[x].value.trim()){
					y.push(el.options[x].value) ;
				}
			}
			return y;
		},

		populateOptions: function( el, n){
			// Populate 'n' numbered options in a select box starting from 1
			if ( typeof el == 'string'){ el = _$(el) ; }
			n = Number(n);
			ASTGUI.selectbox.clear(el);
			var m = ASTGUI.selectbox.append;
			for ( var i=1 ; i <= n ; i++) { m(el, i, i); }
		},

		insert_before: function(el,txt, val, i){ // ASTGUI.selectbox.insert_before(el,txt, val, i);
			if ( typeof el == 'string'){ el = _$(el) ; }
			if($.browser.msie){
				el.add(new Option (txt,val), i );
			}else{
				el.add(new Option (txt,val), el.options[i] );
			}
		},

		insertOption_before: function(el,opt, i){
			if ( typeof el == 'string'){ el = _$(el) ; }
			if($.browser.msie){
				el.add(opt, i );
			}else{
				el.add(opt, el.options[i] );
			} 
		},

		append: function(el, txt, val){ // ASTGUI.selectbox.append(el,txt,val);
			if ( typeof el == 'string'){ el = _$(el) ; }
			el.options[el.options.length] = new Option (txt,val);
		},

		append_option: function(el,opt){
			// append new option to a select box element
			if ( typeof el == 'string'){ el = _$(el) ; }
			if($.browser.msie){
				el.add(opt);
			}else{
				el.add(opt,null);
			}
		},

		remove_i: function(el, i){
			// removes i'th Option from the select box
			if ( typeof el == 'string'){ el = _$(el) ; }
			el.options[i] = null;
		},
	
		clear: function(el){
			// remove all options from the select box
			if ( typeof el == 'string'){ el = _$(el) ; }
			el.options.length = 0;
		},

		selectOption: function(el, opt){ // ASTGUI.selectbox.selectOption(el,opt)
			if ( typeof el == 'string'){ el = _$(el) ; }
			el.selectedIndex = -1;
			for (var x=0; x < el.options.length; x++) {
				if (el.options[x].value == opt){
					el.selectedIndex = x;
				}
			}
		},

		selectOptionMultiple: function(el, opt){ // ASTGUI.selectbox.selectOption(el,opt)
			if ( typeof el == 'string'){ el = _$(el) ; }
			for (var x=0; x < el.options.length; x++) {
				if (el.options[x].value == opt){
					//el.selectedIndex = x;
					el.options[x].selected = true;
				}
			}
		},

		selectOption_Add : function(el, opt){ // adds opt as new options if it is not already found 
			if ( typeof el == 'string'){ el = _$(el) ; }
			el.selectedIndex = -1;
			for (var x=0; x < el.options.length; x++) {
				if (el.options[x].value == opt){
					el.selectedIndex = x;
				} 
			}
			if(el.selectedIndex == -1){
				ASTGUI.selectbox.append(el, opt, opt);
				ASTGUI.selectbox.selectOption(el, opt);
			}
		}
	}, // { selectbox }

	showbg: function(t){ 
		// ASTGUI.showbg(boolean)
		// show/hide a transparent background layer while Showing/Hiding Edit Dialogs (to prevent the user from interacting with other elements on the page)
		try{
		if(t){
			if( !document.getElementById('bg_transparent') ){ 
				var d = document.createElement( 'DIV' ) ;
				d.setAttribute('id','bg_transparent');
				document.body.appendChild(d) ;
			}else{
				var d = document.getElementById('bg_transparent') ;
				d.style.display = '';
			}
			return;
		}
		if( document.getElementById('bg_transparent') ){ 
			var d = document.getElementById('bg_transparent') ;
			d.style.display = 'none';
		}
		}catch(err){}
	},

	showToolTips : function(){
		if(window.jQuery && $.tooltip){
			$('img.tooltipinfo').tooltip({delay:0.5,showURL:false,fixPNG:true,showBody:" - ",extraClass:"pretty fancy",top:-35,left:10});
		}
	},

	sortContextByExten: function(cxt , getAll){ // ASTGUI.sortContextByExten( cxt , boolean); 
		// if boolean set to false or null or undefined --> consider only 's' & interger extensions
		// if boolean set to true --> get every pattern

		/* goal is to sort a context array as sorted by asterisk dialplan (almost)
			Ex: ['exten=s,2,App2','exten=s,1,App1','exten=1,1,App', 'exten=s,n,AppN'] 
				will be converted to 

			{
				s:['s,1,App1','s,2,App2','s,3,AppN'], //each extension will be sorted
				1:['1,1,App']
			}
			This function is help ful when you try to represent a set of context lines in order in the GUI Ex: in VoiceMenus
			WARNING: ******* this function attempts to convert 'n' priority to a number ****
		*/

		var TO_RETURN = {}, EXTENSIONS = {};
		cxt.each( function(line , cxt_index) {
			if( !line.beginsWith('exten=') ) return;
			var exten = ASTGUI.parseContextLine.getExten(line);
			if( !getAll && isNaN(exten) && exten !='s' ) return; // we only handle numbers and 's' at this time
			if( ! EXTENSIONS.hasOwnProperty(exten) ){
				EXTENSIONS[exten] = [] ;
			}
			EXTENSIONS[exten].push(line) ;
		} );
		
		// sorting to make sure we return in the right order
		for( var THIS_EXTEN in EXTENSIONS ){ if(EXTENSIONS.hasOwnProperty(THIS_EXTEN) ){
			TO_RETURN[THIS_EXTEN] = [];
			var TMP_CONTEXT = EXTENSIONS[THIS_EXTEN];
			TMP_CONTEXT.each( function(line, line_index){
				var priority = ASTGUI.parseContextLine.getPriority(line);
				if( priority == 'n' ){
					var new_Priority = Number(ASTGUI.parseContextLine.getPriority(TMP_CONTEXT[line_index-1])) + 1 ;
					if( TO_RETURN[THIS_EXTEN].indexOfLike( THIS_EXTEN + ','+ new_Priority + ',' ) == -1 ){
						var new_line = THIS_EXTEN + ',' + new_Priority + ',' + ASTGUI.parseContextLine.getAppWithArgs(line);
						TO_RETURN[THIS_EXTEN].push(new_line);
					}
					return;
				}else{
					priority = Number( priority ) ;
					for(var g=0; g < TO_RETURN[THIS_EXTEN].length ; g++ ){
						var tmp_priority = Number( ASTGUI.parseContextLine.getPriority( TO_RETURN[THIS_EXTEN][g] ) );
						if( priority < tmp_priority ){
							TO_RETURN[THIS_EXTEN].splice(g,0, line.afterChar('=') );
							return;
						}
					}
					TO_RETURN[THIS_EXTEN].push( line.afterChar('=') );
				}
				return true ;
			});
		}}
		return TO_RETURN;
	},

	startDrag : function(event, movethis ){
		if(!movethis){
			var et = ASTGUI.events.getTarget(event) ;
			while( et.nodeName.toUpperCase() != 'DIV' ){
				et = et.parentNode ;
			}
			var mt = et;
		}else{
			var mt = _$(movethis);
		}
		var MTSW = mt.style.width || $(mt).width();
		var MTSH = mt.style.height  || $(mt).height();
		var mt_pos = ASTGUI.domActions.findPos(mt);
		var tmp_div = document.createElement('DIV'); 
		tmp_div.style.position = 'absolute';
		tmp_div.style.left = mt_pos.cleft ;
		tmp_div.style.top = mt_pos.ctop ;
		tmp_div.style.width = MTSW ;
		tmp_div.style.height = MTSH ;
		$(tmp_div).css({ borderWidth:'2px', borderStyle:'dashed', borderColor:'red', zIndex: 10000 });
		document.body.appendChild(tmp_div);

		var timer;
		var dragdelay = (jQuery.browser.msie) ? 70 : 40;
		var initialcursorX, initialcursorY, initialwindowleft, initialwindowtop, maxleft, maxtop ;
		var stopDrag = function(){
			mt.style.left = tmp_div.style.left ;
			mt.style.top = tmp_div.style.top ;
			ASTGUI.events.remove( document , "mousemove" , movewindow ) ;
			ASTGUI.events.remove( document , "mouseup" , stopDrag ) ;
			clearInterval(timer);
			mt.style.MozOpacity = mt.style.opacity = 1.0;
			tmp_div.parentNode.removeChild(tmp_div);
		};

		mt.style.MozOpacity = mt.style.opacity = 0.85; // ondrag Opacity
		var movewindow = function(event){
			var x,y;
			if(typeof window.scrollX != "undefined"){
				x = event.clientX + window.scrollX;
				y = event.clientY + window.scrollY;
			}else{
				x =  window.event.clientX + document.documentElement.scrollLeft + document.body.scrollLeft;
				y = window.event.clientY + document.documentElement.scrollTop + document.body.scrollTop;
			}
			var tmp_top = initialwindowtop  + y - initialcursorY ; 
			var tmp_left = initialwindowleft + x - initialcursorX;
			if( tmp_left > 0 && tmp_left < maxleft ){ tmp_div.style.left = tmp_left; }
			if( tmp_top > 0 && tmp_top < maxtop ){ tmp_div.style.top  = tmp_top; }
			ASTGUI.events.remove( document , "mousemove" , movewindow ) ;
		};
	
		if(typeof window.scrollX != "undefined"){
			initialcursorX = event.clientX + window.scrollX;
			initialcursorY = event.clientY + window.scrollY;
		}else{
			initialcursorX =  window.event.clientX + document.documentElement.scrollLeft + document.body.scrollLeft;
			initialcursorY = window.event.clientY + document.documentElement.scrollTop + document.body.scrollTop;
		}
	
		initialwindowleft = mt_pos.cleft;
		initialwindowtop = mt_pos.ctop;
	
		if(typeof window.innerWidth != "undefined"){
			maxleft = window.innerWidth - parseInt( MTSW , 10) ;
			maxtop = window.innerHeight - parseInt( MTSH , 10) ;
		}else{
			maxleft = document.body.offsetWidth - parseInt(MTSW, 10) ;
			maxtop = document.body.offsetWidth- parseInt(MTSH, 10) ;
		}

		timer = setInterval( function(){ ASTGUI.events.add( document , "mousemove" , movewindow ) } , dragdelay ) ;
		ASTGUI.events.add( document , "mouseup" , stopDrag ) ;
		if(event.preventDefault){
			event.preventDefault();
		}else{
			event.returnValue = false;
		}
	},

	systemCmd: function(cmd, callbackfunction){
		// ASTGUI.systemCmd(cmd, cbf);
		//	Execute a Unix system command
		top.log.debug("Executing System Command : '" + cmd + "'");
		var delay_cb = function(){
			if (typeof callbackfunction !== 'function') {
				return;
			}

			if( parent.sessionData.PLATFORM.isAA50 ){
				setTimeout( callbackfunction , 500 );
			}else{
				callbackfunction();
			}
		};
		makeRequest({
			     action : 'originate' ,
			    channel : 'Local/executecommand@' + ASTGUI.contexts.guitools ,
			   Variable : 'command=' + cmd ,
			application : 'noop' ,
			    timeout : '60000' ,
			   callback : delay_cb
		});
	},

	systemCmdWithOutput: function( cmd , cb ) {
		// usage :: ASTGUI.systemCmdWithOutput( 'uptime' , callback(output){ /* do something with output */ } );
		// Use this function when you want to execute a specific system command and read the output
		// output will be sent as a argument to the callback function
		//var fcmd = cmd + ' > ' + top.sessionData.directories.guiInstall +  ;
		var tmp_opf = ( top.sessionData.directories.output_SysInfo.afterChar('/') || top.sessionData.directories.output_SysInfo ).rChop('.html') + Math.round( 10000 * Math.random() ) + '.html' ;
		var fcmd = cmd + ' > ' + top.sessionData.directories.guiInstall + tmp_opf ;

		var after = function(){
			parent.document.getElementById('ajaxstatus').style.display = 'none';
			ASTGUI.loadHTML( tmp_opf , function(cmd_output){
				ASTGUI.systemCmd( 'rm ' + top.sessionData.directories.guiInstall + tmp_opf, function(){cb(cmd_output);} );
			});
		};

		var delay_cb = function(){ setTimeout(after,500); };
		if( parent.sessionData.PLATFORM.isAA50 ){
			parent.document.getElementById('ajaxstatus').style.display = '';
			this.systemCmd( fcmd , delay_cb );
		}else{
			this.systemCmd( fcmd , after );
		}
	},

	tabbedOptions: function(el, arr){
		// usage ASTGUI.tabbedOptions ( _$('el') , [{url:'#', desc:'Option1', selected:true}, {url:'x.html', desc:'Option2'}]);
		if ( typeof el == 'string'){ el = _$(el) ; }
		var k = document.createElement('TABLE');
		var nr = k.insertRow(-1);
		var jq_K = $(k);
		jq_K.attr('align','center');
		jq_K.attr('cellpadding','0');
		jq_K.attr('cellspacing','0');
		jq_K.css( {margin:'10px', padding:'10px'});

		arr.each(function(item){
			var nc = nr.insertCell( nr.cells.length );
			if(item.click_function){
				var ih = '<a href=# class=tab onclick="">' + item.desc +'</a>';
				var ih = document.createElement('A');
				ih.href= '#';
				ih.className ='tab';
				ih.innerHTML = '<nobr>' + item.desc + '</nobr>';
				$(ih).click(function(){
					this.blur();
					$(el).find('A').removeClass('tabselected').addClass('tab');
					this.className = 'tabselected';
					item.click_function();
				});
				nc.appendChild(ih);
			}else{
				var ih = (item.selected && item.selected == true )  ? '<nobr><a href=# class=tabselected>' + item.desc +'</a></nobr>' : '<nobr><a href="' + item.url + '" class=tab>' + item.desc +'</a></nobr>';
				nc.innerHTML = ih;
			}
			nc.align = 'center';
			nc.valign = 'bottom';
			$(nc).css( {borderBottom:'4px solid #000000'});
		});
		
		el.appendChild(k);
	},

	toLocalTime: function (k){ // Converts the UTC time read on the appliance to the browsers local time
		// expects k as 'Fri Dec  8 23:59:59 UTC 2008'
		try{
			var tmp = k.split(" ");
			if(tmp[2] == ""){ tmp.splice(2,1) ; }
			// tmp[0] = 'Fri' , tmp[1] ='Dec', tmp[2] = '8', tmp[3] = '23:59:59', tmp[4]= 'always assume UTC', tmp[5] = '2008'
			var temp = tmp[3].split(':'); // hours =  parseInt(temp[0]), minutes = parseFloat(temp[1]);
			// convert these values to local time
			var months_strings = ["jan", "feb", "mar", "apr", "may", "jun", "jul", "aug","sep","oct","nov","dec"];
			var day_strings = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
			var month_no = months_strings.indexOf(tmp[1].toLowerCase());
	
			var utc_date = new Date();
			utc_date.setUTCFullYear ( tmp[5] , month_no , tmp[2] );
			utc_date.setUTCHours ( parseInt(temp[0]), parseInt(temp[1]) );
		
			var lt_hours = utc_date.getHours(); // 0 to 23
			var lt_minutes = utc_date.getMinutes(); // 0 to 59
			var lt_month = months_strings[utc_date.getMonth()]; // 0 to 11
			var lt_dom = utc_date.getDate(); // 1 to 31
			var lt_year = utc_date.getFullYear() ; // 2007
			var lt_day = day_strings[utc_date.getDay()] ; // 0 to 6
		
			return lt_day + " " + lt_month + " " + lt_dom + ", " + lt_hours + ":" + lt_minutes + " " + lt_year;
		}catch(err){
			return '';
		}
	},

	updateaValue: function(e){
		// ASTGUI.updateaValue({ file:'users.conf', context :'6000', variable :'hassip', value :'yes' })
		// Use this function when you have to update only one or two unique values in a config file
		var u = new listOfSynActions(e.file) ;
		u.new_action('update', e.context, e.variable , e.value );
		var g = u.callActions();
		return (g.contains('Response: Success')) ? true : false ;
	},

	updateFieldToValue : function( el, val ){ // updates a field el, to value 'val' - el could be a text box, select-option, or a checkbox
		// ASTGUI.updateFieldToValue(el,val)
		if ( typeof el == 'string'){ el = _$(el) ; }
		var tmp_dfalt = $(el).attr('dfalt');

		switch(el.type){
			// TODO come up with a way to handle radio buttons
			case 'text':
				el.value = val ;
				if( tmp_dfalt && !val ) el.value = tmp_dfalt;
				break ;
			case 'textarea':
				el.value = val ;
				if( tmp_dfalt && !val ) el.value = tmp_dfalt;
				break ;
			case 'checkbox':
				ASTGUI.domActions.checkIf_isAstTrue( el , val);
				if( tmp_dfalt && ( typeof val == 'undefined' || val == '' ) ){
					ASTGUI.domActions.checkIf_isAstTrue( el , tmp_dfalt);
				}
				break;
			case 'radio':
				top.log.error('You are on your own with Radio Buttons');
				break ;
			case 'select-one':
				ASTGUI.selectbox.selectOption(el, val);
				if( tmp_dfalt && !val ) ASTGUI.selectbox.selectOption(el, tmp_dfalt);
				break;
			case 'select-multiple':
				ASTGUI.selectbox.selectOptionMultiple(el, val);
				if( tmp_dfalt && !val ) ASTGUI.selectbox.selectOption(el, tmp_dfalt);
				break;
			default:
				break;
		}
		return;
	},

	highlightField: function( field , msg ){ // ASTGUI.highlightField( field, msg );
		if(typeof field =='string'){ field = _$(field); }
		var pcn = field.className || '' ;
		if(msg){
			 ASTGUI.feedback({ msg: msg, showfor: 3 });
		}
		field.className = 'inputValidationFailed';
		setTimeout( function(){ field.className = pcn; } , 3000 );
		try{ field.focus(); }catch(e){ }
	},

	validateFields : function( fields ){ // ASTGUI.validateFields ( [ 'field_x' , el_x ] );
		for(var g=0; g < fields.length ; g++ ){
			var field = fields[g];
			if(typeof field =='string'){ field = _$(field); }
			var validation = $(field).attr('validation') ;
			if(!validation){ continue; }
			var this_field_name = $(field).attr('field_name') ;
			var x = field.value ;
			var pcn = ( field.className ) ? field.className : '' ;

			switch(validation){
				case 'numeric':
					// check if field's value is numeric - otherwise highlight field and return false
					var fb_msg = (this_field_name) ? this_field_name + ' is a numeric field.<BR> Letters and Punctuation are not allowed in this field.' : 'This is a numeric field.<BR> Letters and Punctuation are not allowed in this field.';
					if ( /[^0-9]/.test(x) ){
						ASTGUI.highlightField( field, fb_msg );
						return false;
					}
					break;
				case 'numeric_plus':
					// check if field's value is numeric or plus sign - otherwise highlight field and return false
					var fb_msg = (this_field_name) ? this_field_name + ' is a numeric field.<BR> Letters and Punctuation are not allowed in this field.' : 'This is a numeric field.<BR> Letters and Punctuation are not allowed in this field.';
					if ( /[^0-9\+]/.test(x) ){
						ASTGUI.highlightField( field, fb_msg );
						return false;
					}
					break;
				case 'numeric_plus_w':
					// check if field's value is numeric, plus sign, or "w" - otherwise highlight field and return false
					var fb_msg = (this_field_name) ? this_field_name + ' is a numeric field.<BR> Letters and Punctuation are not allowed in this field.' : 'This is a numeric field.<BR> Letters and Punctuation are not allowed in this field.';
					if ( /[^0-9w\+]/.test(x) ){
						ASTGUI.highlightField( field, fb_msg );
						return false;
					}
					break;
				case 'alphanumeric':
					if ( /[^a-zA-Z0-9]/.test(x) ){
						var fb_msg = (this_field_name) ? this_field_name + ' is a AlphaNumeric field.<BR> Punctuation and Special Characters are not allowed in this field.' : 'This is a AlphaNumeric field.<BR> Punctuation and Special Characters are not allowed in this field.';
						ASTGUI.highlightField( field, fb_msg );
						return false;
					}
					break;
				case 'alphanumericUnd':
					var fb_msg = (this_field_name) ? this_field_name + ' is a AlphaNumeric field.<BR> Punctuation and Special Characters are not allowed in this field.' : 'This is a AlphaNumeric field.<BR> Punctuation and Special Characters are not allowed in this field.';
					if ( /[^a-zA-Z_0-9]/.test(x) ){
						ASTGUI.highlightField( field, fb_msg );
						return false;
					}
					break;
				case 'alphanumericUndSpace':
					var fb_msg = (this_field_name) ? this_field_name + ' is a AlphaNumeric field.<BR> Punctuation and Special Characters are not allowed in this field.' : 'This is a AlphaNumeric field.<BR> Punctuation and Special Characters are not allowed in this field.';
					if ( /[^a-zA-Z_0-9 ]/.test(x) ){
						ASTGUI.highlightField( field, fb_msg );
						return false;
					}
					break;
				case 'numeric_pound_star':
					var fb_msg = (this_field_name) ? 'Invalid Characters in ' + this_field_name  : 'Invalid character in a Dial Pattern field.' + '  Use only numerics, pound, and star.';
					if ( /[^0-9#\*]/.test(x) ){
						ASTGUI.highlightField( field, fb_msg );
						return false;
					}
					break;
				case 'dialpattern':
					var fb_msg = (this_field_name) ? 'Invalid Characters in ' + this_field_name  : 'Invalid character in a Dial Pattern field.';
					if ( /[^a-zA-Z_0-9\.!\[\]\-\+]/.test(x) ){
						ASTGUI.highlightField( field, fb_msg );
						return false;
					}
					break;
				case 'macaddress':
					var fb_msg = (this_field_name) ? this_field_name + ' is a MAC address field.<BR> Only HEX characters are allowed.' : 'This is a MAC address field.<BR> Only HEX characters are allowed.';
					if ( /[^a-fA-F0-9:\-]/.test(x) ){
						ASTGUI.highlightField( field, fb_msg );
						return false;
					}
					/*
					if ( x.trim() && x.length != 12 ){
						ASTGUI.highlightField( field, 'invalid MAC address !.<BR> MAC address should be 12 digits long.');
						return false;
					}
					*/
					break;
				case 'voipusername':
					var fb_msg = 'Punctuation and Special Characters are not allowed in this field.' ;
					if ( /[^a-zA-Z_0-9\.]/.test(x) ){
						ASTGUI.highlightField( field, fb_msg );
						return false;
					}
					break;
				case 'iporrange':
					var fb_msg = (this_field_name) ? this_field_name + ' does not contain a valid IP address or IP range.' : 'The IP address field does not contain a valid IP address or IP range.' ;
					var pieces = x.split("/");
					for (var j = 0; j < pieces.length; j++){
						if ( !(/^[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}$/.test(pieces[j])) ){
							ASTGUI.highlightField( field, fb_msg );
							return false;
						}
					}
					break;
				default:
					break;
			}
		}
		return true;
	},

	yesOrNo : function(yn){
		// yn = { msg:'' , ifyes: function(){} , ifno: fn (optional), btnYes_text :'Yes' (optional), btnNo_text :'No'(optional) }
		// ASTGUI.yesOrNo ( {msg: 'dddddd', ifyes:function(){alert("Yes");} ,  ifno:function(){alert("No");} , hideNo: true, dialogWidth:490, dialogHeight:290 } );
		ASTGUI.showbg(true);
		var dv = document.createElement("DIV");
		dv.className = 'dialog' ;
		dv.style.width= yn.dialogWidth || 425;
		dv.style.height= yn.dialogHeight || 175;
		dv.style.left = 270;
		dv.style.top = 110;
		(function(){
			var tbl_h = document.createElement("TABLE");
				tbl_h.cellPadding = 0;
				tbl_h.cellSpacing = 0;
				tbl_h.style.width = '100%';
				var newRow = tbl_h.insertRow(-1);
					newRow.className = "dialog_title_tr";
				var newcell = newRow.insertCell( newRow.cells.length );
					ASTGUI.events.add( newRow , "mousedown" , ASTGUI.startDrag );
					newcell.innerHTML = yn.title || 'Are you sure ?' ;
					newcell.className = "dialog_title";
			dv.appendChild(tbl_h);
		})();
		(function(){
			var dv_q = document.createElement("TABLE");
			dv_q.style.width = '100%';

			var newRow = dv_q.insertRow(-1);
			ASTGUI.domActions.tr_addCell( newRow , { html: yn.msg, align: 'center', valign:'middle', height: '70' } );

			var newRow2 = dv_q.insertRow(-1);
			var newcell2 = newRow2.insertCell( newRow2.cells.length );
			newcell2.align = 'center';
			newcell2.valign = 'bottom';
			newcell2.height = '55';
			var btnYES = document.createElement("span");
			btnYES.innerHTML = (yn.btnYes_text)? yn.btnYes_text : 'Yes' ;
			btnYES.className = 'button_yn' ;
			ASTGUI.events.add( btnYES , 'click' , function() {
				dv.parentNode.removeChild(dv);
				ASTGUI.showbg(false);
				yn.ifyes();
			});
			var btnNo = document.createElement("span");
			btnNo.innerHTML = ( yn.btnNo_text ) ? yn.btnNo_text : 'No' ;
			btnNo.className = 'button_yn' ;
			ASTGUI.events.add( btnNo , 'click' , function() {
				dv.parentNode.removeChild(dv);
				ASTGUI.showbg(false);
				if( yn.ifno ){ yn.ifno(); }
			});
			newcell2.appendChild( btnYES );
			if( !yn.hideNo ){
				newcell2.appendChild( btnNo );
			}
			dv.appendChild(dv_q);
		})();
		document.body.appendChild(dv);
	}
}; // ( AstGUI )

ASTGUI.paths = {};
ASTGUI.paths['rawman'] = '../../rawman';

ASTGUI.includeContexts = [ 'default' , 'parkedcalls' , ASTGUI.contexts.CONFERENCES , ASTGUI.contexts.RingGroupExtensions , ASTGUI.contexts.VoiceMenuExtensions , ASTGUI.contexts.QUEUES , ASTGUI.contexts.VoiceMailGroups , ASTGUI.contexts.Directory, ASTGUI.contexts.PageGroups , ASTGUI.contexts.PageAnExtension] ;

ASTGUI.customObject.prototype = {
	getProperty: function(p){
		return (this.hasOwnProperty(p))? this[p] : '' ;
	},
	updateProperties: function(prop){
		for( var g in prop ){
			if( prop.hasOwnProperty(g) ){
				this[g] = prop[g];
			}
		}
	},
	getOwnProperties: function(){
		var a = [] ;
		for( var i in this ) {
			if ( this.hasOwnProperty(i) ){
				a.push(i) ;
			}
		}
		return a ;
	}
};

ASTGUI.TABLE.prototype = {

	addRow: function( e , rowindex ){ // Usage: 'TABLE'.addRow({className : 'alt'});
		var TBL = this.TableElement ;
		var newRow = (typeof rowindex == 'number' ) ? TBL.insertRow(rowindex) : TBL.insertRow(-1) ;
		if( !!e ){
			if( e.hasOwnProperty('className') ){
				if(e.className == 'alt'){
					newRow.className = ((TBL.rows.length)%2==1) ? 'odd':'even';
				}else{
					newRow.className = e.className ;
				}
				delete e.className;
			}
			for( f in e ){
				if( !e.hasOwnProperty(f) ) continue;
				newRow.f = e[f] ;
			}
		}
		this.TableRow = newRow ;
	},

	addCell: function( C ){ // Usage: 'TABLE'.addCell({ html: 'newCell Text', align:'center', width:'20px' });
		var el = this.TableRow ;
		ASTGUI.domActions.tr_addCell( el, C );
	},

	clear : function(){ // Usage: 'TABLE'.clear()
		var el = this.TableElement ;
		ASTGUI.domActions.clear_table(el);
	},

	rowCount : function(){
		var el = this.TableElement ;
		return el.rows.length;
	}
};

var makeRequest = function( params){ // for making Asynchronus requests
	// usage ::  makeRequest ( { action :'getconfig', filename: 'something.conf', callback:callbackfunction() } )
	var cb = params.callback ; delete params.callback;
	if( params.action == "updateconfig" ){
		params.srcfilename = params.filename;
		params.dstfilename = params.filename;
		if(top.sessionData && top.sessionData.DEBUG_MODE ){top.log.ajax("AJAX Request : '" + ASTGUI.getObjectPropertiesAsString(params) + "'");}
		delete params.filename;
	}else{
		if(top.sessionData && top.sessionData.DEBUG_MODE ){top.log.ajax("AJAX Request : '" + ASTGUI.getObjectPropertiesAsString(params) + "'");}
	}

	$.get(ASTGUI.paths.rawman, params, cb);
}; // ( makeRequest )

var makeSyncRequest = function( params){ // for making synchronus requests
	// usage ::  makeSyncRequest ( { action :'getconfig', filename: 'something.conf' } ) // no need for call back function
	if(top.sessionData && top.sessionData.DEBUG_MODE ){top.log.ajax("AJAX Request : '" + ASTGUI.getObjectPropertiesAsString(params) + "'");}
	var s = $.ajax({ url: ASTGUI.paths.rawman, data: params , async: false });
	return s.responseText;
};

var context2json = function(params){ 
	// usage :: context2json({ filename:'something.conf' , context : 'context_1' , usf:0 })
	// get a specific context from a file
	// you can also use config2json - but if you want to save that 20ms wasted for 
	// parsing all those other contexts you are not inrested in, use this.
	// TODO use 'getconfig with a specific context' when running on supported asterisk versions
	var toJSO = function(z){
		var cat = (params.usf) ? new ASTGUI.customObject : [] ;
		var t = z.split("\n");
		var catno = -1 ;
		var l_catno , catname , tlc , subfield , v, subfield_a, subfield_b;
		var catfound = false;
		var cat_for_cache = "";
		for(var r=0, tl =  t.length ; r < tl ; r++){
			tlc = t[r].toLowerCase();
			if( tlc.beginsWith("category-") ){
				cat_for_cache = t[r] + "\n";
				catname = t[r].afterChar(':');
				catname = catname.trim() ;
				if( catname != params.context){ catno = -1; continue; }
				catno = Number( t[r].betweenXY('-', ':') );
				catfound = true;
			}
			if( catno == -1 ){ continue; }
			if( tlc.beginsWith("line-") ){
				cat_for_cache += t[r] + "\n";
				var l_catno = Number( t[r].betweenXY('-','-') );
				if( l_catno != catno ){ continue; }

				subfield = t[r].afterChar(':'); // subfield
				subfield = subfield.trim();
				if(params.usf){
					v = subfield.indexOf('=');
					subfield_a = subfield.substring(0,v); //subfield variable
					subfield_b = subfield.substr(v+1); //subfield variable value
					if( cat.hasOwnProperty(subfield_a) ){
						cat[subfield_a] += ',' + subfield_b ;
					}else{
						cat[subfield_a] = subfield_b;
					}
				}else{
					cat.push( subfield );
				}
			}
		}
		top.sessionData.FileCache[params.filename] = {};
		if (!params.context) { params.context = "default"; }
		top.sessionData.FileCache[params.filename][params.context] = {};
		top.sessionData.FileCache[params.filename][params.context].content = cat_for_cache;
		top.sessionData.FileCache[params.filename][params.context].modified = false;
		return catfound ? cat : null;
	};


	if( params.hasOwnProperty('configFile_output') ){
		return toJSO( params.configFile_output );
	};


	if (params.context
			&& top.sessionData.FileCache.hasOwnProperty(params.filename)
			&& top.sessionData.FileCache[params.filename].hasOwnProperty(params.context)
			&& top.sessionData.FileCache[params.filename][params.context].modified == false
			&& top.sessionData.FileCache[params.filename].modified == false) { // the context we want is up to date in cache
		top.log.debug("reading '" +  params.filename + "(" + params.context + ")" + "' from cache.");
		var s = top.sessionData.FileCache[params.filename][params.context].content;
	} else {
		top.log.ajax("AJAX Request : reading '" +  params.filename + "'");
		if (top.sessionData.AsteriskVersion && ASTGUI.version.gteq("1.6.0")) {
			var s = $.ajax({ url: ASTGUI.paths.rawman+'?action=getconfig&filename='+params.filename+'&category='+params.context, async: false }).responseText;
		} else {
			var s = $.ajax({ url: ASTGUI.paths.rawman+'?action=getconfig&filename='+params.filename, async: false}).responseText;
		}

		if(s.contains('Response: Error') && ( s.contains('Message: Config file not found') || s.contains('Message: Permission denied') ) ){
			// return ASTGUI.globals.fnf; // return 'file not found'
			if(s.contains('Message: Config file not found'))
				top.log.error( ' config file(' + params.filename +') not found ' );
			if(s.contains('Message: Permission denied'))
				top.log.error('permission denied for reading file ' + params.filename );

			return (params.usf) ? new ASTGUI.customObject : [] ;
		}
		if( s.contains('Response: Error') ){
			return (params.usf) ? new ASTGUI.customObject : [] ;
		}
	}

	return toJSO(s);
};

var config2json = function( params ){
	// usage :: config2json({filename:'something.conf', usf:0 }) or config2json({filename:'users.conf', catlist:'yes'})
	//	config2json({ configFile_output: output_string , usf:0 }) // you can also pass the output of a config file
	var toCATLIST = function(z){
		var a = [ ], catname = '' ;
		var t = z.split("\n");
		for(var r=0, tl =  t.length ; r < tl ; r++){
			if( t[r].toLowerCase().beginsWith("category") ){
				catname = t[r].afterChar(':'); // category 
				a.push( catname.trim() );
			}
		}
		return a;
	};
	var toJSO = function(z){
		// This function converts z, the asterisk config file as read using 'action=getconfig' to a JavaScript Object 
		// where z is originalRequest.responseText of the getconfig on a asterisk format config file, 
		// and p is 0 or 1, 
		//	 0 for non unique subfields ( extensions.conf context where there are multiple subfields with same name - -  Ex: 'exten ='   )
		//	 1 for unique subfields ( config files where there are no two subfields of a context have same name )
		//  if not sure ,  use p = 0 
		var p = params.usf;
		var a = new ASTGUI.customObject ;
		var json_data = "";
		var t = z.split("\n");
		var catname, subfield, v, subfield_a , subfield_b;
		var cat_for_cache = [];
		for(var r=0, tl =  t.length ; r < tl ; r++){
			if( t[r].toLowerCase().beginsWith("category") ){
				catname = t[r].afterChar(':'); // category 
				catname = catname.trim() ;
				if (!catname) { catname = "default"; }
				if (!top.sessionData.FileCache[params.filename][catname]) {
					top.sessionData.FileCache[params.filename][catname] = {};
				}
				top.sessionData.FileCache[params.filename][catname].content = t[r] + "\n";
				top.sessionData.FileCache[params.filename][catname].modified = false;
				if(!a[catname]){ // contexts could be spread at different places in the config file
					if(!p){
						a[catname] = [];
					}else{
						a[catname] = new ASTGUI.customObject ;
					}
				}
			}else if ( t[r].toLowerCase().beginsWith("line") ){
				top.sessionData.FileCache[params.filename][catname].content += t[r] + "\n";
				subfield = t[r].afterChar(':'); // subfield
				subfield = subfield.trim();
				if(!p){
					a[catname].push(subfield);
				}else{
					v = subfield.indexOf("=");
					subfield_a = subfield.substring(0,v);//subfield variable
					subfield_b =  subfield.substr(v+1) ;//subfield variable value
					if(a[catname].hasOwnProperty(subfield_a)){
						a[catname][subfield_a] += ',' + subfield_b;
					}else{
						a[catname][subfield_a] = subfield_b;
					}
				}
			}
		}
		return a ;
	};

	if( params.configFile_output ){
		return toJSO( params.configFile_output );
	};

	top.log.ajax("AJAX Request : reading '" +  params.filename + "'");

	if( top.sessionData.FileCache.hasOwnProperty(params.filename) &&  top.sessionData.FileCache[params.filename].modified == false){ // if file is in cache and is not modified since
		var s = top.sessionData.FileCache[params.filename].content ;
	}else{
		var s = $.ajax({ url: ASTGUI.paths.rawman+'?action=getconfig&filename='+params.filename, async: false }).responseText;

		top.sessionData.FileCache[params.filename] = {};
		top.sessionData.FileCache[params.filename].content = s;
		top.sessionData.FileCache[params.filename].modified = false;
	}

	if( s.contains('Response: Error') && s.contains('Message: Config file not found') ){
		// return ASTGUI.globals.fnf; // return 'file not found'
		top.log.error( ' config file(' + params.filename +') not found ' );
		return new ASTGUI.customObject;
	}
	if( s.contains('Response: Error') && s.contains('Message: Permission denied') ){
		top.log.error('permission denied for reading file ' + params.filename );
		return new ASTGUI.customObject;
	}

	if( params.catlist == 'yes'){
		return toCATLIST(s);
	}
	return toJSO(s);
}; // ( config2json )

var listOfSynActions = function(file){
	// this object should be used if you have 1 to 4 update actions needed to be performed synchronusly.
	// if you have a long list of update actions - use Asynchronus variant of this function  (listOfActions)
	/*	:: Usage ::
		var u = new listOfSynActions('users.conf') ;
		u.new_action('newcat', '6002', '', ''); // create new context
		u.new_action('append', '6002', 'allow', 'all'); // append 'allow=all'
		u.new_action('update', '6002', 'disallow', 'none','all'); // update 'disallow=all' to 'disallow=none'
		u.callActions(); // this is Synchronus function - these actions will be called immediately and the result will be returned
	*/
	//
	if ( !(this instanceof listOfSynActions) ){ return new listOfSynActions(file) ; }
	this.FILE_CONTENT = null ;
	this.params = {} ;
	this.params.action = 'updateconfig';
	this.params.srcfilename = file;
	this.params.dstfilename = file;
	if (ASTGUI.version.gteq("1.6.0")) {
		this.FILE_CONTENT = config2json({ filename: file , usf:0 }) ;
	}
	this.actionCount = 0;
};

listOfSynActions.prototype = {
	new_action: function(action, cat, name, value, match){
		var s="";
		if (ASTGUI.version.gteq("1.6.0") && this.FILE_CONTENT != null ) {
			// the update/delete/delcat commands fail in Asterisk 1.6.X/trunk if the corresponding category/variables are not found
			// In Asterisk 1.4 we do not have to do this check
			switch( action ){
				case 'update':
					if( !this.FILE_CONTENT.hasOwnProperty(cat) ) return s;
					if( this.FILE_CONTENT[cat].indexOfLike(name+'=') == -1 ){
						action = 'append';
					}
					break;
				case 'delete':
					if( !this.FILE_CONTENT.hasOwnProperty(cat) || this.FILE_CONTENT[cat].indexOfLike(name+'=') == -1 ){
						return s;
					}
					break;
				case 'delcat':
					if( !this.FILE_CONTENT.hasOwnProperty(cat) ){
						return s;
					}
					break;
				default: break;
			}
		}

		var cnt = "" + this.actionCount;
		if(this.actionCount > 5){
			top.log.error('AG150'); // alert to developer
		}
		while(cnt.length < 6){ cnt = "0" + cnt; }
		this.params['Action-' + cnt] = action; // jquery takes care of encodeURIComponent(action) 
		this.params['Cat-' + cnt] = cat;
		this.params['Var-' + cnt] = name ;
		this.params['Value-' + cnt] = value ;
		if (match){ this.params['Match-' + cnt] = match; }
		this.actionCount += 1 ;
	},
	clearActions: function(newfile){ // newfile is optional
		var fn = newfile || this.params.srcfilename;
		this.actionCount = 0 ;
		this.params = {} ;
		this.params.action = 'updateconfig';
		this.params.srcfilename = this.params.dstfilename = fn;
		if (ASTGUI.version.gteq("1.6.0")) {
			this.FILE_CONTENT = config2json({ filename: fn , usf:0 }) ;
		}
	},
	callActions: function(){
		if(!this.actionCount){ return 'Response: Success'; }
		if(top.sessionData.DEBUG_MODE ){
			top.log.ajax("AJAX Request : '" + ASTGUI.getObjectPropertiesAsString(this.params) + "'"); 
		}
		var s = $.ajax({ url: ASTGUI.paths.rawman, data: this.params , async: false });
		return s.responseText;
	}
};

var listOfActions = function(fn){
	/*
	usage: 	var x = new listOfActions('users.conf');
		x.new_action('delcat', '6001', '', ''); // delete context 6001
		x.new_action('renamecat', '6003', '', '6004'); // rename context 6003 to 6004

		x.new_action('newcat', '6002', '', ''); // create new context
		x.new_action('append', '6002', 'allow', 'all'); // append 'allow=all'
		x.new_action('update', '6002', 'disallow', 'none','all'); // update 'disallow=all' to 'disallow=none'
		x.new_action('delete', '6002', 'hasiax', '',''); // delete subfield 'hasiax' (whatever may be the value)
		x.new_action('delete', '6002', 'hassip', '','yes'); // delete subfield 'hassip=yes' 
		.....
		x.callActions(after); // where after is the callback function

	*/
	if ( !(this instanceof listOfActions) ) { return (fn) ? new listOfActions(fn) : new listOfActions() ; }

	this.FILE_CONTENT = null ;
	this.current_batch = 1 ;
	this.current_batch_actionnumber = 0 ;
	this.actions = {};
	if(fn){ 
		this.filename = fn;
		if (ASTGUI.version.gteq("1.6.0")) {
			this.FILE_CONTENT = config2json({ filename: fn , usf:0 }) ;
		}
	}
};

listOfActions.prototype = {
	filename: function(fn){
		this.filename = fn;
		if (ASTGUI.version.gteq("1.6.0")) {
			this.FILE_CONTENT = config2json({ filename: fn , usf:0 }) ;
		}
	},
	getacn: function(nc){
		return this.current_batch_actionnumber;
	},
	build_action: function(action, count, cat, name, value, match){
		var s="";
		if (ASTGUI.version.gteq("1.6.0") && this.FILE_CONTENT != null ) {
			// the update/delete/delcat commands fail in Asterisk 1.6.X/trunk if the corresponding category/variables are not found
			// In Asterisk 1.4 we do not have to do this check
			switch( action ){
				case 'append':
					if( !this.FILE_CONTENT.hasOwnProperty(cat) ) return s;
					this.FILE_CONTENT[cat].push( name + "=" + value );
					break;
				case 'delcat':
					if( !this.FILE_CONTENT.hasOwnProperty(cat) ){
						return s;
					}
					delete this.FILE_CONTENT[cat] ;
					break;
				case 'delete':
					if( !this.FILE_CONTENT.hasOwnProperty(cat) ) return s;
					var tmp_index = this.FILE_CONTENT[cat].indexOfLike(name+'=') ;
					if( tmp_index == -1 ) return s;
					this.FILE_CONTENT[cat].splice(tmp_index,1);
					break;
				case 'newcat':
					if( this.FILE_CONTENT.hasOwnProperty(cat) ) return s;
					this.FILE_CONTENT[cat] = [] ;
					break;
				case 'update':
					if( !this.FILE_CONTENT.hasOwnProperty(cat) ) return s;
					var tmp_index = this.FILE_CONTENT[cat].indexOfLike(name+'=') ;
					var tmp_line = name + "=" + value ;
					if(  tmp_index == -1 ){
						action = 'append';
						this.FILE_CONTENT[cat].push(tmp_line);
					}else{
						this.FILE_CONTENT[cat][tmp_index] = tmp_line ;
					}
					break;
				default: break;
			}
		}

		var cnt = "" + count;
		while(cnt.length < 6)
			cnt = "0" + cnt;
		s += "&Action-" + cnt + "=" + encodeURIComponent(action);
		s += "&Cat-" + cnt + "=" + encodeURIComponent(cat);
		s += "&Var-" + cnt + "=" + encodeURIComponent(name);
		s += "&Value-" + cnt + "=" + encodeURIComponent(value);
		if (match)
			s += "&Match-" + cnt + "=" + encodeURIComponent(match);
		return s;
	},
	addNewChange: function(nc){
		var t = 'act_' + this.current_batch;
		this.actions[t] = (this.current_batch_actionnumber)? this.actions[t] + nc: nc ;
		if( this.current_batch_actionnumber == 5 ){
			this.current_batch++;
			this.current_batch_actionnumber = 0;
		}else{
			this.current_batch_actionnumber++;
		}
	},
	new_action: function(a,b,c,d,e){
		var z = this.getacn();
		var nc = e?this.build_action(a, z, b, c, d, e) : this.build_action(a, z, b, c, d) ;
		if(nc)this.addNewChange(nc);
	},
	callActions: function(callback){
		var ajxs = parent.document.getElementById('ajaxstatus') ;
		ajxs.style.display = '';
		if( this.current_batch == 1 && this.current_batch_actionnumber == 0 ){ 
			setTimeout( function(){ ajxs.style.display = 'none'; callback(); }, 500 ) ;
			return;
		}
		var pre_uri = "action=updateconfig&srcfilename=" + encodeURIComponent(this.filename) + "&dstfilename=" + encodeURIComponent(this.filename);
		var treq = this.actions;
		var start_sqreqs = function(st){
			var f = treq[ 'act_' + st ];
			if(f){
				top.log.ajax("AJAX Request : '" + pre_uri + f + "'");
				$.ajax({ type: "GET", url: ASTGUI.paths.rawman, data: pre_uri + f , success: function(msg){
					if( msg && typeof msg == 'string' && msg.contains('Response: Error') && msg.contains('Message:') ){
						var err_msg = msg.afterStr('Message:');
						callback(false, err_msg);
					}else{
						start_sqreqs(st+1);
					}
				}});
			}else{
				setTimeout( function(){ ajxs.style.display = 'none'; callback(true); }, 500 ) ;
			}
		};
		start_sqreqs(1);
	}
};


(function(){
	var onload_doThese = function(){
		if( top.sessionData ){
			window.onerror = function(err, url, errcode ){ // Log any errors on this page
				var msg = 'ErrorCode / LineNumber : ' + errcode  + '<BR> Error : ' + err + '<BR> Location: ' + url ;
				top.log.error(msg);
				ASTGUI.dialog.hide();
				if( top.sessionData && top.sessionData.DEBUG_MODE ){ // show alerts only in debug mode
					var alertmsg = 'ErrorCode / LineNumber : ' + errcode  + '\n Error : ' + err + '\n Location: ' + url ;
					alert(alertmsg);
				}
				if ( jQuery.browser.msie ){ // If critical error in IE , reload entire GUI
					top.window.reload();
				}
				return true;
			};
			ASTGUI.showToolTips(); // Load any tooltips in this page

			var AJS = parent.document.getElementById('ajaxstatus') ;
			if( window.jQuery && AJS ){
				$().ajaxStart( function(){ AJS.style.display = ''; });
				$().ajaxStop( function(){
					setTimeout( function(){ AJS.style.display = 'none'; }, 500 );
				});
			}
		}
		if( window.localajaxinit && (typeof localajaxinit == 'function' ) ){
			window.localajaxinit();
		}
	};
	if( window.attachEvent ){
		window.attachEvent( 'onload' , onload_doThese );
	}else if( window.addEventListener ){
		window.addEventListener( 'load' , onload_doThese , false );
	}
})();

