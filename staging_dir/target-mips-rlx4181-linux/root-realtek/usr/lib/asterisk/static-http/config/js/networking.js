/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * networking.html functions
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
function enabledisable_dhcpwan(){
	if( _$('DHCP_WAN').checked ){
		_$('IP_WAN').disabled = true;
		_$('SUBNET_WAN').disabled = true;
		_$('GATEWAY_WAN').disabled = true;
		_$('DNS_WAN').disabled = true;
	}else{
		_$('IP_WAN').disabled = false;
		_$('SUBNET_WAN').disabled = false;
		_$('GATEWAY_WAN').disabled = false;
		_$('DNS_WAN').disabled = false;
	}
}

function enabledisable_dhcplan(){
	if( _$('DHCP_LAN').checked ){
		_$('START_RANGE_LAN').disabled = false;
		_$('END_RANGE_LAN').disabled = false;
		_$('LEASE_LAN').disabled = false;
		_$('MAX_LEASE').disabled = false;

	}else{
		_$('START_RANGE_LAN').disabled = true;
		_$('END_RANGE_LAN').disabled = true;
		_$('LEASE_LAN').disabled = true;
		_$('MAX_LEASE').disabled = true;
	}
}

networking_ApplyChanges = function(){

	var cb = function(){
		var network_params = "/etc/asterisk/scripts/network.params";

		if(_$('DHCP_WAN').checked){var dwv = "on"; }else{var dwv = "off"; }
		if(_$('DHCP_LAN').checked){var dlv = "on"; }else{var dlv = "off"; }
		if(_$('SSHACCESS').checked){var ssv = "yes"; }else{var ssv = "no"; }
		if(_$('GUI_WAN').checked){var gwn = "on"; }else{var gwn = "off"; }
		if(_$('DHCP_WAN_PROVISION').checked){var dwp = "on"; var serveriface = "eth0"; }else{var dwp = "off"; var serveriface = "eth1";}

		var cmd1 = "echo \"DHCP_WAN=" + dwv + " IP_WAN=" + _$('IP_WAN').value + " SUBNET_WAN=" + _$('SUBNET_WAN').value + " GATEWAY_WAN=" + _$('GATEWAY_WAN').value + " DNS_WAN=" + _$('DNS_WAN').value + " GUI_WAN=" + gwn  + " DHCP_WAN_PROVISION=" + dwp + "\" > " + network_params;

		var cmd2 = "echo \" DHCP_LAN=" + dlv + " IP_LAN=" + _$('IP_LAN').value + " SUBNET_LAN=" + _$('SUBNET_LAN').value + " DNS_LAN=" + _$('DNS_LAN').value + "\" >> " + network_params;

		var cmd3 = "echo \" DNS_LAN=" + _$('DNS_LAN').value + " START_RANGE_LAN=" + _$('START_RANGE_LAN').value + " END_RANGE_LAN=" + _$('END_RANGE_LAN').value + " SSHACCESS=" + ssv + "\" >> " + network_params;

		var cmd4 = "echo \" HOSTNAME=" + _$('HOSTNAME').value + " LEASE_LAN=" + _$('LEASE_LAN').value + " NTP_ADDRESS=" + _$('NTP_ADDRESS').value + " TFTP_LAN=" + _$('TFTP_LAN').value + " DOMAIN_LAN=" + _$('DOMAIN_LAN').value + " MAX_LEASE=" + _$('MAX_LEASE').value + "\" >> " + network_params;

		var cmd5 = top.sessionData.directories.script_NetworkSettings;

		(function(){
			var u = new listOfSynActions('phoneprov.conf') ;
			u.new_action('update', 'general','serveriface', serveriface ); 
			var tmp = u.callActions();
		})();

		ASTGUI.dialog.waitWhile('Applying Changes');
		parent.ASTGUI.systemCmd( cmd1, function(){ 
			parent.ASTGUI.systemCmd( cmd2, function(){ 
				parent.ASTGUI.systemCmd( cmd3, function(){ 
					parent.ASTGUI.systemCmd( cmd4, function(){
						setTimeout( function(){top.window.location.reload();}, 4000 );
						parent.ASTGUI.systemCmd( cmd5 + ' &', function(){ });
					});
				});
			});
		});
	};

	(function(){
		var x = new listOfActions();
		x.filename('networking.conf');
		x.new_action('update', 'general', 'HOSTNAME', _$("HOSTNAME").value ); 
		x.new_action('update', 'general', 'NTP_ADDRESS', _$("NTP_ADDRESS").value ); 
		x.new_action('update', 'general', 'SSHACCESS', (_$("SSHACCESS").checked)?'yes':'no' );
		x.new_action('update', 'general', 'TFTP_LAN', _$("TFTP_LAN").value );
		x.new_action('update', 'general', 'DHCP_WAN', (_$("DHCP_WAN").checked)?'yes':'no' );
		x.new_action('update', 'general', 'IP_WAN', _$("IP_WAN").value );
		x.new_action('update', 'general', 'SUBNET_WAN', _$("SUBNET_WAN").value );
		x.new_action('update', 'general', 'GATEWAY_WAN', _$("GATEWAY_WAN").value );
		x.new_action('update', 'general', 'DNS_WAN', _$("DNS_WAN").value );
		x.new_action('update', 'general', 'GUI_WAN', (_$("GUI_WAN").checked)?'yes':'no' );
		x.new_action('update', 'general', 'DHCP_WAN_PROVISION', (_$("DHCP_WAN_PROVISION").checked)?'yes':'no' );
		x.new_action('update', 'general', 'DOMAIN_LAN', _$("DOMAIN_LAN").value );
		x.new_action('update', 'general', 'DHCP_LAN', (_$("DHCP_LAN").checked)?'yes':'no' );
		x.new_action('update', 'general', 'IP_LAN', _$("IP_LAN").value );
		x.new_action('update', 'general', 'SUBNET_LAN', _$("SUBNET_LAN").value );
		x.new_action('update', 'general', 'DNS_LAN', _$("DNS_LAN").value );
		x.new_action('update', 'general', 'START_RANGE_LAN', _$("START_RANGE_LAN").value );
		x.new_action('update', 'general', 'END_RANGE_LAN', _$("END_RANGE_LAN").value );
		x.new_action('update', 'general', 'LEASE_LAN', _$("LEASE_LAN").value );
		x.new_action('update', 'general', 'MAX_LEASE', _$("MAX_LEASE").value );
		x.callActions(cb) ;
	})();
}

function localajaxinit() {
	top.document.title = 'Network Settings' ;
	parent.ASTGUI.dialog.waitWhile('Loading Information ...');
	if(!parent.sessionData.PLATFORM.isAA50_OEM ){
		$('#URLforPolycom').show();
	}
	var t = [
		{url:'#', desc:'General', click_function: function(){ $('#general_div').show(); $('#lan_div').hide(); $('#wan_div').hide(); }  } ,
		{url:'#', desc:'WAN', click_function: function(){ $('#general_div').hide(); $('#lan_div').hide(); $('#wan_div').show(); } },
		{url:'#', desc:'LAN', click_function: function(){ $('#general_div').hide(); $('#lan_div').show(); $('#wan_div').hide(); } },
		{url:'timezone.html', desc:'TimeZone'}
	];
	ASTGUI.tabbedOptions( _$('tabbedMenu') , t );

	var ssh_alert = function(){
		var msg = "Warning: the username and password for ssh does not correspond to \n"
			+ "the username and password that is used to log into the web interface.\n\n"
			+ "The default ssh username is 'root' and the default password is 'digium'. \n\n"
			+ "It is advised that you immediately change your ssh password with the \n"
			+ "'passwd' command and then click on the 'Apply Changes' button \n"
			+ "in the web gui to save your new ssh password.\n\n"
			+ "Notice! Direct editing of the configuration files renders your product \n"
			+ "unsupportable by Digium.  Although you are free to make changes, Digium \n"
			+ "does not provide support for your changes and cannot guarantee their \n"
			+ "proper functioning between firmware revisions.  Further, Digium does \n"
			+ "not provide support for bugs uncovered by manual editing of the \n"
			+ "configuration files.  If your unit becomes inoperable due to editing \n"
			+ "of the configuration files, Digium Technical Support will request that \n"
			+ "you reset your unit to Factory Default configuration.";

		if(_$('SSHACCESS').checked){alert(msg);}
	};
	ASTGUI.events.add( _$('SSHACCESS'), 'click', ssh_alert);
	/*
	ASTGUI.events.add( _$('DHCP_LAN') , 'change', function(){ 
		if( _$('DHCP_LAN').checked ){ _$('DHCP_WAN_PROVISION').checked = false; }
	});

	ASTGUI.events.add( _$('DHCP_WAN_PROVISION') , 'change', function(){ 
		if( _$('DHCP_WAN_PROVISION').checked ){	 _$('DHCP_LAN').checked = false; }
	});
	*/
	try{
		var c = config2json({filename:'networking.conf', usf:1});
		(function(){
			var y = c['general'];
			var aa = function(x){
				if(y.hasOwnProperty(x)){return y[x];}
				return '';
			};
			_$("HOSTNAME").value = aa('HOSTNAME');
			_$("NTP_ADDRESS").value = aa('NTP_ADDRESS');
			_$("SSHACCESS").checked = ( aa("SSHACCESS")=='yes' || aa("SSHACCESS")=='on' ) ? true : false ;
			_$("TFTP_LAN").value = aa('TFTP_LAN');
			_$("DHCP_WAN").checked = (aa("DHCP_WAN")=='on' || aa("DHCP_WAN")=='yes' ) ? true : false ;
			_$("IP_WAN").value = aa('IP_WAN');
			_$("SUBNET_WAN").value = aa('SUBNET_WAN');
			_$("GATEWAY_WAN").value = aa('GATEWAY_WAN');
			_$("DNS_WAN").value = aa('DNS_WAN');
			_$("GUI_WAN").checked = ( aa("GUI_WAN")=='yes' || aa("GUI_WAN")=='on' ) ? true : false ;
			_$("DHCP_WAN_PROVISION").checked = ( aa("DHCP_WAN_PROVISION")=='yes' || aa("DHCP_WAN_PROVISION")=='on' ) ? true : false ;
			_$("DOMAIN_LAN").value = aa('DOMAIN_LAN');
			_$("DHCP_LAN").checked = ( aa("DHCP_LAN")=='on' || aa("DHCP_LAN")=='yes' ) ? true : false ;
			_$("IP_LAN").value = aa('IP_LAN');
			_$("SUBNET_LAN").value = aa('SUBNET_LAN');
			_$("DNS_LAN").value = aa('DNS_LAN');
			_$("START_RANGE_LAN").value = aa('START_RANGE_LAN');
			_$("END_RANGE_LAN").value = aa('END_RANGE_LAN');
			_$("LEASE_LAN").value = aa('LEASE_LAN');
			_$("MAX_LEASE").value = aa('MAX_LEASE');
		})();
	}catch(err){

	}finally{
		parent.ASTGUI.dialog.hide();
	}
	enabledisable_dhcpwan();
	enabledisable_dhcplan();

	if(window.location.href.contains('?tab=wan')){
		$('#tabbedMenu').find('A:eq(1)').click();
	}else if (window.location.href.contains('?tab=lan')){
		$('#tabbedMenu').find('A:eq(2)').click();
	}else{
		$('#tabbedMenu').find('A:eq(0)').click();
	}

	ASTGUI.events.add( _$('DHCP_WAN'), 'click', enabledisable_dhcpwan);
	ASTGUI.events.add( _$('DHCP_LAN'), 'click', enabledisable_dhcplan);
	ASTGUI.COMBOBOX.call( _$('NTP_ADDRESS') , ASTGUI.globals.timeservers, 275 ) ;


}
