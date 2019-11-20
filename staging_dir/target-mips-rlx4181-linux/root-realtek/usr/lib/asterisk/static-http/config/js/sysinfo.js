/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * sysinfo.html functions
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

var percentage_usage = function(du){
	var get_CF_percent = function(a){
		var du_lines = a.split('\n');
		var line;
		for( var t =0 ; t < du_lines.length ; t++ ){
			line = du_lines[t];
			if( ! line.beginsWith('/dev/hda1 ') ){ continue; }
	
			var before_p_str = line.beforeChar('%');
			var start_p = before_p_str.lastIndexOf(' ');
			return before_p_str.substring(start_p+1);
		}
		return 0;
	};
	var p = get_CF_percent(du);
	if( p == 0 ){return;}
	var width_1 = Math.floor( Number(p) * 1.5 ) ;
	var width_2 = 150 - width_1 ;
	var table_percent = "<table><tr><td valign=top>" 
			+ "<table width=150 border=0 cellpadding=0 cellspacing=0 bgcolor=#E6C79D>"
			+ "<tr><td width='" + width_1 + "' height=12 bgcolor='#6D4B1D'></td>"
			+ "<td width='" + width_2 + "'></td></tr></table>"
		+ "</td><td>&nbsp;" + p + "% used</td>"
		+ "</tr></table>";
	_$('CF_Usage_td').innerHTML = table_percent ;
	_$('CF_Usage').style.display = '';
};

function getsysinfohtml(){
	ASTGUI.systemCmdWithOutput( 'uname -a' , function(uname){
		_$('osversion').innerHTML = uname;

		ASTGUI.systemCmdWithOutput( 'uptime' , function(uptime){
			_$('uptime').innerHTML = uptime.replace(/load average/, "<BR>Load Average");

			ASTGUI.systemCmdWithOutput( 'date' , function(DATE_OUTPUT){
				_$('today').innerHTML = (parent.sessionData.PLATFORM.isAA50) ? ASTGUI.toLocalTime(DATE_OUTPUT).camelize() : DATE_OUTPUT ;
				if(parent.sessionData.PLATFORM.isAA50) {
					_$('today').innerHTML += "&nbsp;&nbsp;<A href='date.html' class='splbutton' title='Click to update Date and Time'><B>Edit</B></A>";
				}

				ASTGUI.systemCmdWithOutput( 'hostname' , function(HOSTNAME){
					_$('hostname').innerHTML = HOSTNAME ;
				});
			});
		});
	});

	_$('asterisk').innerHTML =  parent.sessionData.AsteriskVersionString + "<BR>" + "Asterisk GUI-version : " + ( parent.sessionData.gui_version || ASTGUI.globals.version ) ;
	$('#tabbedMenu').find('A:eq(0)').click();
}
