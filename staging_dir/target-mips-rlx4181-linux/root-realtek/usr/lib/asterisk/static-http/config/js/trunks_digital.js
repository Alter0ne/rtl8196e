/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * trunks_digital.html functions
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

var load_DigitalTrunksTable = function(){
	var addCell = ASTGUI.domActions.tr_addCell; // temporarily store the function
	(function(){ // add first row
		var newRow = DOM_table_DigitalTrunks_list.insertRow(-1);
		newRow.className = "frow";
		addCell( newRow , { html:'', width:'15px' } );
		addCell( newRow , { html:'Trunk'} );
		addCell( newRow , { html:'Signalling'} );
		addCell( newRow , { html:'Channels'} );
		addCell( newRow , { html:''} );
	})();	

	(function (){
		var c = parent.sessionData.pbxinfo['trunks']['pri'] ;
		for(var d in c){if(c.hasOwnProperty(d)){
			var newRow = DOM_table_DigitalTrunks_list.insertRow(-1);
			newRow.className = ((DOM_table_DigitalTrunks_list.rows.length)%2==1)?'odd':'even';
			addCell( newRow , { html:'', width:'15px' });
			addCell( newRow , { html: c[d]['trunkname'] });
			addCell( newRow , { html: c[d]['signalling'] }); // 
			addCell( newRow , { html: c[d][top.sessionData.DahdiChannelString] });
			addCell( newRow , { html:''} );
		}}


		var c = parent.sessionData.pbxinfo['trunks']['bri'] ;
		for(var d in c){if(c.hasOwnProperty(d)){
			var newRow = DOM_table_DigitalTrunks_list.insertRow(-1);
			newRow.className = ((DOM_table_DigitalTrunks_list.rows.length)%2==1)?'odd':'even';
			addCell( newRow , { html:'', width:'15px' });
			addCell( newRow , { html: c[d]['trunkname'] });
			addCell( newRow , { html: 'BRI' });
			addCell( newRow , { html: c[d]['ports'] });
			addCell( newRow , { html:''} );
		}}
	})();

	if( DOM_table_DigitalTrunks_list.rows.length == 1  ){
		ASTGUI.domActions.clear_table(DOM_table_DigitalTrunks_list);
		var newRow = DOM_table_DigitalTrunks_list.insertRow(-1);
		addCell( newRow , { html: "<BR> <B>No T1/E1/BRI Trunks found !!</B><BR> Your digital hardware can be configured using the 'Configure Hardware', 'mISDN Config' sections of the GUI.<BR><BR>" } );
	}
};



var localajaxinit = function(){
	top.document.title = 'Manage PRI/BRI Trunks ' ;
	var tmp_providersPage = ( parent.sessionData.PLATFORM.isAA50 || parent.sessionData.PLATFORM.isABE ) ? 'trunks_sps.html' : 'trunks_providers.html';
	var t = [];
		t.push({url:'trunks_analog.html', desc:'Analog Trunks'});
	if( parent.sessionData.PLATFORM.isAA50 || parent.sessionData.PLATFORM.isABE ){
		t.push({url:'trunks_sps.html', desc:'Service Providers'});
	}
		t.push({url:'trunks_voip.html', desc:'VOIP Trunks'});
	if( !parent.sessionData.PLATFORM.isAA50 ){
		t.push({url:'trunks_digital.html', desc:'T1/E1/BRI Trunks', selected:true});
	}
	ASTGUI.tabbedOptions( _$('tabbedMenu') , t);

	// show digital trunks in table_DigitalTrunks_list - parent.sessionData.pbxinfo['trunks']['pri']
	DOM_table_DigitalTrunks_list = _$('table_DigitalTrunks_list');

	load_DigitalTrunksTable();
}
