/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * trunks_sps.html functions
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
var PROVIDERS_URL = 'https://gui-dl.digium.com/providers.js';

loaded_external = false;
var WARNING_MESSAGE = "The service provider information used in this product is downloaded on demand from Digium's web site. "
+ "This download occurs over a secure (SSL)"
+ "connection, ensuring that only safe, tested and supported service provider information will be used.<BR><BR>"
+ "Your web browser should not display any 'insecure connection' or 'unverified certificate' warnings "
+ "when this download occurs. If you see any messages of this type when using the"
+ "'Service Providers' page, you should not allow your browser to proceed with the information download, as the"
+ "security of your connection may have been compromised.<BR><BR>"
+ "For your convenience, this warning message will only be displayed the first time you use the 'Service Providers' page in this product. "
+ "If you choose to continue beyond this point, the system will remember that you have seen this warning message and will not display it again.";


var localajaxinit = function(){
	top.document.title = 'Service Providers' ;
	(function(){
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
	})();
	(function (){
		var t = [];
		if( !parent.sessionData.PLATFORM.AA50_SKU.contains('800') ){
			t.push({url:'trunks_analog.html', desc:'Analog Trunks'});
		}
		if( parent.sessionData.PLATFORM.isAA50 || parent.sessionData.PLATFORM.isABE ){
			t.push({url:'trunks_sps.html', desc:'Service Providers', selected:true });
		}
			t.push({url:'trunks_voip.html', desc:'VOIP Trunks'});
		if( !parent.sessionData.PLATFORM.isAA50 ){
			t.push({url:'trunks_digital.html', desc:'T1/E1/BRI Trunks'});
		}
		ASTGUI.tabbedOptions( _$('tabbedMenu') , t);
	})();

	var LOAD_Providers = function(){
		parent.ASTGUI.dialog.waitWhile(' Loading Providers configuration info ...');
		$.getScript( PROVIDERS_URL , function(tmp_script_status){
			if(tmp_script_status == 404){
				parent.ASTGUI.dialog.hide();
				_$('unableToLoadProviders_container').style.display = '';
				return;
			}
			if(loaded_external){
				whenThisFileisLoaded(); // this function is defined in the loaded file
			}
		});
		(function (){
			var oops = function(){
				if(loaded_external){return;}
				parent.ASTGUI.dialog.hide();
				_$('unableToLoadProviders_container').style.display = '';
				//alert("Failed loading providers configuration");
			};
			setTimeout( oops , 30000 ); // if not loaded with in 10 seconds - then consider as failed
		})();
	};

	var gp = config2json({filename: ASTGUI.globals.configfile, usf:1});
	if( gp.hasOwnProperty('general') && gp['general'].getProperty('warning_providers') == 'yes' ){
		LOAD_Providers();
	}else{
		parent.ASTGUI.yesOrNo({
			msg: WARNING_MESSAGE ,
			ifyes: function(){
				ASTGUI.updateaValue({ file:ASTGUI.globals.configfile , context :'general', variable :'warning_providers', value :'yes' });
				LOAD_Providers();
			},
			ifno:function(){
				ASTGUI.feedback( { msg:'Aborted loading Providers', showfor:2 });
			},
			title : 'Security Warning !',
			btnYes_text :'Continue',
			btnNo_text : 'Cancel',
			dialogWidth: 600,
			dialogHeight: 330
		});
	}

}
