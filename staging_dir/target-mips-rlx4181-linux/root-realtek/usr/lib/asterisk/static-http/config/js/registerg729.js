/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * registerg729.html functions
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

var REGCONTEXT = 'general' ;

var list_keyFiles = function(){
	
	ASTGUI.listSystemFiles( parent.sessionData.directories.astvarlibdir + 'licenses/' , function(listOfFiles) {
		if(listOfFiles.length){
			$('#div_list_keysheading').show().html( "<B>License Keys:</B> " + listOfFiles.join(', '));
		}else{
			$('#div_list_keysheading').hide();
		}
	});
};

var localajaxinit = function(){
	top.document.title = 'G.729 Codec Registration' ;

	var s = ASTGUI.cliCommand('show g729'); // 0/0 encoders/decoders of 2 licensed channels are currently in use
	s = parent.ASTGUI.parseCLIResponse(s);

	if( s.contains('encoders/decoders')){
		if( s.contains('of 0 licensed channels are currently in use') ) {
			$('#div_list_heading').html('You do not have any G.729 Codec License Keys installed');
		}else{
			$('#div_list_heading').html(s);
		}
	}else{
		var s1 = ASTGUI.cliCommand('module show');
		if( !s1.contains('codec_g729a') ){
			$('#div_list_heading').html('<font size=+1> codec_g729a  Not installed ! </font>');
		}
	}

	list_keyFiles();
};


var register_New_license = function(){
		if ( !ASTGUI.checkRequiredFields(['text_g729licensekey', 'text_first_name', 'text_last_name', 'text_address1', 'text_city', 'text_country', 'text_phone', 'text_email']) ) return ;
		var x = new listOfActions(ASTGUI.globals.g729RegInfo);
		x.new_action('delcat', REGCONTEXT, '', '');
		x.new_action('newcat', REGCONTEXT, '', '');
		x.new_action('append', REGCONTEXT, 'first_name', ASTGUI.getFieldValue('text_first_name') );
		x.new_action('append', REGCONTEXT, 'last_name', ASTGUI.getFieldValue('text_last_name') );
		x.new_action('append', REGCONTEXT, 'company', ASTGUI.getFieldValue('text_company') );
		x.new_action('append', REGCONTEXT, 'address1', ASTGUI.getFieldValue('text_address1') );
		x.new_action('append', REGCONTEXT, 'address2', ASTGUI.getFieldValue('text_address2') );
		x.new_action('append', REGCONTEXT, 'city', ASTGUI.getFieldValue('text_city') );
		x.new_action('append', REGCONTEXT, 'state', ASTGUI.getFieldValue('text_state') );
		x.new_action('append', REGCONTEXT, 'post_code', ASTGUI.getFieldValue('text_post_code') );
		x.new_action('append', REGCONTEXT, 'country', ASTGUI.getFieldValue('text_country') );
		x.new_action('append', REGCONTEXT, 'phone', ASTGUI.getFieldValue('text_phone') );
		x.new_action('append', REGCONTEXT, 'email', ASTGUI.getFieldValue('text_email') );

		ASTGUI.dialog.waitWhile('Registering Key...');
		x.callActions(function(){
			ASTGUI.systemCmdWithOutput( top.sessionData.directories.script_Registerg729 + " " + ASTGUI.getFieldValue('text_g729licensekey'), function(result){
				ASTGUI.dialog.hide();
				if(result.contains('SUCCESS')){
					ASTGUI.systemCmd('save_config', function(){
						alert('Registration Successfull ! \n Please reboot for changes to take effect');
						window.location.reload();
					});
				}
				if(result.contains('FAILED')){
					alert('Registration FAILED ! \n' + result.withOut('FAILED'));
					window.location.reload();
				}
			});
		});


};


var download_eula = function(){
	ASTGUI.dialog.waitWhile('downloading EULA for the product...');
	ASTGUI.systemCmdWithOutput('register_tool --geteula --category 1 --product 16 --key=XX', function(TMP_EULA){
		if( !TMP_EULA.trim() ){
			ASTGUI.dialog.hide();
			alert('Unable to download license from the registration server ! \n Please make sure your Asterisk Appliance is connected to the Internet.');
			return;
		}
		ASTGUI.dialog.hide();
		$('#EULA_ACCEPT_DIV').showWithBg();
		$('#euladivpre').html(TMP_EULA.nl2br());
	});
};



var showRegistrationForm = function(){
	$('#EULA_ACCEPT_DIV').hideWithBg();
	$('#REGISTER_G729_FORM_DIV').showWithBg();

	ASTGUI.miscFunctions.createConfig( ASTGUI.globals.g729RegInfo, function(){
		var c = context2json ({ filename: ASTGUI.globals.g729RegInfo , context: REGCONTEXT , usf: 1 });
		if( c === null ){
			var x = new listOfSynActions(ASTGUI.globals.g729RegInfo);
			x.new_action('newcat', REGCONTEXT, '', '');
			x.callActions();
			return;
		}
	
		ASTGUI.updateFieldToValue( 'text_first_name' , c.getProperty('first_name') );
		ASTGUI.updateFieldToValue( 'text_last_name' , c.getProperty('last_name') );
		ASTGUI.updateFieldToValue( 'text_company' , c.getProperty('company') );
		ASTGUI.updateFieldToValue( 'text_address1' , c.getProperty('address1') );
		ASTGUI.updateFieldToValue( 'text_address2' , c.getProperty('address2') );
		ASTGUI.updateFieldToValue( 'text_city' , c.getProperty('city') );
		ASTGUI.updateFieldToValue( 'text_state' , c.getProperty('state') );
		ASTGUI.updateFieldToValue( 'text_post_code' , c.getProperty('post_code') );
		ASTGUI.updateFieldToValue( 'text_country' , c.getProperty('country') );
		ASTGUI.updateFieldToValue( 'text_phone' , c.getProperty('phone') );
		ASTGUI.updateFieldToValue( 'text_email' , c.getProperty('email') );
	});
};
