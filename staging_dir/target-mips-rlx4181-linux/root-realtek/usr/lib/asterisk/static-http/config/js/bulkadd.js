/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * Bulkadd functions
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
var newusers_list = [];
var NEW_USERS = {} ;

var localajaxinit = function(){
	ASTGUI.selectbox.populateArray('RANGE_Number', [5,10,15,20,25,30,35,40,45,50]);
	top.document.title = 'Bulk Add' ;

	(function(){
		var t = [{	url:'#',
				desc:'Create New users from CSV list',
				click_function: function(){
					$('#table_usersFromCSV').show();
					$('#table_usersFromRange').hide();
				}
			},{	url: '#',
				desc: 'Create a Range of new users',
				click_function: function(){
					$('#table_usersFromCSV').hide();
					$('#table_usersFromRange').show();
				}
			}];
		ASTGUI.tabbedOptions( _$('tabbedMenu') , t );
	})();

	$('#tabbedMenu').find('A:eq(0)').click();
};



var create_NEW_USERS = function(){
	var addUser_OR_reloadGUI = function(){
		var tmp_first_user = newusers_list[0];
		parent.pbx.users.add( tmp_first_user , NEW_USERS[tmp_first_user] , function(){
			newusers_list.splice(0,1);
			if( newusers_list.length ){
				addUser_OR_reloadGUI();
			}else{
				alert( 'Users added \n Click Ok to reload GUI' );
				top.window.location.reload();
			}
		});
	};

	ASTGUI.dialog.waitWhile( "Creating list of users ");
	addUser_OR_reloadGUI();
};



var addUsers_from_CSV_field = function(){
	NEW_USERS = {} ;
	newusers_list = [];

	var csv_text = _$('ta_ba_csv').value ;
	var tmp_lines = csv_text.split('\n');
	var tmp_Heads = tmp_lines[0]; tmp_lines.splice(0, 1);
	var HEADS = tmp_Heads.split(',');
	if( HEADS[0] != 'User' ){
		alert('The first column should be User');
		return;
	}

	for(var tli =0; tli< tmp_lines.length ; tli++ ){
		var this_line = tmp_lines[tli];
		if( this_line.trim() == '' ){ continue; }
		var this_user_details = this_line.split(',');
		if( this_user_details.length != HEADS.length ){
			alert('Error: Invalid Number of Fields in line ' + (tli + 1) );
			return;
		}

		var this_user = this_user_details[0]; // User
		if( parent.miscFunctions.ifExtensionAlreadyExists(this_user) ){
			alert('Error: Duplicate Extension \n\n '+ ' Extension ' + this_user + ' already exists \n' + ' Please change extension in line ' + (tli + 1) );
			return;
		}

		newusers_list.push( this_user ) ;
		NEW_USERS[ this_user ] = {} ;
		for( var f = 1 ; f < HEADS.length ; f++ ){
			NEW_USERS[ this_user ][ HEADS[f] ] = this_user_details[ f ] ;
		}
	};

	create_NEW_USERS();
};


var add_RangeOfUsers = function(){
	NEW_USERS = {} ;
	newusers_list = [];

	var t = Number( ASTGUI.getFieldValue('RANGE_Number') );
	var tmp_user = Number(ASTGUI.getFieldValue('RANGE_Start'));

	while(t){
		var tmp_nu = String(tmp_user);
		if( parent.miscFunctions.ifExtensionAlreadyExists(tmp_nu) ){
			tmp_user++ ;
			continue;
		}

		newusers_list.push(tmp_nu);
			top.log.debug( 'adding user ' + tmp_nu);
			NEW_USERS[ tmp_nu ] = {} ;
			NEW_USERS[ tmp_nu ]['fullname'] = 'User ' + tmp_nu;
			NEW_USERS[ tmp_nu ]['cid_number'] = tmp_nu;
			NEW_USERS[ tmp_nu ]['context'] = '';
			NEW_USERS[ tmp_nu ]['hasvoicemail'] = 'yes';
			NEW_USERS[ tmp_nu ]['vmsecret'] = tmp_nu;
			NEW_USERS[ tmp_nu ]['hassip'] = 'yes';
			NEW_USERS[ tmp_nu ]['hasiax'] = 'yes';
			NEW_USERS[ tmp_nu ]['secret'] = tmp_nu;
		tmp_user++ ;
		t--;
	}

	create_NEW_USERS();
};
