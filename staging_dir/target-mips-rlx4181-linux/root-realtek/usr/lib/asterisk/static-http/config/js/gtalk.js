var GTALK_CNF , JABBER_CNF, EXTENSIONS_CNF;

var EDIT_BUDDY ;
var EDIT_ACCOUNT ;


var MANAGE_BUDDIES = {
	listBuddies : function(){ // MANAGE_BUDDIES.listBuddies
		var addCell = ASTGUI.domActions.tr_addCell; // temporarily store the function
		var TBL = _$('table_BuddiesList');
		ASTGUI.domActions.clear_table(TBL);


		for( buddy in GTALK_CNF ){
			if( !GTALK_CNF.hasOwnProperty(buddy) || buddy == 'general' ) continue;

			var newRow = TBL.insertRow(-1);
			newRow.className = ((TBL.rows.length)%2==1)?'odd':'even';
			addCell( newRow , { html:GTALK_CNF[buddy].username , align:'left'});

			var tmp_context = GTALK_CNF[buddy].context || '' ;

			var dest_line = ( tmp_context && tmp_context.contains(ASTGUI.contexts.gtalkIncomingContext) && EXTENSIONS_CNF.hasOwnProperty(tmp_context) ) ? EXTENSIONS_CNF[tmp_context][0] : '' ;
			var dest_args = ASTGUI.parseContextLine.getArgs(dest_line) ;
			addCell( newRow , { html: ASTGUI.parseContextLine.toKnownContext(dest_args) , align:'left' } );


			addCell( newRow , { html:GTALK_CNF[buddy].connection });

			var tmp = "<span class='guiButton' onclick=\"MANAGE_BUDDIES.edit_buddy_form('" + buddy +"')\">Edit</span>&nbsp;"
					+ "<span class='guiButtonDelete' onclick=\"MANAGE_BUDDIES.deleteBuddy('" + buddy +"')\">Delete</span>" ;
			addCell( newRow , { html: tmp });
		}

		if( TBL.rows.length == 0 ){
			var newRow = TBL.insertRow(-1);
			addCell( newRow , { html: "<BR>No peers configured. To send or receive calls from your friends on google talk, Add the peer name by clicking on the 'New peer' button.<BR><BR>" });
		}else{
			var newRow = TBL.insertRow(0);
			newRow.className = 'frow' ;
			addCell( newRow , { html: 'UserName' , align: 'left' });
			addCell( newRow , { html: 'Incoming Calls', align: 'left' });
			addCell( newRow , { html: 'Connection' });
			addCell( newRow , { html: '' });
		}
	},

	deleteBuddy : function(a , silentmode){ // MANAGE_BUDDIES.deleteBuddy(); use silentmode to delete while editing buddy
		if(!silentmode){
			if( !confirm("Delete peer '"+ a + "' ?") ) return true;
		}

		var u = new listOfSynActions('gtalk.conf');
			u.new_action('delcat', a, '', '');
		u.callActions();

		u.clearActions('extensions.conf');
			u.new_action('delcat', GTALK_CNF[a].context , '', '');
		u.callActions();

		if(!silentmode){
			ASTGUI.feedback( { msg:"deleted peer '" + a + "'" , showfor: 3, color:'red', bgcolor:'#FFFFFF' } );
			window.location.reload();
		}
	},

	addBuddy: function(){ // MANAGE_BUDDIES.addBuddy();

		if ( EDIT_BUDDY ){
			this.deleteBuddy(EDIT_BUDDY , true);
		}

		var v = new listOfActions('gtalk.conf');
		var tmp_uname = ASTGUI.getFieldValue('edit_buddyName_text');
		var tmp_connection = ASTGUI.getFieldValue('edit_buddyConnection_select');
		if( !tmp_uname.contains('@') ){ 
			tmp_uname = tmp_uname + '@gmail.com';
		}
		var catname = ( EDIT_BUDDY ) ? EDIT_BUDDY : tmp_uname.beforeChar('@');

		v.new_action('newcat', catname, '', '');
		v.new_action('append', catname , 'username', tmp_uname );
		v.new_action('append', catname , 'disallow', 'all');
		v.new_action('append', catname , 'allow', 'all');
		v.new_action('append', catname , 'context', ASTGUI.contexts.gtalkIncomingContext + catname );

		v.new_action('append', catname , 'connection', tmp_connection );
		v.callActions( function(){

			var W = new listOfSynActions('extensions.conf') ;
			W.new_action('newcat', ASTGUI.contexts.gtalkIncomingContext + catname , '', '');
			W.new_action('append', ASTGUI.contexts.gtalkIncomingContext + catname , 'exten', 's,1,' + ASTGUI.getFieldValue('edit_buddyIncomingCalls_select') );
			W.callActions();

			ASTGUI.feedback( { msg:"updated peer" , showfor: 3, color:'red', bgcolor:'#FFFFFF' } );
			window.location.reload();
		});
	},

	new_buddy_form : function(){ // MANAGE_BUDDIES.new_buddy_form();
		EDIT_BUDDY = '';
		ASTGUI.resetTheseFields([ 'edit_buddyName_text', 'edit_buddyConnection_select', 'edit_buddyIncomingCalls_select']);
		$('#buddy_editdiv .dialog_title > span').html('Add Peer');
		$('#buddy_editdiv').showWithBg();
	},

	edit_buddy_form : function(a){ // MANAGE_BUDDIES.edit_buddy_form();
		if(!a) return;
		EDIT_BUDDY = a;
		ASTGUI.updateFieldToValue( 'edit_buddyName_text',  GTALK_CNF[EDIT_BUDDY].username );
		ASTGUI.updateFieldToValue( 'edit_buddyConnection_select', GTALK_CNF[EDIT_BUDDY].connection );

		var dest_line = ( EXTENSIONS_CNF.hasOwnProperty( GTALK_CNF[EDIT_BUDDY].context ) ) ? EXTENSIONS_CNF[ GTALK_CNF[EDIT_BUDDY].context ][0] : '' ;
		ASTGUI.selectbox.selectDestinationOption( 'edit_buddyIncomingCalls_select' , ASTGUI.parseContextLine.getAppWithArgs(dest_line) );

		$('#buddy_editdiv .dialog_title > span').html( 'Edit Peer ' + EDIT_BUDDY);
		$('#buddy_editdiv').showWithBg();
	}
};

var MANAGE_ACCOUNTS = {
	listAccounts : function(){
		var addCell = ASTGUI.domActions.tr_addCell; // temporarily store the function
		var TBL = _$('table_AccountsList');
		ASTGUI.domActions.clear_table(TBL);

		for( account in JABBER_CNF ){
			if( !JABBER_CNF.hasOwnProperty(account) || account == 'general' ) continue;
			ASTGUI.selectbox.append('edit_buddyConnection_select', account , account);

			var newRow = TBL.insertRow(-1);
			newRow.className = ((TBL.rows.length)%2==1) ? 'odd':'even';
			addCell( newRow , { html: JABBER_CNF[account].username, align: 'left' });
			addCell( newRow , { html: account, align: 'left' });
				var tmp = "<span class='guiButton' onclick=\"MANAGE_ACCOUNTS.edit_Account_form('" + account +"')\">Edit</span>&nbsp;"
					+ "<span class='guiButtonDelete' onclick=\"MANAGE_ACCOUNTS.deleteAccount('" + account +"')\">Delete</span>" ;
			addCell( newRow , { html: tmp, align: 'center' });
		}

		if( TBL.rows.length == 0 ){
			var newRow = TBL.insertRow(-1);
			addCell( newRow , { html: "<BR>No google talk accounts configured. <BR> Please click on 'New gtalk Account' button to send and receive calls via your google talk account.<BR><BR>"});
		}else{
			var newRow = TBL.insertRow(0);
			newRow.className = 'frow' ;
			addCell( newRow , { html: 'UserName' , align: 'left' });
			addCell( newRow , { html: 'Account', align: 'left' });
			addCell( newRow , { html: '' });
		}
	},

	deleteAccount : function(a, silentmode){ // MANAGE_ACCOUNTS.deleteAccount()
		if(!silentmode && !confirm("Delete account '"+ a + "' ?")) { return true; }
		var u = new listOfSynActions('jabber.conf') ;
		u.new_action('delcat', a, '', '');
		u.callActions();
		if( !silentmode ){
			ASTGUI.feedback({ msg:"Deleted jabber account '" + a + "'", showfor: 3, color:'red', bgcolor:'#FFFFFF' });
			window.location.reload();
		}
	},

	saveAccount : function(){ // MANAGE_ACCOUNTS.saveAccount()
		if ( EDIT_ACCOUNT ){
			this.deleteAccount(EDIT_ACCOUNT, true);
		}

		var v = new listOfActions('jabber.conf');
		var tmp_uname = ASTGUI.getFieldValue('edit_account_text');
		if( !tmp_uname.contains('@') ){
			tmp_uname = tmp_uname + '@gmail.com';
		}
		var catname = ( EDIT_ACCOUNT ) ? EDIT_ACCOUNT : tmp_uname.beforeChar('@');
		v.new_action('newcat', catname, '', '');
		v.new_action('append', catname , 'type', 'client');
		v.new_action('append', catname , 'serverhost', 'talk.google.com');
		v.new_action('append', catname , 'username', tmp_uname);
		v.new_action('append', catname , 'secret', ASTGUI.getFieldValue('edit_account_secret'));
		v.new_action('append', catname , 'port', '5222');
		v.new_action('append', catname , 'usetls', 'yes');
		v.new_action('append', catname , 'usesasl', 'yes');
		v.new_action('append', catname , 'statusmessage', ASTGUI.getFieldValue('edit_account_status'));
		v.new_action('append', catname , 'timeout', '100');
		v.callActions( function(){
			ASTGUI.feedback( { msg:"updated account" , showfor: 3, color:'red', bgcolor:'#FFFFFF' } );
			window.location.reload();
		});

	},

	new_Account_form : function(){ // MANAGE_ACCOUNTS.new_Account_form()
		EDIT_ACCOUNT = '';
		ASTGUI.resetTheseFields([ 'edit_account_text', 'edit_account_secret','edit_account_status' ]);
		$('#account_editdiv .dialog_title > span').html('Add new Account');
		$('#account_editdiv').showWithBg();
	},

	edit_Account_form : function(a){ // MANAGE_ACCOUNTS.edit_Account_form()
		if(!a) return;
		EDIT_ACCOUNT = a;

		ASTGUI.updateFieldToValue( 'edit_account_text',  JABBER_CNF[EDIT_ACCOUNT].username );
		ASTGUI.updateFieldToValue( 'edit_account_secret',  JABBER_CNF[EDIT_ACCOUNT].secret );
		ASTGUI.updateFieldToValue( 'edit_account_status',  JABBER_CNF[EDIT_ACCOUNT].statusmessage );

		$('#account_editdiv .dialog_title > span').html('Edit Account ' + EDIT_ACCOUNT );
		$('#account_editdiv').showWithBg();

	}

};



var localajaxinit = function(){
	top.document.title = 'Google Talk Preferences' ;
	GTALK_CNF = config2json({ filename:'gtalk.conf', usf:1 }); // buddies
	JABBER_CNF = config2json({ filename:'jabber.conf', usf:1 }); // accounts
	EXTENSIONS_CNF = config2json({ filename:'extensions.conf', usf:0 });

	var someArray = parent.miscFunctions.getAllDestinations() ; 
	ASTGUI.selectbox.populateArray('edit_buddyIncomingCalls_select', someArray);

	(function(){
		// check if the general section of the config files are configured
		var u = new listOfSynActions('gtalk.conf') ;
		if( !GTALK_CNF.hasOwnProperty('general') ){
			u.new_action('newcat', 'general' , '', '');
		}
		if( GTALK_CNF.hasOwnProperty('general') && !GTALK_CNF['general'].hasOwnProperty('allowguest') ){
			u.new_action('append', 'general' , 'allowguest', 'no');
		}

		var v = new listOfSynActions('jabber.conf') ;
		if( !JABBER_CNF.hasOwnProperty('general') ){
			v.new_action('newcat', 'general' , '', '');
		}
		if( JABBER_CNF.hasOwnProperty('general') && !JABBER_CNF['general'].hasOwnProperty('autoprune') ){
			v.new_action('append', 'general' , 'autoprune', 'no');
		}
		if( JABBER_CNF.hasOwnProperty('general') && !JABBER_CNF['general'].hasOwnProperty('autoregister') ){
			v.new_action('append', 'general' , 'autoregister', 'yes');
		}

		if( u.actionCount || v.actionCount ) {
			u.callActions();
			v.callActions();
			window.location.reload();
		}
	})();

	(function(){
		var t = [{	url:'#',
				desc:'Google Talk Accounts',
				click_function: function(){
					$('#table_BuddiesList_DIV').hide();
					$('#table_AccountsList_DIV').show();
				}
			},{	url: '#',
				desc: '&nbsp;&nbsp;&nbsp;Peers&nbsp;&nbsp;&nbsp;',
				click_function: function(){
					$('#table_BuddiesList_DIV').show();
					$('#table_AccountsList_DIV').hide();
				}
			}];

		ASTGUI.tabbedOptions( _$('tabbedMenu') , t );

		$('#tabbedMenu').find('A:eq(0)').click();
	})();

	MANAGE_ACCOUNTS.listAccounts();
	MANAGE_BUDDIES.listBuddies();
};
