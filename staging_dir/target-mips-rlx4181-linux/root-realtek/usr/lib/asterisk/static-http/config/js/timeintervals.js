/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * timeintervals.html functions
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
var isNewTI ;
var EDIT_TI ;
var TI_LIST = {};
var RMP_TBR_timeIntervals = ['12:00 AM', '12:15 AM', '12:30 AM', '12:45 AM', '01:00 AM', '01:15 AM', '01:30 AM', '01:45 AM', '02:00 AM', '02:15 AM', '02:30 AM', '02:45 AM', '03:00 AM', '03:15 AM', '03:30 AM', '03:45 AM', '04:00 AM', '04:15 AM', '04:30 AM', '04:45 AM', '05:00 AM', '05:15 AM', '05:30 AM', '05:45 AM', '06:00 AM', '06:15 AM', '06:30 AM', '06:45 AM', '07:00 AM', '07:15 AM', '07:30 AM', '07:45 AM', '08:00 AM', '08:15 AM', '08:30 AM', '08:45 AM', '09:00 AM', '09:15 AM', '09:30 AM', '09:45 AM', '10:00 AM', '10:15 AM', '10:30 AM', '10:45 AM', '11:00 AM', '11:15 AM', '11:30 AM', '11:45 AM', '12:00 PM', '12:15 PM', '12:30 PM', '12:45 PM', '01:00 PM', '01:15 PM', '01:30 PM', '01:45 PM', '02:00 PM', '02:15 PM', '02:30 PM', '02:45 PM', '03:00 PM', '03:15 PM', '03:30 PM', '03:45 PM', '04:00 PM', '04:15 PM', '04:30 PM', '04:45 PM', '05:00 PM', '05:15 PM', '05:30 PM', '05:45 PM', '06:00 PM', '06:15 PM', '06:30 PM', '06:45 PM', '07:00 PM', '07:15 PM', '07:30 PM', '07:45 PM', '08:00 PM', '08:15 PM', '08:30 PM', '08:45 PM', '09:00 PM', '09:15 PM', '09:30 PM', '09:45 PM', '10:00 PM', '10:15 PM', '10:30 PM', '10:45 PM', '11:00 PM', '11:15 PM', '11:30 PM', '11:45 PM'] ;

// ASTGUI.contexts.TimeIntervalPrefix
var ti_miscFunctions = {
	check_ifDatesAreValid : function( dt ){  // ti_miscFunctions.check_ifDatesAreValid()
		// return true if dt is a valid date string like '05' or '12-25' or '02-18' etc, otherwise return false
		dt = String(dt) ;
		var valid_values = [];
		for(var r=1; r<32; r++) valid_values.push(r);
		var isValidVal = function(a){
			if ( isNaN(a) ) return false;
			if ( valid_values.contains(Number(a)) ) return true;
			return false;
		}

		if( dt.contains('-') ){
			return isValidVal(dt.split('-')[0]) && isValidVal(dt.split('-')[1]);
		}else{
			return isValidVal(dt);
		}
	},

	update_TI : function(){ // ti_miscFunctions.update_TI();
		var newname = $('#edit_ti_name').val();

		if (newname === '') {
			ASTGUI.highlightField('edit_ti_name', 'Time Interval name is empty');
		}

		for (var ti in TI_LIST) {
			if (!TI_LIST.hasOwnProperty(ti)) {
				continue;
			}

			if (isNewTI && ti.toLowerCase() === newname.toLowerCase()) {
				ASTGUI.highlightField('edit_ti_name', 'A Time Interval already exists by this name!');
				return;
			}

			if (!isNewTI && EDIT_TI.toLowerCase() !== newname.toLowerCase() && ti.toLowerCase() === newname.toLowerCase()) {
				ASTGUI.highlightField('edit_ti_name', 'A Time Interval already exists by this name!');
				return;
			}
		}

		var interval = {};
		interval.time = '*';
		interval.weekdays = '*';
		interval.days = '*';
		interval.months = '*';

		/* if 'Entire Day' isn't checked */
		if (!$('#edit_ti_entireday:checked').length) {
			var start = $('#edit_ti_starttime').val();
			var end = $('#edit_ti_endtime').val();

			var starttime = ASTGUI.miscFunctions.AMPM_to_asteriskTime(start) || '00:00';
			var endtime = ASTGUI.miscFunctions.AMPM_to_asteriskTime(end) || '24:00';

			if (starttime !== '00:00' || endtime !== '24:00') {
				interval.time = starttime + '-' + endtime;
			}
		}

		if ($('#ti_type_byDayofWeek:checked').val()) {
			interval.weekdays = $('#edit_ti_dayofweek_start').val() + '-' + $('#edit_ti_dayofweek_end').val();
		}

		if ($('#ti_type_byGroupofDates:checked').val()) {
			interval.days = $('#edit_ti_from_date').val();

			interval.days = interval.days.toString();
			if (!interval.days.valiDate()) {
				ASTGUI.highlightField('edit_ti_from_date', 'Invalid Date Range!');
				return;
			}

			interval.months = $('#edit_ti_month').val();
		}

		if (isNewTI) {
			var resp = parent.pbx.time_intervals.add(newname, interval);
		} else {
			var resp = parent.pbx.time_intervals.edit(EDIT_TI, newname, interval);
		}

		if (!resp) {
			top.log.error('Error updating Time Interval');
			ASTGUI.feedback({ msg: 'ERROR UPDATING!', showfor: 3, color: 'red', bgcolor: '#ffffff'});
			return;
		}

		ASTGUI.feedback({ msg: "Time Interval '" + newname + "' " + ((isNewTI) ? "created" : "edited") + "!", showfor: 3, color:'green', bgcolor:'#FFFFFF'});
		window.location.reload();

	},

	delete_TI : function(a){ // ti_miscFunctions.delete_TI
		if( !confirm("Delete Time Interval '" + a + "' ?") ) { return true; }
		var t = ASTGUI.contexts.TimeIntervalPrefix + a ;

		var u = new listOfActions('extensions.conf');
		// delete the time interval definition
		u.new_action('delete' , 'globals' , t , '' , '') ;
		var EXT_CNF = config2json({filename:'extensions.conf', usf:0 }) ; 
		for( var ct in EXT_CNF){ if( EXT_CNF.hasOwnProperty(ct) ){
			// delete any lines in any [DID_Trunkx] that are like "include = DID_trunkx_timeinterval_'a', ..."
			if( ct.beginsWith(ASTGUI.contexts.TrunkDIDPrefix) && !ct.contains(ASTGUI.contexts.TimeIntervalPrefix) ){
				var this_trunk_mainDID = EXT_CNF[ct] ;
				this_trunk_mainDID.each(function(this_line){
					if( this_line.beginsWith('include=') && this_line.contains(ASTGUI.contexts.TrunkDIDPrefix) && this_line.contains( ASTGUI.contexts.TimeIntervalPrefix + a + parent.session.delimiter + '${' ) ){
						u.new_action( 'delete' , ct , 'include' , '' , this_line.afterChar('=') ) ;
					}
				});
			}
			// delete any contexts that are like [DID_trunkx_timeinterval_a]
			if( ct.beginsWith(ASTGUI.contexts.TrunkDIDPrefix) && ct.endsWith(ASTGUI.contexts.TimeIntervalPrefix + a ) ){
				u.new_action('delcat', ct, "", "");
			}
		}}

		u.callActions(function(){
			ASTGUI.feedback( { msg:"Time Interval '"+ a + "' deleted !", showfor: 3, color:'red', bgcolor:'#FFFFFF' } );
			window.location.reload();
		});
	},

/*
		sessionData.pbxinfo['timebasedRules'][timebasedrule-custom-2] = {
			label : 'LabelForThisRule',
			matches : [ '00:00-23:59,*,25,dec', '00:00-23:59,*,1,jan', '00:00-23:59,*,4,jul' ], // by a set of Dates
				OR
			matches : [ '00:00-23:59,sun-sat,*,*'], // - by Day of Week, matches.length == 1
			ifMatched : 'voicemenu-custom-1,s,1',
			ifNotMatched : 'default,6000,1'
		}
*/

	edit_TI_form : function(a){ // ti_miscFunctions.show_TI_form
		isNewTI = false;
		EDIT_TI = a ;
		ASTGUI.feedback( { msg: 'Edit Time Interval !', showfor: 2 , color: 'green', bgcolor: '#FFFFFF' } );
		_$('div_ti_edit_title').innerHTML = "Edit Time Interval '" + a + "'" ;
		var ti = TI_LIST[a] ;
		ASTGUI.updateFieldToValue( 'edit_ti_name', a ); // name of time interval
		ASTGUI.resetTheseFields( ['edit_ti_starttime', 'edit_ti_endtime', 'edit_ti_dayofweek_start', 'edit_ti_dayofweek_end', 'edit_ti_from_date', 'edit_ti_month' ] );

		var PIECES = TI_LIST[a].contains(',') ? TI_LIST[a].split(',') : TI_LIST[a].split('|');
		if( PIECES[0] != '*' ){
			_$('edit_ti_entireday').checked = false ;
			ASTGUI.updateFieldToValue( 'edit_ti_starttime', ASTGUI.miscFunctions.asteriskTime_to_AMPM(PIECES[0].split('-')[0] ) );
			ASTGUI.updateFieldToValue( 'edit_ti_endtime', ASTGUI.miscFunctions.asteriskTime_to_AMPM( PIECES[0].split('-')[1]) );
		}else{
			_$('edit_ti_entireday').checked = true ;
		}

		if( PIECES[2] == '*' && PIECES[3] == '*' ){ // by week days
			_$('ti_type_byDayofWeek').checked = true;
			_$('ti_type_byGroupofDates').checked = false;
			ASTGUI.updateFieldToValue( 'edit_ti_dayofweek_start', PIECES[1].split('-')[0] );
			if( PIECES[1].contains('-') ){ // range of week days
				ASTGUI.updateFieldToValue( 'edit_ti_dayofweek_end', PIECES[1].split('-')[1] );
			}
		}else{ // by days of month
			_$('ti_type_byDayofWeek').checked = false;
			_$('ti_type_byGroupofDates').checked = true;
			ASTGUI.updateFieldToValue( 'edit_ti_from_date',  PIECES[2] );
			ASTGUI.updateFieldToValue( 'edit_ti_month',  PIECES[3] );
		}
		_$('ti_type_byDayofWeek').updateStatus();  _$('ti_type_byGroupofDates').updateStatus();  _$('edit_ti_entireday').updateStatus();
		$('#div_ti_edit').showWithBg();
	},

	new_TI_form : function(){ // ti_miscFunctions.new_TI_form
		isNewTI = true;
		EDIT_TI = '' ;
		_$('div_ti_edit_title').innerHTML = 'New Time Interval';
		ASTGUI.feedback( { msg: 'Create New Time Interval !', showfor: 2 , color: 'green', bgcolor: '#FFFFFF' } );
		ASTGUI.resetTheseFields( ['edit_ti_name','edit_ti_dayofweek_start', 'edit_ti_dayofweek_end', 'edit_ti_from_date', 'edit_ti_month' , 'edit_ti_entireday' , 'edit_ti_starttime' , 'edit_ti_endtime' ] );
		/*
			Time Interval Name: id='edit_ti_name'
			radio : id="ti_type_byDayofWeek"
			select id='edit_ti_dayofweek_start'
			select id='edit_ti_dayofweek_end'
			radio: id="ti_type_byGroupofDates"
			From Date : id="edit_ti_from_date"
			To Date : id="edit_ti_to_date"
			Time:
			checkbox id='edit_ti_entireday'
			Start Time : id="edit_ti_starttime"
			End Time : id="edit_ti_endtime"
		*/
		$('#div_ti_edit').showWithBg();
	},

	showTable : function(){ // ti_miscFunctions.showTable
		ASTGUI.domActions.clear_table(_$('table_tilist'));
		var newRow = _$('table_tilist').insertRow(-1);
		newRow.className = 'frow';
		ASTGUI.domActions.tr_addCell( newRow , { html: 'Time Interval Name' } );
		ASTGUI.domActions.tr_addCell( newRow , { html: 'When' } );
		ASTGUI.domActions.tr_addCell( newRow , { html: '' } );
		for( var i in TI_LIST ){ if( TI_LIST.hasOwnProperty(i) ) {
			var tmp = "<span class='guiButton' onclick=\"ti_miscFunctions.edit_TI_form('" + i +"')\">Edit</span>&nbsp;"
					+ "<span class='guiButtonDelete' onclick=\"ti_miscFunctions.delete_TI('" + i +"')\">Delete</span>" ;
			var newRow = _$('table_tilist').insertRow(-1);
			newRow.className = ((_$('table_tilist').rows.length)%2==1)?'odd':'even';
			ASTGUI.domActions.tr_addCell( newRow , { html: i } );
			ASTGUI.domActions.tr_addCell( newRow , { html: ASTGUI.miscFunctions.GotoIftime_in_humanReadable( TI_LIST[i] ) } );
			ASTGUI.domActions.tr_addCell( newRow , { html: tmp , align: 'center' } );

		}}
	}
};
