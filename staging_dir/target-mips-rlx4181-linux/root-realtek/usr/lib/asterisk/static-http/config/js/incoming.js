/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * incoming.html functions
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
var STRING_SEPERATOR = '#######' ; // will be used to store 'actual context line + contextname' as a DOM attribute, will not be written to any file
var isNewIR = false;
var EDIT_CONTEXT_IR = '';
var EDIT_CONTEXT_IR_LINE = '';
var EX_CF;

var incomingRules_MiscFunctions = {
//	newRow.ACTUAL_LINE__CONTEXT = did_this_rule + STRING_SEPERATOR + ASTGUI.contexts.TrunkDIDPrefix + this_trunk ;

	moveUpDown_In_context: function(context, rule , updown ){ 	// var incomingRules_MiscFunctions.moveUpDown_In_context( ct , rule , bool )
		parent.ASTGUI.dialog.waitWhile('Updating Incoming Rule ...');
		ASTGUI.miscFunctions.moveUpDown_In_context( context , rule , updown , function(newcontext){
			parent.ASTGUI.dialog.hide();
			ASTGUI.feedback( { msg: 'updated !', showfor: 2 , color: 'green', bgcolor: '#FFFFFF' } );
			window.location.reload();
		});
	},

	edit_IR : function(s){
		isNewIR = false ;
		var k = s.parentNode;
		while( k.tagName != 'TR' ){ k = k.parentNode; }

		if ( jQuery.browser.msie ){
			var ALC = $(k).attr('ACTUAL_LINE__CONTEXT') || '';
		}else{
			var ALC = ( k.hasOwnProperty('ACTUAL_LINE__CONTEXT') ) ? k.ACTUAL_LINE__CONTEXT : '';
		}

		if( !ALC || !ALC.contains(STRING_SEPERATOR) ) return ;

		EDIT_CONTEXT_IR = ALC.split(STRING_SEPERATOR)[1] ;
		EDIT_CONTEXT_IR_LINE = ALC.split(STRING_SEPERATOR)[0] ;

		if( !EDIT_CONTEXT_IR.contains(ASTGUI.contexts.TimeIntervalPrefix) && EDIT_CONTEXT_IR.endsWith(ASTGUI.contexts.TrunkDefaultSuffix) ){
			var tmp_TRUNK = EDIT_CONTEXT_IR.lChop(ASTGUI.contexts.TrunkDIDPrefix).beforeStr( ASTGUI.contexts.TrunkDefaultSuffix );
		}else{
			var tmp_TRUNK = EDIT_CONTEXT_IR.lChop(ASTGUI.contexts.TrunkDIDPrefix).beforeStr( '_' + ASTGUI.contexts.TimeIntervalPrefix );
		}

		var tmp_edit_itrl_tf = EDIT_CONTEXT_IR.afterStr( ASTGUI.contexts.TimeIntervalPrefix ) ;

		$('.hideOnEdit').hide();
		_$('div_ir_edit_title').innerHTML = 'Edit Incoming Calling Rule, Trunk: ' + tmp_TRUNK + ' , Time Interval: ' + ( tmp_edit_itrl_tf || 'none' ) ;

		ASTGUI.updateFieldToValue( 'edit_itrl_trunk',  tmp_TRUNK ) ;
		ASTGUI.updateFieldToValue( 'edit_itrl_tf', tmp_edit_itrl_tf ) ;
		ASTGUI.updateFieldToValue( 'edit_itrl_pattern', ASTGUI.parseContextLine.getExten( EDIT_CONTEXT_IR_LINE ) );

		$('.localext_byDid').hide();
		if( EDIT_CONTEXT_IR_LINE.contains(',1,Goto(default,${EXTEN') ){
			$('.localext_byDid').show();
			ASTGUI.updateFieldToValue( 'edit_itrl_dest', 'ByDID' );
			var tmp_ldp = EDIT_CONTEXT_IR_LINE.betweenXY('{','}').lChop('EXTEN').lChop(':') ;
			if( !tmp_ldp.length  ){ tmp_ldp = '0'; }
			ASTGUI.updateFieldToValue( 'edit_itrl_LocalDest_Pattern', tmp_ldp );
		}else{
			ASTGUI.updateFieldToValue( 'edit_itrl_dest', ASTGUI.parseContextLine.getAppWithArgs( EDIT_CONTEXT_IR_LINE ) );
		}

		incomingRules_MiscFunctions.enableDisablePattern();
		ASTGUI.feedback( { msg: 'Edit Incoming Rule !', showfor: 2 , color: 'green', bgcolor: '#FFFFFF' } );
		$('#div_ir_edit').showWithBg();
	},

	delete_IR : function(s){ // incomingRules_MiscFunctions.delete_IR()
		//var s = ASTGUI.events.getTarget(event);
		if (!confirm( "Are you sure you want to delete this 'Incoming Rule' ?" ) ) { return; }

		var k = s.parentNode;
		while( k.tagName != 'TR' ){ k = k.parentNode; }

		if ( jQuery.browser.msie ){
			var ALC = $(k).attr('ACTUAL_LINE__CONTEXT') || '';
		}else{
			var ALC = ( k.hasOwnProperty('ACTUAL_LINE__CONTEXT') ) ? k.ACTUAL_LINE__CONTEXT : '';
		}

		if( !ALC || !ALC.contains(STRING_SEPERATOR) ) return ;

		var TMP_CONTEXT = ALC.split(STRING_SEPERATOR)[1] ;
		var TMP_LINE = ALC.split(STRING_SEPERATOR)[0] ;

		var resp = top.pbx.trunks.rules.remove({cxt: TMP_CONTEXT, line: TMP_LINE});
		if (!resp) {
			ASTGUI.feedback({ 
				color: 'red',
				msg: 'Error removing Calling Rule.',
				showfor: 2
			});
		} else {
			ASTGUI.feedback({ 
				color: 'green',
				msg: 'Deleted Incoming Rule.',
				showfor: 2
			});
		}

		window.location.reload();
		return;
	},

	listAllRulesInTable : function(){ // incomingRules_MiscFunctions.listAllRulesInTable();
		EX_CF = config2json({filename:'extensions.conf', usf:0 });
		var t = parent.pbx.trunks.list() || [];

		if ( !t.length ){

			var TBL = document.createElement('TABLE');
				TBL.cellPadding = 0;
				TBL.cellSpacing = 0;
				TBL.border = 0;
				TBL.className = 'table_incomingRulesList';

			var newRow = TBL.insertRow(-1);
			newRow.className = 'even';
			ASTGUI.domActions.tr_addCell( newRow , { html: 'You do not have any trunks created. <BR> You need to have atleast one trunk to be able to manage incoming call rules.', width: '95%' } );

			var tbl_heading = document.createElement('div');
			tbl_heading.style.marginTop = '35px';
			tbl_heading.appendChild(TBL);
			_$('IR_CONTAINER').appendChild(tbl_heading);
			$('.top_buttons').hide();
			return;
		}

		var TMP_FORSORT = [];
		t.each( function(item){
			TMP_FORSORT.push( parent.pbx.trunks.getName(item) + STRING_SEPERATOR +  item);
		});

		TMP_FORSORT.sort();
		TMP_FORSORT.each( function(this_str){
			var a = this_str.split(STRING_SEPERATOR); // a[0] is trunkname , a[1] is trunk 
			var this_trunk = a[1];
			var this_trunk_label = a[0];
			var ttype = parent.pbx.trunks.getType(this_trunk);
			var defaultContext_ContextName = ASTGUI.contexts.TrunkDIDPrefix + this_trunk + ASTGUI.contexts.TrunkDefaultSuffix ;

			ASTGUI.selectbox.append('edit_itrl_trunk', this_trunk_label , this_trunk );

			var TBL = document.createElement('TABLE');
				TBL.cellPadding = 0;
				TBL.cellSpacing = 0;
				TBL.border = 0;
				TBL.className = 'table_incomingRulesList';

			var newRow = TBL.insertRow(-1);
			newRow.className = 'frow';
			ASTGUI.domActions.tr_addCell( newRow , { html: '', width: '25px' } );
			ASTGUI.domActions.tr_addCell( newRow , { html: 'Time Interval' } );
			if( ttype != 'analog' ){
				ASTGUI.domActions.tr_addCell( newRow , { html: 'Pattern' } );
			}
			ASTGUI.domActions.tr_addCell( newRow , { html: 'Destination' } );
			if( ttype != 'analog' ){
				ASTGUI.domActions.tr_addCell( newRow , { html: 'Sort' } );
			}
			ASTGUI.domActions.tr_addCell( newRow , { html: '' } );

			var PREVIOUS_SET_BGClass = 'odd' ; // odd or even

			if( ! EX_CF.hasOwnProperty( ASTGUI.contexts.TrunkDIDPrefix + this_trunk ) ){ return ; }

			EX_CF[ASTGUI.contexts.TrunkDIDPrefix + this_trunk].each( function( did_this_rule , main_did_index ){

				// If is a TimeInterval Context
				if( did_this_rule.beginsWith( 'include=' ) && did_this_rule.contains( '_' + ASTGUI.contexts.TimeIntervalPrefix ) ){
					var THIS_TRUNK_TIMEFRAME_CONTEXT = did_this_rule.betweenXY('=', top.session.delimiter);
					var THIS_TIMEFRAME = THIS_TRUNK_TIMEFRAME_CONTEXT.lChop( ASTGUI.contexts.TrunkDIDPrefix + this_trunk + '_' + ASTGUI.contexts.TimeIntervalPrefix );
					if( ! EX_CF.hasOwnProperty( THIS_TRUNK_TIMEFRAME_CONTEXT ) ){ return ; }

					EX_CF[ THIS_TRUNK_TIMEFRAME_CONTEXT ].each( function( this_rule , this_rule_index ){
						var tmp_exten = ASTGUI.parseContextLine.getExten(this_rule) ;
						if ( tmp_exten == 's' && this_rule.contains('ExecIf') ){ return ;}
						var newRow = TBL.insertRow(-1);
						newRow.ACTUAL_LINE__CONTEXT = this_rule + STRING_SEPERATOR + THIS_TRUNK_TIMEFRAME_CONTEXT ;
						if ( this_rule_index == 0 ){
							PREVIOUS_SET_BGClass = (PREVIOUS_SET_BGClass == 'odd')? 'even' : 'odd' ;
							if( main_did_index != 0 ){
								var tmp_MOVE_TI_UP = "<A href=# title='Move this time Interval context Up'><img src=images/arrow_up.png border=0 onclick=\"incomingRules_MiscFunctions.moveUpDown_In_context('" + ASTGUI.contexts.TrunkDIDPrefix + this_trunk + "','" + did_this_rule + "', 1)\"></A>" ;
								ASTGUI.domActions.tr_addCell( newRow , { html: tmp_MOVE_TI_UP }) ;
							}else{
								ASTGUI.domActions.tr_addCell( newRow , { html: '' }) ;
							}
						}else{
							ASTGUI.domActions.tr_addCell( newRow , { html: '' } );
						}

						newRow.className = PREVIOUS_SET_BGClass ;

						ASTGUI.domActions.tr_addCell( newRow , { html: THIS_TIMEFRAME });
						if( ttype != 'analog' && tmp_exten == 's' ){
							ASTGUI.domActions.tr_addCell( newRow , { html: "'s' (CatchAll)" });
						}else if( ttype != 'analog' && tmp_exten == '_X.' ){
							ASTGUI.domActions.tr_addCell( newRow , { html: "'_X.' (CatchAll)" });
						}else if( ttype != 'analog' ){
							ASTGUI.domActions.tr_addCell( newRow , { html: tmp_exten });
						}

						ASTGUI.domActions.tr_addCell( newRow , { html: ASTGUI.parseContextLine.showAs(this_rule) });

						if( ttype != 'analog' ){
							var tmp_movepriorities = '';
							if( this_rule_index == 0 ){
								tmp_movepriorities = '<img src=images/arrow_blank.png border=0>&nbsp;' ;
							}else{
								tmp_movepriorities = "<A href=#  title='Move this rule Up'><img src=images/arrow_up.png border=0 onclick=\"incomingRules_MiscFunctions.moveUpDown_In_context('" + THIS_TRUNK_TIMEFRAME_CONTEXT + "','" + this_rule+ "', 1)\"></A>&nbsp; " ;
							}

							if( this_rule_index != (EX_CF[ THIS_TRUNK_TIMEFRAME_CONTEXT ].length -1 ) ){
								tmp_movepriorities += "<A href=# title='Move this rule Down'><img src=images/arrow_down.png border=0 onclick=\"incomingRules_MiscFunctions.moveUpDown_In_context('" + THIS_TRUNK_TIMEFRAME_CONTEXT + "','" + this_rule+ "', 0)\"></A>" ;
							}else{
								tmp_movepriorities += '<img src=images/arrow_blank.png border=0>' ;
							}

							ASTGUI.domActions.tr_addCell( newRow , { html: tmp_movepriorities } );
						}

						var tmp_editstr = [] ;
						tmp_editstr[0] = "<span class='guiButton' onclick=\"incomingRules_MiscFunctions.edit_IR(this)\">Edit</span>&nbsp;" ;
						// if( tmp_exten != 's' && tmp_exten != '_X.' ){
							tmp_editstr[1] = "<span class='guiButtonDelete' onclick=\"incomingRules_MiscFunctions.delete_IR(this)\">Delete</span>" ;
						// }
						ASTGUI.domActions.tr_addCell( newRow , { html: tmp_editstr.join(' ') } );
					});

				// Default incoming rules (no time Interval matched)
				}else if(  did_this_rule == 'include=' + defaultContext_ContextName ){

					if( !EX_CF.hasOwnProperty(defaultContext_ContextName) ){ return; }

					var defaultContext = EX_CF[ defaultContext_ContextName ];
					var tmp_defaultContext_length = EX_CF[ defaultContext_ContextName ].length ;

					defaultContext.each( function( this_defaultRule ,  this_defaultRule_Index ){
						if( ! this_defaultRule.beginsWith( 'exten=' ) ){ return ; }// RULES in the main DID_trunk_x
						var tmp_exten = ASTGUI.parseContextLine.getExten(this_defaultRule);
						if ( tmp_exten == 's' && this_defaultRule.contains('ExecIf') ){ return ;}
						var newRow = TBL.insertRow(-1);
						newRow.className = (PREVIOUS_SET_BGClass == 'odd')? 'even' : 'odd' ;
						newRow.ACTUAL_LINE__CONTEXT = this_defaultRule + STRING_SEPERATOR + defaultContext_ContextName ;
						ASTGUI.domActions.tr_addCell( newRow , { html: '' });
						ASTGUI.domActions.tr_addCell( newRow , { html: '<font color=red>none (no TimeIntervals matched)</font>' });
						if( ttype != 'analog' ){

							if( tmp_exten == 's' ){
								ASTGUI.domActions.tr_addCell( newRow , { html: "'s' (CatchAll)"});
							}else if ( tmp_exten == '_X.' ){
								ASTGUI.domActions.tr_addCell( newRow , { html: "'_X.' (CatchAll)"});
							}else{
								ASTGUI.domActions.tr_addCell( newRow , { html: tmp_exten });
							}
							
						}

						ASTGUI.domActions.tr_addCell( newRow , { html: ASTGUI.parseContextLine.showAs(this_defaultRule) });

						if( ttype != 'analog' ){
							var tmp_movepriorities = '' ;

							if( this_defaultRule_Index != 0 ){
								tmp_movepriorities += "<A href=#  title='Move this rule Up'><img src=images/arrow_up.png border=0 onclick=\"incomingRules_MiscFunctions.moveUpDown_In_context('" + defaultContext_ContextName + "','" + this_defaultRule + "', 1)\"></A>&nbsp; " ;
							}else{
								tmp_movepriorities += '<img src=images/arrow_blank.png border=0>&nbsp;' ;
							}

							if( this_defaultRule_Index != (tmp_defaultContext_length - 1) ){
								tmp_movepriorities += "<A href=# title='Move this rule Down'><img src=images/arrow_down.png border=0 onclick=\"incomingRules_MiscFunctions.moveUpDown_In_context('" + defaultContext_ContextName + "','" + this_defaultRule + "', 0)\"></A>" ;
							}else{
								tmp_movepriorities += '<img src=images/arrow_blank.png border=0>&nbsp;' ;
							}

							ASTGUI.domActions.tr_addCell( newRow , { html: tmp_movepriorities } );
						}

						var tmp_editstr = [] ;
						tmp_editstr[0] = "<span class='guiButton' onclick=\"incomingRules_MiscFunctions.edit_IR(this)\">Edit</span>&nbsp;" ;
						tmp_editstr[1] = "<span class='guiButtonDelete' onclick=\"incomingRules_MiscFunctions.delete_IR(this)\">Delete</span>" ;
						ASTGUI.domActions.tr_addCell( newRow , { html: tmp_editstr.join(' ') });
					});
				}else{

				}
			});

			var tbl_heading = document.createElement('div');
			tbl_heading.style.marginTop = '15px';
			tbl_heading.innerHTML = '<B>Trunk - ' + this_trunk_label + '</B>';
			_$('IR_CONTAINER').appendChild(tbl_heading);
			_$('IR_CONTAINER').appendChild(TBL);

		});
	},
	
	new_IR_form: function(){
		isNewIR = true ;
		ASTGUI.resetTheseFields(['edit_itrl_trunk', 'edit_itrl_tf', 'edit_itrl_pattern', 'edit_itrl_dest' ]);
		incomingRules_MiscFunctions.enableDisablePattern();
		$('.localext_byDid').hide();
 		_$('div_ir_edit_title').innerHTML = 'New Incoming Rule';
		ASTGUI.feedback( { msg: 'New Incoming Rule !', showfor: 2 , color: 'green', bgcolor: '#FFFFFF' } );
		$('.hideOnEdit').show();
		$('#div_ir_edit').showWithBg();
	},


	enableDisablePattern:  function(){ // incomingRules_MiscFunctions.enableDisablePattern() ;
		var tn = _$('edit_itrl_trunk').value ;
		if (!tn){
			_$('edit_itrl_pattern').disabled = false;
			return ;
		}
		var ttype = parent.pbx.trunks.getType(tn);
		_$('edit_itrl_pattern').disabled = (ttype == 'analog') ? true : false;
		if( ttype == 'analog' ){
			_$('edit_itrl_pattern').value = 's'  ;
		}
	},

	update_IR : function(){ // incomingRules_MiscFunctions.update_IR() ;

		if( !ASTGUI.checkRequiredFields([ 'edit_itrl_trunk', 'edit_itrl_dest', 'edit_itrl_pattern' ]) ){
			return ;
		}

		if( _$('edit_itrl_tf').selectedIndex == -1 ){
			ASTGUI.feedback ({ msg: 'Please select a Time Interval !' , showfor:2,  color:'#a02920' });
			return ;
		}
		if( !ASTGUI.validateFields([ 'edit_itrl_pattern' ]) ){
			return ;
		}

		var this_trunk = ASTGUI.getFieldValue('edit_itrl_trunk');
		var this_tiName = ASTGUI.getFieldValue('edit_itrl_tf') ;
		var TMP_NEW_PATTERN = ASTGUI.getFieldValue('edit_itrl_pattern');
		var dest = $('#edit_itrl_dest').val();
		var local_dest = ASTGUI.getFieldValue('edit_itrl_LocalDest_Pattern');
		var time_int_name = $('#edit_itrl_tf option:selected').text();
		time_int_name = (time_int_name.contains('no Time Intervals')) ? '' : time_int_name;

		if( ASTGUI.getFieldValue('edit_itrl_dest') == 'ByDID' && parent.pbx.trunks.getType(this_trunk) == 'analog' ){
			ASTGUI.feedback ({ msg: 'Local Extension by DID is not applicable for Analog Trunks !' , showfor:3,  color:'red' });
			return ;
		}

		if( isNewIR == true ){ // create new Incoming Rule
			parent.ASTGUI.dialog.waitWhile('Creating Incoming Rule ...');

			if (parent.pbx.trunks.rules.add({trunk: this_trunk, name: time_int_name, time_interval: time_int_name, dest: dest, pattern: TMP_NEW_PATTERN, digits: local_dest})) {
				ASTGUI.feedback({ msg: 'Added!', showfor: 2, color: 'blue', bgcolor: '#ffffff'});
				window.location.reload();
			}

			parent.ASTGUI.dialog.hide();
		}else{ // edit/update existing incoming rule
			var resp = parent.pbx.trunks.rules.edit({
				line: EDIT_CONTEXT_IR_LINE, 
				dest: dest, 
				digits: local_dest, 
				cxt: EDIT_CONTEXT_IR,
				pattern: TMP_NEW_PATTERN
			});
			if (resp) {
				ASTGUI.feedback( { msg: 'Updated !', showfor: 2 , color: 'blue', bgcolor: '#FFFFFF' } );
			}

			window.location.reload();
		}
	}
};
