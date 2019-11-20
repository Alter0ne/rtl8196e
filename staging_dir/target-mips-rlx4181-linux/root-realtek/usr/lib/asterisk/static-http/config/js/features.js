/* Asterisk-GUI	- an Asterisk configuration interface
 *
 * Call Features javascript
 *
 * Copyright (C) 2007-2008, Digium, Inc.
 *
 * Ryan Brindley <rbrindley@digium.com>
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
 */
var exten_conf;
var feat_conf;
var gui_conf;
var amap_table = $('#application_map_list tbody');
var amap_enabled = [];
var amap_orig_name;
var vals = {};
var dial_options;
var dial_options_list = ['t', 'T', 'h', 'H', 'k', 'K'];
var apps = [ 'Answer', 'Background', 'Busy', 'Congestion', 'DigitTimeout', 'DISA', 'ResponseTimeout', 'Playback' , 'UserEvent' , 'Wait', 'WaitExten', 'Hangup' ];

/**
 * function to edit options
 * @param obj the DOM object
 */
var edit = function(obj) {
	vals[obj.attr('id')] = obj.val();
	if (typeof edit_funcs[obj.attr('id')] !== 'function') {
		top.log.error('edit: edit_funcs["' + obj.attr('id') + '"] is not a function.');
		updateMsg(obj, 'error', 'Error: Edit function was not found.');
		return false;
	}

	try {
		edit_funcs[obj.attr('id')](obj.val());
		updateMsg(obj, 'edit');
	} catch(e) {
		switch (e.name) {
		case 'ShouldRemoveException':
			updateMsg(obj, 'error', 'Error: Should be removed instead.');
		default:
			updateMsg(obj, 'error', e.message);
		}
	}
};

/**
 * object to hold all the (asterisk) editing/removing functions.
 * This is done instead of generalizing them into sectioned objects
 * because the layout/sections/organization may change.
 * NOTE: only dial options use remove_funcs, removing for the others
 * is just their edit_func with '' as the val
 */
var edit_funcs = {};
var remove_funcs = {};

edit_funcs['feature_featuredigittimeout'] = function(val) {
	/* lets validate the val */
	validate(val, {num: true, positive: true});

	/* kk, all good, now update asterisk and go */
	return sendUpdate({cxt: 'general', name: 'featuredigittimeout', val: val});
};

edit_funcs['parkext'] = function(val) {
	/* lets validate */
	validate(val, {num: true, positive: true, extens: true});

	/* custom validation to make sure it doesn't step over parkpos */
	var parkpos = $('#parkpos').val();
	parkpos = parkpos.split('-');
	parkpos[0] = parseInt(parkpos[0], 10);
	parkpos[1] = parseInt(parkpos[1], 10);
	/* return error if parkpos is properly defined and val is inbetween the parkpos numbers */
	if (!isNaN(parkpos[0]) && !isNaN(parkpos[1]) && val >= parkpos[0] && val <= parkpos[1]) {
		throw RangeError('Invalid: Conflicts with Parked Call extensions (' + parkpos[0] + '-' + parkpos[1] + ').');
	}

	/* kk, all good, now update asterisk and go */
	return sendUpdate({cxt: 'general', name: 'parkext', val: val});
};

edit_funcs['parkpos'] = function(val) {
	/* validate special format */
	if (!val.match(/^[0-9][0-9]*\-[0-9][0-9]*$/)) {
		throw TypeError('Invalid: Please use "&lt;num&gt;-&lt;num&gt;" format');
	}

	/* lets split */
	var splits = val.split('-');
	var pos_start = parseInt(splits[0], 10);
	var pos_end = parseInt(splits[1], 10);

	/* now lets validate the start and end */
	validate(pos_start, {num: true, positive: true});
	validate(pos_end, {num: true, positive: true});

	/* we need to do a special validate for extens */
	gui_conf = context2json({
		filename: 'guipreferences.conf',
		context: 'general',
		usf: 1
	});
	var prefs = {ue: 'User Extensions', mm: 'Conference Extensions', qe: 'Queue Extensions', vme: 'Voicemail Extensions', rge: 'Ring Group Extensions', vmg: 'Voicemail Group Extensions'};
	for (pref in prefs) {
		if (!prefs.hasOwnProperty(pref)) {
			continue;
		}

		var start = gui_conf[pref + '_start'];
		var end = gui_conf[pref + '_end'];

		if ((pos_start >= start && pos_start <= end) || (pos_end >= start && pos_end <= end) || (pos_start < start && pos_end >= start)) {
			top.log.warn('edit_funcs[\'parkext\']: conflicts with ' + pref + '.');
			throw RangeError('Invalid: Conflicts with ' + prefs[pref] + ' range, ' + start + '-' + end);
		}
	}

	/* kk, all good, now update asterisk and go */
	return sendUpdate({cxt: 'general', name: 'parkpos', val: val});
}

edit_funcs['parkingtime'] = function(val) {
	/* lets validate */
	validate(val, {num: true, positive: true});

	/* kk, all good, now update asterisk and go */
	return sendUpdate({cxt: 'general', name: 'parkingtime', val: val});
};

edit_funcs['fmap_blindxfer'] = function(val) {
	/* lets validate */
	validate(val, {keypress: true});

	/* kk, all good, now update asterisk and go */
	return sendUpdate({cxt: 'featuremap', name: 'blindxfer', val: val});
};

edit_funcs['fmap_disconnect'] = function(val) {
	/* lets validate */
	validate(val, {keypress: true});

	/* kk, all good, now update asterisk and go */
	return sendUpdate({cxt: 'featuremap', name: 'disconnect', val: val});
};

edit_funcs['fmap_atxfer'] = function(val) {
	/* lets validate */
	validate(val, {keypress: true});

	/* kk, all good, now update asterisk and go */
	return sendUpdate({cxt: 'featuremap', name: 'atxfer', val: val});
};

edit_funcs['fmap_parkcall'] = function(val) {
	/* lets validate */
	validate(val, {keypress: true});

	/* kk, all good, now update asterisk and go */
	return sendUpdate({cxt: 'featuremap', name: 'parkcall', val: val});
};

/* dynamically define all the dial options edit/remove funcs since there the same */
for (var i=0; i<dial_options_list.length; i++) {
	edit_funcs['dial_'+dial_options_list[i]] = function(val) {
		top.pbx.dial_options.add(val);
	}
	remove_funcs['dial_'+dial_options_list[i]] = function(val) {
		top.pbx.dial_options.remove(val);
	}
}

/**
 * function to load options
 */
var load = function() {
	exten_conf = context2json({
		filename: 'extensions.conf',
		context: 'globals',
		usf: 1
	});

	var dial_options = exten_conf['DIALOPTIONS'] || '';

	if (exten_conf['FEATURES'] && exten_conf['FEATURES'].length) {
		amap_enabled = exten_conf['FEATURES'].split('#');
	}

	/* set all the enabled dial options to checked */
	dial_opts = dial_options.split('');
	for (var i=0; i<dial_opts.length; i++) {
		$('#dial_'+dial_opts[i]).attr('checked', 'checked').parents('.feature').removeClass('disabled');
	}

	feat_conf = config2json({
		filename: 'features.conf',
		usf: 1
	});

	if (!feat_conf['general'] || !feat_conf['featuremap'] || !feat_conf['applicationmap']) {
		var u = new listOfSynActions('features.conf');
		if( !feat_conf.hasOwnProperty('general') ) {
			u.new_action('newcat', 'general', '', '');
		}
		if( !feat_conf.hasOwnProperty('featuremap') ) {
			u.new_action('newcat', 'feature_map', '', '');
		}
		if( !feat_conf.hasOwnProperty('applicationmap') ) {
			u.new_action('newcat', 'applicationmap', '', '');
		}
		u.callActions();
		window.location.reload();
		return;
	}

	/* look through the features in feature_map and get the values from features.conf */
	$('#feature_map > .feature > :text').each(function() {
		var vari = $(this).attr('id').split('_')[1];
		$(this).val(feat_conf['featuremap'][vari]);
		if (feat_conf['featuremap'][vari]) {
			$(this).parents('.feature').removeClass('disabled');
		}
	});
	/* look through the features in call_parking and get the values from features.conf */
	$('#call_parking > .feature > :text').each(function() {
		var vari = $(this).attr('id');
		$(this).val(feat_conf['general'][vari]);
		if (feat_conf['general'][vari]) {
			$(this).parents('.feature').removeClass('disabled');
		}
	});
	/* look through the features in feature_options and get the values from features.conf */
	$('#feature_options > .feature > :text').each(function() {
		var vari = $(this).attr('id').split('_')[1];
		$(this).val(feat_conf['general'][vari]);
		if (feat_conf['general'][vari]) {
			$(this).parents('.feature').removeClass('disabled');
		}	
	});

	var amap = feat_conf['applicationmap'];
	for (var name in amap) {
		if (!amap.hasOwnProperty(name)) {
			continue;
		}

		listAmap(name);
	}

	if (amap_table.find('tr:not(.template)').length === 0) {
		var row = $('<tr>').attr('colspan', '7').addClass('noapps').html('No Application Maps Defined.');
		amap_table.append(row);
	}

}

var listAmap = function(name) {
	/* is it new or not? */
	if (name) {
		/* list an existing amap */
		var amap_fields = feat_conf['applicationmap'][name].split(',');
	} else {
		/* set a new amap default values */
		name = '';
		var amap_fields = ['', 'self', '', ''];
	}

	if (amap_table.find('tr:not(.template)').length) {
		amap_table.find('tr.noapps').remove();
	}

	var row = $('#application_map_list > tbody > tr.template').clone();
	/* only set id if its an existing amap */
	/* only check for enabled if its an existing amap */
	if (name !== '') {
		row.attr('id', 'amap_' + name);
		row.find('.enabled :checkbox').attr('checked', (amap_enabled.contains(name)?true:false));
	}
	row.find('.name :text').attr('value', name);
	row.find('.digits :text').attr('value', amap_fields[0]);
	row.find('.active option[value='+amap_fields[1]+']').attr('selected', true);
	row.find('.app_name :text')
		.attr('value', amap_fields[2])
		.autocomplete(apps, {
			autoFill: true,
			width: 150	
	});
	row.find('.app_args :text').attr('value', amap_fields[3]);
	amap_table.append(row);
	row.removeClass('template');
};

/**
 * create a new application map row
 */
var newAmap = function() {
	listAmap('');
};

var editAmap = function(obj) {
	var vali = '';
	var resp;

	/* get the variables */
	obj = obj.parents('tr');
	var enabled = obj.find('.enabled > input').attr('checked');
	var name = obj.find('.name > :text').val();
	var digits = obj.find('.digits > :text').val();
	var active = obj.find('.active option:selected').val();
	var app_name = obj.find('.app_name :text').val();
	var app_args = obj.find('.app_args :text').val();

	if (!validateAmap(obj, {name: name, digits: digits, app_name: app_name}, true)) {
		return false;
	}

	/* generate the applicationmap string */
	var value = [digits, active, app_name, app_args].join();

	/* time to upload to Asterisk */
	var actions = listOfSynActions('features.conf');
	if (obj.attr('id').substring(5) !== name) {
		actions.new_action('delete', 'applicationmap', obj.attr('id').substring(5), '');
	}
	actions.new_action('update', 'applicationmap', name, value);

	resp = actions.callActions();
	if (!resp.contains('Response: Success')) {
		top.log.error('editAmap: error updating features.conf');
		return false;
	}

	if (enabled) {
		actions.clearActions('extensions.conf');
		resp = '';

		if (obj.attr('id') !== '') {
			amap_enabled.splice(amap_enabled.indexOf(obj.attr('id').substring('5'), 1));
		}
		amap_enabled.push(name);
		actions.new_action('update', 'globals', 'FEATURES', amap_enabled.join('#'));
		
		resp = actions.callActions();
		if (!resp.contains('Response: Success')) {
			top.log.error('editAmap: error updating features.conf');
			return false;
		}
	}

	obj.attr('id', 'name');
	return true;
};

var disableAmap = function(obj) {
	/* if it doesn't exist, do nothing */
	if (!amap_enabled.contains(obj.parents('tr').attr('id').substring(5))) {
		return true;
	}

	/* remove from list and make string */
	amap_enabled.splice(amap_enabled.indexOf(obj.parents('tr').attr('id').substring(5)), 1);
	var str = amap_enabled.join('#');
	
	/* update Asterisk */
	var actions = new listOfSynActions('extensions.conf');
	actions.new_action('update', 'globals', 'FEATURES', str);

	var resp = actions.callActions();
	if (!resp.contains('Response: Success')) {
		top.log.error('disableAmap: error updating extensions.conf.');
		top.log.error(resp);
		amap_enabled.push(obj.parents('tr').attr('id').substring(5));
		return false;
	}

	return true;
};

var enableAmap = function(obj) {
	/* if it already exists, do nothing */
	if (amap_enabled.contains(obj.parents('tr').attr('id').substring(5))) {
		return true;
	}

	/* add to list and make string */
	amap_enabled.push(obj.parents('tr').attr('id').substring(5));
	var str = amap_enabled.join('#');
	
	/* update Asterisk */
	var actions = new listOfSynActions('extensions.conf');
	actions.new_action('update', 'globals', 'FEATURES', str);

	var resp = actions.callActions();
	if (!resp.contains('Response: Success')) {
		top.log.error('enableAmap: error updating extensions.conf.');
		top.log.error(resp);
		amap_enabled.splice(amap_enabled.indexOf(obj.parents('tr').attr('id').substring(5)), 1);
		return false;
	}

	return true;
};

var removeAmap = function(obj) {
	/* if this doesn't have an id, its most likely a 'new, unsaved' amap */
	if (!obj.parents('tr').attr('id')) {
		obj.parents('tr').queue(function() {
			$(this).fadeOut(500);
			$(this).dequeue();
		});
		obj.parents('tr').queue(function() {
			$(this).remove();
			$(this).dequeue();
		});
		return true;
	}
	var name = obj.parents('tr').attr('id').substring('5'); /* amap_XXXNAMEXXX */
	var actions = new listOfSynActions('extensions.conf');
	var resp;

	if (amap_enabled.contains(name)) {
		amap_enabled.splice(amap_enabled.indexOf(name), 1);
		actions.new_action('update', 'globals', 'FEATURES', amap_enabled.join('#'));

		resp = actions.callActions();
		if (!resp.contains('Response: Success')) {
			top.log.error('removeAmap: Error updating extensions.conf');
			return false;
		}
	}
	actions.clearActions('features.conf');

	actions.new_action('delete', 'applicationmap', name, '');

	resp = actions.callActions();
	if (!resp.contains('Response: Success')) {
		top.log.error('removeAmap: Error updating features.conf');
		return false;
	}

	obj.parents('tr').queue(function() {
		$(this).fadeOut(500);
		$(this).dequeue();
	});
	obj.parents('tr').queue(function() {
		$(this).remove();
		$(this).dequeue();
	});
	// if number of rows is one, the table is about to be empty
	if (amap_table.find('tr:not(.template)').length === 1) { 
		var row = $('<tr>').attr('colspan', '7').addClass('noapps').html('No Application Maps Defined.');
		amap_table.append(row);
	}
};

var validateAmap = function(obj, params, focus) {
	/* used for single validatation */
	if (params.variable) {
		switch(params.variable) {
		case 'name':
			params.name = params.value;
			break;
		case 'digits':
			params.digits = params.value;
			break;
		case 'app_name':
			params.app_name = params.value;
			break;
		default:
		}	
		delete params.variable;
		delete params.value;
	}
	/* lets validate all the variables before we send */
	try {
		if (typeof params.name !== 'undefined') {
			vali = 'name';
			validate(params.name, {notnull: true, str: true, aststr: true});
		}
		if (typeof params.digits !== 'undefined') {
			vali = 'digits';
			validate(params.digits, {notnull: true, keypress: true});
		}
		if (typeof params.app_name !== 'undefined') {
			vali = 'app_name';
			validate(params.app_name, {notnull: true, str: true});
		}
	} catch(e) {
		if (focus) {
			obj.find('td.'+vali+' input').focus()
		}
		obj.find('td.'+vali+' input').addClass('error');
		top.log.error(e.message);
		return false;
	}

	obj.find('input').removeClass('error');
	return true;
}

/**
 * function to removing options
 * @param obj the DOM object
 */
var remove = function(obj) {
	vals[obj.attr('id')] = obj.val();
	if (obj.attr('type') === 'checkbox') {
		if (typeof remove_funcs[obj.attr('id')] !== 'function') {
			top.log.error('remove: remove_funcs["' + obj.attr('id') + '"] is not a function.');
			updateMsg(obj, 'error', 'Error: Remove function was not found.');
			return false;
		}
	} else {
		if (typeof edit_funcs[obj.attr('id')] !== 'function') {
			top.log.error('remove: edit_funcs["' + obj.attr('id') + '"] is not a function.');
			updateMsg(obj, 'error', 'Error: Remove function was not found.');
			return false;
		}
	}

	try {
		if (obj.attr('type') === 'checkbox') {
			remove_funcs[obj.attr('id')](obj.val());
		} else {
			edit_funcs[obj.attr('id')]('');
		}

		updateMsg(obj, 'remove');
	} catch(e) {
		switch (e.name) {
		default:
			updateMsg(obj, 'error', e.message);
		}
	}
};

/**
 * sends updates to Asterisk.
 * @param cxt variable's context.
 * @param name variable name.
 * @param val variable value.
 * @throws CommunicationError.
 * @return boolean of success.
 */
var sendUpdate = function(params) {
	if (!params.cxt || !params.name) {
		top.log.error('sendUpdate: params.cxt or params.name is undefined.');
		throw TypeError('Error: params.cxt or params.name is undefined.');
	}

	var actions = listOfSynActions('features.conf');
	actions.new_action('update', params.cxt, params.name, params.val);
	var resp = actions.callActions();
	if (!resp.contains('Response: Success')) {
		top.log.error('sendUpdate: error updating features.conf.');
		throw CommunicationError('Error: Unable to update features.conf.');
	}

	return true;
}

/**
 * function to show update mesgs
 * @param obj the DOM object
 * @param msg the msg to display
 */
var updateMsg = function(obj, type, err) {
	var type = type || '';
	var err = err || '';
	switch(type) {
	case 'edit':
		obj.siblings('.update').hide();
		obj.parents('.feature').effect('highlight', {color: '#aeffae'}, 1000);
		break;
	case 'error':
		obj.siblings('.update').html(err);
		obj.siblings('.update').show();
		break;
	default:
		obj.siblings('.update').hide();
		obj.parents('.feature').effect('highlight', {color: '#ffaeae'}, 1000);
	}
};

var validate = function(val, chks) {
	/* does val exist? */
	if (chks.notnull && (typeof val === 'undefined' || val === '' || val === null)) {
		top.log.error('validate: val is not defined.');
		throw TypeError('Invalid: This cannot be empty.');
	}

	/* make sure val is a num */
	if (chks.num && !val.toString().match(/^[0-9][0-9]*$/)) {
		top.log.error('validate: val needs to be a number.');
		throw TypeError('Invalid: This needs to be a number.');
	}

	/* is val a string? */
	if (chks.str && typeof val !== 'string') {
		top.log.error('validate: val needs to be a string.');
		throw TypeError('Invalid: This needs to be a string.');
	}

	/* make sure its 0 or more */
	if (chks.positive && val < 0) {
		top.log.warn('validate: val needs to be 0 or greater.');
		throw RangeError('Invalid: This needs to be 0 or greater.');
	}

	/* lets make sure the exten doesn't conflict with any extension ranges */
	if (chks.extens) {
		gui_conf = context2json({
			filename: 'guipreferences.conf',
			context: 'general',
			usf: 1
		});
		var prefs = {ue: 'User Extensions', mm: 'Conference Extensions', qe: 'Queue Extensions', vme: 'Voicemail Extensions', rge: 'Ring Group Extensions', vmg: 'Voicemail Group Extensions'};
		for (pref in prefs) {
			if (!prefs.hasOwnProperty(pref)) {
				continue;
			}

			var start = parseInt(gui_conf[pref + '_start'], 10);
			var end = parseInt(gui_conf[pref + '_end'], 10);

			if (val >= start && val <= end) {
				top.log.warn('validate: conflicts with ' + pref + '.');
				throw RangeError('Invalid: Conflicts with ' + prefs[pref] + ' range, ' + start + '-' + end);
			}
		}
	}

	if (chks.keypress && val !== '' && !val.match(/^[0-9#\*][0-9#\*]*$/)) {
		top.log.error('validate: val should only contain 0-9, #, or *.');
		throw TypeError('Invalid: This should only contain 0-9, #, or *.');
	}

	if (chks.aststr && !val.match(/^[a-zA-Z0-9_-][a-zA-Z0-9_-]*$/)) {
		top.log.error('validate: val should only contain a-z, A-Z, 0-9, _, or -.');
		throw TypeError('Invalid: This should only contain a-z, A-Z, 0-9, _, or -');
	}
};

/********* EXCEPTIONS **********/
var CommunicationError = function(msg) {
	this.name = "CommunicationError";
	this.msg = msg || "An error occurred while communicating with Asterisk.";
};
