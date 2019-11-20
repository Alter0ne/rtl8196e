/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * Logging functions
 *
 * Copyright (C) 2006-2008, Digium, Inc.
 *
 * Mark Spencer <markster@digium.com>
 * Pari Nannapaneni <pari@digium.com>
 * Ryan Brindley <ryan@digium.com>
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

/**
 * Main Log Object.
 * This log object contains all the needed logging functions for the GUI.
 */
var log = {
	colors: {
		ajax: '#96997c',
		debug: '#4c9996',
		error: '#992b23',
		info: '#9a9a9a',
		warn: '#f47a00'
	},
	html: '' /**< jQuery selector, or jQuery object for holding all the log messages. */
};

/**
 * Log as ajax.
 * @param msg The message.
 */
log.ajax = function(msg) {
	if (!msg || top.session.log_modes.ajax !== true) {
		return true;
	}
	this.doLog(msg, this.colors.ajax);
};

/**
 * Log to the Console.
 * @param msg The message.
 */
log.console = function(msg) {
	if (!msg || top.session.log_modes.console !== true || window.console || window.console.firebug) {
		return true;
	}
	console.log(msg);
};

/**
 * Clear log messages.
 */
log.clear = function() {
	top.session.debug_log = [];
	$(this.html).html('No log messages');
};

/**
 * Debug log.
 * @param msg The message.
 */
log.debug = function(msg) {
	if (!msg || top.session.log_modes.debug !== true) {
		return true;
	}
	this.doLog(msg, this.colors.debug);
};

/**
 * Core Log Function.
 * @param msg The message.
 * @param color The HTML hexademical color code.
 */
log.doLog = function(msg, color) {
	if (!top.session.log) {
		return true;
	}

	if (typeof msg === 'object') {
		msg = 'OBJECT: ' + getProperties(msg);
	}

	var now = new Date();
	var h = now.getHours().addZero() + ':' + now.getMinutes().addZero() + ':' + now.getSeconds().addZero();

	if (top.session.debug_log.length > 250) {
		top.session.debug_log = top.session.debug_log.slice(0,50);
	}

	top.session.debug_log.unshift( h + ' <font color='+ color + '>' + msg + '</font>');
};

/**
 * Log as Error.
 * @param msg The message.
 */
log.error = function(msg) {
	if (!msg || top.session.log_modes.error !== true) {
		return true;
	}
	this.doLog(msg, this.colors.error);
};

/**
 * Log as Info.
 * @param msg The message.
 */
log.info = function(msg) {
	if (!msg || top.session.log_modes.info !== true) {
		return true;
	}
	this.doLog(msg, this.colors.info);
};

/**
 * Init Logging.
 * @param html the jQuery selector or jQuery object storing mesgs
 */
log.init = function(html) {
	if (top.session.DEBUG_PROFILER_BEGIN) {
		return;
	}

	var m = _$('debug_messages');
	var aaaaa = function() {
		if (top.session.log && top.session.debug_log.length) {
			if (m.style.display == '') {
				m.innerHTML = '<li>' + top.session.debug_log.join('<li>');
			}
		}
	};
	m.innerHTML = 'No log messages';
	var now = new Date();
	top.session.DEBUG_PROFILER_BEGIN = now.getTime();
	setInterval(aaaaa, 3000);

	m.style.display = '';
	$('#dbw_flip').click( function() {
		if (m.style.display == '') {
			m.style.display = 'none';
			this.innerHTML = 'Show debug messages';
			this.className = 'dbw_flip_hide';
		} else if ( m.style.display == 'none') {
			m.innerHTML = (top.session.debug_log.length) ? '<li>' + top.session.debug_log.join('<li>') : 'No log messages';
			m.style.display = '';
			this.innerHTML = 'Hide debug messages';
			this.className = 'dbw_flip_show';
		}
	});

	aaaaa();

	_$('debugWindow_which_Ajax').checked = sessionData.DEBUG_WHICH.Ajax ;
	$('#debugWindow_which_Ajax').click(function(){ sessionData.DEBUG_WHICH.Ajax =  this.checked ; });

	_$('debugWindow_which_Debug').checked = sessionData.DEBUG_WHICH.Debug ;
	$('#debugWindow_which_Debug').click(function(){ sessionData.DEBUG_WHICH.Debug = this.checked ; });

	_$('debugWindow_which_Error').checked = sessionData.DEBUG_WHICH.Error ;
	$('#debugWindow_which_Error').click(function(){ sessionData.DEBUG_WHICH.Error = this.checked ; });

	_$('debugWindow_which_Console').checked = sessionData.DEBUG_WHICH.Console ;
	$('#debugWindow_which_Console').click(function(){ sessionData.DEBUG_WHICH.Console = this.checked ; });

	_$('debugWindow_which_Info').checked = sessionData.DEBUG_WHICH.Info ;
	$('#debugWindow_which_Info').click(function(){ sessionData.DEBUG_WHICH.Info = this.checked ; });

	_$('debugWindow_which_Warnings').checked = sessionData.DEBUG_WHICH.Warn ;
	$('#debugWindow_which_Warnings').click(function(){ sessionData.DEBUG_WHICH.Warn = this.checked ; });
};

/**
 * Log as Warning.
 * @param msg The message.
 */
log.warn = function(msg) {
	if (!msg || top.session.log_modes.warn !== true) {
		return true;
	}
	this.doLog(msg, this.colors.warn);
};
