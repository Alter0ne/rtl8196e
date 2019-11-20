/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * All session variables and cookie management methods
 *
 * Copyright (C) 2006-2009, Digium, Inc.
 *
 * Ryan Brindley <ryan@digium.com>
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

/**
 * Session Variables.
 * This is the object that will globally hold all the session variables.
 */
var session = {
	debug_log: [], /**< array holding all the logging */
	delimiter: ',', /**< extensions.conf delimiter, used only in special cases */
	log: false, /**< boolean toggling logging */
	/**
	 * Logging Modes.
	 */
	log_modes: {
		ajax: true, /**< toggle ajax logs */
		console: true, /**< toggle console logs */
		debug: true, /**< toggle debug logs */
		error: true, /**< toggle error logs */
		info: true, /**< toggle info logs */
		warn: true /**< toggle warning logs */
	}
};

/**
 * Cookie Management.
 * This object manages all cookies needs.
 */
var cookies = {};


/**
 * Get Cookie.
 * Get a Cookie. OM NOM NOM NOM
 * @param user Username
 */
cookies.get = function(varname) {
	var ck = top.document.cookie;

	if (ck.indexOf(varname + '=') == -1) {
		top.log.warn("Can't seem to find the user in a cookie, exiting.");
		return '';
	}

	var cookies = ck.split(';');
	for (var i=0; i < cookies.length; i++) {
		var c = cookies[i].trim();
		if (c.beginsWith(varname + '=')) {
			return c.split(varname + '=')[1];
		}
	}

	top.log.info("No cookie found for user, exiting.");
	return '';
};

/**
 * Set Cookie.
 * @param varname Variable to set.
 * @param val Value to set the variable to.
 */
cookies.set = function(varname, val) {
	if (varname == '') {
		top.log.error('Variable name cannot be empty when setting a cookie.');
		return false;
	}	

	var tmp = varname + '=' + val + '; path=/';
	top.document.cookie = tmp;
};

/**
 * Remove Cookie.
 * Remove a cookie...from the cookie jar!
 * @param ck The cookie.
 */
cookies.remove = function(ck) {
	top.log.info('Removing cookie: ' + ck);
	top.document.cookie = ck + '=somevalue; expires=Thu, 16 Aug 1984, 00:00:00 UTC; path=/';
};

/**
 * Clear Cookies.
 * Clear the cookies.
 */
cookies.clear = function() {
	top.log.info('Clearing cookies!');
	top.document.cookie = '';
};
