/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * Javascript functions for accessing manager over HTTP
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
 * Asterisk Manager object
 * This object contains all the methods and variables necessary to communicate with Asterisk.
 */
var astman = {
	rawman: '../../rawman' /**< string variable. This holds the HTTP location of rawman. Was formerly named ASTGUI.paths.rawman */
};

/**
 * Manage Asterisk's Internal Database.
 */
astman.astdb = {
	/**
	 * Object contain default values.
	 */
	defaults: {
		dbname: 'astgui' /**< string variable. This holds the default dbname. Was formerly named ASTGUI.globals.GUI_DB */ 
	},

	/**
	 * Delete by key.
	 * @param k a db object containing: key[, dbname]
	 * @return true or false
	 */
	delete: function(k) {
		if (!k.hasOwnProperty('dbname')) {
			k.dbname = this.defaults.dbname;
		}

		try {
			var s = astman.cliCommand('database del' + k.dbname + ' ' + k.key);
			if (!s.contains('entry removed')) {
				throw new Error(s);
			}
		} catch (err) {
			log.error(err.message);
			return false;
		}
		return true;
	},

	/**
	 * Get by key.
	 * @param k a db object containing: key[, dbname]
	 * @return key value
	 */
	get: function(k) {
		if (!k.hasOwnProperty('dbname')) {
			k.dbname = this.defaults.dbname;
		}

		try {
			if (!k.hasOwnProperty('key')) {
				log.error('k.key is null, cannot complete astdb.update request');
				return false;	
			}

			var s = astman.cliCommand('database get ' + k.dbname + ' ' + k.key);
			if (!s.contains('Value: ')) {
				throw new Error(astman.parseCLI(s));
			}
		} catch (err) {
			log.error(err.message);
			return null;
		}

		var val = astman.parseCLI(s);
		val.trim().withOut('Value: ');
		return val.trim();
	},

	/**
	 * Get all.
	 * @param k a db object containing: [dbname]
	 * @return object with all the key-values
	 */
	getAll: function(k) {
		if (!k.hasOwnProperty('dbname')) {
			k.dbname = this.defaults.dbname;
		}

		var db_keys = {};
		var dbpath = '/' + k.dbname + '/';
		
		try {
			var s = astman.cliCommand('database show ' + k.dbname);
			if (!s.contains(dbpath)) {
				throw new Error(s);
			}
		
			var op = astman.parseCLI(s);
			if (op.trim() === '') {
				return null;
			}

			var lines = op.split('\n');
			lines.each( function(line) {
				line = line.withOut(dbpath);
				var key = line.beforeChar(':').trim();
				var val = line.afterChar(':').trim();
				if (key !== '') {
					db_keys[key] = val;
				}
			});
		} catch (err) {
			log.error(err.message);
			return null;
		}

		return db_keys;
	},

	/**
	 * Update by key.
	 * @param k a db object containing: key, keyvalue[, dbname]
	 * @return true or false
	 */
	update: function (k) {
		if (!k.hasOwnProperty('dbname')) {
			k.dbname = this.defaults.dbname;
		}

		try {
			if (!k.hasOwnProperty('key')) {
				log.error('k.key is null, cannot complete astdb.update request');
				return false;	
			} else if (!k.hasOwnProperty('keyvalue')) {
				log.error('k.keyvalue is null, cannot complete astdb.update request');
				return false;
			}
		
			var s = astman.cliCommand('database put ' + k.dbname + ' ' + k.key + ' ' + k.keyvalue);
			if(!s.contains('successfully')) {
				throw new Error(s);
			}
		} catch (err) {
			log.error(s);
			return false;
		}
		return true;
	}
};

/**
 * Executes a CLI Command.
 * This function takes a string CLI command and sends it to Asterisk to be executed. This function was formerly named ASTGUI.cliCommand
 * @param {String} cmd The CLI Command to be executed.
 * @return the CLI output
 */
astman.cliCommand = function(cmd) {
	if (typeof cmd !== 'string') {
		log.warn('cliCommand: Expecting cmd as String');
		return '';
	}

	log.debug('cliCommand: Executing manager command: "' + cmd + '"');
	return this.makeSyncRequest( {action: 'command', command: cmd });
};

/**
 * Makes a sync request.
 * This function takes an object and makes a synchronous ajax request. This function was formerly named makeSyncRequest
 * @param {Object} params the object containg all the parameters/options
 * @return {String} response text from the ajax call.
 */
astman.makeSyncRequest = function(params) {
	if (top.session && top.session.debug_mode) {
		log.ajax('makeSyncRequest: AJAX Request: "' + getProperties(param) + '"');
	}

	if (typeof params !== 'object') {
		log.error('makeSyncRequest: Expecting params to be an object.');
		return '';
	}

	var s = $.ajax({ url: astman.rawman, data: params, async: false});
	return s.responseText;
};

/**
 * Parses CLI Responses.
 * The function takes a raw CLI response and strips unnecessary info, returning only the useful info. This function use to be called ASTGUI.parseCLIResponse.
 * @param resp The CLI Response to be parsed.
 * @return {String} the parsed CLI responsed
 */
astman.parseCLI = function(resp) {
	if (typeof resp !== 'string') {
		return resp;
	}

	resp = resp.replace(/Response: Follows/, '');
	resp = resp.replace(/Privilege: Command/, '');
	resp = resp.replace(/--END COMMAND--/, '');

	return resp;
};
