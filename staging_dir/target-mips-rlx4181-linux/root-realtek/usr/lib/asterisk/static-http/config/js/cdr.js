/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * cdr.html functions
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
var backend = "CDR-CSV";
var records = [];
var viewCount = 25;	/* number of CDR rows displayed at once */
var offset = 0;		/* index into CDR records for first row of current display */
var fields = [		/* Table column header text */
	"",
	"Account Code", "Source", "Destination", "Dest. Context",
	"Caller ID", "Channel", "Dest. Channel", "Last app.",
	"Last data", "Start time", "Answer Time", "End Time",
	"Duration", "Billable seconds", "Disposition", "AMA flags", 
	"Unique ID", "Log userfield"
];
var svindex = [ /* which field to display for short version, add 1 to get index into headers */
		/* use as-is for index into CDR record fields */
		/* -1 is special case for first column (black header, 1..n for each row) */
	-1, 9, 12, 1, 2, 4, 14
];
var longversion = false; /* long version displays all CDR-CSV fields, short only those selected above */
var sortby = 0;	/* Which column (CDR field) to sort the displayed CDR list by. */ 
		/* negative means descending. Subtract 1 to get index into the records fields array */
var maxColumns = 0;	/* maximum number of columns found in CDR records */

var recordsInbound = [];
var recordsOutbound = [];
var recordsInternal = [];
var recordsExternal = [];
var recordsSystem = [];
var uniqueIDs = [];
var recordsSelected = [];

var showSystemCalls = false; /* don't display CDR records with destination of "asterisk_guitools" */
var showOutboundCalls = true;
var showInboundCalls = true;
var showInternalCalls = true;
var showExternalCalls = true;
var nSelected = 0;

function setSortbyField(index) {	/* comes here on click on one of the table headers */
	offset = 0;
	if (Math.abs(sortby) == index) sortby = -sortby; /* if sorting on same column, simply reverse order */
	else sortby = index;
	loadRecords(true);
}

function sortCompare(a,b) {
	var sb = Math.abs(sortby) - 1;  /* subtract one, js arrays are zero-based index */
	var sign = sortby < 0 ? -1 : 1; /* negative requests descending order */
	var cmpa = a[sb];
	var cmpb = b[sb];
	if (cmpa < cmpb) return(-1*sign);  /* returns -1 if ascending, 1 if descending requested */
	else if (cmpa > cmpb) return(sign); /* returns 1 if ascending, -1 if descending requested */
	else return(0);
}

function nextPage() {
	if (nSelected > offset + viewCount) offset += viewCount;
	loadRecords(false);
}

function prevPage() {
	if (offset) offset -= viewCount;
	if (offset < 0) offset = 0;
	loadRecords(false);
}

function rowClick(tr) {
	if (tr.asteriskCDRuniqueID) {
		var i = uniqueIDs.length;
		var ir = [];
		while (i--) if (uniqueIDs[i] == tr.asteriskCDRuniqueID) ir.push(i);
		var x = ir.length;
		if (x>0) {
			var td = document.getElementById("cdr_calldetail_text");
			var txt = (x>1)?"<B>Multiple records with same Unique ID</B><BR><BR>":"";
			while (x--) {
				var r = records[ir[x]];
				txt = txt+"<B>Record "+ir[x]+", ID:</B> "+r[16]+"<B> ("+r.callType+" call)</B><BR>"+
					"<B>Timestamps:</B> "+r[9]+", "+r[10]+", "+r[11]+"<BR>"+
					"<B>Durations:</B> "+r[12]+", "+r[13]+"<BR>"+
					"<B>Disposition/AMA flags:</B> "+r[14]+" / "+r[15]+"<BR>"+
					"<B>CallerID:</B> "+r[4]+" <B>Source:</B> "+r[1]+" <B>Destination:</B> "+r[2]+"<BR>"+
					"<B>Source Channel:</B> "+r[5]+"<BR>"+
					"<B>Destination Channel:</B> "+r[6]+"<BR>"+
					"<B>Context:</B> "+r[3]+" <B>Application:</B> "+r[7]+" <B>Data:</B> "+r[8]+"<BR>"+
					"<B>Account code:</B> "+r[0]+
					" <B>Userfield:</B> "+r[17]+"<BR><BR>";
			}
			td.innerHTML = txt;
			$(document.getElementById("cdr_calldetail_DIV")).showWithBg();
		}
	}
}

function VisibleHeight() {	/* need to test to make sure works on all browsers */
	var h = 0;
	if( typeof( window.parent.innerHeight ) == 'number' ) {
		h = window.parent.innerHeight;		/* Most browsers except MS IE */
	}
	else if(window.parent.document.documentElement &&
		  window.parent.document.documentElement.offsetHeight) {
		h = window.parent.document.documentElement.offsetHeight; /* MS IE 7 */
	}
	else if( document.body && (document.body.clientHeight ) ) {
		h = document.body.clientHeight;
	}
	return h;
}
function VisibleWidth() {	/* need to test to make sure works on all browsers */
	var w = 0;
	if( typeof( window.parent.innerWidth ) == 'number' ) {
		w = window.parent.innerWidth;		/* Most browsers except MS IE */
	}
	else if(window.parent.document.documentElement && 
		window.parent.document.documentElement.offsetWidth) {
		w = window.parent.document.documentElement.offsetWidth; /* MS IE 7 */
	}
	else if( document.body && (document.body.clientWidth ) ) {
		w = document.body.clientWidth;
	}
	return w;
}

function isset(obj) {
	if (typeof obj != "object")
		return (typeof obj != "undefined");
	for (var i in obj)
		return true;
	return false;
}

function splitCSV(s) {
	/* Split a Comma Separated Values string into an array of strings. Without thinking we might */
	/* use s.split(","); But we have to handle case where comma is legitimately inside a */
	/* quoted field... "a","b,c","d" must split into 3, not 4 pieces. This nasty looking regular */
	/* expression does the job s.split(/,(?=(?:[^\"]*\"[^\"]*\")*(?![^\"]*\"))/); and works on */
	/* firefox and safari, but fails on IE7/8 when "a",,"b". Should split to 3, IE splits to 2 */
	/* So, we roll our own here. */ 
	var result = [];
	var inQuote = false;
	var l = s.length;
	var i = 0
	var iStart = 0;
	for (i=0; i<l; i++) {
		var c = s.charAt(i);
		if (c == ',' && !inQuote) {
			/* found break point */
			result.push(s.substring(iStart,i));
			iStart = i+1;
		}
		else if (c == '"') inQuote = !inQuote;
	}
	/* got to end, push last value */
	if (iStart<=i) result.push(s.substring(iStart,i));
	return result;
}

function loadRecords(buildselected) {

	try {

	_$("longFields").checked = longversion;
	_$("showSystem").checked = showSystemCalls;
	_$("showOutbound").checked = showOutboundCalls;
	_$("showInbound").checked = showInboundCalls;
	_$("showInternal").checked = showInternalCalls;
	_$("showExternal").checked = showExternalCalls;

	var nCdrs = records.length;
	if (viewCount == nCdrs) viewCount = -1;
	ASTGUI.updateFieldToValue( _$("selectViewCount"),viewCount.toString() );
	if (viewCount == -1) viewCount = nCdrs;

	if (buildselected) { 
		recordsSelected = [];
		recordsSelected = recordsSelected.concat((showSystemCalls?recordsSystem:[]),(showOutboundCalls?recordsOutbound:[]),(showInboundCalls?recordsInbound:[]),(showInternalCalls?recordsInternal:[]),(showExternalCalls?recordsExternal:[]));
		nSelected = recordsSelected.length;
		recordsSelected.sort(sortCompare);
	}

	if (offset >= nSelected) offset = Math.max(0,nSelected-viewCount);

	var e = _$("cdr_content_container");
	e.innerHTML = "";
	
	var d = document.createElement("TABLE")
	d.id = "table_CDR_list";
	d.className = "table_CDR";
	d.cellSpacing = 0;

	if (offset == 0) _$("prevPageBtn").disabled = true;
	else _$("prevPageBtn").disabled = false;
	if (offset+viewCount >= nSelected) _$("nextPageBtn").disabled = true;
	else _$("nextPageBtn").disabled = false;

	_$("info").innerHTML = records.length + " Total records; Viewing " + (offset+1) + "-" + Math.min(offset+viewCount,nSelected) + " of " + nSelected +" Selected";

	var thead = document.createElement("thead");
	var tr = document.createElement("tr");
	if (longversion) tr.style.fontSize = "8pt";
	else tr.style.fontSize = "12pt";
	for(var i=0;i<=(longversion?maxColumns:(svindex.length-1));i++) {
		var th = document.createElement("th");
		var x = longversion?i:(svindex[i]+1);
		if ($(d).fixedHeader) th.setAttribute("onclick","javascript:setSortbyField("+x+")");
		else th.onclick=Function('setSortbyField("'+x+'")');	/* needed for IE with no fixedheader */
		if (x==sortby) th.style.textDecoration = "underline";
		else if (x==0-sortby) th.style.textDecoration = "overline";
		th.appendChild(document.createTextNode(fields[x]));
		tr.appendChild(th);
	}
	thead.appendChild(tr);
	d.appendChild(thead);

	var tbody = document.createElement("tbody");
	var c = viewCount;
	var n = 1; /* will use as row numbers (left most column) */
	for(var i=0;c--&&isset(recordsSelected[i+offset]);i++) {
		var r = recordsSelected[i+offset];
		var tr = document.createElement("tr");
		tr.className = (i%2)?"odd":"even";
		if (longversion) tr.style.fontSize = "8pt";
		else tr.style.fontSize = "12pt";
		if (r.duplicate) tr.style.color = "ff0000";
		if (r.uniqueID) tr.asteriskCDRuniqueID = r.uniqueID;
		tr.onclick=function(){rowClick(this);};

		for(var j=-1;j<(longversion?r.length:svindex.length-1);j++) {
			var td = document.createElement("td");
			if (j < 0) {	/* row numbers (first column) */
				td.style.textAlign = "right";
				td.appendChild(document.createTextNode(offset+n));
				n++;
			} else {
				td.appendChild(document.createTextNode(r[(longversion?j:(svindex[j+1]))]));
			}
			tr.appendChild(td);
		}
		tbody.appendChild(tr);
	}
	d.appendChild(tbody);
	e.appendChild(d);

	} catch(e) {
		alert(e);
	} 
	/* table is now built */

	/* Would like to size the width and height of the table to use up all the visible */
	/* space in the browser window... for optimum usability with scroll bars, etc. */
	/* but, don't make it any smaller than 300 pixels wide and 200 pixels high */
	/* -20's are to make room for scroll bars */
	var tableWidth = Math.max(VisibleWidth() - ASTGUI.domActions.findPos(e).cleft - 
				ASTGUI.domActions.findPos(this.parent.DOM_mainscreen).cleft - 20,
				300);
	var tableHeight = Math.max(VisibleHeight() - ASTGUI.domActions.findPos(e).ctop -
				ASTGUI.domActions.findPos(this.parent.DOM_mainscreen).ctop - 20,
				200);

	if ($(d).fixedHeader) {
		/* Add the fixed (will not scroll) header. */
		$(d).fixedHeader({ width: tableWidth, height: tableHeight});
	} 
	else {
		/* The fixedHeader jQuery plugin is missing */
		/* In this case just set the height of the container so that */
		/* we scroll within the visible window */
		e.style.width = tableWidth;
		e.style.height = tableHeight;
	}

	parent.ASTGUI.dialog.hide();
}

var localajaxinit = function() {

	_$('engine').innerHTML = backend;

	_$("longFields").onclick = function(){longversion=this.checked; loadRecords(false);};
	_$("showSystem").onclick = function(){showSystemCalls=this.checked; loadRecords(true);};
	_$("showOutbound").onclick = function(){showOutboundCalls=this.checked; loadRecords(true);};
	_$("showInbound").onclick = function(){showInboundCalls=this.checked; loadRecords(true);};
	_$("showInternal").onclick = function(){showInternalCalls=this.checked; loadRecords(true);};
	_$("showExternal").onclick = function(){showExternalCalls=this.checked; loadRecords(true);};
	_$("selectViewCount").onchange = function(){offset=0; viewCount=parseInt(this.value); loadRecords(false);};
	
	top.document.title = "CDR Viewer" ;

	var jc = context2json({filename: "cdr.conf", context: "general", usf:1 });
	
	if( jc.hasOwnProperty('enable') && !jc['enable'].isAstTrue() ) {
		alert("You do not have CDR enabled. Set enabled = yes in cdr.conf");
	}
	
	var c = context2json({ filename:'http.conf', context: 'general', usf:1 });

	if( c.hasOwnProperty('enablestatic') && !c['enablestatic'].isAstTrue() ) {
		alert("You do not have static file support in http.conf. Set 'enablestatic' = yes in http.conf");
	}

	parent.ASTGUI.dialog.waitWhile(' Grabbing your Records... ');

	parent.ASTGUI.systemCmd(top.sessionData.directories.script_mastercsvexists, function (){
		var content = ASTGUI.loadHTML("./Master.csv"); /* "./" is good enough. */
		records = content.split("\n");
		var intDest = parent.pbx.users.list();

		for(var i=0;i<records.length;i++) {
			records[i] = splitCSV(records[i]); /* cannot use records[i].split(","); */
			var nColumns = records[i].length;
			maxColumns = Math.max(maxColumns,nColumns);
			if (nColumns < 2) { 
				/* humm, line didn't contain enough number of commas */
				/* could be bogus data or blank line. Either way it will */
				/* cause problems later. We should delete the record */
				records.splice(i,1);
				/* now it is gone, but next record has moved up one index */
				/* so we need to decrement i so that we don't skip over it. */
				i--;
				/* Which will work unless the browser javascript for-loop */
				/* optimizer is buggy (records.length must be calculated each loop) */
				continue;
			}
			var r = records[i];
			var toExternal = false;
			var fromExternal = false;
			var toAndFromInternal = false;
			var toAndFromExternal = false;
			var systemCall = false;
			for (j=0; j<nColumns; j++) {
				/* Strips quotation marks from each record*/
				r[j] = r[j].toString().replace(/^[\"]{1}/, "").replace(/[\"]{1}$/, "");
				if ((j==12) || (j==13)) { /* duration or billable seconds */
					/* converts seconds to HH:MM:SS. */
					var s = parseInt(r[j]);
					var h = Math.floor(s/3600); s = s%3600;
					var m = Math.floor(s/60); s = s%60;
					r[j] = h+":"+(m<10?("0"+m):m)+":"+(s<10?("0"+s):s);
				}
				else if (j==4) { /* callerID, may have double quotation marks */
					r[j] = r[j].toString().replace(/\"\"/g, '\"');
				}
				else if (j==3) { /* destination context */
					if (r[j]=="asterisk_guitools") systemCall = true;
				}
				else if (j==5) { /* originating channel */
					var chanName = r[j].substring(r[j].indexOf('/')+1,r[j].lastIndexOf('-'));
					if (chanName.search('@default')>1) {
						/* system generated local calls may have @default after name */
						chanName = chanName.slice(0,chanName.indexOf('@'));
					}
					if (intDest.contains(chanName) || (chanName.length == 0)) toExternal = true;
					else fromExternal = true;
				}
				else if (j==6) { /* destination channel */
					var chanName = r[j].substring(r[j].indexOf('/')+1,r[j].lastIndexOf('-'));
					if (intDest.contains(chanName) || (chanName.length == 0)) toAndFromInternal = toExternal;
					else toAndFromExternal = fromExternal;
				}
				else if (j==16) { /* Unique ID */
					if (systemCall) {
						/* if we just flagged this record as a system call */
						/* there may be another record that is part of the same */
						/* system call. If so it will have the same "first" part */
						/* of unique ID, and the second part will be +/- 1 */
						/* This will most likely be the immediately prior record */
						/* which should be an "Internal" call */
						var UIDParts = r[j].split('.');
						if (recordsInternal.length>0) {
							var priorUIDParts = recordsInternal[recordsInternal.length-1][j].split('.');
							if (UIDParts[0] == priorUIDParts[0]) {
								recordsSystem.push(recordsInternal.pop());
								recordsSystem[recordsSystem.length-1].callType = "System";
							}
						}
					}
					r.uniqueID = r[j];
					var dup = uniqueIDs.indexOf(r.uniqueID);
					if (dup >= 0) {  /* another CDR record has identical uniqueID! */
						r.duplicate = dup;
						records[dup].duplicate = i;
					}
					uniqueIDs[i] = r.uniqueID;
				}
			}
			if (systemCall) { r.callType = "System"; recordsSystem.push(r); }
			else if (toAndFromInternal) { r.callType = "Internal"; recordsInternal.push(r); }
			else if (toAndFromExternal) { r.callType = "External"; recordsExternal.push(r); }
			else if (fromExternal) { r.callType = "Inbound"; recordsInbound.push(r); }
			else { r.callType = "Outbound"; recordsOutbound.push(r); }
			records[i] = r;
			
		}
		sortby = -10;	/* start date/time is 10th field in record, negative means descending */
		maxColumns = Math.min(maxColumns,fields.length-1); /* we don't understand any CDR fields */
						/* that are after the end of the fields[] array */
		loadRecords(true);
	});

}
