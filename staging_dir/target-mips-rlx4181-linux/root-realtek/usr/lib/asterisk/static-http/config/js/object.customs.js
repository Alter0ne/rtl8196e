/*
* Asterisk-GUI	- an Asterisk configuration interface
*
* Javascript functions for accessing manager over HTTP and Some UI components/functions used in AsteriskGUI.
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

// Some custom methods to Array Objects
Array.prototype.replaceAB = function(a, b) { // return new array with all instances of a replaced with b
	var x =[];
	for(var i=0, j = this.length ; i < j; i++ ){
		if( this[i] === a ){
			x.push(b);
		}else{
			x.push(this[i]);
		}
	}
	return x;
};

Array.prototype.lastValue = function(){
	// [0,1,2]
	return (this.length)? this[this.length - 1] : null;
};

Array.prototype.replaceLastWith = function(a){
	if( this.length )
		this[this.length - 1] = a ;
}

Array.prototype.contains = function(str) {
	return this.indexOf(str) != -1 ;
};

Array.prototype.containsLike = function(str) {
	return this.indexOfLike(str) != -1;
};

Array.prototype.each = function(iterator) {
	for(var i=0 , j = this.length; i < j ; i++ ){
		iterator(this[i] , i);
	}
};

Array.prototype.forEach = function(iterator) { // call a function on each element and update the element with the returned value
	var i = this.length;
	while (i--) {
		this[i] = iterator(this[i] , i);
	}
};

Array.prototype.firstAvailable = function(start) {
	start = (!start)? 1 : Number( start );
	if(!this.length)
		return start;
	for( var y=0, x=[], z=this.length ; y < z ; y++ ){
		var NT = Number(this[y]) ;
		if( NT < start )
			continue;
		x.push(NT);
	}
	if( !x.length )
		return start;
	while(true){
		if( x.contains(start) ){
			start++;
		}else{
			return start;
		}
	}
};

Array.prototype.removeFirst = function(){ // opposite of push - removes the first element of the array
	this.splice(0,1);
};

Array.prototype.removeLast = function(){ // removes the last element of the array
	this.pop();
};

if(!Array.indexOf){
	Array.prototype.indexOf = function(a){
		var i = this.length;
		while (i--) {
			if( this[i] === a ){
				return i;
			}
		}
		return -1;
	}
}

Array.prototype.indexOfLike = function( searchString ){
	if(!searchString.length){ return -1; }
	for(var i=0; i < this.length; i++ ){ if( this[i].beginsWith(searchString) ){ return i ; } }
	return -1 ;
};

Array.prototype.lastIndexOfLike = function( searchString ){
	if(!searchString.length){ return -1;}
	var i = this.length;
	while (i--) {
		if( typeof this[i] == 'string' && this[i].beginsWith(searchString) ){ return i; }
	}
	return -1 ;
};

Array.prototype.push_IfNotPresent = function( a ){
	if(!this.contains(a)) this.push(a);
};

Array.prototype.sortNumbers = function() {
	return this.sort(function(a,b){return a - b});
};

Array.prototype.withOut = function(e) {
	var x =[];
	if( typeof e == 'string' || typeof e == 'number' ){
		var y = [e];
	}else if( e instanceof Array ){
		var y = e;
	}else{
		return this;
	}

	for( var a =0 ; a < y.length ; a++ ){
		var b = y[a];
		for( var i=0, j=this.length ; i < j ; i++ ){
			if( !(this[i] === b) && !y.contains(this[i]) && !x.contains(this[i]) ){
				x.push(this[i]);
			}
		}
	}

	return x ;
};

/******************************************
 * Custom methods for Javascript's Object
 *****************************************/
/**
 * Get Properties As String.
 * Gets Object's properties and returns them as a string. Use to be ASTGUI.getObjectPropertiesAsString.
 * Its important to note that the Object shouldn't EVER be prototype'd, it breaks Object as a hash.
 * @returns a string of the object's properties
 */
getProperties = function(obj) {
	var props = [];

	for (var d in obj) {
		if (!obj.hasOwnProperty(d)) {
			continue;
		}

		if (typeof obj[d] === 'object') {
			if (obj[d] instanceof Array) {
				props.push(d + ': [' + obj[d].join(',') + ']');
			} else {
				props.push(d + ': ' + getProperties(obj[d]));
			}
		} else {
			props.push(d + ': ' + obj[d]);
		}
	}
	return '{' + props.join(' ,') + '}';
};

var getProperty = function(obj, p) {
	return (obj.hasOwnProperty(p)) ? obj[p] : '';
};

// String Manipulation, and other custom methods for String Objects
String.prototype.addZero = function(){
	return ( Number(this) < 10)? "0" + this : this;
};

String.prototype.afterChar = function(k){
	if(k.length > 1){ alert('String.afterChar() should be used with a single character'); return null;}
	var v = this.indexOf(k);
	if( v == -1){ return ''; }
	return this.substring(v+1);
};

String.prototype.afterStr = function(x){
	if( !this.contains(x) ){ return ''; }
	if(x.length == 1){ return this.afterChar(x); }
	var pos = this.indexOf(x) + x.length ;
	return this.substr(pos);
};

String.prototype.beforeChar = function(k){
	if(k.length > 1){ 
		alert('String.beforeChar() should be used with a single character');
		return null;
	}
	var v = this.indexOf(k);
	if( v == -1){ return ''; }
	return this.substring(0,v);
};

String.prototype.beforeStr = function(x){
	var r = this.afterStr(x);
	return this.withOut(x+r);
};

String.prototype.beginsWith = function(a){
	return this.length>=a.length && this.substring(0,a.length)==a
};

String.prototype.betweenXY = function(X,Y){
	if(X.length > 1 || Y.length > 1){ alert('String.betweenXY() accepts single character arguments'); return null;}
	var t = this.afterChar(X);
	return t.beforeChar(Y);
};

String.prototype.bold_X = function(x){
	if(x==''){return this ;}
	var position = this.toLowerCase().indexOf( x.toLowerCase() ) ;
	if (position == -1){ return this; }
	var c = this.substr( position , x.length );
	return  this.replace( c, "<B>" + c + "</B>" , "" );
};

String.prototype.camelize = function(){
    var parts = this.split(' '), len = parts.length;
	var camelized = '';
    for (var i = 0; i < len; i++)
      camelized += parts[i].charAt(0).toUpperCase() + parts[i].substring(1) + ' ';
    return camelized;
};

String.prototype.capitalizeFirstChar = function() {
	return this.charAt(0).toUpperCase() + this.substring(1).toLowerCase();
};

String.prototype.contains=function(a){
	return this.indexOf(a)!=-1;
};

String.prototype.endsWith=function(a){
	return this.length >= a.length && this.substring(this.length-a.length)==a
};

String.prototype.escapeHTML = function() {
	var a = document.createTextNode(this);
	var b = document.createElement('div');
	b.appendChild(a);
	return b.innerHTML;
};

String.prototype.isAstTrue = function () {
	return ["yes", "true", "y", "t", "1", "on"].contains(this.toLowerCase().trim());
};

String.prototype.getNoOp = function(){
	return ( this.toLowerCase().indexOf('noop(') == -1 ) ? '' : this.betweenXY('(',')') ; // todo: handle multiple ')'s
};

String.prototype.guiMetaData = function(){
	return this + ' ; GUI metadata';
};

String.prototype.isValueInBetween = function (a,b) {
	a = Number(a);
	b = Number(b);
	var c = Number(this) , a1 = Math.min(a,b) , b1 = Math.max(a,b);
	return ( c >= a1 && c <= b1 ) ? true : false ;
};

String.prototype.lChop = function(c){ // chop a string from the beginning of the string
	if(this.beginsWith(c)){
		return this.substr(c.length);
	}
	return this;
};

String.prototype.rChop = function(c){ // chop a string from the end of the string
	if( this.indexOf(c) == -1 || !this.endsWith(c) ){
		return String(this); //actually we should be doing 'return this;' but for some reason firebug is reporting the returned string as an object
	}
	return this.substr( 0, this.length - c.length);
};

String.prototype.replaceXY = function(X,Y){
	return this.split(X).join(Y);
};

String.prototype.nl2br = function(){ // replace new lines with <BR>
	return this.split('\n').join('<BR>');
};

String.prototype.strip = function(){
	try {
		return this.replace(/^\s+|\s+$/g, "");
	} catch(e) {
		return s;
	}
};

String.prototype.times = function(a){
	return ( a < 1 ) ? '' : new Array(a+1).join(this);
};

String.prototype.stripTags = function() {
	return this.replace(/<\/?[^>]+>/gi, '');
}

String.prototype.trim = function(){ // alias to strip
	/* Thanks to Steve Levithan (http://stevenlevithan.com) for this code */
	var str = this.replace(/^\s\s*/, ''),
		ws = /\s/,
		i = str.length;
	while (ws.test(str.charAt(--i)));
	return str.slice(0, i+1);
};

String.prototype.withOut = function(k){
	return this.split(k).join('');
};

/**
 * Validates Dates
 * validates that this string is of formats: '05' or '02-18'
 * @return boolean
 */
String.prototype.valiDate = function() { /* get it?? */
	if (this.length > 5) {
		/* max format length is '12-31', 5 chars */
		return false;
	} else if (this.length > 2 && this[2] != '-') {
		return false;
	}

	var splits = this.split('-');

	if (splits.length > 1) {
		/* when parsing dates, make sure you are using base 10
		 * parseInt likes to think in octals when numbers
		 * have leading zeros like, 05 */
		var month = parseInt(splits[0], 10);
		var day = parseInt(splits[1], 10);
	} else {
		var day = parseInt(this, 10);
	}

	if (month && (month > 12 || month < 0)) {
		return false;
	}

	if (day && (day < 0 || day > 31)) {
		return false;
	}

	return true;
};


Number.prototype.addZero = function(){
return ( this < 10)? "0" + String(this) : String(this);
};

Number.prototype.isValueInBetween = function (a,b) {
a = Number(a);
b = Number(b);
var a1 = Math.min(a,b) , b1 = Math.max(a,b);
return ( this >= a1 && this <= b1 ) ? true : false ;
};

Number.prototype.guiMetaData = function(){
return String(this) + ' ; GUI metadata';
};
