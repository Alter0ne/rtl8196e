/*
 * Asterisk-GUI	- an Asterisk configuration interface
 *
 * feditor.html functions
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
var global_contextBeingEdited = ""; 
var global_fileBeingEdited = "";

var show_createfile = function(){
	var f = ASTGUI.domActions.findPos(_$('createfile_button')) ;
	_$('CreateFile').style.left = f.cleft ;
	_$('CreateFile').style.top = f.ctop ;
	_$('CreateFile').style.display = "" ;
	_$('New_FileName').value = "";
	_$('New_FileName').focus();
};

var create_file = function(){
	var fn = _$('New_FileName').value;
	if( !fn.endsWith('.conf') ) { fn = fn+'.conf'; }

	ASTGUI.miscFunctions.createConfig( fn, function(){
		cancel_file();
		ASTGUI.selectbox.append(_$('filenames'),fn, fn);
		_$('filenames').selectedIndex = _$('filenames').options.length -1 ;
		loadfile();
	});

};

var cancel_file = function(){
	_$('CreateFile').style.display = "none" ;
};

var delete_context = function(){
	if(!confirm("Are you sure you want to delete the selected context ?")){ return true; }
	var u = new listOfSynActions(global_fileBeingEdited) ;
	u.new_action('delcat', global_contextBeingEdited, '', '');
	u.callActions();
	try{
		_$('AddContext').style.display = "none";
		_$('div_editcontext').style.display = "none"; 
		global_contextBeingEdited = "";
		ASTGUI.feedback( { msg:'context deleted', showfor:2 });
		var t = config2json( { filename:global_fileBeingEdited , usf:0 } )
		fileparsed(t);
	}catch(err){}
};

var cancel_context = function(){
	global_contextBeingEdited = "";
	_$('div_editcontext').style.display = "none";
};

var update_context = function(){ // rename from global_contextBeingEdited to $('context_edited').value
	var u = new listOfSynActions(global_fileBeingEdited) ;
	u.new_action('renamecat', global_contextBeingEdited, '', _$('context_edited').value );
	u.callActions();
	_$('AddContext').style.display = "none";
	_$('div_editcontext').style.display = "none"; 
	global_contextBeingEdited = "";
	ASTGUI.feedback( { msg:'Context Updated', showfor:2 });
	var t = config2json( { filename:global_fileBeingEdited , usf:0 } )
	fileparsed(t);
};

var localajaxinit = function(){
	top.document.title = "File Editor";
	parent.ASTGUI.dialog.waitWhile('loading list of filenames ..');
	ASTGUI.selectbox.append(_$('filenames'),"Config Files", "");
	_$('filenames').options[0].style.fontWeight = "bold";
	parent.ASTGUI.listSystemFiles( top.sessionData.directories.asteriskConfig , function(listOfFiles) {
		try{
			listOfFiles.each( function( file ) {
				if( file.endsWith('.conf') ){ ASTGUI.selectbox.append( _$('filenames'), file , file ); }
			});
		}catch(err){

		}finally{
			parent.ASTGUI.dialog.hide();
		}
	});
	ASTGUI.events.add( _$('filenames') , 'change' , loadfile );
};

var loadfile = function(){
	if( !_$('filenames').value ) return;
	global_fileBeingEdited = _$('filenames').value ;
	_$('AddContext').style.display = "none";
	_$('div_filename').style.display = "";
	_$('CurrentFileName').innerHTML = global_fileBeingEdited;
	var t = config2json({ filename: global_fileBeingEdited , usf:0 });
	fileparsed(t);
};


var showeditcontextContent = function(event){
	var t = (event.srcElement)?event.srcElement:this;
	_$('AddContext').style.display = "none";
	_$('div_editcontext').style.display = "none";
	var i_id = t.getAttribute('id') ;
	global_contextBeingEdited = t.getAttribute('context');
	_$(i_id).insertBefore(_$('div_editcontextContent'), null );
	_$('div_editcontextContent').style.display = "";
	_$('context_Content').value = t.CONTEXTCONTENT ;
	_$('context_Content').rows = t.CONTEXTCONTENT_ROWS + 3;
	//_$('context_edited').size = f.length;
	_$('context_Content').focus();
};


var cancel_contextContent = function(){
	global_contextBeingEdited = "";
	_$('div_editcontextContent').style.display = "none";
};


var update_contextContent = function(){
	parent.ASTGUI.dialog.waitWhile('Saving Changes ...');
	var after_emptyContext = function(){
		var x = new listOfActions(global_fileBeingEdited);
		var u = _$('context_Content').value.split("\n") ;
		u.each(function(line){
			var y = ASTGUI.parseContextLine.read(line);
			if(y.length)
				x.new_action('append', global_contextBeingEdited , y[0], y[1] );
		});
		var cb = function(){
			_$('div_editcontextContent').style.display = "none"; 
			global_contextBeingEdited = "";
			try{
				var t = config2json( { filename:global_fileBeingEdited , usf:0 } );
				fileparsed(t);
			}finally{
				parent.ASTGUI.dialog.hide();
			}
		};
		x.callActions(cb) ;
	};
	ASTGUI.miscFunctions.empty_context({ filename: global_fileBeingEdited, context: global_contextBeingEdited, cb: after_emptyContext });
};


var fileparsed = function(c){
	_$('temp_holding').insertBefore(_$('div_editcontext'), null );
	_$('temp_holding').insertBefore(_$('div_editcontextContent'), null );
	_$('div_editcontext').style.display = "none";
	_$('div_editcontextContent').style.display = "none";

	var zz = _$('file_output');
	var p = "";
	var rows ;
	ASTGUI.domActions.removeAllChilds (zz);

	for( var d in c ){
		if ( c.hasOwnProperty(d) ) {
			var h = document.createElement("div");
			var h_id = "context_" + d;
			h.setAttribute("id",h_id);
			h.setAttribute("context",d);
			h.align="left";
			h.style.backgroundColor = '#4D5423';
			h.style.color = '#FFFFFF';
			h.style.marginTop = '15px' ;
			h.style.width = '95%';
			h.style.fontFamily = "'trebuchet ms',helvetica,sans-serif";
			h.style.fontSize = '10pt' ;
			h.style.padding = '2px 2px 3px 3px' ;
			//h.innerHTML = "&nbsp;&nbsp;[" + d + "]";
			(function(){
				var sp_0 = document.createElement("span");
				sp_0.innerHTML = "&nbsp;&nbsp;+&nbsp;&nbsp;";
				sp_0.className = 'spzero';
				sp_0.id = 'context_spanOne_' + d ;
				h.appendChild(sp_0);

				var sp_1 = document.createElement("span");
				sp_1.innerHTML = "&nbsp;[" + d + "]";
				sp_1.className = 'spzero';
				sp_1.id = 'context_spanTwo_' + d ;
				h.appendChild(sp_1);

				$(sp_1).click( function(event){
					var t = (event.srcElement)?event.srcElement : this;
					event.cancelBubble = true;
					if (event.stopPropagation) event.stopPropagation();

					t.parentNode.insertBefore( _$('div_editcontext'), null );
					global_contextBeingEdited = this.id.lChop('context_spanTwo_');
					_$('AddContext').style.display = "none";
					_$('div_editcontextContent').style.display = "none";
					_$('div_editcontext').style.display = "";
					_$('context_edited').value = global_contextBeingEdited ;
					_$('context_edited').size = global_contextBeingEdited.length;
					_$('context_edited').focus();

				});

				$(h).click(function(){
					var context_name = this.id.lChop('context_');
					var tmp_i_id = "contextContent_" + context_name ;
					var r = this.childNodes[0] ;
					if( r.innerHTML.contains('-') ){
						r.innerHTML = "&nbsp;&nbsp;+&nbsp;&nbsp;";
						$('#'+tmp_i_id).hide();
					}else if( r.innerHTML.contains('+') ){
						r.innerHTML = "<font size='+2'>&nbsp;&nbsp;-&nbsp;&nbsp;</font>";
						$('#'+tmp_i_id).show();
					}
				});
			})();
			zz.appendChild(h);

			var i = document.createElement("div");
			var i_id = "contextContent_" + d;
			i.setAttribute("id", i_id );
			i.setAttribute("context",d);
			i.align= "left";
			i.style.backgroundColor = '#E0E6C4';
			i.style.marginTop = '5px' ;
			i.style.width = '95%';
			i.style.fontSize = '9pt' ;
			i.style.padding = '2px 2px 3px 3px' ;
			i.style.fontFamily = 'courier' ;

			var temp_contextContent = "" ;
			rows = 0;
			if(c[d].length == 0){i.innerHTML += "&nbsp;&nbsp;<BR>" ;}
			for(var r=0; r < c[d].length ; r++ ){
				p = unescape( c[d][r] );
				i.innerHTML += "&nbsp;&nbsp;" + p.replace(/</g, '&lt;').replace(/>/g, '&gt;') + "<BR>" ;
				temp_contextContent += p + "\n";
				rows++;
			}
			
			i.CONTEXTCONTENT = temp_contextContent ;
			i.CONTEXTCONTENT_ROWS = rows ;
			i.style.display = 'none' ;
			zz.appendChild(i);
			//Rico.Corner.round("contextContent_" + d, {compact:true});
			ASTGUI.events.add( _$(i_id) , 'click', showeditcontextContent );
		}
	}
}


var stopBubble = function(b){
    if (!b) { b = window.event; }
    b.cancelBubble = true;
};

var show_addcontext = function(){
	var acb = ASTGUI.domActions.findPos( _$('AddContextButton') ) ;
	_$('AddContext').style.left = acb.cleft;
	_$('AddContext').style.top = acb.ctop ;
	_$('AddContext').style.display = "" ;
	_$('New_ContextName').value = "";
	_$('New_ContextName').focus();
};


var cancel_addcontext = function(){
	_$('AddContext').style.display = "none";
	_$('New_ContextName').value = "";
};

var add_context = function(){
	var u = new listOfSynActions(global_fileBeingEdited) ;
	u.new_action('newcat', _$('New_ContextName').value , '', '');
	u.callActions();
	ASTGUI.feedback( { msg:'Context Added', showfor:2 });
	cancel_addcontext();
	global_contextBeingEdited = "";
	var t = config2json( { filename:global_fileBeingEdited , usf:0 } );
	fileparsed(t);

};
