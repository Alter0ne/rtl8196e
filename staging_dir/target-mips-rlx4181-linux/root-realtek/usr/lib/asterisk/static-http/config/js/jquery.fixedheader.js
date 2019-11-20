/**
 * Stupid Fixed Header 1.0.2- jQuery plugins to create a fixed headers
 * 
 * Require: jQuery 1.2.6
 * Author: Jacky See
 * Blog: http://jacky.seezone.net
 * email:  jackysee at gmail dot com
 * Dual licensed under the MIT and GPL licenses:
 *   http://www.opensource.org/licenses/mit-license.php
 *   http://www.gnu.org/licenses/gpl.html
*/

(function($){
	
	/* created fixed headers , require jquery dimenions plugins*/
	$.fn.fixedHeader = function(o){
		var s = {adjustWidth: $.fixedHeader.calcWidth};
		if(o) $.extend(s,o);
		
		return this.each(function(){
			var table = $(this); //table
			var tId = this.id;
			
			var scrollBarWidth = $.fixedHeader.getScrollBarWidth();
			var IE6 = $.browser.msie && $.browser.version == '6.0';
			
			//wrap a body container
			var bodyContainer = table.wrap('<div></div>').parent()
				.attr('id', tId + "_body_container")
				.css({
					width: s.width,
					height: s.height,
					overflow: 'auto'
				});
			
			//Wrap with an overall container
			var tableContainer = bodyContainer.wrap('<div></div>').parent()
				.attr('id', tId + '_table_container')
				.css('position','relative');

			//clone the header
			var headerContainer = $(document.createElement('div'))
				.attr('id', tId + '_header_container')
				.css({
					width:  bodyContainer.innerWidth() - scrollBarWidth,
					height: table.find('thead').outerHeight(), 
					overflow: 'hidden',
					top: 0, left:0
				})
				.prependTo(tableContainer);
			
			var headerTable = table.clone(true)
				.find('tbody').remove().end()
				.attr('id',tId + "_header")
				.addClass(s.tableClass || table[0].className)
				.css({
					//width: $.browser.msie? table.outerWidth():table.width(), 
					'table-layout':'fixed',
					position:'absolute',
					top:0, left:0
				})
				.append(table.find('thead').clone(true))
				.appendTo(headerContainer);
			
			//sync header width
			var headThs = headerTable.find('th');
			table.find('th').each(function(i){
				headThs.eq(i).css('width', s.adjustWidth(this));
			})
			
			//sync scroll
			var selects = IE6? table.find("select"): null;
			bodyContainer.scroll(function(){
				if(IE6 && selects.size()>0){
					selects.each(function(i){
						this.style.visibility =
							($(this).offset().top - bodyContainer.offset().top) <= table.find("thead").outerHeight() + 10
							? 'hidden':'visible';
					});
				}
				headerTable.css({
					left: '-' + $(this).scrollLeft() + 'px'
				});
			})
			
			//Move it down
			headerContainer.css({
				'position': 'absolute',
				'top': 0
			});
		});
	}
	
	$.fixedHeader = {
		calcWidth: function(th){
			var w = $(th).width();
			var paddingLeft = $.fixedHeader.getComputedStyleInPx(th,'paddingLeft');
			var paddingRight = $.fixedHeader.getComputedStyleInPx(th,'paddingRight');
			var borderWidth = $.fixedHeader.getComputedStyleInPx(th,'borderRightWidth');			
			if($.browser.msie) w = w+borderWidth;
			if($.browser.opera) w = w+borderWidth;
			if($.browser.safari) w = w+paddingLeft+paddingRight+borderWidth*2;
			if($.browser.mozilla && parseFloat($.browser.version) <= 1.8) w=w+borderWidth; //FF2 still got a border-left missing problem, this is the best I can do.
			return w;
		},
		getComputedStyleInPx: function(elem,style){
			var computedStyle = (typeof elem.currentStyle != 'undefined')
				?elem.currentStyle
				:document.defaultView.getComputedStyle(elem, null);
			var val = computedStyle[style];
			val = val? parseInt(val.replace("px","")):0;
			return (!val || val == 'NaN')?0:val;
		},
		getScrollBarWidth: function() { //calculate or get from global the scroll bar width
			if(!$.fixedHeader.scrollBarWidth){ 
				var inner = $(document.createElement('p')).css({width:'100%',height:'100%'});
				var outer = $(document.createElement('div'))
					.css({
						position:'absolute',
						top: '0px',
						left: '0px',
						visibility: 'hidden',
						width: '200px',
						height: '150px',
						overflow: 'hidden'
					})
					.append(inner)
					.appendTo(document.body);
				
				var w1 = inner[0].offsetWidth;
				outer[0].style.overflow = 'scroll';
				var w2 = inner[0].offsetWidth;
				if (w1 == w2) w2 = outer[0].clientWidth;
				document.body.removeChild (outer[0]);
				$.fixedHeader.scrollBarWidth = (w1 - w2);
			}
			return $.fixedHeader.scrollBarWidth;
		}
	}
	
})(jQuery);