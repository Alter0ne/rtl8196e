/*
 * jquery.ejs.js - Ejscript jQuery support
 * http://www.ejscript.com/
 *
 * Copyright (c) 2009 Embedthis Software
 * Dual licensed under GPL licenses and the Embedthis commercial license.
 * See http://www.embedthis.com/ for further licensing details.
 */

(function($) {

    /* Non chainable functions */
    jQuery.extend({
        elog: function(msg) {
            if (window.console) {
                console.debug(msg);
            } else alert(msg);
        }
    });

    /* Chainable functions */
    jQuery.fn.extend({
        /*
         *  Table control client side support
         */
        eTable: function(settings) {
            var $table = $(this);

            /*
             *  Blend options with defaults and save
             */
            var options = { refresh: 60000, sort: null, sortOrder: "ascending" };
            $.extend(options, settings);
            options.dynamicUpdate = options.url ? true: false;

            $table.data("options", options);

            function applyTableData(data) {
                options.sortConfig = $table[0].config;
                $table.replaceWith(data);
                $table = $("#" + $table.get(0).id);
                $table.data("options", options);
                sortTable($table, options)
            };

            /*
             *  Timeout function to update the table data contents
             */
            function updateTable() {
                if (!options.dynamicUpdate) {
                    setTimeout(function() { updateTable.apply($table);}, options.refresh);
                } else {
                    $.ajax({
                        url: options.url,
                        cache: false,
                        type: "GET",
                        error: function (http, msg, e) { $.elog("Error updating table control: " + msg); },
                        success: function (data) { applyTableData(data); },
                        complete: function() {
                            setTimeout(function() { updateTable.apply($table);}, options.refresh);
                        },
                    });
                }
            };

            /*
             *  Get dynamic data immediately, but don't sort until the data arrives
             */
            sortTable($table, options);
            if (options.refresh > 0) {
                setTimeout(function() { updateTable.apply($table);}, options.refresh);
            } 
            return this;
        },

        /*
         *  Toggle dynamic refresh on/off
         */
        eTableToggleRefresh: function() {
            var options = $(this).data("options");
            options.dynamicUpdate = !options.dynamicUpdate;
            var image = $(".-ejs-table-download", $(this)).get(0);
            if (options.dynamicUpdate) {
                $.get(options.url, function (data) { applyTableData(data); });
                image.src = image.src.replace(/red/, "green");
            } else {
                image.src = image.src.replace(/green/, "red");
            }
            return this
        },

        /*
         *  Define table sort options. May be called in the initial page HTML or may be called via the Ajax response.
         */
        eTableSetOptions: function(settings) {
            var options = this.data("options");
            $.extend(options, settings);
            return this
        },

    });

    function sortTable($table, options) {
        if (options.sort) {
            if (!options.sortConfig) {
                var el = $("th", $table).filter(':contains("' + options.sort + '")').get(0);
                if (el) {
                    options.sortConfig = {sortList: [[el.cellIndex, (options.sortOrder.indexOf("asc") >= 0) ? 0 : 1]]};
                } else options.sortConfig ={sortList: [[0, 0]]};
            }
            $table.tablesorter(options.sortConfig);
        }
    }
})(jQuery);

