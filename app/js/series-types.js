function loadSeriesTypes() {
var dropdowns = [
        ["../../json/series-types.json",'#type-series'],
        ];

    R.map ( R.apply(loadSingleDropdown), dropdowns );
}

function loadSingleDropdown( url, id ) {
    jQuery.ajax( url )
        .done( function(data) {

            // If the JSON doesn't contain "default" yet, add it.
            if ( !R.contains(data,"default") ) {
              data.push( "default" );
            }

            var options = R.map( function(item) { return '<option value="' + item + '">' + item + '</option>'; } )( data );
            jQuery(id).append(options);

            // Initialize select2 plugin.
            jQuery(id).select2({
            });

            // Select the default.
            jQuery(id + ' option:contains("default")').attr('selected',true);

            // Inform select2.
            jQuery(id).trigger('change.select2');

        }).fail(function(xhr, ajaxOptions, thrownError) {
            console.log("failure");
            console.log(xhr);
            console.log(ajaxOptions);
            console.log(thrownError);
        });
}

loadSeriesTypes();
