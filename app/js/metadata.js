function loadDropdowns() {
var dropdowns = [
        ["../../json/tags.json",'#tags'],
        ["../../json/libraries.json",'#libraries'],
        ];

    R.map ( R.apply(loadMetaDataDropdown), dropdowns );
}

function loadMetaDataDropdown( url, id ) {
    jQuery.ajax( url )
        .done( function(data) {
            var options = R.map( function(item) { return '<option value="' + item + '">' + item + '</option>'; } )( data );
            jQuery(id).append(options);
            
            // Initialize select2 plugin.
            jQuery(id).select2({
              tags: true,
              tokenSeparators: [',']
            });
        }).fail(function(xhr, ajaxOptions, thrownError) {
            console.log("failure");
            console.log(xhr);
            console.log(ajaxOptions);
            console.log(thrownError);
        });
}

jQuery(document).ready( loadDropdowns );