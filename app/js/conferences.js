function loadConferences() {
var dropdowns = [
        ["../../json/conferences.json",'#conference-presentation'],
        ];

    R.map ( R.apply(loadSingleDropdown), dropdowns );
}

function loadSingleDropdown( url, id ) {
    jQuery.ajax( url )
        .done( function(data) {
            var options = R.map( function(item) { return '<option value="' + item + '">' + item + '</option>'; } )( data );
            jQuery(id).append(options);
            jQuery(id).chosen({ width: '100%' });
        }).fail(function(xhr, ajaxOptions, thrownError) {
            console.log("failure");
            console.log(xhr);
            console.log(ajaxOptions);
            console.log(thrownError);
        });
}

loadConferences();