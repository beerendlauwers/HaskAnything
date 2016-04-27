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
            jQuery(id).chosen({ width: '100%' });
            jQuery(".chosen-container-multi").find("input").on('keydown', function (evt) {
                var stroke;
                stroke = (_ref = evt.which) != null ? _ref : evt.keyCode;
                if (stroke == 9) { // 9 = tab key
                    jQuery(id).append('<option value="' + jQuery(this).val() + '" selected="selected">' + jQuery(this).val() + '</option>');
                    jQuery(id).trigger('chosen:updated');
                    evt.preventDefault();
                }
            });
        }).fail(function(xhr, ajaxOptions, thrownError) {
            console.log("failure");
            console.log(xhr);
            console.log(ajaxOptions);
            console.log(thrownError);
        });
}

loadDropdowns();