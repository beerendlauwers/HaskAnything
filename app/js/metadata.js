function loadDropdowns() {
var dropdowns = [
        ["../../json/tags.json",'#tags'],
        ["../../json/libraries.json",'#libraries'],
        ];

    R.map ( R.apply(loadMetaDataDropdown), dropdowns );
}

function loadMetaDataDropdown( url, id ) {
    $.ajax( url ) // TODO: find a fix?
        .done( function(data) {
            var options = R.map( function(item) { return '<option value="' + item + '">' + item + '</option>'; } )( data );
            $(id).append(options);
            $(id).chosen({ width: '100%' });
            $(".chosen-container-multi").find("input").on('keydown', function (evt) {
                var stroke;
                stroke = (_ref = evt.which) != null ? _ref : evt.keyCode;
                if (stroke == 9) { // 9 = tab key
                    $(id).append('<option value="' + $(this).val() + '" selected="selected">' + $(this).val() + '</option>');
                    $(id).trigger('chosen:updated');
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