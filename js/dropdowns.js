function loadDropdown( select2Settings, url, id ) {
    jQuery.ajax( url )
        .done( function(data) {
            var options = R.map( function(item) { return '<option value="' + item + '">' + item + '</option>'; } )( data );
            jQuery(id).append(options);

            // Initialize select2 plugin.
            jQuery(id).select2(select2Settings);

        }).fail(function(xhr, ajaxOptions, thrownError) {
            console.log("failed to load dropdown " + id);
            console.log(xhr);
            console.log(ajaxOptions);
            console.log(thrownError);
        });
}

function loadDropdownWith( onDoneCallback, url, id ) {
  jQuery.ajax( url )
      .done(onDoneCallback).fail(function(xhr, ajaxOptions, thrownError) {
          console.log("failed to load dropdown " + id);
          console.log(xhr);
          console.log(ajaxOptions);
          console.log(thrownError);
      });
}

function loadDropdowns( dropDowns, select2Settings ) {
  // Turn loadDropdown into a curried function.
  var f = R.curry(loadDropdown);

  jQuery(document).ready(function() {
    // Map over the dropdowns, passing in the select2 settings.
    R.map ( R.apply( f( select2Settings ) ), dropDowns );
  });
}
