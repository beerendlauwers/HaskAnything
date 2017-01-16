function loadSeriesTypes() {
  var dropdowns = [
    ["../../json/series-types.json",'#type-series'],
  ];

  var id = '#type-series';

  var onDropdownCallback = function(data) {

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

  };

  // Turn loadDropdownWith into a curried function.
  var f = R.curry(loadDropdownWith);

  f = f(onDropdownCallback);

  jQuery(document).ready(function() {
    // Map over the dropdowns.
    R.map ( R.apply( f ), dropdowns );
  });
}

loadSeriesTypes();
