function updateFilterResultsHeader() {
  var settings = jQuery.getFacetSettings();
  var count = settings.currentResults.length;
  var resultText = (count > 1 || count == 0) ? "Results" : "Result";
  var finalText = count + " " + resultText;
  if (count > settings.paginationCount) {
    finalText = settings.paginationCount + " " + resultText + ' (out of ' + settings.currentResults.length + ')';
  }
  else {
    finalText = count + " " + resultText;
  }
  jQuery('#resultsHeader').text( finalText );

  jQuery('.facetlist .facetitem .facetitemcount').each( function(index, element) {
    var e = jQuery(element);
    var parent = e.closest('.facetitem');
    var value = e.text();
    if (value == '(0)') {
      parent.addClass('facetitem-empty');
    }
    else {
      parent.removeClass('facetitem-empty');
    }
  });
}

function facetUICreatedCommon() {
  jQuery('.bottomline').insertAfter('#facets');

  // Add a text filter box.
  jQuery('.facetsearch').each( function(index,element) {
    var title = jQuery(element).find('.facettitle');

    var input = jQuery('<input placeholder="Filter..." type="text"></input>');

    input.keyup( function() {
      var text = jQuery(this).val().toLowerCase();

      jQuery(this).closest('.facetsearch').find('.facetlist .facetitem').each( function(index, element) {
        var e = jQuery(element);
        // Don't include the count (the '(1)' stuff).
        var value = e.contents().get(0).nodeValue.toLowerCase();

        if (value.substr(0, text.length) == text) {
          e.show();
        }
        else {
          e.hide();
        }
      });


    });

    title.after(input);
  });
}
