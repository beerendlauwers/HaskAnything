var gPageSize = 20;

function updateFilterResultsHeader() {

  // Update the results header counter.
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

  // Gray out facet items that no longer expand the selection.
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

    var input = jQuery('<input class="facet-filter" placeholder="Filter..." type="text"></input>');

    input.keyup( function() {
      var text = jQuery(this).val().toLowerCase();

      jQuery(this).closest('.facetsearch').find('.facetlist .facetitem').each( function(index, element) {
        var e = jQuery(element);

        // Don't include the count (the '(1)' stuff).
        var value = e.contents().get(0).nodeValue.toLowerCase();

        if (value.includes(text) || e.hasClass('activefacet')) {
          e.attr('data-filtered', true);
          e.show();
        }
        else {
          e.hide();
          e.removeAttr('data-filtered');
        }
      });


      buildPager( jQuery(this).closest('.facetsearch').find('.facetlist') );

      switchPager( jQuery(this).closest('.facetsearch').find('.facetlist'), 0);

    });

    title.after(input);
  });
}

function buildPager(facetList) {

    facetList.find('[data-filtered].facetitem').each( function( index, elem ) {
      var page = parseInt( (index / gPageSize) );

      jQuery(this).data( "on-page", page );
    });

    var count = facetList.find('[data-filtered].facetitem').length;

    facetList.find('.simple-pager').remove();

    facetList.prepend("<div class='simple-pager'></div>");

    for (var pager = 0; pager < (count / gPageSize); pager++) {
      facetList.find('.simple-pager').append('<a href="#" class="pager-page pager-page-' + pager + '" onclick="switchPagerFromButton(this,' + pager + ');">' + (pager + 1) + '</a>');
    }
}

function switchPagerFromButton(pageButton, pageNumber) {
  switchPager(jQuery(pageButton).closest('.facetlist'), pageNumber);
}

function switchPager(facetList, pageNumber) {

  var gPageSize = 10;

  jQuery(facetList).find('[data-filtered].facetitem').each( function( index, elem ) {

      var e = jQuery(elem);

      var idx = e.data( "on-page" );

      var page = idx % gPageSize;
      (pageNumber == page) ? e.show() : e.hide();
    });

  jQuery(facetList).closest('.facetsearch').data("current-page", pageNumber);
}
