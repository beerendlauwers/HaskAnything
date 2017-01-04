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
}
