// Fetch the form elements and funnel them into a handlebars template to get the final source file.
function generateFilePreview(templateName) {
    
    // Fetch elements
    var inputSelector = jQuery(':input');
    
    var allValues = inputSelector.map(function() { return jQuery(this).val(); } );
    var allNames = inputSelector.map(function() { return jQuery(this).attr('id'); } );
    
    // Turn into an object
    var zipped = R.zip( allNames, allValues );
    
    var objs = R.map( function(item) { var obj = {}; obj[item[0]] = item[1]; return obj; }, zipped);
    
    var context = R.reduce( jQuery.extend, {}, objs );
    
    // Handlebars
    var source = jQuery("#handlebars-template").text();
    var template = Handlebars.compile(source);
    
    var preview = template(context);
    
    // Return
    return preview;
}

function displayFilePreview(templateName,selector) {
    jQuery(selector).text( generateFilePreview(templateName) );
}