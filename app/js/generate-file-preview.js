// Fetch the form elements and funnel them into a handlebars template to get the final source file.
function generateFilePreview(templateName) {
    
    // Fetch elements
    var inputSelector = jQuery(':input').not('#submit-pull-request-preview, #title-proposed, #submit-pull-request-feedback');
    
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

function displayFilePreview(templateName,fileSelector,titleSelector) {
    jQuery(fileSelector).text( generateFilePreview(templateName) );
    jQuery(titleSelector).val( generateFileTitle(templateName) );
}

function generateFileTitle(templateName) {
    var str = jQuery('.title-suggestion').val();
    return templateName + "-" + str.replace(/\W+/g, '-').toLowerCase() + ".md";
}

function getFinalFileTitle(templateName, titleSelector) {
    var proposedTitle = generateFileTitle(templateName);
    
    var possiblyChangedTitle = jQuery(titleSelector).val();
    
    if (possiblyChangedTitle.trim() && possiblyChangedTitle !== proposedTitle) {
        return possiblyChangedTitle;
    }
    
    return proposedTitle;
}