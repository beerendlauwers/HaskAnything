// Fetch the form elements and funnel them into a handlebars template to get the final source file.
function generateFilePreview(templateName) {

    // Fetch elements
    var inputSelector = jQuery('.web-submit-element').find(':input').not('.select2-search__field').not('.btn-default').not('.btn');

    var selectors = jQuery(inputSelector).toArray();

    var getValue = function(selector) {
        var v = jQuery(selector).val();

        console.log("selector " + selector.id + ": " + v);

        return v ? v : "";
    };

    var allValues = [];
    jQuery(inputSelector).each( function( idx ) {
        var v = getValue(this);
        allValues.push( v );
    });

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

function setAvailabilityPullRequest( value ) {
    jQuery('#submit-pull-request-button').prop('disabled', !value );
}


function getFinalFileTitle(templateName, titleSelector) {
    var proposedTitle = generateFileTitle(templateName);

    var possiblyChangedTitle = jQuery(titleSelector).val();

    if (possiblyChangedTitle.trim() && possiblyChangedTitle !== proposedTitle) {
        proposedTitle = possiblyChangedTitle;
    }

    // Put it in the correct directory.
    if (templateName == "permission-file") {
      proposedTitle = "app/permissions/" + proposedTitle;
    }
    else {
      proposedTitle = "app/content/" + templateName + "/" + proposedTitle;
    }


    return proposedTitle;
}
