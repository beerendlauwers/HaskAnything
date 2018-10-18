// Fetch the form elements and funnel them into a handlebars template to get the final source file.
function generateFilePreview(templateName) {

    // Fetch elements
    var inputSelector = jQuery('.web-submit-element').find(':input').not('.select2-search__field').not('.btn-default').not('.btn');

    var selectors = jQuery(inputSelector).toArray();

    var getValue = function(selector) {

        var v = jQuery(selector).val();

        // Naturally, checkboxes have some special behaviour that we have to work around.
        if ( jQuery(selector).is(':checkbox') ) {
          v = jQuery(selector).is(':checked');
        }

        var dataIsArray = jQuery(selector).closest('.web-submit-element').hasClass('data-is-array');
        //console.log("selector " + selector.id + " is array? " + dataIsArray);

        if (v && typeof v === 'string' && dataIsArray) {
          v = v.split(',');
        }

        //console.log("selector " + selector.id + " : " + v);
        //console.log(v);

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

    //console.log(objs);

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

    // Get the markdown instance.
    var m = jQuery(fileSelector).data('markdown');

    // Set the preview function so we can do some post-processing.

    // NOTE: Hakyll goes through the JS files as well for processing fields,
    // so that's why we need to add another dollar sign for 'options' -
    // this escapes it.
    m.$options.onPreview = function(e) {
      var originalContent = e.getContent();

      // For some reason the preview button gets disabled again.
      setTimeout( function() {
        m.showButtons('cmdPreview');
        m.enableButtons('cmdPreview');
      }, 30 );

      var preview = normalize(originalContent).trim();

      if (preview.length == 0) {
        preview = '<i>(The file has no body.)</i>';
      }

      return preview;
    };

    // Enable the preview button on the Markdown preview.
    m.showButtons('cmdPreview');
    m.enableButtons('cmdPreview');
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
