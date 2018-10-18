function Lookup(urlInspector, urlConstructor, messageValidator, messageAccessor, urlCaller, messageRenderer) {

    /* String -> a */
    this.urlInspector = urlInspector;
    
    /* a -> String */
    this.urlConstructor = urlConstructor;
    
    /* JSON -> Bool */
    this.messageValidator = messageValidator;
    
    /* JSON -> String */
    this.messageAccessor = messageAccessor;
    
    /* String -> JSON */
    if (typeof urlCaller === 'function') {
        // If this isn't set, we use the default urlCaller defined below.
        this.urlCaller = urlCaller;
    }
    
    /* IO () */
    this.messageRenderer = messageRenderer;
}

var messageValid = false;

// The default urlCaller is an AJAX request that fetches something from the received message and renders it.
Lookup.prototype.urlCaller = function(url,onDone,onFail) {
    var validator = this.messageValidator;
    var accessor = this.messageAccessor;
    var renderer = this.messageRenderer;
    console.log("AJAX to: " + url);
    jQuery.ajax( url )
        .done( function(msg) {

            if (typeof onDone === 'function') {
                onDone(msg);
            }
            
            // Let's validate it first.
            if (validator(msg)) {
            
                // It's valid! Let's fetch the interesting bits.
                var content = accessor(msg);
                
                // Do something with the interesting bits so we get something on screen.
                renderer( content );

                messageValid = true;
            }
            else {
                jQuery('#feedback').hide().html("Message was invalid. See your browser's JavaScript log for the message.").fadeIn();
                console.log("message:");
                console.log(msg);

                messageValid = false;
            }
        })
        .fail( function( jqXHR, textStatus, errorThrown ) {
            if (typeof onDone === 'function') {
                onFail(jqXHR, textStatus, errorThrown);
            }
            jQuery('#feedback').hide().html("Something went wrong. See your browser's JavaScript log for the message.").fadeIn();
            console.log(jqXHR);
            console.log(textStatus);
            console.log(errorThrown);

            messageValid = false;
        }); 
};

Lookup.prototype.lookup = function(onDone, onFail) {
    jQuery('#feedback').html('');
    jQuery('#post-preview > .contents').html('');

    var url = jQuery('#url-input').val();
    
    // We typically extract a bunch of information from the URL the user gave us.
    var result = this.urlInspector( url );
    
    // Then, we construct a call to an API or something.
    var call = this.urlConstructor( result );
    
    // Finally, we call the API or whatver it is. Optionally, we can include some extra callbacks on success or failure.
    this.urlCaller(call, onDone, onFail);
};