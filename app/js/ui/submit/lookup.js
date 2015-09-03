function Lookup(urlInspector, urlConstructor, messageValidator, messageAccessor, urlCaller) {

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
        this.urlCaller = urlCaller;
    }    
}

// The default urlCaller is an AJAX request that fetches markdown from the received message and renders it.
Lookup.prototype.urlCaller = function(url,onDone,onFail) {
    var validator = this.messageValidator;
    var accessor = this.messageAccessor;
    $.ajax( url )
        .done( function(msg) {

            if (typeof onDone === 'function') {
                onDone(msg);
            }
            
            // Let's validate it first.
            if (validator(msg)) {
            
                // It's valid! Let's fetch the interesting bits.
                var body = accessor(msg);
                
                // Convert it to markdown.
                var converter = new showdown.Converter(),
                    html      = converter.makeHtml(body);
                    
                // Display it.
                $('#post-preview > .contents').hide().html(html).fadeIn();
                $('#confirm').slideDown(300);
            }
            else {
                $('#feedback').hide().html("Message was invalid. See your browser's JavaScript log for the message.").fadeIn();
                console.log("message:");
                console.log(msg);
            }
        })
        .fail( function( jqXHR, textStatus, errorThrown ) {
            if (typeof onDone === 'function') {
                onFail(jqXHR, textStatus, errorThrown);
            }
            $('#feedback').hide().html("Something went wrong. See your browser's JavaScript log for the message.").fadeIn();
            console.log(jqXHR);
            console.log(textStatus);
            console.log(errorThrown);
        }); 
};

Lookup.prototype.lookup = function(onDone, onFail) {
    $('#feedback').html('');
    $('#post-preview > .contents').html('');

    var url = $('#url-input').val();
    
    // We typically extract a bunch of information from the URL the user gave us.
    var result = this.urlInspector( url );
    
    // Then, we construct a call to an API or something.
    var call = this.urlConstructor( result );
    
    // Finally, we call the API or whatver it is. Optionally, we can include some extra callbacks on success or failure.
    this.urlCaller(call, onDone, onFail);
};