function inspectURL(url) {
    var interesting = R.drop(2)(R.split('/')(url));
    
    console.log(interesting);
    
    var domain = R.head(R.split('.')(R.head(interesting)));
    
    console.log(domain);
    
    var questionId = R.head(R.drop(2)(interesting));
    
    return {
        id: questionId,
        domain: domain
    };
}

function constructURL(data) {
    return "https://api.stackexchange.com/2.2/questions/" + data.id + "?site=" + data.domain + "&filter=!9YdnSIoOi";
}

function messageIsValid( msg ) {
    return (
         typeof msg === 'object'
    &&   'items' in msg
    &&   typeof msg.items === 'object'
    &&   msg.items.length > 0
    &&   typeof msg.items[0] === 'object'
    &&   'body_markdown' in msg.items[0]
        );
}

function accessMessage( msg ) {
    return msg.items[0].body_markdown;
}

function renderMessage( contents ) {
    // Convert it to markdown.
    var converter = new showdown.Converter(),
        html      = converter.makeHtml(contents);
        
    // Display it.
    jQuery('#post-preview > .contents').hide().html(html).fadeIn();
}

var stackExchangeLookup = new Lookup( inspectURL, constructURL, messageIsValid, accessMessage, undefined, renderMessage );

function lookupStackExchangePost() {
    stackExchangeLookup.lookup();
}