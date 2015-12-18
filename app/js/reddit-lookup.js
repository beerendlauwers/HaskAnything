
function inspectURL(url) {
    return R.last( R.split('/')(url) );
}

function constructURL(data) {
    return "https://www.reddit.com/api/info/.json?id=t1_" + data;
}

function messageIsValid( msg ) {
    return (
         typeof msg === 'object'
    &&   'data' in msg
    &&   'children' in msg.data
    &&   typeof msg.data.children === 'object'
    &&   msg.data.children.length > 0
    &&   typeof msg.data.children[0] === 'object'
    &&   'data' in msg.data.children[0]
    &&   typeof msg.data.children[0].data === 'object'
    &&   'body' in msg.data.children[0].data
        );
}

function accessMessage( msg ) {
    return msg.data.children[0].data.body;
}

function renderMessage( contents ) {
    // Convert it to markdown.
    var converter = new showdown.Converter(),
        html      = converter.makeHtml(contents);
        
    // Display it.
    jQuery('#post-preview > .contents').hide().html(html).fadeIn();
}

/*
function displayTags() {
    jQuery('.metadata').slideDown(300);
    jQuery('#confirm').prop('disabled','disabled');
    jQuery('#confirmed').fadeIn();
    loadDropdowns();
    jQuery('.row.url, .row.preview').slideUp(300);
}
*/

var redditLookup = new Lookup( inspectURL, constructURL, messageIsValid, accessMessage, undefined, renderMessage );

function lookupRedditPost() {
    redditLookup.lookup();
}