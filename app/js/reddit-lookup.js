
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

/*
function displayTags() {
    $('.metadata').slideDown(300);
    $('#confirm').prop('disabled','disabled');
    $('#confirmed').fadeIn();
    loadDropdowns();
    $('.row.url, .row.preview').slideUp(300);
}
*/

var redditLookup = new Lookup( inspectURL, constructURL, messageIsValid, accessMessage, undefined );

function lookupRedditPost() {
    redditLookup.lookup();
}