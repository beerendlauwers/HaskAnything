
function inspectURL(url) {
    var split = R.reverse( R.split('/')(url) );

    var data = {};
    data.user = R.head( R.drop(1)(split) );
    data.repo = R.head( split );
    return data;
}

function constructURL(data) {
    return "https://api.github.com/repos/" + data.user + "/" + data.repo;
}

function messageIsValid( msg ) {
    return (
         typeof msg === 'object'
    &&   'description' in msg
    &&   'html_url' in msg
        );
}

function accessMessage( msg ) {
    var contents = {};
    contents.description = msg.description;
    contents.url = msg.html_url;
    return contents;
}

function renderMessage( contents ) {
    // Convert it to markdown.
    var converter = new showdown.Converter(),
        html      = converter.makeHtml(contents.description);
        
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

var githubLookup = new Lookup( inspectURL, constructURL, messageIsValid, accessMessage, undefined, renderMessage );

function lookupGithubRepository() {
    githubLookup.lookup();
}