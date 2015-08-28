// Invalid call results in 
// {"kind": "Listing", "data": {"modhash": "xvze5vcdorfabc7d16f02af6e0cb4485b83e52f2a9b96c3bad", "children": [], "after": null, "before": null}}

// https://api.stackexchange.com/2.2/questions/32264575?site=stackoverflow&filter=!9YdnSIoOi

// http://stackoverflow.com/questions/32264575/android-geofence-wont-get-any-transition-updates-when-travelling-more-than-10-k
// http://drupal.stackexchange.com/questions/158893/how-to-motivate-visitors-to-comment-technically

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

function lookupStackExchangePost() {

    $('#feedback').html('');
    $('#post-preview > .contents').html('');

    var url = $('#url-input').val();
    
    var result = inspectURL( url );
    
    var redditApiCall = "https://api.stackexchange.com/2.2/questions/" + result.id + "?site=" + result.domain + "&filter=!9YdnSIoOi";

    $.ajax( redditApiCall )
        .done( function(msg) {
            if (messageIsValid(msg)) {
                var body = msg.items[0].body_markdown;
                var converter = new showdown.Converter(),
                    html      = converter.makeHtml(body);
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
            $('#feedback').hide().html("Something went wrong. See your browser's JavaScript log for the message.").fadeIn();
            console.log(jqXHR);
            console.log(textStatus);
            console.log(errorThrown);
        });
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

function displayTags() {
    $('.metadata').slideDown(300);
    $('#confirm').prop('disabled','disabled');
    $('#confirmed').fadeIn();
    loadDropdowns();
    $('.row.url, .row.preview').slideUp(300);
}

/*

// Invalid call results in 
// {"kind": "Listing", "data": {"modhash": "yq76cl7zsrb0175ffc2ebd994b454e1113abe78826c8d9e190", "children": [], "after": null, "before": null}}

$.ajax( "https://www.reddit.com/api/info/.json?id=t1_cu1bvto" ).done( function(msg) {
console.log(msg);
var body = msg.data.children[0].data.body;

console.log(body);

var converter = new showdown.Converter(),
    html      = converter.makeHtml(body);

$('body').html(html);

});

// NICE
    */