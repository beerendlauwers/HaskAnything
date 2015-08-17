// Invalid call results in 
// {"kind": "Listing", "data": {"modhash": "xvze5vcdorfabc7d16f02af6e0cb4485b83e52f2a9b96c3bad", "children": [], "after": null, "before": null}}

function lookupRedditPost() {

    var url = $('#url-input').val();

    $.ajax( url )
        .done( function(msg) {
            if (messageIsValid(msg)) {
                var body = msg.data.children[0].data.body;
                var converter = new showdown.Converter(),
                    html      = converter.makeHtml(body);
                $('#post-preview').html( html );
            }
            else {
                console.log("message was invalid");
                // TODO: some message
            }
        })
        .fail( function() {
            console.log("something failed.");
        });
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