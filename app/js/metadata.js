$(document).ready(function() {   
    console.log("test");
    $.ajax( {
            url: "../../json/tags.json", // TODO: find a fix?
            type: "GET",
            dataType: "jsonp"
        }) // TODO: could we fix this ?
        .done( function(msg) {
            console.log("success");
            console.log(msg);
        }).fail(function(xhr, ajaxOptions, thrownError) {
            console.log("failure");
            console.log(xhr);
            console.log(ajaxOptions);
            console.log(thrownError);
        });
});