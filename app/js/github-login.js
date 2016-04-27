OAuth.initialize('UF1na7MH6xX9Y450CNjiHFpwcPQ');

jQuery('document').ready(function() {
    githubPopulateUsername();
});

function githubAuthorize() {
    OAuth.popup('github', {cache: true})
        .done(function(result) {
            console.log("Logged into Github.");
            githubPopulateUsername(result);
        })
        .fail(function (err) {
          console.log("Something went wrong during OAuth authentication: " + err);
    });
}

function githubLogout() {
    OAuth.clearCache();
    localStorage['githubData'] = undefined;
    
    jQuery('#login-username').text("You're not logged in. ");
    jQuery('#login-avatar').html('');
    jQuery('#login-link').show();
    jQuery('#logout-link').hide();
}

function githubDisplayUsername(githubData) {
    jQuery('#login-username').text(githubData.alias);
    jQuery('#login-avatar').html('<img src="' + githubData.avatar + '"/>');
    
    jQuery('#login-link').hide();
    jQuery('#logout-link').show();
}

function storeGithubData( githubData ) {
    localStorage['githubData'] = JSON.stringify( githubData );
}

function getGithubData() {
    if ( localStorage['githubData'] === "undefined" || localStorage['githubData'] === undefined ) {
        return undefined;
    }
    
    return JSON.parse( localStorage['githubData'] );
}

function githubPopulateUsername(api) {
    // Ensure that our OAuth connection is still in order.
    if (api === undefined) {
        // Fetch from API connection cache.
        api = OAuth.create('github');
    }

    if (!api) {
        return;
    }
    
    // Fetch Github credentials from browser cache.
    var githubData = getGithubData();
    
    if (githubData) {
        githubDisplayUsername(githubData);
    }
    else {
        // Connect to API and get user data.
        api.me().done( function(response){
            storeGithubData( response );
            githubDisplayUsername( response );
        });
    }
}