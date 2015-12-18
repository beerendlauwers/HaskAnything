function getBaseRoot() {
    var href = document.location.href;
    var split = href.split('/');
    if ( R.contains("_site")(split) ) {
        return R.join('/')( R.takeWhile( R.compose( R.not, R.equals("_site") ) , split) ) + "/_site/";
    }
    else {
        return window.location.origin;
    }
}