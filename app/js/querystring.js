function getParameterByName(name, url) {
  var f = function(name,url) {
    var values = [];
    if(!url){
        url = window.location.href;
    }

    name = name.replace(/[\[]/, "\\\[").replace(/[\]]/, "\\\]");

    var pattern = name + '=([^&#]+)';
    var o_reg = new RegExp(pattern,'ig');
    while(true){
        var matches = o_reg.exec(url);
        if(matches && matches[1]){
            values.push(decodeURIComponent(matches[1]));
        }
        else{
            break;
        }
    }

    return values;
  }

  return f(name,url).concat(f(name+"[]",url));
}
