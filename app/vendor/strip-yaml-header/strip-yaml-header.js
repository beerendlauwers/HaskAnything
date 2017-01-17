// LICENSE : MIT
"use strict";
function normalize(markdown) {
    var pattern = pattern = '^(' +
    '((= yaml =)|(---))' +
    '$([\\s\\S]*?)' +
    '\\2' +
    '$' +
    '(?:\\n)?)';

    var yamlRegexp = new RegExp(pattern, 'm');
    
    function replaceByBr(text) {
        return text.replace(yamlRegexp, function (all) {
            var lines = all.split("\n");
            return (new Array(lines.length)).join("\n");
        });

    }

    if (yamlRegexp.test(markdown)) {
        return replaceByBr(markdown);
    }
    return markdown;
}
//module.exports = normalize;
