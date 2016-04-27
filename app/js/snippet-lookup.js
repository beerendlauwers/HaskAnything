function generateFileTitle(templateName) {
    var str = jQuery('#snippet-title').val();
    
    console.log(str);
    
    if (!str) {
        return;
    }
    
    return templateName + "-" + str.replace(/\W+/g, '-').toLowerCase() + ".md";
}