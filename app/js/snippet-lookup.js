function generateFileTitle(templateName) {
    var str = jQuery('#snippet-title').val();
    
    if (!str) {
        return;
    }
    
    return templateName + "-" + str.replace(/\W+/g, '-').toLowerCase() + ".md";
}