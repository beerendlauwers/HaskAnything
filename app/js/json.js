function getJSONData( type ) {
    jQuery.getJSON("json/" + type + ".json", function (json) {
        // How do I get this back???
    });
}

function getTags() {
    getJSONData("tags");
}

function getLibraries() {
    getJSONData("libraries");
}

function getCategories() {
    getJSONData("categories");
}

function loadTags() {
    // Use React to load stuff?
}