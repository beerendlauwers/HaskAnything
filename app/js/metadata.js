function loadMetaData() {
  var dropdowns = [
    ["../../json/tags.json",'#metadata-tags'],
    ["../../json/libraries.json",'#metadata-libraries'],
    ["../../json/permission-files.json",'#permission-file']
  ];

  var settings = {
    tags: true,
    tokenSeparators: [',']
  };

  loadDropdowns( dropdowns, settings );
}

loadMetaData();
