function loadConferences() {
  var dropdowns = [
    ["../../json/conferences.json",'#conference-presentation']
  ];

  var settings = {
    tags: true,
    tokenSeparators: [','],
    maximumSelectionLength: 1
  };

  loadDropdowns( dropdowns, settings );
}

loadConferences();
