function loadArticleTypes() {
  var dropdowns = [
    ["../../json/article-types.json",'#type-article']
  ];

  var settings = {
    multiple: false,
    tags: true,
    createTag: function (params) {
      return {
        id: params.term,
        text: params.term,
        newOption: true
      };
    }
  };

  loadDropdowns( dropdowns, settings );
}

loadArticleTypes();
