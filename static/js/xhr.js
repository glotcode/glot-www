window.XHR = {
  delete: function(url) {
    return $.ajax({
      type: "DELETE",
      url: url,
      dataType: "json",
    });
  },

  jsonPut: function(url, data) {
    return $.ajax({
      type: "PUT",
      url: url,
      data: JSON.stringify(data),
      contentType: "application/json",
      dataType: "json",
    });
  },

  jsonPost: function(url, data) {
    return $.ajax({
      type: "POST",
      url: url,
      data: JSON.stringify(data),
      contentType: "application/json",
      dataType: "json",
    });
  },
};
