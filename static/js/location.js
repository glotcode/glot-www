window.Location = {
  set: function(url) {
    if (location.href === url) {
      this.reload();
    } else {
      location.href = url;
    }
  },

  setRetainHash: function(url) {
    var suffix = this.getHash() ? "#" + this.getHash() : "";
    var newUrl = url + suffix;
    this.set(newUrl);
  },

  getHash: function() {
    return location.hash.slice(1);
  },

  setHash: function(str) {
    location.hash = str;
  },

  reload: function() {
    location.reload();
  }
};
