(function() {
    $("[data-iso-date]").each(function(i, elem) {
        var elem = $(elem);
        var isoDate = elem.data("iso-date");
        var relDate = moment(isoDate).fromNow();
        elem.find(".formated-date").text(relDate);
    });
})();
