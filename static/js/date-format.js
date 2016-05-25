(function() {
    $("[data-iso-date]").each(function(i, elem) {
        var srcElem = $(elem);
        var dstElem = srcElem.find(".formated-date");
        var isoDate = srcElem.data("iso-date");
        var date = moment(isoDate);

        dstElem.text(date.fromNow());
        dstElem.tooltip({
            title: date.format("LLL"),
            delay: {
                "show": 500,
                "hide": 100,
            },
        });
    });
})();
