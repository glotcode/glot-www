(function() {

    const SECOND = {
        seconds: 1,
        singular: "second",
        plural: "seconds",
    };

    const MINUTE = {
        seconds: 60 * SECOND.seconds,
        singular: "minute",
        plural: "minutes",
    };

    const HOUR = {
        seconds: 60 * MINUTE.seconds,
        singular: "hour",
        plural: "hours",
    };

    const DAY = {
        seconds: 24 * HOUR.seconds,
        singular: "day",
        plural: "days",
    };

    const WEEK = {
        seconds: 7 * DAY.seconds,
        singular: "week",
        plural: "weeks",
    };

    const MONTH = {
        seconds: 31 * DAY.seconds,
        singular: "month",
        plural: "months",
    };

    const YEAR = {
        seconds: 365 * DAY.seconds,
        singular: "year",
        plural: "years",
    }

    const UNITS = [
        YEAR,
        MONTH,
        WEEK,
        DAY,
        HOUR,
        MINUTE,
        SECOND,
    ];

    function getTimeDifference(dateA, dateB) {
        const seconds = Math.abs(Math.floor((dateA - dateB) / 1000));

        return UNITS.map((unit) => {
            const value = seconds / unit.seconds;

            return {
                unit,
                value,
            };
        }).find(({ value }) => Math.round(value) >= 1 );
    }

    function formatTimeAgo({ unit, value }) {
        const roundedValue = Math.round(value);

        if (roundedValue === 0) {
            return "Now";
        } else if (roundedValue === 1) {
            return `${roundedValue} ${unit.singular} ago`;
        } else {
            return `${roundedValue} ${unit.plural} ago`;
        }
    }

    function formatDate(date) {
        const day = date.getDate();
        const month = date.toLocaleString('en-us', { month: 'long' });
        const year = date.getFullYear();
        const hours = date.getHours().toString().padStart(2, 0);
        const minutes = date.getMinutes().toString().padStart(2, 0);

        return `${day} ${month} ${year} - ${hours}:${minutes}`
    }

    $("[data-iso-date]").each(function(i, elem) {
        var srcElem = $(elem);
        var dstElem = srcElem.find(".formated-date");
        var isoDate = srcElem.data("iso-date");

        var now = new Date();
        var date = new Date(isoDate)

        const difference = getTimeDifference(date, now);
        const timeAgo = formatTimeAgo(difference);

        dstElem.text(timeAgo);
        dstElem.tooltip({
            title: formatDate(date),
            delay: {
                "show": 500,
                "hide": 100,
            },
        });
    });

})();
