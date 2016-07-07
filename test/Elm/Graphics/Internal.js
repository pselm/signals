
exports.innerHtml = function (element) {
    return function () {
        return element.innerHTML;
    };
};

exports.outerHtml = function (element) {
    return function () {
        return element.outerHTML;
    };
};
