/* global exports */
"use strict";

// module DOM.Renderable

exports.same = function (a) {
    return function (b) {
        return a === b;
    }
};
