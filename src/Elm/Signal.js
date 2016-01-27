"use strict";

// module Elm.Signal

exports.delay = function (millis) {
    return function (action) {
        return function () {
            setTimeout(action, millis);
        };
    };
};
