/* global exports */
"use strict";

// module Elm.VirtualDom

exports.refEq = function (a) {
    return function (b) {
        return a === b;
    };
};
