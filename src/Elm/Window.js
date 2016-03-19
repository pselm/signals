/* global exports */
"use strict";

// module Elm.Window

exports.clientWidth = function (element) {
    return function () {
        return element.clientWidth;
    };
};

exports.clientHeight = function (element) {
    return function () {
        return element.clientHeight;
    };
};

exports.innerHeight = function () {
    return window.innerHeight;
};

