/* global exports */
"use strict";

// module DOM.Renderable

exports.same = function (a) {
    return function (b) {
        return a === b;
    }
};

exports.setRenderable = function (element) {
    return function (renderable) {
        return function () {
            element.dynamicRenderable = renderable;
            return {};
        };
    };
};

exports.getRenderable = function (element) {
    return function () {
        return element.dynamicRenderable || null;
    };
};
