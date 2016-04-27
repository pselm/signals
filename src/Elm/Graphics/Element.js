/* global exports */
"use strict";

// module Elm.Graphics.Element

exports.getImageWidth = function (image) {
    return function () {
        return image.width || 0;
    };
};

exports.getImageHeight = function (image) {
    return function () {
        return image.height || 0;
    };
};

exports.setInnerHtml = function (html) {
    return function (element) {
        return function () {
            element.innerHTML = html;
            return {};
        }
    }
};

exports.same = function (a) {
    return function (b) {
        return a === b;
    }
};

exports.nullableDocument = function () {
    if (window && window.document) {
        return window.document;
    } else {
        return null;
    }
};

exports.removeAttribute = function (attr) {
    return function (element) {
        return function () {
            element.removeAttribute(attr);
        };
    };
};
