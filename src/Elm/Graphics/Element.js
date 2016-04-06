/* global exports */
"use strict";

// module Elm.Graphics.Element

exports.getDimensions = function (node) {
    return function () {
        var style = window.getComputedStyle(node, null);
        var w = Math.ceil(style.getPropertyValue('width').slice(0, -2) - 0);
        var h = Math.ceil(style.getPropertyValue('height').slice(0, -2) - 0);

        return {
            width: w,
            height: h
        };
    }
};

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
