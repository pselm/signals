/* global exports */
"use strict";

// module Elm.Graphics.Element

exports.setStyle = function (key) {
    return function (value) {
        return function (element) {
            return function () {
                element.style[key] = value;
                return {};
            }
        }
    }
};

exports.removeStyle = function (key) {
    return function (element) {
        return function () {
            element.style.removeProperty(key);
            return {};
        }
    }
};

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
    return function () {
        if (window || window.document) {
            return window.document;
        } else {
            return null;
        }
    };
};
