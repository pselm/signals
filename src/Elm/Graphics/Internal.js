/* global exports */
"use strict";

// module Elm.Graphics.Internal

exports.getDimensions = function (node) {
    return function () {
        var style = window.getComputedStyle(node, null);

        // parseFloat will ignore the 'px' at the end, which is convenient ...
        var w = parseFloat(style.getPropertyValue('width'));
        var h = parseFloat(style.getPropertyValue('height'));

        return {
            width: w,
            height: h
        };
    }
};

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
            element.style[key] = "";
            return {};
        }
    }
};

