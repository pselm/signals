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

exports.setProperty = function (key) {
    return function (value) {
        return function (element) {
            return function () {
                element[key] = value;
                return {};
            };
        };
    };
};

exports.setPropertyIfDifferent = function (key) {
    return function (value) {
        return function (element) {
            return function () {
                if (element[key] !== value) {
                    element[key] = value;
                }

                return {};
            };
        };
    };
};

exports.setAttributeNS = function (ns) {
    return function (key) {
        return function (value) {
            return function (element) {
                element.setAttributeNS(ns, key, value);
                return {};
            };
        };
    };
};

exports.getAttributeNS = function (ns) {
    return function (key) {
        return function (element) {
            return element.getAttributeNS(ns, key);
        };
    };
};

exports.removeAttributeNS = function (ns) {
    return function (key) {
        return function (element) {
            element.removeAttributeNS(ns, key);
            return {};
        };
    };
};
