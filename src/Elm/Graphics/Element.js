/* global exports */
"use strict";

// module Elm.Graphics.Element

exports.setInnerHtml = function (html) {
    return function (element) {
        return function () {
            element.innerHTML = html;
            return element;
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
