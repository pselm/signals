/* global exports */
"use strict";

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
    try {
        // This fails in testing on Node, since we don't have a window object.
        // We can use JSDOM to get the document, but there's no point, because
        // this is only used for measuring things, and JSDOM doesn't actually
        // do any layout.
        return window.document;
    } catch (e) {
        return null;
    }
};
