/* global exports */
"use strict";

// module Elm.VirtualDom

exports.getHandlers = function (element) {
    return function () {
        return element.elm_handlers;
    };
};

exports.setHandlers = function (handlers) {
    return function (element) {
        return function () {
            element.elm_handlers = handlers;
            return {};
        };
    };
};

exports.removeHandlers = function (element) {
    return function () {
        element.elm_handlers = undefined;
        return {};
    };
};

exports.getTagger = function (element) {
    return function () {
        return element.elm_tagger;
    };
};

exports.setTagger = function (tagger) {
    return function (element) {
        return function () {
            element.elm_tagger = tagger;
            return {};
        };
    };
};

exports.removeTagger = function (element) {
    return function () {
        element.elm_taggger = undefined;
        return {};
    };
};
