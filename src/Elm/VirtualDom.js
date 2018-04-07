/* global exports */
"use strict";

// module Elm.VirtualDom

exports.refEq = function (a) {
    return function (b) {
        return a === b;
    };
};

exports.setEventNode = function (eventNode) {
    return function (domNode) {
        return function () {
            domNode.elm_event_node_ref = eventNode;
        };
    };
};
