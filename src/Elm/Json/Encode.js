/* global exports */
"use strict";

// module Elm.Json.Encode

exports.encode = function (indentLevel) {
    return function (value) {
	    return JSON.stringify(value, null, indentLevel);
    }
}

exports.encodeNull = null;

