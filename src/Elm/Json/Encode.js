/* global exports */
"use strict";

// module Elm.Json.Encode

exports.encode = function (indentLevel) {
    return function (value) {
	    return JSON.stringify(value, null, indentLevel);
    }
}

exports.encodeNull = null;

exports.encodeObject = function (fst) {
    return function (snd) {
        return function (pairs) {
            var obj = {};
            var len = pairs.length;

            for (var i = 0; i < len; i++) {
                var pair = pairs[i];
                obj[fst(pair)] = snd(pair);
            }

            return obj;
        }
    }
}
