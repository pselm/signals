/* global exports */
"use strict";

// module Elm.Json.Decode


exports.unsafeKeys = function (object) {
    var keys = [];

    for (var key in object) {
        keys.push(key);
    }

    return keys;
}
