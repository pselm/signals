/* global exports */
"use strict";

// module Test.Elm.TaskTest

exports._evenAfter50 = function (error) {
    return function (success) {
        return function (int) {
            return function () {
                setTimeout(function () {
                    if (int % 2 === 0) {
                        success(int)();
                    } else {
                        error("Not even")();
                    }
                }, 50);
            }
        }
    }
};
