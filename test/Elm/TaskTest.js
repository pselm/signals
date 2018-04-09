/* global exports */
"use strict";

// module Test.Elm.TaskTest

exports._evenAfter50 = function (int) {
    return function (onException, onError, onSuccess) {
        setTimeout(function () {
            try {
                if (int % 2 === 0) {
                    onSuccess(int);
                } else if (int % 3 === 0) {
                    throw(new Error("Divisible by 3"));
                } else {
                    onError("Not even or divisible by 3");
                }
            }
            catch (ex) {
                onException(ex);
            }
        }, 50);
    }
};

