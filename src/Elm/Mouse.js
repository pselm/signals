/* global exports */
"use strict";

// module Elm.Mouse

// Turns the event into a MousePosition, where the MousePosition is relative to
// the target.
exports.getOffsetXY = function (event) {
    var target = event.currentTarget;
    var rect = target.getBoundingClientRect();
    var offset = {
        x: Math.round(event.clientX - rect.left),
        y: Math.round(event.clientY - rect.top)
    };

    // We calculate the above result *immediately*, since we fundamentally want
    // the *original* result, at the earliest moment, given the event. However,
    // we can't just *be* that function (I think), because what if we're called
    // again with the same event? Presumably clientX and clientY won't change,
    // but what about getBoundingClientRect? So, we need to record our result
    // when the effect is created, and then return the result whenever executed.
    // At least, I think that's the right way to look at it.
    return function () {
        return offset;
    };
};

