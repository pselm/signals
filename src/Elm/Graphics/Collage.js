/* global exports */
"use strict";

// module Elm.Graphics.Collage

exports.devicePixelRatio = function (window) {
    return function () {
        return window.devicePixelRatio || 1.0;
    };
};

exports.setLineDash = function (dashes) {
    return function (ctx) {
        return function () {
            // setLineDash not available in some browsers
            if (ctx.setLineDash) {
                ctx.setLineDash(dashes);
                return true;
            } else {
                return false;
            }
        };
    };
};

exports.globalAlpha = function (ctx) {
    return function () {
        return ctx.globalAlpha;
    };
};
