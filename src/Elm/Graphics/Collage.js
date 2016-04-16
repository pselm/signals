/* global exports */
"use strict";

// module Elm.Graphics.Collage

exports.setLineJoinImpl = function (lineJoin) {
    return function (miterLimit) {
        return function (ctx) {
            return function () {
                ctx.lineJoin = lineJoin;
                ctx.miterLimit = miterLimit;
                return ctx;
            };
        };
    };
};

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
