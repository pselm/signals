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
        return window.devicePixelRatio;
    };
};

