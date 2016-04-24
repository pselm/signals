var gulp = require("gulp");
var purescript = require("gulp-purescript");
var fork = require('child_process').fork;
var spawn = require('child_process').spawn;
var exec = require('child_process').exec;

var sources = [
    "src/**/*.purs",
    "bower_components/purescript-*/src/**/*.purs",
    "test/**/*.purs",
    "examples/**/*.purs"
];

var foreigns = [
    "src/**/*.js",
    "bower_components/purescript-*/src/**/*.js",
    "test/**/*.js",
    "examples/**/*.js"
];

var nodeBundles = [
    "Examples.Time.Delay",
    "Examples.Time.Every",
    "Examples.Time.FPS",
    "Examples.Time.FPSWhen",
    "Examples.Time.Since",
    "Examples.Time.Timestamp",
    "Test.Main"
];

var htmlBundles = [{
    ps: "Examples.Graphics.StaticElement",
    html: "examples/Graphics/StaticElement.html"
},{
    ps: "Examples.Graphics.UpdateElement",
    html: "examples/Graphics/UpdateElement.html"
},{
    ps: "Examples.Graphics.StaticCollage",
    html: "examples/Graphics/StaticCollage.html"
},{
    ps: "Examples.Keyboard.Console",
    html: "examples/Keyboard/Console.html"
},{
    ps: "Examples.Mouse.FullScreen",
    html: "examples/Mouse/FullScreen.html"
},{
    ps: "Examples.Mouse.Embed",
    html: "examples/Mouse/Embed.html"
},{
    ps: "Examples.Window.FullScreen",
    html: "examples/Window/FullScreen.html"
},{
    ps: "Examples.Window.Embed",
    html: "examples/Window/Embed.html"
}];

var allBundles =
    nodeBundles.concat(
        htmlBundles.map(
            function (bundle) {
                return bundle.ps
            }
        )
    );

function output (module) {
    return "build/" + module.split(".").join("/") + ".js";
}

nodeBundles.map(function (bundle) {
    gulp.task("bundle-" + bundle, ["make"], function () {
        return purescript.pscBundle({
            src: "output/**/*.js",
            output: output(bundle),
            module: bundle,
            main: bundle
        });
    });

    gulp.task(bundle, ["bundle-" + bundle], function (cb) {
        fork(output(bundle)).on('exit', cb);
    });
});

htmlBundles.map(function (bundle) {
    gulp.task("bundle-" + bundle.ps, ["make"], function () {
        return purescript.pscBundle({
            src: "output/**/*.js",
            output: output(bundle.ps),
            module: bundle.ps,
            main: bundle.ps
        });
    });

    gulp.task(bundle.ps, ["bundle-" + bundle.ps], function (cb) {
        exec("open " + bundle.html);
        cb();
    });
});

gulp.task("bundle", allBundles.map(function (bundle) {
    return "bundle-" + bundle;
}));

gulp.task("test", ["Test.Main"]);

gulp.task("make", function () {
    return purescript.psc({
        src: sources,
        ffi: foreigns
    });
});

gulp.task("docs", ["make"], function () {
    return purescript.pscDocs({
        src: sources,
        format: "markdown",
        docgen: {
            "Elm.Graphics.Collage": "docs/Elm/Graphics/Collage.md",
            "Elm.Graphics.Element": "docs/Elm/Graphics/Element.md",
            "Elm.Color": "docs/Elm/Color.md",
            "Elm.Default": "docs/Elm/Default.md",
            "Elm.Keyboard": "docs/Elm/Keyboard.md",
            "Elm.Mouse": "docs/Elm/Mouse.md",
            "Elm.Signal": "docs/Elm/Signal.md",
            "Elm.Task": "docs/Elm/Task.md",
            "Elm.Text": "docs/Elm/Text.md",
            "Elm.Transform2D": "docs/Elm/Transform2D.md",
            "Elm.Time": "docs/Elm/Time.md",
            "Elm.Window": "docs/Elm/Window.md"
        }
    });
});

gulp.task("dotpsci", function () {
    return purescript.psci({
        src: sources,
        ffi: foreigns
    }).pipe(
        gulp.dest(".")
    );
});

gulp.task("default", ["bundle", "docs", "dotpsci"]);
