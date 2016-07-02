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

// A list of things to process via pscBundle.
//
// `module` is the name of the relevant Purescript module
//
// `purs` is the path to the Purescript source file
//
// `html` is the html page that uses the bundle ... it
//        is left out for bundles where main runs in Node.
//
// We make a gulp task for each bundle that uses pscBundle
// to bundle it, and then either runs it via Node (if no
// `html`), or opens the given `html` page.
//
// The task is given each of the names. So, you can invoke
// it (for instance) via any of these:
//
//     gulp Examples.Graphics.StaticElement
//     gulp examples/Graphics/StaticElement.purs
//     gulp examples/Graphics/StaticElement.html
var bundles = [{
    module: "Examples.Graphics.StaticElement",
    purs: "examples/Graphics/StaticElement.purs",
    html: "examples/Graphics/StaticElement.html"
},{
    module: "Examples.Graphics.UpdateElement",
    purs: "examples/Graphics/UpdateElement.purs",
    html: "examples/Graphics/UpdateElement.html"
},{
    module: "Examples.Graphics.UpdateRandomRenderable",
    purs: "examples/Graphics/UpdateRandomRenderable.purs",
    html: "examples/Graphics/UpdateRandomRenderable.html"
},{
    module: "Examples.Graphics.StaticCollage",
    purs: "examples/Graphics/StaticCollage.purs",
    html: "examples/Graphics/StaticCollage.html"
},{
    module: "Examples.Graphics.UpdateCollage",
    purs: "examples/Graphics/UpdateCollage.purs",
    html: "examples/Graphics/UpdateCollage.html"
},{
    module: "Examples.VirtualDom.StaticVirtualDom",
    purs: "examples/VirtualDom/StaticVirtualDom.purs",
    html: "examples/VirtualDom/StaticVirtualDom.html"
},{
    module: "Examples.Keyboard.Console",
    purs: "examples/Keyboard/Console.purs",
    html: "examples/Keyboard/Console.html"
},{
    module: "Examples.Mouse.FullScreen",
    purs: "examples/Mouse/FullScreen.purs",
    html: "examples/Mouse/FullScreen.html"
},{
    module: "Examples.Mouse.Embed",
    purs: "examples/Mouse/Embed.purs",
    html: "examples/Mouse/Embed.html"
},{
    module: "Examples.Window.FullScreen",
    purs: "examples/Window/FullScreen.purs",
    html: "examples/Window/FullScreen.html"
},{
    module: "Examples.Window.Embed",
    purs: "examples/Window/Embed.purs",
    html: "examples/Window/Embed.html"
},{
    module: "Examples.Time.Delay",
    purs: "examples/Time/Delay.purs"
},{
    module: "Examples.Time.Every",
    purs: "examples/Time/Every.purs"
},{
    module: "Examples.Time.FPS",
    purs: "examples/Time/FPS.purs"
},{
    module: "Examples.Time.FPSWhen",
    purs: "examples/Time/FPSWhen.purs"
},{
    module: "Examples.Time.Since",
    purs: "examples/Time/Since.purs"
},{
    module: "Examples.Time.Timestamp",
    purs: "examples/Time/Timestamp.purs"
},{
    module: "Test.Main",
    purs: "test/Test/Main.purs"
}];

function output (module) {
    return "build/" + module.split(".").join("/") + ".js";
}

bundles.map(function (bundle) {
    var bundleName = "bundle-" + bundle.module;

    gulp.task(bundleName, ["make"], function () {
        return purescript.pscBundle({
            src: "output/**/*.js",
            output: output(bundle.module),
            module: bundle.module,
            main: bundle.module
        });
    });

    gulp.task(bundle.module, [bundleName], function (cb) {
        if (bundle.html) {
            exec("open " + bundle.html);
            cb();
        } else {
            fork(output(bundle.module)).on('exit', cb);
        }
    });

    if (bundle.purs) gulp.task(bundle.purs, [bundle.module]);
    if (bundle.html) gulp.task(bundle.html, [bundle.module]);
});

gulp.task("bundle", bundles.map(function (bundle) {
    return "bundle-" + bundle.module;
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
            "DOM.Renderable": "docs/DOM/Renderable.md",
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

gulp.task("default", ["bundle", "docs"]);
