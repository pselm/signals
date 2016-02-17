var gulp = require("gulp");
var purescript = require("gulp-purescript");
var fork = require('child_process').fork;
var spawn = require('child_process').spawn;

var sources = [
    "src/**/*.purs",
    "bower_components/purescript-*/src/**/*.purs",
    "test/**/*.purs",
    "benchmark/**/*.purs",
    "examples/**/*.purs"
];

var foreigns = [
    "src/**/*.js",
    "bower_components/purescript-*/src/**/*.js",
    "test/**/*.js",
    "benchmark/**/*.js",
    "examples/**/*.js"
];

var bundles = [
    "Examples.Time.Delay",
    "Examples.Time.Every",
    "Examples.Time.FPS",
    "Examples.Time.FPSWhen",
    "Examples.Time.Since",
    "Examples.Time.Timestamp",
    "Test.Main"
];

function output (module) {
    return "build/" + module.split(".").join("/") + ".js";
}

bundles.map(function (bundle) {
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

gulp.task("bundle", bundles.map(function (bundle) {
    return "bundle-" + bundle;
}));

gulp.task("test", ["Test.Main"]);

gulp.task("make", function () {
    return purescript.psc({
        src: sources,
        ffi: foreigns
    });
});

// Couldn't figure out psc-docs, so fall back on pulp
gulp.task("docs", function (cb) {
    var docs = spawn("pulp", ["docs"]);
    
    docs.stdout.on('data', function (data) {
        console.log(data.toString());
    });

    docs.stderr.on('data', function (data) {
        console.log(data.toString());
    });

    docs.on('close', cb);
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
