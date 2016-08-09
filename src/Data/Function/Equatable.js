
// Generates a unique tag for a function. We store it on the function, so that we can
// be referentially transparent if we're asked again.
exports.uniqueTagImpl = function () {
    var funcID = 1;

    return function (tagger) {
        return function (func) {
            // We store the tag on the func so that we'll be referentially transparent ...
            // that is, if asked for a tag on a func twice, we'll return the same
            // tag each time.
            if (func._eq_fn_tag) {
                return func._eq_fn_tag;
            } else {
                var tag = tagger(funcID++);
                func._eq_fn_tag = tag;
                return tag;
            }
        };
    };
}();
