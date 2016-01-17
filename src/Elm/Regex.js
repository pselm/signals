/* global exports */
"use strict";

// module Elm.Regex

exports._showRegex = function (r) {
    return "" + r;
};

exports.escape = function (str) {
	return str.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
};

exports.regex = function (raw) {
	return new RegExp(raw, 'g');
};

exports.caseInsensitive = function (re) {
	return new RegExp(re.source, 'gi');
};

exports.contains = function (re) {
    return function (string) {
		return string.match(re) !== null;
	}
};

exports._find = function (context) {
    return function (n) {
        return function (re) {
            return function (str) {
                var out = context.nil;
                var number = 0;
                var string = str;
                var lastIndex = re.lastIndex;
                var prevLastIndex = -1;
                var result;

                while (number++ < n && (result = re.exec(string))) {
                    if (prevLastIndex === re.lastIndex) break;

                    var i = result.length - 1;
                    var subs = context.nil;

                    while (i > 0) {
                        var submatch = result[i];
                        i--;
                        var justSubmatch = submatch === undefined
                            ? context.nothing
                            : context.just(submatch);
                        
                        subs = context.cons(justSubmatch)(subs);
                    }

                    out = context.cons({
                        match: result[0],
                        submatches: subs,
                        index: result.index,
                        number: number
                    })(out);

                    prevLastIndex = re.lastIndex;
                }

                re.lastIndex = lastIndex;
                return out;
            }
        }
    }
}

exports._replace = function (context) {
    return function (n) {
        return function (re) {
            return function (replacer) {
                return function (string) {
                    var count = 0;
                    
                    function jsReplacer (match) {
                        if (count++ >= n) {
                            return match;
                        }

                        var i = arguments.length - 3;
                        var submatches = context.nil;
                        
                        while (i > 0) {
                            var submatch = arguments[i];
                            i--;
                            var justSubmatch = submatch === undefined
                                ? context.nothing
                                : context.just(submatch);

                            submatches = context.cons(justSubmatch)(submatches);
                        }

                        return replacer({
                            match: match,
                            submatches: submatches,
                            index: arguments[arguments.length - 2],
                            number: count
                        });
                    }

                    return string.replace(re, jsReplacer);
                }
            }
        }
    }
};

exports._split = function (n) {
    return function (re) {
        return function (str) {
            if (n === 9007199254740991) {
                return str.split(re);
            }

            var string = str;
            var result;
            var out = [];
            var lastIndex = re.lastIndex;
            var start = lastIndex;

            while (n--) {
                if (!(result = re.exec(string))) break;
                out.push(string.slice(start, result.index));
                start = re.lastIndex;
            }

            out.push(string.slice(start));
            re.lastIndex = lastIndex;
            return out;
        }
    }
};
