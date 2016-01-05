/* global exportss */
"use strict";

// module Elm.String

exports.foldl = function (f) {
    return function (b) {
        return function (str) {
            var len = str.length;
            
            for (var i = 0; i < len; ++i) {
                b = f(str[i])(b);
            }
            
            return b;
        }
    }
}

exports.foldr = function (f) {
    return function (b) {
        return function (str) {
            for (var i = str.length; i--; ) {
                b = f(str[i])(b);
            }

            return b;
        }
    }
}

exports.repeat = function (n) {
    return function (str) {
        var result = '';
        
        while (n > 0) {
            if (n & 1) {
                result += str;
            }

            n >>= 1, str += str;
        }

        return result;
    }
}

exports.slice = function (start) {
    return function (end) {
        return function (str) {
            return str.slice(start, end);
        }
    }
}

exports.right = function (n) {
    return function (str) {
        return n < 1 ? '' : str.slice(-n);
    }
}

exports.dropRight = function (n) {
    return function (str) {
        return n < 1 ? str : str.slice(0, -n);
    }
}

exports.trimLeft = function (str) {
    return str.replace(/^\s+/, '');
}

exports.trimRight = function (str) {
    return str.replace(/\s+$/, '');
}

exports._words = function (str) {
    return str.trim().split(/\s+/g);
}

exports._lines = function (str) {
    return str.split(/\r\n|\r|\n/g);
}

exports.any = function (pred) {
    return function (str) {
        for (var i = str.length; i--; ) {
            if (pred(str[i])) {
                return true;
            }
        }

        return false;
    }
}

exports.all = function (pred) {
    return function (str) {
        for (var i = str.length; i--; ) {
            if (!pred(str[i])) {
                return false;
            }
        }
        
        return true;
    }
}

exports.startsWith = function (sub) {
    return function (str) {
        return str.indexOf(sub) === 0;
    }
}

exports.endsWith = function (sub) {
    return function (str) {
        return str.length >= sub.length &&
            str.lastIndexOf(sub) === str.length - sub.length;
    }
}

exports._indexes = function (sub) {
    return function (str) {
        var subLen = sub.length;
        var i = 0;
        var is = [];

        while ((i = str.indexOf(sub, i)) > -1) {
            is.push(i);
            i = i + subLen;
        }

        return is;
    }
}

exports._toInt = function (helper) {
    return function (s) {
        var len = s.length;
        if (len === 0) {
            return helper.err("could not convert string '" + s + "' to an Int" );
        }
        
        var start = 0;
        if (s[0] === '-') {
            if (len === 1) {
                return helper.err("could not convert string '" + s + "' to an Int" );
            }
            start = 1;
        }
        
        for (var i = start; i < len; ++i) {
            if (!helper.isDigit(s[i])) {
                return helper.err("could not convert string '" + s + "' to an Int" );
            }
        }

        return helper.ok(parseInt(s, 10));
    }
}

exports._toFloat = function (helper) {
    return function (s) {
        var len = s.length;
        if (len === 0) {
            return helper.err("could not convert string '" + s + "' to a Float" );
        }
        
        var start = 0;
        if (s[0] === '-') {
            if (len === 1) {
                return helper.err("could not convert string '" + s + "' to a Float" );
            }
            start = 1;
        }

        var dotCount = 0;
        for (var i = start; i < len; ++i) {
            if (helper.isDigit(s[i])) {
                continue;
            }

            if (s[i] === '.') {
                dotCount += 1;
                if (dotCount <= 1) {
                    continue;
                }
            }

            return helper.err("could not convert string '" + s + "' to a Float" );
        }

        return helper.ok(parseFloat(s));
    }
}

