
exports.tag = function () {
    var funcID = 1;

    return function (func) {
        if (!func._ps_func_id) {
            func._ps_func_id = funcID++;
        }

        return func;
    };
}();
