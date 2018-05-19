/* global exports */
"use strict";

// module Elm.VirtualDom

exports.getHandlers = function (element) {
    return function () {
        return element.elm_handlers;
    };
};

exports.setHandlers = function (handlers) {
    return function (element) {
        return function () {
            element.elm_handlers = handlers;
            return {};
        };
    };
};

exports.removeHandlers = function (element) {
    return function () {
        element.elm_handlers = undefined;
        return {};
    };
};

exports.getTagger = function (element) {
    return function () {
        return element.elm_tagger;
    };
};

exports.setTagger = function (tagger) {
    return function (element) {
        return function () {
            element.elm_tagger = tagger;
            return {};
        };
    };
};

exports.removeTagger = function (element) {
    return function () {
        element.elm_taggger = undefined;
        return {};
    };
};

exports.setStyle = function (key) {
    return function (value) {
        return function (element) {
            return function () {
                element.style[key] = value;
                return {};
            }
        }
    }
};

exports.removeStyle = function (key) {
    return function (element) {
        return function () {
            element.style[key] = "";
            return {};
        }
    }
};

exports.setProperty = function (key) {
    return function (value) {
        return function (element) {
            return function () {
                element[key] = value;
                return {};
            };
        };
    };
};

exports.getProperty = function (key) {
    return function (element) {
        return function () {
            return element[key];
        };
    };
};

exports.removeProperty = function (key) {
    return function (element) {
        return function () {
            // This is what the original Elm code in VirtualDom does.
            //
            // Now, you might think that `delete element[key]` would be
            // better. However, perhaps that is problemaatic when dealing
            // with properties that have a significance to the DOM
            // (as opposed to custom properties).
            element[key] = (typeof element[key] === 'string') ? '' : null;

            // This is a hackish special case. You can't entirely get rid
            // of the `class` attribute this way, so we'll take care of that.
            // This is mostly for testing, since it wouldn't really be a problem
            // in practice.
            if (key === "className") {
                element.removeAttribute("class");
            }

            return {};
        };
    };
};

exports.setPropertyIfDifferent = function (key) {
    return function (value) {
        return function (element) {
            return function () {
                if (element[key] !== value) {
                    element[key] = value;
                }

                return {};
            };
        };
    };
};

exports.setAttributeNS = function (ns) {
    return function (key) {
        return function (value) {
            return function (element) {
                return function () {
                    element.setAttributeNS(ns, key, value);
                    return {};
                };
            };
        };
    };
};

exports.getAttributeNS = function (ns) {
    return function (key) {
        return function (element) {
            return function () {
                return element.getAttributeNS(ns, key);
            };
        };
    };
};

exports.removeAttributeNS = function (ns) {
    return function (key) {
        return function (element) {
            return function () {
                element.removeAttributeNS(ns, key);
                return {};
            };
        };
    };
};

exports.eventHandler = function (fn) {
    return function (i) {
        return function () {
            function handler (event) {
                fn(handler.info)(event)();
            };

            handler.info = i;

            return handler;
        };
    };
};

exports.setHandlerInfo = function (i) {
    return function (handler) {
        return function () {
            handler.info = i;
            return {};
        };
    };
};

exports.addEventHandler = function (type) {
    return function (handler) {
        return function (useCapture) {
            return function (target) {
                return function () {
                    target.addEventListener(type, handler, useCapture);
                    return {};
                };
            };
        };
    };
};

exports.removeEventHandler = function (type) {
    return function (handler) {
        return function (useCapture) {
            return function (target) {
                return function () {
                    target.removeEventListener(type, handler, useCapture);
                    return {};
                };
            };
        };
    };
};

exports.makeCustomEvent = function (eventName) {
    return function (detail) {
        return function () {
            if (typeof(CustomEvent) === 'function') {
                return new CustomEvent(eventName, {
                    detail: detail,
                    bubbles: true
                });
            } else {
                var event = document.createEvent('CustomEvent');
                event.initCustomEvent(eventName, true, false, detail);
                return event;
            }
        };
    };
};

exports._detail = function (customEvent) {
    return customEvent.detail;
};
