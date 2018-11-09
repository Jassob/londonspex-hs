exports = module.exports = {};

// returns a new object with the values at each key mapped using mapFn(value)
exports.objectMap = function(object, mapFn) {
    return Object.keys(object).reduce(function(result, key) {
        result[key] = mapFn(object[key]);
        return result;
    }, {});
};


// returns a new object with the values at each key mapped using mapFn(key, value)
exports.objectMapWithKey = function(object, mapFn) {
    return Object.keys(object).reduce(function(result, key) {
        result[key] = mapFn(key, object[key]);
        return result;
    }, {});
};

// returns a list of the values in the object
exports.objectToList = function(object) {
    let result = [];
    for (let key in object) {
	result.push(object[key]);
    }
    return result;
};
