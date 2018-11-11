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

exports.urlencode = function(object) {
    let escapeArray = function(value) {
	if (!Array.isArray(value)) { return value; }

	let values = value.map((v, k) => typeof v === 'string' ? "\"" + v + "\"" : v);
	return "[" + values + "]";
    };

    return encodeURIComponent(
	Object.keys(object).reduce(function(result, key) {
	    let value = escapeArray(object[key]);
	    let str = key + "=" + value;
	    return result === "" ? str : result + "&" + str;
	}, "")
    );
};

exports.mergeActivities = function(activities, persons) {
    let replaceAttendee = (attendees, attendeeId) => {
	attendees[attendeeId] = persons[attendeeId];
	return attendees;
    };
    return this.objectMap(activities, act => {
	act.host = persons[act.host];
	act.attendees = act.attendees.reduce(replaceAttendee, {});
	return act;
    });
};

exports.unmergeActivity = function(activity) {
    let newAct = Object.assign(activity, null);
    newAct.host = activity.host.email;
    newAct.attendees = Object.keys(activity.attendees);
    return newAct;
};

exports.storageStore = function(key, value) {
    if (typeof(Storage) !== "undefined") {
	localStorage.setItem(key, value);
    } else {
	const cookie = JSON.parse(document.cookie.slice()).key=value;
	let date = new Date();
	date.setDate(date.getDate() + 10);
	document.cookie = cookie + "; expires=" + date.toString();
    }
};

exports.storageRead = function(key) {
    if (typeof(Storage) !== "undefined") {
	return localStorage.getItem(key);
    } else {
	return JSON.parse(document.cookie.slice());
    }
};

exports.storageDelete = function(key) {
    if (typeof(Storage) !== "undefined") {
	localStorage.removeItem(key);
    } else {
	let cookie = JSON.parse(document.cookie.slice());
	delete cookie[key];
	document.cookie = cookie;
    }
};
