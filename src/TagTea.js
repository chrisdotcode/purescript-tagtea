// module TagTea

'use strict';

exports.join = function(c) {
	return function(xs) {
		return xs.join(c);
	};
};

exports.length = function(xs) {
	return xs.length;
};
