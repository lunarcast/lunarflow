const vectors = require("@thi.ng/vectors")

exports.rotate = (a) => (theta) => vectors.rotate([], a, theta)
exports.add = (a) => (b) => vectors.add2([], a, b)
exports.sub = (a) => (b) => vectors.sub2([], a, b)
exports.negate = (a) => vectors.neg([], a)
// exports.add = (a) => (b) => [a[0] + b[0], a[1] + b[1]]
