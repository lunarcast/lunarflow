const geom = require("@thi.ng/geom");
const hiccupCanvas = require("@thi.ng/hiccup-canvas");

exports.mkRect = (attribs) => ({ x, y }) => (width) => (height) =>
  geom.rect([x, y], [width, height], attribs);

exports.mkCircle = (attribs) => ({ x, y }) => (radius) =>
  geom.circle([x, y], radius, attribs);

exports.mkPolygon = (attribs) => (points) =>
  geom.polygon(
    points.map(({ x, y }) => [x, y]),
    attribs
  );

exports.mkGroup = (attribs) => (shapes) => geom.group(attribs, shapes);

exports.renderGeometry = (shape) => (ctx) => () =>
  hiccupCanvas.draw(ctx, shape);
