const geom = require("@thi.ng/geom")
const hiccupCanvas = require("@thi.ng/hiccup-canvas")

exports.mkRect = (attribs) => ({ x, y, width, height }) =>
  geom.rect([x, y], [width, height], attribs)

exports.mkCircle = (attribs) => ({ x, y }) => (radius) =>
  geom.circle([x, y], radius, attribs)

exports.mkPolygon = (attribs) => (points) =>
  geom.polygon(
    points.map(({ x, y }) => [x, y]),
    attribs
  )

exports.mkGroup = (attribs) => (shapes) => geom.group(attribs, shapes)

exports.renderGeometry = (shape) => (ctx) => () => hiccupCanvas.draw(ctx, shape)

exports.boundsImpl = (shape) => {
  const dest = geom.bounds(shape)

  return {
    x: dest.pos[0],
    y: dest.pos[1],
    width: dest.size[0],
    height: dest.size[1]
  }
}

// exports.geometryToRectImpl = (shape) => (build) =>
//   build({ x: geometry.pos[0], y: geometry.pos[1] })(geometry.size[0])(
//     geometry.size[1]
//   )(geometry.attribs)
