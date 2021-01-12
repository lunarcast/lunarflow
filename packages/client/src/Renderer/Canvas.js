exports.fillScreen = (ctx) => () => {
  ctx.canvas.height = window.innerHeight
  ctx.canvas.width = window.innerWidth
}

/**
 * @param {CanvasRenderingContext2D} ctx The canvas context to stretch
 */
exports.stretch = (ctx) => () => {
  const bounds = ctx.canvas.getBoundingClientRect()

  ctx.canvas.width = bounds.width
  ctx.canvas.height = bounds.height
}

exports.onResize = (e) => () => {
  window.onresize = () => e()
}

exports.clear = (bg) => (ctx) => () => {
  ctx.fillStyle = bg
  ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height)
}

/**
 * @param {DOMRect} bounds The bounds to fit the context inside.
 */
exports.fitIntoBounds = (bounds) => (ctx) => () => {
  const boundsHeight = bounds.height
  const boundsWidth = bounds.width
  const canvasBounds = ctx.canvas.getBoundingClientRect()
  const xScale = boundsWidth / canvasBounds.width
  const yScale = boundsHeight / canvasBounds.height
  const max = Math.max(xScale, yScale)

  const scale = 1 / max

  ctx.scale(scale, scale)
  ctx.translate(-bounds.x, -bounds.y)

  if (xScale > yScale) {
    const extra = canvasBounds.height * max - boundsHeight

    ctx.translate(0, extra / 2)
  }
}
