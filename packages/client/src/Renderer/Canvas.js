exports.fillScreen = (ctx) => () => {
  canvas.height = window.innerHeight
  canvas.width = window.innerWidth
}

exports.onResize = (e) => () => {
  window.onresize = () => e()
}

exports.fitIntoBounds = (bounds) => (ctx) => () => {
  ctx.translate(-bounds.x, -bounds.y)

  const diff = 0
  const xScale = (bounds.width + diff) / ctx.canvas.width
  const yScale = (bounds.height + diff) / ctx.canvas.height
  const max = Math.max(xScale, yScale)

  const scale = 1 / max

  ctx.scale(scale, scale)

  if (xScale > yScale) {
    const extra = ctx.canvas.height * max - bounds.height - diff

    ctx.translate(0, extra / 2)
  }
}
