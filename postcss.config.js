const production = process.env.NODE_ENV === "production"

const prodPlugins = production
  ? [
      require("cssnano")({
        preset: "default"
      })
    ]
  : []

module.exports = {
  parser: require("postcss-comment"),
  plugins: [
    require("postcss-import")({}),
    require("postcss-simple-vars"),
    require("postcss-font-magician")({
      foundries: "bootstrap google"
    }),
    require("postcss-preset-env")({}),
    ...prodPlugins
  ]
}
