const path = require("path")

const root = (...paths) => path.resolve(__dirname, "..", ...paths)

module.exports = {
  // Source files
  source: (...args) => root("src", ...args),

  // Purescript output in dev
  output: root("output"),

  // Purescript dead code elimination output
  dceOutput: root("dce-output"),

  // Production build files
  build: root("dist"),

  // Static files that get copied to build folder
  public: root("../public")
}
