const paths = require("./paths")

const { CleanWebpackPlugin } = require("clean-webpack-plugin")
const { ESBuildPlugin } = require("esbuild-loader")
const HtmlWebpackPlugin = require("html-webpack-plugin")

module.exports = {
  entry: [paths.source("entry.js"), paths.source("styles", "index.scss")],

  output: {
    path: paths.build,
    filename: "[name].bundle.js",
    publicPath: "./"
  },

  plugins: [
    // Removes/cleans build folders and unused assets when rebuilding
    new CleanWebpackPlugin(),

    // Generates an HTML file from a template
    new HtmlWebpackPlugin({
      //   favicon: paths.src + '/images/favicon.png',
      template: paths.source("index.html"), // template file
      filename: "index.html" // output file
    }),

    // Faster builds
    new ESBuildPlugin()
  ],
  module: {
    rules: [
      // Esbuild for faster build times
      {
        test: /\.js$/,
        loader: "esbuild-loader",
        options: {
          loader: "jsx"
        }
      },
      {
        test: /\.ts$/,
        loader: "esbuild-loader",
        options: {
          loader: "ts"
        }
      },
      {
        test: /\.ne$/,
        use: [
          {
            loader: "nearley-loader"
          }
        ]
      }
    ]
  },
  resolve: {
    alias: {
      // Preact for smaller bundles
      react: "preact/compat",
      "react-dom": "preact/compat",
      "react-dom/server": "preact/compat"
    }
  }
}
