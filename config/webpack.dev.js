const paths = require("./paths")

const webpack = require("webpack")
const common = require("./webpack.common.js")
const { merge } = require("webpack-merge")

module.exports = merge(common, {
  // Set the mode to development or production
  mode: "development",

  // Control how source maps are generated
  devtool: "inline-source-map",

  // Spin up a server for quick development
  devServer: {
    historyApiFallback: true,
    contentBase: paths.build,
    open: true,
    compress: true,
    hot: true,
    port: 8080
  },
  resolve: {
    alias: {
      purescript: paths.output
    }
  },
  plugins: [
    // Only update what has changed on hot reload
    new webpack.HotModuleReplacementPlugin()
  ],
  module: {
    rules: [
      {
        test: /\.(scss|css)$/,
        use: [
          "style-loader",
          {
            loader: "css-loader",
            options: { sourceMap: true, importLoaders: 1 }
          },
          { loader: "postcss-loader", options: { sourceMap: true } },
          { loader: "sass-loader", options: { sourceMap: true } }
        ]
      }
    ]
  }
})
