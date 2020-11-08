console.log("Loaded root")
console.time("Loaded purescript")

import "./parser.ts"

import("purescript/Main").then(({ main }) => {
  console.timeEnd("Loaded purescript")

  main()
})
