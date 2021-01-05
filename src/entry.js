console.log("Loaded root")
console.time("Loaded purescript")

import("purescript/Main").then(({ main }) => {
  console.timeEnd("Loaded purescript")

  main()
})
