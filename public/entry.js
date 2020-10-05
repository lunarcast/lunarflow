if (production) {
  require("../dist/bundle");
} else {
  require("../output/Main").main();
}
