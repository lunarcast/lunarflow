if (production) {
  require("../dce-output/Main").main();
} else {
  require("../output/Main").main();
}
