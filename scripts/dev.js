const { execSync, spawn } = require("child_process");
const fs = require("fs");
const path = require("path");

function build() {
  execSync("npx elm make src/Main.elm --output=dist/game.js", { stdio: "inherit" });
  require("./copy-assets");
}

build();
fs.watch(path.join(__dirname, "..", "src"), { recursive: true }, () => {
  try {
    build();
  } catch (err) {
    console.error(err.message || err);
  }
});

spawn(process.execPath, [path.join(__dirname, "serve.js"), path.join(__dirname, "..", "dist"), "8000"], {
  stdio: "inherit"
});
