const fs = require("fs");
const path = require("path");

const dist = path.join(__dirname, "..", "dist");
fs.mkdirSync(dist, { recursive: true });

for (const file of ["index.html", "style.css", "boot.js", "sheets.js"]) {
  fs.copyFileSync(path.join(__dirname, "..", "public", file), path.join(dist, file));
}

fs.writeFileSync(path.join(dist, "CNAME"), "takeoff.dba.wtf\n");
console.log("copied assets and CNAME to dist/");
