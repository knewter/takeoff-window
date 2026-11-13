const { spawn } = require("child_process");
const path = require("path");
const fs = require("fs");
const { chromium } = require("playwright");

const outDir = "/opt/cursor/artifacts";
fs.mkdirSync(outDir, { recursive: true });

const port = 4173;
const server = spawn(process.execPath, [path.join(__dirname, "serve.js"), path.join(__dirname, "..", "dist"), String(port)], {
  stdio: "inherit"
});

function wait(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function shot(page, file) {
  const target = path.join(outDir, file);
  await page.screenshot({ path: target, fullPage: false });
  console.log("wrote", target);
}

(async () => {
  await wait(400);
  const browser = await chromium.launch({ args: ["--no-sandbox", "--disable-dev-shm-usage"] });
  const desktop = await browser.newContext({ viewport: { width: 1440, height: 900 } });
  const page = await desktop.newPage();
  await page.goto("http://127.0.0.1:" + port + "/", { waitUntil: "networkidle" });
  await page.waitForSelector(".wordmark");
  await shot(page, "desktop-title.png");

  await page.goto("http://127.0.0.1:" + port + "/?screen=faction", { waitUntil: "networkidle" });
  await page.waitForSelector(".faction-grid");
  await shot(page, "desktop-factions.png");

  await page.goto("http://127.0.0.1:" + port + "/?autostart=1&seed=11&faction=helios&reveal=0", { waitUntil: "networkidle" });
  await page.waitForFunction(() => window.game && window.game.state);
  await page.evaluate(() => window.game.playTurns(12));
  await page.waitForSelector(".tree-wrap");
  await shot(page, "desktop-game.png");
  await desktop.close();

  const phone = await browser.newContext({ viewport: { width: 390, height: 844 }, isMobile: true, hasTouch: true });
  const mobile = await phone.newPage();
  await mobile.goto("http://127.0.0.1:" + port + "/?autostart=1&seed=11&faction=helios&reveal=0", { waitUntil: "networkidle" });
  await mobile.waitForFunction(() => window.game && window.game.state);
  await mobile.evaluate(() => window.game.playTurns(12));
  await mobile.waitForSelector(".tabbar");
  await shot(mobile, "mobile-portrait.png");
  await phone.close();

  const land = await browser.newContext({ viewport: { width: 844, height: 390 }, isMobile: true, hasTouch: true });
  const wide = await land.newPage();
  await wide.goto("http://127.0.0.1:" + port + "/?autostart=1&seed=11&faction=open-wave&reveal=0", { waitUntil: "networkidle" });
  await wide.waitForFunction(() => window.game && window.game.state);
  await wide.evaluate(() => window.game.playTurns(10));
  await wide.waitForSelector(".tabbar");
  await shot(wide, "mobile-landscape.png");
  await land.close();

  await browser.close();
  server.kill();
  process.exit(0);
})().catch((err) => {
  console.error(err);
  server.kill();
  process.exit(1);
});
