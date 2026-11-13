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
  await page.goto("http://127.0.0.1:" + port + "/", { waitUntil: "domcontentloaded" });
  await page.waitForSelector(".wordmark");
  await shot(page, "desktop-title.png");
  const started = await page.evaluate(async () => {
    const result = await Promise.race([
      window.game.start({ seed: "2026", faction: "helios", skipReveal: true }),
      new Promise((_, reject) => setTimeout(() => reject(new Error("start timed out")), 4000))
    ]);
    return result;
  });
  if (!started || typeof started.round !== "number" || started.gameOver !== false) {
    throw new Error("window.game.start() did not resolve a live game: " + JSON.stringify(started));
  }
  console.log("start resolved", JSON.stringify({ round: started.round, year: started.year, seed: started.seed }));

  await page.goto("http://127.0.0.1:" + port + "/?screen=faction", { waitUntil: "domcontentloaded" });
  await page.waitForSelector(".faction-grid");
  await shot(page, "desktop-factions.png");

  await page.goto("http://127.0.0.1:" + port + "/?autostart=1&seed=11&faction=helios&reveal=0", { waitUntil: "domcontentloaded" });
  await page.waitForFunction(() => window.game && window.game.state);
  await page.evaluate(() => window.game.playTurns(6));
  await page.waitForSelector(".tree-wrap");
  await page.waitForSelector(".market");
  await shot(page, "desktop-game.png");
  await desktop.close();

  const phone = await browser.newContext({ viewport: { width: 390, height: 844 }, isMobile: true, hasTouch: true });
  const mobile = await phone.newPage();
  await mobile.goto("http://127.0.0.1:" + port + "/?screen=faction", { waitUntil: "domcontentloaded" });
  await mobile.waitForSelector(".faction-grid");
  await mobile.evaluate(() => {
    const card = [...document.querySelectorAll(".faction-card h3")].find((el) => el.textContent.includes("Crown"));
    if (card) card.scrollIntoView({ block: "center" });
  });
  await shot(mobile, "mobile-factions.png");
  await mobile.goto("http://127.0.0.1:" + port + "/?autostart=1&seed=11&faction=helios&reveal=0", { waitUntil: "domcontentloaded" });
  await mobile.waitForFunction(() => window.game && window.game.state);
  const ended = await mobile.evaluate(async () => {
    const result = await Promise.race([
      window.game.playToEnd(),
      new Promise((_, reject) => setTimeout(() => reject(new Error("playToEnd timed out")), 15000))
    ]);
    return result;
  });
  if (!ended || ended.gameOver !== true) {
    throw new Error("playToEnd did not finish the game: " + JSON.stringify(ended && { gameOver: ended.gameOver, round: ended.round }));
  }
  await mobile.waitForSelector(".victory");
  const victoryText = await mobile.locator(".victory").innerText();
  if (!/window/i.test(victoryText)) {
    throw new Error("victory sheet missing: " + victoryText);
  }
  console.log("victory sheet", victoryText.split("\n")[0]);
  await shot(mobile, "mobile-victory.png");
  await mobile.goto("http://127.0.0.1:" + port + "/?autostart=1&seed=11&faction=helios&reveal=0", { waitUntil: "domcontentloaded" });
  await mobile.evaluate(() => window.game.playTurns(8));
  await mobile.waitForSelector(".tabbar");
  await shot(mobile, "mobile-portrait.png");
  await phone.close();

  const land = await browser.newContext({ viewport: { width: 844, height: 390 }, isMobile: true, hasTouch: true });
  const wide = await land.newPage();
  await wide.goto("http://127.0.0.1:" + port + "/?autostart=1&seed=11&faction=open-wave&reveal=0", { waitUntil: "domcontentloaded" });
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
