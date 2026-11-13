const { chromium } = require("playwright");
const { spawn } = require("child_process");
const path = require("path");

const port = 4199;

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function ready(page) {
  await page.goto("http://127.0.0.1:" + port + "/?autostart=1&seed=2026&faction=helios&reveal=0", {
    waitUntil: "domcontentloaded"
  });
  await page.waitForSelector(".action-bar");
  const got = page.locator("button", { hasText: "Got it" });
  if (await got.count()) {
    await got.click();
    await page.locator("button", { hasText: "Continue" }).click();
    await page.waitForTimeout(200);
  }
}

async function chipsFit(page, width) {
  if (width < 500) {
    const open = page.locator(".tabbar button", { hasText: "Open" });
    if (await open.count()) await open.click();
  }
  await page.locator(".gcard").first().click();
  await page.waitForTimeout(100);
  const boxes = await page.locator(".action-bar .chip").evaluateAll((els) =>
    els.map((el) => {
      const r = el.getBoundingClientRect();
      return { t: el.textContent.trim(), x: r.x, right: r.right, y: r.y, bottom: r.bottom, w: r.width };
    })
  );
  const vw = width;
  const bad = boxes.filter((b) => b.x < -1 || b.right > vw + 1 || b.w < 8);
  if (!boxes.length || bad.length) {
    throw new Error(width + " chips clipped " + JSON.stringify(boxes));
  }
  console.log(width, boxes.map((b) => b.t + ":" + Math.round(b.right)).join(" "));
}

async function rollKind(page, chipText) {
  await page.locator(".gcard").first().click();
  await page.waitForTimeout(80);
  const more = page.locator(".chip", { hasText: "More" });
  if (await more.count()) await more.click();
  await page.locator(".chip", { hasText: chipText }).first().click();
  await page.locator("button", { hasText: "Roll 2d6" }).click();
  await page.waitForFunction(() => document.body.innerText.includes("SUCCESS") || document.body.innerText.includes("FAILS"));
  const text = await page.locator(".roll-res").innerText();
  if (text !== "SUCCESS" && text !== "FAILS") throw new Error("no dice result for " + chipText + ": " + text);
  await page.locator("button", { hasText: "Continue" }).click();
  await page.waitForTimeout(150);
  console.log("rolled", chipText, text);
}

(async () => {
  const server = spawn(process.execPath, [path.join(__dirname, "serve.js"), path.join(__dirname, "..", "dist"), String(port)], {
    stdio: "ignore"
  });
  await sleep(300);
  const browser = await chromium.launch({ args: ["--no-sandbox"] });
  const fails = [];
  try {
    const ctx = await browser.newContext({
      viewport: { width: 412, height: 915 },
      isMobile: true,
      hasTouch: true,
      userAgent: "Mozilla/5.0 (Linux; Android 14; Pixel 7) AppleWebKit/537.36 Chrome/120.0.0.0 Mobile Safari/537.36"
    });
    const page = await ctx.newPage();
    const errors = [];
    page.on("pageerror", (e) => errors.push(String(e)));
    await ready(page);
    const open = page.locator(".tabbar button", { hasText: "Open" });
    if (await open.count()) await open.click();
    await rollKind(page, "Ctrl");
    if (errors.length) throw new Error("control " + errors.join("\n"));
    await ctx.close();

    const ctxD = await browser.newContext({
      viewport: { width: 412, height: 915 },
      isMobile: true,
      hasTouch: true
    });
    const pageD = await ctxD.newPage();
    const errorsD = [];
    pageD.on("pageerror", (e) => errorsD.push(String(e)));
    await ready(pageD);
    const openD = pageD.locator(".tabbar button", { hasText: "Open" });
    if (await openD.count()) await openD.click();
    await rollKind(pageD, "Dstr");
    if (errorsD.length) throw new Error("destroy " + errorsD.join("\n"));
    await ctxD.close();

    for (const width of [360, 390, 412]) {
      const c = await browser.newContext({ viewport: { width: width, height: 800 }, isMobile: true, hasTouch: true });
      const p = await c.newPage();
      await p.addInitScript(() => { document.documentElement.style.fontSize = "120%"; });
      await ready(p);
      await chipsFit(p, width);
      await p.evaluate(() => window.game.playTurns(5));
      await p.waitForTimeout(100);
      const rivals = p.locator(".tabbar button", { hasText: "Rivals" });
      if (await rivals.count()) await rivals.click();
      const owned = p.locator(".rchip");
      if (await owned.count()) {
        await owned.first().click();
        await p.waitForTimeout(80);
        const boxes = await p.locator(".action-bar .chip").evaluateAll((els) =>
          els.map((el) => {
            const r = el.getBoundingClientRect();
            return { t: el.textContent.trim(), right: r.right, x: r.x };
          })
        );
        const clipped = boxes.filter((b) => b.x < -1 || b.right > width + 1);
        if (clipped.length) throw new Error(width + " neutralize clipped " + JSON.stringify(boxes));
        console.log(width, "owned", boxes.map((b) => b.t).join(" "));
      }
      await c.close();
    }
  } catch (err) {
    fails.push(err);
  }
  await browser.close();
  server.kill();
  if (fails.length) {
    console.error(fails[0]);
    process.exit(1);
  }
  console.log("roll regression ok");
})().catch((err) => {
  console.error(err);
  process.exit(1);
});
