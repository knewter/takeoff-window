const { chromium } = require("playwright");
const { spawn } = require("child_process");
const path = require("path");

const port = 4199;

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function openGame(page, width) {
  await page.goto("http://127.0.0.1:" + port + "/?autostart=1&seed=2026&faction=helios&reveal=0&speed=0", {
    waitUntil: "domcontentloaded"
  });
  await page.waitForSelector(".end-turn");
  const score = await page.locator(".score-strip").innerText();
  if (!/\/6/.test(score)) throw new Error("score strip missing " + score);
  if (width < 500) {
    const open = page.locator(".tabbar button", { hasText: "Open" });
    if (await open.count()) await open.click();
  }
  await page.waitForSelector(".gcard");
}

async function dismissCovering(page) {
  const cont = page.locator("button", { hasText: "Continue" });
  if (await cont.count()) {
    await cont.first().click();
    await page.waitForTimeout(150);
  }
}

async function tapCard(page) {
  await dismissCovering(page);
  await page.locator(".gcard").first().click();
  await page.waitForSelector(".act");
}

async function buttonsFit(page, label) {
  const boxes = await page.locator(".act").evaluateAll((els) =>
    els.map((el) => {
      const r = el.getBoundingClientRect();
      return {
        t: el.textContent.replace(/\s+/g, " ").trim(),
        x: r.x,
        y: r.y,
        right: r.right,
        bottom: r.bottom,
        h: r.height
      };
    })
  );
  const vw = page.viewportSize().width;
  const vh = page.viewportSize().height;
  const bad = boxes.filter((b) => b.x < -1 || b.y < -1 || b.right > vw + 1 || b.bottom > vh + 1 || b.h < 48);
  if (!boxes.length || bad.length) {
    throw new Error(label + " actions not fully visible " + JSON.stringify(boxes));
  }
  const clipped = boxes.some((b) => /\b(Ctrl|Dstr|Neut|Wild|Rate|Drop|Join|Cut)\b/.test(b.t));
  if (clipped) throw new Error(label + " abbreviated " + JSON.stringify(boxes));
  console.log(label, boxes.map((b) => b.t.slice(0, 28) + " h" + Math.round(b.h)).join(" | "));
}

async function rollVerb(page, verb) {
  await tapCard(page);
  const btn = page.locator(".act", { hasText: verb }).first();
  if (await btn.isDisabled()) throw new Error(verb + " disabled on the open card");
  await btn.click();
  const roll = page.locator("button", { hasText: "Roll" });
  if (await roll.count()) await roll.first().click();
  await page.waitForTimeout(200);
  const modal = await page.evaluate(() => {
    const m = document.querySelector(".modal");
    return m ? m.innerText.slice(0, 400) : "no modal";
  });
  console.log("modal after", verb, modal.replace(/\s+/g, " ").slice(0, 240));
  try {
    await page.waitForFunction(() => document.body.innerText.includes("SUCCESS") || document.body.innerText.includes("FAILS"), null, { timeout: 4000 });
  } catch (err) {
    const text = await page.locator("body").innerText();
    const errs = await page.evaluate(() => window.__errs || []);
    throw new Error(verb + " produced no dice. errs " + JSON.stringify(errs) + "\n" + text.slice(0, 800));
  }
  const text = await page.locator(".roll-res").innerText();
  if (text !== "SUCCESS" && text !== "FAILS") throw new Error("no dice result for " + verb + ": " + text);
  const sheet = await page.locator(".modal.sheet").boundingBox();
  const dice = await page.locator(".roll-res").boundingBox();
  if (sheet && dice && sheet.height > dice.y + dice.height + 280) {
    throw new Error("dice sheet too tall " + Math.round(sheet.height));
  }
  const back = page.locator("button", { hasText: "Continue" });
  if (await back.count()) await back.click();
  await page.waitForTimeout(200);
  console.log("rolled", verb, text);
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
    await page.addInitScript(() => {
      window.__errs = [];
      window.addEventListener("error", (ev) => window.__errs.push(String(ev.message)));
    });
    await openGame(page, 412);
    await tapCard(page);
    await buttonsFit(page, "412");
    await page.locator(".act", { hasText: "Take control" }).first().click();
    await page.locator("button", { hasText: "Roll" }).click();
    await page.waitForFunction(() => document.body.innerText.includes("SUCCESS") || document.body.innerText.includes("FAILS"));
    const first = await page.locator(".roll-res").innerText();
    if (first !== "SUCCESS" && first !== "FAILS") throw new Error("no dice result " + first);
    console.log("rolled Take control", first);
    const cont = page.locator("button", { hasText: "Continue" });
    if (await cont.count()) await cont.click();
    await page.waitForTimeout(250);
    await dismissCovering(page);
    await tapCard(page);
    const blocked = page.locator(".act.off", { hasText: "Destroy" });
    const reason = await blocked.innerText();
    if (!/Already attacked/.test(reason)) throw new Error("missing disabled reason " + reason);
    console.log("disabled", reason.replace(/\s+/g, " ").trim());
    await page.locator("[data-sheet-close]").evaluate((el) => el.click());
    await page.waitForTimeout(200);
    const quarter = await page.locator(".hud-q").innerText();
    await page.locator(".end-turn").click();
    const confirmEnd = page.locator("button", { hasText: "End the turn" });
    try {
      await confirmEnd.waitFor({ timeout: 1500 });
      await confirmEnd.click();
    } catch (err) {}
    await page.waitForFunction((q) => {
      const el = document.querySelector(".hud-q");
      return el && el.textContent !== q;
    }, quarter, { timeout: 20000 });
    for (let i = 0; i < 6; i++) {
      await dismissCovering(page);
      if (await page.locator(".end-turn").count()) break;
      await page.waitForTimeout(200);
    }
    await page.waitForSelector(".end-turn", { timeout: 20000 });
    await rollVerb(page, "Destroy");
    if (errors.length) throw new Error(errors.join("\n"));
    await ctx.close();

    for (const size of [
      { w: 360, h: 780 },
      { w: 390, h: 844 },
      { w: 412, h: 915 }
    ]) {
      const c = await browser.newContext({
        viewport: { width: size.w, height: size.h },
        isMobile: true,
        hasTouch: true,
        userAgent: "Mozilla/5.0 (Linux; Android 14; Pixel 7) AppleWebKit/537.36 Chrome/120.0.0.0 Mobile Safari/537.36"
      });
      const p = await c.newPage();
      const errs = [];
      p.on("pageerror", (e) => errs.push(String(e)));
      await p.addInitScript(() => {
        document.addEventListener("DOMContentLoaded", () => {
          document.documentElement.style.fontSize = "125%";
        });
      });
      await openGame(p, size.w);
      await tapCard(p);
      await buttonsFit(p, size.w + "x" + size.h + "@125%");
      if (errs.length) throw new Error(errs.join("\n"));
      await c.close();
    }

    const desk = await browser.newContext({ viewport: { width: 1440, height: 900 } });
    const d = await desk.newPage();
    const derrs = [];
    d.on("pageerror", (e) => derrs.push(String(e)));
    await openGame(d, 1440);
    await tapCard(d);
    await buttonsFit(d, "1440");
    if (derrs.length) throw new Error(derrs.join("\n"));
    await desk.close();
    console.log("roll regression ok");
  } catch (err) {
    fails.push(err);
    console.error(err);
  } finally {
    await browser.close();
    server.kill();
  }
  if (fails.length) process.exit(1);
})();
