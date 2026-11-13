const path = require("path");
const { Elm } = require(path.join(__dirname, "..", "dist", "sim.js"));

const smoke = process.argv.includes("--smoke");
const games = smoke ? 24 : 360;
const humanGames = smoke ? 12 : 240;

const app = Elm.Sim.init({
  flags: { games: games, humanGames: humanGames, seed: 2026 }
});

app.ports.emit.subscribe((raw) => {
  const report = JSON.parse(raw);
  console.log(JSON.stringify(report, null, 2));
  if (report.errors !== 0) {
    console.error("simulation reported " + report.errors + " unfinished games");
    process.exit(1);
  }
  if (!smoke) {
    const rates = report.winRates || {};
    const bad = Object.entries(rates).filter(([, r]) => r < 0.08 || r > 0.3);
    if (bad.length) {
      console.error("faction win rate outside 8–30%: " + JSON.stringify(bad));
      process.exit(1);
    }
    if (report.humanWinRate < 0.25 || report.humanWinRate > 0.45) {
      console.error("human policy win rate outside 25–45%: " + report.humanWinRate);
      process.exit(1);
    }
  }
  process.exit(0);
});
