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
    const bad = Object.entries(rates).filter(([, r]) => r < 0.12 || r > 0.22);
    if (bad.length) {
      console.error("faction win rate outside 12–22%: " + JSON.stringify(bad));
      process.exit(1);
    }
    if (report.humanWinRate < 0.25 || report.humanWinRate > 0.35) {
      console.error("human policy win rate outside 25–35%: " + report.humanWinRate);
      process.exit(1);
    }
    const lowHuman = (report.humanByFaction || []).filter((row) => row.games > 0 && row.winRate < 0.15);
    if (lowHuman.length) {
      console.error("human policy under 15% for a faction: " + JSON.stringify(lowHuman));
      process.exit(1);
    }
  }
  process.exit(0);
});
