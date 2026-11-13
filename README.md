# Takeoff Window

A satirical hidden-faction strategy game about the race to AGI, 2026–2031. One human patron sits down with two or three AI rivals. Each chair has a secret goal. The public way to win is the same for everyone: hold six settled groups when the quarter ends.

This is an original work. It is not affiliated with, endorsed by, or derived from any trademarked board game. Real-world labs, funds, and founders appear only as clearly marked parody.

## How to play

The window runs from 2026 through 2031, one round per quarter (20 rounds). If nobody has won when 2031 arrives, the closest patron is scored.

You and each rival are a patron with power, income, and three control sockets. Groups in the open field — chipmakers, labs, grids, funds, headlines — can be attacked:

- **Control** pulls a group, and the branch under it, into your tree. One successful control per turn, and you need a free socket.
- **Neutralize** cuts a group loose. It and its subordinates return to the open field.
- **Destroy** removes a group. Subordinates drift back to the open field.

Roll 2d6. You succeed when the total is less than or equal to your strength and is not 12. A 12 always fails.

```
strength = attacker power
         + transfer from the parent
         + money spent (at most 3)
         + alignment and faction modifiers
         − resistance
         − the defender's parent
         − the defender's spend
         − 2  (every attack is contested)
```

Shared alignments help Control and hurt Neutralize or Destroy (±3). Opposite alignments do the reverse. Corporate/Open, Safety/Accelerationist, and Nationalist/Chaotic are the opposite pairs. Academic stands alone.

Each turn you collect income, then take two actions. Only one of them may be an attack. The other can reorganize a group onto a free socket, release a group, attempt a weights leak (Open Wave, if you hold a lab), or pass. A headline is drawn at the start of each quarter.

A group you just took is privileged and cannot be attacked yet. Most goals count that group only after the privilege ends. The Open Wave's four-group network counts immediately.

Trees are meant to grow past a single row. Low-socket groups hang under a group that still has a socket, once the patron already has someone on the chart.

### Secret goals

| Patron | Goal |
| --- | --- |
| Helios Dynamics | 30 compute. |
| The Open Wave | A weights leak and three Open groups, or the Open Weights Collective plus four groups. |
| The Pause Compact | Neutralize or destroy two Frontier groups that had an owner, or hold a regulator, a media group, and any third group. |
| Directorate 7 | A Frontier group, a regulator, and five settled groups, or five settled groups including two Nationalist ones. |
| The Long Plan | A chipmaker, a hyperscaler, and five settled groups, or 24 compute and a strict compute lead. |
| Crown Sovereign | A power group, a datacenter, and a chipmaker, or 72 money and four settled groups. |

Anyone also wins by holding six settled groups.

Helios hits labs and chipmakers a little harder. Open Wave hits Open groups harder and may spend 4 money to leak weights (succeeds on 10 or less, a 12 still fails). Pause is stronger when neutralizing or destroying Frontier groups. Directorate is stronger against founders and labs. Long Plan chipmakers pay an extra income and ignore export-control penalties. Crown's power groups and datacenters pay an extra income.

## Build and run

Requires Node and the Elm 0.19.1 compiler (installed by `npm install` via the `elm` package).

```bash
npm install
npm test          # elm-test: attacks, income, control, wins
npm run sim       # 360 AI games and 240 scripted-human games
npm run build     # dist/index.html, game.js, style.css, boot.js, CNAME
npm run dev       # rebuild on change and serve dist/ on port 8000
```

`npm run sim:smoke` plays a short batch and fails only if a game does not finish. The full `sim` script also checks that every faction wins between 8% and 30% and that the scripted human wins between 25% and 45%.

Open `dist/index.html` or the dev server. Useful query parameters:

- `?seed=2026` deals a reproducible game
- `?autostart=1&seed=2026&faction=helios&reveal=0` skips the briefing
- `?autoplay=1&seed=2026` plays every seat with the rival policy
- `?screen=faction` opens patron selection

From the browser console, `window.game` exposes `start`, `autoplay`, `playToEnd`, `playTurns`, `runHeadless`, and `state`.

## Balance, from 360 seeded games

Zero games failed to finish. The median length is 13 quarters (mean 13.1, 25th percentile 9, 75th percentile 18). About 97% of games grew a tree at least two levels deep (average depth 2.94).

| Faction | Win rate |
| --- | --- |
| Helios Dynamics | 18.6% |
| The Open Wave | 20.6% |
| The Pause Compact | 11.7% |
| Directorate 7 | 14.4% |
| The Long Plan | 17.8% |
| Crown Sovereign | 16.9% |

A scripted but not omniscient human policy, rotating through every patron, won **30.8%** of 240 games against three rivals.

## Deploy

The site is published to GitHub Pages from the `master` branch. `.github/workflows/pages.yml` builds `dist/` and deploys it with `actions/configure-pages`, `actions/upload-pages-artifact`, and `actions/deploy-pages`. Asset paths are relative, so the same build works at the domain root. `dist/CNAME` contains `takeoff.dba.wtf`.

In the repository settings, set **Pages → Build and deployment → Source** to **GitHub Actions**. The DNS record is a CNAME:

```
takeoff  CNAME  knewter.github.io
```

CI (`.github/workflows/ci.yml`) runs on push and pull request to `master`: build, `elm-test`, and the simulation smoke test.
