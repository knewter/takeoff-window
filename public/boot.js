(function () {
  "use strict";

  function syncPhone() {
    var w = window.innerWidth;
    var h = window.innerHeight;
    var on = w <= 820 || (h <= 520 && w <= 960);
    document.body.classList.toggle("phone", on);
    document.body.classList.toggle("landscape-phone", on && h <= 520);
    document.documentElement.classList.toggle("phone", on);
  }

  syncPhone();
  window.addEventListener("resize", syncPhone);

  var q = new URLSearchParams(location.search);
  var app = Elm.Main.init({
    node: document.getElementById("app"),
    flags: {
      seed: q.get("seed"),
      autoplay: q.get("autoplay") === "1" || q.get("autoplay") === "true",
      faction: q.get("faction"),
      numAI: Number(q.get("ai") || q.get("numAI") || 3),
      screen: q.get("screen"),
      autostart: q.get("autostart") === "1",
      reveal: q.get("reveal") !== "0",
      width: window.innerWidth,
      height: window.innerHeight,
      headless: q.get("headless") === "1",
      n: Number(q.get("n") || 20),
      speed: q.get("speed") ? Number(q.get("speed")) : 420,
      now: Date.now()
    }
  });

  var state = null;
  var waiters = new Map();
  var seq = 0;

  app.ports.outgoing.subscribe(function (msg) {
    if (!msg) return;
    if (msg.tag === "state") state = msg.state;
    if (msg.tag === "reply") {
      var resolve = waiters.get(msg.id);
      if (resolve) {
        waiters.delete(msg.id);
        resolve(msg.payload);
      }
    }
  });

  function call(tag, extra) {
    var id = ++seq;
    return new Promise(function (resolve) {
      waiters.set(id, resolve);
      app.ports.incoming.send(Object.assign({ id: id, tag: tag }, extra || {}));
    });
  }

  window.game = {
    start: function (opts) {
      opts = opts || {};
      return call("start", {
        seed: opts.seed != null ? String(opts.seed) : null,
        faction: opts.faction || null,
        numAI: opts.numAI,
        skipReveal: !!opts.skipReveal,
        autoplay: !!opts.autoplay
      });
    },
    autoplay: function () {
      return call("autoplay", {});
    },
    playToEnd: function () {
      return call("playToEnd", {});
    },
    playTurns: function (n) {
      return call("playTurns", { n: n });
    },
    runHeadless: function (n, seed) {
      return call("runHeadless", { n: n || 24, seedInt: seed || 2026, human: 0 });
    },
    get state() {
      return state;
    }
  };
})();
