(function () {
  "use strict";

  var snaps = ["peek", "half", "full"];

  function viewport() {
    return window.visualViewport ? window.visualViewport.height : window.innerHeight;
  }

  function yFor(name, h, panelH) {
    if (name === "full") return 0;
    if (name === "half") return Math.max(0, panelH - Math.round(h * 0.52));
    if (name === "peek") return Math.max(0, panelH - 72);
    return panelH + 40;
  }

  function nearest(y, h, panelH) {
    var best = "half";
    var bestD = Infinity;
    snaps.forEach(function (name) {
      var d = Math.abs(yFor(name, h, panelH) - y);
      if (d < bestD) {
        bestD = d;
        best = name;
      }
    });
    return best;
  }

  function mount(root) {
    if (!root || root.__sheet) return;
    var panel = root.querySelector("[data-sheet-panel]");
    var handle = root.querySelector("[data-sheet-handle]");
    var body = root.querySelector("[data-sheet-body]");
    if (!panel || !handle) return;
    root.__sheet = true;

    var dismissible = root.getAttribute("data-sheet-dismiss") !== "0";
    var y = 0;
    var snap = "half";
    var drag = null;

    function panelH() {
      return Math.round(viewport() * 0.92);
    }

    function apply(nextY, animate) {
      var h = viewport();
      var ph = panelH();
      y = nextY;
      panel.style.height = ph + "px";
      panel.style.transition = animate ? "transform 220ms cubic-bezier(.2,.8,.2,1)" : "none";
      panel.style.transform = "translateY(" + y + "px)";
      var name = nearest(y, h, ph);
      if (Math.abs(yFor(name, h, ph) - y) > 24 && y > yFor("peek", h, ph) + 40) name = "closed";
      panel.setAttribute("data-snap", name);
      if (body) {
        var full = name === "full";
        body.style.overflowY = full ? "auto" : "hidden";
        body.style.touchAction = full ? "pan-y" : "none";
      }
    }

    function go(name, animate) {
      var h = viewport();
      var ph = panelH();
      snap = name;
      apply(yFor(name, h, ph), animate);
      panel.setAttribute("data-snap", name);
    }

    function close() {
      apply(viewport() + 40, true);
      panel.setAttribute("data-snap", "closed");
      window.setTimeout(function () {
        var btn = root.querySelector("[data-sheet-close]");
        if (btn) btn.click();
      }, 200);
    }

    function onDown(ev) {
      if (ev.button != null && ev.button !== 0) return;
      var scrolling = body && body.scrollTop > 0 && ev.target !== handle && !handle.contains(ev.target);
      if (scrolling) return;
      if (snap === "full" && body && body.contains(ev.target) && !handle.contains(ev.target) && body.scrollTop > 0) return;
      drag = { id: ev.pointerId, y0: ev.clientY, start: y, t0: Date.now(), lastY: ev.clientY, lastT: Date.now(), vy: 0, moved: false };
      try { handle.setPointerCapture(ev.pointerId); } catch (e) {}
      panel.style.transition = "none";
    }

    function onMove(ev) {
      if (!drag || ev.pointerId !== drag.id) return;
      var dy = ev.clientY - drag.y0;
      if (Math.abs(dy) > 4) drag.moved = true;
      var now = Date.now();
      var dt = Math.max(1, now - drag.lastT);
      drag.vy = (ev.clientY - drag.lastY) / dt;
      drag.lastY = ev.clientY;
      drag.lastT = now;
      var h = viewport();
      var ph = panelH();
      var next = Math.min(ph + 80, Math.max(0, drag.start + dy));
      apply(next, false);
      ev.preventDefault();
    }

    function onUp(ev) {
      if (!drag || ev.pointerId !== drag.id) return;
      var dy = ev.clientY - drag.y0;
      var moved = drag.moved;
      var vy = drag.vy;
      drag = null;
      if (!moved) {
        go(snap === "full" ? "peek" : "full", true);
        return;
      }
      var h = viewport();
      var ph = panelH();
      if (dismissible && y > yFor("peek", h, ph) + 48) {
        close();
        return;
      }
      if (dismissible && vy > 1.2 && y > yFor("half", h, ph)) {
        close();
        return;
      }
      if (vy < -0.6) {
        go(snap === "peek" ? "half" : "full", true);
        return;
      }
      if (vy > 0.6 && !dismissible) {
        go(snap === "full" ? "half" : "peek", true);
        return;
      }
      go(nearest(y, h, ph), true);
    }

    handle.addEventListener("pointerdown", onDown);
    panel.addEventListener("pointerdown", function (ev) {
      if (ev.target === handle || handle.contains(ev.target)) return;
      if (ev.target.closest && ev.target.closest("button, a, input, textarea, select, label")) return;
      if (snap !== "full") onDown(ev);
    });
    window.addEventListener("pointermove", onMove, { passive: false });
    window.addEventListener("pointerup", onUp);
    window.addEventListener("pointercancel", onUp);
    window.addEventListener("resize", function () { go(snap, false); });

    go("half", false);
  }

  function scan() {
    document.querySelectorAll("[data-sheet]").forEach(mount);
  }

  if (document.body) {
    new MutationObserver(scan).observe(document.body, { childList: true, subtree: true });
  }
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", scan);
  } else {
    scan();
  }
  window.TakeoffSheets = { scan: scan };
})();
