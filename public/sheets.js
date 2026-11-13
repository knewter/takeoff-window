(function () {
  "use strict";

  function isPhone() {
    var w = window.innerWidth;
    var h = window.innerHeight;
    return w <= 820 || (h <= 520 && w <= 960);
  }

  function viewport() {
    return window.visualViewport ? window.visualViewport.height : window.innerHeight;
  }

  function heightFor(name, h) {
    var full = Math.round(h * 0.92);
    if (name === "full") return full;
    if (name === "half") return Math.min(Math.round(h * 0.56), full);
    if (name === "peek") return Math.min(176, full);
    return 0;
  }

  function nearest(px, h) {
    var best = "half";
    var bestD = Infinity;
    ["peek", "half", "full"].forEach(function (name) {
      var d = Math.abs(heightFor(name, h) - px);
      if (d < bestD) {
        bestD = d;
        best = name;
      }
    });
    return best;
  }

  function clearDesktop(panel) {
    panel.style.height = "";
    panel.style.transform = "";
    panel.style.transition = "";
    panel.style.bottom = "";
  }

  function hoistFoot(panel, body) {
    if (!body) return;
    var row = body.querySelector(".btn-row");
    if (!row || row.parentElement === panel) return;
    row.classList.add("sheet-foot");
    panel.appendChild(row);
  }

  function mount(root) {
    var panel = root.querySelector("[data-sheet-panel]");
    var handle = root.querySelector("[data-sheet-handle]");
    var body = root.querySelector("[data-sheet-body]");
    if (!panel || !handle) return;

    if (!isPhone()) {
      clearDesktop(panel);
      root.__sheet = "desktop";
      return;
    }
    if (root.__sheet === "phone") {
      hoistFoot(panel, body);
      return;
    }
    root.__sheet = "phone";

    var dismissible = root.getAttribute("data-sheet-dismiss") !== "0";
    var snap = root.getAttribute("data-sheet-open") || "half";
    var drag = null;
    hoistFoot(panel, body);
    if (body) {
      new MutationObserver(function () { hoistFoot(panel, body); }).observe(body, { childList: true, subtree: true });
    }

    function apply(px, animate) {
      var h = viewport();
      var full = heightFor("full", h);
      var next = Math.max(0, Math.min(full, px));
      panel.style.bottom = "0";
      panel.style.transform = "none";
      panel.style.transition = animate ? "height 200ms cubic-bezier(.2,.8,.2,1)" : "none";
      panel.style.height = next + "px";
      var name = next < 48 ? "closed" : nearest(next, h);
      panel.setAttribute("data-snap", name);
      if (body) body.style.overflowY = "auto";
    }

    function go(name, animate) {
      snap = name;
      apply(heightFor(name, viewport()), animate);
      panel.setAttribute("data-snap", name);
    }

    function close() {
      apply(0, true);
      panel.setAttribute("data-snap", "closed");
      window.setTimeout(function () {
        var btn = root.querySelector("[data-sheet-close]");
        if (btn) btn.click();
      }, 180);
    }

    function onDown(ev) {
      if (!isPhone()) return;
      if (ev.button != null && ev.button !== 0) return;
      if (body && body.scrollTop > 0 && body.contains(ev.target) && !handle.contains(ev.target)) return;
      drag = {
        id: ev.pointerId,
        y0: ev.clientY,
        h0: panel.getBoundingClientRect().height,
        lastY: ev.clientY,
        lastT: Date.now(),
        vy: 0,
        moved: false
      };
      try { (handle.contains(ev.target) ? handle : panel).setPointerCapture(ev.pointerId); } catch (e) {}
      panel.style.transition = "none";
    }

    function onMove(ev) {
      if (!drag || ev.pointerId !== drag.id) return;
      var dy = ev.clientY - drag.y0;
      if (Math.abs(dy) > 5) drag.moved = true;
      var now = Date.now();
      var dt = Math.max(1, now - drag.lastT);
      drag.vy = (ev.clientY - drag.lastY) / dt;
      drag.lastY = ev.clientY;
      drag.lastT = now;
      apply(drag.h0 - dy, false);
      if (drag.moved) ev.preventDefault();
    }

    function onUp(ev) {
      if (!drag || ev.pointerId !== drag.id) return;
      var moved = drag.moved;
      var vy = drag.vy;
      drag = null;
      if (!moved) {
        go(snap === "full" ? "peek" : "full", true);
        return;
      }
      var h = viewport();
      var px = panel.getBoundingClientRect().height;
      if (dismissible && (px < 56 || (vy > 1.15 && px < heightFor("half", h)))) {
        close();
        return;
      }
      if (vy < -0.55) {
        go(snap === "peek" ? "half" : "full", true);
        return;
      }
      go(nearest(px, h), true);
    }

    handle.addEventListener("pointerdown", onDown);
    panel.addEventListener("pointerdown", function (ev) {
      if (handle.contains(ev.target)) return;
      if (ev.target.closest && ev.target.closest("button, a, input, textarea, select, label")) return;
      if (snap !== "full") onDown(ev);
    });
    window.addEventListener("pointermove", onMove, { passive: false });
    window.addEventListener("pointerup", onUp);
    window.addEventListener("pointercancel", onUp);
    go(snap, false);
  }

  function scan() {
    document.querySelectorAll("[data-sheet]").forEach(mount);
  }

  window.addEventListener("resize", scan);
  if (document.body) new MutationObserver(scan).observe(document.body, { childList: true, subtree: true });
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", scan);
  else scan();
  window.TakeoffSheets = { scan: scan };
})();
