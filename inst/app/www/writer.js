/* Hanzi Writer Shiny bridge */

(function () {
  "use strict";

  const writers   = new Map(); // id -> HanziWriter
  const paused    = new Map(); // id -> bool
  const inStep    = new Map(); // id -> bool (step mode)
  const stepPos   = new Map(); // id -> last-completed stroke index
  const midStroke = new Map(); // id -> bool (animateStroke in flight)
  const gens      = new Map(); // id -> generation (invalidates stale timeouts)
  const practice  = new Map(); // id -> bool (quiz mode)

  // ---- Core helpers ----------------------------------------------------------

  // HanziWriter v3 has no public cancelAnimation(). The internal renderState
  // is the only way to abort running animations.
  function cancelAll(w) {
    if (w && w._renderState) w._renderState.cancelAll();
  }

  function setSpeed(w, s) {
    w._options.strokeAnimationSpeed = s;
    w._options.delayBetweenStrokes  = Math.round(300 / s);
    w._options.delayBetweenLoops    = Math.round(1400 / s);
  }

  function totalStrokes(w) {
    return (w._character && w._character.strokes) ? w._character.strokes.length : 0;
  }

  function nextGen(id) {
    const g = (gens.get(id) || 0) + 1;
    gens.set(id, g);
    return g;
  }

  // ---- Loop (manual stroke chain for position tracking) ----------------------

  function startLoop(id, w) {
    practice.set(id, false);
    inStep.set(id, false);
    paused.set(id, false);
    midStroke.set(id, false);
    stepPos.set(id, 0);
    const gen = nextGen(id);
    cancelAll(w);
    w.hideCharacter({ duration: 0 });
    setTimeout(function () { loopStep(id, w, 0, gen); }, 50);
  }

  function loopStep(id, w, n, gen) {
    if (gens.get(id) !== gen || paused.get(id) || inStep.get(id) || practice.get(id)) return;
    const total = totalStrokes(w);
    if (n >= total) {
      const delay = (w._options.delayBetweenLoops != null) ? w._options.delayBetweenLoops : 1400;
      setTimeout(function () {
        if (gens.get(id) !== gen || paused.get(id)) return;
        cancelAll(w);
        w.hideCharacter({ duration: 0 });
        setTimeout(function () { loopStep(id, w, 0, gen); }, 50);
      }, delay);
      return;
    }
    midStroke.set(id, true);
    w.animateStroke(n, {
      onComplete: function () {
        midStroke.set(id, false);
        stepPos.set(id, n + 1);
        const strokeDelay = w._options.delayBetweenStrokes || 0;
        if (strokeDelay > 0) {
          setTimeout(function () { loopStep(id, w, n + 1, gen); }, strokeDelay);
        } else {
          loopStep(id, w, n + 1, gen);
        }
      }
    });
  }

  // ---- Step mode helpers -----------------------------------------------------

  function enterStepMode(id, w) {
    inStep.set(id, true);
    paused.set(id, true);
    midStroke.set(id, false);
    nextGen(id);
    cancelAll(w);
  }

  // Show strokes 0..(pos-1) instantly, leave cursor at pos
  function rebuildTo(id, w, pos) {
    cancelAll(w);
    stepPos.set(id, pos);
    w.hideCharacter({ duration: 0 });
    if (pos === 0) return;

    const savedSpd = w._options.strokeAnimationSpeed;
    const savedDly = w._options.delayBetweenStrokes;
    w._options.strokeAnimationSpeed = 999;
    w._options.delayBetweenStrokes  = 0;

    let i = 0;
    function next() {
      if (i >= pos) {
        w._options.strokeAnimationSpeed = savedSpd;
        w._options.delayBetweenStrokes  = savedDly;
        return;
      }
      w.animateStroke(i, { onComplete: function () { i++; next(); } });
    }
    setTimeout(next, 40);
  }

  // ---- Writer factory --------------------------------------------------------

  function getOrCreate(id, char) {
    const el = document.getElementById(id);
    if (!el) return null;
    if (writers.has(id)) return writers.get(id);
    const w = HanziWriter.create(id, char, {
      width:        el.offsetWidth  || 280,
      height:       el.offsetHeight || 280,
      padding:      10,
      strokeColor:  "#1a1a1a",
      outlineColor: "#cccccc",
      drawingColor: "#8b1a1a",
      showOutline:  true,
    });
    writers.set(id, w);
    return w;
  }

  // Poll until w._character is populated for the expected char
  function waitForChar(id, gen, w, callback) {
    function poll() {
      if (gens.get(id) !== gen) return;
      if (totalStrokes(w) > 0) { callback(); }
      else { setTimeout(poll, 40); }
    }
    setTimeout(poll, 0);
  }

  // ---- Shiny message handlers ------------------------------------------------

  Shiny.addCustomMessageHandler("draw_hanzi", function (msg) {
    const w = getOrCreate(msg.target, msg.char);
    if (!w) return;
    practice.set(msg.target, false);
    if (msg.speed != null) setSpeed(w, msg.speed);

    const gen = nextGen(msg.target);
    cancelAll(w);
    midStroke.set(msg.target, false);

    if (w._char !== msg.char) {
      w._character = null; // clear stale data so poll waits for new load
      w.setCharacter(msg.char);
    }

    waitForChar(msg.target, gen, w, function () {
      if (gens.get(msg.target) !== gen) return;
      inStep.set(msg.target, false);
      paused.set(msg.target, false);
      stepPos.set(msg.target, 0);
      w.hideCharacter({ duration: 0 });
      setTimeout(function () { loopStep(msg.target, w, 0, gen); }, 50);
    });
  });

  Shiny.addCustomMessageHandler("writer_pause", function (msg) {
    const w = writers.get(msg.target);
    if (!w) return;
    paused.set(msg.target, true);
    // If mid-stroke: freeze it in place (pauseAnimation). The gen is NOT
    // changed so onComplete can continue the chain when we resume.
    // If between strokes: loopStep's guard clause will catch the paused flag.
    if (midStroke.get(msg.target)) {
      w.pauseAnimation();
    }
  });

  Shiny.addCustomMessageHandler("writer_resume", function (msg) {
    const w = writers.get(msg.target);
    if (!w) return;
    if (inStep.get(msg.target)) {
      startLoop(msg.target, w);
      return;
    }
    paused.set(msg.target, false);
    if (midStroke.get(msg.target)) {
      // Resume the frozen stroke; its onComplete will continue the chain
      w.resumeAnimation();
    } else {
      // Between strokes or between loops
      const pos   = stepPos.get(msg.target) || 0;
      const total = totalStrokes(w);
      if (pos >= total) {
        // Between loops: bump gen to cancel the pending timeout, restart fresh
        const gen = nextGen(msg.target);
        cancelAll(w);
        w.hideCharacter({ duration: 0 });
        setTimeout(function () { loopStep(msg.target, w, 0, gen); }, 50);
      } else {
        // Between strokes: continue with same gen
        loopStep(msg.target, w, pos, gens.get(msg.target));
      }
    }
  });

  Shiny.addCustomMessageHandler("writer_step_forward", function (msg) {
    const w = writers.get(msg.target);
    if (!w) return;
    const wasInStep = inStep.get(msg.target);
    if (!wasInStep) {
      enterStepMode(msg.target, w);
      w.hideCharacter({ duration: 0 });
      stepPos.set(msg.target, 0);
      setTimeout(function () {
        if (totalStrokes(w) > 0) {
          w.animateStroke(0, { onComplete: function () { stepPos.set(msg.target, 1); } });
        }
      }, 40);
    } else {
      const pos = stepPos.get(msg.target) || 0;
      if (pos >= totalStrokes(w)) return;
      w.animateStroke(pos, { onComplete: function () { stepPos.set(msg.target, pos + 1); } });
    }
  });

  Shiny.addCustomMessageHandler("writer_step_back", function (msg) {
    const w = writers.get(msg.target);
    if (!w) return;
    if (!inStep.get(msg.target)) {
      enterStepMode(msg.target, w);
      rebuildTo(msg.target, w, Math.max(0, totalStrokes(w) - 1));
    } else {
      const pos = Math.max(0, (stepPos.get(msg.target) || 0) - 1);
      rebuildTo(msg.target, w, pos);
    }
  });

  Shiny.addCustomMessageHandler("writer_set_speed", function (msg) {
    const w = writers.get(msg.target);
    if (!w) return;
    setSpeed(w, msg.speed);
    if (!paused.get(msg.target) && !inStep.get(msg.target) && !practice.get(msg.target)) {
      startLoop(msg.target, w);
    }
  });

  Shiny.addCustomMessageHandler("writer_toggle_practice", function (msg) {
    const w = writers.get(msg.target);
    if (!w) return;
    if (practice.get(msg.target)) {
      w.cancelQuiz();
      practice.set(msg.target, false);
      startLoop(msg.target, w);
    } else {
      enterStepMode(msg.target, w);
      practice.set(msg.target, true);
      w.quiz({
        onComplete: function () {
          practice.set(msg.target, false);
          Shiny.setInputValue(msg.target + "_quiz_done", true, { priority: "event" });
          startLoop(msg.target, w);
        }
      });
    }
  });

})();
