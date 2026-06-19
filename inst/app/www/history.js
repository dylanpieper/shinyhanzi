/* Recent-character history using localStorage
 *
 * update_history  — prepend a character, deduplicate, cap at 20, re-render
 * clear_history   — wipe localStorage and clear the rendered list
 * Clicks on history items fire the Shiny input "history_click".
 */

(function () {
  "use strict";

  const STORAGE_KEY = "shinyhanzi_history";
  const MAX_ITEMS   = 20;

  function loadHistory() {
    try {
      return JSON.parse(localStorage.getItem(STORAGE_KEY) || "[]");
    } catch (e) {
      return [];
    }
  }

  function saveHistory(hist) {
    try {
      localStorage.setItem(STORAGE_KEY, JSON.stringify(hist));
    } catch (e) {}
  }

  function findContainer() {
    // The history_list div is created by mod_history_ui; look for it by class suffix
    return document.querySelector('[id$="history_list"]');
  }

  function renderHistory(hist) {
    const el = findContainer();
    if (!el) return;
    el.innerHTML = "";
    hist.forEach(function (ch) {
      const a = document.createElement("a");
      a.href      = "javascript:void(0)";
      a.className = "hanzi-tile text-decoration-none text-body";
      a.textContent = ch;
      a.addEventListener("click", function () {
        Shiny.setInputValue("history-history_click", ch, { priority: "event" });
      });
      el.appendChild(a);
    });
  }

  Shiny.addCustomMessageHandler("update_history", function (msg) {
    let hist = loadHistory();
    hist = [msg.char].concat(hist.filter((c) => c !== msg.char));
    if (hist.length > MAX_ITEMS) hist = hist.slice(0, MAX_ITEMS);
    saveHistory(hist);
    renderHistory(hist);
  });

  Shiny.addCustomMessageHandler("clear_history", function () {
    saveHistory([]);
    const el = findContainer();
    if (el) el.innerHTML = "";
  });

  // Restore history on page load (after Shiny connects)
  $(document).on("shiny:connected", function () {
    renderHistory(loadHistory());
  });
})();
