(function () {
  "use strict";

  // Compute how many hanzi tiles fit in one row inside the Appears In card,
  // accounting for the two nav buttons, then push the count as a Shiny input.
  function updateAppearsInPageSize() {
    var el = document.getElementById("appears_in-measure");
    if (!el) return;
    var fs = parseFloat(getComputedStyle(document.documentElement).fontSize) || 16;
    var tileSlot = (2.5 + 0.5) * fs;   // tile width + gap-2
    var btnSlot  = 2 * (2.25 + 0.5) * fs; // two btn-sm + gaps (approx)
    var count = Math.max(1, Math.floor((el.offsetWidth - btnSlot) / tileSlot));
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue("appears_in_page_size", count);
    }
  }

  $(document).on("shiny:connected", function () {
    updateAppearsInPageSize();
    var el = document.getElementById("appears_in-measure");
    if (el && window.ResizeObserver) {
      new ResizeObserver(updateAppearsInPageSize).observe(el);
    }
  });
})();
