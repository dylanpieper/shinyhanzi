/* Frequency distribution chart — D3 v7 */

(function () {
  "use strict";

  window.drawFreqChart = function (containerId, data, onCharClick) {
    var container = document.getElementById(containerId);
    if (!container || typeof d3 === "undefined") return;

    d3.select(container).selectAll("*").remove();

    var tipId = containerId + "-tip";
    d3.select("#" + tipId).remove();

    var margin = { top: 28, right: 32, bottom: 60, left: 66 };
    var W  = container.clientWidth  || 900;
    var H  = container.clientHeight || 460;
    var iW = W - margin.left - margin.right;
    var iH = H - margin.top  - margin.bottom;

    var svg = d3.select(container).append("svg").attr("width", W).attr("height", H)
      // Prevent page scroll when wheeling inside the chart
      .style("display", "block");

    var clipId = containerId + "-clip";
    svg.append("defs").append("clipPath").attr("id", clipId)
       .append("rect").attr("width", iW).attr("height", iH + 30);

    var g     = svg.append("g").attr("transform", "translate(" + margin.left + "," + margin.top + ")");
    var gClip = g.append("g").attr("clip-path", "url(#" + clipId + ")");

    var maxRank = d3.max(data, function (d) { return d.rank; });
    var xBase   = d3.scaleLog().domain([0.88, maxRank]).range([0, iW]);
    var yScale  = d3.scaleLinear().domain([0, 100]).range([iH, 0]);

    // ── Axes ──────────────────────────────────────────────────────────────────
    var gX = g.append("g").attr("transform", "translate(0," + iH + ")");
    var gY = g.append("g");

    var decadeTicks = [1, 10, 100, 1000, 10000].filter(function (d) { return d <= maxRank; });
    function applyXAxis(xs) {
      gX.call(d3.axisBottom(xs).tickValues(decadeTicks).tickFormat(d3.format(",")));
      gX.selectAll("text").attr("fill", "#555").style("font-size", "13px");
      gX.selectAll("line,path").attr("stroke", "#ddd");
    }
    gY.call(d3.axisLeft(yScale).tickFormat(function (d) { return d + "%"; }));
    gY.selectAll("text").attr("fill", "#555").style("font-size", "13px");
    gY.selectAll("line,path").attr("stroke", "#ddd");
    applyXAxis(xBase);

    g.append("text").attr("x", iW / 2).attr("y", iH + 52)
      .attr("text-anchor", "middle").attr("fill", "#666").style("font-size", "14px")
      .text("Character rank (log scale)");
    g.append("text").attr("transform", "rotate(-90)").attr("x", -iH / 2).attr("y", -54)
      .attr("text-anchor", "middle").attr("fill", "#666").style("font-size", "14px")
      .text("Cumulative text coverage");

    var hint = g.append("text").attr("x", iW).attr("y", -8)
      .attr("text-anchor", "end").attr("fill", "#bbb").style("font-size", "11px")
      .text("scroll to zoom · drag to pan");

    // ── Curve ─────────────────────────────────────────────────────────────────
    function makeLine(xs) {
      return d3.line()
        .x(function (d) { return xs(d.rank); })
        .y(function (d) { return yScale(d.cumulative_pct); });
    }
    var curvePath = gClip.append("path")
      .datum(data).attr("fill", "none").attr("stroke", "#8b1a1a").attr("stroke-width", 2)
      .attr("d", makeLine(xBase));

    var animMs   = 1600;
    var totalLen = curvePath.node().getTotalLength();
    curvePath
      .attr("stroke-dasharray",  totalLen + " " + totalLen)
      .attr("stroke-dashoffset", totalLen)
      .transition().duration(animMs).ease(d3.easeLinear)
      .attr("stroke-dashoffset", 0);

    // ── Size / opacity ─────────────────────────────────────────────────────────
    function charSz(rank, k) {
      var decay = 22 * Math.exp(-rank / 55);
      var t     = Math.min(1, (k - 1) / 9);
      return Math.max(0, decay * (1 - t) + 15 * t);
    }
    function charAl(rank, k) {
      var decayAl = Math.min(1, Math.max(0.08, Math.exp(-rank / 55)));
      var t       = Math.min(1, (k - 1) / 9);
      return decayAl * (1 - t) + t;
    }

    // ── Tooltip ───────────────────────────────────────────────────────────────
    var tip = d3.select("body").append("div").attr("id", tipId)
      .style("position",       "fixed")
      .style("background",     "rgba(26,26,26,0.92)")
      .style("color",          "#fff")
      .style("padding",        "8px 12px")
      .style("border-radius",  "7px")
      .style("font-size",      "13px")
      .style("line-height",    "1.6")
      .style("pointer-events", "none")
      .style("display",        "none")
      .style("z-index",        "99999")
      .style("max-width",      "220px");

    function showTip(ev, d) {
      var pct      = parseFloat(d.cumulative_pct).toFixed(1);
      var pinyin   = (d.pinyin && d.pinyin !== "null") ? d.pinyin : "";
      var gloss    = (d.gloss  && d.gloss  !== "null") ? d.gloss  : "";
      var shortDef = gloss ? gloss.split(";")[0].trim() : "";
      if (shortDef.length > 48) shortDef = shortDef.slice(0, 48) + "…";

      var html = "<span style='font-size:20px;font-weight:700'>" + d.char + "</span>";
      if (pinyin) html += " &nbsp;<span style='color:#93c5fd'>" + pinyin + "</span>";
      html += "<br><span style='opacity:0.7;font-size:11px'>Rank #" + d.rank +
              " &middot; top " + d.rank + " covers " + pct + "%</span>";
      if (shortDef) html += "<br>" + shortDef;

      tip.style("display", "block").html(html);
      moveTip(ev);
    }
    function moveTip(ev) {
      tip.style("left", (ev.clientX + 14) + "px").style("top", (ev.clientY - 36) + "px");
    }
    function hideTip() { tip.style("display", "none"); }

    // ── Labels ────────────────────────────────────────────────────────────────
    function updateLabels(xs, isZoom, k) {
      k = k || 1;
      var minPx  = Math.max(6, 16 / Math.pow(k, 0.8));
      var picked = [];
      var lastPx = -Infinity;

      data.forEach(function (d) {
        var px = xs(d.rank);
        if (px < -40 || px > iW + 40) return;
        if (px - lastPx >= minPx && charSz(d.rank, k) > 0.5) {
          picked.push(d);
          lastPx = px;
        }
      });

      var sel     = gClip.selectAll(".fc").data(picked, function (d) { return d.rank; });
      var entered = sel.enter().append("text").attr("class", "fc")
        .attr("text-anchor", "middle")
        .attr("fill", "#1a1a1a")
        .attr("opacity", 0)
        .style("cursor", onCharClick ? "pointer" : "default")
        .text(function (d) { return d.char; })
        .on("mouseover", showTip)
        .on("mousemove", moveTip)
        .on("mouseout",  hideTip);

      if (onCharClick) {
        entered.on("click", function (ev, d) { onCharClick(d.char); });
      }

      sel.exit().remove();

      var all = entered.merge(sel);

      if (isZoom) {
        all.attr("x",         function (d) { return xs(d.rank); })
           .attr("y",         function (d) { return yScale(d.cumulative_pct) - 6; })
           .attr("font-size", function (d) { return charSz(d.rank, k) + "px"; })
           .attr("opacity",   function (d) { return charAl(d.rank, k); });
      } else {
        // Initial: position immediately, stagger opacity in sync with curve
        all.attr("x",         function (d) { return xs(d.rank); })
           .attr("y",         function (d) { return yScale(d.cumulative_pct) - 6; })
           .attr("font-size", function (d) { return charSz(d.rank, k) + "px"; });

        entered.each(function (d) {
          var delay = Math.max(0, (xs(d.rank) / iW) * animMs - 60);
          d3.select(this).transition().delay(delay).duration(220)
            .attr("opacity", charAl(d.rank, k));
        });
      }
    }

    updateLabels(xBase, false, 1);

    // ── Zoom — applied to svg so child text elements keep their own events ────
    var zoom = d3.zoom()
      .scaleExtent([1, 150])
      .translateExtent([[0, -10], [iW, iH + 10]])
      .filter(function (ev) { return !ev.button; })
      .on("zoom", function (ev) {
        hint.style("display", "none");
        hideTip();
        // Cancel the initial draw animation so dasharray doesn't interfere
        curvePath.interrupt()
          .attr("stroke-dasharray", null)
          .attr("stroke-dashoffset", null);
        var k  = ev.transform.k;
        var xZ = ev.transform.rescaleX(xBase);
        curvePath.attr("d", makeLine(xZ));
        applyXAxis(xZ);
        updateLabels(xZ, true, k);
      });

    // Apply zoom to the SVG itself — no overlay rect, so text mouseover works
    svg.call(zoom);
  };
})();
