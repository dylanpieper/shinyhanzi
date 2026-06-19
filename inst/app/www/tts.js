/* TTS via SpeakIt-JS — zh-CN voice for hanzi. */

(function () {
  "use strict";

  if (!window.speechSynthesis || typeof Speakit === "undefined") {
    window.speakHanzi = function () {};
    return;
  }

  window.speakHanzi = function (text, rate) {
    Speakit.utteranceRate = (rate != null) ? rate : 0.9;
    if (Speakit.isSpeaking()) {
      Speakit.stopSpeaking();
    }
    Speakit.readText(text, "zh-CN");
  };
})();
