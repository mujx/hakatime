// Third party JS
import "bootstrap";

// Third party CSS
import "typeface-nunito";
import "@fortawesome/fontawesome-free/css/all.min.css";
import "bootstrap/dist/css/bootstrap.min.css";
import "startbootstrap-sb-admin-2/css/sb-admin-2.min.css";

import { main } from "../src/index.js";
import config from "../src/config.js";

function loadEnv() {
  if (process.env.ENABLE_ANIMATION) {
    config.animations.enabled = process.env.ENABLE_ANIMATION === "true";
  }

  if (process.env.HAKA_VERSION) {
    config.currentVersion = process.env.HAKA_VERSION;
  }

  main();
}

// HMR setup. For more info see: https://parceljs.org/hmr.html
if (module.hot) {
  module.hot.accept(function () {
    console.log("Reloaded, running main again");
    loadEnv();
  });
}

loadEnv();
