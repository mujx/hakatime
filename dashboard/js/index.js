// Third party JS
import "bootstrap";

// Third party CSS
import "typeface-nunito";
import "@fortawesome/fontawesome-free/css/all.min.css";
import "bootstrap/dist/css/bootstrap.min.css";
import "startbootstrap-sb-admin-2/css/sb-admin-2.min.css";

import { main } from "../src/index.js";

function loadEnv() {
  /*
    Here we could add variables such as

    var baseUrl = process.env.BASE_URL;

    Parcel will replace `process.env.BASE_URL`
    with the string contents of the BASE_URL environment
    variable at bundle/build time.
    A .env file can also be used to override shell variables
    for more information, see https://en.parceljs.org/env.html
  */

  main();
}

// HMR setup. For more info see: https://parceljs.org/hmr.html
if (module.hot) {
  module.hot.accept(function() {
    console.log("Reloaded, running main again");
    loadEnv();
  });
}

loadEnv();
