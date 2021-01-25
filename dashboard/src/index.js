import m from "mithril";

import * as auth from "./auth.js";
import * as storage from "./storage";

import Dashboard from "./components/dashboard.js";
import Overview from "./components/overview.js";
import Projects from "./components/projects.js";
import Login from "./components/login.js";
import Register from "./components/register.js";

window.addEventListener("storage", function (event) {
  if (event.key === "logout") {
    auth.clearTokens();
    m.route.set("/login", { msg: "You've been logged out" });
  }
});

export function main() {
  const root = document.body;

  m.route(root, "/", {
    "/": {
      onmatch: function () {
        document.title = "Hakatime";

        if (storage.isLoggedIn()) {
          m.route.set("/app");
        } else {
          auth.tryToRefresh(null, () => {
            m.route.set("/app");
          });
        }
      }
    },
    "/login": Login,
    "/register": Register,
    "/app": {
      render: function () {
        return m(Dashboard, m(Overview));
      }
    },
    "/app/projects": {
      render: function () {
        return m(Dashboard, m(Projects));
      }
    }
  });
}
