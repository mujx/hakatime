import m from "mithril";

import * as api from "./api";
import * as storage from "./storage";
import OverviewState from "./models/State.js";
import ProjectState from "./models/ProjectState.js";
import TimeRange from "./models/TimeRange.js";

export function tryToRefresh(errMsg, callback) {
  api
    .refreshToken()
    .then(function (r) {
      storage.updateToken(r);

      typeof callback === "function" && callback();
    })
    .catch(function (e) {
      clearTokens();

      if (errMsg) {
        m.route.set("/login", { msg: errMsg });
      } else {
        if (e) console.log("refresh_token failed:", e.code);
        m.route.set("/login");
      }
    });
}

export function clearTokens() {
  storage.clearToken();

  // to support logging out from all windows.
  window.localStorage.setItem("logout", Date.now());
}

export function logout() {
  // Force log-out will invalidate all other tokens.
  api
    .logout()
    .then(function () {
      clearTokens();
      clearData();

      m.route.set("/login", { msg: "You've been logged out" });
    })
    .catch(function (e) {
      m.route.set("/login", { msg: `Logout failed: ${e.response.error}` });
    });
}

function clearData() {
  OverviewState.clear();
  ProjectState.clear();
  TimeRange.reset();
}

export function checkInterval() {
  if (storage.isLoggedIn()) {
    const now = new Date();
    const then = new Date(storage.getTokenExpiry() - 5 * 60000);

    if (now > then) {
      tryToRefresh();
    }
  }
}

export function retryCall(err, callback) {
  if (err.code == 403) {
    tryToRefresh("The session expired", callback);
    return;
  }

  console.log(err);
  console.log("call failed with", err.response, err.code);
}
