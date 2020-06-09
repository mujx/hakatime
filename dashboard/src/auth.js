import m from "mithril";

import OverviewState from "./models/State.js";
import ProjectState from "./models/ProjectState.js";
import TimeRange from "./models/TimeRange.js";

let inMemToken = {
  token: "",
  tokenExpiry: "",
  username: ""
};

function clearToken() {
  inMemToken = {
    token: "",
    tokenExpiry: "",
    username: ""
  };
}

export function login(r) {
  inMemToken = {
    token: r.token,
    tokenExpiry: r.tokenExpiry,
    username: r.tokenUsername
  };
}

export function tryToRefresh(errMsg, callback) {
  m.request({
    method: "POST",
    url: "/auth/refresh_token"
  })
    .then(function(r) {
      login(r);

      if (callback) callback();
    })
    .catch(function(e) {
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
  clearToken();

  // to support logging out from all windows.
  window.localStorage.setItem("logout", Date.now());
}

export function logout() {
  // Force log-out will invalidate all other tokens.
  m.request({
    method: "POST",
    url: "/auth/logout",
    headers: {
      authorization: getHeaderToken()
    }
  })
    .then(function() {
      clearTokens();
      clearData();

      m.route.set("/login", { msg: "You've been logged out" });
    })
    .catch(function(e) {
      m.route.set("/login", { msg: `Logout failed: ${e.response.error}` });
    });
}

function clearData() {
  OverviewState.clear();
  ProjectState.clear();
  TimeRange.reset();
}

export function checkInterval() {
  if (isLoggedIn()) {
    const now = new Date();
    const then = new Date(inMemToken.tokenExpiry - 5 * 60000);

    if (now > then) {
      tryToRefresh();
    }
  }
}

export function getUsername() {
  return inMemToken.username;
}

export function getHeaderToken() {
  return `Basic ${inMemToken.token}`;
}

export function isLoggedIn() {
  return inMemToken.token !== "" && inMemToken.tokenExpiry !== "";
}

export function retryCall(err, callback) {
  if (err.code == 403) {
    tryToRefresh("The session expired", callback);
    return;
  }

  console.log(err);
  console.log("call failed with", err.response, err.code);
}
