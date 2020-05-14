import m from "mithril";

let inMemToken = null;

export function login({ token, tokenExpiry }) {
  inMemToken = {
    token,
    tokenExpiry
  };
}

export function tryToRefresh(errMsg, callback) {
  m.request({
    method: "POST",
    url: "/auth/refresh_token"
  })
    .then(function({ token, tokenExpiry }) {
      login({
        token,
        tokenExpiry
      });

      if (callback) callback();
    })
    .catch(function(e) {
      clearTokens();

      if (errMsg) {
        m.route.set("/login", { msg: errMsg });
      } else {
        console.log("refresh_token failed", e);
        m.route.set("/login");
      }
    });
}

export function clearTokens() {
  inMemToken = null;

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

      m.route.set("/login", { msg: "You've been logged out" });
    })
    .catch(function(e) {
      m.route.set("/login", { msg: `Logout failed: ${e.response.error}` });
    });
}

export function checkInterval() {
  if (inMemToken) {
    let now = new Date();
    let then = new Date(inMemToken.tokenExpiry - 5 * 60000);

    if (now > then) {
      tryToRefresh();
    }
  }
}

export function getToken() {
  return inMemToken;
}

export function getHeaderToken() {
  return `Basic ${inMemToken.token}`;
}

export function isLoggedIn() {
  return inMemToken != null;
}

export function retryCall(err, callback) {
  if (err.code == 403) {
    tryToRefresh("The session expired", callback);
    return;
  }

  console.log(err);
  console.log("call failed with", err.response, err.code);
}
