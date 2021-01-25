import m from "mithril";

function refreshToken() {
  return m.request({
    method: "POST",
    url: "/auth/refresh_token"
  });
}

function login(creds) {
  return m.request({
    method: "POST",
    url: "/auth/login",
    body: creds
  });
}

function register(username, password) {
  return m.request({
    method: "POST",
    url: "/auth/register",
    body: {
      username,
      password
    }
  });
}

function logout(authToken) {
  return m.request({
    method: "POST",
    url: "/auth/logout",
    headers: {
      authorization: authToken
    }
  });
}

function getTimeline(params, authToken) {
  return m.request({
    url: "/api/v1/users/current/timeline",
    responseType: "json",
    background: true,
    params: params,
    headers: {
      authorization: authToken
    }
  });
}

function getStats(params, authToken) {
  return m.request({
    url: "/api/v1/users/current/stats",
    responseType: "json",
    params: params,
    headers: {
      authorization: authToken
    }
  });
}

function getProject(projectName, params, authToken) {
  return m.request({
    url: `/api/v1/users/current/projects/${projectName}`,
    responseType: "json",
    headers: {
      authorization: authToken
    },
    params: params
  });
}

function createApiToken(authToken) {
  return m.request({
    method: "POST",
    url: "/auth/create_api_token",
    headers: {
      authorization: authToken
    },
    background: true
  });
}

function getTokens(authToken) {
  return m.request({
    method: "GET",
    url: "/auth/tokens",
    background: true,
    headers: {
      authorization: authToken
    }
  });
}

function deleteToken(tokenId, authToken) {
  return m.request({
    method: "DELETE",
    url: "/auth/token/" + tokenId,
    background: true,
    headers: {
      authorization: authToken
    }
  });
}

function getBadgeLink(projectName, authToken) {
  return m.request({
    method: "GET",
    url: `/badge/link/${projectName}`,
    background: true,
    headers: {
      authorization: authToken
    }
  });
}

export {
  createApiToken,
  deleteToken,
  getBadgeLink,
  getProject,
  getStats,
  getTimeline,
  getTokens,
  login,
  logout,
  refreshToken,
  register
};
