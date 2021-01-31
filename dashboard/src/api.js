import m from "mithril";
import * as storage from "./storage";

function baseUrl() {
  return location.href.replace(location.hash, "").replace(/\/$/, "");
}

function refreshToken() {
  return m.request({
    method: "POST",
    url: baseUrl() + "/auth/refresh_token"
  });
}

function login(creds) {
  return m.request({
    method: "POST",
    url: baseUrl() + "/auth/login",
    body: creds
  });
}

function register(username, password) {
  return m.request({
    method: "POST",
    url: baseUrl() + "/auth/register",
    body: {
      username,
      password
    }
  });
}

function logout() {
  return m.request({
    method: "POST",
    url: baseUrl() + "/auth/logout",
    headers: {
      authorization: storage.getHeaderToken()
    }
  });
}

function getTimeline(params) {
  return m.request({
    url: baseUrl() + "/api/v1/users/current/timeline",
    responseType: "json",
    background: true,
    params: params,
    headers: {
      authorization: storage.getHeaderToken()
    }
  });
}

function getStats(params) {
  return m.request({
    url: baseUrl() + "/api/v1/users/current/stats",
    responseType: "json",
    params: params,
    headers: {
      authorization: storage.getHeaderToken()
    }
  });
}

function getProject(projectName, params) {
  return m.request({
    url: baseUrl() + `/api/v1/users/current/projects/${projectName}`,
    responseType: "json",
    headers: {
      authorization: storage.getHeaderToken()
    },
    params: params
  });
}

function createApiToken() {
  return m.request({
    method: "POST",
    url: baseUrl() + "/auth/create_api_token",
    headers: {
      authorization: storage.getHeaderToken()
    },
    background: true
  });
}

function getTokens() {
  return m.request({
    method: "GET",
    url: baseUrl() + "/auth/tokens",
    background: true,
    headers: {
      authorization: storage.getHeaderToken()
    }
  });
}

function deleteToken(tokenId) {
  return m.request({
    method: "DELETE",
    url: baseUrl() + "/auth/token/" + tokenId,
    background: true,
    headers: {
      authorization: storage.getHeaderToken()
    }
  });
}

function getBadgeLink(projectName) {
  return m.request({
    method: "GET",
    url: baseUrl() + `/badge/link/${projectName}`,
    background: true,
    headers: {
      authorization: storage.getHeaderToken()
    }
  });
}

function submitImportRequest({ remoteServer, apiToken, startDate, endDate }) {
  return m.request({
    method: "POST",
    url: baseUrl() + "/import",
    background: true,
    body: {
      remoteServer,
      apiToken,
      startDate: startDate.toISOString(),
      endDate: endDate.toISOString()
    },
    headers: {
      authorization: storage.getHeaderToken()
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
  register,
  submitImportRequest
};
