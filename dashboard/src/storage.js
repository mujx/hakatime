let inMemToken = {
  token: "",
  tokenExpiry: "",
  username: ""
};

export function clearToken() {
  inMemToken = {
    token: "",
    tokenExpiry: "",
    username: ""
  };
}

export function updateToken(r) {
  inMemToken = {
    token: r.token,
    tokenExpiry: r.tokenExpiry,
    username: r.tokenUsername
  };
}

export function getTokenExpiry() {
  return inMemToken.tokenExpiry;
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
