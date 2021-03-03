import m from "mithril";
import * as api from "../api";
import * as storage from "../storage";
import utils from "../utils.js";

const AuthUser = {
  username: "",
  password: ""
};

const ErrMsg = {
  error: ""
};

export default {
  oncreate: () => {
    document.title = "Hakatime | Login";

    if (storage.isLoggedIn()) {
      m.route.set("/app");
      return;
    }
  },
  view: vnode => {
    if (vnode.attrs.msg && ErrMsg.error === "") {
      ErrMsg.error = vnode.attrs.msg;
    }

    return m(
      "div.d-flex.justify-content-center",
      m(
        "div.login-wrap",
        m(
          "form.form-signin",
          {
            onsubmit: function (e) {
              e.preventDefault();

              api
                .login(AuthUser)
                .then(function (creds) {
                  storage.updateToken(creds);

                  ErrMsg.error = "";

                  m.route.set("/app");
                })
                .catch(function (e) {
                  ErrMsg.error = `Login failed: ${utils.mkErrorMessage(e)}`;
                });
            }
          },
          [
            m(
              "div.text-center.mb-4",
              m("h1.h4.mb-3.font-weight-medium", "Login")
            ),
            m("div.form-label-group", [
              m("input.form-control[type=text][placeholder=Username]", {
                id: "inputUsername",
                required: "required",
                oninput: function (e) {
                  AuthUser.username = e.target.value;
                }
              }),
              m("label", { for: "inputUsername" }, "Username")
            ]),
            m("div.form-label-group", [
              m("input.form-control[type=password][placeholder=Password]", {
                id: "inputPassword",
                required: "required",
                oninput: function (e) {
                  AuthUser.password = e.target.value;
                }
              }),
              m("label", { for: "inputPassword" }, "Password")
            ]),
            m(
              "button.btn.btn-lg.btn-primary.btn-block.btn-signin[type=submit]",
              "Sign In"
            ),
            m("p.mt-3.mb-2.text-center.text-danger", ErrMsg.error),
            m("div.mt-4.text-center.text-muted", [
              "Don't have an account?",
              m("a.ml-2", { href: "#!/register" }, "Register here")
            ]),
            m("div.mt-3.d-flex.justify-content-center", [
              m(
                "a",
                { href: "https://github.com/mujx/hakatime", target: "_blank" },
                "Hakatime"
              )
            ])
          ]
        )
      )
    );
  }
};
