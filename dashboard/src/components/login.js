import m from "mithril";
import { login, isLoggedIn, tryToRefresh } from "../auth.js";

var AuthUser = {
  username: null,
  password: null
};

var ErrMsg = {
  error: null
};

export default {
  oncreate: vnode => {
    document.title = "Hakatime | Login";

    if (isLoggedIn()) {
      m.route.set("/app");
      return;
    }
  },
  view: vnode => {
    if (vnode.attrs.msg) {
      ErrMsg.error = vnode.attrs.msg;
    }

    return m(
      "form.form-signin",
      {
        onsubmit: function(e) {
          e.preventDefault();

          m.request({
            method: "POST",
            url: "/auth/login",
            body: AuthUser
          })
            .then(function(creds) {
              login(creds);

              ErrMsg.error = null;

              m.route.set("/app");
            })
            .catch(function(e) {
              if (e.response.error) {
                ErrMsg.error = `Login failed: ${e.response.error}`;
                return;
              }

              ErrMsg.error = `Login failed: ${e.code}`;
            });
        }
      },
      [
        m("div.text-center.mb-4", m("h1.h3.mb-3.font-weight-normal", "Login")),
        m("div.form-label-group", [
          m("input.form-control[type=text][placeholder=Username]", {
            id: "inputUsername",
            required: "required",
            oninput: function(e) {
              AuthUser.username = e.target.value;
            }
          }),
          m("label", { for: "inputUsername" }, "Username")
        ]),
        m("div.form-label-group", [
          m("input.form-control[type=password][placeholder=Password]", {
            id: "inputPassword",
            required: "required",
            oninput: function(e) {
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
        m("p.mt-5.mb-3.text-center", "Hakatime")
      ]
    );
  }
};
