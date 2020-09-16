import m from "mithril";
import Commons from "./common.js";
import { login, isLoggedIn } from "../auth.js";

const User = {
  username: "",
  password: "",
  confirmPassword: ""
};

const ErrMsg = {
  error: ""
};

export default {
  oncreate: () => {
    document.title = "Hakatime | Register";

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
        onsubmit: function (e) {
          e.preventDefault();

          if (User.password.length < 8) {
            ErrMsg.error = "The password is too short (minimum 8 characters)";
            return;
          }

          if (User.password !== User.confirmPassword) {
            ErrMsg.error = "The passwords do not match";
            return;
          }

          m.request({
            method: "POST",
            url: "/auth/register",
            body: {
              username: User.username,
              password: User.password
            }
          })
            .then(function (creds) {
              login(creds);

              ErrMsg.error = "";

              m.route.set("/app");
            })
            .catch(function (e) {
              if (e.response && e.response.error) {
                ErrMsg.error = `Registration failed: ${e.response.error}`;
                return;
              }

              ErrMsg.error = "Registration failed: Unknown error";
            });
        }
      },
      [
        m(
          "div.text-center.mb-4",
          m("h1.h3.mb-3.font-weight-normal", "Register")
        ),
        m("div.form-label-group", [
          m("input.form-control[type=text][placeholder=Username]", {
            id: "inputUsername",
            required: "required",
            oninput: function (e) {
              User.username = e.target.value;
            }
          }),
          m("label", { for: "inputUsername" }, "Username")
        ]),
        m("div.form-label-group", [
          m("input.form-control[type=password][placeholder=Password]", {
            id: "inputPassword",
            required: "required",
            oninput: function (e) {
              User.password = e.target.value;
            }
          }),
          m("label", { for: "inputPassword" }, "Password")
        ]),
        m("div.form-label-group", [
          m("input.form-control[type=password][placeholder=Confirm password]", {
            id: "confirmPassword",
            required: "required",
            oninput: function (e) {
              User.confirmPassword = e.target.value;
            }
          }),
          m("label", { for: "confirmPassword" }, "Confirm password")
        ]),
        m(
          "button.btn.btn-lg.btn-primary.btn-block.btn-signin[type=submit]",
          "Register"
        ),
        m("p.mt-3.mb-2.text-center.text-danger", ErrMsg.error),
        m("div.mt-4.text-center.text-muted", [
          "Already have an account?",
          m("a.ml-2", { href: "#!/login" }, "Login here")
        ]),
        Commons.githubLink()
      ]
    );
  }
};
