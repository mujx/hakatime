import m from "mithril";
import $ from "jquery";

import * as auth from "../auth";

const MODAL_ID = "api-token-modal";

let ApiToken = {
  value: null,
  copyToCliboard: function(e) {
    e.redraw = false;

    const elem = document.createElement("textarea");

    elem.value = ApiToken.value;
    elem.setAttribute("readonly", "");
    elem.style.position = "absolute";
    elem.style.left = "-99999px";

    document.body.appendChild(elem);
    elem.select();

    document.execCommand("copy");
    document.body.removeChild(elem);
  },
  openModal: function() {
    let modal = document.getElementById(MODAL_ID);
    if (!modal) {
      modal = document.createElement("div");
      modal.id = MODAL_ID;
      document.body.appendChild(modal);
    }
    m.mount(modal, Modal);
  },
  closeModal: function(event) {
    event.redraw = false;
    ApiToken.value = null;
    m.mount(document.getElementById(MODAL_ID), null);
  }
};

function createApiTokenDialog(event) {
  event.redraw = false;
  m.request({
    method: "POST",
    url: "/auth/create_api_token",
    headers: {
      authorization: auth.getHeaderToken()
    },
    background: true
  })
    .then(function(res) {
      ApiToken.value = res.apiToken;
      ApiToken.openModal();
    })
    .catch(err => auth.retryCall(err, () => createApiTokenDialog(event)));
}

let Modal = {
  oncreate: vnode => {
    console.log("oncreate");
    $('[data-toggle="tooltip"]').tooltip();
  },
  view: vnode => {
    return m(
      "div.modal.fade.show",
      {
        tabindex: "-1",
        role: "dialog",
        style: "display:block; background-color:rgba(0, 0, 0, 0.3);"
      },
      m(
        "div.modal-dialog.modal-dialog-centered[role=document]",
        m("div.modal-content", [
          m(
            "div.modal-header",
            m("h5.modal-title", "Your new API Token"),
            m(
              "button.close[aria-label=Close]",
              {
                onclick: ApiToken.closeModal
              },
              m("span[aria-hidden=true]", "×")
            )
          ),
          m("div.modal-body", [
            m(
              "p.text-justify",
              "Use this token to setup your Wakatime client."
            ),
            m(
              "p.text-justify.mb-4",
              "Don't forget to update the default upstream URL with your Hakatime instance."
            ),
            m("row", [
              m("code.mr-2", `${ApiToken.value}`),
              m(
                "button.btn.shadow-sm.btn-secondary[data-toggle='tooltip'][data-placement='top'][title='Copy to clipboard']",
                {
                  onclick: ApiToken.copyToCliboard
                },
                m("i.fas.fa-copy")
              )
            ])
          ]),
          m("div.modal-footer", [
            m(
              "button.btn.btn-secondary[type=button]",
              { onclick: ApiToken.closeModal },
              "Close"
            )
          ])
        ])
      )
    );
  }
};

export default {
  onint: vnode => {
    document.title = "Hakatime | Dashboard";
  },
  oncreate: vnode => {
    $('[data-toggle="tooltip"]').tooltip();
  },
  view: vnode => {
    let sideMenuItems = [
      {
        name: "Overview",
        icon: "tachometer-alt",
        link: "/app"
      },
      {
        name: "Projects",
        icon: "book",
        link: "/app/projects"
      }
    ].map(c => {
      let activeClass = m.route.get() === c.link ? "active" : "";

      return m(
        `li.nav-item.${activeClass}`,
        m(
          "a.nav-link",
          {
            href: `#!${c.link}`
          },
          [m(`i.fas.fa-fw.fa-${c.icon}`), m("span", c.name)]
        )
      );
    });

    return [
      // Sidebar
      m("div.d-flex", { id: "wrapper" }, [
        m("ul.navbar-nav.bg-gradient-primary.sidebar.sidebar-dark", [
          m(
            "a.sidebar-brand.d-flex.align-items-center.justify-content-center[href=#]",
            [
              m("div.sidebar-brand-icon", m("i.fas.fa-code")),
              m("div.sidebar-brand-text.mx-3", "Hakatime")
            ]
          ),
          m("hr.sidebar-divider.my-0"),
          sideMenuItems,

          m(
            "li.nav-item",
            m(
              "a.nav-link",
              {
                href: "#!/",
                onclick: e => {
                  e.redraw = false;
                  auth.logout();
                }
              },
              [m("i.fas.fa-fw.fa-sign-out-alt"), m("span", "Logout")]
            )
          )
        ]),

        // Main content
        m("div.d-flex.flex-column", { id: "content-wrapper" }, [
          m("div#content", [
            m(
              "nav.navbar.navbar-expand.navbar-light.topbar.static-top.shadow.mb-4",
              [
                m(
                  "div.collapse.navbar-collapse",
                  { id: "navbarSupportedContent" },
                  [
                    m("ul.navbar-nav.ml-auto.mt-2.mt-lg-0", [
                      m("li.nav-item", [
                        m(
                          "button.btn.btn-sm.btn-success.shadow-sm[data-toggle='tooltip'][data-placement='left'][title='Create a new API token']",
                          {
                            onclick: createApiTokenDialog,
                            role: "button"
                          },
                          m("i.fas.fa-plus.fa-fw.text-white-100")
                        )
                      ])
                    ])
                  ]
                )
              ]
            ),
            m("div.container-fluid", vnode.children)
          ])
        ])
      ])
    ];
  }
};
