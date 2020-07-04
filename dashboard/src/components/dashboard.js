import m from "mithril";
import $ from "jquery";

import * as auth from "../auth";
import utils from "../utils";

const MODAL_ID = "api-token-modal";
const MODAL_TOKEN_LIST_ID = "api-token-list-modal";

const ApiToken = {
  value: null,
  copyToCliboard: function(e) {
    e.redraw = false;
    utils.copyToCliboard(ApiToken.value);
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

const ApiTokenList = {
  renderModal: function(tokens = []) {
    let modal = document.getElementById(MODAL_TOKEN_LIST_ID);

    if (!modal) {
      modal = document.createElement("div");
      modal.id = MODAL_TOKEN_LIST_ID;
      document.body.appendChild(modal);
    }

    m.render(modal, m(TokenListModal(tokens)));
  },
  openModal: function(e) {
    e.redraw = false;

    m.request({
      method: "GET",
      url: "/auth/tokens",
      background: true,
      headers: {
        authorization: auth.getHeaderToken()
      }
    })
      .then(ApiTokenList.renderModal)
      .catch(function(e) {
        // TODO: Notify the user about the error.
        if (e && e.response) {
          console.log(e.response);
          return;
        }

        console.log(e.response);
      });
  },
  closeModal: function(e) {
    e.redraw = false;
    m.render(document.getElementById(MODAL_TOKEN_LIST_ID), null);
  },
  deleteToken: function(t) {
    m.request({
      method: "DELETE",
      url: "/auth/token/" + t.tknId,
      background: true,
      headers: {
        authorization: auth.getHeaderToken()
      }
    })
      .then(function() {
        return m.request({
          method: "GET",
          url: "/auth/tokens",
          background: true,
          headers: {
            authorization: auth.getHeaderToken()
          }
        });
      })
      .then(function(tokens) {
        $('[data-toggle="tooltip"]').tooltip("dispose");

        let modal = document.getElementById(MODAL_TOKEN_LIST_ID);

        if (!modal) {
          modal = document.createElement("div");
          modal.id = MODAL_TOKEN_LIST_ID;
          document.body.appendChild(modal);
        }
        m.render(modal, m(TokenListModal(tokens)));
      })
      .catch(function(e) {
        $('[data-toggle="tooltip"]').tooltip("hide");

        if (e && e.response) {
          console.log(e.response);
          return;
        }

        console.log(e);
      });
  },
  copyTokenToClipbard: function(t) {
    $('[data-toggle="tooltip"]').tooltip("hide");
    utils.copyToCliboard(atob(t.tknId));
  }
};

function TokenListModal(tokens = []) {
  return {
    oncreate: () => {
      $('[data-toggle="tooltip"]').tooltip();
    },
    view: () => {
      return m(
        "div.modal.fade.show",
        {
          tabindex: "-1",
          role: "dialog",
          style: "display:block; background-color:rgba(0, 0, 0, 0.3);"
        },
        m(
          "div.modal-dialog.modal-lg.modal-dialog-centered[role=document]",
          m("div.modal-content", [
            m(
              "div.modal-header",
              m("h5.modal-title", "Active API tokens"),
              m(
                "button.close[aria-label=Close]",
                {
                  style: "outline: 0;",
                  onclick: ApiTokenList.closeModal
                },
                m("span[aria-hidden=true]", "×")
              )
            ),
            m(
              "div.modal-body",
              {
                style: "height: 350px; overflow-y: auto;"
              },
              [
                m("table.table.table-borderless", [
                  m(
                    "thead",
                    m("tr", [
                      m("th", { scope: "col" }, "ID"),
                      m("th", { scope: "col" }, "Last usage"),
                      m("th", { scope: "col" }, "")
                    ])
                  ),
                  m(
                    "tbody",
                    tokens.map(t => {
                      return m("tr", [
                        m("td", { scope: "row" }, t.tknId.substring(0, 6)),
                        m(
                          "td",
                          t.lastUsage
                            ? utils.formatDate(t.lastUsage)
                            : "Not used"
                        ),
                        m("td", [
                          m(
                            "button.btn.btn-sm.btn-success.mr-2[data-toggle=tooltip][title='Copy to clipboard']",
                            {
                              type: "button",
                              onclick: function(e) {
                                e.redraw = false;
                                ApiTokenList.copyTokenToClipbard(t);
                              }
                            },
                            m("i.fas.fa-clipboard")
                          ),
                          m(
                            "button.btn.btn-sm.btn-danger[data-toggle=tooltip][title='Delete token']",
                            {
                              type: "button",
                              tabindex: "0",
                              onclick: function(e) {
                                e.redraw = false;
                                ApiTokenList.deleteToken(t);
                              }
                            },
                            m("i.fas.fa-trash")
                          )
                        ])
                      ]);
                    })
                  )
                ])
              ]
            ),
            m("div.modal-footer", [
              m(
                "button.btn.btn-secondary[type=button]",
                { onclick: ApiTokenList.closeModal },
                "Close"
              )
            ])
          ])
        )
      );
    }
  };
}

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

const Modal = {
  oncreate: () => {
    $('[data-toggle="tooltip"]').tooltip();
  },
  view: () => {
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

function clickLogout(e) {
  e.redraw = false;
  auth.logout();
}

export default {
  onint: () => {
    document.title = "Hakatime | Dashboard";
  },
  oncreate: () => {
    $('[data-toggle="tooltip"]').tooltip();
  },
  view: vnode => {
    const sideMenuItems = [
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
      const activeClass = m.route.get() === c.link ? "active" : "";

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
                onclick: clickLogout
              },
              [m("i.fas.fa-fw.fa-sign-out-alt"), m("span", "Logout")]
            )
          ),
          m("hr.sidebar-divider.d-none.d-md-block"),
          m(
            "div.text-center.d-none.d-md-inline",
            m("button.rounded-circle.border-0", {
              id: "sidebarToggle",
              onclick: function(e) {
                e.redraw = false;

                $("body").toggleClass("sidebar-toggled");
                $(".sidebar").toggleClass("toggled");

                if ($(".sidebar").hasClass("toggled")) {
                  $(".sidebar .collapse").collapse("hide");
                }
              }
            })
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
                      m("li.nav-item.align-self-center", [
                        m(
                          "button.btn.btn-sm.btn-circle.btn-info[data-toggle='tooltip'][data-placement='left'][title='Create a new API token']",
                          {
                            onclick: createApiTokenDialog,
                            role: "button"
                          },
                          m("i.fas.fa-key.fa-fw.text-white-100")
                        )
                      ]),
                      m("div.topbar-divider.d-none.d-sm-block"),
                      m("li.nav-item.dropdown.no-arrow", [
                        m(
                          "a.nav-link.dropdown-toggle[data-toggle=dropdown][aria-haspopup=true][aria-expanded=false]",
                          { id: "userDropdown", role: "button" },
                          m(
                            "span.mr-3.d-none.d-lg-inline.text-gray-600.small",
                            auth.getUsername()
                          ),
                          m(
                            "div.btn.btn-secondary.btn-circle.btn-md",
                            auth
                              .getUsername()
                              .charAt(0)
                              .toUpperCase()
                          )
                        ),
                        m(
                          "div.dropdown-menu.dropdown-menu-right.shadow.animated--grow-in[aria-labelledby=userDropdown]",
                          [
                            m(
                              "a.dropdown-item",
                              {
                                onclick: ApiTokenList.openModal
                              },
                              "Tokens"
                            ),
                            m(
                              "a.dropdown-item",
                              {
                                onclick: clickLogout
                              },
                              "Logout"
                            )
                          ]
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
