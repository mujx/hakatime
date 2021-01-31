import m from "mithril";
import $ from "jquery";

import * as api from "../api";
import * as auth from "../auth";
import * as storage from "../storage";

// Modals
import CreateTokenModal from "../modals/CreateToken";
import TokenListModal from "../modals/TokenList";

function createApiTokenDialog(event) {
  event.redraw = false;
  api
    .createApiToken()
    .then(function (res) {
      CreateTokenModal.openModal(res.apiToken);
    })
    .catch(err => auth.retryCall(err, () => createApiTokenDialog(event)));
}

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
              onclick: function (e) {
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
                            storage.getUsername()
                          ),
                          m(
                            "div.btn.btn-secondary.btn-circle.btn-md",
                            storage.getUsername().charAt(0).toUpperCase()
                          )
                        ),
                        m(
                          "div.dropdown-menu.dropdown-menu-right.shadow.animated--grow-in[aria-labelledby=userDropdown]",
                          [
                            m(
                              "a.dropdown-item",
                              {
                                onclick: TokenListModal.openModal
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
