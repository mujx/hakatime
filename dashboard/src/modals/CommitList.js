import m from "mithril";
import $ from "jquery";

import * as api from "../api";
import utils from "../utils";

const MODAL_ID = "commit-log";

const state = {
  projectName: null,
  repoOwner: null,
  repoName: null,
  githubUser: null,
  commits: [],
  pendingRequest: false
};

function getCommitLog(e) {
  e.redraw = false;
  e.preventDefault();

  state.pendingRequest = true;
  renderModal();

  api
    .getCommitLog(state.projectName, {
      repoName: state.repoName,
      repoOwner: state.repoOwner,
      user: state.githubUser,
      limit: 80
    })
    .then(function (res) {
      state.commits = res.commits;

      state.pendingRequest = false;
      renderModal();
    })
    .catch(function (err) {
      state.pendingRequest = false;
      renderModal();

      utils.showError("Failed to fetch the commit log");
      console.log(err.response);
    });
}

function closeModal(e) {
  if (e) e.redraw = false;
  m.render(document.getElementById(MODAL_ID), null);
}

function renderModal() {
  let modal = document.getElementById(MODAL_ID);

  if (!modal) {
    modal = document.createElement("div");
    modal.id = MODAL_ID;
    document.body.appendChild(modal);
  }

  m.render(modal, m(Modal));
}

function openModal({ projectName }) {
  state.projectName = projectName;
  state.repoName = projectName;

  renderModal();
}

function mkCommitSection() {
  if (state.commits.length > 0) {
    return state.commits.map(function (x) {
      return m(
        "a.list-group-item.list-group-item-action.flex-column.align-items-start",
        { href: x.html_url, target: "_blank" },
        [
          m("div.d-flex.w-100.justify-content-between", [
            m("h6.text-truncate", x.commit.message),
            m(
              "small",
              m(
                "span.badge.badge-pill.badge-info",
                utils.secondsToHms(x.total_seconds)
              )
            )
          ]),
          m(
            "small.text-muted",
            x.author.login +
              " - " +
              new Date(x.commit.author.date).toLocaleString()
          )
        ]
      );
    });
  }

  if (state.pendingRequest) {
    return m("div.spinner", [
      m("div.bounce1"),
      m("div.bounce2"),
      m("div.bounce3")
    ]);
  }

  return m("span.text-center", "Nothing fetched yet...");
}

const Modal = {
  oncreate: () => {
    $('[data-toggle="tooltip"]').tooltip();
  },
  onbeforeremove: function (vnode) {
    vnode.dom.classList.add("fade-out");
    return new Promise(function (resolve) {
      vnode.dom.addEventListener("animationend", resolve);
    });
  },
  view: () => {
    return m(
      "div.modal.fade-in.show",
      {
        style: "display:block; background-color:rgba(0, 0, 0, 0.3);"
      },
      m(
        "div.modal-dialog.modal-lg.modal-dialog-centered[role=document]",
        m("div.modal-content", [
          m(
            "div.modal-header",
            m("h5.modal-title", "Time spent per commit"),
            m(
              "button.close[aria-label=Close]",
              {
                onclick: closeModal
              },
              m("span[aria-hidden=true]", "×")
            )
          ),
          m(
            "div.modal-body",
            m(
              "div.container-fluid",
              m(
                "div.row",
                m(
                  "form.col-lg-12",
                  {
                    onsubmit: getCommitLog
                  },
                  [
                    m("div.row", [
                      m("div.col-md-4", [
                        m("label", { for: "repoOwnerInput" }, "Repo owner"),
                        m("input.form-control", {
                          id: "repoOwnerInput",
                          type: "text",
                          required: "required",
                          placeholder: state.apiToken,
                          oninput: function (e) {
                            state.repoOwner = e.target.value;
                          }
                        })
                      ]),
                      m("div.col-md-4", [
                        m("label", { for: "repoNameInput" }, "Repo name"),
                        m("input.form-control", {
                          id: "repoNameInput",
                          type: "text",
                          required: "required",
                          value: state.projectName,
                          placeholder: state.projectName,
                          oninput: function (e) {
                            state.repoName = e.target.value;
                          }
                        })
                      ]),
                      m("div.col-md-4", [
                        m(
                          "label",
                          { for: "githubUserInput" },
                          "Your GitHub username"
                        ),
                        m("input.form-control", {
                          id: "githubUserInput",
                          type: "text",
                          required: "required",
                          placeholder: state.apiToken,
                          oninput: function (e) {
                            state.githubUser = e.target.value;
                          }
                        })
                      ])
                    ]),
                    m("hr"),
                    m("div.row", [
                      m("div.col-lg-12", [
                        m("h5.strong.mb-4", "Commits"),

                        m(
                          "div.list-group.list-group-flush",
                          {
                            style:
                              "max-height: 300px; overflow: scroll; margin-bottom: 10px;"
                          },
                          mkCommitSection()
                        )
                      ])
                    ]),
                    m("div.form-group.d-flex.flex-row-reverse", [
                      m(
                        "button.btn.btn-secondary.m-1[type=button]",
                        { onclick: closeModal },
                        "Cancel"
                      ),
                      m("button.btn.btn-primary.m-1[type=submit]", "Fetch")
                    ])
                  ]
                )
              )
            )
          )
        ])
      )
    );
  }
};

export default {
  Modal,
  openModal
};
