import m from "mithril";
import $ from "jquery";

import * as api from "../api";
import utils from "../utils";

let availableTokens = [];

const MODAL_ID = "api-token-list-modal";

function activateTooltips() {
  $('[data-toggle="tooltip"]').tooltip();
}

function hideTooltips() {
  $('[data-toggle="tooltip"]').tooltip("hide");
}

function disposeTooltips() {
  $('[data-toggle="tooltip"]').tooltip("dispose");
}

function closeModal(e) {
  e.redraw = false;
  m.render(document.getElementById(MODAL_ID), null);
}

function renderModal(tokens) {
  let modal = document.getElementById(MODAL_ID);

  if (!modal) {
    modal = document.createElement("div");
    modal.id = MODAL_ID;
    document.body.appendChild(modal);
  }

  availableTokens = tokens;

  m.render(modal, m(Modal));

  activateTooltips();
}

function openModal(e) {
  e.redraw = false;

  api
    .getTokens()
    .then(renderModal)
    .catch(function (e) {
      // TODO: Notify the user about the error.
      if (e && e.response) {
        console.log(e.response);
        return;
      }

      console.log(e.response);
    });
}

function deleteToken(t) {
  api
    .deleteToken(t.tknId)
    .then(function () {
      return api.getTokens();
    })
    .then(function (tokens) {
      disposeTooltips();
      renderModal(tokens);
    })
    .catch(function (e) {
      hideTooltips();

      if (e && e.response) {
        console.log(e.response);
        return;
      }

      console.log(e);
    });
}

function copyTokenToClipbard(t) {
  hideTooltips();
  utils.copyToCliboard(atob(t.tknId));
}

const Modal = {
  oncreate: () => {
    activateTooltips();
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
            m("h5.modal-title", "Active API tokens"),
            m(
              "button.close[aria-label=Close]",
              {
                style: "outline: 0;",
                onclick: closeModal
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
                  availableTokens.map(t => {
                    return m("tr", [
                      m("td", { scope: "row" }, t.tknId.substring(0, 6)),
                      m(
                        "td",
                        t.lastUsage ? utils.formatDate(t.lastUsage) : "Not used"
                      ),
                      m("td", [
                        m(
                          "button.btn.btn-sm.btn-success.mr-2[data-toggle=tooltip][title='Copy to clipboard']",
                          {
                            type: "button",
                            onclick: function (e) {
                              e.redraw = false;
                              copyTokenToClipbard(t);
                            }
                          },
                          m("i.fas.fa-clipboard")
                        ),
                        m(
                          "button.btn.btn-sm.btn-danger[data-toggle=tooltip][title='Delete token']",
                          {
                            type: "button",
                            tabindex: "0",
                            onclick: function (e) {
                              e.redraw = false;
                              deleteToken(t);
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
              { onclick: closeModal },
              "Close"
            )
          ])
        ])
      )
    );
  }
};

export default {
  Modal,
  openModal
};
