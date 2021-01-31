import m from "mithril";
import $ from "jquery";

import utils from "../utils";

let tokenValue = null;

const MODAL_ID = "api-token-modal";

function closeModal(e) {
  e.redraw = false;
  tokenValue = null;
  m.mount(document.getElementById(MODAL_ID), null);
}

function openModal(value) {
  tokenValue = value;

  let modal = document.getElementById(MODAL_ID);

  if (!modal) {
    modal = document.createElement("div");
    modal.id = MODAL_ID;
    document.body.appendChild(modal);
  }

  m.mount(modal, Modal);
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
        "div.modal-dialog.modal-dialog-centered[role=document]",
        m("div.modal-content", [
          m(
            "div.modal-header",
            m("h5.modal-title", "Your new API Token"),
            m(
              "button.close[aria-label=Close]",
              {
                onclick: closeModal
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
              m("code.mr-2", `${tokenValue}`),
              m(
                "button.btn.shadow-sm.btn-secondary[data-toggle='tooltip'][data-placement='top'][title='Copy to clipboard']",
                {
                  onclick: function (e) {
                    e.redraw = false;
                    utils.copyToCliboard(tokenValue);
                  }
                },
                m("i.fas.fa-copy")
              )
            ])
          ]),
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
