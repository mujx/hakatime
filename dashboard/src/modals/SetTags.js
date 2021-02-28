import m from "mithril";
import $ from "jquery";
import Tagify from "@yaireo/tagify";
import "@yaireo/tagify/dist/tagify.css";

import * as api from "../api";

const MODAL_ID = "set-tags-modal";

const state = {
  projectName: "this project",
  initialTags: [],
  tagify: null
};

function getValidTags(values) {
  return values.map(x => x.value);
}

function saveTags(e) {
  e.redraw = false;
  e.preventDefault();

  const tags = getValidTags(state.tagify.value);

  console.log("Saving tags", tags);

  api
    .setTags({ project: state.projectName, tags: tags })
    .then(function () {
      closeModal();
    })
    .catch(function (e) {
      console.log(e);
      console.log(e.response);
      // TODO: Show this to the user
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

function openModal({ projectName, initialTags, e }) {
  if (e) e.redraw = false;
  state.projectName = projectName;
  state.initialTags = initialTags;

  renderModal();
}

const Modal = {
  oncreate: () => {
    $('[data-toggle="tooltip"]').tooltip();

    state.tagify = new Tagify(document.querySelector("input[name=basic]"));
    state.tagify.addTags(state.initialTags);
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
            m("h5.modal-title", `Set tags for ${state.projectName}`),
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
                    onsubmit: saveTags
                  },
                  [
                    m("div.form-group.col-lg-12", [
                      m("label", { for: "tagsInput" }, "Tags"),
                      m("input[name=basic]")
                    ]),
                    m("div.form-group.d-flex.flex-row-reverse", [
                      m(
                        "button.btn.btn-secondary.m-1[type=button]",
                        { onclick: closeModal },
                        "Cancel"
                      ),
                      m("button.btn.btn-primary.m-1[type=submit]", "Save")
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
