import m from "mithril";
import $ from "jquery";
import Litepicker from "litepicker";
import { Notyf } from "notyf";
import "notyf/notyf.min.css";

import utils from "../utils";
import * as api from "../api";

let state = {};
let picker = null;
let jobStatusTimer = null;

function notyf() {
  return new Notyf({
    duration: 5000,
    dismissible: true,
    position: {
      y: "top"
    }
  });
}

function checkStatus(res) {
  console.log(res);

  notyf().success(
    "Your request has been submitted. You'll be notified about its progress."
  );

  startJobStatusCheck(function () {
    api
      .checkJobStatus(state)
      .then(function (res) {
        console.log(res);

        if (res.jobStatus == "JobPending") {
          console.log("the submitted job is pending");
        } else if (res.jobStatus == "JobFailed") {
          notyf().error(
            "The import job failed. Please check the server logs for more info."
          );
          stopJobStatusCheck();
        } else if (res.jobStatus == "JobFinished") {
          notyf().success("The import job finished successfully");
          stopJobStatusCheck();
        }
      })
      .catch(function (e) {
        stopJobStatusCheck();
        notyf().error("An error occured: ", e.response.error);
        console.log(e.response);
      });
  });
}

function startJobStatusCheck(callback) {
  jobStatusTimer = setInterval(callback, 10000);
}

function stopJobStatusCheck() {
  if (jobStatusTimer) {
    clearInterval(jobStatusTimer);
    jobStatusTimer = null;
  }
}

function resetState() {
  state = {
    apiToken: null,
    startDate: utils.removeDays(new Date(), 10),
    endDate: new Date()
  };
}

const MODAL_ID = "import-data-modal";
const DATEPICKER_ID = "import-data-date-picker";

function removeTime(d) {
  return new Date(d).toLocaleString().split(",")[0];
}

function formatDateRange(s) {
  if (s.startDate && s.endDate) {
    return ` (${removeTime(s.startDate)} - ${removeTime(s.endDate)})`;
  }

  return "";
}

function startImport(e) {
  e.redraw = false;
  e.preventDefault();

  api
    .submitImportRequest(state)
    .then(checkStatus)
    .catch(function (e) {
      notyf().error("Failed to submit job: ", e.response.error);
      console.log(e.response);
    });
}

function closeModal(e) {
  e.redraw = false;
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

function openModal(e) {
  e.redraw = false;

  resetState();
  renderModal();
}

const Modal = {
  oncreate: () => {
    $('[data-toggle="tooltip"]').tooltip();

    picker = new Litepicker({
      element: document.getElementById(DATEPICKER_ID),
      minDays: 1,
      showTooltip: true,
      autoApply: false,
      allowRepick: true,
      maxDate: new Date(),
      numberOfMonths: 2,
      numberOfColumns: 2,
      singleMode: false,
      resetButton: true,
      mobileFriendly: true,
      onSelect: function (start, end) {
        state.startDate = start;
        state.endDate = end;
        renderModal();
      }
    });
  },
  onbeforeremove: function (vnode) {
    if (picker) picker.destroy();
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
            m("h5.modal-title", "Import heartbeats from Wakatime"),
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
                    onsubmit: startImport
                  },
                  [
                    m("div.form-group.col-lg-12", [
                      m("label", { for: "apiTokenInput" }, "API Token"),
                      m("input.form-control", {
                        id: "apiTokenInput",
                        type: "password",
                        required: "required",
                        placeholder: state.apiToken,
                        oninput: function (e) {
                          state.apiToken = e.target.value;
                        }
                      }),
                      m(
                        "small.form-text.text-muted",
                        "Used to authenticate with the server"
                      )
                    ]),
                    m("div.form-group.col-lg-12", [
                      m(
                        "button.btn.btn-outline-dark",
                        {
                          id: DATEPICKER_ID,
                          onclick: function (e) {
                            e.redraw = false;
                            e.preventDefault();
                            picker.show();
                          }
                        },
                        "Select a date range"
                      ),
                      m(
                        "small.form-text.text-muted",
                        "For which dates to fetch heartbeats" +
                          formatDateRange(state)
                      )
                    ]),
                    m("div.form-group.d-flex.flex-row-reverse", [
                      m(
                        "button.btn.btn-secondary.m-1[type=button]",
                        { onclick: closeModal },
                        "Close"
                      ),
                      m("button.btn.btn-primary.m-1[type=submit]", "Submit")
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
  openModal,
  startJobStatusCheck,
  stopJobStatusCheck
};
