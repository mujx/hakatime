import m from "mithril";
import Litepicker from "litepicker";
import TimeRange from "../models/TimeRange";
import config from "../config.js";
import Cards from "../card_container.js";
import * as api from "../api";
import * as auth from "../auth";
import utils from "../utils";

const State = {
  isInit: false,
  currentLang: null,
  leaderboards: null,
  langs: [],
  dateRangePicker: null
};

function mkLeaderBoard(users = []) {
  return {
    view: () => {
      return m("table.table.table-bordered.table-hover", [
        m(
          "tbody",
          users.map(function (u) {
            return m("tr", [
              m("td", u.name),
              m("td", utils.secondsToHms(u.value))
            ]);
          })
        )
      ]);
    }
  };
}

const LanguagePicker = {
  view: () => {
    return State.langs.map(function (l) {
      return m(
        "a.dropdown-item",
        {
          onclick: () => {
            State.currentLang = l;
          }
        },
        l
      );
    });
  }
};

const LeaderBoard = {
  view: () => {
    const toolbar = m("div.d-flex.mb-4", [
      m("h1.h3.mr-auto.mb-0.text-gray-800", "Leaderboards"),
      m("div.dropdown", [
        m(
          "button.btn.btn-primary.dropdown-toggle.shadow-sm[data-toggle='dropdown'][aria-haspopup='true'][aria-expanded='false']",
          {
            type: "button",
            id: "dropdownMenuButton"
          },
          [
            m("i.fas.fa-calendar.fa-md.text-white-50.mr-2"),
            m("small", `Date range (${TimeRange.dateRange()} days)`),
            m("a", { id: "date-range-picker" })
          ]
        ),
        m(
          'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
          config.dateRangePresets
            .map(r => {
              return m(
                "a.btn.dropdown-item",
                {
                  onclick: e => {
                    e.redraw = false;
                    if (TimeRange.setDaysFromToday(r))
                      fetchLeaderboards({
                        start: TimeRange.start().toISOString(),
                        end: TimeRange.end().toISOString()
                      });
                  }
                },
                `Last ${r} days`
              );
            })
            .concat([
              m("div.dropdown-divider"),
              m(
                "a.btn.dropdown-item",
                {
                  onclick: e => {
                    e.redraw = false;

                    if (!State.dateRangePicker) {
                      State.dateRangePicker = new Litepicker({
                        ...config.datePicker,
                        element: document.getElementById("date-range-picker"),
                        onSelect: (d1, d2) => {
                          fetchLeaderboards({
                            start: d1.toISOString(),
                            end: d2.toISOString()
                          });
                        }
                      });
                    }

                    State.dateRangePicker.show();
                  }
                },
                "Pick a date range"
              )
            ])
        )
      ])
    ]);

    return [
      toolbar,
      m("div.d-flex", [
        m("div.col-6", [
          Cards.mkCardContainer(
            "Overall Time",
            m(mkLeaderBoard(State.leaderboards.global))
          )
        ]),
        m("div.col-6", [
          Cards.mkCardContainer(
            State.currentLang
              ? `Language Usage (${State.currentLang})`
              : "Language Usage",
            m(mkLeaderBoard(State.leaderboards.lang[State.currentLang])),
            m(LanguagePicker)
          )
        ])
      ])
    ];
  }
};

function fetchLeaderboards(params) {
  api
    .getLeaderboards(params)
    .then(function (res) {
      State.leaderboards = res;
      State.isInit = true;
      State.langs = Object.keys(State.leaderboards.lang);
      State.currentLang = State.langs ? State.langs[0] : null;
      m.redraw();
    })
    .catch(err => auth.retryCall(err, () => fetchLeaderboards(params)));
}

export default {
  oninit: function () {
    fetchLeaderboards({
      start: TimeRange.start().toISOString(),
      end: TimeRange.end().toISOString()
    });
  },
  view: () => {
    document.title = "Hakatime | Leaderboards";

    if (!State.isInit) {
      return m("div.spinner", [
        m("div.bounce1"),
        m("div.bounce2"),
        m("div.bounce3")
      ]);
    }

    return m(LeaderBoard);
  }
};
