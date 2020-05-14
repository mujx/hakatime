import m from "mithril";
import _ from "lodash";
import path from "path";
import ApexCharts from "apexcharts/dist/apexcharts.common";

// Models
import { TimeRange } from "../models/TimeRange.js";
import { State } from "../models/State.js";

// Utils
import { mkSingleStatCard } from "../single_stat_card.js";
import cards from "../card_container.js";
import utils from "../utils.js";
import * as auth from "../auth.js";

function fetchProjectStats(event) {
  if (event) LocalState.currentProject = event.target.innerHTML;
  const start = new Date();
  const today = new Date();
  start.setDate(start.getDate() - TimeRange.numOfDays);

  m.request({
    url: `/api/v1/users/current/projects/${LocalState.currentProject}`,
    responseType: "json",
    headers: {
      authorization: auth.getHeaderToken()
    },
    params: {
      start: start.toISOString(),
      end: today.toISOString()
    }
  })
    .then(function(obj) {
      LocalState.obj = obj;
      LocalState.dates = utils.getDaysBetween(
        new Date(obj.startDate),
        new Date(obj.endDate)
      );
    })
    .catch(err => auth.retryCall(err, () => fetchProjectStats(event)));
}

// TODO: This will have to be implemented on the server.
function addTimeOffset(v) {
  const n = parseInt(v);
  const offSet = new Date().getTimezoneOffset() / 60;

  return ((n - offSet) % 23).toString();
}

function mkFileChart() {
  return cards.mkCardContainer("Most active files", m(fileChart()));
}

function mkLineChart() {
  return cards.mkCardContainer("Total activity", m(barChart()));
}

function mkPieChart() {
  return cards.mkCardContainer("Language breakdown", m(pieChart()));
}

function mkWeekDayRadar() {
  return cards.mkCardContainer("Activity per weekday", m(dayRadarChart()));
}

function mkHourDayRadar() {
  return cards.mkCardContainer("Activity per hour of day", m(hourRadarChart()));
}

function hourRadarChart() {
  return {
    view: vnode => {
      return m("div.chart");
    },
    oncreate: vnode => {
      if (LocalState.obj == null) return;

      let data = _.zipObject([...Array(24).keys()], Array(24).fill(0));
      _.forEach(LocalState.obj.hour, function(v) {
        data[addTimeOffset(v.name)] = (v.totalSeconds / 3600).toFixed(1);
      });

      const options = {
        series: [
          {
            data: _.toArray(data),
            name: "Activity"
          }
        ],
        tooltip: {
          y: {
            formatter: function(val) {
              return val + " hours";
            }
          }
        },
        dataLabels: {
          enabled: false
        },
        chart: {
          type: "bar",
          height: "300",
          toolbar: {
            show: false
          }
        },
        plotOptions: {
          bar: {
            columnWidth: "40%",
            endingShape: "rounded"
          }
        },
        yaxis: {
          title: {
            text: "Hours"
          }
        },
        xaxis: {
          categories: [...Array(24).keys()]
        }
      };

      var chart = new ApexCharts(vnode.dom, options);
      chart.render();
    }
  };
}

function dayRadarChart() {
  return {
    view: vnode => {
      return m("div.chart");
    },
    oncreate: vnode => {
      if (LocalState.obj == null) return;

      let data = _.zipObject([...Array(7).keys()], Array(7).fill(0));
      _.forEach(LocalState.obj.weekDay, function(v) {
        data[v.name] = (v.totalSeconds / 3600).toFixed(1);
      });

      const options = {
        series: [
          {
            data: _.toArray(data),
            name: "Activity"
          }
        ],
        tooltip: {
          y: {
            formatter: function(val) {
              return val + " hours";
            }
          }
        },
        dataLabels: {
          enabled: false
        },
        chart: {
          type: "radar",
          height: "300",
          toolbar: {
            show: false
          }
        },
        plotOptions: {
          radar: {
            size: 120,
            polygons: {
              strokeColors: "#e9e9e9",
              fill: {
                colors: ["#f8f8f8", "#fff"]
              }
            }
          }
        },
        yaxis: {
          show: false
        },
        xaxis: {
          categories: [
            "Sunday",
            "Monday",
            "Tuesday",
            "Wednesday",
            "Thursday",
            "Friday",
            "Saturday"
          ]
        }
      };

      var chart = new ApexCharts(vnode.dom, options);
      chart.render();
    }
  };
}

function pieChart() {
  return {
    view: vnode => {
      return m("div.chart");
    },

    oncreate: vnode => {
      if (LocalState.obj == null) return;

      const dataValues = LocalState.obj.languages.map(v => {
        return {
          data: parseFloat((v.totalPct * 100).toFixed(2)),
          name: v.name
        };
      });

      const data = dataValues.map(v => v.data);
      const names = dataValues.map(v => v.name);

      var options = {
        series: data,
        chart: {
          type: "donut",
          height: "260"
        },
        labels: names
      };

      var chart = new ApexCharts(vnode.dom, options);
      chart.render();
    }
  };
}

/*
 * Row with single stats only. Each stat has a name, value, and an icon.
 */
function mkTopStatRow() {
  let totalHrs = utils.secondsToHms(utils.getTotalCodingTime(LocalState.obj));

  return [
    {
      name: "Total coding time",
      value: totalHrs ? `${totalHrs}` : "0",
      icon: "globe",
      textType: "primary"
    },
    {
      name: "Languages",
      value: LocalState.obj.languages.length,
      icon: "code",
      textType: "info"
    },
    {
      name: "Files touched",
      value: LocalState.obj.files.length,
      icon: "file",
      textType: "success"
    },
    {
      name: "Most active language",
      value: utils.getMostActiveLanguage(LocalState.obj),
      icon: "code",
      textType: "success"
    }
  ].map(conf => {
    return m("div.col-xl-3.col-md-6.mb-4", m(mkSingleStatCard(conf)));
  });
}

function fileChart() {
  return {
    view: vnode => {
      return m("div.chart");
    },

    oncreate: vnode => {
      if (LocalState.obj == null) return;

      const myData = _.take(
        _.orderBy(LocalState.obj.files, ["totalSeconds"], ["desc"]).filter(
          v => (v.totalSeconds / 3600).toFixed(1) > 0
        ),
        10
      );
      const data = myData.map(v => (v.totalSeconds / 3600).toFixed(1));
      const categories = myData.map(v => v.name);

      var options = {
        series: [
          {
            data: data,
            name: "Activity"
          }
        ],
        tooltip: {
          y: {
            formatter: function(val) {
              return val + " hours";
            }
          }
        },
        chart: {
          type: "bar",
          height: 360,
          toolbar: {
            show: false
          }
        },
        plotOptions: {
          bar: {
            horizontal: true,
            distributed: true,
            columnWidth: "40%",
            barHeight: "80%",
            endingShape: "rounded",
            backgroundBarColors: ["#f8f8f8", "#fff"]
          }
        },
        dataLabels: {
          enabled: true,
          textAnchor: "start",
          style: {
            colors: ["#fff"],
            fontFamily: "Nunito"
          },
          formatter: function(val, opt) {
            val = opt.w.globals.labels[opt.dataPointIndex];

            if (typeof val === "string") {
              const basename = path
                .dirname(val)
                .split(path.sep)
                .pop();
              const filename = val.replace(/^.*[\\\/]/, "");

              if (!basename) return filename;

              return path.join(basename, filename);
            }

            return val;
          },
          offsetX: 0,
          dropShadow: {
            enabled: true
          }
        },
        yaxis: {
          show: false
        },
        legend: {
          show: false
        },
        xaxis: {
          title: {
            text: "Hours"
          },
          categories: categories
        }
      };

      let myChart = new ApexCharts(vnode.dom, options);
      myChart.render();
    }
  };
}

function barChart() {
  return {
    view: vnode => {
      return m("div.chart");
    },

    oncreate: vnode => {
      if (LocalState.obj == null) return;

      const values = LocalState.obj.dailyTotal.map(v => (v / 3600).toFixed(1));

      const data = _.zip(LocalState.dates, values).map(data => {
        return { x: data[0], y: data[1] };
      });

      var options = {
        chart: {
          type: "bar",
          height: "250",
          toolbar: {
            show: false
          }
        },
        series: [
          {
            name: LocalState.currentProject,
            data: data
          }
        ],
        xaxis: {
          type: "datetime"
        },
        yaxis: {
          title: {
            text: "Hours"
          }
        },
        tooltip: {
          y: {
            formatter: function(val) {
              return val + " hours";
            }
          }
        },
        dataLabels: {
          enabled: false
        },
        plotOptions: {
          bar: {
            columnWidth: "40%",
            endingShape: "rounded"
          }
        }
      };

      let myChart = new ApexCharts(vnode.dom, options);
      myChart.render();
    }
  };
}

let LocalState = {
  projects: [],
  currentProject: null,
  dates: null,
  obj: null,
  fetchProjectList: function() {
    if (!State.obj) return;

    LocalState.projects = _.orderBy(
      State.obj.projects,
      ["totalSeconds"],
      ["desc"]
    )
      .map(p => p.name)
      .filter(n => n !== "Other");

    if (LocalState.projects.length > 0) {
      LocalState.currentProject = LocalState.projects[0];
    }

    fetchProjectStats();
  }
};

export default {
  oninit: LocalState.fetchProjectList,
  view: vnode => {
    document.title = "Hakatime | Projects";

    let customizeBtn = m("div.mr-2", [
      m(
        "button.btn.btn-primary.shadow-sm",
        {
          type: "button"
        },
        m("i.fas.fa-cog.fa-md.text-white-50")
      )
    ]);

    let ranges = [7, 15, 30, 45, 90];
    let toolbar = m("div.d-sm-flex.mb-4", [
      m(
        "h1.h3.mb-0.mr-auto.text-gray-800",
        LocalState.currentProject ? LocalState.currentProject : "Projects"
      ),
      m("div.dropdown.mr-2", [
        m(
          "button.btn.btn-primary.dropdown-toggle.shadow-sm[data-toggle='dropdown'][aria-haspopup='true'][aria-expanded='false']",
          {
            type: "button",
            id: "dropdownMenuButton"
          },
          [m("i.fas.fa-book.fa-md.text-white-50.mr-2"), m("small", "Projects")]
        ),
        m(
          'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
          LocalState.projects.map(project => {
            return m(
              "a.btn.dropdown-item",
              {
                onclick: fetchProjectStats
              },
              project
            );
          })
        )
      ]),
      m("div.dropdown", [
        m(
          "button.btn.btn-primary.dropdown-toggle.shadow-sm[data-toggle='dropdown'][aria-haspopup='true'][aria-expanded='false']",
          {
            type: "button",
            id: "dropdownMenuButton"
          },
          [
            m("i.fas.fa-calendar.fa-md.text-white-50.mr-2"),
            m("small", `Time range (${TimeRange.numOfDays} days)`)
          ]
        ),
        m(
          'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
          ranges.map(r => {
            return m(
              "a.btn.dropdown-item",
              {
                onclick: e => {
                  if (TimeRange.setDays(r)) fetchProjectStats();
                }
              },
              `Last ${r} days`
            );
          })
        )
      ])
    ]);

    if (LocalState.obj == null) {
      return m("div.spinner", [
        m("div.bounce1"),
        m("div.bounce2"),
        m("div.bounce3")
      ]);
    }

    return [
      toolbar,
      m("div.row", mkTopStatRow()),
      m("div.row", [
        m("div.col-xl-8.col-lg-7", mkLineChart()),
        m("div.col-xl-4.col-lg-5", mkPieChart())
      ]),
      m("div.row", [
        m("div.col-xl-6", mkWeekDayRadar()),
        m("div.col-xl-6", mkHourDayRadar())
      ]),
      m("div.row", [m("div.col-xl-12", mkFileChart())])
    ];
  }
};
