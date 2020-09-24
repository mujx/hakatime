import m from "mithril";
import _ from "lodash";
import path from "path";
import ApexCharts from "apexcharts/dist/apexcharts.common";
import Litepicker from "litepicker";

// Models
import TimeRange from "../models/TimeRange.js";
import OverviewState from "../models/State.js";
import LocalState from "../models/ProjectState.js";

// Utils
import { mkSingleStatCard } from "../single_stat_card.js";
import cards from "../card_container.js";
import utils from "../utils.js";
import config from "../config.js";

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

function mkHourDistribution() {
  return cards.mkCardContainer(
    "Activity per hour of day",
    m(hourDistribution())
  );
}

function hourDistribution() {
  let _chart = null;

  return {
    view: () => {
      return m("div.chart");
    },

    onbeforeremove: () => {
      if (_chart) {
        _chart.destroy();
        _chart = null;
      }
    },

    oncreate: vnode => {
      if (LocalState.obj == null || LocalState.obj.totalSeconds === 0) return;

      const data = _.zipObject([...Array(24).keys()], Array(24).fill(0));
      _.forEach(LocalState.obj.hour, function (v) {
        data[utils.addTimeOffset(v.name)] = (v.totalSeconds / 3600).toFixed(1);
      });

      const options = {
        series: [
          {
            data: _.toArray(data),
            name: "Activity"
          }
        ],
        noData: config.noData,
        tooltip: {
          y: {
            formatter: function (val) {
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
          toolbar: config.toolbar,
          animations: config.animations
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

      _chart = new ApexCharts(vnode.dom, options);
      _chart.render();
    }
  };
}

function dayRadarChart() {
  let _chart = null;

  return {
    view: () => {
      return m("div.chart");
    },

    onbeforeremove: () => {
      if (_chart) {
        _chart.destroy();
        _chart = null;
      }
    },

    oncreate: vnode => {
      if (LocalState.obj == null) return;

      const data = _.zipObject([...Array(7).keys()], Array(7).fill(0));
      _.forEach(LocalState.obj.weekDay, function (v) {
        data[v.name] = (v.totalSeconds / 3600).toFixed(1);
      });

      const options = {
        series: [
          {
            data: _.toArray(data),
            name: "Activity"
          }
        ],
        noData: config.noData,
        tooltip: {
          y: {
            formatter: function (val) {
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
          toolbar: config.toolbar,
          animations: config.animations
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

      _chart = new ApexCharts(vnode.dom, options);
      _chart.render();
    }
  };
}

function pieChart() {
  let _chart = null;
  return {
    view: () => {
      return m("div.chart");
    },

    onbeforeremove: () => {
      if (_chart) {
        _chart.destroy();
        _chart = null;
      }
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

      const options = {
        series: data,
        noData: config.noData,
        chart: {
          type: "donut",
          height: "260",
          animations: config.animations
        },
        labels: names
      };

      _chart = new ApexCharts(vnode.dom, options);
      _chart.render();
    }
  };
}

/*
 * Row with single stats only. Each stat has a name, value, and an icon.
 */
function mkTopStatRow() {
  const totalHrs = utils.secondsToHms(utils.getTotalCodingTime(LocalState.obj));

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
  let _chart = null;
  return {
    view: () => {
      return m("div.chart");
    },

    onbeforeremove: () => {
      if (_chart) {
        _chart.destroy();
        _chart = null;
      }
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

      const options = {
        series: [
          {
            data: data,
            name: "Activity"
          }
        ],
        noData: config.noData,
        tooltip: {
          y: {
            formatter: function (val) {
              return val + " hours";
            }
          }
        },
        chart: {
          type: "bar",
          height: 360,
          toolbar: config.toolbar,
          animations: config.animations
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
          formatter: function (val, opt) {
            val = opt.w.globals.labels[opt.dataPointIndex];

            if (typeof val === "string") {
              const basename = path.dirname(val).split(path.sep).pop();
              const filename = val.replace(/^.*[/]/, "");

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

      _chart = new ApexCharts(vnode.dom, options);
      _chart.render();
    }
  };
}

function barChart() {
  let _chart = null;

  return {
    view: () => {
      return m("div.chart");
    },

    onbeforeremove: () => {
      if (_chart) {
        _chart.destroy();
        _chart = null;
      }
    },

    oncreate: vnode => {
      if (!LocalState.obj || LocalState.obj.totalSeconds === 0) return;

      const values = LocalState.obj.dailyTotal.map(v => (v / 3600).toFixed(1));

      const data = _.zip(LocalState.dates, values).map(data => {
        return { x: data[0], y: data[1] };
      });

      const options = {
        chart: {
          type: "bar",
          height: "250",
          toolbar: config.toolbar,
          animations: config.animations
        },
        series: [
          {
            name: LocalState.currentProject,
            data: data
          }
        ],
        noData: config.noData,
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
            formatter: function (val) {
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

      _chart = new ApexCharts(vnode.dom, options);
      _chart.render();
    }
  };
}

let dateRangePicker;

export default {
  oninit: function () {
    if (!OverviewState.obj) {
      OverviewState.fetchItems(() => {
        LocalState.initProjectList(OverviewState.obj.projects);
      });

      return;
    }

    LocalState.initProjectList(OverviewState.obj.projects);
  },
  onremove: () => {
    if (dateRangePicker) {
      dateRangePicker.destroy();
      dateRangePicker = null;
    }
  },
  view: () => {
    document.title = "Hakatime | Projects";

    if (LocalState.obj == null) {
      return m("div.spinner", [
        m("div.bounce1"),
        m("div.bounce2"),
        m("div.bounce3")
      ]);
    }

    const toolbar = m("div.d-sm-flex.mb-4", [
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
                onclick: LocalState.fetchProjectStats
              },
              project
            );
          })
        )
      ]),
      m("div.dropdown.mr-2", [
        m(
          "button.btn.btn-primary.dropdown-toggle.shadow-sm[data-toggle='dropdown'][aria-haspopup='true'][aria-expanded='false']",
          {
            type: "button",
            id: "dropdownMenuButton"
          },
          [
            m("i.fas.fa-clock.fa-md.text-white-50.mr-2"),
            m("small", `Cut-off limit (${TimeRange.timeLimit} mins)`)
          ]
        ),
        m(
          'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
          [5, 10, 15, 20, 30].map(r => {
            return m(
              "a.btn.dropdown-item",
              {
                onclick: () => {
                  if (TimeRange.setTimeLimit(r)) LocalState.fetchProjectStats();
                }
              },
              `${r} mins`
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
            m("small", `Date range (${TimeRange.dateRange()} days)`),
            m("a", { id: "date-range-picker-project" })
          ]
        ),
        m(
          'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
          config.dateRangePresets
            .map(r => {
              return m(
                "a.btn.dropdown-item",
                {
                  onclick: () => {
                    if (TimeRange.setDaysFromToday(r))
                      LocalState.fetchProjectStats();
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

                    if (!dateRangePicker) {
                      dateRangePicker = new Litepicker({
                        ...config.datePicker,
                        element: document.getElementById(
                          "date-range-picker-project"
                        ),
                        onSelect: (d1, d2) => {
                          if (TimeRange.setDays(d1, d2)) {
                            LocalState.fetchProjectStats(
                              null,
                              d1.toISOString(),
                              d2.toISOString()
                            );
                          }
                        }
                      });
                    }

                    dateRangePicker.show();
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
      m("div.row", mkTopStatRow()),
      m("div.row", [
        m("div.col-xl-8.col-lg-7", mkLineChart()),
        m("div.col-xl-4.col-lg-5", mkPieChart())
      ]),
      m("div.row", [
        m("div.col-xl-6", mkWeekDayRadar()),
        m("div.col-xl-6", mkHourDistribution())
      ]),
      m("div.row", [m("div.col-xl-12", mkFileChart())])
    ];
  }
};
