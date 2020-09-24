// Third-party
import m from "mithril";
import Litepicker from "litepicker";
import _ from "lodash";
import ApexCharts from "apexcharts/dist/apexcharts.common";

// Models
import State from "../models/State";
import TimeRange from "../models/TimeRange";

// Utils
import { mkSingleStatCard } from "../single_stat_card.js";
import utils from "../utils.js";
import cards from "../card_container.js";
import config from "../config.js";
import * as auth from "../auth.js";

// Easy access of all charts in the page.
const Charts = {
  timelineChart: {
    chart: null,
    mkOpts: function (series) {
      const options = {
        series: series,
        noData: config.noData,
        grid: {
          show: true,
          xaxis: {
            lines: {
              show: true
            }
          }
        },
        chart: {
          height: 350,
          type: "rangeBar",
          fontFamily: "Nunito",
          toolbar: config.toolbar,
          animations: config.animations
        },
        plotOptions: {
          bar: {
            horizontal: true
          }
        },
        xaxis: {
          type: "datetime",
          labels: {
            datetimeUTC: false
          }
        },
        yaxis: {
          labels: {
            style: {
              fontFamily: "Nunito",
              fontWeight: 500,
              fontSize: "14px"
            }
          }
        },
        tooltip: {
          x: {
            show: true,
            format: "d MMM, HH:mm"
          }
        },
        legend: {
          position: "top",
          horizontalAlign: "left"
        }
      };

      return options;
    }
  },
  column: {
    chart: null,
    mkOpts: function (series) {
      return {
        chart: {
          type: "bar",
          fontFamily: "Nunito",
          height: "250",
          toolbar: config.toolbar,
          animations: config.animations
        },
        noData: config.noData,
        series: [
          {
            name: "Coding time",
            data: series
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
    }
  },
  pieChart: {
    chart: null
  }
};

// Convert data retrieved from the API server to a format suitable for the chart.
function mkTimelineSeries(rawData) {
  return _.keys(rawData.timelineLangs).map(function (k) {
    return {
      name: k,
      data: rawData.timelineLangs[k].map(v => {
        return {
          x: v.tName,
          y: [
            new Date(v.tRangeStart).getTime(),
            new Date(v.tRangeEnd).getTime()
          ]
        };
      })
    };
  });
}

// Check if the given chart item has enough value to be displayed.
// Attempting to reduce noise by remove low values.
function hasEnoughPercentage(val) {
  return val >= 1;
}

/*
 * Row with single stats only. Each stat has a name, value, and an icon.
 */
function mkTopStatRow() {
  const totalHrs = utils.secondsToHms(utils.getTotalCodingTime(State.obj));

  return [
    {
      name: "Total coding time",
      value: totalHrs ? `${totalHrs}` : "0",
      icon: "globe",
      textType: "primary"
    },
    {
      name: "Total projects",
      value: utils.getTotalProjects(State.obj),
      icon: "calculator",
      textType: "info"
    },
    {
      name: "Most active project",
      value: utils.getMostActiveProject(State.obj),
      icon: "crown",
      textType: "success"
    },
    {
      name: "Most active language",
      value: utils.getMostActiveLanguage(State.obj),
      icon: "code",
      textType: "success"
    }
  ].map(conf => {
    return m("div.col-xl-3.col-md-6.mb-4", m(mkSingleStatCard(conf)));
  });
}

function pieChart() {
  let _chart;
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
      if (State.obj == null) return;

      const dataValues = State.obj.projects
        .map(v => {
          return {
            data: parseFloat((v.totalPct * 100).toFixed(2)),
            name: v.name
          };
        })
        .filter(o => hasEnoughPercentage(o.data));

      const data = dataValues.map(v => v.data);
      const names = dataValues.map(v => v.name);

      const options = {
        series: data,
        noData: config.noData,
        chart: {
          fontFamily: "Nunito",
          type: "donut",
          animations: config.animations
        },
        labels: names
      };

      _chart = new ApexCharts(vnode.dom, options);
      _chart.render();
    }
  };
}

function columnChart() {
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
      if (State.obj == null) return;

      const values = State.obj.dailyTotal.map(v => (v / 3600).toFixed(1));
      const series = _.zip(State.dates, values).map(data => {
        return { x: data[0], y: data[1] };
      });

      _chart = new ApexCharts(vnode.dom, Charts.column.mkOpts(series));
      _chart.render();
    }
  };
}

function mkTimeline() {
  let picker;

  const MenuComponent = {
    view: () => {
      const datesItems = [6, 12, 24, 48].map(i => {
        return m(
          "a.dropdown-item",
          {
            onclick: e => {
              e.redraw = false;

              MenuComponent.updateSeries(
                utils.removeHours(new Date(), i),
                new Date()
              );
            }
          },
          `Last ${i} hours`
        );
      });

      return [
        m("a.dropdown-item", { id: "datetime-picker" }, "Pick a date range"),
        m("div.dropdown-divider"),
        m("div.dropdown-header", "Recent"),
        datesItems
      ];
    },
    updateSeries: (d1, d2) => {
      State.fetchTimeline(d1, d2, res => {
        if (Charts.timelineChart.chart)
          Charts.timelineChart.chart.updateSeries(mkTimelineSeries(res));
      });
    },
    onbeforeremove: () => {
      if (picker) picker.destroy();
    },
    oncreate: () => {
      picker = new Litepicker({
        element: document.getElementById("datetime-picker"),
        minDays: 1,
        maxDays: 2,
        maxDate: utils.addDays(new Date(), 1),
        showTooltip: true,
        singleMode: false,
        mobileFriendly: true,
        onSelect: MenuComponent.updateSeries
      });
    }
  };

  return cards.mkCardContainer(
    "Timeline",
    m(timelineChart()),
    m(MenuComponent)
  );
}

function timelineChart() {
  return {
    view: () => {
      return m("div.chart");
    },

    onbeforeremove: () => {
      if (Charts.timelineChart.chart) {
        Charts.timelineChart.chart.destroy();
        Charts.timelineChart.chart = null;
      }
    },

    oncreate: vnode => {
      if (State.timeline == null) return;

      const opts = Charts.timelineChart.mkOpts(
        mkTimelineSeries(State.timeline)
      );

      Charts.timelineChart.chart = new ApexCharts(vnode.dom, opts);
      Charts.timelineChart.chart.render();
    }
  };
}

function heatmapDataForProjects(state, num = 7) {
  const projects = _.orderBy(state.obj.projects, ["totalSeconds"], ["desc"]);

  // List of top N projects.
  const topProjects = _.take(projects, num).map(v => {
    return v.name;
  });

  // Map for faster filtering.
  const projectMap = _.reduce(
    topProjects,
    function (obj, p) {
      obj[p] = true;
      return obj;
    },
    {}
  );

  return projects
    .map(v => {
      return {
        name: v.name,
        data: v.totalDaily.map(i => (i / 3600).toFixed(1))
      };
    })
    .filter(v => {
      return projectMap[v.name] != undefined;
    });
}

function heatmapDataForLangs(state, num = 7) {
  const langs = _.orderBy(state.obj.languages, ["totalSeconds"], ["desc"]);

  // List of top N languages.
  const topLangs = _.take(langs, num).map(v => {
    return v.name;
  });
  // Map for faster filtering.
  const langMap = _.reduce(
    topLangs,
    function (obj, p) {
      obj[p] = true;
      return obj;
    },
    {}
  );

  return langs
    .map(v => {
      return {
        name: v.name,
        data: v.totalDaily.map(i => (i / 3600).toFixed(1))
      };
    })
    .filter(v => {
      return langMap[v.name] != undefined;
    });
}

function heatmapChart(mkDataFn) {
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
      if (State.obj == null) return;

      const series = mkDataFn(State).map(v => {
        return {
          name: v.name,
          data: _.zip(State.dates, v.data).map(o => {
            return { x: o[0], y: o[1] };
          })
        };
      });

      const options = {
        series: series,
        noData: config.noData,
        dataLabels: {
          enabled: false
        },
        chart: {
          type: "heatmap",
          fontFamily: "Nunito",
          height: 250,
          toolbar: config.toolbar,
          animations: config.animations
        },
        xaxis: {
          type: "datetime"
        }
      };

      _chart = new ApexCharts(vnode.dom, options);
      _chart.render();
    }
  };
}

function mkColumnChart() {
  return cards.mkCardContainer("Total activity", m(columnChart()));
}

function mkPieChart() {
  return cards.mkCardContainer("Project breakdown", m(pieChart()));
}

function mkHeatMap() {
  return cards.mkCardContainer(
    "Activity per project",
    m(heatmapChart(heatmapDataForProjects))
  );
}

function mkLangHeatMap() {
  return cards.mkCardContainer(
    "Activity per language",
    m(heatmapChart(heatmapDataForLangs))
  );
}

let dateRangePicker;

export default {
  oninit: State.fetchItems,
  oncreate: () => {
    // Silent refresh.
    State.interval = setInterval(auth.checkInterval, 60000);
  },
  onremove: () => {
    clearInterval(State.interval);
  },
  onbeforeremove: () => {
    if (dateRangePicker) {
      dateRangePicker.destroy();
      dateRangePicker = null;
    }
  },
  view: () => {
    document.title = "Hakatime | Overview";

    if (State.obj == null) {
      return m("div.spinner", [
        m("div.bounce1"),
        m("div.bounce2"),
        m("div.bounce3")
      ]);
    }

    const toolbar = m("div.d-flex.mb-4", [
      m("h1.h3.mr-auto.mb-0.text-gray-800", "Overview"),
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
                  if (TimeRange.setTimeLimit(r)) State.fetchItems();
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
                  onclick: () => {
                    if (TimeRange.setDaysFromToday(r)) State.fetchItems();
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
                        element: document.getElementById("date-range-picker"),
                        onSelect: (d1, d2) => {
                          if (TimeRange.setDays(d1, d2))
                            State.fetchItems(
                              null,
                              d1.toISOString(),
                              d2.toISOString()
                            );
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
        m("div.col-xl-8.col-lg-7", mkColumnChart()),
        m("div.col-xl-4.col-lg-5", mkPieChart())
      ]),
      m("div.row", [
        m("div.col-xl-6", mkHeatMap()),
        m("div.col-xl-6", mkLangHeatMap())
      ]),
      m("div.row", m("div.col-xl-12", mkTimeline()))
    ];
  }
};
