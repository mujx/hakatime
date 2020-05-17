import m from "mithril";
import * as auth from "../auth";
import TimeRange from "./TimeRange";
import utils from "../utils.js";

let Model = {
  // The raw statistics from the server.
  obj: null,
  // Timeline data.
  timeline: null,
  interval: null,
  // Date range.
  dates: null,
  clear: () => {
    Model.obj = null;
    Model.timeline = null;
    Model.dates = null;
  },
  fetchTimeline: (d1, d2, callback) => {
    m.request({
      url: "/api/v1/users/current/timeline",
      responseType: "json",
      background: true,
      params: {
        start: d1.toISOString(),
        end: d2.toISOString()
      },
      headers: {
        authorization: auth.getHeaderToken()
      }
    })
      .then(obj => {
        if (callback) {
          callback(obj);
        } else {
          Model.timeline = obj;
        }
      })
      .catch(err => auth.retryCall(err, Model.fetchItems));
  },
  // Fetch the statistics.
  fetchItems: () => {
    if (!auth.isLoggedIn()) return;

    const start = new Date();
    const today = new Date();
    start.setDate(start.getDate() - TimeRange.numOfDays);

    const headers = {
      authorization: auth.getHeaderToken()
    };

    let yesterday = new Date(today);
    yesterday.setDate(yesterday.getDate() - 1);

    Promise.all([
      m.request({
        url: "/api/v1/users/current/stats",
        responseType: "json",
        params: {
          start: start.toISOString(),
          end: today.toISOString()
        },
        headers: headers
      }),
      m.request({
        url: "/api/v1/users/current/timeline",
        responseType: "json",
        params: {
          start: yesterday.toISOString(),
          end: today.toISOString()
        },
        headers: headers
      })
    ])
      .then(function(values) {
        let obj = values[0];

        Model.obj = obj;
        Model.dates = utils.getDaysBetween(
          new Date(Model.obj.startDate),
          new Date(Model.obj.endDate)
        );

        Model.timeline = values[1];
      })
      .catch(err => auth.retryCall(err, Model.fetchItems));
  }
};

export default Model;
