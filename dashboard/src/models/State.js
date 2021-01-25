import * as api from "../api";
import * as auth from "../auth";
import TimeRange from "./TimeRange";
import utils from "../utils.js";

const Model = {
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
    api
      .getTimeline({
        start: d1.toISOString(),
        end: d2.toISOString(),
        timeLimit: TimeRange.timeLimit
      })
      .then(obj => {
        if (typeof callback === "function") {
          callback(obj);
        } else {
          Model.timeline = obj;
        }
      })
      .catch(err => auth.retryCall(err, Model.fetchTimeline));
  },
  // Fetch the statistics.
  fetchItems: (cb, d1, d2) => {
    Promise.all([
      api.getStats({
        start: d1 || TimeRange.start().toISOString(),
        end: d2 || TimeRange.end().toISOString(),
        timeLimit: TimeRange.timeLimit
      }),
      api.getTimeline({
        start: utils.removeHours(new Date(), 12).toISOString(),
        end: new Date().toISOString(),
        timeLimit: TimeRange.timeLimit
      })
    ])
      .then(function (values) {
        const obj = values[0];

        Model.obj = obj;
        Model.dates = utils.getDaysBetween(
          new Date(Model.obj.startDate),
          new Date(Model.obj.endDate)
        );

        Model.timeline = values[1];

        typeof cb === "function" && cb();
      })
      .catch(err => auth.retryCall(err, () => Model.fetchItems(cb)));
  }
};

export default Model;
