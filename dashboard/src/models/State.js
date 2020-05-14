import m from "mithril";
import * as auth from "../auth";
import { TimeRange } from "./TimeRange";
import utils from "../utils.js";

let State = {
  // The raw statistics from the server.
  obj: null,
  interval: null,
  // Date range.
  dates: null,
  // Fetch the statistics.
  fetchItems: () => {
    if (!auth.isLoggedIn()) return;

    const start = new Date();
    const today = new Date();
    start.setDate(start.getDate() - TimeRange.numOfDays);

    m.request({
      url: "/api/v1/users/current/stats",
      responseType: "json",
      background: true,
      params: {
        start: start.toISOString(),
        end: today.toISOString()
      },
      headers: {
        authorization: auth.getHeaderToken()
      }
    })
      .then(function(obj) {
        State.obj = obj;
        State.dates = utils.getDaysBetween(
          new Date(State.obj.startDate),
          new Date(State.obj.endDate)
        );
        m.redraw();
      })
      .catch(err => auth.retryCall(err, State.fetchItems));
  }
};

export { State };
