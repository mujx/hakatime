import { State } from "./State.js";

let TimeRange = {
  numOfDays: 30,

  setDays: function(d) {
    if (TimeRange.numOfDays === d) return false;
    TimeRange.numOfDays = d;
    return true;
  }
};

export { TimeRange };
