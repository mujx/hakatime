import _ from "lodash";

import TimeRange from "./TimeRange";
import utils from "../utils.js";
import * as auth from "../auth";
import * as api from "../api";

const Model = {
  projects: [],
  currentProject: null,
  dates: null,
  obj: null,
  clear: () => {
    Model.projects = null;
    Model.currentProject = null;
    Model.dates = null;
    Model.obj = null;
  },
  initProjectList: projects => {
    Model.projects = _.orderBy(projects, ["totalSeconds"], ["desc"])
      .map(p => p.name)
      .filter(n => n !== "Other");

    if (Model.projects.length > 0) {
      Model.currentProject = Model.projects[0];
    }

    Model.fetchProjectStats();
  },
  fetchProjectStats: (event, d1, d2) => {
    // If it was triggered by a click event.
    if (event) Model.currentProject = event.target.innerHTML;

    const start = new Date();
    const today = new Date();
    start.setDate(start.getDate() - TimeRange.numOfDays);

    api
      .getProject(
        Model.currentProject,
        {
          start: d1 || start.toISOString(),
          end: d2 || today.toISOString(),
          timeLimit: TimeRange.timeLimit
        },
        auth.getHeaderToken()
      )
      .then(function (obj) {
        Model.obj = obj;
        Model.dates = utils.getDaysBetween(
          new Date(obj.startDate),
          new Date(obj.endDate)
        );
      })
      .catch(err =>
        auth.retryCall(err, () => Model.fetchProjectStats(event, d1, d2))
      );
  }
};

export default Model;
