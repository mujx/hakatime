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
    Model.projects = projects;

    if (Model.projects.length > 0) {
      Model.currentProject = Model.projects[0];
    }

    Model.fetchProjectStats();
  },
  fetchProjectStats: (event, d1, d2) => {
    // If it was triggered by a click event.
    if (event) {
      event.redraw = false;
      Model.currentProject = event.target.innerHTML;
    }

    const start = new Date();
    const today = new Date();
    start.setDate(start.getDate() - TimeRange.numOfDays);

    api
      .getProject(Model.currentProject, {
        start: d1 || start.toISOString(),
        end: d2 || today.toISOString(),
        timeLimit: TimeRange.timeLimit
      })
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
  },
  initialize: () => {
    api
      .getUserProjects({
        start: TimeRange.start().toISOString(),
        end: TimeRange.end().toISOString()
      })
      .then(function ({ projects }) {
        Model.initProjectList(projects);
      })
      .catch(function (err) {
        auth.retryCall(err, () => Model.initialize());
      });
  }
};

export default Model;
