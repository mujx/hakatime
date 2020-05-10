import _ from "lodash";

export default {
  getDaysBetween: function(start, end) {
    for (var arr = [], dt = start; dt <= end; dt.setDate(dt.getDate() + 1)) {
      arr.push(new Date(dt));
    }
    return arr;
  },

  secondsToHms: function(d) {
    d = Number(d);
    const h = Math.floor(d / 3600);
    const m = Math.floor((d % 3600) / 60);

    const hDisplay = h > 0 ? h + (h == 1 ? " hr " : " hrs ") : "";
    const mDisplay = m > 0 ? m + (m == 1 ? " min " : " mins ") : "";

    return hDisplay + mDisplay;
  },

  getTotalCodingTime: function(obj) {
    if (!obj) return;

    return obj.totalSeconds;
  },
  getTotalProjects: function(obj) {
    if (!obj) return;

    return obj.projects.length;
  },
  getMostActiveProject: function(obj) {
    if (!obj) return;

    const res = _.orderBy(obj.projects, ["totalSeconds"], ["desc"])[0];
    if (res) return res.name;
  },
  getMostActiveLanguage: function(obj) {
    if (!obj) return;

    const res = _.orderBy(obj.languages, ["totalSeconds"], ["desc"])[0];
    if (res) return res.name;
  }
};
