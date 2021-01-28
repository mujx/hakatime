import _ from "lodash";

export default {
  truncate: function (input, num) {
    return input.length > num ? `${input.substring(0, num)}...` : input;
  },
  mkErrorMessage: function (e) {
    if (e.response && e.response.message) {
      return e.response.message;
    }

    if (e.response && e.response.error) {
      return e.response.error;
    }

    return "Unknown error";
  },
  copyToCliboard: function (v) {
    const elem = document.createElement("textarea");

    elem.value = v;
    elem.setAttribute("readonly", "");
    elem.style.position = "absolute";
    elem.style.left = "-99999px";

    document.body.appendChild(elem);
    elem.select();

    document.execCommand("copy");
    document.body.removeChild(elem);
  },
  removeHours: function (d, num) {
    const d1 = new Date(d);
    d1.setHours(d.getHours() - num);
    return d1;
  },
  addDays: function (d, num) {
    const d1 = new Date(d);
    d1.setDate(d.getDate() + num);
    return d1;
  },
  getDaysBetween: function (start, end) {
    let arr;
    let dt;

    for (arr = [], dt = start; dt <= end; dt.setDate(dt.getDate() + 1)) {
      arr.push(new Date(dt));
    }
    return arr;
  },
  // TODO: This will have to be implemented on the server.
  addTimeOffset: function (v) {
    const n = parseInt(v);
    const offSet = new Date().getTimezoneOffset() / 60;

    return ((n - offSet) % 23).toString();
  },
  secondsToHms: function (d) {
    d = Number(d);
    const h = Math.floor(d / 3600);
    const m = Math.floor((d % 3600) / 60);

    const hDisplay = h > 0 ? h + (h == 1 ? " hr " : " hrs ") : "";
    const mDisplay = m > 0 ? m + (m == 1 ? " min " : " mins ") : "";

    return hDisplay + mDisplay;
  },

  getTotalCodingTime: function (obj) {
    if (!obj) return 0;

    return obj.totalSeconds;
  },
  getTotalProjects: function (obj) {
    if (!obj) return 0;

    return obj.projects.length;
  },
  getMostActiveProject: function (obj) {
    if (!obj) return "-";

    const res = _.orderBy(obj.projects, ["totalSeconds"], ["desc"])[0];
    if (res) return res.name;
    else return "-";
  },
  getMostActiveLanguage: function (obj) {
    if (!obj) return "-";

    const res = _.orderBy(obj.languages, ["totalSeconds"], ["desc"])[0];
    if (res) return res.name;
    else return "-";
  },

  formatDate: function (d) {
    console.log(d);
    return new Date(d).toISOString().slice(0, 10);
  }
};
