export default {
  getVersionString: function () {
    if (!this.currentVersion) return "";

    return `version: git-${this.currentVersion}`;
  },
  currentVersion: "",
  noData: {
    text: "No data available",
    style: {
      fontSize: "16px",
      fontFamily: "Nunito"
    }
  },
  toolbar: {
    show: false
  },
  animations: {
    enabled: true
  },
  dateRangePresets: [7, 15, 30, 60],
  colors: [
    "#03a9f4",
    "#B0DAF1",
    "#84B082",
    "#775DD0",
    "#FF9800",
    "#A5978B",
    "#FD6A6A",
    "#69D2E7",
    "#C5D86D",
    "#3E1929",
    "#60E1E0",
    "#F7C1BB",
    "#E2C044",
    "#C4BBAF"
  ],
  datePicker: {
    showTooltip: true,
    minDays: 2,
    singleMode: false,
    mobileFriendly: true,
    autoApply: false,
    numberOfMonths: 2
  }
};
