function dateDiff(d1, d2) {
  return Math.ceil(Math.abs(d1 - d2) / (1000 * 60 * 60 * 24));
}

const TimeRange = {
  numOfDays: 15,
  timeLimit: 15,

  d1: null,
  d2: null,

  dateRange: () => {
    if (!TimeRange.d1 || !TimeRange.d2) return TimeRange.numOfDays;

    return dateDiff(TimeRange.d1, TimeRange.d2);
  },

  start: () => {
    if (!TimeRange.d1) {
      TimeRange.d1 = new Date();
      TimeRange.d1.setDate(TimeRange.d1.getDate() - TimeRange.numOfDays);
    }

    return TimeRange.d1;
  },

  end: () => {
    if (!TimeRange.d2) {
      TimeRange.d2 = new Date();
    }

    return TimeRange.d2;
  },

  reset: function () {
    TimeRange.numOfDays = 15;
    TimeRange.d1 = null;
    TimeRange.d2 = null;
  },

  setDaysFromToday: n => {
    const start = new Date();
    const today = new Date();
    start.setDate(start.getDate() - n);

    if (TimeRange.d1 === start && TimeRange.d2 === today) return false;

    TimeRange.d1 = start;
    TimeRange.d2 = today;

    TimeRange.numOfDays = dateDiff(TimeRange.d1, TimeRange.d2);

    return true;
  },

  setDays: function (d1, d2) {
    if (TimeRange.d1 === d1 && TimeRange.d2 === d2) return false;

    TimeRange.d1 = d1;
    TimeRange.d2 = d2;

    TimeRange.numOfDays = dateDiff(TimeRange.d1, TimeRange.d2);

    return true;
  },

  setTimeLimit: function (d) {
    if (TimeRange.timeLimit === d) return false;
    TimeRange.timeLimit = d;
    return true;
  }
};

export default TimeRange;
