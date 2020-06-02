export default {
  numOfDays: 15,
  timeLimit: 15,

  reset: function() {
    this.numOfDays = 15;
  },

  setDays: function(d) {
    if (this.numOfDays === d) return false;
    this.numOfDays = d;
    return true;
  },

  setTimeLimit: function(d) {
    if (this.timeLimit === d) return false;
    this.timeLimit = d;
    return true;
  }
};
