export default {
  numOfDays: 15,

  reset: function() {
    this.numOfDays = 15;
  },

  setDays: function(d) {
    if (this.numOfDays === d) return false;
    this.numOfDays = d;
    return true;
  }
};
