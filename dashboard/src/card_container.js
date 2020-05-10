import m from "mithril";

export default {
  mkCardContainer: function(title, body) {
    return m("div.card.shadow.mb-4", [
      m(
        "div.card-header.py-3.d-flex.flex-row.align-items-center.justify-contents-between",
        [m("h6.m-0.font-weight-bold.text-primary", title)]
      ),
      m("div.card-body", body)
    ]);
  }
};
