import m from "mithril";

export default {
  mkCardContainer: function (title, body, menuActions) {
    const optionsComponent = m("div.dropdown.no-arrow.ml-auto", [
      m(
        "a.dropdown-toggle[data-toggle=dropdown][aria-haspopup=true][aria-expanded=false]",
        {
          role: "button",
          id: "dropdownMenuLink"
        },
        m("i.fas.fa-ellipsis-v.fa-sm.fa-fw.text-gray-400")
      ),

      m(
        "div.dropdown-menu.dropdown-menu-right.shadow.animated--fade-in[aria-labelledby=dropdownMenuLink][x-placement=bottom-end]",
        {
          style:
            "position: absolute; will-change: transform; top: 0px; left: 0px; transform: translate3d(17px, 19px, 0px);"
        },
        menuActions
      )
    ]);

    return m("div.card.shadow.mb-4", [
      m(
        "div.card-header.py-3.d-flex.flex-row.align-items-center.justify-contents-between",
        [
          m("h6.m-0.font-weight-bold.text-primary", title),
          menuActions ? optionsComponent : null
        ]
      ),
      m("div.card-body", body)
    ]);
  }
};
