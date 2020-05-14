import m from "mithril";

function mkFaIcon(name) {
  return `i.fas.fa-2x.text-gray.fa-${name}`;
}

export function mkSingleStatCard({ name, value, icon, textType = "primary" }) {
  return {
    view: () => {
      return m(`div.card.border-left-${textType}.shadow.h-100.py-2`, [
        m("div.card-body", [
          m("div.row.no-gutters.align-items-center", [
            m("div.col.mr-2", [
              m(
                "div.text-xs.font-weight-bold.text-primary.text-uppercase.mb-1",
                name
              ),
              m("h5.mb-0.font-weight-bold.text-gray-800", value)
            ]),
            m("div.col-auto", m(mkFaIcon(icon)))
          ])
        ])
      ]);
    }
  };
}
