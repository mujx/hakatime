import m from "mithril";

const timeoutOptions = [5, 10, 15, 20, 30];

export default {
  timeoutButton: function (limit) {
    return m(
      "button.btn.btn-primary.dropdown-toggle.shadow-sm[data-toggle='dropdown'][aria-haspopup='true'][aria-expanded='false']",
      {
        type: "button",
        id: "dropdownMenuButton"
      },
      [
        m("i.fas.fa-clock.fa-md.text-white-50.mr-2[data-toggle='tooltip']", {
          title:
            "Maximum time allowed between heartbeats when calculating your total coding activity. It combines heartbeats with more or less accuracy."
        }),
        m("small", `Timeout (${limit} mins)`)
      ]
    );
  },
  timeoutDropdown: function (cb) {
    return m(
      'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
      timeoutOptions.map(r => {
        return m(
          "a.btn.dropdown-item",
          {
            onclick: e => {
              e.redraw = false;
              cb(r);
            }
          },
          `${r} mins`
        );
      })
    );
  },
  githubLink: function () {
    return m(
      "p.mt-5.mb-3.text-muted.text-center",
      m(
        "a",
        { href: "https://github.com/mujx/hakatime", target: "_blank" },
        "Hakatime"
      )
    );
  }
};
