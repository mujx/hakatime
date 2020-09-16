import m from "mithril";

export default {
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
