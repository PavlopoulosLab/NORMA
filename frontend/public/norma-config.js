// NORMA page settings for static hosting or opening norma.html from disk.
// When NORMA runs with server.py, the server replaces this file with the
// settings from norma.config.json. Every field is optional.
window.NORMA_CONFIG = {
  mode: "local",
  site: {
    name: "NORMA",
    institution: "",
    contactName: "Pavlopoulos Lab",
    contactEmail: "",
    contactUrl: "https://github.com/PavlopoulosLab/NORMA/issues",
    sourceUrl: "https://github.com/PavlopoulosLab/NORMA",
    licenceName: "MIT License",
    licenceUrl: "https://opensource.org/licenses/MIT",
    privacyUrl: "",
    imprintUrl: "",
    maintainedUntil: "",
    notice: "",
    testedBrowsers: []
  },
  app: { maxNodes: 10000, theme: "white", startTab: "welcome", cdnFallback: true }
};
