window.onload = function () {
  //<editor-fold desc="Changeable Configuration Block">
  let hostpath = window.location.origin;
  let openApiPath = "/openapi";
  if (hostpath === "https://integ.studio.juspay.in")
    openApiPath = "/api/v2/openapi";
  // the following lines will be replaced by docker/configurator, when it runs in a docker-container
  window.ui = SwaggerUIBundle({
    url: hostpath + openApiPath,
    dom_id: '#swagger-ui',
    deepLinking: true,
    presets: [
      SwaggerUIBundle.presets.apis,
      SwaggerUIStandalonePreset
    ],
    plugins: [
      SwaggerUIBundle.plugins.DownloadUrl
    ],
    layout: "StandaloneLayout"
  });

  //</editor-fold>
};
