// Copyright 2017, 2018, Matthias Andreas Benkard.

"use strict";

document.addEventListener('DOMContentLoaded', function() {
  var $ = jQuery;
  $(document.getElementById('content-field')).wymeditor({
    skin: "seamless",
    iframeBasePath: "/journal/wymeditor/iframe/pretty/",
    postInit: function(wym) {
      wym.fullscreen();
    }
  });
});
