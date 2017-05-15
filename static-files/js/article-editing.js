// Copyright 2017, Matthias Andreas Benkard.

"use strict";

jQuery(function($) {
  $.trumbowyg.svgPath = '/journal/trumbowyg/icons.svg';
  $('#content-field').trumbowyg({
    btns: [
      ['viewHTML'],
      ['formatting'],
      'btnGrp-semantic',
      ['superscript', 'subscript'],
      ['link'],
      ['base64'],
      'btnGrp-lists',
      ['horizontalRule'],
      ['removeformat'],
      ['preformatted'],
      ['table', 'tableAddRow', 'tableAddColumn'],
      ['fullscreen']
    ],
    autogrow: true
  });
});
