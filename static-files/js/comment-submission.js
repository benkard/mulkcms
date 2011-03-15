// Copyright 2011, Matthias Andreas Benkard.

"use strict";

jQuery(function($) {
  var acceptable_cashhash = function (hash) {
    return (hash[0] === '0' &&
            hash[1] === '0' &&
            hash[2] === '0' &&
            hash[3] === '0');
  };

  var form_augmented_p = false;
  $('.comment-form').submit(function() {
    var form = $(this);
    $.ajax({
      url: "/RPC/generate-transaction-key",
      dataType: 'json',
      success: function(tkey) {
        if (!form_augmented_p) {
          form.find(':submit').attr("disabled", true);
          var salt = 0;
          var text = form.find('textarea');
          while (!acceptable_cashhash(Sha256.hash(text + ":" + tkey + ":" + salt))) {
            salt++;
          }
          form.prepend('<input type="hidden" name="transaction-key" value="' + tkey + '" />');
          form.prepend('<input type="hidden" name="salt"            value="' + salt + '" />');
          form_augmented_p = true;
          form.submit();
          return false;
        } else {
          return true;
        }
      }
    });
    return false;
  });
  $('.spam-detection-method').text("Hashcash");
  $('.irrelevant-for-hashcash').text('');
});
