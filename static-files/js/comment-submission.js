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
    if (!form_augmented_p) {
      $.ajax({
        url: "/RPC/generate-transaction-key",
        dataType: 'json',
        success: function(tkey) {
          var salt = 0;
          var text = form.find('textarea').val().replace(/\s+/g, "");
          var submit_button = form.find(':submit');
          var status_message;
          submit_button.after('<span class="hashcash-status-message">Calculating Hashcash...</span>');
          status_message = form.find('.hashcash-status-message');
          status_message.fadeOut(0);
          status_message.fadeIn(200);
          submit_button.attr("disabled", true);
          var tryHashcash = function () {
            var tryRightNow = 1000;
            while (!acceptable_cashhash(Sha256.hash(text + ":" + tkey + ":" + salt))) {
              salt++;
              tryRightNow--;
              if (tryRightNow === 0) {
                setTimeout(tryHashcash, 0);
                return;
              }
            }
            form.prepend('<input type="hidden" name="transaction-key" value="' + tkey + '" />');
            form.prepend('<input type="hidden" name="salt"            value="' + salt + '" />');
            form_augmented_p = true;
            //submit_button.removeAttr("disabled");
            $('.comment-form').submit();
            form.submit();
          };
          setTimeout(tryHashcash, 0);
        }
      });
      return false;
    } else {
      return true;
    }
  });
  $('.spam-detection-info').html("This website uses a <a href=\"http://en.wikipedia.org/w/index.php?title=Hashcash&oldid=417692755\">Hashcash</a>-like proof-of-work system for spam detection.");
});
