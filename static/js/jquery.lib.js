/*******************************************************************************
 * Loosely capture live changes on an input and trigger a function for it.
 */
$.fn.livechange = function(ms,trigger){
  return $(this).each(function(){
    var self = this;
    var el = $(self);
    var last_val;
    var check = function(){
      var val = el.val();
      if(val != last_val)
        trigger.call(self);
      last_val = val;
    };
    var checker;
    var restart = function(){
      clearTimeout(checker);
      checker = setInterval(check,ms);
    };
    restart();
    el.keypress(restart).change(restart);
  });
};

/*******************************************************************************
 * Trigger keydown events with the keycode.
 */
$.fn.keycode = function(trigger){
  return $(this).each(function(){
    var self = this;
    $(this).keydown(function(e){
      trigger.call(this,e.which);
    });
  });
};
