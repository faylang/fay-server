window.console.log = function(o){
  $(function(){
    $('body').append($('<div class="alert alert-info"></div>').text(o));
  })
};