window.console.log = function(o){
  $(function(){
    $('body').append($('<div class="alert alert-info"></div>').text(o));
  })
};
window.alert = function(o){
  $(function(){
    var alert = $('<div class="alert"><button type="button" class="close" data-dismiss="alert">×</button><span></span></div>');
    $('body').append(alert);
    alert.find('span').text(o);
    alert.find('button').click(function(){ alert.remove() })
  })
};