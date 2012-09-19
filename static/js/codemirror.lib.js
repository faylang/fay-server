function CodeMirrorLiveChange(mirror,n,b,func){
  var lastvalue = '';
  var t;
  setInterval(function(){
    value = mirror.getValue();
    if(lastvalue!=value) {
      lastvalue = value;
      clearTimeout(t);
      t = setTimeout(func,b);
    }
  },n);
}