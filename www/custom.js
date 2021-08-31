$(document).ready(
  function(){
    if (window.location.hash) {
      var hash_val = '#' + window.location.hash.replace('#','');
      $('a', $('.sidebar')).each(function() {
          if(this.getAttribute('href') == hash_val) {
            this.click();
          };
      });
    }
})