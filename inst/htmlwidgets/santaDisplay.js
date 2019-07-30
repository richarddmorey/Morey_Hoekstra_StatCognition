HTMLWidgets.widget({

  name: 'santaDisplay',
  type: 'output',

  initialize: function(el, width, height)
  {

  },


  renderValue: function(el, x, obj) {

        var ms_per_update = 200;

        var interval = 0;

        function increment_slider(){
          var status = $( "#ani_play_button" ).data("status");

          if(status === undefined){
            clearInterval(interval);
            interval = 0;
            return;
          }

          var max = $( "#ani_slider_container" )
            .slider( "option", "max");
          var m = $( "#ani_slider_container" )
            .slider( "option", "value" );

          if(max>m){
            $( "#ani_slider_container" )
              .slider("value", m+1 )
              .trigger("slidechange");
          }else if( (max == m) ){
            clearInterval(interval);
            interval = 0;
            $( "#ani_play_button" ).removeData("status");
            $( "#ani_play_button" ).removeAttr("data-status");
            $( "#ani_play_button" ).html("▶");
          }
        }

        function reset_play_button(el){
          var status = $( el ).data("status");

          if(status === undefined){
            $( el ).data("status", "playing");
            interval = setInterval(increment_slider, ms_per_update);
            $( "#ani_play_button" ).html("■");
          }else{
            $( el ).removeData("status");
            $( el ).removeAttr("data-status");
            $( "#ani_play_button" ).html("▶");
            clearInterval(interval);
            interval = 0;
          }
        }

        x = JSON.parse(x);
        el.innerHTML = x.snippet;

        var df = x.df;
        var n = df.id.length;
        var type="";
        var ev = 0;
        var n_idx = 0;
        var n_unif = 0;
        var sgn = 0;

        var c = $(".plot_canvas")[0];
        var ctx = c.getContext("2d");
        var h = c.offsetHeight;
				var w = c.offsetWidth;

        var blue = 0;
        red = 255;
        var green = 255 - red;

        $( "#ani_slider_container" )
        .css({ width: "75%"} )
        .slider({
          min: 0,
          max: n
        }).on( "slidechange", function( event, ui ) {
          ctx.clearRect(0, 0, w, h);
          var n_e = 0;
          var n_n = 0;
          var m = $( "#ani_slider_container" )
            .slider( "option", "value" );
          for(var i=0 ; i<m ; i++){
            type = df.type[i];
            ev = df.ev[i];
            n_idx = df.n_idx[i];
            n_unif = (n_idx + 1) / (20 + 1);
            sgn = df.sgn[i];
            if(type == "expt"){
              point(0.5 + sgn*ev/2, 1 - n_unif, 0.01,
              w, h, red, green, blue, ev, ctx, true, true);
              n_e++;
            }else{
              drawX(0.5 + sgn*ev/2, 1 - n_unif, 0.01,
              w, h, 0, 0, 0, 0.3, ctx);
              n_n++;
            }
          }
          $("#num_null").text(n_n);
          $("#num_expt").text(n_e);
        });

        $("#ani_reset_button").click(function(event, ui){
          clearInterval(interval);
          interval = 0;
          $( "#ani_slider_container" )
            .slider("value", 0 )
            .trigger("slidechange");
          $( "#ani_play_button" ).html("▶");
          $( "#ani_play_button" ).removeData("status");
          $( "#ani_play_button" ).removeAttr("data-status");
        });

        $("#ani_play_button").click(function(event, ui){
          reset_play_button( this );
        });


      },

      resize: function(el, width, height, obj) {
        // change
      }

});
