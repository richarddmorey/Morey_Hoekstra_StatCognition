
function transformFunc(e){
	return 1 - Math.pow(e, colorTransformPower);
}

function identityFunc(e){
	return e;
}

function getColor(evidence, r, g, b){
	r = (typeof r !== 'undefined') ?  r : 0;
	g = (typeof g !== 'undefined') ?  g : 0;
	b = (typeof b !== 'undefined') ?  b : 0;

	var intensity = evidence;
    var color = "rgba(" + r + "," + g + "," + b + ", " + intensity +")";
    return color;
}

function getEvidence(d, z, n, idx, transformFunc){
	transformFunc = (typeof transformFunc !== 'function') ?  identityFunc : transformFunc;
	var p = std_n_cdf( z.nextGaussian() + d * Math.sqrt(n[idx]/2) );
	var sgn = (p > 0.5) * 2 - 1;
	p = (p < 0.5) ? p * 2 : (1 - p) * 2 ;
  return { sgn: sgn, ev: transformFunc( p ) };
}

function point(x, y, r, w, h, red, green, blue, alpha, canvas, fill, stroke){
  canvas.beginPath();
  canvas.arc(x * w, y * h, r * (w + h)/2, 0, 2 * Math.PI, true);
  canvas.fillStyle = "rgba("+red+","+green+","+blue+","+alpha+")";
  canvas.strokeStyle = "rgba(0,0,0,.3)";
  canvas.lineWidth=1;
  if(fill) canvas.fill();
  if(stroke) canvas.stroke();
}

function plot_new_point(d, z, n_idx, n, c, h, w){
  var ctx = c.getContext("2d");

  var n_ran_idx = 0;
  var n_unif = 0;

  var ev = 0;

  n_unif = (n_idx + 1) / (n.length + 1);
  ev = getEvidence(d, z, n, n_idx, transformFunc);
  blue = 0;
  red = 255;
  green = 255 - red;
  point(0.5 + ev.sgn*ev.ev/2, 1 - n_unif, 0.01, w, h, red, green, blue, ev.ev, ctx, true, true);
  return { ev: ev.ev, sgn: ev.sgn, n_idx: n_idx, n: n[n_idx], d: d, time: Date.now() };
}


// From https://filosophy.org/code/normal-distributed-random-values-in-javascript-using-the-ziggurat-algorithm/
function Ziggurat(){
  var jsr = 123456789;

  var wn = Array(128);
  var fn = Array(128);
  var kn = Array(128);

  function RNOR(){
    var hz = SHR3();
    var iz = hz & 127;
    return (Math.abs(hz) < kn[iz]) ? hz * wn[iz] : nfix(hz, iz);
  }

  this.nextGaussian = function(){
    return RNOR();
  };

  function nfix(hz, iz){
    var r = 3.442619855899;
    var r1 = 1.0 / r;
    var x;
    var y;
    while(true){
      x = hz * wn[iz];
      if( iz === 0 ){
        x = (-Math.log(UNI()) * r1); 
        y = -Math.log(UNI());
        while( y + y < x * x){
          x = (-Math.log(UNI()) * r1); 
          y = -Math.log(UNI());
        }
        return ( hz > 0 ) ? r+x : -r-x;
      }

      if( fn[iz] + UNI() * (fn[iz-1] - fn[iz]) < Math.exp(-0.5 * x * x) ){
          return x;
      }
      hz = SHR3();
      iz = hz & 127;

      if( Math.abs(hz) < kn[iz]){
        return (hz * wn[iz]);
      }
    }
  }

  function SHR3(){
    var jz = jsr;
    var jzr = jsr;
    jzr ^= (jzr << 13);
    jzr ^= (jzr >>> 17);
    jzr ^= (jzr << 5);
    jsr = jzr;
    return (jz+jzr) | 0;
  }

  function UNI(){
    return 0.5 * (1 + SHR3() / -Math.pow(2,31));
  }

  function zigset(){
    // seed generator based on current time
    jsr ^= new Date().getTime();

    var m1 = 2147483648.0;
    var dn = 3.442619855899;
    var tn = dn;
    var vn = 9.91256303526217e-3;
    
    var q = vn / Math.exp(-0.5 * dn * dn);
    kn[0] = Math.floor((dn/q)*m1);
    kn[1] = 0;

    wn[0] = q / m1;
    wn[127] = dn / m1;

    fn[0] = 1.0;
    fn[127] = Math.exp(-0.5 * dn * dn);

    for(var i = 126; i >= 1; i--){
      dn = Math.sqrt(-2.0 * Math.log( vn / dn + Math.exp( -0.5 * dn * dn)));
      kn[i+1] = Math.floor((dn/tn)*m1);
      tn = dn;
      fn[i] = Math.exp(-0.5 * dn * dn);
      wn[i] = dn / m1;
    }
  }
  zigset();
}


function cdf(x, mean, variance) {
  return 0.5 * (1 + erf((x - mean) / (Math.sqrt(2 * variance))));
}

function erf(x) {
  // save the sign of x
  var sign = (x >= 0) ? 1 : -1;
  x = Math.abs(x);

  // constants
  var a1 =  0.254829592;
  var a2 = -0.284496736;
  var a3 =  1.421413741;
  var a4 = -1.453152027;
  var a5 =  1.061405429;
  var p  =  0.3275911;

  // A&S formula 7.1.26
  var t = 1.0/(1.0 + p*x);
  var y = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * Math.exp(-x * x);
  return sign * y; // erf(-x) = -erf(x);
}

function std_n_cdf(x) {
  return cdf(x, 0, 1);
}
