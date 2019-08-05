String.prototype.replaceAll = function(str1, str2, ignore) {
    return this.replace(new RegExp(str1.replace(/([\/\,\!\\\^\$\{\}\[\]\(\)\.\*\+\?\|\<\>\-\&])/g,"\\$&"),(ignore?"gi":"g")),(typeof(str2)=="string")?str2.replace(/\$/g,"$$$$"):str2);
} 

google.charts.load('current', {'packages':['corechart']});
google.charts.setOnLoadCallback(drawChart);

function drawChart(_data) {
  var data = google.visualization.arrayToDataTable(_data, true);
  // remove volume
  data.removeColumn(5);
  var options = {
    legend:'none',
    backgroundColor: '#1F0318',
    candlestick: {
      fallingColor: { strokeWidth: 0, fill: '#a52714' }, // red
      risingColor: { strokeWidth: 0, fill: '#0f9d58' }   // green
    },
    vAxis: {
        gridlines: {
            color: 'transparent'
        }
    },
    hAxis: {
        gridlines: {
            color: 'transparent'
        }
    },
    animation: {"startup": true}
  };
  var chart = new google.visualization.CandlestickChart(document.getElementById('chart_display'));
  chart.draw(data, options);
}



function drawPredChart(_data) {
  var Preddata = google.visualization.arrayToDataTable(_data, true);

  var options = {
    legend:'none',
    backgroundColor: '#1F0318',
    candlestick: {
      fallingColor: { strokeWidth: 0, fill: '#a52714' }, // red
      risingColor: { strokeWidth: 0, fill: '#0f9d58' }   // green
    },
    vAxis: {
        gridlines: {
            color: 'transparent'
        }
    },
    hAxis: {
        gridlines: {
            color: 'transparent'
        }
    },
    animation: {"startup": true}
  };
  var Predchart = new google.visualization.CandlestickChart(document.getElementById('pred_box'));
  Predchart.draw(Preddata, options);
}


//function updateChart(_data){
//  var data = google.visualization.arrayToDataTable(_data, true);
//  chart.draw(data, options);
//}


var data_points = []

var orderBookData;

var interval = "1h"
var tradingPair = "ETHUSDT";
var numOfCandles = 30;
var url = "http://localhost:8000/?Symbol="+tradingPair+"&INTERVAL="+interval+"&STARTTIME=now&ENDTIME=later&LIMIT="+numOfCandles

function changeTradingPair(pair) {
  tradingPair = pair;
  url = "http://localhost:8000/?Symbol="+tradingPair+"&INTERVAL="+interval+"&STARTTIME=now&ENDTIME=later&LIMIT="+numOfCandles
  asset = pair.split("USDT");
  $("#trading_pair").html(asset[0]+"/USDT");
}



// draw the candlestick chart
setTimeout(function(){
  data = getChartData(url);
  
  if(data != undefined) {
    data = JSON.parse("[" + data + "]");
    
    loadChart(data);
  } 
}, 500);


// draw the Prediction candlestick chart
setTimeout(function(){
  /// request to python listener - NEEDS TO BE DONE THROUGH MESSAGING LAYER
  data = getChartData("http://localhost:8005/?Symbol="+tradingPair+"&INTERVAL="+interval+"&STARTTIME=NOW&ENDTIME=LATER&LIMIT=5000000");
  if(data != undefined) {
    console.log(data) 
  //  data = JSON.parse(data);
    loadPredChart(data);
  } 
}, 500);


// update the candlestick chart
setInterval(function() {
  data = getChartData(url);
  if(data != undefined) {
    data = JSON.parse("[" + data + "]");
    loadChart(data);
  } 
}, 10000);

// update the order books
setInterval(function() {
   fillOrderBook();
}, 500);


// update high,low, averages 
setInterval(function() {
   console.log("running")
   
   //data_points = getChartData(url);
   //data_points = JSON.parse("[" + data_points + "]");
  // data_points = JSON.parse(data_points)
  // displayLowAndHighPrices();
  // displayLatestPrice();
  // displayMovingAvgPrice();
  // fillOrderBook();
 
}, 15000000);


// load the candlestick chart for trading data
function loadChart() {
  data = JSON.parse(data);
  drawChart(data);
}

// load candlestick chart for data + predictions
function loadPredChart(data) {
  data = JSON.parse(data);
  drawPredChart(data);
}

// get data from database 
function getChartData(param) {
  var req = $.ajax({
    url: param,
    async: false
  });
  //console.log(req.responseText);
  return req.responseText;
}


function getHighestPrice() {
  max = 0;
  for(var i=0; i<data_points.length; i++) {
    if(data_points[i][4] > max) {
      max = data_points[i][4];
    } 
  }
  $('#high_price').html('$'+max);
  return max;
}


function getLowestPrice() {
  low = getHighestPrice();
  for(var i=0; i<data_points.length; i++) {
    if(data_points[i][1] < low) {
      low = data_points[i][1];
    } 
  }
  $('#low_price').html('$'+low);
  return low;
}

function displayLowAndHighPrices() {
  getLowestPrice();
}


function displayLatestPrice() {
  $("#current_price").html('$'+data_points[data_points.length-1][3]);
}


function displayMovingAvgPrice() {
  sum = 0;
  for(var i=0; i<data_points.length; i++) {
    sum += data_points[i][3]
  }
  avg = sum / data_points.length;
  $("#moving_avg").html('$'+Number(avg).toFixed(2));
  return avg;
}


function emaSmoothing() {
  return (2/(num_of_candles+1));
}


function displayExpMovingAvgPrice() {
  var s = emaSmoothing();
  var d = num_of_candles;
  var ema = 10;
}

 

function updateCandles() {
  var val = $("#num_of_candles").val();
  if(isPositiveInteger(val)) {
    if(val < 10) {
      alert("The minimum number of candles allowed is 10!");_
      return;
    }  else {
      numOfCandles = $("#num_of_candles").val();
      url = "http://localhost:8000/?Symbol="+tradingPair+"&INTERVAL="+interval+"&STARTTIME=now&ENDTIME=later&LIMIT="+numOfCandles
    }
  } else {
    alert("Please enter a positive integer!");
  }
}


function updateInterval() {
  interval = $( "#slected_intervals" ).val();
  url = "http://localhost:8000/?Symbol="+tradingPair+"&INTERVAL="+interval+"&STARTTIME=now&ENDTIME=later&LIMIT="+numOfCandles
}


function isPositiveInteger(s) {
    return /^\+?[1-9][\d]*$/.test(s);
}
