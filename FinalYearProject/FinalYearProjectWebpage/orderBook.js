function constructOrderBookTable() {
	for(var i=0; i<100; i++) {
		$('#buy_order_table').append('<tr><td id="buy_order_price_'+i+'">'+i+'</td><td id="buy_order_qty_'+i+'">'+i+'</td></tr>');
		$('#sell_order_table').append('<tr><td id="sell_order_price_'+i+'">'+i+'</td><td id="sell_order_qty_'+i+'">'+i+'</td></tr>');
	}
}


var orderBookData;

function fillOrderBook() {
	if($('#buy_order_table tbody').children().length < 100) {
		constructOrderBookTable();
	}
	var Book_url = "https://api.binance.com/api/v1/depth?symbol="+tradingPair;
	  var Book_req = $.ajax({
	    url: Book_url,
	    async: false
	  });
	  //orderBookData = JSON.parse(Book_req.responseText);
          orderBookData = Book_req.responseJSON
	 
  	for(var i=0; i<100; i++) {
  		$("#buy_order_price_"+i).text(orderBookData['bids'][i][0]);
  		$("#buy_order_qty_"+i).text(orderBookData['bids'][i][1]);
  		$("#sell_order_price_"+i).text(orderBookData['asks'][i][0]);
  		$("#sell_order_qty_"+i).text(orderBookData['asks'][i][1]);
  	}
	  
}





//getOrderBookData(url);
