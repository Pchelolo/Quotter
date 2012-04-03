$(function () {
var width = $(document).width(), height = $(document).height(),
	x_num = Math.round(width/310), y_num = Math.round(height/160),
	offset_x = width/2-155, offset_y = height/2-75;
var X_SIZE = 310, Y_SIZE = 160

var field = (function() {
	var grid = [],
		gridChangeListeners = [];

	var fireGrindChangeEvent = function(x, y, newQuote) {
		var index;
		for(index = 0; index < gridChangeListeners.length; index++) {
			gridChangeListeners[index](x, y, newQuote);
		}
	}

	return {
		get : function(x, y) {return grid[x][y];},
		set : function(x, y, quote) {
			if(grid[x] === undefined) grid[x] = [];
			if(grid[x][y] !== undefined) return false;
			else {
				grid[x][y] = quote;
				fireGrindChangeEvent(x, y, quote)
				return true;
			}
		},
		addGridChangeListener : function(gridChangeListener) {
			gridChangeListeners.push(gridChangeListener);
		}
	}
})();

function updateValues() {
	//offset_x -= (width - $(document).width())/2;
	//offset_y -= (height - $(document).height())/2;
	width = $(document).width();
    height = $(document).height();
	x_num = Math.round(width/310);
	y_num = Math.round(height/160);
}

function onChangeOffset() {
	var index, quotes = $(".quote");
	for(index = 0; index < quotes.length; index++) {
		var quote = quotes[index];
		$(quote).css({"left" : $(quote).attr("data-x")*X_SIZE+offset_x, "top" : $(quote).attr("data-y")*Y_SIZE+offset_y});
	}
}

function onResize() {
	updateValues()
}

function onDrag(event) {
	if(onDrag.prevX !== undefined) {
		offset_x += event.pageX - onDrag.prevX;
		offset_y += event.pageY - onDrag.prevY;
		onChangeOffset();
	}
	onDrag.prevX = event.pageX;
	onDrag.prevY = event.pageY;
}

function init() {
	var x, y;
	field.addGridChangeListener(printQuote);
	$(window).resize(onResize);
	$("#layout").mousedown(function() {
		$(this).bind("mousemove", onDrag);
	});
	$("#layout").mouseup(function() {
		onDrag.prevX = undefined;
		onDrag.prevY = undefined;
		$(this).unbind("mousemove");
	})

	var coordsList = [];
	for(x = -x_num/2-1; x<=x_num/2+1; x++) {
		for(y = -y_num/2-1; y<=y_num/2+1; y++) {
			coordsList.push({"x" : x, "y" : y});
		}
	}
	addQuotes(coordsList)
}

function printQuote(x, y, quote) {
	var quoteDisplay = $("<div>")
							.attr("class", "quote")
							.attr("data-x", x).attr("data-y", y)
							.text(x+"    "+y+"   "+quote.quote)
							.css({"left" : x*X_SIZE+offset_x+"px", "top" : y*Y_SIZE+offset_y+"px"});
	$('#layout').append(quoteDisplay);
}


function printError(error) {
	console.log(error)
}

function addQuotes(coordsList) {
	$.getJSON('getquotes.php',{coords : coordsList }, function(resultList) {
		var index;
		for(index = 0; index < resultList.length; index++) {
			field.set(resultList[index].x, resultList[index].y, resultList[index].quote);
		}
	}, printError);
}

init();
})
