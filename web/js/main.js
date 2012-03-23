$(function () {
function printQuotes(quotes) {
	for(quoteIndex in quotes) {
		var quote = quotes[quoteIndex];
		$('body').append("<div>Author :"+quote.author+"</div>");
	}
}

function printError(error) {
	console.log(error)
}

$.getJSON('getquotes.php',{coords : ({x : 1, y : 0}, {x : 1, y : 1}) }, printQuotes, printError);
})
