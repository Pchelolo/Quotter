<!doctype html><html itemscope itemtype="http://schema.org/WebPage"><head><meta http-equiv="content-type" content="text/html; charset=UTF-8"></head><body>
<?php
try {
	$conn = new Mongo('localhost');
	$db = $conn->quotter;
	$collection = $db->quotes;
	$cursor = $collection->find();

	echo $cursor->count() . ' document(s) found. <br/>'; 
	
	foreach ($cursor as $obj) {
		echo 'Author: ' . $obj['author'] . '<br/>';
		echo 'Quote: ' . $obj['quote'] . '<br/>';
		$tags = '';
		foreach($obj['tags'] as $tag) {
			$tags = $tags . $tag . ', ';
		}
		echo 'Tags: ' . $tags . '<br/>';
		$coord = $obj['coord'];
		echo 'Coords: x '.$coord['x'].' y '.$coord['y'].'<br/>';
		echo '<br/>';
	 }
	 $conn->close();
} catch (MongoConnectionException $e) {
	 die('Error connecting to MongoDB server');
} catch (MongoException $e) {
	 die('Error: ' . $e->getMessage());
}

?>
</body></html>
