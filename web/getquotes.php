<?php

/*
if( !isset($_GET['coords'])  ) {
	$return['error'] = true;
	$return['msg'] = "Wrong parameters";
	die();
}
*/
try {
	$conn = new Mongo('localhost');
	$db = $conn->quotter;
	$collection = $db->quotes;
	$quotes = array();
	foreach($_GET['coords'] as $coord) {
		if( !isset($coord['x']) || !is_numeric($coord['x']) || !isset($coord['y']) || !is_numeric($coord['y']) ) {
			$return['error'] = true;
			$return['msg'] = "Wrong parameters";
			die();
		}
		$quote = $collection->findOne( array('coord.x' => intval($coord['x']), 'coord.y' => intval($coord['y'])) );
		if( $quote !== NULL ) {
			array_push($quotes, $quote);
		}
	 	
	}
	echo json_encode($quotes);
	$conn->close();

} catch (MongoConnectionException $e) {
	$return['error'] = true;
	$return['msg'] = "Error connecting to MongoDB server";
	die();
} catch (MongoException $e) {
	$return['error'] = true;
	$return['msg'] = 'Error: ' . $e->getMessage();
	die();
}
?>
