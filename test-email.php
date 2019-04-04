<?php
	ini_set('display_errors', 1);
	error_reporting(E_ALL);
	$from = "bins";
	$to = "kirkland.burthey@syngenta.com";
	$subject = $argv[1];
	$message = "test email functionality";
	$headers = "From:".$from;
	mail($to, $subject, $message, $headers);
	echo "Test email sent\r\n";
?>
