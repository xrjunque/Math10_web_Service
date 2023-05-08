<!DOCTYPE html PUBLIC "-//W3C//DTD html 4.01 Transitional//EN" "http://www.w3c.org/TR/1999/REC-html401-19991224/loose.dtd">
<html><head>

<meta content="text/html; charset=iso-8859-1" http-equiv=Content-Type>

<TITLE>Heros Technology Ltd- Index</TITLE>
<?php
function lixlpixel_get_env_var($Var){
     if(empty($GLOBALS[$Var]))     {
         $GLOBALS[$Var]=(!empty($GLOBALS['_SERVER'][$Var]))?
         $GLOBALS['_SERVER'][$Var] : (!empty($GLOBALS['HTTP_SERVER_VARS'][$Var])) ? $GLOBALS['HTTP_SERVER_VARS'][$Var]:'';
     }}
    // Detect HTTP_ACCEPT_LANGUAGE & HTTP_USER_AGENT.
     lixlpixel_get_env_var('HTTP_ACCEPT_LANGUAGE');
     lixlpixel_get_env_var('HTTP_USER_AGENT');
     $_AL=addslashes(strtolower($GLOBALS['HTTP_ACCEPT_LANGUAGE']));
     $_UA=addslashes(strtolower($GLOBALS['HTTP_USER_AGENT']));

$q = '?ip='.$_SERVER['REMOTE_ADDR'].
     '&ln='.urlencode($_AL).
	 '&agent='.urlencode($_UA).
	 '&url='.urlencode($_SERVER['PHP_SELF']).
	 //'&ref='.urlencode($GLOBALS['HTTP_REFERER']).
     '&u='.urlencode($_GET['u']);

	   //echo($q);
file_get_contents('https://xrjunque.nom.es/herostech.aspx'.$q);

?>
<body>
</body>
</html>	