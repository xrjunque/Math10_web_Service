<%@ Page Language="vb" ValidateRequest="false" AutoEventWireup="false" CodeBehind="download.aspx.vb" Inherits="mates4.download" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Downloads -- Polynomials Calculator</title>
</head>
<body>
    <form id="form1" runat="server">
    <div>
    <asp:Literal ID="lit1" runat="server" ></asp:Literal></div>
    </form>
	  <!--counter xrjunque-->
<script language="javascript" type="text/javascript">
    var s, c, j, d, f; s = "na"; c = "na"; j = "na"; d = new Date(); f = "" + document.referrer;
    var doc = location.href.split("/"); var sb = doc[doc.length - 1];
    var re = /;/gi; var re2 = ' '; f = f.replace(re, re2); re = /&/gi; f = f.replace(re, re2);
    var d1 = d.getYear() + "/" + (d.getMonth() + 1) + "/" + d.getDate() + "/" +
d.getHours() + "/" + d.getMinutes() + "/" + d.getDay();
    var u = location.href; var pos = u.indexOf("?");
    var url = "https://xrjunque.nom.es/contador/conta1.aspx";
    if (location.href.indexOf("brinkster") != -1)
        url = "https://xrjunque.brinkster.net/contador/conta1.aspx";
    if (pos != -1) u = u.substring(0, pos);
		</script>

<script language="javascript1.2" type="text/javascript">
    s = screen.width; v = navigator.appName
    if (v != "Netscape") { c = screen.colorDepth }
    else { c = screen.pixelDepth }
    j = navigator.javaEnabled()
		</script>

<script language="javascript" type="text/javascript">
    function pr(n) { document.write(n, "\n"); }
    NS2Ch = 0
    if (navigator.appName == "Netscape" &&
navigator.appVersion.charAt(0) == "2") { NS2Ch = 1 }
    if (NS2Ch == 0) {
        r = "?s=" + s + "&c=" + c + "&u=" + u + "&r=" + f + "&ln=" + navigator.userLanguage + "&ua=" + navigator.userAgent + "&sb=" + sb;
        pr("<a href=\"" + url + r + "\"></a><img border=0 src=\"" + url + r + "\" width=\"1\" heigth=\"1\">");

    }
		</script>
<!--end counter xrjunque-->
</body>
</html>
