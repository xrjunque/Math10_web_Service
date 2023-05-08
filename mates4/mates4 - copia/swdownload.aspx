<%@ Page Language="vb" MasterPageFile="~/Site1.Master" AutoEventWireup="false"
     CodeBehind="swdownload.aspx.vb" Inherits="mates4.FreeSoftware" %>


<asp:Content ID="Content5" ContentPlaceHolderID="head" runat="server">
    <title>Software download: Windipoles, Winfonts, Math Parser &amp; Evaluator</title>
		<meta content="windipoles,passive filter,simulator,math parser,math evaluator,polynomial calculator source" name="Keywords" />
		<meta content="X.R.Junqué" name="author" />
		<meta content="Mates8 Calculator. Windipoles. Passive filter simulator. Math parser and evaluator. Polynomial calculator source code." name="title" />
		<meta content="Passive filter simulation. Math parser and evaluator in VB.NET, full source code. Polynomials' and functions' zeros, interpolation, derivatives, integrals and more."	name="description" />
        <meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1" />
		<meta content="ALL" name="robots" />
        <link rel="icon" type="image/ico" href="../favicon.ico" />
        <link rel="shortcut icon" href="../favicon.ico" /> 
    <style type="text/css" media="screen">
    .margin { margin-left: 10px; position:relative; top:0px;}
    .grPos { position:relative; left: 20px; top: 0px;}
    </style>

</asp:Content>
<asp:Content ID="Content0" ContentPlaceHolderID="scriptID" runat="server" Visible="false">
</asp:Content>
<asp:Content ID="Content1" ContentPlaceHolderID="content2L" runat="server" Visible="false">
    <asp:Table HorizontalAlign="Center" runat="server">
        <asp:TableRow >
            <asp:TableCell >
                <h2><asp:Label ID="msg" runat="server" >&nbsp;&nbsp;Software download</asp:Label></h2>
            </asp:TableCell>
        </asp:TableRow>
    </asp:Table>
</asp:Content>
    <asp:Content ID="Content3" ContentPlaceHolderID="content2R" runat="server" Visible="false">
</asp:Content>
<asp:Content ID="Content2" ContentPlaceHolderID="maincontent" runat="server">

    

    <asp:Table runat="server" HorizontalAlign="center" Font-Size="Small">
    <asp:TableRow>    
    <asp:TableCell  HorizontalAlign ="center" >
        <asp:PlaceHolder ID="PlaceHolder1"  runat="server"></asp:PlaceHolder>
    </asp:TableCell>
    </asp:TableRow>
    </asp:Table>
    <br />

</asp:Content>
