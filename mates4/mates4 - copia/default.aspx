<%@ Page Language="vb" AutoEventWireup="false" MasterPageFile="~/Site1.Master"
     CodeBehind="default.aspx.vb" Inherits="mates4._Default" Title="Home -- Polynomials Calculator"  %>


	<asp:Content ID="Content1" ContentPlaceHolderID="head" Runat="Server">
    </asp:Content>

	<asp:Content ID="content2" ContentPlaceHolderID="content2L" Runat="Server">

            <asp:Table ID="Table1" runat="server" HorizontalAlign="center">
            
                <asp:TableRow>
                    <asp:TableCell HorizontalAlign="center">

                                <h3><asp:Literal ID="Literal1" runat="server">
                                    Free Online Polynomials and Scientific Calculator.
                                </asp:Literal></h3>
                        <asp:Label ID="lblUpdated" runat="server" Text="Label" Font-Names="Arial" Font-Size="Small"></asp:Label>

                    </asp:TableCell>
                </asp:TableRow>
            </asp:Table> 

	</asp:Content>

	<asp:Content ID="Content3" ContentPlaceHolderID="maincontent" Runat="Server">
                                <table>
                                <thead></thead>
                                <tbody>
                   <tr><td><h3 style="margin-left:100px;"><a href="mobile.aspx">Mobile version</a></h3></td></tr>
                <tr><td style="width:600px; text-align:left;">
                <h4 style="font-family:Arial; font-size:medium;position:relative; margin-left:40px">
                <asp:label id="Label1" runat="server" ForeColor="RoyalBlue" Font-Names="Arial" Font-Size="Medium"
								Font-Bold="True">Welcome / Bienvenido / Benvingut</asp:label></h4>
                </td></tr>
                <tr><td style="width:600px; text-align:left">
    	            <ul style="line-height:30px; font-family:Verdana; font-size:medium ">
                        <li><asp:HyperLink id="HyperLink12" runat="server" Font-Names="Arial" ForeColor="RoyalBlue"
                        Font-Bold="True" Font-Underline="False">A CAS calculator web service.</asp:HyperLink></li>

                        <li><asp:HyperLink id="HyperLink11" runat="server" Font-Names="Arial" ForeColor="RoyalBlue"
                        Font-Bold="True" Font-Underline="False">MathML to/from String (Plain text) Converter</asp:HyperLink></li>
			            
                        <li><asp:HyperLink id="HyperLink4" runat="server" Font-Names="Arial" ForeColor="RoyalBlue"
                        Font-Bold="True" Font-Underline="False">Link to CodeProject Article</asp:HyperLink></li>
			            
                        <li><asp:HyperLink id="HyperLink5" runat="server" Font-Names="Arial" ForeColor="RoyalBlue"
                        Font-Bold="True" Font-Underline="False">Links</asp:HyperLink></li>

			            <li><asp:HyperLink id="HyperLink3" runat="server" Font-Names="Arial" ForeColor="RoyalBlue"
                        Font-Bold="True" Font-Underline="False">Contact</asp:HyperLink></li>

			            <li><asp:HyperLink id="HyperLink7" runat="server" Font-Names="Arial" ForeColor="RoyalBlue"
                        Font-Bold="True" Font-Underline="False">Mates8v8.4 versions Programming Overview Tutorial</asp:HyperLink></li>

			            <li><asp:HyperLink id="HyperLink7b" runat="server" Font-Names="Arial" ForeColor="RoyalBlue"
                        Font-Bold="True" Font-Underline="False">Mates8v8.4 Programming Overview Tutorial (Daniweb)</asp:HyperLink></li>

			            <li><asp:HyperLink id="HyperLink8" runat="server" Font-Names="Arial" ForeColor="RoyalBlue"
                        Font-Bold="True" Font-Underline="False">Check the time in every time zone</asp:HyperLink></li>

			            <li><asp:HyperLink id="HyperLink9" runat="server" Font-Names="Arial" ForeColor="RoyalBlue"
                        Font-Bold="True" Font-Underline="False">Convert expression to RPN (Reverse Polish Notation)</asp:HyperLink></li>

			            <li><asp:HyperLink id="HyperLink10" runat="server" Font-Names="Arial" ForeColor="RoyalBlue"
                        Font-Bold="True" Font-Underline="False">Mobile version</asp:HyperLink></li>
		            </ul>

            </td></tr></tbody></table>
        <!-- img src="precis/images/christmas2.png" width ="600px" style="margin-left:30px" /><br />
        <img src="precis/images/new-year-cards-merry-christmas-happy-new-year.jpg" width ="600px" style="margin-left:30px; align-content:center" / -->


	</asp:Content> 

		
