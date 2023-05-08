<%@ Page Language="vb" AutoEventWireup="false" MasterPageFile="~/Site1.Master"
     CodeBehind="links.aspx.vb" Inherits="mates4.Links" 
Title="SW Download -- Polynomials Calculator" %>


<asp:Content ID="Content1" ContentPlaceHolderID="head" runat="server">
            <style type="text/css" media="screen">
        A:link {text-decoration: none;
                color:Navy ;
                }
        A:visited {text-decoration: none;
                color:Navy;
                   }
        A:active {text-decoration: none;
                color:Navy;
                  }
        A:hover {text-decoration: underline; color: red;
                 }    
    .cuadrBg
    {
    	background-image:Url(/images/cuadr.bmp);
        background-repeat:repeat;
        width:720px;
        border: 2px solid #c0c0c0;
        /* Safari 3-4, iOS 1-3.2, Android 1.6- */
        -webkit-border-radius: 12px; 
        /* Firefox 1-3.6 */
        -moz-border-radius: 12px; 
        /* Opera 10.5, IE 9, Safari 5, Chrome, Firefox 4, iOS 4, Android 2.1+ */
        border-radius: 12px;      
    }
     
    .redLn
    {
    	background-image:Url(/images/cuadr2.jpg);
        background-repeat:repeat-y;
        -moz-border-radius: 15px;
        border-radius: 15px;
    }
    </style>

</asp:Content>
<asp:Content ID="Content2" ContentPlaceHolderID="content2R" Visible="false" runat="server">

</asp:Content>
<asp:Content ID="Content3" ContentPlaceHolderID="maincontent" runat="server">
        <asp:Table runat="server">
        <asp:TableRow>
        <asp:TableCell>
        <asp:Label runat="server">
                <h4 style="font-family:Arial; font-size:medium;position:relative; margin-left:40px">
                Related links</h4>
        </asp:Label>
                <asp:BulletedList ID="BulletedList1" runat="server" DisplayMode="HyperLink">
                <asp:ListItem Value="https://xrjunque.nom.es/swdownload.aspx#mates8"
                           text="Math parser and evaluator example code (source code in VB.Net)" />
                <asp:ListItem Value="http://onlinemathdegrees.org/who-hires-math-majors/"
                         text="Who Hires Math Majors? The Top 10 Employers." />
                <asp:ListItem Value="http://www.sosmath.com/algebra/factor/fac04/fac04.html"
                        text="The Fundamental Theorem of Algebra" />
                <asp:ListItem Value="http://www.uv.es/~diaz/mn/node17.html"
                        text="C&aacute;lculo de ra&iacute;ces de ecuaciones" />
                <asp:ListItem Value="http://en.wikipedia.org/wiki/Durand-Kerner_method"
                        text="Durand-Kerner method" />
                <asp:ListItem Value="http://de.wikipedia.org/wiki/Horner-Schema"
                        text="Horner-Schema" />
                <asp:ListItem Value="http://brownmath.com/alge/polysol.htm"
                        text="Solving Polynomial Equations" />
                <asp:ListItem Value="http://svn.assembla.com/svn/mna/tps/tp4ej5Paulina.pdf"
                        text="Interpolación de la función módulo mediante polinomios de Lagrange" />
                <asp:ListItem Value="http://www.hvks.com/Numerical/websolver.php" 
                        text="Polynomial Web Solver" />
                </asp:BulletedList>
        </asp:TableCell>
        </asp:TableRow>
        <asp:TableRow>
        <asp:TableCell>
        <asp:Literal runat="server">
    	            <ul style="line-height:30px; font-family:Verdana; font-size:medium ">
                        <li><a href="http://www.daniweb.com/profiles/815196/Xavier-Junqu" rel="me">
                            <img src="http://www.daniweb.com/stats/badge/815196/logo.jpg" width="120" height="20" alt="xrj Contributes to DaniWeb" border="0" />
                            </a></li>
		            </ul>
        </asp:Literal>
        </asp:TableCell>
        </asp:TableRow>
        <asp:TableRow>
        <asp:TableCell>
                <asp:Literal ID="Literal2" runat="server">
                <tr><td style="width:600px; text-align:left">
                <h4 style="font-family:Arial; font-size:medium;position:relative; margin-left:40px">
                Miscellaneous links</h4>
                </asp:Literal>
        <asp:BulletedList runat="server" DisplayMode="HyperLink">
        <asp:ListItem Value="http://www.catedralbcn.org"
                Text="Catedral de Barcelona" />
        <asp:ListItem Value="https://www.herostechnology.co.uk"
                Text="Heros technology" />
        <asp:ListItem Value="https://www.m0wwa.co.uk"
                Text="mowwa web corner" />
        <asp:ListItem Value="/antonioluis"
                Text="El retratista Antonio Luis" />
        <asp:ListItem Value="https://www.dxzone.com/dx22062/windipoles.html"
                Text="DXZone" />
        </asp:BulletedList>
        </asp:TableCell>
        </asp:TableRow>
        </asp:Table>

</asp:Content>
