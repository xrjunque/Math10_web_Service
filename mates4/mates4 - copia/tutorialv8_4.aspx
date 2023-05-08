<%@ Page Language="vb" AutoEventWireup="false" MasterPageFile="~/Site1.Master"
    CodeBehind="tutorialv8_4.aspx.vb" Inherits="mates4.tutorialv8_4"
 Title="Mates8.4 Tutorial -- Polynomials Calculator" %>

<asp:Content ID="Content2" ContentPlaceHolderID="content2L" runat="server">
    <asp:Table ID="Table1" runat="server" HorizontalAlign="Left" CssClass="TableTot" Width="100%">
    
        <asp:TableRow>
            <asp:TableCell HorizontalAlign="center">
              <h4>
              &nbsp;&nbsp;Mates8 v8.4: <asp:PlaceHolder ID="PlaceHolder1" runat="server"></asp:PlaceHolder> Programming Overview Tutorial</h4>
            </asp:TableCell>
            <asp:TableCell HorizontalAlign="Center" VerticalAlign="Middle" Height="30">
                <h4>Pages: <asp:PlaceHolder ID="PlaceHolderPagesNum" runat="server"></asp:PlaceHolder></h4>
            </asp:TableCell>
        </asp:TableRow>
    <asp:TableRow>
        <asp:TableCell HorizontalAlign="center" VerticalAlign="Top">
            <asp:HyperLink ID="HyperLink2"  NavigateUrl="download.aspx?file=mates8v8.4.0" runat="server">Mates8v8.4.0 download (.zip)</asp:HyperLink>
        </asp:TableCell>
        <asp:TableCell HorizontalAlign="center" VerticalAlign="Top">
            <asp:HyperLink ID="HyperLink1"  NavigateUrl="download.aspx?file=tutorialv8_4_0" runat="server">Tutorial download (.pdf)</asp:HyperLink>
        </asp:TableCell>
    </asp:TableRow>
    </asp:Table>
</asp:Content>

<asp:Content ID="Content3" ContentPlaceHolderID="content2R" runat="server">
    <asp:Table ID="Table2" runat="server" Height="60">
    
        <asp:TableRow>
        </asp:TableRow>
    </asp:Table>
</asp:Content>
<asp:Content ID="Content4" ContentPlaceHolderID="maincontent" runat="server">
    <asp:Table runat="server" CssClass="TableTot">
    
    <asp:TableRow>
        <asp:TableCell HorizontalAlign="center" VerticalAlign="Top" >
            <asp:Button ID="btnLeft" runat="server" Text="<" Enabled="false" />
            &nbsp;&nbsp;
            <asp:Label ID="lblCurPage" runat="server" Text="Label"></asp:Label>
            &nbsp;&nbsp;
            <asp:Button ID="btnRight" runat="server" Text=">" />
        </asp:TableCell>
    </asp:TableRow>
    <asp:TableRow>
        <asp:TableCell>
            <asp:Image Width="700" runat="server" ID="img" ImageUrl="~/tutorialv8_4_0/intro.png" />
        </asp:TableCell>
    </asp:TableRow>
    </asp:Table>
     <asp:HiddenField ID="HiddenFieldVersion" Value="3" runat="server" />
     <asp:HiddenField ID="HiddenFieldcurPage" Value="0" runat="server" />
</asp:Content>
<asp:Content ID="Content5" ContentPlaceHolderID="afterMC" runat="server">
    <asp:Table CellPadding="3" CellSpacing="3" GridLines="Both" ID="TableLink1" runat="server" CssClass="TableTot" HorizontalAlign="center" BorderStyle="Solid" BorderColor="Black" BorderWidth="2">
    
    </asp:Table>
</asp:Content>