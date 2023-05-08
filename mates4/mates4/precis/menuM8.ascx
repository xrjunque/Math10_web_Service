<%@ Control Language="vb" AutoEventWireup="false" Codebehind="menuM8.ascx.vb" Inherits="menuM8" %>


        <asp:Table ID="Table2" runat="server" Width="680" HorizontalAlign="center"  CssClass="menu"
         Font-Names="Verdana, Arial" Font-Size="Medium">
        
        <asp:TableRow>
        <asp:TableCell>
        <asp:Table ID="Table1" runat="server"  HorizontalAlign="center" Width="650" Height="45" BorderStyle="Solid" BorderColor="#b0b0c0" BorderWidth="1" CellPadding="0" CellSpacing="0">
        
        <asp:TableRow>
        <asp:TableCell HorizontalAlign="center">
            <asp:Table ID="Table3" runat="server" Width="100%">
            
            <asp:TableRow HorizontalAlign="center">
            <asp:TableCell >
               <asp:Literal ID="Literal1" runat="server" />
            </asp:TableCell>
            </asp:TableRow>
            </asp:Table>        
        </asp:TableCell>
        </asp:TableRow>
        </asp:Table>        
    </asp:TableCell>
   </asp:TableRow>
  </asp:Table> 

