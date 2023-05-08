<%@ Page Language="vb" AutoEventWireup="false" CodeBehind="WebServiceInfo.aspx.vb" 
    Inherits="mates4.WebServiceInfo" MasterPageFile="~/Site1.Master" 
    Title="Computer Algebra System -- CAS Calculator" %>
<asp:Content ID="Content1" ContentPlaceHolderID="head" runat="server">
		<meta content="online, computer, algebra, system, cas, calculator" name="Keywords" />
		<meta content="CAS Calculator. Free Online Computer Algebra System" name="description" />

</asp:Content>

<asp:Content id = "Content3" ContentPlaceHolderID="maincontent" runat="server">


        <div id = "Div1" runat="server" style="margin-left:20px;" >
    		    <asp:Table id = "Table1" runat="server">
                
                <asp:TableRow>
                    <asp:TableCell>
                        <asp:Literal ID="Literal1" runat="server">
                        </asp:Literal>
                    </asp:TableCell>
                </asp:TableRow>
		    </asp:Table>
        </div>



</asp:Content>
