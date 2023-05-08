<%@ Page Language="vb" AutoEventWireup="false" MasterPageFile="~/Site1.Master"
     CodeBehind="commentfrm.aspx.vb" Inherits="mates4.commentfrm"
 Title="Comments form -- Polynomials Calculator" %>
<asp:Content ContentPlaceHolderID="head" runat="server">
</asp:Content>
<asp:Content ID="Content2" ContentPlaceHolderID="content2L" runat="server">
            <asp:Label ID="lblWelcome" runat="server" Font-Size="Medium" CssClass="TableComments">
                <p style="margin-left:20px;margin-right:10px;">
			    &nbsp;&nbsp;We welcome your comments and 
			    suggestions. You may also use this form to notify us of bugs. If you would like 
			    a personal response <b>be sure to include your email address.</b>
                </p>
                <asp:Label runat="server" Font-Size="Small" CssClass="TableComments">
                &nbsp;Si lo prefieres, puedes enviar tu mensaje en español.
                </asp:Label>
                <br />
                <asp:Label runat="server" Font-Size="Small" CssClass="TableComments">
                &nbsp;Si així ho voleu, podeu enviar-me el missatge en català.
                </asp:Label>
            </asp:Label>
</asp:Content>
<asp:Content ID="Content4" ContentPlaceHolderID="content2R" Visible="false" runat="server">

</asp:Content>
<asp:Content ID="Content3" ContentPlaceHolderID="maincontent" runat="server">
    
            <asp:Table ID="Table2" runat="server"  CssClass="TableComments">
            
            <asp:TableRow>
                <asp:TableCell CssClass="TableComments">
                    <asp:Label ID="lblCommentForm" runat="server" Text="Comment Form" Font-Size="Large"></asp:Label>
                </asp:TableCell>
            </asp:TableRow>
                    <asp:TableRow>
                        <asp:TableCell>
                        </asp:TableCell>
                    </asp:TableRow>
            <asp:TableRow>
                <asp:TableCell>
                    <asp:Table runat="server" ID="table3">
                    
                    <asp:TableRow BackColor="RoyalBlue" ForeColor="White" Font-Names="Arial" Font-Bold="true">
                        <asp:TableCell ColumnSpan="2">
                            <asp:Label ID="lblContact" runat="server" Text="Contact Information"></asp:Label>
                        </asp:TableCell>
                    </asp:TableRow>

                    <asp:TableRow>
                    <asp:TableCell  HorizontalAlign="Left">
                        <asp:Label ID="litName" runat="server">* First name, Initial: </asp:Label>
                    </asp:TableCell>
                    <asp:TableCell HorizontalAlign="Left">
                        <asp:TextBox ID="first" runat="server" Columns="15"></asp:TextBox>
                        <asp:TextBox ID="initial" runat="server" Columns="2"></asp:TextBox>
                    </asp:TableCell>
                    </asp:TableRow>

                    <asp:TableRow>
                    <asp:TableCell HorizontalAlign="Left">
                        <asp:Label ID="lblLastName" runat="server">Last name: </asp:Label>
                    </asp:TableCell>
                    <asp:TableCell HorizontalAlign="Left">
                        <asp:TextBox ID="lastname" runat="server" Columns="40"></asp:TextBox>
                    </asp:TableCell>
                    </asp:TableRow>

                    <asp:TableRow>
                    <asp:TableCell HorizontalAlign="Left">
                        <asp:Label ID="lblEmail" runat="server">Email address: </asp:Label>
                    </asp:TableCell>
                    <asp:TableCell HorizontalAlign="Left">
                        <asp:TextBox ID="email" runat="server" Columns="35"></asp:TextBox>
                    </asp:TableCell>
                    </asp:TableRow>

                    <asp:TableRow BackColor="RoyalBlue" ForeColor="White" Font-Names="Arial" Font-Bold="true">
                    <asp:TableCell HorizontalAlign="Left" ColumnSpan="2">
                        <asp:Label ID="lblSubjectAndRecipient" runat="server">Subject and Recipient</asp:Label>
                    </asp:TableCell>
                    </asp:TableRow>
                    <asp:TableRow>
                    <asp:TableCell HorizontalAlign="Left">
                        <asp:Label ID="lblSubject" runat="server" Text="Subject: "></asp:Label>
                    </asp:TableCell>
                    <asp:TableCell HorizontalAlign="Left">
                        <asp:TextBox ID="subject" runat="server" Columns="35"></asp:TextBox>
                    </asp:TableCell>
                    </asp:TableRow>

                    <asp:TableRow>
                    <asp:TableCell ColumnSpan="2">
                        <asp:Table ID="Table4" runat="server" >
                        
                            <asp:TableRow>
                                <asp:TableCell>
                                    <asp:Label ID="lblCommQuestSuggestions" runat="server">
                                       * Comments, Questions &amp; Suggestions:
                                    </asp:Label>
                                </asp:TableCell>
                            </asp:TableRow>

                            <asp:TableRow>
                                <asp:TableCell>
                                    <asp:TextBox ID="Comment" Rows="10" columns="55" runat="server" TextMode="MultiLine"></asp:TextBox>
                                </asp:TableCell>
                            </asp:TableRow>
                        
                        </asp:Table>
                    </asp:TableCell>
                    </asp:TableRow> 
                    <asp:TableRow>
                        <asp:TableCell HorizontalAlign="Left" ColumnSpan="2" Font-Size="10">
                            <asp:Table runat="server" BorderColor="Black" BorderWidth="1">
                                <asp:TableRow>
                                    <asp:TableCell>
                                        <asp:Button ID="btnReloadEn" runat="server" Text="Reload" />
                                        &nbsp;
                                        <asp:Label runat="server" ID="lblEn"></asp:Label>
                                        <asp:TextBox ID="tbEn" runat="server" Width="30" Text=""></asp:TextBox>
                                        &nbsp;
                                        <asp:Button ID="btnSubmitEn" runat="server" Text="Submit" />
                                    </asp:TableCell>
                                </asp:TableRow>
                                <asp:TableRow>
                                    <asp:TableCell>
                                        <asp:Button ID="btnReloadEs" runat="server" Text="Recargar" />
                                        &nbsp;
                                        <asp:Label runat="server" ID="lblEs"></asp:Label>
                                        <asp:TextBox ID="tbEs" runat="server" Width="30" Text=""></asp:TextBox>
                                        &nbsp;
                                        <asp:Button ID="btnSubmitEs" runat="server" Text="Enviar" />
                                    </asp:TableCell>
                                </asp:TableRow>
                                <asp:TableRow>
                                    <asp:TableCell>
                                        <asp:Button ID="btnReloadCa" runat="server" Text="Recarregar" />
                                        &nbsp;
                                        <asp:Label runat="server" ID="lblCa"></asp:Label>
                                        <asp:TextBox ID="tbCa" runat="server" Width="30" Text=""></asp:TextBox>
                                        &nbsp;
                                        <asp:Button ID="btnSubmitCa" runat="server" Text="Enviar" />
                                    </asp:TableCell>
                                </asp:TableRow>
                            </asp:Table>
                        </asp:TableCell>
                    </asp:TableRow>
                    </asp:Table>
                </asp:TableCell>
            </asp:TableRow>
        </asp:Table>


</asp:Content>
<asp:Content ID="Content1" ContentPlaceHolderID="afterMC" runat="server">
  <asp:Label ID="Label1" runat="server" Text="" Font-Names="Arial"></asp:Label>
  <asp:TextBox runat="server" ID="tbRecaptcha" Visible="false" ></asp:TextBox>
</asp:Content>
