<%@ Page Language="vb" AutoEventWireup="false" MasterPageFile="~/Site1.Master"
     CodeBehind="quickStart.aspx.vb" Inherits="mates4.quickStart" %>
<asp:Content ID="Content1" ContentPlaceHolderID="head" runat="server">
    <title>Quick Help -- Polynomial Calculator</title>
    <style type="text/css" media="screen">
    P
    {
      text-align: justify;
      text-indent: 8px; 
    }
    .num
    {
      color: green;
      font-weight: bold;
    }
    .lit
    {
      color: navy;
      font-weight: bold;
    }
    </style>
</asp:Content>
<asp:Content ID="Content2" ContentPlaceHolderID="scriptID" runat="server">
</asp:Content>
<asp:Content ID="Content3" ContentPlaceHolderID="content2L" runat="server">
        <h3 class="TableTot">
        <asp:Label ID="lblTitulo" runat="server" CssClass="keyboardPolyC">Quick Start to Polynomials &amp; Scientific Calculator</asp:Label>
        </h3>
</asp:Content>
<asp:Content ID="Content4" ContentPlaceHolderID="content2R" runat="server">
</asp:Content>
<asp:Content ID="Content5" ContentPlaceHolderID="maincontent" runat="server">
                    <asp:Table ID="Table1" runat="server" Width="700px" HorizontalAlign="center" CssClass="TableTot">
                    <asp:TableRow>
                        <asp:TableCell>
                            <p>
                            If there is any previous data, click on <span runat="server" class="lit">Clear All</span> to clear, avoiding possible errors.
                            </p>
                            <p>
                            So, the page should look like:
                            </p>
                        </asp:TableCell>
                    </asp:TableRow>
                    <asp:TableRow>
                        <asp:TableCell HorizontalAlign="Center">
                            <asp:Image runat="server" ImageUrl="~/imgQuickStart/quickStart1.png" />
                        </asp:TableCell>
                    </asp:TableRow>
                    <asp:TableRow>
                        <asp:TableCell>
                            <p>
                            Now, suppose you are told to add <span runat="server" class="num">1234 + 5678 + 9012</span>. As one may expect, 
                            a polynomial calculator does the same as an ordinary calculator, plus more.
                            To solve <span runat="server" class="num">1234 + 5678 + 9012</span> write this <i><strong>mathematical expression</strong></i> in 
                            the left box and click on <span runat="server" class="lit">Calculate</span>:
                            </p>
                        </asp:TableCell>
                    </asp:TableRow>
                    <asp:TableRow>
                        <asp:TableCell HorizontalAlign="Center">
                            <asp:Image runat="server" ImageUrl="~/imgQuickStart/quickStart2.png" />
                        </asp:TableCell>
                    </asp:TableRow>
                    <asp:TableRow>
                        <asp:TableCell>
                            <p>
                            Once the calculation is completed the answer will appear at the end:
                            </p>
                        </asp:TableCell>
                    </asp:TableRow>
                    <asp:TableRow>
                        <asp:TableCell HorizontalAlign="Center">
                            <asp:Image runat="server" ImageUrl="~/imgQuickStart/quickStart3.png" />
                        </asp:TableCell>
                    </asp:TableRow>
                    <asp:TableRow>
                        <asp:TableCell>
                            <p>
                            Realize, the preceeding works the same for most of the math problems 
                            the calculator can manage. Replace the above mentioned 
                            <span runat="server" class="num">1234 + 5678 + 9012</span>
                            by, for example, <span runat="server" class="num">(2*x+5)*(3*x-2)</span>. 
                            Proceed similarly and the answer will be the result of the product of two polynomials. 
                            </p>
                            <p>
                            Likewise, replacing by <span runat="server" class="num">10 - 10 * sqr(x) = 0</span>, the answer will 
                            exhibit the solution to one equation.
                            </p>
                        </asp:TableCell>
                    </asp:TableRow>
                    <asp:TableRow >
                        <asp:TableCell>
                            <h1>Do Not:</h1>
                            <asp:Image runat="server" ImageUrl="~/images/dont1.PNG" />
                            <asp:Image runat="server" ImageUrl="~/images/dont2.PNG" />
                            <asp:Image runat="server" ImageUrl="~/images/dont3.PNG" />
                            <h1>Instead:</h1>
                            <asp:Image runat="server" ImageUrl="~/images/instead.PNG" />
                            <asp:Image runat="server" ImageUrl="~/images/instead2.PNG" />
                        </asp:TableCell>
                    </asp:TableRow>
                </asp:Table>
      
</asp:Content>
<asp:Content ID="Content6" ContentPlaceHolderID="afterMC" runat="server">
</asp:Content>
