<%@ Page Language="vb" ValidateRequest="false" MasterPageFile="~/Site3.Master" AutoEventWireup="false" CodeBehind="mobile.aspx.vb" Inherits="mates4.mobile2"  %>

<asp:Content ID="Content1" ContentPlaceHolderID="headc" runat="server">
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
		<meta content="Xavier R.Junqué" name="author" />
		<meta content="online, polynomials, roots, zeros, calculator, solver, derivatives, integrals, lagrangian interpolation, inverse matrix, cofactor, eigenvalues, eigenvectors, jordan form, linear and non-linear equations solver, partial fractions integration, limits, RPN"
			name="Keywords" />
		<meta content="Free Online Polynomials Calculator and Solver (real/complex coeff./roots); VB.Net Calculator download; source code; tutorial."
			name="description" />
		<meta content="ALL" name="robots" />
        <meta http-equiv="X-UA-Compatible" content="IE=edge" />
        <meta name="viewport" content="width=device-width" />
        <title>Polynomials Calculator</title>
        <link rel="icon" type="image/ico" href="../favicon.ico" />
        <link rel="shortcut icon" href="~/favicon.ico" /> 
    <style type="text/css">
        .all {width:100%;}
    </style>
</asp:Content>
<asp:Content id = "Content3" ContentPlaceHolderID="mobilec" runat="server">
        <div>
        <div style="font:12px arial,verdana;">
            Polynomial&#39;s calculator<br />
            &nbsp;&nbsp;&nbsp;
            <asp:DropDownList ID="DropDownList1" runat="server" AutoPostBack="True">
                <asp:ListItem>Examples</asp:ListItem>
                <asp:ListItem>-3*4/(5+1)</asp:ListItem>
                <asp:ListItem>1600/(1-(1+1/4)^2)</asp:ListItem>
                <asp:ListItem>x2-1 = 0</asp:ListItem>
                <asp:ListItem>Factor(x^3 +x^2 -x -1)</asp:ListItem>
                <asp:ListItem>Roots(x8-1)</asp:ListItem>
                <asp:ListItem>Poly(-1,2)</asp:ListItem>
                <asp:ListItem>x+1 @x=3</asp:ListItem>
                <asp:ListItem>x+y @x=2@y=1</asp:ListItem>
                <asp:ListItem>A^-1 @A=2;0|0;1</asp:ListItem>
                <asp:ListItem>y=x2-1@y={0;2;3}</asp:ListItem>
            </asp:DropDownList>
            <br />
            <asp:Literal ID="literalNote" runat="server">Note 1: if you wish to find the <span style='color:red;'>ROOTS</span> of a polynomial you 
                <span style='color:red;'>must</span> equal the equation <span style='color:red;'>to ZERO</span>.
                For example, write x^2-1<span style='color:red'>= 0</span> and DO NOT just write x^2-1. Alternatively, enter: roots(x^2-1)</asp:Literal>
            <br />
            <asp:Literal ID="literalNote2" runat="server">Note 2: To find a polynomial given the roots (for example if the roots are
                -1,i,-i) enter <span style='color:red;'>POLY(-1;i;-i)</span> </asp:Literal>
            <br />
            Input:<br />
            <asp:TextBox ID="tbInput" Rows="4" TextMode="MultiLine" runat="server" CssClass="all"></asp:TextBox>
            <br />
            <asp:CheckBox ID="chkRnd" Text="Rounding" runat="server" Checked="True" />
&nbsp;&nbsp;&nbsp;
            <asp:CheckBox ID="chkFractions" Text="Fractions" runat="server" Checked="False" />
&nbsp;&nbsp;&nbsp;
            <asp:CheckBox ID="chkCase" Text="Case sensitive" runat="server" Checked="False" />
            <br />
            <br />
            <asp:Button ID="btnGO" runat="server" Text="GO" />
            Output:<br />
            <asp:Literal ID="Output" runat="server"></asp:Literal>
        </div>
            <p>
                <asp:LinkButton ID="LinkCopy" Visible="false" OnClientClick="copyToClipboard('HiddenField1');return false;" runat="server" >
                    Copy response to Clipboard</asp:LinkButton>
                <asp:HiddenField ID="HiddenField1" ClientIDMode="Static" runat="server" />
                <textarea id="ta1" style="opacity: 0.0;"></textarea>
                <span id="success"></span>
            </p>
    <script type="text/javascript">
        function copyToClipboard(element) {
            var temp = document.getElementById('ta1');
            var x = document.getElementById(element);
            temp.textContent = x.value;
            temp.select();
            var success = document.execCommand("copy");
            if (success)
                document.getElementById('success').innerText = "Copied!"
            else
                document.getElementById('success').innerText = "Copy failed."

        }    </script>
        
    <!-- Google Analytics -->
        </div>
</asp:Content>
