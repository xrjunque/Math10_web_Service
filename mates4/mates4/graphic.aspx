<%@ Page Language="vb" MasterPageFile="~/Site1.Master"
     AutoEventWireup="false" CodeBehind="graphic.aspx.vb" Inherits="mates4.graphic" %>

<asp:Content ID="Content5" ContentPlaceHolderID="head" runat="server">
        <style type="text/css" media="screen">
    .margin {left:15px;width:95%; position:relative; top:0px;}
    .grPos {}
    .grPos2 { position:relative; left: 20px; top: 0px;}
    </style>
</asp:Content>
<asp:Content ID="Content1" ContentPlaceHolderID="content2L" runat="server" Visible="false">
    <h3><asp:Label runat="server" >&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Function grapher</asp:Label></h3>
</asp:Content>
<asp:Content ID="Content6" ContentPlaceHolderID="scriptID" runat="server" Visible="false">
    <script type="text/javascript">
        if (document.getElementById)
            getElemById = function (id) {
                return document.getElementById(id);
            }

        else if (document.all)
            getElemById = function (id) {
                return document.all[id];
                // note that we use square brackets here
            }
        function coord(e, mi) {
            try {
                var Xm,Xb,Xmargin,Ym,Yb,Ymargin,Xmin;
                Xm = getElemById('<% = Xm.ClientID %>').value; Xm = parseFloat(Xm);
                Xb = getElemById('<% = Xb.ClientID%>').value; Xb = parseFloat(Xb);
                Xmargin = getElemById('<% = Xmargin.ClientID%>').value; Xmargin = parseFloat(Xmargin);
                Xmin = getElemById('<% = Xmin.ClientID%>').value; Xmin = parseFloat(Xmin);
                Ym = getElemById('<% = Ym.ClientID%>').value; Ym = parseFloat(Ym);
                Yb = getElemById('<% = Yb.ClientID%>').value; Yb = parseFloat(Yb);
                Ymargin = getElemById('<% = Ymargin.ClientID%>').value; Ymargin = parseFloat(Ymargin);
                var rct = mi.getBoundingClientRect();
                var x0 = e.clientX - rct.left - Xmargin;
                var y0 = e.clientY - rct.top - Ymargin;
                var oX = getElemById('<% =tbX.ClientID %>');
                var oY = getElemById('<% =tbY.ClientID%>');
                oX.value = Xm * x0 + Xb;
                oY.value = -Ym * y0 + Yb;
                var str = '' + oX.value;
                var pos = str.indexOf('.');
                if(pos>-1 && str.length - pos>2 && Math.abs(oX.value)>1)
                { oX.value = Math.round(oX.value * 1000 + 0.005) / 1000; }
                var pos2 = str.indexOf('e');
                if (pos2 > pos + 6) oX.value = str.substring(0, pos + 6) + str.substring(pos2);
                str = '' + oY.value;
                pos = str.indexOf('.');
                if (pos > -1 && str.length - pos > 2 && Math.abs(oY.value) > 1)
                { oY.value = Math.round(oY.value * 1000 + 0.005) / 1000; }
                var pos2 = str.indexOf('e');
                if (pos2 > pos + 6) oY.value = str.substring(0, pos + 6) + str.substring(pos2);
            }
            catch (err) {
                alert(err);
            }
        }
    </script>
</asp:Content>
<asp:Content ID="Content3" ContentPlaceHolderID="content2R" runat="server" Visible="false">
</asp:Content>
<asp:Content ID="Content2" ContentPlaceHolderID="maincontent" runat="server">

        <asp:Table ID="Table1" runat="server" CssClass="margin"  BackColor="#EADA90" BorderColor="#C0C0C0" BorderWidth="1">
        <asp:TableRow>
            <asp:TableCell HorizontalAlign="Center">
                <asp:Table ID="Table2" runat="server" Width="95%">
                <asp:TableRow>
                    <asp:TableCell HorizontalAlign="Center">
                        <asp:Label ID="Label1" runat="server" Text="Left: "></asp:Label><br />
                        <asp:TextBox ID="tbLeft" runat="server" Width="80">-4</asp:TextBox>
                    </asp:TableCell>
                    <asp:TableCell HorizontalAlign="Center">
                        <asp:Label ID="Label2" runat="server" Text="Right: "></asp:Label><br />
                        <asp:TextBox ID="tbRight" runat="server" Width="80">4</asp:TextBox>
                    </asp:TableCell>
                    <asp:TableCell HorizontalAlign="Center">
                        <asp:Label ID="Label3" runat="server" Text="Bottom: "></asp:Label><br />
                        <asp:TextBox ID="tbBottom" runat="server" Width="80"></asp:TextBox>
                    </asp:TableCell>
                    <asp:TableCell HorizontalAlign="Center">
                        <asp:Label ID="Label4" runat="server" Text="Top: "></asp:Label><br />
                        <asp:TextBox ID="tbTop" runat="server" Width="80"></asp:TextBox>
                    </asp:TableCell>
                </asp:TableRow>
                <asp:TableRow>
                    <asp:TableCell>
                        <asp:Label ID="Label5" runat="server" Text="Function 1:" ForeColor="black"></asp:Label><br />
                        <asp:Textbox id="f1" runat="server" Columns="18" TextMode="MultiLine" Rows="5">x^3-1</asp:Textbox>
                    </asp:TableCell>
                    <asp:TableCell>
                        <asp:Label ID="Label6" runat="server" Text="Function 2:" ForeColor="blue"></asp:Label><br />
                        <asp:Textbox id="f2" runat="server" Columns="18" TextMode="MultiLine" Rows="5"></asp:Textbox>
                    </asp:TableCell>
                    <asp:TableCell>
                        <asp:Label ID="Label7" runat="server" Text="Function 3:" ForeColor="green"></asp:Label><br />
                        <asp:Textbox id="f3" runat="server" Columns="18" TextMode="MultiLine" Rows="5"></asp:Textbox>
                    </asp:TableCell>
                    <asp:TableCell>
                        <asp:Label ID="Label8" runat="server" Text="Function 4:" ForeColor="red"></asp:Label><br />
                        <asp:Textbox id="f4" runat="server" Columns="18"  TextMode="MultiLine" Rows="5">0</asp:Textbox>
                    </asp:TableCell>
                </asp:TableRow>
                <asp:TableRow>
                    <asp:TableCell HorizontalAlign="Left">
                        <asp:Table runat="server">
                        <asp:TableRow>
                            <asp:TableCell>
                                <asp:Label runat="server" ForeColor="Red" Text="X: " />
                                <asp:TextBox runat="server" ID="tbX" Text="" Width="80" />
                            </asp:TableCell>
                        </asp:TableRow>
                        </asp:Table>
                    </asp:TableCell>
                    <asp:TableCell HorizontalAlign="Left">
                        <asp:Table runat="server">
                        <asp:TableRow>
                            <asp:TableCell>
                                <asp:Label runat="server" ForeColor="Red" Text="Y: " />
                                <asp:TextBox runat="server" ID="tbY" Text="" Width="80" />
                            </asp:TableCell>
                        </asp:TableRow>
                        </asp:Table>
                    </asp:TableCell>
                    <asp:TableCell ColumnSpan="2" HorizontalAlign="Center">
                        <asp:Table runat="server">
                            <asp:TableRow>
                                <asp:TableCell Wrap="false">
                                    <asp:CheckBox ID="chkAutoTopBottom" Text="Auto top-bottom" runat="server" Checked="true" />
                                </asp:TableCell>
                                <asp:TableCell>
                                    <asp:Button ID="btnDraw" runat="server" Text="Draw" />
                                </asp:TableCell>
                                <asp:TableCell>
                                </asp:TableCell>
                            </asp:TableRow>
                        </asp:Table>
                    </asp:TableCell>
                    <asp:TableCell>
                    </asp:TableCell>
                </asp:TableRow>
                <asp:TableRow>
                    <asp:TableCell ColumnSpan="4">
                    </asp:TableCell>
                </asp:TableRow>
                </asp:Table>
            </asp:TableCell>
        </asp:TableRow>
        <asp:TableRow>
            <asp:TableCell HorizontalAlign="Left">
                <asp:Label runat="server" ID="lblMessage" Text="" ForeColor="Red" />
            </asp:TableCell>
       </asp:TableRow>
        <asp:TableRow>
            <asp:TableCell HorizontalAlign="Left">
                <asp:Table ID="TableGraphic" runat="server" BackColor="#FAFAFA" HorizontalAlign="Left">
                <asp:TableRow>
                        <asp:TableCell HorizontalAlign="Left">
                            <asp:Panel runat="server" ID="Panel1" Width="680px">
                            </asp:Panel>
                        </asp:TableCell>
                </asp:TableRow>
                </asp:Table>
            </asp:TableCell>
        </asp:TableRow>
        </asp:Table>


</asp:Content>
<asp:Content ID="Content4" ContentPlaceHolderID="afterMC" runat="server" Visible="false">
    <asp:HiddenField runat="server" ID="Xm" Value="0" />
    <asp:HiddenField runat="server" ID="Xb" Value="0" />
    <asp:HiddenField runat="server" ID="Xmargin" Value="0" />
    <asp:HiddenField runat="server" ID="Xmin" Value="0" />
    <asp:HiddenField runat="server" ID="Ym" Value="0" />
    <asp:HiddenField runat="server" ID="Yb" Value="0" />
    <asp:HiddenField runat="server" ID="Ymargin" Value="0" />
</asp:Content>
