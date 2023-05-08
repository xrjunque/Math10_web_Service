<%@ Page Language="vb" AutoEventWireup="false" MasterPageFile="~/Site1.Master"
    Inherits="mates4.rootfinder" CodeBehind="rootfinder.aspx.vb"
    title="Root Finder -- Polynomials Calculator" %>




<asp:Content ID="Content1" ContentPlaceHolderID="head" runat="server">
            <style type="text/css" media="screen">
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
    .lbtn {width:37px; text-align:center; vertical-align:middle;}
    .txt {margin-left:10px;}
    </style>
</asp:Content>
<asp:Content ID="Content2" ContentPlaceHolderID="content2L" runat="server">
    <asp:Table runat="server">
        <asp:TableRow>
            <asp:TableCell>
                <asp:label id="label1" runat="server" Font-Size="Medium" Font-Names="Verdana"
                    Font-Bold="True">
					    Polynomial roots finder (factoring)
				    </asp:label>
            </asp:TableCell>
        </asp:TableRow>
        <asp:TableRow>
            <asp:TableCell><div id="Div0" class="txt">Write 10x<sup>4</sup>-0x<sup>3</sup>-270x<sup>2</sup>
				-140x+1200 or any other polynomial and click on Calculate to obtain the real 
                and/or complex roots.</div>
            </asp:TableCell>
        </asp:TableRow>
    </asp:Table>
</asp:Content>
<asp:Content id = "Content3" ContentPlaceHolderID="maincontent" runat="server">


        <div id = "Div1" runat="server" style="margin-left:20px;" >
    		    <asp:Table id = "Table1" runat="server">
                
                <asp:TableRow>
                <asp:TableCell VerticalAlign="Middle" ColumnSpan="2">
                </asp:TableCell>
                </asp:TableRow>
                <asp:TableRow>
                    <asp:TableCell VerticalAlign="Middle" HorizontalAlign="Left">
                        <asp:label id="Label6"
                            runat="server"
                            font-Names="Arial" Height="32px" Width="38px" Font-Bold="True">P(x): </asp:label>
                    </asp:TableCell>
                    <asp:TableCell>
                        <asp:Literal ID="Literal10"
                            runat="server">
                        </asp:Literal>
                        <asp:textbox id="TextBox1" spellcheck="false"
                            runat="server" Rows="5"
                            Height="63px" Columns="70" TextMode="MultiLine"> 10*x^4-270*x^2-140*x+1200</asp:textbox>
                    </asp:TableCell>
                </asp:TableRow>
            <asp:TableRow>
            <asp:TableCell ColumnSpan="2">
                <asp:Table ID="Table4" runat="server" CssClass="keyboardMargin">

                    <asp:TableRow>
                        <asp:TableCell RowSpan="5" HorizontalAlign="center" VerticalAlign="Top">
                            <asp:Table ID="Table5" runat="server">

                                <asp:TableRow>
                                    <asp:TableCell>
                                        <asp:Button ID="btn1Q" runat="server" Text="¼" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btn3Q" runat="server" Text="¾" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btnLP" runat="server" Text="(" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btnRP" runat="server" Text=")" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btnOneHalf" runat="server" Text="½" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btnPi" runat="server" Text="&#1055;" CssClass="lbtn"/>
                                    </asp:TableCell>
                                </asp:TableRow>
                                <asp:TableRow>
                                    <asp:TableCell>
                                        <asp:Button ID="btnCube" runat="server" Text="³" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btnx" runat="server" Text="x" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btnPow" runat="server" Text="^" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btnDiv" runat="server" Text="/" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btnMult" runat="server" Text="*" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btnMinus" runat="server" Text="-" CssClass="lbtn"/>
                                    </asp:TableCell>
                                </asp:TableRow>
                                <asp:TableRow>
                                    <asp:TableCell>
                                        <asp:Button ID="btnSquare" runat="server" Text="²" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btnSqrt" runat="server" Text="&#8730;" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btn7" runat="server" Text="7" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btn8" runat="server" Text="8" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btn9" runat="server" Text="9" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btnPlus" runat="server" Text="+" CssClass="lbtn"/>
                                    </asp:TableCell>
                                </asp:TableRow>
                                <asp:TableRow>
                                    <asp:TableCell>
                                        <asp:Button ID="btnOne" runat="server" Text="¹" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btnHexa" runat="server" Text="&h" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btn4" runat="server" Text="4" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btn5" runat="server" Text="5" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btn6" runat="server" Text="6" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btnEqual" runat="server" Text="=" CssClass="lbtn"/>
                                    </asp:TableCell>
                                </asp:TableRow>
                                <asp:TableRow>
                                    <asp:TableCell>
                                        <asp:Button ID="btnZero" runat="server" Text="°" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btnOctal" runat="server" Text="&o" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btn1" runat="server" Text="1" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btn2" runat="server" Text="2" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btn3" runat="server" Text="3" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btnEqualZero" runat="server" Text="=0" CssClass="lbtn"/>
                                    </asp:TableCell>
                                </asp:TableRow>
                                <asp:TableRow>
                                    <asp:TableCell>
                                        <asp:Button ID="btnMult2" runat="server" Text="·" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btnBin" runat="server" Text="&b" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btn0" runat="server" Text="0" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btnDot" runat="server" Text="." CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btnExp" runat="server" Text="e" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell>
                                        <asp:Button ID="btnCE" runat="server" Text="CE" CssClass="lbtn" ToolTip="Clear Entry. Clears last character."/>
                                    </asp:TableCell>
                                </asp:TableRow>

                                <asp:TableRow>
                                    <asp:TableCell VerticalAlign="Top">
                                        <asp:Button ID="btnDiv2" runat="server" Text="÷" CssClass="lbtn"/>
                                    </asp:TableCell>
                                    <asp:TableCell ColumnSpan="4" VerticalAlign="Bottom" HorizontalAlign="Center">
                                        <asp:Button Width="100%" ID="btnCalculate" runat="server" Text="Calculate (Enter key)" style="height: 35px; width:135px;" Font-Bold="True"/>
                                    </asp:TableCell>
                                    <asp:TableCell VerticalAlign="Top">
                                        <asp:Button ID="btnClear" runat="server" Text="Clr" CssClass="lbtn"/>
                                    </asp:TableCell>
                                </asp:TableRow>
                            </asp:Table>
                        </asp:TableCell>
                    </asp:TableRow>
                </asp:Table>
            </asp:TableCell>
            </asp:TableRow>
            <asp:TableRow>
                <asp:TableCell ColumnSpan="2" HorizontalAlign="Left" style="width:150px;height:40px; ">
                    <asp:CheckBox id="chkRounding" style="font-size:small;"
                        runat="server" Font-Names="Arial" Text="Apply rounding" Checked="True"></asp:CheckBox>
                    &nbsp;&nbsp;&nbsp;&nbsp;
                <asp:CheckBox ID="chkFractions" Font-Names="Arial" style="font-size:small;" runat="server" Text="Fractions"/>
                    <br/>
                    <br/>
                    <br/>
			    <asp:Literal ID="Lit3" runat="server" ClientIDMode="Static"></asp:Literal>
            </asp:TableCell>
            </asp:TableRow>
		    </asp:Table>
            <p>
                <asp:LinkButton ID="LinkCopy" Visible="false" OnClientClick="copyToClipboard('HiddenField1');return false;" runat="server" >
                    Copy response to Clipboard</asp:LinkButton>
                <asp:HiddenField ID="HiddenField1" ClientIDMode="Static" runat="server" />
                <textarea id="ta1" style="opacity: 0.0;"></textarea>
                <span id="success"></span>
            </p>
        </div>

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


</asp:Content>
