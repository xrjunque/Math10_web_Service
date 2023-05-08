<%@ Page Language="vb" AutoEventWireup="false" CodeBehind="ConvertAlg2RPN_RPL.aspx.vb" Inherits="mates4.ConvertAlg2RPN_RPL" MasterPageFile="~/Site1.Master" Title="RPN Converter -- Polynomials calculator" %>

<asp:Content ID="Content1" ContentPlaceHolderID="top" runat="server">
</asp:Content>
<asp:Content ID="Content2" ContentPlaceHolderID="head" runat="server">
</asp:Content>
<asp:Content ID="Content3" ContentPlaceHolderID="scriptID" runat="server">
</asp:Content>
<asp:Content ID="Content4" ContentPlaceHolderID="content2L" runat="server">
<style type="text/css">
    .dspLabel {font-family: 'Courier New', Courier, 'Lucida Sans Typewriter', 'Lucida Typewriter';
               margin: 4px 7px 2px 4px;
               border:none;                   
    }
    .btn1px {width:1px;height:1px;border:none; margin:0px 0px 0px 0px;}
    .lblStack {border: 1px solid black; margin: 15px 15px 15px 15px; padding-left:2em; text-align:left;}
    .editDiv {
    font-family: 'Courier New', Courier, 'Lucida Sans Typewriter', 'Lucida Typewriter';
    -moz-appearance: textfield-multiline;
    -webkit-appearance: textarea;
    border: 1px solid gray;
    font: medium -moz-fixed;
    font: -webkit-small-control;
    height: 50px;
    overflow: auto;
    padding: 2px;
    resize: both;
    width: 90%;
    background-color: white;
}
    </style>

    <asp:Table runat="server">
        <asp:TableRow>
            <asp:TableCell>
                <asp:label id="label1" runat="server" Font-Size="Medium" Font-Names="Verdana"
					    Font-Bold="True">
					  &nbsp;&nbsp; Convert Algebraic expression to RPN (Reverse Polish Notation)
				    </asp:label>
            </asp:TableCell>
        </asp:TableRow>
    </asp:Table>
</asp:Content>
<asp:Content ID="Content5" ContentPlaceHolderID="content2R" runat="server">
</asp:Content>
<asp:Content ID="Content6" ContentPlaceHolderID="maincontent" runat="server">
    <div id="Div1" runat="server" style="margin-left:20px;" >
        <asp:Table runat="server">
            <asp:TableRow>
                <asp:TableCell>
                   <asp:DropDownList ID="cbFns" runat="server" AutoPostBack="true"></asp:DropDownList>
                </asp:TableCell>
                <asp:TableCell>
                    <br />
                </asp:TableCell>
                <asp:TableCell>
                   <asp:DropDownList ID="cbExamples" runat="server" AutoPostBack="true">
                       <asp:ListItem Text="Examples" Value="--"></asp:ListItem>
                       <asp:ListItem Text="I. Basic operations" Value="--"></asp:ListItem>
                       <asp:ListItem Text="I.1 2*3+4" Value="2*3+4 // Algebraic input"></asp:ListItem>
                       <asp:ListItem Text="I.2 2^3^2 =2^(3^2)" Value="2^3^2"></asp:ListItem>
                       <asp:ListItem Text="I.3 √(2+2) =sqrt(2+2)" Value="√(2+2)"></asp:ListItem>
                       <asp:ListItem Text="I.4 1600/((1-(1+1/4)^30)/(1-(1+1/4)))" Value="1600/((1-(1+1/4)^30)/(1-(1+1/4)))"></asp:ListItem>
                       <asp:ListItem Text="I.5 (-b+√(b^2-4*a*c))/(2*a)@a=1@b=2@c=3" Value="(-b+√(b^2-4*a*c))/(2*a)@a=1@b=2@c=3"></asp:ListItem>
                       <asp:ListItem Text="I.6 -X" Value="-X"></asp:ListItem>
                       <asp:ListItem Text="I.7 -X+x" Value="-X+x"></asp:ListItem>
                       <asp:ListItem Text="I.8 5!" Value="5!"></asp:ListItem>
                       <asp:ListItem Text="-------------" Value="--"></asp:ListItem>
                       <asp:ListItem Text="             " Value="--"></asp:ListItem>
                       <asp:ListItem Text="II. Evaluating" Value="--"></asp:ListItem>
                       <asp:ListItem Text="II.1 x^2-9*x+20@x=5" Value="x^2-9*x+20@x=5"></asp:ListItem>
                       <asp:ListItem Text="II.2 x^2-9*x+20@x=4.5" Value="x^2-9*x+20@x=4.5"></asp:ListItem>
                       <asp:ListItem Text="II.3 2+x/(y*z)@x=12@y=4@z=-3" Value="2+x/(y*z)@x=12@y=4@z=-3"></asp:ListItem>
                       <asp:ListItem Text="-------------" Value="--"></asp:ListItem>
                       <asp:ListItem Text="             " Value="--"></asp:ListItem>
                       <asp:ListItem Text="III. Modulus" Value="--"></asp:ListItem>
                       <asp:ListItem Text="III.1 11 MOD 2" Value="11 MOD 2"></asp:ListItem>
                       <asp:ListItem Text="III.2 10 MOD 2" Value="10 MOD 2"></asp:ListItem>
                       <asp:ListItem Text="III.3 (6*x^11+18*x^9+4*x^8+36*x^6+16*x^3) MOD 11" Value="(6*x^11+18*x^9+4*x^8+36*x^6+16*x^3) MOD 11"></asp:ListItem>
                       <asp:ListItem Text="III.4 (x^7+x^6+x^5+x^4+x^3+x^2)MOD(x^5+x)" Value="(x^7+x^6+x^5+x^4+x^3+x^2)MOD(x^5+x)"></asp:ListItem>
                       <asp:ListItem Text="-------------" Value="--"></asp:ListItem>
                       <asp:ListItem Text="             " Value="--"></asp:ListItem>
                       <asp:ListItem Text="IV. Vector" Value="--"></asp:ListItem>
                       <asp:ListItem Text="IV.1 DOT([3,-3,1][4,9,2]) // dot product" Value="DOT([3,-3,1][4,9,2]) // dot product"></asp:ListItem>
                       <asp:ListItem Text="IV.1 CROSS([3,-3,1][4,9,2]) // cross product" Value="CROSS([3,-3,1][4,9,2]) // cross product"></asp:ListItem>
                       <asp:ListItem Text="             " Value="--"></asp:ListItem>
                       <asp:ListItem Text="V. Matrix" Value="--"></asp:ListItem>
                       <asp:ListItem Text="V.1 [[2,3],[3,-1]]+[[-1,4][3,3]]" Value="[[2,3],[3,-1]]+[[-1,4][3,3]]"></asp:ListItem>
                       <asp:ListItem Text="V.2 [[2,3],[3,-1]]*[[-1,4][3,3]]" Value="[[2,3],[3,-1]]*[[-1,4][3,3]]"></asp:ListItem>
                       <asp:ListItem Text="V.3 TRN[[2,-3],[3,-1]] // transpose" Value="TRN[[2,-3],[3,-1]] // transpose"></asp:ListItem>
                       <asp:ListItem Text="V.4 DET[[2,3],[3,-1]] //determinant" Value="DET[[2,3],[3,-1]] // determinant"></asp:ListItem>
                       <asp:ListItem Text="V.5 RANK([[1,2],[3,4]])" Value="RANK([[1,2],[3,4]])"></asp:ListItem>
                       <asp:ListItem Text="V.6 RANK([[1,2],[2,4]])" Value="RANK([[1,2],[2,4]])"></asp:ListItem>
                       <asp:ListItem Text="V.7 EGVL([[4,1,-2],[1,2,-1],[2,1,0]]) // eigenvalues" Value="EGVL([[4,1,-2],[1,2,-1],[2,1,0]])"></asp:ListItem>
                       <asp:ListItem Text="-------------" Value="--"></asp:ListItem>
                       <asp:ListItem Text="             " Value="--"></asp:ListItem>
                       <asp:ListItem Text="VI. Miscellany" Value="--"></asp:ListItem>
                       <asp:ListItem Text="VI.1 LCM(6*(x^2-1),4(x+1))" Value="LCM(6*(x^2-1),4*(x+1))"></asp:ListItem>
                       <asp:ListItem Text="VI.2 GCD(6*(x^2-1),4(x+1))" Value="GCD(6*(x^2-1),4*(x+1))"></asp:ListItem>
                       <asp:ListItem Text="-------------" Value="--"></asp:ListItem>
                       <asp:ListItem Text="             " Value="--"></asp:ListItem>
                   </asp:DropDownList>
                </asp:TableCell>
            </asp:TableRow>
        </asp:Table>
        <asp:Table runat="server" Width="95%">
            <asp:TableRow>
                <asp:TableCell>
                    <asp:Label runat="server" Text="Algebraic Linear Input:" ID="lblExpression" />
                    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<asp:LinkButton OnClientClick="copyToClipboard1();" ID="LinkButton2" runat="server">Copy Input to Clipboard</asp:LinkButton>
                    <br />
                    <div id="eDiv" runat="server" class="editDiv" contenteditable="true" oninput="Update1();"></div> 
                    <asp:HiddenField ID="hiddendiv" runat="server" ClientIDMode="Static" />
                    <asp:HiddenField ID="hiddencaretpos" runat="server" />

                </asp:TableCell>
            </asp:TableRow>
            <asp:TableRow>
                <asp:TableCell>
                    <asp:Button ID="btnStandard" runat="server" Text="Standard notation" />
                    &nbsp;&nbsp;&nbsp;&nbsp;
                    <asp:Image ID="imgStandard" runat="server" />
                    <br />
                    <hr style="width:100%" />
                </asp:TableCell>
            </asp:TableRow>
        </asp:Table>
        <asp:Table runat="server" Width="95%">
            <asp:TableRow>
                <asp:TableCell VerticalAlign="Top" Width="45%" HorizontalAlign="Center">
                    <asp:Table runat="server" >
                        <asp:TableRow>
                           <asp:TableCell VerticalAlign="Top">
                               <div runat="server" style="height:0px;">&nbsp;</div>
                            <asp:Label runat="server" Text="Variables (prepend _ if more than one character, e.g.: _k7=2):" ID="lblVariables" />
                               <br />
                               <asp:TextBox Width="205" runat="server"  Rows="7" ID="tbVars" TextMode="MultiLine">
                               </asp:TextBox>
                           </asp:TableCell>
                            <asp:TableCell HorizontalAlign="Center">
                                <br /><br />
                                <asp:Button ID="BtnConvert" runat="server" Text="Convert" Width="70" />
                                    <div runat="server" style="height:10px">&nbsp;</div>
                                <asp:Button ID="BtnClearAll" runat="server" Text="Clear All" Width="70" />
                                    <br />
                            </asp:TableCell>
                        </asp:TableRow>
                        <asp:TableRow>
                            <asp:TableCell ColumnSpan="2">
                                <asp:CheckBox ID="chkRounding" runat="server" Checked="true" Text="Rounding" AutoPostBack="true" />
                            </asp:TableCell>
                        </asp:TableRow>
                        <asp:TableRow>
                            <asp:TableCell ColumnSpan="2">
                                <asp:Label runat="server" ID="lblOutput" />                    
                            </asp:TableCell>
                        </asp:TableRow>
                        <asp:TableRow>
                            <asp:TableCell ColumnSpan="2">
                                <asp:Label runat="server" ID="lblMessage" />
                            </asp:TableCell>
                        </asp:TableRow>
                        <asp:TableRow>
                            <asp:TableCell ColumnSpan="2">
                                <asp:CheckBox ID="chkDetail" runat="server" Checked="false" Text="Alg. Step by Step" AutoPostBack="true" />
                                <br />
                                <asp:Label runat="server" ID="lblDetail" />
                            </asp:TableCell>
                        </asp:TableRow>
                    </asp:Table>
                </asp:TableCell>
                <asp:TableCell HorizontalAlign="Left" VerticalAlign="Top" Width="55%">
                    <asp:Table runat="server">
                        <asp:TableRow VerticalAlign="Top">
                            <asp:TableCell HorizontalAlign="Left" VerticalAlign="Top" Wrap="false">
                                <asp:CheckBox ID="chkRPL" runat="server" Checked="false" Text="RPL" AutoPostBack="true" />
                                <br />
                                <asp:CheckBox ID="chkStack" runat="server" Checked="true" Text="Stack" AutoPostBack="true" />
                            </asp:TableCell>
                            <asp:TableCell HorizontalAlign="Left" VerticalAlign="Top" Wrap="false">
                                <asp:CheckBox ID="chkListProcessing" runat="server" Checked="false" Text="List Processing" AutoPostBack="true" />
                                <br />
                                <asp:CheckBox ID="chkAssign" runat="server" Checked="true" Text="Assignments" AutoPostBack="true" />
                                <br />
                                <asp:CheckBox ID="chkPurge" runat="server" Checked="false" Text="Purge" AutoPostBack="true" />
                            </asp:TableCell>
                            <asp:TableCell HorizontalAlign="Left" VerticalAlign="Top" Wrap="false">
                                <asp:CheckBox ID="chkEval" runat="server" Checked="false" Text="Evaluate" AutoPostBack="true" />
                                <br />
                                <asp:CheckBox ID="chkShowComments" runat="server" Checked="true" Text="Show comments" AutoPostBack="true" />
                            </asp:TableCell>
                        </asp:TableRow>
                        <asp:TableRow>
                            <asp:TableCell ColumnSpan="3" HorizontalAlign="Left">
                                        <br />
                                &nbsp;&nbsp;&nbsp;&nbsp;<asp:Literal ID="Literal1" runat="server">RPN Output:</asp:Literal>
                                <br />
                                &nbsp;&nbsp;&nbsp;&nbsp;<asp:LinkButton OnClientClick="return copyToClipboard2();" ID="LinkButton1" runat="server">Copy RPN to Clipboard</asp:LinkButton>
                                <asp:Label unselectable="off" CssClass="lblStack"  Width="335" runat="server"  ID="lblStack" BackColor="White">
                                        <br /><br /><br /><br /></asp:Label>
                                        <br />
                            </asp:TableCell>
                        </asp:TableRow>
                    </asp:Table>
                </asp:TableCell>
            </asp:TableRow>
        </asp:Table>
            <p>
                <asp:TextBox CssClass="btn1px" ID="tbCopy" TextMode="MultiLine" Rows="0" runat="server"></asp:TextBox>
                <span id="success"></span>
            </p>
        <asp:HiddenField id="hiddeneDiv" runat="server" />
    </div>
</asp:Content>
<asp:Content ID="Content7" ContentPlaceHolderID="afterMC" runat="server">
</asp:Content>
<asp:Content ID="Content8" ContentPlaceHolderID="foot" runat="server">
    <script type="text/javascript">

        function copyToClipboard1() {
            var src = document.getElementById('<% = eDiv.ClientID %>');
            var dst = document.getElementById('<% = tbCopy.ClientID %>');
            var orig = dst.value;
            dst.value = src.innerText;
            dst.focus();
            dst.select();
            var success = document.execCommand("copy");
            if (success)
                document.getElementById('success').innerText = "Copied!";
            else
                document.getElementById('success').innerText = "Copy failed.";
            dst.value = orig;
            return false;
        }

        function copyToClipboard2() {
            var src = document.getElementById('<% = lblStack.ClientID %>');
            var dst = document.getElementById('<% = tbCopy.ClientID %>');
            var orig = dst.value;
            dst.value = src.innerText;
            dst.focus();
            dst.select();
            var success = document.execCommand("copy");
            if (success)
                document.getElementById('success').innerText = "Copied!";
            else
                document.getElementById('success').innerText = "Copy failed.";
            dst.value = orig;
            return false;
        }    

        var vFn = [
            "rank",
            "trn", "cross", "dot", "det", "egvl", "egv",
            "acosh", "acoth", "acsch", "asech", "asinh", "atanh",
            "coth", "csch", "sech", "sign", "gcd", "lcm",
            "acos", "acot", "acsc", "asec", "asin", "atan", "conj", "cosh", "neg", "sinh", "sqrt", "tanh",
            "abs", "arg", "cos", "cot", "csc", "exp", "log", "mod", "sec", "sin", "tan", "sqr",
            "im", "ln", "re", "√"];
        var sFn = "(" + vFn.join("|") + ")";
        var reFn = new RegExp(sFn, "i");
        var reNum = new RegExp("[0-9]*\.?[0-9]+([eE](\s*)[-+]?[0-9]+)?");
        var reOp = new RegExp("[-+*/\^\!]");
        var reVar = new RegExp("_?[A-Za-z]+");

        function Update1() {
            try {
                var el = document.getElementById('<% = eDiv.ClientID %>');
                var value = el.innerText;
                var ln = value.length;
                var elB = document.getElementById('<% = hiddeneDiv.ClientID %>');
                var valueB = elB.value;
                var lnB = valueB.length;

                // Get new caret position:
                var pos = getCaretPosition(el);

                if (ln > 0) {
                    var gValue = "";
                    var vValue = value.split("\n");
                    for (var i = 0; i < vValue.length; i++) {
                        vValue[i] = color(vValue[i]);
                    }
                    var ff = "font-family: Courier New, Courier, Lucida Sans Typewriter, Lucida Typewriter;"
                    el.innerHTML = "<span style='" + ff + "'>" + vValue.join("<br>") + "</span>";
                }
                elB.value = el.innerText;
                document.getElementById('<% = hiddendiv.ClientID %>').value = elB.value;
                if (ln == lnB + 1 && value.charAt(lnB) == "\n")
                    setCaretAtEnd(el);
                else
                    setCaretPos(el, pos);
            }
            catch (e) {
                //alert(e.toString());
            }
        }
        function color(str) {
            var vM = [reOp, reFn, reNum, reVar];
            for (var i = 0; i < vM.length; i++) {
                m = vM[i].exec(str);
                if (m) {
                    var insert;
                    switch (i) {
                        case 0: insert = "<span style='color:#ff1744;font-weight:bold;'>" + m[0] + "</span>"; break;
                        case 1: insert = "<span style='font-weight:bold;'>" + m[0] + "</span>"; break;
                        case 2: insert = "<span style='color:#5c6bc0;'>" + m[0] + "</span>"; break;
                        case 3: insert = "<span style='color:#956733;'>" + m[0] + "</span>"; break;
                    }
                    var pos1 = m.index + m[0].length;
                    var start = str.slice(0, m.index);
                    if (start.length) start = color(start);
                    var end = str.slice(pos1);
                    if (end.length) end = color(end);
                    str = start + insert + end;
                    return str;
                }
            }
            if (str.length == 0) return " ";
            return str;
        }

        function getCaretPosition(element) {
            var ie = (typeof document.selection != "undefined" && document.selection.type != "Control") && true;
            var w3 = (typeof window.getSelection != "undefined") && true;
            var caretOffset = 0;
            var objSel = window.getSelection();
            if (w3) {
                var range = window.getSelection().getRangeAt(0);
                var preCaretRange = range.cloneRange();
                preCaretRange.selectNodeContents(element);
                preCaretRange.setEnd(range.endContainer, range.endOffset);
                caretOffset = preCaretRange.toString().length;
            }
            else if (ie) {
                var textRange = document.selection.createRange();
                var preCaretTextRange = document.body.createTextRange();
                preCaretTextRange.expand(element);
                preCaretTextRange.setEndPoint("EndToEnd", textRange);
                caretOffset = preCaretTextRange.text.length;
            }
            return caretOffset;
        }
        function setCaretPos(el, sPos) {
            var charIndex = 0, range = document.createRange();
            range.setStart(el, 0);
            range.collapse(true);
            var nodeStack = [el], node, foundStart = false, stop = false;
            while (!stop && (node = nodeStack.pop())) {
                if (node.nodeType == 3) {
                    var nextCharIndex = charIndex + node.length;
                    if (!foundStart && sPos >= charIndex && sPos <= nextCharIndex) {
                        range.setStart(node, sPos - charIndex);
                        foundStart = true;
                    }
                    if (foundStart && sPos >= charIndex && sPos <= nextCharIndex) {
                        range.setEnd(node, sPos - charIndex);
                        stop = true;
                    }
                    charIndex = nextCharIndex;
                } else {
                    var i = node.childNodes.length;
                    while (i--) {
                        nodeStack.push(node.childNodes[i]);
                    }
                }
            }
            selection = window.getSelection();
            selection.removeAllRanges();
            selection.addRange(range);
        }
        function setCaretAtEnd(el) {
            el.focus();
            if (typeof window.getSelection != "undefined"
                && typeof document.createRange != "undefined") {
                var range = document.createRange();
                range.selectNodeContents(el);
                range.collapse(false);
                var sel = window.getSelection();
                sel.removeAllRanges();
                sel.addRange(range);
            } else if (typeof document.body.createTextRange != "undefined") {
                var textRange = document.body.createTextRange();
                textRange.moveToElementText(el);
                textRange.collapse(false);
                textRange.select();
            }
        }
        var ua = window.navigator.userAgent;
        var msie = ua.indexOf("MSIE ");
        if (msie > 0 || !!navigator.userAgent.match(/Trident.*rv\:11\./)) {
            var el = document.getElementById('<% = eDiv.ClientID %>');
            el.onkeyup = Update1;
            el.onpaste = Update1;
            el.oncut = Update1;
            el.onblur = Update1;
            document.getElementById('<% = btnStandard.ClientID %>').onfocus = Update1;
            document.getElementById('<% = BtnConvert.ClientID %>').onfocus = Update1;
            document.getElementById('<% = chkAssign.ClientID %>').onfocus = Update1;
            document.getElementById('<% = chkDetail.ClientID %>').onfocus = Update1;
            document.getElementById('<% = chkEval.ClientID %>').onfocus = Update1;
            document.getElementById('<% = chkListProcessing.ClientID %>').onfocus = Update1;
            document.getElementById('<% = chkPurge.ClientID %>').onfocus = Update1;
            document.getElementById('<% = chkRounding.ClientID %>').onfocus = Update1;
            document.getElementById('<% = chkRPL.ClientID %>').onfocus = Update1;
            document.getElementById('<% = chkShowComments.ClientID %>').onfocus = Update1;
            document.getElementById('<% = chkStack.ClientID %>').onfocus = Update1;
        }
    </script>
</asp:Content>

