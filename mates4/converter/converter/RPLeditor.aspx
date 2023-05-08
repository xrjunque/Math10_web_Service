<%@ Page Language="vb" AutoEventWireup="false" CodeBehind="RPLeditor.aspx.vb" Inherits="converter.RPLeditor" %>

<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
    <title></title>
    <style type="text/css">
    .editDiv {
    -moz-appearance: textfield-multiline;
    -webkit-appearance: textarea;
    border: 1px solid gray;
    font: medium -moz-fixed;
    font: -webkit-small-control;
    overflow: auto;
    height:500px;
    padding: 1px;
    width: 400px;
    white-space: pre;
    font-family: Courier New, Courier, monospace;
    font-size: 12px;
}
.fnt {font-family: Arial, Helvetica, sans-serif; }
.btn1px {width:1px;height:1px;
    background-color: Transparent;
    background-repeat:no-repeat;
    border: none;
    cursor:pointer;
    overflow: hidden;
    outline:none;
}
</style>
</head>

<body>
    <div>
        <form runat="server">
<script type="text/javascript" >
    String.prototype.splice = function (idx, rem, str) {
        return this.slice(0, idx) + str + this.slice(idx + Math.abs(rem));
    };
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
    var cr = 0;
    function eDivkeydown(event) {
        if (event.keyCode == 13)
            cr = 1;
        else
            cr = 0;
    }
    function Update1() {
        try {
            sFn = document.getElementById('<% =HiddensFn.ClientID %>').value; 
            reFn = new RegExp(sFn, "i");
            var el = document.getElementById('<% = eDiv.ClientID %>');
            var value = el.innerText;
            var ln = value.length;
            var elB = document.getElementById('<% = hiddeneDiv.ClientID %>');
            var valueB = elB.value;
            var lnB = valueB.length;

            // Get new caret position:
            var pos = getCaretPosition(el) + cr;
            document.getElementById('<% =hiddencaretpos.ClientID %>').value = pos;

            if (ln > 0) {
                var gValue = "";
                var vValue = value.split("\n");
                for (var i = 0; i < vValue.length; i++) {
                    vValue[i] = color(vValue[i]);
                }
                var ff = "font-family: Arial, Helvetica, sans-serif;"
                el.innerHTML = "<span style='" + ff + "'>" + vValue.join("<br>") + "</span>";
            }
            elB.value = el.innerText;
            //document.getElementById('<% = hiddendiv.ClientID %>').value = elB.value;
            if (ln == lnB + 1 && value.charAt(lnB) == "\n") {
                setCaretAtEnd(el);
                pos = elB.value.length;
            }
            else
                setCaretPos(el, pos);
            document.getElementById('<% = hiddencaretpos.ClientID %>').value = pos;
            showCaretPos();
        }
        catch (e) {
            //alert(e.toString());
        }
    }
    function getSelectedNode(val) {
        try {
            var el = document.getElementById('<% = eDiv.ClientID %>');
            var value = el.innerText;
            var pos = getCaretPosition(el);
            var cr = 0;
            for (var i = 0; i < pos; i++) {
                if (value.charAt(i) == '\n') cr++;
            }
            var start = value.slice(0, pos+cr);
            var end = value.slice(pos+cr);
            value = start + val + end;
            el.innerText = value;
            setCaretPos(el, pos + val.length);
            Update1();
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
        alert("This page is unavailable in Internet Explorer.\nYou should update your browser.")
    }
    function showCaretPos() {
        //Update1();
        var el = document.getElementById('<% = eDiv.ClientID %>');
        var caretPosEl = document.getElementById("caretPos");
        var pos = getCaretPosition(el);
        document.getElementById('<% = hiddencaretpos.ClientID %>').value = pos;
        var lineNum = 0;
        var value = el.innerText;
        for (var i = 0; i <= pos; i++) {
            if (i < value.length && value.charAt(i) == '\n') lineNum++;
        }
        //caretPosEl.innerHTML = "Caret position: " + pos;
        caretPosEl.innerHTML = "Line number: " + ++lineNum;
    }

    // Function to download data to a file
    function download() {
        var data = document.getElementById('<% = eDiv.ClientID %>').innerText;
        var data1 = data.replace("\n", "\r\n");
        var filename = "RPLcode.txt";
        var type = "plain/text";
        var file = new Blob([data1], { type: type });
        if (window.navigator.msSaveOrOpenBlob) // IE10+
            window.navigator.msSaveOrOpenBlob(file, filename);
        else { // Others
            var a = document.createElement("a"),
                    url = URL.createObjectURL(file);
            a.href = url;
            a.download = filename;
            document.body.appendChild(a);
            a.click();
            setTimeout(function() {
                document.body.removeChild(a);
                window.URL.revokeObjectURL(url);  
            }, 0); 
        }
    }
    function copyToClipboard1() {
        var src = document.getElementById('<% = hiddeneDiv.ClientID %>');
        var dst = document.getElementById('<% = tbCopy.ClientID %>');
        var orig = dst.value;
        dst.value = src.value;
        dst.focus();
        dst.select();
        var success = document.execCommand("copy");
        if (success)
            document.getElementById('success').innerText = "Copied!";
        else
            document.getElementById('success').innerText = "Copy failed.";
        dst.value = orig;
    }

    </script>
            <asp:Table runat="server">
        <asp:TableRow>
            <asp:TableCell>

    <asp:Table runat="server">
        <asp:TableRow VerticalAlign="Top">
        <asp:TableHeaderCell VerticalAlign="Top">
             <asp:TreeView CssClass="fnt" ID="TreeView1" runat="server" Target="_self"></asp:TreeView>
        </asp:TableHeaderCell>
        </asp:TableRow>
    </asp:Table>
            </asp:TableCell>
            <asp:TableCell VerticalAlign="Top">
                <div>
                    <asp:FileUpload ID="FileUpload1" runat="server" />&nbsp;&nbsp;
                    <asp:Button ID="btnLoad" runat="server" Text="Load" />
                    <br />
                    <asp:Button ID="btnSave" runat="server" Text="Save" OnClientClick="download();" />

                </div>
                <div class="tArea">
                   <div id="eDiv" spellcheck="false" runat="server" class="editDiv" contenteditable="true" onkeydown="eDivkeydown(event);" oninput="Update1();" onclick="this.contentEditable='true';">&nbsp;</div> 
                   <asp:HiddenField ID="hiddencaretpos" runat="server" />
                    <asp:HiddenField ID="hiddeneDiv" runat="server" />
                    <asp:HiddenField ID="hiddendiv" runat="server" ClientIDMode="Static" />
                    <asp:HiddenField ID="HiddensFn" runat="server" />
                   <div class="fnt" id="caretPos">
                   </div>
                    <div>
                     <asp:LinkButton CssClass="fnt" OnClientClick="copyToClipboard1();return false;" ID="LinkButton2" runat="server">Copy Input to Clipboard</asp:LinkButton>
                    </div>
                </div>
                <script type="text/javascript">

                    //document.body.onkeyup = showCaretPos;
                    document.body.onmouseup = showCaretPos;

                </script>
            <p>
                <asp:TextBox CssClass="btn1px" ID="tbCopy" TextMode="MultiLine" Rows="0" runat="server"></asp:TextBox>
                <span id="success"></span>
            </p>
            </asp:TableCell>
            <asp:TableCell VerticalAlign="top">
            </asp:TableCell>
        </asp:TableRow>
    </asp:Table>
        </form>
    </div>
    <script type="text/javascript"> 
        document.getElementById('<% = eDiv.ClientID %>').focus();
        Update1();
    </script>
</body>
</html>
