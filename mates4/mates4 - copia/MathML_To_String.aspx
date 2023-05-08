<%@ Page Language="vb" ValidateRequest="false" AutoEventWireup="false"  MasterPageFile="~/Site2.Master" CodeBehind="MathML_To_String.aspx.vb" Inherits="mates4.MathML_To_String" %>


<asp:Content ID="Content1" ContentPlaceHolderID="head" runat="server">
	<title>Convert MathML to String</title>
    <meta name="viewport" content="width=device-width" />
	<script src="https://polyfill.io/v3/polyfill.min.js?features=es6"></script>
	<script id="MathJax-script" async="async" src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>


</asp:Content>   
<asp:Content id = "Content3" ContentPlaceHolderID="maincontent" runat="server">
    <div style="margin-left:15px;text-align:center;">
	<script>
        var s2 = "";
        var s2a = "";
        var s2b = "";
        var s2c = "";
        var s2d = "";
        function dsp() {
            var e = document.getElementById("examples");
            var val = e.options[e.selectedIndex].value;
            var s3
            switch (val) {
                case "one": { s3 = s2a; break; }
                case "two": { s3 = s2b; break; }
                case "three": { s3 = s2c; break; }
                case "four": { s3 = s2d; break; }
                case "five": { s3 = s2; break; }
            }
            document.getElementById('myTextArea').value = s3;
            convertMathMLToString(s3);
            document.getElementById('div1').innerHTML = s3; // display MathJax
            MathJax.typeset();
        }
        function dsp2() {
            var s3 = document.getElementById('myTextArea').value;
            convertMathMLToString(s3);
            document.getElementById('div1').innerHTML = s3; // display MathJax
            MathJax.typeset();
        }
        function ToMathML() {
            var s3 = document.getElementById('output').value;
            var s4 = convertStringToMathML(s3);
            document.getElementById('myTextArea').value = s4;
        }
    </script>

	<h2>MathML to/from String converter</h2>
	<p>Select an example or insert your MathML code/text and click on 'Convert'</p>
	<select id="examples" onchange="dsp();">
		<option value="one">Sample 1</option>
		<option value="two">Sample 2</option>
		<option value="three">Sample 3</option>
		<option value="four">Sample 4</option>
		<option value="five">Sample 5</option>
	</select>
	<br />
	<table><tr><td>
		<textarea id="myTextArea" spellcheck="false" cols="32" rows="20"></textarea>
		</td>
			   <td valign="middle">
				   <div>
				   	Convert MathML <br /> to Plain Text
				   	<input type="button" onclick="javascript:dsp2();" value=">>" />
				   </div>
				   <div>=================</div>
				   <div>
				   	<input type="button" onclick="javascript:ToMathML();" value="<<" />
				   	Convert Text <br />to MathML<br />
				   </div>
			   </td>
			   <td>
					<textarea spellcheck="false" id="output" cols="32" rows="20"></textarea>
			   </td></tr></table>
	<br />
	<div id="div1" style="font-size: 3em;margin-left:20px;">

	</div>
	<br />


	<script>

        s2 += "<math xmlns='http://www.w3.org/1998/Math/MathML' >\n"
        s2 += "  <mi>x</mi>\n"
        s2 += "  <mo>=</mo>\n"
        s2 += "  <mrow data-mjx-texclass='ORD'>\n"
        s2 += "    <mfrac>\n"
        s2 += "      <mrow>\n"
        s2 += "        <mo>&#x2212;</mo>\n"
        s2 += "        <mi>b</mi>\n"
        s2 += "        <mo>&#xB1;</mo>\n"
        s2 += "        <msqrt>\n"
        s2 += "          <msup>\n"
        s2 += "            <mi>b</mi>\n"
        s2 += "            <mn>2</mn>\n"
        s2 += "          </msup>\n"
        s2 += "          <mo>&#x2212;</mo>\n"
        s2 += "          <mn>4</mn>\n"
        s2 += "          <mi>a</mi>\n"
        s2 += "          <mi>c</mi>\n"
        s2 += "        </msqrt>\n"
        s2 += "      </mrow>\n"
        s2 += "      <mrow>\n"
        s2 += "        <mn>2</mn>\n"
        s2 += "        <mi>a</mi>\n"
        s2 += "      </mrow>\n"
        s2 += "    </mfrac>\n"
        s2 += "  </mrow>\n"
        s2 += "</math>\n"
        s2a = s2;
        s2 = "<math xmlns='http://www.w3.org/1998/Math/MathML' >\n"
        s2 += "   <mfrac>\n"
        s2 += "     <mi>a</mi>\n"
        s2 += "     <mi>b</mi>\n"
        s2 += "   </mfrac>\n"
        s2 += "   <mo>+</mo>\n"
        s2 += "   <mfrac>\n"
        s2 += "     <mrow>\n"
        s2 += "       <mi>a</mi>\n"
        s2 += "       <mo>+</mo>\n"
        s2 += "       <mi>b</mi>\n"
        s2 += "     </mrow>\n"
        s2 += "     <mi>c</mi>\n"
        s2 += "   </mfrac>\n"
        s2 += "   <mo>+</mo>\n"
        s2 += "   <mfrac>\n"
        s2 += "     <mi>c</mi>\n"
        s2 += "     <mrow>\n"
        s2 += "       <mi>a</mi>\n"
        s2 += "       <mo>+</mo>\n"
        s2 += "       <mi>b</mi>\n"
        s2 += "     </mrow>\n"
        s2 += "   </mfrac>\n"
        s2 += "   <mo>-</mo>\n"
        s2 += "   <mfrac>\n"
        s2 += "     <mrow>\n"
        s2 += "        <mi>c</mi>\n"
        s2 += "        <mo>+</mo>\n"
        s2 += "        <mi>d</mi>\n"
        s2 += "     </mrow>\n"
        s2 += "     <mrow>\n"
        s2 += "       <mi>a</mi>\n"
        s2 += "       <mo>+</mo>\n"
        s2 += "       <mi>b</mi>\n"
        s2 += "     </mrow>\n"
        s2 += "   </mfrac>\n"
        s2 += "</math>\n"
        s2b = s2;
        s2 = "<math xmlns='http://www.w3.org/1998/Math/MathML'>\n"
        s2 += "  <mfrac>\n"
        s2 += "    <mi>a</mi>\n"
        s2 += "    <mrow>\n"
        s2 += "       <mfrac>\n"
        s2 += "         <mrow>\n"
        s2 += "           <mi>b</mi>\n"
        s2 += "           <mo>-</mo>\n"
        s2 += "           <mi>c</mi>\n"
        s2 += "         </mrow>\n"
        s2 += "          <mi>d</mi>\n"
        s2 += "       </mfrac>\n"
        s2 += "    </mrow>\n"
        s2 += "  </mfrac>\n"
        s2 += "</math >\n"
        s2c = s2;
        s2 = "<math xmlns='http://www.w3.org/1998/Math/MathML'>\n"
        s2 += " <mi>x</mi>\n"
        s2 += " <mo>=</mo>\n"
        s2 += " <mi>y</mi>\n"
        s2 += " <mtext>,&nbsp;&nbsp;my comment&nbsp;&nbsp;</mtext>\n"
        s2 += "</math>\n"
        s2d = s2;
        s2 = "<math>\n"
        s2 += "  <mo>&int;</mo>\n"
        s2 += "    <mfenced separators=''>\n"
        s2 += "      <mn>5</mn>\n"
        s2 += "      <mi>x</mi>\n"
        s2 += "      <mo>+</mo>\n"
        s2 += "      <mn>2</mn>\n"
        s2 += "      <mi>sin</mi>\n"
        s2 += "      <mfenced separators=''>\n"
        s2 += "        <mi>x</mi>\n"
        s2 += "      </mfenced>\n"
        s2 += "    </mfenced>\n"
        s2 += "  <mi>dx</mi>\n"
        s2 += "</math>\n"


        //document.getElementById("myTextArea").value = s2;
        var e = document.getElementById("examples");
        e.selectedIndex = 0;


        var mo, mn, mi, text, sup, sfrac, sfenced, fracB, subB, sqrtB, row, sqrt, table, mtr, mtd, reMathML, sr;
        var sReplace = new Array(/\s*/gi, /&\#x22C5;/gi, /&\#8289;/gi, /&\#x2212;/gi, /&\#xB1;/gi,
            /<mo><\/mo>/gi, /&InvisibleTimes;/gi, /&PlusMinus;/gi, /&int;/gi,
            /&lt;/gi, /&gt;/gi, /&ne;/gi, /&divide;/gi, /&asymp;/gi, /&le;/gi, /&ge;/gi,
            /&prop;/gi, /&sum;/gi, /&prod;/gi, /&infin;/);
        var sReplaceBy = new Array("", "*", "", "-", "±",
            "", "*", "±", "∫",
            "<", ">", "≠", "÷", "≈", "≤", "≥",
            "∝", "∑", "∏", "∞");

        function convertMathMLToString(sMathML) {
            mo = "\\<mo[^\\>]*\\>(.*?)\\<\\/mo\\>";
            mn = "\\<mn[^\\>]*\\>(.*?)\\<\\/mn\\>";
            mi = "\\<mi[^\\>]*\\>(.*?)\\<\\/mi\\>";
            text = "\\<mtext[^\\>]*\>(.*?)\\<\\/mtext\>";
            sup = "\\<msup[^\>]*\\>(.*?)\\<\\/msup\\>";

            // Enssure there will be no nested <mfenced>, <mrow>, <mfrac>, <msup, <msqrt> inside <mfrac>...</mfrac>:
            sfrac = "<mfrac[^\\>]*>((?:(?!<mfenced|<\\/mfenced>|<mrow>|<\\/mrow>|<mfrac>|<\\/mfrac>|<msup>|<\\/msup>|<msqrt>|<\\/msqrt>).)*)<\\/mfrac>";

            // Enssure there will be no nested <mfenced>, <mrow>, <mfrac>, <msup, <msqrt> inside <msup>...</msup>:
            ssup = "<msup[^\\>]*>((?:(?!<mfenced|<\\/mfenced>|<mrow>|<\\/mrow>|<mfrac>|<\\/mfrac>|<msup>|<\\/msup>|<msqrt>|<\\/msqrt>).)*)<\\/msup>";

            // Enssure there will be no nested <mfenced>, <mrow>, <mfrac>, <msup, <msqrt> inside <msqrt>...</msqrt>:
            ssqrt = "<msqrt[^\\>]*>((?:(?!<mfenced|<\\/mfenced>|<mrow>|<\\/mrow>|<mfrac>|<\\/mfrac>|<msup>|<\\/msup>|<msqrt>|<\\/msqrt>).)*)<\\/msqrt>";

            // Enssure there will be no nested <mfenced>, <mrow>, <mfrac>, <msup, <msqrt> inside <mrow>...</mrow>:
            srow = "<mrow[^\\>]*>((?:(?!<mfenced|<\\/mfenced>|<mrow>|<\\/mrow>|<mfrac>|<\\/mfrac>|<msup>|<\\/msup>|<msqrt>|<\\/mrow>).)*)<\\/mrow>";

            // Enssure there will be no nested <mfenced>, <mrow>, <mfrac>, <msup, <msqrt> inside <mrow>...</mrow>:
            sfenced = "<mfenced[^\\>]*>((?:(?!<mfenced|<\\/mfenced>|<mrow>|<\\/mrow>|<mfrac>|<\\/mfrac>|<msup>|<\\/msup>|<msqrt>|<\\/mrow>).)*)<\\/mfenced>";
            fracB = "@([^@])+@";
            supB = "\ª([^\#])+ª";
            sqrtB = "\¡(([^\¡])+)¡";
            rowB = "\¿(([^\¿])+)¿";
            row = "<mrow[^\\>]*>((?:(?!<mrow>|<\\/mrow>).)*)<\\/mrow>";
            sqrt = "\\<msqrt[^\\>]*\\>(.*?)\\<\\/msqrt\\>";
            table = /\<mtable[^\>]*\>(.*?)\<\/mtable\>/g;
            mtr = /\<mtr[^\>]*\>(.*?)\<\/mtr\>/g;
            mtd = /\<mtd[^\>]*\>(.*?)\<\/mtd\>/g;
            sr = fracB + "|" + supB + "|" + sqrtB + "|" + rowB + "|" + supB + "|" + mo + "|" + mn + "|" + mi + "|" + text;
            reMathML = new RegExp(sr, 'g');
            var mcTxt = sMathML.match(text, 'g');
            if (mcTxt != null) {
                for (var i = 1; i < mcTxt.length; i++) {
                    var sT = mcTxt[i];
                    sT = sT.replace(/\n/g, "º");
                    sT = sT.replace(/\s/g, "·");
                    sMathML = sMathML.replace(mcTxt[i], sT);
                }
            }
            for (var i = 0; i < sReplace.length; i++) {
                sMathML = sMathML.replace(sReplace[i], sReplaceBy[i]);
            }
            var remove = /\<math[^>]*\>|\<\/math\>|\<mstyle[^>]*\>|\<\/mstyle\>/g;
            sMathML = sMathML.replace(remove, "");
            var table = sMathML.match(table);
            var s = "";
            if (table != null && table != "") {
                for (var iT = 0; iT < table.length; iT++) {
                    var tr = table[iT].match(mtr);
                    for (var i = 0; i < tr.length; i++) {
                        var td = tr[i].match(mtd);
                        for (var j = 0; j < td.length; j++) {
                            td[j] = fromInnerToOuter(td[j]);
                            var mc3 = td[j].match(reMathML, 'ig');
                            var pML = new ParseML;
                            s += pML.Evaluate(mc3);
                            if (j < td.length - 1) s += "|";
                        }
                        s += "\n";
                    }
                }
            }
            else {
                sMathML = fromInnerToOuter(sMathML);
                var mc3 = sMathML.match(reMathML, 'ig');
                var pML = new ParseML;
                s += pML.Evaluate(mc3);
            }
            document.getElementById("output").value = s;
        }
        function fromInnerToOuter(sMath) {
            var hallado = 1;
            while (hallado) { // go from most inner <mfrac>, <msup>, <msqrt>, <mrow> outwards
                hallado = false;
                while (1) {
                    var refrac = new RegExp(sfrac);
                    var frac = sMath.match(refrac);
                    if (frac == null || frac[0].substring(0, 1) == "@") break;
                    var s1 = ParseML.fractionOrExp(frac[1], 0);
                    sMath = sMath.replace(frac[0], "@" + s1 + "@");
                    hallado = true;
                }
                while (1) {
                    var refrac = new RegExp(ssup);
                    var frac = sMath.match(refrac);
                    if (frac == null || frac[0].substring(0, 1) == "#") break;
                    var s1 = ParseML.fractionOrExp(frac[1], 1);
                    sMath = sMath.replace(frac[0], "ª" + s1 + "ª");
                    hallado = true;
                }
                while (1) {
                    var refrac = new RegExp(ssqrt);
                    var frac = sMath.match(refrac);
                    if (frac == null || frac[0].substring(0, 1) == "¡") break;
                    var s1 = ParseML.fractionOrExp(frac[1], 2);
                    sMath = sMath.replace(frac[0], "¡" + s1 + "¡");
                    hallado = true;
                }
                while (1) {
                    var refrac = new RegExp(srow);
                    var frac = sMath.match(refrac);
                    if (frac == null || frac[0].substring(0, 1) == "¿") break;
                    var s1 = ParseML.fractionOrExp(frac[1], 2);
                    sMath = sMath.replace(frac[0], "¿" + s1 + "¿");
                    hallado = true;
                }
                while (1) {
                    var refrac = new RegExp(sfenced);
                    var frac = sMath.match(refrac);
                    if (frac == null) break;
                    var s1 = "<mo>(</mo>" + frac[1] + "<mo>)</mo>";
                    sMath = sMath.replace(frac[0], s1);
                    hallado = true;
                }
            }
            return sMath;
        }

        class ParseML {
            constructor() {
                this.m = null;
                this.mc = null;
                this.iC = 0;
            }

            //m, mc;
            //iC = 0;
            Advance() {
                this.iC += 1;
                if (this.iC < this.mc.length)
                    this.m = this.mc[this.iC];
                else
                    this.m = /./.exec(" ");
            }

            Evaluate(mc1) {
                this.iC = 0;
                if (mc1.length == 0) return "";
                this.mc = new Array();
                for (var i = 0; i < mc1.length; i++)
                    this.mc.push(mc1[i]);
                for (var i = 0; i < this.mc.length; i++) {
                    if (i < this.mc.length - 1) {
                        var m1a = new RegExp(mn).exec(this.mc[i]);
                        var m1b = new RegExp(mi).exec(this.mc[i]);
                        var m2a = new RegExp(mn).exec(this.mc[i + 1]);
                        var m2b = new RegExp(mi).exec(this.mc[i + 1]);
                        var m2c = new RegExp(/@/).exec(this.mc[i + 1]);
                        if ((m1a != null || m1b != null) &&
                            (m2a != null || m2b != null || m2c != null)) {
                            this.insertOperator("*", i);
                            i += 1;
                        }

                    }
                }
                this.m = this.mc[0];
                return this.AddSubs();
            }
            insertOperator(sOp, im) {
                var mc2 = new Array();
                for (var i = 0; i < this.mc.length; i++)
                    mc2.push(this.mc[i]);
                mc2.push(null);
                for (var i = this.mc.length; i >= im + 2; i--)
                    mc2[i] = this.mc[i - 1];
                mc2[im + 1] = "<mo>" + sOp + "</mo>";
                this.mc = mc2;
            }
            AddSubs() {
                var s = this.MultDiv();
                var br = 1;
                while (br) {
                    var m1 = new RegExp(mo).exec(this.m);
                    if (m1 == null) break;
                    switch (m1[1]) {
                        case "+": {
                            this.Advance();
                            s += "+" + this.MultDiv();
                            break;
                        }
                        case "-": {
                            this.Advance();
                            s += "-" + this.MultDiv();
                            break;
                        }
                        case "±": {
                            this.Advance();
                            s += "±" + this.MultDiv();
                            break;
                        }
                        default: { br = 0; break; }
                    }
                }
                var m1 = new RegExp(mo).exec(this.m);
                if (m1 != null && m1[1] != ")") { // && m1[1].match(/\=|\<|\>/))
                    s += m1[1];
                    this.Advance();
                    s += this.AddSubs();
                }
                return s;
            }
            MultDiv() {
                var s = this.Pow();
                while (1) {
                    var m1 = new RegExp(mo).exec(this.m);
                    if (m1 != null && m1[1] == "*") {
                        this.Advance();
                        s += "*" + this.Pow();
                    }
                    else {
                        m1 = new RegExp(fracB).exec(this.m);
                        if (m1 != null) {
                            s += m1[0].replace(/@/g, "");
                            this.Advance();
                        }
                        else
                            break;
                    }
                }
                return s;
            }
            Pow() {
                var s = this.Token();
                while (1) {
                    var m1 = new RegExp(supB).exec(this.m);
                    if (m1 == null) break;
                    s += m1[0].replace(/\ª/g, "");
                    this.Advance();
                }
                return s;
            }
            Token() {
                var s = "";
                while (1) {
                    s += this.TokenB();
                    var m1 = new RegExp(text).exec(this.m);
                    if (m1 != null && m1.index == 0) {
                        m1[1] = m1[1].replace(/º/g, "\n");
                        m1[1] = m1[1].replace(/·/g, " ");
                        s += "#" + m1[1] + "#";
                        this.Advance();
                    }
                    else break;
                }
                return s;
            }
            TokenB() {
                var s = "";
                var m1 = new RegExp(mo).exec(this.m);
                if (m1 != null && m1.index == 0 && m1[1] == "(") {
                    this.Advance();
                    s += "(" + this.AddSubs() + ")";
                    this.Advance();
                }
                m1 = new RegExp(mo).exec(this.m);
                if (m1 != null && m1.index == 0 && m1[1] == "-") {
                    s += "-";
                    this.Advance();
                }
                m1 = new RegExp(text).exec(this.m);
                if (m1 != null && m1.index == 0) {
                    s += m1[1];
                    this.Advance();
                }
                m1 = new RegExp(rowB).exec(this.m);
                if (m1 != null && m1.index == 0) {
                    s += m1[1];
                    this.Advance();
                }
                m1 = new RegExp(sqrtB).exec(this.m);
                if (m1 != null && m1.index == 0) {
                    s += "sqrt(" + m1[1] + ")";
                    this.Advance();
                }
                m1 = new RegExp(mi).exec(this.m);
                if (m1 != null && m1.index == 0) {
                    s += m1[1];
                    this.Advance();
                }
                m1 = new RegExp(mn).exec(this.m);
                if (m1 != null && m1.index == 0) {
                    s += m1[1];
                    this.Advance();
                }
                m1 = new RegExp(mo).exec(this.m);
                if (m1 != null && m1.index == 0 && m1[1] == "(") {
                    this.Advance();
                    s += "(" + this.AddSubs() + ")";
                    this.Advance();
                }

                return s;
            }
            static fractionOrExp(m1, fracExpSqrt) {
                var s = "";
                var mRow = m1.match(/\<mrow[^\>]*\>(.*?)\<\/mrow\>/gi);
                var sNum, sDen;
                if (mRow != null) {
                    mRow[0] = mRow[0].replace(/<mrow[^\>]*>|\<\/mrow>/gi, "");
                    if (mRow.length > 1)
                        mRow[1] = mRow[1].replace(/<mrow[^\>]*>|\<\/mrow>/gi, "");
                }
                if (mRow != null && mRow.length == 2) {
                    var pML = new ParseML;
                    var tNum = mRow[0].match(reMathML);
                    sNum = pML.Evaluate(tNum);
                    pML = new ParseML;
                    mRow[1] = mRow[1].replace(/<mrow(^\>)>|<\/mrow>/gi, "")
                    var tDen = mRow[1].match(reMathML);
                    sDen = pML.Evaluate(tDen);
                } else {
                    if (mRow != null && mRow.length == 1 && m1.substring(0, 6) == "<mrow>") {
                        var pML = new ParseML;
                        var tNum = mRow[0].match(reMathML);
                        sNum = pML.Evaluate(tNum);
                        pML = new ParseML;
                        var pos = m1.indexOf("</mrow>");
                        var tDen = m1.substring(pos + 7).match(reMathML);
                        sDen = pML.Evaluate(tDen);
                    }
                    else {
                        if (mRow != null && mRow.length == 1) {
                            var pos = m1.indexOf("<mrow>");
                            var pML = new ParseML;
                            var tNum = m1.substring(0, pos).match(reMathML);
                            sNum = pML.Evaluate(tNum);
                            pML = new ParseML;
                            var tDen = mRow[0].match(reMathML);
                            sDen = pML.Evaluate(tDen);
                        }
                        else {
                            var pML = new ParseML;
                            var tNumDen = m1.match(reMathML);
                            if (fracExpSqrt == 2) {
                                var s1 = pML.Evaluate(tNumDen);
                                return s1;
                            }
                            else {
                                sNum = pML.Evaluate(tNumDen[0].match(reMathML));
                                pML = new ParseML;
                                sDen = pML.Evaluate(tNumDen[1].match(reMathML));
                            }
                        }
                    }
                }
                var m1 = new RegExp(/\-\+/g);
                if (sNum.length > 1 && sNum.substring(1).match(/\-|\+/) != null) {
                    if (sNum.substring(0, 1) != "(" || sNum.substring(sNum.length - 1) != ")")
                        sNum = "(" + sNum + ")";
                }
                m1 = new RegExp(/\-\+/g);
                if (sDen.length > 1 && sDen.substring(1).match(/\-|\+|\*/) != null) {
                    if (sDen.substring(0, 1) != "(" || sDen.substring(sDen.length - 1) != ")")
                        sDen = "(" + sDen + ")";
                }
                if (fracExpSqrt == 0) {
                    s += sNum + "/" + sDen;
                }
                else {
                    if (fracExpSqrt == 1)
                        s += sNum + "^" + sDen;
                }
                return s;
            }

        }

        function convertStringToMathML(s) {
            var vReplace = ["/\s*/g", "/\[/g", "/\]/g", "/\{/g", "/\}/g",
                ":", "÷", "–", 0xBC, 0xBD, 0xBE, 0xB2, 0xB3,
                "×", "arcsin", "arccos", "arctan", "\*\*",
                "arccot", "arccsc", "arcsec", "√"]
            var vReplaceBy = ["", "(", ")", "(", ")",
                "/", "/", "-", "+(1/4)", "+(1/2)", "+(3/4)", "^2", "^3",
                "*", "asin", "acos", "atan", "^",
                "acot", "acsc", "asec", "sqr"]

            mn = "(([0-9]{1,3},[0-9]{3}(?![0-9])|[0-9])+\\.?[0-9]*|[0-9]*\\.?[0-9]+)([eE][-+]?[0-9]+)?";
            mo = "\\-|\\=|\\<|\\>|∫|≠|≈|≤|≥|\\+|\\±|\\*|\\/|\\^|∞|nand|mod|and|nor|xor|not|or|%|!|\\(|\\)";
            mtext = "#(.*?)#"; // comments enclosed inside #:  # my comment #
            mi = "[_a-zA-Z]\\w*";
            //var sAny = "(?<any>[^\\s*\\,\\.])+|(?<any>\\,|\\.)+";
            var patt = mn + "|" + mo + "|" + mi + "|" + mtext;
            var reTot = new RegExp(patt, 'ig');
            for (var i = 0; i < vReplace.length; i++)
                s = s.replace(vReplace[i], vReplaceBy[i]);
            var mc1 = s.match(reTot);
            var pS = new ParseStringToML;
            s = pS.Evaluate(mc1);
            //s = s.Replace("(?<tag><[^/]*/[^>]+>)", "${tag}" + vbCrLf)
            var s1 = "<math xmlns='http://www.w3.org/1998/Math/MathML'>\n";
            s = s1 + s + "</math>\n";
            return s;
        }

        class ParseStringToML {
            constructor() {
                this.m = null;
                this.mc = null;
                this.iC = 0;
            }
            Advance() {
                this.iC += 1;
                if (this.iC < this.mc.length)
                    this.m = this.mc[this.iC];
                else
                    this.m = /./.exec(" ");
            }
            Evaluate(mc1) {
                this.iC = 0;
                if (mc1.length == 0) return "";
                this.mc = new Array();
                for (var i = 0; i < mc1.length; i++)
                    if (mc1[i]) this.mc.push(mc1[i]);
                for (var i = 0; i < this.mc.length; i++) {
                    if (i < this.mc.length - 1) {
                        var m1a = new RegExp(mn).exec(this.mc[i]);
                        var m1b = new RegExp(mi).exec(this.mc[i]);
                        var m2a = new RegExp(mn).exec(this.mc[i + 1]);
                        var m2b = new RegExp(mi).exec(this.mc[i + 1]);
                        if (((m1a != null && m1a.index == 0) || (m1b != null && m1b.index == 0)) &&
                            ((m2a != null && m2a.index == 0) || (m2b != null && m2b.index == 0))) {
                            this.insertOperator("*", i);
                            i += 1;
                        }

                    }
                }
                this.m = this.mc[0];
                return this.AddSubs();
            }
            insertOperator(sOp, im) {
                var mc2 = new Array();
                for (var i = 0; i < this.mc.length; i++)
                    mc2.push(this.mc[i]);
                mc2.push(null);
                for (var i = this.mc.length; i >= im + 2; i--)
                    mc2[i] = this.mc[i - 1];
                mc2[im + 1] = sOp.match(/(?<op>\*|\^)/);
                this.mc = mc2;
            }
            AddSubs() {
                var s = this.MultDiv();
                var br = 1;
                while (br) {
                    var m1 = new RegExp(mo).exec(this.m);
                    if (m1 == null) break;
                    switch (m1[0]) {
                        case "+": {
                            this.Advance();
                            s += "<mo>+</mo>\n" + this.MultDiv();
                            break;
                        }
                        case "-": {
                            this.Advance();
                            s += "<mo>-</mo>\n" + this.MultDiv();
                            break;
                        }
                        case "±": {
                            this.Advance();
                            s += "<mo>±</mo>\n" + this.MultDiv();
                            break;
                        }
                        default: { br = 0; break; }
                    }
                }
                var m2 = new RegExp(mo).exec(this.m);
                if (m2 != null && m2[0].match(/\=/)) {
                    s += "<mo>" + m2[0] + "</mo>\n"; // = sign
                    this.Advance();
                    s += this.AddSubs();
                } else {
                    if (m2 != null) {
                        if (m2[0] == "∫") {
                            s += "<mo>&int;</mo>";
                            this.Advance();
                            s += this.AddSubs();
                            this.Advance();
                            s += this.AddSubs();
                        }
                        else {
                            for (var i = 1; i < sReplaceBy.length; i++) { // all other operators
                                if (m2[0] == sReplaceBy[i]) {
                                    s += "<mo>" + sReplace[i].source.replace("\\", "") + "</mo>\n";
                                    this.Advance();
                                    s += this.AddSubs();
                                    break;
                                }
                            }
                        }
                    }
                }

                return s;
            }
            MultDiv() {
                var s = this.Pow();
                while (1) {
                    var m1 = new RegExp(mo).exec(this.m);
                    if (m1 != null && m1[0] == "*") {
                        this.Advance();
                        s += "<mo>*</mo>\n" + this.Pow();
                    }
                    else {
                        m1 = new RegExp(mo).exec(this.m);
                        if (m1 != null && m1[0] == "/") {
                            this.Advance();
                            var sDen = this.Pow();
                            var mc2 = s.match(/<mo>.*?<\/mo>|.+/gi);
                            if (mc2 != null && mc2[0] == "<mo>(</mo>" && mc2[mc2.length - 1] == "<mo>)</mo>") {
                                var s1 = "";
                                for (var i = 1; i < mc2.length - 1; i++)
                                    s1 += mc2[i] + "\n";
                                if (s1.indexOf(")") == -1) s = s1;
                            }
                            s = "<mrow>" + s + "</mrow>\n";
                            mc2 = sDen.match(/<mo>.*?<\/mo>|.+/gi);
                            if (mc2 != null && mc2[0] == "<mo>(</mo>" && mc2[mc2.length - 1] == "<mo>)</mo>") {
                                var s1 = "";
                                for (var i = 1; i < mc2.length - 1; i++)
                                    s1 += mc2[i] + "\n";
                                if (s1.indexOf(")") == -1) sDen = s1;
                            }
                            /*                            var mc3 = sDen.match(/<m/gi);
                                                        if (mc3 != null && mc2.length > 1)*/
                            sDen = "<mrow>" + sDen + "</mrow>\n";
                            s = "<mfrac>" + s + sDen + "</mfrac>\n";
                            //if (mc2.length > 1 && mc3.length > 1)
                            s = "<mrow>" + s + "</mrow>\n";
                        }
                        else
                            break;
                    }
                }
                return s;
            }
            Pow() {
                var s = this.Token();
                while (1) {
                    var m1 = new RegExp(mo).exec(this.m);
                    if (m1 != null && m1[0] == "^") {
                        var base = s;
                        this.Advance();
                        var exp = this.Token();
                        var mc2 = base.match(/<m/gi);
                        if (mc2 != null && mc2.length > 1)
                            base = "<mrow>" + base + "</mrow>\n";
                        var mc3 = exp.match(/<m/gi);
                        if (mc3 != null && mc2.length > 1)
                            exp = "<mrow>" + exp + "</mrow>\n";
                        s = "<msup>" + base + exp + "</msup>\n";
                    }
                    else break;
                }
                return s;
            }
            Token() {
                var s = "";
                while (1) {
                    s += this.TokenB();
                    var m1 = new RegExp(mtext).exec(this.m);
                    if (m1 != null && m1.index == 0) {
                        s += "<mtext>" + m1[1] + "</mtext>";
                        this.Advance();
                    }
                    else break;
                }
                return s;
            }
            TokenB() {
                var s = "";
                var m1 = new RegExp(mo).exec(this.m);
                if (m1 != null && m1.index == 0 && m1[0] == "(") {
                    this.Advance();
                    s += "<mo>(</mo>\n" + this.AddSubs() + "<mo>)</mo>\n";
                    this.Advance();
                }
                //m1 = new RegExp(mo).exec(this.m);
                if (m1 != null && m1.index == 0 && m1[0] == "-") {
                    s += "<mo>" + m1[0] + "</mo>\n"; // change sign
                    this.Advance();
                }
                m1 = new RegExp(mn).exec(this.m);
                if (m1 != null && m1.index == 0) {
                    s += "<mn>" + m1[0] + "</mn>\n";
                    this.Advance();
                    return s;
                }
                m1 = new RegExp(mi).exec(this.m);
                if (m1 != null && m1.index == 0) {
                    if (m1[0] == "sqr" || m1[0] == "sqrt") {
                        this.Advance();
                        var s1 = "";
                        if (this.m == "(") {
                            this.Advance();
                            s1 = this.AddSubs();
                        }
                        else {
                            s1 = this.AddSubs();
                        }
                        s += "<msqrt>" + s1 + "</msqrt>\n";
                    }
                    else
                        s += "<mi>" + m1[0] + "</mi>\n";
                    this.Advance();
                    //return s;
                }
                m1 = new RegExp(mo).exec(this.m);
                if (m1 != null && m1.index == 0 && m1[0] == "(") {
                    this.Advance();
                    s += "<mo>(</mo>\n" + this.AddSubs() + "<mo>)</mo>\n";
                    this.Advance();
                }
                return s;
            }

        }

    </script>
        <script>
            var e = document.getElementById("examples");
            e.selectedIndex = 0;
            dsp();
        </script>
</div>
</asp:Content>
