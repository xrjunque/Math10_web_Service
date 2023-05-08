
Partial Public Class rootfinder
    Inherits System.Web.UI.Page



    Private textbox2 As New LiteralControl
    Shared max, min, fmin, fmax As Double
    Private resuelto As Boolean
    Shared redondeix As Boolean
    'Public Shared roots(2)() As Double
    Public Shared roots()() As Double
    Shared us As New System.Globalization.CultureInfo("en-US")
    Shared aprox As String
    Shared msgs(99) As String
    'Dim poly As New Polycalc
    'Dim sIdioma As String

    Dim nID As Int32

    Dim mP As New matrixParser
    Dim sQuery As String = ""
    Dim cfg As Config = Config.cfg
    Private Sub Page_PreRender(sender As Object, e As EventArgs) Handles Me.PreRender
        'Introducir aquí el código de usuario para inicializar la página
        Try
            C_sessionWeb.contador(Page, Request, Response)
        Catch ex As Exception

        End Try

        Try
            nID = C_sessionWeb.getID(Page)

            C_sessionWeb.sProvieneDe(nID) = Request.Url.AbsoluteUri

        Catch ex As Exception

        End Try



        If Not Page.IsPostBack Then
            If Len(Trim(TextBox1.Text)) Then
                TextBox1.Text = LCase(TextBox1.Text)
            End If
            Dim tb1 As String = Request.Params("tb1")
            If Not tb1 Is Nothing AndAlso Len(tb1) Then
                TextBox1.Text = tb1
            End If
            'Session.Add("tb1", "")
            'Session.Add("tb3", "")
            Me.Form.Attributes.Add("onkeypress",
                                  "if(event.keyCode==13) document.forms[0].elements['" + Me.btnCalculate.ClientID + "'].click();")
            'TextBox1.Attributes.Add("onfocus", _
            '           "this.value='';")
            Try
                Dim a = Request.Params("a")
                Dim b = Request.Params("b")
                Dim c = Request.Params("c")
                If a IsNot Nothing AndAlso b IsNot Nothing AndAlso c IsNot Nothing Then
                    Dim e2 As String = a + "*x^2+" + b + "*x^1+" + c
                    e2 = Replace(e2, "+-", "-")
                    sQuery = e2
                    TextBox1.Text = e2
                    btnCalculate_Click(Nothing, Nothing)
                    Exit Sub
                End If
            Catch ex As Exception

            End Try
            Try
                Dim tbox1 = Request.Params("tb1")
                Dim rnd = Request.Params("rnd")
                Dim frac = Request.Params("frac")
                If tbox1 IsNot Nothing Then
                    TextBox1.Text = tbox1
                    sQuery = tbox1
                    If rnd IsNot Nothing Then
                        Boolean.TryParse(rnd, chkRounding.Checked)
                    End If
                    If frac IsNot Nothing Then
                        Boolean.TryParse(frac, chkFractions.Checked)
                    End If
                    btnCalculate_Click(Nothing, Nothing)
                    Exit Sub
                End If
            Catch ex As Exception

            End Try
        Else
            'TextBox1.Attributes.Add("onfocus", _
            '          "SetCursorToTextEnd(" + TextBox1.ClientID + ");")
            'TextBox1.Focus()
        End If

        Dim e1 As String = "" ' _
        '"function SetCursorToTextEnd(ctrl) " + vbCrLf + _
        '"{" + vbCrLf + _
        '" if(ctrl.setSelectionRange) {" + vbCrLf + _
        '" ctrl.setSelectionRange(ctrl.value.length, ctrl.value.length);" + vbCrLf + _
        '" }" + vbCrLf + _
        '" else if (ctrl.createTextRange) {" + vbCrLf + _
        '" var range = ctrl.createTextRange();" + vbCrLf + _
        '" range.moveStart('character', ctrl.value.length);" + vbCrLf + _
        '" range.select();" + vbCrLf + _
        '"//  ctrl.focus();" + vbCrLf + _
        '" }        " + vbCrLf + _
        '"}" + vbCrLf
        'ClientScript.RegisterStartupScript(GetType(String), "caret", e1, True)
        Try
            e1 =
        "function AddText(ctrl,str) " + vbCrLf +
        "{" + vbCrLf +
        " ctrl.value+=str;" + vbCrLf +
        "}" + vbCrLf
            ClientScript.RegisterClientScriptBlock(GetType(String), "addText", e1, True)
            e1 =
        "function clearText(ctrl) " + vbCrLf +
        "{" + vbCrLf +
        " ctrl.value='';" + vbCrLf +
        "}" + vbCrLf
            ClientScript.RegisterClientScriptBlock(GetType(String), "clearText", e1, True)
            e1 =
        "function CE(ctrl) " + vbCrLf +
        "{" + vbCrLf +
        " if(ctrl.value.length==1) ctrl.value='';" + vbCrLf +
        " else{if(ctrl.value.length>1) ctrl.value=ctrl.value.substring(0,ctrl.value.length-1);}" + vbCrLf +
        "}" + vbCrLf
            ClientScript.RegisterClientScriptBlock(GetType(String), "CE", e1, True)

            Dim vCtr() As Control = {btn1Q, btn3Q, btnLP, btnRP, btnOneHalf, btnCE,
                                    btnCube, btnx, btnPow, btnDiv, btnMult, btnMinus,
                                    btnSquare, btnSqrt, btn7, btn8, btn9, btnPlus,
                                    btnOne, btnHexa, btn4, btn5, btn6, btnEqual,
                                    btnZero, btnOctal, btn1, btn2, btn3, btnEqualZero,
                                    btnMult2, btnDiv2, btnBin, btn0, btnDot, btnExp, btnPi}
            For Each ctr In vCtr
                'CType(ctr, WebControl).Width = "38"
                'CType(ctr, WebControl).Height = "22"
                CType(ctr, WebControl).Attributes.Add(
                    "onclick", "AddText(" + TextBox1.ClientID + ",'" +
                    CType(ctr, Button).Text + "');return false;")
            Next

            'btnx.Attributes.Add("onclick", "AddText(" + TextBox1.ClientID + ",'x');return false;")
            'btnx2.Attributes.Add("onclick", "AddText(" + TextBox1.ClientID + ",'x^2');return false;")
            'btnx3.Attributes.Add("onclick", "AddText(" + TextBox1.ClientID + ",'x^3');return false;")
            'btnx4.Attributes.Add("onclick", "AddText(" + TextBox1.ClientID + ",'x^4');return false;")
            btnCE.Attributes.Add("onclick", "CE(" + TextBox1.ClientID + ");return false;")
            btnClear.Attributes.Add("onclick", "clearText(" + TextBox1.ClientID + ");return false;")
            Inicio("en")
        Catch ex As Exception

        End Try

    End Sub
    Private Function go() As String
        Dim bGoToPolyCalc As Boolean = True
        Try
            'Session.Timeout = 3
            Dim e1 As String = TextBox1.Text
            If Len(e1) = 0 Then
                logFile(Nothing, "n/a: input is empty. ", "")
                Me.Lit3.Text = msg8.msg(8)
                Return ""
            End If
            e1 = Replace(e1, Chr(10), "")
            e1 = Replace(e1, Chr(13), "")
            e1 = Trim(e1)
            If Len(e1) = 0 Then
                logFile(Nothing, "n/a: input is empty. ", "")
                Me.Lit3.Text = msg8.msg(8)
                Return ""
            End If
            Lit3.Text = ""
            'poly.redondeo = Me.chkRounding  .Checked
            'poly.haydsp = False
            If InStr(e1, ";") = 0 Then
                If InStr(e1, "=") = 0 Then
                    TextBox1.Text += " = 0"
                Else
                    Dim e2() As String = Split(e1, "=")
                    If Len(e2(0)) = 0 Then
                        TextBox1.Text = e2(1) + " = 0"
                    End If
                End If
            Else
            End If
            logFile(Nothing, TextBox1.Text, " redondeo=" + chkRounding.Checked.ToString())
            Dim e3 As String = TextBox1.Text
            e3 = LCase(e3)
            'e3 = Regex.Replace(e3, "\s+", "")

            ' The following settings have to do
            ' with how the input data will be parsed:
            cfg.bCaseSensitive = False
            cfg.bIgnoreSpaces = True
            'If chkDegrees.Checked Then
            '    cfg.degreesType = MathGlobal8.degreesType.centesimal
            'Else

            'End If

            ' The next Configuration settings affect
            ' on how the output data will be retrieved:
            cfg.bRounding = False ' chkRounding.Checked
            cfg.bEngNotation = True
            cfg.bFractions = chkFractions.Checked
            cfg.bDetail = False
            cfg.Base = MathGlobal8.outputBase.decimal
            cfg.timeOutms = 60 * 1000
            cfg.ticksStart = Now.Ticks
            cfg.doTimeOut = timeout.whenTimeIsOver

            'Dim mP As New matrixParser
            Dim nComas As Int32 = Regex.Matches(e3, "\,").Count
            mP.cfg = cfg
            Dim oVars8 As New VarsAndFns(cfg)
            Dim expr As Expression = Nothing
            bGoToPolyCalc = False
            If True Then
                Try
                    Dim ve3() As String = Split(e3, "=")
                    If ve3.Length > 2 Then
                        bGoToPolyCalc = True
                        Exit Try
                    ElseIf ve3.Length = 2 Then
                        e3 = ve3(0) + "-(" + ve3(1) + ")"
                    End If
                    expr = Expression.ParseExpression(e3)
                    If Not expr.IsPolynomial Then
                        bGoToPolyCalc = True
                        'ElseIf expr.getPolynomial.var.Length > 1 Then
                        '    bGoToPolyCalc = True
                        '    Dim vvar() As String = expr.getPolynomial.var
                        '    Dim j1 As Int32
                        '    For j1 = 0 To vvar.Length - 1
                        '        mP.parse(vvar(j1), "")
                        '        If Not mP.ret.curExpr.IsPolynomial Then
                        '            Exit For
                        '        End If
                        '    Next
                        '    If j1 >= vvar.Length AndAlso expr.getPolynomial.PolyResto Is Nothing Then
                        '        bGoToPolyCalc = False
                        '    End If
                        'ElseIf expr.getPolynomial.PolyResto IsNot Nothing Then
                        '    bGoToPolyCalc = True
                        'Else
                        '    bGoToPolyCalc = False
                    End If
                    'If Not bGoToPolyCalc Then
                    '    e3 += "= 0"
                    'End If
                Catch ex As Exception
                    bGoToPolyCalc = True
                End Try
                If bGoToPolyCalc Then Exit Try
            End If
            mP.parse(e3, "", oVars8, cfg)
            If mP.ret Is Nothing Then
                Lit3.Text = "n/a "
                bGoToPolyCalc = True
                logFile(Nothing, Lit3.Text, "")
                Exit Try
            End If
            If nComas AndAlso mP.ret.isMatrix Then
                Lit3.Text = "n/a " + msg8.msg(23)
                bGoToPolyCalc = False
                logFile(Nothing, Lit3.Text, "")
                Exit Try
            End If
            Dim t As String = ""
            cfg.doTimeOut = timeout.never
            If mP.errMsg.Length Then
                Lit3.Text = mP.errMsg
                Exit Try
            ElseIf Not mP.ret.curExpr.IsPolynomial Then
                Response.Clear()
                bGoToPolyCalc = True
                Exit Try
            Else
                Dim vVar(-1) As String
                mP.ret.curExpr.getAllVars(vVar)
                If vVar.Length <> 1 Then
                    Response.Clear()
                    bGoToPolyCalc = True
                    Exit Try
                End If
                Dim Pa As Polynomial = Nothing
                If mP.soe IsNot Nothing Then
                    Pa = mP.soe.getPolynomial
                ElseIf mP.ret.curExpr IsNot Nothing Then
                    Pa = mP.ret.curExpr.getPolynomial
                    Pa.roots = Polynomial.opRoots(Pa)
                Else
                    bGoToPolyCalc = True
                    Exit Try
                End If
                'Trace.Write("len=" + Pa.roots.Length)
                If Pa Is Nothing OrElse Pa.var.Length < 1 OrElse Pa.PolyResto IsNot Nothing Then
                    Response.Clear()
                    bGoToPolyCalc = True
                    Exit Try
                    'Response.Redirect("Polycalc.aspx?tb1=" + Web.HttpUtility.UrlEncode(TextBox1.Text) + "&tb3=", False)
                    'Response.End()
                ElseIf Pa.var.Length > 1 Then
                    Lit3.Text = msg8.num(13) + ". There are " +
                       Pa.var.Length.ToString + " variables: " ' n/a 
                    Lit3.Text += "{"
                    For i As Int32 = 0 To Pa.var.Length - 1
                        Lit3.Text += "'" + Pa.var(i) + "'"
                        If i < Pa.var.Length - 1 Then
                            Lit3.Text += ", "
                        End If
                    Next
                    Lit3.Text += "}"
                    logFile(Nothing, Lit3.Text, "")
                    Exit Try
                    'Response.Clear()
                    'Response.Redirect("Polycalc.aspx?tb1=" + Web.HttpUtility.UrlEncode(TextBox1.Text) + "&tb3=", False)
                End If
                Response.Clear()
                'Dim Va As New Vector(Pa)
                'If Pa.PolyResto IsNot Nothing Then
                '    bGoToPolyCalc = True
                '    Exit Try
                'End If
                C_sessionWeb.sfn(nID) = ""
                'Dim reOpRoots As retOpRoots = Polynomial.opRoots(Va)
                'If reOpRoots Is Nothing Then
                '    Lit3.Text = msg8.num(13) ' n/a
                '    logFile(Nothing, Lit3.Text, "")
                '    Exit Try
                'End If
                'If reOpRoots.mtx.vVect(0).vPoly.Length Then
                'Dim bRnd As Boolean = cfg.bRounding
                'MathGlobal8.bUseRounding = Me.chkRounding.Checked
                cfg.doTimeOut = timeout.never
                cfg.bRounding = chkRounding.Checked
                mP.ret.diffType = retMtx.diffResponse.resto
                Dim e4 As String = mP.ret.toStringRetVal(cfg) + "<br />"
                'MathGlobal8.bUseRounding = bRnd
                Dim bShownIsReal As Boolean = False
                Dim bShownIsCjo As Boolean = False
                Dim An As New Complex(Pa.An)
                Dim s00 As String = ""
                Dim s01 As String = ""
                Dim s10 As String = ""
                Dim s11 As String = ""
                Dim cjo() As Complex = Nothing ' mP.soe.resultValues ' Complex.sortRoots( reOpRoots.cjo)
                If Pa.roots IsNot Nothing Then
                    cjo = Pa.roots.cjo
                Else
                    cjo = mP.retCjo
                End If
                Dim u As Int32 = 12
                Dim h As Int32 = u * cjo.Length

                Dim bIsPoly As Boolean = True
                Dim bOneOrMoreVars As Boolean = (Pa.var.Length >= 1)
                Dim bOneVar As Boolean = (Pa.var.Length = 1)

                'Dim Pa As Polynomial = Nothing
                Dim sFn As String = ""
                Dim sF2 As String = ""
                'Dim reOpRoots As retOpRoots = Nothing
                If bIsPoly AndAlso bOneVar Then

                    'Pa = mP.ret.curExpr.getPolynomial
                    sFn = Pa.ToString
                    C_sessionWeb.sfn(nID) = sFn
                    C_sessionWeb.sf3(nID) = "0"
                    If Pa.PolyResto Is Nothing Then
                        If Pa IsNot Nothing Then
                            bGoToPolyCalc = False
                        End If
                        Dim min As Double = 1000
                        Dim max As Double = -1000
                        If cjo.Length > 1 Then
                            For i = 0 To cjo.Length - 1
                                If cjo(i).IsReal Then
                                    If min > cjo(i).pRe.ToDouble Then
                                        min = cjo(i).pRe.ToDouble
                                    End If
                                    If max < cjo(i).pRe.ToDouble Then
                                        max = cjo(i).pRe.ToDouble
                                    End If
                                End If
                            Next
                            If min < max Then
                                C_sessionWeb.xi(nID) = min
                                C_sessionWeb.xf(nID) = max
                            End If
                        End If
                    End If
                End If
                If bGoToPolyCalc Then
                    Exit Try
                End If
                C_sessionWeb.yAuto(nID) = 1
                C_sessionWeb.sf3(nID) = "0"
                sF2 = Pa.ToStringFactors(cfg)
                t = ""
                t += "I.   Input equation.(P(" + Pa.var(0) + ")=0): " + TextBox1.Text
                If InStr(t, "=") = 0 Then
                    t += " = 0"
                End If
                t += "<br />"
                t += "II.  Interpreted equation..: " + Pa.ToString + " = 0"
                t += "<br />"
                t += "III. Factored form.......:<br /> |factor| "
                t += "<br />"
                Pa.roots.cjo = Complex.sortRoots(Pa.roots.cjo)
                Dim factor() As Polynomial = Polynomial.opFactor(Pa, cfg)
                Dim multiplicity() As Int64 = Pa.getRootsMultiplicity()
                Dim t2 As String = ""
                Try
                    If Not An.IsReal OrElse An.pRe.ToDouble <> 1 Then
                        t2 += An.toString(cfg) + "*"
                    End If
                    Dim j As Int32 = 0
                    j = 0
                    For i As Int32 = 0 To factor.Length - 1
                        If i Then t2 += " * "
                        t2 += "(" + factor(i).toStringPoly(cfg) + ")"
                        If multiplicity(j) > 1 Then
                            t2 += "^" + multiplicity(j).ToString
                            i += multiplicity(j) - 1
                        End If
                        j += 1

                    Next
                    t2 += " = 0 " + vbCrLf
                    If InStr(t2, "i") Then
                        Dim t3 As String = Split(sF2, "=")(0)
                        t2 += t3 + " = 0" + vbCrLf
                    End If
                Catch ex As Exception
                    't2 = mP.ret.toStringRetVal(cfg) + " = 0<br /> => " + vbCrLf
                    't2 += sF2 + " = 0" + vbCrLf
                    't2 = MathGlobal8.getTable(t2, "navy")
                    sF2 = Replace(sF2, "=", "")
                    Dim ve1() As String = Split(sF2, vbCrLf)
                    sF2 = ve1(0) + " = 0" + vbCrLf
                    If ve1.Length > 1 Then
                        sF2 += ve1(1) + " = 0" + vbCrLf
                    End If
                    t2 = sF2
                End Try
                t = Replace(t, "|factor|", t2)
                't += "<br />"
                Dim ts As String = "IV. Solutions:"
                ts += "<table border=""0"" style=""font-size:large; font-weight:bold;"" height=""@"">"
                Dim vectRoots As New Vector(cjo)

                Dim AddedBr As Int32 = 0
                For i As Int32 = 0 To cjo.Length - 1
                    Dim sRoot As String = cjo(i).ToStringComplex(cfg)
                    Dim bsReal As Boolean =
                        IIf(InStr(sRoot, "i") = 0, True, False)
                    If bsReal AndAlso bShownIsReal = False Then
                        C_sessionWeb.xi(nID) = cjo(i).pRe.ToDouble
                        bShownIsReal = True
                        s00 += "Real roots:<br />"
                        s01 += "<br />"
                        h += u
                    ElseIf bsReal = False AndAlso bShownIsCjo = False Then
                        bShownIsCjo = True
                        s10 += "Complex roots:<br />"
                        s11 += "<br />"
                    End If
                    If bsReal AndAlso Not bShownIsCjo Then
                        C_sessionWeb.xf(nID) = cjo(i).pRe.ToDouble
                        s00 += (i + 1).ToString + ".    " + Pa.var(0) + " = <br />"
                        s01 += "<span style=""color:red"">" + sRoot + "</span><br />"
                        h += u
                    Else
                        s10 += (i + 1).ToString + ".    " + Pa.var(0) + " = <br />"
                        If Len(s11) = 0 Then
                            s11 += "<br />"
                            AddedBr += 1
                        End If
                        s11 += "<span style=""color:red"">" + sRoot + "</span><br />"
                    End If
                Next
                ts = Replace(ts, "@", h.ToString)
                If Len(s00) = 0 Then
                    ts += "<tr><td>" + s10 + "</td>"
                    ts += "<td valign=""center"">" + s11 + "</td></tr>"
                ElseIf Len(s10) = 0 Then
                    ts += "<tr><td>" + s00 + "</td>"
                    ts += "<td>" + s01 + "</td></tr>"
                Else
                    ts += "<tr><td>" + s00 + s10 + "</td>"
                    ts += "<td>" + s01 + s11 + "</td></tr>"
                End If
                ts += "</table>"
                t = Replace(t, vbCrLf, "<br />")
                'Dim slit As String = mP.ret.toStringRetVal(cfg) + " = 0<br /> => " + vbCrLf
                'slit += sF2 + " = 0" + vbCrLf
                Dim style As String = "<span style='font-family: 'Courier New', Courier, 'Lucida Sans Typewriter', 'Lucida Typewriter';'>"
                ts = style + ts + "</span>"
                Dim tsolutions As String = "IV. Solutions:" + vbLf
                For i = 0 To cjo.Length - 1
                    tsolutions += (i + 1).ToString + ". " + Pa.var(0) + " = "
                    tsolutions += cjo(i).ToStringComplex(cfg) + vbLf
                Next
                Dim txt As String = Replace(t, "<br />", vbLf)
                txt = Regex.Replace(txt, "\<[^>]+\>", "")
                HiddenField1.Value = txt + tsolutions
                LinkCopy.Visible = True
                Lit3.Text = "" ' MathGlobal8.getTable(slit, "navy")
                t += ts
                Lit3.Text += t
                logFile(Nothing, Lit3.Text, "")
            End If
        Catch ex As Exception
            Dim s1 As String = ex.ToString
            logFile(ex, ex.ToString, Request.UserHostAddress)
            Lit3.Text = "n/a"
        End Try
        If bGoToPolyCalc Then
            Try
                Dim sCfg As String = ""
                sCfg += urlencode("rnd", chkRounding.Checked.ToString)
                sCfg += urlencode("frac", chkFractions.Checked.ToString)
                sCfg += urlencode("ignoreSpaces", "True")
                Dim e2 As String = "Polycalc.aspx?tb1=" +
                        Web.HttpUtility.UrlEncode(TextBox1.Text) +
                        "&tb3=" + sCfg
                Return e2
            Catch ex As Exception

            End Try
        End If
        Return ""
    End Function
    Function urlencode(ByVal sVar As String, ByVal value As String) As String
        Return "&" + sVar + "=" + Web.HttpUtility.UrlEncode(value)
    End Function
    'Private Sub btnGraph_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnGraph.Click
    '    'a la gráfica:
    '    Dim e1 As String = Replace(TextBox1.Text, Chr(10), "")
    '    e1 = Replace(e1, Chr(13), "")
    '    e1 = Trim(e1)
    '    logFile(Nothing, "raíces a graf.: " + TextBox1.Text, "")
    '    If Len(e1) = 0 Then
    '        Me.Lit3.Text = messages.msg(nID, 8)
    '        Exit Sub
    '    End If
    '    'poly.haydsp = False
    '    'poly.Roots = Me
    '    'poly.redondeo = Me.chkRounding  .Checked
    '    'poly.alaGrafica(TextBox1.Text, "")
    'End Sub
    Sub logFile(ByVal ex As Exception, ByVal e1 As String, ByVal e2 As String)
        If Len(sQuery) Then
            sQuery = "query=" + sQuery + "<br>"
        End If
        C_sessionWeb.logFile(ex, sQuery + e1, e2, Me.Page, nID, "rootFinder.txt")
        Exit Sub
        '    Try
        '        Dim f As FileStream
        '        Dim path As String = Server.MapPath(".")
        '        If InStr(path, "inetpub") Then
        '            f = New FileStream(path + "/rootFinder.txt", FileMode.Append)
        '        Else
        '            f = New FileStream(path + _
        '            "/../../database/rootFinder.txt", FileMode.Append, _
        '         FileAccess.Write)
        '        End If
        '        Dim i As Int32
        '        For i = 0 To 27
        '            e1 = Replace(e1, Chr(i), "")
        '        Next
        '        Dim dt As DateTime = Now
        '        Dim host As String = ""
        '        'Dim address As String = Request.UserHostAddress
        '        Try
        '            host = getHostName(address)
        '        Catch ex1 As Exception

        '        End Try
        '        Dim fs As New StreamWriter(f, System.Text.Encoding.Unicode)
        '        fs.WriteLine(Replace(address + Chr(9) _
        '        + host + Chr(9) + Now().ToShortDateString() + _
        '        Chr(9) + dt.ToShortTimeString() + Chr(9) + e1 + Chr(9) + e2, vbCrLf, "<br>"))
        '        'If Not (ex Is Nothing) Then
        '        '    fs.WriteLine(Request.UserHostAddress + Chr(9) _
        '        '    + host + Chr(9) + Now().ToShortDateString() + _
        '        '    Chr(9) + dt.ToShortTimeString() + Chr(9) + _
        '        '    ex.Message + Chr(9) + "texbox1: " + textbox1text)
        '        'End If
        '        fs.Close()
        '        Try
        '            f.Close()
        '        Catch ex2 As Exception

        '        End Try
        '    Catch ex3 As Exception
        '        Dim s1 As String = ex3.ToString
        '        Dim s2 As String = s1
        '    End Try
    End Sub

    'Public Function getHostName(ByVal strIP As String) As String
    '    Dim myIP As System.Net.IPAddress = System.Net.IPAddress.Parse(strIP)
    '    Dim hostInfo As System.Net.IPHostEntry = _
    '    System.Net.Dns.GetHostEntry(myIP)
    '    Return hostInfo.HostName
    'End Function

    Private Sub btnCalculate_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnCalculate.Click
        Try
            Dim e1 As String = go()
            If Not e1 Is Nothing AndAlso Len(e1) > 4 Then
                Response.Redirect(e1, False)
                Context.ApplicationInstance.CompleteRequest()
            Else
                'Session.Abandon()
                'Response.Cookies.Add(New HttpCookie("ASP.NET_SessionId", ""))
            End If
        Catch ex As Exception

        End Try
    End Sub

    'Private Sub btnIntegral_Click(ByVal sender As Object, ByVal e As System.EventArgs) _
    'Handles btnx.Click, btnx2.Click, btnx3.Click, btnx4.Click, btnSqrt.Click
    '    Try
    '        Me.TextBox1.Text += sender.text
    '    Catch ex As Exception

    '    End Try
    'End Sub

    Sub Inicio(ByVal sIdioma As String)

        'Button3.PostBackUrl = "https://xrjunque.nom.es" '?ln=" + messages.idioma
        Select Case sIdioma
            Case "ca"
                label1.Text = "Cercador de rels de polinomis&nbsp;(factoritzaci&oacute;)"
                'Label5.Text = "Escriu 10x<sup>4</sup>-0x<sup>3</sup>-270x<sup>2</sup>-140x+1200 o un altre " + _
                '"polinomi i pulsa 'Calcula' per a obtenir les rels (reals i/o complexes)."
                'Label2.Text = "Note: usa parèntesi. Per exemple 12/3x=(12/3)x " + _
                '            "però per a divdir 12 per 3x escriu 12/(3x)"
                'lblCase.Text = " (no sensible a majúscules)"
                chkRounding.Text = "Aplicar arrodoniment"
                'btnGraph.Text = "Gràfic"
                btnCalculate.Text = "Calcula"
                'HyperLink1.Text = "Comentaris"
                'Button3.Text = "Inici"
                msgs(0) = "<h4>El grau del polinomi es zero.</h4>"
                msgs(1) = "<h4>El polinomi "
                msgs(2) = " te les següents rels:</h4>"
                chkFractions.Text = "Fraccions"
            Case "es"
                label1.Text = "Buscador de ra&iacute;ces de polinomios&nbsp;(factorizaci&oacute;n)"
                'Label5.Text = "Escribe 10x<sup>4</sup>-0x<sup>3</sup>-270x<sup>2</sup>-140x+1200 u otro " + _
                '"polinomio y pulsa 'Calcula' para obtener las raíces (reales y/ó complejas)."
                'Label2.Text = "Nota: usa paréntesis. Por ejemplo 12/3x=(12/3)x, pero para dividir 12 por 3x escribe 12/(3x)"
                'lblCase.Text = " (no sensible a mayúsculas)"
                chkRounding.Text = "Aplicar redondeo"
                'btnGraph.Text = "Gráfica"
                btnCalculate.Text = "Calcula"
                'HyperLink1.Text = "Comentarios"
                'Button3.Text = "Inicio"
                msgs(0) = "<h4>El grado del polinomio es cero.</h4>"
                msgs(1) = "<h4>El polinomio "
                msgs(2) = " tiene las siguientes raíces:</h4>"
                chkFractions.Text = "Fracciones"
            Case "it"
                label1.Text = "Ricercatore di radici di polinomi. (Fattorizzazione)"
                'Label5.Text = "Scrivere 10x<sup>4</sup>-0x<sup>3</sup>-270x" + _
                '        "<sup>2</sup>-140x+1200" + _
                '        " o un altro polinomio e clicca su 'Calcola' per ottenere le radici" + _
                '        " reali e/o complesse."
                'Label2.Text = "Note: usare la parentesi. Per esempio: 12/3x=(12/3)x" + _
                '        " ma per dividere 12 per 3x, scrivere 12/(3x)."
                'lblCase.Text = " (non è case sensitive)"
                chkRounding.Text = "Arrotondamento"
                'btnGraph.Text = "Grafica"
                btnCalculate.Text = "Calcola"
                'HyperLink1.Text = "Commenti"
                'Button3.Text = "Home"
                msgs(0) = "<h4>Il grado di polinomio è zero.</h4>"
                msgs(1) = "<h4>Polinomio "
                msgs(2) = " ha le radici seguenti:</h4>"
                chkFractions.Text = "Frazione"
            Case "fr"
                label1.Text = "Chercheur des racines de polynômes"
                'Label5.Text = "Ecrivez 10x<sup>4</sup>-0x<sup>3</sup>-270x<sup>2</sup>-140x+1200 ou un autre " + _
                '"polynome et appuyer 'Calculer' pour obtenir les racines (réelles et/ou complexes)."
                'Label2.Text = "Note: usez les parenthèsess. Par exemple 12/3x=(12/3)x, mais pour diviser 12 par 3x ecrivez 12/(3x)"
                'lblCase.Text = " (n'est pas sensible à la casse)"
                chkRounding.Text = "Arrondir"
                'btnGraph.Text = "Graphique"
                btnCalculate.Text = "Calculer"
                'HyperLink1.Text = "Commentaires"
                msgs(0) = "<h4>Polynomial degree is zero.</h4>"
                msgs(1) = "<h4>Les racines du polynôme "
                msgs(2) = " sont:</h4>"
                chkFractions.Text = "Fractions"
            Case "pt"
                label1.Text = "Pesquisa de raizes de polinomios (fatoração)"
                'Label5.Text = "Escreva  10x<sup>4</sup>-0x<sup>3</sup>-270x" + _
                '        "<sup>2</sup>-140x+1200" + _
                '        " ou outro Polynomial e prensa 'Calcular' para obter as raizes" + _
                '        " reais e/ou complexas."
                'Label2.Text = "A nota: usa parêntese. Para exemplo 12/3x=(12/3)x " + _
                '        "mas para dividir 12 por 3x escrevem 12/(3x)."
                'lblCase.Text = " (não é sensível a maiúsculas)"
                chkRounding.Text = "Arredondamento"
                'btnGraph.Text = "Gráfico"
                btnCalculate.Text = "Calcular"
                'HyperLink1.Text = "Comments"
                'Button3.Text = "Home"
                msgs(0) = "<h4>O grau de Polynomial é zero.</h4>"
                msgs(1) = "<h4>Polinômio "
                msgs(2) = " tem as seguintes raizes: </h4>"
                chkFractions.Text = "Frações"
            Case "nl"
                label1.Text = "Polynomische wortel zoeker (factorizeren)"
                'Label5.Text = "Neem 10x<sup>4</sup>-0x<sup>3</sup>-270x<sup>2</sup>-140x+1200 of een andere polynoom en klik op 'Bereken' om zijn reële en/of complexe wortels te bekomen"
                'Label2.Text = "Opmerking: gebruik haakjes. Bijvoorbeeld 12/3x=(12/3)x, maar om 12 te delen door 3x doe je 12/(3x)"
                'lblCase.Text = " (is niet hoofdlettergevoelig)"
                chkRounding.Text = "Pas afrondingen toe"
                'btnGraph.Text = "Grafiek"
                btnCalculate.Text = "Bereken"
                'HyperLink1.Text = "Opmerkingen"
                'Button3.Text = "Begin"
                msgs(0) = "<h4>Polynomial degree is zero.</h4>"
                msgs(1) = "<h4>Polynomial "
                msgs(2) = " has the following roots:</h4>"
                chkFractions.Text = "Breuken"
            Case "de"
                label1.Text = "Polynomischer Nullstellesucher(factoring)"
                'Label5.Text = "Schreiben Sie bitte 10x<sup>4</sup>-0x<sup>3</sup>-270x" + _
                '        "<sup>2</sup>-140x+1200" + _
                '        " oder anderes Polynom und klicken 'Kalkulieren'," + _
                '        " die wirklichen oder komplizierten Nullstellen zu erhalten. "
                'Label2.Text = "Benutzen Sie Anmerkung: Klammer. Zum Beispiel:  12/3x=(12/3)x " + _
                '        "aber 12 durch 3x zu teilen schreibt 12/(3x)"
                'lblCase.Text = " (ist nicht case sensitive)"
                chkRounding.Text = "Rundung"
                'btnGraph.Text = "Grafik"
                btnCalculate.Text = "Kalkulieren"
                'HyperLink1.Text = "Comments"
                'Button3.Text = "Home"
                msgs(0) = "<h4>Polynomisches Grad ist null.</h4>"
                msgs(1) = "<h4>Polynom "
                msgs(2) = " hat die Folgenden Nullstellen:</h4>"
                chkFractions.Text = "Bruchrechnung"
            Case Else
                label1.Text = "&nbsp;&nbsp;Polynomial's root finder (factoring)"
                'Label5.Text = "<br />&nbsp;&nbsp;Write 10x<sup>4</sup>-0x<sup>3</sup>-270x" + _
                '        "<sup>2</sup>-140x+1200" + _
                '        " or another Polynomial and click 'Calculate' to obtain the " + _
                '        " real and/or Complex roots. "
                'Label2.Text = "<br />&nbsp;&nbsp;In case of doubt, employ parentheses. For example 12/3x=(12/3)x " + _
                '        " but in order to divide 12 by 3x write 12/(3x)"
                'lblCase.Text = "<br />&nbsp;&nbsp;(is not case sensitive)"
                chkRounding.Text = "Apply rounding"
                'btnGraph.Text = "Graph"
                btnCalculate.Text = "Calculate"
                'HyperLink1.Text = "Comments"
                'Button3.Text = "Home"
                msgs(0) = "<h4>Polynomial degree is zero.</h4>"
                msgs(1) = "<h4>Polynomial "
                msgs(2) = " has the following roots:</h4>"
                chkFractions.Text = "Fractions"
        End Select
    End Sub


    'Private Sub btnClear_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnClear.Click
    '    TextBox1.Text = ""
    'End Sub
End Class
