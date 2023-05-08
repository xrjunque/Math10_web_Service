Public Class mobile2
    Inherits System.Web.UI.Page
    Dim nId As Int32
    Dim vChar() As Char
    Private Sub Page_PreRender(sender As Object, e As EventArgs) Handles Me.PreRender
        Try
            C_sessionWeb.contador(
                Page,
                Request,
                Response)
            nId = C_sessionWeb.getID(Page)
            Dim m As MasterPage = Me.Master
            Dim t As Table = m.FindControl("Tabletot")
            t.Visible = False
        Catch ex2 As Exception

        End Try

    End Sub

    Protected Sub DropDownList1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles DropDownList1.SelectedIndexChanged
        Try
            Dim i As Int32 = DropDownList1.SelectedIndex
            If i < 1 Then
                Exit Sub
            End If
            Calculate(DropDownList1.SelectedValue)
        Catch ex As Exception

        End Try
    End Sub

    Protected Sub btnGO_Click(sender As Object, e As EventArgs) Handles btnGO.Click
        Calculate(tbInput.Text)
    End Sub
    Sub Calculate(sInput As String)
        Try
            If sInput.Length > 600 Then
                Exit Try
            End If
            Output.Text = ""
            Dim t As String
            Try
                If InStr(sInput, "=") Then
                    t = go(sInput)
                    If t.Length Then
                        Output.Text = t
                        C_sessionWeb.logFile(Nothing,
                         sInput, Output.Text, Page, nId, "polycalc.txt")
                        Exit Sub
                    End If
                End If
            Catch ex As Exception

            End Try
            Dim e1 As String = sInput
            e1 = Replace(e1, " ", "")
            e1 = Replace(e1, ",", ";")
            If Len(e1) = 0 Then Exit Sub
            Dim cfg As New Config
            cfg.bRounding = chkRnd.Checked
            cfg.bFractions = chkFractions.Checked
            cfg.bCaseSensitive = chkCase.Checked
            cfg.timeOutms = 30 * 1000
            cfg.doTimeOut = timeout.whenTimeIsOver
            Dim ve() As String = Split(e1, "@")
            Dim bIsMultipleValues As Boolean = False
            Dim i As Int32 = 0
            For i = 1 To ve.Length - 1
                Dim ve2() As String = Split(ve(i), "=")
                ' Starts with { and ends with } ??
                If ve2.Length = 2 AndAlso Regex.IsMatch(ve2(1), "^\x7B[^\x7D]+(\x7D)$") Then
                    bIsMultipleValues = True
                    Exit For
                End If
            Next
            If bIsMultipleValues Then
                e1 = ve(0)
                ve(0) = ""
                Dim vRep()() As String = findVarsRepetitions(Join(ve, "@"))
                If vRep IsNot Nothing Then
                    Dim vIndex(vRep(0).Length - 1) As Int32
                    For i = 0 To vRep(1).Length - 1
                        Dim ve2() As String = Split(vRep(1)(i), ";")
                        vIndex(i) = ve2.Length - 1
                    Next
                    Dim i0(vIndex.Length - 1) As Int32
                    Array.Copy(vIndex, i0, i0.Length)
                    Dim index As Int32 = i0.Length - 1
                    Do
                        Dim sVars As String = ""
                        For j As Int32 = 0 To vIndex.Length - 1
                            sVars += vRep(0)(j) + "="
                            Dim ve2() As String = Split(vRep(1)(j), ";")
                            sVars += ve2(i0(j) - vIndex(j))
                            If j < vIndex.Length - 1 Then
                                sVars += "@"
                            End If
                        Next
                        Dim mP As New matrixParser()
                        mP.parse(e1, sVars, cfg:=cfg)
                        Output.Text += mP.ToStringAll(cfg) + " (" + sVars + ")<br />"
                    Loop While CheckvIndex(vIndex, i0, index)
                    C_sessionWeb.logFile(Nothing,
                         sInput, Output.Text, Page, nId, "polycalc.txt")
                End If
            Else
                Dim mP As New matrixParser()
                Try
                    Dim cfg1 As New Config(cfg)
                    mP.parse(e1, cfg:=cfg1)
                Catch ex As Exception
                    If mP.ret Is Nothing AndAlso
                            (mP.retCjo Is Nothing OrElse mP.retCjo.Length = 0) Then
                        Exit Try
                    End If
                End Try
                Output.Text = mP.ToStringAll(cfg)
                If Output.Text.Length = 0 AndAlso mP.errMsg.Length Then
                    Output.Text = mP.errMsg
                End If
                C_sessionWeb.logFile(Nothing,
                         sInput _
                        , Output.Text, Page, nId, "polycalc.txt")
            End If
        Catch ex As Exception
        End Try
    End Sub
    Function findVarsRepetitions(e1 As String) As String()()
        Dim sRet(1)() As String
        Try
            If Trim(e1).Length = 0 Then
                Return Nothing
            End If
            e1 = Replace(e1, " ", "")
            Dim ve() As String = Split(e1, "@")
            Dim vVar(-1), vVal(-1) As String, iv As Int32 = 0
            For i As Int32 = 0 To ve.Length - 1
                ve(i) = Replace(ve(i), "{", "")
                ve(i) = Replace(ve(i), "}", "")
                Dim e2() As String = Split(ve(i), "=")
                If e2.Length = 2 AndAlso e2(0).Length * e2(1).Length Then
                    Dim pos As Int32 = Array.IndexOf(vVar, e2(0), iv)
                    If pos > -1 Then
                        If vVal(pos).Chars(0) <> ";" Then
                            vVal(pos) += ";"
                        End If
                        vVal(pos) += e2(1)
                    Else
                        ReDim Preserve vVar(iv), vVal(iv)
                        vVar(iv) = e2(0)
                        vVal(iv) = e2(1)
                        iv += 1
                    End If
                End If
            Next
            If iv = 0 Then
                Return Nothing
            End If
            sRet(0) = vVar
            sRet(1) = vVal
        Catch ex As Exception
            Return Nothing
        End Try
        Return sRet
    End Function

    Function CheckvIndex(ByRef vIndex() As Int32, i0() As Int32, ByRef ind As Int32) As Boolean
        Try
            vIndex(ind) -= 1
            Do While vIndex(ind) < 0
                ind -= 1
                If ind < 0 Then Return False
                vIndex(ind) -= 1
            Loop
            For i As Int32 = ind + 1 To i0.Length - 1
                vIndex(i) = i0(i)
                ind = i
            Next
        Catch ex As Exception
            Return False
        End Try
        Return True

    End Function
    Dim mP As New matrixParser
    Dim sQuery As String = ""
    Dim cfg As Config = Config.cfg
    Private Function go(sInput As String) As String
        Dim t As String = ""
        Try
            'Session.Timeout = 3
            Dim e1 As String = sInput
            If Len(e1) = 0 Then
                Return ""
            End If
            e1 = Replace(e1, Chr(10), "")
            e1 = Replace(e1, Chr(13), "")
            e1 = Trim(e1)
            If Len(e1) = 0 Then
                Return ""
            End If
            Dim e3 As String = sInput
            e3 = LCase(e3)

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
            'Dim expr As Expression
            'Try
            '    Dim pos As Int32 = InStr(e3, "=")
            '    If pos = 0 Then Return ""
            '    If InStr(e3, ",") Then Return ""
            '    e1 = Mid(e1, 1, pos - 1) + "-(" + Mid(e3, pos + 1) + ")"
            '    expr = Expression.ParseExpression(e1)
            '    If expr.IsPolynomial Then
            '        e3 = expr.getPolynomial.toStringPoly(New Config)
            '    End If
            'Catch ex As Exception
            '    Return ""
            'End Try

            'mP.parse(e1, cfg:=cfg)
            'If mP.ret.exprMtx.IsPolynomial Then
            '    e3 = mP.ret.exprMtx.getExpr(0, 0).getPolynomial.toStringPoly(New Config)
            'End If

            Dim bGoToPolyCalc As Boolean = False
            'Try
            '    Dim ve3() As String = Split(e3, "=")
            '    If ve3.Length > 2 Then
            '        Return ""
            '    ElseIf ve3.Length = 2 Then
            '        e3 = ve3(0) + "-(" + ve3(1) + ")"
            '    End If
            '    expr = Expression.ParseExpression(e3)
            '    If Not expr.IsPolynomial Then
            '        Return ""
            '    ElseIf expr.getPolynomial.var.Length > 1 Then
            '        bGoToPolyCalc = True
            '        Dim vvar() As String = expr.getPolynomial.var
            '        Dim j1 As Int32
            '        For j1 = 0 To vvar.Length - 1
            '            mP.parse(vvar(j1), "")
            '            If Not mP.ret.curExpr.IsPolynomial Then
            '                Exit For
            '            End If
            '        Next
            '        If j1 >= vvar.Length AndAlso expr.getPolynomial.PolyResto Is Nothing Then
            '        Else
            '            Return ""
            '        End If
            '    ElseIf expr.getPolynomial.PolyResto IsNot Nothing Then
            '        bGoToPolyCalc = True
            '    Else
            '        bGoToPolyCalc = False
            '    End If
            '    If Not bGoToPolyCalc Then
            '        e3 += "= 0"
            '    End If
            'Catch ex As Exception
            '    bGoToPolyCalc = True
            'End Try
            'If bGoToPolyCalc Then
            '    Return ""
            'End If
            mP.parse(e3, "", oVars8, cfg)
            If mP.ret Is Nothing Then
                Return ""
            End If
            If nComas AndAlso mP.ret.isMatrix Then
                Return ""
            End If
            cfg.doTimeOut = timeout.never
            If mP.errMsg.Length Then
                Return ""
            ElseIf mP.ret.curExpr.IsPolynomial Then
                Dim vVar(-1) As String
                mP.ret.curExpr.getAllVars(vVar)
                If vVar.Length <> 1 Then
                    Return ""
                End If
                Dim Pa As Polynomial = mP.soe.getPolynomial

                If Pa Is Nothing OrElse Pa.var.Length < 1 OrElse Pa.PolyResto IsNot Nothing Then
                    Return ""
                ElseIf Pa.var.Length > 1 Then
                    Return ""
                End If
                Response.Clear()
                cfg.doTimeOut = timeout.never
                cfg.bRounding = chkRnd.Checked
                mP.ret.diffType = retMtx.diffResponse.resto
                Dim e4 As String = mP.ret.toStringRetVal(cfg) + "<br />"
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

                Dim sFn As String = ""
                Dim sF2 As String = ""
                If bIsPoly AndAlso bOneVar Then

                    sFn = Pa.ToString
                    C_sessionWeb.sfn(nId) = sFn
                    C_sessionWeb.sf3(nId) = "0"
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
                                C_sessionWeb.xi(nId) = min
                                C_sessionWeb.xf(nId) = max
                            End If
                        End If
                    End If
                End If
                If bGoToPolyCalc Then
                    Exit Try
                End If
                C_sessionWeb.yAuto(nId) = 1
                C_sessionWeb.sf3(nId) = "0"
                sF2 = Pa.ToStringFactors(cfg)
                t = ""
                t += "I.   Input equation.(P(" + Pa.var(0) + ")=0): " + sInput
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
                    sF2 = Replace(sF2, "=", "")
                    Dim ve1() As String = Split(sF2, vbCrLf)
                    sF2 = ve1(0) + " = 0" + vbCrLf
                    If ve1.Length > 1 Then
                        sF2 += ve1(1) + " = 0" + vbCrLf
                    End If
                    t2 = sF2
                End Try
                t = Replace(t, "|factor|", t2)

                Dim ts As String = "IV. Solutions:"
                ts += "<table border=""0"" style=""font-size:large; font-weight:bold;"" height=""@"">"
                Dim vectRoots As New Vector(cjo)

                Dim AddedBr As Int32 = 0
                For i As Int32 = 0 To cjo.Length - 1
                    Dim sRoot As String = cjo(i).ToStringComplex(cfg)
                    Dim bsReal As Boolean =
                        IIf(InStr(sRoot, "i") = 0, True, False)
                    If bsReal AndAlso bShownIsReal = False Then
                        C_sessionWeb.xi(nId) = cjo(i).pRe.ToDouble
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
                        C_sessionWeb.xf(nId) = cjo(i).pRe.ToDouble
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
                Dim style As String = "<span style='font-family: 'Courier New', Courier, 'Lucida Sans Typewriter', 'Lucida Typewriter';'>"
                ts = style + ts + "</span>"
                Dim tsolutions As String = "IV. Solutions:" + vbLf
                For i = 0 To cjo.Length - 1
                    tsolutions += (i + 1).ToString + ". " + Pa.var(0) + " = "
                    tsolutions += cjo(i).ToStringComplex(cfg) + vbLf
                Next
                Dim txt As String = Replace(t, "<br />", vbLf)
                txt = Regex.Replace(txt, "\<[^>]+\>", "")
                t += ts
                HiddenField1.Value = txt + tsolutions
                LinkCopy.Visible = True
            End If
        Catch ex As Exception
            Dim s1 As String = ex.ToString
        End Try
        Return t
    End Function

End Class