Imports System.Globalization
Imports System.Text
Imports System.Text.RegularExpressions
Imports System.Drawing
Imports System.Drawing.drawing2d
Imports tr = System.Windows.Forms.TextRenderer

Public Structure RPN_Stack ' Reverse Polish Notation 
    ' http://en.wikipedia.org/wiki/Reverse_Polish_notation

    Public Shared us As System.Globalization.CultureInfo = Config84.us
    Friend iSt, iToken, iv As Int64
    Friend eP As exprParser84
    Friend oStack() As Token
    Friend iOp, iChgSg As Int64

    Friend Sub New(eP As exprParser84, size As Int64)
        Me.eP = eP
        ReDim oStack(size * 2)
        iSt = 0
    End Sub
    Friend ReadOnly Property Length() As Int64
        Get
            Return Me.iSt
        End Get
    End Property

    Public Function ToStringStack(ByRef sResult As String, cfg As Config, Optional vVarNames() As String = Nothing,
                         Optional vValues() As ExprMatrix = Nothing,
                         Optional bGroup As Boolean = False,
                         Optional bShowVarValues As Boolean = True) As Boolean
        Dim sRet As New StringBuilder
        Dim sFn As String = Join(Config84.vFn, "|")
        Dim i As Int64
        Try
            For i = 0 To Length - 1
                If oStack(i).tipo >= -3 Then
                    Dim e1 As String =
                        oStack(i).ToStringStackTkn(cfg,
                         vVarNames, vValues, bShowVarValues)
                    If eP.err IsNot Nothing Then
                        Return False
                    End If
                    If oStack(i).tipo = tokenType84.chgSgn Then
                        e1 = "NEG"
                        'ElseIf oStack(i).sTkn = "CROSSPRODUCT" Then
                        '   e1 = "CROSS"
                    ElseIf oStack(i).tipo = tokenType84.fn Then
                        e1 = UCase(e1)
                        If e1 = "SQR" OrElse e1 = "SQRT" OrElse e1 = "√" Then
                            e1 = "ƒ" ' "√" ' ƒ
                        End If
                    ElseIf oStack(i).sTkn = "REMAINDER" Then
                        e1 = "REMAINDER"
                    ElseIf oStack(i).sTkn = "%" Then
                        e1 = "MOD"
                    End If
                    If Len(e1) AndAlso
                    oStack(i).tipo <> tokenType84.other Then
                        If Not bGroup Then
                            sRet.Append(e1 + vbCrLf)
                        Else
                            sRet.Append(e1 + " ")
                            Dim tipo As tokenType84 = oStack(i).tipo
                            If tipo = tokenType84.optor OrElse
                            tipo = tokenType84.fn OrElse
                            tipo = tokenType84.chgSgn Then
                                sRet.Append(vbCrLf)
                            End If
                        End If
                    End If
                End If
            Next
        Catch ex As Exception
            eP.err = ex
            Return False
        End Try
        sResult = Replace(sRet.ToString, ";", ",")
        Return True
    End Function
    Public Function ToStringDetail(
                    ByRef Result As Token,
                    ByRef sResult As String,
                    Optional vNames() As String = Nothing,
                    Optional vValues() As ExprMatrix = Nothing,
                    Optional format As outputMessage = outputMessage.plainText,
                    Optional vTkn() As Token = Nothing) As Boolean

        Dim vRet As New StringBuilder(300)
        ' Array vDbl() will contain the (numeric) operands:
        Try
            oStack = vTkn
            iSt = oStack.Length
            Dim vStk(iSt) As Token, vPos(iSt) As Int64
            Dim vPreOp(iSt * 2), vIntermOp(iSt * 2), vPostOp(iSt * 2) As StringBuilder
            Dim tipo As tokenType84
            Dim i As Int64
            Dim vExpr(iSt * 2 - 1) As StringBuilder
            'Dim bHasOptors As Boolean = False
            Dim iMax As Int64 = 0
            Dim iLastOpnd As Int64 = -1
            Dim itknLastOpnd As Int64 = -1
            Dim nOpnds As Int64 = 0
            Dim vOpndns(-1) As Int64, ivopnds As Int64 = 0
            Dim bFirst As Boolean = True
            For i = 0 To iSt - 1
                Dim iTkn As Int64 = oStack(i).iToken
                tipo = oStack(i).tipo
                If tipo = tokenType84.var Then
                    'Dim bDoVar As Boolean = IIf(nOpnds > 1, True, False)
                    'If Not bDoVar Then
                    '    Dim i1 As Int64 = i + 1
                    '    Do While i1 < iSt
                    '        If oStack(i1).sTkn <> ")" Then
                    '            Exit Do
                    '        End If
                    '        i1 += 1
                    '    Loop
                    '    If i1 >= iSt OrElse oStack(i1).tipo = tokenType84.chgSgn Then
                    '        bDoVar = True
                    '    End If
                    'End If
                    If Not bFirst Then
                        ' Variable...
                        vStk(i) = oStack(i).Copy
                        Dim index As Int64 = Array.IndexOf(vNames, oStack(i).sTkn)
                        If index > -1 Then
                            vStk(i).tipo = tokenType84.oprnd
                            vStk(i).dblVal = vValues(index)
                        Else
                            vStk(i).tipo = tokenType84.Polynomial84
                            vStk(i).dblVal = New ExprMatrix(Expression.ParseExpression(vStk(i).sTkn))
                        End If
                        vExpr(iTkn) = New StringBuilder(vStk(i).ToString)
                    End If
                    iLastOpnd = i
                    itknLastOpnd = iTkn
                    nOpnds += 1
                Else 'If tipo <> tokenType84.doNotDetail Then
                    If tipo = tokenType84.oprnd Then
                        iLastOpnd = i
                        itknLastOpnd = iTkn
                        nOpnds += 1
                    ElseIf tipo = tokenType84.fn Then
                        If InStr("mod gcd lcm", LCase(oStack(i).sTkn)) Then
                            nOpnds -= 1
                        End If
                    ElseIf tipo = tokenType84.optor Then
                        If oStack(i).subTipo = optor.factorial OrElse
                        oStack(i).subTipo = optor.not Then
                        Else
                            nOpnds -= 1
                        End If
                    ElseIf oStack(i).sTkn = "(" Then
                        ReDim Preserve vOpndns(ivopnds)
                        vOpndns(ivopnds) = nOpnds
                        ivopnds += 1
                        nOpnds = 0
                    ElseIf oStack(i).sTkn = ")" Then
                        ivopnds -= 1
                        nOpnds = vOpndns(ivopnds) + 1
                    ElseIf tipo = tokenType84.chgSgn Then
                        oStack(i).sTkn = "-"
                    End If
                    'If Not bHasOptors AndAlso
                    'tipo = tokenType84.optor Then
                    '    bHasOptors = True
                    'End If
                    vExpr(iTkn) = New StringBuilder(oStack(i).sTkn)
                End If
                If iTkn > iMax Then iMax = iTkn
                If oStack(i).sTkn <> "(" Then
                    bFirst = False
                End If
            Next
            ReDim Preserve vExpr(iMax)
            For i = 0 To vExpr.Length - 1
                If vExpr(i) Is Nothing Then
                    vExpr(i) = New StringBuilder
                End If
            Next
            Dim sExpr As New StringBuilder(200) '(eP.sbExpr.Length)
            ' = Join(vExpr, "")
            For i = 0 To vExpr.Length - 1
                sExpr.Append(vExpr(i))
            Next
            If oStack(iSt - 1).tipo = tokenType84.chgSgn Then
                sExpr.Insert(0, "-")
            End If



            nOpnds = 0
            Dim vIExpr(iSt) As Int64
            i = 0 : iv = 0

            ' Do as if evaluating the RPN stack. 
            Dim opA As Token = Nothing
            Dim sDivisions As String = String.Empty
            Do
                tipo = oStack(i).tipo
                Select Case tipo
                    Case tokenType84.optor, tokenType84.fn ', tokenType84.chgSgn
chgsgn:
                        Dim sL As New StringBuilder
                        Dim sM As New StringBuilder
                        Dim sR As New StringBuilder

                        ' Each time a new operation takes place
                        ' vPreOp() will contain sExpr as is now
                        ' and vPostOp() the string after the operation.
                        ' This new string will be assigned also to
                        ' sExpr. Here on, continue in the same way 
                        ' with a new operation, till the last operation.
                        ' The current operation is surrounded by [ ].

                        'If tipo = tokenType84.chgSgn Then
                        '    vStk(nOpnds - 1) = -vStk(nOpnds - 1)
                        '    vStk(nOpnds - 1).sTkn = "-" + vStk(nOpnds - 1).sTkn
                        'End If
                        Dim sMNew As String = ""
                        Dim sMInterm As String = String.Empty
                        Dim numOpnds As Int64 = 1
                        opA = vStk(nOpnds - 1)
                        Dim opB As Token = Nothing
                        Dim iOp As Int64 = oStack(i).iToken
                        Dim iOpA As Int64
                        Dim bIsMinus As Boolean = False
                        Dim addToiA As Int64 = 0
                        Dim iAddA As Int64 = 0
                        Dim iAddB As Int64 = 0


                        Select Case oStack(i).tipo
                            Case tokenType84.optor
                                If oStack(i).subTipo <> 33 AndAlso oStack(i).tipo <> tokenType84.chgSgn Then
                                    numOpnds = 2
                                    ' -  +  *  /  ^  %  !
                                    '45 43 42 47 94 37 33
                                    opB = opA.Copy
                                    opA = vStk(nOpnds - 2).Copy
                                    Dim sA1 As String = opA.ToString
                                    Dim sA As String = sA1 ' IIf(sA1.Chars(0) = "-", Mid(sA1, 2), sA1)
                                    Dim sB1 As String = opB.ToString
                                    Dim sB As String = sB1 ' IIf(sB1.Chars(0) = "-", Mid(sB1, 2), sB1)
                                    Select Case oStack(i).subTipo
                                        Case 43
                                            opA += opB
                                            sMNew = opA.ToString
                                        Case 45
                                            opA -= opB
                                            sMNew = opA.ToString
                                        Case 42
                                            If Regex.IsMatch(sA, "[-+]") Then
                                                sA = "(" + sA + ")"
                                            End If
                                            If Regex.IsMatch(sB, "[-+]") Then
                                                sB = "(" + sB + ")"
                                            End If
                                            sMInterm = Token.opMultDetail(opA, opB)
                                            opA *= opB
                                            If Regex.IsMatch(sA, "[-+]") Then
                                                sMNew = "(" + opA.ToString + ")"
                                                sMInterm = "(" + sMInterm + ")"
                                            Else
                                                sMNew = opA.ToString
                                            End If
                                        Case 47
                                            If Regex.IsMatch(sA, "[-+]") Then
                                                sA = "(" + sA + ")"
                                            End If
                                            If Regex.IsMatch(sB, "[-+*]") Then
                                                sB = "(" + sB + ")"
                                            End If
                                            eP.cfg.bDetail = True
                                            If opA.eP Is Nothing Then
                                                opA.eP = eP
                                                opB.eP = eP
                                            End If
                                            opA.eP.cfg = eP.cfg
                                            opB.eP.cfg = eP.cfg
                                            Dim e1 As String = String.Empty
                                            opA = Token.opDivDetail(e1, opA, opB)
                                            sDivisions += e1
                                            eP.cfg.bDetail = False
                                            If Regex.IsMatch(sA, "[-+]") Then
                                                sMNew = "(" + opA.ToString + ")"
                                            Else
                                                sMNew = opA.ToString
                                            End If
                                        Case 94
                                            If Regex.IsMatch(sA, "[-+*/]") Then
                                                sA = "(" + sA + ")"
                                            End If
                                            If Regex.IsMatch(sB, "[-+*/]") Then
                                                sB = "(" + sB + ")"
                                            End If
                                            opA ^= opB
                                            If Regex.IsMatch(sA, "[-+*/]") Then
                                                sMNew = "(" + opA.ToString + ")"
                                            Else
                                                sMNew = opA.ToString
                                            End If
                                        Case 37
                                            opA = opA Mod opB
                                            sMNew = opA.ToString
                                        Case Else
                                            Select Case oStack(i).subTipo
                                                Case optor.and
                                                    opA = opA And opB
                                                    sMNew = opA.ToString
                                                Case optor.or
                                                    opA = opA Or opB
                                                    sMNew = opA.ToString
                                                Case optor.xor
                                                    opA = opA Xor opB
                                                    sMNew = opA.ToString
                                                Case optor.nor
                                                    opA = Not (opA Or opB)
                                                    sMNew = opA.ToString
                                                Case optor.not
                                                    opA = Not (opA)
                                                    sMNew = opA.ToString
                                                Case optor.nand
                                                    opA = Not (opA And opB)
                                                    sMNew = opA.ToString
                                            End Select
                                    End Select
                                    sM = New StringBuilder(sA1 +
                                                       Chr(oStack(i).subTipo) +
                                                       sB)
                                    sMNew = opA.ToString
                                Else
                                    ' "!" --> 1 operand
                                    numOpnds = 1
                                    sM.Append(opA.ToString + "!")
                                    If opA.dblVal.toDouble > 1.0 Then
                                        sM.Append(" (=" + opA.ToString)
                                    End If
                                    Dim j As Int64
                                    For j = Math.Floor(opA.dblVal.toDouble) - 1 To 2 Step -1
                                        opA.dblVal *= New ExprMatrix(j)
                                        sM.Append(" *" + j.ToString)
                                    Next
                                    If opA.dblVal.toDouble > 1.0 Then
                                        sM.Append(" *1)")
                                    End If
                                    sMNew = opA.ToString
                                    iAddB = 1
                                End If
                            Case tokenType84.chgSgn ' change sign
                                opA = -opA
                                sM.Append(opA.ToString)

                                sMNew = opA.ToString
                                bIsMinus = True
                                iOpA = iOp
                            Case tokenType84.fn  ' function
                                Dim sFn As String = oStack(i).sTkn
                                If InStr("LCM GCD MOD", UCase(sFn)) > 0 Then
                                    vRet.Clear()
                                    Exit Try
                                    'opB = vStk(nOpnds - 2)
                                    'numOpnds = 2
                                    'sM = New StringBuilder(sFn + "(" + opA.ToString + "," + opB.ToString + ")")
                                    'opA.dblVal = exprParser.evalFn(sFn, opA.dblVal, Nothing, opB.dblVal)
                                    'sMNew = opA.ToString
                                Else
                                    sM = New StringBuilder(sFn + "(" + opA.ToString + ")")
                                    opA.dblVal = exprParser.evalFn(sFn, vStk(nOpnds - 1).dblVal)
                                    sMNew = opA.ToString
                                End If
                                vExpr(iOp).Remove(0, vExpr(iOp).Length)

                            Case Else ' Here, all operations imply 2 operands:
                        End Select

                        sL.Remove(0, sL.Length)
                        Dim iA As Int64 = vIExpr(nOpnds - numOpnds) + addToiA
                        Dim iB As Int64 = vIExpr(nOpnds - 1)



                        Dim iA0 As Int64 = iA
                        Dim iB0 As Int64 = iB
                        If numOpnds = 2 OrElse tipo = tokenType84.fn Then
                            Do While iA > 0 AndAlso
                            (vExpr(iA - 1) Is Nothing AndAlso vExpr(iA - 1).Length = 0)
                                iA -= 1
                            Loop
                            Do While iB + 1 < vExpr.Length AndAlso
                            (vExpr(iB + 1) IsNot Nothing AndAlso vExpr(iB + 1).Length = 0)
                                iB += 1
                            Loop
                            If iA > 0 AndAlso vExpr(iA - 1).ToString = "(" AndAlso
                            iOp > 0 AndAlso vExpr(iOp - 1).ToString = ")" Then
                                iAddA += 1
                            End If
                        End If
                        For j1 As Int64 = 0 To iA - 1 - iAddA
                            sL.Append(vExpr(j1))
                        Next

                        For j1 As Int64 = iB + 1 + iAddB To vExpr.Length - 1
                            sR.Append(vExpr(j1))
                        Next
                        If sL.Length AndAlso sL.Chars(sL.Length - 1) = "(" AndAlso
                        sR.Length AndAlso sR.Chars(0) = ")" Then
                            sL.Remove(sL.Length - 1, 1)
                            sR.Remove(0, 1)
                        End If
                        If numOpnds = 1 Then
                            vStk(nOpnds - 1) = opA.Copy
                        Else
                            vStk(nOpnds - 2) = opA.Copy
                            vPos(nOpnds - 2) = vPos(nOpnds - 1)
                        End If

                        Dim sOld As StringBuilder
                        sOld = New StringBuilder(sL.ToString + sM.ToString + sR.ToString)
                        Dim nL As Int64 = Regex.Matches(sOld.ToString, "\(").Count
                        Dim nR As Int64 = Regex.Matches(sOld.ToString, "\)").Count
                        If nL < nR Then
                            sR = New StringBuilder(Replace(sR.ToString, ")", "", 1, nR - nL))
                        End If

                        ' Enclose current operation
                        If format = outputMessage.RichTextFormat Then
                            vPreOp(iv) = New StringBuilder(Replace(sL.ToString +
                                    "\cf3\b [ " + sM.ToString + "  ]\cf1\b0 " + sR.ToString, "+-", "-"))
                            If sMInterm.Length Then
                                vIntermOp(iv) = New StringBuilder(Replace(sL.ToString +
                                    "\cf3\b [ " + sMInterm.ToString + "  ]\cf1\b0 " + sR.ToString, "+-", "-"))
                            End If
                        ElseIf format = outputMessage.plainText Then
                            vPreOp(iv) = New StringBuilder(Replace(sL.ToString +
                                    " [ " + sM.ToString + " ] " + sR.ToString, "+-", "-"))
                            If sMInterm.Length Then
                                vIntermOp(iv) = New StringBuilder(Replace(sL.ToString +
                                    " [ " + sMInterm.ToString + "  ] " + sR.ToString, "+-", "-"))
                            End If
                        Else
                            vPreOp(iv) = New StringBuilder(Replace(sL.ToString +
                                    " <span style=""color:orange"">[ " +
                                    sM.ToString + " ]</span> " + sR.ToString, "+-", "-"))
                            If sMInterm.Length Then
                                vIntermOp(iv) = New StringBuilder(Replace(sL.ToString +
                                    " <span style=""color:orange"">[ " +
                                    sMInterm.ToString + " ]</span> " + sR.ToString, "+-", "-"))
                            End If
                        End If
                        'ReDim Preserve vPreOp(iv), vPostOp(iv)
                        If numOpnds = 2 OrElse tipo = tokenType84.fn Then
                            For iOpA = iB + iAddB To iA - iAddA Step -1
                                If vExpr(iOpA) Is Nothing Then
                                ElseIf vExpr(iOpA).Length Then
                                    vExpr(iOpA).Remove(0, vExpr(iOpA).Length)
                                End If
                            Next
                        End If
                        If tipo = tokenType84.chgSgn Then
                            If sL.Length = 1 Then
                                sL.Remove(0, sL.Length)
                            ElseIf sL.Length Then
                                sL.Remove(sL.Length - 1, 1)
                            End If
                            vExpr(iA - 1).Remove(0, vExpr(iA - 1).Length)
                            vExpr(iA).Append(opA.ToString)
                        Else
                            If numOpnds = 2 Then
                                vExpr(iA0) = New StringBuilder(sMNew)
                                nOpnds -= 1
                            ElseIf tipo = tokenType84.fn Then ' function
                                vExpr(iB0) = New StringBuilder(sMNew)
                            ElseIf oStack(i).subTipo <> 33 Then ' <> "!"
                                For iOpA = iOp + 3 To iOp + 1 Step -1
                                    vExpr(iOpA).Remove(0, vExpr(iOpA).Length)
                                Next
                                vExpr(iOpA) = New StringBuilder(sMNew)
                            Else
                                vExpr(iOp) = New StringBuilder(sMNew) ' "!"
                                vExpr(iOp - 1).Remove(0, vExpr(iOp - 1).Length)
                            End If
                        End If
                        sExpr = New StringBuilder(sL.ToString + sMNew.ToString + sR.ToString)
                        If Not Replace(sM.ToString, " ", "") = Replace(sMNew.ToString, " ", "") OrElse
                        (tipo = tokenType84.chgSgn AndAlso i + 1 >= iSt) Then

                            If sOld.ToString <> sL.ToString + sMNew.ToString + sR.ToString Then
                                If format = outputMessage.plainText Then
                                    vPostOp(iv) = New StringBuilder(
                                            Replace(
                                            sL.ToString + " [ " +
                                            sMNew.ToString + " ] " + sR.ToString, "+-", "-"))
                                ElseIf format = outputMessage.RichTextFormat Then
                                    vPostOp(iv) = New StringBuilder(
                                            Replace(
                                            sL.ToString + "\cf2\b [ " +
                                            sMNew.ToString + " ] \cf1\b0 " + sR.ToString, "+-", "-"))
                                Else
                                    vPostOp(iv) = New StringBuilder(sL.ToString +
                                            Replace(
                                            " <span style=""color:red"">[ " +
                                            sMNew.ToString + " ]</span> " +
                                            sR.ToString, "+-", "-"))
                                End If
                            End If
                            vRet.Append(vPreOp(iv))
                            vRet.Append(vbCrLf + "= ")
                            If vIntermOp(iv) IsNot Nothing Then
                                vRet.Append(vIntermOp(iv))
                                vRet.Append(vbCrLf + "= ")
                            End If
                            vRet.Append(vPostOp(iv))
                            vRet.Append(vbCrLf)
                            vRet.Append(vbCrLf)
                            iv += 1
                        End If


                    Case tokenType84.oprnd
                        vStk(nOpnds) = oStack(i).Copy
                        vIExpr(nOpnds) = oStack(i).iToken
                        nOpnds += 1
                    Case tokenType84.chgSgn
                        'vStk(nOpnds - 1) = oStack(i).Copy
                        If oStack(i - 1).tipo <> tokenType84.var Then
                            'vStk(nOpnds - 1) = -vStk(nOpnds - 1)
                        End If
                        GoTo chgsgn
                    Case tokenType84.var
                        vIExpr(nOpnds) = oStack(i).iToken
                        vStk(nOpnds) = oStack(i).Copy
                        Dim index As Int64 = Array.IndexOf(vNames, oStack(i).sTkn)
                        If index > -1 Then
                            vStk(nOpnds).dblVal = vValues(index)
                            vStk(nOpnds).tipo = tokenType84.oprnd
                        Else
                            vStk(nOpnds).tipo = tokenType84.Polynomial84
                            vStk(nOpnds).dblVal = New ExprMatrix(Expression.ParseExpression(vStk(nOpnds).sTkn))
                        End If
                        'vExpr(nOpnds) = New StringBuilder(vStk(i).ToString)
                        nOpnds += 1
                    Case tokenType84.fn
                        vIExpr(nOpnds) = oStack(i).iToken
                        vStk(nOpnds) = oStack(i).Copy
                        vStk(nOpnds).dblVal =
                                exprParser.evalFn(oStack(i).sTkn, vStk(nOpnds - 1).dblVal)
                End Select
                If eP.err IsNot Nothing Then
                    Return False
                End If

                i += 1
            Loop While i < iSt
            If Len(sDivisions) Then
                '    vRet.Append(sDivisions)
            End If
            If format = outputMessage.RichTextFormat Then
                vRet = New StringBuilder(
                    msg884.rtfInit +
                    Replace(vRet.ToString, vbCrLf, "\line" + vbCrLf))
            End If
            Result = opA
        Catch ex As Exception
            eP.err = ex
            Return False
        End Try
        sResult = vRet.ToString
        Return True
    End Function
    Public Function Eval(ByRef Result As Token, Optional vNames() As String = Nothing,
                         Optional vValues() As ExprMatrix = Nothing,
                               Optional ByRef cplxRoots() As Complex = Nothing) As Boolean

        ' http://en.wikipedia.org/wiki/Reverse_Polish_notation

        ' Array vDbl() will contain the (numeric) operands:
        Dim vStk(iSt) As Token, nOpnds As Int32
        Dim i, index As Int32
        Try
            If iSt = 1 Then
                vStk(0) = oStack(0).Copy
                Result = vStk(0)
                Exit Try
            End If
            If vNames Is Nothing Then
                ReDim vNames(-1)
            End If
            Do
                Select Case oStack(i).tipo
                    Case tokenType84.oprnd
                        vStk(nOpnds) = oStack(i).Copy
                        nOpnds += 1
                    Case tokenType84.optor
                        nOpnds -= 1
                        ' Operate:
                        ' -  +  *  /  ^  %  !
                        '45 43 42 47 94 37 33
                        Select Case oStack(i).subTipo
                            Case optor.add : vStk(nOpnds - 1) += vStk(nOpnds)
                            Case optor.substract : vStk(nOpnds - 1) -= vStk(nOpnds)
                            Case optor.multiply : vStk(nOpnds - 1) *= vStk(nOpnds)
                            Case optor.divide : vStk(nOpnds - 1) /= vStk(nOpnds)
                            Case optor.power : vStk(nOpnds - 1) ^= vStk(nOpnds)
                            Case optor.modulo : vStk(nOpnds - 1) =
                                vStk(nOpnds - 1) Mod vStk(nOpnds)
                            Case optor.factorial  ' "!" has 1 operand
                                Dim dbl As Double = vStk(nOpnds).dblVal.getExpr(0, 0).toDouble
                                For j As Int64 = Math.Floor(dbl) - 1 To 2 Step -1
                                    dbl *= j
                                Next
                                vStk(nOpnds).dblVal = New ExprMatrix(dbl)
                                nOpnds += 1
                            Case optor.and : vStk(nOpnds - 1) = vStk(nOpnds - 1) And vStk(nOpnds)
                            Case optor.or : vStk(nOpnds - 1) = vStk(nOpnds - 1) Or vStk(nOpnds)
                            Case optor.not : vStk(nOpnds) = Not vStk(nOpnds)
                            Case optor.xor : vStk(nOpnds - 1) = vStk(nOpnds - 1) Xor vStk(nOpnds)
                            Case optor.nor : vStk(nOpnds - 1) = Not (vStk(nOpnds - 1) Or vStk(nOpnds)) ' NOR
                            Case optor.nand : vStk(nOpnds - 1) = Not (vStk(nOpnds - 1) And vStk(nOpnds)) ' NAND
                        End Select
                    Case tokenType84.var
                        vStk(nOpnds) = oStack(i).Copy
                        index = Array.IndexOf(vNames, oStack(i).sTkn)
                        If index > -1 Then
                            vStk(nOpnds).dblVal = vValues(index)
                            vStk(nOpnds).tipo = tokenType84.oprnd
                        Else
                            vStk(nOpnds).tipo = tokenType84.expression
                            Dim Pa As Polynomial = Polynomial.GetPolynomial(vStk(nOpnds).sTkn)
                            vStk(nOpnds).dblVal = New ExprMatrix(Pa)
                        End If
                        nOpnds += 1
                    Case tokenType84.chgSgn
                        vStk(nOpnds - 1) = oStack(i).Copy
                        vStk(nOpnds - 1) = -vStk(nOpnds - 1)
                    Case tokenType84.fn
                        If oStack(i).sTkn = "roots" Then
                            Dim opRR As retOpRoots = Polynomial.opRoots(
                                vStk(nOpnds - 1).dblVal.getCurExpr.getPolynomial)
                            If opRR IsNot Nothing Then
                                cplxRoots = opRR.cjo
                            Else
                                ReDim cplxRoots(-1)
                                'eP.err = New msg8(eP, "Found no roots")
                            End If
                            Exit Try
                        Else
                            vStk(nOpnds - 1) = oStack(i).Copy
                            vStk(nOpnds - 1).tipo = tokenType84.oprnd
                            vStk(nOpnds - 1).dblVal =
                                exprParser.evalFn(oStack(i).sTkn, vStk(nOpnds - 1).dblVal)
                        End If
                End Select
                If eP IsNot Nothing AndAlso eP.err IsNot Nothing Then
                    Return False
                End If
                i += 1
            Loop While i < iSt
            Result = vStk(0)
        Catch ex As Exception
            If eP IsNot Nothing Then
                eP.err = ex
            End If
            Return False
        End Try
        Return True
    End Function

    Public Shared Function ToImage(ByRef Result As Bitmap,
                         oStack() As Token,
                            Optional vNames() As String = Nothing,
                         Optional vValues() As ExprMatrix = Nothing,
                         Optional ByRef cplxRoots() As Complex = Nothing,
                         Optional cfg As Config = Nothing) As Boolean

        Try
            Dim iSt As Int32 = oStack.Length
            Dim vStk(iSt) As Bitmap, nOpnds As Int32
            Dim i, index As Int32
            If cfg Is Nothing Then
                cfg = New Config
            End If
            If iSt = 1 Then
                Result = TokenTobitmap(oStack, 0, cfg)
                Return True
            End If
            If vNames Is Nothing Then
                ReDim vNames(-1)
            End If
            Dim vOpnds(oStack.Length - 1) As Token
            Dim m As Int32 = 4
            Dim fondo As Brush = Brushes.White
            Dim vRP(oStack.Length - 1) As Int32
            Dim iRP As Int32 = -1
            Dim bRP As Boolean = False
            Dim vHalf(oStack.Length - 1) As Int32
            Do
                If oStack(i).sTkn = "(" Then
                    iRP += 1
                    vRP(iRP) = nOpnds + 1
                ElseIf oStack(i).sTkn = ")" Then
                    bRP = True
                End If
                Select Case oStack(i).tipo
                    Case tokenType84.oprnd
                        vStk(nOpnds) = TokenTobitmap(oStack, i, cfg)
                        vHalf(nOpnds) = vStk(nOpnds).Height / 2
                        vOpnds(nOpnds) = oStack(i)
                        nOpnds += 1
                    Case tokenType84.optor
                        nOpnds -= 1
                        ' Operate:
                        ' -  +  *  /  ^  %  !
                        '45 43 42 47 94 37 33
                        Select Case oStack(i).subTipo
                            Case optor.add : vOpnds(nOpnds - 1) += vOpnds(nOpnds)
                            Case optor.substract : vOpnds(nOpnds - 1) -= vOpnds(nOpnds)
                            Case optor.multiply : vOpnds(nOpnds - 1) *= vOpnds(nOpnds)
                            Case optor.divide : vOpnds(nOpnds - 1) /= vOpnds(nOpnds)
                            Case optor.power : vOpnds(nOpnds - 1) ^= vOpnds(nOpnds)
                            Case optor.modulo : vOpnds(nOpnds - 1) =
                                vOpnds(nOpnds - 1) Mod vOpnds(nOpnds)
                            Case optor.factorial  ' "!" has 1 operand
                                Dim dbl As Double = vOpnds(nOpnds).dblVal.getExpr(0, 0).toDouble
                                For j As Int64 = Math.Floor(dbl) - 1 To 2 Step -1
                                    dbl *= j
                                Next
                                vOpnds(nOpnds).dblVal = New ExprMatrix(dbl)
                                'nOpnds += 1
                            Case optor.and : vOpnds(nOpnds - 1) = vOpnds(nOpnds - 1) And vOpnds(nOpnds)
                            Case optor.or : vOpnds(nOpnds - 1) = vOpnds(nOpnds - 1) Or vOpnds(nOpnds)
                            Case optor.not : vOpnds(nOpnds) = Not vOpnds(nOpnds)
                            Case optor.xor : vOpnds(nOpnds - 1) = vOpnds(nOpnds - 1) Xor vOpnds(nOpnds)
                            Case optor.nor : vOpnds(nOpnds - 1) = Not (vOpnds(nOpnds - 1) Or vOpnds(nOpnds)) ' NOR
                            Case optor.nand : vOpnds(nOpnds - 1) = Not (vOpnds(nOpnds - 1) And vOpnds(nOpnds)) ' NAND

                        End Select
                        Select Case oStack(i).subTipo
                            Case optor.not
                                Dim A As Bitmap = vStk(nOpnds)
                                Dim AH As Int32 = vHalf(nOpnds)
                                Dim op As Bitmap = Nothing
                                Dim mxH As Int32 = A.Height
                                If AH > mxH / 2 Then
                                    mxH = AH * 2
                                End If
                                Dim R As Bitmap = Nothing
                                op = TokenTobitmap(Nothing, 0, cfg, oStack(i).sTkn)
                                If bRP AndAlso iRP > -1 AndAlso vRP(iRP) = nOpnds - 1 Then
                                    Dim lp As Bitmap = TokenTobitmap(Nothing, 0, cfg, "(")
                                    Dim rp As Bitmap = TokenTobitmap(Nothing, 0, cfg, ")")
                                    R = New Bitmap(lp.Width + A.Width + rp.Width + op.Width, mxH)
                                    Dim g As Graphics = Graphics.FromImage(R)
                                    g.Clear(Color.White)
                                    g.DrawImage(lp, New Point(0, (mxH - rp.Height) / 2))
                                    g.DrawImage(op, New Point(lp.Width, (mxH - op.Height) / 2))
                                    g.DrawImage(A, New Point(lp.Width + op.Width, mxH / 2 - AH))
                                    g.DrawImage(rp, New Point(lp.Width + A.Width + op.Width, (mxH - rp.Height) / 2))
                                    iRP -= 1
                                    bRP = False
                                Else
                                    R = New Bitmap(A.Width + op.Width, mxH)
                                    Dim g As Graphics = Graphics.FromImage(R)
                                    g.Clear(Color.White)
                                    g.DrawImage(op, New Point(0, (mxH - op.Height) / 2))
                                    g.DrawImage(A, New Point(op.Width, mxH / 2 - AH))
                                End If
                                vStk(nOpnds) = R
                                vHalf(nOpnds) = mxH / 2
                                nOpnds += 1

                            Case optor.factorial
                                Dim A As Bitmap = vStk(nOpnds)
                                Dim AH As Int32 = vHalf(nOpnds)
                                Dim op As Bitmap = Nothing
                                Dim mxH As Int32 = A.Height
                                Dim R As Bitmap = Nothing
                                op = TokenTobitmap(Nothing, 0, cfg, oStack(i).sTkn)
                                If bRP AndAlso iRP > -1 AndAlso vRP(iRP) = nOpnds - 1 Then
                                    Dim lp As Bitmap = TokenTobitmap(Nothing, 0, cfg, "(")
                                    Dim rp As Bitmap = TokenTobitmap(Nothing, 0, cfg, ")")
                                    R = New Bitmap(lp.Width + A.Width + rp.Width + op.Width, mxH)
                                    Dim g As Graphics = Graphics.FromImage(R)
                                    g.Clear(Color.White)
                                    g.DrawImage(lp, New Point(0, (mxH - rp.Height) / 2))
                                    g.DrawImage(A, New Point(lp.Width, mxH / 2 - AH))
                                    g.DrawImage(op, New Point(lp.Width + A.Width, (mxH - op.Height) / 2))
                                    g.DrawImage(rp, New Point(lp.Width + A.Width + op.Width, (mxH - rp.Height) / 2))
                                    iRP -= 1
                                    bRP = False
                                Else
                                    R = New Bitmap(A.Width + op.Width, mxH)
                                    Dim g As Graphics = Graphics.FromImage(R)
                                    g.Clear(Color.White)
                                    g.DrawImage(A, New Point(0, mxH / 2 - AH))
                                    g.DrawImage(op, New Point(A.Width, (mxH - op.Height) / 2))
                                End If
                                vStk(nOpnds) = R
                                vHalf(nOpnds) = mxH / 2
                                nOpnds += 1

                            Case optor.add, optor.substract, optor.multiply, optor.modulo,
                                 optor.and, optor.or, optor.xor, optor.nand, optor.nor
toModulo:

                                Dim A As Bitmap = vStk(nOpnds - 1)
                                Dim AH As Int32 = vHalf(nOpnds - 1)

                                Dim bAmtx As Boolean = IIf(vOpnds(nOpnds - 1).dblVal.Rows + vOpnds(nOpnds - 1).dblVal.Cols > 2, True, False)
                                Dim bbmtx As Boolean = IIf(vOpnds(nOpnds).dblVal.Rows + vOpnds(nOpnds).dblVal.Cols > 2, True, False)
                                Dim op As Bitmap = Nothing
                                op = TokenTobitmap(Nothing, 0, cfg, oStack(i).sTkn)
                                Dim B As Bitmap = vStk(nOpnds)
                                Dim BH As Int32 = vHalf(nOpnds)
                                Dim mxH As Int32 = Math.Max(AH, BH)
                                'If AH > mxH / 2 Then
                                '    mxH = AH * 2
                                'End If
                                'If BH > mxH / 2 Then
                                '    mxH = BH * 2
                                'End If
                                Dim R As Bitmap = Nothing
                                Dim mas As Int32 = IIf(oStack(i).subTipo = optor.multiply, 1, 0)
                                If bRP AndAlso iRP > -1 AndAlso vRP(iRP) = nOpnds Then
                                    Dim lp As Bitmap = TokenTobitmap(Nothing, 0, cfg, "(")
                                    Dim rp As Bitmap = TokenTobitmap(Nothing, 0, cfg, ")")
                                    R = New Bitmap(lp.Width + A.Width + rp.Width + op.Width + B.Width, Math.Max(A.Height, B.Height))
                                    Dim g As Graphics = Graphics.FromImage(R)
                                    g.Clear(Color.White)
                                    'g.DrawImage(lp, New Point(0, (mxH - rp.Height) / 2))
                                    'g.DrawImage(A, New Point(lp.Width, (mxH - A.Height) / 2))
                                    'g.DrawImage(op, New Point(lp.Width + A.Width, (mxH - op.Height) / 2))
                                    'g.DrawImage(B, New Point(lp.Width + A.Width + op.Width, (mxH - B.Height) / 2))
                                    'g.DrawImage(rp, New Point(lp.Width + A.Width + op.Width + B.Width, (mxH - rp.Height) / 2))
                                    g.DrawImage(lp, New Point(0, mxH - rp.Height / 2))
                                    g.DrawImage(A, New Point(lp.Width, mxH - AH))
                                    If bAmtx OrElse bbmtx Then
                                        g.DrawImage(op, New Point(lp.Width + A.Width, mas + mxH - op.Height / 2))
                                    Else
                                        g.DrawImage(op, New Point(lp.Width + A.Width, mas + mxH - op.Height / 2))
                                    End If
                                    g.DrawImage(B, New Point(lp.Width + A.Width + op.Width, mxH - BH))
                                    g.DrawImage(rp, New Point(lp.Width + A.Width + op.Width + B.Width, mxH - rp.Height / 2))
                                    iRP -= 1
                                    bRP = False
                                Else
                                    R = New Bitmap(A.Width + op.Width + B.Width, Math.Max(A.Height, B.Height))
                                    Dim g As Graphics = Graphics.FromImage(R)
                                    g.Clear(Color.White)
                                    g.DrawImage(A, New Point(0, mxH - AH))
                                    If bAmtx OrElse bbmtx Then
                                        g.DrawImage(op, New Point(A.Width, mas + mxH - op.Height / 2))
                                    Else
                                        g.DrawImage(op, New Point(A.Width, mas + mxH - op.Height / 2))
                                    End If
                                    g.DrawImage(B, New Point(A.Width + op.Width, mxH - BH))
                                End If
                                vStk(nOpnds - 1) = R
                                vHalf(nOpnds - 1) = mxH
                            Case optor.power
                                Dim A As Bitmap = vStk(nOpnds - 1)
                                Dim B As Bitmap = vStk(nOpnds)
                                Dim mxH As Int32 = A.Height * 1 / 3 + B.Height
                                Dim R As Bitmap = Nothing
                                If bRP AndAlso iRP > -1 AndAlso vRP(iRP) = nOpnds Then
                                    Dim lp As Bitmap = TokenTobitmap(Nothing, 0, cfg, "(")
                                    Dim rp As Bitmap = TokenTobitmap(Nothing, 0, cfg, ")")
                                    R = New Bitmap(lp.Width + A.Width + rp.Width + B.Width,
                                                   CInt(A.Height * 2 / 3 + B.Height))
                                    Dim g As Graphics = Graphics.FromImage(R)
                                    g.Clear(Color.White)
                                    g.DrawImage(lp, New Point(0, R.Height - A.Height + lp.Height / 2))
                                    g.DrawImage(A, New Point(lp.Width, R.Height - A.Height))
                                    g.DrawImage(B, New Point(lp.Width + A.Width, 0))
                                    g.DrawImage(rp, New Point(lp.Width + A.Width + B.Width, R.Height - A.Height + rp.Height / 2))
                                    iRP -= 1
                                    bRP = False
                                Else
                                    R = New Bitmap(A.Width + B.Width,
                                                   CInt(A.Height * 2 / 3 + B.Height))
                                    Dim g As Graphics = Graphics.FromImage(R)
                                    g.Clear(Color.White)
                                    g.DrawImage(A, New Point(0, R.Height - A.Height))
                                    g.DrawImage(B, New Point(A.Width, 0))
                                End If
                                vStk(nOpnds - 1) = R
                                vHalf(nOpnds - 1) = R.Height - A.Height + vHalf(nOpnds - 1)
                            Case optor.divide
                                Dim A As Bitmap = vStk(nOpnds - 1)
                                Dim B As Bitmap = vStk(nOpnds)
                                Dim mxW As Int32 = Math.Max(A.Width, B.Width)
                                Dim mxH As Int32 = A.Height + 1 + B.Height
                                Dim R As New Bitmap(mxW, mxH)
                                Dim g As Graphics = Graphics.FromImage(R)
                                g.Clear(Color.White)
                                g.DrawImage(A, New Point((mxW - A.Width) / 2, 0))
                                g.DrawLine(Pens.Black, New Point(1, A.Height), New Point(mxW - 1, A.Height))
                                g.DrawImage(B, New Point((mxW - B.Width) / 2, A.Height + 1))
                                vStk(nOpnds - 1) = R
                                vHalf(nOpnds - 1) = A.Height + 1
                        End Select
                    Case tokenType84.var
                        Dim tkn As Token = oStack(i).Copy
                        index = Array.IndexOf(vNames, oStack(i).sTkn)
                        If index > -1 Then
                            tkn.dblVal = vValues(index)
                            tkn.tipo = tokenType84.oprnd
                        Else
                            tkn.tipo = tokenType84.expression
                            Dim Pa As Polynomial = Polynomial.GetPolynomial(tkn.sTkn)
                            tkn.dblVal = New ExprMatrix(Pa)
                        End If
                        vOpnds(nOpnds) = tkn
                        vStk(nOpnds) = TokenTobitmap(New Token() {tkn}, 0, cfg)
                        vHalf(nOpnds) = vStk(nOpnds).Height / 2
                        nOpnds += 1
                    Case tokenType84.chgSgn
                        vOpnds(nOpnds - 1) = -vOpnds(nOpnds - 1)
                        Dim tkn As Token = vOpnds(nOpnds - 1)
                        vStk(nOpnds - 1) = TokenTobitmap(New Token() {tkn}, 0, cfg)
                        vHalf(nOpnds - 1) = vStk(nOpnds - 1).Height / 2
                        vOpnds(nOpnds - 1) = tkn
                        'vOpnds(nOpnds - 1) = oStack(i).Copy
                        'vOpnds(nOpnds - 1) = -vOpnds(nOpnds - 1)
                    Case tokenType84.fn
                        If oStack(i).sTkn = "MOD" Then
                            nOpnds -= 1
                            GoTo toModulo
                        Else
                            Dim tkn As Token = oStack(i).Copy
                            tkn.tipo = tokenType84.oprnd
                            Dim A As Bitmap = Nothing
                            Dim AH As Int32 = 0
                            Dim bAmtx As Boolean
                            If InStr("CROSS|DOT", oStack(i).sTkn) Then
                                Dim eM As New ExprMatrix(cfg, 2, vOpnds(nOpnds - 1).dblVal.Cols)
                                For j As Int32 = 0 To eM.Cols - 1
                                    eM.getExpr(0, j) = vOpnds(nOpnds - 2).dblVal.getExpr(0, j)
                                    eM.getExpr(1, j) = vOpnds(nOpnds - 1).dblVal.getExpr(0, j)
                                Next
                                Dim tknA As New Token(tokenType84.matrix, 0, eM, 0, 0, "")
                                A = TokenTobitmap(New Token() {tknA}, 0, cfg)
                                AH = A.Height / 2
                                bAmtx = True
                                nOpnds -= 1
                            ElseIf InStr("LCM|GCD", oStack(i).sTkn) Then
                                Dim eM As New ExprMatrix(cfg, 1, 2)
                                eM.getExpr(0, 0) = vOpnds(nOpnds - 2).dblVal.getExpr(0, 0)
                                eM.getExpr(0, 1) = vOpnds(nOpnds - 1).dblVal.getExpr(0, 0)
                                Dim tknA As New Token(tokenType84.matrix, 0, eM, 0, 0, "")
                                A = TokenTobitmap(New Token() {tknA}, 0, cfg)
                                AH = A.Height / 2
                                nOpnds -= 1
                                bAmtx = True
                            Else
                                A = vStk(nOpnds - 1)
                                AH = vHalf(nOpnds - 1)
                                bAmtx = IIf(vOpnds(nOpnds - 1).dblVal.Rows > 1, True, False)
                            End If
                            Dim op As Bitmap = Nothing
                            op = TokenTobitmap(Nothing, 0, cfg, oStack(i).sTkn)
                            Dim mxH As Int32 = Math.Max(AH, op.Height / 2)
                            Dim R As Bitmap = Nothing
                            'Dim lp As Bitmap = TokenTobitmap(Nothing, cfg, "(")
                            'Dim rp As Bitmap = TokenTobitmap(Nothing, cfg, ")")
                            'R = New Bitmap(lp.Width + A.Width + rp.Width + op.Width, mxH)
                            R = New Bitmap(A.Width + op.Width, Math.Max(A.Height, op.Height))
                            Dim g As Graphics = Graphics.FromImage(R)
                            g.Clear(Color.White)
                            If bAmtx Then
                                g.DrawImage(op, New Point(0, mxH - op.Height / 2))
                            Else
                                g.DrawImage(op, New Point(0, mxH - op.Height / 2))
                            End If
                            'g.DrawImage(lp, New Point(op.Width, (mxH - lp.Height) / 2))
                            'g.DrawImage(A, New Point(op.Width + lp.Width, (mxH - A.Height) / 2))
                            'g.DrawImage(rp, New Point(op.Width + lp.Width + A.Width, (mxH - rp.Height) / 2))
                            g.DrawImage(A, New Point(op.Width, mxH - AH))

                            vStk(nOpnds - 1) = R
                            vHalf(nOpnds - 1) = mxH
                            'tkn.dblVal =
                            'exprParser.evalFn(oStack(i).sTkn, vOpnds(nOpnds - 1).dblVal)
                            'vOpnds(nOpnds - 1) = oStack(i).Copy
                            'vOpnds(nOpnds - 1).tipo = tokenType84.oprnd
                            'vOpnds(nOpnds - 1).dblVal =
                            'exprParser.evalFn(oStack(i).sTkn, vOpnds(nOpnds - 1).dblVal)
                        End If
                End Select
                i += 1
            Loop While i < iSt
            Result = vStk(0)
        Catch ex As Exception
            Return False
        End Try
        Return True
    End Function
    Private Shared Function TokenTobitmap(vtkn() As Token, iTkn As Int32, cfg As Config, Optional str As String = "") As Bitmap
        Dim bmp As Bitmap = Nothing
        Try
            Dim fntSize As Single = 10
            Dim fnt = New Font("Courier New", fntSize, FontStyle.Regular)
            Dim br As New SolidBrush(Color.Black)
            Dim g As Graphics = Nothing
            Dim fondo As Color = Color.White
            Dim tkn As Token = Nothing
            If str.Length = 0 Then
                tkn = vtkn(iTkn)
                Dim rows As Int32 = tkn.dblVal.Rows
                Dim cols As Int32 = tkn.dblVal.Cols
                If rows + cols = 2 Then
                    str = tkn.dblVal.ToStringExprMtx(cfg)
                    color1(str, br, fnt, fntSize, bmp)
                    If bmp IsNot Nothing Then
                        Return bmp
                    End If
                Else
                    Dim i, j As Int32
                    Dim bTwoCols As Boolean = IIf(rows = 1 AndAlso cols = 2, True, False)
                    If bTwoCols Then cols += 1
                    Dim vbmp(rows - 1, cols - 1) As Bitmap
                    Dim vS(rows - 1, cols - 1) As Size
                    Dim vmxH(rows - 1) As Int32
                    Dim vmxW(cols - 1) As Int32
                    If bTwoCols Then
                        ReDim vbmp(0, 2)
                        Dim e1 As String = tkn.dblVal.getExpr(0, 0).ToStringExpr(cfg)
                        vbmp(0, 0) = TokenTobitmap(Nothing, 0, cfg, e1)
                        vbmp(0, 1) = TokenTobitmap(Nothing, 0, cfg, ",")
                        e1 = tkn.dblVal.getExpr(0, 1).ToStringExpr(cfg)
                        vbmp(0, 2) = TokenTobitmap(Nothing, 0, cfg, e1)
                        i = 0
                        For j = 0 To cols - 1
                            vS(i, j) = New Size(vbmp(i, j).Width, vbmp(i, j).Height)
                            If vS(i, j).Height > vmxH(i) Then
                                vmxH(i) = vS(i, j).Height
                            End If
                            If vS(i, j).Width > vmxW(j) Then
                                vmxW(j) = vS(i, j).Width
                            End If
                        Next
                    Else
                        For i = 0 To rows - 1
                            For j = 0 To cols - 1
                                Dim e1 As String = tkn.dblVal.getExpr(i, j).ToStringExpr(cfg)
                                vbmp(i, j) = TokenTobitmap(Nothing, 0, cfg, e1)
                                vS(i, j) = New Size(vbmp(i, j).Width, vbmp(i, j).Height)
                                If vS(i, j).Height > vmxH(i) Then
                                    vmxH(i) = vS(i, j).Height
                                End If
                                If vS(i, j).Width > vmxW(j) Then
                                    vmxW(j) = vS(i, j).Width
                                End If
                            Next
                        Next
                    End If
                    Dim w As Int32 = 0
                    For j = 0 To cols - 1
                        w += vmxW(j)
                    Next
                    Dim h As Int32 = 0
                    For i = 0 To rows - 1
                        h += vmxH(i)
                    Next
                    bmp = New Bitmap(w + 2, h + 2)
                    g = Graphics.FromImage(bmp)
                    g.Clear(fondo)
                    h = 0
                    For i = 0 To rows - 1
                        w = 0
                        For j = 0 To cols - 1
                            g.DrawImage(vbmp(i, j), New Point(
                                    1 + w + (vmxW(j) - vS(i, j).Width) / 2,
                                    1 + h + (vmxH(i) - vS(i, j).Height) / 2))
                            w += vmxW(j)
                        Next
                        h += vmxH(i)
                    Next
                    h = bmp.Height - 1
                    w = bmp.Width - 1
                    g.DrawLine(Pens.Black, 0, 0, 0, h)
                    g.DrawLine(Pens.Black, w, 0, w, h)
                    g.DrawLine(Pens.Black, 0, 0, 2, 0)
                    g.DrawLine(Pens.Black, w - 2, 0, w, 0)
                    g.DrawLine(Pens.Black, 0, h, 2, h)
                    g.DrawLine(Pens.Black, w - 2, h, w, h)
                    Exit Try
                End If
            End If
            color1(str, br, fnt, fntSize, bmp)
            If bmp IsNot Nothing Then
                Return bmp
            End If
            Dim s As Size = tr.MeasureText(str, fnt)
            bmp = New Bitmap(s.Width - 2, s.Height)
            g = Graphics.FromImage(bmp)
            g.Clear(fondo)
            g.DrawString(str, fnt, br, New Point(0, 0))
            g.Dispose()
            br.Dispose()
        Catch ex As Exception

        End Try
        Return bmp
    End Function
    Private Shared Sub color1(e1 As String, ByRef br As Brush, ByRef fnt As Font, fntsize As Single, ByRef bmp As Bitmap)
        Try
            bmp = Nothing
            If Len(e1) = 1 Then
                If Regex.IsMatch(e1, "[-+*\/\^\!]") Then
                    br = New SolidBrush(Color.FromArgb(&HFF, &H17, &H44))
                    Exit Sub
                ElseIf Regex.IsMatch(e1, MathGlobal8.sFn) Then
                    fnt = New Font("Courier New", fntsize, FontStyle.Bold)
                    Exit Sub
                ElseIf Regex.IsMatch(e1, MathGlobal8.sLP) OrElse
                    Regex.IsMatch(e1, MathGlobal8.sRP) Then
                    br = New SolidBrush(Color.FromArgb(&HC0, &HC0, &HC0))
                    Exit Sub
                End If
            End If
        Catch ex As Exception

        End Try
        Try
            bmp = Nothing
            Dim m As Match
            Dim mp As New matrixParser
            mp.parse(e1)
            Dim vMc() As Match = mp.cur.vMc
            If vMc.Length = 1 Then
                m = vMc(0)
                If vMc.Length = 2 Then m = vMc(1)
                If m.Groups("num").Success Then
                    br = New SolidBrush(Color.FromArgb(&H5C, &H6B, &HC0))
                ElseIf m.Groups("op").Success Then
                    br = New SolidBrush(Color.FromArgb(&HFF, &H17, &H44))
                ElseIf m.Groups("fn").Success Then
                    fnt = New Font("Courier New", fntsize, FontStyle.Bold)
                ElseIf m.Groups("var2").Success OrElse m.Groups("var").Success Then
                    br = New SolidBrush(Color.FromArgb(&H95, &H67, &H33))
                ElseIf InStr("[]", m.ToString) Then
                    br = New SolidBrush(Color.FromArgb(&HC0, &HC0, &HC0))
                Else
                End If
            ElseIf vMc.Length > 1 Then
                Dim vBmp(vMc.Length - 1) As Bitmap
                Dim mxH As Int32 = 0
                Dim w As Int32 = 0
                Dim cfg As New Config
                For i As Int32 = 0 To vMc.Length - 1
                    vBmp(i) = TokenTobitmap(Nothing, 0, cfg, vMc(i).ToString)
                    If vBmp(i).Height > mxH Then
                        mxH = vBmp(i).Height
                    End If
                    w += vBmp(i).Width
                Next
                bmp = New Bitmap(w, mxH)
                Dim g As Graphics = Graphics.FromImage(bmp)
                g.Clear(Color.White)
                w = 0
                For i = 0 To vBmp.Length - 1
                    g.DrawImage(vBmp(i), New Point(w, 0))
                    w += vBmp(i).Width
                Next
            End If
            Exit Sub
        Catch ex As Exception

        End Try
        Try
            If Regex.IsMatch(e1, "[-+*\/\^\!]") Then
                br = New SolidBrush(Color.FromArgb(&HFF, &H17, &H44))
                Exit Try
            ElseIf Regex.IsMatch(e1, MathGlobal8.sFn) Then
                fnt = New Font("Courier New", fntsize, FontStyle.Bold)
                Exit Try
            ElseIf Regex.IsMatch(e1, MathGlobal8.sLP) OrElse
                    Regex.IsMatch(e1, MathGlobal8.sRP) Then
                br = New SolidBrush(Color.FromArgb(&HC0, &HC0, &HC0))
                Exit Try
            End If
        Catch ex As Exception

        End Try
    End Sub
End Structure

