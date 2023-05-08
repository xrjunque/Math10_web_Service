Imports System.Globalization
Imports System.Text
Imports System.Text.RegularExpressions

Public Structure RPN_Stack ' Reverse Polish Notation 
    ' http://en.wikipedia.org/wiki/Reverse_Polish_notation

    Public Shared us As System.Globalization.CultureInfo = Config84.us
    Friend iSt, iToken, iv As Int32
    Friend eP As exprParser84
    Friend oStack() As Token
    Friend iOp, iChgSg As Int32

    Friend Sub New(eP As exprParser84, size As Int32)
        Me.eP = eP
        ReDim oStack(size * 2)
        iSt = 0
    End Sub
    Friend Sub Clear()
        iSt = 0 : iToken = 0 : iOp = 0 : iChgSg = 0
    End Sub
    Friend Sub Add(elem As Token)
        Try
            elem.eP = eP
            oStack(iSt) = elem
            iSt += 1
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Friend Function bInitialized() As Int32
        Return (oStack IsNot Nothing)
    End Function
    Friend ReadOnly Property length() As Int32
        Get
            Return Me.iSt
        End Get
    End Property
    Public Function Eval(ByRef Result As Token, Optional vNames() As String = Nothing,
                         Optional vValues() As ExprMatrix = Nothing,
                               Optional ByRef cplxRoots() As Complex84 = Nothing) As Boolean

        ' http://en.wikipedia.org/wiki/Reverse_Polish_notation

        ' Array vDbl() will contain the (numeric) operands:
        Dim vStk(iSt) As Token, nOpnds As Int32
        Dim i, index As Int32
        Try
            If iSt < 1 Then
                If iSt = 0 OrElse oStack(0) Is Nothing Then
                    Dim err8 As New msg884(eP)
                    eP.err = err8.num(8) ' Stack is empty
                    Exit Try
                ElseIf iSt = 1 Then
                    vStk(0) = oStack(0).Copy
                    Exit Try
                End If
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
                            Case 43 : vStk(nOpnds - 1) += vStk(nOpnds)
                            Case 45 : vStk(nOpnds - 1) -= vStk(nOpnds)
                            Case 42 : vStk(nOpnds - 1) *= vStk(nOpnds)
                            Case 47 : vStk(nOpnds - 1) /= vStk(nOpnds)
                            Case 94 : vStk(nOpnds - 1) ^= vStk(nOpnds)
                            Case 37
                                vStk(nOpnds - 1) = Token.opFuntion("mod", vStk(nOpnds), vStk(nOpnds - 1))
                                Dim eMA As ExprMatrix = vStk(nOpnds - 1).dblVal
                                Dim eMB As ExprMatrix = vStk(nOpnds).dblVal
                                Dim opA As Polynomial = eMA.getExpr(0, 0).getPolynomial
                                Dim opb As Polynomial = eMB.getExpr(0, 0).getPolynomial
                                If Not (opA.isReal AndAlso opb.isReal) Then
                                    oStack(i).sTkn = "REMAINDER"
                                Else
                                    oStack(i).sTkn = "MOD"
                                End If



                            Case 33 ' "!" has 1 operand
                                For j As Int64 = Math.Floor(vStk(nOpnds).dblVal.toDouble) - 1 To 2 Step -1
                                    vStk(nOpnds).dblVal *= New ExprMatrix(j)
                                Next
                                nOpnds += 1
                            Case Else
                                Select Case oStack(i).subTipo
                                    Case optor.and : vStk(nOpnds - 1) = vStk(nOpnds - 1) And vStk(nOpnds)
                                    Case optor.or : vStk(nOpnds - 1) = vStk(nOpnds - 1) Or vStk(nOpnds)
                                    Case optor.not : vStk(nOpnds) = Not vStk(nOpnds)
                                    Case optor.xor : vStk(nOpnds - 1) = vStk(nOpnds - 1) Xor vStk(nOpnds)
                                    Case optor.nor : vStk(nOpnds - 1) = Not (vStk(nOpnds - 1) Or vStk(nOpnds)) ' NOR
                                    Case optor.nand : vStk(nOpnds - 1) = Not (vStk(nOpnds - 1) And vStk(nOpnds)) ' NAND
                                End Select
                        End Select
                    Case tokenType84.var
                        vStk(nOpnds) = oStack(i).Copy
                        index = Array.IndexOf(vNames, oStack(i).sTkn)
                        If index > -1 Then
                            vStk(nOpnds).dblVal = vValues(index)
                            vStk(nOpnds).tipo = tokenType84.oprnd
                        Else
                            vStk(nOpnds).tipo = tokenType84.Polynomial84
                            vStk(nOpnds).dblVal = New ExprMatrix(
                                Expression.parseExpression(vStk(nOpnds).sTkn))
                        End If
                        nOpnds += 1
                    Case tokenType84.chgSgn
                        'vStk(nOpnds - 1) = oStack(i).Copy
                        vStk(nOpnds - 1) = -vStk(nOpnds - 1)
                    Case tokenType84.fn
                        Dim sFn As String = oStack(i).sTkn
                        If sFn = "roots" Then
                            'Dim opRR As retOpRoots =
                            '    Polynomial.opRoots(
                            '    vStk(nOpnds - 1).dblVal.getExpr(0, 0).getPolynomial)
                            'If opRR IsNot Nothing Then
                            '    cplxRoots = opRR.cjo
                            'Else
                            '    ReDim cplxRoots(-1)
                            '    'eP.err = New msg884(eP, "Found no roots")
                            'End If
                            Exit Try
                        Else
                            Dim vFn() As String = {"arg", "gcd", "lcm", "mod"}
                            If Array.IndexOf(vFn, sFn) = -1 Then
                                vStk(nOpnds - 1) = Token.opFuntion(sFn, vStk(nOpnds - 1), Nothing)
                            Else
                                vStk(nOpnds - 1) = Token.opFuntion(sFn, vStk(nOpnds ), vStk(nOpnds - 1))
                                nOpnds -= 1
                            End If
                            'Case Else
                            'vStk(nOpnds - 1) = oStack(j).Copy
                        End If
                End Select
                If eP.err IsNot Nothing Then
                    Return False
                End If
                i += 1
            Loop While i < iSt
            Result = vStk(0)
        Catch ex As Exception
            If index = -1 Then
                eP.err = New msg884(eP, 12, New String() {oStack(i).sTkn})
            Else
                eP.err = ex
            End If
            Return False
        End Try
        Return True
    End Function

    Public Function ToStringStack(ByRef sResult As String, Optional vVarNames() As String = Nothing,
                         Optional vValues() As ExprMatrix = Nothing,
                         Optional bGroup As Boolean = False,
                         Optional bShowVarValues As Boolean = True) As Boolean
        Dim sRet As New StringBuilder
        Dim sFn As String = Join(Config84.vFn, "|")
        Dim i As Int32
        Try
            For i = 0 To length - 1
                If oStack(i).tipo >= -3 Then
                    Dim e1 As String =
                        oStack(i).ToStringStackTkn(
                         vVarNames, vValues, bShowVarValues)
                    If eP.err IsNot Nothing Then
                        Return False
                    End If
                    If oStack(i).tipo = tokenType84.chgSgn Then
                        e1 = "NEG"
                    ElseIf oStack(i).tipo = tokenType84.fn Then
                        e1 = UCase(e1)
                        If e1 = "SQR" OrElse e1 = "SQRT" Then
                            e1 = "√"
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
        sResult = sRet.ToString
        Return True
    End Function
    Public Function ToStringDetail(
                    ByRef Result As Token,
                    ByRef sResult As String,
                    Optional vNames() As String = Nothing,
                    Optional vValues() As ExprMatrix = Nothing,
                    Optional format As outputMessage = outputMessage.plainText,
                    Optional vTkn() As Token = Nothing) As Boolean

        If vTkn IsNot Nothing Then
            oStack = vTkn
            iSt = oStack.Length
        End If
        Dim vRet As New StringBuilder(300)
        ' Array vDbl() will contain the (numeric) operands:
        Dim vStk(iSt) As Token, vPos(iSt) As Int32
        Dim vPreOp(iSt * 2), vIntermOp(iSt * 2), vPostOp(iSt * 2) As StringBuilder
        Dim tipo As tokenType84
        Dim i As Int32
        Try
            Dim vExpr(iSt * 2 - 1) As StringBuilder
            'Dim bHasOptors As Boolean = False
            Dim iMax As Int32 = 0

            Dim iLastOpnd As Int32 = -1
            Dim itknLastOpnd As Int32 = -1
            Dim nOpnds As Int32 = 0
            Dim vOpndns(-1) As Int32, ivopnds As Int32 = 0
            Dim bFirst As Boolean = True
            For i = 0 To iSt - 1
                Dim iTkn As Int32 = oStack(i).iToken
                tipo = oStack(i).tipo
                If tipo = tokenType84.var Then
                    'Dim bDoVar As Boolean = IIf(nOpnds > 1, True, False)
                    'If Not bDoVar Then
                    '    Dim i1 As Int32 = i + 1
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
                        Dim index As Int32 = Array.IndexOf(vNames, oStack(i).sTkn)
                        If index > -1 Then
                            vStk(i).tipo = tokenType84.oprnd
                            vStk(i).dblVal = vValues(index)
                        Else
                            vStk(i).tipo = tokenType84.Polynomial84
                            vStk(i).dblVal = New ExprMatrix(Expression.parseExpression(vStk(i).sTkn))
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
            Dim sExpr As New StringBuilder(eP.sbExpr.Length)
            ' = Join(vExpr, "")
            For i = 0 To vExpr.Length - 1
                sExpr.Append(vExpr(i))
            Next
            If oStack(iSt - 1).tipo = tokenType84.chgSgn Then
                sExpr.Insert(0, "-")
            End If



            nOpnds = 0
            Dim vIExpr(iSt) As Int32
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
                        Dim numOpnds As Int32 = 1
                        opA = vStk(nOpnds - 1)
                        Dim opB As Token = Nothing
                        Dim iOp As Int32 = oStack(i).iToken
                        Dim iOpA As Int32
                        Dim bIsMinus As Boolean = False
                        Dim addToiA As Int32 = 0
                        Dim iAddA As Int32 = 0
                        Dim iAddB As Int32 = 0


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
                        Dim iA As Int32 = vIExpr(nOpnds - numOpnds) + addToiA
                        Dim iB As Int32 = vIExpr(nOpnds - 1)



                        Dim iA0 As Int32 = iA
                        Dim iB0 As Int32 = iB
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
                        For j1 As Int32 = 0 To iA - 1 - iAddA
                            sL.Append(vExpr(j1))
                        Next

                        For j1 As Int32 = iB + 1 + iAddB To vExpr.Length - 1
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
                        Dim nL As Int32 = Regex.Matches(sOld.ToString, "\(").Count
                        Dim nR As Int32 = Regex.Matches(sOld.ToString, "\)").Count
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
                            vStk(nOpnds - 1) = -vStk(nOpnds - 1)
                        End If
                        GoTo chgsgn
                    Case tokenType84.var
                        vIExpr(nOpnds) = oStack(i).iToken
                        vStk(nOpnds) = oStack(i).Copy
                        Dim index As Int32 = Array.IndexOf(vNames, oStack(i).sTkn)
                        If index > -1 Then
                            vStk(nOpnds).dblVal = vValues(index)
                            vStk(nOpnds).tipo = tokenType84.oprnd
                        Else
                            vStk(nOpnds).tipo = tokenType84.Polynomial84
                            vStk(nOpnds).dblVal = New ExprMatrix(Expression.parseExpression(vStk(nOpnds).sTkn))
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
End Structure

