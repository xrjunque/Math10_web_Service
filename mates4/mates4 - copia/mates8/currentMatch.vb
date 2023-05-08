Imports System.Text.RegularExpressions
Imports System.Text

<Serializable()> _
Public Class currentMatch
    Friend imc As Int64
    Friend vMc() As Match
    Dim vsb() As StringBuilder
    Dim vExpr() As Expression
    Friend vStack() As stackDetail, ivs As Int64
    Dim _tipo As exprType
    Dim _subT1 As exprSubType1
    Dim _subT2 As exprSubType2
    Friend oldT1, oldT1bis As exprSubType1
    Friend oldT2 As exprSubType2
    Friend oldTipo As exprType
    Public str As String
    Friend bIsEquation, bIsDiffEq, bHasDerivative As Boolean
    Friend equationLevel As Int64

    Friend LP As Int64 = 0
    Friend RP As Int64 = 0
    Public msg As String
    Friend vars As VarsAndFns
    Friend re As Regex
    Friend bIsMtxExpr As Boolean
    Friend fstVarName As String
    Friend iCur1 As Int64
    Friend statIntegral As Int64
    Dim curBase As exprSubType1 = exprSubType1.decBase
    Dim curAngle As exprSubType1 = exprSubType1.radian
    Dim bGoToNext As Boolean
    Dim sInsChar As String
    Friend bOldFnHasLP As Boolean
    Friend curValue As Double
    Friend cfg As Config
    'Friend oVarsAt(-1) As VarsAndFns, iAt As Int64 = 0

    Private err1 As Exception
    Friend sb As StringBuilder
    Dim sOld As String
    Dim currType, oldType As tknGnralType
    Friend DxOrder As Int64
    Public bVerify As Boolean = True
    Public bInsert As Boolean = True
    Public bVerifyEq As Boolean = True
    Friend vVars(-1) As VarsAndFns, nLine As Int64
    Public Function Clone() As currentMatch
        Return Me.MemberwiseClone
    End Function
    Public Function parseCurMatch(ByVal expression As String, _
                                  ByRef vars As VarsAndFns, cfg As Config) As currentMatch
        Dim curMatch As currentMatch = Me
        Dim sbMtx As System.Text.StringBuilder
        Try
            sbMtx = New System.Text.StringBuilder(expression.Length)
            If cfg.bDetail Then
                expression = Replace(expression, " ", "")
            End If
            Me.cfg = cfg
            sb = New StringBuilder(expression.Length)

            If cfg.bIgnoreSpaces Then
                expression = Regex.Replace(expression,
                        "\s+", "")
            End If
            With curMatch
                Dim sVar As String = ""
                If vars IsNot Nothing Then
                    .vars = vars
                    sVar = vars.getVarsStrListToAppend
                End If
parseAgain:
                If Len(sVar) > 1 Then
                    cfg.mathGlobal.nomVarsToAppend = sVar
                Else
                    cfg.mathGlobal.nomVarsToAppend = ""
                End If
                .re = New Regex(
                    cfg.mathGlobal.sAll2)

                'iAt = 0
                sVar = ""
                Dim mc As MatchCollection = .re.Matches(expression)
                ReDim vMc(mc.Count - 1)
                If cfg.bDetail Then
                    ReDim vsb(mc.Count - 1), vExpr(mc.Count - 1)
                End If
                mc.CopyTo(vMc, 0)
                Dim bLastWasMtx As Boolean = False
                Dim prevMtxOp As Boolean = False

                ' Find out if current expression is
                ' a matrix expression, i.e. contains matrix operators
                ' or columns and/or rows; or has a matrix
                ' result, i.e. a matrix function as in "roots(x2-1)":
                For i As Int64 = 0 To .vMc.Length - 1
                    If .vMc(i).Groups("mtxFn").Success OrElse
                    .vMc(i).Groups("mtxOp").Success OrElse
                    .vMc(i).Groups("col").Success OrElse
                    .vMc(i).Groups("row").Success OrElse
                    .vMc(i).Groups("integral").Success Then
                        'If Not .vMc(i).Groups("col").Success AndAlso _
                        'Not .vMc(i).Groups("integral").Success AndAlso _
                        'Not .vMc(i).Groups("mtxFn").Success AndAlso _
                        'Not .vMc(i).Groups("row").Success Then
                        '    iAt += 1
                        'End If
                        sbMtx.Append(.vMc(i))
                        .bIsMtxExpr = True
                        bLastWasMtx = False
                        prevMtxOp = False
                    ElseIf vars IsNot Nothing AndAlso
                   (.vMc(i).Groups("var2").Success OrElse
                    .vMc(i).Groups("var").Success) Then
                        Dim eM As ExprMatrix =
                            vars.getValueByName(.vMc(i).ToString, False)
                        If eM IsNot Nothing AndAlso
                        eM.Rows + eM.Cols > 2 Then
                            .bIsMtxExpr = True
                            ' 'expression' contains a variable name whose
                            ' value is a matrix --> copy the value to sbMtx 

                            prevMtxOp = False
                            sbMtx.Append(.vMc(i).ToString)

                        Else
                            If .fstVarName = "" AndAlso
                            Not curMatch.vMc(i).Groups("Dx").Success AndAlso
                            Not curMatch.vMc(i).Groups("Idx").Success Then
                                .fstVarName = .vMc(i).ToString
                            End If
                            sbMtx.Append(.vMc(i))
                            prevMtxOp = False
                        End If
                    Else
                        ' Convert operator into mtxop inserting
                        ' rows (vbcrlf). For ex. if
                        ' expression= "A^-1*A" being A=(1;2|3;4)
                        ' will become into expression="(1;2|3;4)|^|-1|*|(1;2|3;4)"
                        ' (here | represents a carriage return, vbCrLf)
                        prevMtxOp = False
                        sbMtx.Append(.vMc(i).ToString)
                    End If
                Next
                .imc = -1
                .oldTipo = exprType.start  ' start match
                Dim errMsg As String = ""
                .iCur1 = -1
            End With
            'iAt = 0
        Catch ex As Exception
            If curMatch.msg = "" Then
                curMatch.msg = ex.Message
            End If
            Throw New Exception(ex.Message)
        End Try
        Return curMatch
    End Function
    Public Sub reStartCurMatch(Optional ByVal setICur As Int64 = -1)
        Try

            LP = 0 : RP = 0
            oldTipo = 0 : oldT1 = 0 : oldT2 = 0
            msg = ""
            oldTipo = 20 : _tipo = 90
            iCur1 = setICur
            If setICur = -1 Then
                Me.sb = New StringBuilder(sb.Capacity)
            End If
            imc = -1
            ivs = 0
            doNext()
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Property mOld As Match
        Get
            If imc Then
                Dim i As Int64 = imc
                Do While i AndAlso _
                Trim(vMc(i - 1).ToString) = ""
                    i -= 1
                Loop
                Return vMc(i - 1)
            End If
            Return Nothing
        End Get
        Set(value As Match)
            If imc Then
                vMc(imc - 1) = value
            End If
        End Set
    End Property
    Public Property m As Match
        Get
            If imc < vMc.Length Then
                Return vMc(imc)
            End If
            Return vMc(vMc.Length - 1)
        End Get
        Set(value As Match)
            vMc(imc) = value
        End Set
    End Property
    Public ReadOnly Property iCur As Int64
        Get
            If iCur1 < 0 Then
                Return 0
            End If
            Return iCur1
        End Get
    End Property
    Public ReadOnly Property bEnd As Boolean
        Get
            Return imc >= vMc.Length
        End Get
    End Property
    Public ReadOnly Property tipo As exprType
        Get
            Return Me._tipo
        End Get
    End Property
    Public ReadOnly Property subT1 As currentMatch.exprSubType1
        Get
            Return Me._subT1
        End Get
    End Property
    Public ReadOnly Property subT2 As currentMatch.exprSubType2
        Get
            Return Me._subT2
        End Get
    End Property
    Public Property _oldType As exprType
        Set(value As exprType)
            oldType = value
        End Set
        Get
            Return oldType
        End Get
    End Property
    Sub insert_replace_mcOrig(ByVal strToInsOrReplace As String, _
                         Optional incr As Int64 = 0)
        Try
            If Not bInsert Then Exit Sub
            Dim sb As New StringBuilder(Me.sb.ToString)
            sb.Append(strToInsOrReplace)
            imc += incr
            For i As Int64 = imc To vMc.Length - 1
                sb.Append(vMc(i))
            Next
            Dim mc As MatchCollection = re.Matches(sb.ToString)
            ReDim vMc(mc.Count - 1)
            mc.CopyTo(vMc, 0)
            If cfg.bDetail Then
                ReDim vsb(mc.Count - 1), vExpr(mc.Count - 1)
                For i = 0 To vMc.Length - 1
                    vsb(i) = New StringBuilder(vMc(i).ToString)
                Next
            End If
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Sub doNext() ' get next token

        imc += 1

sig:
        If cfg.ID = 0 Then Throw New Exception(msg8.num(13))

        If imc >= vMc.Length Then
            If m.Groups("fn").Success OrElse _
            InStr("@=", sOld) OrElse _
            (Trim(str) = "" AndAlso mOld.Groups("fn").Success) OrElse _
            (m.Groups("op").Success AndAlso sOld <> "!") Then
                err = New msg8B(Me, 14, New String() {str})
                Throw err
            End If
            _tipo = -1
            _subT1 = -1
            _subT2 = -1
            Exit Sub
        End If
        str = vMc(imc).ToString

        If Trim(str) = "" Then
            sb.Append(str)
            imc += 1
            GoTo sig
        End If

        getTipo(vMc(imc), _tipo, _subT1, _subT2)
        If bGoToNext Then
            imc += 1
            bGoToNext = False
            GoTo sig
        End If

        'Trace.WriteLine(str + " " + tipo.ToString + " " + imc.ToString + " " + sb.ToString)
        If Not bVerify OrElse Validate() Then
            iCur1 += 1
            If cfg.bDetail AndAlso vsb IsNot Nothing AndAlso iCur1 < vsb.Length Then
                vsb(iCur1) = New StringBuilder(str)
            End If
            sOld = str
            oldTipo = _tipo
            oldT1bis = oldT1
            oldT1 = _subT1
            oldT2 = _subT2
            oldType = currType
            sb.Append(str)
        ElseIf bGoToNext Then
            ' inserted operators * , ^
            ' or found &rad, &deg, ...
            bGoToNext = False
            GoTo sig
        Else
            Throw err
        End If

    End Sub
    Structure stackDetail
        Dim iOptor As Int64
        Dim optor As currentMatch.exprSubType2
        Dim opA, opB, result As ExprMatrix
        Dim vsb() As StringBuilder
        Dim sOp As String
        Dim cfg As Config
        Public Sub New(iOptor As Int64, _
                       optor As currentMatch.exprSubType2, _
                       opA As ExprMatrix, _
                       opB As ExprMatrix, _
                       result As ExprMatrix, _
                       sOp As String)
            Me.iOptor = iOptor
            Me.optor = optor
            Me.opA = opA
            Me.opB = opB
            Me.result = New ExprMatrix(result)
            Me.sOp = sOp
            Me.cfg = opA.cfg
        End Sub
        Public Function toStrCurMatch( _
                optor As currentMatch.exprSubType2, _
                Optional ByVal sDelimiter As String = "", _
                Optional ByVal bShowCurPos As Boolean = True, _
                Optional outFormat As outputMsgFormat = -1, _
                Optional desde As Int64 = -1, _
                Optional hasta As Int64 = -1) As String
            Dim e1 As String = ""
            'Dim cm As currentMatch
            Try
                If outFormat = -1 Then
                    outFormat = cfg.outputFormat
                End If
                Dim i As Int64 = 0
                Dim vmc() As Match = cfg.cur.vMc
                For i = desde To 0 Step -1
                    If vsb(i).Length Then Exit For
                Next
                desde = i
                i = 0
                Dim nRows As Int64 = 0
                If desde > -1 Then
                    Do
                        If vsb(i) IsNot Nothing Then
                            If i < desde OrElse i > hasta OrElse Not bShowCurPos Then
                                e1 += vsb(i).ToString
                            Else
                                If i = desde Then
                                    If outFormat = outputMsgFormat.HTML Then
                                        e1 += "<span style=""color:red"">"
                                    ElseIf outFormat = outputMsgFormat.RichTextFormat Then
                                        e1 += "\cf2 "
                                    Else
                                        e1 += "["
                                    End If
                                End If
                                e1 += vsb(i).ToString
                                If i = hasta Then
                                    If outFormat = outputMsgFormat.HTML Then
                                        e1 += "</span>"
                                    ElseIf outFormat = outputMsgFormat.RichTextFormat Then
                                        e1 += " \cf1 "
                                    Else
                                        e1 += "]"
                                    End If
                                End If
                            End If
                            If Len(sDelimiter) AndAlso
                            i < vsb.Length - 1 Then
                                e1 += sDelimiter
                            End If
                        End If
                        i += 1
                    Loop While i < vsb.Length
                    If bShowCurPos Then
                        If cfg.outputFormat = outputMsgFormat.HTML Then
                            e1 += "<br />" + vbCrLf
                        Else
                            e1 += vbCrLf
                        End If
                    End If
                Else
                    Do
                        If bShowCurPos Then
                            If outFormat = outputMsgFormat.HTML Then
                                e1 += "<span style=""color:red"">" + _
                                    vsb(i).ToString + "</span>"
                            ElseIf outFormat = outputMsgFormat.RichTextFormat Then
                                e1 += "\cf2 "
                                e1 += vsb(i).ToString + " \cf1 "
                            Else
                                e1 += "[" + vsb(i).ToString + "]"
                            End If
                        Else
                            e1 += vsb(i).ToString
                        End If
                        If Len(sDelimiter) AndAlso _
                        i < vsb.Length - 1 Then
                            e1 += sDelimiter
                        End If
                        i += 1
                    Loop While i < vsb.Length
                    If bShowCurPos Then
                        If cfg.outputFormat = outputMsgFormat.HTML Then
                            e1 += "<br />" + vbCrLf
                        Else
                            e1 += vbCrLf
                        End If
                    End If
                End If

            Catch ex As Exception
                Throw New Exception(msg8.num(13) + vbCrLf + e1)
            End Try
            Return e1
        End Function

        Public Overrides Function ToString() As String
            Dim eb As New StringBuilder(200)
            Dim vSubsTillPow() As currentMatch.exprSubType2 = { _
                exprSubType2.subs, _
                exprSubType2.add, _
                exprSubType2.mult, _
                exprSubType2.div, _
                exprSubType2.pow}
            Dim vsb2(-1) As StringBuilder
            Dim sA As String = ""
            Dim sB As String = ""
            Dim sOp As String = ""
            Dim sResult As String = ""
            Try
                Dim cfg As Config = opA.cfg
                Dim e1 As String = ""
                If optor = exprSubType2.subs AndAlso opB Is Nothing Then
                    iOptor -= 2
                    e1 = toStrCurMatch( _
                       optor, "", True, outputMsgFormat.HTML, iOptor, iOptor + 1)
                Else
                    e1 = toStrCurMatch( _
                       optor, "", True, outputMsgFormat.HTML, iOptor, iOptor + 1)
                End If
                ReDim vsb2(vsb.Length - 1)
                'eb.Append(e1)
                sA = opA.ToStrExprMtx(cfg)
                sOp = optor.ToString
                If Len(Me.sOp) Then
                    sOp = Me.sOp
                End If
                If optor.ToString = "fn" Then
                    eb.Append(vsb(iOptor).ToString + "( " + _
                              sA)
                Else
                    eb.Append(optor.ToString + "( " + _
                              sA)
                End If
                Dim bAIsMn(-1), bBIsMn(-1) As Boolean
                Dim vSumA() As Expression = opA.getCurExpr.exprToSummands(bAIsMn)
                Dim vSumB(-1) As Expression
                If opB Is Nothing Then
                    If optor = exprSubType2.subs Then
                        eb = New StringBuilder("ChangeSign(" + sA)
                    End If
                    eb.Append(" ) = ")
                Else
                    vSumB = opB.getCurExpr.exprToSummands(bBIsMn)
                    eb.Append(" ; ")
                    If vsb(iOptor + 1).ToString = "-" Then ' change sign operator?
                        eb.Append("-")
                    End If
                    sB = opB.ToStrExprMtx(cfg)
                    eb.Append(sB + " ) = ")
                End If
                Dim bRIsMn(-1) As Boolean
                Dim vResult() As Expression = result.getCurExpr.exprToSummands(bRIsMn)
                ' detail multiplication if more than
                ' one summands (brown colored):
                If sOp = "mult" AndAlso vSumA IsNot Nothing AndAlso _
                    vSumA.Length > 1 AndAlso vSumB.Length > 1 Then

                    ' multiplication detail:
                    eb.Append("<br /> =")
                    'Dim eb3 As New StringBuilder(100)
                    'eb3.Append("= ")
                    Dim vProd(vSumA.Length * vSumB.Length - 1) As Expression
                    Dim sLnI = ""
                    Dim bFst As Boolean = True
                    Dim vRsum(-1) As Expression, iR As Int64
                    Dim bHasCommFactors As Boolean = False
                    Dim bHasComm(-1) As Boolean
                    Dim vCommon(-1), vExpr1(-1) As Expression
                    Dim vsComm(-1) As String, vsNotC(-1) As String
                    For i = vSumA.Length - 1 To 0 Step -1
                        For j As Int64 = vSumB.Length - 1 To 0 Step -1
                            ReDim Preserve vRsum(iR), vCommon(iR), vsComm(iR), vExpr1(iR), vsNotC(iR)
                            vRsum(iR) = vSumA(i) * vSumB(j)
                            Dim sCur As String = _
                                vRsum(iR).ToStringExpr(cfg)
                            If Not bFst Then
                                If sCur.Chars(0) <> "-" Then
                                    sLnI += "+"
                                End If
                            Else
                                bFst = False
                            End If
                            Dim e2 As String = sCur
                            If e2.Chars(0) = "-" Then
                                e2 = Mid(e2, 2)
                            End If
                            If Regex.IsMatch(e2, "[-+]") Then
                                sLnI += "(" + sCur + ")"
                                vsNotC(iR) = "(" + sCur + ")"
                            Else
                                sLnI += sCur
                                vsNotC(iR) = sCur
                            End If
                        Next
                    Next
                    Dim sLnI2 As String = ""
                    bFst = False
                    If vRsum(iR).getCommonFactor(cfg, vCommon(iR), vExpr1(iR), vsComm(iR)) Then
                        bHasComm(iR) = True
                        bHasCommFactors = True
                    End If
                    iR += 1
                    If bHasCommFactors Then
                        For i = 0 To vsComm.Length - 1
                            Dim sCur As String = ""
                            If bHasComm(i) Then
                                sCur = vsComm(i)
                            Else
                                sCur = vsNotC(i)
                            End If
                            If Not bFst Then
                                If sCur.Chars(0) <> "-" Then
                                    sLnI2 += "+"
                                End If
                            Else
                                bFst = False
                            End If
                            Dim e2 As String = sCur
                            If e2.Chars(0) = "-" Then
                                e2 = Mid(e2, 2)
                            End If
                            If Regex.IsMatch(e2, "[-+]") Then
                                sLnI2 += "(" + sCur + ")"
                                vsNotC(iR) = "(" + sCur + ")"
                            Else
                                sLnI2 += sCur
                                vsNotC(iR) = sCur
                            End If
                        Next
                    End If
                    'sLnI = "(" + sLnI + ")"
                    eb.Append(sLnI + "<br /> = ")
                ElseIf _
                (optor = exprSubType2.add OrElse _
                 optor = exprSubType2.subs) AndAlso _
                opB IsNot Nothing Then
                    Dim sLnI As New StringBuilder(50)
                    If opA.IsPolynomial AndAlso opB.IsPolynomial AndAlso _
                    vSumA.Length + vSumB.Length <> vResult.Length AndAlso _
                    opA.getExpr(0, 0).getPolynomial.PolyResto Is Nothing AndAlso _
                    opB.getExpr(0, 0).getPolynomial.PolyResto Is Nothing Then
                        Dim Pa As Polynomial = opA.getCurExpr.getPolynomial
                        Dim Pb As Polynomial = opB.getCurExpr.getPolynomial
                        Dim vA() As Polynomial = Pa.splitIntoTerms
                        Dim vB() As Polynomial = Pb.splitIntoTerms
                        If Pa.varAll.Length = 1 AndAlso _
                        Pb.varAll.Length = 1 AndAlso _
                        Pa.varAll(0) = Pb.varAll(0) Then
                            Dim deg As Int64 = Math.Max( _
                            Pa.getDegree, Pb.getDegree)
                            Dim vPa(deg), vPb(deg) As Polynomial
                            For i = 0 To vA.Length - 1
                                Dim cur As Int64 = vA(i).getDegree
                                vPa(cur) = vA(i)
                            Next
                            For i = 0 To vB.Length - 1
                                Dim cur As Int64 = vB(i).getDegree
                                vPb(cur) = vB(i)
                            Next

                            Dim sX As String = Pa.varAll(0)
                            For i = deg To 0 Step -1
                                If vPa(i) IsNot Nothing AndAlso _
                                vPb(i) IsNot Nothing Then
                                    If sLnI.Length Then
                                        sLnI.Append("+")
                                    End If
                                    sLnI.Append("(" + vPa(i).cf(0).toStringComplex(cfg))
                                    If optor = exprSubType2.add Then
                                        sLnI.Append("+")
                                    Else
                                        sLnI.Append("-")
                                    End If
                                    sLnI.Append(vPb(i).cf(0).toStringComplex(cfg) + ")")
                                    If i Then
                                        sLnI.Append("*" + sX)
                                    End If
                                    If i > 1 Then
                                        sLnI.Append("^" + i.ToString)
                                    End If
                                ElseIf vPa(i) IsNot Nothing Then
                                    If sLnI.Length Then
                                        sLnI.Append("+")
                                    End If
                                    sLnI.Append(vPa(i).toStringPoly(cfg))
                                ElseIf vPb(i) IsNot Nothing Then
                                    If sLnI.Length Then
                                        sLnI.Append("+")
                                    End If
                                    sLnI.Append(vPb(i).toStringPoly(cfg))
                                End If
                            Next
                            sLnI.Insert(0, "<br />=")
                            Dim sln As String = Replace(sLnI.ToString, "--", "+")
                            sln = Replace(sln, "+-", "-")
                            eb.Append(sln + "<br /> = ")
                        Else
                            Dim vR(-1) As Polynomial, ir As Int64 = 0
                            For i As Int64 = 0 To vA.Length - 1
                                For j = 0 To vB.Length - 1
                                    If vB(j) IsNot Nothing Then
                                        Dim mult As Complex = Nothing
                                        If vA(i).IsEqual(vB(j), mult) Then
                                            If sLnI.Length Then
                                                sLnI.Append("+")
                                            End If
                                            sLnI.Append("(" + vA(i).toStringPoly(cfg))
                                            If optor = exprSubType2.add Then
                                                sLnI.Append("+")
                                            Else
                                                sLnI.Append("-")
                                            End If
                                            sLnI.Append(vB(j).toStringPoly(cfg) + ")")
                                            vA(i) = Nothing
                                            vB(j) = Nothing
                                            Exit For
                                        End If
                                    End If
                                Next
                            Next
                            Dim lensLnI As Int64 = sLnI.Length
                            For i = 0 To vA.Length - 1
                                If vA(i) IsNot Nothing Then
                                    If sLnI.Length Then
                                        sLnI.Append("+")
                                        If lensLnI > -1 Then lensLnI += 1
                                    End If
                                    If sLnI.Length = lensLnI Then
                                        sLnI.Append("(")
                                        lensLnI = -1
                                    End If
                                    sLnI.Append(vA(i).toStringPoly(cfg))
                                End If
                            Next
                            If lensLnI = -1 Then sLnI.Append(")")
                            lensLnI = sLnI.Length
                            For i = 0 To vB.Length - 1
                                If vB(i) IsNot Nothing Then
                                    If sLnI.Length Then
                                        sLnI.Append("+")
                                        If lensLnI > -1 Then lensLnI += 1
                                    End If
                                    If sLnI.Length = lensLnI Then
                                        sLnI.Append("(")
                                        lensLnI = -1
                                    End If
                                    sLnI.Append(vB(i).toStringPoly(cfg))
                                End If
                            Next
                            If lensLnI = -1 Then sLnI.Append(")")
                            sLnI.Insert(0, "<br />=")
                            Dim sln As String = Replace(sLnI.ToString, "--", "+")
                            sln = Replace(sln, "+-", "-")
                            eb.Append(sln + "<br /> = ")
                        End If
                    End If
                ElseIf _
                (optor = exprSubType2.add OrElse _
                 optor = exprSubType2.subs) AndAlso _
                opB IsNot Nothing Then
                    Return ""
                End If

                sResult = result.ToStrExprMtx(cfg)
                eb.Append(sResult + "<br />")
                For i = 0 To vsb.Length - 1
                    If vsb(i) Is Nothing Then
                        vsb2(i) = New StringBuilder(2)
                    Else
                        vsb2(i) = New StringBuilder(vsb(i).ToString)
                    End If
                Next
                Dim izq As Int64 = 0
                Dim dch As Int64 = 0
                Dim rp As Int64
                Dim pos As Int64 = Array.IndexOf(vSubsTillPow, optor)
                If pos > -1 Then
                    Dim vOp() As String = {"-", "+", "*", "/", "^"}
                    sOp = vOp(pos)
                End If
                If pos > -1 AndAlso _
                opB IsNot Nothing Then
                    Dim i As Int64 = iOptor - 1
                    Do While i > 0 AndAlso _
                    vsb(i).Length = 0 OrElse vsb(i).ToString = ")"
                        If vsb(i).ToString = ")" Then
                            rp += 1
                        End If
                        vsb(i).Remove(0, vsb(i).Length)
                        i -= 1
                    Loop
                    Do While rp
                        If vsb(i).ToString = ")" Then
                            rp += 1
                        ElseIf vsb(i).ToString = "(" Then
                            rp -= 1
                        End If
                        vsb(i).Remove(0, vsb(i).Length)
                        If rp = 0 Then Exit Do
                        i -= 1
                    Loop
                    izq = i
                    vsb(i) = New StringBuilder(result.ToStrExprMtx(cfg))
                    Dim bEnclose As Boolean = False
                    Select Case vsb(iOptor).Chars(0)
                        Case "-"
                            If InStr(vsb(i).ToString, "+") Then
                                bEnclose = True
                            End If
                        Case "*", "/"
                            If Regex.IsMatch(vsb(i).ToString, "[-+]") Then
                                bEnclose = True
                            End If
                        Case "^"
                            If Regex.IsMatch(vsb(i).ToString, "[-+*/]") Then
                                bEnclose = True
                            End If
                    End Select
                    i = iOptor + 1
                    If vsb(i).ToString = "-" Then ' change sign operator?
                        i += 1
                        vsb(i).Remove(0, 1)
                    End If
                    vsb(iOptor).Remove(0, vsb(iOptor).Length)
                    Do While vsb(i).ToString = "("
                        rp -= 1
                        vsb(i).Remove(0, vsb(i).Length)
                        i += 1
                    Loop
                    Do While rp
                        If vsb(i).ToString = ")" Then
                            rp += 1
                        ElseIf vsb(i).ToString = "(" Then
                            rp -= 1
                        End If
                        vsb(i).Remove(0, vsb(i).Length)
                        If rp = 0 Then Exit Do
                        i += 1
                    Loop
                    dch = i
                    vsb(iOptor + 1).Remove(0, vsb(iOptor + 1).Length)
                    If bEnclose Then
                        vsb(izq).Insert(0, "(")
                        vsb(izq).Append(")")
                    Else
                    End If
                    vsb2(izq).Insert(0, "<span style=""color:red"">")
                    vsb2(dch).Append("</span>")

                ElseIf optor = exprSubType2.fn Then
                    Dim i As Int64 = iOptor
                    izq = i
                    i = iOptor + 1
                    Do While vsb(i).ToString = "("
                        rp -= 1
                        vsb(i).Remove(0, vsb(i).Length)
                        i += 1
                    Loop
                    Do While rp
                        If vsb(i).ToString = ")" Then
                            rp += 1
                        ElseIf vsb(i).ToString = "(" Then
                            rp -= 1
                        End If
                        vsb(i).Remove(0, vsb(i).Length)
                        i += 1
                    Loop
                    dch = i
                    vsb(iOptor) = New StringBuilder(result.ToStrExprMtx(cfg))
                    vsb(iOptor + 1).Remove(0, vsb(iOptor + 1).Length)
                    vsb2(izq).Insert(0, "<span style=""color:red"">")
                    vsb2(dch - 1).Append("</span>")
                ElseIf pos = 0 Then
                    ' change Sign
                    Dim i As Int64 = iOptor
                    izq = i
                    i = iOptor + 1
                    Do While vsb(i).ToString = "("
                        rp -= 1
                        vsb(i).Remove(0, vsb(i).Length)
                        i += 1
                    Loop
                    Do While rp
                        If vsb(i).ToString = ")" Then
                            rp += 1
                        ElseIf vsb(i).ToString = "(" Then
                            rp -= 1
                        End If
                        vsb(i).Remove(0, vsb(i).Length)
                        i += 1
                    Loop
                    dch = i
                    vsb(iOptor) = New StringBuilder(result.ToStrExprMtx(cfg))
                    vsb(iOptor + 1).Remove(0, vsb(iOptor + 1).Length)
                    vsb2(izq).Insert(0, "<span style=""color:red"">")
                    vsb2(dch - 1).Append("</span>")

                End If
                Dim allvsb2 As New StringBuilder(vsb2.Length * 2)
                For i = 0 To vsb2.Length - 1
                    allvsb2.Append(vsb2(i).ToString)
                Next
                'vsb2(izq) = New StringBuilder("<span style=""color:orange"">")
                'vsb2(izq).Append(result.ToStrExprMtx(cfg))
                'vsb2(dch - 1) = New StringBuilder("</span>")
                'Dim allvsb3 As New StringBuilder(vsb2.Length * 2)
                'For i = 0 To vsb2.Length - 1
                '    allvsb3.Append(vsb2(i).ToString)
                'Next
                'eb.Insert(0, allvsb3.ToString + "<br />")


                If Replace(sA + sOp + sB, " ", "") = Replace(sResult, " ", "") Then
                    eb.Remove(0, eb.Length)
                Else
                    If Regex.IsMatch(allvsb2.ToString, "(\<br \/\>)$") Then
                        eb.Insert(0, allvsb2.ToString)
                    Else
                        eb.Insert(0, allvsb2.ToString + "<br />")
                    End If
                    eb.Insert(0, "<br />")
                End If
            Catch ex As Exception

            End Try
            Return eb.ToString
        End Function
    End Structure
    Sub pushDetail(iOptor As Int64, optor As currentMatch.exprSubType2, _
                   opA As ExprMatrix,
                   opB As ExprMatrix,
                   result As ExprMatrix, _
                   Optional sOp As String = "")
        ReDim Preserve vStack(ivs)
        vStack(ivs) = New stackDetail(iOptor, optor, opA, opB, result, sOp)
        ivs += 1
    End Sub
    Function popDetail() As stackDetail
        If ivs = 0 Then
            Return Nothing
        End If
        ivs -= 1
        Return vStack(ivs)
    End Function
    Function toStringDetailStack() As String
        Dim eb As New StringBuilder(200)
        If Me.cfg.oDetail.ie Then
            Dim e1 As String = Me.cfg.oDetail.ToStringDivisionHTML("")
            Me.cfg.oDetail.Clear()
            'Return e1
            eb.Append(e1)
        Else

        End If
        Dim i As Int64
        Dim vsb2(vsb.Length - 1) As StringBuilder
        Array.Copy(vsb, vsb2, vsb.Length)
        For i = 0 To ivs - 1
            vStack(i).vsb = vsb2
            eb.Append(vStack(i).ToString)
        Next
        If cfg.outputFormat = outputMsgFormat.plainText Then
            Dim e1 As String = Replace(eb.ToString, "<br />", vbCrLf)
            e1 = Regex.Replace(e1, "\<span[^\>]+\>", "[")
            e1 = Replace(e1, "</span>", "]")
            Return e1
        End If
        Return eb.ToString
    End Function
    Function testNextMatch(ByVal incr As Int64, _
                     ByRef tipo As exprType, _
                     ByRef subT1 As exprSubType1, _
                     ByRef subT2 As exprSubType2, _
                     Optional oldTipo As exprType = -1) As Match
        Dim m As Match = Nothing
        Try
            If imc + incr < vMc.Length Then
                m = vMc(imc + incr)
                Do While imc + incr < vMc.Length AndAlso _
                Len(Trim(m.ToString)) = 0
                    incr += 1
                Loop
                If imc + incr < vMc.Length Then
                    getTipo(m, tipo, subT1, subT2)
                End If
            Else
                tipo = 0 : subT1 = 0 : subT2 = 0
                m = Regex.Match("", " ")
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return m
    End Function
    Function getCurBase() As exprSubType1
        Return curBase
    End Function
    Function getCurAngle() As exprSubType1
        Return curAngle
    End Function

    Private Sub getTipo(ByVal m As Match, _
                     ByRef tipo As exprType, _
                     ByRef subT1 As exprSubType1, _
                     ByRef subT2 As exprSubType2, _
                     Optional ByVal bIncrRPLP As Boolean = True)
        tipo = 0
        subT1 = 0
        subT2 = 0
        'Dim str As String = m.ToString
        If m.Groups("rp").Success Then
            tipo = exprType.RP  ' 0 ' right parenth.
            currType = tknGnralType.RP
        ElseIf m.Groups("lp").Success Then
            tipo = exprType.LP ' 1 ' left parenth.
            currType = tknGnralType.lp
        ElseIf m.Groups("num").Success OrElse
        (curBase = exprSubType1.hexBase AndAlso
         m.Groups("var2").Success AndAlso
        Regex.IsMatch(m.ToString, "[.0-9a-fA-F]")) Then
            If oldTipo = exprType.fn AndAlso
            sOld <> "sqr" AndAlso
            sOld <> "mod" AndAlso
            oldT2 <> exprSubType2.integral Then
                If Len(str) <> 1 OrElse
                str < "2" OrElse str > "9" Then
                    tipo = exprType.num
                Else
                    tipo = exprType.fnExponent
                End If
            Else 'If curBase <> exprSubType1.defaultDecBase Then
                tipo = exprType.num
            End If
            If tipo = exprType.num Then
                tipo = exprType.num ' 2 ' numeric
                currType = tknGnralType.num
                subT1 = Me.getCurBase
                Dim retDec As Decimal
                If str = "." OrElse InStr(str, "..") Then
                    Dim str1 As String = String.Empty
                    If (cfg.outputFormat = outputMsgFormat.HTML) Then
                        str1 = "<span style=""color:red"">" + str + "</span>"
                    Else
                        str1 = "[ " + str + "]"
                    End If
                    Me.iCur1 += 1
                    If cfg.outputFormat = outputMsgFormat.HTML Then
                        Me.msg = msg8.num(57) + "<br />" + Me.ToString
                    Else
                        Me.msg = msg8.num(57) + vbCrLf + Me.ToString
                    End If
                    Throw New Exception(Me.msg)
                End If
                If curBase = exprSubType1.decBase AndAlso
                MathGlobal8.TryParseDbl(str, curValue) Then
                Else
                    Me.tryParseHexDecOctBinBaseNum(retDec)
                End If
            End If
        ElseIf m.Groups("cnt").Success OrElse
            m.Groups("img").Success Then
            tipo = exprType.cnt ' 3 ' constant
            If str = "pi" Then
                Me.curValue = Math.PI
            ElseIf str = "e" Then
                Me.curValue = Math.E
            End If
            currType = tknGnralType.Cnt
        ElseIf m.Groups("custFn").Success Then
            tipo = exprType.custFn ' 8 ' custom function
            LP += 1
            currType = tknGnralType.fn
        ElseIf m.Groups("Dx").Success Then
            Dim custFnID As Int64
            If bIsEquation Then
                bIsDiffEq = True
            End If
            bHasDerivative = True
            If Me.vars.tryGetVarIDByName(str + "(", custFnID) Then
                ' It is not the derivative operator.
                ' There is function D(params)=... defined 
                ' (for example: D(x)=x+1 or d(x)=x+1)
                str += "("
                Me.str = str
                tipo = exprType.custFn ' 8 ' custom function
                LP += 1
                currType = tknGnralType.fn
            Else
                tipo = exprType.Dx '  7 ' derivative (must preceed .Groups("var/var2")
                currType = tknGnralType.Dx 'currType = tknGnralType.lp ' includes Dx
                Dim mOrder As Match = Regex.Match(str, "\d+")
                If mOrder.Success Then
                    If Not Int64.TryParse(mOrder.ToString, Me.DxOrder) Then
                        Me.DxOrder = 1
                    End If
                Else
                    Me.DxOrder = 1
                End If
            End If
        ElseIf m.Groups("Idx").Success AndAlso Len(str) > 1 Then
            If statIntegral AndAlso m.Length > 1 Then
                tipo = exprType.integralRespVar
                If m.Groups("IResp").Success Then
                    subT2 = exprSubType2.integralResp
                End If
                statIntegral = 0
            Else
                tipo = exprType.var ' 4 ' variable
                subT1 = exprSubType1.var2
            End If
            currType = tknGnralType.integrRespTo
        ElseIf m.Groups("var2").Success OrElse
        m.Groups("Idx").Success Then
            tipo = exprType.var ' 4 ' variable
            subT1 = exprSubType1.var2
            currType = tknGnralType.variable
        ElseIf m.Groups("var").Success Then
            tipo = exprType.var ' 4 ' variable
            subT1 = exprSubType1.var
            currType = tknGnralType.variable
        ElseIf m.Groups("fn").Success OrElse str = "%" Then
            str = LCase(str)
            tipo = exprType.fn ' 5 ' function
            subT1 = exprSubType1.Fn
            If str = "%" Then
                m = Regex.Match("mod", MathGlobal8.sOp)
                str = "mod"
                tipo = exprType.fn
                subT1 = exprSubType1.Fn
                subT2 = exprSubType2.modulo
            ElseIf m.Groups("mtxFn").Success Then
                If m.Groups("mtxFn").Success Then
                    subT1 = exprSubType1.mtxFn
                Else
                    subT1 = exprSubType1.Fn
                    If m.Groups("integral").Success Then
                        subT1 = exprSubType1.mtxFn
                        subT2 = exprSubType2.integral
                        statIntegral = 1
                    End If
                End If
            ElseIf m.Groups("integral").Success Then
                subT1 = exprSubType1.Fn
                subT2 = exprSubType2.integral
                statIntegral = 1
                'ElseIf m.Groups("sqr").Success AndAlso str = "√" Then
            Else
                tipo = exprType.fn ' 5 ' function
                If m.Groups("mtxFn").Success Then
                    subT1 = exprSubType1.mtxFn
                Else
                    subT1 = exprSubType1.Fn
                    If m.Groups("integral").Success Then
                        subT2 = exprSubType2.integral
                        statIntegral = 1
                    End If
                End If
            End If
            currType = tknGnralType.fn
        ElseIf str = "!" Then
            currType = tknGnralType.optor
            tipo = exprType.op
            subT1 = exprSubType1.op
            subT2 = exprSubType2.factorial
        ElseIf m.Groups("op").Success OrElse
            m.Groups("mtxOp").Success Then
            tipo = exprType.op  ' 6 ' optor
            currType = tknGnralType.optor
            If m.Groups("mtxOp").Success Then
                tipo = 9
                currType = tknGnralType.mtxOptor
            End If

            'Dim str As String = Me.str '  m.ToString
            If Regex.Match(str, MathGlobal8.sRow).Success Then
                str = _
                   Regex.Replace(m.ToString, _
                        MathGlobal8.sRow, "")
                subT1 = exprSubType1.mtxOp
                subT2 = exprSubType2.mtxsubs
            Else
                subT1 = exprSubType1.op
                subT2 = exprSubType2.subs
            End If
            Dim pos As Int64 = InStr("-+*/^!%", str)
            If pos = -1 Then
                Throw New Exception(String.Format( _
                    msg8.num(28), str)) ' unknown operator
            End If
            subT2 += pos - 1
            cfg.posLastOptor = iCur1

        ElseIf m.Groups("lgOp").Success Then
            tipo = exprType.logicalOptor  ' operator
            subT1 = exprSubType1.logicalOp
            '{"and", "or", "xor", "not", "nand", "nor"
            Dim pos As Int64 = Array.IndexOf(MathGlobal8.vLgOp, LCase(str))
            subT2 = exprSubType2.AND + pos
            currType = tknGnralType.andOrXorNandNor
        ElseIf m.Groups("at").Success Then
            subT2 = exprSubType2.at
            subT1 = exprSubType1.at
            currType = tknGnralType.at
            tipo = exprType.at
        ElseIf m.Length AndAlso _
        m.ToString.Chars(0) = "&" Then
            If m.Groups("hex").Success Then
                subT1 = exprSubType1.hexBase
                If Len(str) = 2 Then
                    currType = tknGnralType.numericBase
                Else
                    tipo = exprType.num ' hexadecimal
                    currType = tknGnralType.num
                End If
            ElseIf m.Groups("dec").Success Then
                subT1 = exprSubType1.decBase
                If Len(str) = 2 Then
                    currType = tknGnralType.numericBase
                Else
                    tipo = exprType.num  ' decimal
                    currType = tknGnralType.num
                End If
            ElseIf m.Groups("oct").Success Then
                subT1 = exprSubType1.octBase
                If Len(str) = 2 Then
                    currType = tknGnralType.numericBase
                Else
                    tipo = exprType.num ' octal
                    currType = tknGnralType.num
                End If
            ElseIf m.Groups("bin").Success Then
                subT1 = exprSubType1.binBase
                If Len(str) = 2 Then
                    currType = tknGnralType.numericBase
                Else
                    tipo = exprType.num ' binary
                    currType = tknGnralType.num
                End If
            ElseIf m.Groups("rad").Success Then
                Me.curAngle = exprSubType1.radian
                bGoToNext = True
            ElseIf m.Groups("deg").Success Then
                Me.curAngle = exprSubType1.degree
                bGoToNext = True
            ElseIf m.Groups("grad").Success Then
                Me.curAngle = exprSubType1.gradian
                bGoToNext = True
            Else
                Dim ce As New curMatchErr
                Dim ex As Exception = ce.err(Nothing, Me, String.Format( _
                msg8.num(5), str))
                Throw ex
            End If
            If Not bGoToNext Then
                Me.curBase = subT1
                If Len(str) > 2 Then
                    'currType = tknGnralType.numericBase
                    'Dim retDec As Decimal
                    str = Mid(str, 3)
                    If Me.tryParseHexDecOctBinBaseNum(curValue) Then
                        'str = retDec.ToString(MathGlobal8.us)
                        subT1 = exprSubType1.decBase
                    End If
                Else
                    mOld = vMc(iCur)
                    bGoToNext = True
                End If
            End If
        ElseIf m.Groups("comment").Success Then
            bGoToNext = True
        Else
            tipo = 9
            If m.Groups("col").Success Then
                bIsEquation = False
                subT1 = exprSubType1.delimiter
                subT2 = exprSubType2.col
                currType = tknGnralType.col
            ElseIf m.Groups("row").Success Then
                bIsEquation = False
                subT1 = exprSubType1.delimiter
                subT2 = exprSubType2.row
                'iAt += 1
                nLine += 1
                If nLine < vVars.Length Then
                    vars = New VarsAndFns(vVars(nLine))
                    'For j As Int64 = 0 To vVars(nLine).getNamesList.Length - 1
                    '    Dim eMtx As ExprMatrix = vVars(nLine).getValueByID(j)
                    '    Dim name As String = vVars(nLine).getVarNameByID(j)
                    '    Dim Id As Int64 = vars.getVarIDByName(name, False)
                    '    If Id = -1 Then
                    '        vars.AddVar(name, Nothing)
                    '        Id = vars.getVarIDByName(name, False)
                    '    End If
                    '    If eMtx IsNot Nothing Then vars.setValue(Id, eMtx)
                    'Next
                End If
                currType = tknGnralType.row
            ElseIf str = "=" Then
                If LP <> RP AndAlso bIsEquation Then
                    Throw New Exception(msg8.num(13))
                End If
                Dim e1 As String = ToStringvMc()
                If LP <> RP AndAlso Not Regex.IsMatch(e1, MathGlobal8.sAllowEqual, RegexOptions.IgnoreCase) Then
                    If InStr(e1, ",") Then
                        Throw New Exception(msg8.num(53))
                    Else
                        Dim sErr As String = ""
                        Dim pos As Int64 = 0
                        If checkParentheses(cfg, e1, True, sErr, pos) Then
                            Throw New Exception(msg8.num(42))
                        End If
                    End If
                End If
                For i As Int64 = imc + 1 To vMc.Length - 1
                    If vMc(i).Value = "|" OrElse InStr(vMc(i).Value, vbCr) OrElse InStr(vMc(i).Value, vbLf) Then
                        Exit For
                    ElseIf vMc(i).Value = "=" Then
                        Throw New Exception(msg8.num(61))
                    End If
                Next
                If bHasDerivative Then
                    bIsDiffEq = True
                End If
                subT1 = exprSubType1.equal
                subT2 = exprSubType2.add
                currType = tknGnralType.equal
                Me.bIsEquation = True
                equationLevel = 1
            ElseIf Not m.Groups("substitute").Success Then
                For i = 0 To m.Groups.Count - 1
                    If m.Groups(i).Success Then
                        Dim nom As String = m.Groups(i).ToString
                    End If
                Next
                Dim ce As New curMatchErr
                Dim ex As Exception = ce.err(Nothing, Me, String.Format(
                msg8.num(5), str))
                If Not ex Is Nothing Then
                    Throw ex
                Else
                    Throw New Exception(String.Format(
                    msg8.num(5), str))
                End If
            Else
                Dim pos As Int64 = Array.IndexOf(MathGlobal8.vSubstitute, str)
                ' set substituting char(s) as source match collection
                ' into cur.mcSubstBy:
                Dim sSubstBy As String = String.Empty
                sSubstBy = MathGlobal8.vSubstituteBy(pos) + " "
                Dim bTratado As Boolean = False
                If pos = 0 Then
                    If imc + 1 < vMc.Length Then
                        Dim e1 As String = ""
                        Dim iLeft As Int32 = 0
                        Dim iRight As Int32 = 0
                        Dim iC As Int32 = imc + 1
                        Do
                            If vMc(iC).Groups("lp").Success Then
                                iLeft += 1
                            ElseIf vMc(iC).Groups("rp").Success Then
                                iRight += 1
                            End If
                            e1 += vMc(iC).Value
                            iC += 1
                        Loop While iC < vMc.Length AndAlso (iLeft > iRight OrElse e1 = "-")
                        Dim eP As New exprParser(cfg)
                        Try
                            eP.parse(e1, "")
                            If eP.msgHTML Is Nothing OrElse eP.msgHTML.Length = 0 Then
                                Dim imc2 As Int32 = imc
                                sSubstBy = "(" + e1 + ")^(1/3)"
                                insert_replace_mcOrig(sSubstBy, iC - imc)
                                imc = imc2 - 1
                                bGoToNext = True
                            Else
                                Throw New Exception(msg8.num(13))
                            End If
                        Catch ex As Exception
                            Throw ex
                        End Try
                    Else
                        Throw New Exception(msg8.num(13))
                    End If
                ElseIf Len(sSubstBy) > 4 AndAlso InStr(sSubstBy, "/") Then
                    Dim e2() As String = Split(Mid(sSubstBy, 2, Len(sSubstBy) - 2), "/")
                    Dim num, den As Int64
                    If e2.Length = 2 AndAlso
                    Int64.TryParse(e2(0), num) AndAlso
                    Int64.TryParse(e2(1), den) Then
                        If oldTipo = exprType.num Then
                            ' for example, 2⅞ = 2+7/8
                            Dim bNeg As Boolean
                            For i As Int64 = iCur - 1 To 0 Step -1
                                If vMc(i).Groups("op").Success Then
                                    If vMc(i).ToString = "-" Then
                                        bNeg = True
                                    End If
                                    Exit For
                                End If
                            Next
                            Dim abs As Double = Math.Abs(curValue)
                            Dim sAbs As String = abs.ToString(MathGlobal8.us)
                            curValue = Math.Sign(curValue) * (abs + num / den)
                            sb.Remove(sb.Length - Len(sAbs), sAbs.Length)
                            'If bNeg Then
                            '    curValue *= -1
                            '    sb.Remove(sb.Length - 1, 1)
                            'Else
                            'End If
                            insert_replace_mcOrig(curValue.ToString(MathGlobal8.us), 1)
                            imc -= 3
                            Me.oldType = exprType.op
                            bGoToNext = True
                            bTratado = True
                        End If
                    End If
                    If Not bTratado Then
                        If oldTipo = exprType.var Then
                            sSubstBy = "^" + sSubstBy
                        Else
                            sSubstBy = sSubstBy
                        End If
                        insert_replace_mcOrig(sSubstBy, 1) ' skip 1?
                        imc -= 2
                        bGoToNext = True
                    End If
                Else
                    insert_replace_mcOrig(sSubstBy, 1)
                    imc -= 2
                    bGoToNext = True
                End If
                'currType = tknGnralType.substitute
            End If
        End If
    End Sub
    Public ReadOnly Property getParsedString
        Get
            Return sb.ToString
        End Get
    End Property
    Private Function tryParseHexDecOctBinBaseNum(ByRef result As Decimal) As Boolean
        Dim Ent As Long = 0
        Dim Fra As Double = 0.0
        Dim sPattern As String = ""
        Dim base As Int64 = 10
        Dim subT1 As exprSubType1 = curBase ' vCurBase(iBase)
        Dim bRet As Boolean = False
        Dim ns As Globalization.NumberStyles
        Try
            Select Case subT1
                Case exprSubType1.hexBase
                    sPattern = "[^.0-9abcdef]"
                    base = 16
                    ns = Globalization.NumberStyles.HexNumber
                Case exprSubType1.decBase
                    sPattern = "[^.0-9]"
                    base = 10
                    ns = Globalization.NumberStyles.Integer
                Case exprSubType1.octBase
                    sPattern = "[^.0-7]"
                    base = 8
                Case exprSubType1.binBase
                    sPattern = "[^.01]"
                    base = 2
            End Select
            Dim str As String = IIf(Me.str.Chars(0) = "&", Mid(Me.str, 3), Me.str)
            If Regex.IsMatch(str, "(?i)" + sPattern + "(-i)") Then
                Exit Try
            End If
            Dim i As Int64 = IIf(imc <= 0, 0, imc)
            Dim posDot As Int64 = InStr(str, ".")
            If posDot = 0 Then
                posDot = Len(str)
            Else
                If posDot < Len(str) AndAlso
                InStr(CInt(posDot + 1), str, ".") Then
                    Exit Try
                End If
                posDot -= 1
            End If
            Dim curEnt As Long = 0L
            For j As Int64 = 0 To Len(str) - 1
                If j <> posDot Then
                    If Not Long.TryParse(str.Chars(j), curEnt) Then
                        ' si llega aqui es porque str.chars(j) = {a,b,..,f,A,B,... ,F}
                        ' asc("A")=65 ==> asc("A")-55=10, asc("B")-55=11,... asc("F")-55=15
                        curEnt = Asc(UCase(str.Chars(j))) - 55
                    End If
                End If
                If j < posDot Then
                    Ent = Ent * base + curEnt
                ElseIf j > posDot Then
                    Fra = Fra + curEnt / base ^ (j - posDot)
                End If
            Next
            bRet = True
            i += 1
            If bRet Then
                'imc = i - 1
                result = CDec(Ent) + CDec(Fra)
                Me.curValue = result
                curBase = getCurBase()
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Try
            If Not bRet Then
                Dim str1 As String = String.Empty
                If cfg.outputFormat = outputMsgFormat.HTML Then
                    str1 += "<span style=""color:red"">" + mOld.ToString + str
                    str1 += "</span><br />" + vbCrLf
                Else
                    str1 += " [ " + mOld.ToString + str + " ] "
                    str1 += vbCrLf
                End If
                Throw New Exception(str1 + msg8.num(57))
            End If
        Catch ex As Exception

        End Try
        Return bRet
    End Function
    'Public Sub verifyLR(Optional ByVal bGetLast As Boolean = False)
    '    Static bDoVerify As Boolean = True
    '    Try
    '        If Not bDoVerify Then Exit Sub
    '        If bGetLast OrElse imc >= mcOrig.Length Then
    '            If LP <> RP Then
    '                bDoVerify = False
    '                Dim e1 As String = toStrCurMatch("", False)
    '                bDoVerify = True
    '                Dim msg As String = String.Empty
    '                Dim pos As Int64
    '                If Not checkParentheses(cfg, e1, False, msg, pos) Then
    '                    iLevel = -1 : imc = mcOrig.Length
    '                    Throw New Exception(msg)
    '                End If
    '            End If
    '        Else
    '            If LP > RP Then
    '                Dim ce As New curMatchErr
    '                Throw ce.err(Nothing, Me, String.Format( _
    '                 msg8.num(1), "')'"))
    '            ElseIf LP < RP Then
    '                Dim ce As New curMatchErr
    '                Throw ce.err(Nothing, Me, String.Format( _
    '                 msg8.num(1), "'('"))
    '            ElseIf LP <> RP AndAlso tipo > 4 AndAlso tipo <> 9 AndAlso str <> "!" _
    '                AndAlso str <> "=" Then
    '                Dim ce As New curMatchErr
    '                Throw ce.err(Nothing, Me, msg8.num(4))
    '            End If
    '        End If
    '    Catch ex As Exception
    '        Throw ex
    '    End Try
    'End Sub
    Public Sub equalSign()
        Try
            Me.bIsEquation = True
            Dim e1 As String = ""
            imc += 1
            ' obtener en e1 el lado derecho
            ' de la igualdad y soslayar ese
            ' "tramo" de mcOrig() al incrementar
            ' imc:
            Dim imcOld As Int64 = imc
            Do While Not bEnd AndAlso _
            Not vMc(imc).Groups("row").Success
                e1 += vMc(imc).ToString
                imc += 1
            Loop
            If imcOld = imc Then
                Throw New Exception( _
                msg8.num(41)) ' missing left or right member
            End If
            Dim sErr As String = ""
            If Not Functions.checkParentheses(e1, False, sErr) OrElse _
            Not Functions.checkParentheses(ToString, False, sErr) Then
                Throw New Exception(msg8.num(42)) ' parentheses not matching
            End If
            If Not bEnd AndAlso _
            vMc(imc).Groups("row").Success Then
                imc -= 1
            End If
            ' ahora incluir lo obtenido
            ' (el signo = inicial es para retroceder
            ' el hilo de la ejecución a matrixParser.mtxExpr()
            ' y, allí, restar):
            imc -= 1
            insert_replace_mcOrig("-(" + e1 + ")")
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Function toStrCurMatch(ByVal bHTML As Boolean, _
                                  Optional imc2 As Int64 = -1) As String
        Dim eb As StringBuilder = Nothing
        Try
            If bEnd Then
                Return ""
            End If
            If imc2 = -1 Then imc2 = imc
            eb = New StringBuilder(vMc.Length * 12)
            For i As Int64 = 0 To vMc.Length - 1
                If i <> imc2 OrElse Not bHTML Then
                    eb.Append(vMc(i))
                Else
                    eb.Append("<span style='color:red'>")
                    eb.Append(vMc(i))
                    eb.Append("</span>")
                End If
            Next
        Catch ex As Exception
            Return ""
        End Try
        Return eb.ToString
    End Function
    Public Function toStrCurMatch( _
                    Optional ByVal sDelimiter As String = "", _
                    Optional ByVal bShowCurPos As Boolean = True, _
                    Optional outFormat As outputMsgFormat = -1, _
                    Optional desde As Int64 = -1, _
                    Optional hasta As Int64 = -1) As String
        Dim e1 As String = ""
        'Dim cm As currentMatch
        Try
            If outFormat = -1 Then
                outFormat = cfg.outputFormat
            End If
            'cm = Me.Clone() ' MathGlobal8.CloneObject(Me)
            'cm.reStartCurMatch()
            Dim icur1 As Int64 = Me.iCur1
            Dim i As Int64 = 0
            Dim nSpaces As Int64 = 0 ' 2017/05/02
            i = 0
            Do
                If i < vMc.Length AndAlso _
                i < Me.iCur1 + 1 + nSpaces AndAlso _
                Trim(vMc(i).ToString) = "" Then
                    nSpaces += 1
                End If
                i += 1
            Loop While i < vMc.Length
            icur1 += nSpaces
            i = 0
            Dim nRows As Int64 = 0
            If desde > -1 Then
                Do
                    If i < desde OrElse i > hasta OrElse Not bShowCurPos Then
                        e1 += vMc(i).ToString
                    Else
                        If i = desde Then
                            If outFormat = outputMsgFormat.HTML Then
                                e1 += "<span style=""color:red"">"
                            ElseIf outFormat = outputMsgFormat.RichTextFormat Then
                                e1 += "\cf2 "
                            Else
                                e1 += "["
                            End If
                        End If
                        e1 += vMc(i).ToString
                        If i = hasta Then
                            If outFormat = outputMsgFormat.HTML Then
                                e1 += "</span>"
                            ElseIf outFormat = outputMsgFormat.RichTextFormat Then
                                e1 += " \cf1 "
                            Else
                                e1 += "]"
                            End If
                        End If
                    End If
                    If vMc(i).Groups("row").Success Then
                        nRows += 1
                    End If
                    If Len(sDelimiter) AndAlso _
                    i < vMc.Length - 1 Then
                        e1 += sDelimiter
                    End If
                    i += 1
                Loop While i < vMc.Length
                If bShowCurPos Then
                    If cfg.outputFormat = outputMsgFormat.HTML Then
                        e1 += "<br />" + vbCrLf
                    Else
                        e1 += vbCrLf
                    End If
                End If
            Else
                Do
                    If i = icur1 + nRows AndAlso bShowCurPos Then
                        If outFormat = outputMsgFormat.HTML Then
                            e1 += "<span style=""color:red"">" + _
                                vMc(i).ToString + "</span>"
                        ElseIf outFormat = outputMsgFormat.RichTextFormat Then
                            e1 += "\cf2 "
                            e1 += vMc(i).ToString + " \cf1 "
                        Else
                            e1 += "[" + vMc(i).ToString + "]"
                        End If
                    Else
                        e1 += vMc(i).ToString
                    End If
                    If vMc(i).Groups("row").Success Then
                        nRows += 1
                    End If
                    If Len(sDelimiter) AndAlso _
                    i < vMc.Length - 1 Then
                        e1 += sDelimiter
                    End If
                    i += 1
                Loop While i < vMc.Length
                If bShowCurPos Then
                    If cfg.outputFormat = outputMsgFormat.HTML Then
                        e1 += "<br />" + vbCrLf
                    Else
                        e1 += vbCrLf
                    End If
                End If
            End If

        Catch ex As Exception
            Throw New Exception(msg8.num(13) + vbCrLf + e1)
        End Try
        Return e1
    End Function
    Public Function toStringNextMatches(numNext As Int64) As String
        Dim e1 As String = String.Empty
        For i = Me.iCur To Me.iCur + numNext
            If i < vMc.Length Then
                e1 += vMc(i).ToString
                If vMc(i).Groups("row").Success Then
                    numNext += 1
                End If
            Else
                Exit For
            End If
        Next
        Return e1
    End Function
    Public ReadOnly Property toStringCurMatch As String
        Get
            Return toStrCurMatch()
        End Get
    End Property
    Public Function ToStringvMc() As String
        Dim i As Int64
        Dim e1 As String = String.Empty
        For i = 0 To vMc.Length - 1
            e1 += vMc(i).Value
        Next
        Return e1
    End Function

    Public Overrides Function ToString() As String
        Return toStrCurMatch()
    End Function

    Friend Class curMatchErr
        Inherits Exception
        Shared bcurIsAnErr As Boolean = False
        Function err(ex As Exception, _
                     ByRef cur As currentMatch, ByVal msg As String, _
                     Optional curiCur As Int64 = -1) As Exception
            Dim ret As Exception = Nothing
            If bcurIsAnErr Then
                Return Nothing
            End If
            msg = ""
            Try
                bcurIsAnErr = True
                Dim i As Int64
                Dim e1 As String = ""
                Dim iCur As Int64 = cur.iCur
                If curiCur <> -1 Then
                    iCur = curiCur
                End If
                If cur.bEnd Then
                    iCur = cur.vMc.Length - 1
                End If

                Dim cur2 As New currentMatch()
                cur2 = cur.Clone ' MathGlobal8.CloneObject(cur)
                cur2.reStartCurMatch(-1)
                cur2.imc = -1
                Dim e2 As String = String.Empty
                With cur2
                    For i = 0 To cur2.vMc.Length - 1
                        Try
sig:
                            Do
                                .imc += 1
                                If .imc >= .vMc.Length Then
                                    'verifyExhaustingMC()
                                    Exit For
                                End If
                                .m = .vMc(.imc) ' next mcOrig
                            Loop While .m.Length = 0
tratSalida:
                            .str = .m.ToString
                            If Trim(.str) = "" Then
                                'iCur1 += 1
                                GoTo sig ' 2013/08/07, ignore white spaces
                            End If
                            .getTipo(.m, .tipo, .subT1, .subT2)
                            If .bGoToNext AndAlso Not .bEnd Then
                                ' found &rad, &deg, ...
                                .bGoToNext = False
                                GoTo sig
                            End If
                            'cur2.doNext()
                        Catch ex2 As Exception
                            Return Nothing
                        End Try
                        e2 += cur2.str
                        If i <> iCur Then
                            e1 += cur2.str
                        ElseIf .cfg.outputFormat = outputMsgFormat.HTML Then
                            e1 += "<span style=""color:red"">" + cur2.str + "</span>"
                        Else
                            e1 += " [ " + cur2.str + " ] "
                        End If
                    Next
                End With
                Dim pos As Int64
                If Not currentMatch.checkParentheses(cur2.cfg, e2, False, msg, pos) Then
                    ret = New Exception(msg)
                    Exit Try
                End If
                cur.msg = e1

                If ex IsNot Nothing Then
                    msg += vbCrLf + ex.ToString
                End If
                ret = New Exception(msg)
            Catch ex3 As Exception
                Throw ex3
            Finally
                bcurIsAnErr = False
            End Try
            Return ret
        End Function
    End Class
    Public Shared Function checkParentheses(cfg As Config, _
                                            ByVal e0 As String, _
                                            ByVal bThrowErr As Boolean, _
                                            ByRef sErr As String, _
                                            ByRef pos As Int64) As Boolean
        Dim depth As Int64 = 0
        Dim ret As Boolean = False
        Dim i As Int64
        Static re As New Regex("\(\s*\)")
        Try
            Dim e1 As New StringBuilder(e0.Length)
            Dim e10 As String = Regex.Replace(e0, "\[|\{", "(")
            e10 = Regex.Replace(e10, "\]|\}", ")")
            Dim patron As String = "[^\(\)]"
            e10 = Regex.Replace(e10, patron, " ")
            e1.Append(e10)
            Do
                Dim m As MatchCollection = re.Matches(e1.ToString)
                If m.Count = 0 Then Exit Do
                For i = 0 To m.Count - 1
                    e1.Chars(m.Item(i).Index) = " " ' left parenth. "(" -->" "
                    e1.Chars(m.Item(i).Index + m.Item(i).Length - 1) = " " ' ")" --> " "
                Next
            Loop
            Dim e5 As String = e1.ToString()
            pos = InStr(e5, "(")
            Dim pos2 As Int64 = 0
            pos2 = -InStr(e5, ")")
            If pos = 0 OrElse (-pos2 > 0 AndAlso -pos2 < pos) Then
                pos = Math.Abs(pos2)
            End If
            If pos Then
                sErr = ""
                'If pos > 1 Then
                '    sErr = Mid(e0, 1, pos - 1)
                'End If
                'sErr = Mid(e0, pos, 1)
                'If pos < e0.Length Then
                '    sErr += Mid(e0, pos)
                'End If
                'If pos = -pos2 Then pos = pos2
                Dim sL As String = String.Empty
                Dim sR As String = String.Empty
                If pos > 1 Then
                    sL = Mid(e0, 1, pos - 1)
                End If
                If pos + 1 <= Len(e0) Then
                    sR = Mid(e0, pos + 1)
                End If
                If pos2 = 0 Then
                    sErr = String.Format(msg8.num(1), "')'")
                Else
                    sErr = String.Format(msg8.num(1), "'('")
                End If
                If cfg.outputFormat = outputMsgFormat.plainText Then
                    sErr += vbCrLf + sL + "[ " + e0.Chars(pos - 1) + "]" + sR + vbCrLf
                ElseIf cfg.outputFormat = outputMsgFormat.HTML Then
                    sErr += "<br />" + vbCrLf + sL + "<span style='color:red'>" + e0.Chars(pos - 1) + "</span>" + sR + vbCrLf
                ElseIf cfg.outputFormat = outputMsgFormat.RichTextFormat Then
                    sErr += vbCrLf + sL + "[ " + e0.Chars(pos - 1) + "]" + sR + vbCrLf
                End If
                If bThrowErr Then
                    Throw New Exception(sErr)
                End If
            Else
                ret = True ' Parentheses Ok
            End If
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return ret
    End Function
    Friend Function vsErr(Optional dimension As Int64 = -1) As String()
        If dimension = 0 Then
            vsErr = New String() {str}
        ElseIf mOld Is Nothing Then
            vsErr = New String() {"", str}
        Else
            vsErr = New String() {mOld.ToString, str}
        End If
    End Function
    Private Function Validate() As Boolean
        If bEnd Then
            If imc = 0 Then Return True
        End If
        'Dim vsErr() As String
        If imc = 0 Then
            Return validateFirstToken(currType, str)
        ElseIf _
        tipo <> exprType.LP AndAlso
        oldTipo = exprType.LP Then
            '(oldT1bis = exprSubType1.mtxOp OrElse _
            'oldT1bis = exprSubType1.mtxFn OrElse _
            'oldT1 = exprSubType1.delimiter OrElse _
            'oldT1 = exprSubType1.mtxOp OrElse _
            'oldT1 = exprSubType1.mtxFn) Then
            If validateFirstToken(currType, str, False) Then
                Return True
            End If
            If imc >= 2 AndAlso vMc(imc - 2).Groups("fn").Success Then
                err = New Exception(msg8.num(14)) ' missing argument
            Else
                err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
            End If
            Return False
        End If
        If bEnd Then
            Return True
        End If
        Try


            '    1           2        3   21       4         5         6         27         8     
            'LP, √, Dx       RP     num, cnt     fn       col       row        =       variable 

            '    9           10        11        12      13 mtxOp    14         15        16     
            '-,+,*,/,%,^,!  modulo  integralRe integral  -,+,*,/,^  and,or,xor  not     binary    

            '   16           16        16         17        17        17        20        25        50
            ' octal       decimal  hexadecimal  radians   degrees   gradians  EOTokens  subtitute  unknown

            If oldType = tknGnralType.Unknown Then
                'ReDim Preserve vsErr(0)
                err = New msg8B(Me, 6, vsErr(0)) ' unknown token
                GoTo notValid
            End If
            Select Case currType
                Case tknGnralType.lp,
                    tknGnralType.Dx    ' (, √, Dx
                    If currType = tknGnralType.lp Then
                        LP += 1
                    End If
                    If oldTipo = exprType.custFn Then
                        Return True
                    End If
                    Select Case oldType
                        Case 2, 3, 8, 11, 21 ' ), num, cnt, var, dx
                            ' *  ^
                            ' 42 94
                            If oldTipo <> exprType.fnExponent AndAlso
                            mOld.ToString <> "]" Then
                                sInsChar = "*"
                                If currType = tknGnralType.lp Then
                                    LP -= 1
                                End If
                            End If
                            'insert_replace_mcOrig("*")
                        Case Else
                            Exit Try
                    End Select
                Case tknGnralType.RP  ' )
                    RP += 1
                    If RP > LP Then
                        err = New msg8B(Me, 9) ' missing (
                    End If
                    Select Case oldType
                        Case 2, 3, 6, 8, 11, 21
                            Exit Try
                        Case Else
                            If sOld = "!" Then Exit Try
                            err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                    End Select
                Case 3, 21 ' num, cnt
                    If currType = 3 AndAlso Regex.Matches(str, "\.\d+\.").Count Then
                        err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                        GoTo notValid
                    End If
                    If oldTipo = exprType.custFn Then
                        Return True
                    End If
                    Select Case oldType
                        Case 2 ' old= {")","var"}  cur={num,constant}
                            If str = cfg.sImg Then
                                sInsChar = "*"
                            ElseIf Not mOld.Groups("mtxOp").Success Then
                                sInsChar = "^" 'insert_replace_mcOrig("^")
                            Else
                                Exit Try
                            End If
                        Case 3, 21 ' old= {num,constant}  cur={num,const}
                            sInsChar = "*" 'insert_replace_mcOrig("*")
                        Case 8
                            If currType = 21 Then
                                sInsChar = "*"
                            Else
                                sInsChar = "^"
                            End If
                        Case 9
                            If sOld = "!" Then
                                sInsChar = "*" 'insert_replace_mcOrig("*")
                            Else
                                Exit Try
                            End If
                        Case Else
                            Exit Try
                    End Select
                Case 4 ' fn
                    If str = "mod" OrElse str = "%" Then Exit Try
                    Select Case oldType
                        Case 2, 3, 8, 21
                            ' *  ^
                            ' 42 94
                            sInsChar = "*" '  insert_replace_mcOrig("*")
                        Case 4
                            'err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                            'GoTo notValid
                        Case Else
                            Exit Try
                    End Select
                Case 5 ' col
                    Select Case oldType
                        Case 2, 3, 8, 11, 13, 21 : Exit Try
                        Case Else
                            If sOld = "!" Then Exit Try
                            err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                    End Select
                Case 6 ' row
                    Select Case oldType
                        Case 1, 2, 3, 8, 11, 13, 21 : Exit Try
                        Case Else
                            err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                    End Select
                Case 7 ' =
                    Select Case oldType
                        Case 2, 3, 8, 11, 21 : Exit Try
                        Case 9
                            If oldT2 = exprSubType2.factorial Then
                                Exit Try
                            End If
                            err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                        Case Else
                            err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                    End Select
                Case 8 ' var
                    If oldTipo = exprType.custFn Then
                        Return True
                    End If
                    Select Case oldType
                        Case 9 ' optor
                            If sOld = "!" Then
                                sInsChar = "*" '   insert_replace_mcOrig("*")
                            End If
                        Case tknGnralType.RP, 21
                            sInsChar = "*" ' insert_replace_mcOrig("*")
                        Case 4
                            ' Commented: allow √x, cos x, ...
                            'err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                            'GoTo notValid
                        Case 3, 8
                            ' *  ^
                            ' 42 94
                            sInsChar = "*" ' insert_replace_mcOrig("*")
                        Case 11
                            ' *  ^
                            ' 42 94
                            sInsChar = "*" 'insert_replace_mcOrig("*")
                        Case Else : Exit Try
                    End Select
                Case 9 ' -,+,*,/,^,!
                    Select Case oldType
                        Case 1, 2, 3, 4, 8, 11, 21 : Exit Try
                        Case 7
                            If subT2 <> exprSubType2.subs Then
                                err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                                GoTo notValid
                            End If
                        Case 5, 6
                            ' tknGnralType.col , tknGnralType.row 
                            Exit Try
                        Case 10
                            err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                        Case 9, 13
                            If str = "-" AndAlso _
                           Regex.IsMatch(sOld, "[+*/^]") Then
                                Exit Try
                            ElseIf sOld <> "!" Then
                                err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                                GoTo notValid
                            End If
                        Case Else
                            If sOld <> "-" AndAlso sOld <> "+" Then
                                err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                                GoTo notValid
                            End If
                            Exit Try
                    End Select
                Case 10 ' mod
                    Select Case oldType
                        Case 2, 3, 8, 11, 21 : Exit Try
                        Case Else
                            err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                    End Select
                Case 11 ' dx
                    Select Case oldType
                        Case 2, 3, 8, 11, 12, 21 : Exit Try
                        Case Else
                            If oldT2 = exprSubType2.integral Then
                                Exit Try
                            End If
                            err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                    End Select
                Case 12 ' ∫ integral
                    Select Case oldType
                        Case 4, 10
                            err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                        Case 2, 8, 11
                            ' *  ^
                            ' 42 94
                            sInsChar = "*" 'insert_replace_mcOrig("*")
                        Case Else : Exit Try
                    End Select
                Case 13 ' matrix operator (-,+,*,/,^)
                    Select Case oldType
                        Case 1, 2, 3, 6, 8, 11, 21 : Exit Try
                        Case 9
                            If sOld <> "!" Then
                                err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                                GoTo notValid
                            End If
                        Case 13
                            err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                        Case Else
                            err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                    End Select
                Case 14 ' tknGnralType.andOrXorNandNor 
                    If str = "not" Then
                        Select Case oldType
                            Case 4
                                err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                                GoTo notValid
                            Case 3, 21, 8
                                sInsChar = "*" 'insert_replace_mcOrig("*")
                                Return True

                            Case Else : Exit Try

                        End Select
                    Else
                        Select Case oldType
                            Case 2, 3, 8, 11 : Exit Try
                            Case Else
                                err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                                GoTo notValid
                        End Select
                    End If

                Case 15 ' NOT
                    Select Case oldType
                        Case 4
                            err = New msg8B(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                        Case 3, 8, 21
                            ' *  ^
                            ' 42 94
                            sInsChar = "*" 'insert_replace_mcOrig("*")
                            Return True
                        Case Else : Exit Try
                    End Select
                Case 16 ' tknGnralType.numericBase
                    ' no hay error pero retorna False
                    ' para ir a buscar el siguiente
                    ' fragmento (token):
                    imc += 1
                    iCur1 += 1
                    bGoToNext = True
                    Return False
                Case 20 ' EOTokens
                    Select Case oldType
                        Case 2, 3, 6, 8, 11, 13, 21 : Exit Try
                        Case 9, 13
                            If str <> "!" Then
                                'ReDim Preserve vsErr(0)
                                err = New msg8B(Me, 3, vsErr(0)) ' ending token is not valid
                                GoTo notValid
                            Else
                                Exit Try
                            End If
                        Case Else
                            'ReDim Preserve vsErr(0)
                            err = New msg8B(Me, 3, vsErr(0)) ' ending token is not valid
                            GoTo notValid
                    End Select
                Case 50 ' unknown token
                    'ReDim Preserve vsErr(0)
                    err = New msg8B(Me, 6, vsErr(0)) ' unknown token
                    GoTo notValid
            End Select
            If err() Is Nothing Then
                If Len(sInsChar) Then
                    insert_replace_mcOrig(sInsChar)
                    sInsChar = ""
                    bGoToNext = True
                    Return False
                End If
                Return True
            End If
        Catch ex As Exception
            err = ex
        End Try
        'sOld = str
        'oldType = currType
        Return True
notValid:
        'icur = sbExpr.Length
        Return False
    End Function
    Private Function validateFirstToken(curGType As tknGnralType, sCur As String, _
                            Optional bSetErr As Boolean = True) As Boolean
        Try

            '    1           2       3    21     4         5         6         7         8     
            'LP, √, Dx       RP     num, cnt     fn       col       row        =       variable 

            '    9           10        11        12      13 mtxOp    14         15        16     
            '-,+,*,/,%,^,!  modulo  integralRe integral  -,+,*,/,^  and,or,xor  not     binary    

            '   16           16        16         17        17        17        20        25        50
            ' octal       decimal  hexadecimal  radians   degrees   gradians  EOTokens  subtitute  unknown

            Dim vsErr() As String = {sCur}
            If imc + 1 >= vMc.Length Then
                ' Is first and last token
                Select Case curGType
                    Case 3, 4, 8, 16, 21 ' num, fn, cnt, var.
                        If curGType = 3 AndAlso Regex.Matches(str, "\.\d+\.").Count Then
                            err = New msg8B(Me, 4, vsErr) ' err in start token
                            Return False
                        End If
                        Return True
                    Case Else
                        If bSetErr Then
                            err = New msg8B(Me, 14, vsErr) ' err in start token
                        End If
                        Return False
                End Select
            Else
                Select Case curGType
                    Case 1, 3, 4, 8, 12, 15, 17, 21, 25
                        If curGType = 3 AndAlso Regex.Matches(str, "\.\d+\.").Count Then
                            err = New msg8B(Me, 4, vsErr) ' err in start token
                            Return False
                        End If
                        If tipo = exprType.LP Then LP += 1
                        Return True
                    Case 9, 13
                        If sCur = "-" OrElse sCur = "+" Then
                            Return True
                        Else
                            If bSetErr Then
                                err = New msg8B(Me, 4, vsErr) ' err in start token
                            End If
                            Return False
                        End If
                    Case 16 '  tknGnralType.numericBase 
                        ' no hay error pero retorna False
                        ' para ir a buscar el siguiente
                        ' fragmento (token):
                        imc += 1
                        iCur1 += 1
                        bGoToNext = True
                        Return False
                    Case 18 ' Dx
                        Return True
                    Case Else
                        If bSetErr Then
                            err = New msg8B(Me, 4, vsErr) ' err in start token
                        End If
                        Return False
                End Select
            End If
        Catch ex As Exception
            err = ex
        End Try
        Return False
    End Function
    Friend Property err As Exception
        Get
            Return err1
        End Get
        Set(value As Exception)
            If err1 Is Nothing AndAlso _
            err1 IsNot value Then
                err1 = New msg8B(Me, value.Message)
                'curOptor = -4
            End If
        End Set
    End Property
    Public Enum exprType
        RP = 0
        LP = 1
        num = 2
        cnt = 3
        custFn = 8
        Dx = 7
        var = 4
        fn = 5
        op = 6
        any = 9
        integralRespVar = 10
        fnExponent = 11
        logicalOptor = 12
        equal = 17
        start = 20
        at = 30
    End Enum
    Public Enum exprSubType1
        Fn = 1
        mtxFn = 2
        var = 3
        var2 = 4
        op = 6
        mtxOp = 7
        delimiter = 8
        equal = 9
        decBase = 10
        hexBase = 11
        octBase = 12
        binBase = 13
        radian = 16
        degree = 17
        gradian = 18
        logicalOp = 20
        at = 30
    End Enum
    Public Enum exprSubType2
        col = 8
        row = 9
        subs = 10
        add = 11
        mult = 12
        div = 13
        pow = 14
        factorial = 15
        modulo = 16
        integralResp = 17
        integral = 18
        mtxsubs = 20
        mtxadd = 21
        mtxmult = 22
        mtxdiv = 23
        mtxpow = 24
        mtxfact = 25 ' ??
        [AND] = 31
        [OR] = 32
        [XOR] = 33
        [NOT] = 34
        [NAND] = 35
        [NOR] = 36
        fundamentalUnitCI = 40
        fundamentalUnitCS = 41
        derivedUnitCI = 42
        derviedUnitCS = 43
        fn = 50
        at = 60
    End Enum

    Friend Enum tknGnralType
        '    1           2         3         4         5         6         7         8     
        'LP, √, Dx       RP     num, cnt     fn       col       row        =       variable 

        '    9           10        11        12      13 mtxOp    14        15        16     
        '-,+,*,/,%,^,!  modulo  integralRe integral  -,+,*,/,^  and,or,xor  not     binary    

        '   16           16        16         17        17        17        20        25        50
        ' octal       decimal  hexadecimal  radians   degrees   gradians  EOTokens  subtitute  unknown
        lp = 1
        RP = 2
        num = 3
        fn = 4
        col = 5
        row = 6
        equal = 7
        variable = 8
        optor = 9
        modulo = 10
        integrRespTo = 11
        integral = 12
        mtxOptor = 13
        andOrXorNandNor = 14
        [not] = 15
        numericBase = 16
        angleBase = 17
        Dx = 18
        EOTokens = 20
        Cnt = 21
        substitute = 25
        at = 30
        Unknown = 50
    End Enum

End Class
