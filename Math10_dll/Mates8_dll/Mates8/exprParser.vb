Imports System.Text.RegularExpressions
Imports System.Text

<Serializable()> _
Public Class exprParser
    Private Sub New()
        MyBase.New()
    End Sub

    Public cur As currentMatch
    Public ret As retMtx
    Dim vars, varsAt As VarsAndFns
    Dim msgHTML1 As String
    Public bIsEquation As Boolean = False
    Dim retIsMtx As Boolean = False
    Dim parent1, parentCpy As matrixParser
    Friend cfg As Config '= Config.cfg
    Dim bAtTreated As Boolean
    Dim OldIsEq As Int64 = -1


    Public Sub New(cfg As Config)
        MyBase.New()
        Me.cfg = cfg
    End Sub

    Public Sub New(ByVal parent As matrixParser, _
                   ByVal sExpression As String, _
                   ByVal sVars As String, _
                   ByVal cfg As Config, _
                   Optional ByRef vars As VarsAndFns = Nothing)
        Try
            Me.parent = parent
            cur = New currentMatch()
            cur.cfg = cfg
            cur.parseCurMatch(sExpression, vars, cfg)
            parent.cur = cur
        Catch ex As Exception
            Throw ex
        End Try
    End Sub

    Public Function tryParse(ByVal sExpression As String, _
                                    ByVal sVars As String, _
                            ByRef result As exprParser, _
                            ByRef msg As String, _
                            Optional ByRef vars As VarsAndFns = Nothing, _
                            Optional ByVal bDoParse As Boolean = True) As Boolean
        Dim ret As Boolean = False
        Try
            '' supress leading and trailing white spaces:
            'sExpression = Regex.Replace(sExpression, "(^(\+|\s)+)|((" + _
            '         MathGlobal8.sCol + "|" + MathGlobal8.sRow + "|\s+)$)", "")
            If sExpression Is Nothing OrElse _
            sExpression.Length = 0 Then
                Return False
            End If
            If vars Is Nothing Then
                VarsAndFns.parseVariables(cfg, sVars, vars)
                If Len(sVars) Then
                    Dim e1() As String = Split(sVars, vbLf)
                    Dim name As String = ""
                    For i = 0 To e1.Length - 1
                        vars.tryGetVarNameByID(i, name)
                        If Len(name) AndAlso _
                        vars.replaceVarsReferences(name) Then
                            msg = String.Format(msg8.num(19), name)
                            Return False
                        End If
                    Next
                End If
            Else
                vars = vars
            End If
            If vars Is Nothing Then
                vars = New VarsAndFns(cfg)
            End If
            Me.vars = vars
            cur = New currentMatch()
            If bDoParse Then
                cur.parseCurMatch(sExpression, vars, cfg)
                Me.ret = nextExpr() ' 2013/08/07 added 'me.ret'=...
            Else
                cur = New currentMatch()
            End If
            'cur.verifyLR()
            ret = True
        Catch ex As Exception
            msg = ex.Message
            msgHTML1 = ex.Message
        Finally
            msgHTML1 = cur.msg
        End Try
        Return ret
    End Function
    Public Sub parse(ByVal sExpression As String, _
                                 ByVal sVars As String, _
                                 Optional ByRef vars As VarsAndFns = Nothing, _
                                 Optional ByVal bDoParse As Boolean = True) 'As exprParser
        Try
            Dim msg As String = ""
            If Not tryParse(sExpression, sVars, Me, msg, vars, _
                             bDoParse) Then
            End If
        Catch ex As Exception
        End Try
    End Sub
    Public ReadOnly Property msgHTML As String
        Get
            Return msgHTML1
        End Get
    End Property
    Public Property parent As matrixParser
        Get
            Return parent1
        End Get
        Set(ByVal value As matrixParser)
            If value IsNot Nothing Then
                parentCpy = value
            End If
            parent1 = value
        End Set
    End Property
    Public Property getVars() As VarsAndFns
        Get
            Return vars
        End Get
        Set(ByVal value As VarsAndFns)
            vars = value
        End Set
    End Property
    Public Function nextExpr(Optional maxCols As Int64 = 0) As retMtx ' expression treatment
        Dim curNCol As Int64 = 0
        Dim retA As retMtx = Nothing
        Dim rMtx As retMtx = Nothing
        Try
            retIsMtx = False
            rMtx = New retMtx(Me)

            Dim bIsEqual As Boolean = False
            Do While Not cur.bEnd
                retA = nextTerm()
                bIsEqual = False
                Do While Not cur.bEnd AndAlso
                (cur.subT2 = currentMatch.exprSubType2.add OrElse
                cur.subT2 = currentMatch.exprSubType2.subs)
                    Dim iOptor As Int64 = cur.iCur
                    Dim subT1 As currentMatch.exprSubType1 = cur.subT1
                    If subT1 = currentMatch.exprSubType1.equal Then
                        bIsEqual = True
                    End If
                    Dim subT2 As currentMatch.exprSubType2 = cur.subT2
                    Dim opA As ExprMatrix =
                        IIf(cfg.bDetail, New ExprMatrix(retA.exprMtx), Nothing)
                    If retA Is Nothing OrElse retA.curExpr Is Nothing Then
                        'cur.iCur1 -= 1
                        cur.str = cur.vMc(cur.iCur1).ToString
                        Do While cur.iCur1 > 0 AndAlso Trim(cur.vMc(cur.iCur1).ToString) = ""
                            cur.iCur1 -= 1
                        Loop
                        Throw New msg8B(cur, 6, cur.vsErr(0))
                    End If
                    Dim retB As retMtx = nextTerm()
                    If retB Is Nothing OrElse retB.curExpr Is Nothing Then
                        cur.iCur1 -= 1
                        cur.str = cur.vMc(cur.iCur1).ToString
                        Do While cur.iCur1 > 0 AndAlso Trim(cur.vMc(cur.iCur1).ToString) = ""
                            cur.iCur1 -= 1
                        Loop
                        Throw New msg8B(cur, 6, cur.vsErr(0))
                    End If
                    If bIsEqual AndAlso cur.equationLevel = 1 Then
                        ' shift right side of the equation to the left:
                        retB.exprMtx = -retB.exprMtx
                    End If
                    If cur.m.Groups("integral").Success Then
                        Exit Do
                    End If
                    If subT2 = currentMatch.exprSubType2.add Then
                        If Not retA.isMatrix AndAlso Not retB.isMatrix Then
                            retA.curExpr = retA.curExpr + retB.curExpr
                        ElseIf retA.isMatrix AndAlso retB.isMatrix Then
                            retA.exprMtx = retA.exprMtx + retB.exprMtx
                        ElseIf retA.isMatrix Then
                            retA.exprMtx = retA.exprMtx + retB.curExpr
                        Else
                            retA.exprMtx = retA.curExpr + retB.exprMtx
                        End If
                    Else
                        If Not retA.isMatrix AndAlso Not retB.isMatrix Then
                            retA.curExpr = retA.curExpr - retB.curExpr
                        ElseIf retA.isMatrix AndAlso retB.isMatrix Then
                            retA.exprMtx = retA.exprMtx - retB.exprMtx
                        ElseIf retA.isMatrix Then
                            retA.exprMtx = retA.exprMtx - retB.curExpr
                        Else
                            retA.exprMtx = retA.curExpr - retB.exprMtx
                        End If

                    End If
                    'If Not retA.isMatrix AndAlso cur.bIsEquation AndAlso _
                    '                        retA.curExpr.getMatchStr = "/" Then
                    '    retA.curExpr = retA.curExpr.getArgs(0)
                    'End If
                    If cfg.bDetail AndAlso Not retA.isMatrix _
                    AndAlso Not retB.isMatrix Then
                        If Not retA.isMatrix AndAlso Not retA.curExpr.IsPolynomial Then
                            Dim rup As New ReduceExprUsingPolynomials
                            retA.curExpr = rup.ReduceUsingPolynomials(retA.curExpr)
                        End If
                        cur.pushDetail(iOptor, subT2, opA, retB.exprMtx, retA.exprMtx)
                    End If
                Loop
                If bAtTreated Then
                    bAtTreated = False
                    retA.curExpr = retA.curExpr.evalExprToExpr(vars)
                    vars = New VarsAndFns(cfg)
                End If
                If retA.rows + retA.cols = 2 Then
                    If retA.curExpr IsNot Nothing Then
                        Dim rV As New retVal(retA.curExpr, vars)
                        rMtx.setRetVal(rV)
                    ElseIf Not (Me.cur.subT1 = currentMatch.exprSubType1.mtxFn OrElse
                    Me.cur.tipo = currentMatch.exprType.integralRespVar OrElse
                    cur.subT2 = currentMatch.exprSubType2.integral) Then
                        Dim e1 As String = cur.toStrCurMatch("", False, False)
                        e1 = Regex.Split(e1, MathGlobal8.sRow)(0)
                        Dim pos As Int64
                        If parent IsNot Nothing Then
                            currentMatch.checkParentheses(cfg, e1, True, parent.errMsg, pos)
                        Else
                            Dim msg As String = String.Empty
                            currentMatch.checkParentheses(cfg, e1, True, msg, pos)
                            Throw New Exception(msg)
                        End If
                    End If
                ElseIf rMtx IsNot Nothing AndAlso rMtx.cols > 1 Then
                    Try
                        Dim curRow As Int64 = rMtx.rows - 1
                        Dim curcol As Int64 = rMtx.cols - 1
                        For col As Int64 = curcol To rMtx.cols + retA.cols - 2
                            rMtx.setRetVal(New retVal(
                                retA.exprMtx.getExpr(curRow, col + 2 - curcol), vars))
                            rMtx.incrCol()
                        Next
                    Catch ex As Exception
                        Throw New Exception(msg8.num(13)) ' n/a
                    End Try
                Else
                    rMtx = retA
                    If cur.m.Groups("row").Success Then
                        cur.doNext()
                    End If
                    Exit Do
                End If
                If Me.cur.subT1 = currentMatch.exprSubType1.mtxFn OrElse
                Me.cur.subT1 = currentMatch.exprSubType1.mtxOp OrElse
                Me.cur.tipo = currentMatch.exprType.RP Then
                    Exit Do
                End If
                If cur.m.Groups("col").Success Then
                    Dim curIsEq As Int64 = IIf(bIsEqual, 0, 1)
                    If OldIsEq <> -1 AndAlso curIsEq <> OldIsEq Then
                        Throw New Exception(msg8.num(53))
                    End If
                    OldIsEq = IIf(bIsEqual, 0, 1)
                    cur.equationLevel = 0
                    curNCol += 1
                    If maxCols AndAlso curNCol >= maxCols Then
                        maxCols = 0
                        Exit Do
                    End If
                    rMtx.incrCol()
                ElseIf cur.m.Groups("row").Success Then
                    Dim curIsEq As Int64 = IIf(bIsEqual, 0, 1)
                    If OldIsEq <> -1 AndAlso curIsEq <> OldIsEq Then
                        Throw New Exception(msg8.num(53))
                    End If
                    OldIsEq = IIf(bIsEqual, 0, 1)
                    cur.equationLevel = 0
                    If Not Me.bIsEquation Then
                        cur.fstVarName = ""
                    End If
                    If maxCols Then
                        Exit Do
                    End If
                    rMtx.incrRow()
                ElseIf Me.cur.subT1 = currentMatch.exprSubType1.mtxFn OrElse
                Me.cur.tipo = currentMatch.exprType.integralRespVar OrElse
                cur.subT2 = currentMatch.exprSubType2.integral Then
                    Exit Do
                End If
            Loop
            If cur.bEnd Then
                Dim curEq As Int64 = IIf(bIsEqual, 0, 1)
                If OldIsEq <> -1 AndAlso curEq <> OldIsEq Then
                    If Not Regex.IsMatch(cur.ToString, MathGlobal8.sAllowEqual, RegexOptions.IgnoreCase) Then
                        Throw New Exception(msg8.num(53))
                    End If
                End If
            End If
            If rMtx Is Nothing Then
                Throw New Exception(msg8.num(13))
            End If
        Catch ex As Exception
            If TypeOf (ex) Is NullReferenceException Then
                Throw New Exception(msg8.num(13))
            ElseIf (ex.Message = "n/a" AndAlso cur.iCur + 1 < cur.vMc.Length) Then
                'Dim e1 As String = cur.toStringCurMatch
                'Throw New Exception(e1 + msg8.num(57))
                Throw ex
            Else
                Throw ex
            End If
        End Try
        rMtx.cfg = cfg
        rMtx.exprMtx.cfg = cfg
        Return rMtx
    End Function
    Private Function nextTerm() As retMtx      ' term treatm.
        Dim retA As retMtx = Nothing
        Try
            retA = nextPower()
            Do While cur.subT2 = currentMatch.exprSubType2.mult OrElse _
            cur.subT2 = currentMatch.exprSubType2.div
                Dim iOptor As Int64 = cur.iCur
                Dim subT2 As currentMatch.exprSubType2 = cur.subT2
                Dim opA As ExprMatrix = _
                    IIf(cfg.bDetail, New ExprMatrix(retA.exprMtx), Nothing)
                Dim retB As retMtx = nextPower()
                'If retB Is Nothing OrElse retB.exprMtx Is Nothing Then
                '    cur.iCur1 += 2
                '    Throw New msg8B(cur, 6, cur.vsErr(0))
                'Else
                If cur.m.Groups("integral").Success Then
                    Exit Try
                End If
                If subT2 = currentMatch.exprSubType2.mult Then
                    If Not retA.isMatrix AndAlso Not retB.isMatrix Then
                        retA.curExpr = retA.curExpr * retB.curExpr
                    Else
                        retA.exprMtx = retA.exprMtx * retB.exprMtx
                    End If
                Else ' division
                    If Not retA.isMatrix AndAlso Not retB.isMatrix Then
                        retA.curExpr = retA.curExpr / retB.curExpr
                    Else
                        retA.exprMtx = retA.exprMtx / retB.exprMtx
                    End If
                End If
                If cfg.bDetail AndAlso Not retA.isMatrix _
                        AndAlso Not retB.isMatrix Then
                    cur.pushDetail(iOptor, subT2, opA, retB.exprMtx, retA.exprMtx)
                End If
            Loop
            If retA Is Nothing Then
                Throw New Exception(msg8.num(13))
            End If
        Catch ex As Exception
            Throw ex
        Finally
        End Try
        Return retA
    End Function
    Private Function nextPower() As retMtx
        Dim retA As retMtx = Nothing
        Dim sgn As Int64
        Try
            retA = nextFactor(sgn)
            Dim iOptor As Int64 = cur.iCur
            If Not cur.bEnd Then
                If cur.subT2 = currentMatch.exprSubType2.pow OrElse _
                cur.subT1 = currentMatch.exprSubType1.Fn Then
                    Dim opA As ExprMatrix = Nothing
                    Dim vExponent(3) As retMtx, irve As Int64 = 0
                    Dim vSgn(3), vpos(3) As Int64
                    Dim sOp(3) As String
                    sOp(0) = cur.str
                    vSgn(0) = sgn
                    vExponent(0) = retA
                    vpos(0) = iOptor
                    irve += 1

                    If cur.statIntegral <> 1 Then
                        vExponent(irve) = nextFactor(vSgn(irve))
                    Else
                        cur.statIntegral += 1
                        vExponent(irve) = nextExpr()
                    End If
                    vpos(irve) = cur.iCur
                    sOp(irve) = cur.m.ToString
                    Do While Not cur.bEnd AndAlso (cur.subT2 = currentMatch.exprSubType2.pow OrElse
                    cur.subT1 = currentMatch.exprSubType1.Fn)
                        If cur.subT2 = currentMatch.exprSubType2.pow AndAlso
                        (vExponent(irve) Is Nothing OrElse vExponent(irve).curExpr Is Nothing) Then
                            Throw New msg8B(cur, 6, cur.vsErr(0))
                        End If
                        If InStr("^sqrt", sOp(irve - 1)) = 0 AndAlso sOp(irve) = "^" Then
                            Dim eMtx As ExprMatrix = evalFn(sOp(irve - 1),
                                        -vExponent(irve).exprMtx, Me, vExponent(irve - 1).exprMtx)
                            vExponent(irve - 1) = New retMtx(Me, eMtx)
                            sOp(irve - 1) = "^"
                        Else
                            irve += 1
                        End If
                        If irve >= sOp.Length Then
                            ReDim Preserve vExponent(irve), vSgn(irve), vpos(irve), sOp(irve)
                        End If
                        vExponent(irve) = nextFactor(vSgn(irve))
                        vpos(irve) = cur.iCur
                        sOp(irve) = cur.m.ToString
                    Loop
                    vExponent(0).cfg = cfg
                    Dim bDetail As Boolean = cfg.bDetail
                    If bDetail Then
                        cfg.bDetail = vExponent(0).isMatrix
                    End If
                    For i As Int64 = irve To 1 Step -1
                        iOptor = vpos(i - 1)
                        If bDetail Then
                            opA = New ExprMatrix(vExponent(i - 1).exprMtx)
                        End If
                        If vSgn(i) = -1 Then
                            If sOp(i - 1) = "^" Then
                                vExponent(i - 1).exprMtx ^= -vExponent(i).exprMtx
                            Else
                                cfg.bDetail = bDetail
                                Dim eMtx As ExprMatrix = evalFn(sOp(i - 1),
                                        -vExponent(i).exprMtx, Me, vExponent(i - 1).exprMtx)
                                cfg.bDetail = False
                                vExponent(i - 1) = New retMtx(Me, eMtx)
                            End If
                        Else
                            If sOp(i - 1).Chars(0) = "^" Then
                                vExponent(i - 1).exprMtx ^= vExponent(i).exprMtx
                            Else
                                cfg.bDetail = bDetail
                                If i > 1 AndAlso sOp(i - 2) = "^" Then
                                    Dim eMtx As ExprMatrix = evalFn(sOp(i - 1),
                                        vExponent(i).exprMtx, Me, vExponent(i - 2).exprMtx ^ vExponent(i - 1).exprMtx)
                                    cfg.bDetail = False
                                    i -= 1
                                    vExponent(i - 1) = New retMtx(Me, eMtx)
                                Else
                                    Dim eMtx As ExprMatrix = evalFn(sOp(i - 1),
                                        vExponent(i).exprMtx, Me, vExponent(i - 1).exprMtx)
                                    cfg.bDetail = False
                                    vExponent(i - 1) = New retMtx(Me, eMtx)
                                End If
                            End If
                        End If
                        If bDetail AndAlso Not vExponent(i).isMatrix Then
                            If sOp(i - 1) = "^" Then
                                cur.pushDetail(iOptor, currentMatch.exprSubType2.pow,
                                     opA, vExponent(i).exprMtx, vExponent(i - 1).exprMtx)
                            Else
                                cur.pushDetail(iOptor, currentMatch.exprSubType2.fn,
                                     opA, vExponent(i).exprMtx, vExponent(i - 1).exprMtx, sOp(i - 1))
                            End If
                        End If
                    Next
                    cfg.bDetail = bDetail
                    retA = vExponent(0)
                End If
            End If
            If sgn = -1 Then
                Dim opA As ExprMatrix = _
                 IIf(cfg.bDetail, New ExprMatrix(retA.exprMtx), Nothing)
                retA.exprMtx = -retA.exprMtx
                If cfg.bDetail Then
                    cur.pushDetail(iOptor, currentMatch.exprSubType2.subs, _
                        opA, Nothing, retA.exprMtx)
                End If
            End If
            If retA Is Nothing Then
                Throw New Exception(msg8.num(13))
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return retA
    End Function


    Private Function nextFactor(ByRef sgn As Int64, _
                                Optional bOnlyOpnd As Boolean = False) As retMtx
        Dim rMtx As New retMtx(Me)
        Dim bFst As Boolean = True
        Try
            sgn = 1
            Do
                Do
sig:
                    cur.doNext()
                    If bFst AndAlso _
                    cur.subT2 = currentMatch.exprSubType2.subs Then
                        sgn *= -1
                    ElseIf cur.subT2 = currentMatch.exprSubType2.factorial Then
                        Exit Do
                    ElseIf cur.bEnd OrElse _
                    cur.tipo = currentMatch.exprType.op OrElse _
                    cur.subT1 = currentMatch.exprSubType1.mtxOp OrElse _
                    cur.subT1 = currentMatch.exprSubType1.Fn OrElse _
                    cur.tipo = currentMatch.exprType.RP OrElse _
                    cur.subT1 = currentMatch.exprSubType1.mtxFn OrElse _
                    cur.subT1 = currentMatch.exprSubType1.delimiter OrElse _
                    cur.tipo = currentMatch.exprType.integralRespVar Then
                        Exit Try
                    Else
                        Exit Do
                    End If
                Loop
                bFst = False

                If cur.tipo = currentMatch.exprType.num Then
                    rMtx.curExpr = New Expression(cur.curValue)

                ElseIf cur.tipo = currentMatch.exprType.var Then
                    Dim varID As Int64 = -1
                    vars = cur.vars
                    Dim eM2 As ExprMatrix = vars.getValueByName( _
                        cur.str, False)
                    Dim sM2 As String = ""
                    If eM2 IsNot Nothing Then
                        Dim vVar() As String = eM2.getAllVars
                        If vVar.Length > 1 OrElse _
                        (vVar.Length = 1 AndAlso _
                         vVar(0) <> cur.str) Then
                            For row As Int64 = 0 To eM2.Rows - 1
                                For col As Int64 = 0 To eM2.Cols - 1
                                    eM2.getExpr(row, col) = _
                                        eM2.getExpr(row, col).evalExprToExpr(vars)
                                Next
                            Next
                        End If
                        rMtx = New retMtx(Me, eM2)
                    ElseIf vars IsNot Nothing AndAlso _
                    vars.tryfindVarWithValue(cur.str, varID) Then
                        Dim eM As ExprMatrix = vars.getValueByID(varID)
                        If eM IsNot Nothing Then
                            rMtx.curExpr = New Expression(eM)
                        Else
                            rMtx.curExpr = Nothing
                        End If
                    ElseIf varID = -1 Then
                        Dim msg As String = ""
                        Dim eM As ExprMatrix = Nothing
                        If Not vars.AddVar(cur.str, Nothing, msg) Then
                            varID = _
                                vars.getVarIDByName(cur.str, False)
                            If varID = -1 Then
                                Throw New Exception(msg)
                            End If
                        Else
                            varID = vars.getVarIDByName(cur.str)
                            eM = vars.getValueByID(varID)
                        End If
                        If eM IsNot Nothing Then
                            rMtx.curExpr = New Expression(eM)
                        Else
                            rMtx.curExpr = New Expression( _
                                Polynomial.GetPolynomial(cur.str))
                        End If
                    End If
                    If rMtx.curExpr Is Nothing Then
                        rMtx.curExpr = New Expression( _
                            Polynomial.GetPolynomial(cur.str))
                    End If

                ElseIf cur.tipo = currentMatch.exprType.LP Then
                    If cur.equationLevel Then cur.equationLevel += 1
                    If Me.parent Is Nothing Then
                        rMtx = Me.nextExpr()
                    Else
                        rMtx = Me.parent.mtxExpr()
                    End If
                    If cur.equationLevel Then cur.equationLevel -= 1
                    If rMtx.isMatrix Then
                        cur.doNext()
                        Exit Do
                    End If

                ElseIf cur.subT2 = currentMatch.exprSubType2.factorial Then
                    If Not rMtx.curExpr.IsReal Then
                        Dim m As Match = Regex.Match("!", MathGlobal8.sOp)
                        Dim fact As New Expression(m)
                        ReDim fact.getArgs(0)
                        fact.getArgs(0) = rMtx.curExpr
                        rMtx.curExpr = fact
                    Else
                        Dim db As Double = Math.Floor(rMtx.curExpr.toDouble)
                        If db = 0 Then
                            db = 1.0
                        Else
                            For i As Int64 = db - 1 To 2 Step -1
                                db *= CDbl(i)
                            Next
                        End If
                        rMtx.curExpr = New Expression(db)
                    End If

                ElseIf cur.tipo = currentMatch.exprType.Dx Then
                    ' Derivative:
                    If Regex.IsMatch(cur.ToString, "=s*[^\""]") Then
                        cur.bIsDiffEq = True
                    End If
                    Dim nOrder As Int64 = cur.DxOrder
                    Dim sDet1 As String = "", sDet2 As String = ""
                    ' Retrive 'respect to var.':
                    Dim sRespVar As String = cur.m.Groups("resp").ToString
                    Dim varID As Int64
                    If sRespVar.Length = 0 Then
                        ' get the first variable's name
                        sRespVar = cur.fstVarName
                    End If
                    If vars.tryfindVarWithValue(sRespVar, varID) Then

                    ElseIf varID = -1 Then
                        Dim msg As String = ""
                        If Not vars.AddVar(sRespVar, Nothing, msg) Then
                            Throw New Exception(msg)
                        End If
                        varID = vars.getVarIDByName(sRespVar)
                    End If
                    rMtx.curExpr = New Expression(0.0)
                    cur.doNext()
                    If Not cur.m.Groups("lp").Success Then
                        ' expected an opening '('
                        Throw New Exception(String.Format( _
                            msg8.num(1), " '('"))
                    End If

                    Dim pos As Int64 = cur.iCur
                    If Me.parent Is Nothing Then
                        rMtx = Me.nextExpr()
                    Else
                        rMtx = Me.parent.mtxExpr()
                    End If
                    If Not rMtx.isMatrix Then
                        Dim vVar(-1) As String
                        rMtx.curExpr.getAllVars(vVar)
                        If vVar.Length = 0 OrElse _
                        (vVar.Length = 1 AndAlso vVar(0) = sRespVar) Then
                            cur.bIsDiffEq = False
                        End If
                    End If
                    Dim sVar As String = vars.getVarNameByID(varID)
                    If cur.bIsDiffEq Then
                        Dim arg As New Expression(rMtx.curExpr)
                        rMtx.curExpr = Expression.AddFnAndArg0("diff", arg)
                        ReDim Preserve rMtx.curExpr.getArgs(2)
                        rMtx.curExpr.getArgs(1) = New Expression(Polynomial.GetPolynomial(sVar))
                        rMtx.curExpr.getArgs(2) = New Expression(nOrder)
                    Else
                        For i As Int64 = 1 To nOrder
                            If cfg.bDetail Then
                                sDet1 = "D" + sVar + "("
                                sDet1 += rMtx.curExpr.ToStringExpr(cfg) + ")"
                                cfg.oDetail.Add(sDet1 + " =")
                            End If
                            rMtx.exprMtx = rMtx.exprMtx.opDerivative(sVar)
                            If cfg.bDetail Then
                                sDet2 = rMtx.curExpr.ToStringExpr(cfg)
                                'cfg.oDetail.Add(sDet1 + " =")
                                cfg.oDetail.Add("= " + sDet2)
                            End If
                        Next
                        If cfg.bDetail Then
                            cfg.oDetail.ClearDivisions()
                        End If
                    End If


                ElseIf cur.m.Groups("cnt").Success Then
                    Select Case LCase(cur.str)
                        Case "pi" : rMtx.curExpr = New Expression(Math.PI)
                        Case "e" : rMtx.curExpr = New Expression(Math.E)
                    End Select

                ElseIf cur.m.Groups("img").Success Then
                    rMtx.curExpr = New Expression(New Polynomial( _
                                New Complex(0.0, 1.0)))

                ElseIf cur.tipo = currentMatch.exprType.at Then
                    Dim rMtx2 As retMtx = Nothing
                    rMtx2 = nextExpr(1)
                    Dim vvar(-1) As String
                    Dim curExpr As Expression = rMtx2.curExpr
                    cur.bIsEquation = False
                    curExpr.getAllVars(vvar)
                    If vvar.Length = 0 Then
                        Throw New Exception(msg8.num(13))
                    End If
                    Dim vR(-1) As Expression
                    If Not curExpr.tryToIsolateToExpression(vvar(0), vR) Then
                        Throw New Exception(msg8.num(13))
                    End If
                    Dim id As Int64
                    If vars.tryGetVarIDByName(vvar(0), id) Then
                        vars.setValue(id, New ExprMatrix(vR(0)))
                    Else
                        vars.AddVar(vvar(0), vR(0))
                    End If
                    bAtTreated = True
                    Exit Do

                ElseIf cur.tipo = currentMatch.exprType.custFn Then
                    Dim varID As Int64 = -1
                    Dim pos As Int64 = cur.iCur
                    Dim fn As customFn = Nothing
                    ' retrive custom function fn:
                    Dim nomFn As String = Mid(cur.str, 1, Len(cur.str) - 1)
                    vars.IsCustomFn(cur.str, fn)

                    ' get fn's parameters:
                    Dim sParams As String = ""
                    Dim fnrMtx As retMtx = Nothing
                    Dim lp As Int32 = 1
                    Dim rp As Int32 = 0
                    Dim e1 As String = ""
                    If InStr(cur.vMc(cur.imc).ToString, "(") = 0 Then
                        cur.doNext()
                    End If
                    If Me.parent Is Nothing Then
                        fnrMtx = Me.nextExpr()
                    Else
                        fnrMtx = Me.parent.mtxExpr()
                    End If
                    If Not cur.m.Groups("rp").Success Then
                        Throw New Exception(String.Format(
                            msg8.num(1), " ')'"))
                    End If


                    Dim bDetail As Boolean = cfg.bDetail
                    cfg.bDetail = False
                    If bDetail Then
                        cfg.oDetail.Add(nomFn + "(" + fnrMtx.ToString + ") =")
                    End If

                    ' eval the function at sParams:
                    rMtx = New retMtx(Me, fn.evalMtxToExprMtx(cfg, fnrMtx))
                    If bDetail Then
                        cfg.oDetail.Add(" = " + rMtx.toStringRetVal(cfg))
                    End If
                    cfg.bDetail = bDetail

                ElseIf cur.subT1 = currentMatch.exprSubType1.logicalOp Then
                    Dim pos As Int64 = cur.iCur
                    Dim subT2 As currentMatch.exprSubType2 = cur.subT2
                    If Not rMtx.curExpr.IsReal Then
                        Dim ce As New currentMatch.curMatchErr
                        'Throw ce.err(Nothing, cur, msg8.num(4), pos - 1)
                        Throw New Exception(msg8.num(0)) ' + vbCrLf + sCur)
                    End If
                    Dim opA As Double = rMtx.curExpr.getPolynomial.cf(0).pRe.ToDouble
                    Dim rvB As retMtx = Nothing
                    Dim opb As Double
                    If cur.subT2 <> currentMatch.exprSubType2.NOT Then
                        If cur.bEnd Then
                            Dim ce As New currentMatch.curMatchErr
                            Throw ce.err(Nothing, cur, msg8.num(4))
                        End If
                        Dim sgnB As Int64 = 1
                        pos = cur.iCur
                        rvB = nextFactor(sgnB)
                        If sgnB = -1 Then
                            rvB.curExpr = -rvB.curExpr
                        End If
                        If Not rvB.curExpr.IsReal Then
                            Dim ce As New currentMatch.curMatchErr
                            cur.imc = cur.iCur
                            Dim sCur As String = String.Empty
                            Try
                                sCur = cur.toStrCurMatch(cfg.outputFormat = outputMsgFormat.HTML)
                            Catch ex As Exception
                            End Try
                            Throw New Exception(msg8.num(0)) ' + vbCrLf + sCur)
                        End If
                        opb = rvB.curExpr.getPolynomial.cf(0).pRe.ToDouble
                    Else
                    End If

                    Dim opR As Double
                    Select Case subT2
                        Case currentMatch.exprSubType2.AND
                            opR = (opA And opb)
                        Case currentMatch.exprSubType2.OR
                            opR = (opA Or opb)
                        Case currentMatch.exprSubType2.NOT
                            opR = Not (opA)
                        Case currentMatch.exprSubType2.XOR
                            opR = (opA Xor opb)
                        Case currentMatch.exprSubType2.NOR
                            opR = Not (opA Or opb)
                        Case currentMatch.exprSubType2.NAND
                            opR = Not (opA And opb)
                    End Select
                    rMtx.curExpr = New Expression(opR)
                    If cfg.bDetail Then
                        cur.pushDetail(pos, subT2,
                                    rMtx.exprMtx, rvB.exprMtx, rMtx.exprMtx)
                    End If
                    If cur.str = "=" Then Exit Do
                ElseIf cur.m.Groups("resto").Success Then
                    Throw New Exception(String.Format( _
                        msg8.num(5), cur.str))
                Else
                    Exit Do
                End If
                If bOnlyOpnd Then
                    Exit Do
                End If
            Loop While Not cur.bEnd AndAlso _
                cur.subT2 <> currentMatch.exprSubType2.integralResp

        Catch ex As Exception
            If TypeOf (ex) Is NullReferenceException OrElse _
            ex.Message = "n/a" Then
                Throw New Exception(msg8.num(13))
            Else
                Throw New Exception(ex.Message)
            End If
        End Try
        'If rMtx IsNot Nothing Then
        '    rMtx.cfg = cfg
        '    If rMtx.curExpr IsNot Nothing Then
        '        rMtx.curExpr.cfg = cfg
        '    End If
        'End If
        Return rMtx
    End Function
    Public Shared Function evalFn( _
                    ByVal sFn As String, _
                    ByVal eMtx As ExprMatrix, _
                    Optional parser As exprParser = Nothing, _
                    Optional moduloOpA As ExprMatrix = Nothing) As ExprMatrix
        Dim reMtx As ExprMatrix = Nothing
        Try
            Dim cfg As Config = Nothing
            Dim cur As currentMatch = Nothing
            If parser IsNot Nothing Then
                cfg = parser.cfg
                cur = parser.cur
            Else
                cfg = New Config
                Dim mp As New matrixParser()
                mp.parse("x")
                cur = mp.cur
            End If
            sFn = LCase(sFn)
            Select Case sFn
                Case "max"
                    reMtx = New ExprMatrix(eMtx.OpMax)
                Case "min"
                    reMtx = New ExprMatrix(eMtx.OpMin)
                Case "cross"
                    reMtx = ExprMatrix.opCrossProduct(eMtx)
                Case "dot"
                    reMtx = ExprMatrix.opDotProduct(eMtx)
                Case "identity"
                    Dim rows As Int64 = eMtx.getExpr(0, 0).toDouble
                    Dim cols As Int64 = 0
                    If eMtx.Cols > 1 Then
                        cols = eMtx.getExpr(0, 1).toDouble
                    End If
                    reMtx = ExprMatrix.Identity(cfg, rows, cols)
                Case "echelon" ' Echelon form
                    reMtx = eMtx.opEchelonForm
                Case "cof" ' cofactor
                    Dim cof As ExprMatrix = Nothing
                    eMtx.opDeterminant(True, cofactorMtx:=cof)
                    reMtx = cof
                Case "eigenvalues", "egvl"
                    reMtx = eMtx.opEigenValues(False)
                Case "eigenvectors", "egv"
                    reMtx = eMtx.opEigenVectors
                Case "transpose", "trn"
                    reMtx = ExprMatrix.opTranspose(eMtx)
                Case "adj"
                    Dim adj As ExprMatrix = Nothing
                    eMtx.opDeterminant(True, adjoint:=adj)
                    reMtx = adj
                Case "trace" ' 
                    reMtx = eMtx.opTrace
                Case "roots"
                    If eMtx.IsPolynomial Then
                        Dim mtx As Matrix = Nothing
                        Try
                            mtx = Polynomial.opRoots(
                                eMtx.getCurExpr.getPolynomial,
                                False, cfg).mtx
                            If mtx Is Nothing Then
                                Throw New Exception(msg8.num(13)) ' n/a
                            End If
                        Catch ex As Exception
                            Throw New Exception(msg8.num(13)) ' n/a
                        End Try
                        reMtx = New ExprMatrix(mtx)
                    ElseIf eMtx.Cols + eMtx.Rows = 2 Then
                        eMtx.getCurExpr.IsEquation = True
                        Dim soe As New SystemOfEquations(Nothing, eMtx, parser.vars, parser.cur, cfg)
                        Dim mtx As New Matrix
                        If soe.resolveSysOfEqs(cfg) AndAlso soe.resultValues.Length Then
                            mtx.setRowAndCol(soe.resultValues.Length - 1, 1)
                            For i As Int64 = 0 To soe.resultValues.Length - 1
                                mtx.vVect(i) = New Vector(soe.resultValues(i))
                            Next
                            reMtx = New ExprMatrix(mtx)
                        Else
                            Throw New Exception(msg8.num(13)) ' n/a
                        End If
                    Else
                        Throw New Exception(msg8.num(13)) ' n/a
                    End If
                Case "lim"
                    Dim sVar As String = String.Empty
                    Dim limit As New Expression(0.0)
                    If eMtx.Cols < 2 Then
                        Throw New Exception(msg8.num(57))
                    End If
                    If eMtx.Cols > 2 Then
                        sVar = eMtx.getExpr(0, 1).getPolynomial.var(0)
                        limit = eMtx.getExpr(0, 2)
                    ElseIf eMtx.getExpr(0, 0).IsPolynomial Then
                        sVar = eMtx.getExpr(0, 0).getPolynomial.var(0)
                        limit = eMtx.getExpr(0, 1)
                    Else
                        sVar = "x"
                        limit = eMtx.getExpr(0, 1)
                    End If
                    reMtx = New ExprMatrix(eMtx.getExpr(0, 0).opLimit(sVar, limit))
                Case "rank"
                    reMtx = New ExprMatrix(eMtx.opRank)
                Case "det"
                    Dim expr As Expression = eMtx.opDeterminant()
                    reMtx = New ExprMatrix(expr)
                Case "jordan"
                    reMtx = New ExprMatrix(eMtx.opJordanForm)
                Case "factor"
                    Dim vect As Vector = eMtx.getVector(0)
                    Dim mtx As Matrix = Nothing
                    Dim Pc(-1) As Polynomial
                    Try
                        If Not eMtx.getExpr(0, 0).IsPolynomial Then
                            Throw New Exception(msg8.num(13))
                        End If
                        Dim Pa As Polynomial = eMtx.getExpr(0, 0).getPolynomial
                        If Pa.isReal Then
                            Dim db As Double = Pa.ToDouble
                            If Math.Floor(db) <> db OrElse db >= 1000000.0 OrElse
                            db < 2 Then
                                Throw New Exception(msg8.num(13))
                            Else
                                Dim iv As Int64 = 0
                                For i As Int64 = 0 To MathGlobal8.vPrime.Length - 1
                                    Dim div As Double = db / MathGlobal8.vPrime(i)
                                    If div = Math.Floor(div) Then
                                        db = div
                                        ReDim Preserve Pc(iv)
                                        Pc(iv) = New Polynomial(MathGlobal8.vPrime(i))
                                        iv += 1
                                        i -= 1
                                        If db = 1 Then Exit For
                                    End If
                                Next
                                If db <> 1 Then
                                    ReDim Preserve Pc(iv)
                                    Pc(iv) = New Polynomial(db)
                                End If
                            End If
                        ElseIf Pa.varAll.Length > 1 OrElse Pa.PolyResto IsNot Nothing Then
                            Throw New Exception(msg8.num(13))
                        Else
                            Pc = Polynomial.opFactor(Pa, cfg)
                        End If
                        mtx = New Matrix
                        ReDim mtx.vVect(Pc.Length - 1)
                        For i As Int64 = 0 To Pc.Length - 1
                            mtx.vVect(i) = New Vector(Pc(i))
                        Next
                        If mtx Is Nothing Then
                            Throw New Exception(msg8.num(13)) ' n/a
                        End If
                    Catch ex As Exception
                        Throw New Exception(msg8.num(13)) ' n/a
                    End Try
                    reMtx = New ExprMatrix(mtx)
                Case "jacobian"
                    reMtx = eMtx.opJacobian(parser.vars)
                Case "lagrangianinterpolation"
                    If eMtx.Cols > 3 Then
                        ' lagrangianInterpolation(a,b,n,f(x))
                        Dim intp As Interpolation =
                            Interpolation.getChebyshevNodes(
                            eMtx.getExpr(0, 0).toDouble,
                            eMtx.getExpr(0, 1).toDouble,
                            eMtx.getExpr(0, 2).toDouble,
                            eMtx.getExpr(0, 3))
                        reMtx = New ExprMatrix(
                            intp.lagrangianinterpolation(cfg, True))
                        If cfg.sesID > -1 Then
                            Try
                                C_session.xi(cfg.sesID) = eMtx.getExpr(0, 0).toDouble
                                C_session.xf(cfg.sesID) = eMtx.getExpr(0, 1).toDouble
                                C_session.sfn(cfg.sesID) = eMtx.getCurExpr.ToStringExpr(cfg)
                                C_session.sf2(cfg.sesID) = eMtx.getExpr(0, 3).ToStringExpr(cfg)
                                C_session.sf3(cfg.sesID) = ""
                                C_session.yAuto(cfg.sesID) = 1
                            Catch ex As Exception

                            End Try
                        End If
                    Else
                        ' lagrangianInterpolation(x0,f(x0)|x1,f(x1)| ...|xn,f(xn))
                        Dim lagrInt As New Interpolation(eMtx.getMatrix)
                        reMtx = New ExprMatrix(
                            lagrInt.lagrangianinterpolation(cfg, True))
                    End If
                Case "gcd", "lcm"
                    Dim bDetail As Boolean = cfg.bDetail
                    cfg.bDetail = False
                    Dim bAllPoly As Boolean = True
                    Dim bAllReal As Boolean = eMtx.getExpr(0, 0).IsReal
                    Dim bHasResto As Boolean = False
                    For i As Int64 = 1 To eMtx.Cols - 1
                        If Not eMtx.getExpr(0, i).IsPolynomial OrElse
                            eMtx.getExpr(0, i).IsReal Then
                            bAllPoly = False
                        End If
                        If Not eMtx.getExpr(0, i).IsPolynomial Then
                            Dim Pa As Polynomial = eMtx.getExpr(0, i).getPolynomial
                            If Pa.PolyResto IsNot Nothing Then
                                bHasResto = True
                                Exit For
                            End If
                        End If
                        If Not eMtx.getExpr(0, i).IsReal Then
                            bAllReal = False
                        End If
                    Next
                    'If sFn = "lcm" AndAlso Not bAllReal Then
                    '    Throw New Exception(msg8.num(13))
                    'ElseIf Not bAllPoly AndAlso Not bAllReal Then
                    '    Throw New Exception(msg8.num(54))
                    'End If
                    If (Not bAllPoly AndAlso Not bAllReal) OrElse bHasResto Then
                        Throw New Exception(msg8.num(54))
                    End If
                    cfg.bDetail = bDetail
                    If bAllReal Then
                        Dim cols As Int64 = eMtx.Cols
                        Dim vDbl(cols - 1) As Double
                        Dim e1 As String = ""
                        For i As Int64 = 0 To cols - 1
                            vDbl(i) = eMtx.getExpr(0, i).toDouble
                        Next
                        Dim vLCM_GCD() As Double = Functions.LCM_GCD(vDbl, cfg, e1)
                        If sFn = "gcd" Then
                            reMtx = New ExprMatrix(vLCM_GCD(0))
                        Else

                            reMtx = New ExprMatrix(vLCM_GCD(1))
                        End If
                    ElseIf sFn = "lcm" Then
                        Dim lcm As New Polynomial(eMtx.getExpr(0, 0).getPolynomial)
                        For i As Int64 = 1 To eMtx.Cols - 1
                            lcm = Polynomial.opLCM(lcm, eMtx.getExpr(0, i).getPolynomial)
                        Next
                        reMtx = New ExprMatrix(lcm)
                    Else
                        If cfg.bDetail Then
                            cfg.oDetail.AddAlways("(You may find an explanation at: " +
                                "<a href='http://en.wikipedia.org/wiki/Greatest_common_divisor_of_two_polynomials'>" +
                                "http://en.wikipedia.org/wiki/Greatest_common_divisor_of_two_polynomials" + "</a>")
                        End If
                        Dim gcd As Polynomial = eMtx.getExpr(0, 0).getPolynomial
                        For i As Int64 = 1 To eMtx.Cols - 1
                            gcd =
                                Polynomial.opGcd(gcd,
                                    eMtx.getExpr(0, i).getPolynomial, cfg)
                        Next
                        reMtx = New ExprMatrix(gcd)
                    End If
                    cfg.bDetail = bDetail
                Case "norm"
                    If eMtx.Cols = 1 Then
                        reMtx = New ExprMatrix(evalFn("mod", eMtx.toComplex))
                    Else
                        Dim vectA As ExprMatrix = New ExprMatrix(eMtx.getExprMtxFromRow(0))
                        Dim vectB As ExprMatrix = Nothing
                        If eMtx.Rows = 1 Then
                            vectB = New ExprMatrix(eMtx.getExprMtxFromRow(0))
                        Else
                            vectB = New ExprMatrix(eMtx.getExprMtxFromRow(1))
                        End If
                        Dim dist As ExprMatrix = vectA - vectB
                        reMtx = dist * ExprMatrix.opTranspose(dist)
                        reMtx = New ExprMatrix(reMtx.getCurExpr ^ New Expression(0.5))
                    End If
                Case "poly"
                    Dim vectA As ExprMatrix = New ExprMatrix(eMtx.getExprMtxFromRow(0))
                    If vectA Is Nothing OrElse vectA.Cols = 0 OrElse vectA.Rows > 1 Then
                        Throw New Exception(msg8.num(58))
                    End If
                    Dim v As Vector = eMtx.getVector(0)
                    Dim x As Polynomial = Polynomial.GetPolynomial("x")
                    Dim r As New Polynomial(1.0)
                    For i = 0 To v.vPoly.Length - 1
                        If v.vPoly(i).getDegree Then
                            Throw New Exception(msg8.num(58))
                        End If
                        r *= x - v.vPoly(i)
                    Next
                    reMtx = New ExprMatrix(r)
                Case "exp"
                    If eMtx.Cols + eMtx.Rows > 2 Then
                        reMtx = New ExprMatrix(eMtx.opExponential)
                    ElseIf eMtx IsNot Nothing AndAlso
                            eMtx.getCurExpr IsNot Nothing AndAlso
                            Not eMtx.getCurExpr.IsComplex Then
                        reMtx = New ExprMatrix(
                        Expression.AddFnAndArg0("exp", eMtx.getCurExpr))
                    ElseIf eMtx IsNot Nothing AndAlso
                            eMtx.getCurExpr IsNot Nothing Then
                        reMtx = New ExprMatrix(
                            evalFn("exp", eMtx.getCurExpr.toComplex))
                    Else
                        reMtx = New ExprMatrix(
                            evalFn("e", eMtx))
                    End If
                Case "re"
                    reMtx = eMtx.opReal
                Case "im"
                    reMtx = eMtx.opImag
                Case "conj"
                    Dim im As ExprMatrix = eMtx.opImag
                    Dim re As ExprMatrix = eMtx.opReal
                    reMtx = re - im
                Case "residue"
                    If Not eMtx.IsPolynomial OrElse
                    eMtx.IsComplex Then
                        Throw New Exception(msg8.num(80))
                    End If
                    Dim Pa As Polynomial = eMtx.getExpr(0, 0).getPolynomial
                    Dim vNum(-1), vDen(-1) As Polynomial
                    Dim vDenExp(-1) As Int64
                    Dim errmsg As String = String.Empty
                    If Pa.PolyResto IsNot Nothing Then
                        Dim mtx As Matrix = Nothing
                        If Not Functions.partialFractionDecomposition(
                         Pa, vNum, vDen, vDenExp, cfg, errmsg) Then
                            Throw New Exception(errmsg)
                        End If

                        Dim vect As New Vector()
                        ReDim vect.vPoly(vNum.Length - 1)
                        For i As Int64 = 0 To vNum.Length - 1
                            If vDenExp(i) = 1 Then
                                vect.vPoly(i) = vNum(i) / vDen(i)
                            Else
                                vect.vPoly(i) = vNum(i) / vDen(i) ^ New Polynomial(vDenExp(i))
                            End If
                        Next
                        mtx = New Matrix(vect)
                        mtx = Matrix.opTranspose(mtx)
                        reMtx = New ExprMatrix(mtx)
                    Else
                        Throw New Exception(msg8.num(80))
                    End If
                Case "eulers"
                    Dim difEq As New Differential_Equations
                    Dim bDetail As Boolean = cfg.bDetail
                    cfg.bDetail = False
                    If eMtx.Cols < 3 Then
                        Throw New Exception(msg8.num(14) + " At Euler's method.")
                    End If
                    Dim eStep As Expression = eMtx.getExpr(0, 0)
                    Dim ePoint As Expression = eMtx.getExpr(0, 1)
                    Dim eEq As Expression = eMtx.getExpr(0, 2)
                    If Not eStep.IsReal Then
                        Throw New Exception(msg8.num(75))
                    End If
                    If Not ePoint.IsReal Then
                        Throw New Exception(msg8.num(76))
                    End If
                    If eEq.IsPolynomial Then
                        Throw New Exception(msg8.num(77))
                    End If
                    Dim vExpr() As Expression = {eStep, ePoint, eEq}
                    cfg.bDetail = bDetail
                    Dim cjo1 As Complex = Differential_Equations.resolveEulers(
                        vExpr, vExpr(2).cfg.cur.vars)
                    reMtx = New ExprMatrix(cjo1)
                Case "isolate"
                    Dim bDetail As Boolean = cfg.bDetail
                    cfg.bDetail = False
                    If eMtx.Cols < 2 Then
                        Throw New Exception(msg8.num(14) + " At isolate function.")
                    End If
                    Dim varToIsolate As Expression = eMtx.getExpr(0, 0)
                    Dim expr As Expression = eMtx.getExpr(0, 1)
                    Dim isolated() As Expression = Nothing
                    If Not varToIsolate.IsPolynomial Then
                        Throw New Exception(msg8.num(13))
                    Else
                        Dim Pa As Polynomial = varToIsolate.getPolynomial
                        If Pa.getDegree = 0 Then
                            Dim vars As VarsAndFns = Nothing
                            If parser IsNot Nothing Then
                                vars = parser.vars
                            End If
                            If vars IsNot Nothing Then
                                For i As Int64 = 0 To vars.length - 1
                                    Dim em As ExprMatrix = vars.getValueByID(i)
                                    If em IsNot Nothing Then
                                        expr = em.getCurExpr
                                        If expr.IsComplex AndAlso
                                        (expr.toComplex - Pa.ToComplex).IsZero Then
                                            Throw New Exception(
                                                String.Format(msg8.num(18),
                                                vars.getNamesList(i) + " = " +
                                                Pa.ToComplex.toString) + " for isolate ")
                                        End If
                                    End If
                                Next
                            End If
                            Throw New Exception(
                                String.Format(msg8.num(18),
                                 Pa.ToComplex.toString))
                        ElseIf Pa.getDegree <> 1 OrElse Pa.cf.Length <> 1 Then
                            Throw New Exception(msg8.num(13))
                        End If
                        Dim sVar As String = Pa.varAll(0)
                        If Not expr.tryToIsolateToExpression(sVar, isolated) OrElse
                        isolated.Length <> 1 Then
                            Throw New Exception(msg8.num(13))
                        End If
                        If parser IsNot Nothing Then
                            parser.cur.bIsEquation = False
                        End If
                        reMtx = New ExprMatrix(isolated(0))
                    End If
                Case "integral", "∫", "integrate"
                    Dim vexpr(-1) As Expression
                    Dim nParams As Int64 = 0
                    Dim IRespVar As String = ""

                    Dim vars As VarsAndFns = Nothing
                    Dim varId As Int64
                    If parser IsNot Nothing Then
                        vars = parser.vars
                        If cur.m.Groups("Idx").Success Then
                            IRespVar = cur.m.Groups("IResp").ToString
                            'cur.doNext()
                        ElseIf cur.m.Groups("IResp").Success Then
                            IRespVar = cur.m.Groups("IResp").ToString
                        ElseIf Len(cur.fstVarName) Then
                            IRespVar = cur.fstVarName
                        Else
                            IRespVar = "x"
                        End If
                        varId = vars.getVarIDByName(IRespVar, False)
                        If varId = -1 Then
                            varId = 0
                            Dim custFn As customFn = Nothing
                            Do While varId + 1 < vars.getNamesList.Length AndAlso
                                vars.IsCustomFn(vars.getVarNameByID(varId), custFn)
                                varId += 1
                            Loop
                            If IRespVar = "" AndAlso
                                vars.getNamesList.Length Then
                                IRespVar = vars.getNamesList(0)
                                Dim msg As String = ""
                                vars.AddVar(IRespVar, Nothing, msg)
                                varId = vars.getVarIDByName(IRespVar)
                            End If
                        End If
                    End If


                    Dim mArg As New ExprMatrix(eMtx)
                    ReDim vexpr(eMtx.Cols - 1)
                    For i As Int64 = 0 To eMtx.Cols - 1
                        Dim expr As Expression = mArg.getExpr(0, i)
                        If expr Is Nothing AndAlso cur.m.Groups("Idx").Success Then
                            expr = New Expression(1.0)
                            mArg.getExpr(0, i) = expr
                        End If
                        vexpr(i) = expr
                    Next
                    If cur.m.Groups("Idx").Success Then
                        IRespVar = cur.m.Groups("IResp").ToString
                        cur.doNext()
                    End If
                    Dim ln0 As Int64 = mArg.getExprAtRow(0).Length
                    Dim ln1 As Int64 = 0
                    Dim row2() As Expression = mArg.getExprAtRow(1)
                    If row2 IsNot Nothing Then
                        ln1 = row2.Length
                    End If
                    If ln0 > 2 AndAlso ln0 > ln1 + 1 AndAlso
                    vexpr(0).IsReal AndAlso vexpr(1).IsReal Then
                        reMtx =
                            mArg.opIntegralDefOrPolynFraction(
                                      varId, vexpr, cur, vars)
                    Else
                        Dim vexpr1(0) As Expression
                        If ln0 < 3 Then
                            vexpr1(0) = vexpr(0)
                        Else
                            vexpr1(0) = vexpr(2)
                        End If
                        reMtx = mArg.opIntegral(
                            cfg, varId, vexpr1, cur, vars, IRespVar, Nothing, Nothing, Nothing) '+ _
                        If ln0 > 2 Then
                            Dim eRet As Expression = reMtx.getExpr(0, 2)
                            reMtx = New ExprMatrix(eRet)
                            Dim oVarA As New VarsAndFns(cfg)
                            oVarA.AddVar(IRespVar, vexpr(0))
                            Dim a As Expression = reMtx.getExpr(0, 0).evalExprToExpr(oVarA)
                            Dim oVarb As New VarsAndFns(cfg)
                            oVarb.AddVar(IRespVar, vexpr(1))
                            Dim b As Expression = reMtx.getExpr(0, 0).evalExprToExpr(oVarb)
                            reMtx.getExpr(0, 0) = b - a
                        End If
                        For i As Int64 = 0 To reMtx.Rows - 1
                            For j As Int64 = 0 To reMtx.getExprAtRow(i).Length - 1
                                Dim expr As Expression = reMtx.getExpr(i, j)
                                If expr IsNot Nothing Then
                                    reMtx.getExpr(i, j) = expr +
                                        New Expression(Polynomial.GetPolynomial(
                                        cfg.mathGlobal.integralCnst))
                                End If
                            Next
                        Next
                    End If
                    If cur.bEnd OrElse
                    cur.subT1 = currentMatch.exprSubType1.delimiter OrElse
                    InStr("-+", cur.str) Then
                        Exit Try
                    End If

                    If cur.m.Groups("integral").Success Then
                        If cur.oldTipo = currentMatch.exprType.op Then
                            cur.imc -= 3
                        Else
                            cur.imc -= 2
                        End If
                        'GoTo sig
                    End If
                Case "orthog"
                    Dim cjo As Complex =
                        Polynomial.opOrthog(eMtx.getMatrix)
                    reMtx = New ExprMatrix(cjo)
                Case "mod", "modulo"
                    If moduloOpA.getCurExpr.IsReal Then
                        ' Real % PositiveInteger or
                        ' Real mod PositiveInteger  
                        If Not eMtx.IsReal OrElse
                        eMtx.toDouble <> Math.Floor(eMtx.toDouble) OrElse
                        eMtx.toDouble < 1 Then
                            Throw New Exception(msg8.num(59)) ' arg. should be a positive integer
                        End If
                        Dim dbMod As Double = eMtx.toDouble
                        Dim db As Double = moduloOpA.getCurExpr.toDouble
                        reMtx = New ExprMatrix(db Mod dbMod)
                    ElseIf moduloOpA.getCurExpr.IsPolynomial Then
                        ' (Polynomial) % PositiveInteger, or
                        ' (Polynomial) mod PositiveInteger 
                        If Not eMtx.IsReal OrElse
                        eMtx.toDouble <> Math.Floor(eMtx.toDouble) OrElse
                        eMtx.toDouble < 1 Then
                            If eMtx.IsPolynomial Then
                                ' (Polynomial) % (Polynomial)
                                ' or (Polynomial) mod (Polynomial)
                                ' see "Modulo Reduction" in:
                                ' http://stackoverflow.com/questions/13202758/multiplying-two-polynomials

                                Dim modP As Polynomial = eMtx.getCurExpr.getPolynomial
                                Dim div As Polynomial = moduloOpA.getCurExpr.getPolynomial / modP
                                If cfg.bDetail Then
                                    cfg.oDetail.Add("(" + moduloOpA.getCurExpr.getPolynomial.toStringPoly(cfg) + ")/(" +
                                                      modP.toStringPoly(cfg) + ") =")
                                    cfg.oDetail.Add("= " + div.toStringPoly(cfg))
                                    cfg.oDetail.Add("=> (" + moduloOpA.getCurExpr.getPolynomial.toStringPoly(cfg) + ")mod(" +
                                                     modP.toStringPoly(cfg) + ") =")
                                End If
                                ' can't extract div.PolyResto directly because very probably
                                ' div.PolyResto and div.PolyDivisor have been reduced:
                                Dim resto As Polynomial = div.PolyResto
                                If resto Is Nothing Then
                                    reMtx = New ExprMatrix(0.0)
                                    If cfg.bDetail Then
                                        cfg.oDetail.Add("= 0")
                                    End If
                                Else
                                    reMtx = New ExprMatrix(resto * modP / div.PolyDivisor)
                                    ' in case division isn't accurate, assure there is no remainder:
                                    reMtx.getCurExpr.getPolynomial.PolyResto = Nothing
                                    reMtx.getCurExpr.getPolynomial.PolyDivisor = Nothing
                                    If cfg.bDetail Then
                                        cfg.oDetail.Add("= (" + resto.toStringPoly(cfg) + ")*(" +
                                                     modP.toStringPoly(cfg) + ")/(" + div.PolyDivisor.toStringPoly(cfg) + ")")
                                        cfg.oDetail.Add("=" + reMtx.getCurExpr.getPolynomial.toStringPoly(cfg))
                                    End If
                                End If

                            Else
                                Throw New Exception(msg8.num(13)) ' n/a
                            End If
                        Else
                            Dim dbMod As Double = eMtx.getCurExpr.toDouble
                            Dim Pa As Polynomial =
                                moduloOpA.getCurExpr.getPolynomial
                            Pa.dbModuloInOperations = dbMod
                            reMtx = New ExprMatrix(Pa)
                        End If
                    Else
                        Throw New Exception(msg8.num(13))
                    End If
                Case Else
                    Dim exp As Int64 = 1
                    If cur IsNot Nothing AndAlso cur.tipo = currentMatch.exprType.fnExponent AndAlso
                    Int64.TryParse(cur.m.ToString, exp) Then
                        Dim tipo As currentMatch.exprType
                        Dim subT1 As currentMatch.exprSubType1
                        Dim subT2 As currentMatch.exprSubType1
                        Dim m As Match = cur.testNextMatch(1, tipo, subT1, subT2)
                        If tipo = currentMatch.exprType.LP Then
                            If exp AndAlso exp < 10 Then
                                Dim sgn As Int64 = 1
                                eMtx = parser.nextFactor(sgn, True).exprMtx
                                If sgn = -1 Then eMtx = -eMtx
                                cur.doNext()
                            End If
                        Else
                            Dim cjo1 As Complex = exprParser.evalFn(sFn, New Complex(exp), cur)
                            reMtx = New ExprMatrix(cjo1)
                            cur._oldType = currentMatch.exprType.num
                            cur.doNext()
                            Exit Try
                        End If
                    End If
                    If eMtx.IsComplex = False Then
                        eMtx.getExpr(0, 0) = Expression.AddFnAndArg0(sFn, eMtx.getCurExpr)
                        If exp <> 1 Then
                            eMtx = eMtx ^ New ExprMatrix(exp)
                        End If
                        reMtx = eMtx
                        Exit Try
                    End If
                    Dim cjo As Complex = exprParser.evalFn(sFn, eMtx.toComplex, cur)
                    If exp <> 1 Then
                        cjo = cjo ^ New Complex(exp)
                    End If
                    reMtx = New ExprMatrix(cjo)
            End Select
        Catch ex As Exception
            Throw ex ' sustituído por esta línea y comentadas las siguientes (2020/11/20):
            'If TypeOf (ex) Is DivideByZeroException OrElse
            'TypeOf (ex) Is ApplicationException OrElse
            'InStr(ex.ToString, "n/a") Then
            '    Throw ex
            'End If
            'Throw New Exception(msg8.num(13))
        End Try
        Return reMtx
    End Function


    Public Shared Function evalFn( _
                    ByVal fn As String, _
                    ByVal cjoA As Complex, _
                    Optional ByVal cur As currentMatch = Nothing) As Complex
        Static vTrigonFns() As String = _
            New String() { _
            "sin", "cos", "tan", _
            "sinh", "cosh", "tanh", _
            "csc", "sec", "cot", _
            "csch", "sech", "coth" _
            }
        Dim rCjo As Complex = Nothing
        Try
            fn = LCase(fn)
            Dim bArgNotValid As Boolean = False
            If cur IsNot Nothing Then
                If cur.getCurAngle <> currentMatch.exprSubType1.radian AndAlso _
                Array.IndexOf(vTrigonFns, fn) > -1 Then
                    If cur.getCurAngle = currentMatch.exprSubType1.degree Then
                        cjoA *= New Complex(MathGlobal8.degree2Radians)
                    Else 'If cur.getCurAngle = currentMatch.exprSubType1.gradian Then
                        cjoA *= New Complex(MathGlobal8.centesimal2Rad)
                    End If
                End If
            End If
repiteFn:
            Select Case Len(fn)
                Case 6
                    Select Case fn
                        Case "logtwo" : rCjo = Complex.opLn(cjoA) / Math.Log(2.0)
                        Case "logten" : rCjo = Complex.opLn(cjoA) / Math.Log(10.0)
                    End Select
                Case 5
                    Select Case fn
                        Case "acosh" : rCjo = Complex.opACosH(cjoA)
                        Case "acoth" : rCjo = Complex.opACotH(cjoA)
                        Case "acsch" : rCjo = Complex.opACscH(cjoA)
                        Case "asech" : rCjo = Complex.opASecH(cjoA)
                        Case "asinh" : rCjo = Complex.opASinH(cjoA)
                        Case "atanh" : rCjo = Complex.opAtanH(cjoA)
                        Case "floor" : rCjo = New Complex(Math.Floor(cjoA.pRe.ToDouble))
                            If cjoA.pIm.IsZero = False Then
                                bArgNotValid = True
                            End If
                        Case "round" : rCjo = New Complex(Math.Round(cjoA.pRe.ToDouble))
                            If cjoA.pIm.IsZero = False Then
                                bArgNotValid = True
                            End If
                    End Select
                Case 4
                    Select Case fn
                        Case "coth" : rCjo = Complex.opCotH(cjoA)
                        Case "csch" : rCjo = Complex.opCscH(cjoA)
                        Case "sech" : rCjo = Complex.opSecH(cjoA)
                        Case "acos" : rCjo = Complex.opACos(cjoA)
                        Case "acot" : rCjo = Complex.opAtan(Complex.one / cjoA)
                        Case "acsc" : rCjo = Complex.opASin(Complex.one / cjoA)
                        Case "asec" : rCjo = Complex.opASec(cjoA)
                        Case "asin" : rCjo = Complex.opASin(cjoA)
                        Case "atan" : rCjo = Complex.opAtan(cjoA)
                        Case "conj" : rCjo = Complex.opConjugate(cjoA)
                        Case "cosh" : rCjo = Complex.opCosH(cjoA)
                        Case "norm" : rCjo = New Complex(cjoA.opNorm, 0.0)
                        Case "sign" : rCjo = New Complex(Math.Sign(cjoA.pRe.ToDouble))
                            If cjoA.pIm.IsZero = False Then
                                bArgNotValid = True
                            End If
                        Case "sinh" : rCjo = Complex.opSinH(cjoA)
                        Case "sqrt" : rCjo = cjoA ^ Complex.oneHalf  ' 2013/08/09
                        Case "tanh" : rCjo = Complex.optanh(cjoA)
                    End Select
                Case 3
                    Select Case fn
                        Case "abs" : rCjo = cjoA.opAbs
                        Case "arg" : rCjo = Complex.opArg(cjoA)
                        Case "cos" : rCjo = Complex.opCos(cjoA)
                        Case "cot" : rCjo = Complex.opCot(cjoA)
                        Case "csc" : rCjo = Complex.opCsc(cjoA) ' csc=1/sin
                        Case "exp" : rCjo = Complex.opExp(cjoA)
                        Case "log" : fn = "ln" : GoTo repiteFn ' log()= ln()
                        Case "mod" : rCjo = New Complex(cjoA.opModulo, 0.0)
                        Case "sec" : rCjo = Complex.opSec(cjoA) ' sec
                        Case "sin" : rCjo = Complex.opSin(cjoA)
                        Case "sqr" : rCjo = cjoA ^ Complex.oneHalf  ' 2013/08/09
                        Case "tan" : rCjo = Complex.optan(cjoA)
                    End Select
                Case 2
                    Select Case fn
                        Case "ln" : rCjo = Complex.opLn(cjoA) ' ln()= log()
                        Case "im" : rCjo = New Complex(New Rational(0.0), cjoA.pIm)
                        Case "re" : rCjo = New Complex(cjoA.pRe, New Rational(0.0))
                    End Select
            End Select
            If bArgNotValid Then
                Throw New Exception( _
                String.Format(msg8.num(29), fn))
            End If
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return rCjo
    End Function
    Public ReadOnly Property ToStringExprParser(cfg As Config) As String
        Get
            Return ret.toStringRetVal(cfg)
        End Get
    End Property
    Public Overrides Function ToString() As String
        Return ret.toStringRetVal(Config.cfg)
    End Function
End Class


<Serializable()> _
Public Class retVal
    Public expr As Expression
    'Public deriv As New Expression(0.0)
    Public sgn As Int64 = 1
    Dim vars As VarsAndFns
    Public rMtx As retMtx = Nothing
    Public Sub New()
    End Sub
    Public Sub New(ByVal expr As Expression, _
                   ByVal vars As VarsAndFns)
        If expr IsNot Nothing Then
            Me.expr = New Expression(expr)
        End If
        Me.vars = vars
    End Sub
    'Public Sub New(ByVal expr As Expression, _
    '               ByVal deriv As Expression, _
    '               ByVal vars As VarsAndFns)
    '    If expr IsNot Nothing Then
    '        Me.expr = New Expression(expr)
    '    End If
    '    If deriv IsNot Nothing Then
    '        Me.deriv = New Expression(deriv)
    '    End If
    '    Me.vars = vars
    'End Sub
    Public Sub New(ByVal rv As retVal)
        With rv
            If .expr IsNot Nothing Then
                Me.expr = .expr.Clone() ' MathGlobal8.CloneObject(.expr)
            End If
            Me.sgn = .sgn
            Me.vars = .vars
        End With
    End Sub
    'Public Sub New(ByVal rv As retVal)
    '    With rv
    '        If .expr IsNot Nothing Then
    '            Me.expr = .expr.Clone() ' MathGlobal8.CloneObject(.expr)
    '        End If
    '        If .deriv IsNot Nothing Then
    '            Me.deriv = .deriv.Clone ' MathGlobal8.CloneObject(.deriv)
    '        End If
    '        Me.sgn = .sgn
    '        Me.vars = .vars
    '    End With
    'End Sub
    Public Sub setVars(ByVal vars As VarsAndFns)
        Me.vars = vars
    End Sub
    Public Sub getSgn()
        If sgn = -1 Then
            expr = -expr
            'If deriv IsNot Nothing Then
            '    deriv = -deriv
            'End If
        End If
        sgn = 1
    End Sub
    Public Overrides Function ToString() As String
        Return expr.ToString
    End Function
    Public ReadOnly Property toStringRetVal(cfg As Config) As String
        Get
            Return expr.ToStringExpr(cfg)
        End Get
    End Property

End Class
