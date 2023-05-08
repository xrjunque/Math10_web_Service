Imports System.Text.RegularExpressions

<Serializable()> _
Public Class matrixParser
    Public Sub New()
        MyBase.New()
    End Sub

    Public ret As retMtx
    Friend vars As VarsAndFns
    Friend parser As exprParser
    Public soe As SystemOfEquations = Nothing
    Dim msgHTML1 As String = "", errMsg1 As String = ""
    Public retCjo(-1) As Complex
    Public retVarNames(-1) As String
    Public cfg As Config
    Dim maxCols As Int64
    Friend bCaseS As Boolean
    Friend cur As currentMatch


    Public Sub New(ByVal ep As exprParser, ByVal vars As VarsAndFns)
        Me.vars = New VarsAndFns(vars)
        Dim ret(0) As exprParser
        Try
            Dim arr() As exprParser = {ep}
            Array.Copy(arr, ret, 1)
        Catch ex As Exception
            Throw ex
        End Try
        parser = ret(0)
    End Sub
    Function tryParse(ByVal sExpression As String, _
                      ByVal sVars As String, _
                      ByRef result As matrixParser, _
                      ByRef msg As String, _
                      ByRef vars As VarsAndFns, _
                      Optional ByRef cfg As Config = Nothing) As Boolean
        Dim m8 As matrixParser = Me
        Dim bRet As Boolean = False
        Dim bErr As Boolean
        Dim bDetail As Boolean
        Dim LP As Int64 = 0
        Dim RP As Int64 = 0
        Try
            msg = ""
            If cfg IsNot Nothing Then
                Me.cfg = cfg
            ElseIf Me.cfg Is Nothing Then
                Me.cfg = Config.cfg
            End If
            cfg = Me.cfg
            If cfg.oDetail IsNot Nothing Then
                cfg.oDetail.Clear()
            End If
            Me.soe = Nothing
            ReDim retCjo(-1)
            bDetail = cfg.bDetail
            If sExpression Is Nothing Then
                Return False
            End If
            soe = Nothing
            ' supress leading and trailing white spaces:
            sExpression = Regex.Replace(sExpression, "(^(\+|\s)+)|((" + _
                     MathGlobal8.sCol + "|" + MathGlobal8.sRow + "|\s+)$)", "")

            If sExpression.Length = 0 Then
                Return False
            End If
            Dim bVarIsNothing As Boolean
            cfg.initialize()
            If vars Is Nothing Then
                vars = New VarsAndFns(cfg)
                bVarIsNothing = True
            End If

            ' Contemplamos el caso p.ejemplo:
            ' x^2*y^5@x=1@y=2
            ' x^2*y^5@x=1@y=-1
            ' En vVars(0) estarán @x=1@y=2
            '    vVars(1)   "     @x=1@y=-1
            ' vVars(0) se añadirá a 'vars' aqui y a medida que en
            ' currentMatch se extráiga una nueva línea se añadirá
            ' vVars(i) a 'vars'
            Dim mcvars As MatchCollection = Regex.Matches(sExpression, "\@+.*(\n\r|\n)?")
            Dim vVars(-1) As VarsAndFns
            If Len(sVars) Then
                If Not VarsAndFns.tryParseVariables(sVars, vars, msg) Then
                    'm8.errMsg1 = msg
                    'If (msg = msg8.num(61)) Then
                    '    msg = msg8.num(82)
                    '    m8.errMsg1 = msg
                    'End If
                    If msg.Length = 0 Then
                        msg = msg8.num(82)
                    End If
                    'msg = msg8.num(82)
                    Exit Try
                End If
                If cfg.oDetail IsNot Nothing Then
                    cfg.oDetail.Clear()
                End If
            End If
            If mcvars.Count Then
                For k As Int64 = 0 To mcvars.Count - 1
                    Dim i As Int64 = Regex.Matches(Mid(sExpression, 1, mcvars(k).Index), "\n").Count
                    ReDim Preserve vVars(i)
                    For k1 = k To i
                        If vVars(k1) Is Nothing Then
                            vVars(k1) = New VarsAndFns(vars)
                        End If
                    Next
                    Dim iVars As String = mcvars(k).Value
                    If Not VarsAndFns.tryParseVariables(iVars, vVars(i), msg) Then
                        m8.errMsg1 = msg
                        Exit Try
                    End If
                    'If i = 0 Then
                    '    For j As Int64 = 0 To vVars(i).getNamesList.Length - 1
                    '        Dim eMtx As ExprMatrix = vVars(i).getValueByID(j)
                    '        vars.AddVar(vVars(i).getVarNameByID(j), Nothing)
                    '        If eMtx IsNot Nothing Then vars.setValue(j, eMtx)
                    '    Next
                    'End If
                Next
                vars = New VarsAndFns(vVars(0))
                sExpression = Regex.Replace(sExpression, "\@+.*[^\n\r]", "")
            End If

            cfg.bDetail = bDetail
            m8.vars = vars
            m8.parser = New exprParser(m8, sExpression, sVars, cfg, m8.vars)
            m8.parser.getVars = m8.vars
            cfg.cur = m8.parser.cur
            cfg.cur.vVars = vVars
            If cfg.bDetail Then
                cfg.oDetail.cur = cfg.cur
            End If
            cfg.bDetail = bDetail
            m8.parser.cfg = cfg
            If m8.cur.bIsMtxExpr Then
                Dim IsEq As Boolean = m8.cur.bIsEquation
                m8.ret = m8.mtxExpr()
                m8.ret.curExpr.IsEquation = IsEq
                Dim isIsoEulers As Boolean = Regex.IsMatch( _
                    sExpression, "isolate|eulers", _
                    RegexOptions.IgnoreCase)
                If m8.cur.bIsEquation <> IsEq AndAlso _
                Not isIsoEulers Then
                    'Throw New Exception(msg8.num(8))
                ElseIf isIsoEulers Then
                    m8.cur.bIsEquation = False
                    m8.cur.bIsDiffEq = False
                End If
            Else
                m8.parser.parent = Nothing
                m8.ret = m8.parser.nextExpr()
                m8.ret.curExpr.IsEquation = m8.cur.bIsEquation
            End If
            If bDetail Then
                cfg.oDetail = m8.ret.cfg.oDetail
            End If
            If m8.cur.LP <> m8.cur.RP Then
                Dim sErr As String = String.Empty
                Dim pos As Int64
                currentMatch.checkParentheses( _
                    cfg, sExpression, True, sErr, pos)
            End If
            If Not cur.bEnd Then
                bErr = True
                errMsg1 = msg8.num(13) + vbCrLf '+ cur.toStrCurMatch(True)
            Else
                bRet = True
            End If
        Catch ex As Exception
            If m8.vars IsNot Nothing Then
                ' try evaluating "a posteriori":
                Try
                    If InStr(ex.Message, "Found a matrix") Then
                        Dim sRow() As String = Regex.Split(sExpression, MathGlobal8.sRow)
                        For i As Int64 = 0 To sRow.Length - 1
                            LP = Regex.Matches(sRow(i), "\(").Count
                            RP = Regex.Matches(sRow(i), "\)").Count
                            If LP <> RP Then
                                bErr = True
                                msg = ex.Message + vbCrLf + vbCrLf + _
                                   "(" + String.Format(msg8.num(63), LP, RP, i + 1) + ")"
                                Exit For
                            End If
                        Next
                        If Not bErr Then
                            For i As Int64 = 0 To sRow.Length / 1
                                Dim sCol() As String = Regex.Split(sRow(i), MathGlobal8.sCol)
                                For j As Int64 = 0 To sCol.Length - 1
                                    LP = Regex.Matches(sCol(j), "\(").Count
                                    RP = Regex.Matches(sCol(j), "\)").Count
                                    If LP <> RP Then
                                        bErr = True
                                        msg = ex.Message + vbCrLf + vbCrLf + _
                                           String.Format(msg8.num(64), LP, RP, i + 1, j + 1)
                                        If bErr AndAlso _
                                        m8.parser IsNot Nothing AndAlso _
                                        m8.cur IsNot Nothing AndAlso _
                                        Not m8.cur.bEnd Then
                                            msg += "<br />" + Replace( _
                                                m8.cur.toStrCurMatch(( _
                                                cfg.outputFormat = outputMsgFormat.HTML)), _
                                                ",", "<span style='color:red'>,</span>")
                                            m8.cur.msg = ""
                                        End If
                                        Exit For
                                    End If
                                Next
                            Next
                        End If
                    End If
                Catch ex3 As Exception

                End Try
                If LP = RP Then
                    If bRet Then
                        Try
                            Dim vars2 As New VarsAndFns(m8.vars) ' save current vars
                            m8.vars = New VarsAndFns(cfg) ' use no variables
                            If m8.ret IsNot Nothing AndAlso _
                            m8.cur.bIsMtxExpr Then
                                m8.ret = m8.mtxExpr()
                            ElseIf m8.ret IsNot Nothing Then
                                m8.parser.parent = Nothing
                                m8.ret = m8.parser.nextExpr()
                            End If
                            If bDetail Then
                                cfg.oDetail = m8.ret.cfg.oDetail
                            End If
                            If Not m8.ret Is Nothing Then
                                For i As Int64 = 0 To m8.ret.rows - 1
                                    For j As Int64 = 0 To m8.ret.cols - 1
                                        Try
                                            Dim expr As Expression = m8.ret.exprMtx.getExpr(i, j)
                                            If expr IsNot Nothing Then
                                                m8.ret.exprMtx.getExpr(i, j) = expr.evalExprToExpr(vars2)
                                            End If
                                        Catch ex3 As Exception

                                        End Try
                                    Next
                                Next
                            End If
                        Catch ex2 As Exception

                        End Try
                        msg = ex.Message ' ex.ToString + ex.StackTrace ' ex.Message
                        bErr = True
                        If m8.parser IsNot Nothing Then
                            m8.cur.msg = msg '  "B" + ex.ToString + ex.StackTrace ' ex.Message
                        End If
                    Else
                        msg = ex.Message
                    End If
                End If
            End If
        Finally
            Dim sErrCurMatch As String = ""
            Try
                If bErr AndAlso _
                m8.parser IsNot Nothing AndAlso _
                m8.cur IsNot Nothing AndAlso _
                Not m8.cur.bEnd AndAlso LP = RP Then
                    sErrCurMatch = "<br />" + m8.cur.toStrCurMatch(True)
                    m8.cur.msg = ""
                End If
            Catch ex2 As Exception

            End Try
            If m8.parser IsNot Nothing AndAlso
            (m8.cur.msg IsNot Nothing OrElse sErrCurMatch.Length) Then
                m8.msgHTML1 = m8.cur.msg + sErrCurMatch
            Else
                m8.errMsg1 = msg
            End If
        End Try
        result = m8
        Return bRet
    End Function
    Public Sub setConfig(cfg As Config)
        Me.cfg = cfg
        Me.cfg.initialize()
    End Sub
    Public Class parseObject
        Public sExpression As String
        Public oVars As VarsAndFns
        Public sVars As String
        Public cfg As Config
        Public msgErr As String
        Public msgHTML As String
        Public mP As matrixParser
        Public Sub New(ByRef mP As matrixParser, _
                     ByVal sExpression As String, _
                     Optional ByVal sVars As String = "", _
                     Optional ByRef vars As VarsAndFns = Nothing, _
                     Optional ByRef cfg As Config = Nothing)
            Me.mP = mP
            Me.sExpression = sExpression
            Me.oVars = vars
            Me.sVars = sVars
            Me.cfg = cfg
        End Sub
        Public Sub parse()
            mP.parse(sExpression, sVars, oVars, cfg)
        End Sub
    End Class
    Public Sub parse(ByVal sExpression As String, _
                     Optional ByVal sVars As String = "", _
                     Optional ByRef vars As VarsAndFns = Nothing, _
                     Optional ByRef cfg As Config = Nothing)
        'Dim msg As String = ""
        Dim result As New matrixParser()
        Dim bDoGCD As Boolean = Polynomial.bDoGCD
        Try
            Dim i, j As Int64
            'If InStr(sExpression, "?") Then
            '    ' arrange input string:
            '    Dim e1() As String = Split(sExpression, "?")
            '    sExpression = e1(0)
            '    If e1.Length > 1 Then
            '        e1(0) = ""
            '        sVars = Join(e1, "|")
            '    End If
            'End If
            Me.msgHTML1 = ""
            Me.errMsg1 = ""

            If cfg IsNot Nothing AndAlso Not cfg.bCaseSensitive Then
                sExpression = LCase(sExpression)
                sVars = LCase(sVars)
            End If
            sExpression = Replace(sExpression, vbCr + vbCrLf, vbCrLf)
            sExpression = Regex.Replace(sExpression, "(\/\*[\w\W]*\*\/)", "", RegexOptions.Singleline)
            sExpression = Regex.Replace(sExpression, MathGlobal8.sComment, vbCrLf)
            sVars = Regex.Replace(sVars, "(\/\*[\w\W]*\*\/)", "", RegexOptions.Singleline)
            sVars = Regex.Replace(sVars, MathGlobal8.sComment, vbCrLf)
            If Not tryParse(sExpression, sVars, result, Me.errMsg1, vars, cfg) Then
                If result IsNot Nothing Then
                    Me.msgHTML1 = result.msgHTML1
                    Me.errMsg1 = result.errMsg1
                End If
                Exit Try
            End If
            Polynomial.bDoGCD = False
            Dim oVar As New VarsAndFns(cfg)
            Dim rup As New ReduceExprUsingPolynomials(oVar)
            Dim bPrevIsEq As Boolean = False
            For i = 0 To ret.exprMtx.Rows - 1
                For j = 0 To ret.exprMtx.ColsInRow(i) - 1
                    If i AndAlso bPrevIsEq <> ret.exprMtx.getExpr(i, j).IsEquation Then
                        Throw New Exception(msg8.num(79))
                    ElseIf i + j = 0 Then
                        bPrevIsEq = ret.exprMtx.getExpr(i, j).IsEquation
                    End If
                    Dim expr As Expression = ret.exprMtx.getExpr(i, j)
                    If expr IsNot Nothing Then
                        If ret.exprMtx.getExpr(i, j).IsComplex Then
                        ElseIf ret.exprMtx.getExpr(i, j).IsPolynomial Then
                            ret.exprMtx.getExpr(i, j).getPolynomial.opReduceCommonExponents()
                            ret.exprMtx.getExpr(i, j).getPolynomial.tryReducePolyResto(True)
                        Else
                            'ret.exprMtx.getExpr(i, j) = _
                            'ret.exprMtx.getExpr(i, j).reduceSummands()

                            expr.reduce()
                            Dim exprReduced As Expression = rup.ReduceUsingPolynomials(expr)
                            exprReduced = rup.ReduceUsingPolynomials(exprReduced)
                            If cfg.bDetail Then
                                Dim e1 As String = expr.ToStringExpr(cfg)
                                Dim e2 As String = exprReduced.ToStringExpr(cfg)
                                If e1 <> e2 Then
                                    cfg.oDetail.Add(e1 + " =" + vbCrLf + "=" + e2)
                                    'cfg.oDetail.Add("= " + e2)
                                End If
                            End If
                            ret.exprMtx.getExpr(i, j) = exprReduced ' rup.ReduceUsingPolynomials(expr)
                            'If Len(expr.ToString) * 2 < Len(ret.exprMtx.getExpr(i, j).ToString) Then
                            '    ret.exprMtx.getExpr(i, j) = expr
                            'End If
                        End If
                    End If
                Next
            Next

            If ret.getParser IsNot Nothing AndAlso
            ret.getParser.cur.bIsDiffEq Then
                ret.curExpr.cfg.cur = ret.getParser.cur
                ret.vars =
                    Differential_Equations.resolveSolvable(ret.curExpr, ret)
            ElseIf ret.getParser IsNot Nothing AndAlso
            ret.getParser.cur.bIsEquation Then
                'Dim nEq As Int64 = 0
                For i = 0 To ret.exprMtx.Rows - 1
                    'If ret.exprMtx.getExpr(i, 0).IsEquation Then
                    '    nEq += 1
                    'End If
                    ret.exprMtx.getExpr(i, 0).IsEquation = True
                Next
                'If nEq <> 0 AndAlso nEq <> ret.exprMtx.Rows Then
                '    errMsg1 = msg8.num(13) + vbCrLf '+ cur.toStrCurMatch(True)
                '    Exit Try
                'End If

                'Dim cfg1 As New Config(cfg)
                'Dim bDetail As Boolean = cfg.bDetail
                'cfg1.bDetail = False
                Dim bDetail As Boolean = cfg.bDetail
                'cfg.bDetail = False
                soe = New SystemOfEquations(
                 Nothing, ret.exprMtx, getVars, Me.getParser.cur, cfg)
                If soe Is Nothing Then
                    Exit Try
                End If
                cfg.resetStartTime()
                ret.diffType = retMtx.diffResponse.varValue
                ret.soe = soe
                If Not soe.resolveSysOfEqs(cfg) Then
                    If soe.sErrMsg.Length Then
                        Me.errMsg1 = soe.sErrMsg
                    ElseIf soe.tipo = eqSysType.isAPolynomial Then
                        ret.curExpr = New Expression(soe.poly)
                        ' a non-lineal eq. transformed into a polynomial:
                        ReDim retCjo(soe.resultValues.Length - 1),
                            retVarNames(0)
                        For i = 0 To soe.resultValues.Length - 1
                            retCjo(i) = soe.resultValues(i)
                        Next
                        retVarNames = soe.resultVars
                    Else
                        If Len(soe.sErrMsg) = 0 Then
                            soe = Nothing
                        End If
                    End If
                ElseIf soe.tipo = eqSysType.PolyDegreeTwoCoefAsVars Then
                    For i = 0 To soe.eG.Rows - 1
                        For j = 0 To soe.eG.Cols - 1
                            soe.eG.getExpr(i, j) =
                                rup.ReduceUsingPolynomials(soe.eG.getExpr(i, j))
                        Next
                    Next
                ElseIf soe.tipo <> eqSysType.isAPolynomial Then
                    'ReDim retCjo(soe.resultValues.Length - 1), _
                    '    retVarNames(soe.resultVars.Length - 1)
                    'For i = 0 To soe.resultValues.Length - 1
                    '    'Dim varId As Int64 = vars.getVarIDByName( _
                    '    '    soe.resultVars(i))
                    '    retCjo(i) = soe.resultValues(i)
                    'Next
                    'retVarNames = soe.resultVars
                    retCjo = soe.resultValues
                    retVarNames = soe.resultVars
                Else
                    'ret.curExpr = New Expression(soe.poly)
                    'ret.exprMtx = New ExprMatrix(soe.poly)
                    'Dim vE(soe.resultValues.Length - 1) As Expression
                    'ret.exprMtx = New ExprMatrix(cfg, vE.Length, 1)
                    'For i = 0 To soe.resultValues.Length - 1
                    '    ret.exprMtx.getExpr(i, 0) = New Expression(soe.resultValues(i))
                    'Next
                    retCjo = soe.resultValues
                    retVarNames = soe.resultVars
                End If
                cfg.bDetail = bDetail

            Else
                retVarNames = vars.getNamesList
                If ret.exprMtx.Cols = 1 Then
                    ReDim retCjo(ret.exprMtx.Rows - 1)
                    Dim bRetCjoIsNull As Boolean = True
                    For i = 0 To ret.exprMtx.Rows - 1
                        If ret.exprMtx.getExpr(i, 0) IsNot Nothing Then
                            If ret.exprMtx.getExpr(i, 0).IsComplex Then
                                retCjo(i) = ret.exprMtx.getExpr(i, 0).toComplex
                                bRetCjoIsNull = False
                            ElseIf ret.exprMtx.getExpr(i, 0).IsPolynomial Then
                                ret.exprMtx.getExpr(i, 0).getPolynomial.tryReducePolyResto()
                            Else
                                Dim expr As Expression =
                                    New Expression(ret.exprMtx.getExpr(i, 0))
                                Dim exprReduced As Expression = rup.ReduceUsingPolynomials(expr)
                                exprReduced = rup.ReduceUsingPolynomials(exprReduced)
                                If cfg.bDetail Then
                                    Dim e1 As String = expr.ToStringExpr(cfg)
                                    Dim e2 As String = exprReduced.ToStringExpr(cfg)
                                    If e1 <> e2 Then
                                        cfg.oDetail.Add(e1 + " =" + vbCrLf + "=" + e2)
                                        'cfg.oDetail.Add("= " + e2)
                                    End If
                                End If
                                ret.exprMtx.getExpr(i, 0) = exprReduced ' rup.ReduceUsingPolynomials(expr)
                            End If
                        End If
                    Next
                    If bRetCjoIsNull OrElse i < ret.rows Then
                        ReDim retCjo(-1)
                    End If
                Else
                    'Dim i, j As Int64
                    For i = 0 To ret.exprMtx.Rows - 1
                        For j = 0 To ret.exprMtx.ColsInRow(i) - 1
                            Dim expr As Expression = ret.exprMtx.getExpr(i, j)
                            If expr IsNot Nothing Then
                                If ret.exprMtx.getExpr(i, j).IsComplex Then
                                ElseIf ret.exprMtx.getExpr(i, j).IsPolynomial Then
                                    ret.exprMtx.getExpr(i, j).getPolynomial.tryReducePolyResto()
                                Else

                                    expr.reduce()
                                    Dim exprReduced As Expression = rup.ReduceUsingPolynomials(expr)
                                    exprReduced = rup.ReduceUsingPolynomials(exprReduced)
                                    If cfg.bDetail Then
                                        Dim e1 As String = expr.ToStringExpr(cfg)
                                        Dim e2 As String = exprReduced.ToStringExpr(cfg)
                                        If e1 <> e2 Then
                                            cfg.oDetail.Add(e1 + " =" + vbCrLf + "=" + e2)
                                            'cfg.oDetail.Add("= " + e2)
                                        End If
                                    End If
                                    ret.exprMtx.getExpr(i, j) = exprReduced ' rup.ReduceUsingPolynomials(expr)
                                    'If Len(expr.ToString) * 2 < Len(ret.exprMtx.getExpr(i, j).ToString) Then
                                    '    ret.exprMtx.getExpr(i, j) = expr
                                    'End If
                                End If
                            End If
                        Next
                    Next

                    'Dim oVar As New VarsAndFns(cfg)
                    'Dim mtx As Matrix = ReduceExprUsingPolynomials.ReduceUsingPolynomials(ret.exprMtx, oVar)
                    'ret.exprMtx = ReduceExprUsingPolynomials.reducedPolynToExprMatrix(New ExprMatrix(mtx), oVar)

                End If
            End If
        Catch ex As Exception
            If TypeOf (ex) Is NullReferenceException Then
                Me.errMsg1 = msg8.num(13)
            Else
                Me.errMsg1 = ex.Message ' ex.tostring
            End If
        Finally
            Polynomial.bDoGCD = bDoGCD
            If ret Is Nothing AndAlso
            Len(Me.errMsg1) = 0 Then
                Me.errMsg1 = msg8.num(13)
            End If
        End Try
    End Sub
    Public ReadOnly Property msgHTML As String
        Get
            Return msgHTML1
        End Get
    End Property
    Public ReadOnly Property errMsg As String
        Get
            Return errMsg1
        End Get
    End Property
    Public Function getVars() As VarsAndFns
        Return vars
    End Function
    Public Function getParser() As exprParser
        Return Me.parser
    End Function
    Public Sub setParser(ByVal parser As exprParser)
        Me.parser = parser
    End Sub
    Public Function mtxExpr(Optional maxCols As Int64 = 0) As retMtx
        Dim ret As New retMtx(parser)
        Try
            Me.maxCols = maxCols
            If cur.bEnd AndAlso _
            parser.ret IsNot Nothing Then
                ret.exprMtx = New ExprMatrix(parser.ret.curExpr)
                Exit Try
            End If
            ret = term()
            Do While Not cur.bEnd
                If Regex.IsMatch(cur.str, "[+]") Then
                    Dim retB As retMtx = term()
                    ret.exprMtx += retB.exprMtx
                ElseIf Regex.IsMatch(cur.str, "[-]") Then
                    Dim retB As retMtx = term()
                    ret.exprMtx -= retB.exprMtx
                Else
                    Exit Do
                End If
            Loop

            If ret.exprMtx IsNot Nothing AndAlso _
            ret.exprMtx.getExpr(0, 0) IsNot Nothing Then
                ret.exprMtx.getExpr(0, 0).IsEquation = parser.bIsEquation
            End If
            If cfg.bDetail AndAlso ret.isMatrix Then
                ret.cfg.cur.ivs = 0
            End If
        Catch ex As Exception
            Throw New Exception(ex.Message)
        Finally
            If ret Is Nothing Then
                Throw New Exception(msg8.num(13))
            End If
            Me.ret = ret
        End Try
        'Me.ret = ret ' New retMtx(ret)
        Return ret
    End Function
    Private Function term() As retMtx
        Dim ret As New retMtx(parser)
        Try
            ret = exponente()
            Do While Not cur.bEnd
                ret.exprMtx.cfg = cfg
                If Regex.IsMatch(cur.str, "[*]") Then
                    Dim retB As retMtx = exponente()
                    ret.exprMtx *= retB.exprMtx
                ElseIf Regex.IsMatch(cur.str, "[/]") Then
                    Dim retB As retMtx = exponente()
                    ret.exprMtx /= retB.exprMtx
                Else
                    Exit Do
                End If
            Loop
        Catch ex As Exception
            Throw New Exception(ex.Message)
        Finally
            If ret Is Nothing Then
                Throw New Exception(msg8.num(13))
            End If
        End Try
        Return ret
    End Function
    Private Function exponente() As retMtx
        Dim ret As New retMtx(parser)
        Try
            ret = factor()
            Do While Not cur.bEnd AndAlso _
            Regex.IsMatch(cur.str, "[\^]")
                Dim rmExp(0) As retMtx
                rmExp(0) = factor()
                'rmExp(0).cfg = Me.cfg
                Dim irve As Int64 = 0
                Do While Not cur.bEnd AndAlso _
                Regex.IsMatch(cur.str, "[\^]")
                    irve += 1
                    ReDim Preserve rmExp(irve)
                    rmExp(irve) = factor()
                    'rmExp(irve).cfg = cfg
                Loop
                For i As Int64 = irve - 1 To 1 Step -1
                    rmExp(i - 1).exprMtx ^= rmExp(i).exprMtx
                Next
                ret.exprMtx ^= rmExp(0).exprMtx
            Loop
        Catch ex As Exception
            Throw New Exception(ex.Message)
        Finally
            If ret Is Nothing Then
                Throw New Exception(msg8.num(13))
            End If
        End Try
        Return ret
    End Function
    Private Function factor() As retMtx  ' expression treatment
        Dim ret As New retMtx(parser)
        Try
siguiente:
            ret = Me.parser.nextExpr()

            If Me.cur.bEnd OrElse _
            Me.cur.tipo = currentMatch.exprType.RP Then
                Dim tipo As currentMatch.exprType
                Dim subT1 As currentMatch.exprSubType1
                Dim subT2 As currentMatch.exprSubType1
                Dim m As Match = cur.testNextMatch(0, tipo, subT1, subT2)
                If subT1 <> currentMatch.exprSubType1.mtxOp AndAlso _
                subT1 <> currentMatch.exprSubType1.mtxFn Then
                    Exit Try
                End If
            ElseIf Me.cur.tipo = currentMatch.exprType.LP Then
                'Dim sgn As Int64 = ret.sgn
                ret = Me.parser.nextExpr()
                'ret.sgn *= sgn
            ElseIf Me.cur.tipo = currentMatch.exprType.custFn Then
                Dim fn As customFn = Nothing
                ' retrive custom function fn:
                vars.IsCustomFn(Me.cur.str, fn)
                'Dim sgn As Int64 = ret.sgn
                ' get fn's parameters:
                Dim sParams As String = ""

                Dim fnRV As retMtx = Me.parser.nextExpr()
                sParams += fnRV.curExpr.ToStringExpr(cfg)
                Do While Me.cur.m.Groups("col").Success OrElse _
                    Me.cur.m.Groups("param").Success
                    Me.cur.doNext()
                    fnRV = Me.parser.nextExpr()
                    sParams += customFn.sParamDelimiter + fnRV.curExpr.ToStringExpr(cfg)
                Loop
                ' eval the function at sParams:
                ret.curExpr = _
                        fn.evalMtx(sParams)
            End If
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return ret
    End Function
    Public Overloads Function ToString(cfg As Config) As String
        Try
            Dim format As outputMsgFormat = cfg.outputFormat
            If soe Is Nothing And ret IsNot Nothing Then
                Dim e1 As String = ret.toStringRetVal(cfg)
                e1 = Regex.Replace(e1, "(?<!(\^|e))\-", " -") ' "-" --> " -", except "^-" or "e-" for ex. "x^-3" "1.2e-2"
                e1 = Replace(e1, "+", " +")
                If format = outputMsgFormat.HTML Then
                    e1 = MathGlobal8.getTable(e1, "black")
                End If
                Return e1
            ElseIf soe IsNot Nothing Then
                Dim e1 As String = soe.ToStringInfo(cfg)
                e1 = Regex.Replace(e1, "(?<!(\^|e))\-", " -") ' "-" --> " -", except "^-" or "e-" for ex. "x^-3" "1.2e-2"
                e1 = Replace(e1, "+", " +")
                If soe.getPolynomial IsNot Nothing AndAlso
                soe.getPolynomial.roots IsNot Nothing Then
                    Me.retCjo = soe.getPolynomial.roots.cjo
                End If
                If format = outputMsgFormat.HTML Then
                    Return MathGlobal8.getTable(e1, "black")
                ElseIf soe.resultValues.Length Then
                    e1 = ""
                    For i = 0 To soe.resultValues.Length - 1
                        Dim sVar As String = ""
                        If i < soe.resultVars.Length Then
                            sVar = soe.resultVars(i)
                        End If
                        e1 += sVar + "=" + soe.resultValues(i).toStringComplex(cfg)
                        If soe.unit IsNot Nothing Then
                            e1 += " " + soe.unit.ToString
                        End If
                        e1 += vbCrLf
                    Next
                    Return e1
                ElseIf Me.retCjo.Length Then
                    e1 = ""
                    For i = 0 To Me.retCjo.Length - 1
                        e1 += Me.retCjo(i).toStringComplex(cfg)
                        If soe.unit IsNot Nothing Then
                            e1 += " " + soe.unit.ToString
                        End If
                        e1 += vbCrLf
                    Next
                    Return e1
                Else
                    Return e1
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return MyBase.ToString()
    End Function
    Public Overloads Function ToString() As String
        Dim cfg As Config = Config.cfg
        Dim omf As outputMsgFormat = cfg.outputFormat
        cfg.outputFormat = outputMsgFormat.plainText
        Dim e1 As String = ToString(cfg)
        cfg.outputFormat = omf
        Return e1
    End Function
    Public Function toStringMatrixParser(cfg As Config) As String
        Try
            If soe Is Nothing And ret IsNot Nothing Then
                Return ret.toStringRetVal(cfg)
            ElseIf soe IsNot Nothing Then
                Dim e1 As String = soe.ToStringInfo(cfg)
                If soe.getPolynomial IsNot Nothing Then
                    Me.retCjo = soe.getPolynomial.roots.cjo
                End If
                Return e1
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return MyBase.ToString()
    End Function
    Public ReadOnly Property ToStringAll(Optional cfg As Config = Nothing) As String
        Get
            If cfg Is Nothing Then
                cfg = New Config
            End If
            Dim sResult As String = ""
            Dim mP As matrixParser = Me
            Dim doTimeout As timeout = cfg.doTimeOut
            Try
                cfg.doTimeOut = timeout.never
                mP.cfg = cfg
                If mP.ret Is Nothing AndAlso
                (mP.retCjo Is Nothing OrElse mP.retCjo.Length = 0) Then
                    If mP.errMsg.Length Then
                        Return mP.errMsg
                    End If
                End If
                Dim seRet As String = ""
                Dim bIsEq As Boolean = False
                If mP.ret IsNot Nothing AndAlso
                mP.ret.getParser IsNot Nothing Then
                    bIsEq = mP.ret.getParser.cur.bIsEquation
                End If
                Dim bIsPoly As Boolean = False
                Dim bIsReal As Boolean = False
                If mP.ret.curExpr IsNot Nothing Then
                    bIsPoly = mP.ret.curExpr.IsPolynomial
                    bIsReal = mP.ret.curExpr.IsReal
                End If
                Dim bIsMtx As Boolean = False
                If mP.ret.exprMtx.Cols + mP.ret.exprMtx.Rows <> 2 Then
                    bIsMtx = True
                    If bIsEq AndAlso mP.ret.exprMtx.Cols <> 1 Then
                        sResult += msg8.num(50) ' found columns, expected a equation 
                        'updateStatusBar(sResult)
                        'DspCalc(e1, sResult, Me.n)
                        Exit Try
                    End If
                End If
                bIsReal = mP.ret.curExpr.IsReal
                sResult += mP.ToString(cfg)
                Dim bOneOrMoreVars As Boolean = (mP.ret.exprMtx.getAllVars.Length >= 1)
                Dim bOneVar As Boolean = (mP.ret.exprMtx.getAllVars.Length = 1)
                If bIsEq AndAlso bOneVar Then
                    seRet += " = 0"
                End If
                Dim sRows() As String = Regex.Split(
                    seRet, MathGlobal8.sRow)
                Dim sCols() As String = Split(
                    Replace(sRows(0), ";", vbTab), vbTab)
                If mP.ret.curExpr.IsPolynomial AndAlso bOneVar AndAlso
                mP.ret.curExpr.getPolynomial.PolyResto Is Nothing Then
                    Dim Pa As Polynomial = mP.ret.curExpr.getPolynomial
                    Dim vsPa() As String = Split(Pa.ToStringFactors(cfg), "=")
                    'C_sessionWeb.sfn(nID) = Pa.toStringPoly(cfg)
                    'C_sessionWeb.sf2(nID) = vsPa(0)
                    'C_sessionWeb.sf3(nID) = "0"
                    'C_sessionWeb.yAuto(nID) = 1
                    Dim min As Double = 1000
                    Dim max As Double = -1000
                    Dim cjo() As Complex = Nothing ' mP.soe.resultValues ' Complex.sortRoots( reOpRoots.cjo)
                    If Pa.roots IsNot Nothing Then
                        cjo = Pa.roots.cjo
                    Else
                        cjo = mP.retCjo
                    End If
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
                        'If min < max Then
                        '    C_sessionWeb.xi(nID) = min
                        '    C_sessionWeb.xf(nID) = max
                        'Else
                        '    C_sessionWeb.xi(nID) = min - 10
                        '    C_sessionWeb.xf(nID) = min + 10
                        'End If
                    End If
                End If
                If bIsEq AndAlso bOneOrMoreVars Then
                    If bOneVar AndAlso
                    cfg.bDetail Then
                        'seRet = cfg.oDetail.ToStringHTML("navy")
                        'seRet = cfg.cur.toStringDetailStack
                        'seRet = cfg.oDetail.ToStringHTML("navy")

                        Dim Pa As Polynomial = mP.ret.curExpr.getPolynomial
                        If Pa IsNot Nothing AndAlso Pa.PolyResto IsNot Nothing Then
                            seRet = "Detail:" + vbCrLf + Pa.toStringPoly(cfg) + " = 0  =>" + vbCrLf
                            Dim div As Polynomial = New Polynomial(Pa.PolyDivisor)
                            Dim resto As New Polynomial(Pa.PolyResto)
                            Pa.PolyResto = Nothing
                            Pa.PolyDivisor = Nothing
                            Pa *= div
                            Pa += resto
                            mP.ret.curExpr = New Expression(Pa)
                            seRet += Pa.toStringPoly(cfg) + " = 0" + vbCrLf
                            sResult += MathGlobal8.getTable(seRet, "navy")
                        ElseIf mP.soe IsNot Nothing Then
                            seRet = mP.soe.sErrMsg
                        End If
                    Else
                        seRet = cfg.oDetail.ToStringDivisionHTML("navy")
                    End If
                ElseIf bIsEq Then
                    If Not bIsReal OrElse mP.ret.curExpr.toDouble <> 0.0 Then
                        sResult += String.Format("<br /><h3>" + msg8.num(50) + "</h3>",
                                        mP.ret.curExpr.toDouble)
                    Else
                        sResult += MathGlobal8.getTable(seRet, "navy")
                        sResult += "<br /><h3>0 = 0</h3>"
                    End If
                Else
                    Dim common As Expression = Nothing
                    Dim expr As Expression = Nothing
                    Dim commonStr As String = ""
                    Dim Expr1 As Expression = Nothing
                    Dim CommExpr As Expression = Nothing
                    Dim commStr As String = ""
                    If Len(seRet) Then
                        sResult += MathGlobal8.getTable(seRet, "navy")
                    End If
                    'If mP.ret.curExpr.getCommonFactor(cfg, CommExpr, Expr1, commStr) Then
                    '    seRet = " = " + commStr + vbCrLf
                    '    sResult += MathGlobal8.getTable(seRet, "navy")
                    'End If
                    If cfg.bDetail AndAlso mP.ret.isMatrix Then
                        sResult += "<br />" + cfg.oDetail.ToStringDivisionHTML("navy")
                    ElseIf cfg.doTimeOut AndAlso Not mP.ret.curExpr.IsPolynomial Then
                        sResult += cfg.cur.toStringDetailStack
                    ElseIf cfg.bDetail Then

                        Dim vDetail() As String = Detall.ToStringDetail(
                            mP.getParser.cur, mP, cfg.oDetail)
                        Dim sDetail As String = Join(vDetail, vbCrLf)
                        sResult += "<br />Detail: <br />"
                        'sResult += MathGlobal8.getTable(sDetail, "")
                        sResult += mP.cur.toStringDetailStack
                        sResult += "<br />" + cfg.oDetail.ToStringDivisionHTML("navy")
                    End If
                End If

            Catch ex As Exception
            Finally
                cfg.doTimeOut = doTimeout
            End Try
            Return sResult
        End Get
    End Property

    Public ReadOnly Property toStrMtxParser As String
        Get
            Return toStringMatrixParser(Config.cfg)
        End Get
    End Property
End Class

