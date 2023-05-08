Imports System.Text.RegularExpressions

<Serializable()> _
Public Class customFn

    Public Shared sParamDelimiter As String = ","
    Dim fnName As String
    Dim vParam(-1) As String
    Dim exprMtx1 As ExprMatrix
    Dim vars As VarsAndFns
    Public cur As currentMatch = Nothing
    Public ReadOnly Property name() As String
        Get
            Return fnName
        End Get
    End Property
    Public Property exprMtx() As ExprMatrix
        Get
            Return exprMtx1
        End Get
        Set(ByVal value As ExprMatrix)
            exprMtx1 = value
        End Set
    End Property
    Public Property param() As String()
        Get
            Return vParam
        End Get
        Set(ByVal value As String())
            vParam = value
        End Set
    End Property

    Public Shared Function tryParseCustomFn( _
            ByVal cfg As Config, _
            ByVal strFnDefinition As String, _
            ByRef result As customFn, _
            ByVal oVar As VarsAndFns, _
            ByVal msg As String) As Boolean
        Dim cFn As customFn = Nothing
        Dim svParam(-1) As String
        Try
            Dim e1() As String = _
                Split(strFnDefinition, "=")
            If e1.Length <> 2 Then
                msg = msg8.num(21)
                Return False
            End If
            e1(0) = Replace(e1(0), " ", "")
            Dim posLP As Int64 = InStr(e1(0), "(")
            If posLP < 2 OrElse Len(e1(0)) - posLP < 2 Then
                msg = msg8.num(22)
                Return False
            End If
            If e1(0).Chars(Len(e1(0)) - 1) <> ")" Then
                msg = msg8.num(23)
                Return False
            End If
            cFn = New customFn
            With cFn
                ' Get function name:
                .fnName = Mid(e1(0), 1, posLP) ' <--include also "(" ,posLP - 1)
                Dim m As Match = Regex.Match(.fnName, _
                        Replace( _
                    cfg.mathGlobal.sVar + "|" + cfg.mathGlobal.sVar2, "{}", ""), _
                        RegexOptions.IgnoreCase)
                If Not m.Success Then
                    msg = String.Format(msg8.num(25), .fnName)
                    Return False
                End If
                e1(0) = Regex.Replace(e1(0), MathGlobal8.sCol, sParamDelimiter)
                svParam = _
                    Regex.Split(Mid(e1(0), posLP + 1, _
                    Len(e1(0)) - posLP - 1), sParamDelimiter)
                Dim i As Int64
                ReDim .vParam(svParam.Length - 1)

                ' Get parameters' names:
                For i = 0 To svParam.Length - 1
                    If Len(svParam(i)) = 0 Then
                        msg = String.Format(msg8.num(24), i + 1)
                        Return False
                    End If
                    Dim Pa As Polynomial = Nothing
                    Try
                        Dim expr As Expression = Expression.ParseExpression(svParam(i))
                        If expr.IsComplex Then
                            svParam(i) = expr.toComplex.toString
                        End If
                    Catch ex As Exception

                    End Try
                    'If Polynomial.tryParsePolynomial(cfg, svParam(i), Pa) Then
                    '    If Pa.isComplex Then
                    '        svParam(i) = Pa.toStringPoly(New Config)
                    '    End If
                    'End If
                    m = Regex.Match(svParam(i), _
                        Replace( _
                    cfg.mathGlobal.sVar + "|" + cfg.mathGlobal.sVar2, "{}", ""), _
                        RegexOptions.IgnoreCase)
                    If Not m.Success Then
                        msg = String.Format(msg8.num(26), svParam(i))
                        Return False
                    End If
                    .vParam(i) = svParam(i)

                    ' añadido para que en (*)
                    ' reconozca la variable (2013/07/21):
                    oVar.AddVar(svParam(i), Nothing, "")
                Next

                '' Get expression (may be a matrix)
                '' and get cFn.vars:
                'Dim pMtx As matrixParser = Nothing
                cfg.initialize()
                'pMtx.bCaseS = cfg.bCaseSensitive
                Dim m8 As New matrixParser
                m8.cfg = cfg
                m8.bCaseS = cfg.bCaseSensitive
                'm8.parse(e1(1), "", oVar) ' (*) ver nota arriba
                m8.parse(Replace(Trim(e1(1)), vbCrLf, "|"), "", oVar)

                If m8.errMsg.Length Then
                    msg = m8.errMsg
                    Return False
                End If
                .vars = New VarsAndFns(oVar)
                'If Not matrixParser.tryParse(e1(1), "", pMtx, msg, .vars) OrElse _
                'pMtx.ret Is Nothing OrElse pMtx.ret.exprMtx Is Nothing Then
                'msg = String.Format(msg8.num(24), strFnDefinition)
                'Return False
                'End If
                '.exprMtx1 = pMtx.ret.exprMtx
                .exprMtx1 = m8.ret.exprMtx
                .cur = m8.ret.getParser.cur
                .cur.bVerifyEq = False
                ' sort .vars.name in the same order
                ' as svParams()
                .vars.sortAs(svParam)
            End With
            result = cFn
        Catch ex As Exception
            Throw ex
        End Try
        Return True
    End Function
    Public Function evalMtx( _
        ByVal semicolonDelimitedValues As String) As Expression
        Dim exprP As exprParser = Nothing
        Try
            Dim i As Int64
            Dim sFnVar() As String = Me.vars.getNamesList
            Dim vP(sFnVar.Length - 1) As String
            Dim i0(vP.Length - 1) As Int64
            ' ordenar las variables de mayor a menor
            ' longitud y alfabéticamente, sino vars.
            ' con igual inicio de nombre "_x" y "_x2" podrían
            ' sustituirse erróneamente unas por otras
            For i = 0 To sFnVar.Length - 1
                vP(i) = String.Format("{0:00}{1}{2:00}", _
                     1000 - Len(sFnVar(i)), sFnVar(i), i)
                i0(i) = i
            Next
            Array.Sort(vP, i0)
            Dim sV() As String = Split( _
                semicolonDelimitedValues, customFn.sParamDelimiter)
            Dim sFnVal As String = Split(ToStringCustFn, "=")(1)

            ' sustituimos las variable por sus valores:
            For i = 0 To sV.Length - 1
                Dim posInFnVar As Int64 = _
                Int64.Parse(Microsoft.VisualBasic.Right(vP(i), 2))
                sFnVal = Replace(sFnVal, _
                    sFnVar(posInFnVar), "(" + sV(posInFnVar) + ")")

            Next
            '...y obtenemos una exprMtx de sFn:
            exprP.parse( _
                sFnVal, "", vars)
        Catch ex As Exception

        End Try
        Return exprP.ret.exprMtx.getExpr(0, 0)
    End Function
    Public Function evalMtx( _
            cfg As Config, _
            ByVal oVars As VarsAndFns) As Expression
        Dim ret As Expression = Nothing
        Try
            ret = Me.exprMtx.getCurExpr.evalExprToExpr(oVars)
        Catch ex As Exception
            Throw ex
        End Try
        Return ret
    End Function
    Public Function evalMtx( _
            cfg As Config, _
            ByVal rMtx As retMtx) As Expression
        Dim ret As Expression = Nothing
        Try
            Dim oVars As New VarsAndFns(Me.vars)
            For i As Int64 = 0 To Me.param.Length - 1
                Dim sP As String = Me.param(i)
                Dim varID As Int64
                If oVars.tryGetVarIDByName(sP, varID) Then
                    oVars.setValue(varID, New ExprMatrix( _
                                   rMtx.exprMtx.getExpr(0, i)))
                End If
            Next
            ret = Me.exprMtx.getCurExpr.evalExprToExpr(oVars)
        Catch ex As Exception
            Throw ex
        End Try
        Return ret
    End Function
    Public Function evalMtxToExprMtx( _
            cfg As Config, _
            ByVal rMtx As retMtx) As ExprMatrix
        Dim ret As ExprMatrix
        Try
            Dim oVars As New VarsAndFns(Me.vars)
            For i As Int64 = 0 To Me.param.Length - 1
                Dim sP As String = Me.param(i)
                Dim varID As Int64
                If oVars.tryGetVarIDByName(sP, varID) Then
                    oVars.setValue(varID, New ExprMatrix( _
                                   rMtx.exprMtx.getExpr(0, i)))
                End If
            Next
            ret = New ExprMatrix(Me.exprMtx)
            For i As Int64 = 0 To Me.exprMtx.Rows - 1
                For j As Int64 = 0 To Me.exprMtx.Cols - 1
                    If oVars.getNamesList.Length = 1 AndAlso oVars.getValueByID(0).IsComplex _
                    AndAlso ret.getExpr(i, j).IsPolynomial Then
                        ret.getExpr(i, j) =
                            New Expression(Me.exprMtx.getExpr(i, j).getPolynomial.evalCjo(
                            oVars.getValueByID(0).toComplex))
                    Else
                        ret.getExpr(i, j) = Me.exprMtx.getExpr(i, j).evalExprToExpr(oVars)
                    End If
                Next
            Next

        Catch ex As Exception
            Throw ex
        End Try
        Return ret
    End Function
    Public ReadOnly Property ToStringCustFn() As String
        Get
            Return ToString()
        End Get
    End Property
    Public Overrides Function ToString() As String
        Return name + Join(vParam, sParamDelimiter) + ") = " + _
            exprMtx.ToStringExprMtx(Config.cfg)
        'Return name + "(" + Join(vParam, sParamDelimiter) + ") = " + _
        'exprMtx.ToStringExprMtx(New Config)
    End Function
End Class