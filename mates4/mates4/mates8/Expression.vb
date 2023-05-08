Imports System.Text
Imports System.Text.RegularExpressions

<Serializable()> _
Public Class Expression
    Dim sgn1 As Int64 = 1
    Dim mOp As Match ' math or function operator
    Dim pA As Polynomial
    Dim Args(-1) As Expression
    Dim IsEquation1 As Boolean
    Dim curPos As Int64 = -1
    Public cfg As Config = Config.cfg
    Dim bIsReducing As Boolean
    Public Sub New(ByVal db As Double)
        pA = New Polynomial(db)
    End Sub
    Public Sub New(ByVal cjo As Complex)
        Me.pA = New Polynomial(cjo)
    End Sub
    Public Sub New(ByVal mOp As Match)
        Me.mOp = MathGlobal8.CloneMatch(mOp)
    End Sub
    Public Sub New(ByVal pA As Polynomial)
        Me.pA = New Polynomial(pA)
    End Sub
    Public Sub New(ByVal expr As Expression)
        CopyExprToMe(expr)
        If cfg IsNot Nothing AndAlso cfg.isTimeout Then
            'Throw New TimeoutException
        End If
    End Sub

    Public Sub setCurPos(curPos As Int64)
        If Me.curPos = -1 Then
            Me.curPos = curPos
        End If
    End Sub
    Public Function Clone() As Expression
        Return Me.MemberwiseClone
    End Function
    Public Property sign As Int64
        Get
            Return sgn1
        End Get
        Set(ByVal value As Int64)
            Try
                If value <> sgn1 AndAlso _
                pA IsNot Nothing Then
                    pA = -pA
                Else
                    sgn1 = value
                End If
            Catch ex As Exception

            End Try
        End Set
    End Property
    Private Sub CopyExprToMe(ByVal expr As Expression)
        Try
            If cfg IsNot Nothing AndAlso cfg.isTimeout Then
                If cfg IsNot Nothing AndAlso cfg.isTimeout Then
                    'Throw New TimeoutException
                End If
            End If
            If expr Is Nothing Then
                Exit Sub
            End If
            With expr
                cfg = .cfg
                sign = .sign
                If .mOp IsNot Nothing Then
                    mOp = MathGlobal8.CloneMatch(.mOp)
                End If
                If .pA IsNot Nothing Then
                    pA = New Polynomial(.pA)
                End If
                IsEquation1 = .IsEquation1
                Dim ln As Int64 = .Args.Length
                If ln Then
                    ' copy all the existent arguments:
                    ReDim Preserve Args(ln - 1)
                    For i As Int64 = 0 To ln - 1
                        Args(i) = New Expression(expr.Args(i))
                    Next
                End If
            End With
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Friend Function getMatchStr() As String
        If mOp Is Nothing Then
            Return ""
        End If
        Return mOp.ToString
    End Function
    Public Sub New(ByVal exprMtx As ExprMatrix)
        Try
            If exprMtx Is Nothing Then
                Return
            End If
            Dim expr As New Expression(exprMtx.getExpr(0, 0))
            CopyExprToMe(expr)
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Shared Function AddFnAndArg0(ByVal mFn As Match, _
                                             ByVal eA As Expression) As Expression
        Dim eC As New Expression(mFn)
        Try
            eC.Args = New Expression() {New Expression(eA)}
        Catch ex As Exception
            Throw ex
        End Try
        Return eC
    End Function
    Public Shared Function AddFnAndArg0(ByVal sFn As String, _
                                             ByVal eA As Expression) As Expression

        Dim eC As Expression = Nothing
        Try
            Dim mFn As Match = Regex.Match(sFn, MathGlobal8.sFn + "|" + _
                                           MathGlobal8.sOp)
            Dim sb As String = eA.getMatchStr
            If (sFn = "exp" AndAlso sb = "ln") OrElse _
            (sFn = "ln" AndAlso sb = "exp") Then
                If eA.sign = 1 Then
                    eC = New Expression(eA.Args(0))
                Else
                    eC = New Expression(New Expression(1.0) / eA.Args(0))
                End If
                Exit Try
            ElseIf sFn = "exp" AndAlso (sb = "*" OrElse sb = "/") Then
                Dim s0 As String = sb
                Dim sA As String = eA.Args(0).getMatchStr
                sb = eA.Args(1).getMatchStr
                If sA = "ln" OrElse sb = "ln" Then
                    Dim iA As Int64 = 0
                    Dim iB As Int64 = 1
                    If sb = "ln" Then
                        iA = 1 : iB = 0
                    End If
                    If s0 = "*" Then
                        If eA.sign = -1 Then
                            ' exp(aa*-ln(bb)) =exp(ln(1/bb)^aa)= bb^-aa
                            eC = eA.Args(iA).Args(0)
                            eC ^= -eA.Args(iB)
                        Else
                            eC = eA.Args(iA).Args(0)
                            eC ^= eA.Args(iB)
                        End If
                    Else
                        If eA.sign = 1 Then
                            ' exp(aa/ln(bb)) = bb^-aa
                            eC = eA.Args(iA).Args(0)
                            eC ^= New Expression(-1.0) / eA.Args(iB)
                        Else
                            eC = eA.Args(iA).Args(0)
                            eC ^= eA.Args(iB)
                        End If
                    End If
                    Exit Try
                End If
            ElseIf sFn = "exp" AndAlso eA.IsPolynomial Then
                Dim Pb As Polynomial = eA.getPolynomial
                If Pb.cf.Length = 1 AndAlso Pb.cf(0).IsReal AndAlso
                Pb.cf(0).pRe.ToDouble <= 0 AndAlso Pb.PolyResto Is Nothing Then
                    If Pb.cf(0).pRe.ToDouble = 0 Then
                        eC = New Expression(1.0)
                        Exit Try
                    End If
                    'eC = New Expression(mFn)
                    'eC.cfg = eA.cfg
                    'ReDim eC.Args(0)
                    'eC.Args(0) = New Expression(-Pb)
                    'Dim m As Match = Regex.Match("/", MathGlobal8.sFn + "|" + _
                    '                       MathGlobal8.sOp)
                    'Dim eD As New Expression(m)
                    'eD.cfg = eA.cfg
                    'ReDim eD.Args(1)
                    'eD.Args(0) = New Expression(1.0)
                    'eD.Args(1) = eC
                    'eC = New Expression(eD)
                    'Exit Try
                End If
            End If

            Dim sgn As Int64 = 1
            If eA.Args.Length Then
                sgn = eA.Args(0).sign
            End If
            If eA.pA IsNot Nothing AndAlso _
            eA.pA.cf.Length = 1 AndAlso _
            eA.pA.cf(0).IsReal Then
                sgn *= Math.Sign( _
                    eA.pA.cf(0).pRe.ToDouble)
            End If
            If sgn = -1 Then
                ' argument's sign is negative:
                Select Case sFn.ToString
                    Case "sin", "tan", "csc", "cot", _
                        "sinh", "tanh", "csch"
                        ' All these are odd functions: f(x) = -f(-x).
                        ' Shift argument's sign into the expression's sign:
                        eC = AddFnAndArg0(mFn, -eA)
                        eC.sign *= -1
                    Case "cos", "sec", _
                        "cosh", "sech", "coth"
                        ' Even functions: f(x) = f(-x)
                        eC = AddFnAndArg0(mFn, -eA)
                    Case Else
                        eC = AddFnAndArg0(mFn, eA)
                End Select
            Else
                eC = AddFnAndArg0(mFn, eA)
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return eC
    End Function
    Public Shared Function ParseExpression(ByVal input As String) As Expression

        Dim retExpr As New Expression(0.0)
        Static rup As New ReduceExprUsingPolynomials
        Try
            input = Replace(input, " ", "")
            input = Regex.Replace(input, MathGlobal8.sLP, "(")
            input = Regex.Replace(input, MathGlobal8.sRP, ")")
            Dim sPat As String = "((?<!e)\-|(?<!e)\+|\*|\/|\^|\))+"
            Dim vM() As String = Regex.Split(input,
             "((?<!e)\-|(?<!e)\+|\*|\/|\^|!)+" + "|" + MathGlobal8.sLP + "|" + MathGlobal8.sRP)
            Dim vExpr(vM.Length - 1) As Expression
            vExpr(0) = New Expression(0.0)
            Dim vOp() As String = {"(", "^", "/", "*", "+", "-"}
            Dim i As Int32
            For i = 0 To vM.Length - 1
                If Len(vM(i)) AndAlso Not Regex.IsMatch(vM(i), sPat) Then
                    Dim db As Double
                    If vM(i) = "(" Then ' a left parenthesis
                        Dim posRP As Int32 = Array.IndexOf(vM, ")", i + 1)
                        Dim sSubExpr As String = String.Empty
                        Dim j As Int64 = i + 1
                        Do
                            If vM(j) = "(" Then
                                ' there are more parentheses enclosed, search
                                ' for the outer RP:
                                posRP = Array.IndexOf(vM, ")", posRP + 1)
                            End If
                            sSubExpr += vM(j)
                            vM(j) = ""
                            j += 1
                        Loop While j < posRP
                        vExpr(i) = Expression.ParseExpression(sSubExpr)
                        vM(i) = "" : vM(j) = ""
                    ElseIf MathGlobal8.TryParseDbl(vM(i), db) Then
                        vExpr(i) = New Expression(db)
                    Else
                        Dim nFn As Int64 = Array.IndexOf(MathGlobal8.vFn, vM(i))
                        If nFn = -1 Then
                            If vM(i) = "i" Then
                                vExpr(i) = New Expression(Complex.i)
                            Else
                                ' Convert a variable (string) into a first degree
                                ' polynomial (for example string 'x' --> polynomial x):
                                Dim x As Polynomial = Polynomial.GetPolynomial(vM(i))
                                ' convert polynomial x into an expression in array vExpr():
                                vExpr(i) = New Expression(x)
                            End If
                        Else
                            ' Functions must enclose arguments by parentheses
                            ' as in cos(x). (Write cos(x)^2 and not cos2(x) nor cosx^2)
                            Dim posRP As Int64 = Array.IndexOf(vM, ")", i + 1)
                            Dim sSubExpr As String = String.Empty
                            For j As Int64 = i + 2 To posRP - 1
                                sSubExpr += vM(j)
                                vM(j) = ""
                            Next
                            ' clear LP and RP:
                            vM(posRP) = "" : vM(i + 1) = ""
                            vExpr(i) = Expression.AddFnAndArg0(vM(i),
                                        Expression.ParseExpression(sSubExpr))
                            vM(i) = ""
                            i = posRP + 1
                        End If
                    End If
                End If
            Next
            Dim pos As Int32 = -1
            For Each e1 As String In vOp
                Do
                    If e1 = "^" Then
                        If pos = -1 Then pos = vM.Length
                        pos = Array.LastIndexOf(vM, e1, pos - 1)
                    Else
                        pos = Array.IndexOf(vM, e1, pos + 1)
                    End If
                    If pos > -1 Then
                        Select Case e1
                            Case "("
                                Dim posRP As Int32 = Array.IndexOf(vM, ")", pos + 1)
                                Dim sSubExpr As String = String.Empty
                                For j As Int64 = pos + 2 To posRP - 1
                                    sSubExpr += vM(j)
                                    vM(j) = ""
                                Next
                                vExpr(pos) = Expression.ParseExpression(sSubExpr)
                                vM(pos) = ""
                            Case "-"
                                If pos = 0 Then
                                    vExpr(0) = vExpr(1).opChgSgn
                                Else
                                    Dim L As Int64 = pos - 1
                                    Do While L > 0 AndAlso vExpr(L) Is Nothing
                                        L -= 1
                                    Loop
                                    Dim R As Int64 = pos + 1
                                    Do While vExpr(R) Is Nothing
                                        R += 1
                                    Loop
                                    vExpr(L) -= vExpr(R)
                                    vExpr(R) = Nothing
                                End If
                                vM(pos) = ""
                            Case Else
                                Dim L As Int64 = pos - 1
                                Do While vExpr(L) Is Nothing
                                    L -= 1
                                Loop
                                Dim sgn As Int64 = 1
                                If L AndAlso vM(L - 1) = "-" Then
                                    L -= 1
                                    If e1 = "^" Then
                                        sgn = -1
                                        vExpr(L) = vExpr(L + 1)
                                    Else
                                        vExpr(L) = vExpr(L + 1).opChgSgn
                                    End If
                                    vExpr(L + 1) = Nothing
                                    vM(L + 1) = vM(L) + vM(L + 1)
                                    vM(L) = ""

                                End If
                                Dim R As Int64 = pos + 1
                                Do While vExpr(R) Is Nothing
                                    R += 1
                                Loop
                                If R > pos + 1 AndAlso vM(R - 1) = "-" Then
                                    vM(R - 1) = ""
                                    vExpr(R) = vExpr(R).opChgSgn
                                End If
                                Select Case e1
                                    Case "-" : vExpr(L) -= vExpr(R)
                                    Case "+" : vExpr(L) += vExpr(R)
                                    Case "*" : vExpr(L) *= vExpr(R)
                                    Case "/" : vExpr(L) /= vExpr(R)
                                    Case "^" : vExpr(L) ^= vExpr(R)
                                        If sgn = -1 Then vExpr(L) = vExpr(L).opChgSgn
                                End Select
                                vExpr(R) = Nothing
                                vM(pos) = ""
                        End Select
                    End If
                Loop While pos <> -1
            Next
            For i = 0 To vExpr.Length - 1
                If vExpr(i) IsNot Nothing Then
                    retExpr += rup.ReduceUsingPolynomials(vExpr(i))
                    'Exit For
                End If
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return retExpr
    End Function
    Public Property IsEquation() As Boolean
        Get
            Return IsEquation1
        End Get
        Set(ByVal value As Boolean)
            IsEquation1 = value
        End Set
    End Property
    Public Property getArgs() As Expression()
        Get
            Return Args
        End Get
        Set(ByVal value As Expression())
            Args = value
        End Set
    End Property
    Public ReadOnly Property getMatch As Match
        Get
            Return mOp
        End Get
    End Property
    Public ReadOnly Property getSign As Int64
        Get
            Return sign
        End Get
    End Property
    Private Function getPolynomialinternal() As Polynomial
        Dim retPoly As Polynomial = Nothing
        Try
            If pA IsNot Nothing Then
                retPoly = pA
                Exit Try
            End If
            If mOp.Groups("fn").Success OrElse
            mOp.Groups("custFn").Success Then
                Throw New Exception(msg8.num(13))
            ElseIf mOp.Groups("num").Success Then
                Dim cjo As Complex = Nothing
                Complex.TryParseComplex(cfg.sImg, mOp.ToString, cjo)
                Return New Polynomial(cjo)
                'Return New Polynomial(Complex.parse(cfg.sImg, mOp.ToString))
                'Return Polynomial.parsePolynomial(mOp.ToString)
            ElseIf mOp.Groups("var").Success Then
                Return Polynomial.GetPolynomial(mOp.ToString)
            ElseIf mOp.Groups("op").Success Then
                Dim e1 As String = mOp.ToString
                Select Case e1
                    Case "-"
                        If Args.Length = 1 Then
                            Return -Args(0).getPolynomialinternal
                        Else
                            Return Args(0).getPolynomialinternal - _
                                Args(1).getPolynomialinternal
                        End If
                    Case "+"
                        Return Args(0).getPolynomialinternal + _
                            Args(1).getPolynomialinternal
                    Case "*"
                        Return Args(0).getPolynomialinternal * _
                            Args(1).getPolynomialinternal
                    Case "/"
                        Return Args(0).getPolynomialinternal / _
                            Args(1).getPolynomialinternal
                    Case "%"
                        Throw New Exception(msg8.num(13))
                    Case "^"
                        Dim auxP As Polynomial = New Polynomial( _
                                                Args(0).getPolynomialinternal)
                        retPoly = New Polynomial(auxP)
                        Dim n As Int64 = Args(1).toDouble()
                        For i As Int64 = 2 To n
                            retPoly *= auxP
                        Next
                End Select
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return retPoly
    End Function
    Public ReadOnly Property getPolynomial() As Polynomial
        Get

            If pA IsNot Nothing Then
                Return pA
            Else
                If Not IsPolynomial() Then
                    Return Nothing
                End If
            End If
            If Me.sign = -1 Then
                Dim Pa As Polynomial = getPolynomialinternal()
                Pa.opChgSgn()
                Return Pa
            End If
            Return getPolynomialinternal()
        End Get
    End Property
    Public Function tryReducePolyResto() As Boolean
        Try
            If pA IsNot Nothing Then
                If pA.tryReducePolyResto() Then
                    Return True
                End If
            End If
        Catch ex2 As Exception
            Throw ex2
        End Try
        Return False
    End Function
    Public Overloads Shared Operator -(ByVal eA As Expression) As Expression
        Dim eC As Expression = Nothing
        Try
            If eA.pA IsNot Nothing Then
                Dim polyA As Polynomial = eA.getPolynomial
                eC = New Expression(-polyA)
            Else
                eC = eA.opChgSgn ' 2013/10/15
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return eC
    End Operator
    Public Overloads Shared Operator -(ByVal eA As Expression, ByVal eB As Expression) As Expression
        Dim eC As Expression = Nothing
        Try
            eC = eA + eB.opChgSgn
        Catch ex As Exception
            Throw ex
        End Try
        Return eC
    End Operator
    Public Overloads Shared Operator +(ByVal eA As Expression, ByVal eB As Expression) As Expression
        Dim eC As Expression = Nothing
        Try
            If eA.IsPolynomial AndAlso eB.IsPolynomial Then
                Dim polyA As Polynomial = eA.getPolynomial
                Dim polyb As Polynomial = eB.getPolynomial
                eC = New Expression(polyA + polyb)
                Exit Try
            End If
            If eA.IsReal AndAlso eA.toDouble = 0 Then
                eC = New Expression(eB)
                Exit Try
            ElseIf eB.IsReal AndAlso eB.toDouble = 0 Then
                eC = New Expression(eA)
                Exit Try
            End If
            eC = exprOp("+", eA, eB)
            eC.reduce()
        Catch ex As Exception
            Throw ex
        End Try
        Return eC
    End Operator
    Public Overloads Shared Operator *(ByVal eA As Expression, ByVal eB As Expression) As Expression
        Dim eC As Expression = Nothing
        Try
            If eA.IsPolynomial AndAlso eB.IsPolynomial Then
                Dim polyA As Polynomial = eA.getPolynomial
                Dim polyb As Polynomial = eB.getPolynomial
                eC = New Expression(polyA * polyb)
                Exit Try
            End If
            If eA.IsReal Then
                Dim dbA As Double = eA.toDouble
                If dbA = 0.0 Then
                    eC = New Expression(0.0)
                    Exit Try
                ElseIf dbA = 1.0 Then
                    eC = New Expression(eB)
                    Exit Try
                ElseIf dbA = -1.0 Then
                    eC = New Expression(-eB)
                    Exit Try
                End If
            ElseIf eB.IsReal Then
                Dim dbB As Double = eB.toDouble
                If dbB = 0.0 Then
                    eC = New Expression(0.0)
                    Exit Try
                ElseIf dbB = 1.0 Then
                    eC = New Expression(eA)
                    Exit Try
                ElseIf dbB = -1 Then
                    eC = New Expression(-eA)
                    Exit Try
                End If
            End If

            Dim bIsMnA() As Boolean = Nothing
            Dim vSumA() As Expression = eA.exprToSummands(bIsMnA)
            Dim bIsMnB() As Boolean = Nothing
            Dim vSumB() As Expression = eB.exprToSummands(bIsMnB)
            eC = New Expression(0.0)
            For i As Int64 = 0 To vSumA.Length - 1
                For j As Int64 = 0 To vSumB.Length - 1
                    If bIsMnA(i) = bIsMnB(j) Then
                        eC += exprOp("*", vSumA(i), vSumB(j))
                    Else
                        eC -= exprOp("*", vSumA(i), vSumB(j))
                    End If
                Next
            Next

            'Dim sgn As Int64 = eA.sign * eB.sign
            'Dim eA2 As New Expression(eA)
            'eA2.sgn1 = 1
            'Dim eB2 As New Expression(eB)
            'eB2.sgn1 = 1
            'eC = exprOp("*", eA2, eB2)
            'eC.sgn1 = sgn

            eC.reduce()
        Catch ex As Exception
            Throw ex
        End Try
        Return eC
    End Operator
    Public Overloads Shared Operator /(ByVal eA As Expression, ByVal eB As Expression) As Expression
        Dim eC As Expression = Nothing
        Try
            If eA.IsComplex AndAlso eB.IsComplex Then
                Dim cjoA As Complex = eA.toComplex
                Dim cjoB As Complex = eB.toComplex
                eC = New Expression(cjoA / cjoB)
                Exit Try
            End If
            If eA.IsPolynomial AndAlso eB.IsPolynomial Then
                Dim polyA As Polynomial = eA.getPolynomial
                Dim polyb As Polynomial = eB.getPolynomial
                eC = New Expression(polyA / polyb)
                Exit Try
            End If
            Dim mult As Complex = Nothing
            If eA.isEqualTo(eB, mult) Then
                eC = New Expression(1.0 / mult)
                Exit Try
            End If
            If eB.getMatchStr = "/" Then
                eC = New Expression(eB)
                eC.Args(0) = New Expression(eB.Args(1))
                eC.Args(1) = New Expression(eB.Args(0))
                If eC.Args(1).IsReal AndAlso Math.Abs(eC.Args(1).toDouble) = 1.0 Then
                    Dim db As Double = eC.Args(1).toDouble
                    eC = eC.Args(0)
                    If db = -1 Then
                        eC = eC.opChgSgn
                    End If
                End If
                Return eA * eC
            ElseIf eA.getMatchStr = "!" AndAlso eB.getMatchStr = "!" Then
                Dim pA As Polynomial = Nothing
                Dim pB As Polynomial = Nothing
                If eA.Args(0).IsPolynomial AndAlso eB.Args(0).IsPolynomial Then
                    pA = eA.Args(0).getPolynomial
                    pB = eB.Args(0).getPolynomial
                    Dim pC As Polynomial = pA - pB
                    If pC.isReal Then
                        Dim db As Double = pC.ToDouble
                        If db = Math.Floor(db) Then
                            If db >= 0 Then
                                pC = New Polynomial(pA)
                                For i1 As Int64 = 1 To db - 1
                                    pC *= (pA - New Polynomial(i1))
                                Next
                                eC = New Expression(pC)
                            Else ' db<0
                                pC = New Polynomial(pB)
                                For i1 As Int64 = 1 To -db - 1
                                    pC *= (pB - New Polynomial(i1))
                                Next
                                eC = exprOp("/", New Expression(1.0), New Expression(pC))
                            End If
                            Exit Try
                        End If
                    End If
                End If
            End If
            Dim sgn As Int64 = eA.sign * eB.sign
            Dim eA2 As New Expression(eA)
            eA2.sgn1 = 1
            Dim eB2 As New Expression(eB)
            eB2.sgn1 = 1
            eC = exprOp("/", eA2, eB2)
            eC.sgn1 = sgn
            eC.reduce()
        Catch ex As Exception
            Throw ex
        End Try
        Return eC
    End Operator
    Public Overloads Shared Operator ^(ByVal eA As Expression, ByVal eB As Expression) As Expression
        Dim eC As Expression = Nothing
        Try
            If eA.IsComplex AndAlso eB.IsComplex Then
                eC = New Expression(eA.toComplex ^ eB.toComplex)
                Exit Try
            End If
            If eB.IsReal Then
                Dim dbB As Double = eB.toDouble
                If dbB = 0.0 Then
                    eC = New Expression(1.0)
                    Exit Try
                ElseIf dbB = 1.0 Then
                    eC = New Expression(eA)
                    Exit Try
                ElseIf dbB > 1.0 AndAlso dbB = Math.Floor(dbB) Then
                    If eA.IsPolynomial Then
                        Dim pA As Polynomial = eA.getPolynomial
                        pA ^= eB.getPolynomial
                        eC = New Expression(pA)
                    Else
                        eC = New Expression(eA)
                        For i As Int64 = 2 To Math.Floor(dbB)
                            eC *= eA
                        Next
                        Dim reduce As New ReduceExprUsingPolynomials
                        eC = reduce.ReduceUsingPolynomials(eC)
                    End If
                    Exit Try
                End If
            End If
            If eA.IsReal Then
                Dim dbA As Double = eA.toDouble
                If dbA = 0.0 Then
                    eC = New Expression(0.0)
                    Exit Try
                ElseIf dbA = 1.0 Then
                    eC = New Expression(1.0)
                    Exit Try
                End If
            End If

            eC = exprOp("^", eA, eB)
            eC.reduce()
        Catch ex As Exception
            Throw ex
        End Try
        Return eC
    End Operator

    Public Function evalToCjo(ByVal Var() As String, _
                              ByVal vCjoValues() As Complex) As Complex
        Dim cjo As Complex = Nothing
        Dim bUseRnd As Boolean = cfg.bRounding
        cfg.bRounding = False
        Try
            Dim oVars As New VarsAndFns(cfg)
            For i As Int64 = 0 To Var.Length - 1
                oVars.AddVar(Var(i), New Expression(vCjoValues(i)))
            Next
            cjo = Me.evalExprToExpr(oVars).pA.ToComplex
        Catch ex As Exception
            cjo = Nothing
        Finally
            cfg.bRounding = bUseRnd
        End Try
        Return cjo
    End Function
    Public Function eval_1var_DblToCjo(ByVal x As Double) As Complex
        Return evalExpression(New Complex(x))
    End Function
    Public Function evalExpression(ByVal xCjo As Complex,
                                   Optional ByVal oVars As VarsAndFns = Nothing,
                                   Optional retNullIfError As Boolean = False) As Complex
        ' input parameters:
        ' 1) First option if expression has just 1 var., say x=-1:
        '    then input parameter xCjo should be = -1 and oVars=nothing (or omitted)
        ' 2) Second option: xCjo is unused and instead oVars is not
        ' nothing and contains one or more variables values. This
        ' can be achived if we now, for ex., the variables names and values,
        ' say x=2, y=3:
        'Dim sExpr As String = "x^2+y^2"
        'Dim oVars1 As VarsAndFns = Nothing
        'Dim eP As exprParser = exprParser.parse( _
        '        sExpr, "", oVars1)
        'eP.nextExpr()
        'oVars1.setValue(oVars1.getVarIDByName("x"), New ExprMatrix(2.0))
        'oVars1.setValue(oVars1.getVarIDByName("y"), New ExprMatrix(3.0))
        'Dim cjo As Complex = eP.ret.curExpr.evalExpression(  Nothing, oVars1)
        'Trace.WriteLine(cjo.toStringComplex(4)) ' shows 13
        'oVars1.setValue(oVars1.getVarIDByName("x"), New ExprMatrix(3.0))
        'oVars1.setValue(oVars1.getVarIDByName("y"), New ExprMatrix(4.0))
        'Dim cjo2 As Complex = eP.ret.curExpr.evalExpression(  Nothing, oVars1)
        'Trace.WriteLine(cjo2.toStringComplex(4)) ' shows 25
        ' note: don't use this snippet above here! copy to a form
        ' or a console application before executing (otherwise stack will overflow)

        Dim retCjo As Complex = Nothing
        Try
            If xCjo Is Nothing Then
                xCjo = oVars.getValueByID(0).getExpr(0, 0).getPolynomial.cf(0)
            End If
            If IsPolynomial() Then
                pA = getPolynomial
                If pA.isComplex OrElse pA.isReal Then
                    retCjo = pA.ToComplex
                    'ElseIf pA.var.Length < 2 Then
                    '   retCjo = pA.evalCjo(xCjo)
                    'If xCjo.pIm.ToDouble = 0.0 Then
                    '    retCjo = New Complex(pA.evalRe(xCjo.pRe.ToDouble))
                    'Else
                    '    'retCjo = pA.evalCjo(xCjo)
                    'End If
                ElseIf oVars IsNot Nothing Then
                    Dim values(pA.var.Length - 1) As Complex
                    For i As Int64 = 0 To values.Length - 1
                        Dim eM As ExprMatrix = oVars.getValueByName(pA.var(i), False)
                        If eM IsNot Nothing AndAlso eM.IsComplex Then
                            values(i) = eM.toComplex
                        Else
                            Dim expr As Expression =
                                oVars.supressInternalVars(eM.getExpr(0, 0))
                            If expr.IsComplex Then
                                values(i) = expr.toComplex
                            End If
                        End If
                    Next
                    retCjo = pA.evalMulti(values)
                Else
                    retCjo = pA.evalCjo(xCjo)
                End If
            ElseIf mOp.Groups("num").Success Then
                Dim dbl As Double
                MathGlobal8.TryParseDbl(mOp, dbl)
                retCjo = New Complex(pA.evalRe(dbl))
            ElseIf mOp.Groups("var2").Success _
            OrElse mOp.Groups("var").Success Then
                If oVars Is Nothing Then
                    retCjo = New Complex(xCjo)
                Else
                    retCjo = New Complex(
                    oVars.getValueByName(mOp.ToString, True).getExpr(0, 0).pA.cf(0))
                End If
            ElseIf mOp.Groups("fn").Success Then
                retCjo = exprParser.evalFn(mOp.ToString,
                            Args(0).evalExpression(xCjo))
            Else
                Dim bArgNotValid As Boolean = False
                Dim a As Complex = Args(0).evalExpression(xCjo, oVars)
                Dim b As Complex = Nothing
                If Args.Length > 1 Then b = Args(1).evalExpression(xCjo, oVars)
                Select Case mOp.ToString
                    Case "-" : retCjo = a - b
                    Case "+" : retCjo = a + b
                    Case "*" : retCjo = a * b
                    Case "/" : retCjo = a / b
                    Case "^" : retCjo = a ^ b
                    Case "%"
                        If Not a.pIm.IsZero OrElse Not b.pIm.IsZero Then
                            bArgNotValid = True
                        Else
                            retCjo = New Complex(a.pRe.ToDouble Mod b.pRe.ToDouble)
                        End If
                    Case "!"
                        If Not a.pIm.IsZero Then
                            bArgNotValid = True
                        Else
                            Dim db As Double = a.pRe.ToDouble
                            If db = 0.0 Then
                                db = 1.0
                            Else
                                For i As Int64 = db - 1 To 2 Step -1
                                    db *= CDbl(i)
                                Next
                            End If
                            retCjo = New Complex(db)
                        End If
                End Select
                If bArgNotValid Then
                    Throw New Exception(
                    String.Format(msg8.num(29), mOp.ToString))
                End If
            End If
        Catch ex As Exception
            If retNullIfError Then
                Return Nothing
            Else
                Throw ex
            End If
        End Try
        If Me.sign = -1 Then
            retCjo = -retCjo
        End If
        Return retCjo
    End Function
    Public Function evalExprToExpr( _
            Optional ByVal oVars As VarsAndFns = Nothing) As Expression

        ' Input: oVars may or not contain one or more variables values. 
        Dim ret As Expression = Nothing
        Dim e1 As String = ""
        Try
            If cfg.bDetail Then
                e1 = Me.ToStringExpr(cfg)
            End If
            If pA IsNot Nothing Then
                ret = New Expression( _
                    pA.evalMultiCjoToExpr(oVars))
            ElseIf mOp.Groups("num").Success Then
                Dim dbl As Double
                MathGlobal8.TryParseDbl(mOp, dbl)
                ret = New Expression(pA.evalRe(dbl))
            ElseIf mOp.Groups("var2").Success _
            OrElse mOp.Groups("var").Success Then
                If oVars Is Nothing Then
                    ret = New Expression( _
                        Polynomial.GetPolynomial(mOp.ToString))
                Else
                    Dim eM As ExprMatrix = _
                        oVars.getValueByName(mOp.ToString, False)
                    If eM Is Nothing Then
                        ret = New Expression( _
                            Polynomial.GetPolynomial(mOp.ToString))
                    Else
                        ret = eM.getExpr(0, 0)
                    End If
                End If
            ElseIf mOp.Groups("fn").Success Then
                Dim evalArg As Expression = Args(0).evalExprToExpr(oVars)
                If evalArg.IsComplex Then
                    ret = New Expression(exprParser.evalFn( _
                                         mOp.ToString, _
                                         evalArg.getPolynomial.cf(0)))
                ElseIf mOp.ToString = "diff" Then
                    Dim sVar As String = Args(0).getPolynomial.var1(0)
                    Dim sDiffResp As String = Args(1).getPolynomial.var1(0)
                    Dim id0 As Int64 = oVars.getVarIDByName(sVar)
                    Dim expr1 As Expression = oVars.getValueByID(id0).getExpr(0, 0)
                    ret = expr1.opDiff(sDiffResp)
                Else
                    ret = Expression.AddFnAndArg0(mOp.ToString, _
                                evalArg)
                End If
            ElseIf mOp.Groups("custFn").Success Then
                ret = oVars.getValueByName(mOp.ToString, True).getCurExpr
            Else
                Dim bArgNotValid As Boolean = False
                Dim arg0 As Expression = Args(0).evalExprToExpr(oVars)
                Dim arg1 As Expression = Nothing '
                If mOp.ToString <> "!" Then
                    arg1 = Args(1).evalExprToExpr(oVars)
                End If
                Select Case mOp.ToString
                    Case "-" : ret = arg0 - arg1
                    Case "+" : ret = arg0 + arg1
                    Case "*" : ret = arg0 * arg1
                    Case "/" : ret = arg0 / arg1
                    Case "^"
                        If arg0.mOp IsNot Nothing AndAlso _
                        arg0.mOp.ToString = "^" Then
                            ' (a^b)^c = a^(b*c)
                            ret = arg0.Args(0) ^ (arg0.Args(1) * arg1)
                        ElseIf arg0.IsReal AndAlso arg1.IsReal Then
                            Try
                                ret = arg0 ^ arg1
                            Catch ex As Exception
                                If arg1.toDouble < 0 Then
                                    ret = New Expression(0.0)
                                ElseIf arg0.toDouble > 0 Then
                                    Throw New Exception(msg8.num(71)) ' infinity
                                Else
                                    Throw New Exception(msg8.num(72)) ' -infinity
                                End If
                            End Try
                        Else
                            ret = arg0 ^ arg1
                        End If
                    Case "%"
                        Dim expr0 As Expression = Args(0).evalExprToExpr(oVars)
                        Dim expr1 As Expression = Args(1).evalExprToExpr(oVars)
                        If expr0.IsPolynomial = False OrElse _
                            expr1.IsPolynomial = False Then
                            bArgNotValid = True
                        Else
                            Dim cjo1 As Complex = expr0.getPolynomial.cf(0)
                            Dim cjo2 As Complex = expr1.getPolynomial.cf(1)
                            If Not cjo1.pIm.IsZero OrElse Not cjo2.pIm.IsZero Then
                                bArgNotValid = True
                            Else
                                ret = New Expression(cjo1.pRe.ToDouble Mod cjo2.pRe.ToDouble)
                            End If
                        End If
                    Case "!"
                        Dim expr As Expression = Args(0).evalExprToExpr(oVars)
                        Dim sVar(-1) As String
                        expr.getAllVars(sVar)
                        If Not expr.IsPolynomial OrElse sVar.Length Then
                            'bArgNotValid = True
                            ret = New Expression(Me)
                            ReDim ret.Args(0)
                            ret.Args(0) = expr
                        Else
                            Dim cjo As Complex = expr.getPolynomial.cf(0)
                            If Not cjo.pIm.IsZero Then
                                bArgNotValid = True
                            Else
                                Dim db As Double = cjo.pRe.ToDouble
                                If db = 0.0 Then
                                    db = 1.0
                                ElseIf db > 50 Then
                                    db = Double.MaxValue / 2
                                ElseIf db < 50 Then
                                    db = -Double.MaxValue / 2
                                Else
                                    For i As Int64 = db - 1 To 2 Step -1
                                        db *= CDbl(i)
                                    Next
                                End If
                                ret = New Expression(db)
                            End If
                        End If
                End Select
                If bArgNotValid Then
                    Throw New Exception( _
                    String.Format(msg8.num(29), mOp.ToString))
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        If Me.sign = -1 Then
            ret = -ret
        End If
        Try
            If ret.IsPolynomial Then
                ret.pA = ret.getPolynomial
            End If
            If Len(e1) Then
                Dim e2 As String = ret.ToStringExpr(cfg)
                If e1 <> e2 Then
                    If cfg.bDetail Then
                        cfg.oDetail.Add(e1 + " =")
                        cfg.oDetail.Add("= " + e2)
                    End If
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return ret
    End Function
    Public Function opChgSgn() As Expression

        ' Input: oVars may or not contain one or more variables values. 
        Dim ret As Expression = New Expression(Me)
        Try
            If pA IsNot Nothing Then
                ret = New Expression(-pA)
            ElseIf mOp Is Nothing Then
                ret = Me
            ElseIf mOp.Groups("num").Success Then
                Dim dbl As Double
                MathGlobal8.TryParseDbl(mOp, dbl)
                ret = New Expression(-dbl)
            ElseIf mOp.Groups("var2").Success _
            OrElse mOp.Groups("var").Success Then
                ret = New Expression( _
                -Polynomial.GetPolynomial(mOp.ToString))
            ElseIf mOp.Groups("fn").Success Then
                ret.sign *= -1
            ElseIf mOp.Groups("custFn").Success Then
                ret.sign *= -1
            Else
                Select Case mOp.ToString
                    Case "-", "+"
                        ret.Args(0) = ret.Args(0).opChgSgn()
                        If Args.Length > 1 Then
                            ret.Args(1) = ret.Args(1).opChgSgn()
                        End If
                    Case "^"
                        ret.sign *= -1
                    Case Else
                        ret.Args(0) = ret.Args(0).opChgSgn()
                End Select
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return ret
    End Function

    Public Shared Function exprOp(ByVal optor As Char, ByVal eA As Expression, ByVal eB As Expression) As Expression
        Static enUso As Boolean = False
        Dim eC As Expression = Nothing
        Try
            If optor = "^" Then
                If eB.IsReal AndAlso eB.toDouble = -1 Then
                    optor = "/"
                    eC = New Expression(Regex.Match(optor, MathGlobal8.sOp))
                    eC.Args = New Expression() { _
                        New Expression(1.0), _
                        New Expression(eA)}
                    Exit Try
                End If
            End If

            If optor = "/" AndAlso eB.getMatchStr = "^" AndAlso _
              Not eB.Args(1).IsReal Then
                optor = "*"
                eB.Args(1) = eB.Args(1).opChgSgn
            End If

            eC = New Expression(Regex.Match(optor, MathGlobal8.sOp))
            'If eA.IsReal AndAlso eA.toDouble = Math.E Then
            '    eC = Expression.AddFnAndArg0("exp", eB)
            '    Exit Try
            'End If

            eC.Args = New Expression() {
                New Expression(eA),
                New Expression(eB)}
        Catch ex As Exception
            Throw ex
        End Try
        Return eC
    End Function
    Public Function toDouble() As Double
        Try
            If pA IsNot Nothing AndAlso pA.isReal Then
                Return pA.cf(0).pRe.ToDouble * Me.sign
            Else
                ' not a real (double) value
                Throw New Exception(msg8.num(13)) ' n/a
            End If
        Catch ex As Exception
            Throw ex
        End Try
    End Function
    Public Function IsReal() As Boolean
        If Not IsPolynomial() Then
            Return False
        ElseIf pA Is Nothing Then
            Dim expr As New Expression(getPolynomial)
            CopyExprToMe(expr)
        End If
        If pA IsNot Nothing AndAlso pA.isReal Then
            Return True
        End If
        Return False
    End Function
    Public Function toComplex() As Complex
        Try
            If Not IsPolynomial() Then
                Return Nothing
            ElseIf pA Is Nothing Then
                Dim expr As New Expression(getPolynomial)
                CopyExprToMe(expr)
            End If
            If pA IsNot Nothing AndAlso pA.isComplex Then
                Return pA.cf(0)
            Else
                ' not a complex:
                Throw New Exception(msg8.num(13)) ' n/a
            End If
        Catch ex As Exception
            Throw ex
        End Try
    End Function
    Public Function IsComplex() As Boolean
        If Not IsPolynomial() Then
            Return False
        ElseIf pA Is Nothing Then
            Dim expr As New Expression(getPolynomial)
            CopyExprToMe(expr)
        End If
        If pA IsNot Nothing AndAlso pA.isComplex Then
            Return True
        End If
        Return False
    End Function
    Public Function IsPolynomial() As Boolean
        If pA IsNot Nothing Then
            Return True
        End If
        If cfg IsNot Nothing AndAlso cfg.isTimeout Then
            'Throw New TimeoutException
        End If
        If mOp IsNot Nothing Then
            If mOp.Groups("fn").Success Then
                If Args.Length = 0 Then
                    Dim a = 0
                End If
                If Args(0).IsComplex Then
                    Try
                        If Me.sgn1 = 1 Then
                            CopyExprToMe(New Expression(exprParser.evalFn(
                                    mOp.ToString, Args(0).getPolynomial.ToComplex)))
                        Else
                            CopyExprToMe(New Expression(-exprParser.evalFn(
                                    mOp.ToString, Args(0).getPolynomial.ToComplex)))
                        End If
                        Return True
                    Catch ex2 As Exception
                        Return False
                    End Try
                Else
                    Return False
                End If
            End If
            If mOp.Groups("custFn").Success Then
                Return False
            ElseIf mOp.Groups("var").Success Then
                Return True
            ElseIf mOp.Groups("op").Success Then
                Dim e1 As String = mOp.ToString
                Select Case e1
                    Case "!"
                        Return Args(0).IsReal
                    Case "-" ', "!"
                        If Args.Length = 1 Then
                            Return Args(0).IsPolynomial
                        Else
                            Return (Args(0).IsPolynomial AndAlso _
                                Args(1).IsPolynomial)
                        End If
                    Case "+", "*", "/", "%"
                        Return (Args(0).IsPolynomial AndAlso _
                            Args(1).IsPolynomial)
                    Case "^"
                        Dim Pa As Polynomial = Nothing
                        Dim Pb As Polynomial = Nothing
                        If Args(0).IsPolynomial Then
                            Pa = New Polynomial(Args(0).getPolynomial)
                        End If
                        If Args(1).IsPolynomial Then
                            Pb = New Polynomial(Args(1).getPolynomial)
                        End If
                        If Args(1).IsReal Then
                            If Args(0).IsReal Then
                                If sgn1 = 1 Then
                                    CopyExprToMe(New Expression(Pa ^ Pb))
                                Else
                                    CopyExprToMe(-New Expression(Pa ^ Pb))
                                End If
                                mOp = Nothing
                                ReDim Args(-1)
                                Return True
                            End If
                            Dim db As Double = Args(1).toDouble
                            If db = Math.Floor(db) AndAlso _
                            Args(0).IsPolynomial Then
                                Dim Pc As Polynomial = Pa ^ Pb
                                If Me.sign = -1 Then
                                    Pc.opChgSgn()
                                End If
                                CopyExprToMe(New Expression(Pc))
                                mOp = Nothing
                                ReDim Args(-1)
                                Return True
                            End If
                        End If
                        Return False
                End Select
            End If
        End If
        Return False
    End Function
    Public Function IsIntegrationConst() As Boolean
        If IsPolynomial() AndAlso _
        InStr(Join(getPolynomial.var), MathGlobal8.sIntConst) Then
            Return True
        End If

        Return False
        'If pA IsNot Nothing AndAlso _
        'pA.var IsNot Nothing AndAlso _
        'pA.var.Length = 1 AndAlso _
        'pA.var(0) = MathGlobal8.integralCnst Then
        '    Return True
        'End If
        'Return False
    End Function
    Public Function reduceSummands() As Expression
        Return New Expression(Me)
        'If bIsReducing Then
        '    Return Me
        'End If
        'bIsReducing = True
        'Dim ret As Expression = Me.ReduceUsingPolynomials
        'bIsReducing = False
        'Return ret

        'Dim retExpr As Expression = Nothing
        ''Dim sgn As Int64 = Me.sign
        'Try
        '    If Me.IsPolynomial Then
        '        Return Me
        '    End If
        '    Dim i As Int64
        '    Dim bIsMinus(-1) As Boolean
        '    Dim vSummands() As Expression = _
        '        exprToSummands(bIsMinus)
        '    If vSummands.Length = 1 Then
        '        Dim bIsDiv(-1) As Boolean
        '        Dim vFactors() As Expression = _
        '            exprToFactors(bIsDiv)
        '        If vFactors.Length > 1 Then
        '            For i = 0 To vFactors.Length - 1
        '                vFactors(i) = vFactors(i).reduceFactors(False)
        '            Next
        '            Return Expression.factorsToExpr(vFactors, bIsDiv)
        '        End If
        '        Return New Expression(Me)
        '    End If
        '    Dim polyC As New Polynomial(0.0)
        '    Dim den As Expression = Nothing
        '    Dim factor As Complex
        '    For i = 0 To vSummands.Length - 1
        '        If vSummands(i) IsNot Nothing Then
        '            If vSummands(i).IsPolynomial Then
        '                If bIsMinus(i) Then
        '                    polyC -= vSummands(i).pA
        '                Else
        '                    polyC += vSummands(i).pA
        '                End If
        '                vSummands(i) = Nothing
        '            Else

        '                factor = Complex.zero
        '                For j As Int64 = i + 1 To vSummands.Length - 1
        '                    If vSummands(j) IsNot Nothing Then
        '                        If vSummands(j).IsPolynomial Then
        '                            If bIsMinus(j) Then
        '                                polyC -= vSummands(j).pA
        '                            Else
        '                                polyC += vSummands(j).pA
        '                            End If
        '                            vSummands(j) = Nothing
        '                        ElseIf vSummands(j) IsNot Nothing Then
        '                            Dim mult As Double
        '                            If vSummands(i).isEqualTo(vSummands(j), mult, "*") Then

        '                                Dim vIsDivA() As Boolean = Nothing
        '                                Dim vFA() As Expression = _
        '                                    vSummands(i).exprToFactors(vIsDivA)
        '                                If bIsMinus(i) <> bIsMinus(j) Then
        '                                    mult *= -1
        '                                End If
        '                                ' if A =vSummands(i) and B=vSummands(j),
        '                                ' expr A is equal to expr B times 1/mult
        '                                ' then A + B = A + A/mult
        '                                ' A+A/mult = A*(1+1/mult)
        '                                Dim mA As Double = 1 + 1 / mult
        '                                If mA = 0 Then
        '                                    vSummands(i) = Nothing
        '                                    vSummands(j) = Nothing
        '                                    Exit For
        '                                Else
        '                                    Dim vF(-1) As Expression
        '                                    If mA = 1 Then
        '                                    ElseIf mA = -1 Then
        '                                    Else
        '                                        ' use 1 factor for
        '                                        ' real multiplicand factor mA
        '                                        ReDim vF(0)
        '                                        vF(0) = New Expression(mA)
        '                                    End If
        '                                    Dim vIsDiv(0) As Boolean
        '                                    For k = 0 To vFA.Length - 1
        '                                        ReDim Preserve vF(vF.Length)
        '                                        ReDim Preserve vIsDiv(vF.Length)
        '                                        vF(vF.Length - 1) = vFA(k).reduceFactors(False)
        '                                        vIsDiv(vF.Length - 1) = vIsDivA(k)
        '                                    Next
        '                                    vSummands(i) = factorsToExpr( _
        '                                        vF, vIsDiv)
        '                                    If mA = -1 Then
        '                                        vSummands(i).opChgSgn() ' 2010/10/17, antes:  vsummands(i).sign *= -1 
        '                                    End If
        '                                    vSummands(j) = Nothing
        '                                End If
        '                            End If
        '                        End If
        '                    End If
        '                Next
        '            End If
        '        End If
        '    Next
        '    Dim posCnst As Int64 = -1 '  Array.IndexOf(vSummands, MathGlobal8.integralCnst)
        '    If posCnst = -1 Then posCnst = vSummands.Length
        '    For j = 0 To vSummands.Length - 1
        '        If vSummands(j) IsNot Nothing Then
        '            vSummands(j) = vSummands(j).reduceFactors(False)
        '            If j < posCnst Then
        '                i = j
        '            ElseIf j < posCnst Then
        '                i = j - 1
        '            Else
        '                i = vSummands.Length - 1
        '            End If
        '            If Not vSummands(i).IsReal OrElse _
        '            vSummands(i).pA.cf(0).pRe.ToDouble <> 0.0 Then
        '                If retExpr Is Nothing Then
        '                    If bIsMinus(i) Then
        '                        retExpr = -vSummands(i)
        '                    Else
        '                        retExpr = vSummands(i)
        '                    End If
        '                Else
        '                    Dim mult As Double
        '                    If retExpr.isEqualTo(vSummands(i), mult) Then
        '                        If mult = 1 Then
        '                            If bIsMinus(i) Then
        '                                retExpr = New Expression(0.0)
        '                            Else
        '                                retExpr = exprOp("*", New Expression(2.0), vSummands(i))
        '                            End If
        '                        ElseIf mult = -1 Then
        '                            If bIsMinus(i) Then
        '                                retExpr = exprOp("*", New Expression(2.0), vSummands(i))
        '                            Else
        '                                retExpr = New Expression(0.0)
        '                            End If
        '                        Else
        '                            If bIsMinus(i) Then
        '                                retExpr = exprOp("-", retExpr, vSummands(i))
        '                            Else
        '                                retExpr = exprOp("+", retExpr, vSummands(i))
        '                            End If
        '                        End If
        '                    Else
        '                        If bIsMinus(i) Then
        '                            retExpr = exprOp("-", retExpr, vSummands(i))
        '                        Else
        '                            retExpr = exprOp("+", retExpr, vSummands(i))
        '                        End If
        '                    End If
        '                End If
        '            End If
        '        End If
        '    Next


        '    If Not polyC.isReal OrElse _
        '            Not polyC.cf(0).esCero Then
        '        If retExpr Is Nothing Then
        '            retExpr = New Expression(polyC)
        '            'ElseIf Array.IndexOf(polyC.var, MathGlobal8.integralCnst, _
        '            '                 0, polyC.var.Length) > -1 Then
        '            '    retExpr = exprOp("+", retExpr, _
        '            '                New Expression( _
        '            '                polyC))
        '        Else
        '            retExpr = exprOp("+", New Expression( _
        '                    polyC), retExpr)
        '        End If
        '    ElseIf retExpr Is Nothing Then
        '        retExpr = New Expression(0.0)
        '    End If
        'Catch ex As Exception
        '    Throw ex
        'End Try
        'Return retExpr
    End Function

    Public Function exprToSummGroupPolynomials(ByRef bIsMinus() As Boolean) As Expression()
        Dim eSumm() As Expression = Nothing
        Try
            eSumm = exprToSummands2(bIsMinus, 0, Nothing, 0)
            Dim eSumm2(-1) As Expression, ie As Int64 = 0
            Dim bIsMinus2(-1) As Boolean

            Dim Pa As Polynomial = Nothing
            For i As Int64 = 0 To eSumm.Length - 1
                If eSumm(i).IsPolynomial Then
                    ' group all polynomial summands in Pa:
                    If ie = 0 Then
                        ReDim Preserve eSumm2(0), bIsMinus2(0)
                        Pa = eSumm(i).getPolynomial
                        bIsMinus2(0) = bIsMinus(0)
                        ie += 1
                    Else
                        If bIsMinus2(0) = bIsMinus(i) Then
                            Pa += eSumm(i).getPolynomial
                        Else
                            Pa -= eSumm(i).getPolynomial
                        End If
                    End If
                    eSumm(i) = Nothing
                End If
            Next
            If ie Then
                eSumm2(0) = New Expression(Pa)
                For i = 0 To eSumm.Length - 1
                    If eSumm(i) IsNot Nothing Then
                        ReDim Preserve eSumm2(ie), bIsMinus2(ie)
                        eSumm2(ie) = eSumm(i)
                        bIsMinus2(ie) = bIsMinus(ie)
                        ie += 1
                    End If
                Next
                eSumm = eSumm2
                bIsMinus = bIsMinus2
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return eSumm
    End Function
    Public Function exprToSummands(ByRef bIsMinus() As Boolean) As Expression()
        Return exprToSummands2(bIsMinus, 0, Nothing, 0)
    End Function
    Private Function exprToSummands2(ByRef bIsMinus() As Boolean,
                             Optional ByRef iv As Int64 = 0,
                           Optional ByRef retvExpr() As Expression = Nothing,
                           Optional ByRef iLevel As Int64 = 0
                           ) As Expression()
        Try
            If Me.IsComplex Then
                ReDim Preserve retvExpr(iv), bIsMinus(iv)
                retvExpr(iv) = New Expression(Me.toComplex)
                iv += 1
                Exit Try
            End If
            iLevel += 1
            If iLevel > 100 Then
                Throw New Exception(msg8.num(13))
            End If
            If pA IsNot Nothing Then
                For i As Int64 = 0 To pA.cf.Length - 1
                    Dim term As New Polynomial(pA.cf(i))
                    ' 2013/08/13, added the 'if' condition
                    ' avoiding zero terms:
                    If Not term.isReal OrElse
                    Not term.cf(0).IsZero Then
                        For j = 0 To pA.exp(i).Length - 1
                            If pA.var.Length > j Then
                                term *= Polynomial.GetPolynomial(pA.var(j)) ^
                                   New Polynomial(pA.exp(i)(j))
                            End If
                        Next
                        ReDim Preserve retvExpr(iv), bIsMinus(iv)
                        retvExpr(iv) = New Expression(term)
                        iv += 1
                    End If
                Next
                If pA.PolyResto IsNot Nothing Then
                    Dim bIsMinusN() As Boolean = Nothing
                    Dim vSumN() As Expression =
                       New Expression(pA.PolyResto).exprToSummands2(bIsMinusN, iLevel:=iLevel)
                    For j As Int64 = 0 To vSumN.Length - 1
                        ReDim Preserve retvExpr(iv), bIsMinus(iv)
                        retvExpr(iv) = New Expression(vSumN(j).pA / pA.PolyDivisor)
                        iv += 1
                    Next
                End If
                Exit Try
            ElseIf mOp IsNot Nothing Then
                Dim i As Int64
                Dim sOp As String = mOp.ToString
                Select Case sOp
                    Case "+"
                        Dim bIsMinusA() As Boolean = Nothing
                        Dim sA As String = Args(0).getMatchStr
                        Dim vSumA(0) As Expression
                        If InStr("-+", sA) Then
                            vSumA = Args(0).exprToSummands2(bIsMinusA, iLevel:=iLevel)
                        Else
                            vSumA(0) = New Expression(Args(0))
                            ReDim bIsMinusA(0)
                        End If
                        i = iv
                        For iv = i To i + vSumA.Length - 1
                            ReDim Preserve retvExpr(iv), bIsMinus(iv)
                            retvExpr(iv) = New Expression(vSumA(iv - i))
                            bIsMinus(iv) = bIsMinusA(iv - i)
                        Next
                        Dim bIsMinusB() As Boolean = Nothing
                        Dim sB As String = Args(1).getMatchStr
                        Dim vSumB(0) As Expression
                        If InStr("-+", sB) Then
                            vSumB = Args(1).exprToSummands2(bIsMinusB, iLevel:=iLevel)
                        Else
                            vSumB(0) = New Expression(Args(1))
                            ReDim bIsMinusB(0)
                        End If
                        i = iv
                        For iv = i To i + vSumB.Length - 1
                            ReDim Preserve retvExpr(iv), bIsMinus(iv)
                            retvExpr(iv) = New Expression(vSumB(iv - i))
                            bIsMinus(iv) = bIsMinusB(iv - i)
                        Next
                        iv -= 1
                        Exit Try
                    Case "-"
                        Dim bIsMinusA() As Boolean = Nothing
                        Dim vSumA(0) As Expression
                        Dim sA As String = Args(0).getMatchStr
                        If InStr("-+", sA) Then
                            vSumA = Args(0).exprToSummands2(bIsMinusA, iLevel:=iLevel)
                        Else
                            vSumA(0) = New Expression(Args(0))
                        End If
                        i = iv
                        For iv = i To i + vSumA.Length - 1
                            ReDim Preserve retvExpr(iv), bIsMinus(iv)
                            retvExpr(iv) = New Expression(vSumA(iv - i))
                            bIsMinus(iv) = bIsMinusA(iv - i)
                        Next
                        If Args.Length = 1 Then
                            For iv = i To i + vSumA.Length - 1
                                bIsMinus(iv) = Not bIsMinusA(i)
                            Next
                            Exit Try
                        Else
                            Dim bIsMinusB() As Boolean = Nothing
                            Dim sB As String = Args(1).getMatchStr
                            Dim vSumB(0) As Expression
                            If InStr("-+", sB) Then
                                vSumB = Args(1).exprToSummands2(bIsMinusB, iLevel:=iLevel)
                            Else
                                vSumB(0) = New Expression(Args(1))
                            End If
                            'For i = 0 To bIsMinus.Length - 1
                            '    bIsMinus(i) = Not bIsMinus(i)
                            'Next
                            i = iv
                            For iv = i To i + vSumB.Length - 1
                                ReDim Preserve retvExpr(iv), bIsMinus(iv)
                                retvExpr(iv) = New Expression(vSumB(iv - i))
                                bIsMinus(iv) = Not bIsMinusB(iv - i)
                            Next
                            iv -= 1
                        End If
                End Select
            End If
            ReDim Preserve retvExpr(iv), bIsMinus(iv)
            'Dim meSgn As Int64 = Me.sign
            'Me.sign = 1
            If iv = 0 AndAlso retvExpr(0) Is Nothing Then
                retvExpr(iv) = New Expression(Me)
            End If
            'retvExpr(iv) = New Expression(Me)
            'If meSgn = -1 Then
            'bIsMinus(iv) = True
            'End If
            iv += 1
        Catch ex As Exception
            Throw ex
        End Try
        Try
            iLevel -= 1
            If iLevel = 0 Then ' AndAlso retvExpr.Length > 1 Then
                If retvExpr Is Nothing Then
                    ' 2013/08/13:
                    retvExpr = New Expression() {New Expression(0.0)}
                    bIsMinus = New Boolean() {False}
                    iv = 1
                ElseIf retvExpr.Length > 1 Then
                    ' on exit (ilevel=0) 
                    ' shift all polynomial terms
                    ' to element 0:
                    Dim i0(bIsMinus.Length - 1) As Int64
                    Dim i1(i0.Length - 1) As Int64
                    Dim retvExprNoZ(-1) As Expression
                    Dim i0NoZ(-1), i1NoZ(-1) As Int64
                    Dim k As Int64 = 0
                    For i = 0 To i0.Length - 1
                        'If sgn = -1 Then
                        'bIsMinus(i) = Not bIsMinus(i)
                        'End If
                        If retvExpr(i).sign = -1 Then
                            retvExpr(i).sign = 1
                            bIsMinus(i) = Not bIsMinus(i) ' antes = true (2010/10/12)
                        End If
                        If Not bIsMinus(i) Then
                            i0(i) = i ' antes -i 2020/10/21
                            If retvExpr(i).pA IsNot Nothing Then
                                i0(i) -= 1000
                            End If
                        Else
                            i0(i) = i
                            If retvExpr(i).pA Is Nothing Then
                                i0(i) += 1000
                            Else
                            End If
                        End If
                        i1(i) = i0(i)
                        If retvExpr(i).pA IsNot Nothing AndAlso
                        retvExpr(i).pA.isReal AndAlso
                        retvExpr(i).pA.ToDouble = 0.0 AndAlso k Then
                            ' supress extra zero elements 
                        Else
                            ReDim Preserve retvExprNoZ(k), i0NoZ(k), i1NoZ(k)
                            retvExprNoZ(k) = retvExpr(i)
                            i0NoZ(k) = i0(i)
                            i1NoZ(k) = i1(i)
                            k += 1
                        End If
                    Next
                    retvExpr = retvExprNoZ
                    i0 = i0NoZ
                    i1 = i1NoZ
                    Array.Sort(i0, retvExpr)
                    Array.Sort(i1, bIsMinus)
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return retvExpr

    End Function



    Public Function reduceFactors( _
                   ChgSgn As Boolean) As Expression
        If ChgSgn Then
            Return -Me
        End If
        Return New Expression(Me)
        'If bIsReducing Then
        '    If ChgSgn Then
        '        Return -Me
        '    End If
        '    Return Me
        'End If
        'bIsReducing = True
        'Dim ret As Expression = Me.ReduceUsingPolynomials
        'bIsReducing = False
        'If ChgSgn Then
        '    Return -ret
        'End If
        'Return ret
        'Dim eRet As Expression = Nothing
        'Try
        '    If Me.IsPolynomial Then
        '        eRet = New Expression(Me.getPolynomial)
        '        If Me.sign = -1 Then
        '            eRet.pA *= New Polynomial(-1.0)
        '            eRet.sign = 1
        '        End If
        '        If ChgSgn Then
        '            eRet.sign *= -1
        '        End If
        '        Exit Try
        '    End If
        '    Dim bIsDiv(-1) As Boolean
        '    Dim vFactors() As Expression = _
        '        exprToFactors(bIsDiv)

        '    If vFactors.Length > 100 Then
        '        Throw New Exception(msg8.num(13)) ' n/a 
        '    End If
        '    If vFactors.Length = 1 Then
        '        'Dim bIsMinus() As Boolean = Nothing
        '        'Dim vSum() As Expression = _
        '        '    vFactors(0).exprToSummands(bIsMinus)
        '        'If vSum.Length > 1 Then
        '        '    For i1 As Int64 = 0 To vSum.Length - 1
        '        '        If bIsMinus(i1) Then
        '        '            vSum(i1) = -vSum(i1)
        '        '            bIsMinus(i1) = False
        '        '        End If
        '        '        vSum(i1) = vSum(i1).reduceFactors
        '        '    Next
        '        '    eRet = summandsToExpr(vSum, bIsMinus)
        '        '    Exit Try
        '        'End If
        '        If Not bIsDiv(0) Then
        '            eRet = New Expression(vFactors(0))
        '        Else
        '            eRet = New Expression(Me)
        '        End If
        '        If ChgSgn Then
        '            eRet.sign *= -1
        '        End If
        '        Exit Try
        '    End If

        '    Dim mult As Double = 1.0
        '    If ChgSgn Then mult *= -1

        '    Dim pA As New Polynomial(1.0)
        '    Dim vFac(vFactors.Length - 1) As Expression
        '    Dim vExp(vFactors.Length - 1) As Expression
        '    Dim iv As Int64 = 0
        '    Dim i, j As Int64
        '    For i = 0 To vFactors.Length - 1
        '        If vFactors(i).IsReal Then
        '            If bIsDiv(i) Then
        '                mult /= vFactors(i).toDouble
        '            Else
        '                mult *= vFactors(i).toDouble
        '                If mult = 0.0 Then
        '                    Return New Expression(0.0)
        '                End If
        '            End If
        '        ElseIf vFactors(i).IsPolynomial AndAlso _
        '        vFactors(i).pA.cf.Length = 1 AndAlso _
        '        vFactors(i).pA.cf(0).pIm.ToDouble = 0.0 AndAlso _
        '        vFactors(i).pA.PolyResto Is Nothing Then
        '            If bIsDiv(i) Then
        '                pA /= vFactors(i).pA
        '            Else
        '                pA *= vFactors(i).pA
        '            End If
        '            'If vFactors(i).sign = -1 AndAlso _
        '            'Me.Args.Length AndAlso _
        '            'Not vFactors(i).isEqualTo(Args(0)) Then
        '            '    pA *= New Polynomial(-1.0)
        '            'End If
        '            ' (2013/06/28) lo anterior sustituido por:
        '            If vFactors(i).sign = -1 Then
        '                pA *= New Polynomial(-1.0)
        '            End If
        '        Else
        '            If Not bIsDiv(i) Then
        '                vExp(iv) = New Expression(1.0)
        '            Else
        '                vExp(iv) = New Expression(-1.0)
        '            End If
        '            Dim arg0 As Expression = Nothing
        '            Dim exp0 As Expression = Nothing
        '            If vFactors(i).mOp IsNot Nothing AndAlso _
        '            vFactors(i).mOp.ToString = "^" Then
        '                arg0 = vFactors(i).Args(0)
        '                exp0 = vFactors(i).Args(1)
        '                If Not bIsDiv(i) Then
        '                    vExp(iv) = exp0
        '                Else
        '                    vExp(iv) = -exp0
        '                End If
        '            End If
        '            For j = 0 To iv - 1
        '                If vFactors(i).isEqualTo(vFac(j)) Then
        '                    If vFactors(i).mOp IsNot Nothing AndAlso _
        '                    vFactors(i).mOp.ToString = "^" Then
        '                        If Not bIsDiv(i) Then
        '                            vExp(j) += vFac(j).Args(1)
        '                        Else
        '                            vExp(j) -= vFac(j).Args(1)
        '                        End If
        '                    Else
        '                        If Not bIsDiv(i) Then
        '                            vExp(j) += New Expression(1.0)
        '                        Else
        '                            vExp(j) += New Expression(-1.0)
        '                        End If
        '                    End If
        '                    Exit For
        '                ElseIf arg0 IsNot Nothing AndAlso _
        '                vFac(j).mOp IsNot Nothing AndAlso _
        '                vFac(j).mOp.ToString = "^" Then
        '                    If arg0.isEqualTo(vFac(j).Args(0)) Then
        '                        If bIsDiv(i) Then
        '                            vExp(j) -= exp0
        '                        Else
        '                            vExp(j) += exp0
        '                        End If
        '                        Exit For
        '                    End If
        '                End If
        '            Next
        '            If j > iv - 1 Then
        '                ReDim Preserve vFac(iv)
        '                vFac(iv) = New Expression(vFactors(i))
        '                If vFac(iv).sign = -1 Then
        '                    mult *= -1
        '                    'vFac(iv) *= New Expression(-1.0) 2010/10/17, comentada y substituída por:
        '                    vFac(iv) = vFac(iv).opChgSgn
        '                End If
        '                iv += 1
        '            End If
        '        End If
        '    Next
        '    ' go through the exponents and set aside
        '    ' the negative exponents:
        '    Dim negativeExp As Expression = Nothing
        '    For i = 0 To iv - 1
        '        'If mult < 0.0 AndAlso vFac(i).IsPolynomial _
        '        'AndAlso vFac(i).pA.cf.Length = 1 Then
        '        '    vFac(i).pA *= New Polynomial(mult)
        '        '    mult = 1.0
        '        'End If
        '        'vFac(i) = vFac(i).reduceSummands
        '        If vFac(i).mOp IsNot Nothing AndAlso _
        '        vFac(i).mOp.ToString = "^" Then
        '            vFac(i) = vFac(i).Args(0)
        '        End If
        '        If vExp(i).IsReal Then
        '            Dim exp As Double = vExp(i).toDouble
        '            If exp = 1.0 Then
        '                If eRet Is Nothing Then
        '                    eRet = vFac(i)
        '                Else
        '                    eRet = exprOp("*", eRet, vFac(i))
        '                End If
        '            ElseIf exp = 0.0 Then
        '            ElseIf exp > 0.0 Then
        '                If eRet Is Nothing Then
        '                    eRet = exprOp("^", vFac(i), vExp(i))
        '                Else
        '                    eRet = exprOp("*", eRet, _
        '                                exprOp("^", vFac(i), vExp(i)))
        '                End If
        '            Else 'If exp < 0 Then
        '                If exp = -1.0 Then
        '                    If negativeExp Is Nothing Then
        '                        negativeExp = vFac(i)
        '                    Else
        '                        negativeExp = exprOp("*", negativeExp, vFac(i))
        '                    End If
        '                Else
        '                    If negativeExp Is Nothing Then
        '                        negativeExp = exprOp("^", vFac(i), -vExp(i))
        '                    Else
        '                        negativeExp = exprOp("*", negativeExp, _
        '                                    exprOp("^", vFac(i), -vExp(i)))
        '                    End If
        '                End If
        '            End If
        '        Else ' non-real exponents:
        '            If eRet Is Nothing Then
        '                eRet = exprOp("^", vFac(i), vExp(i))
        '            Else
        '                eRet = exprOp("*", eRet, _
        '                            exprOp("^", vFac(i), vExp(i)))
        '            End If
        '        End If
        '    Next
        '    If eRet Is Nothing Then
        '        eRet = New Expression(1.0)
        '    End If
        '    If negativeExp IsNot Nothing Then
        '        ' incorporate the negative exponents into eRet:
        '        eRet = exprOp("/", eRet, negativeExp)
        '    End If
        '    pA *= New Polynomial(mult)
        '    Dim pAMinusOne As Polynomial = pA - _
        '        New Polynomial(1.0)
        '    If pAMinusOne.isReal AndAlso pAMinusOne.cf(0).pRe.IsZero Then
        '    Else
        '        Dim pPlusOne As Polynomial = pA + _
        '            New Polynomial(1.0)
        '        If pPlusOne.isReal AndAlso _
        '                    pPlusOne.cf(0).pRe.IsZero Then
        '            eRet.sign = -1
        '        Else
        '            Dim sgn As Int64 = eRet.sign
        '            If pA.cf.Length = 1 AndAlso _
        '            pA.PolyResto Is Nothing Then
        '                Dim db As Double = _
        '                    pA.cf(0).pRe.ToDouble
        '                If db < 0 Then
        '                    db = -db
        '                    pA.cf(0) *= New Complex(-1.0)
        '                    sgn *= -1
        '                End If
        '            End If
        '            eRet = exprOp("*", New Expression(pA), _
        '                            eRet)
        '            eRet.sign = sgn
        '        End If
        '    End If
        'Catch ex As Exception
        '    Throw ex
        'End Try
        ''Dim e1 As String = Me.ToString
        ''Dim e2 As String = eRet.ToStringExpr
        ''If e1.Chars(0) <> e2.Chars(0) AndAlso _
        ''(e1.Chars(0) = "-" OrElse e2.Chars(0) = "-") Then
        ''    Trace.WriteLine(e1 + " <> " + e2)
        ''End If
        'Return eRet
    End Function
    Public Function splitIntoTerms() As Expression()
        Dim vExpr() As Expression = Nothing
        Try
            If Not Me.IsPolynomial Then
                Dim vIsMn() As Boolean = Nothing
                vExpr = Me.exprToSummands(vIsMn)
                For i = 0 To vExpr.Length - 1
                    If vIsMn(i) Then
                        vExpr(i) = -vExpr(i)
                    End If
                Next
                Dim vExpr2(-1) As Expression, i2 As Int64
                For i = 0 To vExpr.Length - 1
                    If vExpr(i).IsPolynomial Then
                        Dim vP() As Polynomial = vExpr(i).getPolynomial.splitIntoTerms
                        For j = 0 To vP.Length - 1
                            ReDim Preserve vExpr2(i2)
                            vExpr2(i2) = New Expression(vP(j))
                            i2 += 1
                        Next
                    Else
                        ReDim Preserve vExpr2(i2)
                        vExpr2(i2) = vExpr(i)
                        i2 += 1
                    End If
                Next
                If vExpr2.Length <> vExpr.Length Then
                    vExpr = vExpr2
                End If
            Else
                Dim Pa As Polynomial = getPolynomial
                Dim vP() As Polynomial = Pa.splitIntoTerms
                ReDim vExpr(vP.Length - 1)
                For i As Int64 = 0 To vExpr.Length - 1
                    vExpr(i) = New Expression(vP(i))
                Next
                If Pa.PolyResto IsNot Nothing Then
                    If vExpr.Length = 1 AndAlso vExpr(0).IsReal AndAlso _
                    vExpr(0).toDouble = 0.0 Then
                        vExpr(0) = New Expression(Pa.PolyResto / Pa.PolyDivisor)
                    Else
                        ReDim Preserve vExpr(vExpr.Length)
                        vExpr(vExpr.Length - 1) = New Expression(Pa.PolyResto / Pa.PolyDivisor)
                    End If
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return vExpr
    End Function

    Public Shared Function summandsToExpr(vSummands() As Expression, bIsMinus() As Boolean) As Expression
        Dim retExpr As Expression = Nothing
        Try
            Dim i As Int64
            Dim zero As New Expression(0.0)
            Dim polyC As New Polynomial(0.0)
            For i = 0 To vSummands.Length - 1
                If vSummands(i) IsNot Nothing Then
                    If vSummands(i).IsPolynomial Then
                        If Not bIsMinus(i) Then
                            polyC += vSummands(i).getPolynomial
                        Else
                            polyC -= vSummands(i).getPolynomial
                        End If
                    Else
                        If bIsMinus(i) Then
                            vSummands(i).sign *= -1
                            'vSummands(i) = -vSummands(i)
                        End If
                        If retExpr Is Nothing Then
                            retExpr = New Expression(vSummands(i))
                        ElseIf vSummands(i).isEqualTo(zero) Then
                        ElseIf retExpr.isEqualTo(zero) Then
                            retExpr = vSummands(i)
                        Else
                            retExpr = _
                                exprOp("+", retExpr, vSummands(i))
                        End If
                    End If
                End If
            Next
            If polyC IsNot Nothing Then
                If retExpr Is Nothing Then
                    retExpr = New Expression(polyC)
                ElseIf polyC.isReal Then
                    If polyC.cf(0).pRe.ToDouble = 0.0 Then
                    Else
                        retExpr = exprOp("+", _
                            New Expression(polyC), retExpr)
                    End If
                Else
                    retExpr = exprOp("+", _
                        New Expression(polyC), retExpr)
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return retExpr
    End Function
    Public Shared Function factorsToExpr( _
                        vFactors() As Expression, _
                        bIsDiv() As Boolean) As Expression
        Dim retExpr As Expression = Nothing
        Dim sgn As Int64 = 1
        Try
            Dim i As Int64
            Dim one As New Expression(1.0)
            Dim mnone As New Expression(-1.0)
            Dim den As Expression = Nothing
            Dim polyNum As New Polynomial(1.0)
            For i = 0 To vFactors.Length - 1
                If vFactors(i) IsNot Nothing Then
                    sgn *= vFactors(i).sign
                    vFactors(i).sign = 1 ' added 2013/10/18
                    If vFactors(i).IsPolynomial Then
                        If vFactors(i).pA Is Nothing Then
                            vFactors(i).pA = vFactors(i).getPolynomial
                        End If
                        If Not bIsDiv(i) Then
                            polyNum *= vFactors(i).pA
                        Else
                            polyNum /= vFactors(i).pA
                        End If
                    ElseIf bIsDiv(i) Then
                        If den Is Nothing OrElse _
                        den.isEqualTo(one) Then
                            den = vFactors(i)
                        ElseIf den.isEqualTo(mnone) Then
                            den = -vFactors(i)
                        ElseIf vFactors(i).isEqualTo(mnone) Then
                            den = -den
                        ElseIf Not vFactors(i).isEqualTo(one) Then
                            den = _
                                exprOp("*", den, vFactors(i))
                        End If
                    ElseIf retExpr Is Nothing Then
                        retExpr = vFactors(i)
                    ElseIf vFactors(i).isEqualTo(one) Then
                    ElseIf vFactors(i).isEqualTo(-one) Then
                        retExpr.sign *= -1
                    ElseIf retExpr.isEqualTo(one) Then
                        retExpr = vFactors(i)
                    ElseIf retExpr.isEqualTo(mnone) Then
                        retExpr = -vFactors(i)
                    Else
                        retExpr = _
                            exprOp("*", retExpr, vFactors(i))
                    End If
                End If
            Next
            If polyNum IsNot Nothing Then
                If retExpr Is Nothing Then
                    retExpr = New Expression(polyNum)
                ElseIf polyNum.isReal Then
                    If polyNum.cf(0).pRe.ToDouble = 1.0 Then
                    ElseIf polyNum.cf(0).pRe.ToDouble = -1.0 Then
                        retExpr = -retExpr
                    Else
                        retExpr = exprOp("*", _
                            New Expression(polyNum), retExpr)
                    End If
                Else
                    retExpr = exprOp("*", _
                        New Expression(polyNum), retExpr)
                End If
            End If
            If den IsNot Nothing Then
                If retExpr Is Nothing Then
                    If den.isEqualTo(one) Then
                        retExpr = one
                    ElseIf den.isEqualTo(mnone) Then
                        retExpr = mnone
                    Else
                        retExpr = exprOp("/", one, den)
                    End If
                ElseIf retExpr.IsPolynomial AndAlso _
                den.IsPolynomial Then
                    retExpr = New Expression(retExpr.pA / den.pA)
                ElseIf den.isEqualTo(one) Then
                ElseIf den.isEqualTo(mnone) Then
                    retExpr = -retExpr
                Else
                    retExpr = exprOp("/", retExpr, den)
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        If sgn = -1 Then
            retExpr.sign *= -1
        End If
        Return retExpr
    End Function
    Public Function exprToFactors( _
             ByRef bIsDiv() As Boolean, Optional splitPolynomials As Boolean = False) _
            As Expression()
        Dim vFact() As Expression = exprToFactors2(bIsDiv, 0, Nothing, 0)
        If Not splitPolynomials Then
            Return vFact
        End If
        Dim i As Int64
        Dim vFact2(-1) As Expression, iv As Int64
        Dim bIsDiv2(-1) As Boolean
        Try
            For i = 0 To vFact.Length - 1
                If Not vFact(i).IsPolynomial Then
                    Dim bDone As Boolean = False
                    If vFact(i).getMatchStr = "exp" Then
                        Dim vTerm() As Expression = vFact(i).Args(0).splitIntoTerms
                        If vTerm.Length > 1 Then
                            For i1 As Int64 = 0 To vTerm.Length - 1
                                ReDim Preserve vFact2(iv), bIsDiv2(iv)
                                vFact2(iv) = Expression.AddFnAndArg0("exp", vTerm(i1))
                                If vFact(i).sign = -1 Then
                                    vFact2(0) = vFact2(0).opChgSgn
                                    vFact(i) = vFact(i).opChgSgn
                                End If
                                bIsDiv2(iv) = bIsDiv(i)
                                iv += 1
                            Next
                            bDone = True
                        End If
                    End If
                    If Not bDone Then
                        ReDim Preserve vFact2(iv), bIsDiv2(iv)
                        vFact2(iv) = vFact(i)
                        bIsDiv2(iv) = bIsDiv(i)
                        iv += 1
                    End If
                Else
                    Dim vVar(-1) As String
                    Dim Pa As Polynomial = vFact(i).getPolynomial
                    Dim Pb As New Polynomial(Pa)
                    Pb.PolyResto = Nothing
                    Pb.PolyDivisor = Nothing
                    If Pb.isReal AndAlso Pb.ToDouble = 0.0 AndAlso _
                    Pb.PolyResto IsNot Nothing Then
                        Dim exprNum As New Expression(Pa.PolyResto)
                        Dim exprDen As New Expression(Pa.PolyDivisor)
                        Dim vNumIsDiv(-1) As Boolean
                        Dim vNumFact() As Expression = exprNum.exprToFactors(vNumIsDiv, True)
                        Dim vDenIsDiv(-1) As Boolean
                        Dim vDenFact() As Expression = exprDen.exprToFactors(vDenIsDiv, True)
                        For j = 0 To vNumFact.Length - 1
                            ReDim Preserve vFact2(iv), bIsDiv2(iv)
                            vFact2(iv) = vNumFact(j)
                            bIsDiv2(iv) = vNumIsDiv(j)
                            iv += 1
                        Next
                        For j = 0 To vDenFact.Length - 1
                            ReDim Preserve vFact2(iv), bIsDiv2(iv)
                            vFact2(iv) = vDenFact(j)
                            bIsDiv2(iv) = Not vDenIsDiv(j)
                            iv += 1
                        Next
                    Else
                        vVar = Pa.varAll
                        If vVar.Length < 2 Then
                            ReDim Preserve vFact2(iv), bIsDiv2(iv)
                            vFact2(iv) = vFact(i)
                            bIsDiv2(iv) = bIsDiv(i)
                            iv += 1
                        Else
                            Dim vFact3(-1) As Expression, bIsDiv3(-1) As Boolean
                            Pa.split_1OnlyTermToExpr(vFact3, bIsDiv3)
                            If vFact3.Length = 0 Then
                                ReDim Preserve vFact2(iv), bIsDiv2(iv)
                                vFact2(iv) = vFact(i)
                                bIsDiv2(iv) = bIsDiv(i)
                                iv += 1
                            Else
                                ReDim Preserve vFact2(iv + vFact3.Length - 1), bIsDiv2(iv + bIsDiv3.Length - 1)
                                For j As Int64 = iv To vFact2.Length - 1
                                    If bIsDiv3(j - iv) Then
                                        vFact2(j) = New Expression(1.0) / vFact3(j - iv)
                                    Else
                                        vFact2(j) = vFact3(j - iv)
                                    End If
                                Next
                                iv = vFact2.Length
                            End If
                        End If
                    End If
                End If
            Next
            bIsDiv = bIsDiv2
        Catch ex As Exception
            Throw ex
        End Try
        Return vFact2
    End Function
    Public Function exprToFactors2(ByRef bIsDiv() As Boolean, _
                                ByRef iv As Int64, _
                                ByRef retvExpr() As Expression, _
                                ByRef iLevel As Int64 _
                                ) As Expression()
        Try
            iLevel += 1
            If pA IsNot Nothing Then
                ReDim Preserve retvExpr(iv), bIsDiv(iv)
                retvExpr(iv) = New Expression(Me)
                iv += 1
                Exit Try
            ElseIf mOp IsNot Nothing Then
                Dim i As Int64
                Dim sOp As String = mOp.ToString
                Select Case sOp
                    Case "-"
                        'If Args.Length = 1 Then
                        '    CopyExprToMe(Args(0))
                        'End If
                    Case "*", "/"
                        Dim bIsDivA() As Boolean = Nothing
                        Dim vFacA() As Expression = _
                            Args(0).exprToFactors(bIsDivA)
                        i = iv
                        For iv = i To i + vFacA.Length - 1
                            ReDim Preserve retvExpr(iv), bIsDiv(iv)
                            retvExpr(iv) = New Expression(vFacA(iv - i))
                            bIsDiv(iv) = bIsDivA(iv - i)
                        Next
                        Dim bIsDivB() As Boolean = Nothing
                        Dim vFacB() As Expression = _
                            Args(1).exprToFactors(bIsDivB)
                        i = iv
                        For iv = i To i + vFacB.Length - 1
                            ReDim Preserve retvExpr(iv), bIsDiv(iv)
                            retvExpr(iv) = New Expression(vFacB(iv - i))
                            If sOp = "/" Then
                                bIsDiv(iv) = Not bIsDivB(iv - i)
                            Else
                                bIsDiv(iv) = bIsDivB(iv - i)
                            End If
                        Next
                        Exit Try
                    Case "^"
                        If Args(1).pA IsNot Nothing AndAlso _
                        Args(1).pA.isReal Then
                            Dim db As Double = Args(1).pA.cf(0).pRe.ToDouble
                            If db >= 0 AndAlso db = Math.Floor(db) Then
                                If db = 0.0 Then
                                    ReDim Preserve retvExpr(iv), bIsDiv(iv)
                                    retvExpr(iv) = New Expression(1.0) ' x^0 = 1
                                    If iv Then
                                        bIsDiv(iv) = bIsDiv(iv - i)
                                    End If
                                    If Me.sign = -1 Then
                                        retvExpr(iv) = retvExpr(iv).opChgSgn
                                    End If
                                    Exit Try
                                ElseIf db = 1.0 Then
                                    If Me.Args(0).IsReal AndAlso _
                                    Me.Args(0).toDouble = 0.0 Then
                                        ReDim Preserve retvExpr(iv), bIsDiv(iv)
                                        retvExpr(iv) = New Expression(1.0) ' 0^0 = 1
                                        If iv Then
                                            bIsDiv(iv) = bIsDiv(iv - i)
                                        End If
                                    Else
                                        ReDim Preserve retvExpr(iv), bIsDiv(iv)
                                        retvExpr(iv) = Args(0)
                                        If iv Then
                                            bIsDiv(iv) = bIsDiv(iv - i)
                                        End If
                                    End If
                                    If Me.sign = -1 Then
                                        retvExpr(iv) = retvExpr(iv).opChgSgn
                                    End If
                                    Exit Try
                                End If
                                ' integer exponent:
                                Dim exp As Int64 = db
                                Dim bIsDivA() As Boolean = Nothing
                                Dim vFacA() As Expression = _
                                       Args(0).exprToFactors(bIsDivA)
                                i = iv
                                Dim i2 As Int64 = i
                                For k As Int64 = 1 To exp
                                    For iv = i To i + vFacA.Length - 1
                                        ReDim Preserve retvExpr(iv), bIsDiv(iv)
                                        retvExpr(iv) = New Expression(vFacA(iv - i))
                                        bIsDiv(iv) = bIsDivA(iv - i)
                                    Next
                                    i = iv
                                Next
                                If Me.sign = -1 Then
                                    'retvExpr(iv) = retvExpr(iv).opChgSgn
                                    retvExpr(i2) = retvExpr(i2).opChgSgn
                                End If
                                Exit Try
                            Else
                                Dim exp As Int64 = db
                                Dim bIsDivA() As Boolean = Nothing
                                Dim vFacA() As Expression = _
                                    Args(0).exprToFactors(bIsDivA)
                                For iv = 0 To vFacA.Length - 1
                                    ReDim Preserve retvExpr(iv), bIsDiv(iv)
                                    retvExpr(iv) = exprOp("^", vFacA(iv - i), Args(1))
                                    bIsDiv(iv) = bIsDivA(iv - i)
                                Next
                                If Me.sign = -1 Then
                                    retvExpr(0) = retvExpr(0).opChgSgn
                                End If
                                Exit Try
                            End If
                        End If
                End Select
            End If
            ReDim Preserve retvExpr(iv), bIsDiv(iv)
            retvExpr(iv) = New Expression(Me)
            iv += 1
        Catch ex As Exception
            Throw ex
        End Try
        Try
            If iv > 40 Then
                Throw New Exception(msg8.num(13)) ' n/a 
            End If
            iLevel -= 1
            If iLevel = 0 AndAlso retvExpr.Length > 1 Then
                ' on exit (ilevel=0) 
                ' shift all polynomial factors
                ' to element 0:
                Dim i0(bIsDiv.Length - 1) As Int64
                Dim i1(i0.Length - 1) As Int64
                For i = 0 To i0.Length - 1
                    If Not bIsDiv(i) Then
                        i0(i) = -i
                        If retvExpr(i).pA IsNot Nothing Then
                            i0(i) -= 100
                        End If
                    Else
                        i0(i) = i
                        If retvExpr(i).pA Is Nothing Then
                            i0(i) += 100
                        End If
                    End If
                    i1(i) = i0(i)
                Next
                Array.Sort(i0, retvExpr)
                Array.Sort(i1, bIsDiv)
                Dim j As Int64 = 1
                Dim bTrat As Boolean = False
                For i = 0 To retvExpr.Length - 2
                    Do While j < retvExpr.Length AndAlso _
                        retvExpr(j) Is Nothing
                        j += 1
                    Loop
                    If retvExpr(i) IsNot Nothing AndAlso _
                    retvExpr(i).IsPolynomial AndAlso _
                    j < retvExpr.Length AndAlso _
                    retvExpr(j).IsPolynomial AndAlso _
                    Not bIsDiv(i) AndAlso Not bIsDiv(j) Then
                        retvExpr(i).pA *= retvExpr(j).pA
                        retvExpr(j) = Nothing
                        j += 1
                        bTrat = True
                    Else
                        Exit For
                    End If
                Next
                If bTrat Then
                    j = 0
                    Dim ret(-1) As Expression
                    Dim bDiv(-1) As Boolean
                    For i = 0 To retvExpr.Length - 1
                        If retvExpr(i) IsNot Nothing Then
                            ReDim Preserve ret(j), bDiv(j)
                            ret(j) = retvExpr(i)
                            bDiv(j) = bIsDiv(i)
                            retvExpr(i).cfg = cfg
                            j += 1
                        End If
                    Next
                    bIsDiv = bDiv
                    retvExpr = ret
                End If
                retvExpr(0).sign *= Me.sign
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return retvExpr
    End Function


    Public Function isEqualTo(eB As Expression, _
                      ByRef Mult As Double, _
                      operation As String) As Boolean
        Static enUsoIsEq As Boolean = False
        Dim bRet As Boolean = False
        Try
            If Me.IsPolynomial <> eB.IsPolynomial Then
                Return False
            ElseIf Me.IsPolynomial Then
                Dim cMult As Complex = Nothing
                If Me.getPolynomial.IsEqual(eB.getPolynomial, cMult) Then
                    If cMult.IsReal Then
                        Mult = 1.0 / cMult.pRe.ToDouble
                        Return True
                    End If
                End If
                Return False
            End If
            Dim cjoMult As Complex = Nothing
            If isEqualTo(eB, cjoMult) Then
                If cjoMult.IsReal Then
                    Mult = 1.0 / cjoMult.pRe.ToDouble
                    Return True
                End If
            End If
            'Dim div As Expression = (Me / eB) '.reduceFactors(False)
            'If div.IsPolynomial Then
            '    If div.pA.isReal Then
            '        Mult = div.toDouble
            '        bRet = True
            '        Exit Try
            '    End If
            'End If
        Catch ex As Exception
            Throw ex
        End Try
        Return bRet
    End Function
    Public Function isEqualTo(ByVal eB As Expression, ByRef Mult As Double) As Boolean
        Try
            Dim cjoMult As Complex = Nothing
            If isEqualTo(eB, cjoMult) Then
                If cjoMult.IsReal Then
                    Mult = cjoMult.pRe.ToDouble
                    Return True
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return False
    End Function
    Public Function isEqualTo(ByRef eB As Expression, Optional ByRef cjoMult As Complex = Nothing) As Boolean
        'Static bTrace As Boolean = True
        Dim bRet As Boolean = False

        Try

            If Me.IsPolynomial <> eB.IsPolynomial Then
                Exit Try
            End If


            If Me.IsPolynomial Then
                Dim cMult As Complex = Nothing
                If Me.getPolynomial.IsEqual(eB.getPolynomial, cMult) Then
                    cjoMult = cMult
                    bRet = True
                End If
                Exit Try
            End If
            'bRet = IsEqualToSummands(eB, cjoMult)
            'If bRet Then
            '    cjoMult = 1 / cjoMult
            'ElseIf Me.mOp IsNot Nothing AndAlso _
            'Me.mOp.ToString = "^" AndAlso _
            'eB.mOp IsNot Nothing AndAlso _
            'eB.mOp.ToString = "^" Then
            '    If Args(0).isEqualTo(eB.Args(0), cjoMult) Then
            '        Dim cjoMult2 As Complex = Nothing
            '        If Args(1).IsEqualToSummands(eB.Args(1), cjoMult2) AndAlso _
            '        cjoMult2.IsReal AndAlso cjoMult2.pRe.ToDouble = 1.0 Then
            '            cjoMult = 1 / cjoMult
            '            bRet = True
            '        End If
            '    End If
            'End If


            If mOp.Groups("fn").Success Then
                Dim b0 As New Expression(eB)
                Dim m As New Complex(1.0)
                Dim vIsDivB() As Boolean = Nothing
                Dim vFactB() As Expression = b0.exprToFactors(vIsDivB)
                If vFactB.Length = 2 Then
                    If eB.getMatchStr = "*" Then
                        ' m * fn ?
                        If b0.Args(0).IsComplex Then
                            m /= eB.Args(0).toComplex
                            b0 = eB.Args(1)
                        ElseIf b0.Args(1).IsComplex Then
                            m /= eB.Args(1).toComplex
                            b0 = eB.Args(0)
                        Else
                            Exit Try
                        End If
                    ElseIf eB.getMatchStr = "/" Then
                        If b0.Args(1).IsComplex Then
                            m *= eB.Args(1).toComplex
                            b0 = eB.Args(0)
                        Else
                            Exit Try
                        End If
                    Else
                        Exit Try
                    End If
                End If
                If mOp.ToString <> b0.getMatchStr Then
                    Exit Try
                End If
                Dim sgn As Int64 = Me.sign * eB.sign
                bRet = Args(0).isEqualTo(b0.Args(0), cjoMult)
                If bRet AndAlso (cjoMult - Complex.one).IsZero Then
                    cjoMult = sgn * m
                Else
                    bRet = False
                End If
                'Trace.Write(Me.ToString + "   " + eB.ToString + "  " + bRet.ToString)
                'If bRet Then
                '    Trace.WriteLine(" " + cjoMult.toString)
                'Else
                '    Trace.WriteLine("")
                'End If
            ElseIf Me.Args.Length <> eB.Args.Length Then
                bRet = False
            ElseIf Me.Args.Length = 1 Then
                ' unary minus:
                Dim sgn As Int64 = Me.sign * eB.sign
                Dim cjo As Complex = Nothing
                If Args(0).isEqualTo(eB.Args(0), cjo) Then
                    If cjoMult Is Nothing Then
                        cjoMult = Complex.one
                    End If
                    cjoMult *= sgn
                    bRet = True
                End If
            Else
                Dim cjo1 As Complex = Nothing
                Dim cjo2 As Complex = Nothing
                Dim a0 As New Expression(Args(0))
                Dim a1 As New Expression(Args(1))
                Dim b0 As New Expression(eB.Args(0))
                Dim b1 As New Expression(eB.Args(1))
                Dim sOpA As String = getMatchStr()
                Dim sOpB As String = eB.getMatchStr
                If sOpA = "+" Then
                    If sOpB = "-" Then
                        ' a0 + a1  ~ b0 - b1
                        ' a0 + a1  ~ b0 + (-b1)
                        b1 = b1.opChgSgn
                    ElseIf sOpB <> "+" Then
                        Exit Try
                    End If
                ElseIf sOpA = "-" Then
                    If sOpB = "+" Then
                        ' a0 - a1   ~ b0 + b1
                        ' a0+(-a1)  ~ b0 + b1
                        a1 = a1.opChgSgn
                        sOpA = "+"
                    ElseIf sOpB <> "+" Then
                        Exit Try
                    End If
                ElseIf sOpA = "*" Then
                    If sOpB = "/" Then
                        ' a0 * a1 ~ b0 / b1
                        ' = a0 * a1 ~ b0 * (1/b1)
                        If b1.getMatchStr = "/" Then
                            Dim aux As Expression = b0
                            b0 = b1 : b1 = aux
                        ElseIf b1.getMatchStr = "^" AndAlso _
                        b1.Args(1).IsReal Then
                            Dim exp As Double = b1.Args(1).toDouble
                            If exp = -1 Then
                                ' b1^-1 = 1/b1
                                b1 = b1.Args(0)
                            Else
                                b1.Args(1) = New Expression(-exp)
                            End If
                        Else
                            b1 = New Expression(1.0) / b1
                        End If
                    ElseIf sOpB <> "*" Then
                        Exit Try
                    End If
                ElseIf sOpA = "/" Then
                    If sOpB = "*" Then
                        ' a0 / a1 ~ b0 * b1
                        ' a0 / a1 ~ b0 / (1/b1)
                        b1 = New Expression(1.0) / b1
                    ElseIf sOpB <> "/" Then
                        Exit Try
                    End If
                End If
                Dim sgn As Int64 = Me.sign * eB.sign
                Select Case sOpA
                    Case "-"
                        If a0.isEqualTo(b0, cjo1) AndAlso _
                        a1.isEqualTo(b1, cjo2) AndAlso _
                        (cjo1 - cjo2).IsZero Then
                            cjoMult = sgn * cjo1
                            bRet = True
                        ElseIf a0.isEqualTo(b1, cjo1) AndAlso _
                        a1.isEqualTo(b0, cjo2) AndAlso _
                        (cjo1 - cjo2).IsZero Then
                            cjoMult = -sgn * cjo1
                            bRet = True
                        End If
                    Case "+"
                        If a0.isEqualTo(b0, cjo1) AndAlso _
                        a1.isEqualTo(b1, cjo2) AndAlso _
                        (cjo1 - cjo2).IsZero Then
                            cjoMult = sgn * cjo1
                            bRet = True
                        ElseIf a0.isEqualTo(b1, cjo1) AndAlso _
                        a1.isEqualTo(b0, cjo2) AndAlso _
                        (cjo1 - cjo2).IsZero Then
                            cjoMult = sgn * cjo1
                            bRet = True
                        End If
                    Case "*"
                        If a0.isEqualTo(b0, cjo1) AndAlso _
                        a1.isEqualTo(b1, cjo2) Then
                            cjoMult = sgn * cjo1 * cjo2
                            bRet = True
                        ElseIf a0.isEqualTo(b1, cjo1) AndAlso _
                        a1.isEqualTo(b0, cjo2) Then
                            cjoMult = sgn * cjo1 * cjo2
                            bRet = True
                        End If
                    Case "/"
                        If a0.isEqualTo(b0, cjo1) AndAlso _
                        a1.isEqualTo(b1, cjo2) Then
                            cjoMult = sgn * cjo1 / cjo2
                            bRet = True
                        ElseIf a0.isEqualTo(b1, cjo1) AndAlso _
                        a1.isEqualTo(b0, cjo2) Then
                            cjoMult = sgn * cjo2 / cjo1
                            bRet = True
                        End If
                    Case "^"
                        If a0.isEqualTo(b0, cjo1) AndAlso _
                        cjo1.IsReal AndAlso cjo1.pRe.ToDouble = 1 AndAlso _
                        a1.isEqualTo(b1, cjo2) AndAlso _
                        cjo2.IsReal AndAlso cjo2.pRe.ToDouble = 1 Then
                            cjoMult = sgn * cjo1
                            bRet = True
                        End If
                End Select
                'Trace.Write(Me.ToString + "   " + eB.ToString + "  " + bRet.ToString)
                'If bRet Then
                '    Trace.WriteLine(" " + cjoMult.toString)
                'Else
                '    Trace.WriteLine("")
                'End If

            End If

            If bRet Then
                cjoMult = Complex.one / cjoMult
            End If



        Catch ex As Exception
            Throw ex
        End Try
        Return bRet
    End Function

    Public Function extractFromPath(ByVal path() As String, _
                                    ByRef result As Expression, _
                                    Optional ByVal sExamMultiple As String = "*") As Boolean
        Try
            ' input example:
            ' extractFromPath(new string() {"/","1"}, result)
            ' 1) The first element of the array string path() ("/") will
            '    have the consequence that "/" will be compared to the
            ' member mOp.tostring; in case they are equal, then:
            ' 2) the second element of path(), i.e., "1" indicates which
            ' element of args() -args(1)- to be set to the output:
            ' 3) result is set to args(1) and return value will be true.

            ' In case 1), if mOp.tostring and "/" were different the
            ' function would not succeed and return value would be 'false'.
            ' Another example: path() = {"/","1","sqr","0"}, if "/" matched
            ' mOp, then arg(1) would be considered and, then arg(1).mOp.toString()
            ' should be equal to "sqr" (or "sqrt") so the function would return true
            ' and output value in 'result' would be arg(1).arg(0)
            Dim i As Int64

            If sExamMultiple = "*" Then
                If path(0) <> Me.getMatchStr Then
                    Return False
                    'Return Nothing
                End If
                Dim bIsDiv() As Boolean = Nothing
                Dim vfac() As Expression = _
                 Me.exprToFactors(bIsDiv)
                If vfac.Length > 1 Then
                    For i = 0 To vfac.Length - 1
                        If bIsDiv(i) = False Then
                            If vfac(i).extractFromPath(path, result) Then
                                Return True
                            ElseIf path(0) = "*" Then
                                If path(1) = "0" Then
                                    If Args.Length Then
                                        result = Args(0)
                                        Return True
                                    End If
                                ElseIf path(1) = "1" Then
                                    If Args.Length > 1 Then
                                        result = Args(1)
                                        Return True
                                    End If
                                End If
                            End If
                        End If
                    Next
                    'Return False
                End If
            End If
            Dim curExpr As Expression = Me
            For i = 0 To path.Length - 1 Step 2
                If curExpr.mOp Is Nothing Then
                    Return False
                End If
                Dim e1 As String
                If path(i) <> "sqrt" Then
                    e1 = path(i)
                Else
                    e1 = "sqr"
                End If
                If curExpr.mOp Is Nothing OrElse _
                curExpr.Args Is Nothing OrElse _
                curExpr.Args.Length = 0 Then
                    Return False
                End If
                If InStr("-+*", curExpr.mOp.ToString) > 0 Then
                    Dim curExpr2 As Expression = curExpr.Args(0)
                    Dim path2(-1) As String
                    If i + 2 < path.Length Then
                        ReDim path2(path.Length - i - 2 - 1)
                        Array.Copy(path, i + 2, path2, 0, path.Length - i - 2)
                        If curExpr2.extractFromPath(path2, result) Then
                            Return True
                        End If
                        curExpr2 = curExpr.Args(1)
                        If curExpr2.extractFromPath(path2, result) Then
                            Return True
                        End If
                        Return False
                    Else
                        Return False
                    End If
                Else
                    If curExpr.mOp Is Nothing OrElse _
                    curExpr.mOp.ToString <> e1 OrElse _
                    curExpr.Args Is Nothing OrElse _
                    curExpr.Args.Length = 0 Then
                        Return False
                    End If

                    If path(i + 1) = "0" Then
                        curExpr = curExpr.Args(0)
                    ElseIf curExpr.Args.Length > 1 Then
                        curExpr = curExpr.Args(1)
                    Else
                        Return False
                    End If
                End If
            Next
            result = New Expression(curExpr)
        Catch ex As Exception
            Return False
        End Try
        Return True
    End Function
    Public Function convertSecCscCot(bConvertSin2To1minusCos2 As Boolean) As Expression
        Dim ret As Expression = Nothing
        Dim sgn As Int64 = Me.sign
        Try
            Dim b As Boolean = bConvertSin2To1minusCos2
            If mOp IsNot Nothing Then
                If mOp.Groups("fn").Success Then
                    Dim evalArg As Expression = Args(0)
                    Select Case mOp.ToString
                        Case "tan" : ret = Expression.AddFnAndArg0("sin", _
                                     evalArg) / Expression.AddFnAndArg0("cos", _
                                     evalArg)
                        Case "csc" : ret = New Expression(1.0) / _
                                    Expression.AddFnAndArg0("sin", evalArg)
                        Case "sec" : ret = New Expression(1.0) / _
                                    Expression.AddFnAndArg0("cos", evalArg)
                        Case "cot" : ret = Expression.AddFnAndArg0("cos", _
                                     evalArg) / Expression.AddFnAndArg0("sin", _
                                     evalArg)
                        Case "tanh" : ret = Expression.AddFnAndArg0("sinh", _
                                     evalArg) / Expression.AddFnAndArg0("cosh", _
                                     evalArg)
                        Case "csch" : ret = New Expression(1.0) / _
                                    Expression.AddFnAndArg0("sinh", evalArg)
                            'Case "sinh" : ret = New Expression(1.0) / _
                            'Expression.AddFnAndArg0("cosh", evalArg)
                        Case "coth" : ret = Expression.AddFnAndArg0("cosh", _
                                     evalArg) / Expression.AddFnAndArg0("sinh", _
                                     evalArg)
                    End Select
                ElseIf mOp.Groups("op").Success Then
                    Select Case mOp.ToString
                        Case "-" : ret = exprOp("-", Args(0).convertSecCscCot(b), Args(1).convertSecCscCot(b))
                        Case "+" : ret = exprOp("+", Args(0).convertSecCscCot(b), Args(1).convertSecCscCot(b))
                        Case "*" : ret = exprOp("*", Args(0).convertSecCscCot(b), Args(1).convertSecCscCot(b))
                        Case "/" : ret = exprOp("/", Args(0).convertSecCscCot(b), Args(1).convertSecCscCot(b))
                        Case "^"
                            If Args(0).IsReal AndAlso _
                            Args(0).toDouble = Math.E Then
                                ret = Expression.AddFnAndArg0("exp", Args(1))
                            Else
                                If b AndAlso mOp IsNot Nothing AndAlso _
                                mOp.ToString = "^" AndAlso _
                                Args(1).IsReal AndAlso _
                                Args(0).mOp IsNot Nothing _
                                AndAlso Args(0).mOp.ToString = "sin" Then
                                    Dim db As Double = Args(1).toDouble
                                    Dim arg As Expression = Args(0).Args(0)
                                    If Math.Floor(db) = db AndAlso _
                                    db Mod 2.0 = 0 Then
                                        Dim ent As Int64 = Math.Floor(db / 2.0)
                                        If ent = 1 Then
                                            ret = New Expression(1.0)
                                            ret -= Expression.AddFnAndArg0("cos", _
                                                arg) ^ New Expression(2.0)
                                        Else
                                            ret = New Expression(1.0)
                                            ret -= Expression.AddFnAndArg0("cos", _
                                                arg) ^ New Expression(2.0)
                                            ret ^= New Expression(ent)
                                        End If
                                        Exit Try
                                    End If
                                End If
                                ret = exprOp("^", Args(0).convertSecCscCot(b), Args(1))
                            End If
                    End Select
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        If ret Is Nothing Then
            ret = Me
        Else
            ret = ret.reduceFactors(False)
            If sgn = -1 Then
                ret.sign = -1
            End If
        End If
        Return ret
    End Function
    Public Overrides Function ToString() As String
        Return ToStringExpr(Config.cfg)
    End Function
    Public Function ToStringExpr( _
                        cfg As Config, _
                        Optional ByRef sOptor As String = "") As String
        Dim sRet As String = ""
        Try
            Dim eA As Expression = Nothing
            eA = Me
            Dim pA As Polynomial = eA.pA
            Dim args() As Expression = Me.Args
            Dim mOp As Match = Me.mOp
            Dim sLeft As String = ""
            Dim sMid As String = ""
            Dim sRight As String = ""
            If pA IsNot Nothing Then
                sLeft = pA.toStringPoly(cfg)
            Else
                If args.Length = 0 Then
                    If mOp IsNot Nothing Then
                        sMid = mOp.ToString
                    End If
                ElseIf args.Length = 1 Then
                    sMid = mOp.ToString
                    If mOp.Groups("fn").Success Then
                        sRight = "(" + args(0).ToStringExpr(cfg) + ")"
                    ElseIf mOp.ToString <> "!" Then
                        sRight = args(0).ToStringExpr(cfg)
                    Else
                        sLeft = args(0).ToStringExpr(cfg)
                        If Regex.IsMatch(sLeft, MathGlobal8.sOp) Then
                            sLeft = "(" + sLeft + ")"
                        End If
                    End If
                Else
                    Dim sOptorsL As String = ""
                    sLeft = args(0).ToStringExpr(cfg, sOptorsL)
                    sMid = mOp.ToString
                    sOptor += sMid
                    Dim sOptorsR As String = ""
                    sRight = args(1).ToStringExpr(cfg, sOptorsR)
                    Dim e1 As String = sLeft
                    If e1.Chars(0) = "-" Then
                        e1 = Mid(e1, 2)
                    End If
                    Dim bEncerrar As Boolean = False
                    Dim sOpL As String = ""
                    If args(0).mOp IsNot Nothing Then
                        If Not args(0).mOp.Groups("fn").Success Then
                            sOpL = args(0).mOp.ToString()
                        End If
                    End If
                    Select Case sMid
                        Case "^", "!"
                            If args(0).pA IsNot Nothing Then
                                If Regex.Match(e1, "[-+*/]").Success Then
                                    bEncerrar = True
                                End If
                            Else
                                If Len(sOpL) AndAlso _
                                InStr("^!", sOpL) = 0 Then
                                    bEncerrar = True
                                End If
                            End If
                        Case "*"
                            If args(0).pA IsNot Nothing Then
                                If Regex.Match(e1, "[-+/]").Success Then
                                    bEncerrar = True
                                End If
                            Else
                                If Len(sOpL) AndAlso _
                                InStr("*^!", sOpL) = 0 Then
                                    bEncerrar = True
                                End If
                            End If
                        Case "/"
                            If args(0).pA IsNot Nothing Then
                                If Regex.Match(e1, "[-+]").Success Then
                                    bEncerrar = True
                                End If
                            Else
                                If Len(sOpL) AndAlso _
                                InStr("*/^!", sOpL) = 0 Then
                                    bEncerrar = True
                                End If
                            End If
                    End Select
                    If bEncerrar Then
                        Dim strim As String = Trim(sLeft)
                        If strim.Chars(0) = "-" Then
                            strim = Mid(strim, 2)
                            If Regex.IsMatch(strim, "[-+]") OrElse _
                            sMid = "^" Then
                                sLeft = "(" + sLeft + ")"
                            Else
                                ' unary minus
                            End If
                        Else
                            sLeft = "(" + sLeft + ")"
                        End If
                    End If
                    e1 = sRight
                    'If e1.Chars(0) = "-" Then
                    'e1 = Mid(e1, 2)
                    'End If

                    bEncerrar = False
                    Dim sOpR As String = ""

                    ' if args(1).mOp is a function, there
                    ' already be parentheses:
                    If args(1).mOp IsNot Nothing AndAlso _
                    Not args(1).mOp.Groups("fn").Success Then
                        sOpR = args(1).mOp.ToString()
                    End If
                    Select Case sMid
                        Case "^", "!"
                            If args(1).pA IsNot Nothing Then
                                If Regex.Match(sRight, "[-+*/]").Success Then
                                    bEncerrar = True
                                End If
                            Else
                                If Len(sOpR) AndAlso _
                                InStr("^!", sOpR) = 0 Then
                                    bEncerrar = True
                                End If
                            End If
                        Case "*"
                            If args(1).pA IsNot Nothing Then
                                If Regex.Match(sRight, "[-+]").Success Then
                                    bEncerrar = True
                                End If
                            Else
                                If Len(sOpR) AndAlso _
                                InStr("*/^!", sOpR) = 0 Then
                                    bEncerrar = True
                                End If
                            End If
                        Case "/"
                            If args(1).pA IsNot Nothing Then
                                If Regex.Match(sRight, "[-+*]").Success Then
                                    bEncerrar = True
                                End If
                            Else
                                If Len(sOpR) AndAlso _
                                InStr("/^!", sOpR) = 0 Then
                                    bEncerrar = True
                                End If
                            End If
                    End Select
                    If bEncerrar Then
                        sRight = "(" + sRight + ")"
                    End If
                End If
            End If
            If eA.sign = -1 Then
                sRet = "-"
            End If
            sRet += sLeft + sMid + sRight
            If sRet.Length > 2 Then
                If Mid(sRet, 1, 2) = "--" Then
                    sRet = Mid(sRet, 3)
                ElseIf Mid(sRet, 1, 2) = "-+" Then
                    sRet = "-" + Mid(sRet, 3)
                ElseIf Mid(sRet, 1, 2) = "+-" Then
                    sRet = Mid(sRet, 2)
                End If
                sRet = Regex.Replace(sRet, "[ ]*\-[ ]*\-", " +") ' "--" --> "+"
                sRet = Regex.Replace(sRet, "[ ]*\+[ ]*\-", " -") ' "+-" --> "-"
                sRet = Regex.Replace(sRet, "[ ]*\+[ ]*\+", " +") ' "++" --> "+"
                sRet = Regex.Replace(sRet, "\*(&h|&o|&b)1\*", "*")
                sRet = Regex.Replace(sRet, "(&h|&o|&b)1\*", "")
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return sRet
    End Function
    Public Function getCommonFactor( _
                                   cfg As Config, _
                                   ByRef commonExpr As Expression, _
                                     ByRef Expr As Expression, _
                                     Optional ByRef toStr As String = Nothing, _
                                     Optional ByRef nFactorCount As Int64 = 0, _
                                     Optional ByRef vFactors() As Expression = Nothing) As Boolean

        Dim bRet As Boolean = False
        Try
            Dim i, j, k As Int64
            Dim bIsMinus() As Boolean = Nothing
            Dim vSum() As Expression = Me.splitIntoTerms()
            ReDim bIsMinus(vSum.Length - 1)
            For i = 0 To vSum.Length - 1
                If vSum(i).sign = -1 Then
                    vSum(i) = -vSum(i)
                    bIsMinus(i) = Not bIsMinus(i)
                End If
            Next
            If vSum.Length < 2 Then
                Return False
            End If
            Dim vFac(vSum.Length - 1)() As Expression
            Dim bIsDiv(vSum.Length - 1)() As Boolean
            Dim cnt0(-1) As Int64
            For i = 0 To vSum.Length - 1
                vFac(i) = vSum(i).exprToFactors(bIsDiv(i))
                If vFac(i).Length = 1 AndAlso _
                vFac(i)(0).IsPolynomial Then
                    Dim Pa As Polynomial = vFac(i)(0).getPolynomial
                    If Pa.cf.Length = 1 AndAlso Pa.var.Length > 1 Then
                        If Pa.cf(0).IsReal AndAlso Pa.cf(0).pRe.ToDouble < 0 Then
                            Pa.cf(0) = -Pa.cf(0)
                            bIsMinus(i) = Not bIsMinus(i)
                        End If
                        Dim vF(-1) As Expression, vDiv(-1) As Boolean
                        If Pa.split_1OnlyTermToExpr(vF, vDiv) Then
                            vFac(i) = vF
                            bIsDiv(i) = vDiv
                            Dim db As Double = 1.0
                            For i1 As Int64 = 0 To vF.Length - 1
                                Dim pavF As Polynomial = vF(i1).getPolynomial
                                If pavF.cf.Length = 1 AndAlso pavF.cf(0).IsReal Then
                                    If vDiv(i1) Then
                                        db /= pavF.cf(0).pRe.ToDouble
                                    Else
                                        db *= pavF.cf(0).pRe.ToDouble
                                    End If
                                    pavF.cf(0) = New Complex(1.0)
                                    vF(i1) = New Expression(pavF)
                                End If
                            Next
                            If db <> 1.0 Then
                                Dim ln As Int64 = vFac(i).Length
                                ReDim Preserve vFac(i)(ln), bIsDiv(i)(ln)
                                For i1 As Int64 = ln To 1 Step -1
                                    bIsDiv(i)(i1) = bIsDiv(i)(i1 - 1)
                                    vFac(i)(i1) = vFac(i)(i1 - 1)
                                Next
                                vFac(i)(0) = New Expression(db)
                                bIsDiv(i)(0) = False
                                'vFac(i)(0) *= New Expression(db)
                                'Dim ln As Int64 = vFac(i).Length
                                'ReDim Preserve vFac(i)(ln), bIsDiv(i)(ln)
                                'vFac(i)(ln) = New Expression(db)
                                ' bIsDiv(i)(ln)=false
                            End If
                        End If
                    End If
                End If
            Next
            ReDim cnt0(vFac(0).Length - 1)
            For i = 0 To cnt0.Length - 1
                cnt0(i) = 1 ' initially cnt(all i) = 1
            Next
            ' search for repeated factors in first summand's 
            ' factors vFac(0):
            For j = 0 To vFac(0).Length - 2
                If cnt0(j) Then
                    For k = j + 1 To vFac(0).Length - 1
                        If vFac(0)(j).isEqualTo(vFac(0)(k)) AndAlso _
                        bIsDiv(0)(j) = bIsDiv(0)(k) Then
                            cnt0(j) += 1
                            cnt0(k) = 0
                        End If
                    Next
                End If
            Next
            ' search for common factors to vFac(0) from vFac(1) to vfac(n)
            For i = 0 To cnt0.Length - 1
                If cnt0(i) Then
                    For j = 1 To vFac.Length - 1
                        Dim cnt1 As Int64 = cnt0(i)
                        For k = 0 To vFac(j).Length - 1
                            Dim mult As Double
                            If vFac(0)(i).isEqualTo(vFac(j)(k), mult) AndAlso _
                            mult = 1 Then
                                cnt1 -= 1
                                If cnt1 = 0 Then
                                    Exit For
                                End If
                            End If
                        Next
                        If cnt1 = cnt0(i) Then
                            ' found none
                            cnt0(i) = 0
                            Exit For
                        ElseIf cnt1 Then
                            ' found less than cnt0(j)
                            cnt0(j) = cnt1
                        Else
                            ' cnt1=0, found all
                        End If
                    Next
                End If
            Next
            ' set to null common factors from vFac(1)() to vfac(n)()
            For i = 0 To cnt0.Length - 1
                If cnt0(i) Then
                    For j = 1 To vSum.Length - 1
                        Dim cnt0B As Int64 = cnt0(i)
                        Dim cnt1 As Int64 = 0
                        For k = 0 To vFac(j).Length - 1
                            If vFac(j)(k) IsNot Nothing Then
                                If Not vFac(0)(i).IsReal AndAlso vFac(0)(i).isEqualTo(vFac(j)(k)) Then
                                    vFac(j)(k) = Nothing
                                    cnt0B -= 1
                                    If cnt0B <= 1 Then
                                        Exit For
                                    End If
                                End If
                            End If
                        Next
                    Next
                End If
            Next
            Dim bThereAreCommF As Boolean = False
            For i = 0 To cnt0.Length - 1
                If cnt0(i) Then
                    bThereAreCommF = True
                    Exit For
                End If
            Next
            If Not bThereAreCommF Then
                Exit Try
            End If
            ' reconstruct vSum(i>0) from vFac(i>0)():
            For j = 1 To vSum.Length - 1
                vSum(j) = New Expression(1.0)
                For k = 0 To vFac(j).Length - 1
                    If vFac(j)(k) IsNot Nothing Then
                        If vFac(j)(k).IsComplex Then
                            'vFac(j)(k) = Nothing
                        Else
                            vSum(j) *= vFac(j)(k)
                        End If
                    End If
                Next
            Next
            ' join common factors in vFac(0) into one expression
            ' and remaining factors into Expr:
            commonExpr = New Expression(1.0)
            'vSum(0) = New Expression(1.0)
            Expr = New Expression(1.0)
            nFactorCount = vFac(0).Length
            ReDim vFactors(vFac(0).Length - 1)
            Dim bIsOne As Boolean = True
            For j = 0 To vFac(0).Length - 1
                If cnt0(j) > 1 Then
                    vFactors(j) = vFac(0)(j) ^ New Expression(cnt0(i) - 1.0)
                    commonExpr *= vFactors(j)
                ElseIf cnt0(j) = 1 Then
                    vFactors(j) = vFac(0)(j)
                    commonExpr *= vFactors(j)
                Else
                    vFactors(j) = vFac(0)(j)
                    'vSum(0) *= vFactors(j)
                    Expr *= vFactors(j)
                    bIsOne = False
                End If
            Next
            If bIsOne Then
                Expr = New Expression(1.0)
            End If
            If bIsMinus(0) Then
                Expr = -Expr
            End If
            Dim curSum As New Expression(0.0)
            For i = 1 To vFac.Length - 1
                Dim curFac As New Expression(1.0)
                For j = 0 To vFac(i).Length - 1
                    If vFac(i)(j) IsNot Nothing Then
                        curFac *= vFac(i)(j)
                    End If
                Next
                If bIsMinus(i) Then
                    curSum -= curFac
                Else
                    curSum += curFac
                End If
            Next
            Expr += curSum
            If toStr IsNot Nothing Then
                toStr = commonExpr.ToStringExpr(cfg)
                Dim vMn() As Boolean = Nothing
                Dim vExprSum() As Expression = _
                    Expr.exprToSummands(vMn)
                If vExprSum.Length = 1 Then
                    toStr += "*" + Expr.ToStringExpr(cfg)
                Else
                    toStr += "*(" + Expr.ToStringExpr(cfg) + ")"
                End If
            End If
            bRet = True
        Catch ex As Exception
            Throw ex
        End Try
        Return bRet
    End Function
    Public Shared Function suprFractionsInEquation(eq As Expression) As Expression
        Dim ret As Expression = Nothing
        Try
            Dim vIsMn() As Boolean = Nothing
            Dim vSumm() As Expression = eq.exprToSummands(vIsMn)
            Dim vIsDiv(vSumm.Length - 1)() As Boolean
            Dim vFact(vSumm.Length - 1)() As Expression
            Dim cDivisor As New Expression(1.0)
            Dim bFound As Boolean = False

            Dim i As Int64
            For i = 0 To vSumm.Length - 1
                If vSumm(i).mOp IsNot Nothing AndAlso _
                vSumm(i).mOp.ToString = "/" AndAlso _
                vSumm(i).Args(1).pA IsNot Nothing Then
                    cDivisor *= New Expression(vSumm(i).Args(1).pA)
                    bFound = True
                ElseIf vSumm(i).pA IsNot Nothing AndAlso _
                vSumm(i).pA.PolyResto IsNot Nothing Then
                    cDivisor *= New Expression(vSumm(i).pA.PolyDivisor)
                    bFound = True
                End If
            Next
            If bFound Then
                For i = 0 To vSumm.Length - 1
                    vSumm(i) *= cDivisor
                Next
                ret = Expression.summandsToExpr(vSumm, vIsMn)
            Else
                ret = New Expression(eq)
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return ret
    End Function
    Public Function containsVar(sVar As String) As Boolean
        Dim bRet As Boolean = False
        Try
            Dim vars() As String = Nothing
            If getAllVars(vars) Then
                If Array.IndexOf(vars, sVar) > -1 Then
                    bRet = True
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return bRet
    End Function
    Public Sub getDepAndIndep(IRespVar As String, _
                ByRef dependent As Expression, _
                ByRef indepen As Expression)
        Try
            ' set aside all factors not dependent on variable IRespVar:
            Dim exprIndep As New Expression(1.0)
            Dim bIsDiv() As Boolean = Nothing
            Dim vFac() As Expression = exprToFactors(bIsDiv)
            Dim eSum As New Expression(1.0)
            For ivf = 0 To vFac.Length - 1
                If vFac(ivf).containsVar(IRespVar) Then
                    If vFac(ivf).pA IsNot Nothing AndAlso _
                    vFac(ivf).pA.cf.Length = 1 AndAlso _
                    vFac(ivf).pA.varAll.Length > 1 AndAlso _
                    vFac(ivf).pA.PolyResto Is Nothing Then
                        Dim pos As Int64 = _
                            Array.IndexOf(vFac(ivf).pA.varAll, IRespVar)
                        If pos = -1 OrElse _
                        vFac(ivf).pA.exp Is Nothing OrElse _
                        vFac(ivf).pA.exp.Length = 0 OrElse _
                        pos >= vFac(ivf).pA.exp(0).Length OrElse _
                        vFac(ivf).pA.exp(0)(pos) = 0 Then
                            If exprIndep Is Nothing Then
                                If bIsDiv(ivf) Then
                                    exprIndep = New Expression(1.0) / vFac(ivf)
                                Else
                                    exprIndep = vFac(ivf)
                                End If
                            ElseIf bIsDiv(ivf) Then
                                exprIndep /= vFac(ivf)
                            Else
                                exprIndep *= vFac(ivf)
                            End If
                        Else
                            Dim Px As Polynomial = _
                                Polynomial.GetPolynomial(IRespVar)
                            Px.exp(0)(0) = vFac(ivf).pA.exp(0)(pos)
                            If bIsDiv(ivf) Then
                                eSum /= New Expression(Px)
                            Else
                                eSum *= New Expression(Px)
                            End If
                            If bIsDiv(ivf) Then
                                exprIndep = _
                                    New Expression(Px) / vFac(ivf)
                            Else
                                exprIndep = _
                                    vFac(ivf) / New Expression(Px)
                            End If
                        End If
                    ElseIf bIsDiv(ivf) Then
                        eSum /= vFac(ivf)
                    Else
                        eSum *= vFac(ivf)
                    End If
                Else
                    If exprIndep Is Nothing Then
                        If bIsDiv(ivf) Then
                            exprIndep = New Expression(1.0) / vFac(ivf)
                        Else
                            exprIndep = vFac(ivf)
                        End If
                    ElseIf bIsDiv(ivf) Then
                        exprIndep /= vFac(ivf)
                    Else
                        exprIndep *= vFac(ivf)
                    End If
                End If
            Next
            dependent = eSum
            indepen = exprIndep
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Function tryToSimplifyEquation() As Expression
        Return New Expression(Me)

    End Function
    Public Function tryToIsolateToPolynomial(ByVal sVar As String, ByRef result As Expression) As Boolean
        Dim bRet As Boolean = False
        Try
            result = Nothing
            If Me.pA IsNot Nothing Then
                If Me.pA.tryToIsolateToPolynomial(sVar, result) Then
                    bRet = True
                    Exit Try
                End If
            End If
        Catch ex As Exception
            bRet = False
        End Try
        Return bRet
    End Function
    Public Function tryToIsolateToExpression(ByVal sVar As String,
                            ByRef vResult() As Expression,
                                  Optional ByRef alternativeExpr As Expression = Nothing,
                                  Optional termToIsolate As Expression = Nothing) As Boolean
        Dim bRet As Boolean = False
        'Dim cur As currentMatch = Me.cfg.cur.Clone
        Dim exprOrig As Expression = Nothing
        Try
            Dim rup As New ReduceExprUsingPolynomials
            exprOrig = rup.ReduceUsingPolynomials(Me)
            CopyExprToMe(exprOrig)
            If termToIsolate IsNot Nothing Then
                sVar = ""
            End If
            If Me.getMatchStr = "/" Then
                ' A/B=0 => A = 0
                Dim arg0 As New Expression(Args(0))
                CopyExprToMe(arg0)
            ElseIf Me.IsPolynomial Then
                ' A + num/den = 0  => A*den + num = 0
                Dim meP As New Polynomial(getPolynomial)
                If meP.PolyResto IsNot Nothing Then
                    Dim num As New Polynomial(meP.PolyResto)
                    Dim den As New Polynomial(meP.PolyDivisor)
                    meP.PolyResto = Nothing
                    meP.PolyDivisor = Nothing
                    meP *= den
                    meP += num
                    CopyExprToMe(New Expression(meP))
                End If
            End If
            alternativeExpr = Nothing
            vResult = Nothing
            Dim i As Int64
            Dim vArg() As Expression = Me.splitIntoTerms()
            Dim vS1(-1) As Expression, i1 As Int64 = 0
            Dim vS2(-1) As Expression, i2 As Int64 = 0
            For i = 0 To vArg.Length - 1
                Dim vVar(-1) As String
                vArg(i).getAllVars(vVar)
                ' split vSum() into terms containing sVar, in vS1(),
                ' and terms without sVar, in vS2():
                Dim mult As Complex = Nothing
                If (Len(sVar) AndAlso Array.IndexOf(vVar, sVar) > -1) OrElse
                (Len(sVar) = 0 AndAlso vArg(i).isEqualTo(termToIsolate, mult)) Then
                    ReDim Preserve vS1(i1)
                    vS1(i1) = vArg(i)
                    i1 += 1
                Else
                    ReDim Preserve vS2(i2)
                    vS2(i2) = vArg(i)
                    i2 += 1
                End If
            Next
            Dim Expr1 As Expression = Nothing
            Dim CommExpr As Expression = Nothing
            'Dim commStr As String = ""
            If i1 > 1 Then
                Dim bmn(vS1.Length - 1) As Boolean
                Dim vS As Expression = Expression.summandsToExpr(vS1, bmn)
                If vS.getCommonFactor(Me.cfg, CommExpr, Expr1) Then
                    Dim vvar(-1) As String
                    If Not CommExpr.getAllVars(vvar) OrElse
                    Array.IndexOf(vvar, sVar) = -1 Then
                        For i = 0 To vS2.Length - 1
                            vS2(i) /= CommExpr
                        Next
                        ReDim vS1(0)
                        vS1(0) = Expr1
                        i1 = 1
                    End If
                    ReDim vvar(-1)
                    If Not Expr1.getAllVars(vvar) OrElse
                    Array.IndexOf(vvar, sVar) = -1 Then
                        For i = 0 To vS2.Length - 1
                            vS2(i) /= Expr1
                        Next
                        ReDim vS1(0)
                        vS1(0) = CommExpr
                        i1 = 1
                    End If
                End If
            End If
            If i1 = 1 AndAlso vS1(0).IsPolynomial Then
                Dim Pa As Polynomial = vS1(0).getPolynomial
                If Pa.cf.Length = 1 AndAlso Pa.PolyResto Is Nothing AndAlso
                Pa.exp(0)(0) < 3 Then
                    Dim deg As Int64 = Pa.getMaxExponentOfVar(sVar)
                    Dim x As Polynomial = Polynomial.GetPolynomial(sVar)
                    For i = 1 To deg
                        Pa /= x
                    Next
                    vS1(0) = New Expression(Pa)
                    ReDim vResult(0)
                    vResult(0) = New Expression(0.0)
                    For i = 0 To i2 - 1
                        vS2(i) /= New Expression(vS1(0))
                        vResult(0) -= vS2(i)
                    Next
                    'If Pa.isReal AndAlso Pa.ToDouble = -1 Then
                    'vResult(0) = -vResult(0)
                    'End If
                    If deg <> 1 Then
                        If deg = 2 Then
                            ReDim Preserve vResult(1)
                            vResult(1) = New Expression(vResult(0))
                            vResult(0) = Expression.AddFnAndArg0("sqr", vResult(0))
                            vResult(1) = -Expression.AddFnAndArg0("sqr", vResult(1))
                        Else
                            vResult(0) = vResult(0) ^ New Expression(1.0 / deg)
                        End If
                    End If
                    'Dim varDegree As Int64 = Pa.getMaxExponentOfVar(sVar)
                    'Dim varP As Polynomial = Polynomial.GetPolynomial(sVar)
                    'For i = 0 To i2 - 1
                    '    vS2(i) /= New Expression(Pa.cf(0))
                    'Next
                    'Dim vMn(i2 - 1) As Boolean
                    'ReDim vResult(0)
                    'vResult(0) = -Expression.summandsToExpr(vS2, vMn)
                    'If varDegree Mod 2 = 0 Then
                    '    ReDim Preserve vResult(1)
                    '    vResult(0) ^= New Expression(New Polynomial(1.0 / varDegree))
                    '    vResult(1) = -vResult(0)
                    'ElseIf varDegree <> 1 Then
                    '    vResult(0) ^= New Expression(New Polynomial(1.0 / varDegree))
                    'End If
                    vResult(0) = rup.ReduceUsingPolynomials(vResult(0))
                    bRet = True
                    Exit Try
                ElseIf Pa.cf.Length = 1 Then
                    Dim pa2 As New Polynomial(Pa)
                    pa2.PolyResto = Nothing
                    pa2.PolyDivisor = Nothing
                    If pa2.isReal AndAlso pa2.ToDouble = 0.0 Then
                        If Pa.PolyResto.isReal Then
                            Dim bIsMn(vS2.Length - 1) As Boolean
                            ReDim vResult(0)
                            vResult(0) = Expression.summandsToExpr(vS2, bIsMn)
                            vResult(0) = New Expression(Pa.PolyResto) / vResult(0)
                            Dim degree As Int64 = Pa.PolyDivisor.getDegree
                            If degree Mod 2 = 0 Then
                                ReDim Preserve vResult(1)
                                vResult(0) ^= New Expression(1 / degree)
                                vResult(1) = vResult(0)
                            ElseIf degree <> 1 Then
                                vResult(0) ^= New Expression(1 / degree)
                            End If
                            If Pa.PolyDivisor.cf.Length = 2 Then
                                vResult(0) = New Expression(Pa.PolyDivisor.cf(1))
                            End If
                            bRet = True
                            Exit Try
                        End If
                    End If
                End If
            ElseIf i1 = 1 Then
                Dim curExpr As New Expression(vS1(0))
                If i2 = 0 Then
                    i2 = 1
                    ReDim vS2(0)
                    vS2(0) = New Expression(0.0)
                End If
                Dim vIsMn2(i2 - 1) As Boolean
                Dim RightSide As Expression = Expression.summandsToExpr(vS2, vIsMn2)
                vS2(0) = -RightSide
                Do
                    If curExpr.sign = -1 Then
                        vS2(0) = vS2(0).opChgSgn
                        curExpr = curExpr.opChgSgn
                    End If
                    Dim sOp As String = curExpr.getMatchStr
                    If sOp = "sqr" Then
                        Dim cur1 As New Expression(Regex.Match("^", MathGlobal8.sOp))
                        ReDim cur1.Args(1)
                        cur1.Args(1) = New Expression(0.5)
                        cur1.Args(0) = New Expression(curExpr.Args(0))
                        curExpr = cur1
                        sOp = "^"
                    End If
                    Select Case sOp
                        Case ""
                            If curExpr.IsPolynomial Then
                                Dim Pa As Polynomial = curExpr.getPolynomial
                                Dim iDiv As Int64
                                Dim vPa() As Polynomial = Pa.splitIntoTermsRestoAndDiv(iDiv)
                                Dim x As Polynomial = Polynomial.GetPolynomial(sVar)
                                Dim mult As New Complex(1.0)
                                Dim ix As Int64 = -1
                                If vPa.Length Then
                                    Dim i3 As Int64 = -1
                                    For i3 = 0 To vPa.Length - 1
                                        If vPa(i3).IsEqual(x, mult) Then
                                            ix = i3
                                            If ix < iDiv Then
                                                vS2(0) /= New Expression(mult)
                                            Else
                                                vS2(0) *= New Expression(mult)
                                            End If
                                            Exit For
                                        End If
                                    Next
                                    If i3 < vPa.Length Then
                                        For i3 = 0 To vPa.Length - 1
                                            If i3 <> ix Then
                                                If i3 < iDiv Then
                                                    vS2(0) /= New Expression(vPa(i3))
                                                Else
                                                    vS2(0) *= New Expression(vPa(i3))
                                                End If
                                            End If
                                        Next
                                    End If
                                    ReDim vResult(0)
                                    vResult(0) = vS2(0)
                                    bRet = True
                                    Exit Do
                                End If
                                curExpr = curExpr - vS2(0)
                                If curExpr.tryToIsolateToExpression(sVar, vResult) Then
                                    ReDim vS2(vResult.Length - 1)
                                    Array.Copy(vResult, vS2, vS2.Length)
                                Else
                                    alternativeExpr = curExpr
                                    Exit Try
                                End If
                            End If
                            ReDim vResult(vS2.Length - 1)
                            Array.Copy(vS2, vResult, vS2.Length)
                            bRet = True
                            Exit Do
                        Case "+", "-", "/", "*", "^"
                            For i = 0 To 1
                                Dim bIsolate As Boolean = True
                                If Len(sVar) AndAlso curExpr.IsPolynomial Then
                                    Dim curP As Polynomial = curExpr.getPolynomial
                                    If Array.IndexOf(curP.varAll, sVar) Then
                                        bIsolate = False
                                    End If
                                End If
                                If bIsolate Then
                                    Select Case sOp
                                        Case "+"
                                            vS2(0) -= curExpr.Args(i)
                                            curExpr = curExpr.Args(1 - i)
                                        Case "-"
                                            vS2(0) += curExpr.Args(i)
                                            curExpr = curExpr.Args(1 - i)
                                        Case "*", "/", "^"
                                            Dim vvar(-1) As String
                                            Dim n As Int64 = 0
                                            If curExpr.Args(0).getAllVars(vvar) AndAlso
                                            Array.IndexOf(vvar, sVar) > -1 Then
                                                n += 1
                                            End If
                                            ReDim vvar(-1)
                                            If curExpr.Args(1).getAllVars(vvar) AndAlso
                                            Array.IndexOf(vvar, sVar) > -1 Then
                                                n += 1
                                            End If
                                            If n > 1 Then
                                                Exit Try
                                            End If
                                            If sOp = "*" Then
                                                Dim vvar1(-1) As String
                                                curExpr.Args(i).getAllVars(vvar1)
                                                If Array.IndexOf(vvar1, sVar) > -1 Then
                                                    Continue For
                                                End If
                                                vS2(0) /= curExpr.Args(i)
                                                curExpr = curExpr.Args(1 - i)
                                            ElseIf sOp = "/" Then
                                                If i = 0 Then
                                                    vS2(0) /= curExpr.Args(0)
                                                    curExpr = curExpr.Args(1)
                                                Else
                                                    vS2(0) *= curExpr.Args(1)
                                                    curExpr = curExpr.Args(0)
                                                End If
                                            Else
                                                If Not curExpr.Args(1).IsReal Then
                                                    Exit Try
                                                End If
                                                Dim db As Double = 1
                                                Dim db0 As Complex = Nothing
                                                Try
                                                    db /= curExpr.Args(1).toDouble
                                                    If vS2(0).IsReal AndAlso vS2(0).toDouble < 0 Then
                                                        If Math.Floor(db) = db Then
                                                            Exit Try
                                                            'If db Mod 2 Then
                                                            '    db0 = New Complex(vS2(0).toDouble ^ db, 0)
                                                            'Else
                                                            '    db0 = New Complex(0, (-vS2(0).toDouble) ^ db)
                                                            'End If
                                                        Else
                                                            db0 = New Complex(vS2(0).toDouble, 0)
                                                            db0 ^= New Complex(db)
                                                        End If
                                                    ElseIf vS2(0).IsComplex Then
                                                        db0 = vS2(0).toComplex ^ New Complex(db)
                                                    Else
                                                        vS2(0) ^= New Expression(db)
                                                        curExpr = curExpr.Args(0)
                                                        GoTo saltry
                                                    End If
                                                Catch ex As Exception
                                                    Exit Try
                                                End Try
                                                vS2(0) = New Expression(db0)
                                                curExpr = curExpr.Args(0)
saltry:
                                            End If
                                        Case "exp"
                                            If i = 0 Then
                                                vS2(0) = Expression.AddFnAndArg0("ln", vS2(0))
                                                vS2(0) /= Expression.AddFnAndArg0("ln", curExpr.Args(1))
                                                curExpr = curExpr.Args(0)
                                            Else
                                                Exit Try
                                            End If
                                    End Select
                                    Exit For
                                Else
                                    Exit Try
                                End If
                            Next
                        Case Else
                            Dim mult As Complex = Complex.zero
                            If sVar = "" AndAlso curExpr.isEqualTo(termToIsolate, mult) Then
                                ReDim vResult(0)
                                vResult(0) = vS2(0)
                                bRet = True
                                Exit Try
                            End If
                            ' For ex.: exp(y)=x --> y=ln(x)
                            Dim iFn As Int64 = Array.IndexOf(
                                New String() {"exp", "ln", "log", "log10", "log2"}, sOp)
                            If iFn > -1 Then
                                Dim vInvFn() As String = New String() {"ln", "exp", "exp", "10", "2"}
                                Dim sOpInv As String = vInvFn(iFn)
                                Dim db As Double
                                If Double.TryParse(sOpInv, db) Then
                                    vS2(0) = New Expression(db) ^ vS2(0)
                                    curExpr = curExpr.Args(0)
                                ElseIf sOpInv = "exp" AndAlso
                                vS2(0).IsReal AndAlso vS2(0).toDouble = 0 Then
                                    vS2(0) = New Expression(1.0)
                                    curExpr = curExpr.Args(0)
                                Else
                                    vS2(0) = Expression.AddFnAndArg0(sOpInv, vS2(0))
                                    curExpr = curExpr.Args(0)
                                End If
                            Else
                                Exit Try
                            End If

                    End Select
                Loop

            End If
            If bRet Then
                For i = 0 To vResult.Length - 1
                    vResult(i) = rup.ReduceUsingPolynomials(vResult(i))
                Next
            End If
        Catch ex As Exception
        Finally
            CopyExprToMe(exprOrig)
        End Try
        'Me.cfg.cur = cur
        Return bRet
    End Function
    Public Function getFractionPowers(
        ByVal sVar As String) As Complex()

        Dim ret(-1) As Complex
        Try
            If pA IsNot Nothing Then
            ElseIf mOp.Groups("fn").Success Then
            Else
                Select Case mOp.ToString
                    Case "^"
                        If Not Args(0).IsComplex Then
                            Dim vvar() As String = Nothing
                            Args(0).getAllVars(vvar)
                            'If Array.IndexOf(vvar, sVar) > -1 AndAlso _
                            'Args(0).IsPolynomial Then
                            If Array.IndexOf(vvar, sVar) > -1 Then
                                If Args(1).IsReal Then
                                    ReDim Preserve ret(ret.Length)
                                    ret(ret.Length - 1) = Args(1).toComplex
                                End If
                            End If
                        End If
                    Case Else
                        If Args.Length = 0 Then
                            Exit Try
                        End If
                        Dim r() As Complex = Args(0).getFractionPowers(sVar)
                        If r.Length Then
                            Dim rlen As Int64 = ret.Length
                            ReDim Preserve ret(rlen + r.Length - 1)
                            Array.Copy(r, 0, ret, rlen, r.Length)
                        End If
                        If Args.Length > 1 Then
                            r = Args(1).getFractionPowers(sVar)
                            If r.Length Then
                                Dim rlen As Int64 = ret.Length
                                ReDim Preserve ret(rlen + r.Length - 1)
                                Array.Copy(r, 0, ret, rlen, r.Length)
                            End If
                        End If
                End Select
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return ret
    End Function
    Public Sub reduce()
        Try
            If Me.IsComplex OrElse Me.IsPolynomial Then
                Exit Sub
            End If
            Dim vExpr() As Expression = Me.splitIntoTerms
            Dim i, j As Int64
            Dim bChanged As Boolean = False

            '  (cosx+isinx)(cosy-isiny) 
            ' Reduce cos2(arg)+sin2(arg) ?
            For i = 0 To vExpr.Length - 2
                Dim eA As Expression = vExpr(i)
                If eA IsNot Nothing Then
                    For j = i + 1 To vExpr.Length - 1
                        If vExpr(j) IsNot Nothing Then
                            Dim eB As Expression = vExpr(j)
                            ' Is sin2(x) + cos2(x) ?:
                            If eA.Args.Length = 2 AndAlso eA.getMatchStr = "^" AndAlso
                            eB.Args.Length = 2 AndAlso eB.getMatchStr = "^" AndAlso
                            eA.Args(1).IsReal AndAlso eA.Args(1).toDouble = 2 AndAlso
                            eB.Args(1).IsReal AndAlso eB.Args(1).toDouble = 2 Then
                                Dim sFnA As String = LCase(eA.Args(0).getMatchStr)
                                Dim sFnB As String = LCase(eB.Args(0).getMatchStr)
                                If (sFnA = "sin" AndAlso sFnB = "cos") OrElse
                                (sFnA = "cos" AndAlso sFnB = "sin") Then
                                    Dim argA As Expression = eA.Args(0).Args(0)
                                    Dim argB As Expression = eB.Args(0).Args(0)
                                    Dim diff As Expression = argA - argB
                                    If diff.IsReal AndAlso diff.toDouble = 0.0 Then
                                        vExpr(i) = New Expression(1.0)
                                        vExpr(j) = Nothing
                                        bChanged = True
                                        Exit For
                                    End If
                                End If
                            ElseIf eA.getMatchStr = "*" AndAlso eB.getMatchStr = "cos" Then
                                Dim argCos As Expression = eB.Args(0)
                                Dim argSin As Expression = Nothing
                                Dim sgn As Int64 = 1
                                If eA.Args(0).getMatchStr = "sin" AndAlso
                                eA.Args(1).IsComplex AndAlso
                                (eA.Args(1).toComplex - Complex.i).IsZero Then
                                    argSin = eA.Args(0).Args(0)
                                ElseIf eA.Args(1).getMatchStr = "sin" AndAlso
                                eA.Args(0).IsComplex AndAlso
                                (eA.Args(0).toComplex - Complex.i).IsZero Then
                                    argSin = eA.Args(1).Args(0)
                                ElseIf eA.Args(0).getMatchStr = "sin" AndAlso
                                eA.Args(1).IsComplex AndAlso
                                (eA.Args(1).toComplex + Complex.i).IsZero Then
                                    argSin = -eA.Args(0).Args(0)
                                    sgn = -1
                                ElseIf eA.Args(1).getMatchStr = "sin" AndAlso
                                eA.Args(0).IsComplex AndAlso
                                (eA.Args(0).toComplex + Complex.i).IsZero Then
                                    argSin = -eA.Args(1).Args(0)
                                    sgn = -1
                                End If
                                If argSin IsNot Nothing Then
                                    If sgn = 1 Then
                                        argCos -= argSin
                                    Else
                                        argCos += argSin
                                    End If
                                    If argCos.IsReal AndAlso argCos.toDouble = 0.0 Then
                                        vExpr(i) = Expression.AddFnAndArg0("exp", argSin)
                                        vExpr(j) = Nothing
                                        bChanged = True
                                        Exit For
                                    End If
                                End If
                            End If
                        End If
                    Next
                End If
            Next
            If bChanged Then
                Dim eC As New Expression(0.0)
                For i = 0 To vExpr.Length - 1
                    If vExpr(i) IsNot Nothing Then
                        eC += vExpr(i)
                    End If
                Next
                CopyExprToMe(eC)
            End If

        Catch ex As Exception

        End Try
    End Sub
    Public Function trigonometricToExponential() As Expression
        Dim ret As Expression = Nothing
        Try
            Dim bIsMn() As Boolean = Nothing
            Dim vSumm() As Expression = Me.exprToSummands(bIsMn)
            For i As Int64 = 0 To vSumm.Length - 1
                Dim bIsDiv() As Boolean = Nothing
                Dim vFac() As Expression = vSumm(i).exprToFactors(bIsDiv)
                For k As Int64 = 0 To vFac.Length - 1
                    vFac(k) = vFac(k).trigonometricToExponential2
                Next
                vSumm(i) = Expression.factorsToExpr(vFac, bIsDiv)
                'vSumm(i) = vSumm(i).trigonometricToExponential2
            Next
            ret = Expression.summandsToExpr(vSumm, bIsMn)
            Dim rup As New ReduceExprUsingPolynomials
            ret = rup.ReduceUsingPolynomials(ret)
        Catch ex As Exception
            Throw ex
        End Try
        Return ret
    End Function
    Public Function trigonometricToExponential2() As Expression
        Dim ret As Expression = Nothing
        Try
            If IsPolynomial() Then
                ret = New Expression(getPolynomial)
            ElseIf mOp.Groups("fn").Success Then
                Dim evalArg As Expression = Args(0)
                Select Case mOp.ToString
                    Case "tanh"
                        ' tanh = sinh/cosh = (1-e^(-2*x))/(1+e^(-2*x))
                        Dim emn2x As Expression = Expression.AddFnAndArg0( _
                            "exp", New Expression(-2.0) * evalArg)
                        Dim one As New Expression(1.0)
                        ret = (one - emn2x) / (one + emn2x)
                    Case "tan"
                        ret = Expression.AddFnAndArg0("sin", _
                         evalArg) / Expression.AddFnAndArg0("cos", _
                         evalArg)
                        ret = ret.trigonometricToExponential
                    Case "csc"
                        ret = New Expression(1.0) / _
                         Expression.AddFnAndArg0("sin", evalArg)
                        ret = ret.trigonometricToExponential
                    Case "sec"
                        ret = New Expression(1.0) / _
                            Expression.AddFnAndArg0("cos", evalArg)
                        ret = ret.trigonometricToExponential
                    Case "cot"
                        ret = Expression.AddFnAndArg0("cos", _
                            evalArg) / Expression.AddFnAndArg0("sin", _
                            evalArg)
                        ret = ret.trigonometricToExponential
                    Case "sin"
                        Dim ixArg As Expression = New Expression(Complex.i) * _
                            evalArg
                        Dim mnixArg As Expression = New Expression(-Complex.i) * _
                            evalArg
                        ret = (Expression.AddFnAndArg0("exp", _
                                ixArg) - _
                            Expression.AddFnAndArg0("exp", _
                              mnixArg)) / New Expression(2 * Complex.i)
                    Case "cos"
                        Dim ixArg As Expression = New Expression(Complex.i) * _
                            evalArg
                        Dim mnixArg As Expression = New Expression(-Complex.i) * _
                            evalArg
                        ret = (Expression.AddFnAndArg0("exp", _
                                ixArg) + _
                            Expression.AddFnAndArg0("exp", _
                              mnixArg)) / New Expression(2)
                    Case "sinh"
                        Dim xArg As Expression = New Expression(Complex.i) * _
                            evalArg
                        Dim mnixArg As Expression = New Expression(-Complex.i) * _
                            evalArg
                        ret = (Expression.AddFnAndArg0("exp", _
                                xArg) - _
                            Expression.AddFnAndArg0("exp", _
                              mnixArg)) / New Expression(2 * Complex.i)
                    Case "cosh"
                        Dim ixArg As Expression = New Expression(Complex.i) * _
                            evalArg
                        Dim mnixArg As Expression = New Expression(-Complex.i) * _
                            evalArg
                        ret = (Expression.AddFnAndArg0("exp", _
                                ixArg) + _
                            Expression.AddFnAndArg0("exp", _
                              mnixArg)) / New Expression(2)
                    Case "tanh"
                        ret = Expression.AddFnAndArg0("sinh", _
                         evalArg) / Expression.AddFnAndArg0("cosh", _
                         evalArg)
                        ret = ret.trigonometricToExponential
                    Case Else
                        ret = New Expression(Me)
                End Select

            Else
                ret = New Expression(Me)
            End If
        Catch ex As Exception
            Throw ex
        End Try
        If Me.Args.Length = 2 Then
            If Me.Args(1) Is Nothing Then
                ReDim Preserve Me.Args(0)
                mOp = Nothing
            ElseIf Me.Args(0) Is Nothing Then
                Me.Args(0) = Me.Args(1)
                ReDim Preserve Me.Args(0)
                mOp = Nothing
            End If
        End If
        If sign = -1 Then
            ret = -ret
        End If
        ret.cfg = cfg
        Return ret
    End Function
End Class