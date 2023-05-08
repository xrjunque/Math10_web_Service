Imports System.Text.RegularExpressions
Imports System.Text
'Imports Microsoft.Web.WebView2

Public Class ParseExpression
    Public Class Result
        Public vExpr(-1) As Expression
        Public sDetail As String
    End Class
#Region "Declarations"
    Dim m, mc() As Match, mc1 As MatchCollection
    Dim iC As Int32
    Dim LP, RP As Int32
    Public vars As New Dictionary(Of String, Expression)
    Dim _signoDecimal As String
    Public vRoots(-1) As Complex, vPoly(-1) As Polynomial
    'Private g10.detail As Boolean
    Dim oD As New List(Of oDetail)
    Dim ivsD As Int32, vsDetail(-1) As String
    Dim r As New Result
    Class oDetail
        Public Shared _mc() As Match
        Public x, y As Int32
        Public m As Match
        Public Sub New(first As Int32, last As Int32, m As Match, mc() As Match)
            x = first
            y = last
            Me.m = m
            If m.Value.Length > 2 AndAlso
                Microsoft.VisualBasic.Left(m.Value, 1) = "(" Then
                Me.m = Regex.Match(Mid(m.Value, 2, m.Length - 2), ".*")
            End If
            _mc = mc
        End Sub
        Public Shared Sub Update(mc() As Match)
            _mc = mc
        End Sub
        Public Overrides Function ToString() As String
            Dim s As String = String.Format("{0}/{1} {2} = ", x, y, m.Value)
            For i As Int32 = x To y
                s += _mc(i).Value
            Next
            Return s
        End Function
    End Class
#End Region

    Public Sub New()
        'G10.CI = New Globalization.CultureInfo("en-US")
    End Sub
    Public Sub New(cultInfo As Globalization.CultureInfo)
        G10.CI = cultInfo
    End Sub
    Public Shared Property CultureInfo As Globalization.CultureInfo
        Get
            Return G10.CI
        End Get
        Set(value As Globalization.CultureInfo)
            G10.CI = value
        End Set
    End Property
    Public Shared Property Decimals As Int32
        Get
            Return G10.nDec
        End Get
        Set(value As Int32)
            If value < 0 OrElse value > 15 Then value = 15
            G10.nDec = value
        End Set
    End Property
    Public Shared Property Imaginary As String
        Get
            Return G10.sImg
        End Get
        Set(value As String)
            If value <> "i" AndAlso value <> "j" Then value = "i"
            G10.sImg = value
        End Set
    End Property

#Region "Parse and Evaluate"
    Public Function Evaluate(vars As Dictionary(Of String, Expression), strExpression As String) As Result
        Dim r As Result = Nothing
        Try
            Me.vars = vars
            If Trim(strExpression) = "" Then Exit Try
            r = Evaluate(strExpression)
        Catch ex As Exception
            Throw
        End Try
        Return r
    End Function
    Public Function Evaluate(vars As Dictionary(Of String, Expression)) As Result
        Dim expr As Expression
        Try
            If vars IsNot Nothing Then
                Me.vars = vars
            End If
            iC = 0
            LP = 0 : RP = 0
            ReDim vRoots(-1), vPoly(-1)
            m = mc(0)
            If mc.Count < 2 Then
                Exit Try
            End If
            expr = AddSubs()
            If vPoly.Length Then
                ReDim r.vExpr(vPoly.Length - 1)
                For i As Int32 = 0 To vPoly.Length - 1
                    If vPoly(i) IsNot Nothing Then
                        r.vExpr(i) = New Expression(vPoly(i))
                    End If
                Next
            ElseIf vRoots.Length Then
                ReDim r.vExpr(vRoots.Length - 1)
                For i As Int32 = 0 To vRoots.Length - 1
                    r.vExpr(i) = New Expression(vRoots(i))
                Next
            ElseIf r.vExpr.Length <= 1 Then
                If expr IsNot Nothing Then
                    expr = ReduceExprUsingPolynomials.TryToReduce(expr)
                End If
                r.vExpr = New Expression() {expr}
                If G10.detail Then
                    Dim lst As New List(Of String)
                    lst.AddRange(vsDetail)
                    Dim j As Int32, i As Int32
                    Dim lst2 As New List(Of String)
                    For j = 0 To lst.Count - 2 Step 2
                        Dim s1 As String = Regex.Replace(lst(j), "<[^>]+>", "")
                        Dim s2 As String = Regex.Replace(lst(j + 1), "<[^>]+>", "")
                        If s1 <> s2 Then
                            lst2.Add(lst(j))
                            lst2.Add(lst(j + 1))
                        End If
                    Next
                    For i = j To lst.Count - 1
                        If InStr(lst(i), "table") Then
                            lst2.Add(lst(i))
                        End If
                    Next
                    r.sDetail = Join(lst2.ToArray, vbCrLf)
                End If
            End If
        Catch ex As Exception
            Throw
        End Try
        Return r
    End Function
    Function FindMaching(s1 As String) As String
        Try
            Dim s As String = ""
            Do While s <> s1
                s = s1
                s1 = Regex.Replace(s, "\([^\)\(]*\)", "")
            Loop
            s1 = Regex.Replace(s1, "\s*", "")
            If InStr(s1, "(") Then Return ")"
            If InStr(s1, ")") Then Return "("
        Catch ex As Exception

        End Try
        Return ""
    End Function
    'Public Async Function EvaluateAsync(sExpression As String) As Task(Of String())
    '    Dim sRet(1) As String
    '    Try
    '        Dim r As Result = Nothing
    '        Dim action As Action(Of String) =
    '            Sub(str As String)
    '                r = Evaluate(str)
    '            End Sub
    '        Dim t1 As New Task(action, sExpression)
    '        Await t1
    '        sRet(0) = r.ToString
    '        sRet(1) = r.sDetail
    '    Catch ex As Exception
    '        sRet(0) = ex.Message
    '        sRet(1) = ""
    '    End Try
    '    Return sRet
    'End Function
    Public Function Evaluate(expression As String) As Result
        Try
            If expression.Length = 0 Then
                Throw New Exception(Msg10.Num(100)) ' empty
            End If
            'G10.detail = False : G10.sDetail = ""
            expression = G10.reSubstitute.Replace(expression, AddressOf Substitute)
            setVariables(expression)
            expression = Split(expression, "@")(0)
            mc1 = Regex.Matches(expression + ChrW(27), G10.reTot.ToString + "|;|\|")
            ReDim mc(mc1.Count - 1)
            mc1.CopyTo(mc, 0)
            ReDim r.vExpr(-1)
            r = Evaluate(Me.vars)
        Catch ex As Exception
            Throw
        End Try
        Return r
    End Function
    Private Function Substitute(ms As Match) As String
        Try
            Dim pos As Int32 = Array.IndexOf(G10.vSubstitute, ms.Value)
            Dim sm As String = LCase(ms.Value)
            If pos = -1 Then
                If Len(ms.Value) = 1 Then
                    Return G10.vSubstituteBy(pos)
                ElseIf ms.Value = "**" Then
                    Return G10.vSubstituteBy(pos)
                ElseIf ms.Value.Chars(0) = "@" Then
                    Dim vs() As String = Split(ms.Value, "=")
                    vs(0) = Mid(vs(0), 2)
                    Dim eP As New ParseExpression(CultureInfo)
                    If vars.ContainsKey(vs(0)) Then
                        vars(vs(0)) = eP.Evaluate(vs(1)).vExpr(0)
                    Else
                        vars.Add(vs(0), eP.Evaluate(vs(1)).vExpr(0))
                    End If
                    Return ""
                ElseIf Left(sm, 4) = "&mod" Then
                    Return sm
                ElseIf ms.Groups("cult").Success Then
                    CultureInfo = New Globalization.CultureInfo(ms.Groups("cult").Value)
                    Return ""
                ElseIf InStr(sm, "mathml") Then
                    G10.mathml = True
                    Return ""
                ElseIf InStr(sm, "html") Then
                    G10.mathml = False
                    Return ""
                Else
                    Return ms.Value
                End If
            End If
            Return G10.vSubstituteBy(pos)
        Catch ex As Exception
            Throw
        End Try
    End Function
    Sub setVariables(sExpr As String)
        Try
            Dim pM As New parseMatrix
            Dim pOM As New ParseOneMatrix
            Dim pE As New ParseExpression
            Dim mcVars As MatchCollection = Regex.Matches(
                sExpr,
                "\s*(?<=@)\s*(?<vars>[a-zA-Z\d]+)\s*(?<parval>(?<param>[()\w\d,]+)?\s*=\s*(?<val>[^@←]+))",
                RegexOptions.IgnorePatternWhitespace)
            Dim sNameErr As New List(Of String), sValueErr As New List(Of String)
            For i As Int32 = 0 To mcVars.Count - 1
                Dim varValue As String = Regex.Replace(mcVars(i).Groups("val").Value, "\|*$", "")
                Dim varName As String = Regex.Replace(mcVars(i).Groups("vars").Value, "\|*$", "")
                If mcVars(i).Groups("param").Success Then
                    ' Is a function, get the parameters and the definition
                    varName += mcVars(i).Groups("param").Value
                End If
                If Not Regex.IsMatch(varValue, G10.sMtxFn + "|" + G10.RowOrColSeparator) Then ' First, vars and function expressions
                    Try
                        Dim r As ParseExpression.Result = pE.Evaluate(vars, varValue)
                        If vars.ContainsKey(varName) Then
                            vars.Remove(varName)
                        End If
                        vars.Add(varName, r.vExpr(0))
                    Catch ex As Exception
                    End Try
                End If
            Next
        Catch ex As Exception

        End Try
    End Sub
    Private Function AddSubs() As Expression
        Dim value, valueb As Expression
        Dim optor As Char
        Dim iC0 As Int32
        Try
            iC0 = iC
            value = MultDiv()
            Do While InStr("-+", m.Value)
                optor = m.Value
                valueb = MultDiv()
                If optor = "+" Then
                    value += valueb
                Else
                    value -= valueb
                End If
                If G10.detail Then DoDetail(iC0, 3, value)
            Loop
            If m.Value = "=" Then
                value -= AddSubs()
                value.IsEquation = True
            End If
            If iC < mc.Length AndAlso InStr(mc(iC).Value, "&") Then
                Dim m1 As Match = m
                Token(1)
                m = m1
            End If

        Catch ex As Exception
            Throw
        End Try
        Return value
    End Function
    Private Function MultDiv() As Expression
        Dim value, value2b As Expression
        Dim optor2 As Char
        Dim iC2 As Int32
        Dim s As String = ""
        Dim p As Polynomial = Nothing
        Dim intPart As Polynomial = Nothing
        Try
            iC2 = iC
            value = Pow()
            Do While InStr("*/÷", m.Value)
                optor2 = m.Value
                value2b = Pow()
                Dim bDetailHere As Boolean = True
                Dim value3 As Expression

                If G10.detail AndAlso
                    (Not value.IsDouble AndAlso Not value2b.IsDouble) Then
                    bDetailHere = False
                End If
                If optor2 = "*" Then
                    value3 = value * value2b
                ElseIf G10.detail AndAlso value.IsPolynomial AndAlso value2b.IsPolynomial _
                AndAlso Not value.IsComplex AndAlso Not value2b.IsComplex Then
                    Dim s1 As String = "Division of " + value.ToString + "|" + vbCrLf + "by " + value2b.ToString + "|" + vbCrLf
                    p = New Polynomial(0.0)
                    p.resto = value.ToPolynomial.resto
                    p.divisor = value2b.ToPolynomial.resto
                    intPart = New Polynomial
                    intPart.resto = New List(Of Term)
                    p.Reduce(intPart.resto)
                    G10.sDetail = Replace(G10.sDetail, value2b.ToString, " ", 1, 1)
                    DoDetail2(iC2, iC - 1, intPart, s1 + G10.sDetail, p.ToString)
                    If Regex.Matches(G10.sDetail, vbCrLf).Count > 2 Then
                        s = GetDetail()
                    End If
                    value3 = New Expression(intPart + p)
                Else
                    value3 = value / value2b
                End If
                If G10.detail Then
                    If p Is Nothing Then
                        DoDetail(iC2, 3, value3)
                    Else
                        DoDetail2(iC2, 3, New Polynomial(intPart), "", p.ToString)
                        If Len(s) Then
                            ReDim Preserve vsDetail(ivsD)
                            vsDetail(ivsD) = GetDetail() : ivsD += 1
                        End If
                    End If
                End If
                value = value3
            Loop
        Catch ex As Exception
            Throw
        End Try
        Return value
    End Function


    Private Function Pow() As Expression
        Dim value3 As Expression
        Dim optor3 As Char
        Dim sign3 As Int32 = 1
        Dim iC3(0) As Int32
        Dim vExpr(0) As Expression
        Dim i As Int32 = 0
        Try
            iC3(0) = iC
            value3 = Token(sign3)
            Do While InStr("^", m.Value)
                vExpr(i) = value3
                optor3 = m.Value
                ReDim Preserve iC3(i + 1)
                iC3(i + 1) = iC
                Dim sign4 As Int32 = 1
                Dim value4 As Expression = Token(sign4)
                If sign4 = -1 Then value4.OpChgSign()
                ReDim Preserve vExpr(vExpr.Length)
                value3 = value4
                i += 1
            Loop
            If i Then
                vExpr(i) = value3
                For j As Int32 = i - 1 To 0 Step -1 ' go backwards in for ex. 2^3^2 = 256 = 2^(3^2)
                    value3 = vExpr(j) ^ vExpr(j + 1)
                    vExpr(j) = value3
                    If G10.detail Then DoDetail(iC3(j), 3, value3)
                Next
                iC3(0) = iC
            End If
            If sign3 = -1 Then
                value3.OpChgSign()
            End If
        Catch ex As Exception
            Throw
        End Try
        Return value3
    End Function
    Private Sub InsertOptor(sOptor)
        ReDim Preserve mc(mc.Length)
        For i As Int32 = mc.Count - 1 To iC - 1 Step -1
            mc(i) = mc(i - 1)
        Next
        m = Regex.Match(sOptor, "(?<op>[*\^])") ' insert missing operator
        mc(iC - 1) = m
        If G10.detail Then
            Dim i As Int32
            For i = 0 To oD.Count - 1
                If oD(i).x = iC - 1 Then
                    oD.Insert(i, New oDetail(iC - 1, iC - 1, Regex.Match(sOptor, "(?<op>[*\^])"), mc.ToArray))
                    Exit For
                End If
            Next
            For i = i + 1 To oD.Count - 1
                oD(i).x += 1
                oD(i).y += 1
            Next
        End If
    End Sub
    Private Sub ChangePattern()
        Try
            G10.Initialize()
            Dim s As String = ""
            For i As Int32 = iC To mc.Count - 1
                s += mc(i).Value
            Next
            If Len(s) Then
                mc1 = Regex.Matches(s, G10.reTot.ToString + "|;|\|")
                ReDim Preserve mc(iC + mc1.Count - 1)
                mc1.CopyTo(mc, iC)
            End If
        Catch ex As Exception

        End Try
    End Sub
    Public Sub setModes()
        If Left(m.Value.ToLower, 4) = "&dec" Then
            G10.nDec = Int32.Parse(Mid(m.Value, 5))
        ElseIf Left(m.Value.ToLower, 4) = "&mod" Then
            G10.mMod = Int32.Parse(Mid(m.Value, 5))
            Polynomial.bReduce = IIf(G10.mMod > 1, False, True)
        ElseIf Left(m.Value.ToLower, 4) = "&var" Then
            If InStr(m.Value, "1") Then
                G10.var = True
            Else
                G10.var = False
            End If
            G10.Initialize()
            ChangePattern()
        Else
            Select Case LCase(m.Value)
                Case "&mathml1" : G10.mathml = True
                Case "&mathml0" : G10.mathml = False
                Case "&detail1" : G10.detail = True
                Case "&detail0" : G10.detail = False
                Case "&rad" : G10.angleMode = G10.AngleModes.radians
                Case "&deg" : G10.angleMode = G10.AngleModes.degree
                Case "&grad" : G10.angleMode = G10.AngleModes.gradian
                Case "&h" : G10.currBase = Rational.Base.Hexadecimal
                    ChangePattern()
                Case "&d" : G10.currBase = Rational.Base.Decimal
                    ChangePattern()
                Case "&o" : G10.currBase = Rational.Base.Octal
                    ChangePattern()
                Case "&b" : G10.currBase = Rational.Base.Binary
                    ChangePattern()
                Case "&j" : G10.sImg = "j"
                Case "&i" : G10.sImg = "i"
                Case "&eng0" : G10.eng = False
                Case "&eng1" : G10.eng = True
                Case "&fra0" : G10.frac = False
                Case "&fra1" : G10.frac = True
                Case "&irtnl0" : G10.bTryIrrationals = False
                Case "&irtnl1" : G10.bTryIrrationals = True
            End Select
        End If
    End Sub
    Private Sub Advance()
        If iC < mc.Length Then
            m = mc(iC)
            iC += 1
        End If
    End Sub
    Private Function Token(ByRef sign As Int32) As Expression
        Dim value As Expression = Nothing
        Dim ic0 As Int32 = iC
        Try
            Advance()
            Do While Left(m.Value, 1) = "&"
                setModes()
                Advance()
                If iC >= mc.Count Then Exit Try
            Loop
            If m.Value = "(" Then
                LP += 1
                value = AddSubs()
                If m.Value = "," Then Exit Try
                If m.Value <> ")" Then
                    Dim s As String = String.Format(Msg10.Num(5), errorAt(iC)) ' unknown token
                    Throw New Exception(s)
                End If
                RP += 1
                Advance()
                If m.Groups("num").Success OrElse m.Groups("cnt").Success OrElse InStr(Imaginary + "(", m.Value) OrElse
                    m.Groups("fn").Success Then
                    If m.Groups("num").Success Then
                        InsertOptor("^")
                    Else
                        InsertOptor("*")
                    End If
                End If
            ElseIf m.Groups("num").Success Then
                If G10.currBase = Rational.Base.Decimal Then
                    'Dim db As Double
                    'If Not Double.TryParse(m.Value, Globalization.NumberStyles.AllowThousands Or
                    '     Globalization.NumberStyles.AllowExponent Or Globalization.NumberStyles.AllowDecimalPoint, G10.CI, db) Then
                    '    Throw New Exception(String.Format(Msg10.Num(5), errorAt(iC)))
                    'End If
                    'value = New Expression(db)
                    Dim rtn As Rational = Rational.ParseRational(m.Value)
                    value = New Expression(New Complex(rtn, New Rational(0)))
                Else
                    Dim dec As Decimal
                    If Not TryParseHexDecOctBinBaseNum(m.Value, dec) Then
                        Throw New Exception(String.Format(Msg10.Num(5), errorAt(iC)))
                    End If
                    Dim rtnl As New Rational(dec)
                    value = New Expression(New Complex(rtnl, New Rational(0)))
                End If
                Advance()
                If m.Groups("vars").Success OrElse m.Groups("fn").Success OrElse m.Groups("cnt").Success OrElse InStr(Imaginary + "(", m.Value) Then
                    InsertOptor("*")
                End If
            ElseIf m.Groups("vars").Success OrElse
            (vars IsNot Nothing AndAlso vars.ContainsKey(m.Value)) Then
                Dim mclen As Int32 = 1
                Dim expr As Expression = Eval_User_Defined_Fn(m.Value, mclen)
                If expr IsNot Nothing Then
                    value = expr
                    If G10.detail Then DoDetail(ic0, mclen, value, True, False)
                ElseIf vars IsNot Nothing AndAlso vars.ContainsKey(m.Value) Then
                    value = New Expression(vars(m.Value))
                    Dim originalValue As New Expression(value)
                    Dim pE As New ParseExpression
                    Dim sVar As String = m.Value
                    Dim b As Boolean = G10.mathml
                    G10.mathml = False
                    Dim value2 As New Expression(value)
                    Dim sValues(0) As Expression
                    Dim nIter As Int32 = 0
                    sValues(nIter) = value2
                    Dim sV2 As String = value2.ToString
                    If Left(sV2, 1) = "-" Then sV2 = Mid(sV2, 2)
                    Do While (vars.ContainsKey(sV2) OrElse sValues(nIter) <> value) AndAlso nIter < 20
                        nIter += 1
                        value = value2
                        value2 = pE.Evaluate(vars, value.ToString).vExpr(0)
                        ReDim Preserve sValues(nIter)
                        sValues(nIter) = value2
                        For i = 0 To nIter - 1
                            If sValues(i) = sValues(nIter) Then
                                Exit Do ' Likely there is recusivity, as in
                                '         parsing "a @a=b @b=a
                            End If
                        Next
                        sV2 = value2.ToString
                        If Left(sV2, 1) = "-" Then sV2 = Mid(sV2, 2)
                    Loop
                    If vars.ContainsKey(sVar) Then
                        vars.Remove(sVar)
                    End If
                    vars.Add(sVar, originalValue)
                    G10.mathml = b
                    If G10.detail Then DoDetail(ic0, 1, value)
                ElseIf m.Value <> G10.sImg Then
                    value = New Expression(m.Value, 1.0)
                Else
                    value = New Expression(Complex.i)
                End If
                Advance()
                If m.Groups("num").Success OrElse m.Groups("cnt").Success Then
                    InsertOptor("^")
                ElseIf m.Value = "(" OrElse m.Groups("var").Success Then
                    InsertOptor("*")
                End If
            ElseIf m.Groups("cnt").Success Then
                If LCase(m.Value) = "e" Then
                    If Not G10.bTryIrrationals Then
                        value = New Expression(Math.E)
                    Else
                        value = New Expression("e", 1)
                    End If
                ElseIf LCase(m.Value) = "pi" Then
                    If Not G10.bTryIrrationals Then
                        value = New Expression(Math.PI)
                    Else
                        value = New Expression("π", 1)
                    End If
                ElseIf m.Groups("phi").Success Then
                    If Not G10.bTryIrrationals Then
                        value = New Expression(G10.phi)
                    Else
                        value = New Expression("φ")
                    End If
                End If
                Advance()
                Exit Try
            ElseIf InStr("-+", m.Value) Then
                sign = -sign
                value = Token(sign)
            ElseIf m.Groups("der").Success Then
                Dim sVar As String = ""
                If m.Groups("resp").Success Then
                    sVar = m.Groups("resp").Value
                End If
                Dim sgn As Int32 = 1
                If mc(iC).Value <> "(" Then
                    value = Token(sgn)
                    If sgn = -1 Then value.OpChgSign()
                Else
                    Advance()
                    value = AddSubs()
                    Advance()
                End If
                If sVar = "" Then
                    Dim vVar() As String = value.GetVars
                    If vVar.Length Then
                        sVar = vVar(0)
                    Else
                        sVar = "x"
                    End If
                End If
                value = value.Derivative(sVar)
            ElseIf m.Groups("any").Success Then
                Dim s As String = String.Format(Msg10.Num(5), errorAt(iC)) ' unknown token
                Throw New Exception(s)
            End If
            If m.Groups("fn").Success Then
                Dim mFn As Match = m
                Dim m1 As Match = m
                Dim sgn As Int32 = 1
                If mc(iC).Value <> "(" Then
                    value = Token(sgn)
                    If sgn = -1 Then value.OpChgSign()
                Else
                    Advance()
                    value = AddSubs()
                    Advance()
                End If
                If value.IsComplex Then
                    value = New Expression(EvalFn(mFn.Value, value.ToComplex))
                ElseIf mFn.Value = "partialfractions" Then
                    Dim Pa As Polynomial = value.ToPolynomial
                    Pa.replaceConstants()
                    Dim vNum(-1), vDen(-1) As Polynomial
                    Dim vDenExp(-1) As Int64
                    Dim errmsg As String = String.Empty
                    If Pa.resto IsNot Nothing Then
                        Dim mtx As ExprMatrix = Nothing
                        If Not Polynomial.partialFractionDecomposition(
                             New Expression(Pa), vNum, vDen, vDenExp, errmsg) Then
                            Throw New Exception(errmsg)
                        End If

                        Dim vect As New ExprVector()
                        ReDim vect.vExpr(vNum.Length - 1)
                        For i As Int64 = 0 To vNum.Length - 1
                            If vDenExp(i) = 1 Then
                                vect.vExpr(i) = New Expression(vNum(i) / vDen(i))
                            Else
                                vect.vExpr(i) = New Expression(vNum(i) / vDen(i) ^ New Polynomial(vDenExp(i)))
                            End If
                        Next
                        r.vExpr = vect.ToArray

                    Else
                        Throw New Exception(Msg8.num(80))
                    End If
                Else
                    Dim arg As Expression = value
                    Select Case mFn.Value.ToLower
                        Case "csc"
                            value = New Expression(1)
                            value /= New Expression("sin", New Expression(1.0), arg)
                        Case "sec"
                            value = New Expression(1)
                            value /= New Expression("cos", New Expression(1.0), arg)
                        Case "cot"
                            value = New Expression("cos", New Expression(1.0), arg)
                            value /= New Expression("sin", New Expression(1.0), arg)
                        Case "csch"
                            value = New Expression(1)
                            value /= New Expression("sinh", New Expression(1.0), arg)
                        Case "sech"
                            value = New Expression(1)
                            value /= New Expression("cosh", New Expression(1.0), arg)
                        Case "coth"
                            value = New Expression("cosh", New Expression(1.0), arg)
                            value /= New Expression("sinh", New Expression(1.0), arg)
                        Case "tan"
                            value = New Expression("sin", New Expression(1.0), arg)
                            value /= New Expression("cos", New Expression(1.0), arg)
                        Case Else
                            value = New Expression(mFn.Value.ToLower, New Expression(1.0), arg)
                    End Select
                End If
            ElseIf m.Groups("fct").Success Then
                Dim sFn As String = LCase(m.Value)
                Dim str As String = ""
                m = mc(iC + 1)
                For iC = iC To mc.Count - 2
                    str += mc(iC).Value
                Next
                Dim pos As Int32 = InStr(str, "&")
                If pos > 0 Then str = Left(str, pos - 1)
                Dim r2 As New parseMatrix.Result
                If Not (m.Groups("num").Success OrElse
                        m.Groups("var").Success) Then

                    Dim pM As New parseMatrix
                    str = parseMatrix.removeStartEndParentheses(str)
                    r2.eMtx = pM.Evaluate(str).eMtx
                End If
                If sFn = "roots" Then
                    Dim bDetail As Boolean = G10.detail
                    G10.detail = False
                    value = r2.eMtx.Item(0, 0) ' = AddSubs()
                    If value Is Nothing Then
                        Throw New Exception(String.Format(Msg10.Num(5), errorAt(iC)))
                    End If
                    If value.IsPolynomial Then
                        Dim Pa As Polynomial = value.ToPolynomial
                        Pa.replaceConstants()
                        vRoots = Pa.RootsNumerator
                    Else
                        value = ReduceExprUsingPolynomials.TryToReduce(value)
                        vRoots = value.Roots_Muller_Method
                    End If
                    G10.detail = bDetail
                Else
                    str = r2.eMtx.ToString
                    Dim i As Int32

                    If Regex.IsMatch(sFn, "factors|factorize") Then ' cnt = 1 Then
                        Dim vStr(1) As String
                        Dim lp As Int32 = Regex.Match(str, "[^(]").Index
                        If lp Then
                            Dim s As String = Replace(str, ")", "@", 1, lp)
                            Dim rp As Int32 = InStrRev(s, "@")
                            vStr(0) = Left(str, rp)
                            lp = InStr(rp + 1, str, "/")
                            If lp Then
                                vStr(1) = Mid(str, lp + 1)
                            Else
                                ReDim Preserve vStr(0)
                            End If
                        Else
                            vStr = Regex.Split(str, "\/|÷")
                        End If
                        Dim parse0 As New ParseExpression(G10.CI)
                        Dim r0 As Result = parse0.Evaluate(vStr(0))
                        Dim expr As Expression = r0.vExpr(0)
                        If expr.IsPolynomial Then
                            Dim p0 As Polynomial = expr.ToPolynomial
                            p0.replaceConstants()
                            vPoly = Polynomial.Factorize(p0, Decimals)
                            iC = mc.Length - 1
                        Else
                            Throw New ArgumentOutOfRangeException
                        End If
                        If vStr.Length > 1 Then
                            Dim parse1 As New ParseExpression(G10.CI)
                            Dim expr1 As Expression = parse1.Evaluate(vStr(1)).vExpr(0)
                            If expr1.IsPolynomial Then
                                Dim p1 As Polynomial = expr1.ToPolynomial
                                p1.replaceConstants()
                                Dim vpoly2() As Polynomial = Polynomial.Factorize(p1, G10.nDec)
                                i = vPoly.Length
                                Dim j As Int32 = vpoly2.Length
                                ReDim Preserve vPoly(i + j)
                                Array.Copy(vpoly2, 0, vPoly, i + 1, j)

                                iC = mc.Length - 1
                            Else
                                Throw New ArgumentOutOfRangeException
                            End If
                        End If
                    ElseIf sFn = "gcd" Then
                        Dim gcd As Polynomial = New Polynomial(r2.eMtx.Item(0, 0).ToPolynomial)
                        For i = 1 To r2.eMtx.Columns(0) - 1
                            Dim p As Polynomial = r2.eMtx.Item(0, i).ToPolynomial
                            gcd = Polynomial.opGcd(gcd, p)
                        Next
                        vPoly = Polynomial.Factorize(gcd, G10.nDec)
                        iC = mc.Length - 1
                    ElseIf sFn = "lcm" Then
                        Dim lcm As Polynomial = New Polynomial(r2.eMtx.Item(0, 0).ToPolynomial)
                        For i = 1 To r2.eMtx.Columns(0) - 1
                            Dim p As Polynomial = r2.eMtx.Item(0, i).ToPolynomial
                            lcm = Polynomial.opLCM(lcm, p)
                        Next
                        vPoly = Polynomial.Factorize(lcm, G10.nDec)
                        iC = mc.Length - 1
                    End If
                End If
                Exit Try
            ElseIf m.Value = "," OrElse m.Groups("other").Success Then
                Throw New Exception(errorAt(iC - 1) + " " + Msg10.Num(119))
            ElseIf regex.IsMatch(m.Value, "[-+*/^!]") AndAlso regex.IsMatch(mc(iC).Value, "[*/^!]") Then
                Throw New Exception(errorAt(iC - 1, 1))
            ElseIf iC = 1 AndAlso m.Groups("op").Success AndAlso
                Not Regex.IsMatch(m.Value, "[-+]") Then
                Throw New Exception(errorAt(iC - 1))
            ElseIf iC + 1 >= mc.Count AndAlso m.Groups("op").Success Then
                Throw New Exception(errorAt(iC - 1))
            ElseIf m.Groups("lop").Success Then
                If m.Value = "!" Then
                    If value Is Nothing OrElse Not value.IsDouble Then
                        Throw New Exception(String.Format(Msg10.Num(5), errorAt(iC)))
                    End If
                    Dim db As Double = value.ToDouble
                    If db <> Math.Floor(db) Then
                        Throw New Exception(String.Format(Msg10.Num(5), errorAt(iC)))
                    End If
                    For i As Int32 = db - 1 To 2 Step -1
                        db *= i
                    Next
                    value = New Expression(db)
                    Advance()
                    Exit Try
                End If
                Dim mFn As Match = m
                Dim sOptor As String = LCase(m.Value)
                Dim dbA As Double
                If sOptor <> "not" Then
                    If Not value.IsDouble Then
                        Throw New ArgumentOutOfRangeException
                    End If
                    dbA = value.ToDouble
                End If
                Dim dbB As Double
                Dim sgn As Int32 = 1
                Dim value3 = Token(sgn)
                If sgn = -1 Then
                    value3.OpChgSign()
                End If
                If Not value3.IsDouble Then
                    Throw New ArgumentOutOfRangeException
                End If
                dbB = value3.ToDouble
                Select Case sOptor
                    Case "nand" : value = New Expression(Not (dbA And dbB))
                    Case "and" : value = New Expression(dbA And dbB)
                    Case "mod", "%" : value = New Expression(dbA Mod dbB)
                    Case "nor" : value = New Expression(Not (dbA Or dbB))
                    Case "xor" : value = New Expression(dbA Xor dbB)
                    Case "not" : value = New Expression(Not dbB) : ic0 += 1
                    Case "or" : value = New Expression(dbA Or dbB)
                End Select
            End If
            Do While Left(m.Value, 1) = "&"
                setModes()
                Advance()
            Loop
            If value Is Nothing Then
                Throw New Exception(String.Format(Msg10.Num(5), errorAt(iC)))
            End If
        Catch ex As Exception
            Throw
        End Try
        Return value
    End Function
    Function Eval_User_Defined_Fn(strFn As String, Optional ByRef mcLen As Int32 = 0) As Expression
        Dim ret As Expression = Nothing
        Static nCall As Int32 = 0
        Dim bDetail As Boolean
        Try
            If nCall = 0 Then
                bDetail = G10.detail
                G10.detail = False
            End If
            nCall += 1
            Dim sFn As String = strFn
            Dim sMtxVars As String = ""
            Dim expr As Expression = Nothing
            If vars Is Nothing OrElse vars.Count = 0 Then Exit Try
            ' Find in mtxVars user defined function strFn:
            For i As Int32 = 0 To vars.Count - 1
                Dim vsB() As String = Regex.Split(vars.Keys(i), "\(|\)")
                If vsB.Length > 1 Then
                    ' is a user function like for ex. f(x,y) = (x+y;x-y)
                    If sFn = vsB(0).Replace(" ", "") Then
                        sFn = vars.Keys(i)
                        expr = vars.Values(i) : Exit For
                    End If
                End If
            Next
            If expr Is Nothing Then
                Exit Try
            End If
            Dim mc2(mc.Count - 1) As Match
            Array.Copy(mc, mc2, mc.Count)
            Dim cpyVars As Dictionary(Of String, Expression) = vars
            vars = New Dictionary(Of String, Expression)
            Dim fnVars As New Dictionary(Of String, Expression)
            Dim vsFn() As String = Regex.Split(sFn, "\(|\)")
            Dim sFnParams() As String = Split(vsFn(1), ",")
            Dim iC2 As Int32 = iC
            iC = iC2
            Dim sParams As String = ""
            Dim lp As Int32 = 0
            Dim rp As Int32 = 0
            mcLen = 1
            Do
                Advance()
                If m.Value = "(" Then lp += 1
                If m.Value = ")" Then rp += 1
                sParams += m.Value
                mcLen += 1
            Loop While lp <> rp
            iC2 = iC
            Dim vsP() As String = Regex.Split(sParams, "\(|\)")
            Dim vParams() As String = Split(vsP(1), ",")
            If sFnParams.Length <> vParams.Length Then
                Throw New Exception(String.Format(Msg10.Num(114), sFn, sParams))
            End If
            For j As Int32 = 0 To sFnParams.Length - 1
                Dim exprParam As Expression = Evaluate(Nothing, vParams(j)).vExpr(0)
                Dim snP As String = sFnParams(j).Replace(" ", "")
                If snP <> exprParam.ToString Then
                    If fnVars.ContainsKey(snP) Then
                        fnVars.Remove(snP)
                    End If
                    fnVars.Add(snP, exprParam)
                End If
            Next
            Dim seM As String = "(" + expr.ToString + ")"
            iC = 0
            ret = Evaluate(fnVars, seM).vExpr(0)
            ReDim mc(mc2.Length - 1)
            Array.Copy(mc2, mc, mc2.Count)
            vars = cpyVars
            iC = iC2

        Catch ex As Exception
            Throw
        Finally
            nCall -= 1
            If nCall = 0 Then
                G10.detail = bDetail
            End If
        End Try
        Return ret
    End Function

    Private Function errorAt(iErr As Int32, Optional length As Int32 = 0) As String
        Dim s As String = ""
        Try
            Dim j As Int32
            For j = 0 To iErr - 1
                If Left(mc(j).Value, 1) = "(" Then
                    s += "("
                Else
                    s += mc(j).ToString
                End If
            Next
            If Left(mc(j).Value, 1) = "(" Then
                s += " [ <span style='color:red'>(</span> ] "
            Else
                s += " [ <span style='color:red'>"
                For j = j To j + length
                    s += mc(j).ToString()
                Next
                s += "</span> ] "
            End If
            For j = j To mc.Count - 2
                If Left(mc(j).Value, 1) = "(" Then
                    s += "("
                Else
                    s += mc(j).ToString
                End If
            Next
            s = s.Replace("←", " ").Replace("|", "<br />")
        Catch ex As Exception

        End Try
        s = "<h3>" + Msg8.num(13) + "</h3><br />" + s
        Return s
    End Function

#End Region


    Public Const rad2Degrees As Double = 180 / Math.PI
    Public Const rad2Centesimal As Double = 200 / Math.PI
    Public Const degree2Radians As Double = Math.PI / 180
    Public Const centesimal2Rad As Double = Math.PI / 200
    Public Shared Function EvalFn(
                    ByVal fn As String,
                    ByVal cjoA As Complex) As Complex
        Static vTrigonFns() As String =
            New String() {
            "sin", "cos", "tan",
            "sinh", "cosh", "tanh",
            "csc", "sec", "cot",
            "csch", "sech", "coth"
            }
        Dim rCjo As Complex = Nothing
        Try
            fn = LCase(fn)
            Dim bArgNotValid As Boolean = False
            If G10.angleMode = G10.AngleModes.degree Then
                cjoA *= New Complex(degree2Radians)
            ElseIf G10.angleMode = G10.AngleModes.gradian Then
                cjoA *= New Complex(centesimal2Rad)
            End If
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
                        Case "log" : rCjo = Complex.opLn(cjoA) ' log()= ln()
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
                Throw New ArgumentOutOfRangeException
            End If
        Catch ex As Exception
            Throw
        End Try
        Return rCjo
    End Function
    Private Function TryParseHexDecOctBinBaseNum(str As String, ByRef result As Decimal) As Boolean
        Dim Ent As Long = 0
        Dim Fra As Double = 0.0
        Dim sPattern As String = ""
        Dim base As Int64 = 10
        Dim bRet As Boolean = False
        Dim ns As Globalization.NumberStyles
        Dim sDot As String = CultureInfo.NumberFormat.NumberDecimalSeparator
        Try
            Select Case G10.currBase
                Case Rational.Base.Hexadecimal
                    sPattern = "[^.0-9abcdef]"
                    base = 16
                    ns = Globalization.NumberStyles.HexNumber
                Case Rational.Base.Decimal
                    sPattern = "[^.0-9]"
                    base = 10
                    ns = Globalization.NumberStyles.Integer
                Case Rational.Base.Octal
                    sPattern = "[^.0-7]"
                    base = 8
                Case Rational.Base.Binary
                    sPattern = "[^.01]"
                    base = 2
            End Select
            str = IIf(str.Chars(0) = "&", Mid(str, 3), str)
            If Regex.IsMatch(str, "(?i)" + sPattern + "(-i)") Then
                Exit Try
            End If
            Dim posDot As Int64 = InStr(str, sDot)
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
                        ' ascw("A")=65 ==> ascw("A")-55=10, ascw("B")-55=11,... ascw("F")-55=15
                        curEnt = AscW(UCase(str.Chars(j))) - 55
                    End If
                End If
                If j < posDot Then
                    Ent = Ent * base + curEnt
                ElseIf j > posDot Then
                    Fra = Fra + curEnt / base ^ (j - posDot)
                End If
            Next
            result = CDec(Ent) + CDec(Fra)
            bRet = True
        Catch ex As Exception
            Throw
        End Try
        Return bRet
    End Function
#Region "g10.detail"
    Private Sub DoDetail(iC As Int32, nElements As Int32,
                         expr As Expression, Optional dsp As Boolean = True, Optional countLP As Boolean = True)
        Dim bMathML As Boolean = G10.mathml
        Try
            G10.mathml = False
            Dim i As Int32 = 0
            If oD.Count = 0 Then
                For i = 0 To mc.Count - 1
                    oD.Add(New oDetail(i, i, mc(i), mc.ToArray))
                Next
            End If

            ReDim Preserve vsDetail(ivsD)
            vsDetail(ivsD) = ""
            i = 0
            Do While oD(i).y < iC
                If oD(i).m.Value = "-" AndAlso oD(i).y >= iC Then
                    Exit Do
                End If
                If dsp Then
                    If InStr(oD(i).m.Value, "&") = 0 Then
                        If oD(i).m.Value = "/" Then
                            Dim m As Match = Regex.Match(vsDetail(ivsD), "[-+]")
                            If m.Success AndAlso Right(vsDetail(ivsD), 1) <> ")" Then
                                vsDetail(ivsD) = "(" + vsDetail(ivsD) + ")"
                            End If
                        End If
                        vsDetail(ivsD) += oD(i).m.Value
                    End If
                End If
                i += 1
            Loop
            If dsp Then
                vsDetail(ivsD) += "["
            End If
            Dim ifirst As Int32 = i
            Dim x As Int32 = oD(i).x
            Dim cnt As Int32 = nElements
            Do
                If dsp Then
                    If InStr(oD(i).m.Value, "&") Then
                        cnt += 1
                    Else
                        vsDetail(ivsD) += oD(i).m.Value
                    End If
                End If
                If countLP AndAlso Regex.IsMatch(oD(i).m.Value, "\(") Then cnt += 2
                i += 1
                cnt -= 1
            Loop While cnt AndAlso i < oD.Count - 1
            If dsp Then
                vsDetail(ivsD) += "]"
            End If
            Dim ilast As Int32 = i - 1
            Dim y As Int32 = oD(i - 1).y
            Do While i < oD.Count - 1
                If dsp Then
                    If InStr(oD(i).m.Value, "&") = 0 Then
                        vsDetail(ivsD) += oD(i).m.Value
                    End If
                End If
                i += 1
            Loop
            For i = ifirst To ilast
                oD.RemoveAt(ifirst)
            Next
            oD.Insert(ifirst, New oDetail(x, y, Regex.Match(expr.ToString, ".*"), mc.ToArray))
            If dsp Then
                ivsD += 1
                ReDim Preserve vsDetail(ivsD)
                vsDetail(ivsD) = ""
                For i = 0 To oD.Count - 2
                    If i = ifirst Then vsDetail(ivsD) += "@@" ' "<span style='color:red;'>"
                    If InStr(oD(i).m.Value, "&") = 0 Then
                        vsDetail(ivsD) += oD(i).m.Value
                    End If
                    If i = ifirst Then vsDetail(ivsD) += "@" '"</span>"
                Next
                ivsD += 1
            End If
            If ivsD Then
                Dim s1 As String = Regex.Replace(vsDetail(ivsD - 2), "\[|\]", "")
                Dim s2 As String = Regex.Replace(vsDetail(ivsD - 1), "@", "")
                If s1 = s2 Then
                    ivsD -= 2
                    ReDim Preserve vsDetail(ivsD)
                Else
                    vsDetail(ivsD - 2) = vsDetail(ivsD - 2).Replace("[", "<span style='color:red;font-weight:bold;'>").Replace("]", "</span>@").Replace("@", "")
                    If oD.Count > 2 Then
                        vsDetail(ivsD - 1) = vsDetail(ivsD - 1).Replace("@@", "<span style='color:black;font-weight:bold'>").Replace("@", "</span>")
                    Else
                        vsDetail(ivsD - 1) = vsDetail(ivsD - 1).Replace("@@", "<span style='color:black;font-weight:900;font-size:120%'>").Replace("@", "</span>")
                    End If
                End If
            End If
        Catch ex As Exception
        Finally
            G10.mathml = bMathML
        End Try
    End Sub

    Dim lstPre As New List(Of String)
    Dim lstPost As New List(Of String)
    Private Sub DoDetail2(iPre As Int32, iPost As Int32, result As Polynomial, Optional sAdd As String = "", Optional sResto As String = "")
        Try
            If sAdd Is Nothing Then sAdd = ""
            Dim sResult As String = result.ToString
            If sResto <> "" Then
                If sResto.Chars(0) <> "-" Then
                    sResto = "+" + sResto
                End If
                sResult += sResto
            End If
            Dim sPre As String = String.Empty
            For i As Int32 = 0 To iPre - 1
                sPre += mc(i).Value
            Next
            Dim sInt As String = String.Empty
            For i As Int32 = iPre To iPost
                sInt += mc(i).Value
                mc(i) = Regex.Match("", ".")
            Next
            mc(iPre) = Regex.Match(sResult, ".*")
            Dim sPost As String = String.Empty
            For i As Int32 = iPost + 1 To mc.Length - 2
                sPost += mc(i).Value
            Next
            Dim s1 As String = sPre + " [ " + sInt + " ] " + sPost
            Dim s2 As String = sPre + " [ " + sResult + " ] " + sPost
            If Replace(s1, " ", "") <> Replace(s2, " ", "") Then
                If sAdd.Length Then
                    Dim s As String = ""
                    Dim nTd As Int32 = 0
                    Dim vAdd() As String = Split(sAdd, vbCrLf)
                    If InStr(vAdd(0), "|") Then
                        ' Give format to a division:
                        Dim t As String = "<table border='1' bordercolor='#d0d0d0' cellspacing='1' cellpadding='1'>"
                        For i = 0 To vAdd.Length - 1
                            t += "<tr align='center'>"
                            If Left(vAdd(i), 4) = "----" Then
                                t += "<td style='white-space:nowrap;' colspan='" + nTd.ToString + "'><hr></td>"
                            Else
                                nTd = 0
                                Dim td() As String = Split(vAdd(i), "|")
                                For j = 0 To td.Length - 1
                                    t += "<td style='white-space:nowrap;'>" + td(j) + "</td>"
                                    nTd += 1
                                Next
                            End If
                            t += "</tr>"
                        Next
                        t += "</table>"
                        lstPre.Add(t)
                        lstPost.Add("<span style='color:red;'>" + sResult + "</span>")
                    Else
                        Dim s3(vAdd.Length - 1) As String
                        For i As Int32 = 0 To vAdd.Length - 1
                            s3(i) = Regex.Replace(s1, "\[[^\]]+\]", "[" + vAdd(i) + "]")
                            s3(i) = Replace(s3(i), "+-", "-")
                        Next
                        If Replace(s3(s3.Length - 1), " ", "") = Replace(s2, " ", "") Then
                            ReDim Preserve s3(s3.Length - 2)
                        End If
                        lstPre.Add(Join(s3, vbCrLf))
                        lstPost.Add(s2)
                    End If
                Else
                    lstPre.Add(s1)
                    lstPost.Add(s2)
                End If
            End If
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Function GetDetail() As String
        Dim sb As New StringBuilder
        Try
            For i = 0 To lstPre.Count - 1
                sb.Append(lstPre(i) + vbCrLf)
                sb.Append(lstPost(i) + vbCrLf)
            Next
        Catch ex As Exception
            Throw
        End Try
        Return sb.ToString
    End Function

#End Region


End Class
