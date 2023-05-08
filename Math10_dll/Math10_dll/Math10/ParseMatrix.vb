Imports System.Linq.Expressions
Imports System.Text.RegularExpressions
'Imports System.Windows
'Imports System.Windows.Input

Public Class parseMatrix
    Public Class Result
        Public eMtx As ExprMatrix
        Public sDetail As String
    End Class
#Region "Declarations"
    Dim m, mc() As Match, mc1 As MatchCollection
    Dim sExpr As String
    Dim iC As Int32
    Public vars As New Dictionary(Of String, Expression)
    Public mtxVars As New Dictionary(Of String, ExprMatrix)
    Public vRoots(-1) As Complex, vPoly(-1) As Polynomial
    Dim ivsD As Int32, vsDetail() As String
    Dim r As New Result
    Public sDetail As String
    Dim vMtx(-1) As ExprMatrix
    Dim strForOutputFormat As String = ""

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
    'Public Async Function EvaluateAsync(strExpression As String, Optional vars As Dictionary(Of String, ExprMatrix) = Nothing) As Task(Of String())
    '    Dim sRet(1) As String
    '    Try
    '        CheckParenthesesMatched(strExpression)

    '        Dim r As Result = Nothing
    '        Dim action As Action(Of Object) =
    '            Sub(str As Object)
    '                r = Evaluate(vars, str)
    '            End Sub
    '        Dim t1 As New Task(action, strExpression)
    '        t1.Start()
    '        Await t1
    '        Dim mcFormat As MatchCollection = Regex.Matches(strForOutputFormat, G10.sModes + "|" + G10.RowOrColSeparator)
    '        'sRet(0) = PrepareOutput(r, mcFormat)
    '        If r.sDetail IsNot Nothing Then
    '            sRet(1) = r.sDetail
    '        Else
    '            sRet(1) = ""
    '        End If
    '    Catch ex As Exception
    '        sRet(0) = ex.Message
    '        sRet(1) = ""
    '    End Try
    '    Return sRet
    'End Function
    Sub CheckParenthesesMatched(s As String)
        Dim LP As Int32 = Regex.Matches(s, "\(").Count
        Dim RP As Int32 = Regex.Matches(s, "\)").Count
        If LP > RP Then
            Throw New Exception(String.Format(Msg10.Num(101), ")"))
        ElseIf LP < RP Then
            Throw New Exception(String.Format(Msg10.Num(101), "("))
        End If
        Do
            Dim m1 As Match = Regex.Match(s, "\((?<m>[^()]+)\)")
            If Not m1.Success Then Exit Do
            s = s.Remove(m1.Index, m1.Length)
            s = s.Insert(m1.Index, StrDup(m1.Length, " "))
        Loop
        If Regex.IsMatch(s, "\)|\(") Then
            Throw New Exception(Msg10.Num(118))
        End If
    End Sub
    Private Function PrepareOutput(r As Result, mcFormat As MatchCollection) As String
        Dim s As String = ""
        Try
            Dim k As Int32 = 0
            Dim vRow(r.eMtx.Rows - 1) As String
            Dim mathmlMixed As Boolean = False
            For i As Int32 = 0 To mcFormat.Count - 1
                If InStr(mc(i).Value.ToLower, "mathml") = 0 Then
                    mathmlMixed = True
                    Exit For
                End If
            Next
            For i As Int32 = 0 To r.eMtx.Rows - 1
                Dim vCol(r.eMtx.Columns(i) - 1) As String ' = Split(vRow(i), ";")
                For j As Int32 = 0 To r.eMtx.Columns(i) - 1
                    Do While k < mcFormat.Count AndAlso mcFormat(k).Groups("sep").Success = False
                        m = mcFormat(k)
                        setModes()
                        k += 1
                    Loop
                    If G10.mathml AndAlso G10.currBase <> Rational.Base.Hexadecimal Then
                        vCol(j) = ConvertStringToMathML.ConvertToMathML(r.eMtx.Item(i, j).ToString)
                        If G10.currBase <> Rational.Base.Decimal Then
                            vCol(j) = vCol(j).Replace(">h", ">&amp;h")
                            vCol(j) = vCol(j).Replace(">o", ">&amp;o")
                        End If
                        If mathmlMixed Then
                            vCol(j) = "<math>" + vCol(j) + "</math>"
                        End If
                    Else
                        vCol(j) = r.eMtx.Item(i, j).ToString
                    End If
                    If k < mcFormat.Count AndAlso mcFormat(k).Groups("col").Success Then
                        k += 1
                    End If
                Next
                If G10.mathml AndAlso G10.currBase <> Rational.Base.Hexadecimal AndAlso Not mathmlMixed Then
                    vRow(i) = "<mtd>" + Join(vCol, "</mtd><mtd>") + "</mtd>"
                Else
                    vRow(i) = "<td>" + Join(vCol, "</td><td>") + "</td>"
                End If
                k += 1
            Next
            If G10.mathml AndAlso G10.currBase <> Rational.Base.Hexadecimal AndAlso Not mathmlMixed Then
                s = "<math><mtable><mtr>" + Join(vRow, "</mtr><mtr>") + "</mtr></mtable></math>"
            Else
                s = "<table class='center'><tr>" + Join(vRow, "</tr><tr>") + "</tr></table>"
                's = s.Replace("<table>", "<table class='center'>")
                's = s.Replace("<td>", "<td class='mytd'>")
            End If
        Catch ex As Exception
            Throw
        End Try
        Return s
    End Function
    Public Function Evaluate(mtxVars As Dictionary(Of String, ExprMatrix), sMtx As String) As Result
        Dim r As New Result
        Try
            iC = 0
            If mtxVars IsNot Nothing Then Me.mtxVars = mtxVars
            ReDim vRoots(-1), vPoly(-1)
            ReDim vsDetail(-1)
            r = Evaluate(sMtx)
            For i As Int32 = 0 To r.eMtx.Rows - 1
                For j As Int32 = 0 To r.eMtx.Columns(i) - 1
                    r.eMtx.Item(i, j) = ReduceExprUsingPolynomials.TryToReduce(r.eMtx.Item(i, j))
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return r
    End Function
    Public Function Evaluate(sMtx As String) As Result
        Dim ret As New parseMatrix.Result
        Dim r As New ExprMatrix
        Try
            If sMtx.Length = 0 Then
                Throw New Exception(Msg10.Num(100)) ' empty
            End If
            G10.Initialize()
            sExpr = G10.reSubstitute.Replace(sMtx, AddressOf Substitute)

            ' Remove starting and trailing |:
            sExpr = sExpr.Replace(vbCr + vbCrLf, "|").Replace(vbCrLf, "|").Replace(vbLf, "").Replace("||", "|").Replace(ChrW(9), ";")
            sExpr = removeComments(sExpr)
            sExpr = Regex.Replace(sExpr, "\+\s*\-", "-")
            sExpr = Regex.Replace(sExpr, "^\|*|\|*$", "") ' 
            'G10.detail = False
            SetMtxVarsAndVars(sExpr)

            'sExpr = Regex.Replace(sExpr, "\|\s*@\s*(?<vars>[\w(),\d]+)\s*=\s*(?<val>[^@←]+)", "")
            'sExpr = Regex.Replace(sExpr, "@\s*\|+", "")
            'sExpr = Regex.Split(sExpr, "\|@")(0)
            sExpr = Regex.Split(sExpr, "@")(0)
            sExpr = Regex.Replace(sExpr, G10.sModes + "\s*\|+", "${mode} ")
            If (Regex.IsMatch(sExpr, "(\)\s*\|)$") AndAlso InStr(sExpr, "(|") = 0) _
            OrElse Right(sExpr, 1) = "|" Then
                sExpr = Left(sExpr, Len(sExpr) - 1)
            End If


            If (Not IsItAMatrixExpression(sExpr) OrElse InStr(sExpr, "=")) Then 'AndAlso Not forceToParseMatrix Then

                ' ************* Not a matrix ************

                sExpr = Regex.Replace(sExpr, "(?<op>[-+*/÷^(])\|", "${op}")
                Dim pOMtx As New ParseOneMatrix
                Dim r1 As ParseOneMatrix.Result = pOMtx.Evaluate(vars, sExpr)(0)(0)
                If pOMtx.HasAtLeastOneEquation Then
                    If pOMtx.HasAtleastOneNotEquation Then
                        Throw New Exception(Msg8.num(53))
                    End If
                    r1.eMtx.replaceConstants()
                    r1.eMtx = r1.eMtx.ConvertrigonometricToExp
                    Dim rows As Int32 = r1.eMtx.Rows
                    Dim cols As Int32 = r1.eMtx.Columns(0)
                    If rows + cols = 2 Then
                        ' One equation
                        Dim expr As Expression = r1.eMtx.Item(0, 0)
                        If expr.IsPolynomial Then
                            Dim poly As Polynomial = expr.ToPolynomial
                            Dim vVars() As String = poly.GetVars
                            If vVars.Length = 1 Then
                                Dim vRoots() As Complex = poly.RootsNumerator  ' one only variable
                                r1.eMtx = New ExprMatrix(vRoots.Length, 2)
                                For i As Int32 = 0 To vRoots.Length - 1
                                    If vRoots.Length > 1 Then
                                        r1.eMtx.Item(i, 0) = New Expression(vVars(0) + "_" + (i + 1).ToString + "=", 1)
                                    Else
                                        r1.eMtx.Item(i, 0) = New Expression(vVars(0) + "=", 1)
                                    End If
                                    r1.eMtx.Item(i, 1) = New Expression(vRoots(i))
                                Next
                                r = r1.eMtx
                            ElseIf vVars.Length > 1 Then
                                Throw New Exception(String.Format(Msg8.num(69), expr.ToString, Join(poly.GetVars, ",")))
                            End If
                        ElseIf expr.GetVars.Length = 1 Then
                            Dim vRoots() As Complex = expr.Roots_Muller_Method()
                            r = New ExprMatrix(vRoots)
                            r = ExprMatrix.opTranspose(r)
                        End If
                    ElseIf r1.eMtx.IsMatrixOfLinearEqs Then
                        r = ExprMatrix.resolveLinearSystemOfEquations(r1.eMtx)
                    Else
                        ' non-linear system of equations
                        If SystemOfEquations.Newton_Raphson(r1.eMtx) Then
                            r = r1.eMtx
                        Else
                            Throw New Exception(Msg8.num(13)) ' n/a
                        End If
                    End If
                ElseIf r1.eMtx.Rows = 0 Then
                    r = New ExprMatrix(New Expression(" ", 1))
                Else
                    r = r1.eMtx
                End If
                If G10.detail AndAlso vsDetail IsNot Nothing Then
                    sDetail = Join(r1.vsDetail, vbCrLf)
                End If
                Exit Try

                ' ************* End Not a matrix ************
            End If
            strForOutputFormat = sExpr
            If InStr(sExpr, "(") = 0 Then
                Get_mc("(" + sExpr + ")")
            Else
                Get_mc(sExpr)
            End If
            iC = 0
            r = extractMatrix(ChrW(27))
        Catch ex As Exception
            If G10.bCancel Then
                G10.bCancel = False
                sDetail = "Cancelled"
            Else
                Throw
            End If
        Finally
            ret.eMtx = r
            ret.sDetail = sDetail
        End Try
        Return ret
    End Function
    Private Function removeComments(s As String) As String
        s = Regex.Replace(s, "/\*(.)*\*/|'[^|]*|", "")
        s = Regex.Replace(s, "<[^>]+>", "")
        Return s
    End Function

    Sub Get_mc(s As String)
        Try
            's = Regex.Replace(s, G10.sOp + "\|", "${op}") 
            s = Regex.Replace(s, "^\|*", "")
            Dim sMtxVars As String = GetMtxVars(s)
            Dim pattern As String = G10.sMtxTot + sMtxVars
            mc1 = Regex.Matches(s, pattern)
            Dim mc2(mc1.Count - 1) As Match
            mc1.CopyTo(mc2, 0)
            ReDim vMtx(mc2.Length - 1)
            ReDim mc(mc2.Length - 1)
            Dim lpCount As Int32 = Regex.Matches(s, "\(").Count
            Dim lp As Int32 = 0
            Dim rp As Int32 = 0
            Dim curr As Int32 = lpCount
            Do
                Dim m1 As Match = Regex.Match(s, "\((?<m>[^()]+)\)")
                If Not m1.Success Then Exit Do
                s = s.Remove(m1.Index, m1.Length)
                s = s.Insert(m1.Index, StrDup(m1.Length, " "))
                If InStr(m1.Value, ",") Then
                    Dim mc3 As MatchCollection = G10.reTot.Matches(m1.Value)
                    For Each m In mc3
                        If m.Value = "," Then Continue Do
                    Next
                End If
                Dim iStart As Int32
                For iStart = 0 To mc2.Count - 1
                    If mc2(iStart).Index = m1.Index Then Exit For
                Next
                Dim iEnd As Int32
                For iEnd = 0 To mc2.Count - 1
                    If mc2(iEnd).Index = m1.Index + m1.Length - 1 Then Exit For
                Next
                mc2(iStart) = Regex.Match("(" + iEnd.ToString, ".*")
                Array.Copy(mc2, 0, mc, 0, mc2.Count)
                iC = iStart + 1
                Try
                    Dim eMtx As ExprMatrix = extractMatrix("\)")
                    vMtx(iStart) = eMtx
                Catch ex As Exception
                    Dim addMoreTokens As Int32 = 0
                    If Int32.TryParse(ex.Message, addMoreTokens) Then
                        mc1.CopyTo(mc, 0)
                        Throw New Exception(errorAt(iC - 1, addMoreTokens))
                    Else
                        Throw
                    End If
                End Try
                For i As Int32 = iStart + 1 To iEnd
                    mc2(i) = Regex.Match("", ".")
                Next
            Loop
            iC = 0
            mc = mc2
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Function ReplaceVarsByValues(s As String) As String
        Dim s1 As String = ""
        Try
            If mtxVars.Count = 0 Then
                Return s
            End If
            Dim sMtxVars As String = ""
            For i As Int32 = 0 To mtxVars.Count - 1
                Dim vs() As String = Regex.Split(mtxVars.Keys(i), "\(|\)")
                Dim sFn As String = vs(0).Replace(" ", "")
                sMtxVars += sFn + IIf(i < mtxVars.Count - 1, "|", "")
            Next
            mc1 = Regex.Matches(s, G10.sMtxTot + "|" + sMtxVars)
            Dim pos As Int32 = 0
            For Each m In mc1
                If pos < m.Index Then
                    s1 += s.Substring(pos, m.Index - pos)
                    pos = m.Index
                End If
                If mtxVars.ContainsKey(m.Value) Then
                    s1 += "(" + mtxVars(m.Value).ToString_eMtx(False) + ")"
                Else
                    s1 += m.Value
                End If
                pos = m.Index + m.Length
            Next
            If pos < s.Length Then
                s1 += s.Substring(pos)
            End If
        Catch ex As Exception
            Throw
        End Try
        Return s1
    End Function
    Function GetMtxVars(s As String) As String
        Dim sMtxVars As String = ""
        ' Find user defined functions:
        For i As Int32 = 0 To mtxVars.Count - 1
            If Not vars.ContainsKey(mtxVars.Keys(i)) Then
                Dim vs() As String = Regex.Split(mtxVars.Keys(i), "\(|\)")
                If vs.Length > 1 Then
                    ' is a user function like for ex. f(x,y) = (x+y;x-y)
                    Dim sFn As String = vs(0).Replace(" ", "")
                    sMtxVars += sFn + "\(|" ' append the name of the function and "(", i.e. "f("
                End If
            End If
        Next
        If Len(sMtxVars) Then
            sMtxVars = "|(?<usr>" + Left(sMtxVars, Len(sMtxVars) - 1) + ")"
        End If
        Return sMtxVars
    End Function
    Public Shared Function removeStartEndParentheses(s As String) As String
        Try
            Do While Left(s, 1) = "(" AndAlso Right(s, 1) = ")"
                Dim s1 As String = Mid(s, 2, Len(s) - 2)
                Dim lp As Int32 = InStr(s1, "(")
                Dim rp As Int32 = InStr(s1, ")")
                If lp < rp Then
                    s = s1
                Else
                    Exit Do
                End If
            Loop
        Catch ex As Exception

        End Try
        Return s
    End Function

    Public Sub SetMtxVarsAndVars(sVars As String)
        Static enUso As Boolean = False
        Dim b As Boolean = G10.mathml
        G10.mathml = False
        Try
            If enUso Then Exit Sub
            enUso = True
            Dim pM As New parseMatrix
            Dim pOM As New ParseOneMatrix
            Dim pE As New ParseExpression
            Dim mcVars As MatchCollection = Regex.Matches(
                sExpr,
                "(?<=@)\s*(?<vars>[a-zA-Z\d]+)\s*(?<parval>(?<param>[()\w\d,]+)?\s*=\s*(?<val>[^@←]+))",
                RegexOptions.IgnorePatternWhitespace)
            Dim sNameErr As New List(Of String), sValueErr As New List(Of String), ie As Int32
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
                        If mtxVars.ContainsKey(varName) Then
                            mtxVars.Remove(varName)
                        End If
                        mtxVars.Add(varName, New ExprMatrix(r.vExpr(0)))
                    Catch ex As Exception
                    End Try
                End If
            Next
            For i As Int32 = 0 To mcVars.Count - 1
                Dim varValue As String = mcVars(i).Groups("val").Value
                varValue = Regex.Replace(varValue, "\|*$", "") ' remove trailing '|'
                Dim varName As String = mcVars(i).Groups("vars").Value
                If mcVars(i).Groups("param").Success Then
                    ' Is a matrix function, get the parameters and the definition:
                    varName += mcVars(i).Groups("param").Value
                End If
                If Not Regex.IsMatch(varValue, G10.sMtxFn + "|[;|\t]") Then
                ElseIf Not Regex.IsMatch(varValue, G10.sMtxFn + "|" + G10.sMtxOp) Then ' Second, matrix vars. and functions
                    Try
                        Dim r As ParseOneMatrix.Result = pOM.Evaluate(vars, varValue)(0)(0)
                        If mtxVars.ContainsKey(varName) Then
                            mtxVars.Remove(varName)
                        End If
                        mtxVars.Add(varName, r.eMtx)
                        If r.eMtx.Rows = 1 AndAlso r.eMtx.Columns(0) = 1 Then
                            If vars.ContainsKey(varName) Then
                                vars.Remove(varName)
                            End If
                            vars.Add(varName, r.eMtx.Item(0, 0))
                        End If
                    Catch ex As Exception
                        sNameErr.Add(varName)
                        sValueErr.Add(varValue)
                        ie += 1
                    End Try
                Else
                    Try
                        Dim r As parseMatrix.Result = pM.Evaluate(mtxVars, varValue)
                        If mtxVars.ContainsKey(varName) Then
                            mtxVars.Remove(varName)
                        End If
                        If r.eMtx.Rows = 0 Then
                            sNameErr.Add(varName)
                            sValueErr.Add(varValue)
                            ie += 1
                            Continue For
                        End If
                        mtxVars.Add(varName, r.eMtx)
                        If r.eMtx.Rows = 1 AndAlso r.eMtx.Columns(0) = 1 Then
                            If vars.ContainsKey(varName) Then
                                vars.Remove(varName)
                            End If
                            vars.Add(varName, r.eMtx.Item(0, 0))
                        End If
                    Catch ex As Exception
                        sNameErr.Add(varName)
                        sValueErr.Add(varValue)
                        ie += 1
                    End Try
                End If
            Next
            If ie Then
                For nIter As Int32 = 0 To ie - 1
                    For i As Int32 = 0 To ie - 1
                        Try
                            Dim r As parseMatrix.Result = pM.Evaluate(mtxVars, sValueErr(i))
                            If mtxVars.ContainsKey(sNameErr(i)) Then
                                mtxVars.Remove(sNameErr(i))
                            End If
                            If r.eMtx.Rows = 0 Then Continue For
                            mtxVars.Add(sNameErr(i), r.eMtx)
                            If r.eMtx.Rows = 1 AndAlso r.eMtx.Columns(0) = 1 Then
                                If vars.ContainsKey(sNameErr(i)) Then
                                    vars.Remove(sNameErr(i))
                                End If
                                vars.Add(sNameErr(i), r.eMtx.Item(0, 0))
                            End If
                            ie -= 1
                            If ie <= 0 Then Exit For
                        Catch ex As Exception
                            Throw
                        End Try
                    Next
                    If ie <= 0 Then Exit For
                Next
            End If
        Catch ex As Exception
            Throw
        End Try
        Try
            For i = 0 To mtxVars.Count - 1
                Dim eM As ExprMatrix = mtxVars.Values(i)
                If eM.Rows = 1 AndAlso eM.Columns(0) = 1 Then
                    If Not vars.ContainsKey(mtxVars.Keys(i)) Then
                        vars.Add(mtxVars.Keys(i), eM.Item(0, 0))
                    End If
                End If
            Next
        Catch ex As Exception
        Finally
            enUso = False
            G10.mathml = b
        End Try
    End Sub
    Private Function IsItAMatrixExpression(str As String) As Boolean
        Try
            If InStr(str, "=") Then Return False ' a system of one or more equations
            ' Now, 2 possible cases:
            ' 1) contains a matrix operator 
            '  like in det(1;2 | 1;3 )            
            If Regex.IsMatch(str, G10.RowOrColSeparator + "|" + G10.sMtxFn) Then ' G10.columnSeparator + "|" + G10.rowSeparator + "|" + G10.sMtxFn) Then
                Return True
            End If
            ' 2) contains a variable who's value represents
            '  a matrix (for example: "A @A=1;b|2;1 @b=5"
            Dim vVars() As String = Regex.Split(str, "[-+*/÷^=\d()]+")
            Dim lnValues As Int32 = mtxVars.Values.ToArray.Length
            For i = 0 To mtxVars.Count - 1
                Try
                    If mtxVars.Values(i).Rows > 1 OrElse mtxVars.Values(i).Columns(0) > 1 Then
                        Return True
                    End If
                    For j = 0 To vVars.Length - 1
                        If mtxVars.ContainsKey(vVars(j)) AndAlso
                        Not vars.ContainsKey(vVars(j)) Then
                            Return True
                        End If
                    Next
                Catch ex As Exception
                End Try
            Next
        Catch ex As Exception

        End Try
        Return False
    End Function
    Private Function Substitute(ms As Match) As String
        Try
            Dim pos As Int32 = Array.IndexOf(G10.vSubstitute, ms.Value)
            Dim sm As String = LCase(ms.Value)
            If pos = -1 Then
                If Len(ms.Value) = 1 Then
                    pos = Array.IndexOf(G10.vSubstitute, "\" + ms.Value)
                ElseIf ms.Value = "**" Then
                    pos = Array.IndexOf(G10.vSubstitute, "\*\*")
                ElseIf ms.Value.Chars(0) = "@" Then
                    Return ms.Value
                ElseIf ms.Groups("cult").Success Then
                    G10.CI = New Globalization.CultureInfo(ms.Groups("cult").Value)
                    CultureInfo = G10.CI
                    Return ""
                ElseIf InStr(sm, "detail") Then
                    G10.detail = True
                    Return ""
                ElseIf InStr(sm, "mathml") Then
                    G10.mathml = True
                    Return ""
                ElseIf InStr(sm, "html") Then
                    G10.mathml = False
                    Return ""
                End If
            End If
            Return G10.vSubstituteBy(pos)
        Catch ex As Exception
            Throw
        End Try
    End Function
    Dim initialLP As Boolean = False
    Private Function extractMatrix(stopAt As String) As ExprMatrix
        Dim value As ExprMatrix
        Dim valueRowCol As New ExprMatrix
        Dim row, col As Int32
        Try
            Do
                value = AddSubs()
                If m.Groups("sep").Success OrElse Regex.IsMatch(m.Value, stopAt) OrElse iC >= mc.Count Then
                    If row + col > 0 Then
                        valueRowCol.RedimRowColumn(row, col)
                        valueRowCol.Item(row, col) = value.Item(0, 0)
                        value = valueRowCol
                    Else
                        valueRowCol = value
                    End If
                End If
                If m.Groups("col").Success Then
                    col += 1
                ElseIf m.Groups("row").Success Then
                    row += 1
                    col = 0
                End If
            Loop While iC < mc.Count AndAlso Not Regex.IsMatch(m.Value, stopAt)
        Catch ex As Exception
            Throw
        End Try
        Return value
    End Function
    Private Function AddSubs() As ExprMatrix
        Dim value, valueb As ExprMatrix
        Dim optor As Char
        Dim iC0 As Int32
        Try
            iC0 = iC
            value = MultDiv()
            Do While Regex.IsMatch(m.Value, "[-+]")
                optor = m.Value
                valueb = MultDiv()
                If optor = "+" Then
                    value += valueb
                Else
                    value -= valueb
                End If
            Loop
        Catch ex As Exception
            Throw
        End Try
        Return value
    End Function
    Private Function MultDiv() As ExprMatrix
        Dim value, value2b As ExprMatrix
        Dim optor2 As Char
        Dim iC2 As Int32
        Try
            iC2 = iC
            value = Pow()
            Do While Regex.IsMatch(m.Value, "[*/÷]")
                optor2 = m.Value
                value2b = Pow()
                Dim bDetailHere As Boolean = True
                Dim IsImg As Boolean = False
                Dim value3 As ExprMatrix

                If optor2 = "*" Then
                    If value2b.IsExpression Then
                        Dim expr As Expression = value2b.getItems(0)(0)
                        value3 = value * expr
                    Else
                        value3 = value * value2b
                    End If
                Else
                    If value2b.IsComplex Then
                        Dim cjo As Complex = Complex.one / value2b.ToComplex
                        value3 = value * cjo
                    ElseIf value.IsPolynomial AndAlso value2b.IsPolynomial Then
                        value3 = New ExprMatrix(
                            New Expression(value.ToPolynomial / value2b.ToPolynomial))
                    ElseIf value.IsExpression AndAlso value2b.IsExpression Then
                        value3 = value / value2b
                    Else
                        Dim invMtx As ExprMatrix = value2b ^ New ExprMatrix(-1.0)
                        value3 = value * invMtx
                    End If
                End If
                value = value3
            Loop
        Catch ex As Exception
            Throw
        End Try
        Return value
    End Function

    Private Function Pow() As ExprMatrix
        Dim value As ExprMatrix
        Dim optor3 As Char
        Dim sign3 As Int32 = 1
        Dim iC3(0) As Int32
        Dim vExpr(0) As ExprMatrix
        Dim i As Int32 = 0
        Try
            iC3(0) = iC
            value = Token(sign3)
            Do While Regex.IsMatch(m.Value, "[\^]")
                vExpr(i) = value
                optor3 = m.Value
                ReDim Preserve iC3(i + 1)
                iC3(i + 1) = iC
                Dim sign4 As Int32 = 1
                Dim value4 As ExprMatrix = Token(sign4)
                If sign4 = -1 Then value4 = -value4
                ReDim Preserve vExpr(vExpr.Length)
                value = value4
                i += 1
            Loop
            If i Then
                vExpr(i) = value
                For j As Int32 = i - 1 To 0 Step -1 ' go backwards, in for ex. 2^3^2 = 256 = 2^(3^2)
                    value = vExpr(j) ^ vExpr(j + 1)
                    vExpr(j) = value
                Next
                iC3(0) = iC
            End If
            If sign3 = -1 Then
                value = -value
            End If
        Catch ex As Exception
            Throw
        End Try
        Return value
    End Function
    Private Sub InsertOperator(sOptor)
        ReDim Preserve mc(mc.Length), vMtx(mc.Length)
        For i As Int32 = mc.Count - 1 To iC - 1 Step -1
            If i Then
                mc(i) = mc(i - 1)
                If vMtx(i - 1) Is Nothing Then
                    vMtx(i) = Nothing
                Else
                    vMtx(i) = New ExprMatrix(vMtx(i - 1))
                End If
                If Left(mc(i).Value, 1) = "(" AndAlso mc(i).Length > 1 Then
                    Dim j As Int32 = Int32.Parse(Mid(mc(i).Value, 2))
                    mc(i) = Regex.Match("(" + (j + 1).ToString, ".*")
                End If
            End If
        Next
        m = Regex.Match(sOptor, "(?<op>[*\^()])") ' insert missing operator
        mc(iC - 1) = m
        If iC > 2 AndAlso Left(mc(iC - 3).Value, 1) = "(" AndAlso mc(iC - 3).Length > 1 Then
            Dim j As Int32 = Int32.Parse(Mid(mc(iC - 3).Value, 2))
            mc(iC - 3) = Regex.Match("(" + (j + 1).ToString, ".*")
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
                mc1 = Regex.Matches(s, G10.sMtxTot)
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
            Do
                m = mc(iC)
                iC += 1
            Loop While iC < mc.Length AndAlso m.Value = ""
        Else
            m = Regex.Match("←", ".*")
        End If
    End Sub
    Private Function Token(ByRef sign As Int32) As ExprMatrix
        Static pOM As New ParseOneMatrix
        Dim value As New ExprMatrix
        Try
            Advance()
            Do While m.Value = "-"
                sign = -sign
                Advance()
            Loop
            Do While Left(m.Value, 1) = "&"
                setModes()
                Advance()
            Loop
            If Left(m.Value, 1) = "(" Then
                value = vMtx(iC - 1)
                If m.Value.Length > 1 Then iC = Int32.Parse(Mid(m.Value, 2))
                Advance()
            End If
            If m.Groups("num").Success OrElse
                 m.Groups("vars").Success OrElse
                (vars IsNot Nothing AndAlso vars.ContainsKey(m.Value)) _
                OrElse m.Groups("cnt").Success _
                OrElse m.Groups("der").Success Then
                Dim mclen As Int32 = 1
                Dim exprMtx As ExprMatrix = Eval_User_Defined_Fn(m.Value)
                If exprMtx IsNot Nothing Then
                    value = exprMtx
                ElseIf vars.ContainsKey(m.Value) AndAlso vars(m.Value) IsNot Nothing Then
                    value = New ExprMatrix(vars(m.Value))
                ElseIf mtxVars.ContainsKey(m.Value) AndAlso mtxVars(m.Value).Rows Then
                    value = mtxVars(m.Value)
                Else
                    Dim pE As New ParseExpression
                    Dim vExpr() As Expression = pE.Evaluate(m.Value).vExpr
                    value = New ExprMatrix(vExpr)
                End If
                Dim IsVar As Boolean = m.Groups("vars").Success
                Advance()
                If m.Groups("vars").Success Then
                    InsertOperator("*")
                ElseIf IsVar Then
                    If m.Groups("num").Success Then
                        InsertOperator("^")
                    ElseIf m.Groups("fn").Success OrElse m.Groups("mtxFn").Success Then
                        InsertOperator("*")
                    End If
                End If
            End If
            If m.Groups("mtxFn").Success OrElse m.Groups("fn").Success Then
                Dim IsFn As Boolean = m.Groups("fn").Success
                Dim m1 As Match = m
                Dim iC2 As Int32 = iC
                Dim sFn As String = m.Value.ToLower
                Dim arg As ExprMatrix
                initialLP = True
                Dim argB As ExprMatrix = Nothing
                If sFn = "mod" Then
                    arg = value
                    Dim sgn As Int32 = 1
                    argB = Token(sgn)
                    If sgn = -1 Then argB = -argB
                    'argB = vMtx(iC - 1)
                Else
                    arg = vMtx(iC)
                    iC = Int32.Parse(Mid(mc(iC).Value, 2))
                End If
                value = EvalMatrixFunctions(sFn, arg, argB, m1)
                If sFn <> "mod" Then Advance()
            ElseIf mtxVars.ContainsKey(m.Value) Then
                If mtxVars IsNot Nothing AndAlso mtxVars.ContainsKey(m.Value) Then
                    ' Get variable's value from mtxVars
                    Dim value1 As New ExprMatrix(mtxVars(m.Value))
                    Dim originalValue As New ExprMatrix(value1)
                    Dim pE As New parseMatrix
                    ' See if m.value has another variable name (contained in dictionary 'vars'):
                    Dim sVar As String = m.Value
                    mtxVars.Remove(sVar)
                    Dim value2 As ExprMatrix = pE.Evaluate(mtxVars, value.ToString).eMtx

                    Dim sValues(0) As String
                    Dim nIter As Int32 = 0
                    sValues(nIter) = value2.ToString
                    Do While sValues(nIter) <> value.ToString
                        nIter += 1
                        value = value2
                        value2 = pE.Evaluate(mtxVars, value.ToString).eMtx
                        ReDim Preserve sValues(nIter)
                        sValues(nIter) = value2.ToString
                        If Array.IndexOf(sValues, sValues(nIter), 0, nIter - 1) > -1 Then
                            Exit Do ' there is recusivity, like in
                            '         parsing "a @a=b @b=a
                        End If
                    Loop
                    mtxVars.Add(sVar, originalValue)
                    value = originalValue
                ElseIf vars.ContainsKey(m.Value) Then
                    Dim b As Boolean = G10.mathml
                    G10.mathml = False
                    value = pOM.Evaluate(vars,
                              vars(m.Value).ToStringExpression(G10.nDec, G10.sImg, G10.CI))(0)(0).eMtx
                    G10.mathml = b
                ElseIf m.Value <> G10.sImg Then
                    ' Set r()().eM to a polynomial of degree one (1.0)
                    value = New ExprMatrix(New Expression(m.Value, 1.0))
                Else
                    ' Set r()().eM to complex i
                    value = New ExprMatrix(New Expression(Complex.i))
                End If
                Advance()
            ElseIf m.Value = "," OrElse m.Groups("other").Success Then
                Throw New Exception("0")
            ElseIf m.Groups("op").Success AndAlso (iC >= mc.Count OrElse (mc(iC).Groups("op").Success AndAlso
                Not Regex.IsMatch(mc(iC).Value, "[-+]"))) Then
                Throw New Exception("1")
            ElseIf (iC = 1 OrElse mc(iC - 2).Groups("sep").Success) AndAlso m.Groups("op").Success AndAlso
                Not Regex.IsMatch(m.Value, "[-+]") Then
                Throw New Exception("0")
            ElseIf regex.IsMatch(m.Value, "[-+*/^]") AndAlso (iC >= mc.Count OrElse mc(iC).Groups("sep").Success OrElse mc(iC).Value = ")") Then
                Throw New Exception("0")
            End If
            Do While Left(m.Value, 1) = "&"
                setModes()
                Advance()
            Loop

        Catch ex As Exception
            Throw
        End Try
        Return value
    End Function
    Function Eval_User_Defined_Fn(strFn As String) As ExprMatrix
        Dim ret As ExprMatrix = Nothing
        Dim mc2(mc.Count - 1) As Match
        Dim iC2 As Int32 = iC
        Try
            If iC >= mc.Length OrElse mc(iC).Value <> "(" Then
                'Exit Try
            End If
            Dim mVal As String = "" 'm.Value
            Dim lp As Int32 = 0
            Dim rp As Int32 = 0
            Dim iC3 As Int32
            For iC3 = iC To mc.Length - 1
                If mc(iC3).Value = "(" Then
                    lp += 1
                    If vMtx(iC3) IsNot Nothing Then
                        mVal += vMtx(iC3).ToString
                    Else
                        mVal += "("
                    End If
                Else
                    If mc(iC3).Value = ")" Then rp += 1
                    If lp = rp OrElse mc(iC3).Groups("sep").Success Then Exit For
                    mVal += mc(iC3).Value
                End If
            Next
            If Left(strFn, 1) = "(" AndAlso Right(strFn, 1) = ")" Then
                strFn = Mid(strFn, 2, Len(strFn) - 2)
                mVal = Mid(mVal, 2, Len(mVal) - 2)
            End If
            Dim vs() As String = Regex.Split(strFn, "\(|\)")
            Dim sFn As String = vs(0).Replace(" ", "")
            Dim sMtxVars As String = ""
            Dim eM As ExprMatrix = Nothing
            ' Find in mtxVars user defined function strFn:
            For i As Int32 = 0 To mtxVars.Count - 1
                Dim vsB() As String = Regex.Split(mtxVars.Keys(i), "\(|\)")
                If vsB.Length > 1 Then
                    ' is a user function like for ex. f(x,y) = (x+y;x-y)
                    If sFn = vsB(0).Replace(" ", "") Then
                        sFn = mtxVars.Keys(i)
                        eM = mtxVars.Values(i) : Exit For
                    End If
                End If
                'End If
            Next
            If eM Is Nothing Then
                Return Nothing
            End If
            'iC = iC3
            If mVal = "" Then mVal = vMtx(iC).ToString
            iC2 = iC
            Array.Copy(mc, mc2, mc.Count)
            Dim fnVars As New Dictionary(Of String, ExprMatrix)
            Dim vsFn() As String = Regex.Split(sFn, "\(|\)")
            Dim sFnParams() As String = Split(vsFn(1), ",")
            'iC = iC2
            Dim sParams As String = mVal
            Dim vsP() As String = Regex.Split(sParams, "\(|\)")
            If vsP.Length > 1 Then vsP(0) = vsP(1)
            Dim vParams() As String = Split(vsP(0), ",")
            If sFnParams.Length <> vParams.Length Then
                Throw New Exception(String.Format(Msg10.Num(114), sFn, sParams))
            End If
            For j As Int32 = 0 To sFnParams.Length - 1
                Dim pMtx As New parseMatrix
                Dim exprParam As ExprMatrix = pMtx.Evaluate(Nothing, vParams(j)).eMtx
                Dim snP As String = sFnParams(j).Replace(" ", "")
                If fnVars.ContainsKey(snP) Then
                    fnVars.Remove(snP)
                End If
                fnVars.Add(snP, exprParam)
            Next
            Dim seM As String = "(" + eM.ToString + ")"
            Dim cpyVars As Dictionary(Of String, ExprMatrix) = mtxVars
            Dim vars2 As New Dictionary(Of String, Expression)
            For i As Int32 = 0 To fnVars.Count - 1
                If mtxVars.ContainsKey(fnVars.Keys(i)) Then
                    mtxVars.Remove(fnVars.Keys(i))
                End If
                mtxVars.Add(fnVars.Keys(i), New ExprMatrix(fnVars.Values(i)))
                Try
                    If Not vars2.ContainsKey(fnVars.Keys(i)) Then
                        vars2.Add(fnVars.Keys(i), fnVars.Values(i).Item(0, 0))
                    End If
                Catch ex As Exception

                End Try
            Next
            initialLP = True
            Dim pOM As New ParseOneMatrix
            ret = pOM.Evaluate(vars2, seM)(0)(0).eMtx
            ReDim mc(mc2.Length - 1)
            Array.Copy(mc2, mc, mc2.Count)
            mtxVars = cpyVars
            For iC4 As Int32 = iC2 To mc.Length - 1
                If mc(iC4).Value = "(" Then lp += 1
                If mc(iC4).Value = ")" Then rp += 1
                mVal += mc(iC4).Value
                mc(iC4) = Regex.Match("", ".")
                If lp = rp Then Exit For
            Next
        Catch ex As Exception
            Throw
        End Try
        Return ret
    End Function
    Public Function EvalMatrixFunctions(sFn As String, arg As ExprMatrix, argB As ExprMatrix, m As Match) As ExprMatrix
        Dim eMtx As ExprMatrix = Nothing
        Try
            If m.Groups("fn").Success Then
                Dim pE As New ParseExpression
                Dim expr As Expression = pE.Evaluate(sFn + ("(" + arg.Item(0, 0).ToString.ToLower + ")")).vExpr(0)
                eMtx = New ExprMatrix(expr)
                Exit Try
            End If
            Select Case sFn
                Case "max"
                    eMtx = New ExprMatrix(arg.OpMax)
                Case "min"
                    eMtx = New ExprMatrix(arg.OpMin)
                Case "cross"
                    eMtx = ExprMatrix.opCrossProduct(arg)
                Case "dot"
                    eMtx = ExprMatrix.opDotProduct(arg)
                Case "identity"
                    Dim rows As Int64 = arg.Item(0, 0).ToDouble
                    Dim cols As Int64 = 0
                    If arg.Columns(0) > 1 Then
                        cols = arg.Item(0, 1).ToDouble
                    End If
                    eMtx = ExprMatrix.Identity(rows, cols)
                Case "echelon"
                    Dim sVars(arg.Columns(0) - 1) As String
                    For i = 0 To sVars.Length - 1
                        sVars(i) = ChrW(&H41 + i)
                    Next
                    eMtx = ExprMatrix.GaussianElimination(arg, False, sVars, False, Nothing)
                Case "inverse"
                    ExprMatrix.GaussianElimination(arg, False, Nothing, True, eMtx)
                Case "cof" ' cofactor
                    Dim cof As ExprMatrix = Nothing
                    arg.opDeterminant(True, cofactorMtx:=cof)
                    eMtx = cof
                Case "eigenvalues", "egvl"
                    eMtx = arg.opEigenValues(False)
                Case "eigenvectors", "egv"
                    eMtx = arg.opEigenVectors(False)
                Case "transpose", "trn"
                    eMtx = ExprMatrix.opTranspose(arg)
                Case "adj"
                    Dim adj As ExprMatrix = Nothing
                    arg.opDeterminant(True, adjoint:=adj)
                    eMtx = adj
                Case "trace" ' 
                    eMtx = arg.opTrace
                Case "roots"
                    If arg.IsPolynomial Then
                        eMtx = New ExprMatrix(arg.ToPolynomial.RootsNumerator)
                        If eMtx Is Nothing Then
                            Throw New Exception(Msg8.num(13)) ' n/a
                        End If
                        eMtx = ExprMatrix.opTranspose(eMtx)
                    End If
                Case "lim"
                    Dim sVar As String = String.Empty
                    Dim limit As New Expression(0.0)
                    If arg.Columns(0) < 2 Then
                        Throw New Exception(Msg8.num(57))
                    End If
                    If arg.Columns(0) > 2 Then
                        sVar = arg.Item(0, 1).ToPolynomial.GetVars(0)
                        limit = arg.Item(0, 2)
                    ElseIf arg.Item(0, 0).IsPolynomial Then
                        sVar = arg.Item(0, 0).ToPolynomial.GetVars(0)
                        limit = arg.Item(0, 1)
                    Else
                        sVar = "x"
                        limit = arg.Item(0, 1)
                    End If
                    eMtx = New ExprMatrix(arg.Item(0, 0).OpLimit(sVar, limit))
                Case "rank"
                    eMtx = New ExprMatrix(arg.opRank)
                Case "det"
                    Dim expr As Expression = arg.opDeterminant()
                    eMtx = New ExprMatrix(expr)
                Case "jordan"
                    eMtx = New ExprMatrix(arg.opJordanForm)
                Case "jacobian"
                    eMtx = arg.opJacobian(arg.GetVars)
                Case "lagrangianinterpolation"
                    If arg.Columns(0) > 3 Then
                        ' lagrangianInterpolation(a,b,n,f(x))
                        Dim intp As Interpolation =
                                Interpolation.getChebyshevNodes(
                                arg.Item(0, 0).ToDouble,
                                arg.Item(0, 1).ToDouble,
                                arg.Item(0, 2).ToDouble,
                                arg.Item(0, 3))
                        eMtx = New ExprMatrix(
                                intp.lagrangianinterpolation(True))
                    Else
                        ' lagrangianInterpolation(x0,f(x0)|x1,f(x1)| ...|xn,f(xn))
                        Dim lagrInt As New Interpolation(arg)
                        eMtx = New ExprMatrix(
                                lagrInt.lagrangianinterpolation(True))
                    End If
                Case "gcd", "lcm"
                    Dim eM As ExprMatrix = arg
                    Dim bAllPoly As Boolean = True
                    Dim bAllReal As Boolean = eM.Item(0, 0).IsDouble
                    Dim bHasResto As Boolean = False
                    For i As Int64 = 1 To eM.Columns(0) - 1
                        If Not eM.Item(0, i).IsPolynomial OrElse
                                eM.Item(0, i).IsDouble Then
                            bAllPoly = False
                        End If
                        If Not eM.Item(0, i).IsPolynomial Then
                            Dim Pa As Polynomial = eM.Item(0, i).ToPolynomial
                            If Pa.resto IsNot Nothing Then
                                bHasResto = True
                                Exit For
                            End If
                        End If
                        If Not eM.Item(0, i).IsDouble Then
                            bAllReal = False
                        End If
                    Next
                    If (Not bAllPoly AndAlso Not bAllReal) OrElse bHasResto Then
                        Throw New Exception(Msg8.num(54))
                    End If
                    Dim e1 As String = ""
                    If bAllReal Then
                        Dim cols As Int64 = eM.Columns(0)
                        Dim vDbl(cols - 1) As Double
                        For i As Int64 = 0 To cols - 1
                            vDbl(i) = eM.Item(0, i).ToDouble
                        Next
                        Dim vLCM_GCD() As Double = ExprMatrix.LCM_GCD(vDbl, e1)
                        If sFn = "gcd" Then
                            eMtx = New ExprMatrix(vLCM_GCD(0))
                        Else

                            eMtx = New ExprMatrix(vLCM_GCD(1))
                        End If
                    ElseIf sFn = "lcm" Then
                        Dim lcm As New Polynomial(eM.Item(0, 0).ToPolynomial)
                        Dim db As Double = lcm.LeadingTerm.cf.ToDouble
                        For i As Int64 = 1 To eM.Columns(0) - 1
                            lcm = Polynomial.opLCM(lcm, eM.Item(0, i).ToPolynomial)
                            Dim db2 As Double = eM.Item(0, i).ToPolynomial.LeadingTerm.cf.ToDouble
                            db = ExprMatrix.LCM_GCD(New Double() {Math.Abs(db), Math.Abs(db2)}, e1)(1)
                        Next
                        If db <> 1.0 Then lcm *= New Polynomial(db)
                        eMtx = New ExprMatrix(lcm)
                    Else
                        Dim gcd As Polynomial = eM.Item(0, 0).ToPolynomial
                        Dim db As Double = gcd.LeadingTerm.cf.ToDouble
                        For i As Int64 = 1 To eM.Columns(0) - 1
                            gcd = Polynomial.opGcd(gcd, eM.Item(0, i).ToPolynomial)
                            Dim db2 As Double = eM.Item(0, i).ToPolynomial.LeadingTerm.cf.ToDouble
                            db = ExprMatrix.LCM_GCD(New Double() {Math.Abs(db), Math.Abs(db2)}, e1)(0)
                        Next
                        If db <> 1.0 Then gcd *= New Polynomial(db)
                        eMtx = New ExprMatrix(gcd)
                    End If
                Case "exp"
                    Throw New Exception(Msg8.num(13))
                Case "conj"
                    eMtx = arg
                    Dim im As ExprMatrix = eMtx.opImag
                    Dim re As ExprMatrix = eMtx.opReal()
                    eMtx = re - im
                Case "inverse"
                    arg.opDeterminant(True, arg)
                Case "mod", "modulo"
                    Dim moduloOpA As ExprMatrix = New ExprMatrix(arg.Item(0, 0)) ' eMtx
                    eMtx = New ExprMatrix(argB.Item(0, 0)) ' arg ' AddSubs
                    Dim modOpA As Expression = arg.Item(0, 0)
                    If moduloOpA.getLastExpr.IsDouble Then
                        ' Real % PositiveInteger or
                        ' Real mod PositiveInteger  
                        If Not eMtx.IsDouble OrElse
                            eMtx.Item(0, 0).ToDouble <> Math.Floor(eMtx.Item(0, 0).ToDouble) OrElse
                            eMtx.Item(0, 0).ToDouble < 1 Then
                            Throw New Exception(Msg8.num(59)) ' arg. should be a positive integer
                        End If
                        Dim dbMod As Double = eMtx.Item(0, 0).ToDouble
                        Dim db As Double = moduloOpA.getLastExpr.ToDouble
                        eMtx = New ExprMatrix(db Mod dbMod)
                    ElseIf moduloOpA.getLastExpr.IsPolynomial Then
                        ' (Polynomial) % PositiveInteger, or
                        ' (Polynomial) mod PositiveInteger 
                        If Not eMtx.IsDouble OrElse
                            eMtx.Item(0, 0).ToDouble <> Math.Floor(eMtx.Item(0, 0).ToDouble) OrElse
                            eMtx.Item(0, 0).ToDouble < 1 Then
                            If eMtx.IsPolynomial Then
                                ' (Polynomial) % (Polynomial)
                                ' or (Polynomial) mod (Polynomial)
                                ' see "Modulo Reduction" in:
                                ' http://stackoverflow.com/questions/13202758/multiplying-two-polynomials

                                Dim modP As Polynomial = eMtx.getLastExpr.ToPolynomial
                                Dim div As Polynomial = moduloOpA.getLastExpr.ToPolynomial / modP
                                Dim b As Boolean = Polynomial.bReduce
                                Polynomial.bReduce = True
                                Dim intpart As New List(Of Term)
                                div.Reduce(intpart)
                                eMtx = New ExprMatrix(New Polynomial(intpart) + div)
                                Polynomial.bReduce = b
                            Else
                                Throw New Exception(Msg8.num(13)) ' n/a
                            End If
                        Else
                            Dim Pa As Polynomial =
                                    moduloOpA.getLastExpr.ToPolynomial
                            Dim bMod As Boolean = G10.mMod
                            G10.mMod = eMtx.ToDouble
                            eMtx = New ExprMatrix(Pa)
                            G10.mMod = bMod
                        End If
                        Exit Try
                    Else
                        Throw New Exception(Msg8.num(13))
                    End If
                Case "factors", "factorize"
                    Dim p1 As Polynomial = arg.Item(0, 0).ToPolynomial
                    Dim num As New Polynomial(p1.resto)
                    Dim den As New Polynomial(p1.divisor)
                    Dim vpolyNum() As Polynomial = Polynomial.Factorize(num, G10.nDec)
                    Dim i As Int32
                    Dim lstPoly As New List(Of Polynomial)
                    For i = 0 To vpolyNum.Length - 1
                        lstPoly.Add(vpolyNum(i))
                    Next
                    Dim vPolyDen() As Polynomial = Polynomial.Factorize(den, G10.nDec)
                    Dim db As Double = num.LeadingTerm.cf.ToDouble / den.LeadingTerm.cf.ToDouble
                    If db <> 1.0 Then
                        lstPoly.Insert(0, New Polynomial(db))
                    End If
                    If vPolyDen IsNot Nothing AndAlso Not den.IsComplex Then
                        lstPoly.Add(Nothing)
                        Dim j As Int32 = i + 1
                        For i = 0 To vPolyDen.Length - 1
                            lstPoly.Add(vPolyDen(i))
                        Next
                    End If
                    eMtx = New ExprMatrix(lstPoly.Count, 1)
                    For i = 0 To lstPoly.Count - 1
                        If lstPoly(i) IsNot Nothing Then
                            eMtx.Item(i, 0) = New Expression(lstPoly(i))
                        Else
                            eMtx.Item(i, 0) = New Expression("----", 1)
                        End If
                    Next
                Case "partialfractions"
                    Dim vNum(-1), vDen(-1) As Polynomial
                    Dim vDenExp(-1) As Int64
                    Dim errmsg As String = String.Empty
                    Dim mtx As ExprMatrix = Nothing
                    If Not Polynomial.partialFractionDecomposition(
                             arg.Item(0, 0), vNum, vDen, vDenExp, errmsg) Then
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
                    eMtx = New ExprMatrix(vect.ToArray)
                Case "integral", "integrate", "∫"
                    Dim diferentiateRespVar As String = "x"
                    If m.Groups("difResp").Success Then
                        diferentiateRespVar = m.Groups("difResp").Value
                    End If
                    eMtx = New ExprMatrix(arg.Item(0, 0).opInmediateIntegral(diferentiateRespVar))

                    'Case "orthog"
                    '    Dim cjo As Complex =
                    '        .ToPolynomial)
                    '    reMtx = New ExprMatrix(cjo)
            End Select
        Catch ex As Exception
            Throw
        End Try
        Return eMtx
    End Function
    Private Function errorAt(iErr As Int32, Optional AddMoreTokens As Int32 = 0) As String
        Dim s As String = ""
        Try
            Dim j As Int32
            For j = 1 To iErr - 1
                If Left(mc(j).Value, 1) = "(" Then
                    s += "("
                Else
                    s += mc(j).Value
                    If InStr(mc(j).Value, "&") Then s += " "
                End If
            Next
            If Left(mc(j).Value, 1) = "(" Then
                s += " [ <span style='color:red'>(</span> ] "
            Else
                s += " [ <span style='color:red'>"
                For j = j To j + AddMoreTokens
                    s += mc(j).Value
                Next
                s += "</span> ] "
            End If
            For j = j To mc.Count - 2
                If Left(mc(j).Value, 1) = "(" Then
                    s += "("
                Else
                    s += mc(j).Value
                End If
            Next
            s = s.Replace("←", " ").Replace("|", "<br />")
        Catch ex As Exception

        End Try
        s = "<h3>" + Msg8.num(13) + "</h3><br />" + s
        Return s
    End Function

#End Region

    Public Overrides Function ToString() As String
        Dim eOneMtx As New ParseOneMatrix(Me.r.eMtx)
        Return eOneMtx.ToString
    End Function
End Class
