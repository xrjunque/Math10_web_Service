Imports System.Text.RegularExpressions
Public Class ConvertStringToMathML
    Public Shared ReadOnly vSubstitute() As String = {"\[", "\]", "\{", "\}",
                             ":", "÷", "–",
         "−", "×", "·", "arcsin", "arccos", "arctan", "\*\*",
                            "arccot", "arccsc", "arcsec", "√"
                            }
    Public Shared ReadOnly vSubstituteBy() As String = {"(", ")", "(", ")",
                             "/", "/", "-",
         "-", "*", "*", "asin", "acos", "atan", "^",
                            "acot", "acsc", "asec", "sqr"
                            }
    Friend Shared spaces As String = "\*?(?<space>&nbsp;|&emsp;)"

    Public Shared Function ConvertToMathML(s As String) As String
        Try
            Dim CI As Globalization.CultureInfo = G10.CI

            Dim _signoDecimal As String = CI.NumberFormat.NumberDecimalSeparator ' signo decimal: ¿ una coma ?
            Dim _separadorMiles As String = CI.NumberFormat.NumberGroupSeparator ' separador de miles: ¿un punto?
            Dim s2 As String = _separadorMiles
            If Asc(s2) = 160 Then
                s2 = "( |" + s2 + ")"
            Else
                s2 = "\" + s2
            End If
            Dim sNum As String = "(?<num> |&emsp;|∞|П|(([0-9]{1,3}" + _separadorMiles +
                "[0-9]{3}(?![0-9])|[0-9])+\" + _signoDecimal +
                "?[0-9]*|[0-9]*\" + _signoDecimal +
                "?[0-9]+)([eE][-+]?[0-9]+)?)"
            'Dim sPost As String = "(?![a-zA-Z_]+)"
            Dim sOp As String = "(?<op> |∈|,|-|\=|\±|\<|\>|≠|≈|≤|≥|\+|ζ|\*|▒|(?<sum>∑|∫|∏)|Γ|\/|\^|π|e|√|∛|ϕ|_|nand|mod|and|nor|xor|not|or|%|!|\(|\))"
            Dim sVar As String = "(?<var>[a-zA-Zℙℂ ])+"
            Dim mtext As String = """(?<text>.*?)""" ' comments enclosed inside double quotes:  " my comment "
            Dim prime As String = "(?<prime>(?<var>′+))"
            Dim path As String = IO.Path.GetFullPath("Entities.txt")
            Using fs As New IO.FileStream(path, IO.FileMode.Open, IO.FileAccess.Read)
                Using sr As New IO.StreamReader(fs)
                    Do While Not sr.EndOfStream
                        Dim vEntity() As String = sr.ReadLine.Split(vbTab)
                        If vEntity.Length > 4 Then
                            s = Replace(s, vEntity(1), vEntity(0))
                            If InStr(s, vEntity(0)) AndAlso Len(vEntity(0)) Then
                                Dim tipo As String = vEntity(4)
                                Select Case tipo
                                    Case "v" : sVar = Replace(sVar, " ", " " + vEntity(0), 1)
                                    Case "o" : sOp = Replace(sOp, " |", " |" + vEntity(0) + "|", 1)
                                    Case "n" : sNum = Replace(sNum, " |", " |" + vEntity(0) + "|", 1)
                                End Select
                            End If
                        End If
                    Loop
                End Using
            End Using
            sVar = Replace(sVar, " ", "", 1)
            sOp = Replace(sOp, " |", "", 1)
            sNum = Replace(sNum, " |", "", 1)
            Dim evaluator As String = sNum + "|" + sOp + "|" + sVar + "|" + mtext + "|" + prime + "|" + spaces
            Dim reTot As New Regex(evaluator)

            'For i As Int32 = 0 To vSubstitute.Length - 1
            's = Regex.Replace(s, vSubstitute(i), vSubstituteBy(i))
            'Next
            s = Replace(s, "&nbsp;", """ """)
            's = Regex.Replace(s, "\*?&nbsp;", " ")
            's = Regex.Replace(s, "\*?&emsp;", " ")
            Dim mc1 As MatchCollection = reTot.Matches(s)
            Dim pS As New ParseStringToML
            s = pS.Evaluate(mc1)
            s = Regex.Replace(s, "(?<tag><[^/]*/[^>]+>)", "${tag}" + vbCrLf)
            'Dim s1 As String = "<math xmlns='http://www.w3.org/1998/Math/MathML' mode='display'>" + vbCrLf
            's = s1 + s + "</math>" + vbCrLf
        Catch ex As Exception

        End Try
        Return s
    End Function
End Class
Public Class ParseStringToML
    Dim m, mc() As Match
    Dim iC As Int32
    Function Evaluate(mc1 As MatchCollection) As String
        Try
            ReDim mc(mc1.Count - 1)
            mc1.CopyTo(mc, 0)
            For i = 0 To mc.Length - 1
                If i < mc.Length - 1 Then
                    If ((mc(i).Groups("num").Success AndAlso mc(i + 1).Groups("var").Success) OrElse
                        (mc(i).Groups("var").Success AndAlso mc(i + 1).Groups("num").Success)) Then
                        InsertOptor("*", i)
                    ElseIf ((mc(i).Value = ")" AndAlso mc(i + 1).Groups("var").Success) OrElse
                        (mc(i).Value = ")" AndAlso mc(i + 1).Groups("num").Success)) Then
                        If Regex.IsMatch(mc(i + 1).Value, "&emsp;") Then
                        Else
                            InsertOptor("*", i)
                        End If
                    ElseIf mc(i).Value = ")" AndAlso Regex.IsMatch(mc(i + 1).Value, "∏|∑|∫|Γ") Then
                        InsertOptor("*", i)
                    ElseIf ((mc(i).Groups("num").Success AndAlso mc(i + 1).Groups("prime").Success) OrElse
                    (mc(i).Groups("var").Success AndAlso mc(i + 1).Groups("prime").Success)) Then
                        InsertOptor("^", i)
                    End If
                End If
            Next
            iC = -1
            'm = mc(0)
            Advance()
            Dim s As String = AddSub()
            Do While iC < mc.Count - 1
                Advance()
                s += AddSub()
            Loop
            Return s
        Catch ex As Exception
            Throw
        End Try
    End Function
    Private Sub InsertOptor(sOptor, im)
        ReDim Preserve mc(mc.Length)
        For i As Int32 = mc.Count - 1 To im + 1 Step -1
            mc(i) = mc(i - 1)
        Next
        m = Regex.Match(sOptor, "(?<op>[*\^])") ' insert missing operator
        mc(im + 1) = m
    End Sub
    Private Sub Advance()
        Try
            iC += 1
            If iC < mc.Count Then
                m = mc(iC)
                Do While iC < mc.Count AndAlso m.Value = "<"
                    Do
                        iC += 1
                        m = mc(iC)
                    Loop While m.Value <> ">"
                    iC += 1
                    If iC < mc.Count Then
                        m = mc(iC)
                    Else
                        m = Regex.Match(" ", ".")
                    End If
                Loop
            Else
                m = Regex.Match(" ", ".")
            End If
        Catch ex As Exception

        End Try
    End Sub
    Private Function AddSub()
        Dim s As String = ""
        Try
            s = MultDiv()
            Do
                If m.Value = "+" Then
                    Advance()
                    s += "<mo>+</mo>" + MultDiv()
                ElseIf m.Value = "-" Then
                    Advance()
                    s += "<mo>-</mo>" + MultDiv()
                ElseIf m.Value = "±" Then
                    Advance()
                    s += "<mo>±</mo>" + MultDiv()
                Else
                    Exit Do
                End If
            Loop
            If m.Value = "=" Then
                s += "<mo>=</mo>"
                Advance()
                s += AddSub()
            ElseIf m.Groups("op").Success AndAlso m.Value <> ")" Then
                If InStr("∑∏∫", m.Value) Then
                    Dim sOp As String = m.Value
                    Dim sOver As String = "", sUnder As String = ""
                    Advance()
                    If m.Value = "_" Then
                        Advance()
                        Dim iC2 As Int32 = iC
                        Do
                            Dim iC3 As Int32 = iC
                            sUnder += Token(True)
                            If m.Value = "^" OrElse m.Value = "▒" Then
                                Exit Do
                            End If
                            If iC3 = iC Then
                                sUnder += "<mo>" + m.Value + "</mo>"
                                Advance()
                            End If
                        Loop While m.Value <> "^" AndAlso m.Value <> "▒"
                        If iC - iC2 > 1 Then
                            sUnder = "<mrow>" + sUnder + "</mrow>"
                        End If
                    Else
                        sUnder = "<mi> </mi>"
                    End If
                    If m.Value = "^" Then
                        Advance()
                        Dim iC2 As Int32 = iC
                        Do
                            Dim iC3 As Int32 = iC
                            sOver += Token(True)
                            If m.Value = "_" OrElse m.Value = "▒" Then
                                Exit Do
                            End If
                            If iC3 = iC Then
                                sOver += "<mo>" + m.Value + "</mo>"
                                Advance()
                            End If
                        Loop While m.Value <> "_" AndAlso m.Value <> "▒"
                        If iC - iC2 > 1 Then
                            sOver = "<mrow>" + sOver + "</mrow>"
                        End If
                    Else
                        sOver = "<mi> </mi>"
                    End If
                    If m.Value = "▒" Then
                        Advance()
                    End If
                    If sOp = "∫" Then
                        sOp = "<mo>&int;</mo>"
                    ElseIf sOp = "∏" Then
                        sOp = "<mo>&prod;</mo>"
                    Else
                        sOp = "<mo>&sum;</mo>"
                    End If
                    If Len(sUnder + sOver) Then
                        s += "<munderover>" + sOp
                        s += sUnder + sOver + "</munderover>"
                    Else
                        s += sOp
                        Advance()
                    End If
                    If iC < mc.Count - 1 Then
                        'Advance()
                        s += AddSub()
                    End If
                Else
                    If m.Value <> "(" Then
                        s += "<mo>" + m.Value + "</mo>"
                        Advance()
                    End If
                    s += AddSub()
                End If
            End If
        Catch ex As Exception
            Throw
        End Try
        Return s
    End Function
    Private Function MultDiv()
        Dim s As String = ""
        Try
            s = Pow()
            Do
                If m.Value = "*" Then
                    Advance()
                    's += "<mo>×</mo>" + Pow()
                    s += "<mo>·</mo>" + Pow()
                    's += Pow()
                ElseIf m.Value = "/" Then
                    Dim sNum As String = s
                    Advance()
                    Dim sDen As String = Pow()
                    Dim mc2 As MatchCollection = Regex.Matches(s, "<mo>.*?<\/mo>")
                    If mc2.Count > 1 AndAlso mc2(0).Value = "<mo>(</mo>" _
                     AndAlso mc2(mc2.Count - 1).Value = "<mo>)</mo>" Then
                        Dim i As Int32 = mc2.Count - 1
                        If InStr(s, mc2(i).Value) = mc2(i).Index + 1 Then
                            s = s.Replace(mc2(0).Value, "").Replace(mc2(i).Value, "")
                        End If
                    End If
                    s = "<mrow>" + s + "</mrow>"
                    Dim mc3 As MatchCollection = Regex.Matches(sDen, "<mo>.*?<\/mo>")
                    If mc3.Count > 1 AndAlso mc3(0).Value = "<mo>(</mo>" _
                     AndAlso mc3(mc3.Count - 1).Value = "<mo>)</mo>" Then
                        Dim i As Int32 = mc3.Count - 1
                        If InStr(sDen, mc3(i).Value) = mc3(i).Index + 1 Then
                            sDen = sDen.Replace(mc3(0).Value, "").Replace(mc3(i).Value, "")
                        End If
                    End If
                    sDen = "<mrow>" + sDen + "</mrow>"
                    s = "<mfrac>" + s + sDen + "</mfrac>"
                    '    s = "<mrow>" + s + "</mrow>"
                Else
                    Exit Do
                End If
            Loop
        Catch ex As Exception
            Throw
        End Try
        Return s
    End Function
    Private Function Pow()
        Dim s As String = ""
        Try
            s = Token()
            Do While m.Value = "^" OrElse m.Value = "_"
                Dim sOp As String = m.Value
                Dim base As String = s
                Advance()
                Dim exp As String = ""
                If m.Value = "-" Then
                    exp = "<mo>-</mo>" : Advance()
                End If
                exp += Token(True)
                Dim mc2 As MatchCollection = Regex.Matches(base, "\<m")
                If mc2.Count > 1 Then
                    base = "<mrow>" + base + "</mrow>"
                End If
                Dim mc3 As MatchCollection = Regex.Matches(exp, "\<m")
                If mc3.Count > 1 Then
                    exp = "<mrow>" + exp + "</mrow>"
                End If
                If sOp = "^" Then
                    s = "<msup>" + base + exp + "</msup>"
                Else
                    s = "<msub>" + base + exp + "</msub>"
                End If
            Loop
        Catch ex As Exception
            Throw
        End Try
        Return s
    End Function
    Private Function Token(Optional oneToken As Boolean = False)
        Dim s As String = ""
        Do
            s += TokenB(oneToken)
            If m.Groups("text").Success Then
                s += "<mtext>" + Replace(m.Groups("text").Value, " ", "&nbsp;") + "</mtext>"
                Advance()
            Else
                Exit Do
            End If
        Loop
        Return s
    End Function
    Private Function TokenB(Optional oneToken As Boolean = False)
        Dim s As String = ""
        Try
            If m.Value = "(" Then
                Advance()
                Dim s1 As String = AddSub()
                If Left(s1, 10) = "<mo>(</mo>" AndAlso Right(s1, 10) = "<mo>)</mo>" AndAlso
                InStr(10, s1, "(") = 0 Then
                    s += s1
                Else
                    s += "<mo>(</mo>" + s1 + "<mo>)</mo>"
                End If
                Advance()
                If oneToken Then
                    Return s
                End If
            End If
            If m.Groups("num").Success Then
                s += "<mn>" + m.Value + "</mn>"
                Advance()
                Return s
            End If
            If m.Groups("var").Success Then
                If m.Value = "sqr" OrElse m.Value = "sqrt" Then
                    Advance()
                    Dim s1 As String = ""
                    If m.Value = "(" Then
                        Advance()
                    End If
                    s1 = AddSub()
                    s += "<msqrt>" + s1 + "</msqrt>"
                Else
                    s += "<mi>" + m.Value + "</mi>"
                End If
                Advance()
                'Return s ' ...there could be a '('
            End If
            If m.Value = "(" Then
                s += "<mo>(</mo>"
                Advance()
                s += AddSub()
                s += "<mo>)</mo>"
                Advance()
            End If
        Catch ex As Exception
            Throw
        End Try
        Return s
    End Function
End Class