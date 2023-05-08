Imports System.Text.RegularExpressions

Public Class MathMLToString
    Friend Shared op As String = "\<mo[^\>]*\>" + "(?<op>[^\<]+)" + "\</mo\>"
    Friend Shared num As String = "\<mn[^\>]*\>" + "(?<num>[^\<]+)" + "\</mn\>"
    Friend Shared var As String = "\<mi[^\>]*\>" + "(?<var>[^\<]+)" + "\</mi\>"
    Friend Shared text As String = "\<mtext[^\>]*\>" + "(?<text>[^\<]+)" + "\</text\>"
    Friend Shared sup As String = "<msup[^\\>]*>(?<sup>(?:(?!<msup>|<\/msup>).)*)<\/msup>"
    Friend Shared frac As String = "<mfrac[^\\>]*>(?<frac>(?:(?!<mfrac>|<\/mfrac>).)*)<\/mfrac>"
    Friend Shared fracB As String = "\|(?<frac>.*?)\|"
    Friend Shared row As String = "<mrow[^\\>]*>(?<row>(?:(?!<mrow>|<\/mrow>).)*)<\/mrow>"
    Friend Shared sqrt As String = "<msqrt[^\\>]*>(?<sqrt>(?:(?!<msqrt>|<\/msqrt>).)*)<\/msqrt>"
    Friend Shared table As String = "\<mtable.*?\>(?<tbl>.*?)\</mtable\>"
    Friend Shared tr As String = "\<mtr.*?\>(?<tr>.*?)\</mtr\>"
    Friend Shared td As String = "\<mtd.*?\>(?<td>.*?)\</mtd\>"
    Friend Shared vStr() As String = New String() {fracB, sqrt, sup, row, op, num, var, text}
    Friend Shared reMathML As New Regex("(?i)" + Join(vStr, "|") + "(?-i)")
    Friend Shared sReplace() As String = New String() {"\s*", "&\#x22C5;", "&\#8289;", "&\#x2212;", "&\#xB1;"}
    Friend Shared sReplaceBy() As String = New String() {"", "*", "", "-", "±"}

    Public Shared Function convertToString(sMathML As String) As String
        ' It's a simple converter: no integrals, no summantion, etcetera.
        ' Just additions, substractions, multiplications, fractions and powers.
        Dim s As String = ""
        Try
            For i As Int32 = 0 To sReplace.Length - 1
                sMathML = Regex.Replace(sMathML, sReplace(i), sReplaceBy(i))
            Next
            Dim sRemove As String = "\<math[^>]*\>|\</math\>|\<mstyle[^>]*\>|\</mstyle\>"
            sMathML = Regex.Replace(LCase(sMathML), sRemove, "")

            Dim mtbl As Match = Regex.Match(sMathML, table, RegexOptions.IgnoreCase)
            If mtbl.Success Then
                Dim stbl As String = mtbl.Groups("tbl").Value
                Dim mc1 As MatchCollection = Regex.Matches(stbl, tr, RegexOptions.IgnoreCase)
                For Each mtr As Match In mc1
                    Dim str As String = mtr.Groups("tr").Value
                    Dim mc2 As MatchCollection = Regex.Matches(str, td)
                    For Each mtd As Match In mc2
                        Dim std As String = mtd.Groups("td").Value
                        Do
                            Dim mfrac As MatchCollection = Regex.Matches(std, frac)
                            If mfrac.Count = 0 Then Exit Do
                            Dim m As Match = mfrac(0)
                            Dim s1 As String = ParseML.fraction(m)
                            s1 = "|" + s1 + "|"
                            std = Replace(std, m.Value, s1, 1, 1)
                        Loop
                        Dim mc3 As MatchCollection = reMathML.Matches(std)
                        Dim pML As New ParseML
                        s += pML.Evaluate(mc3)
                    Next
                Next
            Else
                Do
                    Dim mfrac As MatchCollection = Regex.Matches(sMathML, frac)
                    If mfrac.Count = 0 Then Exit Do
                    Dim m As Match = mfrac(0)
                    Dim s1 As String = ParseML.fraction(m)
                    s1 = "|" + s1 + "|"
                    sMathML = Replace(sMathML, m.Value, s1, 1, 1)
                Loop
                Dim mc3 As MatchCollection = reMathML.Matches(sMathML)
                Dim pML As New ParseML
                s += pML.Evaluate(mc3)
            End If
        Catch ex As Exception

        End Try
        Return s
    End Function
    Private Class ParseML

        Dim iC As Int32
        Dim m, mc(-1) As Match
        Dim sEval As String
        Sub Advance()
            iC += 1
            If iC < mc.Length Then
                m = mc(iC)
            Else
                m = Regex.Match(" ", ".")
            End If
        End Sub
        Public Function Evaluate(mc1 As MatchCollection) As String
            If mc1.Count = 0 Then Return ""
            ReDim mc(mc1.Count - 1)
            mc1.CopyTo(mc, 0)
            'For i As Int32 = 0 To mc.Count - 1
            '    sEval += mc(i).Value
            '    If (mc(i).Groups("var").Success OrElse mc(i).Groups("num").Success) AndAlso
            '    i < mc.Count - 1 AndAlso (mc(i + 1).Groups("var").Success OrElse mc(i + 1).Groups("num").Success) Then
            '        insertOperator("*", i)
            '        sEval += "*"
            '        i += 1
            '    End If
            'Next
            m = mc(0)
            Return AddSubs()
        End Function
        Sub insertOperator(sOp As String, im As Int32)
            Try
                ReDim Preserve mc(mc.Length)
                For i As Int32 = mc.Length - 1 To im + 2 Step -1
                    mc(i) = mc(i - 1)
                Next
                mc(im + 1) = MathMLToString.reMathML.Match("<mo>" + sOp + "</mo>")
            Catch ex As Exception

            End Try
        End Sub
        Private Function AddSubs() As String
            Dim s As String = ""
            Try
                s = MultDiv()
                Do While m.Groups("op").Success AndAlso Regex.IsMatch(m.Value, "[-+]")
                    Dim sm As String = m.Groups("op").Value
                    Advance()
                    If sm = "+" Then
                        s += "+" + MultDiv()
                    Else
                        s += "-" + MultDiv()
                    End If
                Loop
            Catch ex As Exception

            End Try
            Return s
        End Function
        Private Function MultDiv() As String
            Dim s As String = ""
            Try
                s = Pow()
                Do While (m.Groups("op").Success AndAlso InStr(m.Value, "*")) OrElse m.Groups("frac").Success
                    If Not m.Groups("frac").Success Then
                        Advance()
                        s += "*" + Pow()
                    Else
                        s += m.Groups("frac").Value
                        Advance()
                    End If
                Loop
            Catch ex As Exception

            End Try
            Return s
        End Function
        Public Shared Function fraction(m As Match) As String
            Dim s As String = ""
            Try
                Dim mRow As MatchCollection = Regex.Matches(m.Value, MathMLToString.row)
                Dim sNum, sDen As String
                If mRow.Count = 2 Then
                    Dim mcNum As MatchCollection = MathMLToString.reMathML.Matches(mRow(0).Value)
                    Dim mcRow As MatchCollection = MathMLToString.reMathML.Matches(mcNum(0).Groups("row").Value)
                    Dim pML As New ParseML
                    sNum = pML.Evaluate(mcRow)
                    Dim mcDen As MatchCollection = MathMLToString.reMathML.Matches(mRow(1).Value)
                    mcRow = MathMLToString.reMathML.Matches(mcDen(0).Groups("row").Value)
                    Dim pMLDen As New ParseML
                    sDen = pMLDen.Evaluate(mcRow)
                ElseIf mRow.Count = 1 AndAlso mRow(0).Index = 7 Then
                    Dim mcNum As MatchCollection = MathMLToString.reMathML.Matches(mRow(0).Value)
                    Dim mcRow As MatchCollection = MathMLToString.reMathML.Matches(mcNum(0).Groups("row").Value)
                    Dim pML As New ParseML
                    sNum = pML.Evaluate(mcRow)
                    Dim mcPost As MatchCollection = MathMLToString.reMathML.Matches(Mid(m.Value, mRow(0).Length))
                    pML = New ParseML
                    sDen = pML.Evaluate(mcPost)
                ElseIf mRow.Count = 1 Then
                    Dim pos As Int32 = InStr(m.Value, "<mrow>")
                    Dim mcPre As MatchCollection = MathMLToString.reMathML.Matches(Left(m.Value, pos - 1))
                    Dim pMLnum As New ParseML
                    sNum = pMLnum.Evaluate(mcPre)
                    Dim mcDen As MatchCollection = MathMLToString.reMathML.Matches(mRow(0).Value)
                    Dim mcRow As MatchCollection = MathMLToString.reMathML.Matches(mcDen(0).Groups("row").Value)
                    Dim pML As New ParseML
                    sDen = pML.Evaluate(mcDen)
                Else
                    Dim mc As MatchCollection = MathMLToString.reMathML.Matches(m.Value)
                    Dim mcNum As MatchCollection = MathMLToString.reMathML.Matches(mc(0).Value)
                    Dim pMLNum As New ParseML
                    sNum = pMLNum.Evaluate(mcNum)
                    Dim mcDen As MatchCollection = MathMLToString.reMathML.Matches(mc(1).Value)
                    Dim pMLDen As New ParseML
                    sDen = pMLDen.Evaluate(mcDen)
                End If
                If Len(sNum) > 1 AndAlso Regex.IsMatch(Mid(sNum, 2), "[-+]") Then
                    sNum = "(" + sNum + ")"
                End If
                If Len(sDen) > 1 AndAlso Regex.IsMatch(Mid(sDen, 2), "[-+*]") Then
                    sDen = "(" + sDen + ")"
                End If
                s += sNum + "/" + sDen

            Catch ex As Exception

            End Try
            Return s
        End Function
        Private Function Pow() As String
            Dim s As String = ""
            Try
                s = Token()
                If m.Groups("op").Success AndAlso (m.Groups("op").Value = "=" _
                OrElse m.Groups("op").Value = "±") Then
                    s += m.Groups("op").Value
                    Advance()
                    s += Token()
                End If
                Do While m.Groups("sup").Success
                    Dim sSup As String = m.Groups("sup").Value
                    Dim pM As New ParseML
                    Dim mcPow As MatchCollection = MathMLToString.reMathML.Matches(sSup)
                    Dim pMLbase As New ParseML
                    Dim mbase As MatchCollection = MathMLToString.reMathML.Matches(mcPow(0).Value)
                    Dim a As String = pMLbase.Evaluate(mbase)
                    Dim pMLexp As New ParseML
                    Dim mExp As MatchCollection = MathMLToString.reMathML.Matches(mcPow(1).Value)
                    Dim b As String = pMLexp.Evaluate(mExp)
                    s += a + "^" + b
                    Advance()
                    If m.Groups("var").Success Then
                        m = MathMLToString.reMathML.Match("<mo>*</mo>")
                        iC -= 1
                    End If
                Loop
            Catch ex As Exception

            End Try
            Return s
        End Function
        Private Function Token() As String
            Dim s As String = ""
            Try
                If m.Groups("op").Success AndAlso m.Groups("op").Value = "(" Then
                    Advance()
                    s += "(" + AddSubs() + ")"
                    Advance()
                End If
                If m.Groups("op").Success AndAlso m.Groups("op").Value = "-" Then
                    s += m.Groups("op").Value  ' change sign 
                    Advance()
                End If
                If m.Groups("text").Success Then
                    s += m.Groups("text").Value
                    Advance()
                End If
                If m.Groups("row").Success Then
                    Dim mcRow As MatchCollection = MathMLToString.reMathML.Matches(m.Groups("row").Value)
                    Dim pML As New ParseML
                    s += pML.Evaluate(mcRow)
                End If
                If m.Groups("sqrt").Success Then
                    Dim mcRow As MatchCollection = MathMLToString.reMathML.Matches(m.Groups("sqrt").Value)
                    Dim pML As New ParseML
                    s += "sqrt(" + pML.Evaluate(mcRow) + ")"
                End If
                If m.Groups("var").Success Then
                    s += m.Groups("var").Value
                    Advance()
                ElseIf m.Groups("num").Success Then
                    s += m.Groups("num").Value
                    Advance()
                End If
                If m.Groups("op").Success AndAlso m.Groups("op").Value = "(" Then
                    Advance()
                    s += "(" + AddSubs() + ")"
                    Advance()
                End If
            Catch ex As Exception

            End Try
            Return s
        End Function
    End Class
End Class
