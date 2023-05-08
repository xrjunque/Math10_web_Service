Imports System.Text.RegularExpressions
Public Class ParseOneMatrix
    ' ParseOneMatrix parses a unique matrix of expressions, 
    ' but does no operation (-,+,*,...) bwtween two or more
    ' (expression) matrices. Just parses one matrix and
    ' evaluates the elements inside it.
    Public Class Result
        Public eMtx As New ExprMatrix
        Public vsDetail() As String
        Public Overloads Function ToString()
            Return eMtx.ToString
        End Function
    End Class

    Dim r(-1)() As Result
    Public bPassAsOneColumn As Boolean = False
    Public Sub New()
    End Sub
    Public Sub New(eMtx As ExprMatrix)
        ReDim r(0), r(0)(0)
        r(0)(0) = New Result
        r(0)(0).eMtx = New ExprMatrix(eMtx)
    End Sub
    Public HasAtLeastOneEquation As Boolean
    Public HasAtleastOneNotEquation As Boolean
    Public Function Evaluate(vars As Dictionary(Of String, Expression), sMtx As String) As Result()()
        Try
            HasAtleastOneNotEquation = False
            HasAtLeastOneEquation = False
            'sMtx = Replace(sMtx, " ", "")
            ReDim r(0), r(0)(0)
            r(0)(0) = New Result
            sMtx = Regex.Replace(sMtx, "(^\|*)(?<supr>[^\(\)]+)(\|*)$", "${supr}")
            Dim sign As Int32 = 1
            If Left(sMtx, 2) = "-(" Then
                sign = -1
                sMtx = Mid(sMtx, 3, Len(sMtx) - 3)
            End If
            Dim sRows() As String = Regex.Split(sMtx, G10.rowSeparator) ' "\r\n|[-+*/÷^%]?\|")
            removeLeadingAndTrailingUnnecessaryParentheses(sRows)
            Dim i, j As Int32
            i = 0
            For i1 As Int32 = 0 To sRows.Length - 1
                If Regex.Replace(sRows(i1), "\s+", "").Length Then
                    Dim sCols() As String = Regex.Split(sRows(i1), G10.columnSeparator)
                    removeLeadingAndTrailingUnnecessaryParentheses(sCols)

                    Dim eP As New ParseExpression(G10.CI)
                    For j = 0 To sCols.Length - 1
                        If Trim(sCols(j)) = "" Then Continue For
                        removeLeadingAndTrailingUnnecessaryParentheses(sCols)
                        Dim r2 As ParseExpression.Result = eP.Evaluate(vars, sCols(j))
                        If r2 Is Nothing OrElse r2.vExpr Is Nothing Then Continue For
                        If r2.vExpr.Length = 0 OrElse r2.vExpr(0) Is Nothing Then Continue For
                        If r2.vExpr(0).IsEquation Then
                            HasAtLeastOneEquation = True
                        Else
                            HasAtleastOneNotEquation = True
                        End If
                        If r2.vExpr Is Nothing OrElse r2.vExpr.Length = 0 Then Continue For
                        If sCols.Length = 1 Then
                            r(0)(0).eMtx.AddRow(r2.vExpr)
                            ReDim Preserve r(0)(0).vsDetail(i)
                        Else
                            r(0)(0).eMtx.AddItemToRow(r2.vExpr(0), i, j)
                            ReDim Preserve r(0)(0).vsDetail(i)
                        End If
                        If r2.sDetail IsNot Nothing Then
                            If r(0)(0).vsDetail(i) IsNot Nothing Then
                                r(0)(0).vsDetail(i) += r2.sDetail.Replace(vbCrLf, "</br>") + "</br>"
                            Else
                                r(0)(0).vsDetail(i) = r2.sDetail.Replace(vbCrLf, "</br>") + "</br>"
                            End If
                        Else
                            r(0)(0).vsDetail(i) = ""
                        End If
                        If r2.vExpr.Length > 1 Then
                            r(0)(0).eMtx = ExprMatrix.opTranspose(r(0)(0).eMtx)
                        End If
                        'Else
                        '    ReDim Preserve r(i)(j)
                        '    r(i)(j) = New Result
                        '    Dim r2 As ParseExpression.Result = eP.Evaluate(sCols(j))
                        '    r(i)(j).eM.AddRow(r2.vExpr)
                        '    ReDim Preserve r(i)(j).vsDetail(i)
                        '    If r2.sDetail IsNot Nothing Then
                        '        If r(i)(j).vsDetail(i) IsNot Nothing Then
                        '            r(i)(j).vsDetail(i) += r2.sDetail.Replace(vbCrLf, "</br>")
                        '        Else
                        '            r(i)(j).vsDetail(i) = r2.sDetail.Replace(vbCrLf, "</br>")
                        '        End If
                        '    Else
                        '        r(i)(j).vsDetail(i) = ""
                        '    End If
                        'End If
                    Next
                    i += 1
                End If
            Next
            If sign = -1 Then
                r(0)(0).eMtx = -r(0)(0).eMtx
            End If
        Catch ex As Exception
            Throw
        End Try
        Return r
    End Function
    Public Shared Sub removeLeadingAndTrailingUnnecessaryParentheses(ByRef sRowsOrCols() As String, Optional first As Int32 = 0)
        Try
            If sRowsOrCols.Length > 1 Then
                Dim last As Int32 = sRowsOrCols.Length - 1
                Do While last <> first AndAlso InStr(sRowsOrCols(last), ")") = 0
                    last -= 1
                Loop
                Do While last <> first AndAlso InStr(sRowsOrCols(first), "(") = 0
                    first += 1
                Loop
                If last = first Then Exit Sub
                Do While sRowsOrCols(first).Length AndAlso sRowsOrCols(sRowsOrCols.Length - 1).Length
                    ' Remove leading and Trailing unnecesary (if any) Parentheses:
                    Dim lp As Int32 = Regex.Matches(sRowsOrCols(first), "\(").Count - Regex.Matches(sRowsOrCols(first), "\)").Count
                    Dim rp As Int32 = -Regex.Matches(sRowsOrCols(last), "\(").Count + Regex.Matches(sRowsOrCols(last), "\)").Count
                    If lp AndAlso rp AndAlso Regex.IsMatch(sRowsOrCols(first), "^(\s*\()") _
                        AndAlso Regex.IsMatch(sRowsOrCols(last), "(\)\s*)$") Then
                        Dim posLP As Int32 = sRowsOrCols(first).IndexOf("(")
                        sRowsOrCols(first) = sRowsOrCols(first).Remove(posLP, 1)
                        Dim posRP As Int32 = InStrRev(sRowsOrCols(last), ")")
                        sRowsOrCols(last) = sRowsOrCols(last).Remove(posRP - 1, 1)
                    Else
                        Exit Do
                    End If
                Loop
            End If
        Catch ex As Exception

        End Try
    End Sub
    Public Overrides Function ToString() As String
        Dim sRet As String = ""
        Dim vCode(r.Length - 1)() As String
        Try
            sRet += "<table class='center'>"
            For iRow As Int32 = 0 To r.Length - 1
                sRet += "<tr>"
                If r(iRow) IsNot Nothing Then
                    For jCol As Int32 = 0 To r(iRow).Length - 1
                        ReDim Preserve vCode(iRow)(r(iRow).Length - 1)
                        sRet += "<td>"
                        If r(iRow)(jCol) IsNot Nothing Then
                            Dim eMtx As ExprMatrix = r(iRow)(jCol).eMtx
                            Dim s As String = ""
                            If eMtx IsNot Nothing Then
                                s = eMtx.ToString_eMtx(True, G10.nDec, G10.sImg, G10.CI)
                            ElseIf G10.mathml Then
                                s = ""
                            Else
                                s = "-----"
                            End If
                            vCode(iRow)(jCol) = eMtx.ToString_eMtx(False, 15)
                            sRet += s
                        Else
                            vCode(iRow)(jCol) = ""
                        End If
                        If r(iRow)(jCol).vsDetail IsNot Nothing Then
                            For iv As Int32 = 0 To r(iRow)(jCol).vsDetail.Length - 1
                                If r(iRow)(jCol).vsDetail(iv) IsNot Nothing Then
                                    sRet += r(iRow)(jCol).vsDetail(iv) + "</br>"
                                End If
                            Next
                        End If
                        sRet += "</td>"
                    Next jCol
                Else
                    sRet += "<td></td>"
                End If
                sRet += "</tr>"
            Next iRow
            sRet += "</table>"
            sRet += "<!-- "
            For iRow As Int32 = 0 To vCode.Length - 1
                If vCode(iRow) IsNot Nothing Then
                    For jCol As Int32 = 0 To vCode(iRow).Length - 1
                        sRet += String.Format("@{0}@{1}@>", iRow, jCol) + vCode(iRow)(jCol)
                    Next
                End If
            Next
            sRet += " -->"
        Catch ex As Exception
            Throw
        End Try
        Return sRet
    End Function

End Class
