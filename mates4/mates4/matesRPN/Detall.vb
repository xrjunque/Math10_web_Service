Imports System.Text.RegularExpressions
Imports System.Text

#Region "class Detall84"
<Serializable()>
Public Class Detall84
    Dim sExpr(-1) As StringBuilder, ie As Int32 = 0
    Dim v_iCur(-1) As Int32, ivs As Int32 = 0
    Dim sDivTblHTML(-1) As String, iDT As Int32 = 0

    Public Sub Clear()
        ie = 0
        ivs = 0
        iDT = 0
        ReDim sExpr(-1), v_iCur(-1), sDivTblHTML(-1)
    End Sub
    Public Enum addDivTerm
        first
        last
        firstAndLast
        other
    End Enum
    'Public Sub addDivisionHTML(ByVal cfg As Config84, _
    '                       ByVal dividendo As Polynomial84, _
    '                       ByVal divisor As Polynomial84, _
    '                       ByVal cociente As Polynomial84, _
    '                       ByVal multiplier As Polynomial84, _
    '                       ByVal newTerm As Polynomial84, _
    '                       ByVal unionVars() As String, _
    '                       ByVal curTerm As addDivTerm)
    '    Try
    '        Dim t0 As String = _
    '            "Division of: " + _
    '            dividendo.toStringPoly(cfg) _
    '            + " by " + _
    '            divisor.toStringPoly(cfg) + " = <br />"
    '        Dim divTbl As New divisionTable84(cfg, unionVars)
    '        Dim mtx As Matrix = divTbl.oneTermDivision(dividendo, newTerm)
    '        ReDim Preserve sDivTblHTML(iDT)
    '        Dim tbl(2)() As String
    '        Dim i, j As Int32
    '        ReDim Preserve tbl(0)(mtx.vVect(0).vPoly.Length)
    '        For i = 0 To 2
    '            ReDim Preserve tbl(i)(mtx.vVect(0).vPoly.Length)
    '            If i = 0 Then
    '                If curTerm = addDivTerm.first OrElse _
    '                curTerm = addDivTerm.firstAndLast Then
    '                    tbl(i)(0) = "(A) Dividend="
    '                Else
    '                    tbl(i)(0) = "(A) Remainder="
    '                End If
    '            ElseIf i = 1 Then
    '                tbl(i)(0) = "(B) " + divisor.toStringPoly(cfg) + _
    '                    " times " + _
    '                    "<span style=""color:red"">" + _
    '                    multiplier.toStringPoly(cfg) + "</span>(quotient term)= "
    '            Else
    '                If curTerm = addDivTerm.first OrElse _
    '                curTerm = addDivTerm.firstAndLast Then
    '                    tbl(i)(0) = "(A)-(B)= reminder="
    '                ElseIf curTerm = addDivTerm.last Then
    '                    tbl(i)(0) = "(A)-(B)=final reminder="
    '                Else
    '                    tbl(i)(0) = "(A)-(B)= new reminder="
    '                End If
    '            End If
    '            For j = 1 To mtx.vVect(i).vPoly.Length
    '                Dim Pa As Polynomial84 = _
    '                     mtx.vVect(i).vPoly(j - 1)
    '                If (curTerm = addDivTerm.last OrElse _
    '                    curTerm = addDivTerm.firstAndLast) AndAlso _
    '                i = 2 Then
    '                    tbl(i)(j) = "<span style=""color:red"">"
    '                Else
    '                    tbl(i)(j) = ""
    '                End If
    '                If Pa Is Nothing Then
    '                    tbl(i)(j) += " "
    '                Else
    '                    tbl(i)(j) += Pa.toStringPoly(cfg)
    '                    If InStr(tbl(i)(j), "-") = 0 Then
    '                        tbl(i)(j) = "+" + tbl(i)(j)
    '                    End If
    '                    tbl(i)(j) = Replace(tbl(i)(j), "+0", "0")
    '                    If InStr(tbl(i)(j), ">0") Then
    '                        tbl(i)(j) = Replace(tbl(i)(j), "+<", "<")
    '                    End If
    '                End If
    '                If curTerm = addDivTerm.last AndAlso _
    '                i = 2 Then
    '                    tbl(i)(j) += "</span>"
    '                End If
    '            Next
    '        Next
    '        Dim t As String = ""
    '        For i = 0 To 2
    '            t += Join(tbl(i), ";") + vbCrLf
    '        Next
    '        t = t0 + _
    '            MathGlobal8.getTable(t, "grey")
    '        Me.sDivTblHTML(iDT) = t
    '        iDT += 1
    '    Catch ex As Exception
    '        Throw ex
    '    End Try
    'End Sub
    Public Sub addDivisionRTF(ByVal cfg As Config84,
                       ByVal dividendo As Polynomial84,
                       ByVal divisor As Polynomial84,
                       ByVal cociente As Polynomial84,
                       ByVal multiplier As Polynomial84,
                       ByVal newTerm As Polynomial84,
                       ByVal unionVars() As String,
                       ByVal curTerm As addDivTerm)
        Try
            Dim t0 As String =
                "\cf1 Division of: " +
                dividendo.toStringPoly(cfg) _
                + " by " +
                divisor.toStringPoly(cfg) + " = \line"
            Dim divTbl As New divisionTable84(cfg, unionVars)
            Dim mtx()() As Polynomial84 = divTbl.oneTermDivision(dividendo, newTerm)
            ReDim Preserve sDivTblHTML(iDT)
            Dim tbl(2)() As String
            Dim i, j As Int32
            ReDim Preserve tbl(0)(mtx(0).Length)
            For i = 0 To 2
                ReDim Preserve tbl(i)(mtx(0).Length)
                If i = 0 Then
                    If curTerm = addDivTerm.first OrElse
                    curTerm = addDivTerm.firstAndLast Then
                        tbl(i)(0) = "(A) Dividend="
                    Else
                        tbl(i)(0) = "(A) Remainder="
                    End If
                ElseIf i = 1 Then
                    tbl(i)(0) = "(B) " + divisor.toStringPoly(cfg) +
                        " times " +
                        "\cf2 " +
                        multiplier.toStringPoly(cfg) + "\cf1 (quotient term)= "
                Else
                    If curTerm = addDivTerm.first OrElse
                    curTerm = addDivTerm.firstAndLast Then
                        tbl(i)(0) = "(A)-(B)= reminder="
                    ElseIf curTerm = addDivTerm.last Then
                        tbl(i)(0) = "(A)-(B)=final reminder="
                    Else
                        tbl(i)(0) = "(A)-(B)= new reminder="
                    End If
                End If
                For j = 1 To mtx(i).Length
                    Dim Pa As Polynomial84 =
                         mtx(i)(j - 1)
                    If (curTerm = addDivTerm.last OrElse
                        curTerm = addDivTerm.firstAndLast) AndAlso
                    i = 2 Then
                        tbl(i)(j) = "\cf2 "
                    Else
                        tbl(i)(j) = ""
                    End If
                    If Pa Is Nothing Then
                        tbl(i)(j) += " "
                    Else
                        tbl(i)(j) += Pa.toStringPoly(cfg)
                        If InStr(tbl(i)(j), "-") = 0 Then
                            If tbl(i)(j) = "\cf2 0" Then
                                tbl(i)(j) = " " + tbl(i)(j)
                            Else
                                tbl(i)(j) = "+" + tbl(i)(j)
                            End If
                        End If
                        tbl(i)(j) = Replace(tbl(i)(j), "+0", "0")
                        If InStr(tbl(i)(j), ">0") Then
                            tbl(i)(j) = Replace(tbl(i)(j), "+<", "<")
                        End If
                    End If
                    If curTerm = addDivTerm.last AndAlso
                    i = 2 Then
                        tbl(i)(j) += "\cf1 "
                    End If
                Next
            Next
            Dim t As String = ""
            For i = 0 To 2
                t += Join(tbl(i), ";") + vbCrLf
            Next
            t = t0 + getRTF_Tabs(t)
            Me.sDivTblHTML(iDT) = t
            iDT += 1
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Shared Function getRTF_Tabs(ByVal e1 As String) As String
        Dim t As String = "\line"

        Try
            Dim i, j As Int32
            e1 = Replace(e1, "<br />", vbCrLf)
            e1 = Replace(e1, "<br/>", vbCrLf)
            e1 = Replace(e1, "<br>", vbCrLf)
            e1 = Replace(e1, "|", vbCrLf)
            Dim vRow() As String = Split(e1, vbCrLf)
            For i = 0 To vRow.Length - 1
                vRow(i) = Replace(vRow(i), ";;", ";")
                vRow(i) = Replace(vRow(i), ";", vbTab)
                Dim vCol() As String = Split(vRow(i), vbTab)
                Dim t2 As String = vCol(0) + "\line"
                't2 = "<TR ALIGN=""LEFT"" borderColor=" + borderColor + ">"
                't2 = "<TR ALIGN=""LEFT"" borderColor=""#d0d0d0"">"
                Dim bAdd As Boolean = False
                For j = 1 To vCol.Length - 1
                    t2 += "\tab " + vCol(j)
                    If Len(Trim(vCol(j))) Then
                        bAdd = True
                    End If
                Next
                t2 += "\line "
                If bAdd Then
                    t += t2
                End If
            Next
            t += "\line "
            't = Replace(t, "<", "&lt;")
            't = Replace(t, ">", "&gt;")
        Catch ex As Exception
        End Try
        Return t
    End Function

    Public Sub Add(ByVal sExpr As String)
        Try
            sExpr = Replace(sExpr, vbCrLf, "\line")
            If InStr(sExpr, "=") Then
                Dim e1() As String = Split(Replace(sExpr, " ", ""), "=")
                If e1(0) = e1(1) Then
                    Exit Try
                End If
            Else
                Exit Sub
            End If
            Dim pos As Int32 = -1 ' Array.IndexOf(Me.sExpr, sExpr, 0, ie)
            For i = 0 To Me.sExpr.Length - 1
                If Me.sExpr(i).ToString = sExpr Then
                    Exit Try
                End If
            Next
            'If pos = -1 Then
            ReDim Preserve Me.sExpr(ie)
            Me.sExpr(ie) = New StringBuilder(sExpr)
            ie += 1
            'End If
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Sub AddAlways(ByVal sexpr As String)
        Try
            sexpr = Replace(sexpr, vbCrLf, "\line" + vbCrLf)
            ReDim Preserve Me.sExpr(ie)
            Me.sExpr(ie) = New StringBuilder(sexpr)
            ie += 1
        Catch ex As Exception
            Throw ex
        End Try
    End Sub

    'Public Function ToStringHTML() As String
    '    If ie = 0 Then
    '        Return ""
    '    End If
    '    Dim t As String = "Detail:<br><table border=""1"" borderColor=""#d0d0d0"">"
    '    Try
    '        Dim i As Int32
    '        Do While ie > 0 AndAlso sExpr(ie - 1).ToString = "<br />"
    '            ie -= 1
    '        Loop
    '        If ie = 0 Then
    '            Return ""
    '        End If
    '        For i = 0 To ie - 1
    '            t += "<tr borderColor=""#d0d0d0""><td>" + sExpr(i).ToString + "</td></tr>"
    '        Next
    '        t += "</table>"
    '        If Me.iDT Then
    '            For i = 0 To Me.iDT - 1
    '                t += "<br />" + Me.sDivTblHTML(i)
    '            Next
    '        End If
    '    Catch ex As Exception
    '        Throw ex
    '    End Try
    '    Return t
    'End Function
    Public Overloads Function ToString(format As outputMessage) As String
        'If ie = 0 Then
        '    Return ""
        'End If
        'Dim t As String = "Division detail:"
        'If format = outputMessage.RichTextFormat Then
        '    t += "\line"
        'ElseIf format = outputMessage.HTML Then
        '    t += "<br><table border=""1"" borderColor=""#d0d0d0"">"
        'End If
        'Dim i As Int32
        'Do While ie > 0 AndAlso sExpr(ie - 1).ToString = "<br />"
        '    ie -= 1
        'Loop
        'If ie = 0 Then
        '    Return ""
        'End If
        Dim t As String = String.Empty
        Try
            'For i = 0 To ie - 1
            '    If format = outputMessage.RichTextFormat Then
            '        t += "" + sExpr(i).ToString + "\line" + vbCrLf
            '    ElseIf format = outputMessage.HTML Then
            '        t += "<tr borderColor=""#d0d0d0""><td>" + sExpr(i).ToString + "</td></tr>"
            '    End If
            'Next
            'If format = outputMessage.HTML Then
            '    t += "</table>"
            'ElseIf format = outputMessage.RichTextFormat Then
            '    t += "\line"
            'End If
            If Me.iDT Then
                For i = 0 To Me.iDT - 1
                    If format = outputMessage.RichTextFormat Then
                        t += "\line" + Me.sDivTblHTML(i)
                    ElseIf format = outputMessage.HTML Then
                        t += "<br />" + Me.sDivTblHTML(i)
                    End If
                Next
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return t
    End Function
End Class
#End Region

#Region "Detall84_2"
<Serializable()>
Public Class divisionTable84
    Dim tbl(-1)() As String
    Dim iRows As Int32 = 0
    Dim i As Int32 = 0
    Dim Reminder As Polynomial84
    Dim Cociente As Polynomial84
    Dim vars() As String
    Dim cfg As Config84
    Public Sub New(ByVal cfg As Config84,
                   ByVal unionVars() As String)
        Try
            Me.cfg = cfg
            vars = unionVars
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Function oneTermDivision(
                ByVal num As Polynomial84,
                ByVal den As Polynomial84) As Polynomial84()()
        Dim rMtx As Polynomial84()()
        Try
            ReDim rMtx(0), rMtx(0)(-1)
            Dim nSum As Int32 = num.cf.Length
            Dim nSub As Int32 = den.cf.Length
            Dim vSum(nSum - 1) As Polynomial84
            Dim vSumDeg(nSum - 1) As Int32
            Dim vSubs(nSub - 1) As Polynomial84
            Dim vSubDeg(nSub - 1) As Int32
            Dim vR(-1) As Int32, iR As Int32 = 0
            Dim i, j As Int32
            For i = 0 To num.cf.Length - 1
                vSum(i) =
                    num.GetPolyFromTerm(i, vars)
                vSumDeg(i) = vSum(i).getDegree
            Next
            For i = 0 To den.cf.Length - 1
                vSubs(i) =
                    den.GetPolyFromTerm(i, vars)
                vSubDeg(i) = vSubs(i).getDegree
            Next
            Array.Sort(vSubDeg, vSubs)
            Array.Reverse(vSubDeg)
            Array.Reverse(vSubs)
            Dim mult As Complex84 = Nothing
            Dim vSb2(vSubs.Length - 1) As Int32
            For i = 0 To vSumDeg.Length - 1
                For j = 0 To vSubDeg.Length - 1
                    If vSb2(j) = 0 Then

                        If vSumDeg(i) = vSubDeg(j) Then
                            Dim dA As New Polynomial84(vSum(i))
                            Dim dB As New Polynomial84(vSubs(j))
                            dA.cf(0) = Complex84.one
                            dB.cf(0) = Complex84.one
                            Dim dif As Polynomial84 =
                                dA - dB
                            If dif.isComplex Then
                                ReDim Preserve vR(iR)
                                vR(iR) = (1 + i) * 1000 + j
                                iR += 1
                                vSb2(j) = 1
                                Exit For
                            Else
                                'ReDim Preserve vR(iR)
                                'vR(iR) = i
                                'iR += 1
                                'Exit For
                                'ReDim Preserve vR(iR)
                                'vR(iR) = -j - 1
                                'iR += 1
                                'vSb2(j) = 1
                            End If
                        ElseIf vSumDeg(i) < vSubDeg(j) Then
                            ReDim Preserve vR(iR)
                            vR(iR) = -j - 1
                            iR += 1
                            vSb2(j) = 1
                            If j = vSubDeg.Length - 1 Then
                                Do While i < vSumDeg.Length
                                    ReDim Preserve vR(iR)
                                    vR(iR) = i
                                    iR += 1
                                    i += 1
                                Loop
                                Exit For
                            End If
                        Else 'If vSumDeg(i) > vSubDeg(j) Then
                            ReDim Preserve vR(iR)
                            vR(iR) = i
                            iR += 1
                            Exit For
                        End If
                    End If
                Next
                If j >= vSubs.Length Then
                    ReDim Preserve vR(iR)
                    vR(iR) = i
                    iR += 1
                End If
            Next
            For j = 0 To vSb2.Length - 1
                If vSb2(j) = 0 Then
                    ReDim Preserve vR(iR)
                    vR(iR) = -j - 1
                    iR += 1
                    vSb2(j) = 1
                End If
            Next
            iR = vR.Length
            Dim A(iR - 1), B(iR - 1), C(iR - 1) As Polynomial84
            For i = 0 To iR - 1
                Select Case vR(i)
                    Case Is >= 1000
                        Dim i1 As Int32 = Math.Floor(vR(i) / 1000) - 1
                        j = vR(i) - (i1 + 1) * 1000
                        A(i) = vSum(i1)
                        B(i) = vSubs(j)
                        C(i) = A(i) - B(i)
                    Case Is >= 0
                        A(i) = vSum(vR(i))
                        C(i) = A(i)
                    Case Is < 0
                        B(i) = vSubs(-vR(i) - 1)
                        C(i) = -B(i)
                End Select
            Next
            'rMtx.setRowAndCol(2, iR - 1)
            ReDim Preserve rMtx(2), rMtx(0)(iR - 1), rMtx(1)(iR - 1), rMtx(2)(iR - 1)

            For i = 0 To iR - 1
                rMtx(0)(i) = A(i)
                rMtx(1)(i) = B(i)
                rMtx(2)(i) = C(i)
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return rMtx
    End Function
End Class
#End Region