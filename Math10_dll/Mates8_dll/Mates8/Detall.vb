Imports System.Text.RegularExpressions
Imports System.Text

#Region "class Detall"
<Serializable()> _
Public Class Detall
    Dim v_iCur(-1) As Int64, ivs As Int64 = 0

    Friend sExpr(-1) As StringBuilder, ie As Int64 = 0
    Dim iv, iLastiv As Int64
    Dim vRet(-1) As String
    Dim vM(-1) As String
    Friend cur As currentMatch
    Dim sDivTblHTML(-1) As String, iDT As Int64 = 0
    Public Const rtfInit As String = "{\rtf1\ansi\deff0 {\fonttbl {\f0 Calibri;}}" + vbCrLf + _
    "{\colortbl;\red0\green0\blue0;\red255\green0\blue0;\red204\green85\blue0;}" + vbCrLf
    Dim cfg As Config = Config.cfg
    Dim bFst As Boolean = True
    Dim vDividendo(-1), vDivisor(-1) As Polynomial, ivDD As Int64

    Public Sub New(ByVal cur As currentMatch)
        Me.cur = cur
    End Sub
    Public ReadOnly Property getCur As currentMatch
        Get
            Return Me.cur
        End Get
    End Property
    Public Sub Clear()
        ie = 0
        iv = 0 : iLastiv = 0
        iDT = 0
        bFst = True
        ivDD = 0
        ReDim sExpr(-1), sDivTblHTML(-1), vRet(-1), vM(-1), vDividendo(-1), vDivisor(-1)
    End Sub
    Public Sub ClearDivisions()
        iDT = 0
        ReDim sDivTblHTML(-1)
    End Sub

    Public Shared Function ToStringDetail(cur As currentMatch, mp As matrixParser, oDetall As Detall) As String()
        Return oDetall.vRet
    End Function
    Public Enum addDivTerm
        first
        last
        firstAndLast
        other
    End Enum
    Public Sub addDivision(ByVal cfg As Config, _
                           ByVal dividendo As Polynomial, _
                           ByVal divisor As Polynomial, _
                           ByVal cociente As Polynomial, _
                           ByVal multiplier As Polynomial, _
                           ByVal newTerm As Polynomial, _
                           ByVal unionVars() As String, _
                           ByVal curTerm As addDivTerm)
        Try
            Dim i, j As Int64
            For i = 0 To ivDD - 1
                Dim resta As Polynomial = vDividendo(i) - dividendo
                If resta.isReal AndAlso resta.ToDouble = 0 Then
                    resta = vDivisor(i) - divisor
                    If resta.isReal AndAlso resta.ToDouble = 0 Then
                        Exit Sub
                    End If
                End If
            Next
            ReDim Preserve vDividendo(ivDD), vDivisor(ivDD)
            vDividendo(ivDD) = New Polynomial(dividendo)
            vDivisor(ivDD) = New Polynomial(divisor)
            ivDD += 1
            Dim t0 As String = _
                "Division of: " + _
                dividendo.toStringPoly(cfg) _
                + " by <span style='font-weight:bold;color:navy'> " + _
                divisor.toStringPoly(cfg) + " </span>= <br />"
            Dim divTbl As New divisionTable(cfg, unionVars)
            Dim mtx As Matrix = divTbl.oneTermDivision(dividendo, newTerm)
            ReDim Preserve sDivTblHTML(iDT)
            Dim tbl(2)() As String
            ReDim Preserve tbl(0)(mtx.vVect(0).vPoly.Length)
            For i = 0 To 2
                ReDim Preserve tbl(i)(mtx.vVect(0).vPoly.Length)
                If i = 0 Then
                    If curTerm = addDivTerm.first OrElse _
                    curTerm = addDivTerm.firstAndLast Then
                        tbl(i)(0) = "(A) Dividend="
                    Else
                        tbl(i)(0) = "(A) Remainder="
                    End If
                ElseIf i = 1 Then
                    tbl(i)(0) = "(B) " + divisor.toStringPoly(cfg) + _
                        " times " + _
                        "<span style=""color:red"">" + _
                        multiplier.toStringPoly(cfg) + "</span>(quotient term)= "
                Else
                    If curTerm = addDivTerm.first OrElse _
                    curTerm = addDivTerm.firstAndLast Then
                        tbl(i)(0) = "(A)-(B)= reminder="
                    ElseIf curTerm = addDivTerm.last Then
                        tbl(i)(0) = "(A)-(B)=final reminder="
                    Else
                        tbl(i)(0) = "(A)-(B)= new reminder="
                    End If
                End If
                For j = 1 To mtx.vVect(i).vPoly.Length
                    Dim Pa As Polynomial = _
                         mtx.vVect(i).vPoly(j - 1)
                    If (curTerm = addDivTerm.last OrElse _
                        curTerm = addDivTerm.firstAndLast) AndAlso _
                    i = 2 Then
                        tbl(i)(j) = "<span style=""color:red"">"
                    Else
                        tbl(i)(j) = ""
                    End If
                    If Pa Is Nothing Then
                        tbl(i)(j) += " "
                    Else
                        tbl(i)(j) += Pa.toStringPoly(cfg)
                        If InStr(tbl(i)(j), "-") = 0 Then
                            tbl(i)(j) = "+" + tbl(i)(j)
                        End If
                        tbl(i)(j) = Replace(tbl(i)(j), "+0", "0")
                        If InStr(tbl(i)(j), ">0") Then
                            tbl(i)(j) = Replace(tbl(i)(j), "+<", "<")
                        End If
                    End If
                    If curTerm = addDivTerm.last AndAlso _
                    i = 2 Then
                        tbl(i)(j) += "</span>"
                    End If
                Next
            Next
            Dim t As String = ""
            For i = 0 To 2
                t += Join(tbl(i), ";") + vbCrLf
            Next
            t = t0 + _
                MathGlobal8.getTable(t, "grey")
            't += "<TABLE BORDER=""1"" CELLPADDING=""1"" CELLSPACING=""2"" BORDERCOLOR=""#d0d0d0"" >"
            't += "<tr><td rowspan='3'>"
            't += tbl(0)(0)
            't += "</td>"
            'For i = 0 To 2
            '    If i = 2 Then
            '        t += "<tfoot>"
            '    End If
            '    If i Then
            '        t += "<tr>"
            '    End If
            '    For j = 0 To tbl(i).Length - 1
            '        If j = 0 Then
            '            t += "<td align=""left"">"
            '        Else
            '            t += "<td align=""right"">"
            '        End If
            '        t += tbl(i)(j)
            '        t += "</td>"
            '    Next
            '    t += "</tr>" + vbCrLf
            '    If i = 2 Then
            '        t += "</tfoot>"
            '    End If
            'Next
            't += "</table>"
            Me.sDivTblHTML(iDT) = t
            iDT += 1
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Sub Add(ByVal sExpr As String)
        Try
            If Regex.IsMatch(sExpr, "[,;]") Then
                'Exit Sub
            End If
            sExpr = Replace(sExpr, vbCrLf, "<br>")
            If InStr(sExpr, "=") Then
                Dim e1() As String = Split(Replace(sExpr, " ", ""), "=")
                If e1(0) = e1(1) Then
                    Exit Try
                End If
            Else
                Exit Sub
            End If
            Dim pos As Int64 = -1 ' Array.IndexOf(Me.sExpr, sExpr, 0, ie)
            For i = 0 To Me.sExpr.Length - 1
                If Me.sExpr(i).ToString = sExpr Then
                    Exit Try
                End If
            Next
            'If pos = -1 Then
            sExpr = Replace(sExpr, "+-", "-")
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
            sexpr = Replace(sexpr, vbCrLf, "<br />")
            sexpr = Replace(sexpr, "+-", "-")
            ReDim Preserve Me.sExpr(ie)
            Me.sExpr(ie) = New StringBuilder(sexpr)
            ie += 1
        Catch ex As Exception
            Throw ex
        End Try
    End Sub

    Public Function ToStringDivisionHTML(ByVal borderColor As String) As String

        Dim t As String = String.Empty
        If ie Then
            t = "Detail:<br><table border=""1"" borderColor=""#d0d0d0"">"
            Try
                Dim i As Int64
                Do While ie > 0 AndAlso sExpr(ie - 1).ToString = "<br />"
                    ie -= 1
                Loop
                If ie = 0 Then
                    Return ""
                End If
                For i = 0 To ie - 1
                    t += "<tr borderColor=""#d0d0d0""><td>" + sExpr(i).ToString + "</td></tr>"
                Next
                t += "</table>"
                If Me.iDT Then
                    For i = 0 To Me.iDT - 1
                        t += "<br />" + Me.sDivTblHTML(i)
                    Next
                End If
            Catch ex As Exception
                Throw ex
            End Try
        ElseIf iDT Then
            Dim i As Int64
            For i = 0 To Me.iDT - 1
                t += "<br />" + Me.sDivTblHTML(i)
            Next
        End If
        Return t
    End Function
    Public Shared Function opMultDetail(ByVal eA As Expression, ByVal eB As Expression) As String
        Dim e1 As String = String.Empty
        Dim e2 As String = String.Empty
        Try
            Dim vA() As Expression = eA.splitIntoTerms
            Dim vB() As Expression = eB.splitIntoTerms
            Dim vM(2)() As Expression, iM As Int64 = 0
            If vA.Length + vB.Length <= 2 Then
                Return ""
            End If
            Dim i, j As Int64
            Dim bPoly As Boolean = True
            Dim i0(-1) As Int64
            For i = 0 To vA.Length - 1
                For j = 0 To vB.Length - 1
                    If Len(e1) Then e1 += "+"
                    e1 += vA(i).ToStringExpr(eA.cfg) + "*" + vB(j).ToStringExpr(eA.cfg)
                    If Not vA(i).IsPolynomial OrElse _
                    Not vB(j).IsPolynomial Then
                        bPoly = False
                    End If
                    If bPoly Then
                        ReDim Preserve vM(0)(iM), vM(1)(iM), _
                            vM(2)(iM), i0(iM)
                        vM(0)(iM) = vA(i)
                        vM(1)(iM) = vB(j)
                        vM(2)(iM) = vA(i) * vB(j)
                        Dim poly2 As Polynomial
                        poly2 = vM(2)(i).getPolynomial
                        Dim poly2B As New Polynomial(poly2)
                        poly2B.PolyResto = Nothing
                        poly2B.PolyDivisor = Nothing
                        Dim deg As Int64 = poly2.getDegree
                        If poly2B.isReal AndAlso poly2B.ToDouble = 0 AndAlso _
                        poly2.PolyResto IsNot Nothing Then
                            'deg = poly2.PolyResto.getDegree
                            poly2 = Polynomial.opNormalize(poly2.PolyResto) _
                                / poly2.PolyDivisor
                        Else
                            poly2 = Polynomial.opNormalize(poly2)
                        End If

                        i0(iM) = (deg * 10000 + i * 100 + j)
                        iM += 1

                    End If
                Next
            Next
            'e1 = Regex.Replace(e1, "\+\s*\-", " -") + vbCrLf
            If Not bPoly Then
                e2 = e1
            Else
                'Dim i1(i0.Length - 1) As Int64
                'For i = 0 To vM.Length - 1
                '    Array.Copy(i0, i1, i0.Length)
                '    Array.Sort(i1, vM(i))
                'Next
                e2 = e1 + vbCrLf
                e1 = ""
                Dim poly2 As Polynomial
                For i = 0 To iM - 1
                    Dim pA As Polynomial = vM(0)(i).getPolynomial
                    Dim cffA As Complex = pA.An
                    If cffA.IsZero AndAlso _
                    pA.PolyResto IsNot Nothing Then
                        pA = pA.PolyResto
                        cffA = pA.An
                    End If
                    Dim pB As Polynomial = vM(1)(i).getPolynomial
                    Dim cffb As Complex = pB.An
                    If cffb.IsZero AndAlso _
                    pB.PolyResto IsNot Nothing Then
                        pB = pB.PolyResto
                        cffb = pB.An
                    End If
                    e1 += "+"
                    If Not (cffA.IsReal AndAlso _
                     cffA.pRe.ToDouble = 1.0) Then
                        e1 += cffA.toStringComplex(eA.cfg)
                        e1 += "*"
                    End If
                    If Not (cffb.IsReal AndAlso _
                     cffb.pRe.ToDouble = 1.0) Then
                        e1 += cffb.toStringComplex(eA.cfg)
                        e1 += "*"
                    End If
                    poly2 = vM(2)(i).getPolynomial
                    Dim poly2B As New Polynomial(poly2)
                    poly2B.PolyResto = Nothing
                    poly2B.PolyDivisor = Nothing
                    Dim deg As Int64 = poly2.getDegree
                    If poly2B.isReal AndAlso poly2B.ToDouble = 0 AndAlso _
                    poly2.PolyResto IsNot Nothing Then
                        deg = poly2.PolyResto.getDegree
                        poly2 = Polynomial.opNormalize(poly2.PolyResto) _
                            / poly2.PolyDivisor
                    Else
                        poly2 = Polynomial.opNormalize(poly2)
                    End If
                    If deg OrElse e1 = "+" Then
                        e1 += poly2.toStringPoly(eA.cfg)
                    ElseIf Mid(e1, Len(e1)) = "+" Then
                        e1 += "1"
                    End If
                Next
                If e1.Chars(Len(e1) - 1) = "*" Then
                    e1 = Mid(e1, 1, Len(e1) - 1)
                End If
                If e1.Chars(0) = "+" Then
                    e1 = Mid(e1, 2)
                End If
                e1 += vbCrLf
                Dim i2(i0.Length - 1) As Int64
                'Array.Copy(i0, i2, i0.Length)
                'Array.Sort(i2, vM(0))
                'Array.Copy(i0, i2, i0.Length)
                'Array.Sort(i2, vM(1))
                'Array.Copy(i0, i2, i0.Length)
                'Array.Sort(i2, vM(2))
                'Array.Reverse(vM(0))
                'Array.Reverse(vM(1))
                'Array.Reverse(vM(2))
                'Array.Reverse(i2)
                Array.Clear(i2, 0, i2.Length)

                j = 0
                For i = 0 To vM(2).Length - 1
                    Dim mult As Complex = Nothing
                    If i AndAlso _
                    vM(2)(i - 1).getPolynomial.IsEqual( _
                    vM(2)(i).getPolynomial, mult) Then
                        i2(j) += 1 ' #of equal degrees
                    Else
                        j = i
                    End If
                Next

                Dim e3 As String = String.Empty
                For i = 0 To vM(0).Length - 1
                    'poly2 = _
                    'Polynomial.opNormalize(vM(2)(i).getPolynomial)
                    poly2 = vM(2)(i).getPolynomial
                    Dim cA As Complex = poly2.An
                    If cA.IsZero AndAlso _
                    poly2.PolyResto IsNot Nothing Then
                        poly2 = poly2.PolyResto / poly2.PolyDivisor
                        cA = poly2.An
                    End If
                    If i2(i) = 0 Then
                        e3 += "+" + poly2.toStringPoly(eA.cfg)
                    Else
                        e3 += "+("
                        For j = 0 To i2(i)
                            Dim cffA As Complex = vM(2)(i + j).getPolynomial.An
                            e3 += "+" + cffA.toStringComplex(eA.cfg)
                        Next
                        e3 += ")*" + Polynomial.opNormalize( _
                            vM(2)(i).getPolynomial).toStringPoly(eA.cfg)
                        i += i2(i)
                    End If
                Next
                If e3.Chars(0) = "+" Then
                    e3 = Mid(e3, 2)
                End If
                e2 += e1 + e3
                i = vM(0).Length - 1
            End If



            e2 = Regex.Replace(e2, "\+\s*\-", " -") ' + -  --> -
            e2 = Regex.Replace(e2, "\(\s*\+", "(") '  ( +  --> (
            If e2.Chars(0) = "+" Then
                e2 = Mid(e2, 2)
            End If

        Catch ex As Exception
            Dim s1() As String = Split(ex.ToString, vbCrLf)
        End Try
        Return e2
    End Function
    Public Shared Function opAddtDetail(ByVal eA As Expression, ByVal eB As Expression, bIsMinus As Boolean) As String
        Dim e1 As String = String.Empty
        Try
            Dim vA() As Expression = eA.splitIntoTerms
            Dim vB() As Expression = eB.splitIntoTerms
            If vA.Length + vB.Length <= 2 Then
                Return ""
            End If
            Dim e2 As String = String.Empty
            Dim i, j As Int64
            Dim iM As Int64 = vA.Length
            Dim vM() As Expression = vA
            If vM.Length > 1 Then
                Dim i0(vM.Length - 1) As Int64
                For i = 0 To iM - 1
                    If vM(i).IsPolynomial Then
                        i0(i) = vM(i).getPolynomial.getDegree
                    Else
                        i0(i) = -1
                    End If
                Next
                Array.Sort(i0, vM)
                Array.Reverse(vM)
                vA = vM
            End If
            vM = vB
            iM = vM.Length
            If vM.Length > 1 Then
                Dim i0(vM.Length - 1) As Int64
                For i = 0 To iM - 1
                    If vM(i).IsPolynomial Then
                        i0(i) = vM(i).getPolynomial.getDegree
                    Else
                        i0(i) = -1
                    End If
                Next
                Array.Sort(i0, vM)
                Array.Reverse(vM)
                vB = vM
            End If

            ' Detailing addition of terms of equal order:
            For i = 0 To vA.Length - 1
                For j = 0 To vB.Length - 1
                    If vB(j) IsNot Nothing Then
                        Dim mult As Complex = Nothing
                        If vA(i).isEqualTo(vB(j), mult) AndAlso _
                        Not (vA(i).IsReal OrElse vA(i).IsComplex) Then
                            If Len(e2) Then e2 += "+"
                            If vA(i).IsPolynomial Then
                                Dim cffA As New Complex(vA(i).getPolynomial.An)
                                Dim Pa As Polynomial = Polynomial.opNormalize(vA(i).getPolynomial)
                                Dim cffB As Complex = vB(j).getPolynomial.An
                                e2 += "(" + cffA.toStringComplex(eA.cfg) + _
                               IIf(bIsMinus, " - ", " + ") + _
                               cffB.toStringComplex(eA.cfg) + ")" + _
                                    "*" + Pa.toStringPoly(eA.cfg)
                            Else
                                e2 += "(" + Complex.one.toStringComplex(eA.cfg) + _
                              " + " + mult.toStringComplex(eA.cfg) + ")" + _
                                    "*" + vA(i).ToStringExpr(eA.cfg)
                            End If
                            vA(i) = Nothing
                            vB(j) = Nothing
                            Exit For
                        End If
                    End If
                Next
            Next
            If e2 = "" Then
                Return e2
            End If
            For i = 0 To vA.Length - 1
                If vA(i) IsNot Nothing Then
                    e2 += "+" + vA(i).ToStringExpr(eA.cfg)
                End If
            Next
            For i = 0 To vB.Length - 1
                If vB(i) IsNot Nothing Then
                    e2 += IIf(bIsMinus, " - ", " + ") _
                        + vB(i).ToStringExpr(eA.cfg)
                End If
            Next
            e2 = Regex.Replace(e2, "\-\s*\-", " +")
            e2 = Regex.Replace(e2, "\+\s*\-", " -")
            If e2.Chars(0) = "+" Then
                e2 = Mid(e2, 2)
            End If
            e1 += e2

        Catch ex As Exception

        End Try
        Return e1
    End Function


End Class
#End Region

#Region "Detall_2"
<Serializable()> _
Public Class divisionTable
    Dim tbl(-1)() As String
    Dim iRows As Int64 = 0
    Dim i As Int64 = 0
    Dim Reminder As Polynomial
    Dim Cociente As Polynomial
    Dim vars() As String
    Dim cfg As Config
    Public Sub New(ByVal cfg As Config, _
                   ByVal unionVars() As String)
        Try
            Me.cfg = cfg
            vars = unionVars
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Function oneTermDivision( _
                ByVal num As Polynomial, _
                ByVal den As Polynomial) As Matrix
        Dim rMtx As New Matrix()
        Try
            Dim nSum As Int64 = num.cf.Length
            Dim nSub As Int64 = den.cf.Length
            Dim vSum(nSum - 1) As Polynomial
            Dim vSumDeg(nSum - 1) As Int64
            Dim vSubs(nSub - 1) As Polynomial
            Dim vSubDeg(nSub - 1) As Int64
            Dim vR(-1) As Int64, iR As Int64 = 0
            Dim i, j As Int64
            For i = 0 To num.cf.Length - 1
                vSum(i) = _
                    num.GetPolyFromTerm(i, num.var)
                vSumDeg(i) = vSum(i).getDegree
            Next
            'Array.Sort(vSumDeg, vSum)
            'Array.Reverse(vSum)
            'Array.Reverse(vSumDeg)
            For i = 0 To den.cf.Length - 1
                vSubs(i) = _
                    den.GetPolyFromTerm(i, den.var)
                vSubDeg(i) = vSubs(i).getDegree
            Next
            Array.Sort(vSubDeg, vSubs)
            Array.Reverse(vSubDeg)
            Array.Reverse(vSubs)
            Dim mult As Complex = Nothing
            Dim vSb2(vSubs.Length - 1) As Int64
            For i = 0 To vSumDeg.Length - 1
                For j = 0 To vSubDeg.Length - 1
                    If vSb2(j) = 0 Then

                        If vSumDeg(i) = vSubDeg(j) Then
                            Dim dA As New Polynomial(vSum(i))
                            Dim dB As New Polynomial(vSubs(j))
                            dA.cf(0) = Complex.one
                            dB.cf(0) = Complex.one
                            Dim dif As Polynomial = _
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
            Dim A(iR - 1), B(iR - 1), C(iR - 1) As Polynomial
            For i = 0 To iR - 1
                Select Case vR(i)
                    Case Is >= 1000
                        Dim i1 As Int64 = Math.Floor(vR(i) / 1000) - 1
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
            rMtx.setRowAndCol(2, iR - 1)
            For i = 0 To iR - 1
                rMtx.vVect(0).vPoly(i) = A(i)
                rMtx.vVect(1).vPoly(i) = B(i)
                rMtx.vVect(2).vPoly(i) = C(i)
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return rMtx
    End Function
End Class
#End Region