Imports System.Text.RegularExpressions
Imports System.Text

Public Class Functions84

    Public Shared Function busquedaDicotomica(ByVal ini As Double, ByVal fin As Double, ByVal fn As Polynomial84) As Complex84()
        Dim a As Complex84 = Nothing, fa As New Complex84(0.0)
        Dim fb As New Complex84(0.0), b As Complex84 = Nothing
        Dim fM As New Complex84(0.0), M As Complex84 = Nothing
        Dim nVueltas As Int32 = 0
        If ini > fin Then
            a = New Complex84(fin, 0) : b = New Complex84(ini, 0)
        Else
            a = New Complex84(ini, 0) : b = New Complex84(fin, 0)
        End If
        Try
            'If Not fn.tryEvalReDecimal(a.pRe.aDouble, fa.pRe.aDouble) Then
            fa.pRe = fn.evalPrecis84(a.pRe)
            'End If
            'If Not fn.tryEvalReDecimal(b.pRe.aDecimal, fb.pRe.aDecimal) Then
            fb.pRe = fn.evalPrecis84(b.pRe)
            'End If
        Catch ex As Exception
            Return Nothing
        End Try
        Dim sA As Int32 = fa.pRe.sgn
        Dim sB As Int32 = fb.pRe.sgn
        Dim sM As Int32
        If sA = 0 Then
            Dim retA() As Complex84 = {a, fa}
            Return retA
        ElseIf sB = 0 Then
            Dim retB() As Complex84 = {b, fb}
            Return retB
        End If
        If ini = fin Then Return Nothing
        'If Math.Sign(Math.Round(fa.Re, 10)) = Math.Sign(Math.Round(fb.Re, 10)) Then Return Nothing
        If sA = sB Then Return Nothing
        Dim iQuitDecimal As Int32 = 0
        Do
            M = (a + b) / 2
            'If iQuitDecimal > 40 Then
            fM.pRe = fn.evalPrecis84(M.pRe)
            'ElseIf Not fn.tryEvalReDecimal(M.pRe.aDecimal, fM.pRe.aDecimal) Then
            '   fM.pRe = fn.evalPrecis84(M.pRe)
            '  iQuitDecimal += 1
            'End If
            sM = fM.pRe.sgn
            If sA = sM Then
                If (a - M).esCero Then 'a.pRe.aDouble = M.pRe.aDouble Then
                    Exit Do
                End If
                a = M : fa = fM
            ElseIf sB = sM Then
                If (b - M).esCero Then ' b.pRe.aDouble = M.pRe.aDouble Then
                    Exit Do
                End If
                b = M : fb = fM
            Else
                Exit Do
            End If
            nVueltas += 1
        Loop While nVueltas < 2000

        'If fM.opModulo > 10 ^ -3 Then
        '    If Not Math.Abs((M.opModulo - fM.opModulo) / fM.opModulo) > 10 ^ 10 Then
        '        Return Nothing
        '    End If
        'End If
        If fb.opModulo < fM.opModulo Then
            M = b : fM = fb
        ElseIf fa.opModulo < fM.opModulo Then
            M = a : fM = fa
        End If
salida:
        M = New Complex84(M.pRe.ToDecimal)
        Dim ret() As Complex84 = {M, fM}
        Return ret
    End Function
    Public Shared Function busquedaDicotomicaDbl(ByVal ini As Double, ByVal fin As Double, ByVal fn As Polynomial84) As Complex84()
        Dim a, b, fa, fb, M, fM As Double
        Dim nVueltas As Int32 = 0
        Try
            If ini > fin Then
                a = fin : b = ini
            Else
                a = ini : b = fin
            End If
            Try
                fa = fn.evalRe(a)
                fb = fn.evalRe(b)
            Catch ex As Exception
                Return Nothing
            End Try
            Dim sA As Int32 = Math.Sign(fa)
            Dim sB As Int32 = Math.Sign(fb)
            Dim sM As Int32
            If sA = 0 Then
                Dim retA() As Complex84 = {
                    New Complex84(a), New Complex84(fa)}
                Return retA
            ElseIf sB = 0 Then
                Dim retB() As Complex84 = {
                    New Complex84(b), New Complex84(fb)}
                Return retB
            End If
            If ini = fin Then Return Nothing
            If sA = sB Then Return Nothing
            Dim iQuitDecimal As Int32 = 0
            Do
                M = (a + b) / 2
                'fM = fn.evalRe(M)
                fM = fn.evalPrecis84(New Precis84(M)).ToDouble
                sM = Math.Sign(fM)
                If sA = sM Then
                    If a = M Then
                        Exit Do
                    End If
                    a = M : fa = fM
                ElseIf sB = sM Then
                    If b = M Then
                        Exit Do
                    End If
                    b = M : fb = fM
                Else
                    Exit Do
                End If
                nVueltas += 1
            Loop While nVueltas < 2000

            If Math.Abs(fb) < Math.Abs(fM) Then
                M = b : fM = fb
            ElseIf Math.Abs(fa) < Math.Abs(fM) Then
                M = a : fM = fa
            End If
        Catch ex As Exception
            Return Nothing
        End Try
        Dim ret() As Complex84 = {
            New Complex84(M), New Complex84(fM)}
        Return ret
    End Function

    Public Shared Function busquedaDicotomicaPr(ByVal ini As Double, ByVal fin As Double, ByVal fn As Polynomial84) As Complex84()
        Dim a, b, fa, fb, M, fM As Precis84
        Dim nVueltas As Int32 = 0
        Dim mult As Double = 10 ^ 15
        ini = Math.Round(ini * 10 ^ 10) / 10 ^ 10
        fin = Math.Round(fin * 10 ^ 10) / 10 ^ 10
        If ini > fin Then
            a = New Precis84(fin)
            b = New Precis84(ini, mult)
        Else
            a = New Precis84(ini)
            b = New Precis84(fin)
        End If
        Try
            fa = fn.evalPrecis84(a)
            fb = fn.evalPrecis84(b)
        Catch ex As Exception
            Return Nothing
        End Try
        Dim sA As Int32 = fa.sgn
        Dim sB As Int32 = fb.sgn
        Dim sM As Int32
        If sA = 0 Then
            Dim retA(1) As Complex84
            retA(0) = New Complex84(a.ToDouble)
            retA(1) = New Complex84(fa.ToDouble)
            Return retA
        ElseIf sB = 0 Then
            Dim retB(1) As Complex84
            retB(0) = New Complex84(b.ToDouble)
            retB(1) = New Complex84(fb.ToDouble)
            Return retB
        End If
        If ini = fin Then Return Nothing
        If sA = sB Then Return Nothing
        Dim iQuitDecimal As Int32 = 0
        Do
            M = (a + b) / 2
            fM = fn.evalPrecis84(M)
            sM = fM.sgn
            If fM.IsZero Then
                Exit Do
            End If
            If sA = sM Then
                'If (a - M).IsZero Then
                '    Exit Do
                'End If
                a = M : fa = fM
            ElseIf sB = sM Then
                'If (b - M).IsZero Then
                '    Exit Do
                'End If
                b = M : fb = fM
            Else
                Exit Do
            End If
            nVueltas += 1
        Loop While nVueltas < 2000

        If Math.Abs(fb.ToDouble) < Math.Abs(fM.ToDouble) Then
            M = b : fM = fb
        ElseIf Math.Abs(fa.ToDouble) < Math.Abs(fM.ToDouble) Then
            M = a : fM = fa
        End If
salida:
        Dim ret() As Complex84 = {
            New Complex84(0), New Complex84(0)}
        ret(0).pRe = New Precis84(M)
        ret(1).pRe = New Precis84(fM)
        Return ret
    End Function
    Public Shared Function busquedaMinimoEnIntervalo(ByVal ini As Double,
                                                     ByVal fin As Double,
                                                     ByVal fn As Polynomial84,
                                                     ByVal numIntervalos As Int32) As Complex84()
        Dim fMin As Double = Double.MaxValue
        Dim xMin As Double
        Try
            If ini < fin Then
                Dim aux As Double = ini
                ini = fin : fin = aux
            End If
            xMin = ini : fMin = Math.Abs(fn.evalRe(ini))
            If numIntervalos < 2 Then
                Exit Try
            End If
            For x As Double = ini To fin Step (fin - ini) / numIntervalos
                Dim fCur As Double = Math.Abs(fn.evalRe(x))
                If fCur < fMin Then
                    xMin = x
                    fCur = fMin
                End If
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return New Complex84() {
            New Complex84(xMin),
            New Complex84(fMin)
            }
    End Function
    'Public Shared Function Inverse_Matrix_GaussPivotTotal_poly( _
    '                        ByVal Ma As Matrix, ByRef sErrMsg As String) As Matrix
    '    Dim i1, i, j As Int32 ', M, N As Int32

    '    Dim dsp As Boolean = False
    '    Dim iIni As Int32 = 0
    '    Dim zero As New Complex84(0, 0)
    '    Dim uno As New Complex84(1, 0)
    '    Dim polyZero As New Polynomial84(zero)
    '    Dim polyUno As New Polynomial84(uno)
    '    Dim Mc As New Matrix(Ma)
    '    Dim retGC As New retGaussElim_Mtx
    '    Dim nRows As Int32 = Ma.vVect.Length
    '    Dim nCols As Int32 = Ma.vVect(0).vPoly.Length
    '    Dim B As New Matrix
    '    Dim nomVars(nRows - 1) As Int32
    '    Try
    '        'Array.Copy(nomVar, nomVars, nomVar.Length)
    '        Dim rows(nRows - 1), cols(nCols - 1) As Int32
    '        ReDim B.vVect(nRows - 1)
    '        For i = 0 To rows.Length - 1
    '            B.vVect(i) = New Vector
    '            If rows.Length > 1 Then
    '                Mc.vVect(i) = Vector.opConjugate(Ma.vVect(i))
    '            Else
    '                Mc.vVect(i) = New Vector(Ma.vVect(i))
    '            End If
    '            ReDim B.vVect(i).vPoly(nCols - 1)
    '            For j = 0 To nCols - 1
    '                B.vVect(i).vPoly(j) = polyZero
    '                If j = i Then
    '                    B.vVect(i).vPoly(j) = polyUno
    '                End If
    '            Next
    '            rows(i) = i : cols(i) = i
    '            'B(i) = Bm(i)
    '        Next
    '        'Dim M, N As Int32
    '        ' 1) conseguir ceros debajo de la diagonal:
    '        'For i1 = 0 To nRows - 2
    '        'M = i1 : N = i1
    '        'Dim norm As Double = 0 'Mc.vVect(M).vPoly(cols(N)).cf(0).opNorm
    '        'Dim dbk1 As Double = 0.0
    '        'For k1 As Int32 = 0 To Mc.vVect(M).vPoly(cols(N)).cf.Length - 1
    '        '    dbk1 = Mc.vVect(M).vPoly(cols(N)).cf(k1).opNorm
    '        '    If dbk1 > norm Then
    '        '        norm = dbk1
    '        '    End If
    '        'Next
    '        'For i = i1 + 1 To nRows - 1
    '        '    For j = i1 To i1 ' B.Length - 1
    '        '        Dim norm2 As Double = 0.0
    '        '        Dim dbk2 As Double = 0.0
    '        '        For k2 As Int32 = 0 To Mc.vVect(rows(i)).vPoly(cols(cols(j))).cf.Length - 1
    '        '            dbk2 = Mc.vVect(rows(i)).vPoly(cols(j)).cf(k2).opNorm
    '        '            If dbk2 > norm2 Then
    '        '                norm2 = dbk2
    '        '            End If
    '        '        Next
    '        '        If norm < norm2 Then
    '        '            M = i : N = j
    '        '        End If
    '        '    Next
    '        'Next
    '        'If M <> i1 OrElse N <> i1 Then
    '        '    Dim ijt As Int32 = rows(M)
    '        '    rows(M) = rows(i1)
    '        '    rows(i1) = ijt

    '        '    ijt = cols(N)
    '        '    cols(N) = cols(i1)
    '        '    cols(i1) = ijt
    '        'End If

    '        'M = i1 : N = i1
    '        'Dim norm As Double = Mc.vVect(M).vPoly(cols(N)).cf(0).opNorm
    '        'For i = i1 + 1 To nRows - 1
    '        '    For j = i1 To i1 ' B.Length - 1
    '        '        If norm < Mc.vVect(rows(i)).vPoly(cols(j)).cf(0).opNorm Then
    '        '            M = i : N = j
    '        '        End If
    '        '    Next
    '        'Next
    '        'If M <> i1 OrElse N <> i1 Then
    '        '    Dim ijt As Int32 = rows(M)
    '        '    rows(M) = rows(i1)
    '        '    rows(i1) = ijt

    '        '    ijt = cols(N)
    '        '    cols(N) = cols(i1)
    '        '    cols(i1) = ijt
    '        'End If

    '        'For i = i1 + 1 To nRows - 1
    '        '    Dim poly As Polynomial84 = Mc.vVect(rows(i)).vPoly(cols(i1))
    '        '    'If cjo.Re <> 0 OrElse cjo.Im <> 0 Then
    '        '    If Not (Mc.vVect(rows(i1)).vPoly(cols(i1)).isComplex AndAlso _
    '        '            Mc.vVect(rows(i1)).vPoly(cols(i1)).cf(0).esCero) Then
    '        '        Dim alfa As Polynomial84 = poly / Mc.vVect(rows(i1)).vPoly(cols(i1))
    '        '        If Not (alfa.isComplex AndAlso alfa.cf(0).esCero) Then
    '        '            For j = i1 + 1 To nRows - 1
    '        '                Mc.vVect(rows(i)) -= _
    '        '                    Mc.vVect(rows(i1)) * alfa
    '        '                B.vVect(rows(i)) -= _
    '        '                    B.vVect(rows(i1)) * alfa
    '        '            Next
    '        '        End If
    '        '    End If
    '        '    Mc.vVect(rows(i)).vPoly(cols(i1)) = polyZero
    '        'Next
    '        'Next
    '        ' conseguir unos en la diagonal:
    '        'For i = nRows - 1 To 0 Step -1
    '        '    'Dim cjoMult As Polynomial84 = polyUno / Mc.vVect(rows(i)).vPoly(cols(i))
    '        '    'Mc.vVect(rows(i)) *= cjoMult
    '        '    'B.vVect(rows(i)) *= cjoMult
    '        '    Dim polyDiv As New Polynomial84(Mc.vVect(rows(i)).vPoly(cols(i)))
    '        '    For k As Int32 = 0 To nCols - 1
    '        '        Mc.vVect(rows(i)).vPoly(k) /= polyDiv
    '        '        B.vVect(rows(i)).vPoly(k) /= polyDiv
    '        '    Next
    '        '    For j = i - 1 To 0 Step -1
    '        '        Dim cjo As Polynomial84 = Mc.vVect(rows(j)).vPoly(cols(i))
    '        '        If Not (cjo.isComplex AndAlso cjo.cf(0).esCero) Then
    '        '            Dim vI As Vector = B.vVect(rows(i)) * cjo
    '        '            Dim vJ As Vector = B.vVect(rows(j)) * Mc.vVect(rows(i)).vPoly(cols(i))
    '        '            B.vVect(rows(j)) = vJ - vI
    '        '        End If
    '        '    Next
    '        'Next
    '        'retGC.mtx = New Matrix(B)
    '        'For i = 0 To nRows - 1
    '        '    Dim posI As Int32 = Array.IndexOf(rows, i)
    '        '    For j = 0 To nCols - 1
    '        '        Dim posJ As Int32 = Array.IndexOf(cols, j)
    '        '        retGC.mtx.vVect(i).vPoly(j).cf(0) = _
    '        '            B.vVect(posI).vPoly(posJ).cf(0)
    '        '    Next
    '        'Next

    '        'salto:
    '        ' Conseguir ceros debajo de la diagonal:
    '        For iCol As Int32 = 0 To nCols - 2
    '            i1 = iCol
    '            Dim M, N As Int32
    '            M = i1 : N = i1
    '            Dim norm As Double = Mc.vVect(M).vPoly(cols(N)).cf(0).opNorm
    '            For i = i1 + 1 To nRows - 1
    '                For j = i1 To i1 ' B.Length - 1
    '                    If norm < Mc.vVect(rows(i)).vPoly(cols(j)).cf(0).opNorm Then
    '                        M = i : N = j
    '                    End If
    '                Next
    '            Next
    '            If M <> i1 OrElse N <> i1 Then
    '                Dim ijt As Int32 = rows(M)
    '                rows(M) = rows(i1)
    '                rows(i1) = ijt

    '                ijt = cols(N)
    '                cols(N) = cols(i1)
    '                cols(i1) = ijt
    '            End If
    '            Dim polyCol As New Polynomial84(Mc.vVect(rows(iCol)).vPoly(cols(iCol)))
    '            For i = iCol + 1 To nCols - 1
    '                Dim polyI As New Polynomial84(Mc.vVect(rows(i)).vPoly(cols(iCol)))
    '                If Not (polyI.isComplex AndAlso polyI.cf(0).esCero) Then
    '                    Dim vICol As Vector = Mc.vVect(rows(iCol)) * polyI
    '                    Dim vI As Vector = polyCol * Mc.vVect(rows(i))
    '                    Mc.vVect(rows(i)) = vI - vICol
    '                    vICol = B.vVect(rows(iCol)) * polyI
    '                    vI = polyCol * B.vVect(rows(i))
    '                    B.vVect(rows(i)) = vI - vICol
    '                End If
    '            Next
    '        Next
    '        ' Conseguir ceros encima de la diagonal:
    '        For iCol = nCols - 1 To 1 Step -1
    '            Dim polyCol As New Polynomial84(Mc.vVect(rows(iCol)).vPoly(cols(iCol)))
    '            For i = iCol - 1 To 0 Step -1
    '                Dim polyI As New Polynomial84(Mc.vVect(rows(i)).vPoly(cols(iCol)))
    '                If Not (polyI.isComplex AndAlso polyI.cf(0).esCero) Then
    '                    Dim vICol As Vector = Mc.vVect(rows(iCol)) * polyI
    '                    Dim vI As Vector = polyCol * Mc.vVect(rows(i))
    '                    Mc.vVect(rows(i)) = vI - vICol
    '                    vICol = B.vVect(rows(iCol)) * polyI
    '                    vI = polyCol * B.vVect(rows(i))
    '                    B.vVect(rows(i)) = vI - vICol
    '                End If
    '            Next
    '        Next
    '        ' Conseguir unos en la diagonal:
    '        For i = 0 To nRows - 1
    '            Dim div As Polynomial84 = Mc.vVect(rows(i)).vPoly(cols(i))
    '            If div.isReal AndAlso _
    '            div.ToDouble = 0 Then
    '                sErrMsg = msg884.num(51)
    '                Return Nothing
    '            End If
    '            'Dim polyDiag As Polynomial84 = polyUno / _
    '            'Mc.vVect(rows(i)).vPoly(cols(i))
    '            Dim polyDiag As Polynomial84 = polyUno / div
    '            B.vVect(rows(i)) *= polyDiag
    '            'Mc.vVect(i) *= polyDiag
    '        Next

    '        retGC.mtx = New Matrix(B)
    '        For i = 0 To nRows - 1
    '            'B.vVect(rows(i)) = Vector.opConjugate(B.vVect(rows(i)))
    '            For j = 0 To nCols - 1
    '                retGC.mtx.vVect(i).vPoly(j) = _
    '                    New Polynomial84(B.vVect(rows(i)).vPoly(cols(j)))
    '            Next
    '        Next
    '    Catch ex As Exception
    '        Throw New Exception("Inverse matrix: n/a")
    '    End Try
    '    Return retGC.mtx
    'End Function

    'Public Shared Function gcd(ByVal vectA As Vector) As Int32
    '    Try
    '        Dim Re1 As Double = vectA.vPoly(0).cf(0).pRe.ToDouble
    '        Dim Re2 As Double = vectA.vPoly(1).cf(0).pRe.ToDouble
    '        If Re1 = 0 Then
    '            Return Re2
    '        ElseIf Re2 = 0 Then
    '            Return Re1
    '        End If
    '        Re1 = Math.Abs(Re1)
    '        Re2 = Math.Abs(Re2)
    '        Dim larger As Int32 = Math.Max(Re1, Re2)
    '        Dim smaller As Int32 = Math.Min(Re1, Re2)
    '        Do
    '            Dim remainder As Int32 = larger Mod smaller
    '            If remainder = 0 Then
    '                Return smaller
    '            End If
    '            smaller = remainder
    '        Loop
    '    Catch ex As Exception

    '    End Try
    '    Return 0
    'End Function


    '    Public Shared Function ImagRoots_Newton_Raphson(ByVal pa As Polynomial84, ByVal cfg As Config84) As Complex84()
    '        Dim root(-1) As Complex84, ir As Int32 = 0
    '        Try
    '            Dim ts As New TimeSpan(Now.Ticks)
    '            Dim degree = pa.getDegree
    '            If pa.hasComplexCoeff Then
    '                Exit Try
    '            ElseIf degree < 3 Then
    '                If degree = 0 Then
    '                    Exit Try
    '                ElseIf degree = 2 Then
    '                    Dim a1, b1, c1 As Complex84
    '                    a1 = pa.cf(0)
    '                    If pa.cf.Length = 2 Then
    '                        c1 = pa.cf(1)
    '                        ReDim root(1)
    '                        'root(0) = -(-c1 / a1) ^ 0.5 ' -(-a * c) ^ 0.5 / a
    '                        'root(1) = (-c1 / a1) ^ 0.5 ' (-a * c) ^ 0.5 / a
    '                        root(0) = -(-c1 / a1) ^ Complex84.oneHalf  ' 2013/08/09
    '                        root(1) = (-c1 / a1) ^ Complex84.oneHalf  '  2013/08/09
    '                        Exit Try
    '                    Else
    '                        b1 = pa.cf(1)
    '                        c1 = pa.cf(2)
    '                        ReDim root(1)
    '                        'root(0) = (-b1 + (b1 * b1 - 4 * a1 * c1) ^ 0.5) / (2.0 * a1)
    '                        'root(1) = (-b1 - (b1 * b1 - 4 * a1 * c1) ^ 0.5) / (2.0 * a1)
    '                        root(0) = (-b1 + (b1 * b1 - 4 * a1 * c1) ^ Complex84.oneHalf) / (2.0 * a1) '  2013/08/09
    '                        root(1) = (-b1 - (b1 * b1 - 4 * a1 * c1) ^ Complex84.oneHalf) / (2.0 * a1) '  2013/08/09
    '                        Exit Try
    '                    End If
    '                End If
    '            End If
    '            If degree Mod 2 Then
    '                Exit Try
    '            End If
    '            Dim maxVueltas As Int32 = 100
    '            Do
    '                Dim Der As Polynomial84 = pa.opDerivative(pa.var(0))
    '                Dim x0 As New Complex84(0.9, -0.5)
    '                If Der.evalCjo(x0).IsZero Then
    '                    x0 = New Complex84(0.5, -0.9)
    '                End If
    '                Dim nVueltas As Int32 = 0
    '                Dim dif As Complex84 = Nothing
    '                Dim errMax As Double = pa.cf(0).opNorm / 10 ^ (pa.getDegree ^ 2)
    '                Do
    '                    Dim den As Complex84 = Der.evalCjo(x0)
    '                    If den.IsZero Then
    '                        den = New Complex84(0.0)
    '                    End If
    '                    Dim x1 As Complex84 = _
    '                         x0 - pa.evalCjo(x0) / Der.evalCjo(x0)
    '                    dif = x1 - x0
    '                    nVueltas += 1
    '                    x0 = x1
    '                    Dim ts2 As New TimeSpan(Now.Ticks - ts.Ticks)
    '                    If ts2.TotalMilliseconds > 2 * cfg.timeOutms / 3 Then
    '                        nVueltas = maxVueltas
    '                    End If
    '                Loop While nVueltas < maxVueltas AndAlso dif.opNorm > 0
    '                ReDim Preserve root(ir + 1)
    '                root(ir) = New Complex84(x0)
    '                x0.pIm.opChgSgn()
    '                root(ir + 1) = New Complex84(x0)
    '                ir += 2
    '                degree -= 2
    '                If True OrElse degree Then
    '                    'Trace.WriteLine(pa.toStringPoly)
    '                    Dim c() As Double = New Double() { _
    '                        1.0, -(2 * x0.pRe).ToDecimal, _
    '                        (x0.pRe * x0.pRe + _
    '                        x0.pIm * x0.pIm).ToDecimal}
    '                    Dim polyC As Polynomial84 = _
    '                        Polynomial84.GetPolyomial(pa.var(0), c)
    '                    pa /= polyC
    '                    pa.PolyResto = Nothing
    '                    pa.PolyDivisor = Nothing
    '                    'Dim pa2 As Polynomial84 = pa * polyC
    '                    'Trace.WriteLine(pa2.toStringPoly)
    '                End If
    '                Dim ts3 As New TimeSpan(Now.Ticks - ts.Ticks)
    '                If ts3.TotalMilliseconds >= 2.5 * CDbl(cfg.timeOutms) / 3.0 Then
    '                    Exit Do
    '                End If
    '            Loop While degree > 0
    '        Catch ex As Exception
    '            GoTo err
    '        End Try
    '        Return root

    'err:    ' something went wrong:

    '        ReDim root(-1)
    '        Return root
    '    End Function
    Shared Function getTable(ByVal e1 As String, ByVal borderColor As String) As String
        Dim t As String = "<TABLE BORDER=""1"" CELLPADDING=""1"" CELLSPACING=""2"" BORDERCOLOR=""#d0d0d0"" >"

        Try
            Dim i, j As Int32
            e1 = Replace(e1, "<br />", vbCrLf)
            e1 = Replace(e1, "<br/>", vbCrLf)
            e1 = Replace(e1, "<br>", vbCrLf)
            e1 = Replace(e1, "|", vbCrLf)
            Dim vRow() As String = Split(e1, vbCrLf)
            For i = 0 To vRow.Length - 1
                vRow(i) = Replace(vRow(i), vbTab, ";")
                vRow(i) = Replace(vRow(i), ";;", ";")
                Dim vCol() As String = Split(vRow(i), ";")
                Dim t2 As String = ""
                't2 = "<TR ALIGN=""LEFT"" borderColor=" + borderColor + ">"
                t2 = "<TR ALIGN=""LEFT"" borderColor=""#d0d0d0"">"
                Dim bAdd As Boolean = False
                For j = 0 To vCol.Length - 1
                    t2 += "<TD wrap> " + vCol(j) + "</TD>"
                    If Len(Trim(vCol(j))) Then
                        bAdd = True
                    End If
                Next
                t2 += "</TR>"
                If bAdd Then
                    t += t2
                End If
            Next
            t += "</TABLE>"
            't = Replace(t, "<", "&lt;")
            't = Replace(t, ">", "&gt;")
        Catch ex As Exception
        End Try
        Return t
    End Function
End Class
