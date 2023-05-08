Public Class PolyApproximation
    Dim pApprox As Polynomial84
    Dim fnExpr As Expression
    Dim sVar As String
    Dim ini, fin As Double
    Dim nIntervals As Int32
    Dim rootsFound() As Complex84
    Dim bestApprox1 As Complex84
    'Public Sub New(ByVal fnExpr As Expression, ByVal sVar As String, _
    '               ByVal iniRe As Double, ByVal finRe As Double, _
    '               ByVal nIntervals As Int32)
    '    Me.fnExpr = fnExpr
    '    Me.sVar = sVar
    '    ini = iniRe
    '    fin = finRe
    '    Me.nIntervals = nIntervals
    '    findRoots()
    'End Sub
    'Public Function polyApprox(Optional ByVal iniRe As Double = 0, _
    '                           Optional ByVal finRe As Double = 0, _
    '                           Optional ByVal nIntervals As Int32 = -1) As Polynomial84
    '    Try
    '        If nIntervals > -1 Then
    '            ini = iniRe
    '            fin = finRe
    '            Me.nIntervals = nIntervals
    '        End If
    '        If ini >= fin OrElse Me.nIntervals <= 1 Then
    '            Return Nothing
    '        End If
    '        pApprox = Nothing
    '        Dim oInterp As Interpolation = _
    '         Interpolation.getPointsExpr(fnExpr, sVar, _
    '             ini, fin, Me.nIntervals)
    '        Dim Vandermonde As Matrix = oInterp.getVandermondeMatrix()
    '        Dim Mtx As Matrix = Vandermonde * oInterp.An
    '        Dim invMtx As Matrix = Mtx ^ -1
    '    Catch ex As Exception
    '        Throw ex
    '    End Try
    '    Return pApprox
    'End Function
    'Public Function findRoots() As Complex84()
    '    Dim reRoots() As Complex84 = Nothing, iRe As Int32
    '    Try
    '        If pApprox Is Nothing Then
    '            polyApprox()
    '        End If
    '        Dim Va As New Vector(pApprox)
    '        Dim roots() As Complex84 = _
    '            Polynomial84.opRoots(Va).cjo
    '        Dim vVar() As String = New String() {sVar}
    '        Dim minErr As Double = Double.MaxValue
    '        Dim iMin As Int32
    '        For i As Int32 = 0 To roots.Length - 1
    '            Dim cjo As Complex84 = _
    '                Me.fnExpr.evalToCjo(vVar, New Complex84() {roots(i)})
    '            If cjo.esCero Then
    '                ReDim Preserve reRoots(iRe)
    '                reRoots(iRe) = roots(i)
    '                cjo += 1
    '            End If
    '            Dim curErr As Double = cjo.opModulo
    '            If curErr < minErr Then
    '                iMin = i
    '                minErr = curErr
    '            End If
    '        Next
    '        bestApprox1 = roots(iMin)
    '    Catch ex As Exception
    '        Throw ex
    '    End Try
    '    Return reRoots
    'End Function
    Public ReadOnly Property bestApprox() As Complex84
        Get
            Return bestApprox1
        End Get
    End Property
End Class
