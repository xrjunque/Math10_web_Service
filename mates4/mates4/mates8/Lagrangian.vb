Imports System.Text

Public Class Lagrangian

    Public nId As Int64
    Dim PolyFn As Polynomial
    Dim ptsMtx As Matrix
    Dim nIntervals As Int64
    Public retPoly As Polynomial
    Public Sub New(ByVal pointsMtx As Matrix)
        Me.ptsMtx = pointsMtx
        nIntervals = Me.ptsMtx.vVect.Length - 1
    End Sub

    Private Function getPointsExpr(ByVal PolyFn As Polynomial, _
                                         ByVal a As Double, _
                                         ByVal b As Double, _
                                         ByVal nIntervals As Int64) As Matrix
        Dim Mc As New Matrix()
        Dim db As Double
        Try
            If Me.nIntervals = 0 Then
                If nIntervals < 1 Then
                    nIntervals = 1
                End If
                Me.nIntervals = nIntervals
            End If
            ReDim Mc.vVect(nIntervals)
            Dim i As Int64
            For i = 0 To nIntervals
                Mc.vVect(i) = New Vector(0.0)
                ReDim Mc.vVect(i).vPoly(1)
                db = ((nIntervals - i) * a + b * i) / nIntervals
                Dim cjo As New Complex(db, 0)
                Mc.vVect(i).vPoly(0) = New Polynomial(cjo)
                Mc.vVect(i).vPoly(1) = New Polynomial(PolyFn.evalCjo(cjo))
            Next
        Catch ex As Exception
            Throw New Exception( _
            String.Format(msg8.msg(24), _
            db.ToString(MathGlobal8.us)))
        End Try
        Return Mc
    End Function
    Public Function lagrangianinterpolation() As Polynomial
        Try
            retPoly = New Polynomial(0.0)
            ' For Lagrangian interpolation let's obtain Lagragian polynomial, this is,
            ' calculate Gn(x) = retPoly = sum[k=0 to n] of( products(j=0 to n, j<>k) of ((x-xj)/(xk - xj)).
            ' i.e.:
            '                     n              n
            '                   ------        -------
            '                   \              |   |  (x - xj)
            ' Gn(x) = retPoly = /      f(xk) * |   | ----------
            '                   -----          |   |  (xk- xj)
            '                   k=0             k<>j
            '
            ' where x is the variable of our Polynomial
            ' and xj, xk (0<=j,k<=n  are the given points
            ' (we have previouly taken nInterval=n points equidistant
            ' between the given points a and b)
            ' and f(xk) is the value, in the point xk, of the function to approximate.
            '
            Dim j, k As Int64
            Dim x As Polynomial = Polynomial.GetPolynomial("x")
            For k = 0 To nIntervals
                Dim polyNumerator As New Polynomial(1.0)
                Dim cjoDenominator As New Complex(1.0, 0.0) ' f(xk)
                For j = 0 To nIntervals
                    If j <> k Then
                        polyNumerator *= (x - ptsMtx.vVect(j).vPoly(0))
                        cjoDenominator *= (ptsMtx.vVect(k).vPoly(0).cf(0) - _
                                   ptsMtx.vVect(j).vPoly(0).cf(0))
                    End If
                Next
                retPoly += polyNumerator * ptsMtx.vVect(k).vPoly(1) / New Polynomial(cjoDenominator)
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return retPoly
    End Function
    Public Overrides Function ToString() As String
        If retPoly Is Nothing Then
            lagrangianinterpolation()
        End If
        Dim e1 As String = "Polynomial approximation:" + vbCrLf
        e1 += retPoly.ToString()
        Return e1
    End Function
End Class
