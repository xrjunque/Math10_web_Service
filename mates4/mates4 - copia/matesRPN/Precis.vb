Imports System.Text.RegularExpressions

<Serializable()>
Public Class Precis84
    Private prDblN As Double
    Private prDblD As Double = 1.0
    Private sgn1 As Int32 = 1
    Private exp2 As Int32 = 0 ' Double = 0.0
    'Private expMin As Int32 = -Math.Log(1.0E+200) / Math.Log(2)
    'Private expMax As Int32 = -expMin
    Private vPrime() As Int32 = {3, 5, 7, 11, 13, 17}
    Public Sub New()
    End Sub
    Public Sub New(ByVal opB As Precis84)
        Me.CopyFrom(opB)
    End Sub
    Public Sub CopyFrom(ByVal opB As Precis84)
        opB.actualiza()
        prDblN = opB.prDblN
        prDblD = opB.prDblD
        sgn1 = opB.sgn1
        exp2 = opB.exp2
        'actualiza()
    End Sub
    Public Function getDenominator() As Double
        Dim dbN As Double
        Dim dbD As Double = 1.0
        Try
            If exp2 = 0 AndAlso Me.prDblD = Math.Floor(Me.prDblD) AndAlso
            Me.prDblD < 100000.0 AndAlso
            Me.prDblN < 1000.0 Then
                Return Me.prDblD
            End If
            dbN = Math.Abs(Me.ToDouble)
            Do While dbN <> Math.Floor(dbN)
                dbN *= 10.0
                dbD *= 10.0
            Loop
            If dbN < 10 ^ 6 AndAlso dbD < 10 ^ 6 Then
                Dim g As Double = gcd(dbN, dbD)
                If g > 1.0 Then
                    'dbN /= g
                    dbD /= g
                End If
            End If
        Catch ex As Exception

        End Try
        Return dbD
    End Function
    'Public Property prDblN As Double
    '    Get
    '        Return prDblN1
    '    End Get
    '    Set(value As Double)
    '        If value <> Math.Floor(value) Then
    '            value = value
    '        End If
    '        prDblN1 = value
    '    End Set
    'End Property
    'Public Property prDblD As Double
    '    Get
    '        Return prDblD1
    '    End Get
    '    Set(value As Double)
    '        If value <> Math.Floor(value) Then
    '            value = value
    '        End If
    '        prDblD1 = value
    '    End Set
    'End Property

    Public Sub New(ByVal db As Double)
        Try
            sgn1 = Math.Sign(db)
            If sgn1 = 0 Then Exit Sub
            prDblN = Math.Abs(db)
            If prDblN <> Math.Floor(prDblN) AndAlso
            prDblN < 10 ^ 12 Then
                Do While prDblN <> Math.Floor(prDblN)
                    prDblN *= 2.0
                    exp2 -= 1
                    If Double.IsInfinity(prDblN) Then
                        Throw New IndexOutOfRangeException
                    End If
                    If Double.IsInfinity(prDblD) Then
                        Throw New IndexOutOfRangeException
                    End If
                Loop

                'Dim decN As Decimal = CDec(prDblN)
                'Dim decD As Decimal = CDec(prDblD)
                'Do While decN <> Math.Floor(decN)
                '    decN *= 10D
                '    decD *= 10D
                '    If Double.IsInfinity(prDblN) Then
                '        Throw New Exception(msg884.num(13)) ' n/a
                '    End If
                '    If Double.IsInfinity(prDblD) Then
                '        Throw New Exception(msg884.num(13)) ' n/a
                '    End If
                'Loop
                'prDblN = decN
                'prDblD = decD
            End If

            actualiza()
            'If db <> ToDouble Then
            '    db = ToDouble
            'End If
        Catch ex As Exception
            Dim s1 As String = ex.ToString
            Dim s2 As String = s1
            Throw New Exception(ex.Message)
        End Try
    End Sub
    Public Sub New(ByVal num As Double, ByVal den As Double)
        Try
            sgn1 = Math.Sign(num) * Math.Sign(den)
            prDblN = Math.Abs(num)
            prDblD = Math.Abs(den)
            actualiza()
        Catch ex As Exception
            Dim s1 As String = ex.ToString
            Dim s2 As String = s1
            Throw New Exception(ex.Message)
        End Try
    End Sub
    Public Sub New(ByVal e1 As String)
        Try
            If e1 = "0" Then
                sgn1 = 0
                Exit Sub
            End If
            If InStr(LCase(e1), "infinity") Then
                Throw New IndexOutOfRangeException
            End If
            e1 = Replace(e1, ",", "")
            Dim prDblN As Decimal = 0D
            Dim prDblD As Decimal = 1D
            If Len(e1) > 2 AndAlso e1.Chars(0) = "(" Then e1 = Mid(e1, 2, Len(e1) - 2)
            If Len(e1) AndAlso e1.Chars(0) = "." Then e1 = "0" + e1
            If Len(e1) > 2 AndAlso Mid(e1, 1, 2) = "-." Then e1 = "-0." + Mid(e1, 3)
            e1 = LCase(e1)
            If InStr(e1, "e") = 0 Then e1 += "e0"
            Dim e2() As String = Split(e1, "e")

            Dim i As Int32 = 0
            Dim e3 As String = e2(0)
            Dim s1 As Int32 = 1
            If Mid(e3, 1, 1) = "-" Then
                s1 = -1 : e3 = Mid(e3, 2)
            End If
            Dim posComa As Int32 = InStr(e3, ".")
            Do While i < e3.Length AndAlso ((e3.Chars(i) >= "0" AndAlso e3.Chars(i) <= "9") OrElse e3.Chars(i) = ".")
                If e3.Chars(i) <> "." Then
                    prDblN = prDblN * 10D + Decimal.Parse(e3.Chars(i))
                End If
                i += 1
            Loop
            Dim exp10 As Int32 = Int32.Parse(e2(1))
            If posComa > 0 Then
                exp10 -= Len(e3) - posComa
            End If
            'If s1 = -1 Then prDbl *= -1D
            If prDblN = 0 Then
                sgn1 = 0
            Else
                If exp10 Then
                    ' a*10^x = a*2^y
                    ' => x*log2(10)= y*log2(2) = y
                    ' => y = x*log2(10) = x*log10(10)/log10(2)= x/log10(2)
                    ' a*2^y = a*2^(int(y)+frac(y) = a*2^frac(y) * 2^int(y)
                    Dim y As Double = exp10 / Math.Log10(2)
                    Dim yAbs As Int32 = Math.Abs(y)
                    exp2 = Math.Floor(yAbs)
                    If y > 0 Then
                        Me.prDblN *= 2 ^ (y - exp2)
                    Else
                        Me.prDblD *= 2.0
                        Me.prDblD *= 2 ^ (y - exp2)
                    End If
                    exp2 = 0
                End If
                sgn1 = s1
                actualiza()
            End If
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
    End Sub
    Public Property ToDecimal() As Double
        Get
            Return ToDouble
        End Get
        Set(ByVal value As Double)
            Dim prA As New Precis84(value)
            Me.CopyFrom(prA)
        End Set
    End Property
    Public ReadOnly Property ToDouble() As Double
        Get
            Return sgn * (prDblN / prDblD) * 2.0 ^ exp2
        End Get
    End Property
    ReadOnly Property sgn() As Int32
        Get
            Return sgn1
        End Get
    End Property
    Public Function IsZero() As Boolean
        Return (sgn1 = 0)
    End Function
    Public ReadOnly Property num() As Double
        Get
            Return prDblN
        End Get
    End Property
    Public ReadOnly Property den() As Double
        Get
            Return prDblD
        End Get
    End Property
    Public Function opCompara(ByVal opB As Precis84) As Int32
        Dim dif As Int32 = Math.Sign(sgn - opB.sgn)
        If dif Then Return dif
        'Dim opC As New Precis84(Me)
        'Dim opD As New Precis84(opB)
        'dif = Math.Sign(opC.sgn - opD.sgn)
        'If dif Then Return dif
        'dif = Math.Sign(opC.prDblN * opD.prDblD - opD.prDblN * opC.prDblD)
        'Return dif
        Return Math.Sign(prDblN * opB.prDblD * 2.0 ^ exp2 - prDblD * opB.prDblD * 2.0 ^ opB.exp2)
    End Function
    Public Function opCompara(ByVal opB As Precis84, ByVal redondeo As Boolean) As Int32
        Dim dif As Int32 = Math.Sign(sgn - opB.sgn)
        If Not redondeo AndAlso dif Then Return dif
        If redondeo AndAlso (Math.Abs(ToDouble) <= 0.001 OrElse Math.Abs(opB.ToDouble) <= 0.001) Then
            Return Math.Sign(ToDouble() - opB.ToDouble)
        End If
        Return Math.Sign(Math.Round(ToDouble(), 3) - Math.Round(opB.ToDouble, 3))
    End Function
    Public Sub opSuma(ByVal opB As Precis84)
        Try
            If sgn1 = 0 OrElse opB.sgn1 = 0 Then
                If sgn1 = 0 Then
                    CopyFrom(opB)
                End If
            Else
                Dim dblN As Double = sgn1 * prDblN * opB.prDblD +
                     opB.sgn1 * opB.prDblN * prDblD * 2.0 ^ (opB.exp2 - exp2)
                If Double.IsInfinity(dblN) Then
                    If Math.Abs(Me.ToDouble) >
                        Math.Abs(opB.ToDouble) Then
                        If Math.Abs(Me.ToDouble * 1.0E-50) >
                            Math.Abs(opB.ToDouble) Then
                            Exit Try
                        End If
                    Else
                        If Math.Abs(Me.ToDouble * 1.0E+50) <
                            Math.Abs(opB.ToDouble) Then
                            CopyFrom(opB)
                            Exit Try
                        End If
                    End If
                End If
                sgn1 = Math.Sign(dblN)
                If sgn1 = 0 Then
                    dblN = 0.0
                    Exit Try
                End If
                prDblN = Math.Abs(dblN)
                prDblD *= opB.prDblD
                actualiza()
            End If
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Shared Function opSuma(ByVal opA As Precis84, ByVal opB As Precis84) As Precis84
        Dim opC As New Precis84(opA)
        opC.opSuma(opB)
        Return opC
    End Function
    Public Shared Function opResta(ByVal opA As Precis84, ByVal opB As Precis84) As Precis84
        Dim opC As New Precis84(opA)
        opC.opSuma(-opB)
        Return opC
    End Function
    Public Sub opMult(ByVal opB As Precis84)
        Try
            sgn1 *= opB.sgn1
            If sgn1 Then
                prDblN *= opB.prDblN
                prDblD *= opB.prDblD
                exp2 += opB.exp2
                actualiza()
            End If
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Shared Function opMult(ByVal opA As Precis84, ByVal opB As Precis84) As Precis84
        Dim opC As New Precis84(opA)
        opC.opMult(opB)
        Return opC
    End Function
    Public Sub opMult(ByVal db As Double)
        Try
            Dim opB As New Precis84(db)
            Me.opMult(opB)
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
    End Sub
    Public Shared Function opMult(ByVal opA As Precis84, ByVal db As Double) As Precis84
        Dim opC As New Precis84(opA)
        opC.opMult(db)
        Return opC
    End Function
    Public Sub opDiv(ByVal opB As Precis84)
        Try
            If opB.sgn = 0.0 Then
                Throw New DivideByZeroException
            End If
            Dim D As Double = prDblD * opB.prDblN
            If sgn1 Then
                sgn1 *= opB.sgn1
                prDblN = prDblN * opB.prDblD
                prDblD = D
                exp2 -= opB.exp2
                actualiza()
            End If
        Catch ex As Exception
            Throw ex
        End Try
    End Sub

    Public Shared Function opDiv(ByVal opA As Precis84, ByVal opB As Precis84) As Precis84
        Dim opC As New Precis84(opA)
        opC.opDiv(opB)
        Return opC
    End Function
    Public Sub opDiv(ByVal nID As Int32, ByVal db As Double)
        Try
            Dim opB As New Precis84(db)
            Me.opDiv(opB)
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
    End Sub
    Public Sub opChgSgn()
        sgn1 *= -1
    End Sub
    Public Shared Function opMod(ByVal opA As Precis84, ByVal opB As Precis84) As Double
        Return opA.ToDouble Mod opB.ToDouble
    End Function
    Public Shared Function opPow(ByVal opA As Precis84, ByVal expB As Double) As Precis84
        Dim opC As Precis84 = Nothing
        Try
            If expB = 0.0 Then
                opC = New Precis84(1.0)
                Exit Try
            ElseIf expB = 1.0 Then
                opC = New Precis84(opA)
                Exit Try
            ElseIf expB = -1.0 Then
                opC = New Precis84(opA)
                opC.exp2 *= -1
                Dim auxDen As Double = opC.prDblD
                opC.prDblD = opC.prDblN
                opC.prDblN = auxDen
                opC.actualiza()
                Exit Try
            End If
            opC = New Precis84(0.0)
            Dim bDone As Boolean = False
            If expB = Math.Floor(expB) AndAlso expB <= 64 Then
                Dim exp As Int32 = Math.Abs(expB)
                Try
                    If exp > 0 Then
                        If opA.IsZero Then
                            opC = New Precis84(0.0)
                            bDone = True
                            Exit Try
                        End If
                        Dim log2 As Int32 = Math.Floor(Math.Log10(exp) / Math.Log10(2.0))
                        Dim vNum(log2), vDen(log2) As Double
                        Dim vExp(log2) As Int32
                        Dim curExp As Int32 = 2
                        vNum(0) = opA.prDblN
                        vDen(0) = opA.prDblD
                        vExp(0) = 1
                        For i As Int32 = 1 To log2
                            vNum(i) = vNum(i - 1) * vNum(i - 1)
                            vDen(i) = vDen(i - 1) * vDen(i - 1)
                            vExp(i) = curExp
                            curExp *= 2
                        Next
                        curExp = vExp(log2)
                        opC.prDblN = vNum(log2)
                        opC.prDblD = vDen(log2)
                        For i As Int32 = log2 - 1 To 0 Step -1
                            If curExp + vExp(i) <= exp Then
                                opC.prDblN *= vNum(i)
                                opC.prDblD *= vDen(i)
                                curExp += vExp(i)
                                If curExp = exp Then
                                    Exit For
                                End If
                            End If
                        Next
                        opC.exp2 = opA.exp2 * exp
                        If expB < 0 Then
                            'opC = New Precis84(1.0) / opC
                            Dim auxDbl As Double = opC.prDblD
                            opC.prDblD = opC.prDblN
                            opC.prDblN = auxDbl
                            opC.exp2 *= -1
                        End If
                        If opA.sgn = -1 AndAlso
                        (expB Mod 2) Then
                            opC.sgn1 = -1
                        Else
                            opC.sgn1 = 1
                        End If
                    ElseIf Not opA.IsZero Then
                        opC = New Precis84(1.0)
                    Else
                        opC = New Precis84(0.0)
                    End If
                    Try
                        opC.actualiza()
                        bDone = True
                    Catch ex As Exception

                    End Try
                Catch ex As Exception

                End Try
                'Try
                '    opC.sgn1 = opA.sgn1
                '    Dim N As Double = opA.prDblN
                '    Dim D As Double = opA.prDblD
                '    If expB < 0 Then
                '        D = opA.prDblN
                '        N = opA.prDblD
                '        expB = -expB
                '    End If
                '    opC.prDblN = N
                '    opC.prDblD = D
                '    If opA.sgn = -1 AndAlso _
                '    (expB Mod 2) = 0 Then
                '        opC.sgn1 = 1
                '    End If
                '    opC.exp = opA.exp * expB
                '    For i = 2 To expB
                '        opC.prDblN *= N
                '        opC.prDblD *= D
                '    Next
                '    bDone = True
                'Catch ex2 As Exception

                'End Try
            End If
            If Not bDone Then
                opC = New Precis84(opA)
                opC.exp2 *= expB
                Dim l10 As Int32 = Math.Floor(Math.Log(opC.prDblN) / Math.Log(2.0))
                If l10 <> 0 Then
                    opC.prDblN *= 2 ^ l10
                    opC.exp2 += l10
                End If
                opC.prDblN = Math.Pow(opC.prDblN, expB)
                opC.sgn1 = Math.Pow(opA.sgn, expB)
                opC.prDblN = Math.Abs(opC.prDblN)
                l10 = Math.Floor(Math.Log10(opC.prDblD))
                If l10 <> 0 Then
                    opC.prDblD *= 2 ^ l10
                    opC.exp2 -= l10
                End If
                opC.prDblD = Math.Pow(opC.prDblD, expB)
            End If
            opC.actualiza()
        Catch ex As Exception
            Throw ex
        End Try
        Return opC
    End Function
    Public Shared Function gcd(ByVal u1 As Double, ByVal v1 As Double) As Double

        ' http://en.wikipedia.org/wiki/Binary_GCD_algorithm
        'int shift;
        '/* Let shift := lg K, where K is the greatest power of 2
        '      dividing both u and v. */
        'for (shift = 0; ((u | v) & 1) == 0; ++shift) {
        '       u >>= 1;
        '       v >>= 1;
        '}
        Dim shift As Int32 = 0
        Dim u As Long = CLng(u1)
        Dim v As Long = CLng(v1)
        If u = 0 OrElse v = 0 Then ' added this check point to avoid a no ending loop
            Return 0.0
        End If
        Do
            If ((u Or v) And 1) <> 0 Then
                Exit Do
            End If
            shift += 1
            u >>= 1
            v >>= 1
        Loop
        'while ((u & 1) == 0)
        '  u >>= 1;
        While (u And 1) = 0
            u >>= 1
        End While

        '/* From here on, u is always odd. */
        'do {
        '     /* remove all factors of 2 in v -- they are not common */
        '     /*   note: v is not zero, so while will terminate */
        '     while ((v & 1) == 0)  /* Loop X */
        '         v >>= 1;

        '     /* Now u and v are both odd. Swap if necessary so u <= v,
        '        then set v = v - u (which is even). For bignums, the
        '        swapping is just pointer movement, and the subtraction
        '        can be done in-place. */
        '     if (u > v) {
        '       unsigned int t = v; v = u; u = t;}  // Swap u and v.
        '     v = v - u;                       // Here v >= u.
        '   } while (v != 0);

        Do
            While (v And 1) = 0
                v >>= 1
            End While
            If u > v Then
                Dim aux As Double = v : v = u : u = aux
            End If
            v -= u
        Loop While v
        '/* restore common factors of 2 */
        'return u << shift;

        Return u << shift
    End Function

    Private Sub actualiza()
        '    Dim value As Double = Me.ToDouble
        Const maxExp As Int32 = 50
        Const max As Double = 2.0 ^ maxExp
        Const min As Double = 2.0 ^ -maxExp
        Try
            If sgn1 = 0 OrElse prDblN = 0.0 Then
                sgn1 = 0
                Exit Sub
            End If
            If Double.IsInfinity(prDblN) OrElse
            Double.IsInfinity(prDblD) OrElse
            Double.IsNaN(prDblN) OrElse
            Double.IsNaN(prDblD) Then
                Throw New ArgumentOutOfRangeException
                Exit Try
            End If
            If (prDblN = 1.0 AndAlso prDblD = 1.0) OrElse
            Double.IsInfinity(prDblN) OrElse
            Double.IsInfinity(prDblD) OrElse
            Double.IsNaN(prDblN) OrElse
            Double.IsNaN(prDblD) Then
                Exit Try
            ElseIf prDblN = prDblD Then
                prDblN = 1.0
                prDblD = 1.0
                Exit Try
            End If
            Do While prDblN > max
                prDblN /= max
                exp2 += maxExp
            Loop
            Do While prDblN < min
                prDblN *= max
                exp2 -= maxExp
            Loop
            Do While prDblD > max
                prDblD /= max
                exp2 -= maxExp
            Loop
            Do While prDblD < min
                prDblD *= max
                exp2 += maxExp
            Loop
            If prDblN >= 10 ^ 8 OrElse
            prDblD >= 10 ^ 8 Then
                Dim db As Double = ToDouble
                If db > 1.0 AndAlso db < 10 ^ 5 Then
                    Dim i As Int32
                    Dim dbRnd As Double
                    For i = 0 To vPrime.Length - 1
                        dbRnd = Math.Round(db * vPrime(i), 8)
                        If dbRnd = Math.Floor(dbRnd) Then
                            Exit For
                        End If
                    Next
                    If i < vPrime.Length Then
                        prDblN = dbRnd
                        prDblD = vPrime(i)
                        exp2 = 0 ' 2010/10/25 added
                    End If
                End If
                Exit Try
            End If
            If Math.Floor(prDblN) <> prDblN OrElse
             Math.Floor(prDblD) <> prDblD OrElse
             Math.Floor(prDblN) <> prDblN OrElse
            prDblN = 1.0 OrElse
            Math.Floor(prDblD) <> prDblD OrElse
            prDblD = 1.0 Then
                Exit Try
            End If
            If prDblN > prDblD Then
                Dim g As Double = gcd(prDblN, prDblD)
                If g > 1 Then
                    prDblN /= g
                    prDblD /= g
                End If
            Else
                Dim g As Double = gcd(prDblD, prDblN)
                If g > 1 Then
                    prDblN /= g
                    prDblD /= g
                End If
            End If

        Catch ex As Exception
            Throw ex
        End Try
        'If Me.ToDouble <> value Then
        '    value = Me.ToDouble
        'End If
    End Sub
    Public Shared Operator +(ByVal prA As Precis84, ByVal prB As Precis84) As Precis84
        Return Precis84.opSuma(prA, prB)
    End Operator
    Public Shared Operator +(ByVal dbA As Double, ByVal prB As Precis84) As Precis84
        Return Precis84.opSuma(New Precis84(dbA), prB)
    End Operator
    Public Shared Operator +(ByVal prA As Precis84, ByVal dbB As Double) As Precis84
        Return Precis84.opSuma(prA, New Precis84(dbB))
    End Operator
    Public Shared Operator -(ByVal prA As Precis84, ByVal prB As Precis84) As Precis84
        Return Precis84.opResta(prA, prB)
    End Operator
    Public Shared Operator -(ByVal dbA As Double, ByVal prB As Precis84) As Precis84
        Return Precis84.opResta(New Precis84(dbA), prB)
    End Operator
    Public Shared Operator -(ByVal prA As Precis84, ByVal dbB As Double) As Precis84
        Return Precis84.opResta(prA, New Precis84(dbB))
    End Operator
    Public Shared Operator -(ByVal prA As Precis84) As Precis84
        Dim prB As New Precis84(prA)
        prB.opChgSgn()
        Return prB
    End Operator
    Public Shared Operator *(ByVal prA As Precis84, ByVal prB As Precis84) As Precis84
        Return Precis84.opMult(prA, prB)
    End Operator
    Public Shared Operator *(ByVal dbA As Double, ByVal prB As Precis84) As Precis84
        Return Precis84.opMult(New Precis84(dbA), prB)
    End Operator
    Public Shared Operator *(ByVal prA As Precis84, ByVal dbB As Double) As Precis84
        Return Precis84.opMult(prA, New Precis84(dbB))
    End Operator
    Public Shared Operator /(ByVal prA As Precis84, ByVal prB As Precis84) As Precis84
        Return Precis84.opDiv(prA, prB)
    End Operator
    Public Shared Operator /(ByVal dbA As Double, ByVal prB As Precis84) As Precis84
        Return Precis84.opDiv(New Precis84(dbA), prB)
    End Operator
    Public Shared Operator /(ByVal prA As Precis84, ByVal dbB As Double) As Precis84
        Return Precis84.opDiv(prA, New Precis84(dbB))
    End Operator
    Public Shared Operator ^(ByVal prA As Precis84, ByVal prB As Precis84) As Precis84
        Return Precis84.opPow(prA, prB.ToDouble)
    End Operator
    Public Shared Operator ^(ByVal prA As Precis84, ByVal dbB As Double) As Precis84
        Return Precis84.opPow(prA, dbB)
    End Operator
    Public Shared Function toStringBase(ByVal cfg As Config84,
                                        ByVal db As Double) As String
        Dim sDb As String
        Dim sgn As Int32
        Try
            sgn = Math.Sign(db)
            db = Math.Abs(db)
            sDb = db.ToString("e15", Config84.us)
            Dim vDb() As String = Split(sDb, "e")
            Dim fra As Double = db - Math.Floor(db)
            Dim sFra As String = ""
            Dim db1 As Double = Math.Floor(db)
            sDb = db1.ToString
            Dim j As Int32 = 0
            Dim e1 As String = ""
            Dim i As Int32
            If cfg.base =
            numBase.decimal Then
                sDb = db.ToString(Config84.us)
            ElseIf cfg.base =
            numBase.hexadecimal Then
                sDb = UCase(Hex(db1))
                If fra Then
                    Dim nDec As Int32 = cfg.nDecimals
                    If cfg.bRounding Then
                        Dim ent1 As Double = Math.Floor(fra * 2 ^ 32)

                        ' Round to 'nDec' hexadecimal digits:
                        ent1 = (ent1 + 8 * 16 ^ (nDec + 1))

                        sFra = Convert.ToString(CLng(ent1), 16)
                    Else
                        sFra = Convert.ToString(CLng(fra * 2 ^ 32), 16)
                    End If
                    If sFra.Length < 8 Then
                        sFra = StrDup(8 - sFra.Length, "0") + sFra
                    End If
                    If cfg.bRounding AndAlso Len(sFra) > nDec Then
                        ' Truncate 'sFra' to 'nDec' characters:
                        sFra = Mid(sFra, 1, nDec)
                    End If
                    sFra = UCase(sFra)
                End If
            ElseIf cfg.base =
            numBase.octal Then
                sDb = Convert.ToString(CLng(db1), 8)
                If fra Then
                    Dim nDec As Int32 = cfg.nDecimals
                    If cfg.bRounding Then
                        Dim ent1 As Double = Math.Floor(fra * 2 ^ 36)

                        ' Round to 'nDec' octal digits
                        Dim ln As Long = 2L ^ 36
                        ln >>= 3 * nDec + 1
                        ent1 += ln

                        sFra = Convert.ToString(CLng(ent1), 8)
                    Else
                        sFra = Convert.ToString(CLng(fra * 2 ^ 36), 8)
                    End If
                    If sFra.Length < 12 Then
                        sFra = StrDup(12 - sFra.Length, "0") + sFra
                    End If
                    If cfg.bRounding AndAlso Len(sFra) > nDec Then
                        ' Truncate 'sFra' to 'nDec' characters:
                        sFra = Mid(sFra, 1, nDec)
                    End If



                    'sFra = Convert.ToString(CLng(fra * 2 ^ 36), 8)
                    'If sFra.Length < 12 Then
                    '    sFra = StrDup(12 - sFra.Length, "0") + sFra
                    'End If
                End If
            Else
                sDb = Convert.ToString(CLng(db1), 2)
                If fra Then
                    Dim nDec As Int32 = cfg.nDecimals
                    If cfg.bRounding Then
                        Dim ent1 As Double = Math.Floor(fra * 2 ^ 32)

                        ' Round to 'nDec' octal digits
                        Dim ln As Long = 2L ^ 32
                        ln >>= nDec + 1
                        ent1 += ln

                        sFra = Convert.ToString(CLng(ent1), 2)
                    Else
                        sFra = Convert.ToString(CLng(fra * 2 ^ 32), 2)
                    End If
                    If sFra.Length < 32 Then
                        sFra = StrDup(32 - sFra.Length, "0") + sFra
                    End If
                    If cfg.bRounding AndAlso Len(sFra) > nDec Then
                        ' Truncate 'sFra' to 'nDec' characters:
                        sFra = Mid(sFra, 1, nDec)
                    End If



                    ''sFra = Convert.ToString(CLng(fra * 2 ^ 32), 2)
                    ''If sFra.Length < 32 Then
                    ''    sFra = StrDup(32 - sFra.Length, "0") + sFra
                    ''End If
                End If
            End If
            If fra Then
                i = Len(sFra) - 1
                Do While i AndAlso sFra.Chars(i) = "0"
                    'sFraRe = Mid(sFraRe, 1, i)
                    i -= 1
                Loop
                sFra = Mid(sFra, 1, i + 1)
                j = 0
                For i = 0 To Len(sFra) - 1
                    e1 += sFra.Chars(i)
                    j += 1
                    If j Mod 4 = 0 Then
                        e1 += " "
                    End If
                Next
            End If
            Dim e2 As String = ""
            j = 0
            For i = Len(sDb) - 1 To 0 Step -1
                e2 = sDb.Chars(i) + e2
                j += 1
                If j Mod 4 = 0 Then
                    e2 = " " + e2
                End If
            Next
            sDb = e2
            If Len(e1) Then
                sDb += "." + e1
            End If
            'sDb = UCase(sDb)

        Catch ex As Exception
            Throw ex
        End Try
        Try
            If cfg.base <>
            numBase.decimal Then
                If cfg.base =
                numBase.hexadecimal Then
                    sDb = "&h" + sDb
                ElseIf cfg.base =
                numBase.octal Then
                    sDb = "&o" + sDb
                ElseIf cfg.base =
                numBase.binary Then
                    sDb = "&b" + sDb
                End If
            End If
            If sgn = -1 Then
                sDb = "-" + sDb
            End If
        Catch ex As Exception

        End Try
        Return sDb
    End Function
    Public Overrides Function ToString() As String

        'Dim db As Double = Me.ToDouble
        'Dim sRet As String = ""
        'Try
        '    If cfg.base = _
        '    numBase.decimal Then
        '        sRet = db.ToString(Config84.us)
        '    Else
        '        sRet = Precis84.baseToString(db)
        '    End If
        'Catch ex As Exception
        '    Return db.ToString(Config84.us)
        'End Try
        'Return sRet
        'Return toStrRational(New Config84)
        Return ToDouble.ToString(Config84.us)
    End Function
    Public ReadOnly Property toStringRational As String
        Get
            Return toStrRational(New Config84)
        End Get
    End Property
    Public Function toStrRational(ByVal cfg As Config84) As String
        ' output string will be ="" if its not
        ' possible to return an integer or fraction
        Dim e1 As String = ""
        Try
            'If exp <> 0 Then
            '    Return ""
            'End If
            actualiza()
            Dim num, den As Double
            If cfg.base = numBase.decimal Then
                Do While prDblD < 1.0
                    prDblD *= 2.0
                    prDblN *= 2.0
                Loop
                'Do While exp2 > 0 AndAlso prDblN < 2.0 ^ expMax
                '    prDblN *= 2.0
                '    exp2 -= 1
                'Loop
                'Do While exp2 < 0 AndAlso prDblD < 2.0 ^ expMax
                '    prDblD *= 2.0
                '    exp2 += 1
                'Loop
                If cfg.bRounding Then
                    num = Math.Round(prDblN, cfg.nDecimals)
                    den = Math.Round(prDblD, cfg.nDecimals)
                Else
                    num = prDblN
                    den = prDblD
                End If
                If exp2 > 0 Then
                    num *= 2 ^ exp2
                ElseIf exp2 < 0 Then
                    den *= 2 ^ -exp2
                End If
                If num > 10 ^ 4 OrElse
                den > 10 ^ 4 OrElse
                num / den > 10 ^ 4 OrElse
                num < 10 ^ -3 OrElse
                den < 10 ^ -3 Then
                    Return ""
                End If
                If num = Math.Floor(num) AndAlso
                den = Math.Floor(den) AndAlso
                num > 1.0 AndAlso
                den > 1.0 Then
                    Dim d As Double = gcd(num, den)
                    If d > 1.0 Then
                        num /= d
                        den /= d
                    End If
                End If
            Else
                Do While prDblD < 1.0
                    prDblD *= 2.0
                    prDblN *= 2.0
                Loop
                Do While exp2 > 0
                    prDblN *= 2.0
                    exp2 -= 1
                Loop
                Do While exp2 < 0
                    prDblD *= 2.0
                    exp2 += 1
                Loop
                If num > 10 ^ 4 OrElse
                den > 10 ^ 4 OrElse
                num / den > 10 ^ 4 OrElse
                num < 10 ^ -3 OrElse
                den < 10 ^ -3 Then
                    Return ""
                End If
                num = prDblN
                den = prDblD
            End If
            Do While exp2 < 0
                prDblD *= 2.0
                exp2 += 1
            Loop
            Dim sNum As String = num.ToString(Config84.us)
            Dim sDen As String = den.ToString(Config84.us) ' (New Precis84(Me.prDblD)).ToString
            If Regex.IsMatch(sNum + sDen, "\.|E|e") Then
                Return ""
            End If
            If cfg.base <>
            numBase.decimal Then
                sNum = Precis84.toStringBase(cfg, prDblN)
                sDen = Precis84.toStringBase(cfg, prDblD)
            End If

            If sNum = "0" Then
                e1 = "0"
            ElseIf sDen = sNum Then
                e1 = "1"
            ElseIf sDen = "1" OrElse _
            sDen = "&h1" OrElse _
            sDen = "&o1" OrElse _
            sDen = "&b1" Then
                e1 = sNum
            Else
                e1 = sNum + "/" + sDen
                'e1 += vbCrLf + "= " + ToString()
            End If
            If sgn1 = -1 Then
                e1 = "-" + e1
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return e1
    End Function

End Class
