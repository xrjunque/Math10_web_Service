Imports System.Text.RegularExpressions

<Serializable()>
Public Class Rational
    Public Sub New()
        MyBase.New()
    End Sub
    Private prDblN As Double
    Private prDblD As Double = 1.0
    Public Sub New(ByVal opB As Rational)
        Me.CopyFrom(opB)
    End Sub
    Public Sub CopyFrom(ByVal opB As Rational)
        opB.actualiza()
        prDblN = opB.prDblN
        prDblD = opB.prDblD
    End Sub
    Public Function getDenominator() As Double
        Return Me.prDblD
    End Function

    Public Sub New(ByVal db As Double)
        prDblN = db
    End Sub
    Public Sub New(ByVal num As Double, ByVal den As Double)
        Try
            prDblN = num
            prDblD = den
            actualiza()
        Catch ex As Exception
            Dim s1 As String = ex.ToString
            Dim s2 As String = s1
            Throw New Exception(ex.Message)
        End Try
    End Sub
    Public Shared Function ParseNumber(ByVal e1 As String) As Double
        Dim num As Double = 0D
        Try
            If Not MathGlobal8.TryParseDbl(e1, num) Then
                If InStr(LCase(e1), "infinity") Then
                    Throw New Exception(msg8.msg(32))
                End If
                e1 = Replace(e1, ",", "")
                If Len(e1) > 2 AndAlso e1.Chars(0) = "(" Then e1 = Mid(e1, 2, Len(e1) - 2)
                If Len(e1) AndAlso e1.Chars(0) = "." Then e1 = "0" + e1
                If Len(e1) > 2 AndAlso Mid(e1, 1, 2) = "-." Then e1 = "-0." + Mid(e1, 3)
                e1 = LCase(e1)
                If InStr(e1, "e") = 0 Then e1 += "e0"
                Dim e2() As String = Split(e1, "e")

                Dim i As Int64 = 0
                Dim e3 As String = e2(0)
                Dim s1 As Int64 = 1
                If Mid(e3, 1, 1) = "-" Then
                    s1 = -1 : e3 = Mid(e3, 2)
                End If
                Dim posComa As Int64 = InStr(e3, ".")
                Do While i < e3.Length AndAlso ((e3.Chars(i) >= "0" AndAlso e3.Chars(i) <= "9") OrElse e3.Chars(i) = ".")
                    If e3.Chars(i) <> "." Then
                        num = num * 10D + Decimal.Parse(e3.Chars(i))
                    End If
                    i += 1
                Loop
                Dim exp10 As Int64 = Int64.Parse(e2(1))
                If posComa > 0 Then
                    exp10 -= Len(e3) - posComa
                End If
                num *= 10 ^ exp10
            End If
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return num
    End Function
    Public Shared Function ParseRational(sNum As String) As Rational
        Dim r As New Rational()
        Try
            Dim ve1() As String = Split(sNum, "/")
            r.prDblN = ParseNumber(ve1(0))
            If ve1.Length > 1 Then
                r.prDblD = ParseNumber(ve1(1))
            End If
        Catch ex As Exception
        End Try
        Return r
    End Function
    Public Property ToDecimal() As Double
        Get
            Return ToDouble
        End Get
        Set(ByVal value As Double)
            Dim prA As New Rational(value)
            Me.CopyFrom(prA)
        End Set
    End Property
    Public ReadOnly Property ToDouble() As Double
        Get
            Return prDblN / prDblD
        End Get
    End Property
    ReadOnly Property sgn() As Int64
        Get
            Return Math.Sign(prDblD) * Math.Sign(prDblN)
        End Get
    End Property
    Public Function IsZero() As Boolean
        Return (ToDouble = 0.0)
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
    Public Function opCompara(ByVal opB As Rational) As Int64
        Dim dif As Int64 = Math.Sign(sgn - opB.sgn)
        If dif Then Return dif
        Return Math.Sign(prDblN * opB.prDblD - prDblD * opB.prDblD)
    End Function
    Public Function opCompara(ByVal opB As Rational, ByVal redondeo As Boolean) As Int64
        Dim dif As Int64 = Math.Sign(sgn - opB.sgn)
        If Not redondeo AndAlso dif Then Return dif
        If redondeo AndAlso (Math.Abs(ToDouble) <= 0.001 OrElse Math.Abs(opB.ToDouble) <= 0.001) Then
            Return Math.Sign(ToDouble() - opB.ToDouble)
        End If
        Return Math.Sign(Math.Round(ToDouble(), 3) - Math.Round(opB.ToDouble, 3))
    End Function
    Public Sub opSuma(ByVal opB As Rational)
        Try
            Dim dbl As Double = ToDouble + opB.ToDouble
            Dim opC As New Rational(prDblN, prDblD)
            prDblN = opC.prDblN * opB.prDblD + opC.prDblD * opB.prDblN
            prDblD = (prDblD * opB.prDblD)
            If Double.IsInfinity(prDblD) OrElse Double.IsInfinity(prDblN) _
            OrElse Double.IsNaN(prDblD) OrElse Double.IsNaN(prDblN) Then
                CopyFrom(New Rational(dbl))
            Else
                actualiza()
            End If
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Shared Function opSuma(ByVal opA As Rational, ByVal opB As Rational) As Rational
        Dim opC As New Rational(opA)
        opC.opSuma(opB)
        Return opC
    End Function
    Public Shared Function opResta(ByVal opA As Rational, ByVal opB As Rational) As Rational
        Dim opC As New Rational(opA)
        opC.opSuma(-opB)
        Return opC
    End Function
    Public Sub opMult(ByVal opB As Rational)
        Try
            Dim dbl As Double = ToDouble * opB.ToDouble
            prDblN *= opB.prDblN
            prDblD *= opB.prDblD
            If Double.IsInfinity(prDblD) OrElse Double.IsInfinity(prDblN) _
            OrElse Double.IsNaN(prDblD) OrElse Double.IsNaN(prDblN) Then
                CopyFrom(New Rational(dbl))
            Else
                actualiza()
            End If
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Shared Function opMult(ByVal opA As Rational, ByVal opB As Rational) As Rational
        Dim opC As New Rational(opA)
        opC.opMult(opB)
        Return opC
    End Function
    Public Sub opMult(ByVal db As Double)
        Try
            Dim opB As New Rational(db)
            Me.opMult(opB)
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
    End Sub
    Public Shared Function opMult(ByVal opA As Rational, ByVal db As Double) As Rational
        Dim opC As New Rational(opA)
        opC.opMult(db)
        Return opC
    End Function
    Public Sub opDiv(ByVal opB As Rational)
        Try
            Dim dbl As Double = ToDouble / opB.ToDouble
            prDblN *= opB.prDblD
            prDblD *= opB.prDblN
            If Double.IsInfinity(prDblD) OrElse Double.IsInfinity(prDblN) _
            OrElse Double.IsNaN(prDblD) OrElse Double.IsNaN(prDblN) Then
                CopyFrom(New Rational(dbl))
            Else
                actualiza()
            End If
        Catch ex As Exception
            Throw ex
        End Try
    End Sub

    Public Shared Function opDiv(ByVal opA As Rational, ByVal opB As Rational) As Rational
        Dim opC As New Rational(opA)
        opC.opDiv(opB)
        Return opC
    End Function
    Public Sub opDiv(ByVal nID As Int64, ByVal db As Double)
        Try
            Dim opB As New Rational(db)
            Me.opDiv(opB)
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
    End Sub
    Public Sub opChgSgn()
        prDblN *= -1.0
    End Sub
    Public Function opFactors() As Double()
        Dim ret(-1) As Double
        Try
            Dim db As Double = Math.Abs(ToDouble)
            If Math.Floor(db) <> db OrElse db >= 1000000.0 OrElse
                            db < 2 Then
                If db = 1.0 Then
                    ret = New Double() {1.0}
                End If
            Else
                Dim iv As Int64 = 0
                For i As Int64 = 0 To MathGlobal8.vPrime.Length - 1
                    Dim div As Double = db / MathGlobal8.vPrime(i)
                    If div = Math.Floor(div) Then
                        db = div
                        ReDim Preserve ret(iv)
                        ret(iv) = MathGlobal8.vPrime(i)
                        iv += 1
                        i -= 1
                        If db = 1 Then Exit For
                    End If
                Next
                If db <> 1 Then
                    ReDim Preserve ret(iv)
                    ret(iv) = db
                End If
            End If
        Catch ex As Exception
            ReDim ret(-1)
        End Try
        Return ret
    End Function
    Public Shared Function opMod(ByVal opA As Rational, ByVal opB As Rational) As Double
        Return opA.ToDouble Mod opB.ToDouble
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
        Dim shift As Int64 = 0
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
        Dim sN As Int64
        Try
            If Double.IsInfinity(prDblN) OrElse
            Double.IsNaN(prDblN) Then
                Throw New ArgumentOutOfRangeException
            End If
            If Double.IsInfinity(prDblD) OrElse
            Double.IsNaN(prDblD) Then
                Throw New ArgumentOutOfRangeException
            End If
            If Math.Sign(prDblD) = -1 Then
                prDblN *= -1
                prDblD *= -1
            End If
            sN = Math.Sign(prDblN)
            prDblN = Math.Abs(prDblN)
            If prDblN = prDblD Then
                prDblD = 1.0
                prDblN = 1.0
                Exit Try
            End If
            Dim N1 As Double = Math.Log10(prDblN)
            Dim D1 As Double = Math.Log10(prDblD)
            If (N1 < -50 AndAlso D1 < -50) OrElse
            (N1 > 50 AndAlso D1 > 50) Then
                prDblN /= prDblD
                prDblD = 1.0
                Exit Try
            End If
            Dim N As Double = Math.Abs(N1)
            Dim D As Double = Math.Abs(D1)
            Dim ndbl As Double = Math.Abs(Math.Log10(ToDouble))
            If ndbl < N AndAlso ndbl < D Then
                prDblN = ToDouble * sN
                prDblD = 1.0
                Exit Sub
            End If
            If prDblD <> Math.Floor(prDblD) OrElse
            prDblD = 1.0 OrElse prDblD > 10 ^ 6 Then
                Exit Try
            End If
            If prDblN <> Math.Floor(prDblN) OrElse
            prDblN = 1.0 OrElse prDblN > 10 ^ 6 Then
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
        prDblN *= sN
    End Sub
    Public Shared Operator +(ByVal prA As Rational, ByVal prB As Rational) As Rational
        Return Rational.opSuma(prA, prB)
    End Operator
    Public Shared Operator +(ByVal dbA As Double, ByVal prB As Rational) As Rational
        Return Rational.opSuma(New Rational(dbA), prB)
    End Operator
    Public Shared Operator +(ByVal prA As Rational, ByVal dbB As Double) As Rational
        Return Rational.opSuma(prA, New Rational(dbB))
    End Operator
    Public Shared Operator -(ByVal prA As Rational, ByVal prB As Rational) As Rational
        Return Rational.opResta(prA, prB)
    End Operator
    Public Shared Operator -(ByVal dbA As Double, ByVal prB As Rational) As Rational
        Return Rational.opResta(New Rational(dbA), prB)
    End Operator
    Public Shared Operator -(ByVal prA As Rational, ByVal dbB As Double) As Rational
        Return Rational.opResta(prA, New Rational(dbB))
    End Operator
    Public Shared Operator -(ByVal prA As Rational) As Rational
        Dim prB As New Rational(prA)
        prB.opChgSgn()
        Return prB
    End Operator
    Public Shared Operator *(ByVal prA As Rational, ByVal prB As Rational) As Rational
        Return Rational.opMult(prA, prB)
    End Operator
    Public Shared Operator *(ByVal dbA As Double, ByVal prB As Rational) As Rational
        Return Rational.opMult(New Rational(dbA), prB)
    End Operator
    Public Shared Operator *(ByVal prA As Rational, ByVal dbB As Double) As Rational
        Return Rational.opMult(prA, New Rational(dbB))
    End Operator
    Public Shared Operator /(ByVal prA As Rational, ByVal prB As Rational) As Rational
        Return Rational.opDiv(prA, prB)
    End Operator
    Public Shared Operator /(ByVal dbA As Double, ByVal prB As Rational) As Rational
        Return Rational.opDiv(New Rational(dbA), prB)
    End Operator
    Public Shared Operator /(ByVal prA As Rational, ByVal dbB As Double) As Rational
        Return Rational.opDiv(prA, New Rational(dbB))
    End Operator
    Public Shared Operator ^(ByVal prA As Rational, ByVal prB As Rational) As Rational
        Return Rational.opPow(prA, prB.ToDouble)
    End Operator
    Public Shared Operator ^(ByVal prA As Rational, ByVal dbB As Double) As Rational
        Return Rational.opPow(prA, dbB)
    End Operator
    Public Shared Function opPow(ByVal opA As Rational, ByVal expB As Double) As Rational
        Dim opC As Rational = Nothing
        Try
            opC = New Rational(Math.Pow(opA.ToDouble, expB))
            opC.actualiza()
        Catch ex As Exception
            Throw ex
        End Try
        Return opC
    End Function
    Public Shared Function toStringBase(cfg As Config, ByVal db As Double) As String
        If cfg IsNot Nothing Then
            Return toStringBase(db, cfg.bRounding, cfg.base, cfg.nDecimals)
        Else
            Return toStringBase(db, False, MathGlobal8.outputBase.decimal, 3)
        End If
    End Function
    Public Shared Function toStringBase(ByVal db As Double,
                                        Optional bRounding As Boolean = False,
                                        Optional base As MathGlobal8.outputBase = MathGlobal8.outputBase.decimal,
                                        Optional nDecimals As Int32 = 3) As String
        Dim sDb As String
        Dim sgn As Int64
        Try
            sgn = Math.Sign(db)
            db = Math.Abs(db)
            sDb = db.ToString("e15", MathGlobal8.us)
            Dim vDb() As String = Split(sDb, "e")
            Dim fra As Double = db - Math.Floor(db)
            Dim sFra As String = ""
            Dim db1 As Double = Math.Floor(db)
            sDb = db1.ToString
            Dim j As Int64 = 0
            Dim e1 As String = ""
            Dim i As Int64
            If base =
            MathGlobal8.outputBase.decimal Then
                sDb = db.ToString(MathGlobal8.us)
            ElseIf base =
            MathGlobal8.outputBase.hexadecimal Then
                sDb = UCase(Hex(db1))
                If fra Then
                    Dim nDec As Int64 = nDecimals
                    If bRounding Then
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
                    If bRounding AndAlso Len(sFra) > nDec Then
                        ' Truncate 'sFra' to 'nDec' characters:
                        sFra = Mid(sFra, 1, nDec)
                    End If
                    sFra = UCase(sFra)
                End If
            ElseIf Base =
            MathGlobal8.outputBase.octal Then
                sDb = Convert.ToString(CLng(db1), 8)
                If fra Then
                    Dim nDec As Int64 = nDecimals
                    If bRounding Then
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
                    If bRounding AndAlso Len(sFra) > nDec Then
                        ' Truncate 'sFra' to 'nDec' characters:
                        sFra = Mid(sFra, 1, nDec)
                    End If
                End If
            Else
                sDb = Convert.ToString(CLng(db1), 2)
                If fra Then
                    Dim nDec As Int64 = nDecimals
                    If bRounding Then
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
                    If bRounding AndAlso Len(sFra) > nDec Then
                        ' Truncate 'sFra' to 'nDec' characters:
                        sFra = Mid(sFra, 1, nDec)
                    End If
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
            If base <>
            MathGlobal8.outputBase.decimal Then
                If base =
                MathGlobal8.outputBase.hexadecimal Then
                    sDb = "&h" + sDb
                ElseIf Base =
                MathGlobal8.outputBase.octal Then
                    sDb = "&o" + sDb
                ElseIf Base =
                MathGlobal8.outputBase.binary Then
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
        Return ToString(Nothing)
    End Function
    Public Overloads Function ToString(Optional cfg As Config = Nothing) As String

        ' output string will be ="" if its not
        ' possible to return an integer or fraction
        Dim e1 As String = ""
        Try
            Dim base As MathGlobal8.outputBase = MathGlobal8.outputBase.decimal
            Dim bRounding As Boolean = False
            Dim nDecimals As Int32 = 16
            If cfg IsNot Nothing Then
                base = cfg.base
                bRounding = cfg.bRounding
                nDecimals = cfg.nDecimals
            End If
            actualiza()
            Dim num, den As Double

            If base = MathGlobal8.outputBase.decimal Then
                Do While prDblD AndAlso prDblD < 1.0
                    prDblD *= 2.0
                    prDblN *= 2.0
                Loop
                num = Math.Abs(prDblN)
                den = prDblD
                If Math.Floor(num) <> num AndAlso Math.Floor(10000 / num) =
                10000 / num Then
                    den *= 10000 / num : num = 10000
                End If
                If bRounding Then
                    num = Math.Round(num, nDecimals)
                    den = Math.Round(den, nDecimals)
                    If sgn * num / den <> prDblN / prDblD Then
                        Return ""
                    End If
                End If
                If (Math.Floor(den) <> den AndAlso
                Math.Floor(den * 1000) = den * 1000) OrElse
                (Math.Floor(num) <> num AndAlso
                Math.Floor(num * 1000) = num * 1000) Then
                    num *= 1000
                    den *= 1000
                End If

                If base = MathGlobal8.outputBase.decimal Then
                    Dim db As Double = Me.ToDouble
                    If Math.Floor(db) <> db AndAlso
                    Math.Floor(1.0 / db) = 1.0 / db AndAlso
                    db > 10 ^ -10 AndAlso db < 10 ^ 10 Then
                        If db < 0 Then
                            Return "-1/" + (1.0 / -db).ToString(MathGlobal8.us)
                        Else
                            Return "1/" + (1.0 / db).ToString(MathGlobal8.us)
                        End If
                    End If
                End If
                If num = Math.Floor(num) AndAlso
                den = Math.Floor(den) AndAlso
                num > 1.0 AndAlso
                den > 1.0 AndAlso
                num < 10 ^ 6 AndAlso
                den < 10 ^ 6 Then

                    Dim d As Double = gcd(num, den)
                    If d > 1.0 Then
                        num /= d
                        den /= d
                    End If
                End If
                If num > 10 ^ 5 OrElse
                den > 10 ^ 5 OrElse
                num / den > 10 ^ 5 OrElse
                num < 10 ^ -3 OrElse
                den < 10 ^ -3 Then
                    Return ""
                End If
            Else
                Do While prDblD < 1.0
                    prDblD *= 2.0
                    prDblN *= 2.0
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
            Dim sNum As String = num.ToString(MathGlobal8.us)
            Dim sDen As String = den.ToString(MathGlobal8.us) ' (New Precis(Me.prDblD)).ToString
            If Regex.IsMatch(sNum + sDen, "E|e") Then
                Return ""
            End If
            If base <>
                MathGlobal8.outputBase.decimal Then
                sNum = Rational.toStringBase(prDblN, bRounding, base, nDecimals)
                sDen = Rational.toStringBase(prDblD, bRounding, base, nDecimals)
            End If

            If sNum = "0" Then
                e1 = "0"
            ElseIf sDen = sNum Then
                e1 = "1"
            ElseIf sDen = "1" OrElse
            sDen = "&h1" OrElse
            sDen = "&o1" OrElse
            sDen = "&b1" Then
                e1 = sNum
            Else
                e1 = sNum + "/" + sDen
                'e1 += vbCrLf + "= " + ToString()
            End If
            If sgn = -1 Then
                e1 = "-" + e1
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return e1
    End Function

End Class
