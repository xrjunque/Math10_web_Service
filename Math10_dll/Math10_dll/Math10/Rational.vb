Imports System.Text.RegularExpressions
Imports System.Numerics

Public Class Rational
    Public Shared vPrime() As Int64 = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997}
    Public Sub New()
        MyBase.New()
    End Sub

    Private Numerator As BigInteger = 0
    Private Denominator As BigInteger = 1
    Private bReduced As Boolean = False
    Private bReduce As Boolean = True
    Public tipo As Double = 0.0
    Public Sub New(ByVal opB As Rational)
        Me.CopyFrom(opB)
    End Sub
    Public Sub CopyFrom(ByVal opB As Rational)
        Numerator = opB.Numerator
        Denominator = opB.Denominator
        tipo = opB.tipo
        bReduced = opB.bReduced
    End Sub
    Public Function getDenominator() As Double
        Return Me.Denominator
    End Function

    Public Sub New(ByVal db As Double)
        If Double.IsInfinity(db) OrElse Double.IsNaN(db) Then
            tipo = db
            Exit Sub
        End If
        Dim i As Int32 = 0
        Dim s As String = String.Format("{0,25:E16}", db)
        Dim vs() As String = Split(s, "E")
        Dim sInt As String = vs(0).Chars(2) + Mid(vs(0), 5)
        Dim exp As Int32 = Int32.Parse(vs(1)) - 16
        If exp < 0 Then
            i += -exp
            exp = 0
        End If
        Numerator = BigInteger.Parse(sInt) * BigInteger.Pow(10, exp)
        If s.Chars(1) = "-" Then Numerator = -Numerator
        Denominator = BigInteger.Pow(10, i)
    End Sub
    Public Sub New(ByVal num As Double, ByVal den As Double)
        Try
            If Double.IsInfinity(den) AndAlso
            (Not Double.IsNaN(num) AndAlso Not Double.IsInfinity(num)) Then
                tipo = 0.0
                num = 0
                den = 1
                Exit Try
            ElseIf Double.IsInfinity(num) OrElse Double.IsNaN(num) OrElse
            Double.IsInfinity(den) OrElse Double.IsNaN(den) Then
                tipo = num / den
                Exit Try
            End If
            Dim rNum As New Rational(num)
            Dim rDen As New Rational(den)
            Numerator = rNum.Numerator * rDen.Denominator
            Denominator = rNum.Denominator * rDen.Numerator
            reduce()
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Shared Function ParseRational(ByVal s As String, Optional cultureInfo As Globalization.CultureInfo = Nothing) As Rational
        Dim rtn As New Rational
        Try
            If s = "0" Then Exit Try
            If InStr(s, "∞") Then
                rtn.tipo = IIf(InStr(s, "-"), Double.NegativeInfinity, Double.PositiveInfinity)
                Exit Try
            End If
            Dim i As Int32 = 0
            s = UCase(s)
            Dim sgn As Int32 = 1
            If InStr(s, "E") = 0 Then s += "E+00"
            Dim vs() As String = Split(s, "E")
            Dim exp As Int32 = Int32.Parse(vs(1))
            Dim m As Match = Regex.Match(vs(0), "\" + G10.CI.NumberFormat.CurrencyDecimalSeparator)
            If m.Success Then
                vs(0) = vs(0).Remove(m.Index, 1)
                exp -= vs(0).Length - m.Index
            End If
            Dim sInt As String = vs(0)
            If exp < 0 Then
                i += -exp
                exp = 0
            End If
            sInt = sInt.Replace(G10.CI.NumberFormat.CurrencyGroupSeparator, "")
            rtn.Numerator = BigInteger.Parse(sInt) * BigInteger.Pow(10, exp) ' New BigInteger(db)
            If sgn = -1 Then rtn.Numerator = -rtn.Numerator
            rtn.Denominator = BigInteger.Pow(10, i)

        Catch ex As Exception
            Throw
        End Try
        Return rtn
    End Function
    Public Shared Function ParseFraction(sNum As String) As Rational
        Dim r As New Rational()
        Try
            Dim ve1() As String = Split(sNum, "/")
            Dim N As Rational = ParseRational(ve1(0))
            Dim D As Rational = ParseRational(ve1(1))
            r.Numerator = N.Numerator * D.Denominator
            r.Denominator = N.Denominator * D.Numerator
        Catch ex As Exception
            Throw
        End Try
        Return r
    End Function
    Public ReadOnly Property ToDouble() As Double
        Get
            Try
                If tipo <> 0.0 Then Return tipo
                If Numerator.IsZero Then
                    Return 0.0
                End If
                Dim n As Double = Numerator
                If Denominator = 1 Then Return n
                If Numerator = Denominator Then Return 1
                If -Numerator = Denominator Then Return -1
                Dim d As Double = Denominator
                If Double.IsInfinity(Numerator) OrElse Double.IsInfinity(d) Then
                    Dim lgNum As Int32 = BigInteger.Log10(BigInteger.Abs(Numerator))
                    Dim lgDen As Int32 = BigInteger.Log10(Denominator)
                    Dim exp As Int32 = lgNum - lgDen
                    Dim sgn As Int32 = Numerator.Sign
                    Dim sN As String = BigInteger.Abs(Numerator).ToString
                    Dim sD As String = Denominator.ToString
                    Dim expN As Int32 = Len(sN)
                    Dim expD As Int32 = Len(sD)
                    If Len(sN) > 16 Then
                        expN = Len(sN) - 16
                        sN = Left(sN, 17)
                    Else
                        expN = 1
                    End If
                    If Len(sD) > 16 Then
                        expD = Len(sD) - 16
                        sD = Left(sD, 17)
                    Else
                        expD = 1
                    End If
                    Dim dbN As Double = Double.Parse(sN)
                    Dim dbD As Double = Double.Parse(sD)
                    Return sgn * dbN * 10 ^ (expN - expD) / dbD
                End If
                Return n / d
            Catch ex As Exception
                Throw
            End Try
        End Get
    End Property
    ReadOnly Property Sign() As Int64
        Get
            Return Denominator.Sign * Numerator.Sign
        End Get
    End Property
    Public Function IsZero() As Boolean
        If tipo Then Return False
        Return (Numerator = 0)
    End Function
    Public Property Num() As Double
        Get
            If tipo Then Return tipo
            Return CType(Numerator, Double)
        End Get
        Set(value As Double)
            Do While value <> Math.Floor(value) AndAlso value < 10.0 ^ 17
                value *= 10
                Denominator *= 10
            Loop
            Numerator = value
        End Set
    End Property
    Public ReadOnly Property Den() As Double
        Get
            Return CType(Denominator, Double)
        End Get
    End Property
    Public Function OpCompara(ByVal opB As Rational) As Int64
        Dim dif As Int64 = Math.Sign(Sign - opB.Sign)
        If dif Then Return dif
        Return (Numerator * opB.Denominator - Denominator * opB.Denominator).Sign
    End Function
    Public Function OpCompara(ByVal opB As Rational, ByVal redondeo As Boolean) As Int64
        Dim dif As Int64 = Math.Sign(Sign - opB.Sign)
        If Not redondeo AndAlso dif Then Return dif
        If redondeo AndAlso (Math.Abs(ToDouble) <= 0.001 OrElse Math.Abs(opB.ToDouble) <= 0.001) Then
            Return Math.Sign(ToDouble() - opB.ToDouble)
        End If
        Return Math.Sign(Math.Round(ToDouble(), 3) - Math.Round(opB.ToDouble, 3))
    End Function
    Public Sub OpSuma(ByVal opB As Rational)
        Try
            If tipo OrElse opB.tipo Then
                If Double.IsNaN(tipo) OrElse Double.IsNaN(opB.tipo) Then
                    tipo = Double.NaN
                    Exit Try
                End If
                tipo += opB.tipo
                Exit Try
            End If
            If opB.bReduced Then bReduced = True
            If Denominator = opB.Denominator Then
                Numerator += opB.Numerator
            Else
                Dim N As BigInteger = Numerator * opB.Denominator + Denominator * opB.Numerator
                Dim D As BigInteger = Denominator * opB.Denominator
                Numerator = N
                Denominator = D
            End If
            reduce()
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Shared Function OpSuma(ByVal opA As Rational, ByVal opB As Rational) As Rational
        Dim opC As New Rational(opA)
        opC.OpSuma(opB)
        Return opC
    End Function
    Public Shared Function OpResta(ByVal opA As Rational, ByVal opB As Rational) As Rational
        Dim opC As New Rational(opA)
        opC.OpSuma(-opB)
        Return opC
    End Function
    Public Sub OpMult(ByVal opB As Rational)
        Try
            If tipo OrElse opB.tipo Then
                If opB.tipo = 0 Then
                    tipo *= opB.ToDouble
                ElseIf tipo = 0 Then
                    tipo = opB.tipo * ToDouble
                Else
                    tipo *= opB.tipo
                End If
                Exit Try
            End If
            If opB.bReduced Then bReduced = True
            Numerator *= opB.Numerator
            Denominator *= opB.Denominator
            reduce()
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Shared Function OpMult(ByVal opA As Rational, ByVal opB As Rational) As Rational
        Dim opC As New Rational(opA)
        opC.OpMult(opB)
        Return opC
    End Function
    Public Sub OpMult(ByVal db As Double)
        Try
            Dim opB As New Rational(db)
            Me.OpMult(opB)
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Shared Function OpMult(ByVal opA As Rational, ByVal db As Double) As Rational
        Dim opC As New Rational(opA)
        opC.OpMult(db)
        Return opC
    End Function
    Public Sub OpDiv(ByVal opB As Rational)
        Try
            If opB.IsZero Then
                If IsZero() Then
                    CopyFrom(New Rational(Double.NaN))
                Else
                    CopyFrom(New Rational(ToDouble / 0.0))
                End If
                Exit Try
            ElseIf tipo OrElse opB.tipo Then
                CopyFrom(New Rational(ToDouble / opB.ToDouble))
                Exit Try
            End If
            If opB.bReduced Then bReduced = True
            If Numerator = opB.Numerator AndAlso Denominator = opB.Denominator Then
                Numerator = 1
                Denominator = 1
                Exit Try
            End If
            Numerator *= opB.Denominator
            Denominator *= opB.Numerator
            reduce()
        Catch ex As Exception
            Throw
        End Try
    End Sub

    Public Shared Function OpDiv(ByVal opA As Rational, ByVal opB As Rational) As Rational
        Dim opC As New Rational(opA)
        opC.OpDiv(opB)
        Return opC
    End Function
    Public Sub OpDiv(ByVal db As Double)
        Try
            Dim opB As New Rational(db)
            Me.OpDiv(opB)
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Sub OpChgSgn()
        tipo = -tipo
        Numerator = -Numerator
    End Sub
    Public Shared Function opFactors(db As Double) As Double()
        Dim ret(-1) As Double
        Try
            If Math.Floor(db) <> db OrElse db < 2 Then
                If db = 1.0 Then
                    ret = New Double() {1.0}
                End If
            Else
                Dim iv As Int64 = 0
                For i As Int64 = 0 To vPrime.Length - 1
                    Dim div As Double = db / vPrime(i)
                    If div = Math.Floor(div) Then
                        db = div
                        ReDim Preserve ret(iv)
                        ret(iv) = vPrime(i)
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
    Public Shared Function OpMod(ByVal opA As Rational, ByVal opB As Rational) As Double
        If opA.tipo OrElse opB.tipo Then
            Throw New ArgumentOutOfRangeException
        End If
        Return opA.ToDouble Mod opB.ToDouble
    End Function
    Public Shared Function GCD(ByVal u1 As Double, ByVal v1 As Double) As Double

        ' http://en.wikipedia.org/wiki/Binary_GCD_algorithm
        'int shift;
        '/* Let shift := lg K, where K is the greatest power of 2
        '      dividing both u and v. */
        'for (shift = 0; ((u | v) & 1) == 0; ++shift) {
        '       u >>= 1;
        '       v >>= 1;
        '}
        u1 = Math.Abs(u1) : v1 = Math.Abs(v1)
        If u1 > Long.MaxValue OrElse v1 > Long.MaxValue Then
            Return 1
        End If
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
    Public Sub reduce(Optional nBytes As Int32 = 150)
        Dim sN As Int64
        Try
            If G10.bCancel Then
                Throw New Exception("Cancelled")
            End If
            If tipo Then Exit Sub
            If Denominator.Sign = -1 Then
                Numerator = -Numerator
                Denominator = -Denominator
            End If
            sN = Numerator.Sign
            If Numerator.IsZero Then
                Denominator = 1
                Exit Try
            End If
            Numerator = BigInteger.Abs(Numerator)
            If Numerator = Denominator Then
                Denominator = 1.0
                Numerator = 1.0
                Exit Try
            End If
            Dim lnN As Int32 = Numerator.ToByteArray.Length
            Dim lnD As Int32 = Denominator.ToByteArray.Length
            If lnN > nBytes AndAlso lnD > nBytes Then

                Dim vN() As Byte = Numerator.ToByteArray
                Dim vD() As Byte = Denominator.ToByteArray
                Array.Reverse(vN)
                Array.Reverse(vD)
                Dim min As Int32 = Math.Min(lnN - nBytes, lnD - nBytes)
                ReDim Preserve vN(lnN - min), vD(lnD - min)
                Array.Reverse(vN)
                Array.Reverse(vD)
                Numerator = New BigInteger(vN)
                Denominator = New BigInteger(vD)
                If vN.Length < lnN OrElse vD.Length < vD.Length Then
                    bReduced = True
                End If
                Exit Try
            End If
            If bReduced = False Then
                Dim A As BigInteger = Numerator
                Dim B As BigInteger = Denominator
                Dim C As BigInteger = BigInteger.GreatestCommonDivisor(A, B)
                A /= C : B /= C
                Numerator = A
                Denominator = B
            End If
        Catch ex As Exception
            Throw
        End Try
        If sN = -1 Then Numerator = -Numerator
    End Sub
    Public Function Abs() As Rational
        Dim prC As New Rational(Me)
        prC.Numerator = BigInteger.Abs(Numerator)
        prC.Denominator = BigInteger.Abs(Denominator)
        Return prC
    End Function
    Public Shared Operator =(ByVal prA As Rational, ByVal prB As Rational) As Boolean
        Return (prA - prB).IsZero
    End Operator
    Public Shared Operator <>(ByVal prA As Rational, ByVal prB As Rational) As Boolean
        Return Not (prA - prB).IsZero
    End Operator
    Public Shared Operator >(ByVal prA As Rational, ByVal prB As Rational) As Boolean
        Dim prC As Rational = prA - prB
        Return prC.Sign = 1
    End Operator
    Public Shared Operator <(ByVal prA As Rational, ByVal prB As Rational) As Boolean
        Dim prC As Rational = prA - prB
        Return prC.Sign = -1
    End Operator
    Public Shared Operator +(ByVal prA As Rational, ByVal prB As Rational) As Rational
        Return Rational.OpSuma(prA, prB)
    End Operator
    Public Shared Operator +(ByVal dbA As Double, ByVal prB As Rational) As Rational
        Return Rational.OpSuma(New Rational(dbA), prB)
    End Operator
    Public Shared Operator +(ByVal prA As Rational, ByVal dbB As Double) As Rational
        Return Rational.OpSuma(prA, New Rational(dbB))
    End Operator
    Public Shared Operator -(ByVal prA As Rational, ByVal prB As Rational) As Rational
        Return Rational.OpResta(prA, prB)
    End Operator
    Public Shared Operator -(ByVal dbA As Double, ByVal prB As Rational) As Rational
        Return Rational.OpResta(New Rational(dbA), prB)
    End Operator
    Public Shared Operator -(ByVal prA As Rational, ByVal dbB As Double) As Rational
        Return Rational.OpResta(prA, New Rational(dbB))
    End Operator
    Public Shared Operator -(ByVal prA As Rational) As Rational
        Dim prB As New Rational(prA)
        prB.OpChgSgn()
        Return prB
    End Operator
    Public Shared Operator *(ByVal prA As Rational, ByVal prB As Rational) As Rational
        Return Rational.OpMult(prA, prB)
    End Operator
    Public Shared Operator *(ByVal dbA As Double, ByVal prB As Rational) As Rational
        Return Rational.OpMult(New Rational(dbA), prB)
    End Operator
    Public Shared Operator *(ByVal prA As Rational, ByVal dbB As Double) As Rational
        Return Rational.OpMult(prA, New Rational(dbB))
    End Operator
    Public Shared Operator /(ByVal prA As Rational, ByVal prB As Rational) As Rational
        Return Rational.OpDiv(prA, prB)
    End Operator
    Public Shared Operator /(ByVal dbA As Double, ByVal prB As Rational) As Rational
        Return Rational.OpDiv(New Rational(dbA), prB)
    End Operator
    Public Shared Operator /(ByVal prA As Rational, ByVal dbB As Double) As Rational
        Return Rational.OpDiv(prA, New Rational(dbB))
    End Operator
    Public Shared Operator ^(ByVal prA As Rational, ByVal prB As Rational) As Rational
        Return Rational.opPow(prA, prB.ToDouble)
    End Operator
    Public Shared Operator ^(ByVal prA As Rational, ByVal dbB As Double) As Rational
        Return Rational.opPow(prA, dbB)
    End Operator
    Public Shared Function opPow(ByVal opA As Rational, ByVal expB As Double) As Rational
        Dim opC As New Rational(0.0)
        Try
            If Double.IsInfinity(expB) Then
                If opA.tipo = 0 Then
                    Dim db1 = opA.ToDouble
                    If db1 = 1.0 Then
                        opC = New Rational(Double.NaN)
                    Else
                        opC = New Rational(db1 ^ expB)
                    End If
                    Exit Try
                End If
                opC.tipo = opA.tipo ^ expB
                Exit Try
            ElseIf opA.tipo = Double.NaN OrElse Double.IsNaN(expB) Then
                opC.tipo = Double.NaN
                Exit Try
            ElseIf opA.tipo Then
                opC.tipo = opA.tipo ^ expB
                Exit Try
            End If
            Dim sgn As Int32 = opA.Sign
            Dim NumA As BigInteger = BigInteger.Abs(opA.Numerator)
            Dim lnNum As BigInteger = BigInteger.Log(NumA)
            Dim lnDen As BigInteger = BigInteger.Log(opA.Denominator)
            Dim toDbl As Double = opA.ToDouble
            If (Not Double.IsInfinity(toDbl) AndAlso toDbl <> 1.0) OrElse lnDen.IsZero Then
                Dim db As Double = opA.ToDouble
                db = Math.Pow(db, expB)
                opC = New Rational(db)
            Else
                Dim db As Double
                ' opC = opA ^expB = exp(expB * log(opA.Num/opA.Den))
                Dim expR As New Rational(expB)
                Dim NumC As BigInteger = expR.Numerator * lnNum
                db = NumC / (expR.Numerator * lnDen)
                db = Math.E ^ db
                If sgn = -1 Then db = -db
                opC = New Rational(db)

            End If
            If opA.bReduced Then opC.bReduced = True
            opC.reduce()
        Catch ex As Exception
            Throw
        End Try
        Return opC
    End Function
    Public Shared Function toStringBase(ByVal db As Double,
                                        nDecimals As Int32,
                                        cultureInfo As Globalization.CultureInfo,
                                        base As Rational.Base) As String
        Dim sDb As String
        Dim sgn As Int64
        Try
            sgn = Math.Sign(db)
            db = Math.Abs(db)
            sDb = db.ToString("e15", cultureInfo)
            Dim bRounding As Boolean = IIf(nDecimals = 15, False, True)
            Dim vDb() As String = Split(sDb, "e")
            Dim fra As Double = db - Math.Floor(db)
            Dim sFra As String = ""
            Dim db1 As Double = Math.Floor(db)
            sDb = db1.ToString
            Dim j As Int64 = 0
            Dim e1 As String = ""
            Dim i As Int64
            If base =
            Rational.Base.Decimal Then
                sDb = db.ToString(cultureInfo)

            ElseIf base = Rational.Base.Hexadecimal Then
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

            ElseIf base = Rational.Base.Octal Then
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

        Catch ex As Exception
            Throw
        End Try
        Try
            If sgn = -1 Then
                sDb = "-" + sDb
            End If
            'Select Case base
            '    Case Rational.Base.Hexadecimal : sDb = "&h" + sDb
            '    Case Rational.Base.Octal : sDb = "&o" + sDb
            '    Case Rational.Base.Hexadecimal : sDb = "&b" + sDb
            'End Select
        Catch ex As Exception

        End Try
        Return sDb
    End Function
    Public Overrides Function toString() As String
        Return ToStringRational(G10.nDec, G10.CI, G10.currBase)
    End Function
    Public Enum Base
        Hexadecimal
        [Decimal]
        Octal
        Binary
    End Enum

    Public Function ToStringRational(Optional numDecimals As Int32 = 15,
                                    Optional cultureInfo As Globalization.CultureInfo = Nothing,
                                    Optional currBase As Base = Base.Decimal) _
                                    As String
        Dim e1 As String = ""
        Try
            If cultureInfo Is Nothing Then cultureInfo = New Globalization.CultureInfo("en-US")
            Dim nDec As Int32 = IIf(numDecimals > 15, 15, numDecimals)
            If nDec < 0 Then nDec = 0

            If tipo Then
                Return tipo.ToString()
            End If
            reduce()
            If currBase = Rational.Base.Decimal Then
                If Numerator.IsZero Then Return "0"
                If tipo Then Return tipo.ToString()
                If Denominator.Sign = -1 Then
                    Numerator = -Numerator
                    Denominator = -Denominator
                End If
                Dim lgNum As Int32 = BigInteger.Log10(BigInteger.Abs(Numerator))
                Dim lgDen As Int32 = BigInteger.Log10(Denominator)
                Dim exp As Int32 = lgNum - lgDen
                Dim sgn As Int32 = Numerator.Sign
                Dim sSgn As String = IIf(sgn < 0, "-", "")
                Dim sN As String = BigInteger.Abs(Numerator).ToString
                Dim sD As String = Denominator.ToString
                Dim expN As Int32 = Len(sN)
                Dim expD As Int32 = Len(sD)
                Dim sExpN As String = ""
                Dim sExpD As String = ""
                If Len(sN) > 16 Then
                    expN = Len(sN) - 16
                    sN = Left(sN, 17)
                    If expN Then sExpN = "e" + expN.ToString
                Else
                    expN = 1
                End If
                If Len(sD) > 16 Then
                    expD = Len(sD) - 16
                    sD = Left(sD, 17)
                    If expD Then sExpD = "e" + expD.ToString
                Else
                    expD = 1
                End If
                Dim dbN As Double = Double.Parse(sN)
                Dim dbD As Double = Double.Parse(sD)
                Dim db As Double = dbN / dbD
                Dim sDb As String = String.Format(cultureInfo, "{0,25:E16}", Math.Abs(db)).Replace(" ", "")
                Dim vs() As String = Split(sDb, "E")
                If G10.bTryIrrationals AndAlso Not bReduced Then
                    ' Find out if it is a stored irrational:
                    For i As Int32 = 0 To G10.namedIrrationals.Count - 1
                        Dim f As Double = G10.namedIrrationals.Keys(i) / db
                        f = Math.Round(f, 12)
                        Dim invf As Double = db / G10.namedIrrationals.Keys(i)
                        invf = Math.Round(invf, 12)
                        If f = Math.Floor(f) Then
                            Return sSgn + "(" + G10.namedIrrationals.Values(i) + IIf(f <> 1.0, "/" + f.ToString, "")
                        ElseIf invf = Math.Floor(invf) Then
                            Return sSgn + "(" + IIf(invf <> 1.0, invf.ToString + ")" + "*", "") + G10.namedIrrationals.Values(i)
                        End If
                    Next
                    For i As Int32 = 0 To G10.Irrationals2.Count - 1
                        Dim f As Double = G10.Irrationals2.Keys(i) / db
                        f = Math.Round(f, 12)
                        Dim invf As Double = db / G10.Irrationals2.Keys(i)
                        invf = Math.Round(invf, 12)
                        If f = Math.Floor(f) Then
                            Return sSgn + "(" + G10.Irrationals2.Values(i) + ")" + IIf(f <> 1.0, "/" + f.ToString, "")
                        ElseIf invf = Math.Floor(invf) Then
                            Return sSgn + "(" + IIf(invf <> 1.0, invf.ToString + ")" + "*", "") + G10.Irrationals2.Values(i)
                        End If
                        f = G10.Irrationals3.Keys(i) / db
                        f = Math.Round(f, 12)
                        invf = db / G10.Irrationals3.Keys(i)
                        invf = Math.Round(invf, 12)
                        If f = Math.Floor(f) Then
                            Return sSgn + "(" + G10.Irrationals3.Values(i) + ")" + IIf(f <> 1.0, "/" + f.ToString, "")
                        ElseIf invf = Math.Floor(invf) Then
                            Return sSgn + "(" + IIf(invf <> 1.0, invf.ToString + ")" + "*", "") + G10.Irrationals3.Values(i)
                        End If
                    Next
                End If
                If G10.frac AndAlso Not bReduced AndAlso sN.Length < 14 Then
                    If sD = "1" Then
                        Return sSgn + sN
                    End If
                    Return sSgn + sN + sExpN + "/" + sD + sExpD
                End If
                If Math.Abs(db) < 100000 AndAlso Math.Abs(db) > 0.00001 Then
                    db = sgn * dbN * 10 ^ (expN - expD) / dbD
                    If nDec <> 15 Then
                        db = Math.Round(db, nDec)
                    End If
                    Return db.ToString(cultureInfo)
                End If
                exp = expN - expD + Int32.Parse(vs(1))
                Dim dif As Int32 = exp Mod 3
                If dif < 0 Then dif = (3 - (-exp Mod 3)) Mod 3
                If dif AndAlso G10.eng Then
                    exp -= dif
                    vs(0) = vs(0).Insert(2 + dif, cultureInfo.NumberFormat.CurrencyDecimalSeparator)
                    vs(0) = vs(0).Remove(1, 1)
                End If
                If nDec < 15 Then
                    Dim pos As Int32 = 2 + nDec
                    If pos + 1 < vs(0).Length Then
                        Dim dec As Decimal = Decimal.Parse(vs(0), cultureInfo)
                        dec = Math.Round(dec, nDec)
                        vs(0) = dec.ToString(cultureInfo)
                    End If
                End If
                Dim sExp As String = IIf(exp = 0, "", "e" + exp.ToString)
                Do While Len(vs(0)) > 2 AndAlso Right(vs(0), 1) = "0"
                    vs(0) = Left(vs(0), Len(vs(0)) - 1)
                Loop
                If Right(vs(0), 1) = cultureInfo.NumberFormat.CurrencyDecimalSeparator Then
                    vs(0) = Left(vs(0), Len(vs(0)) - 1)
                End If
                Return sSgn + vs(0) + sExp
            End If
            Dim sNum As String = Num.ToString(cultureInfo)
            Dim sDen As String = Den.ToString(cultureInfo) ' (New Precis(Me.denominator)).ToString
            sNum = Rational.toStringBase(Me.Num, nDec, cultureInfo, currBase)
            sDen = Rational.toStringBase(Me.Den, nDec, cultureInfo, currBase)

            If sNum = "0" Then
                e1 = "0"
            ElseIf sDen = sNum Then
                e1 = "1"
            ElseIf sDen = "1" Then
                e1 = sNum
            Else
                e1 = sNum + "/" + sDen
            End If
            If Sign = -1 Then
                e1 = "-" + e1
            End If
        Catch ex As Exception
            Throw
        End Try
        Return e1
    End Function
End Class

