Imports System.Text.RegularExpressions

<Serializable()>
Public Class Complex84
    'Public Shared sI As String = MathGlobal8.sImag
    Public Shared us As New System.Globalization.CultureInfo("en-US")
    Public Shared oneHalf As New Complex84(0.5)
    Public pRe, pIm As Precis84
    Public Sub New()
    End Sub
    Public Sub New(ByVal Re As Double)
        Me.pRe = New Precis84(Re)
        Me.pIm = New Precis84(0)
    End Sub
    Public Sub New(ByVal Re As Double, ByVal Im As Double)
        Me.pRe = New Precis84(Re)
        Me.pIm = New Precis84(Im)
    End Sub
    Public Sub New(ByVal cjo As Complex84)
        Me.pRe = New Precis84(cjo.pRe)
        Me.pIm = New Precis84(cjo.pIm)
    End Sub
    Public Sub New(ByVal prRe As Precis84, ByVal prIm As Precis84)
        Me.pRe = New Precis84(prRe)
        Me.pIm = New Precis84(prIm)
    End Sub
    Public Shared ReadOnly Property zero() As Complex84
        Get
            Return New Complex84(0.0)
        End Get
    End Property
    Public Shared ReadOnly Property one() As Complex84
        Get
            Return New Complex84(1.0)
        End Get
    End Property
    Public Shared ReadOnly Property minusOne() As Complex84
        Get
            Return New Complex84(-1.0)
        End Get
    End Property
    Public Shared ReadOnly Property i() As Complex84
        Get
            Return New Complex84(0.0, 1.0)
        End Get
    End Property
    Public Shared ReadOnly Property minusi() As Complex84
        Get
            Return New Complex84(0.0, -1.0)
        End Get
    End Property
    Public Shared Function parse(cfg As Config84, ByVal strCjo As String) As Complex84
        Dim cjo As New Complex84
        Try
            If strCjo.Chars(0) <> "-" Then strCjo = "+" + strCjo
            ' suponemos que sólo habrán paréntesis en los extremos de strCjo
            ' y no en el interior:  SÍ (( i*1+2)) pero NO ((1+i)+(2))
            Do While strCjo.Length > 2 AndAlso strCjo.Chars(0) = "(" _
                AndAlso Mid(strCjo, Len(strCjo)) = ")"
                strCjo = Mid(strCjo, 2, Len(strCjo) - 2)
            Loop
            Dim mc As MatchCollection = Regex.Matches(strCjo, "[-+]+[^-+]+")
            Dim i As Int32
            Dim db As Double
            For i = 0 To mc.Count - 1
                Dim e1 As String = mc(i).Value
                If InStr(e1, cfg.sImg) = 0 Then 'If InStr(e1, "i") = 0 Then
                    If Double.TryParse(
                            e1,
                            Globalization.NumberStyles.Number Or Globalization.NumberStyles.AllowExponent,
                            us,
                            db) Then
                        cjo.pRe += db
                    Else
                        Exit Try  ' couldn't parse
                    End If
                Else
                    'e1 = Regex.Replace(e1, "[i\*]", "")
                    e1 = Regex.Replace(e1, "[" + cfg.sImg + "\*]", "")
                    If Double.TryParse(
                            e1,
                            Globalization.NumberStyles.Number Or Globalization.NumberStyles.AllowExponent,
                            us,
                            db) Then
                        cjo.pIm += db
                    Else
                        Exit Try  ' couldn't parse
                    End If
                End If
            Next
            Return cjo
        Catch ex As Exception

        End Try

        ' try another solution:
        'Try
        '    Dim sy1 As SY = SY.parse(strCjo, True, True)
        '    Dim mtx As Matrix = sy1.evalMatrix()
        '    cjo = mtx.vVect(0).vPoly(0).cf(0)
        'Catch ex2 As Exception
        '    Throw New Exception(ex2.Message)
        'End Try
        Return cjo
    End Function
    'Public ReadOnly Property isNaN() As Boolean
    '    Get
    '        If Double.IsNaN(pRe) OrElse Double.IsNaN(pIm) Then
    '            Return True
    '        ElseIf Double.IsInfinity(pRe) OrElse Double.IsInfinity(pIm) Then
    '            Return True
    '        End If
    '        Return False
    '    End Get
    'End Property
    Public Shared Operator +(ByVal cjoA As Complex84, ByVal db As Double) As Complex84
        Return New Complex84(cjoA.pRe + New Precis84(db), cjoA.pIm)
    End Operator
    Public Shared Operator +(ByVal db As Double, ByVal cjoA As Complex84) As Complex84
        Return New Complex84(cjoA.pRe + db, cjoA.pIm)
    End Operator
    Public Shared Operator +(ByVal cjoA As Complex84, ByVal cjoB As Complex84) As Complex84
        Dim prRe As Precis84 = cjoA.pRe + cjoB.pRe
        Dim prIm As Precis84 = cjoA.pIm + cjoB.pIm
        Return New Complex84(prRe, prIm)
    End Operator
    Public Shared Operator -(ByVal cjoA As Complex84) As Complex84
        'Dim prRe As Precis84 = cjoA.pRe
        'Dim prIm As Precis84 = cjoA.pIm
        'Return New Complex84(-prRe, -prIm)
        Return New Complex84(-cjoA.pRe, -cjoA.pIm)
    End Operator
    Public Shared Operator -(ByVal cjoA As Complex84, ByVal db As Double) As Complex84
        Return cjoA - New Complex84(db)
        'Return New Complex84(cjoA.pRe - db, cjoA.pIm)
    End Operator
    Public Shared Operator -(ByVal db As Double, ByVal cjoA As Complex84) As Complex84
        Return New Complex84(db) - cjoA
        'Return New Complex84(db - cjoA.pRe, cjoA.pIm)
    End Operator
    Public Shared Operator -(ByVal cjoA As Complex84, ByVal cjoB As Complex84) As Complex84
        Dim Re As Precis84 = cjoA.pRe - cjoB.pRe
        Dim Im As Precis84 = cjoA.pIm - cjoB.pIm
        Return New Complex84(Re, Im)
    End Operator
    Public Shared Operator *(ByVal cjoA As Complex84, ByVal db As Double) As Complex84
        Return New Complex84(cjoA.pRe * db, cjoA.pIm * db)
    End Operator
    Public Shared Operator *(ByVal db As Double, ByVal cjoB As Complex84) As Complex84
        Return New Complex84(cjoB.pRe * db, cjoB.pIm * db)
    End Operator
    Public Shared Operator *(ByVal cjoA As Complex84, ByVal cjoB As Complex84) As Complex84
        If cjoA.pIm.IsZero AndAlso cjoB.pIm.IsZero Then
            Return New Complex84(cjoA.pRe * cjoB.pRe, New Precis84(0.0))
        End If
        Dim prRe As Precis84 = cjoA.pRe * cjoB.pRe - cjoA.pIm * cjoB.pIm
        Dim prIm As Precis84 = cjoA.pRe * cjoB.pIm + cjoA.pIm * cjoB.pRe
        Return New Complex84(prRe, prIm)
    End Operator
    Public Shared Operator /(ByVal num As Double, ByVal cjoA As Complex84) As Complex84
        Return New Complex84(num, 0.0) / cjoA
    End Operator
    Public Shared Operator /(ByVal cjoA As Complex84, ByVal den As Double) As Complex84
        Dim prD As New Precis84(1.0, den)
        Return cjoA * New Complex84(prD, New Precis84(0.0))
        'Dim pRe As New Precis84(cjoA.pre.
        'cjoC.pRe /= den : cjoC.pIm /= den
        'Return cjoC
    End Operator
    Public Shared Operator /(ByVal cjoA As Complex84, ByVal cjoB As Complex84) As Complex84
        If cjoB.IsZero Then
            Throw New IndexOutOfRangeException
        End If
        If cjoA.pIm.IsZero AndAlso cjoB.pIm.IsZero Then
            Dim pRe As Precis84 = cjoA.pRe / cjoB.pRe
            Dim pZero As New Precis84(0.0)
            Return New Complex84(pRe, pZero)
        End If
        '  x       a + b i      ac + bd     bc - ad
        ' ---   = ---------  = --------- + ---------
        '  y       c + d i      c^2+d^2     c^2 +d^2
        Dim a As Precis84 = cjoA.pRe
        Dim b As Precis84 = cjoA.pIm
        Dim c As Precis84 = cjoB.pRe
        Dim d As Precis84 = cjoB.pIm
        Dim Den As Precis84 = c * c + d * d
        If Den.IsZero Then
            Throw New Exception("Division by zero.")
        End If
        Return New Complex84((a * c + b * d) / Den,
                           (b * c - a * d) / Den)
    End Operator
    Public Function opModulo() As Double
        Dim pr As Precis84 = Nothing
        Try
            pr = pRe * pRe + pIm * pIm
            Return Math.Sqrt(pr.ToDouble)
        Catch ex As Exception
            Dim db As Double = Double.MaxValue / 2.0
            If pr IsNot Nothing Then
                db *= pr.sgn
            End If
            Return db
        End Try
    End Function
    Public Shared Operator ^(ByVal cjoA As Complex84, ByVal cjoB As Complex84) As Complex84
        If cjoA.pIm.IsZero AndAlso cjoB.pIm.IsZero Then
            Dim rB As Double = cjoB.pRe.ToDouble
            If rB = Math.Floor(rB) Then
                Dim powAB As Precis84 = cjoA.pRe ^ cjoB.pRe
                Return New Complex84(powAB, New Precis84(0.0))
            End If
        End If
        If cjoB.IsZero Then
            Return Complex84.one
        ElseIf cjoA.IsZero Then
            Return Complex84.zero
        End If
        ' if a= (a1,a2) = (r,alfa) (r=modulus, alfa=argument=atan2(a2,a1))
        ' and b=(c,d) then
        ' a^b = r^c * exp(-d*alfa) * [ cos(d*log(r) + c * alfa + i * sin(d*log(r)+c*alfa)
        Dim c As Double = cjoB.pRe.ToDouble
        Dim d As Double = cjoB.pIm.ToDouble
        Dim r As Double = cjoA.opModulo
        If r = 0 Then
            Return Complex84.zero
        End If
        Dim alfa As Double = Math.Atan2(cjoA.pIm.ToDouble, cjoA.pRe.ToDouble)
        Dim rcTimesExpdalfa As Double = Math.Pow(r, c) * Math.Exp(-d * alfa)
        Dim dlogrPluscalfa As Double = d * Math.Log(r) + c * alfa
        Return New Complex84(rcTimesExpdalfa * Math.Cos(dlogrPluscalfa),
                             rcTimesExpdalfa * Math.Sin(dlogrPluscalfa))
    End Operator
    Public Shared Function opCos(ByVal x As Complex84) As Complex84
        If x.pIm.IsZero Then
            Dim re As Double = x.pRe.ToDouble Mod (2 * Math.PI)
            Return New Complex84(Math.Cos(re))
        End If
        '           e^(ix)+e^(-ix)
        ' cos(x) = ---------------
        '                2 
        ''
        Dim ix As Complex84 = New Complex84(-x.pIm, x.pRe)
        Dim minusix As New Complex84(x.pIm, -x.pRe)
        Dim e As New Complex84(Math.Exp(1), 0.0)
        Dim eix As Complex84 = e ^ ix
        Dim eminusix As Complex84 = e ^ minusix
        Dim num As Complex84 = eix + eminusix
        Dim ret As New Complex84(num.pRe / 2.0, num.pIm / 2.0)
        Return ret
    End Function
    Public Shared Function opArg(ByVal x As Complex84) As Complex84
        Return New Complex84(Math.Atan2(x.pIm.ToDouble, x.pRe.ToDouble), 0.0)
    End Function
    Public Shared Function opExp(ByVal x As Complex84) As Complex84
        If x.pIm.IsZero Then
            Return New Complex84(Math.E ^ x.pRe.ToDouble, 0.0)
        End If
        Return New Complex84(Math.E, 0.0) ^ x
    End Function
    Public Shared Function opLn(ByVal x As Complex84) As Complex84
        If x.pIm.IsZero Then
            Return New Complex84(Math.Log(x.pRe.ToDouble), 0.0)
        End If

        ' ln z = ln |z| + i*arg(z)
        Dim arg As Double = Math.Atan2(x.pIm.ToDouble, x.pRe.ToDouble)
        Dim re As Double = Math.Log(x.opModulo)
        Return New Complex84(re, arg)

    End Function
    Public Shared Function opACsc(ByVal z As Complex84) As Complex84
        '           
        ' acsc(z) = asin(1.0/z)
        '           
        '
        Dim arg As Complex84 = one / z
        Return opASin(z)
    End Function
    Public Shared Function opACscH(ByVal z As Complex84) As Complex84
        '           
        ' acscH(z) = asinh(1.0/z)
        '           
        '
        Dim arg As Complex84 = one / z
        Return opASinH(z)
    End Function
    Public Shared Function opACos(ByVal z As Complex84) As Complex84
        '           
        ' acos(z) = -i * log[z + i*(1.0 - z*z))^0.5]
        '           
        '
        Dim arg As Complex84 = z + i * (one - z * z) ^ oneHalf
        Return -Complex84.i * opLn(arg)
    End Function
    Public Shared Function opACosH(ByVal z As Complex84) As Complex84
        '           
        ' acosh(z) = log(z + sqrt(z * z - 1.0))
        '           
        '
        Dim arg As Complex84 = z + i * (z * z - one) ^ oneHalf
        Return opLn(arg)
    End Function
    Public Shared Function opACot(ByVal z As Complex84) As Complex84
        '           
        ' acot(z) = atan(1.0 / z)
        '           
        '
        Return opAtan(one / z)
    End Function
    Public Shared Function opACotH(ByVal z As Complex84) As Complex84
        '           
        ' acoth(z) = atanh(1.0 / z)
        '           
        '
        Return opAtanH(one / z)
    End Function
    Public Shared Function opASec(ByVal z As Complex84) As Complex84
        '           
        ' asec(z) = acos(1.0 / z)
        '           
        '
        Return opACos(one / z)
    End Function
    Public Shared Function opASecH(ByVal z As Complex84) As Complex84
        '           
        ' asech(z) = acosh(1.0 / z)
        '           
        '
        Return opACosH(one / z)
    End Function
    Public Shared Function opASin(ByVal z As Complex84) As Complex84
        '           
        ' asin(z) = -i * log(i*z +(1.0 - z*z))^0.5)
        '           
        '
        Dim arg As Complex84 = i * z + (one - z * z) ^ oneHalf
        Return -i * opLn(arg)
    End Function

    Public Shared Function opASinH(ByVal z As Complex84) As Complex84
        '           
        ' asinh(z) = log(z +(z*z + 1.0)^0.5)
        '           
        '
        Dim arg As Complex84 = z + (z * z + one) ^ oneHalf
        Return opLn(arg)
    End Function

    Public Shared Function opAtan(ByVal z As Complex84) As Complex84
        '           
        ' atan(z)= 1/(2i) * ln ((1 + iz)/(1 - iz)) ' 2014/03/06 http://mathforum.org/library/drmath/view/72732.html
        '
        Return opLn((i - z) / (i + z)) * i * -0.5
    End Function
    Public Shared Function opAtanH(ByVal z As Complex84) As Complex84
        '           
        ' atanh(z) = log((1.0 + z) / (1.0 - z)) /2  2014/03/06
        '           
        '
        Return opLn((one + z) / (one - z)) * 0.5
    End Function
    Public Shared Function opCosH(ByVal x As Complex84) As Complex84
        '           e^(x)+e^(-x)
        ' cos(x) = ---------------
        '               2
        '
        Dim ex As Complex84 = New Complex84(Math.E) ^ x
        Dim eminusx As Complex84 = New Complex84(Math.E) ^ -x
        Dim num As Complex84 = ex + eminusx
        Dim ret As New Complex84(num.pRe / 2.0, num.pIm / 2.0)
        Return ret
    End Function
    Public Shared Function opCot(ByVal z As Complex84) As Complex84
        ' 
        ' cot(z) = cos(z) / sin(z)
        ' 
        '
        Return opCos(z) / opSin(z)
    End Function
    Public Shared Function opCotH(ByVal z As Complex84) As Complex84
        ' 
        ' coth(z) = cosh(z) / sinh(z)
        ' 
        '
        Return opCosH(z) / opSinH(z)
    End Function
    Public Shared Function opCsc(ByVal z As Complex84) As Complex84
        ' 
        ' csc(z) = 1.0 / sin(z)
        ' 
        '
        Return one / opSin(z)
    End Function
    Public Shared Function opCscH(ByVal z As Complex84) As Complex84
        ' 
        ' csch(z) = 1.0 / sinh(z)
        ' 
        '
        Return one / opSinH(z)
    End Function
    Public Shared Function opSec(ByVal z As Complex84) As Complex84
        ' 
        ' csc(z) = 1.0 / cos(z)
        ' 
        '
        Return one / opCos(z)
    End Function
    Public Shared Function opSecH(ByVal z As Complex84) As Complex84
        ' 
        ' sech(z) = 1.0 / cosh(z)
        ' 
        '
        Return one / opCosH(z)
    End Function
    Public Shared Function opSin(ByVal x As Complex84) As Complex84
        If x.pIm.IsZero Then
            ' Needed because if arg>> then math.sin(arg) fails:
            Dim re As Double = x.pRe.ToDouble Mod (2 * Math.PI)
            Return New Complex84(Math.Sin(re))
        End If
        '           e^(ix)-e^(-ix)          e^(ix)-e^(-ix)
        ' sin(x) = --------------- = - i * ----------------
        '               2 * i                     2
        ''
        Dim ix As Complex84 = New Complex84(-x.pIm, x.pRe)
        Dim minusix As New Complex84(x.pIm, -x.pRe)
        Dim e As New Complex84(Math.Exp(1), 0.0)
        Dim eix As Complex84 = e ^ ix
        Dim eminusix As Complex84 = e ^ minusix
        Dim num As Complex84 = eix - eminusix
        Dim ret As New Complex84(num.pRe / 2.0, num.pIm / 2.0)
        Dim retTimesMinusI As New Complex84(ret.pIm, -ret.pRe)
        Return retTimesMinusI
    End Function
    Public Shared Function opSinH(ByVal x As Complex84) As Complex84
        '           e^(z)-e^(-z)
        ' sinh(z) = ---------------
        '               2
        '
        Dim minusx As New Complex84(x)
        minusx.opChgSgn()
        Dim e As New Complex84(Math.Exp(1), 0)
        Dim eix As Complex84 = opExp(x)
        Dim eminusx As Complex84 = opExp(minusx)
        Dim num As Complex84 = eix - eminusx
        Dim ret As New Complex84(num.pRe / 2.0, num.pIm / 2.0)
        Return ret
    End Function
    Public Shared Function optan(ByVal z As Complex84) As Complex84
        ' 
        ' tan(z) = sin(z) / cos(z)
        ' 
        '
        Return opSin(z) / opCos(z)
    End Function
    Public Shared Function optanh(ByVal z As Complex84) As Complex84
        ' 
        ' tanh(z) = sinh(z) / cosh(z)
        ' 
        '
        Return opSinH(z) / opCosH(z)
    End Function

    Public Function opNorm() As Double
        Return (pRe * pRe + pIm * pIm).ToDouble
    End Function
    Public Shared Function opConjugate(ByVal x As Complex84) As Complex84
        Return New Complex84(x.pRe, -x.pIm)
    End Function
    Public Shared Function opIgual(ByVal x As Complex84, ByVal y As Complex84) As Boolean
        Return ((x.pRe - y.pRe).IsZero AndAlso (x.pIm - y.pIm).IsZero)
    End Function
    Public Sub opChgSgn()
        pRe *= -1.0
        pIm *= -1.0
    End Sub
    Public ReadOnly Property IsReal() As Boolean
        Get
            Return (pIm.ToDouble = 0.0)
        End Get
    End Property
    Public Function IsZero() As Boolean
        Return (pRe.IsZero AndAlso pIm.IsZero)
    End Function
    Public Function esCero() As Boolean
        Return (pRe.IsZero AndAlso pIm.IsZero)
    End Function
    Public Function toStringComplex(cfg As Config84) As String
        Dim nDec As Int32 = cfg.nDecimals
        If nDec < 0 OrElse nDec > 15 Then
            nDec = 15
        End If
        Dim sI As String = cfg.sImg
        Dim s As String = ""
        Dim iNotZero As Int32 = 0
        Dim Re As Double = Me.pRe.ToDouble
        Dim Im As Double = Me.pIm.ToDouble
        Dim ReAbs As Double = Math.Abs(Re)
        Dim ImAbs As Double = Math.Abs(Im)
        If cfg.bRounding Then
            If ImAbs < 10 ^ -6 AndAlso
             ReAbs > ImAbs * 10 ^ 6 Then
                Im = 0.0
            ElseIf ReAbs < 10 ^ -6 AndAlso
            ImAbs > ReAbs * 10 ^ 6 Then
                Re = 0.0
            End If
        End If

        Dim sRe As String = Re.ToString("e15", us)
        Dim vRe() As String = Split(sRe, "e")
        Dim fraRe As Double = Re - Math.Floor(Re)
        Dim sFraRe As String = ""
        'Dim i As Int32
        If cfg.bFractions Then
            'Dim raRe As New Rational(pRe.num * pRe.sgn, pRe.den)
            sRe = pRe.toStrRational(cfg)
            If Len(sRe) Then
                ' sRe contains an integer or a fraction
                GoTo doIm
            Else
                ' if sRe="" value should be shown "normally"
                ' because num. and/or denominator aren't
                ' integers
            End If
        End If
        If cfg.base <> numBase.decimal Then
            sRe = Precis84.toStringBase(cfg, Re)
        Else
            If cfg.bEngNotation AndAlso vRe.Length > 1 AndAlso
            (Math.Abs(Re) < 0.001 OrElse Math.Abs(Re) > 100000) Then
                Dim exp As Int32 = Int32.Parse(vRe(1))
                Dim mant As Double = Double.Parse(vRe(0), us)
                If cfg.bEngNotation AndAlso
                (Math.Abs(Re) < 0.1 OrElse Math.Abs(Re) >= 1000) Then
                    If exp < 0 OrElse mant < 10 Then
                        Do While Math.Abs(exp) Mod 3
                            mant *= 10 : exp -= 1
                        Loop
                    Else
                        Do While Math.Abs(exp) Mod 3
                            mant /= 10 : exp += 1
                        Loop
                    End If
                ElseIf cfg.bEngNotation Then
                    Do While exp > 0
                        mant *= 10 : exp -= 1
                    Loop
                    Do While exp < 0
                        mant /= 10 : exp += 1
                    Loop
                End If
                If cfg.bRounding AndAlso Math.Round(mant, nDec) Then
                    mant = Math.Round(mant, nDec)
                    If mant = 1000 OrElse mant = -1000 Then
                        exp += 3
                        mant /= 1000
                    End If
                End If
                If exp = 0 Then
                    sRe = mant.ToString(us)
                Else
                    sRe = mant.ToString(us) + "e" + exp.ToString
                End If
            ElseIf cfg.bRounding Then
                sRe = Math.Round(Re, nDec).ToString(us)
            Else
                sRe = Re.ToString(us)
            End If
        End If
doIm:
        Dim sIm As String = Im.ToString("e12", us)
        Dim vim() As String = Split(sIm, "e")
        Dim fraIm As Double = Im - Math.Floor(Im)
        Dim sFraIm As String = ""
        If cfg.bFractions Then
            'Dim raIm As New Rational(pIm.num * pIm.sgn, pIm.den)
            sIm = pIm.toStrRational(cfg)
            If Len(sIm) Then
                ' sIm contains an integer or a fraction
                GoTo endIm
            Else
                ' if sIm="" value should be shown "normally"
                ' because num. and/or denominator aren't
                ' integers
            End If
        End If
        If cfg.base <>
            numBase.decimal Then

            sIm = Precis84.toStringBase(cfg, Im)

        Else
            If cfg.bEngNotation AndAlso vim.Length > 1 AndAlso
            (Math.Abs(Im) < 0.001 OrElse Math.Abs(Im) > 100000) Then
                Dim exp As Int32 = Int32.Parse(vim(1))
                Dim mant As Double = Double.Parse(vim(0), us)
                If cfg.bEngNotation AndAlso
                (Math.Abs(Im) < 0.1 OrElse Math.Abs(Im) >= 1000) Then
                    If exp < 0 OrElse mant <= 10 Then
                        Do While Math.Abs(exp) Mod 3
                            mant *= 10 : exp -= 1
                        Loop
                    Else
                        Do While Math.Abs(exp) Mod 3
                            mant /= 10 : exp += 1
                        Loop
                    End If
                ElseIf cfg.bEngNotation Then
                    Do While exp > 0
                        mant *= 10 : exp -= 1
                    Loop
                    Do While exp < 0
                        mant /= 10 : exp += 1
                    Loop
                End If
                If cfg.bRounding AndAlso Math.Round(mant, nDec) Then
                    mant = Math.Round(mant, nDec)
                    If mant = 1000 OrElse mant = -1000 Then
                        exp += 3
                        mant /= 1000
                    End If
                End If
                If exp = 0 Then
                    sIm = mant.ToString(us)
                Else
                    sIm = mant.ToString(us) + "e" + exp.ToString
                End If
            ElseIf cfg.bRounding Then
                sIm = Math.Round(Im, nDec).ToString(us)
            Else
                sIm = Im.ToString(us)
            End If
        End If
endIm:
        s = sRe + "+" + sI + "*" + sIm
        If Re = 0.0 AndAlso Im = 0.0 Then
            s = "0"
        ElseIf Re = 0.0 Then
            If Im > 0.0 Then
                'If Im = 1.0 Then
                If sIm = "1" Then
                    s = sI ' "i"
                Else
                    s = sI + "*" + sIm
                End If
            Else
                'If Im = -1.0 Then
                If sIm = "-1" Then
                    s = "-" + sI ' "-i"
                Else
                    If sIm.Chars(0) = "-" Then
                        sIm = Mid(sIm, 2)
                    End If
                    s = "-" + sI + "*" + sIm
                End If
            End If
        ElseIf Im = 0.0 Then
            s = sRe
        ElseIf Im > 0.0 Then
            If sIm = "1" Then
                s = sRe + "+" + sI
            ElseIf sIm = "-1" Then
                s = sRe + "-" + sI
            Else
                s = sRe + "+" + sI + "*" + sIm
            End If
        Else
            If sIm = "-1" Then
                s = sRe + "-" + sI
            Else
                If sIm.Chars(0) = "-" Then
                    sIm = Mid(sIm, 2)
                End If
                s = sRe + "-" + sI + "*" + sIm
            End If
        End If
        Return s
    End Function
    Public Overloads Function toString() As String
        Return toStringComplex(New Config84)
    End Function
    Public Shared Function toStringV(ByVal vCjo() As Complex84) As String
        Dim e1 As String = ""
        Try
            Dim i As Int32
            For i = 0 To vCjo.Length - 1
                e1 += String.Format("({0}) ", i)
                If Not vCjo(i) Is Nothing Then
                    e1 += vCjo(i).toString
                End If
            Next
            e1 += vbCrLf
        Catch ex As Exception

        End Try
        Return e1
    End Function
    Public Shared Function toStringV2(ByVal vCjo() As Complex84) As String
        Dim e1 As String = "("
        Try
            Dim i As Int32
            For i = 0 To vCjo.Length - 1
                If Not vCjo(i) Is Nothing Then
                    e1 += vCjo(i).toString
                Else
                    e1 += "---"
                End If
                If i < vCjo.Length - 1 Then
                    e1 += ";"
                End If
            Next
            e1 += ")" + vbCrLf
        Catch ex As Exception

        End Try
        Return e1
    End Function
    Public Shared Function toStringM(ByVal mtxCjo()() As Complex84) As String
        Dim e1 As String = ""
        Try
            Dim i, j As Int32
            For i = 0 To mtxCjo.Length - 1
                For j = 0 To mtxCjo(i).Length - 1
                    e1 += String.Format("({0},{1}) ", i, j)
                    If Not mtxCjo(i)(j) Is Nothing Then
                        e1 += mtxCjo(i)(j).toString
                    End If
                Next
                e1 += vbCrLf
            Next
        Catch ex As Exception

        End Try
        Return e1
    End Function
    Public Shared Function toStringM(ByVal mtxCjo(,) As Complex84) As String
        Dim e1 As String = ""
        Try
            Dim i, j As Int32
            For i = 0 To mtxCjo.GetLength(0) - 1
                For j = 0 To mtxCjo.GetLength(1) - 1
                    e1 += String.Format("({0},{1}) ", i, j)
                    If Not mtxCjo(i, j) Is Nothing Then
                        e1 += mtxCjo(i, j).toString
                    End If
                Next
                e1 += vbCrLf
            Next
        Catch ex As Exception

        End Try
        Return e1
    End Function
    Public Shared Function sortRoots(ByVal roots() As Complex84) As Complex84()
        Dim sdb(roots.Length - 1) As String
        Dim cjo(roots.Length - 1) As Complex84
        Dim re(-1) As Double
        Dim cjoReOrig(-1) As Complex84
        Dim cjo3(-1)
        Try
            Dim i, k As Int32
            For i = 0 To roots.Length - 1
                Dim c0 As Double = roots(i).pRe.ToDouble
                Dim c1 As Double = roots(i).pIm.ToDouble
                If c1 = 0 Then
                    ReDim Preserve re(re.Length), cjoReOrig(re.Length)
                    re(re.Length - 1) = c0
                    ' to avoid loosing fractional accuracy:
                    cjoReOrig(re.Length - 1) = New Complex84(roots(i))
                Else
                    Dim alfa As Double = Math.Atan2(c1, Math.Abs(c0))
                    Dim aux As Double = 100000 - Math.Floor(Math.Abs(alfa) * 10000)
                    aux = aux * 10 + 2 + Math.Sign(c0)
                    sdb(k) = ""
                    If c1 Then
                        sdb(k) = "1"
                    End If
                    Dim db As Double = roots(i).opModulo
                    Dim ent As Double = Math.Abs(c0) ' Math.Floor(db)
                    Dim fra As Double = ent - c0 '  db - ent
                    If c0 < 0 Then ent = 10000000000000 - ent
                    sdb(k) += aux.ToString + "." +
                        String.Format("{0:00000000000000.00000000000000}.{1:0000000000000.00000000000000}", ent, fra)
                    cjo(k) = New Complex84(roots(i))
                    ReDim Preserve cjo3(cjo3.Length)
                    cjo3(cjo3.Length - 1) = New Complex84(roots(i))
                    k += 1
                End If
            Next
            'If re.Length Then
            Dim cjo2(cjo.Length - 1) As Complex84
            i = 0
            If re.Length Then
                Array.Sort(re, cjoReOrig)
                For i = 0 To re.Length - 1
                    cjo2(i) = cjoReOrig(i) ' New Complex84(re(i))
                Next
            End If
            If cjo3.Length Then
                ReDim Preserve sdb(k - 1)
                Array.Sort(sdb, cjo3)
                For j As Int32 = 0 To cjo3.Length - 1
                    cjo2(i) = cjo3(j)
                    i += 1
                Next
            End If
            cjo = cjo2
            'End If
        Catch ex As Exception
            Throw ex
        End Try
        Return cjo
    End Function
End Class
