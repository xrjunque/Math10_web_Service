Imports System.Text.RegularExpressions
Imports System.Text
Imports System.Collections
Imports System.Numerics

Public Class Complex
    Public pRe, pIm As Rational
    Public Sub New()
    End Sub
    Public Sub New(ByVal Re As Double)
        Me.pRe = New Rational(Re)
        Me.pIm = New Rational(0)
    End Sub
    Public Sub New(ByVal Re As Double, ByVal Im As Double)
        Me.pRe = New Rational(Re)
        Me.pIm = New Rational(Im)
    End Sub
    Public Sub New(ByVal cjo As Complex)
        Me.pRe = New Rational(cjo.pRe)
        Me.pIm = New Rational(cjo.pIm)
    End Sub
    Public Sub New(ByVal prRe As Rational, ByVal prIm As Rational)
        Me.pRe = New Rational(prRe)
        Me.pIm = New Rational(prIm)
    End Sub
    Public Shared ReadOnly Property zero() As Complex
        Get
            Return New Complex(0.0)
        End Get
    End Property
    Public Shared ReadOnly Property one() As Complex
        Get
            Return New Complex(1.0)
        End Get
    End Property
    Public Shared ReadOnly Property minusOne() As Complex
        Get
            Return New Complex(-1.0)
        End Get
    End Property
    Public Shared ReadOnly Property oneHalf() As Complex
        Get
            Return New Complex(0.5)
        End Get
    End Property
    Public Shared ReadOnly Property i() As Complex
        Get
            Return New Complex(0.0, 1.0)
        End Get
    End Property
    Public Shared ReadOnly Property minusi() As Complex
        Get
            Return New Complex(0.0, -1.0)
        End Get
    End Property
    Public Function ToDouble() As Double
        Return pRe.ToDouble
    End Function
    Public Shared Operator +(ByVal cjoA As Complex, ByVal db As Double) As Complex
        Return New Complex(cjoA.pRe + New Rational(db), cjoA.pIm)
    End Operator
    Public Shared Operator +(ByVal db As Double, ByVal cjoA As Complex) As Complex
        Return New Complex(cjoA.pRe + db, cjoA.pIm)
    End Operator
    Public Shared Operator +(ByVal cjoA As Complex, ByVal cjoB As Complex) As Complex
        Dim prRe As Rational = cjoA.pRe + cjoB.pRe
        Dim prIm As Rational = cjoA.pIm + cjoB.pIm
        Return New Complex(prRe, prIm)
    End Operator
    Public Shared Operator -(ByVal cjoA As Complex) As Complex
        Return New Complex(-cjoA.pRe, -cjoA.pIm)
    End Operator
    Public Shared Operator -(ByVal cjoA As Complex, ByVal db As Double) As Complex
        Return cjoA - New Complex(db)
    End Operator
    Public Shared Operator -(ByVal db As Double, ByVal cjoA As Complex) As Complex
        Return New Complex(db) - cjoA
    End Operator
    Public Shared Operator -(ByVal cjoA As Complex, ByVal cjoB As Complex) As Complex
        Dim Re As Rational = cjoA.pRe - cjoB.pRe
        Dim Im As Rational = cjoA.pIm - cjoB.pIm
        Return New Complex(Re, Im)
    End Operator
    Public Shared Operator *(ByVal cjoA As Complex, ByVal db As Double) As Complex
        Return New Complex(cjoA.pRe * db, cjoA.pIm * db)
    End Operator
    Public Shared Operator *(ByVal db As Double, ByVal cjoB As Complex) As Complex
        Return New Complex(cjoB.pRe * db, cjoB.pIm * db)
    End Operator
    Public Shared Operator *(ByVal cjoA As Complex, ByVal cjoB As Complex) As Complex
        If cjoA.pIm.IsZero AndAlso cjoB.pIm.IsZero Then
            Return New Complex(cjoA.pRe * cjoB.pRe, New Rational(0.0))
        End If
        Dim CI As Globalization.CultureInfo = Nothing
        If G10.detail Then
            Dim s As String = ""
            Dim sReA As String = IIf(cjoA.pRe.IsZero, "", cjoA.pRe.ToStringRational(G10.nDec, G10.CI))
            Dim sImA As String = IIf(cjoA.pIm.IsZero, "", cjoA.pIm.ToStringRational(G10.nDec, G10.CI))
            Dim sReB As String = IIf(cjoB.pRe.IsZero, "", cjoB.pRe.ToStringRational(G10.nDec, G10.CI))
            Dim sImB As String = IIf(cjoB.pIm.IsZero, "", cjoB.pIm.ToStringRational(G10.nDec, G10.CI))
            s = " (" + cjoA.ToStringComplex(G10.nDec, G10.sImg, G10.CI) + ")*(" + cjoB.ToStringComplex(G10.nDec, G10.sImg, G10.CI) + ") " + vbCrLf
            If Len(sReA) AndAlso Len(sReB) Then
                s += sReA + "*" + sReB
            End If
            If Len(sImA) AndAlso Len(sImB) Then
                s += " - " + sImA + "*" + sImB
            End If
            Dim sImTerm1 As String = ""
            If Len(sReA) AndAlso Len(sImB) Then
                sImTerm1 = sReA + "*" + sImB
            End If
            Dim sImTerm2 As String = ""
            If Len(sImA) AndAlso Len(sReB) Then
                sImTerm2 = sImA + "*" + sReB
            End If
            If Len(sImTerm1) OrElse Len(sImTerm2) Then
                s += " + i*(" + sImTerm1 + "+" + sImTerm2 + ") "
            End If
            G10.sDetail = s + vbCrLf
        End If
        Dim prRe As Rational = cjoA.pRe * cjoB.pRe - cjoA.pIm * cjoB.pIm
        Dim prIm As Rational = cjoA.pRe * cjoB.pIm + cjoA.pIm * cjoB.pRe
        If G10.detail Then
            Dim s As String = ""
            s = prRe.ToStringRational(G10.nDec, G10.CI)
            If Not prIm.IsZero Then
                s += "+i*" + prIm.ToStringRational(G10.nDec, G10.CI)
            End If
            G10.sDetail += s
        End If
        Return New Complex(prRe, prIm)
    End Operator
    Public Shared Operator /(ByVal num As Double, ByVal cjoA As Complex) As Complex
        Return New Complex(num, 0.0) / cjoA
    End Operator
    Public Shared Operator /(ByVal cjoA As Complex, ByVal den As Double) As Complex
        Dim prD As New Rational(1.0 / den)
        Return cjoA * New Complex(prD, New Rational(0.0))
    End Operator
    Public Shared Operator /(ByVal cjoA As Complex, ByVal cjoB As Complex) As Complex
        If cjoB.IsZero Then
            Dim pRe As Rational = cjoA.pRe / cjoB.pRe
            Dim pZero As New Rational(0.0)
            Return New Complex(pRe, pZero)
            ' Throw New DivideByZeroException
        End If
        If cjoA.pIm.IsZero AndAlso cjoB.pIm.IsZero Then
            Dim pRe As Rational = cjoA.pRe / cjoB.pRe
            Dim pZero As New Rational(0.0)
            Return New Complex(pRe, pZero)
        End If
        '  x       a + b i      ac + bd        bc - ad
        ' ---   = ---------  = --------- + i* --------- 
        '  y       c + d i      c^2+d^2        c^2 +d^2
        Dim a As New Rational(cjoA.pRe)
        Dim b As New Rational(cjoA.pIm)
        Dim c As New Rational(cjoB.pRe)
        Dim d As New Rational(cjoB.pIm)
        Dim sDen As String = ""
        Dim sRe As String = ""
        Dim sIm As String = ""
        Dim sa As String = ""
        Dim sb As String = ""
        Dim sc As String = ""
        Dim sd As String = ""
        If G10.detail Then
            G10.sDetail = "(" + cjoA.ToStringComplex(G10.nDec, G10.sImg, G10.CI) +
                ")/(" + cjoB.ToStringComplex(G10.nDec, G10.sImg, G10.CI) + ")" + vbCrLf
            sa = a.ToStringRational(G10.nDec, G10.CI)
            sb = b.ToStringRational(G10.nDec, G10.CI)
            sc = c.ToStringRational(G10.nDec, G10.CI)
            sd = d.ToStringRational(G10.nDec, G10.CI)
            sDen = "(" + sc + "*" + sc + "+" + sd + "*" + sd + ")"
        End If
        Dim Den As Rational = c * c + d * d
        If Den.IsZero Then
            Throw New Exception("Division by zero.")
        End If
        If G10.detail Then '.detail Then
            sRe = "(" + sa + "*" + sc + "+" + sb + "*" + sd + ")"
            sRe += "/" + sDen
            sIm = "(" + sb + "*" + sc + "-(" + sa + "*" + sd + "))"
            sIm += "/" + sDen
            G10.sDetail += (sRe + "+i*" + sIm).Replace("+-", "-").Replace("-+", "-")
        End If
        Return New Complex((a * c + b * d) / Den,
                           (b * c - a * d) / Den)
    End Operator
    Public Function opAbs() As Complex
        Return New Complex((pRe * pRe + pIm * pIm) ^ New Rational(0.5), New Rational(0.0))
    End Function
    Public Function opModulo() As Double
        Dim pr As Rational = Nothing
        Try
            pr = pRe * pRe + pIm * pIm
            Return Math.Sqrt(pr.ToDouble)
        Catch ex As Exception
            Dim db As Double = Double.MaxValue / 2.0
            If pr IsNot Nothing Then
                db *= pr.Sign
            End If
            Return db
        End Try
    End Function
    Public Shared Operator ^(ByVal cjoA As Complex, ByVal cjoB As Complex) As Complex
        If cjoA.pIm.IsZero AndAlso cjoB.pIm.IsZero Then
            Dim rB As Double = cjoB.pRe.ToDouble
            Dim cA As New Complex(cjoA)
            If rB < 0 Then
                cA = 1 / cA
                rB = -rB
            End If
            If cA.IsZero Then
                Dim sgn As Int64 = Math.Sign(rB)
                If sgn = 1 Then
                    Return New Complex(0.0)
                ElseIf sgn = 0 Then
                    Return New Complex(1.0)
                Else
                    Throw New DivideByZeroException
                End If
            End If
            If cA.pRe.Sign > 0 Then
                Dim powAB As Rational = cA.pRe ^ rB
                Return New Complex(powAB, New Rational(0.0))
            ElseIf rB = 0.5 Then
                'if x>0, (-x)^0.5=sqr(-x)=sqr(x)*sqr(-1)=sqr(x)*i
                Dim powAB As Rational = New Rational(Math.Sqrt(-cA.pRe.ToDouble))
                Return New Complex(New Rational(0.0), powAB)
            ElseIf rB >= 1.0 Then
                If rB = Math.Floor(rB) AndAlso rB Mod 2 = 0 Then
                    Dim powAB As Rational = cA.pRe ^ rB
                    Return New Complex(powAB, New Rational(0.0))
                End If
            End If
        End If
        If cjoB.IsZero Then
            Return Complex.one
        ElseIf cjoA.IsZero Then
            Return Complex.zero
        End If
        If cjoA.opModulo = 1 AndAlso cjoB.IsDouble Then
            ' z = a+b*i ={forma polar} = (|z|)alpha = (|z|)atan(b/a) = (|z|)atan2(b,a)
            ' 
            Dim s As String = ""
            Dim alpha As Double = Math.Atan2(cjoA.pIm.ToDouble, cjoA.pRe.ToDouble)
            ' (Zalpha)^n = Z(n*alpha) si el módulo |z|=1:
            Dim n As Double = (cjoB.pRe.ToDouble * alpha) Mod (2 * Math.PI)
            If G10.detail Then
                s += "z=a+b*i= {forma polar} = (|z|)alpha = (|z|)atan(b/a) = (|z|)atan2(b,a)" + vbCrLf
                s += "(Zalpha)^n = Z(n*alpha) if |z|=1:" + vbCrLf
                s += "alpha=" + alpha.ToString(G10.CI) + vbCrLf
                s += "n = " + cjoB.pRe.ToDouble.ToString(G10.CI) + "*" + alpha.ToString(G10.CI) + " Mod 2 * Math.PI" + vbCrLf
                s += "  = " + n.ToString(G10.CI) + vbCrLf
            End If
            If Math.Abs(n) = Math.PI / 2 OrElse Math.Abs(n) = 3 * Math.PI / 2 Then
                Return New Complex(0.0, Math.Sin(n))
            ElseIf Math.Abs(n) = Math.PI Then
                Return New Complex(Math.Cos(n), 0.0)
            End If
            Return New Complex(Math.Cos(n), Math.Sin(n))
        End If
        Dim expB As Double
        If cjoB.pIm.IsZero Then
            expB = cjoB.pRe.ToDouble
            If expB = Math.Floor(expB) Then ' AndAlso expB <= 64 Then
                Dim exp As Int64 = Math.Abs(expB)
                Try
                    Dim cjoC As Complex = Nothing
                    If exp > 0 Then
                        Dim log2 As Int64 = Math.Floor(Math.Log10(exp) / Math.Log10(2.0))
                        Dim vNum(log2) As Complex
                        Dim vExp(log2) As Int64
                        Dim curExp As Int64 = 2
                        vNum(0) = New Complex(cjoA)
                        vExp(0) = 1
                        For i As Int64 = 1 To log2
                            vNum(i) = vNum(i - 1) * vNum(i - 1)
                            vExp(i) = curExp
                            curExp *= 2
                        Next
                        curExp = vExp(log2)
                        cjoC = vNum(log2)
                        For i As Int64 = log2 - 1 To 0 Step -1
                            If curExp + vExp(i) <= exp Then
                                cjoC *= vNum(i)
                                curExp += vExp(i)
                                If curExp = exp Then
                                    Exit For
                                End If
                            End If
                        Next
                        If expB < 0 Then
                            cjoC = Complex.one / cjoC
                        End If
                        Return cjoC
                    End If
                Catch ex As Exception
                    Throw
                End Try
            End If
        End If



        ' if a= (a1,a2) = (r,alfa) (r=modulus, alfa=argument=atan2(a2,a1))
        ' and b=(c,d) then
        ' a^b = r^c * exp(-d*alfa) * [ cos(d*log(r) + c * alfa + i * sin(d*log(r)+c*alfa)
        Dim c As Double = cjoB.pRe.ToDouble
        Dim d As Double = cjoB.pIm.ToDouble
        Dim r As Double = cjoA.opModulo
        If r = 0 Then
            Return Complex.zero
        End If
        Dim alfa As Double = Math.Atan2(cjoA.pIm.ToDouble, cjoA.pRe.ToDouble)
        Dim rcTimesExpdalfa As Double = Math.Pow(r, c) * Math.Exp(-d * alfa)
        If G10.detail Then
            Dim s As String = ""
            s += "if a= (a1,a2) = (r,alfa) (r=modulus, alfa=argument=atan2(a2,a1))" + vbCrLf
            s += " and b=(c,d) then" + vbCrLf
            s += " a^b = r^c * exp(-d*alfa) * [ cos(d*log(r) + c * alfa + i * sin(d*log(r)+c*alfa)" + vbCrLf
            s += " r = " + r.ToString(G10.CI) + vbCrLf
            s += " c = " + c.ToString(G10.CI) + vbCrLf
            s += " d = " + d.ToString(G10.CI) + vbCrLf
            s += " alpha = " + alfa.ToString(G10.CI) + vbCrLf
            s += " [1] r^c * exp(-d*alfa) = " + rcTimesExpdalfa.ToString(G10.CI) + vbCrLf
            s += " w = d*log(r) + c * alfa =" + (d * Math.Log(r) + c * alfa).ToString(G10.CI) + vbCrLf
            s += " a^b = [1] * (cos(w) + i*sin(w)"
            G10.sDetail = s
        End If
        If alfa = Math.PI Then
            Return New Complex(-rcTimesExpdalfa, 0.0)
        End If
        Dim dlogrPluscalfa As Double = d * Math.Log(r) + c * alfa
        Return New Complex(rcTimesExpdalfa * Math.Cos(dlogrPluscalfa),
                             rcTimesExpdalfa * Math.Sin(dlogrPluscalfa))
    End Operator
    Public Shared Function opCos(ByVal x As Complex) As Complex
        If x.pIm.IsZero Then
            Dim re As Double = x.pRe.ToDouble Mod (2 * Math.PI)
            Dim re2 As Double = re / (Math.PI / 2.0)
            If re2 = Math.Floor(re2) AndAlso re2 Mod 2 Then
                Return New Complex(0.0)
            End If
            Return New Complex(Math.Cos(re))
        End If
        '           e^(ix)+e^(-ix)
        ' cos(x) = ---------------
        '                2 
        ''
        Dim ix As Complex = New Complex(-x.pIm, x.pRe)
        Dim minusix As New Complex(x.pIm, -x.pRe)
        Dim e As New Complex(Math.Exp(1), 0.0)
        Dim eix As Complex = e ^ ix
        Dim eminusix As Complex = e ^ minusix
        Dim num As Complex = eix + eminusix
        Dim ret As New Complex(num.pRe / 2.0, num.pIm / 2.0)
        Return ret
    End Function
    Public Shared Function opArg(ByVal x As Complex) As Complex
        Return New Complex(Math.Atan2(x.pIm.ToDouble, x.pRe.ToDouble), 0.0)
    End Function
    Public Shared Function opExp(ByVal x As Complex) As Complex
        If x.pIm.IsZero Then
            If x.pRe.IsZero Then
                Return New Complex(1.0)
            End If
            Return New Complex(Math.E ^ x.pRe.ToDouble, 0.0)
        End If
        Return New Complex(Math.E, 0.0) ^ x
    End Function
    Public Shared Function opLn(ByVal x As Complex) As Complex
        If x.pIm.IsZero AndAlso x.pRe.Sign = 1 Then
            Return New Complex(Math.Log(x.pRe.ToDouble), 0.0)
        End If

        ' ln z = ln |z| + i*arg(z)
        Dim arg As Double = Math.Atan2(x.pIm.ToDouble, x.pRe.ToDouble)
        Dim re As Double = Math.Log(x.opModulo)
        Return New Complex(re, arg)

    End Function
    Public Shared Function opACsc(ByVal z As Complex) As Complex
        '           
        ' acsc(z) = asin(1.0/z)
        '           
        '
        Dim arg As Complex = one / z
        Return opASin(z)
    End Function
    Public Shared Function opACscH(ByVal z As Complex) As Complex
        '           
        ' acscH(z) = ln(1/z + sqr(1+1/x^2) )
        '           
        '
        Dim arg As Complex = one / z + (one + one / (z * z)) ^ oneHalf
        Return opLn(arg)
    End Function
    Public Shared Function opACos(ByVal z As Complex) As Complex
        '           
        ' acos(z) = -i * log[z + i*(1.0 - z*z))^0.5]
        '           
        '
        Dim arg As Complex = z + i * (one - z * z) ^ oneHalf
        Return -Complex.i * opLn(arg)
    End Function
    Public Shared Function opACosH(ByVal z As Complex) As Complex
        '           
        ' acosh(z) = ln(z + sqrt(z * z - 1.0))
        '           
        '
        Dim arg As Complex = z + (z * z - one) ^ oneHalf
        Return opLn(arg)
    End Function
    Public Shared Function opACot(ByVal z As Complex) As Complex
        '           
        ' acot(z) = atan(1.0 / z)
        '           
        '
        Return opAtan(one / z)
    End Function
    Public Shared Function opACotH(ByVal z As Complex) As Complex
        '           
        ' acoth(z) = atanh(1.0 / z)
        '           
        '
        Return opAtanH(one / z)
    End Function
    Public Shared Function opASec(ByVal z As Complex) As Complex
        '           
        ' asec(z) = asin(sqr(x^2-1)/x)
        '           
        '
        Return opASin((z * z - one) ^ oneHalf / z)
    End Function
    Public Shared Function opASecH(ByVal z As Complex) As Complex
        '           
        ' asech(z) = acosh(1.0 / z)
        '           
        '
        Return opACosH(one / z)
    End Function
    Public Shared Function opASin(ByVal z As Complex) As Complex
        '
        '
        '
        Dim r As Complex = Nothing
        If z.IsDouble AndAlso Math.Abs(z.pRe.ToDouble) < 1.0 Then
            '  asin(z) = -i*ln(i*z +(|1-z^2|)^0.5*e^((i/2)*arg(1-z^2)))
            ' if |z|<1, z real => arg(1-z^2)=0 =>
            ' => asin(z)=0.5*ln(i*z+(|1-z^2|)^0.5)
            r = -i * opLn(i * z + (1 - z * z) ^ oneHalf)
            r.pIm = New Rational(0.0)
        Else
            Dim z1 As Complex = one - z * z
            Dim absZ1 As Double = Math.Abs(z1.pRe.ToDouble)
            r = -i * opLn(i * z + z1 ^ oneHalf)
        End If
        Return r
    End Function

    Public Shared Function opASinH(ByVal z As Complex) As Complex
        '           
        ' asinh(z) = log(z +(z*z + 1.0)^0.5)
        '           
        '
        Dim arg As Complex = z + (z * z + one) ^ oneHalf
        Return opLn(arg)
    End Function

    Public Shared Function opAtan(ByVal z As Complex) As Complex
        '           
        ' atan(z)= 1/(2i) * ln ((1 + iz)/(1 - iz)) ' 2014/03/06 http://mathforum.org/library/drmath/view/72732.html
        '
        If z.IsDouble Then
            Return New Complex(Math.Atan2(z.pRe.Num, z.pRe.Den), 0.0) ' 2017/02/24
        End If
        Return opLn((i - z) / (i + z)) * i * -0.5
    End Function
    Public Shared Function opAtanH(ByVal z As Complex) As Complex
        '           
        ' atanh(z) = log((1.0 + z) / (1.0 - z)) /2  2014/03/06
        '           
        '
        Return opLn((one + z) / (one - z)) * 0.5
    End Function
    Public Shared Function opCosH(ByVal x As Complex) As Complex
        '           e^(x)+e^(-x)
        ' cos(x) = ---------------
        '               2
        '
        Dim ex As Complex = New Complex(Math.E) ^ x
        Dim eminusx As Complex = New Complex(Math.E) ^ -x
        Dim num As Complex = ex + eminusx
        Dim ret As New Complex(num.pRe / 2.0, num.pIm / 2.0)
        Return ret
    End Function
    Public Shared Function opCot(ByVal z As Complex) As Complex
        ' 
        ' cot(z) = cos(z) / sin(z)
        ' 
        '
        Return opCos(z) / opSin(z)
    End Function
    Public Shared Function opCotH(ByVal z As Complex) As Complex
        ' 
        ' coth(z) = cosh(z) / sinh(z)
        ' 
        '
        Return opCosH(z) / opSinH(z)
    End Function
    Public Shared Function opCsc(ByVal z As Complex) As Complex
        ' 
        ' csc(z) = 1.0 / sin(z)
        ' 
        '
        Return one / opSin(z)
    End Function
    Public Shared Function opCscH(ByVal z As Complex) As Complex
        ' 
        ' csch(z) = 1.0 / sinh(z)
        ' 
        '
        Return one / opSinH(z)
    End Function
    Public Shared Function opSec(ByVal z As Complex) As Complex
        ' 
        ' csc(z) = 1.0 / cos(z)
        ' 
        '
        Return one / opCos(z)
    End Function
    Public Shared Function opSecH(ByVal z As Complex) As Complex
        ' 
        ' sech(z) = 1.0 / cosh(z)
        ' 
        '
        Return one / opCosH(z)
    End Function
    Public Shared Function opSin(ByVal x As Complex) As Complex
        If x.pIm.IsZero Then
            If Math.IEEERemainder(x.pRe.ToDouble, Math.PI) = 0 Then
                Return New Complex(0.0)
            End If
            ' Needed because if arg>> then math.sin(arg) fails:
            Dim re As Double = x.pRe.ToDouble Mod (2 * Math.PI)
            Return New Complex(Math.Sin(re))
        End If
        '           e^(ix)-e^(-ix)          e^(ix)-e^(-ix)
        ' sin(x) = --------------- = - i * ----------------
        '               2 * i                     2
        ''
        Dim ix As Complex = New Complex(-x.pIm, x.pRe)
        Dim minusix As New Complex(x.pIm, -x.pRe)
        Dim e As New Complex(Math.Exp(1), 0.0)
        Dim eix As Complex = e ^ ix
        Dim eminusix As Complex = e ^ minusix
        Dim num As Complex = eix - eminusix
        Dim ret As New Complex(num.pRe / 2.0, num.pIm / 2.0)
        Dim retTimesMinusI As New Complex(ret.pIm, -ret.pRe)
        Return retTimesMinusI
    End Function
    Public Shared Function opSinH(ByVal x As Complex) As Complex
        '           e^(z)-e^(-z)
        ' sinh(z) = ---------------
        '               2
        '
        Dim minusx As New Complex(x)
        minusx.opChgSgn()
        Dim e As New Complex(Math.Exp(1), 0)
        Dim eix As Complex = opExp(x)
        Dim eminusx As Complex = opExp(minusx)
        Dim num As Complex = eix - eminusx
        Dim ret As New Complex(num.pRe / 2.0, num.pIm / 2.0)
        Return ret
    End Function
    Public Shared Function optan(ByVal z As Complex) As Complex
        ' 
        ' tan(z) = sin(z) / cos(z)
        ' 
        '
        Return opSin(z) / opCos(z)
    End Function
    Public Shared Function optanh(ByVal z As Complex) As Complex
        ' 
        ' tanh(z) = sinh(z) / cosh(z)
        ' 
        '
        Return opSinH(z) / opCosH(z)
    End Function

    Public Function opNorm() As Double
        Return (pRe * pRe + pIm * pIm).ToDouble
    End Function
    Public Shared Function opConjugate(ByVal x As Complex) As Complex
        Return New Complex(x.pRe, -x.pIm)
    End Function
    Public Shared Function opConjugateAndSwapReIm(ByVal x As Complex) As Complex
        Return New Complex(-x.pIm, x.pRe)
    End Function
    Public Shared Function opIgual(ByVal x As Complex, ByVal y As Complex) As Boolean
        Return ((x.pRe - y.pRe).IsZero AndAlso (x.pIm - y.pIm).IsZero)
    End Function
    Public Sub opChgSgn()
        pRe.OpChgSgn()
        pIm.OpChgSgn()
    End Sub
    Public ReadOnly Property IsDouble() As Boolean
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
    Public Overrides Function toString() As String
        Return ToStringComplex(G10.nDec, G10.sImg, G10.CI, G10.currBase)
    End Function
    Public Function ToStringComplex(Optional numDecimals As Int32 = 15,
                                    Optional sImg As String = "i",
                                    Optional cultureInfo As Globalization.CultureInfo = Nothing,
                                    Optional currBase As Rational.Base = Rational.Base.Decimal) _
                                    As String


        If cultureInfo Is Nothing Then cultureInfo = New Globalization.CultureInfo("en-US")
        Dim sRe As String = ""
        'If G10.currBase <> Rational.Base.Decimal Then
        'sRe = Rational.toStringBase(pRe.ToDouble, numDecimals, cultureInfo, G10.currBase)
        'Else
        sRe = pRe.toString
        'End If
        Dim sIm As String = ""
        'If G10.currBase <> Rational.Base.Decimal Then
        'sIm = Rational.toStringBase(pIm.ToDouble, numDecimals, cultureInfo, G10.currBase)
        'Else
        sIm = pIm.toString
        'End If
        sIm = Replace(sIm, " ", "")
        Dim s As String = sRe + "+" + G10.sImg + "*" + sIm
        If sRe = "0" AndAlso sIm = "0" Then
            s = "0"
        ElseIf sRe = "0" Then
            If sIm = "1" Then
                s = G10.sImg  ' "i"
            ElseIf sIm = "-1" Then
                s = "-" + G10.sImg
            ElseIf sIm.Chars(0) = "-" Then
                s = "-" + G10.sImg + "*" + Mid(sIm, 2)
            Else
                s = G10.sImg + "*" + sIm
            End If
        Else
            Dim s1 As String = Regex.Replace(sIm, "(&h|&o|&b)", "")
            If s1 = "0" Then
                s = sRe
            End If
        End If

            s = s.Replace("+-", "-")
        Return s
    End Function
    Public Shared Function ToStringV(ByVal vCjo() As Complex) As String
        Dim e1 As String = ""
        Try
            Dim i As Int64
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
    Public Overloads Shared Function ToString(ByVal vCjo() As Complex) As String
        Dim e1 As String = "("
        Try
            Dim i As Int64
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
    Public Overloads Shared Function ToString(ByVal mtxCjo()() As Complex) As String
        Dim e1 As String = ""
        Try
            Dim i, j As Int64
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
    Public Overloads Shared Function ToString(ByVal mtxCjo(,) As Complex) As String
        Dim e1 As String = ""
        Try
            Dim i, j As Int64
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
    Public Const rad2Degrees As Double = 180 / Math.PI
    Public Const rad2Centesimal As Double = 200 / Math.PI
    Public Const degree2Radians As Double = Math.PI / 180
    Public Const centesimal2Rad As Double = Math.PI / 200

    Public Shared Function EvalFn(
                    ByVal fn As String,
                    ByVal cjoA As Complex) As Complex
        Static vTrigonFns() As String =
            New String() {
            "sin", "cos", "tan",
            "sinh", "cosh", "tanh",
            "csc", "sec", "cot",
            "csch", "sech", "coth"
            }
        Dim rCjo As Complex = Nothing
        Try
            fn = LCase(fn)
            Dim bArgNotValid As Boolean = False
            If G10.angleMode = G10.AngleModes.degree Then
                cjoA *= New Complex(degree2Radians)
            ElseIf g10.angleMode = G10.AngleModes.gradian Then
                cjoA *= New Complex(centesimal2Rad)
            End If
repiteFn:
            Select Case Len(fn)
                Case 6
                    Select Case fn
                        Case "logtwo" : rCjo = Complex.opLn(cjoA) / Math.Log(2.0)
                        Case "logten" : rCjo = Complex.opLn(cjoA) / Math.Log(10.0)
                    End Select
                Case 5
                    Select Case fn
                        Case "acosh" : rCjo = Complex.opACosH(cjoA)
                        Case "acoth" : rCjo = Complex.opACotH(cjoA)
                        Case "acsch" : rCjo = Complex.opACscH(cjoA)
                        Case "asech" : rCjo = Complex.opASecH(cjoA)
                        Case "asinh" : rCjo = Complex.opASinH(cjoA)
                        Case "atanh" : rCjo = Complex.opAtanH(cjoA)
                        Case "floor" : rCjo = New Complex(Math.Floor(cjoA.pRe.ToDouble))
                            If cjoA.pIm.IsZero = False Then
                                bArgNotValid = True
                            End If
                        Case "round" : rCjo = New Complex(Math.Round(cjoA.pRe.ToDouble))
                            If cjoA.pIm.IsZero = False Then
                                bArgNotValid = True
                            End If
                    End Select
                Case 4
                    Select Case fn
                        Case "coth" : rCjo = Complex.opCotH(cjoA)
                        Case "csch" : rCjo = Complex.opCscH(cjoA)
                        Case "sech" : rCjo = Complex.opSecH(cjoA)
                        Case "acos" : rCjo = Complex.opACos(cjoA)
                        Case "acot" : rCjo = Complex.opAtan(Complex.one / cjoA)
                        Case "acsc" : rCjo = Complex.opASin(Complex.one / cjoA)
                        Case "asec" : rCjo = Complex.opASec(cjoA)
                        Case "asin" : rCjo = Complex.opASin(cjoA)
                        Case "atan" : rCjo = Complex.opAtan(cjoA)
                        Case "conj" : rCjo = Complex.opConjugate(cjoA)
                        Case "cosh" : rCjo = Complex.opCosH(cjoA)
                        Case "norm" : rCjo = New Complex(cjoA.opNorm, 0.0)
                        Case "sign" : rCjo = New Complex(Math.Sign(cjoA.pRe.ToDouble))
                            If cjoA.pIm.IsZero = False Then
                                bArgNotValid = True
                            End If
                        Case "sinh" : rCjo = Complex.opSinH(cjoA)
                        Case "sqrt" : rCjo = cjoA ^ Complex.oneHalf  ' 2013/08/09
                        Case "tanh" : rCjo = Complex.optanh(cjoA)
                    End Select
                Case 3
                    Select Case fn
                        Case "abs" : rCjo = cjoA.opAbs
                        Case "arg" : rCjo = Complex.opArg(cjoA)
                        Case "cos" : rCjo = Complex.opCos(cjoA)
                        Case "cot" : rCjo = Complex.opCot(cjoA)
                        Case "csc" : rCjo = Complex.opCsc(cjoA) ' csc=1/sin
                        Case "exp" : rCjo = Complex.opExp(cjoA)
                        Case "log" : rCjo = Complex.opLn(cjoA)' log()= ln()
                        Case "mod" : rCjo = New Complex(cjoA.opModulo, 0.0)
                        Case "sec" : rCjo = Complex.opSec(cjoA) ' sec
                        Case "sin" : rCjo = Complex.opSin(cjoA)
                        Case "sqr" : rCjo = cjoA ^ Complex.oneHalf
                        Case "tan" : rCjo = Complex.optan(cjoA)
                    End Select
                Case 2
                    Select Case fn
                        Case "ln" : rCjo = Complex.opLn(cjoA) ' ln()= log()
                        Case "im" : rCjo = New Complex(New Rational(0.0), cjoA.pIm)
                        Case "re" : rCjo = New Complex(cjoA.pRe, New Rational(0.0))
                    End Select
            End Select
            If bArgNotValid Then
                Throw New ArgumentOutOfRangeException
            End If
        Catch ex As Exception
            Throw
        End Try
        Return rCjo
    End Function
    Private Function replaceByMathML(m As Match) As String
        Static IsMinus As Boolean = False
        Dim s As String = IIf(m.Value = G10.sImg, "+", "")
        Dim val As String = m.Value
        If m.Value.Length > 1 AndAlso m.Value.Chars(0) = "-" Then
            val = Mid(val, 2)
            s = "<mo>-</mo>"
        End If
        If IsMinus Then
            s = "<mo>-</mo>"
            IsMinus = False
        End If
        If m.Groups("num").Success Then
            Return s + "<mn>" + val + "</mn>"
        ElseIf m.Groups("op").Success Then
            If val = "-" Then
                IsMinus = True
            ElseIf val = "*" Then
                Return "<mo>" + m.Value + "</mo>"
            ElseIf val = "÷" OrElse val = "/" Then
                Return "<mo>" + m.Value + "</mo>"
            ElseIf val = "^" Then
                Return "<mo>" + m.Value + "</mo>"
            ElseIf val = "%" Then
                Return "<mo>" + m.Value + "</mo>"
            End If
        ElseIf InStr("ij", val) Then
            Return s + "<mi>" + val + "</mi>"
        Else
            Return s + "<mi>" + val + "</mi>"
        End If
        Return ""
    End Function
End Class
