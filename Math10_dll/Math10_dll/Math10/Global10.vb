Imports System.IO
Imports System.Text.RegularExpressions
'Imports System.Windows

Public Class G10
    Public Shared ReadOnly vSubstitute() As String = {"\[", "\]", "\{", "\}",
                             ":", "–", ChrW(&HBC), ChrW(&HBD), ChrW(&HBE), ChrW(&HB2), ChrW(&HB3),
        ChrW(&H2212), ChrW(&H3C0), "〖", "〗", "⅞", "",
                            "П", "⁹", "⁸", "⁷", "⁶", "⁵", "⁴", "³", "²", "¹", "°", "·",
         "−", "×", "arcsin", "arccos", "arctan", "\*\*",
                            "arccot", "arccsc", "arcsec", "√",
                            "@[^=]+=[^@]+", "infinity", "==",
                             "integral", "integrate", "%"
                            }
    Public Shared ReadOnly vSubstituteBy() As String = {"(", ")", "{", "}",
                             "÷", "-", "+(1/4)", "+(1/2)", "+(3/4)", "^2", "^3",
         "-", "PI", "(", ")", "+(7/8)", "-",
                            "(PI)", "^9", "^8", "^7", "^6", "^5", "^4", "^3", "^2", "^1", "^0", "*",
         "-", "*", "asin", "acos", "atan", "^",
                            "acot", "acsc", "asec", "sqr",
                            "", "∞", "=",
                             "∫", "∫", "mod"
                            }
    Public Shared ReadOnly sImFn As String =
    "logtwo|logten|acosh|acoth|acsch|asech|asinh|atanh|floor|round|norm|conj|coth|csch|sech|acos|acot|acsc|asec|asin|atan|cosh|sign|sinh|sqrt|tanh|abs|cos|cot|csc|exp|log|sec|sin|sqr|tan|ln|re|im|%"
    Public Shared ReadOnly vImFn() As String = Split(sImFn, "|")
    Public Shared ReadOnly vMtxFn() As String =
                             {"identity", "echelon", "cof", "cross", "dot",
                      "eigenvalues", "egvl", "eigenvectors", "egv",
                      "transpose", "trn", "adj", "factorize", "factors",
                      "trace", "rank", "lim", "roots", "gcd", "lcm",
                      "det", "jordan", "inverse", "mod",
                      "jacobian", "partialfractions",
                      "lagrangianinterpolation", "orthog",
                      "max", "min"}
    Public Shared vSpecialChars() As String = Nothing
    Public Shared sOp, sMtxFn, sMtxOp, sVar, sModes As String
    Public Shared sDerivative As String = "Derivative|(?-i)D"
    Public Shared sDiferential As String = "d(?<difResp>[a-z])"
    Private Shared _CI As New Globalization.CultureInfo("en-US")
    Public Shared nDec As Int32 = 15
    Public Shared mMod As Int32 = 0
    Public Shared sImg As String = "i"
    Public Shared frac As Boolean
    Public Shared eng As Boolean = True
    Public Shared var As Boolean = False ' &var1 (var=true) => variables can contain numbers, &var0 (var=false) => no numbers
    Public Shared _currBase As Rational.Base = Rational.Base.Decimal
    Shared sNum As String
    Shared sLOp As String
    Public Shared reTot, reMtxTot, reSubstitute As Regex
    Public Shared sMtxTot As String
    Public Shared evaluator As String
    Public Shared angleMode As AngleModes = AngleModes.radians
    Public Shared detail As Boolean
    Public Shared sDetail As String
    Public Shared mathml As Boolean
    Public Shared columnSeparator As String = "[;\t]"
    Public Shared rowSeparator As String = "\|"
    Public Shared RowOrColSeparator As String = "(?<sep>(?<col>" + columnSeparator + ")|(?<row>" + rowSeparator + "))"
    Public Shared bCancel As Boolean = False
    Public Shared bTryIrrationals As Boolean = True
    Public Shared phi As Double = 1.6180339887498949
    Public Shared namedIrrationals As New Dictionary(Of Double, String)
    Public Shared Irrationals2 As New Dictionary(Of Double, String)
    Public Shared Irrationals3 As New Dictionary(Of Double, String)
    Enum AngleModes
        radians
        degree
        gradian ' gon, grad, grade or centesimal
    End Enum
#Region "Initializations"
    Public Shared Property CI As Globalization.CultureInfo
        Get
            Return _CI
        End Get
        Set(value As Globalization.CultureInfo)
            _CI = value
            Initialize()
        End Set
    End Property
    Public Shared Property currBase As Rational.Base
        Get
            Return _currBase
        End Get
        Set(value As Rational.Base)
            _currBase = value
        End Set
    End Property
    Public Shared Sub Initialize()
        If vSpecialChars Is Nothing Then
            getSpecialChars()
        End If
        Dim _signoDecimal As String = CI.NumberFormat.NumberDecimalSeparator ' signo decimal: ¿ una coma ?
        Dim _separadorMiles As String = CI.NumberFormat.NumberGroupSeparator ' separador de miles: ¿un punto?
        Dim s2 As String = _separadorMiles
        Dim sD As String = _signoDecimal
        If AscW(s2) = 160 OrElse AscW(s2) = 8239 Then
            s2 = "( |" + s2 + ")"
        ElseIf s2 = "." OrElse s2 = "/" Then
            s2 = "\" + s2
        End If
        If sD = "." OrElse s2 = "/" Then
            sD = "\."
        End If

        '[\d]{1}[\.\dA-F]*
        Dim sBase As String = "(?<base>(?s)&[hobd](?-s))"
        If G10.currBase <> Rational.Base.Hexadecimal Then
            sNum = "(?<num>((\d{1,3}((" + s2 + "\d{3})*(?!\d))(" + sD + "[0-9]+)?)|[\d]{1}[" + sD + "\d]*)([eE](\s*)[-+]?[0-9]+)?)"
        Else
            sNum = "(?i)(?<num>\d[.0-9b-f]*|a+(?!nd))(?-i)"
        End If
        sOp = "(?<op>[-+*/÷\^=])"
        sMtxOp = "(?<op>[-+*/÷\^]\s*\|)"
        Dim sPost As String = "(?![a-zA-Z_]+)"
        sModes = "(?i)(?<mode>(&dec|&mod)\s*\d+|&mathml0|&mathml1|&var0|&var1|&irtnl0|&irtnl1|&rad|&deg|&grad|&detail0|&detail1|&fra0|&fra1|&[hobdij])(?-i)"
        sLOp = "(?i)(?<lop>nand|mod|and|nor|xor|not|or|!)(?-i)" + sPost
        Dim sFn As String = "(?i)(?<fn>" + sImFn + "(?-i))" + sPost
        sMtxFn = "(?i)(?<mtxFn>∫|(" + Join(vMtxFn, "|") + "(?-i))" + sPost + ")"
        Dim sCn As String = "(?<cnt>e|(?i)pi|(?<phi>phi|ϕ|φ|Φ)(?-i))" + sPost
        Dim sLP As String = "\("
        Dim sRP As String = "\)"
        Dim sDer As String = "(?<der>" + sDerivative + "(?<resp>[_a-zA-Z]\w*)?)"
        Dim sDif As String = "(?<dif>(?-i)" + sDiferential + ")"
        Dim sc As String = Regex.Replace(Join(vSpecialChars, ""), "÷|∫|∞", "")
        If var Then
            sVar = "(?!0)(?<vars>∞|(?<chars>[_a-zA-Z" + sc + "])([_a-zA-Z" + sc + "\d]*))"
        Else
            sVar = "(?!0)(?<vars>∞|(?<chars>_?[a-zA-Z" + sc + "]))"
        End If
        Dim sEnd As String = "(?<end>\e)+"
        Dim sOther As String = "(?<other>[\x00-\x08\x0A-\x0C\x0E-\x1A\x1C-\x1F\x7E])+"
        Dim vStr() As String = {sModes, sNum, sOp, sLOp, sFn, sCn, sDer, sDif, sLP, sRP, sVar, sEnd, ",", sOther}
        evaluator = Join(vStr, "|")
        reTot = New Regex(evaluator, RegexOptions.IgnorePatternWhitespace)
        sMtxTot = sMtxFn + "|" + evaluator + "|\(|\)|" + RowOrColSeparator
        reSubstitute = New Regex("(" + Join(G10.vSubstitute, "|") + ")" + sPost, RegexOptions.IgnoreCase)
        If namedIrrationals.Count = 0 Then
            namedIrrationals.Add(Math.E, "e")
            namedIrrationals.Add(Math.PI.ToString, "π")
            namedIrrationals.Add(phi, "ϕ")
            namedIrrationals.Add(Math.E ^ 2, "e^2")
            namedIrrationals.Add(Math.PI ^ 2, "^2")
            namedIrrationals.Add(phi ^ 2, "ϕ^2")
            namedIrrationals.Add(Math.Sqrt(Math.E), "√e")
            namedIrrationals.Add(Math.Sqrt(Math.PI), "√pi")
            namedIrrationals.Add(Math.Sqrt(phi), "√ϕ")
            For i = 0 To Rational.vPrime.Length - 1
                Irrationals2.Add(Math.Sqrt(Rational.vPrime(i)), "√" + Rational.vPrime(i).ToString)
                Irrationals3.Add((Rational.vPrime(i)) ^ 0.33333333333333331, "∛" + Rational.vPrime(i).ToString)
            Next
        End If
    End Sub
    Shared Sub getSpecialChars()
        'Dim info As New FileStream(IO.Directory.GetCurrentDirectory + "\SpecialChars.txt", FileMode.Open, FileAccess.Read)
        'Dim sr As New StreamReader(info)
        'Dim vSpecialChars() As String = Split(sr.ReadToEnd(), vbCrLf)
        'SR.Close()
        Dim s As String = My.Resources.String1
        If InStr(s, "∞") Then
            s = Mid(s, InStr(s, "∞") + 1)
        End If
        Dim vSpecialChars() As String = Split(s, vbCrLf)
        Dim lst As New List(Of String)
        G10.vSpecialChars = vSpecialChars
    End Sub
#End Region
End Class
Public Class Msg10
    Public Shared ReadOnly Property Num(ByVal i As Int64) As String
        Get
            Select Case i
                Case 5 : Return "Found error around: {0}"
                Case 100 : Return "Expression is empty."
                Case 101 : Return "Missing a matching {0}."
                Case 102 : Return "n/a: missing an operator "
                Case 103 : Return "n/a: not all rows have the same number of columns."
                Case 104 : Return "Missing variable {0} value."
                Case 105 : Return "n/a: There is more than one variable: '{0}'"
                Case 106 : Return "n/a: Could not extract vector matrix from expression matrix."
                Case 107 : Return "Vector or matrix sizes do not match."
                Case 108 : Return "Error at expression vector."
                Case 109 : Return "Error at expression matrix."
                Case 110 : Return "n/a: Can't invert the matrix."
                Case 111 : Return "n/a: Not a deteminate system of equations."
                Case 112 : Return "n/a: Expected an integer value."
                Case 113 : Return "n/a: Error in function '{0}'."
                Case 114 : Return "n/a: User function '{0}' parameters {1} do not match."
                Case 115 : Return "Limit does not exist"
                Case 116 : Return "Cancelled"
                Case 117 : Return "Divisor is not polynomial."
                Case 118 : Return "Parenthesis not matching."
                Case 119 : Return "<br />Found a coma (,) and no function."
                Case Else : Return "Unknown error."
            End Select
            Return ""
        End Get
    End Property
End Class
