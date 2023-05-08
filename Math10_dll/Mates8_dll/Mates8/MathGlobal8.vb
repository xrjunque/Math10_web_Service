﻿Imports System.Reflection
Imports System.IO
Imports System.Text.RegularExpressions
Imports System.Runtime.Serialization


<Serializable()> _
Public Class MathGlobal8
    Public Enum FnType
        num = 1
        complex = 2
        trig = 4
        polyn = 8
        vector = 16
        matrix = 32
        other = 64
    End Enum
    Public Shared us As New Globalization.CultureInfo("en-US")
    Public Shared vPrime() As Int64 = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997}
    Public Shared vSubstitute() As String = {"³√", "(+", ":", "÷", "–", Chr(&HBC), Chr(&HBD), Chr(&HBE), Chr(&HB2), Chr(&HB3),
     ChrW(&H2212), ChrW(&H3C0), "〖", "〗", "⅞", "**", "",
                        "П", "⁹", "⁸", "⁷", "⁶", "⁵", "⁴", "³", "²", "¹", "°", "·",
                        "−", "×", "arcsin", "arccos", "arctan",
                        "arccot", "arccsc", "arcsec", "limit",
                        "infinite", "infinity", "infinit",
                        "√", "sqrt"
                        }
    Public Shared vSubstituteBy() As String = {"", "(", "/", "/", "-", "(1/4)", "(1/2)", "(3/4)", "^2", "^3",
     "-", "PI", "(", ")", "(7/8)", "^", "-",
                        "(PI)", "^9", "^8", "^7", "^6", "^5", "^4", "^3", "^2", "^1", "^0", "*",
                        "-", "*", "asin", "acos", "atan",
                        "acot", "acsc", "asec", "lim",
                        "∞", "∞", "∞",
                        "sqr", "sqr"
                        }
    ' ³ ² ¼ ½ ¾
    ' ³ ² ¼ ½ ¾
    Public Shared sSubstitute As String
    Public Shared reSubstitute As Regex

    Public Shared vFn() As String = {
        "identity", "echelon", "cof", "eigenvalues", "eigenvectors",
        "egvl", "egv",
        "transpose", "trn", "adj", "trace",
        "roots", "rank",
        "det", "factor", "jacobian", "lagrangianinterpolation", "orthog", "jordan",
        "floor", "logten", "round", "isolate", "eulers", "residue",
        "acosh", "acoth", "acsch", "asech", "asinh", "atanh",
        "coth", "csch", "sech",
        "acos", "acot", "acsc", "asec", "asin", "atan",
        "conj", "cosh", "logtwo", "norm", "sign", "sinh", "tanh",
        "abs", "arg", "cos", "cot", "csc",
        "exp", "log", "mod", "sec", "sin", "sqr", "tan",
        "gcd", "lim", "lcm", "cross", "dot",
        "im", "ln", "re", "diff", "poly",
        "max", "min"}
    Public Shared vFnType() As FnType = {
       FnType.matrix, FnType.matrix, FnType.matrix, FnType.matrix, FnType.matrix,
       FnType.matrix, FnType.matrix,
       FnType.matrix, FnType.matrix, FnType.matrix, FnType.matrix,
      FnType.polyn, FnType.matrix,
        FnType.matrix, FnType.polyn, FnType.matrix, FnType.polyn, FnType.polyn, FnType.matrix,
        FnType.num, FnType.num, FnType.num, FnType.other, FnType.other, FnType.polyn,
       FnType.trig, FnType.trig, FnType.trig, FnType.trig, FnType.trig, FnType.trig,
        FnType.trig, FnType.trig, FnType.trig,
        FnType.trig, FnType.trig, FnType.trig, FnType.trig, FnType.trig, FnType.trig,
        FnType.complex, FnType.trig, FnType.num, FnType.complex, FnType.num, FnType.trig, FnType.trig,
        FnType.num, FnType.complex, FnType.trig, FnType.trig, FnType.trig,
        FnType.num, FnType.num, FnType.polyn, FnType.trig, FnType.trig, FnType.num, FnType.trig,
        FnType.polyn, FnType.other, FnType.polyn, FnType.vector, FnType.vector,
        FnType.complex, FnType.num, FnType.complex, FnType.other, FnType.vector,
        FnType.matrix, FnType.matrix}
    Public Shared vInvFn() As String = {
        "", "", "", "", "",
        "", "", "", "",
        "", "",
        "", "", "", "", "", "",
        "", "", "", "", "", "",
        "cosh", "coth", "csch", "sech", "sinh", "tanh",
        "acoth", "acsch", "asech",
        "cos", "cot", "csc", "sec", "sin", "tan",
        "", "acosh", "", "", "", "asinh", "atanh",
        "", "", "acos", "acot", "acsc",
        "ln", "exp", "", "asec", "asin", "", "atan",
        "", "", "", "", "",
        "", "exp", "", "", ""}
    Public Shared vIntegral() As String = {"integral", "integrate", "∫"} ', "det"}
    Public Shared vIntegralBrowserEquivalent() As String = {"&#8747;", "&#8747;", "&#8747;"} ', "&#8734;"}
    Public Shared sAllowEqual As String = "isolate|euler"
    Friend Shared sMtxFn() As String =
                     {"identity", "echelon", "cof", "cross",
                      "eigenvalues", "eigenvectors",
                      "transpose", "trn", "adj",
                      "trace", "roots", "rank",
                      "det", "jordan",
                      "factor", "jacobian",
                      "lagrangianinterpolation", "orthog", "lim", "mod",
                      "max", "min"}
    'Shared vMtxFn(-1) As String '= { _
    '"lagrangianinterpolation", _
    '"jacobian", "orthog", "factor"}
    Public Shared sFn As String
    Public Shared vCntsCaseNonSen() As String = {"pi"}
    Public Shared vCntsCaseSen() As String = {"e"}
    Shared sCnts As String = Join(vCntsCaseSen, "|") + _
        "|(?i)" + Join(vCntsCaseNonSen, "|") + "(?-i)"

    Public Shared sGreek As String =
    "ΑαΒβΓγΔδΕεΖζΗηΘθΙιΚκΛλΜμΝνΞξΟοΠπΡρΣσΤτΥυΦφΧχΨψΩω" + "∞"
    Public Shared vGreekBrowserEquivalents() As String = {"&#391;", "&#945;", "&#932;", "&#946;", "&#915;", "&#947;", "&#916;", "&#948;", "&#917;", "&#949;", "&#918;", "&#950;", "&#919;", "&#951;", "&#920;", "&#952;", "&#921;", "&#953;", "&#922;", "&#954;", "&#923;", "&#955;", "&#924;", "&#956;", "&#925;", "&#957;", "&#926;", "&#958;", "&#927;", "&#959;", "&#928;", "&#960;", "&#929;", "&#961;", "&#931;", "&#963;", "&#932;", "&#964;", "&#933;", "&#965;", "&#934;", "&#966;", "&#935;", "&#967;", "&#936;", "&#968;", "&#937;", "&#969;", "&#8734;"}

    Public nomVarsToAppend As String = ""
    Public Shared vOpWeight() As Int64 = {1, 2, 3, 3, 4, 5, 6}

    Public Shared vOp() As String = {"+", "-", "*", "/", "%", "^", "!"}
    Public Shared vLgOp() As String = {"and", "or", "xor", "not", "nand", "nor"}

    Public Shared sCnt As String '= "(?<cnt>(?i)(" + sCnts + "){1}(?-i))"
    Public Shared sLP As String = "(?<lp>\(|\{|\[){1}"
    Public Shared sRP As String = "(?<rp>\)|\}|\]){1}"
    Public Shared sCol As String = "(?<col>(\;|\t|\,){1})"
    Public Shared sRow As String = "(?<row>(\r\n|\r|\n|\||\]\[){1,})"
    Public Shared sSpace As String = "(?<space>[ ]+)"
    Public Shared sOp As String = "(?<op>\" + Join(vOp, "|\") + "{1})"
    Public Shared sLgOp As String = "(?<lgOp>(?i)" + Join(vLgOp, "|") + "(?-i))"
    Public Shared sMtxOp As String = "(((\r|\n|\|)+||(?<=\]))(?<mtxOp>\" + Join(vOp, "|\") + ")((\r|\n|\|)+|(?=\[)))"
    Shared sHex As String = "(?<hex>(?i)&h([.0-9b-f]|[a](?!nd))*(?-i))"
    Shared sDec As String = "(?<dec>&(d|D)[.0-9]*)"
    Shared sOct As String = "(?<oct>&(o|O)[.0-7]*)"
    Shared sBin As String = "(?<bin>&(b|B)[.0-7]*)"
    Shared sSqrt As String = "(?<fn>(?<sqr>√))"
    'Public Shared sNum = "(?<num>(([0-9]{1,3},[0-9]{3}(?![0-9])|[0-9])+\.?[0-9]*|[0-9]*\.?[0-9]+)([eE][-+]?[0-9]+)?)"
    Public Shared sNum As String = "(?<num>((\d{1,3}((\,\d{3})+(?!\d))(\.[0-9]+)?)|[\.\d]+)([eE](\s*)[-+]?[0-9]+)?)"
    Shared sRad As String = "(?<rad>&(?i)rad(?-i))"
    Shared sDeg As String = "(?<deg>&(?i)deg(?-i))"
    Shared sGrad As String = "(?<grad>&(?i)grad(?-i))"
    Dim vAll() As String

    Public sVar As String = "(?<var>_([0-9a-zA-Z_" + sGreek + "])*{})"
    'Public sAt As String = "(?<at>@(.)+)(?='|$|" + sRow + ")"
    Public sAt As String = "(?<at>@)"
    Public Shared sComment As String = "(?<comment>\/\*(.)+\*\/|((\'|\/\/)(.)+(" + sRow + "?|$)))"
    Public Shared sEqual As String = "(?<equal>\=)"
    Public sVar2 As String
    Dim sDerResp As String
    Dim sIntegResp As String

    Dim sAll As String
    'Public Shared integralCnst As String = "_constant"
    Dim iIntegralCnst As Int64 = 1
    Public Const rad2Degrees As Double = 180 / Math.PI
    Public Const rad2Centesimal As Double = 200 / Math.PI
    Public Const degree2Radians As Double = Math.PI / 180
    Public Const centesimal2Rad As Double = Math.PI / 200
    Public Const maxPolyDegree As Int64 = 120
    Friend Shared ticksStart, timeOutms, lnCount As Long


    Public Sub New(cfg As Config)
        Initialize(cfg)
    End Sub
    Public Function sAll2() As String
        If InStr(sAll, "{}") = 0 Then
            Return sAll
        Else
            If nomVarsToAppend.Length = 0 Then
                Return Replace(sAll, "{}", "")
            End If
        End If
        Return Replace(sAll, "{}", "|" + nomVarsToAppend)
        'Return Replace(sAll, "{}", "|" +oVars.getVarsStrListToAppend 
    End Function
    Public Sub Initialize(cfg As Config)
        'Optional ByVal sImaginary As String = "i")
        Try
            'If cfg Is Nothing Then
            'cfg = New Config
            'End If
            iIntegralCnst = 1
            Dim i, j, k As Int64
            'sImg1 = cfg.sImg
            'sFn = "(?<fn>(?i)(?<mtxFn>" + Join(vMtxFn, "|") + ")|" + Join(vFn, "|") + _
            '    "|(?<integral>" + Join(vIntegral, "|") + ")" + _
            '     "(?-i))"
            sFn = "(?<fn>(?i)" + Join(vFn, "|") +
                "|(?<integral>" + Join(vIntegral, "|") + ")" +
                 "(?-i))"
            Dim sMsg As String = String.Empty


            If cfg.bVarName1Char Then
                sVar2 = "(?<var2>([a-zA-Z" + sGreek + "]){1})"
            ElseIf Not cfg.bNumsInVarName Then
                sVar2 = "(?<var2>([a-zA-Z" + sGreek + "]){1}([a-zA-Z" + sGreek + "]){0,1})"
            Else
                sVar2 = "(?<var2>([a-zA-Z" + sGreek + "]){1}([0-9a-zA-Z" + sGreek + "]){0,1})"
            End If
            sDerResp = "(?<Dx>(?i)(derivative|derivate|d)(?<DOrder>[1-9]?)(?<resp>" +
                                            sVar + "|" + sVar2 + "?)" + "(?-i))(?=\()"
            sIntegResp = "(?<Idx>d(?i)(?<IResp>" +
                                            sVar + "|" + sVar2 + "?)" + "(?-i)(?<!=\)))"
            sSubstitute = ""
            For i = 0 To vSubstitute.Length - 1
                For j = 0 To vSubstitute(i).Length - 1
                    Dim b() As Byte = System.Text.UnicodeEncoding.Unicode.GetBytes(
                        vSubstitute(i).Chars(j))
                    Dim asc1 As Int64 = 0
                    For k = b.Length - 1 To 0 Step -1
                        asc1 = asc1 * 256 + CInt(b(k))
                    Next
                    Dim sHex As String = Hex(asc1)
                    Do While sHex.Length < 4
                        sHex = "0" + sHex
                    Loop
                    sSubstitute += String.Format("\u{0}", sHex)
                Next
                sSubstitute += "|"
            Next
            sSubstitute = "(?<substitute>" + Left(sSubstitute, Len(sSubstitute) - 1) + ")"

            Dim sImag As String

            If cfg.bVarName1Char Then
                sCnt = "(?<cnt>(" + sCnts + "){1})"
                sImag = "(?<img>" + cfg.sImg + "{1})"
            Else
                ' e+g = 2.718.. + g, but
                ' ae+g is a variable. The same stands for "pi" or "i".
                ' So "aee+g" becomes ---> "2.718*ae + g".
                ' Constants (or 'i'/'j') can't be preceeded or followed by a letter:
                sCnt = "(?<cnt>(?i)(?<![A-Za-z]+)(" + sCnts + "){1}(?![A-Za-z]+)(?-i))"
                sImag = "(?<img>(?<![A-Za-z]+)" + cfg.sImg + "(?![A-Za-z]+){1})"
            End If
            vAll = New String() {sEqual, sComment,
                                 sSubstitute, sFn, sDerResp, sIntegResp, sNum, sHex, sDec, sOct, sBin,
                                 sRad, sDeg, sGrad,
                                  sLgOp, sVar, sCnt, sImag, sVar2, sMtxOp, sOp, sLP, sRP,
                                 sSqrt,
                                   sCol, sRow,
                                 "(?<param>" + customFn.sParamDelimiter + ")",
                                 sSpace,
                                  sAt,
                                  "(?<eq>\=)", "(?<resto>.+)"}
            sAll = "(?<all>\G" + Join(vAll, "|") + ")"
        Catch ex As Exception
            Throw ex
        End Try
    End Sub

    'Public Shared Function regOptions() As RegexOptions
    '    Dim regOpt As RegexOptions
    '    If bIgnoreSpaces Then
    '        regOpt = RegexOptions.IgnorePatternWhitespace
    '    End If
    '    If Not bCaseSensitive Then
    '        regOpt = (regOpt Or RegexOptions.IgnoreCase)
    '    End If
    '    Return regOpt
    'End Function
    Public Shared sIntConst As String = "_K"
    Public ReadOnly Property integralCnst As String
        Get
            Dim e1 As String = String.Format(sIntConst + "{0}", iIntegralCnst)
            iIntegralCnst += 1
            Return e1
        End Get
    End Property
    Public Shared Function TryParseDbl(ByVal m As Match, ByRef result As Double) As Boolean
        If Double.TryParse(m.ToString, _
            Globalization.NumberStyles.Float Or _
            Globalization.NumberStyles.AllowThousands, _
            MathGlobal8.us, _
             result) Then
            Return True
        End If
        Return False
    End Function
    Public Shared Function TryParseDbl(ByVal e1 As String, ByRef result As Double) As Boolean
        If Double.TryParse(e1, _
            Globalization.NumberStyles.Float Or _
            Globalization.NumberStyles.AllowThousands, _
            MathGlobal8.us, _
             result) Then
            Return True
        End If
        Return False
    End Function
    Public Shared Function CloneObject(ByVal obj As Object) As Object
        Static bf As New Formatters.Binary.BinaryFormatter( _
                Nothing, New StreamingContext( _
                StreamingContextStates.Clone))
        Dim ret As Object = Nothing
        Try
            Dim ms As New IO.MemoryStream(1000)
            bf.Serialize(ms, obj)
            ms.Seek(0, SeekOrigin.Begin)
            ret = bf.Deserialize(ms)
            ms.Close()
        Catch ex As Exception
            Throw ex
        End Try
        Return ret
    End Function
    Public Shared Function CloneMatch(ByVal m As Match) As Match
        Dim ret(0) As Match
        Try
            Dim arr() As Match = {m}
            Array.Copy(arr, ret, 1)
        Catch ex As Exception
            Throw ex
        End Try
        Return ret(0)
        'Static bf As New Formatters.Binary.BinaryFormatter( _
        '        Nothing, New StreamingContext( _
        '        StreamingContextStates.Clone))
        'Dim ret As Object = Nothing
        'Try
        '    Dim ms As New IO.MemoryStream(5000)
        '    'Dim bf As New Formatters.Binary.BinaryFormatter( _
        '    'Nothing, New StreamingContext(StreamingContextStates.Clone))
        '    bf.Serialize(ms, m)
        '    ms.Seek(0, SeekOrigin.Begin)
        '    ret = bf.Deserialize(ms)
        '    ms.Close()
        'Catch ex As Exception
        '    Throw ex
        'End Try
        'Return ret
    End Function
    Public Shared ReadOnly Property isTimeout As Boolean
        Get
            lnCount += 1
            If lnCount > 5000000 Then
                lnCount = 0
                Return True
            End If
            Dim ln As Long = Now.Ticks
            If ln < ticksStart + timeOutms * 10 ^ 4 Then
                Return False
            End If
            Return False
        End Get
    End Property

    Public Shared Function IsOperand(ByVal m As Match) As Boolean
        Return m.Groups("num").Success OrElse _
                m.Groups("cnt").Success OrElse _
                m.Groups("img").Success OrElse _
                IsVariable(m)
    End Function
    Public Shared Function IsVariable(ByVal m As Match) As Boolean
        Return m.Groups("var").Success OrElse _
                m.Groups("var2").Success
    End Function
    Shared Function getTable(ByVal e1 As String, ByVal borderColor As String) As String
        Dim t As String = "<TABLE id=""div1"" BORDER=""1"" CELLPADDING=""1"" CELLSPACING=""2"" BORDERCOLOR=""#d0d0d0"" >"

        Try
            Dim i, j As Int64
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
                t2 = "<TR ALIGN=""LEFT"" style='text-align:right' borderColor=" + borderColor + ">"
                't2 = "<TR ALIGN=""RIGHT"" borderColor=""#d0d0d0"">"
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
    Shared Function NameAndVersion() As String
        Dim sNV As String = String.Empty
        Try
            Dim asmbly As Assembly = System.Reflection.Assembly.GetAssembly(GetType(MathGlobal8))
            Dim name As AssemblyName = asmbly.GetName()
            sNV = name.Version.ToString
            sNV = name.Name + " -- " + Left(sNV, Len(sNV) - 2)
        Catch ex As Exception

        End Try
        Return sNV
    End Function
    Public Enum outputBase
        [decimal]
        hexadecimal
        octal
        binary
    End Enum
End Class


Public Class msg8
    Public Shared ReadOnly Property num(ByVal i As Int64) As String
        Get
            Select Case i
                Case 0 : Return "Argument isn't a real number."
                Case 1 : Return "Missing a matching {0}."
                Case 2 : Return "An operator is missing."
                Case 3 : Return "Tokens' vector is already occupied."
                Case 4 : Return "An operand is missing."
                Case 5 : Return "Found an unknown or illegal token: {0}"
                Case 6 : Return "Missing a left parentheses after the function {0}."
                Case 7 : Return "Unknown error."
                Case 8 : Return "A system of equations: expected " + vbCrLf +
                                "one and only one '=' sign at each equation."
                Case 9 : Return "n/a: invalid argument."
                Case 10 : Return "Derivative {0} n/a."
                Case 11 : Return "Can't derivate: {0} isn't a valid variable."
                Case 12 : Return "System of equations resolution: n/a."
                Case 13 : Return "n/a"
                Case 14 : Return "n/a: there is missing an argument/operand."
                Case 15 : Return "Name '{0}' isn't a valid name."
                Case 16 : Return "Variable '{0}' not found."
                Case 17 : Return "Variable '{0}' already exists."
                Case 18 : Return "Variable assignment '{0}' is not valid."
                Case 19 : Return "Recursively nested variable '{0}'."
                Case 20 : Return "n/a, an equation was expected."
                Case 21 : Return "Func. definition should have one and only one '=' sign."
                Case 22 : Return "Func. definition should have at least one parameter."
                Case 23 : Return "Func.definition is missing last closing ')'."
                Case 24 : Return "Func.definition is missing parameter number {0}."
                Case 25 : Return "Function name {0} isn't a valid name."
                Case 26 : Return "Name {0} isn't a valid fn. parameter name."
                Case 27 : Return "Can't parse function's definition in: {0}"
                Case 28 : Return "Unknown operator: {0}"
                Case 29 : Return "Fn's {0} argument is not valid."
                Case 30 : Return "Missing with respect to which variable integrate, e.g.: 'dx'"
                Case 31 : Return "Not a definite integral: n/a"
                Case 32 : Return "Logical operator isn't valid. Please, check -for ex.: spaces or commas- in the input data."
                Case 33 : Return "Function exponent '{0}' in '{1}' not valid, when expected a number from the interval [2,9]."
                Case 34 : Return "n/a: expected a left parenthesis (or exponent and left parenthesis) in the function '{0}', e.g. '{1}'"
                Case 35 : Return "Missing key word 'integral', or '∫', preceding to '{0}'."
                Case 36 : Return "Couldn't parse an hexadecimal number"
                Case 37 : Return "Couldn't parse a decimal number"
                Case 38 : Return "Couldn't parse an octal number"
                Case 39 : Return "Couldn't parse a binary number"
                Case 40 : Return "n/a: There {0} equation{1}/expression{1} and {2} variable{3}: "
                Case 41 : Return "n/a: missing left or right side of the equation."
                Case 42 : Return "n/a, parentheses not matching."
                Case 43 : Return "Found a matrix while expected an expression." + vbCrLf + " Should you check option to ignore carriage return 'CR'?."
                Case 44 : Return "n/a: the operation has timed out."
                Case 45 : Return "The solution found out from the (linear) system of equations:"
                Case 46 : Return "The solution found out from the (non linear) system of equations:"
                Case 47 : Return "Found no solution."
                Case 48 : Return "follows:"
                Case 49 : Return "These roots ({0}-{0}0)*({0}-{0}1)... constitute the polynomial:"
                Case 50 : Return "Found columns while expected an equation. Please, check -for ex.: spaces- in the input data ."
                Case 51 : Return "n/a: The equations of the linear system aren't independent."
                Case 52 : Return "Please check. The entry '{0}' is not valid. (Should be for ex. x=3, or P(x)=x^2+2)"
                Case 53 : Return "n/a: The system seems mixed with expression(s) and equation(s). " +
                        vbCrLf + "Use a point '.' as decimal separator (instead of a coma ',')."
                Case 54 : Return "n/a: Not all the gcd()'s arguments are neither polynomial nor real"
                Case 55 : Return "n/a: variables are recursively nested."
                Case 56 : Return "False, {0} is not zero."
                Case 57 : Return "n/a: syntax error, please check the input data."
                Case 58 : Return "Argument isn't a complex, nor real, number."
                Case 59 : Return "Argument should be a positive integer."
                Case 60 : Return "The equality is true."
                Case 61 : Return "n/a: the equation contains more than one equal sign."
                Case 62 : Return "n/a: unexpected start element {0}"
                Case 63 : Return "Found {0} left and {1} right parenthesis at row #{2}"
                Case 64 : Return "Found {0} left and {1} right parenthesis at row #{2}, column #{3}." + "<br />" +
                    " Be sure you use a dot ""."" as decimal separator, <span style='color:red'>NOT a comma "",""</span>:" + "<br />" +
                    " 3.5x+2 (right) 3<span style='color:red'>,</span>5x+2 (WRONG) "
                Case 65 : Return "{0} exceeds maximum #of intervals={1}"
                Case 66 : Return "n/a, could not define the points of interpolation."
                Case 67 : Return "Input is empty."
                Case 68 : Return "Partial fraction descomposition n/a: numerator's degree >= denominator's degree"
                Case 69 : Return "n/a: in '{0}' there is more than one variable {{{1}}}."
                Case 70 : Return "n/a, each point must have 2 coordinates (x,y)=(x,f(x)). Should you replace colons (,) by semicolons (;) ?"
                Case 71 : Return "Infinity"
                Case 72 : Return "-Infinity"
                Case 73 : Return "n/a, not a linear first order differential equation."
                Case 74 : Return "n/a: incompatible system of equations."
                Case 75 : Return "First parameter 'Step' should be a real number"
                Case 76 : Return "Second parameter '#of steps' should be a real number"
                Case 77 : Return "Third parameter should be a differential equation"
                Case 78 : Return "n/a: missing initial condition"
                Case 79 : Return "n/a: mixed equations/expressions"
                Case 80 : Return "n/a: input is not a polynomial fraction"
                Case 81 : Return "n/a: Timeout."
                Case 82 : Return "n/a: found error at variables box. Try to comment inserting an apostrophe (') before each line."
                Case 83 : Return "n/a: found a non-integer coefficient in the (normalized) polynomial {0}"
                Case 84 : Return "Argument(s) isn't a complex, nor real, number."

                Case Else : Return "Unknown error."
            End Select
            Return ""
        End Get
    End Property
    Public Shared Function msg(ByVal i As Int64,
                    Optional ByVal params() As Object = Nothing,
                    Optional ByVal dspNumError As Boolean = True,
                    Optional ByVal sIdioma As String = "en") As String
        Dim ret As String
        Select Case sIdioma
            Case "ca"
                Select Case i
                    Case 1000 : ret = "Operador {0} desconegut."
                    Case 1001 : ret = "Element desconegut: '{0}'"
                    Case 1002 : ret = "Manca aparellar un parèntesi."
                    Case 1003 : ret = "Error descunegut."
                    Case 1004 : ret = "Manca un operand (o un argument)."
                    Case 1005 : ret = "Divisió per zero."
                    Case 1006 : ret = "Divisió polinòmica multivariables no disponible."
                    Case 1007 : ret = "Exponente fuera de rango."
                    Case 1008 : ret = "L'exponent del polinomi es imaginari: operació n/a"
                    Case 1009 : ret = "L'exponent del polinomi no enter: operació n/a"
                    Case 1010 : ret = "L'exponent del polinomi es un polinomi: operació n/a"
                    Case 1011 : ret = "L'element es una constant: no existeixen pas les rels."
                    Case 1012 : ret = "El polinomi té múltiples variables: rels n/a"
                    Case 1013 : ret = "Vectors de diferentes longituts: operació n/a"
                    Case 1014 : ret = "El divisor és un vector: operació n/a"
                    Case 1015 : ret = "Exponent negatiu: operació n/a"
                    Case 1016 : ret = "Exponent no enter: operació n/a"
                    Case 1017 : ret = "Exponent imaginari: operació n/a"
                    Case 1018 : ret = "Exponent polinomial: operació n/a"
                    Case 1019 : ret = "Mides de matrius no concordants: operació n/a"
                    Case 1020 : ret = "Exponent nul: operació n/a"
                    Case 1021 : ret = "Matriu amb exponent no enter: operació n/a"
                    Case 1022 : ret = "Matriu amb exponent nul: operació n/a"
                    Case 1023 : ret = "Manca un parèntesi de tancament (o hi ha algún de més d'apertura)."
                    Case 1024 : ret = "Manca un parèntesi d'apertura (o hi ha algún de més de tancament)."
                    Case 1025 : ret = "El sistema d'equacions és buit."
                    Case 1026 : ret = "Trobades {0} equacions i {1} variables ({1}); sistema d'equacions n/a."
                    Case 1027 : ret = "'{0}' és present com a única variable a les ecuacions #{1} y #{2}; sistema d'equacions n/a."
                    Case 1028 : ret = "'{0}' no és cap equació."
                    Case 1029 : ret = "Resolució del sistema d'equacions n/a."
                    Case 1030 : ret = "Manca un valor per a la variable '{0}'."
                    Case Else : ret = "Error."
                End Select
            Case "es"
                Select Case i
                    Case 1000 : ret = "Operador {0} desconocido."
                    Case 1001 : ret = "Elemento desconocido: '{0}'"
                    Case 1002 : ret = "Falta emparejar un paréntesis."
                    Case 1003 : ret = "Error desconocido."
                    Case 1004 : ret = "Falta un operando (o un argumento)."
                    Case 1005 : ret = "División por cero."
                    Case 1006 : ret = "División polinómica multivariables no disponible."
                    Case 1007 : ret = "Exponent fora dels marges admesos."
                    Case 1008 : ret = "El exponente del polinomio es imaginario: operación n/a"
                    Case 1009 : ret = "El exponente del polinomio es no entero: operación n/a"
                    Case 1010 : ret = "El exponente del polinomio es un polinomio: operación n/a"
                    Case 1011 : ret = "El elemento es una constante: no existen raíces."
                    Case 1012 : ret = "El polinomio tiene múltiples variables: raíces n/a"
                    Case 1013 : ret = "Vectores de distintas longitudes: operación n/a"
                    Case 1014 : ret = "El divisor es un vector: operación n/a"
                    Case 1015 : ret = "Exponente negativo: operación n/a"
                    Case 1016 : ret = "Exponente no entero: operación n/a"
                    Case 1017 : ret = "Exponente imaginario: operación n/a"
                    Case 1018 : ret = "Exponente polinomial: operación n/a"
                    Case 1019 : ret = "Tamaños de matrices no concordantes: operación n/a"
                    Case 1020 : ret = "Exponente nulo: operación n/a"
                    Case 1021 : ret = "Matiz con exponente no entero: operación n/a"
                    Case 1022 : ret = "Matiz con exponente nulo: operación n/a"
                    Case 1023 : ret = "Falta un paréntesis de cierre (o sobra alguno de apertura)."
                    Case 1024 : ret = "Falta un paréntesis de apertura (o sobra alguno de cierre)."
                    Case 1025 : ret = "El sistema de ecuaciones está vacío."
                    Case 1026 : ret = "Halladas {0} equaciones y {1} variables ({1}); sistema de ecuaciones n/a."
                    Case 1027 : ret = "'{0}' está presente como única variable en las ecuaciones #{1} y #{2}; sistema de equaciones n/a."
                    Case 1028 : ret = "'{0}' no es una ecuación."
                    Case 1029 : ret = "Resolución del sistema de ecuaciones n/a."
                    Case 1030 : ret = "Falta el valor de la variable '{0}'."
                    Case Else : ret = "Error."
                End Select
            Case Else
                Select Case i
                    Case 0 : ret = "Error. Function approximation only performed with one variable (variable x)."
                    Case 1 : ret = "One and only one variable for graphics."
                    Case 2 : ret = "Syntax error: missing an operand."
                    Case 3 : ret = "Syntax error. Missing @@ a closing parenthesis (or excessive opening parentheses)."
                    Case 4 : ret = "Syntax error. Missing @@ an opening parenthesis (or excessive closing parentheses)."
                    Case 5 : ret = "Found error in f(x) or in the 'constants/defs.'. Please check."
                    Case 6 : ret = "Found a colon (:) or semicolon (;) in f(x) (only valids for interpolation; see notes)."
                    Case 7 : ret = "Error. Please, don't use 'e' as variable nor as constant to substitute (see the notes)." +
                        "Only use 'e' for example as in 3e-5 (=3*10^-5). For neperian constant write exp(1) (= 2.71828182845905)."
                    Case 8 : ret = "Empty function."
                    Case 9 : ret = "Couldn't find the roots."
                    Case 10 : ret = "You may only use 'x' as variable in f(x)"
                    Case 11 : ret = "Error. You can't place '=', '<' nor '>'."
                    Case 12 : ret = "Error. Recursive nested variable @@."
                    Case 13 : ret = "Syntax error: missing an operator (+,-,/,*, etc.)."
                    Case 14 : ret = "Error: missing an operand or division by zero."
                    Case 15 : ret = "Syntax error: found '@@'.<br>(code ##)"
                    Case 16 : ret = "Error: argument in function '@@' isn't valid."
                    Case 17 : ret = "Found error at point {0}."
                    Case 18 : ret = "You may only use 'x' as variable to obtain f(x)"
                    Case 19 : ret = "Error: only admitted one equal sign '='."
                    Case 20 : ret = "Real solutions:"
                    Case 21 : ret = "Root {0}:"
                    Case 22 : ret = "Polynomial approximation at interval [{0},{1}]. ({2} points)"
                    Case 23 : ret = "Use a point '.' as decimal separator (instead of a coma ',')."
                    Case 24 : ret = "Function undefined for {0}"
                    Case 25 : ret = "Complex roots:"
                    Case 26 : ret = " ...or division by zero."
                    Case 27 : ret = "Please, separate the 'constants/func. definitions' by semicolons (;)."
                    Case 28 : ret = "Division by zero."
                    Case 29, 39 : ret = "Found an error in constants/func. definitions box. Please, check."
                    Case 30 : ret = "Error. Different arguments."
                    Case 31 : ret = "Ilegal character: {0}"
                    Case 32 : ret = "Out of range value error."
                    Case 33 : ret = "Error. x (or a number, or expression) must preced '<b>^</b>' and needs an integer exponent. Example: (x+1)^2"
                    Case 34 : ret = "Polynomial approximation in Polynomial Calculator."
                    Case 35 : ret = "Interval [{0},{1}]. ({2} points)"
                    Case 36 : ret = "<br /> (Perhaps, try another interval/points.)"
                    Case 37 : ret = "Polynomial approximation."
                    Case 38 : ret = "Please, wait ..."
                        'Case 39 : ret = "Found an error in the 'constants/defs.'. Please check."
                    Case 40 : ret = "Singular matrix."
                    Case 41 : ret = "Matrix isn't square."
                    Case 42 : ret = "Equation isn't linear."
                    Case 43 : ret = "hexadecimal|octal|binary"
                    Case 44 : ret = "Missing a parenthesis"
                    Case 45 : ret = "Not implemented: {0}"
                    Case 46 : ret = "More..."
                    Case 47 : ret = "Close"
                    Case 48 : ret = "Max. {0} points."
                    Case 49 : ret = "Error, the number of arguments does not match the function definition."
                    Case 50 : ret = "False"


                    Case 1000 : ret = "Unknown operator {0}."
                    Case 1001 : ret = "Unkwonw token: '{0}'"
                    Case 1002 : ret = "Missing matching parenthesis."
                    Case 1003 : ret = "Unknown error."
                    Case 1004 : ret = "Missing an operand (or argument)."
                    Case 1005 : ret = "Division by zero."
                    Case 1006 : ret = "Multivariate polynomial division n/a."
                    Case 1007 : ret = "Exponent out of range."
                    Case 1008 : ret = "Exponent is imaginary: operation n/a"
                    Case 1009 : ret = "Exponent non integer: operation n/a"
                    Case 1010 : ret = "Exponent is a polynomial: n/a"
                    Case 1011 : ret = "Item is constant: there are no roots."
                    Case 1012 : ret = "The polynomial has multiple variables: roots n/a"
                    Case 1013 : ret = "Different vectors' lengths: operation n/a"
                    Case 1014 : ret = "Divisor is a vector: operation n/a"
                    Case 1015 : ret = "Negative exponent: operation n/a"
                    Case 1016 : ret = "Non integer exponent: operation n/a"
                    Case 1017 : ret = "Imaginary exponent: operation n/a"
                    Case 1018 : ret = "Polynomial exponent: operation n/a"
                    Case 1019 : ret = "Matrix sizes don't match: operation n/a"
                    Case 1020 : ret = "Null exponent: operation n/a"
                    Case 1021 : ret = "Matrix exponent non integer: operation n/a"
                    Case 1022 : ret = "Matrix exponent is null: operation n/a"
                    Case 1023 : ret = "Missing a closing parenthesis (or excessive opening parentheses)."
                    Case 1024 : ret = "Missing an opening parenthesis (or excessive closing parentheses)."
                    Case 1025 : ret = "Empty system of equations."
                    Case 1026 : ret = "Found {0} equations and {1} variables ({1}); system of equations n/a."
                    Case 1027 : ret = "{0} is present as unique variable in equations #{1} & #{2}; system of equations n/a."
                    Case 1028 : ret = "'{0}' is no equation."
                    Case 1029 : ret = "System of Equations solution n/a."
                    Case 1030 : ret = "Missing a value for the variable '{0}'."
                    Case Else : ret = "Error."
                End Select
        End Select
        If Not params Is Nothing Then
            ' pass arguments ("{0}, {1}, etc.) in params() to string 'ret':
            ret = String.Format(ret, params)
        End If
        If dspNumError AndAlso i <> 5 Then
            ret += " (Err.#:" + i.ToString + ")"
        End If
        Return ret
    End Function
End Class
