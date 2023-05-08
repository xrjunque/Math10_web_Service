Imports System.Reflection
Imports System.Text.RegularExpressions
Imports System.Runtime.Serialization

<Serializable()>
Public Class Config84

    Public Shared us As New Globalization.CultureInfo("en-US")
    Public Shared sNum As String = "(?<num>((\d{1,3}((\,\d{3})+)(\.[0-9]+)?)|[\.\d]+)([eE][-+]?[0-9]+)?)"
    Public Shared sHex As String = "\G(?<hex>(?i)([.0-9b-f]|[a](?!nd))+(?-i))"
    Public Shared sDec As String = "\G(?<dec>[.0-9]+)"
    Public Shared sOct As String = "\G(?<oct>[.0-7]+)"
    Public Shared sBin As String = "\G(?<bin>[.01]+)"

    ' Functions84 of one argument:
    Public Shared vFn() As String = {
        "rank",
        "trn", "cross", "dot", "det", "egvl", "egv",
        "acosh", "acoth", "acsch", "asech", "asinh", "atanh",
        "coth", "csch", "sech", "sign", "gcd", "lcm",
        "acos", "acot", "acsc", "asec", "asin", "atan", "conj", "cosh", "neg", "sinh", "sqrt", "tanh",
        "abs", "arg", "cos", "cot", "csc", "exp", "log", "mod", "sec", "sin", "tan", "sqr",
        "im", "ln", "re", "√"}
    Public Shared vLogOp() As String = {"and", "or", "not", "xor", "nor", "nand"}
    Public Shared sGreek As String =
    "\u0391-\u03A9\u03B1-\u03C9\u0388-\u0390\u0386\u03D0-\u03E1"
    '   Ucase     |    Lcase   | with Tonos |A_Tono|cursive, Archaic,Alternates
    Public Shared sCol As String = "(?<col>(\;|\t|\,){1})"
    Public Shared sRow As String = "(?<row>(\r\n|\r|\n|\|){1,})"
    'Public Shared sVar As String = "(?<var>[a-zA-Z" + sGreek + "]{1})"
    Public Shared sVar2 As String = "(?<var2>_[0-9a-zA-Z_" + sGreek + "]+)"
    Public Shared vConst() As String = {"e", "pi"}
    Public outputFormat As outputMessage = outputMessage.RichTextFormat
    Public sImg As String = "i"
    Public nDecimals As Int64 = 3
    Public bFractions As Boolean = False
    Public bDetail As Boolean = False
    Public bRounding As Boolean = False
    Public bEngNotation As Boolean = True
    Public base As numBase = numBase.decimal
    'Friend oDetail As New Detall84
    Public Shared Function TryParseDbl(ByVal e1 As String, ByRef result As Double) As Boolean
        If Double.TryParse(e1,
            Globalization.NumberStyles.Float Or
            Globalization.NumberStyles.AllowThousands,
            us,
             result) Then
            Return True
        End If
        Return False
    End Function
    Shared Function NameAndVersion() As String
        Dim sNV As String = String.Empty
        Try
            Dim asmbly As Assembly = System.Reflection.Assembly.GetAssembly(GetType(Config84))
            Dim name As AssemblyName = asmbly.GetName()
            sNV = name.Version.ToString
            sNV = name.Name + " -- " + Left(sNV, Len(sNV) - 2)
        Catch ex As Exception

        End Try
        Return sNV
    End Function

End Class
Public Enum outputMessage
    plainText
    RichTextFormat
    HTML
End Enum