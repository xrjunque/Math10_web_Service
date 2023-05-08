Imports System.ComponentModel
Imports System.Web.Services
Imports System.Web.Services.Protocols
Imports System.Text.RegularExpressions
Imports m8 = Mates8_dll
Imports m10 = Math10_dll

' Para permitir que se llame a este servicio web desde un script, usando ASP.NET AJAX, quite la marca de comentario de la línea siguiente.
' <System.Web.Script.Services.ScriptService()> _
<System.Web.Services.WebService(Namespace:="http://xrjunque.nom.es/")>
<System.Web.Services.WebServiceBinding(ConformsTo:=WsiProfiles.BasicProfile1_1)>
<ToolboxItem(False)>
Public Class Math10
    Inherits System.Web.Services.WebService

    <WebMethod(Description:="Computer Algebra System (CAS). 
    Enter a Math expression (for ex. (x+y)*z) followed (if needed) by @variable=value
    (@x=1)")>
    Public Function Math10(str As String) As String ' inverse(A)@A=a;3;3|3;a;3|3;3;a
        Try
            C_sessionWeb.logFile(Nothing, str, "", Me.Context, 0, "polycalc.txt")
            If Regex.IsMatch(str, "integral|integrate|∫") OrElse
            (Regex.IsMatch(str, "D[^(]\(") AndAlso InStr(str, "=")) Then
                Dim p8 As New m8.matrixParser
                p8.parse(str)
                Dim cfg As New m8.Config
                str = p8.ret.toStringRetVal(cfg)
            Else
                Dim p10 As New m10.parseMatrix
                str = p10.Evaluate(str).eMtx.ToString
            End If
            C_sessionWeb.logFile(Nothing, str, "", Me.Context, 0, "polycalc.txt")
        Catch ex As Exception
            C_sessionWeb.logFile(ex, str, "", Me.Context, 0, "polycalc.txt")
            str = ex.Message
        End Try
        Return str
    End Function

End Class