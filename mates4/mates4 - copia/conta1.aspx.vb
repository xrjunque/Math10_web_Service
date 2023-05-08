Imports System.IO

Public Class conta1
    Inherits System.Web.UI.Page

    Dim paginaOrigen As web

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        validAddress()
        readWrite()
    End Sub
    Sub validAddress()
        Try
            ' obtener el nombre de la página de la cual proviene:
            Dim e1 As String = Request.Url.AbsolutePath
            If InStr(e1, "herostech.co.uk") > 0 Then
                paginaOrigen = web.herostech
            End If

            paginaOrigen = web.herostech

        Catch ex As Exception

        End Try
    End Sub
    Sub readWrite()
        Try
            Dim fs As FileStream = Nothing
            If paginaOrigen = web.herostech Then
                fs = New FileStream(Server.MapPath("~\txt\herostech.txt"), _
                                         FileMode.Append)
            End If
            Dim sw As New StreamWriter(fs, UnicodeEncoding.Unicode)
            Dim ip As String = Request.QueryString("ip")
            sw.Write(ip + vbTab)
            Dim dns As String = getHostName(ip)
            sw.Write(dns + vbTab)
            Dim sFch As String = Now.ToString("yyyy/MM/dd   hh:mm:ss")
            sw.Write(sFch + vbTab)
            Dim dt As DateTime = Now
            sw.Write(dt.ToShortTimeString + vbTab)
            sw.Write(vbTab)
            Dim suri As String = Request.QueryString("refAbsUri")
            Dim vs() As String = Split(suri, "/")
            sw.WriteLine(vs(vs.Length - 1))

            'sw.WriteLine(Request.RawUrl)
            sw.Close()
            fs.Close()
            Response.End()
        Catch ex As Exception

        End Try
    End Sub
    Public Shared Function getHostName(ByVal strIP As String) As String
        Dim myIP As System.Net.IPAddress = System.Net.IPAddress.Parse(strIP)
        Dim hostInfo As System.Net.IPHostEntry =
        System.Net.Dns.GetHostEntry(myIP)
        Return hostInfo.HostName
    End Function
    Enum web As Int32
        herostech = 1
    End Enum
End Class