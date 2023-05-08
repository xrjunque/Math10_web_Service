Imports System.Web
Imports System.Net
Imports System.IO
Public Class contaHeros
    Inherits System.Web.UI.Page

    Dim IsM0WWA As Boolean = True
    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        'TextBox1.Text = "1"
        contador2(
                Page,
                Request,
                Response)
    End Sub


    Public Sub contador2(ByVal page As UI.Page,
                                ByVal Request As HttpRequest,
                               ByVal Response As HttpResponse)
        Try
            If page.IsPostBack = False Then
                'Exit Sub
            End If
            Dim query, url, s, c, r, u, ip, ua, ln, r1, sb, v As String
            r = "" : ip = "" : ln = "" : ua = ""
            Try
                r = Request.ServerVariables("HTTP_REFERER")
            Catch ex As Exception
            End Try
            'Try
            '    If Request.UrlReferrer IsNot Nothing AndAlso _
            '    Request.UrlReferrer.AbsoluteUri IsNot Nothing AndAlso _
            '    Request.UrlReferrer.AbsoluteUri > Len(r) Then
            '        r = Request.UrlReferrer.AbsoluteUri
            '    End If
            'Catch ex As Exception

            'End Try
            Try
                ip = Request.Params("ip")  ' Request.ServerVariables("REMOTE_ADDR")
                If ip Is Nothing Then ip = ""
                If InStr(ip, "<ip>") Then
                    ip = Split(ip, "<ip>")(1)
                    ip = Split(ip, "</ip>")(0)
                    ip = Replace(ip, vbCr, "")
                    ip = Replace(ip, vbLf, "")
                End If
            Catch ex As Exception
            End Try
            If ip = "" Then
                Try
                    ip = Request.UserHostAddress
                Catch ex As Exception

                End Try
            End If
            Try
                ua = Request.ServerVariables("HTTP_USER_AGENT") + IIf(Request.Browser.Crawler, " Robot", "")
            Catch ex As Exception
            End Try
            Try
                ln = Request.ServerVariables("HTTP_ACCEPT_LANGUAGE")
            Catch ex As Exception
            End Try
            Try
                u = LCase(getPageName(Request)) ' nombre de la página
            Catch ex As Exception
                u = ""
            End Try
            Try
                If u IsNot Nothing AndAlso
                Len(u) = 0 Then
                    u = LCase(page.AppRelativeVirtualPath)
                    u = Replace(u, "\", "/")
                    If InStr(u, "/") Then
                        Dim vU() As String = Split(u, "/")
                        u = vU(vU.Length - 1)
                    End If
                End If
            Catch ex As Exception

            End Try
            s = "0"
            c = "0"
            v = "0"
            r1 = r
            'If t Is Nothing OrElse t = "" OrElse
            't2 Is Nothing OrElse t2 = "" Then
            '    t = "0"
            '    t2 = "0"
            'End If
            sb = u ' subpágina

            'query = "s=" & s & "&ip=" & ip & "&c=" & c
            query = "&ip=" & ip


            Dim sDom As String = "https://herostechnology.co.uk"
            If IsM0WWA Then
                sDom = "https://m0wwa.co.uk"
            End If
            'If Request.UrlReferrer IsNot Nothing AndAlso
            'Request.UrlReferrer.Host IsNot Nothing Then
            '    query += "&refHost=" + Request.UrlReferrer.Host
            'Else
            '    query += "&refHost=" + sDom
            'End If
            Dim suri As String = ""
            If Request.UrlReferrer IsNot Nothing AndAlso
            Request.UrlReferrer.AbsoluteUri IsNot Nothing Then
                suri = "&" + Request.UrlReferrer.AbsoluteUri
                Dim vs() As String = Split(suri, "/")
                suri = "/" + vs(vs.Length - 1)
            Else
                'query += "&refAbsUri=" + sDom + Replace( _
                'Request.FilePath, "\", "/")  '+ page.AppRelativeVirtualPath
                suri = sDom + Replace(Replace(
                    page.AppRelativeVirtualPath, "\", "/"), "~", "")
                Dim vs() As String = Split(suri, "/")
                suri = "/" + vs(vs.Length - 1)
            End If
            query += suri


            ' OJO!! !! !! el nombre de la página está en el querystring("r")

            Me.readWrite(ip, Request.QueryString("r"), ua)



            'url = "https://xrjunque.nom.es/conta1.aspx?" & query

            ''url = "https://localhost:64842/conta1.aspx?" & query ' s=&ip=127.0.0.1&c=32&u=https://localhost/mates/RootFinder.aspx&ln=en-us&r=&ua=Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; InfoPath.1; .NET CLR 1.1.4322)&sb=default&t=633054938628130000&t2=633054938642973750&v=5071"

            TextBox1.Text += query + vbCrLf
            ''Response.Clear()
            'If False Then
            '    Dim req As WebRequest = WebRequest.Create(url)
            '    Dim wr As WebResponse = req.GetResponse
            '    Dim sr As IO.Stream = wr.GetResponseStream
            '    Dim b(wr.ContentLength - 1) As Byte
            '    sr.Read(b, 0, b.Length)
            '    sr.Close()
            '    wr.Close()
            'Else
            '    Dim webClient As New System.Net.WebClient
            '    Dim result As String = webClient.DownloadString(url)
            '    TextBox1.Text += result + vbCrLf
            'End If
            'Response.Write(System.Text.ASCIIEncoding.ASCII.GetString(b))
        Catch ex As Exception
            Dim s1 As String = ex.ToString
            TextBox1.Text += ex.ToString
        End Try
    End Sub
    Public Shared Function getPageName(ByVal Request As HttpRequest) As String
        Dim e1 As String = ""
        Try
            Dim sPath As String = Request.Url.AbsolutePath
            Dim fi As New IO.FileInfo(sPath)
            e1 = fi.Name
        Catch ex As Exception
            e1 = "err"
        End Try
        Return e1
    End Function
    Sub readWrite(ip As String, suri As String, ua As String)
        Try
            Dim fs As FileStream = Nothing
            Dim path As String = Server.MapPath("~\pages\txt\herostech.txt")
            If IsM0WWA Then
                path = Server.MapPath("..\herostechnology\pages\txt\herostech.txt")
            End If
            If Request.IsLocal Then
                path = Server.MapPath("~\txt\herostech.txt")
            End If
            fs = New FileStream(path, FileMode.Append)
            Dim sw As New StreamWriter(fs, UnicodeEncoding.Unicode)
            sw.Write(ip + vbTab)
            Dim dns As String = ""
            Try
                dns = getHostName(ip)
            Catch ex As Exception

            End Try
            sw.Write(dns + vbTab)
            Dim sFch As String = Now.ToString("yyyy/MM/dd   hh:mm:ss")
            sw.Write(sFch + vbTab)
            Dim dt As DateTime = Now
            sw.Write(dt.ToShortTimeString + vbTab)
            sw.Write(vbTab)
            Dim vs() As String = Split(suri, "/")
            sw.Write(vs(vs.Length - 1))
            sw.Write(vbTab)
            sw.Write(ua)
            sw.WriteLine("")
            'sw.WriteLine(Request.RawUrl)
            sw.Close()
            fs.Close()
        Catch ex As Exception
            TextBox1.Text += ex.ToString + vbCrLf
        End Try
    End Sub
    Public Shared Function getHostName(ByVal strIP As String) As String
        Dim myIP As System.Net.IPAddress = System.Net.IPAddress.Parse(strIP)
        Dim hostInfo As System.Net.IPHostEntry =
        System.Net.Dns.GetHostEntry(myIP)
        Return hostInfo.HostName
    End Function

End Class