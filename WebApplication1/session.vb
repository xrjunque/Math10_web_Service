Imports System.IO
Imports System.Web
Imports System.Threading
Imports System.Net
Imports System.Web.HttpUtility

Public Class C_sessionWeb

    Shared mtx As New Mutex
    Public Shared Function getHostName(ByVal strIP As String) As String
        Dim myIP As System.Net.IPAddress = System.Net.IPAddress.Parse(strIP)
        Dim hostInfo As System.Net.IPHostEntry =
        System.Net.Dns.GetHostEntry(myIP)
        Return hostInfo.HostName
    End Function
    Shared Sub logFile(ByVal ex As Exception, ByVal e1 As String, ByVal e2 As String,
                ByRef ctxt As HttpContext, ByVal nID As Int32, ByVal nomFile As String)
        Try
            'autoLog.WaitOne()
            mtx.WaitOne()
            Dim f As FileStream
            If (ctxt.Request.UrlReferrer IsNot Nothing AndAlso
            InStr(ctxt.Request.UrlReferrer.AbsoluteUri, "//localhost:") > 0) Then
                f = New FileStream(ctxt.Server.MapPath(".") + "/LOCAL.txt", FileMode.Append)
            Else
                f = New FileStream(
                "C:\inetpub\wwwroot\txt\" + nomFile, FileMode.Append,
             FileAccess.Write)
            End If
            Dim dt As DateTime = Now
            Dim host As String = ""
            Try
                host = getHostName(ctxt.Request.UserHostAddress)
            Catch ex1 As Exception

            End Try
            Dim sFch As String = Now.ToString("yyyy/MM/dd   hh:mm:ss")
            Dim fs As New StreamWriter(f, System.Text.Encoding.Unicode)
            fs.WriteLine(ctxt.Request.UserHostAddress + Chr(9) _
            + host + Chr(9) + sFch +
            Chr(9) + dt.ToShortTimeString() + Chr(9) + e1 + Chr(9) + e2)
            If Not (ex Is Nothing) Then
                'Dim e3 As String = ""
                'If haydsp Then e3 = TextBox1.Text
                fs.WriteLine(ctxt.Request.UserHostAddress + Chr(9) _
                + host + Chr(9) + Now().ToShortDateString() +
                Chr(9) + dt.ToShortTimeString() + Chr(9) +
                ex.ToString)
            End If
            Try
                fs.Close()
                f.Close()
            Catch ex2 As Exception

            End Try
        Catch ex3 As Exception
        Finally
            'autoLog.Set()
            mtx.ReleaseMutex()
        End Try
    End Sub
    Public Shared Sub contador(ByRef Page As System.Web.UI.Page)
        Try
            If Not Page.IsPostBack Then
                Try
                    'If Page.Request.UserHostAddress.ToString = "2.152.192.220" Then
                    'Exit Sub
                    'ElseIf Page.Request.Browser.Crawler OrElse
                    'Regex.IsMatch(Page.Request.UserAgent, "bot|crawl|spider|scan|census", RegexOptions.IgnoreCase) Then
                    'Exit Sub
                    'End If
                Catch ex As Exception

                End Try
                Dim host As String = ""
                Try
                    host = getHostName(Page.Request.UserHostAddress)
                    If Regex.IsMatch(host, "bot|crawl|spider|scan|census", RegexOptions.IgnoreCase) Then
                        Exit Sub
                    End If
                Catch ex1 As Exception

                End Try
                Dim fs As New FileStream(
                        Page.Server.MapPath("~\txt\calculadora.txt"), FileMode.Append)
                Dim sw As New StreamWriter(fs, UnicodeEncoding.Unicode)
                'For i = 0 To Page.Request.ServerVariables.Count - 1
                '    sw.WriteLine(Page.Request.ServerVariables.Keys(i).ToString + _
                '        "=" + Page.Request.ServerVariables.Item(i).ToString)
                'Next

                sw.Write(Page.Request.UserHostAddress.ToString + vbTab)

                sw.Write(host + vbTab)

                Dim sFch As String = Now.ToString("yyyy/MM/dd   hh:mm:ss")
                sw.Write(sFch + vbTab)

                Dim dt As DateTime = Now
                sw.Write(dt.ToShortTimeString + vbTab)

                If Page.Request.UserLanguages Is Nothing Then
                    sw.Write(" " + vbTab)
                Else
                    sw.Write(Join(Page.Request.UserLanguages, " ") + vbTab)
                End If
                If Page.Request.UrlReferrer Is Nothing Then
                    sw.Write(" " + vbTab)
                Else
                    sw.Write(Page.Request.UrlReferrer.ToString + vbTab)
                End If

                If Page.Request.Url Is Nothing Then
                    sw.Write(" " + vbTab)
                Else
                    sw.Write(URLDecode(Page.Request.Url.OriginalString) + vbTab)
                End If

                If Page.Request.UserAgent Is Nothing Then
                    sw.Write(" " + vbTab)
                Else
                    sw.Write(Page.Request.UserAgent + vbTab)
                End If
                If Page.Request.Url Is Nothing Then
                    sw.Write(" " + vbTab)
                Else
                    sw.Write(Page.Request.Url.AbsolutePath)
                End If
                sw.WriteLine("")
                sw.Close()
                fs.Close()
            End If
        Catch ex1 As Exception

        End Try
    End Sub
        Public Shared t, t2 As String
        Public Delegate Sub delegContador(ByVal page As UI.Page,
                                ByVal Request As HttpRequest,
                               ByVal Response As HttpResponse)

        Public Shared Sub contador(ByVal page As UI.Page,
                                ByVal Request As HttpRequest,
                               ByVal Response As HttpResponse)
            Try
                ' 0)
                contador(page)
                ' 1) la siguiente llamada:
                'contador2(page, Request, Response)

                ' ó 2) el sig. código:
                'Dim d As New delegContador(AddressOf contador2)
                'Dim ar As IAsyncResult = d.BeginInvoke( _
                '    page, Request, Response, Nothing, Nothing)
                'Dim ts As New TimeSpan(Now.Ticks)
                'Dim ts2 As New TimeSpan(Now.Ticks - ts.Ticks)
                'Do While Not ar.IsCompleted AndAlso _
                'ts2.TotalMilliseconds < 30000
                '    System.Threading.Thread.Sleep(200)
                '    ts2 = New TimeSpan(Now.Ticks - ts.Ticks)
                'Loop
                'd.EndInvoke(ar)
            Catch ex As Exception

            End Try
        End Sub
        Public Shared Sub contador2(ByVal page As UI.Page,
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
                    ip = Request.ServerVariables("REMOTE_ADDR")
                Catch ex As Exception
                End Try
                If ip = "" Then
                    Try
                        ip = Request.UserHostAddress
                    Catch ex As Exception

                    End Try
                End If
                Try
                    ua = Request.ServerVariables("HTTP_USER_AGENT")
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
                If t Is Nothing OrElse t = "" OrElse
            t2 Is Nothing OrElse t2 = "" Then
                    t = "0"
                    t2 = "0"
                End If
                sb = u ' subpágina

                query = "s=" & s & "&ip=" & ip & "&c=" & c & "&u=" & u & "&ln=" & ln &
             "&r=" & r1 & "&ua=" & ua & "&sb=" & sb & "&t=" & t & "&t2=" & t2 & "&v=" & v

                Dim sDom As String = "https://xrjunque.nom.es"
                If Request.UrlReferrer IsNot Nothing AndAlso
            Request.UrlReferrer.Host IsNot Nothing Then
                    query += "&refHost=" + Request.UrlReferrer.Host
                Else
                    query += "&refHost=" + sDom
                End If
                If Request.UrlReferrer IsNot Nothing AndAlso
            Request.UrlReferrer.AbsoluteUri IsNot Nothing Then
                    query += "&refAbsUri=" + Request.UrlReferrer.AbsoluteUri
                Else
                    'query += "&refAbsUri=" + sDom + Replace( _
                    'Request.FilePath, "\", "/")  '+ page.AppRelativeVirtualPath
                    query += "&refAbsUri=" + sDom + Replace(Replace(
                    page.AppRelativeVirtualPath, "\", "/"), "~", "")
                End If

                url = "https://xrjunque.nom.es/contador/conta1.aspx?" & query

                'url = "https://localhost:64842/conta1.aspx?" & query ' s=&ip=127.0.0.1&c=32&u=https://localhost/mates/RootFinder.aspx&ln=en-us&r=&ua=Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; InfoPath.1; .NET CLR 1.1.4322)&sb=default&t=633054938628130000&t2=633054938642973750&v=5071"


                'Response.Clear()
                Dim req As WebRequest = WebRequest.Create(url)
                Dim wr As WebResponse = req.GetResponse
                Dim sr As IO.Stream = wr.GetResponseStream
                Dim b(wr.ContentLength - 1) As Byte
                sr.Read(b, 0, b.Length)
                sr.Close()
                wr.Close()
                'Response.Write(System.Text.ASCIIEncoding.ASCII.GetString(b))
            Catch ex As Exception
                Dim s1 As String = ex.ToString
            End Try
        End Sub
        Public Shared Sub contador3(ByVal page As UI.Page,
                                ByVal Request As HttpRequest,
                               ByVal Response As HttpResponse, sb As String, u As String)
            Try
                If page.IsPostBack = False Then
                    'Exit Sub
                End If
                Dim query, url, s, c, r, ip, ua, ln, r1, v As String
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
                    ip = Request.ServerVariables("REMOTE_ADDR")
                Catch ex As Exception
                End Try
                If ip = "" Then
                    Try
                        ip = Request.UserHostAddress
                    Catch ex As Exception

                    End Try
                End If
                Try
                    ua = Request.ServerVariables("HTTP_USER_AGENT")
                Catch ex As Exception
                End Try
                Try
                    ln = Request.ServerVariables("HTTP_ACCEPT_LANGUAGE")
                Catch ex As Exception
                End Try
                'Try
                '    u = LCase(getPageName(Request)) ' nombre de la página
                'Catch ex As Exception
                '    u = ""
                'End Try
                'Try
                '    If u IsNot Nothing AndAlso _
                '    Len(u) = 0 Then
                '        u = LCase(page.AppRelativeVirtualPath)
                '        u = Replace(u, "\", "/")
                '        If InStr(u, "/") Then
                '            Dim vU() As String = Split(u, "/")
                '            u = vU(vU.Length - 1)
                '        End If
                '    End If
                'Catch ex As Exception

                'End Try
                s = "0"
                c = "0"
                v = "0"
                r1 = r
                If t Is Nothing OrElse t = "" OrElse
            t2 Is Nothing OrElse t2 = "" Then
                    t = "0"
                    t2 = "0"
                End If
                sb = u ' subpágina

                query = "s=" & s & "&ip=" & ip & "&c=" & c & "&u=" & u & "&ln=" & ln &
             "&r=" & r1 & "&ua=" & ua & "&sb=" & sb & "&t=" & t & "&t2=" & t2 & "&v=" & v

                Dim sDom As String = "https://xrjunque.nom.es"
                If Request.UrlReferrer IsNot Nothing AndAlso
            Request.UrlReferrer.Host IsNot Nothing Then
                    query += "&refHost=" + Request.UrlReferrer.Host
                Else
                    query += "&refHost=" + sDom
                End If
                If Request.UrlReferrer IsNot Nothing AndAlso
            Request.UrlReferrer.AbsoluteUri IsNot Nothing Then
                    query += "&refAbsUri=" + Request.UrlReferrer.AbsoluteUri
                Else
                    'query += "&refAbsUri=" + sDom + Replace( _
                    'Request.FilePath, "\", "/")  '+ page.AppRelativeVirtualPath
                    query += "&refAbsUri=" + sDom + Replace(Replace(
                    page.AppRelativeVirtualPath, "\", "/"), "~", "")
                End If

                url = "https://xrjunque.nom.es/contador/conta1.aspx?" & query

                'url = "https://localhost:64842/conta1.aspx?" & query ' s=&ip=127.0.0.1&c=32&u=https://localhost/mates/RootFinder.aspx&ln=en-us&r=&ua=Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; InfoPath.1; .NET CLR 1.1.4322)&sb=default&t=633054938628130000&t2=633054938642973750&v=5071"


                'Response.Clear()
                Dim req As WebRequest = WebRequest.Create(url)
                Dim wr As WebResponse = req.GetResponse
                Dim sr As IO.Stream = wr.GetResponseStream
                Dim b(wr.ContentLength - 1) As Byte
                sr.Read(b, 0, b.Length)
                sr.Close()
                wr.Close()
                'Response.Write(System.Text.ASCIIEncoding.ASCII.GetString(b))
            Catch ex As Exception
                Dim s1 As String = ex.ToString
            End Try
        End Sub


        Public Shared Function URLDecode(ByVal strToDecode As String) As String
            Dim strin, strOut, strLeft, strRight As String, intPos As Int32
            strin = strToDecode
            strOut = ""
            intPos = InStr(strin, "+") ' look for + and replace with space
            Do While intPos
                strLeft = ""
                strRight = ""
                If intPos > 1 Then strLeft = Left(strin, intPos - 1)
                If intPos < Len(strin) Then strRight = Mid(strin, intPos + 1)
                strin = strLeft & " " & strRight
                intPos = InStr(strin, "+") ' and then look for next onie
            Loop
            intPos = InStr(strin, "%") ' look for ASCII coded characters
            Do While intPos
                If intPos > 1 Then strOut = strOut & Left(strin, intPos - 1)
                strOut = strOut & Chr(CInt("&H" & Mid(strin, intPos + 1, 2)))
                If intPos > Len(strin) - 3 Then
                    strin = ""
                Else
                    strin = Mid(strin, intPos + 3)
                End If
                intPos = InStr(strin, "%") ' and then look for next one
            Loop
            Return strOut & strin
        End Function
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


    End Class
