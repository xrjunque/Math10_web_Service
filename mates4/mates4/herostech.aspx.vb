Imports System.IO

Public Class herostech1
    Inherits System.Web.UI.Page

    Dim pagina, resolucio, colorDepth, ip, osExpl, idioma, referer, subpag As String
    Dim sVol, sRefHost, sRefAbsUri As String
    Protected Sub form1_Load(sender As Object, e As System.EventArgs) Handles form1.Load
        If Page.IsPostBack Then
            Exit Sub
        End If
        'If InStr(Request.UserHostName, "herostechnology.co.uk") < 1 Then
        '    Exit Sub
        'End If
        responseHeader()
        retrieveQueryParams()
        writeCounter()
    End Sub
    Public Sub responseHeader()
        Try
            Response.Clear()
            Response.AddHeader("P3P", "CP=""OUR IND DSP NON COR""")
        Catch ex As Exception

        End Try
    End Sub
    Public Shared Function getHostName(ByVal strIP As String) As String
        Dim myIP As System.Net.IPAddress = System.Net.IPAddress.Parse(strIP)
        Dim hostInfo As System.Net.IPHostEntry = _
        System.Net.Dns.GetHostEntry(myIP)
        Return hostInfo.HostName
    End Function
    Sub writeCounter()
        Try
            Dim fs As New FileStream( _
                    Page.Server.MapPath("~\txt\herostech.txt"), FileMode.Append)
            Dim sw As New StreamWriter(fs, UnicodeEncoding.Unicode)
            'For i = 0 To Page.Request.ServerVariables.Count - 1
            '    sw.WriteLine(Page.Request.ServerVariables.Keys(i).ToString + _
            '        "=" + Page.Request.ServerVariables.Item(i).ToString)
            'Next

            Dim ip As String = Request.Params("ip")
            If ip Is Nothing Then
                sw.Write(" " + vbTab)
            Else
                sw.Write(Replace(ip, "'", "") + vbTab)
            End If


            Dim host As String = String.Empty
            Try
                host = getHostName(ip)
            Catch ex1 As Exception
            End Try
            If host Is Nothing Then
                sw.Write(" " + vbTab)
            Else
                sw.Write(Replace(host, "'", "") + vbTab)
            End If

            Dim sFch As String = Now.ToString("yyyy/MM/dd   hh:mm:ss")
            sw.Write(sFch + vbTab)

            Dim dt As DateTime = Now
            sw.Write(dt.ToShortTimeString + vbTab)


            If Page.Request.Params("ln") Is Nothing Then
                sw.Write(" " + vbTab)
            Else
                sw.Write(Page.Request.Params("ln") + vbTab)
            End If

            Dim U As String = Request.Params("u")
            If U Is Nothing Then
                sw.Write(" ")
            Else
                sw.Write(U + vbTab)
            End If

            If Request.Params("r") Is Nothing Then
                sw.Write(" " + vbTab)
            Else
                sw.Write(Request.Params("r") + vbTab)
            End If
            If Page.Request.Params("ua") Is Nothing Then
                sw.Write(" " + vbTab)
            Else
                sw.Write(Page.Request.Params("ua") + vbTab)
            End If
            'If Page.Request.Url Is Nothing Then
            '    sw.Write(" " + vbTab)
            'Else
            '    sw.Write(Page.Request.Url.AbsolutePath)
            'End If




            'If Page.Request.UserLanguages Is Nothing Then
            '    sw.Write(" " + vbTab)
            'Else
            '    sw.Write(Join(Page.Request.UserLanguages, " ") + vbTab)
            'End If

            'Dim urlReferrer As String = Request.Params("r")
            'If urlReferrer Is Nothing Then
            '    sw.Write(" " + vbTab)
            'Else
            '    urlReferrer = Replace(urlReferrer, "'", "")
            '    sw.Write(urlReferrer + vbTab)
            'End If

            'Dim userAgent As String = Request.Params("ua")
            'If userAgent Is Nothing Then
            '    sw.Write(" " + vbTab)
            'Else
            '    userAgent = Replace(userAgent, "'", "")
            '    sw.Write(userAgent + vbTab)
            'End If




            sw.WriteLine("")
            sw.Close()
            fs.Close()
        Catch ex1 As Exception

        End Try
    End Sub
    Sub retrieveQueryParams()
        Try
            Dim coll As NameValueCollection = Request.Params
            ' Get names of all keys into a string array.
            Dim arr1() As String = coll.AllKeys
            Response.Write("<br>arr1.length: " + arr1.Length.ToString)
            For loop1 As Int32 = 0 To arr1.Length - 1
                Response.Write("Key: " + Server.HtmlEncode(arr1(loop1)) + "<br>")
                Dim arr2() As String = coll.GetValues(arr1(loop1))
                For loop2 As Int32 = 0 To arr2.Length - 1
                    Response.Write("Value " + loop2.ToString + ": " + Server.HtmlEncode(arr2(loop2)).ToString + "<br>")
                Next
                Dim p1 As Int32 = arr1(loop1).IndexOf(";")
                If p1 > -1 Then
                    arr1(loop1) = arr1(loop1).Substring(p1 + 1)
                End If
                Select Case arr1(loop1)
                    Case "u" : pagina = Server.HtmlEncode(arr2(0)) : Exit For
                    Case "s" : resolucio = Server.HtmlEncode(arr2(0)) : Exit For
                    Case "c" : colorDepth = Server.HtmlEncode(arr2(0)) : Exit For
                    Case "ip" : ip = Server.HtmlEncode(arr2(0)) : Exit For
                    Case "ua" : osExpl = Server.HtmlEncode(arr2(0)) : Exit For
                    Case "ln" : idioma = Server.HtmlEncode(arr2(0)) : Exit For
                    Case "r" : referer = Server.HtmlEncode(arr2(0)) : Exit For
                    Case "sb" : subpag = Server.HtmlEncode(arr2(0)) : Exit For
                    Case "v" : sVol = Server.HtmlEncode(arr2(0)) : Exit For
                    Case "refHost" : sRefHost = Server.HtmlEncode(arr2(0)) : Exit For
                    Case "refAbsUri" : sRefAbsUri = Server.HtmlEncode(arr2(0)) : Exit For
                End Select

            Next
        Catch ex As Exception
            Dim s1 As String = ex.ToString
            Dim s2 As String = s1
        End Try
    End Sub
End Class