Imports System.IO
Imports System.Net
Imports System.Xml

Public Class latLong
    Inherits System.Web.UI.Page

    Private Sub form1_Load(sender As Object, e As System.EventArgs) Handles form1.Load
        Try
            If Page.IsPostBack = False Then
                Dim ip As String = Request.Params("ip")
                Dim req As New WebClient
                Dim s As Stream
                'If InStr(Request.Url.Host, "heros") Then
                If False Then
                    Try
                        '' usuario: HerosTechnologyCoUK contraseña: 24390000
                        s = req.OpenRead("https://api.ipinfodb.com/v3/ip-city/?key=55609fda8e6820cc05078967f4fdea9ce2c6eda360b923367ddbc9ceee81dbf9&ip=" + ip)

                        ' usuario: heros contraseña: rosafont
                        's = req.OpenRead("http://api.ipinfodb.com/v3/ip-city/?key=62dd66c407a9ed84b5118b2d47a072f6319ae90d5868e651898124a8fb5d527c&ip=" + ip)
                    Catch ex As Exception
                        Response.Redirect("http://ipinfodb.com?ip=" + ip)
                        Exit Sub
                    End Try

                    ' para dejar como antes, descomentar aqui:
                    'Response.Redirect("https://www.ipaddressguide.com/ip-geolocation?ip=" + ip)
                    'Response.Redirect("https://dnslytics.com/ip/" + ip)
                    'Exit Sub
                Else
                    ' BUENO:
                    s = req.OpenRead("http://api.ipinfodb.com/v3/ip-city/?key=2c1e269acf2f86f71f79aa6730fe13fb4a94e9a90919a34bf34fa4417a0d9d74&ip=" + ip)
                    ' END_BUENO
                    's = req.OpenRead("http://ip-api.com/xml/#" + ip + "?lang=es")

                End If
                'If InStr(Request.Url.Host, "heros") Then
                If True Then
                    Dim sr As New StreamReader(s)
                    Dim a(0) As Char
                    Dim e1 As String = String.Empty
                    Do While Not sr.EndOfStream
                        sr.Read(a, 0, a.Length)
                        e1 += a(0)
                    Loop
                    With Literal1
                        .Text = "<span style=""font-family:Arial""><br />"
                        Dim e2() As String = Split(e1, ";")
                        e1 = Replace(e1, ";", "<br />")
                        Dim sLat As String = e2(8)
                        Dim sLong As String = e2(9)

                        e1 += " &nbsp;<a href=""https://www.latlong.net/c/?lat=" + sLat + "&long=" + sLong + """ >" + sLat + "," + sLong + "</a></span>"
                        .Text += e1

                    End With
                Else
                    Dim ve() As String = {"country", "region", "regionName", "city", "zip", "lat", "lon", "timezone"}
                    Dim sLong As String = ""
                    Dim sLat As String = ""
                    Dim e1 As String = ""
                    Using xtr As XmlReader = XmlReader.Create(s)
                        Do While xtr.Read
                            If xtr.IsStartElement Then
                                Dim sNom As String = xtr.Name
                                Dim iv As Int32 = Array.IndexOf(ve, sNom)
                                Dim inner As String = ""
                                If iv > -1 Then
                                    inner = xtr.ReadInnerXml
                                    ve(iv) += ": " + inner
                                End If
                                If sNom = "lat" Then sLat = recorta(inner, False)
                                If sNom = "lon" Then sLong = recorta(inner, False)
                            End If
                        Loop
                    End Using
                    For i As Int32 = 0 To ve.Length - 1
                        e1 += ve(i)
                    Next
                    Literal1.Text = "<span style=""font-family:Arial""><br />"
                    e1 = recorta(e1, True)
                    e1 += " &nbsp;<a href=""https://www.latlong.net/c/?lat=" + sLat + "&long=" + sLong + """ >" + sLat + "," + sLong + "</a></span>"
                    Literal1.Text += e1


                End If

            End If
        Catch ex As Exception
            Literal1.Text = ex.ToString
        End Try

    End Sub
    Function recorta(e1 As String, br As Boolean) As String
        e1 = Replace(e1, "<![CDATA[", "")
        e1 = Replace(e1, "]", "")
        If br Then
            e1 = Replace(e1, ">", "<br />")
        Else
            e1 = Replace(e1, ">", "")
        End If
        Return e1
    End Function
End Class
