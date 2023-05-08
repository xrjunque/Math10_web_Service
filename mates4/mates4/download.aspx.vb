Imports System.IO
Imports System.Web
Imports System.Net

Partial Public Class download
    Inherits System.Web.UI.Page
    Dim file As String
    Dim origen As String = ""
    Dim bWindipoles As Boolean
    Dim sLit As String = "<h1>Wrong link</h1><h4>You should perform your download from a valid page.</h4>" + _
    "<br /><a href=""https://xrjunque.nom.es/swdownload.aspx?"">Go to SW download page.</a>"
    Dim sLit2 As String = "<h1>Download not available.</h1><h4>You should perform a current file download.</h4>" + _
            "<br /><a href=""https://xrjunque.nom.es/swdownload.aspx?"">Go to SW download page.</a>"
    Dim ruta As String
    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Dim e2 As String = String.Empty
        Dim sUrl As String = String.Empty
        Try
            Dim o = Request.Params("file")
            If o Is Nothing Then
                Exit Sub
            End If
            e2 = CStr(o)
            If Trim(e2) = "" Then
                Exit Sub
            End If
            'Try
            '    Dim o As Object = Request.UrlReferrer
            '    Dim o1 As Object = IIf(o Is Nothing, Nothing, Request.UrlReferrer.AbsoluteUri)
            'Catch ex As Exception
            '    'If LCase(e2) = "windipoles" Then
            '    '    bWindipoles = True
            '    '    origen = "Brinkster "
            '    '    brinkster()
            '    'End If
            'End Try
            'Exit Sub


            Dim vValidUrls() As String = _
                {"https://herostechnology.co.uk/", _
                 "https://www.herostechnology.co.uk/", _
                 "https://www.m0wwa.co.uk/", _
                 "https://xrjunque.nom.es", _
                 "https://xrjunque.nom.es", _
                 "https://www.xrjunque.nom.es", _
                 "https://www.xrjunque.nom.es" _
                }
            If Request.UrlReferrer IsNot Nothing AndAlso _
            Request.UrlReferrer.AbsoluteUri IsNot Nothing Then
                sUrl = Request.UrlReferrer.AbsoluteUri
            Else
                lit1.Text = "<h1>Wrong link</h1><h4>You should perform your download from a valid page."
                lit1.Text += "<br /><a href='https://xrjunque.nom.es/swdownload.aspx'>https://xrjunque.nom.es/swdownload.aspx</a>"
                Response.Redirect("https://xrjunque.nom.es/swdownload.aspx")
                Exit Try
            End If
            Dim lnUrl As Int32 = Len(sUrl)
            Dim i As Int32
            If Request.IsLocal Then
                origen = "Brinkster "
                brinkster()
                Exit Try
            End If
            For i = 0 To vValidUrls.Length - 1
                Dim ln As Int32 = Len(vValidUrls(i))
                If ln <= lnUrl AndAlso _
                Mid(sUrl, 1, ln) = vValidUrls(i) Then
                    Select Case i
                        Case 0, 1
                            origen = "HEROStech "
                            heros()
                            Exit For
                        Case 2
                            origen = "m0wwa "
                            m0wwa()
                            Exit For
                        Case 3
                            If InStr(sUrl, vValidUrls(0)) OrElse _
                            InStr(sUrl, vValidUrls(1)) Then
                                origen = "HEROStech "
                                heros()
                                Exit For
                            ElseIf InStr(sUrl, vValidUrls(2)) Then
                                origen = "m0wwa "
                                m0wwa()
                                Exit For
                            End If

                            origen = "Brinkster "
                            brinkster()
                            Exit For
                        Case 4
                            origen = "Brinkster "
                            brinkster()
                            Exit For
                    End Select
                End If
            Next
            If i >= vValidUrls.Length Then
                lit1.Text = sLit ' comentar aqui y descomentar las siguientes:
                'If InStr(e2, "mates8") > 0 Then
                '    origen = "Brinkster "
                '    brinkster()
                'Else
                '    lit1.Text = sLit
                'End If
            End If
            'If Request.UrlReferrer Is Nothing Then
            '    'sLit = "<h1>Wrong link</h1><h4>You should perform your download from a valid page."
            'ElseIf InStr(Request.UrlReferrer.AbsoluteUri, "https://www.herostechnology.co.uk/") Then
            '    origen = "HEROStech "
            '    heros()
            'ElseIf InStr(Request.UrlReferrer.AbsoluteUri, "https://www.m0wwa.co.uk/") Then
            '    origen = "m0wwa "
            '    m0wwa()
            'ElseIf InStr(Request.UrlReferrer.AbsoluteUri, "https://xrjunque.nom.es") Then
            '    origen = "Brinkster "
            '    brinkster()
            'ElseIf InStr(Request.UrlReferrer.AbsoluteUri, "https://www.xrjunque.nom.es") Then
            '    origen = "Brinkster "
            '    brinkster()
            'End If
            'heros()
        Catch ex1 As Threading.ThreadAbortException
        Catch ex As Exception
            logFile(ex, e2, sUrl)
            lit1.Text = ex.ToString
            'sLit = "<h1>Wrong link</h1><h4>You should perform your download from a valid page."
            'Dim s1 As String = ex.ToString()
            'Dim s2 As String = s1
            'Throw ex
        Finally
        End Try
    End Sub
    Private Shared Function extraeNomDominio(ByVal e1 As String) As String
        e1 = LCase(e1)
        If Mid(e1, 1, 7) = "https://" Then
            e1 = Mid(e1, 8)
        End If
        If Len(e1) > 4 AndAlso Mid(e1, 1, 4) = "www." Then
            e1 = Mid(e1, 5)
        End If
        Dim e2() As String = Split(e1, "/")
        e1 = e2(0)
        Return e1
    End Function

    Sub m0wwa()
        Dim i As Int32
        file = Request.QueryString("file")
        If file Is Nothing Then Exit Sub
        Dim downloadFile As String = ""
        ruta = Server.MapPath(".") + "/../database/downloads/m0wwa/"
        Dim rutaLOCAL As String = "C:\Users\Xavier\SkyDrive\Documentos\VisualStudio\mates04_\_mates04\_mates04\bin\"
        Dim nomFile() As String = {
                                   "calcoil.zip", "rfcalc.zip",
                                   "RFSim99.zip",
                                   "SCR_Preselector_CAT_web.zip",
                                   "WinDipolesv4.8.8.zip",
                                   "WinFonts.zip",
                                   "WinRanking.zip",
                                   "M0WWA_SCR_Preselector_QEX article.pdf",
                                   "M0WWA_tiny_Preselector_I2C_control_module_manual.pdf",
                                   "i2c_fe.zip",
                                   "experimenter.zip",
                                   "SCR_Preselector_WinRad.zip"
                                    }
        Select Case LCase(file)
            Case "calcoil" : i = 0
            Case "rfcalc" : i = 1
            Case "rfsim99" : i = 2
            Case "preselector" : i = 3
            Case "windipoles" : i = 4
            Case "winfonts" : i = 5
            Case "winranking" : i = 6
            Case "qexarticle" : i = 7
            Case "tinyctrlmodule" : i = 8
            Case "frontend" : i = 9
            Case "experimenter" : i = 10
            Case "scr_wrad" : i = 11
            Case Else
                Exit Sub
        End Select
        logFile(Nothing, "download: ", file + "=" + nomFile(i))

        download(nomFile(i), downloadFile, ruta, rutaLOCAL, downloadFile.Length)
        'If InStr(Request.UserHostAddress, "localhost") OrElse InStr(Request.UserHostAddress, "127.0.") Then
        '    downloadFile = rutaLOCAL + nomFile(i)
        'Elseb
        '    downloadFile = ruta + nomFile(i)
        'End If
        'Response.ClearHeaders()
        'Response.AddHeader("Content-Disposition", "attachment; filename=" & nomFile(i))
        'Response.WriteFile(downloadFile)
        ''Response.Write(downloadFile
        'logFile(Nothing, "downloaded: ", downloadFile)
    End Sub
    Sub heros()
        Dim i As Int32
        file = Request.QueryString("file")
        If file Is Nothing Then Exit Sub
        'logFile(Nothing, "download: ", file)
        Dim downloadFile As String = ""
        ruta = Server.MapPath(".") + "/../database/downloads/herostech/"
        Dim rutaLOCAL As String = "E:\new\Visual Studio Projects\mates8\mates8\images\"
        Dim nomFile() As String = {
                                   "fonts.zip", "winranking.zip",
                                   "WinDipolesv4.8.8.zip",
                                   "calcoil.zip", "rfcalc.zip",
                                   "RFSim99.zip",
                                   "SCR_Preselector_CAT_web.zip",
                                    "WinFonts.zip",
                                    "M0WWA_SCR_Preselector_QEX article.pdf",
                                    "WinPic2004-1021.zip",
                                    "SCR_Preselector_Brochure.pdf",
                                    "WinToroids.zip",
                                    "tinySCRPreSelector.zip",
                                    "tinyexperimenter.zip",
                                   "i2c_fe.zip",
                                   "SCR_Preselector_WinRad.zip",
                                   "CAT_Decoder.zip",
                                   "ASCII_code.pdf",
                                   "Band_acronym_Frequency_Wavelength.pdf",
                                   "Herostech_dBm_Volts_Power_conversion_table.pdf",
                                   "Herostech_Return_Loss_VSWR_conversion_table.pdf",
                                   "Herostech_S-Units_dBm_Volts_conversion_table.pdf",
                                   "Relay_form_definitions.pdf",
                                   "Windows_Vista_keyboard_shortcuts.pdf",
                                   "Windows7_keyboard_shortcuts.pdf",
                                   "Windows8_keyboard_shortcuts.pdf",
                                   "Windows95_98_ME_keyboard_shortcuts.pdf",
                                   "WindowsXP_keyboard_shortcuts.pdf"
                                    }
        Select Case LCase(file)
            Case "winfont" : i = 0
            Case "winranking" : i = 1
            Case "windipoles" : i = 2
            Case "calcoil" : i = 3
            Case "rfcalc" : i = 4
            Case "rfsim99" : i = 5
            Case "preselector" : i = 6
            Case "winfonts" : i = 7
            Case "qexarticle" : i = 8
            Case "winpic" : i = 9
            Case "preselectorbrochure" : i = 10
                Response.Clear()
                Response.Redirect("https://xrjunque.nom.es/herostech/SCR_Preselector_Brochure.pdf", False)

            Case "wintoroids" : i = 11
            Case "tinypresel" : i = 12
            Case "experimenter" : i = 13
            Case "frontend" : i = 14
            Case "scr_wrad" : i = 15
            Case "cat_decoder" : i = 16
            Case "ascii" : i = 17
            Case "band" : i = 18
            Case "dbm" : i = 19
            Case "returnloss" : i = 20
            Case "sunits" : i = 21
            Case "relay" : i = 22
            Case "vistashortcuts" : i = 23
            Case "win7shortcuts" : i = 24
            Case "win8shortcuts" : i = 25
            Case "win9598meshortcuts" : i = 26
            Case "winxpshortcuts" : i = 27
            Case Else
                Exit Sub
        End Select
        'logFile(Nothing, "download: ", file + "=" + nomFile(i))
        download(nomFile(i), downloadFile, ruta, rutaLOCAL, downloadFile.Length)
        'If InStr(Request.UserHostAddress, "localhost") OrElse InStr(Request.UserHostAddress, "127.0.") Then
        '    downloadFile = rutaLOCAL + nomFile(i)
        'Else
        '    downloadFile = ruta + nomFile(i)
        'End If
        'Response.ClearHeaders()
        'Response.AddHeader("Content-Disposition", "attachment; filename=" & nomFile(i))
        'Response.WriteFile(downloadFile)
        ''Response.Write(downloadFile
        'logFile(Nothing, "downloaded: ", downloadFile)
    End Sub
    Sub brinkster()
        Dim i As Int32
        file = Request.QueryString("file")
        If file Is Nothing Then Exit Sub
        'logFile(Nothing, "download: ", file)
        Dim downloadFile As String = ""
        'Dim ruta As String = Server.MapPath(".") + "/downloads/"
        Dim ruta As String = "C:\inetpub\wwwroot\downloads\"
        'ruta = "e:\sites\premium2\xrjunque\database\downloads\brinkster\"

        Dim rutaLOCAL As String = ruta ' "C:\Users\Xavier\Documents\Visual Studio 2010\Projects\mates4\mates4\mates4\downloads"
        Dim nomFile() As String = {
                                    "math10_exe.zip", "math10_exe",
                                    "math10_src.zip", "math10_src",
                                    "mates8v8.4.194.zip", "mates8v8.4.194",
                                    "mates8v8.3.192source.zip", "mates8v8.3.192source",
                                   "fonts.zip", "winfont",
                                   "winranking.zip", "winranking",
                                   "WinDipolesv4.8.8.zip", "windipoles",
                                    "PolyCalcInExcel_v8.3.192.zip", "polycalcinexcel",
                                    "mates8_excel_v8.3.192.zip", "mates8_excel",
                                    "mates8NetDll.zip", "mates8netlib",
                                    "TestMates8 for .NET programming.zip", "testnetmates8",
                                    "TestMates8 for VB6 programming.zip", "testvb6mates8",
                                    "Mates8 usage in VB.Net.pdf", "mates8vbnetusage",
                                    "Mates8 usage in VB6.pdf", "mates8vb6usage",
                                    "Mates8 User's Guide.pdf", "mates8users",
                                    "tutorialv8_4_0.pdf", "tutorialv8_4_0",
                                    "tutorialv8_4_2.pdf", "tutorialv8_4_2",
                                    "TimeZones.zip", "timezones",
                                    "tutorialv8_4_3.pdf", "tutorialv8_4_3",
                                    "informes.zip", "informes",
                                    "Mates8 Manual de usuario.pdf", "mates8usuario",
                                    "Convert2rpn.zip", "convert2rpn",
                                    "SCR_Preselector_CAT_2020_CD_content_Phil.zip", "preselector",
                                    "Mates8 User's Guide.htm", "mates8usershtm",
                                    "ExecMacroToWordDocsInFolder.zip", "wordmacros"
                                    }

        i = Array.IndexOf(nomFile, LCase(file)) - 1
        If i < 0 Then
            Dim bMates8 As Boolean = False
            If InStr(LCase(file), "mates8") > 0 Then
                bMates8 = True
                i = 0
            End If
            If InStr(LCase(file), "source") > 0 Then
                If bMates8 Then
                    i = 2
                End If
            End If
            If i < 0 Then
                lit1.Text = sLit2
                Exit Sub
            End If
        End If
        Try
            'logFile(Nothing, "downloading: ", file + "=" + nomFile(i))
            download(nomFile(i), downloadFile, ruta, rutaLOCAL, downloadFile.Length)
        Catch ex2 As Threading.ThreadAbortException
        Catch ex As Exception
            logFile(Nothing, "download ERROR: ", file + "=" + nomFile(i) + " " + ex.ToString)
        End Try
        'If InStr(Request.UserHostAddress, "localhost") OrElse InStr(Request.UserHostAddress, "127.0.") Then
        '    downloadFile = rutaLOCAL + nomFile(i)
        'Else
        '    downloadFile = ruta + nomFile(i)
        'End If
        'Response.ClearHeaders()
        'Response.AddHeader("Content-Disposition", "attachment; filename=" & nomFile(i))
        'Response.WriteFile(downloadFile)
        ''Response.Write(downloadFile
        'logFile(Nothing, "downloaded: ", downloadFile)
    End Sub
    Sub downloadBAD(ByVal nomfile As String, ByVal downloadFile As String, ByVal ruta As String, ByVal rutaLOCAL As String, ByVal length As Int32)


        '  WEB CLIENT   WEB CLIENT  WEB CLIENT  WEB CLIENT !!!!!!


        Try
            If Request.IsLocal Then
                ruta = rutaLOCAL
            End If
            'Dim remoteUri As String = ruta   ' "https://www.contoso.com/library/homepage/images/"
            Dim fileName As String = nomfile

            'Dim myStringWebResource As String = Nothing
            ' Create a new WebClient instance. 
            Dim myWebClient As New WebClient()

            ' Concatenate the domain with the Web resource filename. Because DownloadFile  
            'requires a fully qualified resource name, concatenate the domain with the Web resource file name.
            'myStringWebResource = remoteUri
            If Not IO.File.Exists(ruta + fileName) Then
                logFile(Nothing, "archivo no encontrado: ", ruta + fileName)
                Exit Sub
            End If
            'logFile("Downloading File ""{0}"" from ""{1}"" ......." + ControlChars.Cr + ControlChars.Cr, fileName, myStringWebResource)
            logFile(Nothing, "downloading: ", "=" + ruta + fileName)
            ' The DownloadFile() method downloads the Web resource and saves it into the current file-system folder.
            'myWebClient.BaseAddress = ruta
            myWebClient.BaseAddress = String.Empty
            Dim uri As New System.Uri(ruta + fileName)
            myWebClient.DownloadFile(uri, "C:\\" + fileName)

            logFile(Nothing, "Successfully Downloaded: " + fileName, "")
            'Console.WriteLine((ControlChars.Cr + "Downloaded file saved in the following file system folder:" + ControlChars.Cr + ControlChars.Tab + Application.StartupPath))





        Catch ex As Exception
            logFile(ex, "download ERROR: " + downloadFile + " ruta:" + ruta + nomfile, ex.Message)
        End Try
    End Sub

    Sub download(ByVal nomfile As String, ByVal downloadFile As String, ByVal ruta As String, ByVal rutaLOCAL As String, ByVal length As Int32)
        Try
            If Request.IsLocal Then
                downloadFile = rutaLOCAL + nomfile '(i)
            Else
                downloadFile = ruta + nomfile '(i)
            End If
            logFile(Nothing, "downloading: ", downloadFile + "=" + nomfile)
            System.Threading.Thread.Sleep(700)

            Dim fi As New IO.FileInfo(downloadFile)
            'Response.ClearHeaders()
            'Response.AddHeader("Content-Disposition", "attachment; filename=" & nomfile)
            'Response.AddHeader("Content-Length", fi.Length)

            HttpContext.Current.Response.ClearContent()
            HttpContext.Current.Response.ClearHeaders()
            HttpContext.Current.Response.AddHeader("Content-Disposition", "inline; filename=" & nomfile)
            HttpContext.Current.Response.AddHeader("Content-Length", fi.Length)

            Dim e1() As String = Split(nomfile, ".")
            Dim fileExt As String = e1(e1.Length - 1)
            'Response.ContentType = MimeTypes.getMime2(fileExt)
            'If bWindipoles Then
            '    'Set the appropriate ContentType.
            '    Response.ContentType = "Application/pdf"
            '    'Get the physical path to the file.
            '    Dim FilePath As String = MapPath("acrobat.pdf")
            '    'Write the file directly to the HTTP content output stream.
            '    Response.WriteFile(FilePath)
            '    logFile(Nothing, "downloaded: ", downloadFile)
            '    Response.End()
            'End If

            If InStr(nomfile, ".pdf") Then
                HttpContext.Current.Response.ContentType = "application/octet-stream"
            ElseIf InStr(LCase(nomfile), ".zip") Then
                HttpContext.Current.Response.ContentType = "application/zip"
            Else
                HttpContext.Current.Response.ContentType = "application/octet-stream"
            End If
            HttpContext.Current.Response.WriteFile(downloadFile)
            HttpContext.Current.Response.Flush()
            logFile(Nothing, "downloaded: ", downloadFile + "=" + nomfile)
            System.Threading.Thread.Sleep(700)
            'HttpContext.Current.Response.Close()
            'Dim ms As New MemoryStream(fi.Length)
            'Dim fs As New IO.FileStream(downloadFile, FileMode.Open)
            'Dim br As New BinaryReader(fs)
            'Dim pack As Int32 = 1024 ' 10K
            'Dim startBytes As Int32 = 0
            'br.BaseStream.Seek(startBytes,SeekOrigin.Begin )
            'Dim maxCount As Int32 = Math.Floor((fi.Length - startBytes) / pack) + 1

            'Dim b(fi.Length - 1) As Byte
            'b = br.ReadBytes(fi.Length)
            'HttpContext.Current.Response.BinaryWrite(b)

            'Dim i As Int32
            'Dim cnt As Int32 = 0
            'Do
            '    If Response.IsClientConnected Then
            '        If cnt + pack < fi.Length Then
            '            Dim b() As Byte = br.ReadBytes(pack)
            '            ms.Write(b, 0)
            '        Else
            '            Response.BinaryWrite(br.ReadBytes(fi.Length - cnt))
            '            Exit Do
            '        End If
            '        cnt += pack
            '    Else
            '        Exit Sub
            '    End If
            'Loop

            ''HttpContext.Current.Response.WriteFile(downloadFile)
            'br.Close()

            'Dim nomArchivo As String = ""
            'Try
            '    nomfile = Replace(nomfile, "/", "\")
            '    Dim e2() As String = Split(nomfile, "\")
            '    If e2.Length Then
            '        nomArchivo = e2(e2.Length - 1)
            '    End If
            'Catch ex As Exception

            'End Try

            'System.Threading.Thread.Sleep(1000)
            HttpContext.Current.Response.End()
        Catch ex1 As Threading.ThreadAbortException
            logFile(Nothing, "downloaded: ", downloadFile + "=" + nomfile)
        Catch ex As Exception
            logFile(Nothing, "download ERROR: ", downloadFile + "=" + nomfile)
        End Try
    End Sub
    Sub logFile(ByVal ex As Exception, ByVal e1 As String, ByVal e2 As String)
        Try
            Try
                If Len(e1) Then
                    e1 = Replace(e1, vbCrLf, "")
                End If
                If Len(e2) Then
                    e2 = Replace(e2, vbCrLf, "")
                End If
            Catch ex2 As Exception

            End Try
            Dim f As FileStream
            If Request.IsLocal Then
                Exit Sub
            End If
            f = New FileStream( _
            "C:\inetpub\wwwroot\txt\downloads.txt", FileMode.Append, _
             FileAccess.Write)
            Dim dt As DateTime = Now.ToUniversalTime()
            Dim host As String = ""
            Try
                host = getHostName(Request.UserHostAddress)
            Catch ex1 As Exception

            End Try
            Dim fs As New StreamWriter(f, System.Text.Encoding.Unicode)
            fs.WriteLine(origen + Chr(9) + Request.UserHostAddress + Chr(9) _
            + host + Chr(9) + Now().ToUniversalTime().ToShortDateString() + _
            Chr(9) + dt.ToShortTimeString() + Chr(9) + e1 + Chr(9) + e2)
            If Not (ex Is Nothing) Then
                fs.WriteLine(Request.UserHostAddress + Chr(9) _
                + host + Chr(9) + Now().ToUniversalTime().ToShortDateString() + _
                Chr(9) + dt.ToShortTimeString() + Chr(9) + _
                ex.Message + Chr(9) + "texbox1: " + e1 + "<br>" + e2)
            End If
            fs.Close()
            Try
                f.Close()
            Catch ex2 As Exception

            End Try
        Catch ex3 As Exception

        End Try
    End Sub
    Public Function getHostName(ByVal strIP As String) As String
        Dim myIP As System.Net.IPAddress = System.Net.IPAddress.Parse(strIP)
        Dim hostInfo As System.Net.IPHostEntry = _
        System.Net.Dns.GetHostEntry(myIP)
        Return hostInfo.HostName
    End Function
End Class


